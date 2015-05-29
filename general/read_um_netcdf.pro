PRO read_um_netcdf,run_ids,varname,stream,dates,box,variable,longitudes,$
                   latitudes,levels,ens_avg=ens_avg, time_avg=time_avg

n_runs = N_ELEMENTS(run_ids)
basedir = '/home/ss901165/um_output/'
month_abbr = ['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']

; Decompose [dates] array and get beginning and ending months for read
start_month = FLOOR(dates(0)/30.)
stop_month = FLOOR(dates(1)/30.)
number = 2
WHILE start_month ge 12 DO BEGIN
	start_month = start_month - 12
	stop_month = stop_month - 12
	number = number + 1	
ENDWHILE
n_months = stop_month-start_month+1
n_days = dates(1)-dates(0)+1
IF start_month ne stop_month THEN BEGIN
    start_pos = dates(0)-start_month*30
    start_count = 30-start_pos
    stop_count = 30-((stop_month+1)*30-dates(1))+1
ENDIF ELSE BEGIN
    start_pos = dates(0)-start_month*30
    start_count = dates(1)-dates(0)+1
    stop_count = start_count
ENDELSE

FOR i=0,n_runs-1 DO BEGIN
    varstart=0
    FOR j=0,n_months-1 DO BEGIN
        CASE j OF
            0 : BEGIN
                offset = start_pos
                count  = start_count
            END
            n_months-1 : BEGIN
                offset = 0
                count = stop_count
            END
            ELSE : BEGIN
                offset = 0
                count = 30
            END
        ENDCASE
	month = j+start_month
	WHILE month ge 12 DO month = month-12
	filename=basedir+run_ids(i)+'/'+run_ids(i)+'a.p'+stream+'j'+strtrim(string(number),1)+month_abbr(month)+'.nc'
	IF month_abbr(month) eq 'dec' THEN number = number+1
        ; print,filename
        id = NCDF_OPEN(filename)
        varid = NCDF_VARID(id,varname)
        varstruct = NCDF_VARINQ(id,varid)
        IF i eq 0 and j eq 0 THEN BEGIN
            FOR k=0,FIX(varstruct.ndims)-1 DO BEGIN
                NCDF_DIMINQ,id,FIX(varstruct.dim(k)),dimname,dimsize
                CASE k OF
                    0 : BEGIN
                        lonid = NCDF_VARID(id,dimname)
                        NCDF_VARGET,id,lonid,longitudes
                        start_lon = MIN([NEAREST(longitudes,box(1)),$
                                         NEAREST(longitudes,box(4))])
                        stop_lon = MAX([NEAREST(longitudes,box(4)),$
                                        NEAREST(longitudes,box(1))])
                        n_lon = stop_lon-start_lon+1
                        longitudes = longitudes(start_lon:stop_lon)
                    END
                    1 : BEGIN
                        latid = NCDF_VARID(id,dimname)
                        NCDF_VARGET,id,latid,latitudes
                        start_lat = MIN([NEAREST(latitudes,box(0)),$
                                         NEAREST(latitudes,box(3))])
                        stop_lat = MAX([NEAREST(latitudes,box(0)),$
                                        NEAREST(latitudes,box(3))])
                        n_lat = stop_lat-start_lat+1
                        latitudes = latitudes(start_lat:stop_lat)
                    END
                    2 : BEGIN
                        lid = NCDF_VARID(id,dimname)
                        NCDF_VARGET,id,lid,levels
                        n_levels = dimsize             
                        start_lev = MIN([NEAREST(levels,box(2)),$
                                         NEAREST(levels,box(5))])
                        stop_lev = MAX([NEAREST(levels,box(2)),$
                                        NEAREST(levels,box(5))])
                        n_lev = stop_lev-start_lev+1
                        levels = levels(start_lev:stop_lev)
                    END
                    ELSE : BEGIN
                        temp = NCDF_VARID(id,dimname)
                    END
                ENDCASE
            ENDFOR
            variable = fltarr(n_lon,n_lat,n_lev,n_days,n_runs)
        ENDIF
        print,'Now reading data from run '+run_ids(i)+' month '+month_abbr(month)
        NCDF_VARGET,id,varid,temp_var,count=[n_lon,n_lat,n_lev,count],$
          offset=[start_lon,start_lat,start_lev,offset]
;        NCDF_VARGET,id,varid,temp_var,count=[n_lon,n_lat,n_lev,MIN([30,n_days-j*30])],$
;          offset=[start_lon,start_lat,start_lev,0]
	NCDF_CLOSE,id
;        print,MIN([30,n_days-j*30)
        varstop = varstart+count-1
        variable(*,*,*,varstart:varstop,i) = temp_var
        varstart = varstop+1
;        print,varstart,varstop
;        print,'max=',MAX(temp_var*60.*60.*24.),'min=',MIN(temp_var*60.*60.*24.)       
    ENDFOR
ENDFOR

 IF Keyword_Set(ens_avg) and not Keyword_Set(time_avg) THEN BEGIN
     print,'Doing ensemble mean'
     variable_avg = fltarr(n_lon,n_lat,n_lev,n_days)
     FOR i=0,n_lon-1 DO BEGIN
         FOR j=0,n_lat-1 DO BEGIN
             FOR k=0,n_lev-1 DO BEGIN
                 FOR m=0,n_days-1 DO BEGIN
                     variable_avg(i,j,k,m) = MEAN(variable(i,j,k,m,*))
                 ENDFOR
             ENDFOR
         ENDFOR
     ENDFOR
     variable = variable_avg
 ENDIF ELSE IF Keyword_Set(time_avg) and not Keyword_Set(ens_avg) THEN BEGIN
     print,'Doing time mean'
     variable_avg = fltarr(n_lon,n_lat,n_lev,n_runs)
     FOR i=0,n_lon-1 DO BEGIN
         FOR j=0,n_lat-1 DO BEGIN
             FOR k=0,n_lev-1 DO BEGIN
                 FOR m=0,n_runs-1 DO BEGIN
                     variable_avg(i,j,k,m) = MEAN(variable(i,j,k,*,m))
                 ENDFOR
             ENDFOR
         ENDFOR
     ENDFOR
     variable=variable_avg
 ENDIF ELSE IF Keyword_Set(time_avg) and Keyword_Set(ens_avg) THEN BEGIN
     print,'Doing both means'
     variable_avg = fltarr(n_lon,n_lat,n_lev,n_days)
     variable_avg2 = fltarr(n_lon,n_lat,n_lev)
     FOR i=0,n_lon-1 DO BEGIN
         FOR j=0,n_lat-1 DO BEGIN
             FOR k=0,n_lev-1 DO BEGIN
                 ;FOR m=0,n_days-1 DO BEGIN
                 ;    variable_avg(i,j,k,m) = MEAN(variable(i,j,k,m,*))
                 ;ENDFOR
                 variable_avg2(i,j,k) = MEAN(variable(i,j,k,*,*))
             ENDFOR
         ENDFOR
     ENDFOR
     variable=variable_avg2
 ENDIF

END
