PRO process_ibtracs_as_track

ibtracs_nfiles=2
start_year=1950
stop_year=2008
n_years=stop_year-start_year+2  ; Need to add two to account for fact that seasons
                                ; start the year before the year given
                                ; in the IBTRACS file.
gregorian_start_year=1858
gregorian_start_month=11
gregorian_start_date=17

; Give a statistics file from TRACK from which to obtain latitude and
; longitude grids
track_input='/home/ss901165/higem_qccce/es_control_eafeb/stat_trs_scl.HiGEM150_vort_wind_mslp.attain20_1.nc'
grid_longitude=OPEN_AND_EXTRACT(track_input,'long')
n_lon=N_ELEMENTS(grid_longitude)
grid_latitude=OPEN_AND_EXTRACT(track_input,'lat')
n_lat=N_ELEMENTS(grid_latitude)

track_density=fltarr(n_lon,n_lat,12)
track_density_by_year=fltarr(n_lon,n_lat,n_years) ; Number of storms per season (July-June)
track_density_by_year_and_month=fltarr(n_lon,n_lat,12,n_years)
track_density(*,*,*)=0
track_density_by_year(*,*,*)=0
track_density_by_year_and_month(*,*,*,*)=0
genesis_density_by_month=fltarr(n_lon,n_lat,12)
genesis_density_by_year=fltarr(n_lon,n_lat,n_years)
lysis_density_by_month=fltarr(n_lon,n_lat,12)
lysis_density_by_year=fltarr(n_lon,n_lat,n_years)

; Calculate Julian distance to first of January of the next year.
julian_start_date=GREGORIAN_TO_JULIAN(gregorian_start_date,$
                                      gregorian_start_month,$
                                      gregorian_start_year)
julian_offset=DAYS_SINCE([julian_start_date,gregorian_start_year],$
                         [1,gregorian_start_year+1])

FOR n=0,ibtracs_nfiles-1 DO BEGIN
   CASE n OF 
      0 : BEGIN
         ibtracs_infile='/home/ss901165/datasets/IBTRACS/Basin.SP.ibtracs.v02r01.nc'
         
      END
      1 : BEGIN
         ibtracs_infile='/home/ss901165/datasets/IBTRACS/Basin.SI.ibtracs.v02r01.nc'
      END
   ENDCASE
   year=OPEN_AND_EXTRACT(ibtracs_infile,'season')
   n_storms=N_ELEMENTS(year)
   latitude=OPEN_AND_EXTRACT(ibtracs_infile,'lat')/100.
   longitude=OPEN_AND_EXTRACT(ibtracs_infile,'lon')/100.
   n_record=N_ELEMENTS(latitude(*,0))

   temp=where(year eq start_year)
   start_storm=temp(0)
   grid_flag=fltarr(n_lon,n_lat)
   FOR i=start_storm,n_storms-1 DO BEGIN
;FOR i=start_storm,start_storm+20 DO BEGIN
;FOR i=443,445 DO BEGIN
      print,'Processing storm = '+STRTRIM(STRING(i+1),1)
      dates=OPEN_AND_EXTRACT(ibtracs_infile,'time',$
                             offset=[0,i],count=[n_record,1])
      n_dates=N_ELEMENTS(where(dates le 1E10))
      grid_flag(*,*)=0
                                ; Note that for the Southern
                                ; Hemisphere, "seasons" start on July
                                ; 1 of the previous year in the
                                ; IBTRACS file.
      ndays_in_year=DAYS_IN_YEAR(year(i))
      july1_julian=GREGORIAN_TO_JULIAN(1,7,year(i)-1)
      today_offset_wholeyears=DAYS_SINCE([julian_start_date,gregorian_start_year],$
                                         [july1_julian,year(i)-1])
      print,'Year = ',year(i),' days in year = ',ndays_in_year
      FOR j=0,n_dates-1 DO BEGIN
                                ;print,'Date = ',dates(j),' j = ',j,' n_dates = ',n_dates
                                ; Deals with storms that cross season
                                ; boundaries: persist storm in current
                                ; season until end of life.
         IF dates(j)-today_offset_wholeyears ge ndays_in_year THEN $
            dates(j)=today_offset_wholeyears+ndays_in_year-0.1
                                ; For Southern Hemisphere, "years"
                                ; begin on 1 July the previous year                               
         IF ndays_in_year eq 366 THEN BEGIN
            today_gregorian=JULIAN_TO_GREGORIAN(dates(j)-today_offset_wholeyears,start_month=7,/LEAPYEAR)
         ENDIF ELSE $
            today_gregorian=JULIAN_TO_GREGORIAN(dates(j)-today_offset_wholeyears,start_month=7)
         IF (today_gregorian(1) lt 7) THEN BEGIN
            today_calendar_year=year(i)-1
            today_season_month=FLOOR(today_gregorian(1))
            today_gregorian(1)=today_gregorian(1)+6 ; Convert to January-December calendar
         ENDIF ELSE BEGIN
            today_calendar_year=year(i)
            today_season_month=FLOOR(today_gregorian(1))
            today_gregorian(1)=today_gregorian(1)-6 ; Convert to January-December calendar
         ENDELSE
         IF longitude(j,i) le 0 THEN $
            longitude(j,i)=longitude(j,i)+360.
                                ;print,' Month = ',today_gregorian(1),' lon = ',longitude(j,i),' lat = ',latitude(j,i)
                                ; Assign storm's position to
                                ; grid boxes, based on circle with
                                ; five degree radius.
         FOR k=0,n_lon-1 DO BEGIN
            FOR m=0,n_lat-1 DO BEGIN
               distance=SQRT((grid_longitude(k)-longitude(j,i))^2+$
                             (grid_latitude(m)-latitude(j,i))^2)
                                ; IF distance le 5 and grid_flag(k,m)
                                ; ne 1 THEN BEGIN
               IF distance le 5 and grid_flag(k,m) ne 1 THEN BEGIN
;               print,'Found grid point ',grid_longitude(k),grid_latitude(m),$
;                     ' for storm centre ',longitude(j,i),latitude(j,i)
                  month_index=FLOOR(today_gregorian(1))-1
                  track_density(k,m,month_index)=$
                     track_density(k,m,month_index)+1
                                ; Note it is necessary to add one to
                                ; the year here, because the season
                                ; begins the year *before* the year
                                ; given in the IBTRACS netCDF file.
                  track_density_by_year(k,m,today_calendar_year-start_year+1)=$
                     track_density_by_year(k,m,today_calendar_year-start_year+1)+1
                  track_density_by_year_and_month(k,m,month_index,today_calendar_year-start_year+1)=$
                     track_density_by_year_and_month(k,m,month_index,today_calendar_year-start_year+1)+1                  
                  IF (j eq 0) THEN BEGIN
                     genesis_density_by_month(k,m,month_index)=$
                        genesis_density_by_month(k,m,month_index)+1
                     genesis_density_by_year(k,m,today_calendar_year-start_year+1)=$
                        genesis_density_by_year(k,m,today_calendar_year-start_year+1)+1
                                ;print,'Found genesis point: ',longitude(j,i),' ',latitude(j,i)
                  ENDIF
                  grid_flag(k,m)=1               
               ENDIF
               IF distance le 5 and j eq n_dates-1 THEN BEGIN
                  month_index=FLOOR(today_gregorian(1))-1
                  lysis_density_by_month(k,m,month_index)=$
                     lysis_density_by_month(k,m,month_index)+1
                  lysis_density_by_year(k,m,today_calendar_year-start_year+1)=$
                     lysis_density_by_year(k,m,today_calendar_year-start_year+1)+1
                                ;print,'Found lysis point ',longitude(j,i),' ',latitude(j,i)
               ENDIF
            ENDFOR
         ENDFOR
      ENDFOR
   ENDFOR
ENDFOR
FOR i=0,n_lon-1 DO BEGIN
   FOR j=0,n_lat-1 DO BEGIN
      FOR k=0,11 DO BEGIN
         track_density(i,j,k)=track_density(i,j,k)/FLOAT(n_years)
         lysis_density_by_month(i,j,k)=lysis_density_by_month(i,j,k)/FLOAT(n_years)
         genesis_density_by_month(i,j,k)=genesis_density_by_month(i,j,k)/FLOAT(n_years)
      ENDFOR
   ENDFOR
ENDFOR

output_netcdf_file='/home/ss901165/datasets/IBTRACS/ibtracs_southern_hemisphere.jan-dec_mmean_densities.'+STRTRIM(STRING(start_year),1)+'-'+STRTRIM(STRING(stop_year),1)+'.as_track.nc'
out_id=NCDF_CREATE(output_netcdf_file,/CLOBBER)
out_dimids=intarr(4)
out_varids=intarr(17)
out_dimids(0)=NCDF_DIMDEF(out_id,'longitude',n_lon)
out_dimids(1)=NCDF_DIMDEF(out_id,'latitude',n_lat)
out_dimids(2)=NCDF_DIMDEF(out_id,'month',12)
out_dimids(3)=NCDF_DIMDEF(out_id,'year',n_years)
out_varids(0)=NCDF_VARDEF(out_id,'longitude',[out_dimids(0)])
out_varids(1)=NCDF_VARDEF(out_id,'latitude',[out_dimids(1)])
out_varids(2)=NCDF_VARDEF(out_id,'month',[out_dimids(2)])
out_varids(3)=NCDF_VARDEF(out_id,'year',[out_dimids(3)])
out_varids(4)=NCDF_VARDEF(out_id,'avg_monthly_track_density',[out_dimids(0),out_dimids(1)])
out_varids(5)=NCDF_VARDEF(out_id,'clim_monthly_track_density',[out_dimids(0),out_dimids(1),out_dimids(2)])
out_varids(6)=NCDF_VARDEF(out_id,'avg_yearly_track_density',[out_dimids(0),out_dimids(1)])
out_varids(7)=NCDF_VARDEF(out_id,'monthly_track_density',[out_dimids(0),out_dimids(1),out_dimids(2),out_dimids(3)])
out_varids(8)=NCDF_VARDEF(out_id,'yearly_track_density',[out_dimids(0),out_dimids(1),out_dimids(3)])
out_varids(9)=NCDF_VARDEF(out_id,'avg_monthly_genesis_density',[out_dimids(0),out_dimids(1)])
out_varids(10)=NCDF_VARDEF(out_id,'clim_monthly_genesis_density',[out_dimids(0),out_dimids(1),out_dimids(2)])
out_varids(11)=NCDF_VARDEF(out_id,'avg_yearly_genesis_density',[out_dimids(0),out_dimids(1)])
out_varids(12)=NCDF_VARDEF(out_id,'yearly_genesis_density',[out_dimids(0),out_dimids(1),out_dimids(3)])
out_varids(13)=NCDF_VARDEF(out_id,'avg_monthly_lysis_density',[out_dimids(0),out_dimids(1)])
out_varids(14)=NCDF_VARDEF(out_id,'clim_monthly_lysis_density',[out_dimids(0),out_dimids(1),out_dimids(2)])
out_varids(15)=NCDF_VARDEF(out_id,'avg_yearly_lysis_density',[out_dimids(0),out_dimids(1)])
out_varids(16)=NCDF_VARDEF(out_id,'yearly_lysis_density',[out_dimids(0),out_dimids(1),out_dimids(3)])

NCDF_ATTPUT,out_id,out_varids(0),'units','degrees_east'
NCDF_ATTPUT,out_id,out_varids(1),'units','degrees_north'
NCDF_ATTPUT,out_id,out_varids(2),'units','months'
NCDF_ATTPUT,out_id,out_varids(3),'units','years since '+STRTRIM(STRING(start_year),1)
NCDF_ATTPUT,out_id,out_varids(4),'units','transits per month within 5 degree circle'
NCDF_ATTPUT,out_id,out_varids(5),'units','transits per month within 5 degree circle'
NCDF_ATTPUT,out_id,out_varids(6),'units','transits per year within 5 degree circle'
NCDF_ATTPUT,out_id,out_varids(7),'units','transits per month within 5 degree circle'
NCDF_ATTPUT,out_id,out_varids(8),'units','transits per year within 5 degree circle'
NCDF_ATTPUT,out_id,out_varids(9),'units','events per month within 5 degree circle'
NCDF_ATTPUT,out_id,out_varids(10),'units','events per month within 5 degree circle'
NCDF_ATTPUT,out_id,out_varids(11),'units','events per year within 5 degree circle'
NCDF_ATTPUT,out_id,out_varids(12),'units','events per year within 5 degree circle'
NCDF_ATTPUT,out_id,out_varids(13),'units','events per month within 5 degree circle'
NCDF_ATTPUT,out_id,out_varids(14),'units','events per month within 5 degree circle'
NCDF_ATTPUT,out_id,out_varids(15),'units','events per year within 5 degree circle'
NCDF_ATTPUT,out_id,out_varids(16),'units','events per year within 5 degree circle'

NCDF_CONTROL,out_id,/ENDEF

NCDF_VARPUT,out_id,out_varids(0),grid_longitude
NCDF_VARPUT,out_id,out_varids(1),grid_latitude
NCDF_VARPUT,out_id,out_varids(2),indgen(12)
NCDF_VARPUT,out_id,out_varids(3),indgen(n_years)

mean_track_density=fltarr(n_lon,n_lat)
mean_track_density_by_year=fltarr(n_lon,n_lat)
mean_genesis_density_by_month=fltarr(n_lon,n_lat)
mean_genesis_density_by_year=fltarr(n_lon,n_lat)
mean_lysis_density_by_month=fltarr(n_lon,n_lat)
mean_lysis_density_by_year=fltarr(n_lon,n_lat)
FOR i=0,n_lon-1 DO BEGIN
   FOR j=0,n_lat-1 DO BEGIN
      mean_track_density(i,j)=TOTAL(track_density(i,j,*))/12.
      mean_track_density_by_year(i,j)=TOTAL(track_density_by_year(i,j,*))/FLOAT(n_years)
      mean_genesis_density_by_month(i,j)=TOTAL(genesis_density_by_month(i,j,*))/12.
      mean_genesis_density_by_year(i,j)=TOTAL(genesis_density_by_year(i,j,*))/FLOAT(n_years)
      mean_lysis_density_by_month(i,j)=TOTAL(lysis_density_by_month(i,j,*))/12.
      mean_lysis_density_by_year(i,j)=TOTAL(lysis_density_by_year(i,j,*))/FLOAT(n_years)
   ENDFOR
ENDFOR

NCDF_VARPUT,out_id,out_varids(4),mean_track_density
NCDF_VARPUT,out_id,out_varids(5),track_density
NCDF_VARPUT,out_id,out_varids(6),mean_track_density_by_year
NCDF_VARPUT,out_id,out_varids(7),track_density_by_year_and_month
NCDF_VARPUT,out_id,out_varids(8),track_density_by_year
NCDF_VARPUT,out_id,out_varids(9),mean_genesis_density_by_month
NCDF_VARPUT,out_id,out_varids(10),genesis_density_by_month
NCDF_VARPUT,out_id,out_varids(11),mean_genesis_density_by_year
NCDF_VARPUT,out_id,out_varids(12),genesis_density_by_year
NCDF_VARPUT,out_id,out_varids(13),mean_lysis_density_by_month
NCDF_VARPUT,out_id,out_varids(14),lysis_density_by_month
NCDF_VARPUT,out_id,out_varids(15),mean_lysis_density_by_year
NCDF_VARPUT,out_id,out_varids(16),lysis_density_by_year

STOP

END


