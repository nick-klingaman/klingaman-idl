PRO hadgem3kpp_ocnTcorr_make_smooth_correction,infile,outfile,in_varname,in_zname,out_varname,n_days_per_year,mean_length

longitude=OPEN_AND_EXTRACT(infile,'longitude')
latitude=OPEN_AND_EXTRACT(infile,'latitude')
z=OPEN_AND_EXTRACT(infile,in_zname)
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)
n_z=N_ELEMENTS(z)

;sfcorr_in=OPEN_AND_EXTRACT(infile,in_varname)
;n_days=N_ELEMENTS(REFORM(sfcorr_in(0,0,0,*)))
n_days=360
n_years=n_days/n_days_per_year
sfcorr_out=fltarr(n_lon,n_lat,n_z,n_days_per_year)

;IF TOTAL(where(ABS(sfcorr_in) ge 1e10)) ge 0 THEN $
;   sfcorr_in[where(ABS(sfcorr_in ge 1e10))]=!Values.F_NaN

FOR y=0,n_years-1 DO BEGIN
   print,'Running for year ',y+1,' of ',n_years
   FOR i=0,n_lon-1 DO BEGIN
      print,'Running for longitude band ',i+1,' of ',n_lon
      sfcorr_in=OPEN_AND_EXTRACT(infile,in_varname,offset=[i,0,0,0],count=[1,n_lat,n_z,n_days])
      FOR j=0,n_lat-1 DO BEGIN
;         lsm_test=REFORM(OPEN_AND_EXTRACT(infile,in_varname,offset=[i,j,0,0],count=[1,1,1,1]))
;         IF lsm_test lt 1e19 THEN BEGIN
;            sfcorr_in=OPEN_AND_EXTRACT(infile,in_varname,offset=[i,j,0,y*n_days_per_year],count=[1,1,n_z,n_days_per_year])
         IF sfcorr_in(0,j,0,0) lt 1e19 THEN BEGIN
            FOR k=0,n_z-1 DO BEGIN
               temp=REFORM([REFORM(sfcorr_in(0,j,k,*)),REFORM(sfcorr_in(0,j,k,*))])
               IF TOTAL(where(ABS(temp) ge 1000)) ge 0 THEN $
                  temp(where(ABS(temp) ge 1000))=!Values.F_NaN
               temp=SMOOTH(temp,mean_length,/NaN)
               sfcorr_out(i,j,k,*)=[temp(n_days_per_year:n_days_per_year+mean_length/2-1),$
                                    temp(mean_length/2:n_days_per_year-1)];/FLOAT(n_years)+$
                                ;sfcorr_out(i,j,k,*)
               IF TOTAL(where(sfcorr_out(i,j,k,*) le -10000)) ge 0 THEN $
                  STOP
            ENDFOR
         ENDIF ELSE $
            sfcorr_out(i,j,*,*)=1e20
      ENDFOR
   ENDFOR
ENDFOR

id=NCDF_CREATE(outfile,/CLOBBER)
dimids=intarr(4)
dimids(0)=NCDF_DIMDEF(id,'longitude',n_lon)
dimids(1)=NCDF_DIMDEF(id,'latitude',n_lat)
dimids(2)=NCDF_DIMDEF(id,'z',n_z)
dimids(3)=NCDF_DIMDEF(id,'t',n_days_per_year)
varids=intarr(5)
varids(0)=NCDF_VARDEF(id,'longitude',dimids(0))
varids(1)=NCDF_VARDEF(id,'latitude',dimids(1))
varids(2)=NCDF_VARDEF(id,'z',dimids(2))
varids(3)=NCDF_VARDEF(id,'t',dimids(3))
varids(4)=NCDF_VARDEF(id,out_varname,[dimids(0),dimids(1),dimids(2),dimids(3)])
;NCDF_CONTROL,id,/ENDEF
NCDF_ATTPUT,id,varids(4),'missing_value',1e20
NCDF_CONTROL,id,/ENDEF
NCDF_VARPUT,id,varids(0),longitude
NCDF_VARPUT,id,varids(1),latitude
NCDF_VARPUT,id,varids(2),z
NCDF_VARPUT,id,varids(3),indgen(n_days_per_year)+0.5
IF in_varname eq 'scorr' THEN BEGIN
   NCDF_VARPUT,id,varids(4),sfcorr_out
ENDIF ELSE IF in_varname eq 'fcorr_z' THEN BEGIN
   NCDF_VARPUT,id,varids(4),sfcorr_out ;/1024./4000.
ENDIF ELSE $
   NCDF_VARPUT,id,varids(4),sfcorr_out
NCDF_CLOSE,id

STOP
END

