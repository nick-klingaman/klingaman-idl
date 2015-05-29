PRO qld_wet_days_sequences_terciles

; Count the number of sequences of consecutive wet days
; above a given threshold.  Stratify based on terciles
; of seasonal-mean rainfall.

; File containing timeseries of Nov-Apr mean rainfall
silo_seasmean_infile='/home/ss901165/datasets_mango/SILO/one_quarter/SILO_precip.nov-apr_smeans.1900-2008.0.25x0.25.nc'
; File containing timeseries of Nov-Apr daily rainfall
silo_dailymean_infile='/home/ss901165/datasets_mango/SILO/one_quarter/SILO_precip.nov-apr_dmeans.1900-2008.0.25x0.25.nc'

silo_nyears=107
silo_ndays_per_year=181

; Thresholds for rainfall
rain_thresholds=[1,5,10,15,20,25,30,35,40,45,50]
time_thresholds=[2,3,4,5,6,7,8,9,10]
n_rain_thresholds=N_ELEMENTS(rain_thresholds)
n_time_thresholds=N_ELEMENTS(time_thresholds)

; Box to consider
box=[-10,140,-30,154]
;box=[-15,145,-20,150]

; Read latitude and longitude, get number of points
silo_longitude=OPEN_AND_EXTRACT(silo_seasmean_infile,'longitude')
silo_latitude=OPEN_AND_EXTRACT(silo_seasmean_infile,'latitude')
DEFINE_BOUNDARIES,box,silo_latitude,silo_longitude,silo_box_tx,/LIMIT
silo_nlon=N_ELEMENTS(silo_longitude)
silo_nlat=N_ELEMENTS(silo_latitude)

silo_nsequences_wet=fltarr(silo_nlon,silo_nlat,n_rain_thresholds,n_time_thresholds)
silo_nsequences_normal=fltarr(silo_nlon,silo_nlat,n_rain_thresholds,n_time_thresholds)
silo_nsequences_dry=fltarr(silo_nlon,silo_nlat,n_rain_thresholds,n_time_thresholds)

FOR j=0,silo_nlon-1 DO BEGIN
   print,'-> Calculating for j = '+STRTRIM(STRING(j+1),1)+' of '+STRTRIM(STRING(silo_nlon),1)
   FOR k=0,silo_nlat-1 DO BEGIN
      print,'---> Calculating for k = '+STRTRIM(STRING(k+1),1)+' of '+STRTRIM(STRING(silo_nlat),1)
                                ; Read seasonal mean data
      silo_seasmeans=REFORM(OPEN_AND_EXTRACT(silo_seasmean_infile,'rain',$
                                             offset=[silo_box_tx(1)+j,silo_box_tx(0)+k,0],$
                                             count=[1,1,silo_nyears]))
      ; Sort seasonal mean data
      sorted_indices=SORT(silo_seasmeans)
      silo_seasmeans_sorted=silo_seasmeans[sorted_indices]
      upper_tercile_indices=sorted_indices(silo_nyears*2/3+1:silo_nyears-1)
      lower_tercile_indices=sorted_indices(0:silo_nyears/3-1)
      middle_tercile_indices=sorted_indices(silo_nyears/3:silo_nyears*2/3)
      
      IF silo_seasmeans(0) lt 1E20 THEN BEGIN
         print,silo_seasmeans(0)
         FOR m=0,2 DO BEGIN
            CASE m OF
               0: BEGIN
                  indices=upper_tercile_indices
               END
               1: BEGIN
                  indices=middle_tercile_indices
               END
               2: BEGIN
                  indices=lower_tercile_indices
               END
            ENDCASE
            n_indices=N_ELEMENTS(indices)
            n_sequences=intarr(n_rain_thresholds,n_time_thresholds)
            FOR n=0,n_indices-1 DO BEGIN
               silo_thisyear_precip=REFORM(OPEN_AND_EXTRACT(silo_dailymean_infile,'rain',$
                                                            offset=[silo_box_tx(1)+j,silo_box_tx(0)+k,0,indices(n)],$
                                                            count=[1,1,silo_ndays_per_year,1]))
               FOR p=0,n_rain_thresholds-1 DO BEGIN
                  silo_thisyear_precip_masked=fltarr(silo_ndays_per_year)
                  IF TOTAL(where(silo_thisyear_precip ge rain_thresholds(p))) ne -1 THEN $
                     silo_thisyear_precip_masked[where(silo_thisyear_precip ge rain_thresholds(p))]=1.0
                  silo_thisyear_precip_masked[where(silo_thisyear_precip lt rain_thresholds(p))]=0.0
                  FOR r=0,n_time_thresholds-1 DO BEGIN
                     consecutive=0
                     FOR s=0,silo_ndays_per_year-1 DO BEGIN
                        IF silo_thisyear_precip_masked(s) eq 1.0 THEN BEGIN
                           consecutive=consecutive+1
                        ENDIF ELSE $
                           consecutive=0
                        IF consecutive eq time_thresholds(r) THEN $
                           n_sequences(p,r)=n_sequences(p,r)+1
                     ENDFOR
                  ENDFOR
               ENDFOR
            ENDFOR
            CASE m OF
               0: BEGIN
                  silo_nsequences_wet(j,k,*,*)=n_sequences(*,*)
               END
               1: BEGIN
                  silo_nsequences_normal(j,k,*,*)=n_sequences(*,*)
               END
               2: BEGIN
                  silo_nsequences_dry(j,k,*,*)=n_sequences(*,*)
               END
            ENDCASE
         ENDFOR
      ENDIF ELSE BEGIN
         silo_nsequences_wet(j,k,*,*)=!Values.F_NaN
         silo_nsequences_normal(j,k,*,*)=!Values.F_NaN
         silo_nsequences_dry(j,k,*,*)=!Values.F_NaN
      ENDELSE
   ENDFOR
ENDFOR

outfile='/home/ss901165/datasets_mango/SILO/one_quarter/SILO.nov-apr_dmeans.1900-2008.wet_day_sequences.by_tercile.0.25x0.25.nc'
id=NCDF_CREATE(outfile,/CLOBBER)
dimids=intarr(4)
dimids(0)=NCDF_DIMDEF(id,'longitude',silo_nlon)
dimids(1)=NCDF_DIMDEF(id,'latitude',silo_nlat)
dimids(2)=NCDF_DIMDEF(id,'rain_threshold',n_rain_thresholds)
dimids(3)=NCDF_DIMDEF(id,'time_threshold',n_time_thresholds)
varids=intarr(7)
varids(0)=NCDF_VARDEF(id,'longitude',[dimids(0)])
varids(1)=NCDF_VARDEF(id,'latitude',[dimids(1)])
varids(2)=NCDF_VARDEF(id,'rain_threshold',[dimids(2)])
varids(3)=NCDF_VARDEF(id,'time_threshold',[dimids(3)])
varids(4)=NCDF_VARDEF(id,'sequences_upper_years',[dimids(0),dimids(1),dimids(2),dimids(3)])
varids(5)=NCDF_VARDEF(id,'sequences_middle_years',[dimids(0),dimids(1),dimids(2),dimids(3)])
varids(6)=NCDF_VARDEF(id,'sequences_lower_years',[dimids(0),dimids(1),dimids(2),dimids(3)])
NCDF_CONTROL,id,/ENDEF
NCDF_VARPUT,id,varids(0),silo_longitude
NCDF_VARPUT,id,varids(1),silo_latitude
NCDF_VARPUT,id,varids(2),rain_thresholds
NCDF_VARPUT,id,varids(3),time_thresholds
NCDF_VARPUT,id,varids(4),silo_nsequences_wet/FLOAT(N_ELEMENTS(upper_tercile_indices))
NCDF_VARPUT,id,varids(5),silo_nsequences_normal/FLOAT(N_ELEMENTS(middle_tercile_indices))
NCDF_VARPUT,id,varids(6),silo_nsequences_dry/FLOAT(N_ELEMENTS(lower_tercile_indices))
NCDF_CLOSE,id

STOP

END



