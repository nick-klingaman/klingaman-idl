PRO aus_rainfall_extremes_convective_fraction

ncep_dir='/home/ss901165/datasets_mango/NCEP_REANALYSIS'
ncep_percentiles_input_file=ncep_dir+'/precipitation/ncep-ncar_reanalysis.may-apr_dmeans.1958-2000.precip_percentiles.t62_gauss.nc'
ncep_precipitation_input_file=ncep_dir+'/precipitation/ncep-ncar_reanalysis.may-apr_dmeans.1958-2000.precip.t62_gauss.nc'
ncep_convective_input_file=ncep_dir+'/convective_precipitation/ncep-ncar_reanalysis.may-apr_dmeans.1958-2000.conv_precip.t62_gauss.nc'
ncep_mask_input_file=ncep_dir+'/lsmask.t62_gauss.nc'

era40_dir='/home/ss901165/datasets/ERA40'
era40_percentiles_input_file=era40_dir+'/PRECIP/era40.may-apr_dmeans.1958-2000.precip_percentiles.aus_domain.nc'
era40_precipitation_input_file=era40_dir+'/PRECIP/era40.may-apr_dmeans.1958-2001.precip.aus_domain.nc'
era40_convective_input_file=era40_dir+'/CP/era40.may-apr_dmeans.1958-2000.convective_precip.aus_domain.nc'
era40_mask_input_file=era40_dir+'/era40_lsm.nc'

box=[-45,112,-10,154]
percentile_to_plot=99

n_datasets=2
dataset_names=['NCEP-NCAR','ERA-40']

n_years=43
n_days=365

FOR i=0,n_datasets-1 DO BEGIN
   CASE i OF 
      0 : BEGIN
         percentiles_input_file=ncep_percentiles_input_file
         precipitation_input_file=ncep_precipitation_input_file
         convective_input_file=ncep_convective_input_file
         mask_input_file=ncep_mask_input_file
         precip_name='prate'
         conv_name='cprat'
         mask_name='lsmask'
         mask_longitude_name='lon'
         mask_latitude_name='lat'
         precip_multiplier_first=1E-7
         precip_multiplier_second=86400.
         precip_add_offset=0.0032765         
         conv_multiplier_first=1E-7
         conv_add_offset=0.0031765
         conv_multiplier_second=86400.
         
      END
      1 : BEGIN
         percentiles_input_file=era40_percentiles_input_file
         precipitation_input_file=era40_precipitation_input_file
         convective_input_file=era40_convective_input_file
         mask_input_file=era40_mask_input_file
         precip_name='precip'
         conv_name='CP'
         mask_name='LSM'
         mask_longitude_name='longitude'
         mask_latitude_name='latitude'
         precip_multiplier_first=1
         precip_multiplier_second=1000.
         precip_add_offset=0
         conv_multiplier_first=1
         conv_multiplier_second=1000.
         conv_add_offset=0
      END
   ENDCASE
     
   percentiles_longitude=OPEN_AND_EXTRACT(percentiles_input_file,'longitude')
   percentiles_latitude=OPEN_AND_EXTRACT(percentiles_input_file,'latitude')
   DEFINE_BOUNDARIES,box,percentiles_latitude,percentiles_longitude,percentiles_box_tx,/LIMIT
   percentiles_nlat=N_ELEMENTS(percentiles_latitude)
   percentiles_nlon=N_ELEMENTS(percentiles_longitude)

   precipitation_longitude=OPEN_AND_EXTRACT(precipitation_input_file,'longitude')
   precipitation_latitude=OPEN_AND_EXTRACT(precipitation_input_file,'latitude')
   DEFINE_BOUNDARIES,box,precipitation_latitude,precipitation_longitude,precipitation_box_tx,/LIMIT
   precipitation_nlat=N_ELEMENTS(precipitation_latitude)
   precipitation_nlon=N_ELEMENTS(precipitation_longitude)

   mask_longitude=OPEN_AND_EXTRACT(mask_input_file,mask_longitude_name)
   mask_latitude=OPEN_AND_EXTRACT(mask_input_file,mask_latitude_name)
   DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
   mask_nlat=N_ELEMENTS(mask_latitude)
   mask_nlon=N_ELEMENTS(mask_longitude)

   mask=OPEN_AND_EXTRACT(mask_input_file,mask_name,$
                         offset=[mask_box_tx(1),mask_box_tx(0)],$
                         count=[mask_nlon,mask_nlat])
   
   convective_longitude=OPEN_AND_EXTRACT(convective_input_file,'longitude')
   convective_latitude=OPEN_AND_EXTRACT(convective_input_file,'latitude')
   DEFINE_BOUNDARIES,box,convective_latitude,convective_longitude,convective_box_tx,/LIMIT
   convective_nlat=N_ELEMENTS(convective_latitude)
   convective_nlon=N_ELEMENTS(convective_longitude)
   
   print,'Reading percentiles ...'
   percentiles=OPEN_AND_EXTRACT(percentiles_input_file,'percentile_of_daily_rain',$
                                offset=[percentiles_box_tx(1),percentiles_box_tx(0),0,0],$
                                count=[percentiles_nlon,percentiles_nlat,n_years,n_days])
   
   print,'Reading precipitation ...'
   precipitation=(OPEN_AND_EXTRACT(precipitation_input_file,precip_name,$
                                  offset=[precipitation_box_tx(1),precipitation_box_tx(0),0,0],$
                                  count=[precipitation_nlon,precipitation_nlat,n_days,n_years])$
                  *precip_multiplier_first+precip_add_offset)*precip_multiplier_second
   
   print,'Reading convective precipitation ...'
   convective_precipitation=(OPEN_AND_EXTRACT(convective_input_file,conv_name,$
                                             offset=[convective_box_tx(1),convective_box_tx(0),0,0],$
                                             count=[convective_nlon,convective_nlat,n_days,n_years])$
                             *conv_multiplier_first+conv_add_offset)*conv_multiplier_second
   
   mean_convective_fraction=fltarr(convective_nlon,convective_nlat)

   FOR j=0,convective_nlon-1 DO BEGIN
      FOR k=0,convective_nlat-1 DO BEGIN
         IF mask(j,k) ne 0 THEN BEGIN
                                ; This is a land point
                                ; Place all variables into a timeseries of length n_years*n_days
            thispt_percentiles_ts=fltarr(n_years*n_days)
            thispt_precipitation_ts=fltarr(n_years*n_days)
            thispt_convective_ts=fltarr(n_years*n_days)
            FOR m=0,n_years-1 DO BEGIN
               thispt_percentiles_ts(m*n_days:(m+1)*n_days-1)=REFORM(percentiles(j,k,m,*))
               thispt_precipitation_ts(m*n_days:(m+1)*n_days-1)=REFORM(precipitation(j,k,*,m))
               thispt_convective_ts(m*n_days:(m+1)*n_days-1)=REFORM(convective_precipitation(j,k,*,m))
            ENDFOR
                                ; A vector of all events containing the total precipitation
            all_events_precipitation=thispt_precipitation_ts[where(thispt_percentiles_ts ge percentile_to_plot and $
                                                                   thispt_percentiles_ts lt 2e19)]
                                ; A vector of all events containing the convective precipitation
            all_events_convective_precipitation=thispt_convective_ts[where(thispt_percentiles_ts ge percentile_to_plot and $
                                                                           thispt_percentiles_ts lt 2e19)]
                                ; Compute mean convective fraction here and store in mean_convective_fraction
            convective_fraction=all_events_convective_precipitation/all_events_precipitation
            mean_convective_fraction(j,k)=MEAN(convective_fraction)
            ;IF j eq 1 and k eq 7 then print,convective_fraction
            print,N_ELEMENTS(convective_fraction),N_ELEMENTS(where(thispt_percentiles_ts lt 2e19)),mean_convective_fraction(j,k)
            IF where(convective_fraction gt 1.01) ne -1 THEN $
               STOP
         ENDIF ELSE $
                                ; This is an ocean point
            mean_convective_fraction(j,k)=!Values.F_NaN
      ENDFOR
   ENDFOR
   print,'-------------------------------------------'
; End of dataset loop
ENDFOR

STOP
END
