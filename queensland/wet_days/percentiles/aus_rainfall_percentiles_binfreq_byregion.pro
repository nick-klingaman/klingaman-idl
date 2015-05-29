PRO aus_rainfall_percentiles_binfreq_byregion

; Plot a climatological percentile of daily rainfall for Queensland.
  
; Input files containing observed (SILO) rainfall percentiles
silo_t62_input_file='/home/ss901165/datasets_mango/SILO/t62/SILO.may-apr_dmeans.1958-2000.precip_percentiles.t62.nc'
silo_era40_input_file='/home/ss901165/datasets_mango/SILO/era40_resolution/SILO.may-apr_dmeans.1958-2000.precip_percentiles.era40.nc'

box=[-25,140,-10,153]
region_name='northeast'

n_datasets=3
dataset_names=['20th Century Reanalysis','NCEP-NCAR Reanalysis','ERA-40 Reanalysis']
n_days=365
n_years=43

; Set up the PostScript file and define the ranges of the axes

all_colors=strarr(n_datasets)
all_styles=intarr(n_datasets)
FOR i=0,n_datasets-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         rainfall_input_file='/home/ss901165/datasets_mango/20THC_REANALYSIS/precip/20thc_reanalysis.may-apr_dmeans.1958-2000.precip.nc'
         variable_name='PRATE'
         multiplier_first=1
         multiplier_second=86400.
         add_offset=0
         percentiles_input_file=silo_t62_input_file
         color='blue'
      END
      1 : BEGIN
         rainfall_input_file='/home/ss901165/datasets_mango/NCEP_REANALYSIS/precipitation/ncep-ncar_reanalysis.may-apr_dmeans.1958-2001.precip.t62_gauss.nc'         
         variable_name='prate'
         multiplier_first=1E-7
         multiplier_second=86400.
         add_offset=0.0032765
         percentiles_input_file=silo_t62_input_file
         color='purple'
      END
      2 : BEGIN
         rainfall_input_file='/home/ss901165/datasets/ERA40/PRECIP/era40.may-apr_dmeans.1958-2001.precip.aus_domain.nc'         
         variable_name='precip'
         multiplier_first=1
         multiplier_second=1000.
         add_offset=0
         percentiles_input_file=silo_era40_input_file
         color='brown'
      END
   ENDCASE
   all_colors(i)=color   
                                ; Read latitude and longitude from the daily rainfall file
   rainfall_longitude=OPEN_AND_EXTRACT(rainfall_input_file,'longitude')
   rainfall_latitude=OPEN_AND_EXTRACT(rainfall_input_file,'latitude')
   DEFINE_BOUNDARIES,box,rainfall_latitude,rainfall_longitude,rainfall_box_tx,/LIMIT
                                ; Get the number of latitude and
                                ; longitude points in the box
   rainfall_nlon=N_ELEMENTS(rainfall_longitude)
   rainfall_nlat=N_ELEMENTS(rainfall_latitude)
   
                                ; Read latitude and longitude from the rainfall percentiles file
   percentiles_longitude=OPEN_AND_EXTRACT(percentiles_input_file,'longitude')
   percentiles_latitude=OPEN_AND_EXTRACT(percentiles_input_file,'latitude')
   DEFINE_BOUNDARIES,box,percentiles_latitude,percentiles_longitude,percentiles_box_tx,/LIMIT
                                ; Get the number of latitude and longitude points in the box
   percentiles_nlon=N_ELEMENTS(percentiles_longitude)
   percentiles_nlat=N_ELEMENTS(percentiles_latitude)
   
                                ; Read the percentiles available in the file
   percentiles=OPEN_AND_EXTRACT(percentiles_input_file,'percentile')
   n_percentiles=N_ELEMENTS(percentiles)

                                ; Read the climatological percentiles of daily rainfall
   clim_percentile=REFORM(OPEN_AND_EXTRACT(percentiles_input_file,'amount_at_percentile',$
                                           offset=[percentiles_box_tx(1),percentiles_box_tx(0),0],$
                                           count=[percentiles_nlon,percentiles_nlat,n_percentiles]))
   clim_percentile[where(clim_percentile eq 2e20)]=!Values.F_NaN
   
                                ; Read the daily rainfall amounts for all days and all years
   print,'Reading daily rainfall ...'
   reanalysis_daily_rainfall=(OPEN_AND_EXTRACT(rainfall_input_file,variable_name,$
                                              offset=[rainfall_box_tx(1),rainfall_box_tx(0),0,0],$
                                              count=[rainfall_nlon,rainfall_nlat,n_days,n_years])$
                              *multiplier_first+add_offset)*multiplier_second
   print,'... done reading'

   wet_days=fltarr(rainfall_nlon,rainfall_nlat)
   reanalysis_rainfall_binned=fltarr(n_percentiles)
   FOR j=0,rainfall_nlon-1 DO BEGIN
      FOR k=0,rainfall_nlat-1 DO BEGIN
         IF FINITE(clim_percentile(j,k,0)) eq 1 THEN BEGIN
            thispt_daily_rainfall=REFORM(reanalysis_daily_rainfall(j,k,*,*))
            wet_days(j,k)=N_ELEMENTS(where(thispt_daily_rainfall ge 1.))
            FOR m=0,n_percentiles-1 DO BEGIN
               IF m eq 0 THEN BEGIN
                  lower_bound=1
                  upper_bound=clim_percentile(j,k,0)
               ENDIF ELSE IF m eq n_percentiles-1 THEN BEGIN
                  lower_bound=clim_percentile(j,k,n_percentiles-1)
                  upper_bound=10000
               ENDIF ELSE BEGIN
                  lower_bound=clim_percentile(j,k,m)
                  upper_bound=clim_percentile(j,k,m+1)
               ENDELSE
               IF TOTAL(where(thispt_daily_rainfall ge lower_bound and thispt_daily_rainfall lt upper_bound)) ge 0 THEN BEGIN
                  reanalysis_rainfall_binned(m)=reanalysis_rainfall_binned(m)+$
                                                N_ELEMENTS(where(thispt_daily_rainfall ge lower_bound and thispt_daily_rainfall lt upper_bound))
               ENDIF
            ENDFOR
         ENDIF
      ENDFOR
   ENDFOR            
   reanalysis_rainfall_binned=reanalysis_rainfall_binned/(TOTAL(wet_days))
   print,reanalysis_rainfall_binned

   ; Draw the plot here using HIST or GPLOT

ENDFOR

; PSCLOSE here

STOP
END



