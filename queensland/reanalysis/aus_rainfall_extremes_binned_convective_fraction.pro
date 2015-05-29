PRO aus_rainfall_extremes_binned_convective_fraction_ncep

;Input files
ncep_dir='/home/ss901165/datasets_mango/NCEP_REANALYSIS'
ncep_percentiles_input_file=ncep_dir+'/precipitation/ncep-ncar_reanalysis.may-apr_dmeans.1958-2000.precip_percentiles.t62_gauss.nc'
ncep_precipitation_input_file=ncep_dir+'/precipitation/ncep-ncar_reanalysis.may-apr_dmeans.1958-2000.precip.t62_gauss.nc'
ncep_convective_input_file=ncep_dir+'/convective_precipitation/ncep-ncar_reanalysis.may-apr_dmeans.1958-2000.conv_precip.t62_gauss.nc'
ncep_mask_input_file=ncep_dir+'/lsmask.t62_gauss.nc'

;era40_dir='/home/ss901165/datasets/ERA40'
;era40_percentiles_input_file=era40_dir+'/PRECIP/era40.may-apr_dmeans.1958-2000.precip_percentiles.aus_domain.nc'
;era40_precipitation_input_file=era40_dir+'/PRECIP/era40.may-apr_dmeans.1958-2001.precip.aus_domain.nc'
;era40_convective_input_file=era40_dir+'/CP/era40.may-apr_dmeans.1958-2000.convective_precip.aus_domain.nc'
;era40_mask_input_file=era40_dir+'/era40_lsm.nc'

box=[-25,140,-10,153]
region_name='northeast'

n_bars=3
bar_names=['90th percentile','95th percentile','99th percentile']
bar_colors=['blue','purple','brown']
n_days=365
n_years=43

convective_fraction_bins=[0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]
n_bins=N_ELEMENTS(convective_fraction_bins)

psfile='/home/ss901165/idl/queensland/reanalysis/aus_rainfall_extremes_binned_convective_fraction.NCEP.'+region_name+'.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=90,MARGIN=3000,SPACE1=100,SPACE2=500,TFONT=6,TCHARSIZE=78,SPACE3=800
GSET,XMIN=0,XMAX=1.0,YMIN=0,YMAX=1.0,TITLE='Probability of the NCEP/NCAR Reanalysis rainfall convective fraction falling within the convective rainfall bins in the '+region_name+' region - 1958-2000' 

all_colors=strarr(n_bars)
all_styles=intarr(n_bars)
FOR i=0,n_bars-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         rainfall_input_file=ncep_precipitation_input_file
         n_members=1
         longitude_name='longitude'
         latitude_name='latitude'
         variable_name='prate'
         conv_name='cprat'
         precip_multiplier_first=1E-7
         precip_multiplier_second=86400.
         precip_add_offset=0.0032765         
         conv_multiplier_first=1E-7
         conv_add_offset=0.0031765
         conv_multiplier_second=86400.
         percentiles_input_file=ncep_percentiles_input_file
         percentile_to_plot=70
         color='blue'
         mask_input_file=ncep_mask_input_file
         mask_longitude_name='lon'
         mask_latitude_name='lat'
         mask_name='lsmask'
      END
      1 : BEGIN
         rainfall_input_file=ncep_precipitation_input_file
         n_members=1
         longitude_name='longitude'
         latitude_name='latitude'
         variable_name='prate'
         conv_name='cprat'
         multiplier_first=1E-7
         multiplier_second=86400.
         add_offset=0.0032765
         percentiles_input_file=ncep_percentiles_input_file
         percentile_to_plot=80
         color='purple'
      END
      2 : BEGIN
         rainfall_input_file=ncep_precipitation_input_file
         n_members=1
         longitude_name='longitude'
         latitude_name='latitude'
         variable_name='prate'
         conv_name='cprat'
         multiplier_first=1E-7
         multiplier_second=86400.
         add_offset=0.0032765
         percentiles_input_file=ncep_percentiles_input_file
         percentile_to_plot=90
         color='brown'
      END
   ENDCASE
   all_colors(i)=color
                                ;Read latitude and longitude
   
   rainfall_longitude=OPEN_AND_EXTRACT(rainfall_input_file,longitude_name)
   rainfall_latitude=OPEN_AND_EXTRACT(rainfall_input_file,latitude_name)
   DEFINE_BOUNDARIES,box,rainfall_latitude,rainfall_longitude,rainfall_box_tx,/LIMIT
   
                                ;Number of latitude and longitude points
   
   rainfall_nlon=N_ELEMENTS(rainfall_longitude)
   rainfall_nlat=N_ELEMENTS(rainfall_latitude)
   
                                ;Read latitude and longitude from rainfall percentiles file
   
   percentiles_longitude=OPEN_AND_EXTRACT(percentiles_input_file,'longitude')
   percentiles_latitude=OPEN_AND_EXTRACT(percentiles_input_file,'latitude')
   DEFINE_BOUNDARIES,box,percentiles_latitude,percentiles_longitude,percentiles_box_tx,/LIMIT
   
                                ;Number of latitude and longitude points
   
   percentiles_nlon=N_ELEMENTS(percentiles_longitude)
   percentiles_nlat=N_ELEMENTS(percentiles_latitude)
   
                                ;Read land sea mask
   mask_longitude=OPEN_AND_EXTRACT(mask_input_file,mask_longitude_name)
   mask_latitude=OPEN_AND_EXTRACT(mask_input_file,mask_latitude_name)
   DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
   mask_nlat=N_ELEMENTS(mask_latitude)
   mask_nlon=N_ELEMENTS(mask_longitude)
   
   mask=OPEN_AND_EXTRACT(mask_input_file,mask_name,$
                         offset=[mask_box_tx(1),mask_box_tx(0)],$
                         count=[mask_nlon,mask_nlat])                  
   
                                ; Read in daily percentiles of rainfall                                
   percentiles=OPEN_AND_EXTRACT(percentiles_input_file,'percentile_of_daily_rain',$
                                offset=[percentiles_box_tx(1),percentiles_box_tx(0),0,0],$
                                count=[percentiles_nlon,percentiles_nlat,n_years,n_days])
   
                                ;Read total daily rainfall     
   precipitation=(OPEN_AND_EXTRACT(rainfall_input_file,variable_name,$
                                   offset=[rainfall_box_tx(1),rainfall_box_tx(0),0,0],$
                                   count=[rainfall_nlon,rainfall_nlat,n_days,n_years])$
                  *precip_multiplier_first+precip_add_offset)*precip_multiplier_second                             
   
                                ;Read the convective rainfall data
   wet_days=fltarr(rainfall_nlon,rainfall_nlat)
   convective_rainfall_binned=fltarr(n_bins+1)
   convective_input_file=ncep_convective_input_file
   
   convective_rainfall=(OPEN_AND_EXTRACT(convective_input_file,conv_name,$
                                         offset=[rainfall_box_tx(1),rainfall_box_tx(0),0,0],$
                                         count=[rainfall_nlon,rainfall_nlat,n_days,n_years])$
                        *conv_multiplier_first+conv_add_offset)*conv_multiplier_second
   
                                ; Calculate the convective fraction for all days and all years 
   
   FOR j=0,rainfall_nlon-1 DO BEGIN
      FOR k=0,rainfall_nlat-1 DO BEGIN
         IF mask(j,k) ne 0 THEN BEGIN
                                ; This is a land point
                                ; Place all variables into a timeseries of length n_years*n_days
            thispt_percentiles_ts=fltarr(n_years*n_days)
            thispt_precipitation_ts=fltarr(n_years*n_days)
            thispt_convective_ts=fltarr(n_years*n_days)
            FOR m=0,n_years-1 DO BEGIN
               thispt_percentiles_ts(m*n_days:(m+1)*n_days-1)=REFORM(percentiles(j,k,m,*))
               thispt_precipitation_ts(m*n_days:(m+1)*n_days-1)=REFORM(precipitation(j,k,*,m))
               thispt_convective_ts(m*n_days:(m+1)*n_days-1)=REFORM(convective_rainfall(j,k,*,m))
            ENDFOR
                                ; A vector of all events containing the total precipitation
            all_events_precipitation=thispt_precipitation_ts[where(thispt_percentiles_ts ge percentile_to_plot and $
                                                                   thispt_percentiles_ts lt 2e19)]
                                ; A vector of all events containing the convective precipitation
            all_events_convective_precipitation=thispt_convective_ts[where(thispt_percentiles_ts ge percentile_to_plot and $
                                                                           thispt_percentiles_ts lt 2e19)]
            
                                ; Compute mean convective fraction
            convective_fraction=all_events_convective_precipitation/all_events_precipitation
            
                                ;Bin convective fraction rainfall            
            wet_days(j,k)=N_ELEMENTS(convective_fraction)
            FOR m=0,n_bins DO BEGIN
               IF m eq 0 THEN BEGIN
                  lower_bound=0
                  upper_bound=convective_fraction_bins(0)
               ENDIF ELSE IF m eq n_bins THEN BEGIN
                  lower_bound=convective_fraction_bins(n_bins-1)
                  upper_bound=10000
               ENDIF ELSE BEGIN
                  lower_bound=convective_fraction_bins(m-1)
                  upper_bound=convective_fraction_bins(m)
               ENDELSE
               IF TOTAL(where(convective_fraction ge lower_bound and convective_fraction lt upper_bound)) ge 0 THEN BEGIN
                  convective_rainfall_binned(m)=convective_rainfall_binned(m)+$
                                                N_ELEMENTS(where(convective_fraction ge lower_bound and $
                                                                 convective_fraction lt upper_bound)) 
               ENDIF
            ENDFOR
         ENDIF          
                 
      ENDFOR
   ENDFOR
                                ;Divide binned convective fraction by the number of total events
   convective_rainfall_binned(*)=convective_rainfall_binned(*)/TOTAL(wet_days(*,*))
   print,convective_rainfall_binned
   HIST,X=[0,convective_fraction_bins]+0.01*i+0.05,Y=convective_rainfall_binned,WIDTH=60,COL=546,FILLCOL=FSC_COLOR(color)
ENDFOR

AXES,XSTEP=0.1,YSTEP=0.1,YMINOR=0.05,XTITLE='Convective Fraction',YTITLE='Probability',NDECS=1
GLEGEND,LEGPOS=1,LABELS=REVERSE(['90th percentile','95th percentile','99th percentile']),COL=REVERSE(FSC_COLOR(all_colors))

PSCLOSE 

STOP
END
