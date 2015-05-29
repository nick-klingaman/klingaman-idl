PRO hadgem3_monwg_precip_amounts_length

box=[10,70,30,90]
region_name='india'

n_models=7
max_length=10

mask_input_file='/home/ss901165/um_output/mask_n96_hadgem3-7.1.nc'
mask_longitude=OPEN_AND_EXTRACT(mask_input_file,'longitude')
mask_latitude=OPEN_AND_EXTRACT(mask_input_file,'latitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)
mask=REFORM(OPEN_AND_EXTRACT(mask_input_file,'lsm',$
                             offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                             count=[mask_nlon,mask_nlat,1,1]))

mask_latrev=fltarr(mask_nlon,mask_nlat)
FOR i=0,mask_nlat-1 DO $
   mask_latrev(*,i)=mask(*,mask_nlat-i-1)
   
precip_bins=[0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,5.0,6.0,7.0,8.0,10.0,12.,14.,16.,18,20,25,30,40,50]
n_precip_bins=N_ELEMENTS(precip_bins)

length_distribution=fltarr(n_models,n_precip_bins,max_length)
length_distribution_scaled=fltarr(n_models,n_precip_bins,max_length)
FOR s=0,n_models-1 DO BEGIN
   CASE s OF
      0 : BEGIN
         infile='/home/ss901165/datasets/TRMM_3B42V6/n96/TRMM_3B42v6A.jan-dec_dmeans.1999-2010.n96.nc'
         varname='precip'
         day_offset=152
         n_days=122
         year_offset=0
         n_years=11
         multiplier=1.
         this_mask=mask_latrev
         model_name='trmm_n96'
      END
      5 : BEGIN
         infile='/home/ss901165/um_output3/hadgem3_monwg/akkvg/akkvg.jan-dec_dmeans.i2-j7.precip.nc'
         varname='precip'
         day_offset=150
         n_days=120
         year_offset=0
         n_years=16
         multiplier=86400.
         this_mask=mask
         model_name='ga30_akkvg'
      END
      6 : BEGIN
         infile='/home/ss901165/um_output3/hadgem3_monwg/glosea4_hindcasts/glosea4_hindcasts.0425_000.prcp_dmeans.nc'
         varname='precipitation_flux'
         day_offset=37
         n_days=115
         year_offset=0
         n_years=14
         multiplier=1.
         this_mask=mask
         model_name='glosea4_0425'
      END
      3 : BEGIN
         infile='/home/ss901165/um_output3/hadgem3_monwg/glosea4_hindcasts/glosea4_hindcasts.0501_001.prcp_dmeans.nc'
         varname='precipitation_flux'
         day_offset=31
         n_days=120
         year_offset=0
         n_years=14
         multiplier=1.
         this_mask=mask
         model_name='glosea4_0501'
      END
      4 : BEGIN
         infile='/home/ss901165/um_output3/hadgem3_monwg/glosea4_hindcasts/glosea4_hindcasts.0509_002.prcp_dmeans.nc'
         varname='precipitation_flux'
         day_offset=22
         n_days=120
         year_offset=0
         n_years=14
         multiplier=1.
         this_mask=mask
         model_name='glosea4_0509'
      END
      1 : BEGIN
         infile='/home/ss901165/um_output3/hadgem3a_amip2_control_vn74/hadgem3a_amip2_ctl_vn74.jan-dec_dmeans.years1-30.precip.nc'
         varname='precip'
         day_offset=150
         n_days=120
         year_offset=0
         n_years=30
         multiplier=86400.
         this_mask=mask
         model_name='nick_climsst_1xentrain'
      END
      2 : BEGIN
         infile='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmeans.years1-30.precip.nc'
         varname='precip'
         day_offset=150
         n_days=120
         year_offset=0
         n_years=30
         multiplier=86400.
         this_mask=mask
         model_name='nick_climsst_1.5xentrain'
      END
   ENDCASE
   
   longitude=OPEN_AND_EXTRACT(infile,'longitude')
   latitude=OPEN_AND_EXTRACT(infile,'latitude')
   DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
   nlon=N_ELEMENTS(longitude)
   nlat=N_ELEMENTS(latitude)
   
   daily_rainfall=(OPEN_AND_EXTRACT(infile,varname,$
                                        offset=[box_tx(1),box_tx(0),day_offset,year_offset],$
                                        count=[nlon,nlat,n_days,n_years]))*multiplier 
;   FOR i=0,nlon-1 DO BEGIN
;      FOR j=0,nlat-1 DO BEGIN
;         FOR n=0,nyears-1 DO BEGIN
;            thispt_precip=REFORM(precip(i,j,*,n))
;            temp=fltarr(n_days)
;            temp(*)=-999.99
;            FOR k=0,n_percentiles DO BEGIN
;               IF k eq 0 THEN BEGIN
;                  lower_bound=1
;                  upper_bound=precip_bins(k)
;                  percentile=0
;               ENDIF ELSE IF k eq n_percentiles THEN BEGIN
;                  lower_bound=ctl_percentiles(k-1)
;                  upper_bound=10000
;                  percentile=percentiles(n_percentiles-1)
;               ENDIF ELSE BEGIN
;                  lower_bound=precip_bins(k-1)
;                  upper_bound=precip_bins(k)
;                  percentile=percentiles(k-1)
;               ENDELSE
;               test=where(thispt_precip ge lower_bound and thispt_precip lt upper_bound)
;               IF TOTAL(test) ne -1 THEN $
;                  temp[where(thispt_precip ge lower_bound and thispt_precip lt upper_bound)]=percentile+0.01
;            ENDFOR
;            daily_rainfall(i,j,n,*)=temp
;         ENDFOR
;      ENDFOR
;   ENDFOR
   
;   FOR m=0,n_models-1 DO BEGIN
;      CASE m OF
;         0 : BEGIN
;            daily_rainfall=ctl_daily_rainfall
;            model_name='higem_ctl'
;            rainfall_nlon=ctl_nlon
;            rainfall_nlat=ctl_nlat
;            n_years=ctl_nyears
;         END
;         1 : BEGIN
;            daily_rainfall=daily_rainfall
;            model_name='higem_stbl'
;            rainfall_nlon=nlon
;            rainfall_nlat=nlat
;            n_years=nyears
;         END
;      ENDCASE
;      
   n_wet_days=fltarr(nlon,nlat)
   n_extreme_days=fltarr(nlon,nlat)
   
                                ; Loop over all percentiles
   FOR n=0,n_precip_bins-1 DO BEGIN
      precip_threshold=precip_bins(n)
                                ; Define a (large) array to hold the lengths of the events
      event_length=fltarr(N_ELEMENTS(where(daily_rainfall ge precip_threshold)))
                                ; Define a counter for the number of the event
      event_counter=LONG(0)
                                ; Define a counter for the number of land points
      n_land_points=0
                                ; Loop over all longitude bands
      FOR i=0,nlon-1 DO BEGIN
                                ; Loop over all latitude points within this longitude band
         FOR j=0,nlat-1 DO BEGIN
            IF this_mask(i,j) eq 1 THEN BEGIN
               n_land_points=n_land_points+1         
                                ; Make a timeseries of rainfall at this point
               thispt_daily_rainfall=fltarr(LONG(n_days)*LONG(n_years))
               FOR k=LONG(0),LONG(n_years)-1 DO $
                  thispt_daily_rainfall(k*n_days:(k+1)*n_days-1)=daily_rainfall(i,j,*,k)
               n_wet_days(i,j)=N_ELEMENTS(where(thispt_daily_rainfall ge precip_bins(0)))
               n_extreme_days(i,j)=N_ELEMENTS(where(thispt_daily_rainfall ge precip_threshold and $
                                                    thispt_daily_rainfall lt 2e19))         
                                ; Make a flag to determine if we are in an event (=1) or not (=0)
               event_flag=0
                                ; Make a flag to determine if we have already used our one day below the threshold
               event_skip=0
                                ; Make a temporary variable to hold the length of the current event
               this_event_length=0
               FOR k=LONG(0),LONG(n_days)*LONG(n_years)-1 DO BEGIN
                                ;print,thispt_daily_rainfall(k),event_flag,event_skip,this_event_length,event_counter
                  IF thispt_daily_rainfall(k) ge precip_threshold and thispt_daily_rainfall(k) lt 2e19 $ 
                     and event_flag eq 1 THEN BEGIN
                                ; We are already in an event            
                     this_event_length=this_event_length+1
                     event_skip=0
                  ENDIF ELSE IF thispt_daily_rainfall(k) ge precip_threshold and thispt_daily_rainfall(k) lt 2e19 $
                     and event_flag eq 0 THEN BEGIN
                                ; We are at the start of an event
                     event_flag=1
                     this_event_length=1
                     event_skip=0
                  ENDIF ELSE IF thispt_daily_rainfall(k) lt precip_threshold or thispt_daily_rainfall(k) gt 2e19 $
                     and event_flag eq 1 THEN BEGIN
                                ; The rainfall is below the percentile of interest
;                     IF event_skip eq 0 THEN BEGIN
;                                ; This is our grace day
;                                ;print,'Setting skip ...'
;                        event_skip=1
;                        this_event_length=this_event_length+1
;                     ENDIF ELSE BEGIN
                                ;print,'Terminating event ...'
                                ; We are out of the event
                        event_length(event_counter)=this_event_length-1
                                ;print,thispt_daily_rainfall(k-this_event_length:k-2),this_event_length-1
                        event_flag=0
                        event_skip=0
                        this_event_length=0
                        event_counter=event_counter+LONG(1)                  
;                     ENDELSE
                  ENDIF ELSE BEGIN
                                ; We are not in an event
                     event_flag=0
                     event_skip=0
                     this_event_length=0
                  ENDELSE
               ENDFOR
            ENDIF
         ENDFOR
      ENDFOR
      FOR i=1,max_length DO $
         length_distribution(s,n,i-1)=N_ELEMENTS(where(event_length eq i))/(FLOAT(n_land_points)*FLOAT(n_years))
   ENDFOR
   
   mylevs=['0.01','0.03','0.05','0.1','0.15','0.2','0.3','0.4','0.6','0.8','1.0','1.5','2.0','2.5','3.0','3.5','4.0']
   mylevs_scaled=['0.01','0.03','0.05','0.07','0.09','0.12','0.15','0.18','0.22','0.26','0.30','0.35','0.40','0.45','0.50','0.60','0.70']
   
   psfile='/home/ss901165/idl/hadgem3_monwg/isv_metrics/hadgem3_monwg_precip_amounts_length.'+model_name+'.'+$
          'jjas_'+region_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=1000,XOFFSET=0,YOFFSET=1000,TFONT=3,$
          TCHARSIZE=100,SPACE3=500
   GSET,YMIN=1,YMAX=max_length+1,XMIN=0,XMAX=n_precip_bins,TITLE='Frequency of runs of precip from '+model_name+' for JJAS - '+region_name
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   LEVS,MANUAL=mylevs
   CON,Y=indgen(max_length)+1.5,X=indgen(n_precip_bins)+0.5,FIELD=REFORM(length_distribution(s,*,*)),$
       CB_TITLE='Frequency (instances per year per gridpoint)',/BLOCK,/NOLINES
   AXES,YVALS=indgen(max_length)+1.5,YLABELS=STRMID(STRTRIM(STRING(indgen(max_length)+1),1),0,2),$
        XVALS=indgen(n_precip_bins)+0.5,XLABELS=STRMID(STRTRIM(STRING(precip_bins),1),0,4),$
        YTITLE='Length of run (days)',XTITLE='Daily rainfall (mm)',ORIENTATION=30
   PSCLOSE,/NOVIEW
   
   FOR n=0,n_precip_bins-1 DO BEGIN
      total_length_distribution=TOTAL(length_distribution(s,n,*))
      FOR i=1,max_length DO $
         length_distribution_scaled(s,n,i-1)=length_distribution(s,n,i-1)/FLOAT(total_length_distribution)
   ENDFOR

   psfile='/home/ss901165/idl/hadgem3_monwg/isv_metrics/hadgem3_monwg_precip_amounts_length.fractional_frequency.'+model_name+'.'+$
          'jjas_'+region_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=1000,XOFFSET=0,YOFFSET=1000,TFONT=3,$
          TCHARSIZE=100,SPACE3=500
   GSET,YMIN=1,YMAX=max_length+1,XMIN=0,XMAX=n_precip_bins,TITLE='Fractional frequency of runs of precip from '+model_name+' for JJAS - '+region_name
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_scaled)+1,/REV
   LEVS,MANUAL=mylevs_scaled
   CON,Y=indgen(max_length)+1.5,X=indgen(n_precip_bins)+0.5,FIELD=REFORM(length_distribution_scaled(s,*,*)),$
       CB_TITLE='Fractional frequency (of all events in each precipitation bin)',/BLOCK,/NOLINES
   AXES,YVALS=indgen(max_length)+1.5,YLABELS=STRMID(STRTRIM(STRING(indgen(max_length)+1),1),0,2),$
        XVALS=indgen(n_precip_bins)+0.5,XLABELS=STRMID(STRTRIM(STRING(precip_bins),1),0,4),$
        YTITLE='Length of run (days)',XTITLE='Daily rainfall (mm)',ORIENTATION=30
   PSCLOSE,/NOVIEW

   IF s gt 0 THEN BEGIN

      mylevs_ratio=['0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95','1.05','1.18','1.33','1.54','1.82','2.22','2.86','4.00','6.67']
      psfile='/home/ss901165/idl/hadgem3_monwg/isv_metrics/hadgem3_monwg_precip_amounts_length.'+model_name+'-ratio-trmm.jjas_'+region_name+'.ps'
      PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=1000,XOFFSET=0,YOFFSET=1000,TFONT=3,$
          TCHARSIZE=100,SPACE3=500
      GSET,YMIN=1,YMAX=max_length+1,XMIN=0,XMAX=n_precip_bins,TITLE='Ratio of freq of runs from '+model_name+' div TRMM for JJAS - '+region_name
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_ratio)+1,/REV
      LEVS,MANUAL=mylevs_ratio
      CON,Y=indgen(max_length)+1.5,X=indgen(n_precip_bins)+0.5,FIELD=REFORM(length_distribution(s,*,*))/REFORM(length_distribution(0,*,*)),$
          CB_TITLE='Ratio of frequencies (instances per year per gridpoint)',/BLOCK,/NOLINES
      AXES,YVALS=indgen(max_length)+1.5,YLABELS=STRMID(STRTRIM(STRING(indgen(max_length)+1),1),0,2),$
           XVALS=indgen(n_precip_bins)+0.5,XLABELS=STRMID(STRTRIM(STRING(precip_bins),1),0,4),$
           YTITLE='Length of run (days)',XTITLE='Daily rainfall (mm)',ORIENTATION=30
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/hadgem3_monwg/isv_metrics/hadgem3_monwg_precip_amounts_length.fractional_frequency.'+$
             model_name+'-ratio-trmm.jjas_'+region_name+'.ps'
      PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=1000,XOFFSET=0,YOFFSET=1000,TFONT=3,$
          TCHARSIZE=100,SPACE3=500
      GSET,YMIN=1,YMAX=max_length+1,XMIN=0,XMAX=n_precip_bins,TITLE='Ratio of frac freq of runs from '+model_name+' div TRMM for JJAS - '+region_name
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_ratio)+1,/REV
      LEVS,MANUAL=mylevs_ratio
      CON,Y=indgen(max_length)+1.5,X=indgen(n_precip_bins)+0.5,FIELD=REFORM(length_distribution_scaled(s,*,*))/REFORM(length_distribution_scaled(0,*,*)),$
          CB_TITLE='Ratio of fractional frequencies (of all events in each precipitation bin)',/BLOCK,/NOLINES
      AXES,YVALS=indgen(max_length)+1.5,YLABELS=STRMID(STRTRIM(STRING(indgen(max_length)+1),1),0,2),$
           XVALS=indgen(n_precip_bins)+0.5,XLABELS=STRMID(STRTRIM(STRING(precip_bins),1),0,4),$
           YTITLE='Length of run (days)',XTITLE='Daily rainfall (mm)',ORIENTATION=30
      PSCLOSE,/NOVIEW

   ENDIF
ENDFOR


STOP

END 
