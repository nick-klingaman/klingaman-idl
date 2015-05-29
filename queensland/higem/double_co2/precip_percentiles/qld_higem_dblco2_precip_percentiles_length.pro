PRO qld_higem_dblco2_precip_percentiles_length

n_seasons=5
box=[-30,138,-10,154]
region_name='queensland'

ctl_nyears=149
stbl_nyears=31

mask_input_file='/home/ss901165/um_output/mask_n144_higam.nc'
mask_longitude=OPEN_AND_EXTRACT(mask_input_file,'longitude')
mask_latitude=OPEN_AND_EXTRACT(mask_input_file,'latitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)
mask=REFORM(OPEN_AND_EXTRACT(mask_input_file,'lsm',$
                             offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                             count=[mask_nlon,mask_nlat,1,1]))
n_models=2
max_length=10

FOR s=4,n_seasons-1 DO BEGIN
   CASE s OF
      0 : BEGIN
                                ;Input files
         ctl_input_file='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.dec-feb_dmeans.h9-w8.precip_percentiles.aus_domain.nc'
         stbl_input_file='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.dec-feb_dmeans.o2-r3.precip.global_domain.nc'
         n_days=90
         season_name='dec-feb'
      END
      1 : BEGIN
                                ;Input files
         ctl_input_file='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.mar-may_dmeans.h9-w8.precip_percentiles.aus_domain.nc'
         stbl_input_file='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.mar-may_dmeans.o2-r3.precip.global_domain.nc'
         n_days=90
         season_name='mar-may'
      END
      2 : BEGIN
                                ;Input files
         ctl_input_file='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.jun-aug_dmeans.h9-w8.precip_percentiles.aus_domain.nc'
         stbl_input_file='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.jun-aug_dmeans.o2-r3.precip.global_domain.nc'
         n_days=90
         season_name='jun-aug'
      END
      3 : BEGIN
                                ;Input files
         ctl_input_file='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.sep-nov_dmeans.h9-w8.precip_percentiles.aus_domain.nc'
         stbl_input_file='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.sep-nov_dmeans.o2-r3.precip.global_domain.nc'
         n_days=90
         season_name='sep-nov'
      END
      4 : BEGIN
                                ;Input files
         ctl_input_file='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.may-apr_dmeans.h9-w8.precip_percentiles.aus_domain.nc'
         stbl_input_file='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.may-apr_dmeans.o2-r3.precip.global_domain.nc'
         n_days=360
         season_name='may-apr'
      END
   ENDCASE

   percentiles=OPEN_AND_EXTRACT(ctl_input_file,'percentile')
   n_percentiles=N_ELEMENTS(percentiles)
   length_distribution=fltarr(n_models,n_percentiles,max_length)
   
   ctl_longitude=OPEN_AND_EXTRACT(ctl_input_file,'longitude')
   ctl_latitude=OPEN_AND_EXTRACT(ctl_input_file,'latitude')
   DEFINE_BOUNDARIES,box,ctl_latitude,ctl_longitude,ctl_box_tx,/LIMIT
   ctl_nlon=N_ELEMENTS(ctl_longitude)
   ctl_nlat=N_ELEMENTS(ctl_latitude)
   
   stbl_longitude=OPEN_AND_EXTRACT(stbl_input_file,'longitude')
   stbl_latitude=OPEN_AND_EXTRACT(stbl_input_file,'latitude')
   DEFINE_BOUNDARIES,box,stbl_latitude,stbl_longitude,stbl_box_tx,/LIMIT
   stbl_nlon=N_ELEMENTS(stbl_longitude)
   stbl_nlat=N_ELEMENTS(stbl_latitude)
   
   ctl_daily_rainfall=(OPEN_AND_EXTRACT(ctl_input_file,'percentile_of_daily_rain',$
                                        offset=[ctl_box_tx(1),ctl_box_tx(0),0,0],$
                                        count=[ctl_nlon,ctl_nlat,ctl_nyears,n_days]))
   
   ctl_percentiles=OPEN_AND_EXTRACT(ctl_input_file,'amount_at_percentile',$
                                    offset=[ctl_box_tx(1),ctl_box_tx(0),0],$
                                    count=[ctl_nlon,ctl_nlat,n_percentiles])
   
   stbl_precip=REFORM(OPEN_AND_EXTRACT(stbl_input_file,'precip',$
                                       offset=[stbl_box_tx(1),stbl_box_tx(0),0,0],$
                                       count=[stbl_nlon,stbl_nlat,n_days,stbl_nyears]))*86400.
   
   stbl_daily_rainfall=fltarr(stbl_nlon,stbl_nlat,stbl_nyears,n_days)
   
   FOR i=0,stbl_nlon-1 DO BEGIN
      FOR j=0,stbl_nlat-1 DO BEGIN
         FOR n=0,stbl_nyears-1 DO BEGIN
            thispt_stbl_precip=REFORM(stbl_precip(i,j,*,n))
            temp=fltarr(n_days)
            temp(*)=-999.99
            FOR k=0,n_percentiles DO BEGIN
               IF k eq 0 THEN BEGIN
                  lower_bound=1
                  upper_bound=ctl_percentiles(i,j,k)
                  percentile=0
               ENDIF ELSE IF k eq n_percentiles THEN BEGIN
                  lower_bound=ctl_percentiles(i,j,n_percentiles-1)
                  upper_bound=10000
                  percentile=percentiles(n_percentiles-1)
               ENDIF ELSE BEGIN
                  lower_bound=ctl_percentiles(i,j,k-1)
                  upper_bound=ctl_percentiles(i,j,k)
                  percentile=percentiles(k-1)
               ENDELSE
               test=where(thispt_stbl_precip ge lower_bound and thispt_stbl_precip lt upper_bound)
               IF TOTAL(test) ne -1 THEN $
                  temp[where(thispt_stbl_precip ge lower_bound and thispt_stbl_precip lt upper_bound)]=percentile+0.01
            ENDFOR
            stbl_daily_rainfall(i,j,n,*)=temp
         ENDFOR
      ENDFOR
   ENDFOR
   
   FOR m=0,n_models-1 DO BEGIN
      CASE m OF
         0 : BEGIN
            daily_rainfall=ctl_daily_rainfall
            model_name='higem_ctl'
            rainfall_nlon=ctl_nlon
            rainfall_nlat=ctl_nlat
            n_years=ctl_nyears
         END
         1 : BEGIN
            daily_rainfall=stbl_daily_rainfall
            model_name='higem_stbl'
            rainfall_nlon=stbl_nlon
            rainfall_nlat=stbl_nlat
            n_years=stbl_nyears
         END
      ENDCASE
      
      n_wet_days=fltarr(rainfall_nlon,rainfall_nlat)
      n_extreme_days=fltarr(rainfall_nlon,rainfall_nlat)
      
                                ; Loop over all percentiles
      FOR n=0,n_percentiles-1 DO BEGIN
         percentile_to_plot=percentiles(n)
                                ; Define a (large) array to hold the lengths of the events
         event_length=fltarr(rainfall_nlon*rainfall_nlat*n_years*n_days*(100-percentile_to_plot)/100.)
                                ; Define a counter for the number of the event
         event_counter=LONG(0)
                                ; Define a counter for the number of land points
         n_land_points=0
                                ; Loop over all longitude bands
         FOR i=0,rainfall_nlon-1 DO BEGIN
                                ; Loop over all latitude points within this longitude band
            FOR j=0,rainfall_nlat-1 DO BEGIN
               IF mask(i,j) eq 1 THEN BEGIN
                  n_land_points=n_land_points+1         
                                ; Make a timeseries of rainfall at this point
                  thispt_daily_rainfall=fltarr(LONG(n_days)*LONG(n_years))
                  FOR k=LONG(0),LONG(n_years)-1 DO $
                     thispt_daily_rainfall(k*n_days:(k+1)*n_days-1)=daily_rainfall(i,j,k,*)
                  n_wet_days(i,j)=N_ELEMENTS(where(thispt_daily_rainfall lt 2e19))
                  n_extreme_days(i,j)=N_ELEMENTS(where(thispt_daily_rainfall ge percentile_to_plot and $
                                                       thispt_daily_rainfall lt 2e19))         
                                ; Make a flag to determine if we are in an event (=1) or not (=0)
                  event_flag=0
                                ; Make a flag to determine if we have already used our one day below the threshold
                  event_skip=0
                                ; Make a temporary variable to hold the length of the current event
                  this_event_length=0
                  FOR k=LONG(0),LONG(n_days)*LONG(n_years)-1 DO BEGIN
                                ;print,thispt_daily_rainfall(k),event_flag,event_skip,this_event_length,event_counter
                     IF thispt_daily_rainfall(k) ge percentile_to_plot and thispt_daily_rainfall(k) lt 2e19 $ 
                        and event_flag eq 1 THEN BEGIN
                                ; We are already in an event            
                        this_event_length=this_event_length+1
                        event_skip=0
                     ENDIF ELSE IF thispt_daily_rainfall(k) ge percentile_to_plot and thispt_daily_rainfall(k) lt 2e19 $
                     and event_flag eq 0 THEN BEGIN
                                ; We are at the start of an event
                        event_flag=1
                        this_event_length=1
                        event_skip=0
                     ENDIF ELSE IF thispt_daily_rainfall(k) lt percentile_to_plot or thispt_daily_rainfall(k) gt 2e19 $
                        and event_flag eq 1 THEN BEGIN
                                ; The rainfall is below the percentile of interest
                        IF event_skip eq 0 THEN BEGIN
                                ; This is our grace day
                                ;print,'Setting skip ...'
                           event_skip=1
                           this_event_length=this_event_length+1
                        ENDIF ELSE BEGIN
                                ;print,'Terminating event ...'
                                ; We are out of the event
                           event_length(event_counter)=this_event_length-1
                                ;print,thispt_daily_rainfall(k-this_event_length:k-2),this_event_length-1
                           event_flag=0
                           event_skip=0
                           this_event_length=0
                           event_counter=event_counter+LONG(1)                  
                        ENDELSE
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
            length_distribution(m,n,i-1)=N_ELEMENTS(where(event_length eq i))/(FLOAT(n_land_points)*FLOAT(n_years))
      ENDFOR
      
      mylevs=['0.01','0.03','0.05','0.1','0.2','0.3','0.4','0.5','0.6','0.8','1.0','1.5','2.0','2.5','3.0']
      
      psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/qld_higem_dblco2_precip_percentiles_length.'+model_name+'.'+$
             season_name+'_'+region_name+'.ps'
      PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=1000,XOFFSET=0,YOFFSET=1000,TFONT=3,$
             TCHARSIZE=100,SPACE3=500
      GSET,YMIN=1,YMAX=max_length+1,XMIN=0,XMAX=n_percentiles,TITLE='Frequency of runs of precip from '+model_name+' for '+season_name+' - Queensland land'
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV
      LEVS,MANUAL=mylevs
      CON,Y=indgen(max_length)+1.5,X=indgen(n_percentiles)+0.5,FIELD=REFORM(length_distribution(m,*,*)),$
          CB_TITLE='Frequency (instances per year per gridpoint)',/BLOCK,/NOLINES
      AXES,YVALS=indgen(max_length)+1.5,YLABELS=STRMID(STRTRIM(STRING(indgen(max_length)+1),1),0,2),$
           XVALS=indgen(n_percentiles)+0.5,XLABELS=STRMID(STRTRIM(STRING(percentiles),1),0,4),$
           YTITLE='Length of run (days)',XTITLE='Percentile of daily rainfall',ORIENTATION=45
      PSCLOSE,/NOVIEW
   ENDFOR
   
   mylevs=['-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15']
   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/qld_higem_dblco2_precip_percentiles_length.higem_stbl-minus-higem_ctl.'+$
          season_name+'_'+region_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=200,SPACE2=1000,XOFFSET=0,YOFFSET=1000,TFONT=3,$
          TCHARSIZE=100,SPACE3=500
   GSET,YMIN=1,YMAX=max_length+1,XMIN=0,XMAX=n_percentiles,TITLE='Diff in frequency of runs of precip for HiGEM 2xCO2 minus HiGEM CTL for '+season_name+' - Queensland land'
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   LEVS,MANUAL=mylevs
   CON,Y=indgen(max_length)+1.5,X=indgen(n_percentiles)+0.5,FIELD=REFORM(length_distribution(1,*,*))-REFORM(length_distribution(0,*,*)),$
       CB_TITLE='Difference in frequency (instances per year per gridpoint)',/BLOCK,/NOLINES
   AXES,YVALS=indgen(max_length)+1.5,YLABELS=STRMID(STRTRIM(STRING(indgen(max_length)+1),1),0,2),$
        XVALS=indgen(n_percentiles)+0.5,XLABELS=STRMID(STRTRIM(STRING(percentiles),1),0,4),$
        YTITLE='Length of run (days)',XTITLE='Percentile of daily rainfall',ORIENTATION=45
   PSCLOSE,/NOVIEW
   
   mylevs=['0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95','1.05','1.18','1.33','1.54','1.82','2.22','2.86','4.00','6.67']
   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/qld_higem_dblco2_precip_percentiles_length.higem_stbl-ratio-higem_ctl.'+$
          season_name+'_'+region_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=200,SPACE2=1000,XOFFSET=0,YOFFSET=1000,TFONT=3,$
          TCHARSIZE=100,SPACE3=500
   GSET,YMIN=1,YMAX=max_length+1,XMIN=0,XMAX=n_percentiles,TITLE='Ratio of frequency of runs of precip for HiGEM 2xCO2 div HiGEM CTL for '+season_name+' - Queensland land'
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   LEVS,MANUAL=mylevs
   CON,Y=indgen(max_length)+1.5,X=indgen(n_percentiles)+0.5,FIELD=FLOAT(REFORM(length_distribution(1,*,*)))/FLOAT(REFORM(length_distribution(0,*,*))),$
       CB_TITLE='Ratio of frequencies (no units)',/BLOCK,/NOLINES
   AXES,YVALS=indgen(max_length)+1.5,YLABELS=STRMID(STRTRIM(STRING(indgen(max_length)+1),1),0,2),$
        XVALS=indgen(n_percentiles)+0.5,XLABELS=STRMID(STRTRIM(STRING(percentiles),1),0,4),$
        YTITLE='Length of run (days)',XTITLE='Percentile of daily rainfall',ORIENTATION=45
   PSCLOSE,/NOVIEW
ENDFOR

STOP

END 
