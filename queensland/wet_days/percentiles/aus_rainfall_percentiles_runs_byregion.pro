PRO aus_rainfall_percentiles_runs_byregion

;Input files
silo_t62_input_file='/home/ss901165/datasets_mango/SILO/t62/SILO.may-apr_dmeans.1958-2000.precip_percentiles.t62.nc'

box=[-25,140,-10,153]
region_name='northeast'

n_datasets=1
dataset_names=['SILO t62 data']
n_days=365
n_years=43

percentile_to_plot=90

psfile='/home/ss901165/idl/queensland/aus_rainfall_percentiles_runs_byregion.'+region_name+'.ps'
PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=3000,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=0,TFONT=3,$
       TCHARSIZE=100,SPACE3=500
GSET,XMIN=0,XMAX=10,YMIN=0,YMAX=1500,TITLE='Frequency of observational data forming xth day runs at the '+$
     STRTRIM(STRING(percentile_to_plot),1)+'in the '+region_name+'region - 1958-2000'

rainfall_longitude=OPEN_AND_EXTRACT(silo_t62_input_file,'longitude')
rainfall_latitude=OPEN_AND_EXTRACT(silo_t62_input_file,'latitude')
DEFINE_BOUNDARIES,box,rainfall_latitude,rainfall_longitude,rainfall_box_tx,/LIMIT

rainfall_nlon=N_ELEMENTS(rainfall_longitude)
rainfall_nlat=N_ELEMENTS(rainfall_latitude)

;percentiles=OPEN_AND_EXTRACT(silo_t62_input_file,'percentile')
;n_percentiles=N_ELEMENTS(percentiles)

;clim_percentile=REFORM(OPEN_AND_EXTRACT(silo_t62_input_file,'percentile_of_daily_rain',$
                                      ;  offset=[percentiles_box_tx(1),percentiles_box_tx(0),0],$
                                      ;  count=[percentiles_nlon,percentiles_nlat,n_percentiles]))
;clim_percentile[where(clim_percentile eq 2e20)]=!Values.F_NaN

observational_daily_rainfall=(OPEN_AND_EXTRACT(silo_t62_input_file,'percentile_of_daily_rain',$
                                               offset=[rainfall_box_tx(1),rainfall_box_tx(0),0,0],$
                                               count=[rainfall_nlon,rainfall_nlat,n_years,n_days]))

; Define a (large) array to hold the lengths of the events
event_length=fltarr(rainfall_nlon*rainfall_nlat*n_years*n_days*(100-percentile_to_plot)/100.)
; Define a counter for the number of the event
event_counter=LONG(0)
; Define a counter for the number of land points
n_land_points=0

n_wet_days=fltarr(rainfall_nlon,rainfall_nlat)
n_extreme_days=fltarr(rainfall_nlon,rainfall_nlat)
                                ; Loop over all longitude bands
FOR i=0,rainfall_nlon-1 DO BEGIN
  ; print,'Scanning longitude band '+STRTRIM(STRING(i+1),1)+' out of '+STRTRIM(STRING(rainfall_nlon),1)+' ...'
                                ; Loop over all latitude points within this longitude band
   FOR j=0,rainfall_nlat-1 DO BEGIN
      IF N_ELEMENTS(where(observational_daily_rainfall(i,j,*,*) ne 2e20)) gt 1 THEN BEGIN
         n_land_points=n_land_points+1         
                                ; Make a timeseries of rainfall at this point
         thispt_daily_rainfall=fltarr(n_days*n_years)
         FOR k=0,n_years-1 DO $
            thispt_daily_rainfall(k*n_days:(k+1)*n_days-1)=observational_daily_rainfall(i,j,k,*)
         n_wet_days(i,j)=N_ELEMENTS(where(thispt_daily_rainfall lt 2e19))
         n_extreme_days(i,j)=N_ELEMENTS(where(thispt_daily_rainfall ge percentile_to_plot and $
                                              thispt_daily_rainfall lt 2e19))
         ;thispt_daily_rainfall[where(thispt_daily_rainfall eq 2e20)]=!Values.F_NaN
                                ; Make a flag to determine if we are in an event (=1) or not (=0)
         event_flag=0
                                ; Make a flag to determine if we have already used our one day below the threshold
         event_skip=0
                                ; Make a temporary variable to hold the length of the current event
         this_event_length=0
         FOR k=0,n_days*n_years-1 DO BEGIN
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
                  print,thispt_daily_rainfall(k-this_event_length:k-2),this_event_length-1
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

max_length=10
length_distribution=fltarr(max_length)
FOR i=1,max_length DO $
   length_distribution(i-1)=N_ELEMENTS(where(event_length eq i))

HIST,X=indgen(max_length)+1,Y=length_distribution,WIDTH=100;,COL=546,FILLCOL=420
AXES,XSTEP=1,YSTEP=50,XTITLE='Length of run (days)',YTITLE='Number of occurrences in record' 

PSCLOSE

STOP

END 
