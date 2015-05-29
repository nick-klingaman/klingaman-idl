PRO mjo_indices_phase_composites_ts,rmm_infile,var_infile,clim_infile,varname,nc_varname,ndays_per_year,model_name,$
                                    model_nyear=model_nyear,rmm_offset=rmm_offset,model_offset=model_offset,$
                                    field_multiplier=field_multiplier,year_first=year_first,latavg_range=latavg_range,$
                                    lon_range=lon_range,mask_file=mask_file,mask_value=mask_value,single_plot=single_plot
  
; Make phase composites of the MJO from a model integration, given a
; file with the RMM indices, one with model output from the variable
; of interest and one with the daily climatology of that variable.

n_phases=8
phase_angle_start=findgen(n_phases)*(360./FLOAT(n_phases))
phase_angle_stop=phase_angle_start+(360./FLOAT(n_phases))
phases=indgen(n_phases)+n_phases/2+1
phases[where(phases gt n_phases)]=phases[where(phases gt n_phases)]-n_phases

IF KEYWORD_SET(latavg_range) THEN BEGIN
   cross_section=1
   IF N_ELEMENTS(latavg_range ne 2) THEN $
      print,'You must give two values for latavg_range, like this: [min_lat,max_lat]'
ENDIF ELSE $
   cross_section=0

IF KEYWORD_SET(lon_range) THEN BEGIN
   our_lon_range=lon_range
ENDIF ELSE $
   our_lon_range=[0,360]

; This case statement controls the plotting options for each variable, including the contour intervals and
; whether to draw vectors on the plot (for winds only).  You can add new variables to this by copying one of the
; parts (between the variable name and the END statement) and changing the name of the variable and any other
; necessary options.  You must keep the variable names within the BEGIN...END bit the same, though.
CASE varname OF
   'u850' : BEGIN
                                ; Contour intervals
      mylevs_anom=['-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5']
                                ; Do you want to reverse the colour scale (0=no, 1=yes)?
      color_rev=0
                                ; Do you want to plot vectors on the map as well as contours (0=no, 1=yes)?
      plot_vector=1
                                ; In which direction should the vectors be plotted ('u' or 'v')?
      vector_direction='u'
                                ; What is the reference magnitude for the vectors (for the key vector)?
      vector_ref_mag=1.0
                                ; Plot every nth vector
      vector_stride=4
                                ; Units for this variable (to print under the colour bar)
      units='m s!U-1!N'
   END
   'u200' : BEGIN
      mylevs_anom=['-11','-9','-7','-5','-3','-1','1','3','5','7','9','11']
      color_rev=0
      plot_vector=1
      vector_direction='u'
      vector_ref_mag=2.5
      vector_stride=4
      units='m s!U-1!N'
   END
   'olr' : BEGIN
      mylevs_anom=['-33','-27','-21','-15','-9','-3','3','9','15','21','27','33']
      color_rev=0
      plot_vector=0
      units='W m!U-2!N'
   END
   'precip' : BEGIN
      mylevs_anom=['-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5']      
      color_rev=1
      plot_vector=0
      units='mm day!U-1!N'
      white_num=[8]
   END
   'q' : BEGIN
      mylevs_anom=['-0.65','-0.55','-0.45','-0.35','-0.25','-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55','0.65']
      color_rev=1
      plot_vector=0
      cross_section=1
      ystep=-100
      yminor=-50
      xstep=10
      xminor=5
      units='g kg!U-1!N'
      white_num=[9]
   END
   'surf_temp' : BEGIN      
      mylevs_anom=['-0.27','-0.21','-0.15','-0.09','-0.03','0.03',$
                   '0.09','0.15','0.21','0.27']
      color_rev=0
      plot_vector=0
      cross_section=0
      units='K'
   END
   'sst' : BEGIN
      mylevs_anom=['-0.27','-0.21','-0.15','-0.09','-0.03','0.03','0.09','0.15','0.21','0.27']
      color_rev=0
      plot_vector=0
      cross_section=0
      units='K'
      white_num=[7]
   END
   'ocnT' : BEGIN
      mylevs_anom=['-0.285','-0.255','-0.225','-0.195','-0.165','-0.135','-0.105','-0.075','-0.045','-0.015',$
                   '0.015','0.045','0.075','0.105','0.135','0.165','0.195','0.225','0.255','0.285']
      color_rev=0
      plot_vector=0
      cross_section=1
      ystep=-10
      yminor=-5
      xstep=15
      xminor=5
      units='K'
   END
   'omega' : BEGIN
      mylevs_anom=['-0.65','-0.55','-0.45','-0.35','-0.25','-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55','0.65']
      color_rev=1
      plot_vector=0
      cross_section=1
      ystep=-100
      yminor=-50
      xstep=30
      xminor=15
      units='g kg!U-1!N'
   END
ENDCASE

ncid_in=NCDF_OPEN(var_infile)
varid=NCDF_VARID(ncid_in,nc_varname)
varstruct=NCDF_VARINQ(ncid_in,varid)

NCDF_DIMINQ,ncid_in,varstruct.dim(0),longitude_name,n_lon
NCDF_DIMINQ,ncid_in,varstruct.dim(1),latitude_name,n_lat
longitude=OPEN_AND_EXTRACT(var_infile,longitude_name)
latitude=OPEN_AND_EXTRACT(var_infile,latitude_name)
;print,longitude_name,latitude_name
;print,latitude,longitude
;help,latitude,longitude
IF cross_section eq 1 THEN BEGIN
   box=[MIN(latavg_range),MIN(our_lon_range),MAX(latavg_range),MAX(our_lon_range)]
ENDIF ELSE $
   box=[-30.1,MIN(our_lon_range),30.1,MAX(our_lon_range)]
DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)

IF KEYWORD_SET(mask_file) THEN BEGIN
   mask_longitude=OPEN_AND_EXTRACT(mask_file,'longitude')
   mask_latitude=OPEN_AND_EXTRACT(mask_file,'latitude')
   DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
   mask_nlat=N_ELEMENTS(mask_latitude)
   mask_nlon=N_ELEMENTS(mask_longitude)

   mask=OPEN_AND_EXTRACT(mask_file,'lsm',$
                         offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                         count=[mask_nlon,mask_nlat,1,1])
ENDIF

IF KEYWORD_SET(year_first) THEN BEGIN
   year_first=year_first
ENDIF ELSE $
   year_first=0

; Work out dimension numbers based on keyword options
IF cross_section eq 1 THEN BEGIN
   ; Read vertical coordinate
   NCDF_DIMINQ,ncid_in,varstruct.dim(2),vert_name,n_vert
   vertical=OPEN_AND_EXTRACT(var_infile,vert_name)
   IF year_first eq 1 THEN BEGIN
      year_dim=3
      time_dim=4
   ENDIF ELSE BEGIN
      year_dim=4
      time_dim=3
   ENDELSE
ENDIF ELSE BEGIN
   vert_dim=!Values.F_NaN
   IF year_first eq 1 THEN BEGIN
      year_dim=2
      time_dim=3
   ENDIF ELSE BEGIN
      year_dim=3
      time_dim=2
   ENDELSE
ENDELSE

; Read time
NCDF_DIMINQ,ncid_in,varstruct.dim(time_dim),time_name,n_time
time=OPEN_AND_EXTRACT(var_infile,time_name)

; Work out number of years, either from keyword option or by computation
IF KEYWORD_SET(model_nyear) THEN BEGIN
   our_nyear=model_nyear
ENDIF ELSE BEGIN
   IF (cross_section eq 0 and varstruct.ndims eq 3) or $
      (cross_section eq 1 and varstruct.ndims eq 4) THEN BEGIN
      our_nyear=time/ndays_per_year
   ENDIF ELSE IF (cross_section eq 0 and varstruct.ndims eq 4) or $
      (cross_section eq 1 and varstruct.ndims eq 5) THEN BEGIN
      NCDF_DIMINQ,ncid_in,varstruct.dim(year_dim),year_name,n_year
      our_nyear=n_year
   ENDIF ELSE BEGIN
      print,'Unable to work out the number of years in the file.'
      IF KEYWORD_SET(latavg_range) THEN BEGIN
         print,'Since you are doing a cross-section plot, you must use a four-dimensional'
         print,'(lon x lat x depth x time) or a five-dimensional (lon x lat x depth x day x year)'
         print,'input file.  If you are trying to use a three-dimensional file (lon x lat x time)'
         print,'you must not specify a cross-section plot (using the latavg_range keyword)'
      ENDIF ELSE BEGIN
         print,'Since you are not doing a cross-section plot, you must use a three-dimensional'
         print,'(lon x lat x time) or a four-dimensional (lon x lat x day x year) input file.'
         print,'If you are trying to use a five-dimensional file (lon x lat x depth x day x year)'
         print,'then you must specify a cross-section plot (using the latavg_range keyword).'
      ENDELSE
   ENDELSE
ENDELSE

IF KEYWORD_SET(rmm_offset) THEN BEGIN
   our_rmm_offset=rmm_offset
ENDIF ELSE $
   our_rmm_offset=0
IF KEYWORD_SET(model_offset) THEN BEGIN
   our_model_offset=model_offset
ENDIF ELSE $
   our_model_offset=0

IF KEYWORD_SET(field_multiplier) THEN BEGIN
   our_field_multiplier=field_multiplier
ENDIF ELSE $
   our_field_multiplier=1.

; Read daily climatology of variable
IF KEYWORD_SET(latavg_range) THEN BEGIN
   var_clim_fullgrid=REFORM(OPEN_AND_EXTRACT(clim_infile,nc_varname,$
                                             offset=[box_tx(1),box_tx(0),0,0],count=[n_lon,n_lat,n_vert,ndays_per_year]))$
                     *FLOAT(our_field_multiplier)
   var_clim=fltarr(n_lon,n_vert,ndays_per_year)
   FOR i=0,n_lon-1 DO $
      FOR j=0,n_vert-1 DO $
         FOR k=0,ndays_per_year-1 DO $
            var_clim(i,j,k)=MEAN(var_clim_fullgrid(i,*,j,k),/NaN)
   var_clim_store=var_clim
ENDIF ELSE BEGIN
   var_clim=REFORM(OPEN_AND_EXTRACT(clim_infile,nc_varname,$
                                    offset=[box_tx(1),box_tx(0),0],count=[n_lon,n_lat,ndays_per_year]))*FLOAT(our_field_multiplier)
ENDELSE
 
n_strong_days=intarr(n_phases)
IF cross_section eq 0 THEN BEGIN
   phase_anomalies=fltarr(n_lon,n_lat,n_phases)
ENDIF ELSE $
   phase_anomalies=fltarr(n_lon,n_vert,n_phases)

IF KEYWORD_SET(latavg_range) THEN $
   var_clim=var_clim_store

IF varstruct.ndims eq 3 THEN BEGIN
   variable=REFORM(OPEN_AND_EXTRACT(var_infile,nc_varname,$
                                    offset=[box_tx(1),box_tx(0),ndays_per_year*(i+our_model_offset)],$
                                    count=[n_lon,n_lat,ndays_per_year]))*our_field_multiplier
ENDIF ELSE IF (cross_section eq 0 and varstruct.ndims eq 4) THEN BEGIN
   IF year_first eq 1 THEN BEGIN
      variable=REFORM(OPEN_AND_EXTRACT(var_infile,nc_varname,$
                                       offset=[box_tx(1),box_tx(0),our_model_offset,0],$
                                       count=[n_lon,n_lat,our_nyear,ndays_per_year]))*our_field_multiplier
   ENDIF ELSE $
      variable=REFORM(OPEN_AND_EXTRACT(var_infile,nc_varname,$
                                       offset=[box_tx(1),box_tx(0),0,our_model_offset],$
                                       count=[n_lon,n_lat,ndays_per_year,our_nyear]))*our_field_multiplier
ENDIF ELSE IF cross_section eq 1 THEN BEGIN
   IF varstruct.ndims eq 4 THEN BEGIN
      variable_fullgrid=REFORM(OPEN_AND_EXTRACT(var_infile,nc_varname,$
                                                offset=[box_tx(1),box_tx(0),0,ndays_per_year*(i+our_model_offset)],$
                                                count=[n_lon,n_lat,n_vert,ndays_per_year]))*our_field_multiplier
   ENDIF ELSE IF varstruct.ndims eq 5 THEN BEGIN
      IF year_first eq 1 THEN BEGIN
         variable_fullgrid=REFORM(OPEN_AND_EXTRACT(var_infile,nc_varname,$
                                                   offset=[box_tx(1),box_tx(0),0,our_model_offset,0],$
                                                   count=[n_lon,n_lat,n_vert,our_nyear,ndays_per_year]))*our_field_multiplier
      ENDIF ELSE $
         variable_fullgrid=REFORM(OPEN_AND_EXTRACT(var_infile,nc_varname,$
                                                   offset=[box_tx(1),box_tx(0),0,0,our_model_offset],$
                                                   count=[n_lon,n_lat,n_vert,ndays_per_year,our_nyear]))*our_field_multiplier
   ENDIF
                                ; Search for values equal to zero, as this indicates missing data in UM on pressure levels.
   FOR j=0,n_lon-1 DO BEGIN
      FOR k=0,n_lat-1 DO BEGIN
         FOR m=0,n_vert-1 DO BEGIN
            temp=REFORM(variable_fullgrid(j,k,m,*))
            IF TOTAL(where(temp eq 0)) ge 0 THEN BEGIN
               variable_fullgrid(j,k,m,*,*)=!Values.F_NaN
                                ; Also reset climatology to missing value
               var_clim_fullgrid(j,k,m,*)=!Values.F_NaN                  
            ENDIF 
         ENDFOR
      ENDFOR
                                ; Recompute daily climatology
      FOR k=0,n_vert-1 DO $
         FOR m=0,ndays_per_year-1 DO $
            var_clim(j,k,m)=MEAN(var_clim_fullgrid(j,*,k,m),/NaN)
   ENDFOR
                                ;IF TOTAL(where(variable_fullgrid eq 0)) ge 0 THEN $
                                ;   variable_fullgrid[where(variable_fullgrid eq 0)]=!Values.F_NaN
   variable=fltarr(n_lon,n_vert,ndays_per_year,our_nyear)      
   FOR j=0,n_lon-1 DO $
      FOR k=0,n_vert-1 DO $
         FOR m=0,ndays_per_year-1 DO $
            FOR n=0,our_nyear-1 DO $
               variable(j,k,m,n)=MEAN(variable_fullgrid(j,*,k,m,n),/NaN)
ENDIF
   
IF TOTAL(where(variable ge 1E10)) gt 0 THEN $
   variable[where(variable ge 1E10)]=!Values.F_NaN
;   phase_ts=REFORM(OPEN_AND_EXTRACT(rmm_infile,'phase',$
;                                       offset=[our_rmm_offset+i,0],count=[1,ndays_per_year]))
rmm1_ts=REFORM(OPEN_AND_EXTRACT(rmm_infile,'rmm1',$
                                offset=[our_rmm_offset,0],count=[our_nyear,ndays_per_year]))
rmm2_ts=REFORM(OPEN_AND_EXTRACT(rmm_infile,'rmm2',$
                                offset=[our_rmm_offset,0],count=[our_nyear,ndays_per_year]))
phase_angle=ATAN(rmm2_ts/rmm1_ts)
phase_angle[where(rmm2_ts lt 0 and rmm1_ts lt 0)]=phase_angle[where(rmm2_ts lt 0 and rmm1_ts lt 0)]+!Pi
phase_angle[where(rmm2_ts lt 0 and rmm1_ts ge 0)]=phase_angle[where(rmm2_ts lt 0 and rmm1_ts ge 0)]+2.*!Pi
phase_angle[where(rmm2_ts ge 0 and rmm1_ts lt 0)]=phase_angle[where(rmm2_ts ge 0 and rmm1_ts lt 0)]+!Pi
phase_angle=phase_angle*180./!Pi

amplitude=REFORM(OPEN_AND_EXTRACT(rmm_infile,'amplitude',$
                                     offset=[our_rmm_offset,0],count=[our_nyear,ndays_per_year]))
phase_angle_ts=fltarr(our_nyear*ndays_per_year)
amplitude_ts=fltarr(our_nyear*ndays_per_year)
FOR i=0,our_nyear-1 DO BEGIN
   phase_angle_ts(i*ndays_per_year:(i+1)*ndays_per_year-1)=phase_angle(i,*)
   amplitude_ts(i*ndays_per_year:(i+1)*ndays_per_year-1)=amplitude(i,*)
ENDFOR

FOR j=0,n_phases-1 DO BEGIN
   print,'------> Phase '+STRTRIM(STRING(j+1),1)
   strong_days=where(phase_angle_ts ge phase_angle_start(j) and phase_angle_ts lt phase_angle_stop(j) and amplitude_ts ge 1)
   IF TOTAL(strong_days) gt 0 THEN BEGIN
      n_strong_days(j)=N_ELEMENTS(strong_days)+n_strong_days(j)
      print,j,N_ELEMENTS(strong_days)
      FOR k=0,n_lon-1 DO BEGIN
         IF cross_section eq 0 THEN $
            ny=n_lat
         IF cross_section eq 1 THEN $
            ny=n_vert
         FOR m=0,ny-1 DO BEGIN
            var_gridpt_ts=fltarr(our_nyear*ndays_per_year)
            IF KEYWORD_SET(year_first) THEN BEGIN
               FOR i=0,our_nyear-1 DO $
                  var_gridpt_ts(i*ndays_per_year:(i+1)*ndays_per_year-1)=REFORM(variable(k,m,i,*))-REFORM(var_clim(k,m,*))
            ENDIF ELSE $
               FOR i=0,our_nyear-1 DO $
                  var_gridpt_ts(i*ndays_per_year:(i+1)*ndays_per_year-1)=REFORM(variable(k,m,*,i))-REFORM(var_clim(k,m,*))
            ;FOR i=120,our_nyear*ndays_per_year-1 DO $
            ;   var_gridpt_ts(i)=var_gridpt_ts(i)-MEAN(var_gridpt_ts(i-120:i-1),/NaN)
            phase_anomalies(k,m,j)=MEAN(var_gridpt_ts[strong_days],/NaN)
         ENDFOR
      ENDFOR      
   ENDIF
ENDFOR

;FOR j=0,n_phases-1 DO $ 
;   phase_anomalies(*,*,j)=phase_anomalies(*,*,j)/n_strong_days(j)

IF KEYWORD_SET(single_plot) THEN BEGIN
                                ; Make one PostScript file containing all phases
   psfile='/home/ss901165/idl/mjo_indices/mjo_indices_phase_composites.'+model_name+'.'+varname+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1200,SPACE2=200,XOFFSET=500,YOFFSET=1000,TFONT=2,$
          TCHARSIZE=90,SPACE3=400,YPLOTS=4,XPLOTS=2,YSPACING=1500,XSPACING=2000
   
   FOR j=0,n_phases-1 DO BEGIN
      POS,XPOS=(j/4)+1,YPOS=4-(j MOD 4)
      
      IF color_rev eq 0 THEN $
         CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_anom)+1
      IF color_rev eq 1 THEN $
         CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_anom)+1,/REV
      LEVS,MANUAL=mylevs_anom
      IF cross_section eq 1 THEN BEGIN
         GSET,XMIN=our_lon_range(0),XMAX=our_lon_range(1),YMIN=MAX(ABS(vertical)),YMAX=MIN(ABS(vertical))
         y_coord=ABS(vertical)
      ENDIF ELSE BEGIN
         MAP,LATMIN=MIN(latitude),LATMAX=MAX(latitude),LONMIN=our_lon_range(0),LONMAX=our_lon_range(1)
         y_coord=latitude
      ENDELSE
      IF KEYWORD_SET(mask_value) THEN BEGIN
         phase_anomalies_plot=REFORM(phase_anomalies(*,*,j))
         phase_anomalies_plot[where(mask eq mask_value)]=!Values.F_NaN
      ENDIF ELSE $
         phase_anomalies_plot=REFORM(phase_anomalies(*,*,j))
      CON,X=longitude,Y=y_coord,FIELD=REFORM(phase_anomalies_plot(*,*)),/NOLINELABELS,$
          TITLE='Phase composite of '+varname+' for MJO phase '+STRTRIM(STRING(phases(j)),1)+' from '+model_name+' over '+$
          STRTRIM(STRING(n_strong_days(j)),1)+' days ('+STRTRIM(STRING(our_nyear),1)+' years)',$
          POSITIVE_STYLE=2,NEGATIVE_STYLE=2,/NOCOLBAR
      IF plot_vector eq 1 THEN BEGIN
         IF vector_direction eq 'u' THEN BEGIN
            zeroes=fltarr(n_lon,n_lat)
            zeroes(*,*)=0.
            VECT,U=REFORM(phase_anomalies(*,*,j)),V=zeroes,X=longitude,Y=latitude,TYPE=4,MAG=vector_ref_mag,MUNITS=' m s!U-1!N',$
                 STRIDE=vector_stride
         ENDIF ELSE IF vector_direction eq 'v' THEN BEGIN
            zeroes=fltarr(n_lon,n_lat)
            zeroes(*,*)=0.
            VECT,V=REFORM(phase_anomalies(*,*,j)),U=zeroes,X=longitude,Y=latitude,TYPE=4,MAG=vector_ref_mag,MUNITS=' m s!U-1!N',$
                 STRIDE=vector_stride
         ENDIF         
      ENDIF
      IF cross_section eq 1 THEN $
         AXES,XSTEP=xstep,YSTEP=ystep,XMINOR=xminor,YMINOR=yminor,NDECS=2,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)'
   ENDFOR
   COLBAR,COORDS=[5000,1000,25000,1500],TITLE='Anomalies in '+varname+' from daily climatology ('+units+')'
   PSCLOSE,/NOVIEW
ENDIF ELSE BEGIN
   FOR j=0,n_phases-1 DO BEGIN
                                ; Make individual plots for each phase
      psfile='/home/ss901165/idl/mjo_indices/mjo_indices_phase_composites.'+model_name+'.'+varname+'_phase'+STRTRIM(STRING(phases(j)),1)+'of'+STRTRIM(STRING(n_phases),1)+'.ps'
      PSOPEN,file=psfile,FONT=6,CHARSIZE=180,MARGIN=2000,SPACE2=2500,XOFFSET=1000,YOFFSET=3500,TFONT=2,$
             TCHARSIZE=90,SPACE3=900,YSIZE=10000
      IF color_rev eq 0 THEN $
         CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_anom)+1,white=[white_num]
      IF color_rev eq 1 THEN $
         CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_anom)+1,/REV,white=[white_num]
      LEVS,MANUAL=mylevs_anom
      IF cross_section eq 1 THEN BEGIN
         GSET,XMIN=our_lon_range(0),XMAX=our_lon_range(1),YMIN=MAX(ABS(vertical)),YMAX=MIN(ABS(vertical))
         y_coord=ABS(vertical)
      ENDIF ELSE BEGIN
         MAP,LATMIN=MIN(latitude),LATMAX=MAX(latitude),LONMIN=our_lon_range(0),LONMAX=our_lon_range(1)
         y_coord=latitude
      ENDELSE
      IF KEYWORD_SET(mask_value) THEN BEGIN
         phase_anomalies_plot=REFORM(phase_anomalies(*,*,j))
         phase_anomalies_plot[where(mask eq mask_value)]=!Values.F_NaN
      ENDIF ELSE $
         phase_anomalies_plot=REFORM(phase_anomalies(*,*,j))
      IF j eq 0 THEN BEGIN
         CON,X=longitude,Y=y_coord,FIELD=REFORM(phase_anomalies_plot(*,*)),/NOLINELABELS,$
                                ;TITLE='Phase composite of '+varname+' for MJO phase '+STRTRIM(STRING(j+1),1)+' from '+model_name+' over '+$
                                ;STRTRIM(STRING(n_strong_days(j)),1)+' days ('+STRTRIM(STRING(our_nyear),1)+' years)',$
             POSITIVE_STYLE=2,NEGATIVE_STYLE=1,CB_TITLE='Composite anomaly in '+varname+' ('+units+')',CB_WIDTH=120
      ENDIF ELSE $
          CON,X=longitude,Y=y_coord,FIELD=REFORM(phase_anomalies_plot(*,*)),/NOLINELABELS,$
                                ;TITLE='Phase composite of '+varname+' for MJO phase '+STRTRIM(STRING(j+1),1)+' from '+model_name+' over '+$
                                ;STRTRIM(STRING(n_strong_days(j)),1)+' days ('+STRTRIM(STRING(our_nyear),1)+' years)',$
              POSITIVE_STYLE=2,NEGATIVE_STYLE=1,CB_TITLE='Composite anomaly in '+varname+' ('+units+')',/NOCOLBAR
      IF plot_vector eq 1 THEN BEGIN
         IF vector_direction eq 'u' THEN BEGIN
            zeroes=fltarr(n_lon,n_lat)
            zeroes(*,*)=0.
            VECT,U=REFORM(phase_anomalies(*,*,j)),V=zeroes,X=longitude,Y=latitude,TYPE=4,MAG=vector_ref_mag,MUNITS=' m s!U-1!N',$
                 STRIDE=vector_stride
         ENDIF ELSE IF vector_direction eq 'v' THEN BEGIN
            zeroes=fltarr(n_lon,n_lat)
            zeroes(*,*)=0.
            VECT,V=REFORM(phase_anomalies(*,*,j)),U=zeroes,X=longitude,Y=latitude,TYPE=4,MAG=vector_ref_mag,MUNITS=' m s!U-1!N',$
                 STRIDE=vector_stride
         ENDIF         
      ENDIF
      IF cross_section eq 1 THEN $
         AXES,XSTEP=xstep,YSTEP=ystep,XMINOR=xminor,YMINOR=yminor,NDECS=2,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)'
      PSCLOSE
   ENDFOR
ENDELSE

;phase_anomalies_mean=fltarr(n_lon,n_lat)
;FOR i=0,n_lon-1 DO $
;   FOR j=0,n_lat-1 DO $
;      phase_anomalies_mean(i,j)=MEAN(phase_anomalies(i,j,*),/NaN)
;
;psfile='/home/ss901165/idl/mjo_indices/mjo_indices_phase_composites.'+model_name+'.'+varname+'_mean_allphases.ps'
;PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1000,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=8000
;CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_anom)+1
;IF color_rev eq 0 THEN $
;   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_anom)+1
;IF color_rev eq 1 THEN $
;   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_anom)+1,/REV
;LEVS,MANUAL=mylevs_anom
;IF cross_section eq 1 THEN BEGIN
;   GSET,XMIN=0.,XMAX=360.,YMIN=MAX(latitude),YMAX=MIN(latitude)
;ENDIF ELSE $
;   MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3)
;CON,X=longitude,Y=latitude,FIELD=phase_anomalies_mean,/NOLINELABELS,$
;    TITLE='Mean anomaly in '+varname+' for all MJO phases from '+model_name+' over '+$
;    STRTRIM(STRING(TOTAL(n_strong_days)),1)+' days ('+STRTRIM(STRING(our_nyear),1)+' years)',$
;    POSITIVE_STYLE=2,NEGATIVE_STYLE=2
;IF plot_vector eq 1 THEN BEGIN
;   IF vector_direction eq 'u' THEN BEGIN
;      zeroes=fltarr(n_lon,n_lat)
;      zeroes(*,*)=0.
;      VECT,U=phase_anomalies_mean,V=zeroes,X=longitude,Y=latitude,TYPE=4,MAG=vector_ref_mag,MUNITS=' m s!U-1!N',$
;           STRIDE=vector_stride
;   ENDIF ELSE BEGIN
;      zeroes=fltarr(n_lon,n_lat)
;      zeroes(*,*)=0.
;      VECT,V=phase_anomalies_mean,U=zeroes,X=longitude,Y=latitude,TYPE=4,MAG=vector_ref_mag,MUNITS=' m s!U-1!N',$
;           STRIDE=vector_stride
;   ENDELSE
;ENDIF
;IF cross_section eq 1 THEN $
;   AXES,XSTEP=xstep,YSTEP=ystep,XMINOR=xminor,YMINOR=yminor,NDECS=2
;PSCLOSE,/NOVIEW
;
STOP

END
