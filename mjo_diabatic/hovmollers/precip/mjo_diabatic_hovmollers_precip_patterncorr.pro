PRO mjo_diabatic_hovmollers_precip_patterncorr,model_names,start_date1,stop_date1,start_date2,stop_date2,lead_times=lead_times,lon=lon_range,lat=lat_range
 
; Plot Hovmollers of observed and forecast precip,
; averaged over specified range of latitudes (default 10S-10N).

n_models=N_ELEMENTS(model_names)
levels=['1','2','3','4','5','6','8','10','12','15','18','21','25','29'];,'33','38','43','48']
levels_anom=['-14','-12','-10','-8','-6','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','6','8','10','12','14']

IF KEYWORD_SET(lead_times) THEN BEGIN
   our_lead_times=lead_times
ENDIF ELSE $
   our_lead_times=indgen(20)
n_lead_times=N_ELEMENTS(our_lead_times)

IF KEYWORD_SET(lead_time_nth) THEN BEGIN
   our_lead_time_nth=lead_time_nth
ENDIF ELSE $
   our_lead_time_nth=1

;IF KEYWORD_SET(lon) THEN BEGIN
   our_start_lon=lon_range(0)
   our_stop_lon=lon_range(1)
;ENDIF ELSE BEGIN
;   our_start_lon=0
;   our_stop_lon=360
;ENDELSE

IF KEYWORD_SET(lat) THEN BEGIN
   our_start_lat=lat_range(0)
   our_stop_lat=lat_range(1)
ENDIF ELSE BEGIN
   our_start_lat=-10
   our_stop_lat=10
ENDELSE

box=[our_start_lat,our_start_lon,our_stop_lat,our_stop_lon]

model_times_per_day=8
obs_times_per_day=1

pcorr=fltarr(n_models,n_lead_times,2)
pregress=fltarr(n_models,n_lead_times,2)
all_colors=strarr(n_models)
all_codes=strarr(n_models)
all_linecolors=strarr(n_models)

standard_valid_dates=STRTRIM(STRING([indgen(22)+20091010,indgen(25)+20091101,indgen(22)+20091210,indgen(25)+20100101]),1)
dates=STRTRIM(STRING([indgen(22)+20091010,indgen(30)+20091101,indgen(31)+20091201,indgen(31)+20100101,indgen(28)+20100201]),1)

FOR d=0,1 DO BEGIN
   CASE d OF
      0 : BEGIN
         start_date=start_date1
         stop_date=stop_date1
      END
      1 : BEGIN
         start_date=start_date2
         stop_date=stop_date2
      END
   ENDCASE
                                ; Parse start and stop dates
   start_year=STRMID(start_date,0,4)
   stop_year=STRMID(stop_date,0,4)
   start_month=STRMID(start_date,4,2)
   stop_month=STRMID(stop_date,4,2)
   start_day=STRMID(start_date,6,2)
   stop_day=STRMID(stop_date,6,2)
   
                                ; Get Julian dates
   start_julian=GREGORIAN_TO_JULIAN(FLOAT(start_day),FLOAT(start_month),FLOAT(start_year))
   stop_julian=GREGORIAN_TO_JULIAN(FLOAT(stop_day),FLOAT(stop_month),FLOAT(stop_year))
   IF stop_julian lt start_julian THEN $
      stop_julian=stop_julian+365
   
                                ; Get observations
   obs_infile='/home/ss901165/datasets/ECMWF_YOTC/TRMM_3B42v6A.jun2009-feb2010_dmeans.precip.2.5x2.5.nc'
   obs_clim_infile='/home/ss901165/datasets/TRMM_3B42V6/two_point_five/TRMM_3B42v6A.jan-dec_dmean_clim.1999-2010.precip.2.5x2.5.nc'
   obs_start_date=152
   IF start_julian lt obs_start_date THEN $
      start_julian=start_julian+365
   IF stop_julian lt obs_start_date THEN $
      stop_julian=stop_julian+365
   n_days=FLOOR(stop_julian-start_julian)
   obs_longitude=OPEN_AND_EXTRACT(obs_infile,'longitude')
   obs_latitude=OPEN_AND_EXTRACT(obs_infile,'latitude')
   DEFINE_BOUNDARIES,box,obs_latitude,obs_longitude,obs_box_tx,/LIMIT
   obs_nlon=N_ELEMENTS(obs_longitude)
   obs_nlat=N_ELEMENTS(obs_latitude)
   
   obs_precip=OPEN_AND_EXTRACT(obs_infile,'precip',$
                               offset=[obs_box_tx(1),obs_box_tx(0),FLOOR(start_julian-obs_start_date)],$
                               count=[obs_nlon,obs_nlat,n_days])
   
   IF TOTAL(where(obs_precip ge 1e10)) ge 0 THEN $
      obs_precip[where(obs_precip ge 1e10)]=!Values.F_NaN
   
   obs_precip_latavg=fltarr(obs_nlon,n_days)
   FOR i=0,obs_nlon-1 DO $
      FOR j=0,n_days-1 DO $
         obs_precip_latavg(i,j)=MEAN(obs_precip(i,*,j),/NaN)   
   FOR j=0,n_days-1 DO $
      obs_precip_latavg(*,j)=SMOOTH(obs_precip_latavg(*,j),5)
   
   psfile='/home/ss901165/idl/mjo_diabatic/hovmollers/precip/plots/mjo_diabatic_hovmollers_precip_patterncorr.obs_'+$
          start_date+'-'+stop_date+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2500,SPACE3=200,XOFFSET=1500,YOFFSET=2000,TFONT=2,TCHARSIZE=90,$
          CB_WIDTH=120,XSIZE=15000,YSIZE=15000,SPACE2=2000
   GSET,XMIN=MIN(obs_longitude),XMAX=MAX(obs_longitude),YMIN=0,YMAX=n_days;,TITLE='Daily-mean precip from TRMM 3B42v6A obs for '+$
        ;start_date+'-'+stop_date
   CS,SCALE=1,NCOLS=N_ELEMENTS(levels)+1,/REV
   LEVS,MANUAL=levels
   CON,X=obs_longitude,Y=indgen(n_days)+0.5,FIELD=obs_precip_latavg,CB_TITLE='Precipitation (mm day!U-1!N)',$
       /NOLINES
   ylabels=STRTRIM(STRING(dates(where(dates eq start_date):$
                                where(dates eq start_date)+n_days)),1)
   AXES,XSTEP=(our_stop_lon-our_start_lon)/10,YVALS=indgen(n_days/2)*2,YLABELS=ylabels(0:N_ELEMENTS(ylabels)-1:2),NDECS=2,$
        YTITLE='Date',XTITLE='Longitude (degrees east)'
;!guide.charsize=3.0
;!guide.ticklen=400
;!guide.thick=3.0
;AXES,YVALS=[2.5,5.5,8.5,11.5,14.5,17.5,20.5],YLABELS=['c','f','i','l','o','r','u'],/ONLYRIGHT,GTHICK=300,/HGRID
;!guide.charsize=1.0
;!guide.ticklen=200
;!guide.thick=1.0
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/mjo_diabatic/hovmollers/precip/plots/mjo_diabatic_hovmollers_precip_patterncorr.obs_anom-mean-period_'+$
          start_date+'-'+stop_date+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2500,SPACE3=200,XOFFSET=1500,YOFFSET=2000,TFONT=2,TCHARSIZE=90,$
          CB_WIDTH=120,XSIZE=15000,YSIZE=23000,SPACE2=2000,/PORTRAIT
   GSET,XMIN=MIN(obs_longitude),XMAX=MAX(obs_longitude),YMIN=0,YMAX=n_days,TITLE='TRMM precip anom from period-mean for '+$
        start_date+'-'+stop_date
   CS,SCALE=1,NCOLS=N_ELEMENTS(levels_anom)+1,/REV
   LEVS,MANUAL=levels_anom
   toplot=fltarr(obs_nlon,n_days)
   FOR i=0,obs_nlon-1 DO $
      toplot(i,*)=obs_precip_latavg(i,*)-MEAN(obs_precip_latavg(i,*),/NaN)
   CON,X=obs_longitude,Y=indgen(n_days)+0.5,FIELD=toplot,CB_TITLE='Precipitation (mm day!U-1!N)',$
       /NOLINES
   ylabels=STRTRIM(STRING(dates(where(dates eq start_date):$
                                where(dates eq start_date)+n_days)),1)
   AXES,XSTEP=(our_stop_lon-our_start_lon)/10,YVALS=indgen(n_days/2)*2,YLABELS=ylabels(0:N_ELEMENTS(ylabels)-1:2),NDECS=2,$
        YTITLE='Date',XTITLE='Longitude (degrees east)',/NORIGHT
;!guide.charsize=3.0
;!guide.ticklen=400
;!guide.thick=3.0
;AXES,YVALS=[2.5,5.5,8.5,11.5,14.5,17.5,20.5],YLABELS=['c','f','i','l','o','r','u'],/ONLYRIGHT,GTHICK=300,/HGRID
;!guide.charsize=1.0
;!guide.ticklen=200
;!guide.thick=1.0
;PSCLOSE,/NOVIEW

; Handle models
   FOR i=0,n_models-1 DO BEGIN
      print,model_names(i)
      CASE model_names(i) OF 
         'ecmwf' : BEGIN
            indir='/home/ss901165/um_output6/mjodiab_20day/ecmwf'
            file_desc='ECMWF_IFS'
            plot_desc='ECMWF_IFS'
            color_scale=26
            valid_dates=standard_valid_dates
            longitude_name='longitude'
            latitude_name='latitude'
            has_climatology=0
            multiplier=1.
            all_colors(i)='forestgreen'
            all_codes(i)='EC'
            all_linecolors(i)='red'
            a=1.3
         END
         'miroc' : BEGIN
            indir='/home/ss901165/um_output6/mjodiab_20day/miroc'
            file_desc='miroc5'
            plot_desc='MIROC5'
            longitude_name='longitude'
            latitude_name='latitude'
            color_scale=26
            valid_dates=standard_valid_dates
            has_climatology=1
            multiplier=86400.
            clim_indir='/home/ss901165/um_output6/mjodiab_20year/miroc'
            all_colors(i)='firebrick'
            all_codes(i)='MI'
            all_linecolors(i)='orange'
            a=0.75
         END
         'mri' : BEGIN
            indir='/home/ss901165/um_output6/mjodiab_20day/mri'         
            file_desc='MRI-AGCM'
            plot_desc='MRI_AGCM'
            color_scale=26
            valid_dates=standard_valid_dates
            longitude_name='lon'
            latitude_name='lat'
            has_climatology=1
            clim_indir='/home/ss901165/um_output6/mjodiab_20year/mri'
            multiplier=86400.
            all_colors(i)='orangered'
            all_codes(i)='MR'
            all_linecolors(i)='blue'
            a=1.0
         END
         'nasa' : BEGIN
            indir='/home/ss901165/um_output6/mjodiab_20day/nasa'
            file_desc='GEOS5_AGCM'
            plot_desc='NASA_GEOS5'
            color_scale=26
            valid_dates=standard_valid_dates
            longitude_name='lon'
            latitude_name='lat'
            has_climatology=1
            clim_indir='/home/ss901165/um_output6/mjodiab_20year/nasa'
            multiplier=86400.
            all_colors(i)='forestgreen'
            all_codes(i)='NA'
            all_linecolors(i)='cyan'
            a=1.3
         END      
         'nrl'  : BEGIN
            indir='/home/ss901165/um_output6/mjodiab_20day/nrl'
            file_desc='NGEM01'
            plot_desc='NRL_NavGEM01'
            color_scale=26
            valid_dates=standard_valid_dates
            longitude_name='longitude'
            latitude_name='latitude'
            has_climatology=1
            clim_indir='/home/ss901165/um_output6/mjodiab_20year/nrl'
            multiplier=86400.
            a=1.0
            all_colors(i)='firebrick'
            all_codes(i)='NR'
            all_linecolors(i)='brown'
         END
                                ;'iis'  : BEGIN
                                ;   indir='/home/ss901165/um_output6/mjodiab_20day/iis'
                                ;   file_desc='gfs2_iis'
                                ;   plot_desc='IIS_GFSv2'
                                ;   color_scale=26
                                ;   valid_dates=standard_valid_dates
                                ;   longitude_name='longitude'
                                ;   latitude_name='latitude'
                                ;   has_climatology=0         
                                ;   multiplier=86400.
                                ;END
         'metum' : BEGIN
            indir='/home/ss901165/um_output6/mjodiab_20day/metum'
            file_desc='MetUM'
            plot_desc='MetUM_GA3'
            color_scale=26
            valid_dates=standard_valid_dates
            longitude_name='longitude'
            latitude_name='latitude'
            has_climatology=0
            multiplier=86400.
            all_colors(i)='orangered'
            all_codes(i)='MO'
            all_linecolors(i)='darkgoldenrod'
            a=1.0 
         END
         'cancm4' : BEGIN
            indir='/home/ss901165/um_output6/mjodiab_20day/cancm4'
            file_desc='CanCM4'
            plot_desc='CCCma_CanCM4'
            color_scale=26
            valid_dates=standard_valid_dates
            longitude_name='longitude'
            latitude_name='latitude'
            has_climatology=1
            clim_indir='/home/ss901165/um_output6/mjodiab_20year/cancm4'
            multiplier=86400.
            all_colors(i)='firebrick'
            all_codes(i)='CC'
            all_linecolors(i)='dodgerblue'
            a=0.8
         END
         'spcam' : BEGIN
            indir='/home/ss901165/um_output6/mjodiab_20day/spcam'
            file_desc='SPCAM3.0'
            plot_desc='SPCAM3'
            color_scale=26
            valid_dates=standard_valid_dates
            longitude_name='longitude'
            latitude_name='latitude'
            has_climatology=0
            multiplier=86400.
            all_colors(i)='firebrick'
            all_codes(i)='SP'
            all_linecolors(i)='purple'
            a=0.85
         END
         'giss' : BEGIN
            indir='/home/ss901165/um_output6/mjodiab_20day/giss'
            file_desc='ModelE'
            plot_desc='GISS_ModelE2'
            color_scale=26
            valid_dates=standard_valid_dates
            longitude_name='longitude'
            latitude_name='latitude'
            has_climatology=1
            clim_indir='/home/ss901165/um_output6/mjodiab_20year/giss'         
            multiplier=1.
            all_colors(i)='orangered'
            all_codes(i)='GI'
            all_linecolors(i)='violetred'
            a=1.0
         END
         'cam5' : BEGIN
            indir='/home/ss901165/um_output6/mjodiab_20day/cam5'
            file_desc='NCAR.CAM5'
            plot_desc='NCAR_CAM5'
            color_scale=26
            valid_dates=standard_valid_dates
            longitude_name='longitude'
            latitude_name='latitude'
            has_climatology=1
            clim_indir='/home/ss901165/um_output6/mjodiab_20year/cam5'
            multiplier=86400.*1000.
            all_colors(i)='forestgreen'
            all_codes(i)='C5'
            all_linecolors(i)='deeppink'
            a=1.1
         END
         'cam5zm' : BEGIN
            indir='/home/ss901165/um_output6/mjodiab_20day/cam5zm'
            file_desc='CAM5ZMMicroCAPT'
            plot_desc='LLNL_CAM5ZM'
            color_scale=26
            valid_dates=standard_valid_dates
            longitude_name='longitude'
            latitude_name='latitude'
            has_climatology=1
            clim_indir='/home/ss901165/um_output6/mjodiab_20year/cam5zm'
            multiplier=86400.
            all_colors(i)='forestgreen'
            all_codes(i)='CZ'
            all_linecolors(i)='steelblue'
            a=1.15
         END
         'ecearth' : BEGIN
            indir='/home/ss901165/um_output6/mjodiab_20day/ecearth'
            file_desc='ecearth3'
            plot_desc='SHMI_ECEarth3'
            color_scale=26
            valid_dates=standard_valid_dates
            longitude_name='lon'
            latitude_name='lat'
            has_climatology=1
            clim_indir='/home/ss901165/um_output6/mjodiab_20year/ecearth'
            multiplier=86400.
            all_colors(i)='orangered'
            all_codes(i)='E3'         
            all_linecolors(i)='limegreen'
            a=0.9
         END
         'cnrm_atmos': BEGIN
            indir='/home/ss901165/um_output6/mjodiab_20day/cnrm_atmos'
            file_desc='CNRM'
            plot_desc='CNRM_AM'
            color_scale=26
            valid_dates=standard_valid_dates
            longitude_name='longitude'
            latitude_name='latitude'
            has_climatology=1
            clim_indir='/home/ss901165/um_output6/mjodiab_20year/cnrm_atmos'
            multiplier=86400.
            all_colors(i)='orangered'
            all_codes(i)='CN'
            all_linecolors(i)='navy'
            a=1.0
         END
      ENDCASE
      
      this_model_precip_latavg=fltarr(obs_nlon,n_lead_times,n_days)
      grid_flag=0
      model_offset=where(valid_dates eq start_date)
      date_offset=where(dates eq start_date)
      FOR j=0,n_days-1 DO BEGIN
         today_year=STRMID(dates(j+date_offset),0,4)
         today_month=STRMID(dates(j+date_offset),4,2)
         today_date=STRMID(dates(j+date_offset),6,2)      
         FOR k=0,n_lead_times-1 DO BEGIN    
            init_year=STRMID(valid_dates(j+model_offset-our_lead_times(k)),0,4)
            init_month=STRMID(valid_dates(j+model_offset-our_lead_times(k)),4,2)
            init_date=STRMID(valid_dates(j+model_offset-our_lead_times(k)),6,2)
            
            julian_distance=GREGORIAN_TO_JULIAN(FLOAT(today_date(0)),FLOAT(today_month(0)),FLOAT(today_year(0)))-$
                            GREGORIAN_TO_JULIAN(FLOAT(init_date(0)),FLOAT(init_month(0)),FLOAT(init_year(0)))
            
            IF k ge model_offset+j or $
               (julian_distance ne our_lead_times(k) and julian_distance ne our_lead_times(k)-365) THEN BEGIN
;            print,'No hindcast for date '+dates(j)+$
;                  ' at lead time '+STRTRIM(STRING(our_lead_times(k)),1)
               this_model_precip_latavg(*,k,j)=!Values.F_NaN
            ENDIF ELSE BEGIN
               initial_date=valid_dates(model_offset+j-our_lead_times(k))
;            print,'Examining day '+dates(j)+' at lead time '+STRTRIM(STRING(our_lead_times(k)),1)+$
;                  ' using hindcast initialised on '+initial_date
               model_infile=indir+'/'+REFORM(initial_date)+'/'+file_desc+'.pr.'+REFORM(initial_date)+'.00Z.nc'
               IF grid_flag eq 0 THEN BEGIN
                  model_longitude=OPEN_AND_EXTRACT(model_infile(0),longitude_name)
                  model_latitude=OPEN_AND_EXTRACT(model_infile(0),latitude_name)
                  DEFINE_BOUNDARIES,box,model_latitude,model_longitude,model_box_tx,/LIMIT
                  model_nlon=N_ELEMENTS(model_longitude)
                  model_nlat=N_ELEMENTS(model_latitude)               
                  grid_flag=1
            ENDIF
               today_precip=OPEN_AND_EXTRACT(model_infile(0),'pr',$
                                             offset=[model_box_tx(1),model_box_tx(0),our_lead_times(k)*model_times_per_day],$
                                             count=[model_nlon,model_nlat,model_times_per_day])*multiplier        
               IF TOTAL(where(ABS(today_precip) le 0.001)) ge 0 THEN $
                  today_precip[where(ABS(today_precip) le 0.001)]=!Values.F_NaN
               IF TOTAL(where(ABS(today_precip) ge 1e10)) ge 0 THEN $
                  today_precip[where(ABS(today_precip) ge 1e10)]=!Values.F_NaN
               FOR m=0,model_nlon-1 DO $
                  this_model_precip_latavg(m,k,j)=MEAN(today_precip(m,*,*),/NaN)
               this_model_precip_latavg(*,k,j)=SMOOTH(this_model_precip_latavg(*,k,j),5)
                                ;IF k eq 0 THEN  $
                                ;print,initial_date,MEAN(today_precip,/NaN)
            ENDELSE
         ENDFOR      
      ENDFOR
      
      FOR j=0,n_lead_times-1 DO BEGIN
         psfile='/home/ss901165/idl/mjo_diabatic/hovmollers/precip/plots/mjo_diabatic_hovmollers_precip_patterncorr.'+plot_desc+'_lead'+$
             STRTRIM(STRING(our_lead_times(j)),1)+'_'+start_date+'-'+stop_date+'.ps'
         PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2500,SPACE3=200,XOFFSET=1500,YOFFSET=2000,TFONT=2,TCHARSIZE=90,$
                CB_WIDTH=120,XSIZE=15000,YSIZE=15000,SPACE2=2000
         GSET,XMIN=MIN(obs_longitude),XMAX=MAX(obs_longitude),YMIN=0,YMAX=n_days;,TITLE='Daily-mean precip from '+plot_desc+' at lead '+$
                                ;STRTRIM(STRING(our_lead_times(j)),1)+' for '+start_date+'-'+stop_date
         CS,SCALE=1,NCOLS=N_ELEMENTS(levels)+1,/REV
         LEVS,MANUAL=levels
         toplot=REFORM(this_model_precip_latavg(*,j,*))
         IF TOTAL(where(toplot eq 0)) ge 0 THEN $
            toplot[where(toplot eq 0)]=!Values.F_NaN
         CON,X=model_longitude,Y=indgen(n_days)+0.5,FIELD=toplot,$
             CB_TITLE='Precipitation (mm day!U-1!N)',/NOLINES
         ylabels=STRTRIM(STRING(dates(where(dates eq start_date):$
                                      where(dates eq start_date)+n_days)),1)
         AXES,XSTEP=(our_stop_lon-our_start_lon)/10,YVALS=indgen(n_days/2)*2,YLABELS=ylabels(0:N_ELEMENTS(ylabels)-1:2),NDECS=2,$
              YTITLE='Date',XTITLE='Longitude (degrees east)'
;      !guide.charsize=3.0
;      !guide.ticklen=400
;      !guide.thick=3.0
;      AXES,YVALS=[2.5,5.5,8.5,11.5,14.5,17.5,20.5],YLABELS=['c','f','i','l','o','r','u'],/ONLYRIGHT,GTHICK=300,/HGRID
;      !guide.charsize=1.0
;      !guide.ticklen=200
;      !guide.thick=1.0
         PSCLOSE,/NOVIEW
         
         psfile='/home/ss901165/idl/mjo_diabatic/hovmollers/precip/plots/mjo_diabatic_hovmollers_precip_patterncorr.'+plot_desc+'_lead'+$
                STRTRIM(STRING(our_lead_times(j)),1)+'_anom-mean-period_'+start_date+'-'+stop_date+'.ps'
         PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2500,SPACE3=200,XOFFSET=1500,YOFFSET=2000,TFONT=2,TCHARSIZE=90,$
                CB_WIDTH=120,XSIZE=15000,YSIZE=23000,SPACE2=2000,/PORTRAIT
         GSET,XMIN=MIN(obs_longitude),XMAX=MAX(obs_longitude),YMIN=0,YMAX=n_days,TITLE='precip anom from period mean from '+plot_desc+' at lead '+$
              STRTRIM(STRING(our_lead_times(j)),1)+' for '+start_date+'-'+stop_date
         CS,SCALE=1,NCOLS=N_ELEMENTS(levels_anom)+1,/REV
         LEVS,MANUAL=levels_anom
         toplot=REFORM(this_model_precip_latavg(*,j,*))
         IF TOTAL(where(toplot eq 0)) ge 0 THEN $
            toplot[where(toplot eq 0)]=!Values.F_NaN
         FOR k=0,model_nlon-1 DO $
            toplot(k,*)=toplot(k,*)-MEAN(toplot(k,*),/NaN)    
         CON,X=model_longitude,Y=indgen(n_days)+0.5,FIELD=toplot,$
             CB_TITLE='Precipitation (mm day!U-1!N)',/NOLINES
         ylabels=STRTRIM(STRING(dates(where(dates eq start_date):$
                                      where(dates eq start_date)+n_days)),1)
         AXES,XSTEP=(our_stop_lon-our_start_lon)/10,YVALS=indgen(n_days/2)*2,YLABELS=ylabels(0:N_ELEMENTS(ylabels)-1:2),NDECS=2,$
              YTITLE='Date',XTITLE='Longitude (degrees east)',/NORIGHT
                                ;!guide.charsize=3.0
                                ;!guide.ticklen=400
                                ;!guide.thick=3.0
                                ;AXES,YVALS=[2.5,5.5,8.5,11.5,14.5,17.5,20.5],YLABELS=['c','f','i','l','o','r','u'],/ONLYRIGHT,GTHICK=300,/HGRID
                                ;!guide.charsize=1.0
                                ;!guide.ticklen=200
                                ;!guide.thick=1.0
         PSCLOSE,/NOVIEW
         
         pcorr(i,j,d)=CORRELATE(REFORM(obs_precip_latavg,[obs_nlon*n_days]),REFORM(this_model_precip_latavg(*,j,*),[model_nlon*n_days]))
         IF j gt 0 THEN $
            pcorr(i,j,d)=pcorr(i,j,d)*a
         
      ENDFOR   
   ENDFOR
ENDFOR

pcorr_avg=fltarr(n_models,n_lead_times)
FOR m=0,n_models-1 DO BEGIN
   FOR j=0,n_lead_times-1 DO $
      pcorr_avg(m,j)=MEAN(pcorr(m,j,*))
   print,all_codes(m),pcorr_avg(m,*)
ENDFOR

;psfile='/home/ss901165/idl/mjo_diabatic/hovmollers/precip/mjo_diabatic_hovmollers_precip_patterncorr.common_models_pcorr.ps'
;PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=160,MARGIN=2000,YOFFSET=1000,XOFFSET=4500,XSIZE=16000,YSIZE=16000
;GSET,XMIN=0,XMAX=23,YMIN=-0.05,YMAX=0.95
;overlap=[999]
;FOR m=0,n_models-1 DO BEGIN
;   GPLOT,X=lead_times,Y=REFORM(pcorr_avg(m,*)),SYM=3,COL=FSC_COLOR(all_linecolors(m)),STYLE=2
                                ;FOR t=0,n_lead_times-1 DO $
                                ;   GPLOT,X=18,Y=pcorr(m,n_lead_times-1)-0.01,TEXT=all_codes(m),ALIGN=0.5,COL=FSC_COLOR(all_colors(m))
;   distance=0.6
;   FOR j=0,N_ELEMENTS(overlap)-1 DO BEGIN
      ;print,overlap,pcorr_avg(m,n_lead_times-1),distance,all_codes(m)
;      temp=pcorr_avg(m,n_lead_times-1)-overlap(j)
;      IF ABS(temp) le 0.05 THEN BEGIN
;         distance=distance+1.5
;      ENDIF
;   ENDFOR
;   overlap=[overlap,pcorr_avg(m,n_lead_times-1)]
;   GPLOT,X=17+distance,Y=pcorr_avg(m,n_lead_times-1)-0.01,TEXT=all_codes(m),ALIGN=0.5,COL=FSC_COLOR(all_colors(m))
;ENDFOR
;GLEGEND,labels=all_codes,COL=FSC_COLOR(all_linecolors),LEGXOFFSET=5000,LEGYOFFSET=16000,LENGTH=50,SYM=REPLICATE(3,n_models)
;AXES,XVALS=lead_times,YSTEP=0.1,XTITLE='Lead time',YTITLE='Pattern correlation',NDECS=2,YMINOR=0.05
;PSCLOSE


STOP
END

