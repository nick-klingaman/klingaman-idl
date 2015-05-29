PRO hadgem3_monwg_isv_metrics_horizontal_variance_sst

low_limit=30
high_limit=50
low_limit_str=STRTRIM(STRING(low_limit),1)
high_limit_str=STRTRIM(STRING(high_limit),1)

tmi_n96_infile='/home/ss901165/datasets/TMI_AMSRE/n96/tmi_fusion.jan-dec_dmeans.1998-2008.n96.anom_filter_'+low_limit_str+high_limit_str+'.nc'
tmi_n96_clim_infile='/home/ss901165/datasets/TMI_AMSRE/n96/tmi_fusion.jan-dec_dmeans_clim.1998-2008.n96.nc'

ahsaf_infile='/home/ss901165/um_output3/hadgem3_monwg/ahsaf/ahsaf.surftemp.daily_20years.anom_filter_'+low_limit_str+high_limit_str+'.nc'
ahsaf_clim_infile='/home/ss901165/um_output3/hadgem3_monwg/ahsaf/ahsaf.surftemp.clim.daily_20years.nc'

aitcb_infile='/home/ss901165/um_output3/hadgem3_monwg/aitcb/aitcb.jan-dec_dmeans.h9-j8.sst_filter_'+low_limit_str+high_limit_str+'.nc'
aitcb_clim_infile='/home/ss901165/um_output3/hadgem3_monwg/aitcb/aitcb.jan-dec_dmeans_clim.h9-j8.sst.nc'

aitke_infile='/home/ss901165/um_output3/hadgem3_monwg/aitke/aitke.jan-dec_dmeans.i0-j2.sst_filter_'+low_limit_str+high_limit_str+'.nc'
aitke_clim_infile='/home/ss901165/um_output3/hadgem3_monwg/aitke/aitke.jan-dec_dmeans_clim.i0-j2.sst.nc'

aitkh_infile='/home/ss901165/um_output3/hadgem3_monwg/aitkh/aitkh.jan-dec_dmeans.h9-i8.sst_filter_'+low_limit_str+high_limit_str+'.nc'
aitkh_clim_infile='/home/ss901165/um_output3/hadgem3_monwg/aitkh/aitkh.jan-dec_dmeans_clim.h9-i8.sst.nc'

mask_n96_infile='/home/ss901165/um_output/mask_n96.nc'

box=[-30,41,30,179]
box_read=[-30,40,30,180]
; For 30-50 day
mylevs=['0.03','0.06','0.09','0.12','0.15','0.18','0.21','0.24','0.27','0.30','0.33','0.36','0.39','0.42']
mylevs_scaled=['0.07','0.14','0.21','0.28','0.35','0.42','0.49','0.56','0.63','0.70','0.77','0.84','0.91','0.98','1.05']
mylevs_ratio=['0.20','0.25','0.33','0.40','0.50','0.66','0.75','0.83','0.90','1.00','1.11','1.20','1.33','1.50','2.00','3.00','4.00','5.00']
; For 10-20 day
;mylevs=['0.7','1.4','2.1','2.8','3.5','4.2','4.9','5.6','6.3','7.0','7.7','8.4','9.1','9.8']
;mylevs_scaled=['0.1','0.2','0.3','0.4','0.5','0.6','0.7','0.8','0.9','1.0','1.1','1.2','1.3']
; For 2-120 day
;mylevs=['3.0','4.5','6.0','7.5','9.0','10.5','12.0','13.5','15.0','16.5','18.0','19.5','21.0']
;mylevs_scaled=['0.3','0.6','0.9','1.2','1.5','1.8','2.1','2.4','2.7','3.0','3.3','3.6','3.9','4.2']
n_sets=5

FOR i=0,n_sets-1 DO BEGIN
    CASE i OF 
       0: BEGIN          
          infile=tmi_n96_infile
          clim_infile=tmi_n96_clim_infile
          mask_infile=mask_n96_infile
          varname='sst'
          compare_to_obs=0
          plot_title='TMI (N96, 1999-2008)'
          psfile_title='tmi'
          filter_start_read=181-high_limit/2
          fived=0
          clim_start_read=181
          clim_fourd=0
          n_days=62
          n_years=10
          multiplier=1
          show_plot=0
       END
       1: BEGIN
          infile=ahsaf_infile
          clim_infile=ahsaf_clim_infile
          compare_to_obs=1
          varname='temp'
          obs_infile=tmi_n96_infile
          obs_clim_infile=tmi_n96_clim_infile
          obs_filter_start_read=91-high_limit/2
          obs_clim_start_read=91
          obs_clim_fourd=0
          obs_ndays=62
          obs_nyears=10
          obs_multiplier=1
          reverse_obslat=1
          plot_title='HadGEM3-AO (ahsaf, N96, w, 30 years)'
          psfile_title='ahsaf'
          clim_start_read=180
          clim_fourd=1
          filter_start_read=90-high_limit/2
          fived=0
          n_days=60
          n_years=20
          multiplier=1.
          show_plot=0
       END
       2: BEGIN
          infile=aitcb_infile
          clim_infile=aitcb_clim_infile
          compare_to_obs=1
          obs_infile=tmi_n96_infile
          obs_clim_infile=tmi_n96_clim_infile
          plot_title='HadGEM3-AO (aitcb, N96, w, 20 years)'
          psfile_title='aitcb'
          clim_start_read=180
          clim_fourd=1
          filter_start_read=180-high_limit/2
          n_days=60
          n_years=20
          multiplier=1.
          show_plot=0
       END
       3: BEGIN
          infile=aitke_infile
          clim_infile=aitke_clim_infile
          compare_to_obs=1
          obs_infile=tmi_n96_infile
          obs_clim_infile=tmi_n96_clim_infile
          plot_title='HadGEM3-AO (aitke, N96, w, 13 years)'
          psfile_title='aitke'
          clim_start_read=180
          clim_fourd=1
          filter_start_read=180-high_limit/2
          n_days=60
          n_years=13
          multiplier=1.
          show_plot=0
       END
       4: BEGIN
          infile=aitkh_infile
          clim_infile=aitkh_clim_infile
          compare_to_obs=1
          obs_infile=tmi_n96_infile
          obs_clim_infile=tmi_n96_clim_infile
          plot_title='HadGEM3-AO (aitkh, N96, w, 10 years)'
          psfile_title='aitkh'
          clim_start_read=180
          clim_fourd=1
          filter_start_read=180-high_limit/2
          n_days=60
          n_years=10
          multiplier=1.
          show_plot=0
       END
    ENDCASE
    
    longitude=OPEN_AND_EXTRACT(infile,'longitude')
    latitude=OPEN_AND_EXTRACT(infile,'latitude')
    DEFINE_BOUNDARIES,box_read,latitude,longitude,box_tx,/LIMIT
    n_lat=N_ELEMENTS(latitude)
    n_lon=N_ELEMENTS(longitude)

    mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
    mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
    DEFINE_BOUNDARIES,box_read,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
    mask_nlat=N_ELEMENTS(mask_latitude)
    mask_nlon=N_ELEMENTS(mask_longitude)
    mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                          offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                          count=[mask_nlon,mask_nlat,1,1]))
    print,i

    ;print,i,latitude,longitude
                                ; Read filtered rainfall for each year
    allyears_sst_filtered=fltarr(n_lon,n_lat,n_days*n_years)    
    FOR j=0,n_years-1 DO BEGIN
       IF fived eq 1 THEN BEGIN
          allyears_sst_filtered(*,*,j*n_days:(j+1)*n_days-1)=$
             REFORM(OPEN_AND_EXTRACT(infile,varname,$
                                     offset=[box_tx(1),box_tx(0),0,j,filter_start_read],$
                                     count=[n_lon,n_lat,1,1,n_days]))*multiplier
       ENDIF ELSE BEGIN
          allyears_sst_filtered(*,*,j*n_days:(j+1)*n_days-1)=$
             REFORM(OPEN_AND_EXTRACT(infile,varname,$
                                     offset=[box_tx(1),box_tx(0),j,filter_start_read],$
                                     count=[n_lon,n_lat,1,n_days]))*multiplier        
       ENDELSE
    ENDFOR
                                ; Read climatological rainfall and
                                ; take mean over season
    IF clim_fourd eq 0 THEN BEGIN
       clim_sst=REFORM(OPEN_AND_EXTRACT(clim_infile,varname,$
                                           offset=[box_tx(1),box_tx(0),clim_start_read],$
                                           count=[n_lon,n_lat,n_days]))*multiplier
    ENDIF ELSE $
       clim_sst=REFORM(OPEN_AND_EXTRACT(clim_infile,varname,$
                                           offset=[box_tx(1),box_tx(0),0,clim_start_read],$
                                           count=[n_lon,n_lat,1,n_days]))*multiplier

    clim_sst_seasmean=fltarr(n_lon,n_lat)
    FOR j=0,n_lon-1 DO BEGIN
       FOR k=0,n_lat-1 DO BEGIN
          clim_sst_seasmean(j,k)=MEAN(clim_sst(j,k,*),/NaN)
       ENDFOR
    ENDFOR
    
    IF compare_to_obs eq 1 THEN BEGIN
       obs_longitude=OPEN_AND_EXTRACT(obs_infile,'longitude')
       obs_latitude=OPEN_AND_EXTRACT(obs_infile,'latitude')
       DEFINE_BOUNDARIES,box_read,obs_latitude,obs_longitude,obs_box_tx,/LIMIT
       obs_nlat=N_ELEMENTS(latitude)
       obs_nlon=N_ELEMENTS(longitude)
       
       obs_allyears_sst_filtered=fltarr(obs_nlon,obs_nlat,obs_ndays*obs_nyears)
       FOR j=0,obs_nyears-1 DO BEGIN
          obs_allyears_sst_filtered(*,*,j*obs_ndays:(j+1)*obs_ndays-1)=$
             REFORM(OPEN_AND_EXTRACT(obs_infile,'sst',$
                                     offset=[obs_box_tx(1),obs_box_tx(0),j,obs_filter_start_read],$
                                     count=[obs_nlon,obs_nlat,1,obs_ndays]))*obs_multiplier
       ENDFOR
       IF obs_clim_fourd eq 0 THEN BEGIN
          obs_clim_sst=REFORM(OPEN_AND_EXTRACT(obs_clim_infile,'sst',$
                                                  offset=[obs_box_tx(1),obs_box_tx(0),obs_clim_start_read],$
                                                  count=[obs_nlon,obs_nlat,obs_ndays]))*obs_multiplier
       ENDIF ELSE $
          obs_clim_sst=REFORM(OPEN_AND_EXTRACT(obs_clim_infile,'sst',$
                                                  offset=[obs_box_tx(1),obs_box_tx(0),0,obs_clim_start_read],$
                                                  count=[obs_nlon,obs_nlat,1,obs_ndays]))*obs_multiplier
       obs_clim_sst_seasmean=fltarr(obs_nlon,obs_nlat)
       FOR j=0,obs_nlon-1 DO BEGIN
          IF reverse_obslat eq 1 THEN BEGIN 
             FOR k=1,obs_nlat-1 DO $
                obs_clim_sst_seasmean(j,obs_nlat-k-1)=MEAN(obs_clim_sst(j,k,*),/NaN)
          ENDIF ELSE BEGIN
             FOR k=1,obs_nlat-1 DO $
                obs_clim_sst_seasmean(j,k)=MEAN(obs_clim_sst(j,k,*),/NaN)
          ENDELSE
       ENDFOR       
    ENDIF

    IF TOTAL(where(allyears_sst_filtered ge 1E20)) gt 0 THEN $
       allyears_sst_filtered[where(allyears_sst_filtered ge 1E20)]=!Values.F_NaN
    
    sst_filtered_variance=fltarr(n_lon,n_lat)
    FOR j=0,n_lon-1 DO BEGIN
       FOR k=0,n_lat-1 DO BEGIN
          IF mask(j,n_lat-k-1) eq 0 THEN BEGIN
             sst_filtered_variance(j,k)=VARIANCE(allyears_sst_filtered(j,k,*),/NaN)
          ENDIF ELSE $
             sst_filtered_variance(j,k)=!Values.F_NaN
       ENDFOR
    ENDFOR

    IF compare_to_obs eq 1 THEN BEGIN
       obs_sst_filtered_variance=fltarr(n_lon,n_lat)
       ratio_obs_model_variance=fltarr(n_lon,n_lat)
       ratio_obs_model_variance_scaled=fltarr(n_lon,n_lat)
       IF TOTAL(where(obs_allyears_sst_filtered ge 1E20)) gt 0 THEN $
          obs_allyears_sst_filtered[where(obs_allyears_sst_filtered ge 1E20)]=!Values.F_NaN
       FOR j=0,n_lon-1 DO BEGIN
          FOR k=0,n_lat-1 DO BEGIN             
             IF reverse_obslat eq 1 THEN BEGIN
                obs_sst_filtered_variance(j,obs_nlat-k-1)=VARIANCE(obs_allyears_sst_filtered(j,k,*),/NaN)             
             ENDIF ELSE $
                obs_sst_filtered_variance(j,k)=VARIANCE(obs_allyears_sst_filtered(j,k,*),/NaN)
          ENDFOR
       ENDFOR
       FOR j=0,n_lon-1 DO BEGIN
          FOR k=0,n_lat-1 DO BEGIN
             ratio_obs_model_variance(j,k)=SQRT(sst_filtered_variance(j,k))/SQRT(obs_sst_filtered_variance(j,k))
             ratio_obs_model_variance_scaled(j,k)=SQRT(sst_filtered_variance(j,k))/clim_sst_seasmean(j,k)/$
                                                  (SQRT(obs_sst_filtered_variance(j,k))/obs_clim_sst_seasmean(j,k))
          ENDFOR
       ENDFOR
    ENDIF

             
    psfile='/home/ss901165/idl/hadgem3_monwg/isv_metrics/hadgem3_monwg_isv_metrics_horizontal_variance_sst.'+psfile_title+'_'+low_limit_str+high_limit_str+'.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100
    CS,IDL=3,NCOLS=N_ELEMENTS(mylevs)+1,/REV
    MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
    LEVS,MANUAL=mylevs
    CON,FIELD=SQRT(sst_filtered_variance),X=longitude,Y=latitude,$
      TITLE='Std. dev. in '+low_limit_str+'-'+high_limit_str+' day filtered rainfall (mm/day) - '+plot_title,$
      /NOLINES
    IF (show_plot eq 1) THEN BEGIN
       PSCLOSE
    ENDIF ELSE $
       PSCLOSE,/NOVIEW    

    scaled=SQRT(sst_filtered_variance)/clim_sst_seasmean
   ; scaled[where(clim_sst_seasmean le 1)]=!Values.F_NaN

    psfile='/home/ss901165/idl/hadgem3_monwg/isv_metrics/hadgem3_monwg_isv_metrics_horizontal_variance_sst.'+psfile_title+'_'+low_limit_str+high_limit_str+'_scaled.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100
    CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_scaled)+1
    MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
    LEVS,MANUAL=mylevs_scaled
    CON,FIELD=scaled,X=longitude,Y=latitude,$
      TITLE='Std. dev. in '+low_limit_str+'-'+high_limit_str+' day filtered rainfall (mm/day) scaled by mean - '+plot_title,$
      /NOLINES
    IF (show_plot eq 1) THEN BEGIN
       PSCLOSE
    ENDIF ELSE $
       PSCLOSE,/NOVIEW   

    IF compare_to_obs eq 1 THEN BEGIN      

       psfile='/home/ss901165/idl/hadgem3_monwg/isv_metrics/hadgem3_monwg_isv_metrics_horizontal_variance_sst.ratio-'+psfile_title+'-obs_'+low_limit_str+high_limit_str+'.ps'
       PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100
       CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio)+1,/REV
       MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
       LEVS,MANUAL=mylevs_ratio
       CON,FIELD=ratio_obs_model_variance,X=longitude,Y=latitude,$
           TITLE='Ratio of std. dev. in '+low_limit_str+'-'+high_limit_str+' day filtered rainfall (mm/day) - '+plot_title+' divided by TMI',$
           /NOLINES
       IF (show_plot eq 1) THEN BEGIN
          PSCLOSE
       ENDIF ELSE $
          PSCLOSE,/NOVIEW    

       psfile='/home/ss901165/idl/hadgem3_monwg/isv_metrics/hadgem3_monwg_isv_metrics_horizontal_variance_sst.ratio-'+psfile_title+'-obs_'+low_limit_str+high_limit_str+'_scaled.ps'
       PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100
       CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio)+1,/REV
       MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
       LEVS,MANUAL=mylevs_ratio
       CON,FIELD=ratio_obs_model_variance_scaled,X=longitude,Y=latitude,$
           TITLE='Ratio of std. dev. in '+low_limit_str+'-'+high_limit_str+' day filtered rainfall (mm/day) scaled by mean - '+plot_title+' divided by TMI',$
           /NOLINES
       IF (show_plot eq 1) THEN BEGIN
          PSCLOSE
       ENDIF ELSE $
          PSCLOSE,/NOVIEW    

    ENDIF

ENDFOR

STOP

END
