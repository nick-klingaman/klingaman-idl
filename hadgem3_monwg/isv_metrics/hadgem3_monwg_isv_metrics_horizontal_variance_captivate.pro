PRO hadgem3_monwg_isv_metrics_horizontal_variance_captivate

low_limit=2
high_limit=120
high_limit_m3=120
low_limit_str=STRTRIM(STRING(low_limit),1)
high_limit_str=STRTRIM(STRING(high_limit),1)
high_limit_m3_str=STRTRIM(STRING(high_limit_m3),1)

trmm_n96_infile='/home/ss901165/datasets_mango/TRMM_3B42V6/n96/TRMM_3B42v6A.jan-dec_dmeans.1999-2010.anom_filter_'+low_limit_str+high_limit_str+'.n96.nc'
trmm_n96_clim_infile='/home/ss901165/datasets_mango/TRMM_3B42V6/n96/TRMM_3B42v6A.jan-dec_dmean_clim.1999-2010.n96.nc'

trmm_n216_infile='/home/ss901165/datasets_mango/TRMM_3B42V6/n216/TRMM_3B42v6A.jan-dec_dmeans.1999-2010.anom_filter_'+low_limit_str+high_limit_str+'.n216.nc'
trmm_n216_clim_infile='/home/ss901165/datasets_mango/TRMM_3B42V6/n216/TRMM_3B42v6A.jan-dec_dmeans_clim.1999-2010.n216.nc'

airxv_infile='/home/ss901165/um_output3/hadgem3_monwg/airxv/hadgem3a_morph3_final_n96_amip2_airxv.jan-dec_dmeans_anom_filter'$
             +low_limit_str+high_limit_m3_str+'.1982-2008.precip.nc'
airxv_clim_infile='/home/ss901165/um_output3/hadgem3_monwg/airxv/hadgem3a_morph3_final_n96_amip2_airxv.jan-dec_dmeans_clim.1982-2008.precip.nc'

aicvx_infile='/home/ss901165/um_output3/hadgem3_monwg/aicvx/hadgem3ao_morph3_final_n96_orca1_aicvx.jan-dec_dmeans_anom_filter'$
             +low_limit_str+high_limit_m3_str+'.years1-30.precip.nc'
aicvx_clim_infile='/home/ss901165/um_output3/hadgem3_monwg/aicvx/hadgem3ao_morph3_final_n96_orca1_aicvx.jan-dec_dmeans_clim.years1-30.precip.nc'

ajpdr_infile='/home/ss901165/um_output3/hadgem3_monwg/ajpdr/hadgem2ao_final_n96_pdctl_ajpdr.jan-dec_dmeans.2029-2078.anom_filter_'$
             +low_limit_str+high_limit_str+'.precip.nc'
ajpdr_clim_infile='/home/ss901165/um_output3/hadgem3_monwg/ajpdr/hadgem2ao_final_n96_pdctl_ajpdr.jan-dec_dmean_clim.2029-2078.precip.nc'

ajtzr_infile='/home/ss901165/um_output3/hadgem3_monwg/ajtzr/hadgem3ao_captivate_n96_orca1_ajtzr.jan-dec_dmeans.1979-2008.anom_filter_'$
             +low_limit_str+high_limit_str+'.precip.nc'
ajtzr_clim_infile='/home/ss901165/um_output3/hadgem3_monwg/ajtzr/hadgem3ao_captivate_n96_orca1_ajtzr.jan-dec_dmean_clim.1979-2008.precip.nc'

xfhhk_infile='/home/ss901165/um_output3/hadgem3_monwg/xfhhk/hadgem3hao_captivate_n216_orca025_xfhhk.jan-dec_dmeans.1980-2059.anom_filter_'$
             +low_limit_str+high_limit_str+'.precip.nc'
xfhhk_clim_infile='/home/ss901165/um_output3/hadgem3_monwg/xfhhk/hadgem3hao_captivate_n216_orca025_xfhhk.jan-dec_dmean_clim.1980-2059.precip.nc'

nick_ctl_infile='/home/ss901165/um_output3/hadgem3a_amip2_control_vn74/hadgem3a_amip2_ctl_vn74.jan-dec_dmeans.years1-30.anom_filter_'$
                +low_limit_str+high_limit_str+'.precip.nc'
nick_ctl_clim_infile='/home/ss901165/um_output3/hadgem3a_amip2_control_vn74/hadgem3a_amip2_ctl_vn74.jan-dec_dmean_clim.years1-30.precip.nc'

nick_entrain_infile='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmeans.years1-30.anom_filter_'$
                    +low_limit_str+high_limit_str+'.precip.nc'
nick_entrain_clim_infile='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmean_clim.years1-30.precip.nc'

box=[-29,41,40,179]
box_read=[-30,40,41,180]
; For 30-50 day
IF low_limit eq 30 and high_limit eq 50 THEN BEGIN
   mylevs=['0.5','1.0','1.5','2.0','2.5','3.0','3.5','4.0','4.5','5.0','5.5','6.0','6.5','7.0']
   mylevs_scaled=['0.07','0.14','0.21','0.28','0.35','0.42','0.49','0.56','0.63','0.70','0.77','0.84','0.91','0.98','1.05']
   mylevs_ratio=['0.20','0.25','0.33','0.40','0.50','0.66','0.75','0.83','0.90','1.00','1.11','1.20','1.33','1.50','2.00','3.00','4.00','5.00']
ENDIF
; For 10-20 day
IF low_limit eq 10 and high_limit eq 30 THEN BEGIN
   mylevs=['0.7','1.4','2.1','2.8','3.5','4.2','4.9','5.6','6.3','7.0','7.7','8.4','9.1','9.8']
   mylevs_scaled=['0.1','0.2','0.3','0.4','0.5','0.6','0.7','0.8','0.9','1.0','1.1','1.2','1.3']
   mylevs_ratio=['0.20','0.25','0.33','0.40','0.50','0.66','0.75','0.83','0.90','1.00','1.11','1.20','1.33','1.50','2.00','3.00','4.00','5.00']
ENDIF
; For 2-120 day
IF low_limit eq 2 and high_limit eq 120 THEN BEGIN
   mylevs=['3.0','4.5','6.0','7.5','9.0','10.5','12.0','13.5','15.0','16.5','18.0','19.5','21.0']
   mylevs_scaled=['0.3','0.6','0.9','1.2','1.5','1.8','2.1','2.4','2.7','3.0','3.3','3.6','3.9','4.2']
   mylevs_ratio=['0.20','0.25','0.33','0.40','0.50','0.66','0.75','0.83','0.90','1.00','1.11','1.20','1.33','1.50','2.00','3.00','4.00','5.00']
ENDIF
mylevs_clim=['1','2','3','4','5','6','7','9','11','13','15','18','21','24','27','30']
mylevs_diff_clim=['-13','-11','-9','-7','-5','-3','-1','1','3','5','7','9','11','13']

n_sets=9

FOR i=0,n_sets-1 DO BEGIN
   print,'Now running for model '+STRTRIM(STRING(i+1),1)+' of '+STRTRIM(STRING(n_sets),1)+' ...'
   CASE i OF 
      0: BEGIN
           infile=trmm_n96_infile
           clim_infile=trmm_n96_clim_infile
           compare_to_obs=0
           plot_title='TRMM (N96, 1999-2010)'
           psfile_title='trmm'
           filter_start_read=151;-high_limit/2
           clim_start_read=151
           clim_fourd=0
           n_days=122
           n_years=12
           multiplier=1
           show_plot=0
           year_dimension_last=0
        END
      1: BEGIN
         infile=trmm_n216_infile
         clim_infile=trmm_n216_clim_infile
         compare_to_obs=0
         plot_title='TRMM (N216, 1999-2010)'
         psfile_title='trmm_n216'
         clim_start_read=151
         clim_fourd=0
         filter_start_read=151
         n_days=122
         n_years=12
         multiplier=1
         show_plot=0
         year_dimension_last=0
      END      
      2: BEGIN
         infile=xfhhk_infile
         clim_infile=xfhhk_clim_infile
         compare_to_obs=1
         obs_infile=trmm_n216_infile
         obs_clim_infile=trmm_n216_clim_infile
         obs_nyears=12
         obs_ndays=122
         obs_filter_start_read=151 ;-high_limit/2
         obs_clim_start_read=151
         obs_multiplier=1.
         obs_clim_fourd=0
         reverse_obslat=0
         plot_title='HadGEM3-A MORPH3 Final Assessment (xfhhk, N216, 80 years)'
         psfile_title='xfhhk'
         clim_start_read=150
         clim_fourd=0
         filter_start_read=150  ;-high_limit/2
         n_days=120
         n_years=80
         multiplier=86400.
         show_plot=0
         year_dimension_last=1
      END
      3: BEGIN
         infile=airxv_infile
         clim_infile=airxv_clim_infile
         compare_to_obs=1
         obs_infile=trmm_n96_infile
         obs_clim_infile=trmm_n96_clim_infile
         obs_nyears=12
         obs_ndays=122
         obs_filter_start_read=151 ;-high_limit/2
         obs_clim_start_read=151
         obs_multiplier=1.
         obs_clim_fourd=0
         reverse_obslat=0
         plot_title='HadGEM3-A MORPH3 Final Assessment (airxv, N96, 27 years)'
         psfile_title='airxv'
         clim_start_read=150
         clim_fourd=0
         filter_start_read=150  ;-high_limit/2
         n_days=120
         n_years=27
         multiplier=86400.
         show_plot=0
         year_dimension_last=1
      END
      4: BEGIN
         infile=aicvx_infile
         clim_infile=aicvx_clim_infile
         compare_to_obs=1
         obs_infile=trmm_n96_infile
         obs_clim_infile=trmm_n96_clim_infile
         reverse_obslat=0
         plot_title='HadGEM3-AO MORPH3 Final Assessment (aicvx, N96, 30 years)'
         psfile_title='aicvx'
         clim_start_read=150
         clim_fourd=0          
         filter_start_read=150  ;-high_limit/2
         n_days=120
         n_years=30
         multiplier=86400.
         show_plot=0
         year_dimension_last=1
      END
      5: BEGIN
         infile=ajpdr_infile
         clim_infile=ajpdr_clim_infile
         compare_to_obs=1
         obs_infile=trmm_n96_infile
         obs_clim_infile=trmm_n96_clim_infile
         reverse_obslat=1
         plot_title='HadGEM2-AO control for comparison (ajpdr, N96, 50 years)'
         psfile_title='ajpdr'
         clim_start_read=150
         clim_fourd=0
         filter_start_read=150  ;-high_limit/2
         n_days=120
         n_years=49
         multiplier=86400.
         show_plot=0
         year_dimension_last=0
      END
      6: BEGIN
         infile=ajtzr_infile
         clim_infile=ajtzr_clim_infile
         compare_to_obs=1
         obs_infile=trmm_n96_infile
         obs_clim_infile=trmm_n96_clim_infile
         reverse_obslat=1
         plot_title='HadGEM3-AO Captivate Final (ajtzr, N96, 30 years)'
         psfile_title='ajtzr'
         clim_start_read=150
         clim_fourd=0
         filter_start_read=150
         n_days=120
         n_years=29
         multiplier=86400.
         show_plot=0
         year_dimension_last=0
      END
      7: BEGIN
         infile=nick_ctl_infile
         clim_infile=nick_ctl_clim_infile
         compare_to_obs=1
         obs_infile=trmm_n96_infile
         obs_clim_infile=trmm_n96_clim_infile
         reverse_obslat=1
         plot_title='HadGEM3-A clim SST w/ 1.0x entrain (30 years)'
         psfile_title='1.0xentrain'
         clim_start_read=150
         clim_fourd=0
         filter_start_read=150
         n_days=120
         n_years=30
         multiplier=86400.
         show_plot=0
         year_dimension_last=0
      END
      8: BEGIN
         infile=nick_entrain_infile
         clim_infile=nick_entrain_clim_infile
         compare_to_obs=1
         obs_infile=trmm_n96_infile
         obs_clim_infile=trmm_n96_clim_infile
         reverse_obslat=1
         plot_title='HadGEM3-A clim SST w/ 1.5x entrain (30 years)'
         psfile_title='1.5xentrain'
         clim_start_read=150
         clim_fourd=0
         filter_start_read=150
         n_days=120
         n_years=30
         multiplier=86400.
         show_plot=0
         year_dimension_last=0
      END
   ENDCASE
    
    longitude=OPEN_AND_EXTRACT(infile,'longitude')
    latitude=OPEN_AND_EXTRACT(infile,'latitude')
    DEFINE_BOUNDARIES,box_read,latitude,longitude,box_tx,/LIMIT
    n_lat=N_ELEMENTS(latitude)
    n_lon=N_ELEMENTS(longitude)
    ;print,i,latitude,longitude
                                ; Read filtered rainfall for each year
    allyears_precip_filtered=fltarr(n_lon,n_lat,n_days*n_years)    
    IF year_dimension_last eq 1 THEN BEGIN
       FOR j=0,n_years-1 DO BEGIN
          allyears_precip_filtered(*,*,j*n_days:(j+1)*n_days-1)=$
             REFORM(OPEN_AND_EXTRACT(infile,'precip',$
                                     offset=[box_tx(1),box_tx(0),filter_start_read,j],$
                                     count=[n_lon,n_lat,n_days,1]))*multiplier
       ENDFOR
    ENDIF ELSE BEGIN
       FOR j=0,n_years-1 DO BEGIN
          allyears_precip_filtered(*,*,j*n_days:(j+1)*n_days-1)=$
             REFORM(OPEN_AND_EXTRACT(infile,'precip',$
                                     offset=[box_tx(1),box_tx(0),j,filter_start_read],$
                                     count=[n_lon,n_lat,1,n_days]))*multiplier        
       ENDFOR
    ENDELSE
                                ; Read climatological rainfall and
                                ; take mean over season
    IF clim_fourd eq 0 THEN BEGIN
       clim_precip=REFORM(OPEN_AND_EXTRACT(clim_infile,'precip',$
                                           offset=[box_tx(1),box_tx(0),clim_start_read],$
                                           count=[n_lon,n_lat,n_days]))*multiplier
    ENDIF ELSE $
       clim_precip=REFORM(OPEN_AND_EXTRACT(clim_infile,'precip',$
                                           offset=[box_tx(1),box_tx(0),0,clim_start_read],$
                                           count=[n_lon,n_lat,1,n_days]))*multiplier

    clim_precip_seasmean=fltarr(n_lon,n_lat)
    FOR j=0,n_lon-1 DO BEGIN
       FOR k=0,n_lat-1 DO BEGIN
          clim_precip_seasmean(j,k)=MEAN(clim_precip(j,k,*),/NaN)
       ENDFOR
    ENDFOR
    
    IF compare_to_obs eq 1 THEN BEGIN
       obs_longitude=OPEN_AND_EXTRACT(obs_infile,'longitude')
       obs_latitude=OPEN_AND_EXTRACT(obs_infile,'latitude')
       DEFINE_BOUNDARIES,box_read,obs_latitude,obs_longitude,obs_box_tx,/LIMIT
       obs_nlat=N_ELEMENTS(latitude)
       obs_nlon=N_ELEMENTS(longitude)
       
       obs_allyears_precip_filtered=fltarr(obs_nlon,obs_nlat,obs_ndays*obs_nyears)
       FOR j=0,obs_nyears-1 DO BEGIN
          obs_allyears_precip_filtered(*,*,j*obs_ndays:(j+1)*obs_ndays-1)=$
             REFORM(OPEN_AND_EXTRACT(obs_infile,'precip',$
                                     offset=[obs_box_tx(1),obs_box_tx(0),j,obs_filter_start_read],$
                                     count=[obs_nlon,obs_nlat,1,obs_ndays]))*obs_multiplier
       ENDFOR
       IF obs_clim_fourd eq 0 THEN BEGIN
          obs_clim_precip=REFORM(OPEN_AND_EXTRACT(obs_clim_infile,'precip',$
                                                  offset=[obs_box_tx(1),obs_box_tx(0),obs_clim_start_read],$
                                                  count=[obs_nlon,obs_nlat,obs_ndays]))*obs_multiplier
       ENDIF ELSE $
          obs_clim_precip=REFORM(OPEN_AND_EXTRACT(obs_clim_infile,'precip',$
                                                  offset=[obs_box_tx(1),obs_box_tx(0),0,obs_clim_start_read],$
                                                  count=[obs_nlon,obs_nlat,1,obs_ndays]))*obs_multiplier
       obs_clim_precip_seasmean=fltarr(obs_nlon,obs_nlat)
       FOR j=0,obs_nlon-1 DO BEGIN
          IF reverse_obslat eq 1 THEN BEGIN 
             FOR k=1,obs_nlat-1 DO $
                obs_clim_precip_seasmean(j,obs_nlat-k-1)=MEAN(obs_clim_precip(j,k,*),/NaN)
          ENDIF ELSE BEGIN
             FOR k=1,obs_nlat-1 DO $
                obs_clim_precip_seasmean(j,k)=MEAN(obs_clim_precip(j,k,*),/NaN)
          ENDELSE
       ENDFOR       
    ENDIF

    IF TOTAL(where(allyears_precip_filtered ge 1E20)) gt 0 THEN $
       allyears_precip_filtered[where(allyears_precip_filtered ge 1E20)]=!Values.F_NaN
    
    precip_filtered_variance=fltarr(n_lon,n_lat)
    FOR j=0,n_lon-1 DO $
       FOR k=0,n_lat-1 DO $
          precip_filtered_variance(j,k)=VARIANCE(allyears_precip_filtered(j,k,*),/NaN)
    
    IF compare_to_obs eq 1 THEN BEGIN
       obs_precip_filtered_variance=fltarr(n_lon,n_lat)
       ratio_obs_model_variance=fltarr(n_lon,n_lat)
       ratio_obs_model_variance_scaled=fltarr(n_lon,n_lat)
       IF TOTAL(where(obs_allyears_precip_filtered ge 1E20)) gt 0 THEN $
          obs_allyears_precip_filtered[where(obs_allyears_precip_filtered ge 1E20)]=!Values.F_NaN
       FOR j=0,n_lon-1 DO BEGIN
          FOR k=0,n_lat-1 DO BEGIN             
             IF reverse_obslat eq 1 THEN BEGIN
                obs_precip_filtered_variance(j,obs_nlat-k-1)=VARIANCE(obs_allyears_precip_filtered(j,k,*),/NaN)             
             ENDIF ELSE $
                obs_precip_filtered_variance(j,k)=VARIANCE(obs_allyears_precip_filtered(j,k,*),/NaN)
          ENDFOR
       ENDFOR
       FOR j=0,n_lon-1 DO BEGIN
          FOR k=0,n_lat-1 DO BEGIN
             ratio_obs_model_variance(j,k)=SQRT(precip_filtered_variance(j,k))/SQRT(obs_precip_filtered_variance(j,n_lat-k-1))
             ratio_obs_model_variance_scaled(j,k)=SQRT(precip_filtered_variance(j,k))/clim_precip_seasmean(j,k)/$
                                                  (SQRT(obs_precip_filtered_variance(j,n_lat-k-1))/obs_clim_precip_seasmean(j,n_lat-k-1))
          ENDFOR
       ENDFOR
    ENDIF

    psfile='/home/ss901165/idl/hadgem3_monwg/isv_metrics/hadgem3_monwg_isv_metrics_horizontal_variance.'+psfile_title+'_clim_precip.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100
    CS,SCALE=23,NCOLS=N_ELEMENTS(mylevs_clim)+1
    MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
    LEVS,MANUAL=mylevs_clim
    CON,FIELD=clim_precip_seasmean,X=longitude,Y=latitude,$
        TITLE='Clim JJAS precip - '+plot_title,/NOLINES,/NOAXES
    AXES,XSTEP=20,YSTEP=8
    IF (show_plot eq 1) THEN BEGIN
       PSCLOSE
    ENDIF ELSE $
       PSCLOSE,/NOVIEW
    
             
    psfile='/home/ss901165/idl/hadgem3_monwg/isv_metrics/hadgem3_monwg_isv_metrics_horizontal_variance.'+psfile_title+'_'+low_limit_str+high_limit_str+'.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100
    CS,SCALE=24,NCOLS=N_ELEMENTS(mylevs)+1
    MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
    LEVS,MANUAL=mylevs
    CON,FIELD=SQRT(precip_filtered_variance),X=longitude,Y=latitude,$
      TITLE='Std. dev. in '+low_limit_str+'-'+high_limit_str+' day filtered rainfall (mm/day) - '+plot_title,$
      /NOLINES,/NOAXES
    AXES,XSTEP=20,YSTEP=8
;    AXES,XVALS=['45','65','85','105','125','145','165'],XLABELS=['45E','65E','85E','105E','125E','145E','165E'],YVALS=['-25','-15','-5','5','15','25'],YLABELS=['25S','15S','5S','5N','15N','25N']
    IF (show_plot eq 1) THEN BEGIN
       PSCLOSE
    ENDIF ELSE $
       PSCLOSE,/NOVIEW

    scaled=SQRT(precip_filtered_variance)/clim_precip_seasmean
    scaled[where(clim_precip_seasmean le 1)]=!Values.F_NaN

    psfile='/home/ss901165/idl/hadgem3_monwg/isv_metrics/hadgem3_monwg_isv_metrics_horizontal_variance.'+psfile_title+'_'+low_limit_str+high_limit_str+'_scaled.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100
    CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_scaled)+1,/REV
    MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
    LEVS,MANUAL=mylevs_scaled
    CON,FIELD=scaled,X=longitude,Y=latitude,$
      TITLE='Std. dev. in '+low_limit_str+'-'+high_limit_str+' day filtered rainfall (mm/day) scaled by mean - '+plot_title,$
      /NOLINES,/NOAXES
    AXES,XSTEP=20,YSTEP=8
    IF (show_plot eq 1) THEN BEGIN
       PSCLOSE
    ENDIF ELSE $
       PSCLOSE,/NOVIEW   

    IF compare_to_obs eq 1 THEN BEGIN 

       psfile='/home/ss901165/idl/hadgem3_monwg/isv_metrics/hadgem3_monwg_isv_metrics_horizontal_variance.diff-'+psfile_title+'-obs_clim_precip.ps'
       PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100
       CS,SCALE=23,NCOLS=N_ELEMENTS(mylevs_diff_clim)+1
       MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
       LEVS,MANUAL=mylevs_diff_clim
       CON,FIELD=clim_precip_seasmean-obs_clim_precip_seasmean,X=longitude,Y=latitude,$
           TITLE='Diff in clim JJAS precip - '+plot_title+' minus TRMM',/NOLINES,/NOAXES
       AXES,XSTEP=20,YSTEP=8
       IF (show_plot eq 1) THEN BEGIN
          PSCLOSE
       ENDIF ELSE $
          PSCLOSE,/NOVIEW

       ratio_obs_model_variance[where(clim_precip_seasmean le 1 or obs_clim_precip_seasmean le 1)]=!Values.F_NaN

       psfile='/home/ss901165/idl/hadgem3_monwg/isv_metrics/hadgem3_monwg_isv_metrics_horizontal_variance.ratio-'+psfile_title+'-obs_'+low_limit_str+high_limit_str+'.ps'
       PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100
       CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio)+1,/REV
       MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
       LEVS,MANUAL=mylevs_ratio
       CON,FIELD=ratio_obs_model_variance,X=longitude,Y=latitude,$
           TITLE='Ratio of std. dev. in '+low_limit_str+'-'+high_limit_str+' day filtered rainfall (mm/day) - '+plot_title+' divided by TRMM',$
           /NOLINES,/NOAXES
       AXES,XSTEP=20,YSTEP=8
       IF (show_plot eq 1) THEN BEGIN
          PSCLOSE
       ENDIF ELSE $
          PSCLOSE,/NOVIEW    
       
       ratio_obs_model_variance_scaled[where(clim_precip_seasmean le 1 or obs_clim_precip_seasmean le 1)]=!Values.F_NaN

       psfile='/home/ss901165/idl/hadgem3_monwg/isv_metrics/hadgem3_monwg_isv_metrics_horizontal_variance.ratio-'+psfile_title+'-obs_'+low_limit_str+high_limit_str+'_scaled.ps'
       PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100
       CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio)+1,/REV
       MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
       LEVS,MANUAL=mylevs_ratio
       CON,FIELD=ratio_obs_model_variance_scaled,X=longitude,Y=latitude,$
           TITLE='Ratio of std. dev. in '+low_limit_str+'-'+high_limit_str+' day filtered rainfall (mm/day) scaled by mean - '+plot_title+' divided by TRMM',$
           /NOLINES,/NOAXES
       AXES,XSTEP=20,YSTEP=8
       IF (show_plot eq 1) THEN BEGIN
          PSCLOSE
       ENDIF ELSE $
          PSCLOSE,/NOVIEW    

    ENDIF

ENDFOR

STOP

END
