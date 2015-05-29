PRO hadgem3kpp_mean_state_entrain_compare_obs_xsec
  
; Compare the mean state from the HadGEM3-A AMIP2 1.0x and 1.5x entrainment runs
; to observations and ERA-Interim reanalysis

n_variables=3
amip2_ctl_dir='/home/ss901165/um_output3/hadgem3a_amip2_control_vn74'
amip2_15xentrain_dir='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74'
morph3_dir='/home/ss901165/um_output3/hadgem3_monwg/airxv'

FOR i=0,n_variables-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         variable_name='q_latavg_15S-15N'
         amip2_ctl_infile=amip2_ctl_dir+'/hadgem3a_amip2_ctl_vn74.jan-dec_amean_clim.years1-30.q.nc'
         amip2_15xentrain_infile=amip2_15xentrain_dir+'/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_amean_clim.years1-30.q.nc'
         obs_full_infile='/home/ss901165/datasets_mango/ERA-INTERIM/Q/ERA_interim.jan-dec_amean_clim.1989-2009.q.mjo_domain.latavg_15S-15N.n96.nc'
         hadgem3_horizname='longitude'
         hadgem3_latname='latitude'
         hadgem3_latavg_minval=0.001
         hadgem3_vertname='p'
         hadgem3_varname='q'
         obs_horizname='longitude'
         obs_vertname='p'
         obs_varname='Q'
         obs_name='ERA-Interim'
         mylevs_raw=['0.06','0.12','0.25','0.5','0.75','1','1.5','2','3','4','5','6','8','10','12','14']
         mylevs_diff=['-5.5','-3.5','-2.5','-2.0','-1.5','-1.0','-0.75','-0.5','-0.25','-0.12','-0.06',$
                      '0.06','0.12','0.25','0.5','0.75','1.0','1.5','2.0','2.5','3.5','5.5']
         mylevs_model_diff=['-2.5','-2.0','-1.5','-1.0','-0.75','-0.5','-0.25','-0.12','-0.06','-0.03','-0.01',$
                            '0.01','0.03','0.06','0.12','0.25','0.5','0.75','1.0','1.5','2.0','2.5']
         view=0
         hadgem3_ndims=3
         hadgem3_mult=1000.
         obs_ndims=2
         obs_revlat=0
         color_rev=1
         obs_mult=1000.
         cb_units='Specific humidity (g kg!U-1!N)'
      END      
      1 : BEGIN
         variable_name='u_latavg_15S-15N'
         amip2_ctl_infile=amip2_ctl_dir+'/hadgem3a_amip2_ctl_vn74.jan-dec_amean_clim.years1-30.u.nc'
         amip2_15xentrain_infile=amip2_15xentrain_dir+'/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_amean_clim.years1-30.u.nc'
         obs_full_infile='/home/ss901165/datasets_mango/ERA-INTERIM/U/ERA_interim.jan-dec_amean_clim.1989-2009.u.latavg_15S-15N.n96.nc'
         hadgem3_horizname='longitude'
         hadgem3_latname='latitude'
         hadgem3_latavg_minval=0.01
         hadgem3_vertname='p'
         hadgem3_varname='u'
         obs_horizname='longitude'
         obs_vertname='p'
         obs_varname='U'
         obs_name='ERA-Interim'
         mylevs_raw=['-9.5','-8.5','-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5','8.5','9.5']
         mylevs_diff=['-5.2','-4.4','-3.6','-2.8','-2.0','-1.2','-0.4','0.4','1.2','2.0','2.8','3.6','4.4','5.2']
         mylevs_model_diff=['-4.5','-3.9','-3.3','-2.7','-2.1','-1.5','-0.9','-0.3','0.3','0.9','1.5','2.1','2.7','3.3','3.9','4.5']
         view=0
         hadgem3_ndims=3
         hadgem3_mult=1.
         obs_ndims=2
         obs_revlat=0
         color_rev=0
         obs_mult=1.
         cb_units='Zonal-mean zonal wind (m s!U-1!N)'
      END
      2 : BEGIN
         variable_name='temp_latavg_15S-15N'
         amip2_ctl_infile=amip2_ctl_dir+'/hadgem3a_amip2_ctl_vn74.jan-dec_amean_clim.years1-30.temp.nc'
         amip2_15xentrain_infile=amip2_15xentrain_dir+'/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_amean_clim.years1-30.temp.nc'
         obs_full_infile='/home/ss901165/datasets_mango/ERA-INTERIM/T/ERA_interim.jan-dec_amean_clim.1989-2009.t.latavg_15S-15N.n96.nc'
         hadgem3_horizname='longitude'
         hadgem3_latname='latitude'
         hadgem3_latavg_minval=100
         hadgem3_vertname='p'
         hadgem3_varname='temp'
         obs_horizname='longitude'
         obs_vertname='p'
         obs_varname='T'
         obs_name='ERA-Interim'
         mylevs_raw=['180','190','200','210','220','230','240','250','260','270','280','290','300']
         mylevs_diff=['-4.5','-3.9','-3.3','-2.7','-2.1','-1.5','-0.9','-0.3','0.3','0.9','1.5','2.1','2.7','3.3','3.9','4.5']
         mylevs_model_diff=['-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5']
         view=0
         hadgem3_ndims=4
         hadgem3_mult=1.
         obs_ndims=2
         obs_revlat=0
         color_rev=0
         obs_mult=1.
         cb_units='Zonal-mean zonal wind (m s!U-1!N)'
      END
   ENDCASE

   box=[-15,0,15,359]

   hadgem3_horizontal=OPEN_AND_EXTRACT(amip2_ctl_infile,hadgem3_horizname)
   hadgem3_latitude=OPEN_AND_EXTRACT(amip2_ctl_infile,hadgem3_latname)
   hadgem3_vertical=OPEN_AND_EXTRACT(amip2_ctl_infile,hadgem3_vertname)
   DEFINE_BOUNDARIES,box,hadgem3_latitude,hadgem3_horizontal,hadgem3_box_tx,/LIMIT
   hadgem3_nhoriz=N_ELEMENTS(hadgem3_horizontal)
   hadgem3_nlat=N_ELEMENTS(hadgem3_latitude)
   hadgem3_nvert=N_ELEMENTS(hadgem3_vertical)

   obs_horizontal=OPEN_AND_EXTRACT(obs_full_infile,obs_horizname)
   obs_vertical=OPEN_AND_EXTRACT(obs_full_infile,obs_vertname)
   obs_nhoriz=N_ELEMENTS(obs_horizontal)
   obs_nvert=N_ELEMENTS(obs_vertical)

   IF hadgem3_ndims eq 2 THEN BEGIN
      amip2_ctl_var=REFORM(OPEN_AND_EXTRACT(amip2_ctl_infile,hadgem3_varname))*hadgem3_mult
      amip2_15xentrain_var=REFORM(OPEN_AND_EXTRACT(amip2_15xentrain_infile,hadgem3_varname))*hadgem3_mult
   ENDIF ELSE BEGIN
      amip2_ctl_threed_var=REFORM(OPEN_AND_EXTRACT(amip2_ctl_infile,hadgem3_varname,offset=[hadgem3_box_tx(1),hadgem3_box_tx(0),0,0],$
                                                   count=[hadgem3_nhoriz,hadgem3_nlat,hadgem3_nvert,1]))*hadgem3_mult
      amip2_15xentrain_threed_var=REFORM(OPEN_AND_EXTRACT(amip2_15xentrain_infile,hadgem3_varname,offset=[hadgem3_box_tx(1),hadgem3_box_tx(0),0,0],$
                                                          count=[hadgem3_nhoriz,hadgem3_nlat,hadgem3_nvert,1]))*hadgem3_mult
      amip2_ctl_var=fltarr(hadgem3_nhoriz,hadgem3_nvert)
      amip2_15xentrain_var=fltarr(hadgem3_nhoriz,hadgem3_nvert)
      FOR j=0,hadgem3_nhoriz-1 DO BEGIN
         FOR k=0,hadgem3_nvert-1 DO BEGIN
            temp=REFORM(amip2_ctl_threed_var(j,*,k))
            amip2_ctl_var(j,k)=MEAN(temp[where(ABS(temp) ge hadgem3_latavg_minval)])
            temp=REFORM(amip2_15xentrain_threed_var(j,*,k))
            amip2_15xentrain_var(j,k)=MEAN(temp[where(ABS(temp) ge hadgem3_latavg_minval)])
         ENDFOR
      ENDFOR
   ENDELSE
   IF obs_ndims eq 2 THEN BEGIN      
      obs_full_var=REFORM(OPEN_AND_EXTRACT(obs_full_infile,obs_varname))*obs_mult     
      obs_modelplev_var=fltarr(obs_nhoriz,hadgem3_nvert)
      FOR j=0,hadgem3_nvert-1 DO BEGIN
         nearest_obs_lev=NEAREST(obs_vertical,hadgem3_vertical(j))
         obs_modelplev_var(*,j)=obs_full_var(*,nearest_obs_lev)
      ENDFOR
   ENDIF
   IF obs_revlat eq 1 THEN BEGIN
      temp=fltarr(obs_nhoriz,hadgem3_nvert)
      FOR j=0,obs_nhoriz-1 DO $
         temp(*,j)=obs_modelplev_var(*,obs_nhoriz-j-1)
      obs_modelplev_var=temp
   ENDIF

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state_entrain/hadgem3kpp_mean_state_entrain_compare_amip_obs.amip2_ctl.'+variable_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1750,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=8000
   IF color_rev eq 0 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1
   IF color_rev eq 1 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1,/REV,white=raw_white
   LEVS,MANUAL=mylevs_raw
   GSET,XMIN=MIN(hadgem3_horizontal),XMAX=MAX(hadgem3_horizontal),YMAX=MIN(hadgem3_vertical),YMIN=MAX(hadgem3_vertical)
   CON,X=hadgem3_horizontal,Y=hadgem3_vertical,FIELD=amip2_ctl_var,/NOLINELABELS,TITLE='Annual-mean '+variable_name+' for HadGEM3-A vn7.4 1.0x entrain (30 years)',$
       POSITIVE_STYLE=2,NEGATIVE_STYLE=2,CB_TITLE=cb_units
   AXES,XSTEP=30,YSTEP=-100,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)'
   IF view eq 1 THEN $
      PSCLOSE
   IF view eq 0 THEN $
      PSCLOSE,/NOVIEW   

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state_entrain/hadgem3kpp_mean_state_entrain_compare_amip_obs.amip2_15xentrain.'+variable_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1750,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=8000
   IF color_rev eq 0 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1
   IF color_rev eq 1 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1,/REV,white=raw_white
   LEVS,MANUAL=mylevs_raw
   GSET,XMIN=MIN(hadgem3_horizontal),XMAX=MAX(hadgem3_horizontal),YMAX=MIN(hadgem3_vertical),YMIN=MAX(hadgem3_vertical)
   CON,X=hadgem3_horizontal,Y=hadgem3_vertical,FIELD=amip2_15xentrain_var,/NOLINELABELS,TITLE='Annual-mean '+variable_name+' for HadGEM3-A vn7.4 1.5x entrain (30 years)',$
       POSITIVE_STYLE=2,NEGATIVE_STYLE=2,CB_TITLE=cb_units
   AXES,XSTEP=30,YSTEP=-100,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)'
   IF view eq 1 THEN $
      PSCLOSE
   IF view eq 0 THEN $
      PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state_entrain/hadgem3kpp_mean_state_entrain_compare_amip_obs.obs_full.'+variable_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1750,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=8000
   IF color_rev eq 0 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1
   IF color_rev eq 1 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1,/REV,white=raw_white
   LEVS,MANUAL=mylevs_raw
   GSET,XMIN=MIN(hadgem3_horizontal),XMAX=MAX(hadgem3_horizontal),YMAX=MIN(hadgem3_vertical),YMIN=MAX(hadgem3_vertical)
   CON,X=hadgem3_horizontal,Y=hadgem3_vertical,FIELD=obs_modelplev_var,/NOLINELABELS,TITLE='Annual-mean '+variable_name+' for '+obs_name+' (1989-2008)',$
       POSITIVE_STYLE=2,NEGATIVE_STYLE=2,CB_TITLE=cb_units
   AXES,XSTEP=30,YSTEP=-100,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)'
   IF view eq 1 THEN $
      PSCLOSE
   IF view eq 0 THEN $
      PSCLOSE,/NOVIEW
                                   
                                ; ---- Difference against observations
                                ;      1989-2008 ----

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state_entrain/hadgem3kpp_mean_state_entrain_compare_amip_obs.amip2_ctl-minus-obs_full.'+variable_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1750,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=8000
   IF color_rev eq 0 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1
   IF color_rev eq 1 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV
   LEVS,MANUAL=mylevs_diff
   GSET,XMIN=MIN(hadgem3_horizontal),XMAX=MAX(hadgem3_horizontal),YMAX=MIN(hadgem3_vertical),YMIN=MAX(hadgem3_vertical)
   CON,X=hadgem3_horizontal,Y=hadgem3_vertical,FIELD=amip2_ctl_var-obs_modelplev_var,/NOLINELABELS,$
       TITLE='Annual-mean '+variable_name+' for HadGEM3-A vn7.4 1.0x entrain (30 years) minus '+obs_name+' (1989-2008)',$
       POSITIVE_STYLE=2,NEGATIVE_STYLE=2,CB_TITLE=cb_units
   AXES,XSTEP=30,YSTEP=-100,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)'
   IF view eq 1 THEN $
      PSCLOSE
   IF view eq 0 THEN $
      PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state_entrain/hadgem3kpp_mean_state_entrain_compare_amip_obs.amip2_15xentrain-minus-obs_full.'+variable_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1750,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=8000
   IF color_rev eq 0 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1
   IF color_rev eq 1 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV
   LEVS,MANUAL=mylevs_diff
   GSET,XMIN=MIN(hadgem3_horizontal),XMAX=MAX(hadgem3_horizontal),YMAX=MIN(hadgem3_vertical),YMIN=MAX(hadgem3_vertical)
   CON,X=hadgem3_horizontal,Y=hadgem3_vertical,FIELD=amip2_15xentrain_var-obs_modelplev_var,/NOLINELABELS,$
       TITLE='Annual-mean '+variable_name+' for HadGEM3-A vn7.4 1.5x entrain (30 years) minus '+obs_name+' (1989-2008)',$
       POSITIVE_STYLE=2,NEGATIVE_STYLE=2,CB_TITLE=cb_units
   AXES,XSTEP=30,YSTEP=-100,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)'
   IF view eq 1 THEN $
      PSCLOSE
   IF view eq 0 THEN $
      PSCLOSE,/NOVIEW
   
                                ; Differences between two AMIP clim SST runs 

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state_entrain/hadgem3kpp_mean_state_entrain_compare_amip_obs.amip2_15xentrain-minus-amip2_ctl.'+$
          variable_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1750,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=8000
   IF color_rev eq 0 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_model_diff)+1
   IF color_rev eq 1 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_model_diff)+1,/REV   
   LEVS,MANUAL=mylevs_model_diff
   GSET,XMIN=MIN(hadgem3_horizontal),XMAX=MAX(hadgem3_horizontal),YMAX=MIN(hadgem3_vertical),YMIN=MAX(hadgem3_vertical)
   CON,X=hadgem3_horizontal,Y=hadgem3_vertical,FIELD=amip2_15xentrain_var-amip2_ctl_var,/NOLINELABELS,$
       TITLE='Annual-mean '+variable_name+' for HadGEM3-A vn7.4 1.5x entrain (30 years) minus HadGEM3-A vn7.4 1.0x entrain (30 years)',$
       POSITIVE_STYLE=2,NEGATIVE_STYLE=2,CB_TITLE=cb_units
   AXES,XSTEP=30,YSTEP=-100,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)'
    IF view eq 1 THEN $
      PSCLOSE
   IF view eq 0 THEN $
      PSCLOSE,/NOVIEW


ENDFOR



STOP

END

         
         
