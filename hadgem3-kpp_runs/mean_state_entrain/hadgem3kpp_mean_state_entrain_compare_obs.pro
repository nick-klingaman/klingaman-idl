PRO hadgem3kpp_mean_state_entrain_compare_obs
  
; Compare the mean state from the HadGEM3-A AMIP2 1.0x and 1.5x entrainment runs
; to observations and ERA-Interim reanalysis

n_variables=2
amip2_ctl_dir='/home/ss901165/um_output3/hadgem3a_amip2_control_vn74'
amip2_15xentrain_dir='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74'
morph3_dir='/home/ss901165/um_output3/hadgem3_monwg/airxv'

plot_box=[-30,0,30,360]

FOR i=0,n_variables-1 DO BEGIN
   CASE i OF
      6 : BEGIN
         variable_name='u200'
         amip2_ctl_infile=amip2_ctl_dir+'/hadgem3a_amip2_ctl_vn74.jan-dec_amean_clim.years1-30.u200.nc'
         amip2_15xentrain_infile=amip2_15xentrain_dir+'/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_amean_clim.years1-30.u200.nc'         
         morph3_full_infile=morph3_dir+'/hadgem3a_morph3_final_n96_amip2_airxv.jan-dec_amean_clim.1989-2008.u200.nc'         
         obs_full_infile='/home/ss901165/datasets_mango/ERA-INTERIM/U200/U200.jan-dec_amean_clim.1989-2008.mjo_domain.n96.nc'
         hadgem3_latname='latitude'
         hadgem3_lonname='longitude'
         hadgem3_varname='u'
         obs_latname='latitude'
         obs_lonname='longitude'
         obs_varname='U'      
         obs_name='ERA-Interim'
         mylevs_raw=['-33','-27','-21','-15','-9','-3','3','9','15','21','27','33']
         mylevs_diff=['-10.5','-8.5','-6.5','-4.5','-2.5','-0.5','0.5','2.5','4.5','6.5','8.5','10.5']
         view=0
         hadgem3_ndims=4
         hadgem3_mult=1.
         obs_ndims=2
         obs_revlat=1
         color_rev=0
         obs_mult=1
      END
      7 : BEGIN
         variable_name='u850'
         amip2_ctl_infile=amip2_ctl_dir+'/hadgem3a_amip2_ctl_vn74.jan-dec_amean_clim.years1-30.u850.nc'
         amip2_15xentrain_infile=amip2_15xentrain_dir+'/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_amean_clim.years1-30.u850.nc'
         morph3_full_infile=morph3_dir+'/hadgem3a_morph3_final_n96_amip2_airxv.jan-dec_amean_clim.1989-2008.u850.nc'
         obs_full_infile='/home/ss901165/datasets_mango/ERA-INTERIM/U850/U850.jan-dec_amean_clim.1989-2008.mjo_domain.n96.nc'
         hadgem3_latname='latitude'
         hadgem3_lonname='longitude'
         hadgem3_varname='u'
         obs_latname='latitude'
         obs_lonname='longitude'
         obs_varname='U'
         obs_name='ERA-Interim'
         mylevs_raw=['-8','-6','-4','-2','0','2','4','6','8']
         mylevs_diff=['-4.4','-3.6','-2.8','-2.0','-1.2','-0.4','0.4','1.2','2.0','2.8','3.6','4.4']
         view=0
         hadgem3_ndims=4
         hadgem3_mult=1.
         obs_ndims=2
         obs_revlat=1
      END
      2 : BEGIN
         variable_name='olr'
         amip2_ctl_infile=amip2_ctl_dir+'/hadgem3a_amip2_ctl_vn74.jan-dec_amean_clim.years1-30.olr.2.5x2.5.nc'
         amip2_15xentrain_infile=amip2_15xentrain_dir+'/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_amean_clim.years1-30.olr.2.5x2.5.nc'
         morph3_full_infile=morph3_dir+'/hadgem3a_morph3_final_n96_amip2_airxv.jan-dec_amean_clim.1989-2008.olr.2.5x2.5.nc'         
         obs_full_infile='/home/ss901165/datasets/NOAA_CIRES_OLR/daily/NOAA_CIRES_OLR.jan-dec_amean_clim.1989-2008.2.5x2.5.nc'
         hadgem3_latname='latitude'
         hadgem3_lonname='longitude'
         hadgem3_varname='olr'
         obs_latname='latitude'
         obs_lonname='longitude'
         obs_varname='olr'
         obs_name='NOAA CIRES'
         mylevs_raw=['180','190','200','210','220','230','240','250','260','270','280','290','300','310']
         mylevs_diff=['-60','-52','-44','-36','-28','-20','-12','-4','4','12','20','28','36','44','52','60']
         view=0
         hadgem3_ndims=2
         hadgem3_mult=1.
         obs_ndims=2
         obs_revlat=0
      END
      3 : BEGIN
         variable_name='precip'
         amip2_ctl_infile=amip2_ctl_dir+'/hadgem3a_amip2_ctl_vn74.jan-dec_amean_clim.years1-30.precip.nc'
         amip2_15xentrain_infile=amip2_15xentrain_dir+'/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_amean_clim.years1-30.precip.nc' 
         morph3_full_infile=morph3_dir+'/hadgem3a_morph3_final_n96_amip2_airxv.jan-dec_amean_clim.1989-2008.precip.nc'
         obs_full_infile='/home/ss901165/datasets_mango/TRMM_3B42V6/n96/TRMM_3B42v6A.jan-dec_amean_clim.1999-2009.n96.nc'
         hadgem3_latname='latitude'
         hadgem3_lonname='longitude'
         hadgem3_varname='precip'
         obs_latname='latitude'
         obs_lonname='longitude'
         obs_varname='precip'
         obs_name='TRMM 3B42v6A'
         mylevs_raw=['1','2','3','4','5','7','9','11','13','15','18','21']
         raw_white=[2]
         mylevs_diff=['-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5']
         view=0
         hadgem3_ndims=4
         hadgem3_mult=86400.
         obs_ndims=2
         obs_revlat=1
         color_rev=1
      END
      4 : BEGIN
         variable_name='v200'
         amip2_ctl_infile=amip2_ctl_dir+'/hadgem3a_amip2_ctl_vn74.jan-dec_amean_clim.years1-30.v200.nc'
         amip2_15xentrain_infile=amip2_15xentrain_dir+'/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_amean_clim.years1-30.v200.nc'
         morph3_full_infile=morph3_dir+'/hadgem3a_morph3_final_n96_amip2_airxv.jan-dec_amean_clim.1989-2008.v200.nc'        
         obs_full_infile='/home/ss901165/datasets_mango/ERA-INTERIM/V200/V200.jan-dec_amean_clim.1989-2008.mjo_domain.n96.nc'
         hadgem3_latname='latitude'
         hadgem3_lonname='longitude'
         hadgem3_varname='v'
         obs_latname='latitude'
         obs_lonname='longitude'
         obs_varname='V'
         obs_name='ERA-Interim'
         mylevs_raw=['-9.75','-8.25','-6.75','-5.25','-3.75','-2.25','-0.75','0.75','2.25','3.75','5.25','6.75','8.25','9.75']
         mylevs_diff=['-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5']
         view=0
         hadgem3_ndims=4
         hadgem3_mult=1.
         obs_ndims=2
         obs_revlat=1
         color_rev=0
      END
      5 : BEGIN
         variable_name='v850'
         amip2_ctl_infile=amip2_ctl_dir+'/hadgem3a_amip2_ctl_vn74.jan-dec_amean_clim.years1-30.v850.nc'
         amip2_15xentrain_infile=amip2_15xentrain_dir+'/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_amean_clim.years1-30.v850.nc'
         morph3_full_infile=morph3_dir+'/hadgem3a_morph3_final_n96_amip2_airxv.jan-dec_amean_clim.1989-2008.v850.nc'
         obs_full_infile='/home/ss901165/datasets_mango/ERA-INTERIM/V850/V850.jan-dec_amean_clim.1989-2008.mjo_domain.n96.nc'
         hadgem3_latname='latitude'
         hadgem3_lonname='longitude'
         hadgem3_varname='v'
         obs_latname='latitude'
         obs_lonname='longitude'
         obs_varname='V'
         obs_name='ERA-Interim'
         mylevs_raw=['-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5']
         mylevs_diff=['-5.2','-4.4','-3.6','-2.8','-2.0','-1.2','-0.4','0.4','1.2','2.0','2.8','3.6','4.4','5.2']
         view=0
         hadgem3_ndims=4
         hadgem3_mult=1.
         obs_ndims=2
         obs_revlat=1
         color_rev=0
      END
      1 : BEGIN
         variable_name='taux'
         amip2_ctl_infile=amip2_ctl_dir+'/hadgem3a_amip2_ctl_vn74.jan-dec_amean_clim.years1-24.taux.nc'
         amip2_15xentrain_infile=amip2_15xentrain_dir+'/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_amean_clim.years1-30.taux.nc'
         morph3_full_infile=amip2_15xentrain_dir+'/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_amean_clim.years1-30.taux.nc'
         obs_full_infile='/home/ss901165/datasets_mango/ERA-INTERIM/IEWS/ERA_interim.jan-dec_amean_clim.1989-2009.iews.trop_domain.n96.nc'
         mask_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/mask_n96_hadgem3-7.3.nc'
         mask='land'
         hadgem3_latname='latitude'
         hadgem3_lonname='longitude_1'
         hadgem3_varname='taux'
         obs_latname='latitude'
         obs_lonname='longitude'
         obs_varname='IEWS'
         obs_name='ERA-Interim'
         mylevs_raw=['-11','-9','-7','-5','-3','-1','1','3','5','7','9','11']
         mylevs_diff=['-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5']
         view=0
         hadgem3_ndims=4
         hadgem3_mult=100.
         obs_ndims=2
         obs_revlat=1
         color_rev=0
         obs_mult=100.
         cb_units='Zonal wind stress (N m!U-2!N * 100)'
      END
      0 : BEGIN
         variable_name='tauy'
         amip2_ctl_infile=amip2_ctl_dir+'/hadgem3a_amip2_ctl_vn74.jan-dec_amean_clim.years1-24.tauy.nc'
         amip2_15xentrain_infile=amip2_15xentrain_dir+'/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_amean_clim.years1-30.tauy.nc'
         morph3_full_infile=amip2_15xentrain_dir+'/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_amean_clim.years1-30.tauy.nc'
         obs_full_infile='/home/ss901165/datasets_mango/ERA-INTERIM/INSS/ERA_interim.jan-dec_amean_clim.1989-2009.inss.trop_domain.n96.nc'
         mask_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/mask_n96_hadgem3-7.3.nc'
         mask='land'
         hadgem3_latname='latitude_1'
         hadgem3_lonname='longitude'
         hadgem3_varname='tauy'
         obs_latname='latitude'
         obs_lonname='longitude'
         obs_varname='INSS'
         obs_name='ERA-Interim'
         mylevs_raw=['-11','-9','-7','-5','-3','-1','1','3','5','7','9','11']
         mylevs_diff=['-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5']
         view=1
         hadgem3_ndims=4
         hadgem3_mult=100.
         obs_ndims=2
         obs_revlat=1
         color_rev=0
         obs_mult=100.
         cb_units='Meridional wind stress (N m!U-2!N * 100)'
      END
   ENDCASE

   hadgem3_latitude=OPEN_AND_EXTRACT(amip2_ctl_infile,hadgem3_latname)
   hadgem3_longitude=OPEN_AND_EXTRACT(amip2_ctl_infile,hadgem3_lonname)
   DEFINE_BOUNDARIES,plot_box,hadgem3_latitude,hadgem3_longitude,hadgem3_box_tx,/LIMIT
   hadgem3_nlat=N_ELEMENTS(hadgem3_latitude)
   hadgem3_nlon=N_ELEMENTS(hadgem3_longitude)

   obs_latitude=OPEN_AND_EXTRACT(obs_full_infile,obs_latname)
   obs_longitude=OPEN_AND_EXTRACT(obs_full_infile,obs_lonname)
   DEFINE_BOUNDARIES,plot_box,obs_latitude,obs_longitude,obs_box_tx,/LIMIT
   obs_nlat=N_ELEMENTS(obs_latitude)
   obs_nlon=N_ELEMENTS(obs_longitude)

   IF hadgem3_ndims eq 2 THEN BEGIN
      amip2_ctl_var=REFORM(OPEN_AND_EXTRACT(amip2_ctl_infile,hadgem3_varname,offset=[hadgem3_box_tx(1),hadgem3_box_tx(0)],$
                                            count=[hadgem3_nlon,hadgem3_nlat]))*hadgem3_mult
      amip2_15xentrain_var=REFORM(OPEN_AND_EXTRACT(amip2_15xentrain_infile,hadgem3_varname,offset=[hadgem3_box_tx(1),hadgem3_box_tx(0)],$
                                                   count=[hadgem3_nlon,hadgem3_nlat]))*hadgem3_mult
      morph3_full_var=REFORM(OPEN_AND_EXTRACT(morph3_full_infile,hadgem3_varname,offset=[hadgem3_box_tx(1),hadgem3_box_tx(0)],$
                                              count=[hadgem3_nlon,hadgem3_nlat]))*hadgem3_mult
   ENDIF ELSE BEGIN
      amip2_ctl_var=REFORM(OPEN_AND_EXTRACT(amip2_ctl_infile,hadgem3_varname,offset=[hadgem3_box_tx(1),hadgem3_box_tx(0),0],$
                                            count=[hadgem3_nlon,hadgem3_nlat,1]))*hadgem3_mult
      amip2_15xentrain_var=REFORM(OPEN_AND_EXTRACT(amip2_15xentrain_infile,hadgem3_varname,offset=[hadgem3_box_tx(1),hadgem3_box_tx(0),0],$
                                            count=[hadgem3_nlon,hadgem3_nlat,1]))*hadgem3_mult
      morph3_full_var=REFORM(OPEN_AND_EXTRACT(morph3_full_infile,hadgem3_varname,offset=[hadgem3_box_tx(1),hadgem3_box_tx(0),0],$
                                              count=[hadgem3_nlon,hadgem3_nlat,1]))*hadgem3_mult
   ENDELSE
   IF obs_ndims eq 2 THEN BEGIN
      obs_full_var=REFORM(OPEN_AND_EXTRACT(obs_full_infile,obs_varname,offset=[obs_box_tx(1),obs_box_tx(0)],$
                                           count=[obs_nlon,obs_nlat]))*obs_mult
   ENDIF
   IF obs_revlat eq 1 THEN BEGIN
      temp=fltarr(obs_nlon,obs_nlat)
      FOR j=0,obs_nlat-1 DO $
         temp(*,j)=obs_full_var(*,obs_nlat-j-1)
      obs_full_var=temp
   ENDIF
   IF mask eq 'land' or mask eq 'ocean' THEN BEGIN
      lsm_var=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',offset=[hadgem3_box_tx(1),hadgem3_box_tx(0),0,0],$
                                      count=[hadgem3_nlon,hadgem3_nlat,1,1]))
      IF mask eq 'land' THEN BEGIN
         amip2_ctl_var[where(lsm_var eq 1)]=!Values.F_NaN
         amip2_15xentrain_var[where(lsm_var eq 1)]=!Values.F_NaN
         morph3_full_var[where(lsm_var eq 1)]=!Values.F_NaN
         obs_full_var[where(lsm_var eq 1)]=!Values.F_NaN
      ENDIF ELSE IF mask eq 'ocean' THEN BEGIN
         amip2_ctl_var[where(lsm_var eq 0)]=!Values.F_NaN
         amip2_15xentrain_var[where(lsm_var eq 0)]=!Values.F_NaN
         morph3_full_var[where(lsm_var eq 0)]=!Values.F_NaN
         obs_full_var[where(lsm_var eq 0)]=!Values.F_NaN
      ENDIF
   ENDIF

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state_entrain/hadgem3kpp_mean_state_entrain_compare_amip_obs.amip2_ctl.'+variable_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1000,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=8000
   IF color_rev eq 0 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1
   IF color_rev eq 1 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1,/REV,white=raw_white
   LEVS,MANUAL=mylevs_raw
   MAP,LATMIN=plot_box(0),LATMAX=plot_box(2),LONMIN=plot_box(1),LONMAX=plot_box(3)
   CON,X=hadgem3_longitude,Y=hadgem3_latitude,FIELD=amip2_ctl_var,/NOLINELABELS,TITLE='Annual-mean '+variable_name+' for HadGEM3-A vn7.4 1.0x entrain (30 years)',$
       POSITIVE_STYLE=2,NEGATIVE_STYLE=2,CB_TITLE=cb_units
   IF view eq 1 THEN $
      PSCLOSE
   IF view eq 0 THEN $
      PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state_entrain/hadgem3kpp_mean_state_entrain_compare_amip_obs.amip2_15xentrain.'+variable_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1000,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=8000
   IF color_rev eq 0 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1
   IF color_rev eq 1 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1,/REV,white=raw_white
   LEVS,MANUAL=mylevs_raw
   MAP,LATMIN=plot_box(0),LATMAX=plot_box(2),LONMIN=plot_box(1),LONMAX=plot_box(3)
   CON,X=hadgem3_longitude,Y=hadgem3_latitude,FIELD=amip2_15xentrain_var,/NOLINELABELS,TITLE='Annual-mean '+variable_name+' for HadGEM3-A vn7.4 1.5x entrain (30 years)',$
       POSITIVE_STYLE=2,NEGATIVE_STYLE=2,CB_TITLE=cb_units
   IF view eq 1 THEN $
      PSCLOSE
   IF view eq 0 THEN $
      PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state_entrain/hadgem3kpp_mean_state_entrain_compare_amip_obs.morph3_full.'+variable_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1000,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=8000
   IF color_rev eq 0 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1
   IF color_rev eq 1 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1,/REV,white=raw_white
   LEVS,MANUAL=mylevs_raw
   MAP,LATMIN=plot_box(0),LATMAX=plot_box(2),LONMIN=plot_box(1),LONMAX=plot_box(3)
   CON,X=hadgem3_longitude,Y=hadgem3_latitude,FIELD=morph3_full_var,/NOLINELABELS,TITLE='Annual-mean '+variable_name+' for HadGEM3-A MORPH3 AMIP2 (1989-2008)',$
       POSITIVE_STYLE=2,NEGATIVE_STYLE=2,CB_TITLE=cb_units
   IF view eq 1 THEN $
      PSCLOSE
   IF view eq 0 THEN $
      PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state_entrain/hadgem3kpp_mean_state_entrain_compare_amip_obs.obs_full.'+variable_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1000,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=8000
   IF color_rev eq 0 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1
   IF color_rev eq 1 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1,/REV,white=raw_white
   LEVS,MANUAL=mylevs_raw
   MAP,LATMIN=plot_box(0),LATMAX=plot_box(2),LONMIN=plot_box(1),LONMAX=plot_box(3)
   CON,X=hadgem3_longitude,Y=hadgem3_latitude,FIELD=obs_full_var,/NOLINELABELS,TITLE='Annual-mean '+variable_name+' for '+obs_name+' (1989-2008)',$
       POSITIVE_STYLE=2,NEGATIVE_STYLE=2,CB_TITLE=cb_units
   IF view eq 1 THEN $
      PSCLOSE
   IF view eq 0 THEN $
      PSCLOSE,/NOVIEW
                                   
                                ; ---- Difference against MORPH3
                                ;      1989-2008 ----
   
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state_entrain/hadgem3kpp_mean_state_entrain_compare_amip_obs.amip2_ctl-minus-morph3_full.'+variable_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1000,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=8000
   IF color_rev eq 0 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1
   IF color_rev eq 1 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV
   LEVS,MANUAL=mylevs_diff
   MAP,LATMIN=plot_box(0),LATMAX=plot_box(2),LONMIN=plot_box(1),LONMAX=plot_box(3)
   CON,X=hadgem3_longitude,Y=hadgem3_latitude,FIELD=amip2_ctl_var-morph3_full_var,/NOLINELABELS,$
       TITLE='Annual-mean '+variable_name+' for HadGEM3-A vn7.4 1.0x entrain (30 years) minus MORPH3 AMIP2 (1989-2008)',$
       POSITIVE_STYLE=2,NEGATIVE_STYLE=2,CB_TITLE=cb_units
   IF view eq 1 THEN $
      PSCLOSE
   IF view eq 0 THEN $
      PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state_entrain/hadgem3kpp_mean_state_entrain_compare_amip_obs.amip2_15xentrain-minus-morph3_full.'+variable_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1000,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=8000
   IF color_rev eq 0 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1
   IF color_rev eq 1 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV
   LEVS,MANUAL=mylevs_diff
   MAP,LATMIN=plot_box(0),LATMAX=plot_box(2),LONMIN=plot_box(1),LONMAX=plot_box(3)
   CON,X=hadgem3_longitude,Y=hadgem3_latitude,FIELD=amip2_15xentrain_var-morph3_full_var,/NOLINELABELS,$
       TITLE='Annual-mean '+variable_name+' for HadGEM3-A vn7.4 1.5x entrain (30 years) minus MORPH3 AMIP2 (1989-2008)',$
       POSITIVE_STYLE=2,NEGATIVE_STYLE=2,CB_TITLE=cb_units
   IF view eq 1 THEN $
      PSCLOSE
   IF view eq 0 THEN $
      PSCLOSE,/NOVIEW

                                ; ---- Difference against observations
                                ;      1989-2008 ----

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state_entrain/hadgem3kpp_mean_state_entrain_compare_amip_obs.amip2_ctl-minus-obs_full.'+variable_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1000,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=8000
   IF color_rev eq 0 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1
   IF color_rev eq 1 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV
   LEVS,MANUAL=mylevs_diff
   MAP,LATMIN=plot_box(0),LATMAX=plot_box(2),LONMIN=plot_box(1),LONMAX=plot_box(3)
   CON,X=hadgem3_longitude,Y=hadgem3_latitude,FIELD=amip2_ctl_var-obs_full_var,/NOLINELABELS,$
       TITLE='Annual-mean '+variable_name+' for HadGEM3-A vn7.4 1.0x entrain (30 years) minus '+obs_name+' (1989-2008)',$
       POSITIVE_STYLE=2,NEGATIVE_STYLE=2,CB_TITLE=cb_units
   IF view eq 1 THEN $
      PSCLOSE
   IF view eq 0 THEN $
      PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state_entrain/hadgem3kpp_mean_state_entrain_compare_amip_obs.amip2_15xentrain-minus-obs_full.'+variable_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1000,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=8000
   IF color_rev eq 0 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1
   IF color_rev eq 1 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV
   LEVS,MANUAL=mylevs_diff
   MAP,LATMIN=plot_box(0),LATMAX=plot_box(2),LONMIN=plot_box(1),LONMAX=plot_box(3)
   CON,X=hadgem3_longitude,Y=hadgem3_latitude,FIELD=amip2_15xentrain_var-obs_full_var,/NOLINELABELS,$
       TITLE='Annual-mean '+variable_name+' for HadGEM3-A vn7.4 1.5x entrain (30 years) minus '+obs_name+' (1989-2008)',$
       POSITIVE_STYLE=2,NEGATIVE_STYLE=2,CB_TITLE=cb_units
   IF view eq 1 THEN $
      PSCLOSE
   IF view eq 0 THEN $
      PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state_entrain/hadgem3kpp_mean_state_entrain_compare_amip_obs.morph3_full-minus-obs_full.'+variable_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1000,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=8000
   IF color_rev eq 0 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1
   IF color_rev eq 1 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV   
   LEVS,MANUAL=mylevs_diff
   MAP,LATMIN=plot_box(0),LATMAX=plot_box(2),LONMIN=plot_box(1),LONMAX=plot_box(3)
   CON,X=hadgem3_longitude,Y=hadgem3_latitude,FIELD=morph3_full_var-obs_full_var,/NOLINELABELS,$
       TITLE='Annual-mean '+variable_name+' for MORPH3 AMIP2 (1989-2008) minus '+obs_name+' (1989-2008)',$
       POSITIVE_STYLE=2,NEGATIVE_STYLE=2,CB_TITLE=cb_units
   IF view eq 1 THEN $
      PSCLOSE
   IF view eq 0 THEN $
      PSCLOSE,/NOVIEW
   
                                ; Differences between two AMIP clim SST runs 

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state_entrain/hadgem3kpp_mean_state_entrain_compare_amip_obs.amip2_15xentrain-minus-amip2_ctl.'+$
          variable_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1000,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=8000
   IF color_rev eq 0 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1
   IF color_rev eq 1 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV   
   LEVS,MANUAL=mylevs_diff
   MAP,LATMIN=plot_box(0),LATMAX=plot_box(2),LONMIN=plot_box(1),LONMAX=plot_box(3)
   CON,X=hadgem3_longitude,Y=hadgem3_latitude,FIELD=amip2_15xentrain_var-amip2_ctl_var,/NOLINELABELS,$
       TITLE='Annual-mean '+variable_name+' for HadGEM3-A vn7.4 1.5x entrain (30 years) minus HadGEM3-A vn7.4 1.0x entrain (30 years)',$
       POSITIVE_STYLE=2,NEGATIVE_STYLE=2,CB_TITLE=cb_units
   IF view eq 1 THEN $
      PSCLOSE
   IF view eq 0 THEN $
      PSCLOSE,/NOVIEW


ENDFOR



STOP

END

         
         
