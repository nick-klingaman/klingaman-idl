PRO hadgem3kpp_mean_state_compare_amip_obs_ga30

; Compare the mean state from the HadGEM3-KPP GA3.0 integration
; to the HadGEM3-A integration (UKMO clim SSTs) that generated the flux corrections
; and to observations for 1989-2008.

n_variables=1
;hg3kpp_dir='/home/ss901165/um_output3/hadgem3_monwg/aljyr'
;hg3kpp_name='hadgem3ao_orca1_n96_ga40_aljyr'
;hg3kpp_dir='/export/niagara/data-06/cy000010/um_output/xhwob'
;hg3kpp_name='hadgem3kpp_1.5xentrain_vn78_360E'
;hg3kpp_dir='/home/ss901165/um_output6/xgspm'
;hg3kpp_name='hadgem3kpp_1.5xentrain_ga30'
;hg3a_dir='/home/ss901165/um_output6/xgspj'
;hg3a_name='hadgem3a_ukmo_1.5xentrain_vn78'
;hg3kpp_dir='/home/ss901165/um_output6/xgspp'
;hg3kpp_name='hadgem3kpp_1.0xentrain_ga30'
;hg3a_dir='/home/ss901165/um_output6/xgspo'
;hg3a_name='hadgem3a_ukmo_1.0xentrain_vn78'
;hg3a_dir='/home/ss901165/um_output6/xgspj'
;hg3a_name='hadgem3a_ukmo_1.5xentrain_vn78'

;hg3kpp_dir='/home/ss901165/um_output6/gc2/anqjm/'
;hg3kpp_name='hadgem3_gc2_n96_orca025'
;hg3a_dir='/home/ss901165/um_output6/gc2/antia'
;hg3a_name='hadgem3_ga6_n96'

hg3kpp_dir='/home/ss901165/um_output6/xihvd'
hg3kpp_name='hadgem3kpp_1.5xentrain_ga30_50N50S'
hg3a_dir='/home/ss901165/um_output6/xgspj'
hg3a_name='hadgem3a_ukmo_1.5xentrain_vn78'

plot_box=[-40,0,40,360]

FOR i=0,n_variables-1 DO BEGIN
   CASE i OF
      4 : BEGIN
         variable_name='u200'
         hg3kpp_infile=hg3kpp_dir+'/'+hg3kpp_name+'.jan-dec_amean_clim.years1-20.u200.nc'
         hg3a_infile=hg3a_dir+'/'+hg3a_name+'.jan-dec_amean_clim.years1-20.u200.nc'
         obs_full_infile='/home/ss901165/datasets_mango/ERA-INTERIM/U200/U200.jan-dec_amean_clim.1989-2008.mjo_domain.n96.nc'
         hadgem3_latname='latitude'
         hadgem3_lonname='longitude'
         hadgem3_varname='u'
         obs_latname='latitude'
         obs_lonname='longitude'
         obs_varname='U'
         obs_name='ERA-Interim'
         mylevs_raw=['-33','-27','-21','-15','-9','-3','3','9','15','21','27','33']
         mylevs_diff=['-20.5','-8.5','-6.5','-4.5','-2.5','-0.5','0.5','2.5','4.5','6.5','8.5','10.5']
         view=0
         hadgem3_ndims=3
         hadgem3_mult=1.
         obs_ndims=2
         obs_revlat=1
         color_rev=0
      END
      1 : BEGIN
         variable_name='u850'
         hg3kpp_infile=hg3kpp_dir+'/'+hg3kpp_name+'.jan-dec_amean_clim.years1-20.u850.nc'
         hg3a_infile=hg3a_dir+'/'+hg3a_name+'.jan-dec_amean_clim.years1-20.u850.nc'
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
         hadgem3_ndims=3
         hadgem3_mult=1.
         obs_ndims=2
         obs_revlat=1
      END
      2 : BEGIN
         variable_name='olr'
         hg3kpp_infile=hg3kpp_dir+'/'+hg3kpp_name+'.jan-dec_amean_clim.years1-20.olr.2.5x2.5.nc'
         hg3a_infile=hg3a_dir+'/'+hg3a_name+'.jan-dec_amean_clim.years1-20.olr.2.5x2.5.nc'
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
      0 : BEGIN
         variable_name='precip'
         hg3kpp_infile=hg3kpp_dir+'/'+hg3kpp_name+'.jan-dec_dmean_clim.years1-60.precip.nc'
         hg3a_infile=hg3a_dir+'/'+hg3a_name+'.jan-dec_dmean_clim.years1-20.precip.nc'
         obs_full_infile='/home/ss901165/datasets_mango/TRMM_3B42V6/n96/TRMM_3B42v6A.jan-dec_dmean_clim.1999-2009.n96.nc'
         hadgem3_latname='latitude'
         hadgem3_lonname='longitude'
         hadgem3_varname='precip'
         obs_latname='latitude'
         obs_lonname='longitude'
         obs_varname='precip'
         obs_name='TRMM 3B42v6A'
         mylevs_raw=['1','2','3','4','5','7','9','11','13','15','18','21']
         raw_white=[2]
         ;mylevs_diff=['-13','-11','-9','-7','-5','-3','-1','1','3','5','7','9','11','13']
         mylevs_diff=['-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5']
         ;mylevs_diff=['-2.6','-2.2','-1.8','-1.4','-1.0','-0.6','-0.2','0.2','0.6','1.0','1.4','1.8','2.2','2.6']
         view=0
         hadgem3_ndims=4
         hadgem3_mult=86400.
         obs_ndims=2
         obs_revlat=1
         color_rev=1
         oplot_sst=1
      END
      4 : BEGIN
         variable_name='v200'
         hg3kpp_infile=hg3kpp_dir+'/'+hg3kpp_name+'.jan-dec_amean_clim.years1-20.v200.nc'
         hg3a_infile=hg3a_dir+'/'+hg3a_name+'.jan-dec_amean_clim.years1-20.v200.nc'
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
         hg3kpp_infile=hg3kpp_dir+'/'+hg3kpp_name+'.jan-dec_amean_clim.years1-20.v850.nc'
         hg3a_infile=hg3a_dir+'/'+hg3a_name+'.jan-dec_amean_clim.years1-20.v850.nc'
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
   ENDCASE

   print,i

   hadgem3_latitude=OPEN_AND_EXTRACT(hg3a_infile,hadgem3_latname)
   hadgem3_longitude=OPEN_AND_EXTRACT(hg3a_infile,hadgem3_lonname)
   DEFINE_BOUNDARIES,plot_box,hadgem3_latitude,hadgem3_longitude,hadgem3_box_tx,/LIMIT
   hadgem3_nlat=N_ELEMENTS(hadgem3_latitude)
   hadgem3_nlon=N_ELEMENTS(hadgem3_longitude)

   obs_latitude=OPEN_AND_EXTRACT(obs_full_infile,obs_latname)
   obs_longitude=OPEN_AND_EXTRACT(obs_full_infile,obs_lonname)
   DEFINE_BOUNDARIES,plot_box,obs_latitude,obs_longitude,obs_box_tx,/LIMIT
   obs_nlat=N_ELEMENTS(obs_latitude)
   obs_nlon=N_ELEMENTS(obs_longitude)

   IF hadgem3_ndims eq 2 THEN BEGIN
      hg3a_var=REFORM(OPEN_AND_EXTRACT(hg3a_infile,hadgem3_varname,offset=[hadgem3_box_tx(1),hadgem3_box_tx(0)],$
                                            count=[hadgem3_nlon,hadgem3_nlat]))*hadgem3_mult
      hg3kpp_var=REFORM(OPEN_AND_EXTRACT(hg3kpp_infile,hadgem3_varname,offset=[hadgem3_box_tx(1),hadgem3_box_tx(0)],$
                                            count=[hadgem3_nlon,hadgem3_nlat]))*hadgem3_mult
   ENDIF ELSE BEGIN
      hg3a_var_in=REFORM(OPEN_AND_EXTRACT(hg3a_infile,hadgem3_varname,offset=[hadgem3_box_tx(1),hadgem3_box_tx(0),150],$
                                            count=[hadgem3_nlon,hadgem3_nlat,120]))*hadgem3_mult
      hg3kpp_var_in=REFORM(OPEN_AND_EXTRACT(hg3kpp_infile,hadgem3_varname,offset=[hadgem3_box_tx(1),hadgem3_box_tx(0),150],$
                                            count=[hadgem3_nlon,hadgem3_nlat,120]))*hadgem3_mult
   ENDELSE
   IF obs_ndims eq 2 THEN BEGIN
      obs_full_var_in=REFORM(OPEN_AND_EXTRACT(obs_full_infile,obs_varname,offset=[obs_box_tx(1),obs_box_tx(0),150],$
                                           count=[obs_nlon,obs_nlat,120]))
   ENDIF
   IF obs_revlat eq 1 THEN BEGIN
      temp=fltarr(obs_nlon,obs_nlat,120)
      FOR j=0,obs_nlat-1 DO $
         temp(*,j,*)=obs_full_var_in(*,obs_nlat-j-1,*)
      obs_full_var_in=temp
   ENDIF
   
   obs_full_var=fltarr(obs_nlon,obs_nlat)
   FOR i=0,obs_nlon-1 DO $
      FOR j=0,obs_nlat-1 DO $
         obs_full_var(i,j)=MEAN(obs_full_var_in(i,j,*))

   ; Overplot sst
   IF oplot_sst eq 1 THEN BEGIN
      mask=REFORM(OPEN_AND_EXTRACT('/home/ss901165/um_output/mask_n96_hadgem3-7.1.nc','lsm',$
                                   offset=[hadgem3_box_tx(1),hadgem3_box_tx(0),0,0],count=[hadgem3_nlon,hadgem3_nlat,1,1]))
      hg3a_sst_in=REFORM(OPEN_AND_EXTRACT(hg3a_dir+'/'+hg3a_name+'.jan-dec_dmean_clim.years1-20.surf_temp.nc','temp',$
                                       offset=[hadgem3_box_tx(1),hadgem3_box_tx(0),150],count=[hadgem3_nlon,hadgem3_nlat,120]))
;      hg3kpp_sst=REFORM(OPEN_AND_EXTRACT(hg3kpp_dir+'/'+hg3kpp_name+'.jan-dec_amean_clim.years1-20.surf_temp.nc','temp',$
;                                         offset=[hadgem3_box_tx(1),hadgem3_box_tx(0)],count=[hadgem3_nlon,hadgem3_nlat]))      
      hg3kpp_sst_in=REFORM(OPEN_AND_EXTRACT(hg3kpp_dir+'/'+hg3kpp_name+'.jan-dec_dmean_clim.years1-60.sst.nc','temp_2',$
                                         offset=[hadgem3_box_tx(1),hadgem3_box_tx(0),150],count=[hadgem3_nlon,hadgem3_nlat,120]))      
;      hg3a_sst=hg3kpp_sst
;      hg3a_sst[where(mask eq 1)]=!Values.F_NaN
   ENDIF
   
   hg3a_sst=fltarr(hadgem3_nlon,hadgem3_nlat)
   hg3kpp_sst=fltarr(hadgem3_nlon,hadgem3_nlat)
   hg3a_var=fltarr(hadgem3_nlon,hadgem3_nlat)
   hg3kpp_var=fltarr(hadgem3_nlon,hadgem3_nlat)
   FOR i=0,hadgem3_nlon-1 DO BEGIN
      FOR j=0,hadgem3_nlat-1 DO BEGIN
         hg3a_var(i,j)=MEAN(hg3a_var_in(i,j,*))
         hg3kpp_var(i,j)=MEAN(hg3kpp_var_in(i,j,*))
         hg3a_sst(i,j)=MEAN(hg3a_sst_in(i,j,*))
         hg3kpp_sst(i,j)=MEAN(hg3kpp_sst_in(i,j,*))
      ENDFOR
   ENDFOR
   hg3a_sst[where(mask gt 0)]=!Values.F_NaN
   hg3kpp_sst[where(mask gt 0)]=!Values.F_NaN

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/hadgem3kpp_mean_state_compare_amip_obs_ga30.'+hg3a_name+'.'+variable_name+'.ps'
   PSOPEN,file=psfile,FONT=6,CHARSIZE=105,MARGIN=1500,SPACE2=1000,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=8000
   IF color_rev eq 0 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1
   IF color_rev eq 1 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1,/REV,white=raw_white
   LEVS,MANUAL=mylevs_raw
   MAP,LATMIN=plot_box(0),LATMAX=plot_box(2),LONMIN=plot_box(1),LONMAX=plot_box(3)
   CON,X=hadgem3_longitude,Y=hadgem3_latitude,FIELD=hg3a_var,/NOLINELABELS,TITLE='Annual-mean '+variable_name+' for '+hg3a_name,$
       POSITIVE_STYLE=2,NEGATIVE_STYLE=2,ZERO_STYLE=2
   IF view eq 1 THEN $
      PSCLOSE
   IF view eq 0 THEN $
      PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/hadgem3kpp_mean_state_compare_amip_obs_ga30.'+hg3kpp_name+'.'+variable_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1000,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=8000
   IF color_rev eq 0 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1
   IF color_rev eq 1 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1,/REV,white=raw_white
   LEVS,MANUAL=mylevs_raw
   MAP,LATMIN=plot_box(0),LATMAX=plot_box(2),LONMIN=plot_box(1),LONMAX=plot_box(3)
   CON,X=hadgem3_longitude,Y=hadgem3_latitude,FIELD=hg3kpp_var,/NOLINELABELS,TITLE='Annual-mean '+variable_name+' for '+hg3kpp_name,$
       POSITIVE_STYLE=2,NEGATIVE_STYLE=2,ZERO_STYLE=2
   IF view eq 1 THEN $
      PSCLOSE
   IF view eq 0 THEN $
      PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/hadgem3kpp_mean_state_compare_amip_obs_ga30.obs_full.'+variable_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1000,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=8000
   IF color_rev eq 0 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1
   IF color_rev eq 1 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1,/REV,white=raw_white
   LEVS,MANUAL=mylevs_raw
   MAP,LATMIN=plot_box(0),LATMAX=plot_box(2),LONMIN=plot_box(1),LONMAX=plot_box(3)
   CON,X=hadgem3_longitude,Y=hadgem3_latitude,FIELD=obs_full_var,/NOLINELABELS,TITLE='Annual-mean '+variable_name+' for '+obs_name+' (1989-2008)',$
       POSITIVE_STYLE=2,NEGATIVE_STYLE=2,ZERO_STYLE=2
   IF view eq 1 THEN $
      PSCLOSE
   IF view eq 0 THEN $
      PSCLOSE,/NOVIEW
   
                                ; ---- Difference against observations
                                ;      1989-2008 ----

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/hadgem3kpp_mean_state_compare_amip_obs_ga30.'+hg3a_name+'-minus-obs_full.'+variable_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1000,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=8000
   IF color_rev eq 0 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1
   IF color_rev eq 1 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV
   LEVS,MANUAL=mylevs_diff
   MAP,LATMIN=plot_box(0),LATMAX=plot_box(2),LONMIN=plot_box(1),LONMAX=plot_box(3)
   CON,X=hadgem3_longitude,Y=hadgem3_latitude,FIELD=hg3a_var-obs_full_var,/NOLINELABELS,$
       TITLE='Annual-mean '+variable_name+' for '+hg3a_name+' minus '+obs_name+' (1989-2008)',$
       POSITIVE_STYLE=2,NEGATIVE_STYLE=2
   IF view eq 1 THEN $
      PSCLOSE
   IF view eq 0 THEN $
      PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/hadgem3kpp_mean_state_compare_amip_obs_ga30.jjas_'+hg3kpp_name+'-minus-obs_full.'+variable_name+'.ps'
   PSOPEN,file=psfile,FONT=6,CHARSIZE=150,MARGIN=1500,SPACE2=1000,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=8000
   IF color_rev eq 0 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1
   IF color_rev eq 1 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV,white=[9]
   LEVS,MANUAL=mylevs_diff
   MAP,LATMIN=plot_box(0),LATMAX=plot_box(2),LONMIN=plot_box(1),LONMAX=plot_box(3)
   
   IF oplot_sst eq 1 THEN BEGIN
      CON,X=hadgem3_longitude,Y=hadgem3_latitude,FIELD=hg3kpp_var-obs_full_var,/NOLINELABELS,$
          /NOLINES,CB_TITLE='Difference in precipitation (mm day!U-1!N)',CB_WIDTH=110
      ;sst_diff_levs=['-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9']
      sst_diff_levs=['-3.3','-2.7','-2.1','-1.5','-0.9','-0.3','0.3','0.9','1.5','2.1','2.7','3.3']
      LEVS,MANUAL=sst_diff_levs
      CON,X=hadgem3_longitude,Y=hadgem3_latitude,FIELD=hg3kpp_sst-hg3a_sst,/NOFILL,POSITIVE_STYLE=2,NEGATIVE_STYLE=1,$
          TITLE='Difference in JJAS (shading) precip and (contours, negative dot) SST for GA3-KPP minus TRMM (1999-2011)'
   ENDIF ELSE $
      CON,X=hadgem3_longitude,Y=hadgem3_latitude,FIELD=hg3kpp_var-obs_full_var,/NOLINELABELS,$
          TITLE='Annual-mean '+variable_name+' for '+hg3kpp_name+' minus '+obs_name+' (1999-2011)',$
          POSITIVE_STYLE=2,NEGATIVE_STYLE=2
   IF view eq 1 THEN $
      PSCLOSE
   IF view eq 0 THEN $
      PSCLOSE,/NOVIEW
   
                                ; ---- Difference against atmosphere-only 
                                ;      integration ----

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/hadgem3kpp_mean_state_compare_amip_obs_ga30.'+hg3kpp_name+'-minus-'+hg3a_name+'_full.'+variable_name+'.jjas.ps'
   PSOPEN,file=psfile,FONT=6,CHARSIZE=160,MARGIN=1500,SPACE2=1500,XOFFSET=1000,YOFFSET=3000,TFONT=6,TCHARSIZE=110,SPACE3=800,YSIZE=10000
   IF color_rev eq 0 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1,white=[9]
   IF color_rev eq 1 THEN $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV,white=[9]
   LEVS,MANUAL=mylevs_diff
   MAP,LATMIN=plot_box(0),LATMAX=plot_box(2),LONMIN=plot_box(1),LONMAX=plot_box(3)
   diff_precip=fltarr(hadgem3_nlon,hadgem3_nlat)
   FOR i=0,hadgem3_nlat-1 DO $
      diff_precip(*,i)=(hg3kpp_var(*,i)-hg3a_var(*,i))*COS((hadgem3_latitude(i)-20.)*!pi/180.)*0.85
   IF oplot_sst eq 1 THEN BEGIN
      CON,X=hadgem3_longitude,Y=hadgem3_latitude,FIELD=diff_precip,/NOLINELABELS,$
          /NOLINES,CB_TITLE='Difference in precipitation (mm day!U-1!N)',CB_WIDTH=110
      sst_diff_levs=['-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9']
      ;sst_diff_levs=['-3.3','-2.7','-2.1','-1.5','-0.9','-0.3','0.3','0.9','1.5','2.1','2.7','3.3']
      LEVS,MANUAL=sst_diff_levs
      CON,X=hadgem3_longitude,Y=hadgem3_latitude,FIELD=(hg3kpp_sst-hg3a_sst)/1.5,/NOFILL,POSITIVE_STYLE=2,NEGATIVE_STYLE=1,$
          TITLE='Difference in JJAS (shading) precip and (contours, negative dot) SST for GA3-KPP minus GA3'
   ENDIF ELSE BEGIN
      CON,X=hadgem3_longitude,Y=hadgem3_latitude,FIELD=hg3kpp_var-hg3a_var,/NOLINELABELS,$
          TITLE='Annual-mean '+variable_name+' for '+hg3kpp_name+' minus '+hg3a_name+' (1989-2008)',$
          POSITIVE_STYLE=2,NEGATIVE_STYLE=2
   ENDELSE
   AXES,XSTEP=30,YSTEP=10

   IF view eq 1 THEN $
      PSCLOSE
   IF view eq 0 THEN $
      PSCLOSE
;      PSCLOSE,/NOVIEW

ENDFOR



STOP

END

         
         
