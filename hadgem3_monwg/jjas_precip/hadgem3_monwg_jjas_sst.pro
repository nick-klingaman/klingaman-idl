PRO hadgem3_monwg_jjas_sst_v2


nick_1xentrain_infile='/home/ss901165/um_output3/hadgem3a_amip2_control_vn74/hadgem3a_amip2_ctl_vn74.jan-dec_dmean_clim.years1-30.surf_temp.nc'
nick_15xentrain_infile='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmean_clim.years1-30.surf_temp.nc'
nick_15xentrain_iav_infile='/home/ss901165/um_output4/hadgem3a_amip2_iav_1.5xentrain_vn74/hadgem3a_amip2_iav_1.5xentrain_vn74.may-sep_dmean_clim.years1-22.surf_temp.nc'

mylevs=['290','291','292','293','294','295','296','297','298','299','300','301','302','303']
mylevs_diff=['-0.75','-0.65','-0.55','-0.45','-0.35','-0.25','-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75']

n96_mask_file='/home/ss901165/um_output/mask_n96.nc'

box=[-40,0,40,360]

n_sets=1
FOR m=0,n_sets-1 DO BEGIN
   CASE m OF
      0 : BEGIN
         expt_infile=nick_15xentrain_iav_infile
         ctrl_infile=nick_15xentrain_infile
         expt_name='hadgem3a_amip2_iav_1.5xentrain_vn74'
         ctrl_name='hadgem3a_amip2_1.5xentrain_vn74'
         expt_desc='Nick AMIP2 IAV SST ctl (22 years)'
         ctrl_desc='Nick AMIP2 clim SST 1.5x entrain (30 years)'
         expt_offset=30
         ctrl_offset=150
         expt_count=120
         ctrl_count=120
         ctrl_revlat=0
         expt_mult=1.
         ctrl_mult=1.
      END
      1 : BEGIN
         expt_infile=nick_15xentrain_infile
         ctrl_infile=trmm_infile
         expt_name='hadgem3a_amip2_1.5xentrain_vn74'
         ctrl_name='trmm'
         expt_desc='Nick AMIP2 clim SST 1.5x entrain (30 years)'
         ctrl_desc='TRMM (1999-2010)'
         expt_offset=150
         ctrl_offset=150
         expt_count=120
         ctrl_count=120
         ctrl_revlat=1
         expt_mult=86400.
         ctrl_mult=1.
      END
      2 : BEGIN
         expt_infile=nick_15xentrain_infile
         ctrl_infile=nick_1xentrain_infile
         expt_name='hadgem3a_amip2_1.5xentrain_vn74'
         ctrl_name='hadgem3a_amip2_ctl_vn74'
         expt_desc='Nick AMIP2 clim SST 1.5x entrain (30 years)'
         ctrl_desc='Nick AMIP2 clim SST ctl (1.0x entrain, 30 years)'
         expt_offset=150
         ctrl_offset=150
         expt_count=120
         ctrl_count=120
         ctrl_revlat=0
         expt_mult=86400.
         ctrl_mult=86400.
      END
      3 : BEGIN
         expt_infile=captivate_final_infile
         ctrl_infile=trmm_infile
         expt_name='captivate_final_n96_orca1'
         ctrl_name='trmm'
         expt_desc='Captivate final N96-ORCA1 (ajtzr, 30 years)'
         ctrl_desc='TRMM (1999-2010)'
         expt_offset=150
         ctrl_offset=150
         expt_count=120
         ctrl_count=120
         ctrl_revlat=1
         expt_mult=86400.
         ctrl_mult=1.
      END
      4 : BEGIN
         expt_infile=akkvg_infile
         ctrl_infile=trmm_infile
         expt_name='captivate_ga3.0_n96_amip2'
         ctrl_name='trmm'
         expt_desc='Captivate GA3.0 N96 AMIP2 (akkvg, 20 years)'
         ctrl_desc='TRMM (1999-2010)'
         expt_offset=150
         ctrl_offset=150
         expt_count=120
         ctrl_count=120
         ctrl_revlat=1
         expt_mult=86400.
         ctrl_mult=1.
      END
      5 : BEGIN
         expt_infile=nick_15xentrain_iav_infile
         ctrl_infile=trmm_infile
         expt_name='hadgem3a_amip2_iav_1.5xentrain_vn74'
         ctrl_name='trmm'
         expt_desc='Nick AMIP2 IAV SST 1.5x entrain (22 years)'
         ctrl_desc='TRMM (1999-2010)'
         expt_offset=30
         ctrl_offset=150
         expt_count=120
         ctrl_count=120
         ctrl_revlat=1
         expt_mult=86400.
         ctrl_mult=1.
      END
      6 : BEGIN
         expt_infile=nick_15xentrain_iav_infile
         ctrl_infile=nick_15xentrain_infile
         expt_name='hadgem3a_amip2_iav_1.5xentrain_vn74'
         ctrl_name='hadgem3a_amip2_1.5xentrain_vn74'
         expt_desc='Nick AMIP2 IAV SST 1.5x entrain (22 years)'
         ctrl_desc='Nick AMIP2 clim SST 1.5x entrain (30 years)'
         expt_offset=30
         ctrl_offset=150
         expt_count=120
         ctrl_count=120
         ctrl_revlat=0
         expt_mult=86400.
         ctrl_mult=86400.
      END
   ENDCASE

                                ; Temperature grid information
   expt_longitude=OPEN_AND_EXTRACT(expt_infile,'longitude')
   expt_latitude=OPEN_AND_EXTRACT(expt_infile,'latitude')
   DEFINE_BOUNDARIES,box,expt_latitude,expt_longitude,expt_box_tx,/LIMIT
   expt_nlon=N_ELEMENTS(expt_longitude)
   expt_nlat=N_ELEMENTS(expt_latitude)
   
   ctrl_longitude=OPEN_AND_EXTRACT(ctrl_infile,'longitude')
   ctrl_latitude=OPEN_AND_EXTRACT(ctrl_infile,'latitude')
   DEFINE_BOUNDARIES,box,ctrl_latitude,ctrl_longitude,ctrl_box_tx,/LIMIT
   ctrl_nlon=N_ELEMENTS(ctrl_longitude)
   ctrl_nlat=N_ELEMENTS(ctrl_latitude)

   expt_sst_mean=fltarr(expt_nlon,expt_nlat)
   ctrl_sst_mean=fltarr(ctrl_nlon,ctrl_nlat)

   mask_longitude=OPEN_AND_EXTRACT(n96_mask_file,'longitude')
   mask_latitude=OPEN_AND_EXTRACT(n96_mask_file,'latitude')
   DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
   mask_nlon=N_ELEMENTS(mask_longitude)
   mask_nlat=N_ELEMENTS(mask_latitude)
   
   n96_mask=REFORM(OPEN_AND_EXTRACT(n96_mask_file,'lsm',offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                                    count=[mask_nlon,mask_nlat,1,1]))
   n96_mask_rev=fltarr(mask_nlon,mask_nlat)
   FOR i=0,mask_nlon-1 DO $
      FOR j=0,mask_nlat-1 DO $
         n96_mask_rev(i,mask_nlat-j-1)=n96_mask(i,j)

   this_sst=REFORM(OPEN_AND_EXTRACT(expt_infile,'temp',$
                                       offset=[expt_box_tx(1),expt_box_tx(0),expt_offset],$
                                       count=[expt_nlon,expt_nlat,expt_count]))*expt_mult
   FOR j=0,expt_nlon-1 DO $
      FOR k=0,expt_nlat-1 DO $
         expt_sst_mean(j,k)=MEAN(this_sst(j,k,*))
    
   this_sst=REFORM(OPEN_AND_EXTRACT(ctrl_infile,'temp_1',$
                                       offset=[ctrl_box_tx(1),ctrl_box_tx(0),ctrl_offset],$
                                       count=[ctrl_nlon,ctrl_nlat,ctrl_count]))*ctrl_mult
    FOR j=0,ctrl_nlon-1 DO $
       FOR k=0,ctrl_nlat-1 DO $
          ctrl_sst_mean(j,k)=MEAN(this_sst(j,k,*))    
    
    expt_sst_mean[where(n96_mask_rev eq 1)]=!Values.F_NaN
    ctrl_sst_mean[where(n96_mask_rev eq 1)]=!Values.F_NaN

    diff_expt_ctrl=fltarr(expt_nlon,expt_nlat)
    IF ctrl_revlat eq 1 THEN BEGIN
       FOR j=0,ctrl_nlat-1 DO $
          diff_expt_ctrl(*,j)=expt_sst_mean(*,j)-ctrl_sst_mean(*,ctrl_nlat-j-1)
    ENDIF ELSE $
       diff_expt_ctrl=expt_sst_mean-ctrl_sst_mean

   
   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_sst.'+expt_name+'.clim_sst.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=10000,SPACE3=500
   CS,SCALE=40,NCOLS=N_ELEMENTS(mylevs)+1
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=expt_sst_mean,X=expt_longitude,Y=expt_latitude,$
       TITLE="JJAS clim SST from "+expt_desc,CB_TITLE='Kelvin',$
       /NOLINES
   PSCLOSE;,/NOVIEW
    
   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_sst.'+ctrl_name+'.clim_sst.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=10000,SPACE3=500
   CS,SCALE=40,NCOLS=N_ELEMENTS(mylevs)+1
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=ctrl_sst_mean,X=ctrl_longitude,Y=ctrl_latitude,$
       TITLE="JJAS clim SST from "+ctrl_desc,CB_TITLE='Kelvin',$
       /NOLINES
   PSCLOSE;,/NOVIEW
   
   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_sst.'+expt_name+'-minus-'+ctrl_name+'.clim_sst.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
   LEVS,MANUAL=mylevs_diff
   CON,FIELD=diff_expt_ctrl,X=expt_longitude,Y=expt_latitude,$
       TITLE="Diff in JJAS SST for "+expt_desc+" minus "+ctrl_desc,CB_TITLE='Kelvin',$
       /NOLINES
   PSCLOSE;,/NOVIEW
ENDFOR

STOP
END
