PRO hadgem3_monwg_jjas_precip_entrain

hadgem3_akkvg_infile='/home/ss901165/um_output3/hadgem3_monwg/akkvg/akkvg.jan-dec_dmean_clim.i2-j7.precip.nc'
amip2_ctl_infile='/home/ss901165/um_output3/hadgem3a_amip2_control_vn74/hadgem3a_amip2_ctl_vn74.jan-dec_dmean_clim.years1-30.precip.nc'
amip2_15xentrain_infile='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmean_clim.years1-30.precip.nc'
trmm_infile='/home/ss901165/datasets_mango/TRMM_3B42V6/n96/TRMM_3B42v6A.jan-dec_dmean_clim.1999-2009.n96.nc'

time_offset=150
n_days=120

box=[-30,40,30,180]

; Precip grid information
hadgem3_longitude=OPEN_AND_EXTRACT(hadgem3_akkvg_infile,'longitude')
hadgem3_latitude=OPEN_AND_EXTRACT(hadgem3_akkvg_infile,'latitude')
DEFINE_BOUNDARIES,box,hadgem3_latitude,hadgem3_longitude,hadgem3_box_tx,/LIMIT
hadgem3_nlon=N_ELEMENTS(hadgem3_longitude)
hadgem3_nlat=N_ELEMENTS(hadgem3_latitude)

trmm_longitude=OPEN_AND_EXTRACT(trmm_infile,'longitude')
trmm_latitude=OPEN_AND_EXTRACT(trmm_infile,'latitude')
DEFINE_BOUNDARIES,box,trmm_latitude,trmm_longitude,trmm_box_tx,/LIMIT
trmm_nlon=N_ELEMENTS(trmm_longitude)
trmm_nlat=N_ELEMENTS(trmm_latitude)

hadgem3_akkvg_precip_mean=fltarr(hadgem3_nlon,hadgem3_nlat)
amip2_ctl_precip_mean=fltarr(hadgem3_nlon,hadgem3_nlat)
amip2_15xentrain_precip_mean=fltarr(hadgem3_nlon,hadgem3_nlat)
trmm_precip_mean=fltarr(trmm_nlon,trmm_nlat)

this_precip=REFORM(OPEN_AND_EXTRACT(hadgem3_akkvg_infile,'precip',$
                                    offset=[hadgem3_box_tx(1),hadgem3_box_tx(0),time_offset],$
                                    count=[hadgem3_nlon,hadgem3_nlat,n_days]))*86400.
FOR j=0,hadgem3_nlon-1 DO $
   FOR k=0,hadgem3_nlat-1 DO $
      hadgem3_akkvg_precip_mean(j,k)=MEAN(this_precip(j,k,*))

this_precip=REFORM(OPEN_AND_EXTRACT(amip2_ctl_infile,'precip',$
                                    offset=[hadgem3_box_tx(1),hadgem3_box_tx(0),time_offset],$
                                    count=[hadgem3_nlon,hadgem3_nlat,n_days]))*86400.
FOR j=0,hadgem3_nlon-1 DO $
   FOR k=0,hadgem3_nlat-1 DO $
      amip2_ctl_precip_mean(j,k)=MEAN(this_precip(j,k,*))

this_precip=REFORM(OPEN_AND_EXTRACT(amip2_15xentrain_infile,'precip',$
                                    offset=[hadgem3_box_tx(1),hadgem3_box_tx(0),time_offset],$
                                    count=[hadgem3_nlon,hadgem3_nlat,n_days]))*86400.
FOR j=0,hadgem3_nlon-1 DO $
   FOR k=0,hadgem3_nlat-1 DO $
      amip2_15xentrain_precip_mean(j,k)=MEAN(this_precip(j,k,*))

this_precip=REFORM(OPEN_AND_EXTRACT(trmm_infile,'precip',$
                                    offset=[trmm_box_tx(1),trmm_box_tx(0),time_offset],$
                                    count=[trmm_nlon,trmm_nlat,n_days]))    
FOR j=0,trmm_nlon-1 DO $
   FOR k=0,trmm_nlat-1 DO $
      trmm_precip_mean(j,k)=MEAN(this_precip(j,k,*))

diff_akkvg_trmm=fltarr(hadgem3_nlon,hadgem3_nlat)
diff_amip2_ctl_trmm=fltarr(hadgem3_nlon,hadgem3_nlat)
diff_amip2_15xentrain_trmm=fltarr(hadgem3_nlon,hadgem3_nlat)
FOR j=0,hadgem3_nlat-1 DO BEGIN
   diff_akkvg_trmm(*,j)=hadgem3_akkvg_precip_mean(*,hadgem3_nlat-j-1)-trmm_precip_mean(*,j)
   diff_amip2_ctl_trmm(*,j)=amip2_ctl_precip_mean(*,hadgem3_nlat-j-1)-trmm_precip_mean(*,j)
   diff_amip2_15xentrain_trmm(*,j)=amip2_15xentrain_precip_mean(*,hadgem3_nlat-j-1)-trmm_precip_mean(*,j)
ENDFOR
  
mylevs=['1','3','5','7','9','11','13','15','17','19','21','23','25']
psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_entrain.akkvg_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[2]
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=hadgem3_akkvg_precip_mean,X=hadgem3_longitude,Y=hadgem3_latitude,$
    TITLE="JJAS-mean clim rainfall (mm day!U-1!N) from HadGEM3 GA3.0 akkvg (1982-1997)",$
    /NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_entrain.hadgem3a_amip2_ctl_vn74_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[2]
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=amip2_ctl_precip_mean,X=hadgem3_longitude,Y=hadgem3_latitude,$
    TITLE="JJAS-mean clim rainfall (mm day!U-1!N) from Nick HadGEM3-A (vn7.4) 1.0x entrain (30 years)",$
    /NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_entrain.trmm_3B42v6_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[2]
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=trmm_precip_mean,X=trmm_longitude,Y=trmm_latitude,$
    TITLE="JJAS-mean clim rainfall (mm day!U-1!N) from TRMM 3B42v6A (1999-2009)",/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_entrain.hadgem3a_amip2_15xentrain_vn74_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[2]
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=amip2_15xentrain_precip_mean,X=hadgem3_longitude,Y=hadgem3_latitude,$
    TITLE="JJAS-mean clim rainfall (mm day!U-1!N) from Nick HadGEM3-A (vn7.4) 1.5x entrain (30 years)",/NOLINES
PSCLOSE,/NOVIEW

mylevs=['-13','-11','-9','-7','-5','-3','-1','1','3','5','7','9','11','13']
psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_entrain.akkvg_minus_trmm.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1,/REV
;white=FSC_COLOR("white",N_ELEMENTS(mylevs)/2+1)
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=diff_akkvg_trmm,X=hadgem3_longitude,Y=trmm_latitude,$
  TITLE="Diff in JJAS-mean rainfall (mm day!U-1!N) for HadGEM3 akkvg minus TRMM",$
  /NOLINES
PSCLOSE

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_entrain.amip2-ctl_minus_trmm.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1,/REV
;white=FSC_COLOR("white",N_ELEMENTS(mylevs)/2+1)
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=diff_amip2_ctl_trmm,X=hadgem3_longitude,Y=trmm_latitude,$
  TITLE="Diff in JJAS-mean rainfall (mm day!U-1!N) for HadGEM3 (vn7.4) AMIP2 1.0x entrain minus TRMM",$
  /NOLINES
PSCLOSE

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_entrain.amip2-1.5xentrain_minus_trmm.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1,/REV
;white=FSC_COLOR("white",N_ELEMENTS(mylevs)/2+1)
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=diff_amip2_15xentrain_trmm,X=hadgem3_longitude,Y=trmm_latitude,$
  TITLE="Diff in JJAS-mean rainfall (mm day!U-1!N) for HadGEM3 (vn7.4) AMIP2 1.5x entrain minus TRMM",$
  /NOLINES
PSCLOSE

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_entrain.amip2-1.5xentrain_minus_amip2-ctl.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1,/REV
;white=FSC_COLOR("white",N_ELEMENTS(mylevs)/2+1)
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=amip2_15xentrain_precip_mean-amip2_ctl_precip_mean,X=hadgem3_longitude,Y=hadgem3_latitude,$
  TITLE="Diff in JJAS-mean rainfall (mm day!U-1!N) for HadGEM3 (vn7.4) AMIP2 1.5x entrain minus AMIP2 1.0x entrain",$
  /NOLINES
PSCLOSE

STOP

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_entrain.ahsaf_minus_trmm.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1,/REV
white=FSC_COLOR("white",N_ELEMENTS(mylevs)/2+1)
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=diff_hadgem3_trmm,X=hadkpp_longitude,Y=hadkpp_latitude,$
  TITLE="Diff in JJAS-mean rainfall (mm day!U-1!N) for HadGEM3-AO_ahsaf_n96 minus TRMM_3B43V6_n96 clim (1998-2007)",$
  /NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_entrain.hadkpp_minus_trmm.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1,/REV
white=FSC_COLOR("white",N_ELEMENTS(mylevs)/2+1)
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=(hadkpp_precip_mean-trmm_precip_mean)*0.75,X=hadkpp_longitude,Y=hadkpp_latitude,$
  TITLE="Diff in JJAS-mean rainfall (mm day!U-1!N) for HadKPP_n144_1mtop minus TRMM_3B43V6_n96 clim (1998-2007)",$
  /NOLINES
PSCLOSE

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_entrain.dge_minus_trmm.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1,/REV
white=FSC_COLOR("white",N_ELEMENTS(mylevs)/2+1)
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=dge_precip_mean-trmm_precip_mean,X=hadkpp_longitude,Y=hadkpp_latitude,$
  TITLE="Diff in JJAS-mean rainfall (mm day!U-1!N) for HadAM3_Daily_n96 minus TRMM_3B43V6_n96 clim (1998-2007)",$
  /NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_entrain.ahsaf_minus_dge.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1,/REV
white=FSC_COLOR("white",N_ELEMENTS(mylevs)/2+1)
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=diff_hadgem3_dge,X=hadkpp_longitude,Y=hadkpp_latitude,$
  TITLE="Diff in JJAS-mean rainfall (mm day!U-1!N) for HadGEM3-AO_ahsaf_n96 minus HadAM3_Daily_n96",/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_entrain.ahsaf_minus_hadgem1.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1,/REV
white=FSC_COLOR("white",N_ELEMENTS(mylevs)/2+1)
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=hadgem3_precip_mean-hadgem1_precip_mean,X=hadgem3_longitude,Y=hadgem3_latitude,$
  TITLE="Diff in JJAS-mean rainfall (mm day!U-1!N) for HadGEM3-AO_ahsaf_n96 minus HadGEM1_xciel_n96",/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_entrain.hadgem1_minus_trmm.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1,/REV
white=FSC_COLOR("white",N_ELEMENTS(mylevs)/2+1)
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=diff_hadgem1_trmm,X=hadkpp_longitude,Y=hadkpp_latitude,$
  TITLE="Diff in JJAS-mean rainfall (mm day!U-1!N) for HadGEM1_xciel_n96 minus TRMM_3B43V6_n96 clim (1998-2007)",/NOLINES
PSCLOSE,/NOVIEW

STOP

END
