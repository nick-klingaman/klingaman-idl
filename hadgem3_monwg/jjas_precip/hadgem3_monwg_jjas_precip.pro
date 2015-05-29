PRO hadgem3_monwg_jjas_precip

hadgem3_mmean_infile='/home/ss901165/um_output2/hadgem3_monwg/ahsaf/ahsaf.precip.clim.mmeans_20years.nc'
hadgem1_mmean_infile='/home/ss901165/um_output2/hadgem1_monwg/hadgem1.clim.jan-dec_mmeans.precip.monsoon_domain.nc'
hadkpp_mmean_infile='/home/ss901165/um_output2/hadkpp_npiso_60lev/hadkpp_npiso.ensmean.mjjas_mmeans.precip.monsoon_domain.n96.nc'
trmm_mmean_infile='/home/ss901165/datasets/TRMM_3B43V6/3B43V6_mmean_climatology.jan-dec.1998-2007.n96grid.nc'
dge_mean_infile='/home/ss901165/um_output/dge/precip/daily.ensmean.seasmean.jjas.precip.n96.nc'

hadgem3_start_month=6
hadgem3_stop_month=9

hadgem1_start_month=6
hadgem1_stop_month=9

hadkpp_start_month=2
hadkpp_stop_month=5

trmm_start_month=6
trmm_stop_month=9

n_months=hadgem3_stop_month-hadgem3_start_month+1

box=[-30,40,30,180]

; Precip grid information
hadgem3_longitude=OPEN_AND_EXTRACT(hadgem3_mmean_infile,'longitude')
hadgem3_latitude=OPEN_AND_EXTRACT(hadgem3_mmean_infile,'latitude')
DEFINE_BOUNDARIES,box,hadgem3_latitude,hadgem3_longitude,hadgem3_box_tx,/LIMIT
hadgem3_nlon=N_ELEMENTS(hadgem3_longitude)
hadgem3_nlat=N_ELEMENTS(hadgem3_latitude)

hadkpp_longitude=OPEN_AND_EXTRACT(hadkpp_mmean_infile,'longitude')
hadkpp_latitude=OPEN_AND_EXTRACT(hadkpp_mmean_infile,'latitude')
DEFINE_BOUNDARIES,box,hadkpp_latitude,hadkpp_longitude,hadkpp_box_tx,/LIMIT
hadkpp_nlon=N_ELEMENTS(hadkpp_longitude)
hadkpp_nlat=N_ELEMENTS(hadkpp_latitude)

trmm_longitude=OPEN_AND_EXTRACT(trmm_mmean_infile,'longitude')
trmm_latitude=OPEN_AND_EXTRACT(trmm_mmean_infile,'latitude')
DEFINE_BOUNDARIES,box,trmm_latitude,trmm_longitude,trmm_box_tx,/LIMIT
trmm_nlon=N_ELEMENTS(trmm_longitude)
trmm_nlat=N_ELEMENTS(trmm_latitude)

dge_longitude=OPEN_AND_EXTRACT(dge_mean_infile,'longitude')
dge_latitude=OPEN_AND_EXTRACT(dge_mean_infile,'latitude')
DEFINE_BOUNDARIES,box,dge_latitude,dge_longitude,dge_box_tx,/LIMIT
dge_nlon=N_ELEMENTS(dge_longitude)
dge_nlat=N_ELEMENTS(dge_latitude)

hadgem1_longitude=OPEN_AND_EXTRACT(hadgem1_mmean_infile,'longitude')
hadgem1_latitude=OPEN_AND_EXTRACT(hadgem1_mmean_infile,'latitude')
DEFINE_BOUNDARIES,box,hadgem1_latitude,hadgem1_longitude,hadgem1_box_tx,/LIMIT
hadgem1_nlon=N_ELEMENTS(hadgem1_longitude)
hadgem1_nlat=N_ELEMENTS(hadgem1_latitude)

hadgem3_precip_mean=fltarr(hadgem3_nlon,hadgem3_nlat)
hadkpp_precip_mean=fltarr(hadkpp_nlon,hadkpp_nlat)
trmm_precip_mean=fltarr(trmm_nlon,trmm_nlat)
hadgem1_precip_mean=fltarr(hadgem1_nlon,hadgem1_nlat)
FOR i=0,n_months-1 DO BEGIN
    this_precip=REFORM(OPEN_AND_EXTRACT(hadgem3_mmean_infile,'precip',$
                                       offset=[hadgem3_box_tx(1),hadgem3_box_tx(0),0,i+hadgem3_start_month-1],$
                                       count=[hadgem3_nlon,hadgem3_nlat,1,1]))*86400.
    FOR j=0,hadgem3_nlon-1 DO $
      FOR k=0,hadgem3_nlat-1 DO $
      hadgem3_precip_mean(j,k)=hadgem3_precip_mean(j,k)+this_precip(j,k)/FLOAT(n_months)    

    this_precip=REFORM(OPEN_AND_EXTRACT(hadgem1_mmean_infile,'precip',$
                                       offset=[hadgem1_box_tx(1),hadgem1_box_tx(0),0,0,i+hadgem1_start_month-1],$
                                       count=[hadgem1_nlon,hadgem1_nlat,1,1,1]))*86400.
    FOR j=0,hadgem1_nlon-1 DO $
      FOR k=0,hadgem1_nlat-1 DO $
      hadgem1_precip_mean(j,k)=hadgem1_precip_mean(j,k)+this_precip(j,k)/FLOAT(n_months)    

    this_precip=REFORM(OPEN_AND_EXTRACT(hadkpp_mmean_infile,'precip',$
                                       offset=[hadkpp_box_tx(1),hadkpp_box_tx(0),0,i+hadkpp_start_month-1],$
                                       count=[hadkpp_nlon,hadkpp_nlat,1,1]))*86400.
    FOR j=0,hadkpp_nlon-1 DO $
      FOR k=0,hadkpp_nlat-1 DO $
      hadkpp_precip_mean(j,k)=hadkpp_precip_mean(j,k)+this_precip(j,k)/FLOAT(n_months)    

    this_precip=REFORM(OPEN_AND_EXTRACT(trmm_mmean_infile,'precip',$
                                        offset=[trmm_box_tx(1),trmm_box_tx(0),i+trmm_start_month-1],$
                                        count=[trmm_nlon,trmm_nlat,1]))
    
    FOR j=0,trmm_nlon-1 DO $
      FOR k=0,trmm_nlat-1 DO $
      trmm_precip_mean(j,k)=trmm_precip_mean(j,k)+this_precip(j,k)/FLOAT(n_months)
    
ENDFOR

dge_precip_mean=REFORM(OPEN_AND_EXTRACT(dge_mean_infile,'precip',$
                                        offset=[dge_box_tx(1),dge_box_tx(0),0,0],$
                                        count=[dge_nlon,dge_nlat,1,1]))*86400.

diff_hadgem3_hadkpp=fltarr(hadgem3_nlon,hadgem3_nlat)
diff_hadgem3_trmm=fltarr(hadgem3_nlon,hadgem3_nlat)
diff_hadgem3_dge=fltarr(dge_nlon,dge_nlat)
diff_hadgem1_trmm=fltarr(hadgem1_nlon,hadgem1_nlat)
FOR j=0,hadgem3_nlat-1 DO BEGIN
  diff_hadgem3_hadkpp(*,j)=hadgem3_precip_mean(*,hadgem3_nlat-j-1)-hadkpp_precip_mean(*,j)
  diff_hadgem3_trmm(*,j)=hadgem3_precip_mean(*,hadgem3_nlat-j-1)-trmm_precip_mean(*,j)
  diff_hadgem3_dge(*,j)=hadgem3_precip_mean(*,hadgem3_nlat-j-1)-dge_precip_mean(*,j)
ENDFOR
FOR j=0,hadgem1_nlat-1 DO $
  diff_hadgem1_trmm(*,j)=hadgem1_precip_mean(*,hadgem1_nlat-j-1)-trmm_precip_mean(*,j)
  
mylevs=['1','3','5','7','9','11','13','15','17','19','21','23','25']
psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip.ahsaf_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
CS,SCALE=25,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=hadgem3_precip_mean,X=hadgem3_longitude,Y=hadgem3_latitude,$
  TITLE="JJAS-mean clim rainfall (mm day!U-1!N) from HadGEM3-AO_ahsaf_n96",$
  /NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip.hadkpp_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
CS,SCALE=25,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=hadkpp_precip_mean,X=hadkpp_longitude,Y=hadkpp_latitude,$
  TITLE="JJAS-mean clim rainfall (mm day!U-1!N) from HadKPP_n96",$
  /NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip.trmm_3B43V6_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
CS,SCALE=25,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=trmm_precip_mean,X=hadkpp_longitude,Y=hadkpp_latitude,$
  TITLE="JJAS-mean clim rainfall (mm day!U-1!N) from TRMM_3B43V6_n96 (1998-2007)",/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip.hadam3_daily_ostia_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
CS,SCALE=25,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=dge_precip_mean,X=hadkpp_longitude,Y=hadkpp_latitude,$
  TITLE="JJAS-mean clim rainfall (mm day!U-1!N) from HadAM3_Daily_OSTIA_n96",/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip.hadgem1_xciel_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
CS,SCALE=25,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=hadgem1_precip_mean,X=hadgem1_longitude,Y=hadgem1_latitude,$
  TITLE="JJAS-mean clim rainfall (mm day!U-1!N) from HadGEM1_xciel_n96",/NOLINES
PSCLOSE,/NOVIEW

mylevs=['-25','-21','-17','-13','-9','-7','-5','-3','-1','1','3','5','7','9','11','13','15','17','21','25']
psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip.ahsaf_minus_hadkpp.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1,/REV
white=FSC_COLOR("white",N_ELEMENTS(mylevs)/2+1)
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=diff_hadgem3_hadkpp,X=hadkpp_longitude,Y=hadkpp_latitude,$
  TITLE="Diff in JJAS-mean rainfall (mm day!U-1!N) for HadGEM3-AO_ahsaf_n96 minus HadKPP_n144_1mtop",$
  /NOLINES
PSCLOSE

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip.ahsaf_minus_trmm.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1,/REV
white=FSC_COLOR("white",N_ELEMENTS(mylevs)/2+1)
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=diff_hadgem3_trmm,X=hadkpp_longitude,Y=hadkpp_latitude,$
  TITLE="Diff in JJAS-mean rainfall (mm day!U-1!N) for HadGEM3-AO_ahsaf_n96 minus TRMM_3B43V6_n96 clim (1998-2007)",$
  /NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip.hadkpp_minus_trmm.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1,/REV
white=FSC_COLOR("white",N_ELEMENTS(mylevs)/2+1)
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=(hadkpp_precip_mean-trmm_precip_mean)*0.75,X=hadkpp_longitude,Y=hadkpp_latitude,$
  TITLE="Diff in JJAS-mean rainfall (mm day!U-1!N) for HadKPP_n144_1mtop minus TRMM_3B43V6_n96 clim (1998-2007)",$
  /NOLINES
PSCLOSE

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip.dge_minus_trmm.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1,/REV
white=FSC_COLOR("white",N_ELEMENTS(mylevs)/2+1)
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=dge_precip_mean-trmm_precip_mean,X=hadkpp_longitude,Y=hadkpp_latitude,$
  TITLE="Diff in JJAS-mean rainfall (mm day!U-1!N) for HadAM3_Daily_n96 minus TRMM_3B43V6_n96 clim (1998-2007)",$
  /NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip.ahsaf_minus_dge.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1,/REV
white=FSC_COLOR("white",N_ELEMENTS(mylevs)/2+1)
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=diff_hadgem3_dge,X=hadkpp_longitude,Y=hadkpp_latitude,$
  TITLE="Diff in JJAS-mean rainfall (mm day!U-1!N) for HadGEM3-AO_ahsaf_n96 minus HadAM3_Daily_n96",/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip.ahsaf_minus_hadgem1.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1,/REV
white=FSC_COLOR("white",N_ELEMENTS(mylevs)/2+1)
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=hadgem3_precip_mean-hadgem1_precip_mean,X=hadgem3_longitude,Y=hadgem3_latitude,$
  TITLE="Diff in JJAS-mean rainfall (mm day!U-1!N) for HadGEM3-AO_ahsaf_n96 minus HadGEM1_xciel_n96",/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip.hadgem1_minus_trmm.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1,/REV
white=FSC_COLOR("white",N_ELEMENTS(mylevs)/2+1)
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=diff_hadgem1_trmm,X=hadkpp_longitude,Y=hadkpp_latitude,$
  TITLE="Diff in JJAS-mean rainfall (mm day!U-1!N) for HadGEM1_xciel_n96 minus TRMM_3B43V6_n96 clim (1998-2007)",/NOLINES
PSCLOSE,/NOVIEW

STOP

END
