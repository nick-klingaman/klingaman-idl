PRO hadgem3_monwg_jjas_lh

hadgem3a_mmean_infile='/home/ss901165/um_output2/hadgem3_monwg/ahrqc/ahrqc.lh.apr-oct.clim.mmeans_20years.nc'
hadgem3ao_mmean_infile='/home/ss901165/um_output2/hadgem3_monwg/ahsaf/ahsaf.lh.apr-oct.clim.mmeans_20years.nc'
era40_infile='/home/ss901165/datasets/ERA40/SLHF/SLHF.mjjas_clim.1958-2007.monsoon_domain.n96.nc'

box=[-30,40,30,180]

hadgem3a_start_month=2
hadgem3a_stop_month=5
hadgem3a_nmonths=hadgem3a_stop_month-hadgem3a_start_month+1

hadgem3ao_start_month=2
hadgem3ao_stop_month=5
hadgem3ao_nmonths=hadgem3ao_stop_month-hadgem3ao_start_month+1

era40_start_day=30
era40_stop_day=151
era40_ndays=era40_stop_day-era40_start_day+1

hadgem3a_longitude=OPEN_AND_EXTRACT(hadgem3a_mmean_infile,'longitude')
hadgem3a_latitude=OPEN_AND_EXTRACT(hadgem3a_mmean_infile,'latitude')
DEFINE_BOUNDARIES,box,hadgem3a_latitude,hadgem3a_longitude,hadgem3a_box_tx,/LIMIT
hadgem3a_nlon=N_ELEMENTS(hadgem3a_longitude)
hadgem3a_nlat=N_ELEMENTS(hadgem3a_latitude)

hadgem3ao_longitude=OPEN_AND_EXTRACT(hadgem3ao_mmean_infile,'longitude')
hadgem3ao_latitude=OPEN_AND_EXTRACT(hadgem3ao_mmean_infile,'latitude')
DEFINE_BOUNDARIES,box,hadgem3ao_latitude,hadgem3ao_longitude,hadgem3ao_box_tx,/LIMIT
hadgem3ao_nlon=N_ELEMENTS(hadgem3ao_longitude)
hadgem3ao_nlat=N_ELEMENTS(hadgem3ao_latitude)

era40_longitude=OPEN_AND_EXTRACT(era40_infile,'longitude')
era40_latitude=OPEN_AND_EXTRACT(era40_infile,'latitude')
DEFINE_BOUNDARIES,box,era40_latitude,era40_longitude,era40_box_tx,/LIMIT
era40_nlon=N_ELEMENTS(era40_longitude)
era40_nlat=N_ELEMENTS(era40_latitude)

hadgem3a_lh_mean=fltarr(hadgem3a_nlon,hadgem3a_nlat)
hadgem3ao_lh_mean=fltarr(hadgem3ao_nlon,hadgem3ao_nlat)
era40_lh_mean=fltarr(era40_nlon,era40_nlat)

FOR i=0,hadgem3a_nmonths-1 DO BEGIN
    this_lh=REFORM(OPEN_AND_EXTRACT(hadgem3a_mmean_infile,'lh',$
                                        offset=[hadgem3a_box_tx(1),hadgem3a_box_tx(0),0,i+hadgem3a_start_month-1],$
                                        count=[hadgem3a_nlon,hadgem3a_nlat,1,1]))
    FOR j=0,hadgem3a_nlon-1 DO $
      FOR k=0,hadgem3a_nlat-1 DO $
      hadgem3a_lh_mean(j,k)=hadgem3a_lh_mean(j,k)+this_lh(j,k)/FLOAT(hadgem3a_nmonths)    
ENDFOR

FOR i=0,hadgem3ao_nmonths-1 DO BEGIN
    this_lh=REFORM(OPEN_AND_EXTRACT(hadgem3ao_mmean_infile,'lh',$
                                        offset=[hadgem3ao_box_tx(1),hadgem3ao_box_tx(0),0,i+hadgem3ao_start_month-1],$
                                        count=[hadgem3ao_nlon,hadgem3ao_nlat,1,1]))
    FOR j=0,hadgem3ao_nlon-1 DO $
      FOR k=0,hadgem3ao_nlat-1 DO $
      hadgem3ao_lh_mean(j,k)=hadgem3ao_lh_mean(j,k)+this_lh(j,k)/FLOAT(hadgem3ao_nmonths)    
ENDFOR

FOR i=0,era40_ndays-1 DO BEGIN
    this_lh=REFORM(OPEN_AND_EXTRACT(era40_infile,'SLHF',$
                                    offset=[era40_box_tx(1),era40_box_tx(0),i+era40_start_day-1],$
                                    count=[era40_nlon,era40_nlat,1]))/(-86400.)
    FOR j=0,era40_nlon-1 DO $
      FOR k=0,era40_nlat-1 DO $
      era40_lh_mean(j,k)=era40_lh_mean(j,k)+this_lh(j,k)/FLOAT(era40_ndays)    
ENDFOR

diff_hadgem3a_era40=fltarr(hadgem3a_nlon,hadgem3a_nlat)
diff_hadgem3ao_era40=fltarr(hadgem3ao_nlon,hadgem3ao_nlat)
FOR i=0,hadgem3a_nlat-1 DO $
  diff_hadgem3a_era40(*,i)=hadgem3a_lh_mean(*,hadgem3a_nlat-i-1)-era40_lh_mean(*,i)
FOR i=0,hadgem3ao_nlat-1 DO $
  diff_hadgem3ao_era40(*,i)=hadgem3ao_lh_mean(*,hadgem3ao_nlat-i-1)-era40_lh_mean(*,i)

mylevs=[-20,0,20,40,60,80,100,120,140,160,180,200,220,240]
psfile='/home/ss901165/idl/hadgem3_monwg/jjas_lh/hadgem3_monwg_jjas_lh.hadgem3a_ahrqc_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
CS,IDL=16,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=hadgem3a_lh_mean,X=hadgem3a_longitude,Y=hadgem3a_latitude,$
  TITLE="JJAS-mean clim latent heat flux (W m!U-2!N) from HadGEM3-A_ahrqc_n96",$
  /NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_lh/hadgem3_monwg_jjas_lh.hadgem3ao_ahsaf_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
CS,IDL=16,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=hadgem3ao_lh_mean,X=hadgem3ao_longitude,Y=hadgem3ao_latitude,$
  TITLE="JJAS-mean clim latent heat flux (W m!U-2!N) from HadGEM3-AO_ahsaf_n96",$
  /NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_lh/hadgem3_monwg_jjas_lh.era40_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
CS,IDL=16,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=era40_lh_mean,X=era40_longitude,Y=era40_latitude,$
  TITLE="JJAS-mean clim latent heat flux (W m!U-2!N) from ERA-40 (1958-2007)",$
  /NOLINES
PSCLOSE,/NOVIEW

mylevs=[-75,-65,-55,-45,-35,-25,-15,-5,5,15,25,35,45,55,65,75]
psfile='/home/ss901165/idl/hadgem3_monwg/jjas_lh/hadgem3_monwg_jjas_lh.ahrqc_minus_ahsaf.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1,/REV
white=FSC_COLOR("white",N_ELEMENTS(mylevs)/2+2)
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=hadgem3a_lh_mean-hadgem3ao_lh_mean,X=hadgem3a_longitude,Y=hadgem3a_latitude,$
  TITLE="Diff in JJAS-mean clim latent heat flux (W m!U-2!N) for HadGEM3-A_ahrqc_n96 minus HadGEM3-AO_ahsaf_n96",$
  /NOLINES
PSCLOSE

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_lh/hadgem3_monwg_jjas_lh.ahrqc_minus_era40.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1,/REV
white=FSC_COLOR("white",N_ELEMENTS(mylevs)/2+2)
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=diff_hadgem3a_era40,X=era40_longitude,Y=era40_latitude,$
  TITLE="Diff in JJAS-mean clim latent heat flux (W m!U-2!N) for HadGEM3-A_ahrqc_n96 minus ERA-40_n96 (1958-2007)",$
  /NOLINES
PSCLOSE

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_lh/hadgem3_monwg_jjas_lh.ahsaf_minus_era40.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1,/REV
white=FSC_COLOR("white",N_ELEMENTS(mylevs)/2+2)
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=diff_hadgem3ao_era40,X=era40_longitude,Y=era40_latitude,$
  TITLE="Diff in JJAS-mean clim latent heat flux (W m!U-2!N) for HadGEM3-AO_ahsaf_n96 minus ERA-40_n96 (1958-2007)",$
  /NOLINES
PSCLOSE

STOP

END
