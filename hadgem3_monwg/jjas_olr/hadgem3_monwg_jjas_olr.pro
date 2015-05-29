PRO hadgem3_monwg_jjas_olr

hadgem3ao_clim_file='/home/ss901165/um_output2/hadgem3_monwg/ahsaf/ahsaf.olr.apr-oct.clim.mmeans_20years_2.5x2.5.nc'
hadgem3a_clim_file='/home/ss901165/um_output2/hadgem3_monwg/ahrqc/ahrqc.olr.apr-oct.clim.mmeans_20years_2.5x2.5.nc'
hadgem1_clim_file='/home/ss901165/um_output2/hadgem1_monwg/hadgem1.clim.jan-dec_mmeans.olr.monsoon_domain_2.5x2.5.nc'
hadkpp_clim_file='/home/ss901165/um_output2/hadkpp_npiso_60lev/hadkpp_npiso.ensmean.mjjas_mmeans.olr.monsoon_domain_2.5x2.5.nc'
higem_clim_file='/home/ss901165/um_output2/higem_monwg/higem.clim.jan-dec_mmeans.olr.monsoon_domain_2.5x2.5.nc'
noaa_clim_file='/home/ss901165/datasets/NOAA_CIRES_OLR/climatology/olr.mon.ltm.nc'

box=[-31,40,30,182]

; Get grid information
hadgem3ao_longitude=OPEN_AND_EXTRACT(hadgem3ao_clim_file,'longitude')
hadgem3ao_latitude=OPEN_AND_EXTRACT(hadgem3ao_clim_file,'latitude')
DEFINE_BOUNDARIES,box,hadgem3ao_latitude,hadgem3ao_longitude,hadgem3ao_box_tx,/LIMIT
hadgem3ao_nlon=N_ELEMENTS(hadgem3ao_longitude)
hadgem3ao_nlat=N_ELEMENTS(hadgem3ao_latitude)

hadgem3a_longitude=OPEN_AND_EXTRACT(hadgem3a_clim_file,'longitude')
hadgem3a_latitude=OPEN_AND_EXTRACT(hadgem3a_clim_file,'latitude')
DEFINE_BOUNDARIES,box,hadgem3a_latitude,hadgem3a_longitude,hadgem3a_box_tx,/LIMIT
hadgem3a_nlon=N_ELEMENTS(hadgem3a_longitude)
hadgem3a_nlat=N_ELEMENTS(hadgem3a_latitude)

hadgem1_longitude=OPEN_AND_EXTRACT(hadgem1_clim_file,'longitude')
hadgem1_latitude=OPEN_AND_EXTRACT(hadgem1_clim_file,'latitude')
DEFINE_BOUNDARIES,box,hadgem1_latitude,hadgem1_longitude,hadgem1_box_tx,/LIMIT
hadgem1_nlon=N_ELEMENTS(hadgem1_longitude)
hadgem1_nlat=N_ELEMENTS(hadgem1_latitude)

hadkpp_longitude=OPEN_AND_EXTRACT(hadkpp_clim_file,'longitude')
hadkpp_latitude=OPEN_AND_EXTRACT(hadkpp_clim_file,'latitude')
DEFINE_BOUNDARIES,box,hadkpp_latitude,hadkpp_longitude,hadkpp_box_tx,/LIMIT
hadkpp_nlon=N_ELEMENTS(hadkpp_longitude)
hadkpp_nlat=N_ELEMENTS(hadkpp_latitude)

higem_longitude=OPEN_AND_EXTRACT(higem_clim_file,'longitude')
higem_latitude=OPEN_AND_EXTRACT(higem_clim_file,'latitude')
DEFINE_BOUNDARIES,box,higem_latitude,higem_longitude,higem_box_tx,/LIMIT
higem_nlon=N_ELEMENTS(higem_longitude)
higem_nlat=N_ELEMENTS(higem_latitude)

noaa_longitude=OPEN_AND_EXTRACT(noaa_clim_file,'lon')
noaa_latitude=OPEN_AND_EXTRACT(noaa_clim_file,'lat')
DEFINE_BOUNDARIES,box,noaa_latitude,noaa_longitude,noaa_box_tx,/LIMIT
noaa_nlon=N_ELEMENTS(noaa_longitude)
noaa_nlat=N_ELEMENTS(noaa_latitude)

hadgem3ao_offset_jun=2
hadgem3a_offset_jun=2
hadgem1_offset_jun=5
hadkpp_offset_jun=1
higem_offset_jun=5
noaa_offset_jun=5
n_months=4

hadgem3ao_clim=fltarr(hadgem3ao_nlon,hadgem3ao_nlat)
hadgem3a_clim=fltarr(hadgem3a_nlon,hadgem3a_nlat)
hadgem1_clim=fltarr(hadgem1_nlon,hadgem1_nlat)
hadkpp_clim=fltarr(hadkpp_nlon,hadkpp_nlat)
higem_clim=fltarr(higem_nlon,higem_nlat)
noaa_clim=fltarr(noaa_nlon,noaa_nlat)
FOR i=0,n_months-1 DO BEGIN
    hadgem3ao_month=REFORM(OPEN_AND_EXTRACT(hadgem3ao_clim_file,'olr',$
                                          offset=[hadgem3ao_box_tx(1),hadgem3ao_box_tx(0),0,hadgem3ao_offset_jun+i],$
                                          count=[hadgem3ao_nlon,hadgem3ao_nlat,1,1]))
    hadgem3a_month=REFORM(OPEN_AND_EXTRACT(hadgem3a_clim_file,'olr',$
                                          offset=[hadgem3a_box_tx(1),hadgem3a_box_tx(0),0,hadgem3a_offset_jun+i],$
                                          count=[hadgem3a_nlon,hadgem3a_nlat,1,1]))
    hadgem1_month=REFORM(OPEN_AND_EXTRACT(hadgem1_clim_file,'olr',$
                                          offset=[hadgem1_box_tx(1),hadgem1_box_tx(0),hadgem1_offset_jun+i],$
                                          count=[hadgem1_nlon,hadgem1_nlat,1]))
    hadkpp_month=REFORM(OPEN_AND_EXTRACT(hadkpp_clim_file,'olr',$
                                          offset=[hadkpp_box_tx(1),hadkpp_box_tx(0),0,hadkpp_offset_jun+i],$
                                          count=[hadkpp_nlon,hadkpp_nlat,1,1]))
    higem_month=REFORM(OPEN_AND_EXTRACT(higem_clim_file,'olr',$
                                        offset=[higem_box_tx(1),higem_box_tx(0),0,higem_offset_jun+i],$
                                        count=[higem_nlon,higem_nlat,1,1]))
    noaa_month=REFORM(OPEN_AND_EXTRACT(noaa_clim_file,'olr',$
                                       offset=[noaa_box_tx(1),noaa_box_tx(0),noaa_offset_jun+i],$
                                       count=[noaa_nlon,noaa_nlat,1]))*0.01+327.65

    FOR j=0,hadgem3ao_nlon-1 DO $
      FOR k=0,hadgem3ao_nlat-1 DO $
      hadgem3ao_clim(j,k)=hadgem3ao_clim(j,k)+hadgem3ao_month(j,k)/FLOAT(n_months)

    FOR j=0,hadgem3a_nlon-1 DO $
      FOR k=0,hadgem3a_nlat-1 DO $
      hadgem3a_clim(j,k)=hadgem3a_clim(j,k)+hadgem3a_month(j,k)/FLOAT(n_months)
    
    FOR j=0,hadgem1_nlon-1 DO $
      FOR k=0,hadgem1_nlat-1 DO $
      hadgem1_clim(j,k)=hadgem1_clim(j,k)+hadgem1_month(j,k)/FLOAT(n_months)
    
    FOR j=0,hadkpp_nlon-1 DO $
      FOR k=0,hadkpp_nlat-1 DO $
      hadkpp_clim(j,k)=hadkpp_clim(j,k)+hadkpp_month(j,k)/FLOAT(n_months)

    FOR j=0,higem_nlon-1 DO $
      FOR k=0,higem_nlat-1 DO $
      higem_clim(j,k)=higem_clim(j,k)+higem_month(j,k)/FLOAT(n_months)

    FOR j=0,noaa_nlon-1 DO $
      FOR k=0,noaa_nlat-1 DO $
      noaa_clim(j,k)=noaa_clim(j,k)+noaa_month(j,k)/FLOAT(n_months)
ENDFOR

mylevs=['180','190','200','210','220','230','240','250','260','270','280','290','300','310','320']
psfile='/home/ss901165/idl/hadgem3_monwg/jjas_olr/hadgem3_monwg_jjas_olr.hadgem3ao_ahsaf_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
LEVS,MANUAL=mylevs
CS,SCALE=22,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
CON,FIELD=hadgem3ao_clim,X=hadgem3ao_longitude,Y=hadgem3ao_latitude,$
  TITLE="JJAS-mean clim OLR (W m!U-2!N) from HadGEM3-AO_ahsaf_n96 (20 years)",/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_olr/hadgem3_monwg_jjas_olr.hadgem3a_ahrqc_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
LEVS,MANUAL=mylevs
CS,SCALE=22,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
CON,FIELD=hadgem3a_clim,X=hadgem3a_longitude,Y=hadgem3a_latitude,$
  TITLE="JJAS-mean clim OLR (W m!U-2!N) from HadGEM3-A_ahrqc_n96 (20 years)",/NOLINES
PSCLOSE

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_olr/hadgem3_monwg_jjas_olr.hadgem1_xciel_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
LEVS,MANUAL=mylevs
CS,SCALE=22,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
CON,FIELD=hadgem1_clim,X=hadgem1_longitude,Y=hadgem1_latitude,$
  TITLE="JJAS-mean clim OLR (W m!U-2!N) from HadGEM1a_xciel_n96 (30 years)",/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_olr/hadgem3_monwg_jjas_olr.hadkpp_1mtop_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
LEVS,MANUAL=mylevs
CS,SCALE=22,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
CON,FIELD=hadkpp_clim,X=hadkpp_longitude,Y=hadkpp_latitude,$
  TITLE="JJAS-mean clim OLR (W m!U-2!N) from HadKPP_n144_1mtop (30 members)",/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_olr/hadgem3_monwg_jjas_olr.higem_xbylr_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
LEVS,MANUAL=mylevs
CS,SCALE=22,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
CON,FIELD=higem_clim,X=higem_longitude,Y=higem_latitude,$
  TITLE="JJAS-mean clim OLR (W m!U-2!N) from HiGEM_xbylr_n144 (81 years)",/NOLINES
PSCLOSE

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_olr/hadgem3_monwg_jjas_olr.noaa_cires_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
LEVS,MANUAL=mylevs
CS,SCALE=22,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
CON,FIELD=noaa_clim,X=noaa_longitude,Y=noaa_latitude,$
  TITLE="JJAS-mean clim OLR (W m!U-2!N) from NOAA-CIRES AVHRR (1975-2008)",/NOLINES
PSCLOSE,/NOVIEW

;diff_hadgem3_noaa=fltarr(hadgem3_nlon,hadgem3_nlat)
;FOR i=0,hadgem3_nlon-1 DO $
;  FOR j=0,hadgem3_nlat-1 DO $
;  diff_hadgem3_noaa(i,j)=hadgem3_clim(i,j)-noaa_clim(i,hadgem3_nlat-j-1)

mylevs=['-55','-45','-35','-25','-15','-5','5','15','25','35','45','55']
psfile='/home/ss901165/idl/hadgem3_monwg/jjas_olr/hadgem3_monwg_jjas_olr.diff_ahsaf_noaa.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
LEVS,MANUAL=mylevs
CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
CON,FIELD=hadgem3ao_clim-noaa_clim,X=hadgem3ao_longitude,Y=hadgem3ao_latitude,$
  TITLE="Diff in JJAS-mean clim OLR (W m!U-2!N) for HadGEM3-AO_ahsaf minus NOAA-CIRES AVHRR",/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_olr/hadgem3_monwg_jjas_olr.diff_ahrqc_noaa.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
LEVS,MANUAL=mylevs
CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
CON,FIELD=hadgem3a_clim-noaa_clim,X=hadgem3a_longitude,Y=hadgem3a_latitude,$
  TITLE="Diff in JJAS-mean clim OLR (W m!U-2!N) for HadGEM3-A_ahrqc minus NOAA-CIRES AVHRR",/NOLINES
PSCLOSE

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_olr/hadgem3_monwg_jjas_olr.diff_xciel_noaa.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
LEVS,MANUAL=mylevs
CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
CON,FIELD=hadgem1_clim-noaa_clim,X=hadgem1_longitude,Y=hadgem1_latitude,$
  TITLE="Diff in JJAS-mean clim OLR (W m!U-2!N) for HadGEM1a_xciel minus NOAA-CIRES AVHRR",/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_olr/hadgem3_monwg_jjas_olr.diff_hadkpp_noaa.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
LEVS,MANUAL=mylevs
CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
CON,FIELD=hadkpp_clim-noaa_clim,X=hadkpp_longitude,Y=hadkpp_latitude,$
  TITLE="Diff in JJAS-mean clim OLR (W m!U-2!N) for HadKPP_1mtop minus NOAA-CIRES AVHRR",/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_olr/hadgem3_monwg_jjas_olr.diff_xbylr_noaa.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
LEVS,MANUAL=mylevs
CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
CON,FIELD=higem_clim-noaa_clim,X=higem_longitude,Y=higem_latitude,$
  TITLE="Diff in JJAS-mean clim OLR (W m!U-2!N) for HiGEM_xbylr_n144 minus NOAA-CIRES AVHRR",/NOLINES
PSCLOSE,/NOVIEW

STOP

END

