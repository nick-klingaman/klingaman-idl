PRO hadgem3_monwg_jjas_sst

hadgem3_clim_file='/home/ss901165/um_output2/hadgem3_monwg/ahsaf/ahsaf.surftemp.clim.mmeans_20years.nc'
hadgem1_clim_file='/home/ss901165/um_output2/hadgem1_monwg/hadgem1.clim.jan-dec_mmeans.surftemp.monsoon_domain.nc'
tmi_clim_file='/home/ss901165/datasets/TMI_AMSRE/tmi.fusion.1998-2008.jjas_mmean_clim.n96.nc'
hadkpp_clim_file='/home/ss901165/um_output2/hadkpp_npiso_60lev/hadkpp_npiso.ensmean.mjjas_mmeans.sst.monsoon_domain.nc'
higem_clim_file='/home/ss901165/um_output2/higem_monwg/higem.clim.jan-dec_mmeans.surftemp.monsoon_domain.nc'
tmi_n144_clim_file='/home/ss901165/datasets/TMI_AMSRE/tmi.fusion.1998-2008.jjas_mmean_clim.n144.nc'

box=[-30,41,30,180]

mask_n96_file='/home/ss901165/um_output/mask_n96.nc'
mask_n96_longitude=OPEN_AND_EXTRACT(mask_n96_file,'longitude')
mask_n96_latitude=OPEN_AND_EXTRACT(mask_n96_file,'latitude')
DEFINE_BOUNDARIES,box,mask_n96_latitude,mask_n96_longitude,mask_n96_box_tx,/LIMIT
mask_n96_nlon=N_ELEMENTS(mask_n96_longitude)
mask_n96_nlat=N_ELEMENTS(mask_n96_latitude)
mask_n96=REFORM(OPEN_AND_EXTRACT(mask_n96_file,'lsm',offset=[mask_n96_box_tx(1),mask_n96_box_tx(0),0,0],$
                                 count=[mask_n96_nlon,mask_n96_nlat,1,1]))

mask_n144_file='/home/ss901165/um_output/mask_n144.nc'
mask_n144_longitude=OPEN_AND_EXTRACT(mask_n144_file,'longitude')
mask_n144_latitude=OPEN_AND_EXTRACT(mask_n144_file,'latitude')
DEFINE_BOUNDARIES,box,mask_n144_latitude,mask_n144_longitude,mask_n144_box_tx,/LIMIT
mask_n144_nlon=N_ELEMENTS(mask_n144_longitude)
mask_n144_nlat=N_ELEMENTS(mask_n144_latitude)
mask_n144=REFORM(OPEN_AND_EXTRACT(mask_n144_file,'lsm',offset=[mask_n144_box_tx(1),mask_n144_box_tx(0),0,0],$
                                 count=[mask_n144_nlon,mask_n144_nlat,1,1]))

; Get grid information
hadgem3_longitude=OPEN_AND_EXTRACT(hadgem3_clim_file,'longitude')
hadgem3_latitude=OPEN_AND_EXTRACT(hadgem3_clim_file,'latitude')
DEFINE_BOUNDARIES,box,hadgem3_latitude,hadgem3_longitude,hadgem3_box_tx,/LIMIT
hadgem3_nlon=N_ELEMENTS(hadgem3_longitude)
hadgem3_nlat=N_ELEMENTS(hadgem3_latitude)

hadgem1_longitude=OPEN_AND_EXTRACT(hadgem1_clim_file,'longitude')
hadgem1_latitude=OPEN_AND_EXTRACT(hadgem1_clim_file,'latitude')
DEFINE_BOUNDARIES,box,hadgem1_latitude,hadgem1_longitude,hadgem1_box_tx,/LIMIT
hadgem1_nlon=N_ELEMENTS(hadgem1_longitude)
hadgem1_nlat=N_ELEMENTS(hadgem1_latitude)

tmi_longitude=OPEN_AND_EXTRACT(tmi_clim_file,'longitude')
tmi_latitude=OPEN_AND_EXTRACT(tmi_clim_file,'latitude')
DEFINE_BOUNDARIES,box,tmi_latitude,tmi_longitude,tmi_box_tx,/LIMIT
tmi_nlon=N_ELEMENTS(tmi_longitude)
tmi_nlat=N_ELEMENTS(tmi_latitude)

tmi_n144_longitude=OPEN_AND_EXTRACT(tmi_n144_clim_file,'longitude')
tmi_n144_latitude=OPEN_AND_EXTRACT(tmi_n144_clim_file,'latitude')
DEFINE_BOUNDARIES,box,tmi_n144_latitude,tmi_n144_longitude,tmi_n144_box_tx,/LIMIT
tmi_n144_nlon=N_ELEMENTS(tmi_n144_longitude)
tmi_n144_nlat=N_ELEMENTS(tmi_n144_latitude)

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

hadgem3_offset_jun=5
hadgem1_offset_jun=5
hadkpp_offset_jun=1
higem_offset_jun=5
tmi_offset_jun=0
n_months=4

hadgem3_clim=fltarr(hadgem3_nlon,hadgem3_nlat)
hadgem1_clim=fltarr(hadgem1_nlon,hadgem1_nlat)
hadkpp_clim=fltarr(hadkpp_nlon,hadkpp_nlat)
higem_clim=fltarr(higem_nlon,higem_nlat)
tmi_clim=fltarr(tmi_nlon,tmi_nlat)
tmi_n144_clim=fltarr(tmi_n144_nlon,tmi_n144_nlat)
FOR i=0,n_months-1 DO BEGIN
    hadgem3_month=REFORM(OPEN_AND_EXTRACT(hadgem3_clim_file,'temp',$
                                          offset=[hadgem3_box_tx(1),hadgem3_box_tx(0),0,hadgem3_offset_jun+i],$
                                          count=[hadgem3_nlon,hadgem3_nlat,1,1]))-273.15
    hadgem1_month=REFORM(OPEN_AND_EXTRACT(hadgem1_clim_file,'temp',$
                                          offset=[hadgem1_box_tx(1),hadgem1_box_tx(0),0,hadgem1_offset_jun+i],$
                                          count=[hadgem1_nlon,hadgem1_nlat,1,1]))-273.15
    tmi_month=REFORM(OPEN_AND_EXTRACT(tmi_clim_file,'sst',$
                                      offset=[tmi_box_tx(1),tmi_box_tx(0),0,tmi_offset_jun+i],$
                                      count=[tmi_nlon,tmi_nlat,1,1]))
    tmi_n144_month=REFORM(OPEN_AND_EXTRACT(tmi_n144_clim_file,'sst',$
                                           offset=[tmi_n144_box_tx(1),tmi_n144_box_tx(0),0,tmi_offset_jun+i],$
                                           count=[tmi_n144_nlon,tmi_n144_nlat,1,1]))
    hadkpp_month=REFORM(OPEN_AND_EXTRACT(hadkpp_clim_file,'T',$
                                         offset=[hadkpp_box_tx(1),hadkpp_box_tx(0),0,hadkpp_offset_jun+i],$
                                         count=[hadkpp_nlon,hadkpp_nlat,1,1]))
    higem_month=REFORM(OPEN_AND_EXTRACT(higem_clim_file,'temp',$
                                        offset=[higem_box_tx(1),higem_box_tx(0),0,higem_offset_jun+i],$
                                        count=[higem_nlon,higem_nlat,1,1]))-273.15

    FOR j=0,hadgem3_nlon-1 DO BEGIN
        FOR k=0,hadgem3_nlat-1 DO BEGIN
            IF mask_n96(j,hadgem3_nlat-k-1) eq 0 THEN BEGIN
                hadgem3_clim(j,k)=hadgem3_clim(j,k)+hadgem3_month(j,k)/FLOAT(n_months)
            ENDIF ELSE $
              hadgem3_clim(j,k)=!Values.F_NaN     
        ENDFOR
    ENDFOR
    
    FOR j=0,hadgem1_nlon-1 DO BEGIN
        FOR k=0,hadgem1_nlat-1 DO BEGIN
            IF mask_n96(j,hadgem1_nlat-k-1) eq 0 THEN BEGIN
                hadgem1_clim(j,k)=hadgem1_clim(j,k)+hadgem1_month(j,k)/FLOAT(n_months)
            ENDIF ELSE $
              hadgem1_clim(j,k)=!Values.F_NaN     
        ENDFOR
    ENDFOR

    FOR j=0,tmi_nlon-1 DO BEGIN
        FOR k=0,tmi_nlat-1 DO BEGIN
            IF mask_n96(j,k) eq 0 and tmi_month(j,k) lt 35 THEN BEGIN
                tmi_clim(j,k)=tmi_clim(j,k)+tmi_month(j,k)/FLOAT(n_months)
            ENDIF ELSE $
              tmi_clim(j,k)=!Values.F_NaN     
        ENDFOR
    ENDFOR

    FOR j=0,tmi_n144_nlon-1 DO BEGIN
        FOR k=0,tmi_n144_nlat-1 DO BEGIN
            IF mask_n144(j,k) eq 0 and tmi_n144_month(j,k) lt 35 THEN BEGIN
                tmi_n144_clim(j,k)=tmi_n144_clim(j,k)+tmi_n144_month(j,k)/FLOAT(n_months)
            ENDIF ELSE $
              tmi_n144_clim(j,k)=!Values.F_NaN     
        ENDFOR
    ENDFOR

    FOR j=0,hadkpp_nlon-1 DO BEGIN
        FOR k=0,hadkpp_nlat-1 DO BEGIN
            IF mask_n144(j,k) eq 0 and hadkpp_month(j,k) lt 35 THEN BEGIN
                hadkpp_clim(j,k)=hadkpp_clim(j,k)+hadkpp_month(j,k)/FLOAT(n_months)
            ENDIF ELSE $
              hadkpp_clim(j,k)=!Values.F_NaN     
        ENDFOR
    ENDFOR

    FOR j=0,higem_nlon-1 DO BEGIN
        FOR k=0,higem_nlat-1 DO BEGIN
            IF mask_n144(j,higem_nlat-k-1) eq 0 and higem_month(j,k) lt 35 THEN BEGIN
                higem_clim(j,k)=higem_clim(j,k)+higem_month(j,k)/FLOAT(n_months)
            ENDIF ELSE $
              higem_clim(j,k)=!Values.F_NaN     
        ENDFOR
    ENDFOR

ENDFOR

mylevs=['22','22.5','23','23.5','24','24.5','25','25.5','26','26.5','27','27.5','28','28.5','29','29.5','30']
psfile='/home/ss901165/idl/hadgem3_monwg/jjas_sst/hadgem3_monwg_jjas_sst.hadgem3_ahsaf_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
CS,SCALE=22,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=42,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=hadgem3_clim,X=hadgem3_longitude,Y=hadgem3_latitude,$
  TITLE="JJAS-mean clim SST (C) from HadGEM3-AO_ahsaf_n96 (20 years)",/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_sst/hadgem3_monwg_jjas_sst.hadgem1_xciel_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
CS,SCALE=22,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=42,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=hadgem1_clim,X=hadgem1_longitude,Y=hadgem1_latitude,$
  TITLE="JJAS-mean clim SST (C) from HadGEM1a_xciel_n96 (30 years)",/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_sst/hadgem3_monwg_jjas_sst.tmi_n96_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
CS,SCALE=22,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=42,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=tmi_clim,X=tmi_longitude,Y=tmi_latitude,$
  TITLE="JJAS-mean clim SST (C) from TMI_n96 (1998-2008)",/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_sst/hadgem3_monwg_jjas_sst.tmi_n144_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
CS,SCALE=22,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=42,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=tmi_n144_clim,X=tmi_n144_longitude,Y=tmi_n144_latitude,$
  TITLE="JJAS-mean clim SST (C) from TMI_n144 (1998-2008)",/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_sst/hadgem3_monwg_jjas_sst.hadkpp_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
CS,SCALE=22,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=42,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=hadkpp_clim,X=tmi_n144_longitude,Y=tmi_n144_latitude,$
  TITLE="JJAS-mean clim SST (C) from HadKPP_n144_1mtop",/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_sst/hadgem3_monwg_jjas_sst.higem_xbylr_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
CS,SCALE=22,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=42,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=higem_clim,X=higem_longitude,Y=higem_latitude,$
  TITLE="JJAS-mean clim SST (C) from HiGEM_n144_xbylr (71 years)",/NOLINES
PSCLOSE

diff_hadgem3_tmi=fltarr(hadgem3_nlon,hadgem3_nlat)
diff_hadgem1_tmi=fltarr(hadgem1_nlon,hadgem1_nlat)
diff_higem_tmi=fltarr(higem_nlon,higem_nlat)
FOR i=0,hadgem3_nlon-1 DO BEGIN
    FOR j=0,hadgem3_nlat-1 DO BEGIN
        diff_hadgem3_tmi(i,j)=hadgem3_clim(i,hadgem3_nlat-j-1)-tmi_clim(i,j)
        diff_hadgem1_tmi(i,j)=hadgem1_clim(i,hadgem1_nlat-j-1)-tmi_clim(i,j)
    ENDFOR
ENDFOR
FOR i=0,higem_nlon-1 DO $
  FOR j=0,higem_nlat-1 DO $
        diff_higem_tmi(i,j)=higem_clim(i,higem_nlat-j-1)-tmi_n144_clim(i,j)

mylevs=['-3.8','-3.4','-3.0','-2.6','-2.2','-1.8','-1.4','-1.0','-0.6','-0.2',$
        '0.2','0.6','1.0','1.4','1.8','2.2','2.6','3.0','3.4','3.8']
psfile='/home/ss901165/idl/hadgem3_monwg/jjas_sst/hadgem3_monwg_jjas_sst.diff_ahsaf_tmi.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1
white=FSC_COLOR("white",N_ELEMENTS(mylevs)/2+2)
MAP,LONMIN=42,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=diff_hadgem3_tmi,X=tmi_longitude,Y=tmi_latitude,$
  TITLE="Diff in JJAS-mean clim SST (C) for HadGEM3-AO_ahsaf_n96 minus TMI_n96",/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_sst/hadgem3_monwg_jjas_sst.diff_xciel_tmi.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1
white=FSC_COLOR("white",N_ELEMENTS(mylevs)/2+2)
MAP,LONMIN=42,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=diff_hadgem1_tmi,X=tmi_longitude,Y=tmi_latitude,$
  TITLE="Diff in JJAS-mean clim SST (C) for HadGEM1a_xciel_n96 minus TMI_n96",/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_sst/hadgem3_monwg_jjas_sst.diff_hadkpp_tmi.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1
white=FSC_COLOR("white",N_ELEMENTS(mylevs)/2+2)
MAP,LONMIN=42,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=hadkpp_clim-tmi_n144_clim,X=tmi_n144_longitude,Y=tmi_n144_latitude,$
  TITLE="Diff in JJAS-mean clim SST (C) for HadKPP_n144_1mtop minus TMI_n144",/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_sst/hadgem3_monwg_jjas_sst.diff_xbylr_tmi.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1
white=FSC_COLOR("white",N_ELEMENTS(mylevs)/2+2)
MAP,LONMIN=42,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=diff_higem_tmi,X=tmi_n144_longitude,Y=tmi_n144_latitude,$
  TITLE="Diff in JJAS-mean clim SST (C) for HiGEM_n144_xbylr minus TMI_n144",/NOLINES
PSCLOSE

STOP

END


