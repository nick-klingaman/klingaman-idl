PRO hadgem3_monwg_jjas_maxprecip_spatial

hadgem3a_mmean_file='/home/ss901165/um_output2/hadgem3_monwg/ahrqc/ahrqc.precip.apr-oct.mmeans_20years.nc'
hadgem3ao_mmean_file='/home/ss901165/um_output2/hadgem3_monwg/ahsaf/ahsaf.precip.mmeans_20years.nc'
cmap_mmean_file='/home/ss901165/datasets/CMAP_PRECIP/monthly/cmap.1979-2007.jjas_mmeans.nc'
imd_file='/home/ss901165/datasets/IMD_GRIDDED/imd_1x1v2_1951-2004.mjjas.nc'

hadgem3a_offset_jun=2
hadgem3a_offset_sep=5
hadgem3a_ntime=hadgem3a_offset_sep-hadgem3a_offset_jun+1
hadgem3a_nyears=20

hadgem3ao_offset_jun=5
hadgem3ao_offset_sep=8
hadgem3ao_ntime=hadgem3ao_offset_sep-hadgem3ao_offset_jun+1
hadgem3ao_nyears=20

cmap_offset_jun=0
cmap_offset_sep=3
cmap_ntime=cmap_offset_sep-cmap_offset_jun+1
cmap_nyears=20

imd_offset_jun=31
imd_offset_sep=152
imd_ntime=imd_offset_sep-imd_offset_jun+1
imd_offset_year=28
imd_nyears=20

box=[-30,40,30,180]
cmap_box=[-30,40,30,182]

; Get grid information
hadgem3a_longitude=OPEN_AND_EXTRACT(hadgem3a_mmean_file,'longitude')
hadgem3a_latitude=OPEN_AND_EXTRACT(hadgem3a_mmean_file,'latitude')
DEFINE_BOUNDARIES,box,hadgem3a_latitude,hadgem3a_longitude,hadgem3a_box_tx,/LIMIT
hadgem3a_nlon=N_ELEMENTS(hadgem3a_longitude)
hadgem3a_nlat=N_ELEMENTS(hadgem3a_latitude)

hadgem3ao_longitude=OPEN_AND_EXTRACT(hadgem3ao_mmean_file,'longitude')
hadgem3ao_latitude=OPEN_AND_EXTRACT(hadgem3ao_mmean_file,'latitude')
DEFINE_BOUNDARIES,box,hadgem3ao_latitude,hadgem3ao_longitude,hadgem3ao_box_tx,/LIMIT
hadgem3ao_nlon=N_ELEMENTS(hadgem3ao_longitude)
hadgem3ao_nlat=N_ELEMENTS(hadgem3ao_latitude)

cmap_longitude=OPEN_AND_EXTRACT(cmap_mmean_file,'lon')
cmap_latitude=OPEN_AND_EXTRACT(cmap_mmean_file,'lat')
DEFINE_BOUNDARIES,cmap_box,cmap_latitude,cmap_longitude,cmap_box_tx,/LIMIT
cmap_nlon=N_ELEMENTS(cmap_longitude)
cmap_nlat=N_ELEMENTS(cmap_latitude)

imd_longitude=OPEN_AND_EXTRACT(imd_file,'longitude')
imd_latitude=OPEN_AND_EXTRACT(imd_file,'latitude')
DEFINE_BOUNDARIES,box,imd_latitude,imd_longitude,imd_box_tx,/LIMIT
imd_nlon=N_ELEMENTS(imd_longitude)
imd_nlat=N_ELEMENTS(imd_latitude)

; Get monthly mean rainfall for all years
hadgem3a_precip=REFORM(OPEN_AND_EXTRACT(hadgem3a_mmean_file,'precip',$
                                        offset=[hadgem3a_box_tx(1),hadgem3a_box_tx(0),0,hadgem3a_offset_jun,0],$
                                        count=[hadgem3a_nlon,hadgem3a_nlat,1,hadgem3a_ntime,hadgem3a_nyears]))*86400.
hadgem3ao_precip=REFORM(OPEN_AND_EXTRACT(hadgem3ao_mmean_file,'precip',$
                                         offset=[hadgem3ao_box_tx(1),hadgem3ao_box_tx(0),0,hadgem3ao_offset_jun,0],$
                                         count=[hadgem3ao_nlon,hadgem3ao_nlat,1,hadgem3ao_ntime,hadgem3ao_nyears]))*86400.
cmap_precip=REFORM(OPEN_AND_EXTRACT(cmap_mmean_file,'precip',$
                                    offset=[cmap_box_tx(1),cmap_box_tx(0),cmap_offset_jun,0],$
                                    count=[cmap_nlon,cmap_nlat,cmap_ntime,cmap_nyears]))
imd_precip=REFORM(OPEN_AND_EXTRACT(imd_file,'rf',$
                                   offset=[imd_box_tx(1),imd_box_tx(0),0,imd_offset_jun,imd_offset_year],$
                                   count=[imd_nlon,imd_nlat,1,imd_ntime,imd_nyears]))

; Average over all months
hadgem3a_precip_timemean=fltarr(hadgem3a_nlon,hadgem3a_nlat,hadgem3a_nyears)
FOR i=0,hadgem3a_nlon-1 DO $
  FOR j=0,hadgem3a_nlat-1 DO $
  FOR k=0,hadgem3a_nyears-1 DO $
  hadgem3a_precip_timemean(i,j,k)=MEAN(hadgem3a_precip(i,j,*,k))
hadgem3ao_precip_timemean=fltarr(hadgem3ao_nlon,hadgem3ao_nlat,hadgem3ao_nyears)
FOR i=0,hadgem3ao_nlon-1 DO $
  FOR j=0,hadgem3ao_nlat-1 DO $
  FOR k=0,hadgem3ao_nyears-1 DO $
  hadgem3ao_precip_timemean(i,j,k)=MEAN(hadgem3ao_precip(i,j,*,k))
cmap_precip_timemean=fltarr(cmap_nlon,cmap_nlat,cmap_nyears)
FOR i=0,cmap_nlon-1 DO $
  FOR j=0,cmap_nlat-1 DO $
  FOR k=0,cmap_nyears-1 DO $
  cmap_precip_timemean(i,j,k)=MEAN(cmap_precip(i,j,*,k))
imd_precip_timemean=fltarr(imd_nlon,imd_nlat,imd_nyears)
FOR i=0,imd_nlon-1 DO $
  FOR j=0,imd_nlat-1 DO $
  FOR k=0,imd_nyears-1 DO $
  imd_precip_timemean(i,j,k)=MEAN(imd_precip(i,j,*,k))
imd_precip_timemean[where(imd_precip_timemean gt 1000)]=!Values.F_NaN

; Find year of maximum rainfall
hadgem3a_maxrain_year=fltarr(hadgem3a_nlon,hadgem3a_nlat)
hadgem3a_corr_rank=fltarr(hadgem3a_nlon,hadgem3a_nlat)
hadgem3ao_maxrain_year=fltarr(hadgem3ao_nlon,hadgem3ao_nlat)
hadgem3ao_corr_rank=fltarr(hadgem3ao_nlon,hadgem3ao_nlat)
cmap_maxrain_year=fltarr(cmap_nlon,cmap_nlat)
cmap_corr_rank=fltarr(cmap_nlon,cmap_nlat)
imd_maxrain_year=fltarr(imd_nlon,imd_nlat)
imd_corr_rank=fltarr(imd_nlon,imd_nlat)
FOR i=0,hadgem3a_nlon-1 DO BEGIN
    FOR j=0,hadgem3a_nlat-1 DO BEGIN
        temp=REFORM(hadgem3a_precip_timemean(i,j,*))
        sorted=SORT(temp)
        hadgem3a_corr_rank(i,j)=CORRELATE(indgen(hadgem3a_nyears),sorted)
        hadgem3a_maxrain_year(i,j)=where(temp eq MAX(temp))
    ENDFOR
ENDFOR
FOR i=0,hadgem3ao_nlon-1 DO BEGIN
    FOR j=0,hadgem3ao_nlat-1 DO BEGIN
        temp=REFORM(hadgem3ao_precip_timemean(i,j,*))
        sorted=SORT(temp)
        hadgem3ao_corr_rank(i,j)=CORRELATE(indgen(hadgem3ao_nyears),sorted)
        hadgem3ao_maxrain_year(i,j)=where(temp eq MAX(temp))
    ENDFOR
ENDFOR
FOR i=0,cmap_nlon-1 DO BEGIN
    FOR j=0,cmap_nlat-1 DO BEGIN
        temp=REFORM(cmap_precip_timemean(i,j,*))
        sorted=SORT(temp)
        cmap_corr_rank(i,j)=CORRELATE(indgen(cmap_nyears),sorted)
        cmap_maxrain_year(i,j)=where(temp eq MAX(temp))
    ENDFOR
ENDFOR
FOR i=0,imd_nlon-1 DO BEGIN
    FOR j=0,imd_nlat-1 DO BEGIN
        IF FINITE(imd_precip_timemean(i,j,0)) eq 1 THEN BEGIN
            temp=REFORM(imd_precip_timemean(i,j,*))
            sorted=SORT(temp)
            imd_corr_rank(i,j)=CORRELATE(indgen(imd_nyears),sorted)
            imd_maxrain_year(i,j)=where(temp eq MAX(temp))
        ENDIF ELSE BEGIN
            imd_corr_rank(i,j)=!Values.F_NaN
            imd_maxrain_year(i,j)=!Values.F_NaN
        ENDELSE
    ENDFOR
ENDFOR

mylevs=indgen(hadgem3a_nyears)+1
psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_maxprecip_spatial.ahrqc_20years.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=120
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=hadgem3a_maxrain_year,X=hadgem3a_longitude,Y=hadgem3a_latitude,$
  TITLE="Year of maximum JJAS-mean rain rate (mm day!U-1!N) from HadGEM3-A_ahrqc_n96",/NOLINES,/BLOCK
PSCLOSE,/NOVIEW

mylevs=['-0.50','-0.45','-0.40','-0.35','-0.30','-0.25','-0.20','-0.15','-0.10','-0.05','0.00',$
        '0.05','0.10','0.15','0.20','0.25','0.30','0.35','0.40','0.45','0.50']
psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_maxprecip_spatial_corrrank.ahrqc_20years.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=120
CS,SCALE=22,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=hadgem3a_corr_rank,X=hadgem3a_longitude,Y=hadgem3a_latitude,$
  TITLE="Rank correlation of JJAS-mean rain rate (mm day!U-1!N) from HadGEM3-A_ahrqc_n96",/NOLINES,/BLOCK,CB_WIDTH=112,$
  CB_NTH=2
PSCLOSE,/NOVIEW

mylevs=indgen(hadgem3ao_nyears)+1
psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_maxprecip_spatial.ahsaf_20years.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=120
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=hadgem3ao_maxrain_year,X=hadgem3ao_longitude,Y=hadgem3ao_latitude,$
  TITLE="Year of maximum JJAS-mean rain rate (mm day!U-1!N) from HadGEM3-AO_ahsaf_n96",/NOLINES,/BLOCK
PSCLOSE,/NOVIEW

mylevs=['-0.50','-0.45','-0.40','-0.35','-0.30','-0.25','-0.20','-0.15','-0.10','-0.05','0.00',$
        '0.05','0.10','0.15','0.20','0.25','0.30','0.35','0.40','0.45','0.50']
psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_maxprecip_spatial_corrrank.ahsaf_20years.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=120
CS,SCALE=22,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=hadgem3ao_corr_rank,X=hadgem3ao_longitude,Y=hadgem3ao_latitude,$
  TITLE="Rank correlation of JJAS-mean rain rate (mm day!U-1!N) from HadGEM3-AO_ahsaf_n96",/NOLINES,/BLOCK,CB_WIDTH=112,$
  CB_NTH=2
PSCLOSE,/NOVIEW

mylevs=indgen(cmap_nyears)+1
psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_maxprecip_spatial.cmap_1979-1998.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=120
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=cmap_maxrain_year,X=cmap_longitude,Y=cmap_latitude,$
  TITLE="Year of maximum JJAS-mean rain rate (mm day!U-1!N) from CMAP_2.5x2.5 (1979-1998)",/NOLINES,/BLOCK
PSCLOSE,/NOVIEW

mylevs=['-0.50','-0.45','-0.40','-0.35','-0.30','-0.25','-0.20','-0.15','-0.10','-0.05','0.00',$
        '0.05','0.10','0.15','0.20','0.25','0.30','0.35','0.40','0.45','0.50']
psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_maxprecip_spatial_corrrank.cmap_1979-1998.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=120
CS,SCALE=22,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
LEVS,MANUAL=mylevs
CON,FIELD=cmap_corr_rank,X=cmap_longitude,Y=cmap_latitude,$
  TITLE="Rank correlation of JJAS-mean rain rate (mm day!U-1!N) from CMAP_2.5x2.5 (1979-1998)",/NOLINES,/BLOCK,CB_WIDTH=112,$
  CB_NTH=2
PSCLOSE,/NOVIEW

mylevs=indgen(imd_nyears)+1
psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_maxprecip_spatial.imd_1979-1998.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=120
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=67,LONMAX=100,LATMIN=7,LATMAX=29
LEVS,MANUAL=mylevs
CON,FIELD=imd_maxrain_year,X=imd_longitude,Y=imd_latitude,$
  TITLE="Year of maximum JJAS-mean rain rate (mm day!U-1!N) from IMD_1x1 (1979-1998)",/NOLINES,/BLOCK
PSCLOSE

mylevs=['-0.50','-0.45','-0.40','-0.35','-0.30','-0.25','-0.20','-0.15','-0.10','-0.05','0.00',$
        '0.05','0.10','0.15','0.20','0.25','0.30','0.35','0.40','0.45','0.50']
psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_maxprecip_spatial_corrrank.imd_1979-1998.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=120
CS,SCALE=22,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=67,LONMAX=100,LATMIN=7,LATMAX=29
LEVS,MANUAL=mylevs
CON,FIELD=imd_corr_rank,X=imd_longitude,Y=imd_latitude,$
  TITLE="Rank correlation of JJAS-mean rain rate (mm day!U-1!N) from IMD_1x1 (1979-1998)",/NOLINES,/BLOCK,CB_WIDTH=112,$
  CB_NTH=2
PSCLOSE

mask_file='/home/ss901165/um_output/mask_n96.nc'
mask_longitude=OPEN_AND_EXTRACT(mask_file,'longitude')
mask_latitude=OPEN_AND_EXTRACT(mask_file,'latitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)
mask=REFORM(OPEN_AND_EXTRACT(mask_file,'lsm',offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                             count=[mask_nlon,mask_nlat,1,1]))
mask_rev=fltarr(mask_nlon,mask_nlat)
FOR i=0,mask_nlat-1 DO $
  mask_rev(*,i)=mask(*,mask_nlat-i-1)

hadgem3a_maxrain_year[where(mask_rev eq 0)]=!Values.F_NaN
hadgem3a_corr_rank[where(mask_rev eq 0)]=!Values.F_NaN

mylevs=indgen(hadgem3a_nyears)+1
psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_maxprecip_spatial.ahrqc_20years_zoomindia.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=120
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=67,LONMAX=100,LATMIN=7,LATMAX=29
LEVS,MANUAL=mylevs
CON,FIELD=hadgem3a_maxrain_year,X=hadgem3a_longitude,Y=hadgem3a_latitude,$
  TITLE="Year of maximum JJAS-mean rain rate (mm day!U-1!N) from HadGEM3-A_ahrqc_n96",/NOLINES,/BLOCK
PSCLOSE

mylevs=['-0.50','-0.45','-0.40','-0.35','-0.30','-0.25','-0.20','-0.15','-0.10','-0.05','0.00',$
        '0.05','0.10','0.15','0.20','0.25','0.30','0.35','0.40','0.45','0.50']
psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_maxprecip_spatial_corrrank.ahrqc_20years_zoomindia.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=120
CS,SCALE=22,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=67,LONMAX=100,LATMIN=7,LATMAX=29
LEVS,MANUAL=mylevs
CON,FIELD=hadgem3a_corr_rank,X=hadgem3a_longitude,Y=hadgem3a_latitude,$
  TITLE="Rank correlation of JJAS-mean rain rate (mm day!U-1!N) from HadGEM3-A_ahrqc_n96",/NOLINES,/BLOCK,CB_WIDTH=112,$
  CB_NTH=2
PSCLOSE

STOP
END
