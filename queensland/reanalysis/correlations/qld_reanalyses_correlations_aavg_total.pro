PRO qld_reanalyses_correlations_aavg_total

; Add TRMM 0.25x0.25!

eraint_smeans_file='/home/ss901165/datasets_mango/ERA-INTERIM/precipitation/ERA_interim.nov-apr_smeans.1989-2008.precip.aus_domain.nc'
eraint_mask_file='/home/ss901165/datasets_mango/ERA-INTERIM/eraint_mask.nc'
era40_smeans_file='/home/ss901165/datasets/ERA40/precipitation/era40.nov-apr_smeans.1978-2001.precip.aus_domain.nc'
era40_mask_file='/home/ss901165/datasets/ERA40/era40_lsm.nc'
silo_smeans_file='/home/ss901165/datasets_mango/SILO/eraint_resolution/SILO_precip.nov-apr_smeans.1978-2007.eraint.nc'
silo_era40_smeans_file='/home/ss901165/datasets_mango/SILO/era40_resolution/SILO_precip.nov-apr_smeans.1978-2007.era40.nc'
silo_n144_smeans_file='/home/ss901165/datasets_mango/SILO/n144/SILO_precip.nov-apr_smeans.1978-2007.n144.nc'
silo_n216_smeans_file='/home/ss901165/datasets_mango/SILO/n216/SILO_precip.nov-apr_smeans.1978-2007.n216.nc'
higam_smeans_file='/home/ss901165/higam_qccce/hpcx_amip2_xcquc/higam_xcquc.nov-apr_smeans.h8-k2.precip.aus_domain.nc'
nugam_smeans_file='/home/ss901165/nugam_qccce/es_amip2_eagud/nugam_eagud.nov-apr_smeans.h8-k3.precip.aus_domain.nc'
higam_mask_file='/home/ss901165/um_output/mask_n144_higam.nc'
nugam_mask_file='/home/ss901165/um_output/mask_n216_nugam.nc'

overall_start_year=1978
overall_stop_year=2007
overall_nyears=overall_stop_year-overall_start_year+1
ndays_per_season=181
box=[-30,140,-10,154]

eraint_start_year=1989
eraint_stop_year=2007
eraint_start_offset=0
eraint_nyears=(eraint_stop_year-eraint_start_year)+1
eraint_years=indgen(eraint_nyears)+(eraint_start_year-overall_start_year)

era40_start_year=1978
era40_stop_year=2001
era40_start_offset=0
era40_nyears=(era40_stop_year-era40_start_year)+1
era40_years=indgen(era40_nyears)+(era40_start_year-overall_start_year)

silo_start_year=1978
silo_stop_year=2007
silo_start_offset=0
silo_nyears=(silo_stop_year-silo_start_year)+1
silo_years=indgen(silo_nyears)+(silo_start_year-overall_start_year)

higam_start_year=1978
higam_stop_year=2001
higam_start_offset=0
higam_nyears=higam_stop_year-higam_start_year+1
higam_years=indgen(higam_nyears)+(higam_start_year-overall_start_year)

nugam_start_year=1978
nugam_stop_year=2002
nugam_start_offset=0
nugam_nyears=nugam_stop_year-nugam_start_year+1
nugam_years=indgen(nugam_nyears)+(nugam_start_year-overall_start_year)

eraint_longitude=OPEN_AND_EXTRACT(eraint_smeans_file,'longitude')
eraint_latitude=OPEN_AND_EXTRACT(eraint_smeans_file,'latitude')
DEFINE_BOUNDARIES,box,eraint_latitude,eraint_longitude,eraint_box_tx,/LIMIT
eraint_nlon=N_ELEMENTS(eraint_longitude)
eraint_nlat=N_ELEMENTS(eraint_latitude)

eraint_mask_longitude=OPEN_AND_EXTRACT(eraint_mask_file,'longitude')
eraint_mask_latitude=OPEN_AND_EXTRACT(eraint_mask_file,'latitude')
DEFINE_BOUNDARIES,box,eraint_mask_latitude,eraint_mask_longitude,eraint_mask_box_tx,/LIMIT
eraint_mask_nlon=N_ELEMENTS(eraint_mask_longitude)
eraint_mask_nlat=N_ELEMENTS(eraint_mask_latitude)

era40_longitude=OPEN_AND_EXTRACT(era40_smeans_file,'longitude')
era40_latitude=OPEN_AND_EXTRACT(era40_smeans_file,'latitude')
DEFINE_BOUNDARIES,box,era40_latitude,era40_longitude,era40_box_tx,/LIMIT
era40_nlon=N_ELEMENTS(era40_longitude)
era40_nlat=N_ELEMENTS(era40_latitude)

era40_mask_longitude=OPEN_AND_EXTRACT(era40_mask_file,'longitude')
era40_mask_latitude=OPEN_AND_EXTRACT(era40_mask_file,'latitude')
DEFINE_BOUNDARIES,box,era40_mask_latitude,era40_mask_longitude,era40_mask_box_tx,/LIMIT
era40_mask_nlon=N_ELEMENTS(era40_mask_longitude)
era40_mask_nlat=N_ELEMENTS(era40_mask_latitude)

silo_longitude=OPEN_AND_EXTRACT(silo_smeans_file,'longitude')
silo_latitude=OPEN_AND_EXTRACT(silo_smeans_file,'latitude')
DEFINE_BOUNDARIES,box,silo_latitude,silo_longitude,silo_box_tx,/LIMIT
silo_nlon=N_ELEMENTS(silo_longitude)
silo_nlat=N_ELEMENTS(silo_latitude)

silo_era40_longitude=OPEN_AND_EXTRACT(silo_era40_smeans_file,'longitude')
silo_era40_latitude=OPEN_AND_EXTRACT(silo_era40_smeans_file,'latitude')
DEFINE_BOUNDARIES,box,silo_era40_latitude,silo_era40_longitude,silo_era40_box_tx,/LIMIT
silo_era40_nlon=N_ELEMENTS(silo_era40_longitude)
silo_era40_nlat=N_ELEMENTS(silo_era40_latitude)

silo_n144_longitude=OPEN_AND_EXTRACT(silo_n144_smeans_file,'longitude')
silo_n144_latitude=OPEN_AND_EXTRACT(silo_n144_smeans_file,'latitude')
DEFINE_BOUNDARIES,box,silo_n144_latitude,silo_n144_longitude,silo_n144_box_tx,/LIMIT
silo_n144_nlon=N_ELEMENTS(silo_n144_longitude)
silo_n144_nlat=N_ELEMENTS(silo_n144_latitude)

silo_n216_longitude=OPEN_AND_EXTRACT(silo_n216_smeans_file,'longitude')
silo_n216_latitude=OPEN_AND_EXTRACT(silo_n216_smeans_file,'latitude')
DEFINE_BOUNDARIES,box,silo_n216_latitude,silo_n216_longitude,silo_n216_box_tx,/LIMIT
silo_n216_nlon=N_ELEMENTS(silo_n216_longitude)
silo_n216_nlat=N_ELEMENTS(silo_n216_latitude)

higam_longitude=OPEN_AND_EXTRACT(higam_smeans_file,'longitude_1')
higam_latitude=OPEN_AND_EXTRACT(higam_smeans_file,'latitude_1')
DEFINE_BOUNDARIES,box,higam_latitude,higam_longitude,higam_box_tx,/LIMIT
higam_nlon=N_ELEMENTS(higam_longitude)
higam_nlat=N_ELEMENTS(higam_latitude)

higam_mask_longitude=OPEN_AND_EXTRACT(higam_mask_file,'longitude')
higam_mask_latitude=OPEN_AND_EXTRACT(higam_mask_file,'latitude')
DEFINE_BOUNDARIES,box,higam_mask_latitude,higam_mask_longitude,higam_mask_box_tx,/LIMIT
higam_mask_nlon=N_ELEMENTS(higam_mask_longitude)
higam_mask_nlat=N_ELEMENTS(higam_mask_latitude)

nugam_longitude=OPEN_AND_EXTRACT(nugam_smeans_file,'longitude_1')
nugam_latitude=OPEN_AND_EXTRACT(nugam_smeans_file,'latitude_1')
DEFINE_BOUNDARIES,box,nugam_latitude,nugam_longitude,nugam_box_tx,/LIMIT
nugam_nlon=N_ELEMENTS(nugam_longitude)
nugam_nlat=N_ELEMENTS(nugam_latitude)

nugam_mask_longitude=OPEN_AND_EXTRACT(nugam_mask_file,'longitude')
nugam_mask_latitude=OPEN_AND_EXTRACT(nugam_mask_file,'latitude')
DEFINE_BOUNDARIES,box,nugam_mask_latitude,nugam_mask_longitude,nugam_mask_box_tx,/LIMIT
nugam_mask_nlon=N_ELEMENTS(nugam_mask_longitude)
nugam_mask_nlat=N_ELEMENTS(nugam_mask_latitude)

eraint_seasmeans=REFORM(OPEN_AND_EXTRACT(eraint_smeans_file,'TP',$
                                         offset=[eraint_box_tx(1),eraint_box_tx(0),0,eraint_start_offset],$
                                         count=[eraint_nlon,eraint_nlat,1,eraint_nyears]))*1000
eraint_mask=REFORM(OPEN_AND_EXTRACT(eraint_mask_file,'LSM',$
                                   offset=[eraint_mask_box_tx(1),eraint_mask_box_tx(0),0,0],$
                                   count=[eraint_mask_nlon,eraint_mask_nlat,1,1]))
era40_seasmeans=REFORM(OPEN_AND_EXTRACT(era40_smeans_file,'precip',$
                                         offset=[era40_box_tx(1),era40_box_tx(0),era40_start_offset],$
                                         count=[era40_nlon,era40_nlat,era40_nyears]))*1000
era40_mask=REFORM(OPEN_AND_EXTRACT(era40_mask_file,'LSM',$
                                   offset=[era40_mask_box_tx(1),era40_mask_box_tx(0),0,0],$
                                   count=[era40_mask_nlon,era40_mask_nlat,1,1]))
silo_seasmeans=REFORM(OPEN_AND_EXTRACT(silo_smeans_file,'rain',$
                                       offset=[silo_box_tx(1),silo_box_tx(0),silo_start_offset],$
                                       count=[silo_nlon,silo_nlat,silo_nyears]))
silo_era40_seasmeans=REFORM(OPEN_AND_EXTRACT(silo_era40_smeans_file,'rain',$
                                             offset=[silo_era40_box_tx(1),silo_era40_box_tx(0),silo_start_offset],$
                                             count=[silo_era40_nlon,silo_era40_nlat,silo_nyears]))
silo_n144_seasmeans=REFORM(OPEN_AND_EXTRACT(silo_n144_smeans_file,'rain',$
                                            offset=[silo_n144_box_tx(1),silo_n144_box_tx(0),silo_start_offset],$
                                            count=[silo_n144_nlon,silo_n144_nlat,silo_nyears]))
silo_n216_seasmeans=REFORM(OPEN_AND_EXTRACT(silo_n216_smeans_file,'rain',$
                                            offset=[silo_n216_box_tx(1),silo_n216_box_tx(0),silo_start_offset],$
                                            count=[silo_n216_nlon,silo_n216_nlat,silo_nyears]))
higam_seasmeans=REFORM(OPEN_AND_EXTRACT(higam_smeans_file,'precip',$
                                        offset=[higam_box_tx(1),higam_box_tx(0),0,0,higam_start_offset],$
                                        count=[higam_nlon,higam_nlat,1,1,higam_nyears]))*86400.
higam_mask=REFORM(OPEN_AND_EXTRACT(higam_mask_file,'lsm',$
                                   offset=[higam_mask_box_tx(1),higam_mask_box_tx(0),0,0],$
                                   count=[higam_mask_nlon,higam_mask_nlat,1,1]))
nugam_seasmeans=REFORM(OPEN_AND_EXTRACT(nugam_smeans_file,'precip',$
                                        offset=[nugam_box_tx(1),nugam_box_tx(0),0,0,nugam_start_offset],$
                                        count=[nugam_nlon,nugam_nlat,1,1,nugam_nyears]))*86400.
nugam_mask=REFORM(OPEN_AND_EXTRACT(nugam_mask_file,'lsm',$
                                   offset=[nugam_mask_box_tx(1),nugam_mask_box_tx(0),0,0],$
                                   count=[nugam_mask_nlon,nugam_mask_nlat,1,1]))

higam_mask_latrev=fltarr(higam_mask_nlon,higam_mask_nlat)
FOR i=0,higam_mask_nlat-1 DO $
   higam_mask_latrev(*,higam_mask_nlat-i-1)=higam_mask(*,i)
nugam_mask_latrev=fltarr(nugam_mask_nlon,nugam_mask_nlat)
FOR i=0,nugam_mask_nlat-1 DO $
   nugam_mask_latrev(*,nugam_mask_nlat-i-1)=nugam_mask(*,i)

;silo_n144_seasmeans_latrev=fltarr(silo_n144_nlon,silo_n144_nlat,silo_nyears)
;FOR i=0,silo_n144_nlat-1 DO $
;   silo_n144_seasmeans_latrev(*,i,*)=silo_n144_seasmeans(*,silo_n144_nlat-i-1,*)

;temp=REFORM(silo_seasmeans(*,*,0))
;eraint_mask=where(temp gt 1000)
;temp=REFORM(silo_n144_seasmeans_latrev(*,*,0))
;higam_mask_silo=where(temp gt 1000)
FOR i=0,eraint_nyears-1 DO BEGIN
   temp=eraint_seasmeans(*,*,i)
   temp[where(eraint_mask eq 0)]=!Values.F_NaN
   eraint_seasmeans(*,*,i)=temp
ENDFOR
FOR i=0,era40_nyears-1 DO BEGIN
   temp=era40_seasmeans(*,*,i)
   temp[where(era40_mask eq 0)]=!Values.F_NaN
   era40_seasmeans(*,*,i)=temp
ENDFOR
FOR i=0,higam_nyears-1 DO BEGIN
   temp=higam_seasmeans(*,*,i)
;   temp[higam_mask_silo]=!Values.F_NaN
   temp[where(higam_mask eq 0)]=!Values.F_NaN
   higam_seasmeans(*,*,i)=temp
ENDFOR
FOR i=0,nugam_nyears-1 DO BEGIN
   temp=nugam_seasmeans(*,*,i)
;   temp[nugam_mask_silo]=!Values.F_NaN
   temp[where(nugam_mask eq 0)]=!Values.F_NaN
   nugam_seasmeans(*,*,i)=temp
ENDFOR
FOR i=0,silo_nyears-1 DO BEGIN
   temp=silo_seasmeans(*,*,i)
   temp[where(eraint_mask eq 0)]=!Values.F_NaN
   silo_seasmeans(*,*,i)=temp

   temp=silo_era40_seasmeans(*,*,i)
   temp[where(era40_mask eq 0)]=!Values.F_NaN
   silo_era40_seasmeans(*,*,i)=temp

   temp=silo_n144_seasmeans(*,*,i)
   temp[where(higam_mask_latrev eq 0)]=!Values.F_NaN
   silo_n144_seasmeans(*,*,i)=temp

   temp=silo_n216_seasmeans(*,*,i)
   temp[where(nugam_mask eq 0)]=!Values.F_NaN
   silo_n216_seasmeans(*,*,i)=temp   
ENDFOR
silo_seasmeans[where(silo_seasmeans gt 1000)]=!Values.F_NaN
silo_era40_seasmeans[where(silo_era40_seasmeans gt 1000)]=!Values.F_NaN
silo_n144_seasmeans[where(silo_n144_seasmeans gt 1000)]=!Values.F_NaN
silo_n216_seasmeans[where(silo_n216_seasmeans gt 1000)]=!Values.F_NaN

era40_seasmeans=era40_seasmeans*ndays_per_season
eraint_seasmeans=eraint_seasmeans*ndays_per_season
silo_seasmeans=silo_seasmeans*ndays_per_season
silo_era40_seasmeans=silo_era40_seasmeans*ndays_per_season
silo_n144_seasmeans=silo_n144_seasmeans*ndays_per_season
silo_n216_seasmeans=silo_n216_seasmeans*ndays_per_season
higam_seasmeans=higam_seasmeans*180.
nugam_seasmeans=nugam_seasmeans*180.

silo_seasmeans_aavg=fltarr(silo_nyears)
silo_era40_seasmeans_aavg=fltarr(silo_nyears)
silo_n144_seasmeans_aavg=fltarr(silo_nyears)
silo_n216_seasmeans_aavg=fltarr(silo_nyears)
eraint_seasmeans_aavg=fltarr(eraint_nyears)
era40_seasmeans_aavg=fltarr(era40_nyears)
higam_seasmeans_aavg=fltarr(higam_nyears)
nugam_seasmeans_aavg=fltarr(nugam_nyears)
FOR i=0,silo_nyears-1 DO BEGIN
   silo_seasmeans_aavg(i)=MEAN(silo_seasmeans(*,*,i),/NaN)
   silo_era40_seasmeans_aavg(i)=MEAN(silo_era40_seasmeans(*,*,i),/NaN)
   silo_n144_seasmeans_aavg(i)=MEAN(silo_n144_seasmeans(*,*,i),/NaN)
   silo_n216_seasmeans_aavg(i)=MEAN(silo_n216_seasmeans(*,*,i),/NaN)
ENDFOR
FOR i=0,era40_nyears-1 DO $
   era40_seasmeans_aavg(i)=MEAN(era40_seasmeans(*,*,i),/NaN)
FOR i=0,eraint_nyears-1 DO $
   eraint_seasmeans_aavg(i)=MEAN(eraint_seasmeans(*,*,i),/NaN)
FOR i=0,higam_nyears-1 DO $
   higam_seasmeans_aavg(i)=MEAN(higam_seasmeans(*,*,i),/NaN)
FOR i=0,nugam_nyears-1 DO $
   nugam_seasmeans_aavg(i)=MEAN(nugam_seasmeans(*,*,i),/NaN)

psfile='/home/ss901165/idl/queensland/reanalysis/correlations/qld_reanalyses_correlations_aavg_total.nov-apr_aavg_total.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=1500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
;GSET,XMIN=overall_start_year-1,XMAX=overall_stop_year+1,YMIN=0,YMAX=900
GSET,XMIN=overall_start_year,XMAX=overall_stop_year,YMIN=0,YMAX=200
red=FSC_COLOR("red",10)
black=FSC_COLOR("black",11)
blue=FSC_COLOR("blue",12)
cyan=FSC_COLOR("cyan",13)
pink=FSC_COLOR("pink",14)

GPLOT,X=era40_years+overall_start_year,Y=era40_seasmeans_aavg/silo_era40_seasmeans_aavg[era40_years]*100,COL=14
GPLOT,X=eraint_years+overall_start_year,Y=eraint_seasmeans_aavg/silo_seasmeans_aavg[eraint_years]*100,COL=10
GPLOT,X=silo_years+overall_start_year,Y=silo_seasmeans_aavg/silo_seasmeans_aavg*100,COL=11
GPLOT,X=higam_years+overall_start_year,Y=higam_seasmeans_aavg/silo_n144_seasmeans_aavg[higam_years]*100,COL=12
GPLOT,X=nugam_years+overall_start_year,Y=nugam_seasmeans_aavg/silo_n216_seasmeans_aavg[nugam_years]*100,COL=13
AXES,XSTEP=2,YSTEP=10,XTITLE="Year at beginning of November-April season",YTITLE="November-April rain over Queensland land points as %age of SILO at same res"
GPLOT,TEXT='Correlations:',Y=190,X=1992,ALIGN=0.0
GPLOT,TEXT='SILO and ERA-40: '+STRMID(STRTRIM(STRING(CORRELATE(silo_era40_seasmeans_aavg[era40_years],era40_seasmeans_aavg)),1),0,5),Y=180,X=1992,ALIGN=0.0
GPLOT,TEXT='SILO and ERA-Interim: '+STRMID(STRTRIM(STRING(CORRELATE(silo_seasmeans_aavg[eraint_years],eraint_seasmeans_aavg)),1),0,5),Y=170,X=1992,ALIGN=0.0
GPLOT,TEXT='SILO and HiGAM: '+STRMID(STRTRIM(STRING(CORRELATE(silo_n144_seasmeans_aavg[higam_years],higam_seasmeans_aavg)),1),0,5),Y=160,X=1992,ALIGN=0.0
GPLOT,TEXT='SILO and NuGAM: '+STRMID(STRTRIM(STRING(CORRELATE(silo_n216_seasmeans_aavg[nugam_years],nugam_seasmeans_aavg)),1),0,5),Y=150,X=1992,ALIGN=0.0

;HIST,X=era40_years-0.4+overall_start_year,Y=era40_seasmeans_aavg,FILLCOL=14,WIDTH=40
;HIST,X=eraint_years-0.2+overall_start_year,Y=eraint_seasmeans_aavg,FILLCOL=10,WIDTH=40
;HIST,X=silo_years+overall_start_year,Y=silo_seasmeans_aavg,FILLCOL=11,WIDTH=40
;HIST,X=higam_years+0.2+overall_start_year,Y=higam_seasmeans_aavg,FILLCOL=12,WIDTH=40
;HIST,X=nugam_years+0.4+overall_start_year,Y=nugam_seasmeans_aavg,FILLCOL=13,WIDTH=40
;GPLOT,X=indgen(silo_nyears-11)+1905,Y=silo_aavg_precip_ts_smooth(5:silo_nyears-6),COL=11,THICK=225
;AXES,XSTEP=2,YSTEP=100,XTITLE="Year at beginning of November-April season",YTITLE="November-April total rainfall over Queensland land points"
items=REVERSE(['SILO','ERA-40','ERA-Interim','HiGAM','NuGAM'])

;GPLOT,TEXT='Correlations:',Y=850,X=1992,ALIGN=0.0
;GPLOT,TEXT='SILO and ERA-40: '+STRMID(STRTRIM(STRING(CORRELATE(silo_era40_seasmeans_aavg[era40_years],era40_seasmeans_aavg)),1),0,5),Y=820,X=1992,ALIGN=0.0
;GPLOT,TEXT='SILO and ERA-Interim: '+STRMID(STRTRIM(STRING(CORRELATE(silo_seasmeans_aavg[eraint_years],eraint_seasmeans_aavg)),1),0,5),Y=790,X=1992,ALIGN=0.0
;GPLOT,TEXT='SILO and HiGAM: '+STRMID(STRTRIM(STRING(CORRELATE(silo_n144_seasmeans_aavg[higam_years],higam_seasmeans_aavg)),1),0,5),Y=760,X=1992,ALIGN=0.0
;GPLOT,TEXT='SILO and NuGAM: '+STRMID(STRTRIM(STRING(CORRELATE(silo_n216_seasmeans_aavg[nugam_years],nugam_seasmeans_aavg)),1),0,5),Y=730,X=1992,ALIGN=0.0

LEGEND,labels=items,COL=REVERSE([11,14,10,12,13]),LEGPOS=1
PSCLOSE

silo_eraint_spatial_corr=fltarr(silo_nlon,silo_nlat)
FOR i=0,silo_nlon-1 DO $
   FOR j=0,silo_nlat-1 DO $
      silo_eraint_spatial_corr(i,j)=CORRELATE(silo_seasmeans(i,j,[eraint_years]),eraint_seasmeans(i,j,*))

mylevs=['0.3','0.35','0.4','0.45','0.5','0.55','0.6','0.65','0.7','0.75','0.8','0.85','0.9','0.95']
psfile='/home/ss901165/idl/queensland/reanalysis/correlations/qld_reanalyses_correlations_aavg_total.nov-apr_total_silo-corr-eraint.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=1500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV;,white=[2]
MAP,LONMIN=140,LONMAX=153,LATMIN=-30,LATMAX=-10,/HIRES
LEVS,MANUAL=mylevs
CON,FIELD=silo_eraint_spatial_corr,X=silo_longitude,Y=silo_latitude,$
    TITLE="Corr in Nov-Apr total rain between SILO and ERA-Interim (1989-2007) - at ERA-Int res",$
    /BLOCK,/CB_RIGHT,/NOLINES
PSCLOSE,/NOVIEW

silo_higam_spatial_corr=fltarr(silo_n144_nlon,silo_n144_nlat)
FOR i=0,silo_n144_nlon-1 DO $
   FOR j=0,silo_n144_nlat-1 DO $
      silo_higam_spatial_corr(i,j)=CORRELATE(silo_n144_seasmeans(i,j,[higam_years]),higam_seasmeans(i,silo_n144_nlat-j-1,*))

mylevs=['-0.30','-0.25','-0.20','-0.15','-0.10','-0.05','0.0','0.05','0.10','0.15','0.20','0.25','0.30']
psfile='/home/ss901165/idl/queensland/reanalysis/correlations/qld_reanalyses_correlations_aavg_total.nov-apr_total_silo-corr-higam.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=1500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV;,white=[2]
MAP,LONMIN=140,LONMAX=153,LATMIN=-30,LATMAX=-10,/HIRES
LEVS,MANUAL=mylevs
CON,FIELD=silo_higam_spatial_corr,X=silo_n144_longitude,Y=silo_n144_latitude,$
    TITLE="Corr in Nov-Apr total rain between SILO and HiGAM (1979-2002) - at HiGAM N144 res",$
    /BLOCK,/CB_RIGHT,/NOLINES
PSCLOSE

silo_nugam_spatial_corr=fltarr(silo_n216_nlon,silo_n216_nlat)
FOR i=0,silo_n216_nlon-1 DO $
   FOR j=0,silo_n216_nlat-1 DO $
      silo_nugam_spatial_corr(i,j)=CORRELATE(silo_n216_seasmeans(i,j,[nugam_years]),nugam_seasmeans(i,j,*))

mylevs=['-0.30','-0.25','-0.20','-0.15','-0.10','-0.05','0.0','0.05','0.10','0.15','0.20','0.25','0.30']
psfile='/home/ss901165/idl/queensland/reanalysis/correlations/qld_reanalyses_correlations_aavg_total.nov-apr_total_silo-corr-nugam.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=1500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV;,white=[2]
MAP,LONMIN=140,LONMAX=153,LATMIN=-30,LATMAX=-10,/HIRES
LEVS,MANUAL=mylevs
CON,FIELD=silo_nugam_spatial_corr,X=silo_n216_longitude,Y=silo_n216_latitude,$
    TITLE="Corr in Nov-Apr total rain between SILO and NuGAM (1979-2003) - at NuGAM N216 res",$
    /BLOCK,/CB_RIGHT,/NOLINES
PSCLOSE

STOP
END
