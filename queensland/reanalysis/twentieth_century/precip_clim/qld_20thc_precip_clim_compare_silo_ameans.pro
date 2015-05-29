PRO qld_20thc_precip_clim_compare_silo_ameans

; Compare the precipitation over Queensland from the 20th century
; reanalysis to that from SILO.

twentyc_ameans_infile='/home/ss901165/datasets_mango/20THC_REANALYSIS/precip/20thc_reanalysis.nov-apr_smeans.1891-2007.precip.nc'
twentyc_ameans_sprd_infile='/home/ss901165/datasets_mango/20THC_REANALYSIS/precip_sprd/20thc_reanalysis.nov-apr_smeans.1891-2007.precip_sprd.nc'
silo_ameans_infile='/home/ss901165/datasets_mango/SILO/t62/SILO_precip.nov-apr_smeans.1891-2007.t62.nc'

qld_box=[-10,138,-30,154]
nyear=117 ; for 1891-2007

twentyc_latitude=OPEN_AND_EXTRACT(twentyc_ameans_infile,'latitude')
twentyc_longitude=OPEN_AND_EXTRACT(twentyc_ameans_infile,'longitude')
DEFINE_BOUNDARIES,qld_box,twentyc_latitude,twentyc_longitude,twentyc_box_tx,/LIMIT
twentyc_nlon=N_ELEMENTS(twentyc_longitude)
twentyc_nlat=N_ELEMENTS(twentyc_latitude)

silo_latitude=OPEN_AND_EXTRACT(silo_ameans_infile,'latitude')
silo_longitude=OPEN_AND_EXTRACT(silO_ameans_infile,'longitude')
DEFINE_BOUNDARIES,qld_box,silo_latitude,silo_longitude,silo_box_tx,/LIMIT
silo_nlon=N_ELEMENTS(silo_longitude)
silo_nlat=N_ELEMENTS(silo_latitude)

twentyc_ameans=REFORM(OPEN_AND_EXTRACT(twentyc_ameans_infile,'PRATE',$
                                       offset=[twentyc_box_tx(1),twentyc_box_tx(0),0,0],$
                                       count=[twentyc_nlon,twentyc_nlat,1,nyear]))*86400.
twentyc_ameans_sprd=REFORM(OPEN_AND_EXTRACT(twentyc_ameans_sprd_infile,'PRATE',$
                                            offset=[twentyc_box_tx(1),twentyc_box_tx(0),0,0],$
                                            count=[twentyc_nlon,twentyc_nlat,1,nyear]))*86400.
silo_ameans=REFORM(OPEN_AND_EXTRACT(silo_ameans_infile,'rain',$
                                    offset=[silo_box_tx(1),silo_box_tx(0),0],$
                                    count=[silo_nlon,silo_nlat,nyear]))
mask=intarr(silo_nlon,silo_nlat)
mask(*,*)=0
t63_weights=fltarr(silo_nlon,silo_nlat)
FOR i=0,silo_nlon-1 DO BEGIN
   FOR j=0,silo_nlat-1 DO BEGIN
      IF silo_ameans(i,j) lt 1E20 THEN BEGIN
         mask(i,j)=1
      ENDIF
   ENDFOR
ENDFOR
nvalid_pts=N_ELEMENTS(where(mask eq 1))
FOR i=0,silo_nlon-1 DO BEGIN
   FOR j=0,silo_nlat-1 DO BEGIN
      IF mask(i,j) eq 1 THEN BEGIN
         t63_weights(i,j)=COS(3.14159*silo_latitude(j)/180.)
      ENDIF ELSE $
         t63_weights(i,j)=0.
   ENDFOR
ENDFOR
t63_weights=t63_weights/TOTAL(t63_weights)

twentyc_ameans_qld_aavg=fltarr(nyear)
twentyc_ameans_sprd_qld_aavg=fltarr(nyear)
silo_ameans_qld_aavg=fltarr(nyear)
FOR i=0,nyear-1 DO BEGIN
   silo_ameans_qld_aavg(i)=TOTAL(REFORM(silo_ameans(*,*,i))*t63_weights)
   twentyc_ameans_qld_aavg(i)=TOTAL(REFORM(twentyc_ameans(*,*,i))*t63_weights)
   twentyc_ameans_sprd_qld_aavg(i)=TOTAL(REFORM(twentyc_ameans_sprd(*,*,i))*t63_weights)
ENDFOR
twentyc_ameans_clim=fltarr(twentyc_nlon,twentyc_nlat)
twentyc_ameans_sprd_clim=fltarr(twentyc_nlon,twentyc_nlat)
silo_ameans_clim=fltarr(silo_nlon,silo_nlat)
FOR i=0,silo_nlon-1 DO $
   FOR j=0,silo_nlat-1 DO $
      silo_ameans_clim(i,j)=MEAN(silo_ameans(i,j,*))
FOR i=0,twentyc_nlon-1 DO BEGIN
   FOR j=0,twentyc_nlat-1 DO BEGIN
      IF mask(i,j) eq 1 THEN BEGIN
         twentyc_ameans_clim(i,j)=MEAN(twentyc_ameans(i,j,*))
         twentyc_ameans_sprd_clim(i,j)=MEAN(twentyc_ameans_sprd(i,j,*))
      ENDIF ELSE BEGIN
         twentyc_ameans_clim(i,j)=!Values.F_NaN
         twentyc_ameans_sprd_clim(i,j)=!Values.F_NaN
      ENDELSE
   ENDFOR
ENDFOR

mylevs=[100,200,300,400,500,600,700,800,900,1000,1200,1400,1600,1800]
mylevs_sprd=[50,100,150,200,250,300,350,400,450,500,600,700,800,900]
mylevs_diff=[-550,-450,-350,-250,-150,-50,50,150,250,350,450,550]
mylevs_ratio=['0.45','0.55','0.65','0.75','0.85','0.95','1.05','1.15','1.25','1.35','1.45','1.55']

psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/precip_clim/qld_20thc_precip_clim_compare_silo_ameans.silo_t62_may-apr_1891-2007_clim.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=300,XOFFSET=1200,YOFFSET=1000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
LEVS,MANUAL=mylevs
MAP,LATMIN=qld_box(2),LATMAX=qld_box(0),LONMIN=qld_box(1)+1,LONMAX=qld_box(3)-0.25,/HIRES
CON,X=silo_longitude,Y=silo_latitude,FIELD=silo_ameans_clim*365,/NOLINELABELS,$
    TITLE='Clim ann-total (May-Apr) precip from SILO on T62 grid (as 20th cent.) - 1891-2008',/BLOCK,/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/precip_clim/qld_20thc_precip_clim_compare_silo_ameans.20thc_t62_may-apr_1891-2007_clim.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=300,XOFFSET=1200,YOFFSET=1000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
LEVS,MANUAL=mylevs
MAP,LATMIN=qld_box(2),LATMAX=qld_box(0),LONMIN=qld_box(1)+1,LONMAX=qld_box(3)-0.25,/HIRES
CON,X=silo_longitude,Y=silo_latitude,FIELD=twentyc_ameans_clim*365,/NOLINELABELS,$
    TITLE='Clim ann-total (May-Apr) precip from 20th Cent V2 - 1891-2008',/BLOCK,/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/precip_clim/qld_20thc_precip_clim_compare_silo_ameans.20thc_sprd_t62_may-apr_1891-2007_clim.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=300,XOFFSET=1200,YOFFSET=1000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
LEVS,MANUAL=mylevs
MAP,LATMIN=qld_box(2),LATMAX=qld_box(0),LONMIN=qld_box(1)+1,LONMAX=qld_box(3)-0.25,/HIRES
CON,X=silo_longitude,Y=silo_latitude,FIELD=twentyc_ameans_sprd_clim*365,/NOLINELABELS,$
    TITLE='Clim ann-total (May-Apr) ensemble spread in precip from 20th Cent V2 - 1891-2008',/BLOCK,/NOLINES
PSCLOSE
       
psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/precip_clim/qld_20thc_precip_clim_compare_silo_ameans.20thc-minus-silo_t62_may-apr_1891-2007_clim.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=300,XOFFSET=1200,YOFFSET=1000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV,white=[8]
LEVS,MANUAL=mylevs_diff
MAP,LATMIN=qld_box(2),LATMAX=qld_box(0),LONMIN=qld_box(1)+1,LONMAX=qld_box(3)-0.25,/HIRES
CON,X=silo_longitude,Y=silo_latitude,FIELD=twentyc_ameans_clim*365-silo_ameans_clim*365,/NOLINELABELS,$
    TITLE='Diff in clim ann-total (May-Apr) precip for 20thC V2 minus SILO - 1891-2008',/BLOCK,/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/precip_clim/qld_20thc_precip_clim_compare_silo_ameans.20thc-ratio-silo_t62_may-apr_1891-2007_clim.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=300,XOFFSET=1200,YOFFSET=1000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio)+1,/REV,white=[8]
LEVS,MANUAL=mylevs_ratio
MAP,LATMIN=qld_box(2),LATMAX=qld_box(0),LONMIN=qld_box(1)+1,LONMAX=qld_box(3)-0.25,/HIRES
CON,X=silo_longitude,Y=silo_latitude,FIELD=twentyc_ameans_clim*365/(silo_ameans_clim*365),/NOLINELABELS,$
    TITLE='Ratio of clim ann-total (May-Apr) precip for 20thC V2 divided by SILO - 1891-2008',/BLOCK,/NOLINES
PSCLOSE,/NOVIEW
 
psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/precip_clim/qld_20thc_precip_clim_compare_silo_ameans.20thc_silo_t62_may-apr_1891-2007_ts.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2500,SPACE2=300,XOFFSET=400,YOFFSET=1000,TFONT=2,TCHARSIZE=80
GSET,XMIN=1890,XMAX=2008,YMIN=200,YMAX=1500,TITLE='Comparison of 20thC V2 (ensemble-mean) and SILO (on T62 grid) area-avg, annual total (May-April) rainfall for Queensland - 1891-2007'
black=FSC_COLOR("black",2)
red=FSC_COLOR("red",3)
HIST,X=indgen(nyear)+1891,Y=silo_ameans_qld_aavg*365,FILLCOL=2,WIDTH=45
GPLOT,X=indgen(nyear)+1891,Y=twentyc_ameans_qld_aavg*365,COL=3,THICK=200
AXES,XSTEP=10,YSTEP=100,XTITLE='Year at beginning of May-April annual-mean',YTITLE='Queensland area-averaged, annual-total (May-April) rainfall (mm/year)',/NORIGHT
GSET,XMIN=1890,XMAX=2008,YMIN=-0.2,YMAX=1.3
GPLOT,X=indgen(nyear)+1891,Y=twentyc_ameans_qld_aavg*365/(silo_ameans_qld_aavg*365),STYLE=2,THICK=150,COL=3
AXES,XSTEP=10,YSTEP=0.1,YTITLE='20thC V2 (ens-mean) QLD area-avg, ann-mean rain as fraction of SILO',/ONLYRIGHT,NDECS=2
GPLOT,X=indgen(nyear)+1891,Y=REPLICATE(1.0,nyear),STYLE=1,COL=2,THICK=125
items=['Ratio of 20thC V2 ens-mean to SILO','20thC ens-mean rainfall','SILO rainfall (on T62 grid)']
LEGEND,labels=items,LEGPOS=9,COL=[3,3,2],STYLE=[2,0,0],SYM=[1,1,1],SIZE=[0,0,100],LENGTH=[100,100,0]
PSCLOSE

psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/precip_clim/qld_20thc_precip_clim_compare_silo_ameans.20thc_sprd_silo_t62_may-apr_1891-2007_ts.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2500,SPACE2=300,XOFFSET=400,YOFFSET=1000,TFONT=2,TCHARSIZE=100
GSET,XMIN=1890,XMAX=2008,YMIN=200,YMAX=1500,TITLE='Comparison of 20thC V2 and SILO (on T62 grid) area-averaged, annual-mean (May-April) rainfall for Queensland - 1891-2007'
black=FSC_COLOR("black",2)
red=FSC_COLOR("red",3)
HIST,X=indgen(nyear)+1891,Y=silo_ameans_qld_aavg*365,FILLCOL=2,WIDTH=45
GPLOT,X=indgen(nyear)+1891,Y=twentyc_ameans_qld_aavg*365,COL=3,THICK=200
GPLOT,X=indgen(nyear)+1891,Y=twentyc_ameans_sprd_qld_aavg*365,COL=3,STYLE=2,THICK=200
AXES,XSTEP=10,YSTEP=100,XTITLE='Year at beginning of May-April annual-mean',YTITLE='Queensland area-averaged, annual-total (May-April) rainfall (mm/year)',/NORIGHT
GSET,XMIN=1890,XMAX=2008,YMIN=-1.0,YMAX=2.75
GPLOT,X=indgen(nyear)+1891,Y=twentyc_ameans_sprd_qld_aavg/twentyc_ameans_qld_aavg,STYLE=2,THICK=200
AXES,XSTEP=10,YSTEP=0.25,YTITLE='20thC V2 QLD ensemble spread as fraction of 20thC V2 QLD ensemble-mean',/ONLYRIGHT,NDECS=2
items=['Ratio of 20thC V2 ens-sprd to ens-mean','20thc ens-sprd rainfall','20thC ens-mean rainfall','SILO rainfall (on T62 grid)']
LEGEND,labels=items,LEGPOS=9,COL=[3,2,3,2],STYLE=[2,2,0,0],SYM=[1,1,1,1],SIZE=[0,0,0,100],LENGTH=[100,100,100,0]
PSCLOSE

hadisst_nino4_infile='/home/ss901165/datasets/NINO/nino4_hadisst.nov-apr_smeans.1871-2008.nc'
hadisst_offset=20
hadisst_nino4=REFORM(OPEN_AND_EXTRACT(hadisst_nino4_infile,'NINO4',offset=[hadisst_offset],count=[nyear]))
hadisst_nino4=hadisst_nino4-MEAN(hadisst_nino4)

elnino_years=where(hadisst_nino4 gt 0.2)
lanina_years=where(hadisst_nino4 lt -0.2)
neutral_years=where(hadisst_nino4 le 0.2 and hadisst_nino4 ge -0.2)
elnino_regression=REGRESS(silo_ameans_qld_aavg[elnino_years]*365,twentyc_ameans_qld_aavg[elnino_years]*365,CONST=elnino_constant,CORRELATION=elnino_correlation)
lanina_regression=REGRESS(silo_ameans_qld_aavg[lanina_years]*365,twentyc_ameans_qld_aavg[lanina_years]*365,CONST=lanina_constant,CORRELATION=lanina_correlation)
neutral_regression=REGRESS(silo_ameans_qld_aavg[neutral_years]*365,twentyc_ameans_qld_aavg[neutral_years]*365,CONST=neutral_constant,CORRELATION=neutral_correlation)
total_regression=REGRESS(silo_ameans_qld_aavg*365.,twentyc_ameans_qld_aavg*365.,CONST=total_constant,CORRELATION=total_correlation)

psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/precip_clim/qld_20thc_precip_clim_compare_silo_ameans.20thc_silo_t62_may-apr_1891-2007_scatter.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2500,SPACE2=300,XOFFSET=400,YOFFSET=1000,TFONT=2,TCHARSIZE=100
GSET,XMIN=200,XMAX=1300,YMIN=200,YMAX=1300,TITLE='Comparison of 20thC V2 and SILO (on T62 grid) area-averaged, annual-mean (May-April) rainfall for Queensland - 1891-2007'
red=FSC_COLOR("red",3)
blue=FSC_COLOR("blue",4)
black=FSC_COLOR("black",2)
GPLOT,X=silo_ameans_qld_aavg*365,Y=twentyc_ameans_qld_aavg*365,COL=2,THICK=200,SYM=6,/NOLINES
GPLOT,X=silo_ameans_qld_aavg[lanina_years]*365,Y=twentyc_ameans_qld_aavg[lanina_years]*365,COL=4,THICK=200,SYM=6,/NOLINES
GPLOT,X=silo_ameans_qld_aavg[elnino_years]*365,Y=twentyc_ameans_qld_aavg[elnino_years]*365,COL=3,THICK=200,SYM=6,/NOLINES
GPLOT,X=findgen(90)*10+300,Y=(findgen(90)*10+300)*elnino_regression(0)+elnino_constant,THICK=150,COL=3
GPLOT,X=findgen(90)*10+300,Y=(findgen(90)*10+300)*lanina_regression(0)+lanina_constant,THICK=150,COL=4
GPLOT,X=findgen(90)*10+300,Y=(findgen(90)*10+300)*total_regression(0)+total_constant,THICK=150,COL=2
GPLOT,X=findgen(90)*10+300,Y=(findgen(90)*10+300)*neutral_regression(0)+neutral_constant,THICK=150,COL=2,STYLE=2

GPLOT,X=300,Y=1250,TEXT='For La Nina years (Nino 4 < -0.2) - '+STRTRIM(STRING(N_ELEMENTS(lanina_years)),1)+' years',ALIGN=0.0
GPLOT,X=300,Y=1200,TEXT='20thC_rain = '+STRMID(STRTRIM(STRING(lanina_regression(0)),1),0,7)+' * SILO_rain + '+$
      STRMID(STRTRIM(STRING(lanina_constant),1),0,5),ALIGN=0.0
GPLOT,X=300,Y=1150,TEXT='Correlation = '+STRMID(STRTRIM(STRING(lanina_correlation),1),0,5),ALIGN=0.0

GPLOT,X=300,Y=1050,TEXT='For El Nino years (Nino 4 > 0.2) - '+STRTRIM(STRING(N_ELEMENTS(elnino_years)),1)+' years',ALIGN=0.0
GPLOT,X=300,Y=1000,TEXT='20thC_rain = '+STRMID(STRTRIM(STRING(elnino_regression(0)),1),0,7)+' * SILO_rain + '+$
      STRMID(STRTRIM(STRING(elnino_constant),1),0,5),ALIGN=0.0
GPLOT,X=300,Y=950,TEXT='Correlation = '+STRMID(STRTRIM(STRING(elnino_correlation),1),0,5),ALIGN=0.0

GPLOT,X=300,Y=850,TEXT='For neutral years (-0.2 <= Nino 4 <= 0.2) - '+STRTRIM(STRING(N_ELEMENTS(neutral_years)),1)+' years',ALIGN=0.0
GPLOT,X=300,Y=800,TEXT='20thC_rain = '+STRMID(STRTRIM(STRING(neutral_regression(0)),1),0,7)+' * SILO_rain + '+$
      STRMID(STRTRIM(STRING(neutral_constant),1),0,6),ALIGN=0.0
GPLOT,X=300,Y=750,TEXT='Correlation = '+STRMID(STRTRIM(STRING(neutral_correlation),1),0,5),ALIGN=0.0

GPLOT,X=800,Y=1250,TEXT='For all years - '+STRTRIM(STRING(nyear),1)+' years',ALIGN=0.0
GPLOT,X=800,Y=1200,TEXT='20thC_rain = '+STRMID(STRTRIM(STRING(total_regression(0)),1),0,7)+' * SILO_rain + '+$
      STRMID(STRTRIM(STRING(total_constant),1),0,5),ALIGN=0.0
GPLOT,X=800,Y=1150,TEXT='Correlation = '+STRMID(STRTRIM(STRING(total_correlation),1),0,5),ALIGN=0.0

GPLOT,X=[200,1300],Y=[200,1300],STYLE=1,COL=2
AXES,XSTEP=100,YSTEP=100,XTITLE='SILO (T62) Queensland area-averaged May-April total rainfall (mm/year)',YTITLE='20thC V2 Queensland area-averaged May-April total rainfall (mm/year)'
PSCLOSE

STOP

END

