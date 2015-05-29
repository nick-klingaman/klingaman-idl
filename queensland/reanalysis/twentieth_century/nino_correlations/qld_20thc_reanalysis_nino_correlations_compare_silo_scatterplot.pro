PRO qld_20thc_reanalysis_nino_correlations_compare_silo_scatterplot

; Make a scatterplot of Queensland rainfall anomalies versus NINO4
; values.  Compare SILO/HadISST and HiGEM control runs.

; Timeseries of observed NINO values
obs_nino_infile='/home/ss901165/datasets/NINO/nino4_hadisst.may-apr_ameans.1871-2008.nc'
obs_nino_start_year=1871
obs_nino_end_year=2008
obs_nino_nyears=obs_nino_end_year-obs_nino_start_year+1

; Timeseries of Australian rainfall from 20th Century V2 reanalysis, area-averaged by region
twentyc_precip_infile='/home/ss901165/datasets_mango/20THC_REANALYSIS/precip/20thc_reanalysis.may-apr_dmeans.1891-2007.precip.qld_region_aavg.nc'
twentyc_start_year=1891
twentyc_end_year=2007
twentyc_nyears=twentyc_end_year-twentyc_start_year+1

; Timeseries of Australian rainfall from SILO on T62 grid,
; area-averaged by region
silo_precip_infile='/home/ss901165/datasets_mango/SILO/t62/SILO_precip.may-apr_ameans.1891-2007.t62.qld_region_aavg.nc'
silo_start_year=1891
silo_end_year=2007
silo_nyears=silo_end_year-silo_start_year+1

; Read number of regions from TWENTYC input file
n_regions=N_ELEMENTS(OPEN_AND_EXTRACT(twentyc_precip_infile,'region'))

; Timeseries of 20thC V2 precipitation (note: use observed NINO4 as
; reanalysis is forced by HadISST SSTs).
twentyc_precip_infile='/home/ss901165/datasets_mango/20THC_REANALYSIS/precip/20thc_reanalysis.may-apr_ameans.1891-2007.precip.qld_region_aavg.nc'

; Read NINO indices
obs_nino_offset=(twentyc_start_year-obs_nino_start_year)
obs_nino_count=twentyc_nyears
obs_nino4_amean=OPEN_AND_EXTRACT(obs_nino_infile,'NINO4',offset=[obs_nino_offset],count=[obs_nino_count])
obs_nino4_amean=obs_nino4_amean-MEAN(obs_nino4_amean)

; Read timeseries of area-averaged rainfall
twentyc_precip_region_aavg=OPEN_AND_EXTRACT(twentyc_precip_infile,'rain_aavg_region',$
                                         offset=[0,0],count=[n_regions,twentyc_nyears])*86400.*365.
twentyc_precip_qld_aavg=OPEN_AND_EXTRACT(twentyc_precip_infile,'rain_aavg_allqld',$
                                      offset=[0],count=[twentyc_nyears])*365.*86400.
silo_precip_region_aavg=OPEN_AND_EXTRACT(silo_precip_infile,'rain_aavg_region',$
                                         offset=[0,0],count=[n_regions,twentyc_nyears])*365.
silo_precip_qld_aavg=OPEN_AND_EXTRACT(silo_precip_infile,'rain_aavg_allqld',$
                                      offset=[0],count=[twentyc_nyears])*365.

; Calculations for 20th Century reanalysis
lanina_years=where(obs_nino4_amean lt -0.2)
weak_lanina_years=where(obs_nino4_amean lt -0.2 and obs_nino4_amean gt -0.86)
elnino_years=where(obs_nino4_amean gt 0.2)
lanina_nino4=obs_nino4_amean[lanina_years]
weak_lanina_nino4=obs_nino4_amean[weak_lanina_years]
elnino_nino4=obs_nino4_amean[elnino_years]

twentyc_lanina_qld_aavg=twentyc_precip_qld_aavg[lanina_years]
twentyc_weak_lanina_qld_aavg=twentyc_precip_qld_aavg[weak_lanina_years]
twentyc_elnino_qld_aavg=twentyc_precip_qld_aavg[elnino_years]

silo_lanina_qld_aavg=silo_precip_qld_aavg[lanina_years]
silo_weak_lanina_qld_aavg=silo_precip_qld_aavg[weak_lanina_years]
silo_elnino_qld_aavg=silo_precip_qld_aavg[elnino_years]

twentyc_lanina_regression=REGRESS(lanina_nino4,twentyc_lanina_qld_aavg,CONST=twentyc_lanina_constant,CORRELATION=twentyc_lanina_correlation)
twentyc_weak_lanina_regression=REGRESS(weak_lanina_nino4,twentyc_weak_lanina_qld_aavg,CONST=twentyc_weak_lanina_constant,CORRELATION=twentyc_weak_lanina_correlation)
twentyc_elnino_regression=REGRESS(elnino_nino4,twentyc_elnino_qld_aavg,CONST=twentyc_elnino_constant,CORRELATION=twentyc_elnino_correlation)

silo_lanina_regression=REGRESS(lanina_nino4,silo_lanina_qld_aavg,CONST=silo_lanina_constant,CORRELATION=silo_lanina_correlation)
silo_weak_lanina_regression=REGRESS(weak_lanina_nino4,silo_weak_lanina_qld_aavg,CONST=silo_weak_lanina_constant,CORRELATION=silo_weak_lanina_correlation)
silo_elnino_regression=REGRESS(elnino_nino4,silo_elnino_qld_aavg,CONST=silo_elnino_constant,CORRELATION=silo_elnino_correlation)

psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/nino_correlations/qld_20thc_reanalysis_nino_correlations_compare_silo_scatterplot.20thc_v2_hadisst.all_qld.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2500,SPACE2=500,XOFFSET=200,YOFFSET=0,TFONT=2,TCHARSIZE=100,SPACE3=600
GSET,XMIN=-2.5,XMAX=2,YMIN=200,YMAX=1300,TITLE='Nino 4 (HadISST) against Queensland area-averaged rainfall (20th Century V2) - May-Apr annual means - 1891-2007'
red=FSC_COLOR("red",2)
blue=FSC_COLOR("blue",3)
GPLOT,X=obs_nino4_amean,Y=twentyc_precip_qld_aavg,/NOLINES,SYM=6
AXES,XTITLE='Annual mean (May-April) Nino 4 index (!Uo!NC)',XSTEP=0.5,YTITLE='Annual total (May-April) Queensland area-averaged rainfall (mm)',YSTEP=50,NDECS=2
GPLOT,X=REPLICATE(0,12),Y=indgen(12)*100+200
GPLOT,X=findgen(140)*(-0.01)-0.20,Y=(findgen(150)*(-0.01)-0.20)*twentyc_lanina_regression(0)+twentyc_lanina_constant,THICK=150,COL=3
GPLOT,X=findgen(76)*(-0.01)-0.20,Y=(findgen(76)*(-0.01)-0.20)*twentyc_weak_lanina_regression(0)+twentyc_weak_lanina_constant,THICK=200,COL=3,STYLE=2
GPLOT,X=findgen(140)*0.01+0.20,Y=(findgen(150)*0.01+0.20)*twentyc_elnino_regression(0)+twentyc_elnino_constant,THICK=150,COL=2

GPLOT,X=-2.2,Y=1180,TEXT='For La Nina years (Nino 4 < -0.2) ',ALIGN=0.0
GPLOT,X=-2.2,Y=1140,TEXT='QLD_rain = '+STRMID(STRTRIM(STRING(twentyc_lanina_regression(0)),1),0,7)+' * nino4 + '+$
      STRMID(STRTRIM(STRING(twentyc_lanina_constant),1),0,6),ALIGN=0.0
GPLOT,X=-2.2,Y=1100,TEXT='Correlation = '+STRMID(STRTRIM(STRING(twentyc_lanina_correlation),1),0,5),ALIGN=0.0
GPLOT,X=-2.2,Y=1000,TEXT='For La Nina years excl. top 10% (-0.86 < Nino 4 < -0.2) ',ALIGN=0.0
GPLOT,X=-2.2,Y=960,TEXT='QLD_rain = '+STRMID(STRTRIM(STRING(twentyc_weak_lanina_regression(0)),1),0,7)+' * nino4 + '+$
      STRMID(STRTRIM(STRING(twentyc_weak_lanina_constant),1),0,6),ALIGN=0.0
GPLOT,X=-2.2,Y=920,TEXT='Correlation = '+STRMID(STRTRIM(STRING(twentyc_weak_lanina_correlation),1),0,5),ALIGN=0.0
GPLOT,X=0.5,Y=950,TEXT='For El Nino years (Nino 4 > 0.2) ',ALIGN=0.0
GPLOT,X=0.5,Y=900,TEXT='QLD_rain = '+STRMID(STRTRIM(STRING(twentyc_elnino_regression(0)),1),0,7)+' * nino4 + '+$
      STRMID(STRTRIM(STRING(twentyc_elnino_constant),1),0,6),ALIGN=0.0
GPLOT,X=0.5,Y=850,TEXT='Correlation = '+STRMID(STRTRIM(STRING(twentyc_elnino_correlation),1),0,5),ALIGN=0.0
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/nino_correlations/qld_20thc_reanalysis_nino_correlations_compare_silo_scatterplot.obs_silo_hadisst.all_qld.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2500,SPACE2=500,XOFFSET=200,YOFFSET=0,TFONT=2,TCHARSIZE=100,SPACE3=600
GSET,XMIN=-2.5,XMAX=2,YMIN=200,YMAX=1300,TITLE='Nino 4 (HadISST) against Queensland area-averaged rainfall (SILO on T62 grid) - May-Apr annual means - 1891-2007'
red=FSC_COLOR("red",2)
blue=FSC_COLOR("blue",3)
GPLOT,X=obs_nino4_amean,Y=silo_precip_qld_aavg,/NOLINES,SYM=6
AXES,XTITLE='Annual mean (May-April) Nino 4 index (!Uo!NC)',XSTEP=0.5,YTITLE='Annual total (May-April) Queensland area-averaged rainfall (mm)',YSTEP=50,NDECS=2
GPLOT,X=REPLICATE(0,12),Y=indgen(12)*100+200
GPLOT,X=findgen(140)*(-0.01)-0.20,Y=(findgen(150)*(-0.01)-0.20)*silo_lanina_regression(0)+silo_lanina_constant,THICK=150,COL=3
GPLOT,X=findgen(76)*(-0.01)-0.20,Y=(findgen(76)*(-0.01)-0.20)*silo_weak_lanina_regression(0)+silo_weak_lanina_constant,THICK=200,COL=3,STYLE=2
GPLOT,X=findgen(140)*0.01+0.20,Y=(findgen(150)*0.01+0.20)*silo_elnino_regression(0)+silo_elnino_constant,THICK=150,COL=2

GPLOT,X=-2.2,Y=470,TEXT='For La Nina years (Nino 4 < -0.2) ',ALIGN=0.0
GPLOT,X=-2.2,Y=430,TEXT='QLD_rain = '+STRMID(STRTRIM(STRING(silo_lanina_regression(0)),1),0,7)+' * nino4 + '+$
      STRMID(STRTRIM(STRING(silo_lanina_constant),1),0,6),ALIGN=0.0
GPLOT,X=-2.2,Y=390,TEXT='Correlation = '+STRMID(STRTRIM(STRING(silo_lanina_correlation),1),0,5),ALIGN=0.0
GPLOT,X=-2.2,Y=320,TEXT='For La Nina years excl. top 10% (-0.86 < Nino 4 < -0.2) ',ALIGN=0.0
GPLOT,X=-2.2,Y=280,TEXT='QLD_rain = '+STRMID(STRTRIM(STRING(silo_weak_lanina_regression(0)),1),0,7)+' * nino4 + '+$
      STRMID(STRTRIM(STRING(silo_weak_lanina_constant),1),0,6),ALIGN=0.0
GPLOT,X=-2.2,Y=240,TEXT='Correlation = '+STRMID(STRTRIM(STRING(silo_weak_lanina_correlation),1),0,5),ALIGN=0.0
GPLOT,X=0.5,Y=950,TEXT='For El Nino years (Nino 4 > 0.2) ',ALIGN=0.0
GPLOT,X=0.5,Y=900,TEXT='QLD_rain = '+STRMID(STRTRIM(STRING(silo_elnino_regression(0)),1),0,7)+' * nino4 + '+$
      STRMID(STRTRIM(STRING(silo_elnino_constant),1),0,6),ALIGN=0.0
GPLOT,X=0.5,Y=850,TEXT='Correlation = '+STRMID(STRTRIM(STRING(silo_elnino_correlation),1),0,5),ALIGN=0.0

PSCLOSE,/NOVIEW

ysteps=[100,50,75,50,50]
FOR i=0,n_regions-1 DO BEGIN
   ; Processing for 20th Century Reanalysis V2  
   twentyc_this_region_aavg=REFORM(twentyc_precip_region_aavg(i,*))
   twentyc_lanina_region_aavg=twentyc_this_region_aavg[lanina_years]
   twentyc_weak_lanina_region_aavg=twentyc_this_region_aavg[weak_lanina_years]
   twentyc_elnino_region_aavg=twentyc_this_region_aavg[elnino_years]
   twentyc_lanina_regression=REGRESS(lanina_nino4,twentyc_lanina_region_aavg,CONST=twentyc_lanina_constant,CORRELATION=twentyc_lanina_correlation)
   twentyc_weak_lanina_regression=REGRESS(weak_lanina_nino4,twentyc_weak_lanina_region_aavg,CONST=twentyc_weak_lanina_constant,CORRELATION=twentyc_weak_lanina_correlation)
   twentyc_elnino_regression=REGRESS(elnino_nino4,twentyc_elnino_region_aavg,CONST=twentyc_elnino_constant,CORRELATION=twentyc_elnino_correlation)
   
   psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/nino_correlations/qld_20thc_reanalysis_nino_correlations_compare_silo_scatterplot.20thc_v2_hadisst.qld_region'+STRTRIM(STRING(i+1),1)+'.ps'
   ymin=FLOOR(MIN(twentyc_this_region_aavg)*0.9)/100*100.
   ymax=MAX(twentyc_this_region_aavg)*1.1
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2500,SPACE2=500,XOFFSET=200,YOFFSET=0,TFONT=2,TCHARSIZE=100,SPACE3=600
   GSET,XMIN=-2.5,XMAX=2,YMIN=ymin,YMAX=ymax,TITLE='Nino 4 (HadISST) against Queensland region '+STRTRIM(STRING(i+1),1)+' area-averaged rainfall (20th Century V2) - May-Apr annual means - 1891-2007'
   red=FSC_COLOR("red",2)
   blue=FSC_COLOR("blue",3)
   GPLOT,X=obs_nino4_amean,Y=twentyc_this_region_aavg,/NOLINES,SYM=6
   AXES,XTITLE='Annual mean (May-April) Nino 4 index (!Uo!NC)',XSTEP=0.5,YTITLE='Annual total (May-April) Queensland region '+STRTRIM(STRING(i+1),1)+' area-averaged rainfall (mm)',YSTEP=ysteps(i),NDECS=2
   GPLOT,X=REPLICATE(0,ymax-ymin),Y=indgen(ymax-ymin)+ymin
   GPLOT,X=findgen(140)*(-0.01)-0.20,Y=(findgen(150)*(-0.01)-0.20)*twentyc_lanina_regression(0)+twentyc_lanina_constant,THICK=150,COL=3
   GPLOT,X=findgen(76)*(-0.01)-0.20,Y=(findgen(76)*(-0.01)-0.20)*twentyc_weak_lanina_regression(0)+twentyc_weak_lanina_constant,THICK=200,COL=3,STYLE=2
   GPLOT,X=findgen(140)*0.01+0.20,Y=(findgen(150)*0.01+0.20)*twentyc_elnino_regression(0)+twentyc_elnino_constant,THICK=150,COL=2
   GPLOT,X=-2.4,Y=ymin+7*ysteps(i),TEXT='For La Nina years (Nino 4 < -0.2) ',ALIGN=0.0,CHARSIZE=80
   GPLOT,X=-2.4,Y=ymin+6*ysteps(i),TEXT='QLD_region'+STRTRIM(STRING(i+1),1)+'_rain = '+STRMID(STRTRIM(STRING(twentyc_lanina_regression(0)),1),0,7)+' * nino4 + '+$
         STRMID(STRTRIM(STRING(twentyc_lanina_constant),1),0,6),ALIGN=0.0,CHARSIZE=80
   GPLOT,X=-2.4,Y=ymin+5*ysteps(i),TEXT='Correlation = '+STRMID(STRTRIM(STRING(twentyc_lanina_correlation),1),0,5),ALIGN=0.0,CHARSIZE=80
   
   GPLOT,X=-2.4,Y=ymin+3*ysteps(i),TEXT='For La Nina years excl. top 10% (-0.86 < Nino 4 < -0.2) ',ALIGN=0.0,CHARSIZE=80
   GPLOT,X=-2.4,Y=ymin+2*ysteps(i),TEXT='QLD_region'+STRTRIM(STRING(i+1),1)+'_rain = '+STRMID(STRTRIM(STRING(twentyc_weak_lanina_regression(0)),1),0,7)+' * nino4 + '+$
         STRMID(STRTRIM(STRING(twentyc_weak_lanina_constant),1),0,6),ALIGN=0.0,CHARSIZE=80
   GPLOT,X=-2.4,Y=ymin+ysteps(i),TEXT='Correlation = '+STRMID(STRTRIM(STRING(twentyc_weak_lanina_correlation),1),0,5),ALIGN=0.0,CHARSIZE=80

   GPLOT,X=0.2,Y=ymax-2*ysteps(i),TEXT='For El Nino years (Nino 4 > 0.2) ',ALIGN=0.0,CHARSIZE=80
   GPLOT,X=0.2,Y=ymax-3*ysteps(i),TEXT='QLD_region'+STRTRIM(STRING(i+1),1)+'_rain = '+STRMID(STRTRIM(STRING(twentyc_elnino_regression(0)),1),0,7)+' * nino4 + '+$
         STRMID(STRTRIM(STRING(twentyc_elnino_constant),1),0,6),ALIGN=0.0,CHARSIZE=80
   GPLOT,X=0.2,Y=ymax-4*ysteps(i),TEXT='Correlation = '+STRMID(STRTRIM(STRING(twentyc_elnino_correlation),1),0,5),ALIGN=0.0,CHARSIZE=80
   PSCLOSE,/NOVIEW

    ; Processing for 20th Century Reanalysis V2  
   silo_this_region_aavg=REFORM(silo_precip_region_aavg(i,*))
   silo_lanina_region_aavg=silo_this_region_aavg[lanina_years]
   silo_weak_lanina_region_aavg=silo_this_region_aavg[weak_lanina_years]
   silo_elnino_region_aavg=silo_this_region_aavg[elnino_years]
   silo_lanina_regression=REGRESS(lanina_nino4,silo_lanina_region_aavg,CONST=silo_lanina_constant,CORRELATION=silo_lanina_correlation)
   silo_weak_lanina_regression=REGRESS(weak_lanina_nino4,silo_weak_lanina_region_aavg,CONST=silo_weak_lanina_constant,CORRELATION=silo_weak_lanina_correlation)
   silo_elnino_regression=REGRESS(elnino_nino4,silo_elnino_region_aavg,CONST=silo_elnino_constant,CORRELATION=silo_elnino_correlation)
   
   psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/nino_correlations/qld_20thc_reanalysis_nino_correlations_compare_silo_scatterplot.obs_silo_hadisst.qld_region'+STRTRIM(STRING(i+1),1)+'.ps'
   ymin=FLOOR(MIN(silo_this_region_aavg)*0.9)/100*100.
   ymax=MAX(silo_this_region_aavg)*1.1
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2500,SPACE2=500,XOFFSET=200,YOFFSET=0,TFONT=2,TCHARSIZE=100,SPACE3=600
   GSET,XMIN=-2.5,XMAX=2,YMIN=ymin,YMAX=ymax,TITLE='Nino 4 (HadISST) against Queensland region '+STRTRIM(STRING(i+1),1)+' area-averaged rainfall (SILO on T62 grid) - May-Apr annual means - 1891-2007'
   red=FSC_COLOR("red",2)
   blue=FSC_COLOR("blue",3)
   GPLOT,X=obs_nino4_amean,Y=silo_this_region_aavg,/NOLINES,SYM=6
   AXES,XTITLE='Annual mean (May-April) Nino 4 index (!Uo!NC)',XSTEP=0.5,YTITLE='Annual total (May-April) Queensland region '+STRTRIM(STRING(i+1),1)+' area-averaged rainfall (mm)',YSTEP=ysteps(i),NDECS=2
   GPLOT,X=REPLICATE(0,ymax-ymin),Y=indgen(ymax-ymin)+ymin
   GPLOT,X=findgen(140)*(-0.01)-0.20,Y=(findgen(150)*(-0.01)-0.20)*silo_lanina_regression(0)+silo_lanina_constant,THICK=150,COL=3
   GPLOT,X=findgen(76)*(-0.01)-0.20,Y=(findgen(76)*(-0.01)-0.20)*silo_weak_lanina_regression(0)+silo_weak_lanina_constant,THICK=200,COL=3,STYLE=2
   GPLOT,X=findgen(140)*0.01+0.20,Y=(findgen(150)*0.01+0.20)*silo_elnino_regression(0)+silo_elnino_constant,THICK=150,COL=2
   GPLOT,X=-2.4,Y=ymin+7*ysteps(i),TEXT='For La Nina years (Nino 4 < -0.2) ',ALIGN=0.0,CHARSIZE=80
   GPLOT,X=-2.4,Y=ymin+6*ysteps(i),TEXT='QLD_region'+STRTRIM(STRING(i+1),1)+'_rain = '+STRMID(STRTRIM(STRING(silo_lanina_regression(0)),1),0,7)+' * nino4 + '+$
         STRMID(STRTRIM(STRING(silo_lanina_constant),1),0,6),ALIGN=0.0,CHARSIZE=80
   GPLOT,X=-2.4,Y=ymin+5*ysteps(i),TEXT='Correlation = '+STRMID(STRTRIM(STRING(silo_lanina_correlation),1),0,5),ALIGN=0.0,CHARSIZE=80
   
   GPLOT,X=-2.4,Y=ymin+3*ysteps(i),TEXT='For La Nina years excl. top 10% (-0.86 < Nino 4 < -0.2) ',ALIGN=0.0,CHARSIZE=80
   GPLOT,X=-2.4,Y=ymin+2*ysteps(i),TEXT='QLD_region'+STRTRIM(STRING(i+1),1)+'_rain = '+STRMID(STRTRIM(STRING(silo_weak_lanina_regression(0)),1),0,7)+' * nino4 + '+$
         STRMID(STRTRIM(STRING(silo_weak_lanina_constant),1),0,6),ALIGN=0.0,CHARSIZE=80
   GPLOT,X=-2.4,Y=ymin+ysteps(i),TEXT='Correlation = '+STRMID(STRTRIM(STRING(silo_weak_lanina_correlation),1),0,5),ALIGN=0.0,CHARSIZE=80

   GPLOT,X=0.2,Y=ymax-2*ysteps(i),TEXT='For El Nino years (Nino 4 > 0.2) ',ALIGN=0.0,CHARSIZE=80
   GPLOT,X=0.2,Y=ymax-3*ysteps(i),TEXT='QLD_region'+STRTRIM(STRING(i+1),1)+'_rain = '+STRMID(STRTRIM(STRING(silo_elnino_regression(0)),1),0,7)+' * nino4 + '+$
         STRMID(STRTRIM(STRING(silo_elnino_constant),1),0,6),ALIGN=0.0,CHARSIZE=80
   GPLOT,X=0.2,Y=ymax-4*ysteps(i),TEXT='Correlation = '+STRMID(STRTRIM(STRING(silo_elnino_correlation),1),0,5),ALIGN=0.0,CHARSIZE=80
   PSCLOSE,/NOVIEW

ENDFOR

STOP

END
