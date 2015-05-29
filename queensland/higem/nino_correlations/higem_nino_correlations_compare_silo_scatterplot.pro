PRO higem_nino_correlations_compare_silo_scatterplot

; Make a scatterplot of Queensland rainfall anomalies versus NINO4
; values.  Compare SILO/HadISST and HiGEM control runs.

; Timeseries of observed NINO values
obs_nino_infile='/home/ss901165/datasets/NINO/nino4_hadisst.may-apr_ameans.1871-2008.nc'
obs_nino_start_year=1871
obs_nino_end_year=2007
obs_nino_nyears=obs_nino_end_year-obs_nino_start_year+1

; Timeseries of Australian rainfall from SILO, area-averaged by region
;silo_precip_infile='/home/ss901165/datasets_mango/SILO/one_quarter/SILO_precip.may-apr_ameans.1900-2008.0.25x0.25.qld_region_aavg.nc'
silo_precip_infile='/home/ss901165/datasets_mango/SILO/n144/SILO_precip.may-apr_ameans.1900-2008.n144.qld_region_aavg.nc'
silo_start_year=1900
silo_end_year=2007
silo_nyears=silo_end_year-silo_start_year+1

; Read number of regions from SILO input file
n_regions=N_ELEMENTS(OPEN_AND_EXTRACT(silo_precip_infile,'region'))

; Timeseries of HiGEM precipitation and NINO indices
higem_xbylr_precip_infile='/home/ss901165/higem_qccce/hpcx_control_xbylr/higem_xbylr.may-apr_ameans.h9-t5.precip.qld_aavg.nc'
higem_xbylr_nino_infile='/home/ss901165/higem_qccce/hpcx_control_xbylr/higem_xbylr.may-apr_ameans.h9-t5.nino_indices.nc'
higem_eafeb_precip_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.may-apr_ameans.h9-w8.precip.qld_aavg.nc'
higem_eafeb_nino_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.may-apr_ameans.h9-w8.nino_indices.nc'

; Read NINO indices
obs_nino_offset=(silo_start_year-obs_nino_start_year)
obs_nino_count=silo_nyears

obs_nino4_fullts=OPEN_AND_EXTRACT(obs_nino_infile,'NINO4')
obs_nino4_fullts_trend=REGRESS(indgen(obs_nino_nyears),obs_nino4_fullts)
obs_nino4_fullts_detrended=obs_nino4_fullts-(indgen(obs_nino_nyears)*obs_nino4_fullts_trend(0))
obs_nino4_fullts_detrended=obs_nino4_fullts_detrended-MEAN(obs_nino4_fullts_detrended)
;obs_nino4_fullts_detrended=obs_nino4_fullts-MEAN(obs_nino4_fullts)

;obs_nino4_amean=OPEN_AND_EXTRACT(obs_nino_infile,'NINO4',offset=[obs_nino_offset],count=[obs_nino_count])
obs_nino4_amean=obs_nino4_fullts_detrended(obs_nino_offset:obs_nino_nyears-1)
obs_nino_count=N_ELEMENTS(obs_nino4_amean)
;obs_nino4_mean=MEAN(obs_nino4_amean)
;obs_nino4_amean=obs_nino4_amean-MEAN(obs_nino4_amean)
;obs_nino4_trend=REGRESS(indgen(obs_nino_count),obs_nino4_amean)
;obs_nino4_amean=obs_nino4_amean-obs_nino4_fullts_mean
;obs_nino4_amean=obs_nino4_amean-obs_nino4_fullts_trend(0)*(indgen(obs_nino_count)+obs_nino_offset)

; Construct a smoothed timeseries of Nino 4
;obs_nino4_smoothed=OPEN_AND_EXTRACT(obs_nino_infile,'NINO4',offset=[obs_nino_offset-10],count=[obs_nino_count+10])
;obs_nino4_smoothed_mean=MEAN(obs_nino4_smoothed)
obs_nino4_smoothed=obs_nino4_fullts_detrended(obs_nino_offset-10:obs_nino_nyears-1)
;obs_nino4_smoothed_trend=REGRESS(indgen(obs_nino_count+10),obs_nino4_smoothed)
;obs_nino4_smoothed=obs_nino4_smoothed-obs_nino4_fullts_trend(0)*(indgen(obs_nino_count+10)+obs_nino_offset-10)
obs_nino4_smoothed=SMOOTH(obs_nino4_smoothed,21)
obs_nino4_smoothed=obs_nino4_smoothed(10:N_ELEMENTS(obs_nino4_smoothed)-11)
;obs_nino4_smoothed=obs_nino4_smoothed-MEAN(obs_nino4_smoothed)

higem_xbylr_nyears=116
higem_xbylr_nino4_amean=OPEN_AND_EXTRACT(higem_xbylr_nino_infile,'nino4',offset=[0],count=[higem_xbylr_nyears])
higem_eafeb_nyears=149
higem_eafeb_nino4_amean=OPEN_AND_EXTRACT(higem_eafeb_nino_infile,'nino4',offset=[0],count=[higem_eafeb_nyears])

; Read timeseries of area-averaged rainfall
silo_precip_region_aavg=OPEN_AND_EXTRACT(silo_precip_infile,'rain_aavg_region',$
                                         offset=[0,0],count=[n_regions,silo_nyears])
silo_precip_qld_aavg=OPEN_AND_EXTRACT(silo_precip_infile,'rain_aavg_allqld',$
                                      offset=[0],count=[silo_nyears])*365.
higem_xbylr_precip_qld_aavg=OPEN_AND_EXTRACT(higem_xbylr_precip_infile,'rain_aavg_allqld',offset=[0],count=[higem_xbylr_nyears])*360.*86400.
higem_eafeb_precip_qld_aavg=OPEN_AND_EXTRACT(higem_eafeb_precip_infile,'rain_aavg_allqld',offset=[0],count=[higem_eafeb_nyears])*360.*86400.

; Calculations for SILO
;obs_nino4_amean=obs_nino4_amean(0:obs_nino_count-11)
;silo_precip_qld_aavg=silo_precip_qld_aavg(0:obs_nino_count-11)
sorted_obs_nino4_amean=obs_nino4_amean[SORT(obs_nino4_amean)]
;lanina_boundary=sorted_obs_nino4_amean(obs_nino_count/3-1)
lanina_boundary=-0.3
lanina_boundary_str=STRMID(STRTRIM(STRING(lanina_boundary),1),0,5)
;elnino_boundary=sorted_obs_nino4_amean(obs_nino_count*2/3-1)
elnino_boundary=0.3
elnino_boundary_str=STRMID(STRTRIM(STRING(elnino_boundary),1),0,4)

lanina_years=where(obs_nino4_amean lt lanina_boundary+0.1)
weak_lanina_years=where(obs_nino4_amean lt lanina_boundary+0.1 and obs_nino4_amean gt -1.0)
elnino_years=where(obs_nino4_amean gt elnino_boundary-0.1)
lanina_nino4=obs_nino4_amean[lanina_years]
weak_lanina_nino4=obs_nino4_amean[weak_lanina_years]
elnino_nino4=obs_nino4_amean[elnino_years]
lanina_qld_aavg=silo_precip_qld_aavg[lanina_years]
weak_lanina_qld_aavg=silo_precip_qld_aavg[weak_lanina_years]
elnino_qld_aavg=silo_precip_qld_aavg[elnino_years]
weak_years=where((obs_nino4_amean gt -1.0 and obs_nino4_amean lt lanina_boundary) or (obs_nino4_amean lt 0.75 and obs_nino4_amean gt 0.2))
lanina_regression=REGRESS(lanina_nino4,lanina_qld_aavg,CONST=lanina_constant,CORRELATION=lanina_correlation)
weak_lanina_regression=REGRESS(weak_lanina_nino4,weak_lanina_qld_aavg,CONST=weak_lanina_constant,CORRELATION=weak_lanina_correlation)
elnino_regression=REGRESS(elnino_nino4,elnino_qld_aavg,CONST=elnino_constant,CORRELATION=elnino_correlation)
all_regression=REGRESS(obs_nino4_amean,silo_precip_qld_aavg,CONST=all_constant,CORRELATION=all_correlation)

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo_scatterplot.obs_silo_hadisst.all_qld.small_boundary.1900-2008.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2500,SPACE2=500,XOFFSET=200,YOFFSET=2000,TFONT=2,TCHARSIZE=100,SPACE3=600
GSET,XMIN=-1.5,XMAX=1.5,YMIN=100,YMAX=1300,TITLE='Nino 4 (HadISST) against Queensland area-averaged rainfall (SILO N144) - 1900-2008'
red=FSC_COLOR("red",2)
blue=FSC_COLOR("blue",3)
black=FSC_COLOR("black",4)
GPLOT,X=obs_nino4_amean,Y=silo_precip_qld_aavg,/NOLINES,SYM=6
AXES,XTITLE='Annual mean (May-April) Nino 4 index (!Uo!NC)',XSTEP=0.5,YTITLE='Annual total (May-April) Queensland area-averaged rainfall (mm)',YSTEP=100,NDECS=2,$
     XMINOR=0.1,YMINOR=50
GPLOT,X=[0,0],Y=[100,1300],STYLE=2
GPLOT,X=[lanina_boundary,-1.4],Y=[lanina_boundary,-1.4]*lanina_regression(0)+lanina_constant,THICK=150,COL=3
GPLOT,X=[lanina_boundary,-0.9],Y=[lanina_boundary,-0.9]*weak_lanina_regression(0)+weak_lanina_constant,THICK=200,COL=3,STYLE=2
GPLOT,X=[elnino_boundary,1.4],Y=[elnino_boundary,1.4]*elnino_regression(0)+elnino_constant,THICK=150,COL=2
GPLOT,X=[-1.4,1.4],Y=[-1.4,1.4]*all_regression(0)+all_constant,THICK=150,COL=4

GPLOT,X=-1.4,Y=410,TEXT='For La Nina years (N4 < '+lanina_boundary_str+') ',ALIGN=0.0
GPLOT,X=-1.4,Y=370,TEXT='QLD_rain = '+STRMID(STRTRIM(STRING(lanina_regression(0)),1),0,7)+' * nino4 + '+$
      STRMID(STRTRIM(STRING(lanina_constant),1),0,6),ALIGN=0.0
GPLOT,X=-1.4,Y=330,TEXT='Correlation = '+STRMID(STRTRIM(STRING(lanina_correlation),1),0,5),ALIGN=0.0

GPLOT,X=-1.4,Y=230,TEXT='For La Nina years excl. top 10% (-1.16 < N4 < '+lanina_boundary_str+') ',ALIGN=0.0
GPLOT,X=-1.4,Y=190,TEXT='QLD_rain = '+STRMID(STRTRIM(STRING(weak_lanina_regression(0)),1),0,7)+' * nino4 + '+$
      STRMID(STRTRIM(STRING(weak_lanina_constant),1),0,6),ALIGN=0.0
GPLOT,X=-1.4,Y=150,TEXT='Correlation = '+STRMID(STRTRIM(STRING(weak_lanina_correlation),1),0,5),ALIGN=0.0

GPLOT,X=0.2,Y=1000,TEXT='For El Nino years (N4 > '+elnino_boundary_str+') ',ALIGN=0.0
GPLOT,X=0.2,Y=960,TEXT='QLD_rain = '+STRMID(STRTRIM(STRING(elnino_regression(0)),1),0,7)+' * nino4 + '+$
      STRMID(STRTRIM(STRING(elnino_constant),1),0,6),ALIGN=0.0
GPLOT,X=0.2,Y=920,TEXT='Correlation = '+STRMID(STRTRIM(STRING(elnino_correlation),1),0,5),ALIGN=0.0

GPLOT,X=0.2,Y=1200,TEXT='For all years',ALIGN=0.0
GPLOT,X=0.2,Y=1160,TEXT='QLD_rain = '+STRMID(STRTRIM(STRING(all_regression(0)),1),0,7)+' * nino4 + '+$
      STRMID(STRTRIM(STRING(all_constant),1),0,6),ALIGN=0.0
GPLOT,X=0.2,Y=1120,TEXT='Correlation = '+STRMID(STRTRIM(STRING(all_correlation),1),0,5),ALIGN=0.0

PSCLOSE

obs_nino4_amean_remove_smoothed=obs_nino4_amean(0:obs_nino_count-11)-obs_nino4_smoothed
;print,obs_nino4_amean_remove_smoothed(SORT(obs_nino4_amean_remove_smoothed))

sorted_obs_nino4_amean_remove_smoothed=obs_nino4_amean_remove_smoothed[SORT(obs_nino4_amean_remove_smoothed)]
smoothed_lanina_boundary=sorted_obs_nino4_amean_remove_smoothed(obs_nino_count/3-1)
smoothed_lanina_boundary=-0.3
smoothed_lanina_boundary_str=STRMID(STRTRIM(STRING(smoothed_lanina_boundary),1),0,5)
smoothed_elnino_boundary=sorted_obs_nino4_amean_remove_smoothed(obs_nino_count*2/3-1)
smoothed_elnino_boundary=0.3
smoothed_elnino_boundary_str=STRMID(STRTRIM(STRING(smoothed_elnino_boundary),1),0,4)

smoothed_lanina_years=where(obs_nino4_amean_remove_smoothed lt smoothed_lanina_boundary)
smoothed_lanina_nino4=obs_nino4_amean_remove_smoothed[smoothed_lanina_years]
smoothed_lanina_qld_aavg=silo_precip_qld_aavg[smoothed_lanina_years]
smoothed_weak_lanina_years=where(obs_nino4_amean_remove_smoothed lt smoothed_lanina_boundary and obs_nino4_amean_remove_smoothed gt -0.92)
smoothed_weak_lanina_nino4=obs_nino4_amean_remove_smoothed[smoothed_weak_lanina_years]
smoothed_weak_lanina_qld_aavg=silo_precip_qld_aavg[smoothed_weak_lanina_years]
smoothed_elnino_years=where(obs_nino4_amean_remove_smoothed gt smoothed_elnino_boundary)
;smoothed_elnino_years=elnino_years
smoothed_elnino_nino4=obs_nino4_amean_remove_smoothed[smoothed_elnino_years]
smoothed_elnino_qld_aavg=silo_precip_qld_aavg[smoothed_elnino_years]

smoothed_lanina_regression=REGRESS(smoothed_lanina_nino4,smoothed_lanina_qld_aavg,CONST=smoothed_lanina_constant,$
                                   CORRELATION=smoothed_lanina_correlation)
smoothed_weak_lanina_regression=REGRESS(smoothed_weak_lanina_nino4,smoothed_weak_lanina_qld_aavg,CONST=smoothed_weak_lanina_constant,$
                                        CORRELATION=smoothed_weak_lanina_correlation)
smoothed_elnino_regression=REGRESS(smoothed_elnino_nino4,smoothed_elnino_qld_aavg,CONST=smoothed_elnino_constant,$
                                   CORRELATION=smoothed_elnino_correlation)
smoothed_all_regression=REGRESS(obs_nino4_amean_remove_smoothed,silo_precip_qld_aavg(0:obs_nino_count-11),CONST=smoothed_all_constant,$
                                CORRELATION=smoothed_all_correlation)

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo_scatterplot.obs_silo_hadisst.remove_21yr_smoothed_nino4.all_qld.small_boundary.1900-1998.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2500,SPACE2=500,XOFFSET=200,YOFFSET=2000,TFONT=2,TCHARSIZE=100,SPACE3=600
GSET,XMIN=-1.5,XMAX=1.5,YMIN=100,YMAX=1300,TITLE='Nino 4 (HadISST) against Queensland area-averaged rainfall (SILO 0.25) - 1900-1998'
GPLOT,X=obs_nino4_amean_remove_smoothed,Y=silo_precip_qld_aavg(0:obs_nino_count-11),/NOLINES,SYM=6
AXES,XTITLE='Annual mean (May-April) Nino 4 index (!Uo!NC) with 21 year running mean removed',XSTEP=0.5,YTITLE='Annual total (May-April) Queensland area-averaged rainfall (mm)',NDECS=2,$
     XMINOR=0.1,YSTEP=100,YMINOR=50
GPLOT,X=[0,0],Y=[100,1300],STYLE=1

blue=FSC_COLOR("blue",3)
red=FSC_COLOR("red",2)
black=FSC_COLOR("black",4)

xpoints=[smoothed_lanina_boundary,-1.4]
GPLOT,X=xpoints,Y=xpoints*smoothed_lanina_regression(0)+smoothed_lanina_constant,COL=3,STYLE=0,THICK=150
xpoints=[smoothed_lanina_boundary,-0.921]
GPLOT,X=xpoints,Y=xpoints*smoothed_weak_lanina_regression(0)+smoothed_weak_lanina_constant,COL=3,STYLE=2,THICK=150
xpoints=[smoothed_elnino_boundary,1.4]
GPLOT,X=xpoints,Y=xpoints*smoothed_elnino_regression(0)+smoothed_elnino_constant,COL=2,STYLE=0,THICK=150
xpoints=[-1.4,1.4]
GPLOT,X=xpoints,Y=xpoints*smoothed_all_regression(0)+smoothed_all_constant,COL=4,STYLE=0,THICK=150

GPLOT,X=-1.4,Y=410,TEXT='For La Nina years (N4 < '+smoothed_lanina_boundary_str+') ',ALIGN=0.0
GPLOT,X=-1.4,Y=370,TEXT='QLD_rain = '+STRMID(STRTRIM(STRING(smoothed_lanina_regression(0)),1),0,7)+' * nino4 + '+$
      STRMID(STRTRIM(STRING(smoothed_lanina_constant),1),0,6),ALIGN=0.0
GPLOT,X=-1.4,Y=330,TEXT='Correlation = '+STRMID(STRTRIM(STRING(smoothed_lanina_correlation),1),0,5),ALIGN=0.0

GPLOT,X=-1.4,Y=230,TEXT='For La Nina years excl. top 10% (-0.92 < N4 < '+smoothed_lanina_boundary_str+') ',ALIGN=0.0
GPLOT,X=-1.4,Y=190,TEXT='QLD_rain = '+STRMID(STRTRIM(STRING(smoothed_weak_lanina_regression(0)),1),0,7)+' * nino4 + '+$
      STRMID(STRTRIM(STRING(smoothed_weak_lanina_constant),1),0,6),ALIGN=0.0
GPLOT,X=-1.4,Y=150,TEXT='Correlation = '+STRMID(STRTRIM(STRING(smoothed_weak_lanina_correlation),1),0,5),ALIGN=0.0

GPLOT,X=0.2,Y=870,TEXT='For El Nino years (N4 > '+smoothed_elnino_boundary_str+') ',ALIGN=0.0
GPLOT,X=0.2,Y=830,TEXT='QLD_rain = '+STRMID(STRTRIM(STRING(smoothed_elnino_regression(0)),1),0,7)+' * nino4 + '+$
      STRMID(STRTRIM(STRING(smoothed_elnino_constant),1),0,6),ALIGN=0.0
GPLOT,X=0.2,Y=790,TEXT='Correlation = '+STRMID(STRTRIM(STRING(smoothed_elnino_correlation),1),0,5),ALIGN=0.0

GPLOT,X=0.2,Y=1050,TEXT='For all years',ALIGN=0.0
GPLOT,X=0.2,Y=1010,TEXT='QLD_rain = '+STRMID(STRTRIM(STRING(smoothed_all_regression(0)),1),0,7)+' * nino4 + '+$
      STRMID(STRTRIM(STRING(smoothed_all_constant),1),0,6),ALIGN=0.0
GPLOT,X=0.2,Y=970,TEXT='Correlation = '+STRMID(STRTRIM(STRING(smoothed_all_correlation),1),0,5),ALIGN=0.0

PSCLOSE

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo_scatterplot.obs_silo_hadisst.n4-vs-smoothedn4_ranks.all_qld.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2500,SPACE2=500,XOFFSET=200,YOFFSET=1000,TFONT=2,TCHARSIZE=100,SPACE3=600
GSET,XMIN=-1.5,XMAX=1.5,YMIN=-0.25,YMAX=0.25,TITLE='Queensland May-April total, area-averaged rainfall with Pacific SSTs'
CS,SCALE=26,NCOLS=obs_nino_count-11,/REV
smallest_size_dot=80
dot_spacing=2
blue=FSC_COLOR("blue",2)
red=FSC_COLOR("red",3)
silo_precip_qld_aavg_rank=SORT(silo_precip_qld_aavg(0:obs_nino_count-11))
FOR i=0,obs_nino_count-12 DO BEGIN
   this_rank_position=silo_precip_qld_aavg_rank(i)
   IF i lt (obs_nino_count-11)/2 THEN BEGIN
      this_color=3
   ENDIF ELSE $
      this_color=2
   GPLOT,X=obs_nino4_amean(this_rank_position),Y=obs_nino4_smoothed(this_rank_position),SIZE=100,COL=this_color,SYM=6
ENDFOR
GPLOT,X=[-1.5,1.5],Y=[0,0],STYLE=1
GPLOT,X=[0,0],Y=[-0.25,0.25],STYLE=1
GLEGEND,labels=['Above-median rainfall','Below-median rainfall'],COL=[2,3],SYM=[6,6],LEGPOS=3,LENGTH=[0,0]
AXES,XSTEP=0.3,XMINOR=0.1,YSTEP=0.05,YMINOR=0.025,XTITLE='Annual mean (May-April) Nino 4',YTITLE='21-year running mean of Annual mean (May-April) Nino 4',NDECS=2
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo_scatterplot.obs_silo_hadisst.smoothedn4-vs-regression_residuals.all_qld.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2500,SPACE2=500,XOFFSET=200,YOFFSET=1000,TFONT=2,TCHARSIZE=100,SPACE3=600
GSET,XMIN=-0.25,XMAX=0.25,YMIN=-500,YMAX=500,TITLE='21-year smoothed, Annual mean N4 against QLD rainfall regression residuals from full N4 timeseries'
;regression_residuals=fltarr(obs_nino_count-11)
;elnino_regression_residuals=fltarr(N_ELEMENTS(elnino_years))
;lanina_regression_residuals=fltarr(N_ELEMENTS(lanina_years))
blue=FSC_COLOR("blue",2)
red=FSC_COLOR("red",3)
black=FSC_COLOR("black",4)
FOR i=0,obs_nino_count-12 DO BEGIN
   regression_residual=silo_precip_qld_aavg(i)-(obs_nino4_amean(i)*all_regression(0)+all_constant)
;FOR i=0,N_ELEMENTS(elnino_years)-1 DO BEGIN
;   IF elnino_years(i) lt obs_nino_count-11 THEN BEGIN
;      this_residual=elnino_qld_aavg(i)-(elnino_nino4(i)*elnino_regression(0)+elnino_constant)
;      print,this_residual
;      GPLOT,X=obs_nino4_smoothed(elnino_years(i)),Y=this_residual,COL=3,SYM=6,SIZE=100
;   ENDIF
;ENDFOR
;FOR i=0,N_ELEMENTS(lanina_years)-1 DO $
;   lanina_regression_residuals(i)=lanina_qld_aavg(i)-(lanina_nino4(i)*lanina_regression(0)+lanina_constant)
   IF obs_nino4_amean(i) gt elnino_boundary THEN BEGIN
      GPLOT,X=obs_nino4_smoothed(i),Y=regression_residual,SIZE=100,SYM=6,/NOLINES,COL=3
   ENDIF ELSE IF obs_nino4_amean(i) lt lanina_boundary THEN BEGIN
      GPLOT,X=obs_nino4_smoothed(i),Y=regression_residual,SIZE=100,SYM=6,/NOLINES,COL=2
   ENDIF ELSE $
      GPLOT,X=obs_nino4_smoothed(i),Y=regression_residual,SIZE=100,SYM=6,/NOLINES,COL=4
ENDFOR
GPLOT,X=[0,0],Y=[-500,500],STYLE=1
GPLOT,X=[-0.25,0.25],Y=[0,0],STYLE=1
AXES,XSTEP=0.05,XMINOR=0.025,YSTEP=100,YMINOR=25,NDECS=2,YTITLE='Residual from regression (mm; positive for more rainfall than expected)',$
     XTITLE='21-year running mean of Annual mean Nino 4 (!Uo!NC)'
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo_scatterplot.obs_silo_hadisst.smoothedn4-vs-regression_diffs.all_qld.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2500,SPACE2=500,XOFFSET=200,YOFFSET=1000,TFONT=2,TCHARSIZE=100,SPACE3=600
GSET,XMIN=-1.5,XMAX=1.5,YMIN=-30,YMAX=30,TITLE='Difference in regression residuals: Residual from (N4_HF,Q_rain) - Residual from (N4,Q_rain)'
blue=FSC_COLOR("blue",2)
red=FSC_COLOR("red",3)
black=FSC_COLOR("black",4)
FOR i=0,obs_nino_count-12 DO BEGIN
;   full_regression_residual=ABS(silo_precip_qld_aavg(i)-(obs_nino4_amean(i)*all_regression(0)+all_constant))
;   hf_regression_residual=ABS(silo_precip_qld_aavg(i)-(obs_nino4_smoothed(i)*smoothed_all_regression(0)+smoothed_all_constant))
   IF obs_nino4_amean(i) gt elnino_boundary and obs_nino4_amean_remove_smoothed(i) gt 0.2 THEN BEGIN
      full_regression_residual=ABS(silo_precip_qld_aavg(i)-(obs_nino4_amean(i)*all_regression(0)+all_constant))
      hf_regression_residual=ABS(silo_precip_qld_aavg(i)-(obs_nino4_amean_remove_smoothed(i)*smoothed_all_regression(0)+smoothed_all_constant))
      GPLOT,X=obs_nino4_amean(i),Y=hf_regression_residual-full_regression_residual,SIZE=100,SYM=6,/NOLINES,COL=3
   ENDIF ELSE IF obs_nino4_amean(i) lt lanina_boundary and obs_nino4_amean_remove_smoothed(i) lt lanina_boundary THEN BEGIN
      full_regression_residual=ABS(silo_precip_qld_aavg(i)-(obs_nino4_amean(i)*all_regression(0)+all_constant))
      hf_regression_residual=ABS(silo_precip_qld_aavg(i)-(obs_nino4_amean_remove_smoothed(i)*smoothed_all_regression(0)+smoothed_all_constant))
      GPLOT,X=obs_nino4_amean(i),Y=hf_regression_residual-full_regression_residual,SIZE=100,SYM=6,/NOLINES,COL=2
   ENDIF ELSE BEGIN
      full_regression_residual=ABS(silo_precip_qld_aavg(i)-(obs_nino4_amean(i)*all_regression(0)+all_constant))
      hf_regression_residual=ABS(silo_precip_qld_aavg(i)-(obs_nino4_amean_remove_smoothed(i)*smoothed_all_regression(0)+smoothed_all_constant))
      GPLOT,X=obs_nino4_amean(i),Y=hf_regression_residual-full_regression_residual,SIZE=100,SYM=6,/NOLINES,COL=4
   ENDELSE
ENDFOR
AXES,XSTEP=0.3,XMINOR=0.15,YSTEP=5,YMINOR=2.5,NDECS=2,YTITLE='Diff in ABS(regression residual) for Nov-Apr rain (mm; negative for better HF regression)',$
     ;XTITLE='21-year running mean of Annual mean Nino 4 (!Uo!NC)'
     XTITLE='Annual mean (May-April) Nino 4 (!Uo!NC)'
GPLOT,X=[0,0],Y=[-80,80],STYLE=1
GPLOT,X=[-1.5,1.5],Y=[0,0],STYLE=1
PSCLOSE,/NOVIEW

FOR i=0,N_ELEMENTS(elnino_years)-1 DO BEGIN
   IF elnino_years(i) lt obs_nino_count-11 THEN BEGIN
      IF i eq 0 THEN BEGIN
         early_elnino_years=[elnino_years(i)]
      ENDIF ELSE $
         early_elnino_years=[early_elnino_years,elnino_years(i)]
   ENDIF
ENDFOR
contribution_to_covariance_full=fltarr(N_ELEMENTS(early_elnino_years))
contribution_to_covariance_hf=fltarr(N_ELEMENTS(early_elnino_years))
smoothed_nino4_elnino_years=fltarr(N_ELEMENTS(early_elnino_years))

FOR i=0,N_ELEMENTS(early_elnino_years)-1 DO BEGIN
   contribution_to_covariance_full(i)=(silo_precip_qld_aavg(early_elnino_years(i))-MEAN(silo_precip_qld_aavg[early_elnino_years]))*$
                                      (obs_nino4_amean(early_elnino_years(i))-MEAN(obs_nino4_amean[early_elnino_years]))/$
                                      (N_ELEMENTS(early_elnino_years)-1)
   contribution_to_covariance_hf(i)=(silo_precip_qld_aavg(early_elnino_years(i))-MEAN(silo_precip_qld_aavg[early_elnino_years]))*$
                                    (obs_nino4_amean_remove_smoothed(early_elnino_years(i))-MEAN(obs_nino4_amean_remove_smoothed[early_elnino_years]))/$
                                    (N_ELEMENTS(early_elnino_years)-1)
   smoothed_nino4_elnino_years(i)=obs_nino4_smoothed(early_elnino_years(i))
ENDFOR
  
;ysteps=[100,50,75,50,50]
;FOR i=0,n_regions-1 DO BEGIN
;   this_region_aavg=REFORM(silo_precip_region_aavg(i,*))*180.
;   lanina_region_aavg=this_region_aavg[lanina_years]
;   weak_lanina_region_aavg=this_region_aavg[weak_lanina_years]
;   elnino_region_aavg=this_region_aavg[elnino_years]
;   lanina_regression=REGRESS(lanina_nino4,lanina_region_aavg,CONST=lanina_constant,CORRELATION=lanina_correlation)
;   weak_lanina_regression=REGRESS(weak_lanina_nino4,weak_lanina_region_aavg,CONST=weak_lanina_constant,CORRELATION=weak_lanina_correlation)
;   elnino_regression=REGRESS(elnino_nino4,elnino_region_aavg,CONST=elnino_constant,CORRELATION=elnino_correlation)
;   
;   psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo_scatterplot.obs_silo_hadisst.qld_region'+STRTRIM(STRING(i+1),1)+;'.ps'
;   ymin=FLOOR(MIN(this_region_aavg)*0.9)/100*100.
;   ymax=MAX(this_region_aavg)*1.1
;   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2500,SPACE2=500,XOFFSET=200,YOFFSET=0,TFONT=2,TCHARSIZE=100,SPACE3=600
;   GSET,XMIN=-2.5,XMAX=2,YMIN=ymin,YMAX=ymax,TITLE='Nino 4 (HadISST) against Queensland region '+STRTRIM(STRING(i+1),1)+' area-averaged rainfall (SILO 0.25) - 1900-20;;08'
 ;  red=FSC_COLOR("red",2)
 ;  blue=FSC_COLOR("blue",3)
 ;  GPLOT,X=obs_nino4_amean,Y=this_region_aavg,/NOLINES,SYM=6
 ;  AXES,XTITLE='Annual mean (May-April) Nino 4 index (!Uo!NC)',XSTEP=0.5,YTITLE='Annual total (May-April) Queensland region '+STRTRIM(STRING(i+1),1)+' area-averaged ra;infall (mm)',YSTEP=ysteps(i),NDECS=2
 ;  GPLOT,X=REPLICATE(0,ymax-ymin),Y=indgen(ymax-ymin)+ymin
 ;  GPLOT,X=findgen(140)*(-0.01)-0.10,Y=(findgen(150)*(-0.01)-0.10)*lanina_regression(0)+lanina_constant,THICK=150,COL=3
 ;  GPLOT,X=findgen(76)*(-0.01)-0.10,Y=(findgen(76)*(-0.01)-0.10)*weak_lanina_regression(0)+weak_lanina_constant,THICK=200,COL=3,STYLE=2
 ;  GPLOT,X=findgen(140)*0.01+0.10,Y=(findgen(150)*0.01+0.10)*elnino_regression(0)+elnino_constant,THICK=150,COL=2
 ;  GPLOT,X=-2.4,Y=ymin+7*ysteps(i),TEXT='For La Nina years (Nino 4 < -0.1) ',ALIGN=0.0,CHARSIZE=80
 ;  GPLOT,X=-2.4,Y=ymin+6*ysteps(i),TEXT='QLD_region'+STRTRIM(STRING(i+1),1)+'_rain = '+STRMID(STRTRIM(STRING(lanina_regression(0)),1),0,7)+' * nino4 + '+$
 ;        STRMID(STRTRIM(STRING(lanina_constant),1),0,6),ALIGN=0.0,CHARSIZE=80
 ;  GPLOT,X=-2.4,Y=ymin+5*ysteps(i),TEXT='Correlation = '+STRMID(STRTRIM(STRING(lanina_correlation),1),0,5),ALIGN=0.0,CHARSIZE=80
 ;  
 ;  GPLOT,X=-2.4,Y=ymin+3*ysteps(i),TEXT='For La Nina years excl. top 10% (-0.86 < Nino 4 < -0.1) ',ALIGN=0.0,CHARSIZE=80
 ;  GPLOT,X=-2.4,Y=ymin+2*ysteps(i),TEXT='QLD_region'+STRTRIM(STRING(i+1),1)+'_rain = '+STRMID(STRTRIM(STRING(weak_lanina_regression(0)),1),0,7)+' * nino4 + '+$
 ;        STRMID(STRTRIM(STRING(weak_lanina_constant),1),0,6),ALIGN=0.0,CHARSIZE=80
 ;  GPLOT,X=-2.4,Y=ymin+ysteps(i),TEXT='Correlation = '+STRMID(STRTRIM(STRING(weak_lanina_correlation),1),0,5),ALIGN=0.0,CHARSIZE=80
;
;   GPLOT,X=0.2,Y=ymax-2*ysteps(i),TEXT='For El Nino years (Nino 4 > 0.1) ',ALIGN=0.0,CHARSIZE=80
;   GPLOT,X=0.2,Y=ymax-3*ysteps(i),TEXT='QLD_region'+STRTRIM(STRING(i+1),1)+'_rain = '+STRMID(STRTRIM(STRING(elnino_regression(0)),1),0,7)+' * nino4 + '+$
;         STRMID(STRTRIM(STRING(elnino_constant),1),0,6),ALIGN=0.0,CHARSIZE=80
;   GPLOT,X=0.2,Y=ymax-4*ysteps(i),TEXT='Correlation = '+STRMID(STRTRIM(STRING(elnino_correlation),1),0,5),ALIGN=0.0,CHARSIZE=80
;   PSCLOSE,/NOVIEW
;ENDFOR

; Calculations for HiGEM
xbylr_lanina_years=where(higem_xbylr_nino4_amean lt -0.1)
xbylr_weak_lanina_years=where(higem_xbylr_nino4_amean lt -0.1 and higem_xbylr_nino4_amean gt -0.83)
xbylr_elnino_years=where(higem_xbylr_nino4_amean gt 0.1)
xbylr_lanina_nino4=higem_xbylr_nino4_amean[xbylr_lanina_years]
xbylr_weak_lanina_nino4=higem_xbylr_nino4_amean[xbylr_weak_lanina_years]
xbylr_elnino_nino4=higem_xbylr_nino4_amean[xbylr_elnino_years]
xbylr_lanina_qld_aavg=higem_xbylr_precip_qld_aavg[xbylr_lanina_years]
xbylr_weak_lanina_qld_aavg=higem_xbylr_precip_qld_aavg[xbylr_weak_lanina_years]
xbylr_elnino_qld_aavg=higem_xbylr_precip_qld_aavg[xbylr_elnino_years]
xbylr_lanina_regression=REGRESS(xbylr_lanina_nino4,xbylr_lanina_qld_aavg,CONST=xbylr_lanina_constant,CORRELATION=xbylr_lanina_correlation)
xbylr_weak_lanina_regression=REGRESS(xbylr_weak_lanina_nino4,xbylr_weak_lanina_qld_aavg,CONST=xbylr_weak_lanina_constant,CORRELATION=xbylr_weak_lanina_correlation)
xbylr_elnino_regression=REGRESS(xbylr_elnino_nino4,xbylr_elnino_qld_aavg,CONST=xbylr_elnino_constant,CORRELATION=xbylr_elnino_correlation)

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo_scatterplot.higem_xbylr.all_qld.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2500,SPACE2=500,XOFFSET=200,YOFFSET=0,TFONT=2,TCHARSIZE=100,SPACE3=600
GSET,XMIN=-2.5,XMAX=2,YMIN=200,YMAX=900,TITLE='Nino 4 against Queensland area-averaged rainfall for HiGEM xbylr - '+$
     STRTRIM(STRING(higem_xbylr_nyears),1)+' years'
red=FSC_COLOR("red",2)
blue=FSC_COLOR("blue",3)
GPLOT,X=higem_xbylr_nino4_amean,Y=higem_xbylr_precip_qld_aavg,/NOLINES,SYM=6
AXES,XTITLE='Annual mean (May-April) Nino 4 index (!Uo!NC)',XSTEP=0.5,YTITLE='Annual total (May-April) Queensland area-averaged rainfall (mm)',YSTEP=50,NDECS=2
GPLOT,X=REPLICATE(0,8),Y=indgen(8)*100+200
GPLOT,X=findgen(140)*(-0.01)-0.10,Y=(findgen(150)*(-0.01)-0.10)*xbylr_lanina_regression(0)+xbylr_lanina_constant,THICK=150,COL=3
GPLOT,X=findgen(73)*(-0.01)-0.10,Y=(findgen(73)*(-0.01)-0.10)*xbylr_weak_lanina_regression(0)+xbylr_weak_lanina_constant,THICK=200,COL=3,STYLE=2
GPLOT,X=findgen(140)*0.01+0.10,Y=(findgen(150)*0.01+0.10)*xbylr_elnino_regression(0)+xbylr_elnino_constant,THICK=150,COL=2
GPLOT,X=-2.2,Y=470,TEXT='For La Nina years (Nino 4 < -0.2) ',ALIGN=0.0
GPLOT,X=-2.2,Y=430,TEXT='QLD_rain = '+STRMID(STRTRIM(STRING(xbylr_lanina_regression(0)),1),0,7)+' * nino4 + '+$
      STRMID(STRTRIM(STRING(xbylr_lanina_constant),1),0,6),ALIGN=0.0
GPLOT,X=-2.2,Y=390,TEXT='Correlation = '+STRMID(STRTRIM(STRING(xbylr_lanina_correlation),1),0,5),ALIGN=0.0

GPLOT,X=-2.2,Y=320,TEXT='For La Nina years excl. top 10% (-0.83 < Nino 4 < -0.2) ',ALIGN=0.0
GPLOT,X=-2.2,Y=280,TEXT='QLD_rain = '+STRMID(STRTRIM(STRING(xbylr_weak_lanina_regression(0)),1),0,7)+' * nino4 + '+$
      STRMID(STRTRIM(STRING(xbylr_weak_lanina_constant),1),0,6),ALIGN=0.0
GPLOT,X=-2.2,Y=240,TEXT='Correlation = '+STRMID(STRTRIM(STRING(xbylr_weak_lanina_correlation),1),0,5),ALIGN=0.0

GPLOT,X=0.5,Y=850,TEXT='For El Nino years (Nino 4 > 0.2) ',ALIGN=0.0
GPLOT,X=0.5,Y=800,TEXT='QLD_rain = '+STRMID(STRTRIM(STRING(xbylr_elnino_regression(0)),1),0,7)+' * nino4 + '+$
      STRMID(STRTRIM(STRING(xbylr_elnino_constant),1),0,6),ALIGN=0.0
GPLOT,X=0.5,Y=750,TEXT='Correlation = '+STRMID(STRTRIM(STRING(xbylr_elnino_correlation),1),0,5),ALIGN=0.0
PSCLOSE

eafeb_lanina_years=where(higem_eafeb_nino4_amean lt -0.3)
eafeb_weak_lanina_years=where(higem_eafeb_nino4_amean lt -0.3 and higem_eafeb_nino4_amean gt -1.4)
eafeb_elnino_years=where(higem_eafeb_nino4_amean gt 0.3)
eafeb_lanina_nino4=higem_eafeb_nino4_amean[eafeb_lanina_years]
eafeb_weak_lanina_nino4=higem_eafeb_nino4_amean[eafeb_weak_lanina_years]
eafeb_elnino_nino4=higem_eafeb_nino4_amean[eafeb_elnino_years]
eafeb_lanina_qld_aavg=higem_eafeb_precip_qld_aavg[eafeb_lanina_years]
eafeb_weak_lanina_qld_aavg=higem_eafeb_precip_qld_aavg[eafeb_weak_lanina_years]
eafeb_elnino_qld_aavg=higem_eafeb_precip_qld_aavg[eafeb_elnino_years]
eafeb_lanina_regression=REGRESS(eafeb_lanina_nino4,eafeb_lanina_qld_aavg,CONST=eafeb_lanina_constant,CORRELATION=eafeb_lanina_correlation)
eafeb_weak_lanina_regression=REGRESS(eafeb_weak_lanina_nino4,eafeb_weak_lanina_qld_aavg,CONST=eafeb_weak_lanina_constant,CORRELATION=eafeb_weak_lanina_correlation)
eafeb_elnino_regression=REGRESS(eafeb_elnino_nino4,eafeb_elnino_qld_aavg,CONST=eafeb_elnino_constant,CORRELATION=eafeb_elnino_correlation)

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo_scatterplot.higem_eafeb.all_qld.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2500,SPACE2=500,XOFFSET=200,YOFFSET=2000,TFONT=2,TCHARSIZE=100,SPACE3=600
GSET,XMIN=-2.5,XMAX=2,YMIN=200,YMAX=1000,TITLE='Nino 4 against Queensland area-averaged rainfall for HiGEM eafeb - '+$
     STRTRIM(STRING(higem_eafeb_nyears),1)+' years'
red=FSC_COLOR("red",2)
blue=FSC_COLOR("blue",3)
GPLOT,X=higem_eafeb_nino4_amean,Y=higem_eafeb_precip_qld_aavg,/NOLINES,SYM=6
AXES,XTITLE='Annual mean (May-April) Nino 4 index (!Uo!NC)',XSTEP=0.5,YTITLE='Annual total (May-April) Queensland area-averaged rainfall (mm)',YSTEP=50,NDECS=2
GPLOT,X=REPLICATE(0,9),Y=indgen(9)*100+200
GPLOT,X=[-2.4,-0.3],Y=[-2.4,-0.3]*eafeb_lanina_regression(0)+eafeb_lanina_constant,THICK=150,COL=3
GPLOT,X=[-1.0,-0.3],Y=[-1.0,-0.3]*eafeb_weak_lanina_regression(0)+eafeb_weak_lanina_constant,THICK=200,COL=3,STYLE=2
GPLOT,X=[0.3,1.0],Y=[0.3,1.0]*eafeb_elnino_regression(0)+eafeb_elnino_constant,THICK=150,COL=2
GPLOT,X=-2.2,Y=470,TEXT='For La Nina years (Nino 4 < -0.3) ',ALIGN=0.0
GPLOT,X=-2.2,Y=430,TEXT='QLD_rain = '+STRMID(STRTRIM(STRING(eafeb_lanina_regression(0)),1),0,7)+' * nino4 + '+$
      STRMID(STRTRIM(STRING(eafeb_lanina_constant),1),0,6),ALIGN=0.0
GPLOT,X=-2.2,Y=390,TEXT='Correlation = '+STRMID(STRTRIM(STRING(eafeb_lanina_correlation),1),0,5),ALIGN=0.0

GPLOT,X=-2.2,Y=320,TEXT='For La Nina years excl. top 10% (-1.01 < Nino 4 < -0.3) ',ALIGN=0.0
GPLOT,X=-2.2,Y=280,TEXT='QLD_rain = '+STRMID(STRTRIM(STRING(eafeb_weak_lanina_regression(0)),1),0,7)+' * nino4 + '+$
      STRMID(STRTRIM(STRING(eafeb_weak_lanina_constant),1),0,6),ALIGN=0.0
GPLOT,X=-2.2,Y=240,TEXT='Correlation = '+STRMID(STRTRIM(STRING(eafeb_weak_lanina_correlation),1),0,5),ALIGN=0.0

GPLOT,X=0.5,Y=850,TEXT='For El Nino years (Nino 4 > 0.3) ',ALIGN=0.0
GPLOT,X=0.5,Y=810,TEXT='QLD_rain = '+STRMID(STRTRIM(STRING(eafeb_elnino_regression(0)),1),0,7)+' * nino4 + '+$
      STRMID(STRTRIM(STRING(eafeb_elnino_constant),1),0,6),ALIGN=0.0
GPLOT,X=0.5,Y=770,TEXT='Correlation = '+STRMID(STRTRIM(STRING(eafeb_elnino_correlation),1),0,5),ALIGN=0.0
PSCLOSE

STOP

END
