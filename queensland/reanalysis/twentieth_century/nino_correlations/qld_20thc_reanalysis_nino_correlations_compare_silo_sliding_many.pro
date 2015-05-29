PRO qld_20thc_reanalysis_nino_correlations_compare_silo_sliding_many

; Use sliding windows of many lengths to compare annual or
; seasonal-mean correlations between NINO indices and area-averaged
; Queensland rainfall.

; Timeseries of observed NINO values
obs_nino_infile='/home/ss901165/datasets/NINO/nino4_hadisst.may-apr_ameans.1871-2008.nc'
obs_nino_start_year=1871
obs_nino_end_year=2008
obs_nino_nyears=obs_nino_end_year-obs_nino_start_year+1

; Timeseries of Australian rainfall from SILO on T62 grid,
; area-averaged by region
silo_precip_infile='/home/ss901165/datasets_mango/SILO/t62/SILO_precip.may-apr_ameans.1891-2007.t62.qld_region_aavg.nc'
silo_start_year=1891
silo_end_year=2007
silo_nyears=silo_end_year-silo_start_year+1

; Timeseries of Australian rainfall from 20th Century V2 reanalysis, area-averaged by region
twentyc_precip_infile='/home/ss901165/datasets_mango/20THC_REANALYSIS/precip/20thc_reanalysis.may-apr_ameans.1891-2007.precip.qld_region_aavg.nc'
twentyc_start_year=1891
twentyc_end_year=2007
twentyc_nyears=twentyc_end_year-twentyc_start_year+1

; Read NINO indices
obs_nino_offset=(silo_start_year-obs_nino_start_year)
obs_nino_count=silo_nyears
obs_nino4_amean=OPEN_AND_EXTRACT(obs_nino_infile,'NINO4',offset=[obs_nino_offset],count=[obs_nino_count])

n_regions=5

; Read rainfall
twentyc_precip_region_aavg=OPEN_AND_EXTRACT(twentyc_precip_infile,'rain_aavg_region',$
                                         offset=[0,0],count=[n_regions,twentyc_nyears])*86400.
twentyc_precip_qld_aavg=OPEN_AND_EXTRACT(twentyc_precip_infile,'rain_aavg_allqld',$
                                      offset=[0],count=[twentyc_nyears])*86400.
silo_precip_region_aavg=OPEN_AND_EXTRACT(silo_precip_infile,'rain_aavg_region',$
                                         offset=[0,0],count=[n_regions,twentyc_nyears])
silo_precip_qld_aavg=OPEN_AND_EXTRACT(silo_precip_infile,'rain_aavg_allqld',$
                                      offset=[0],count=[twentyc_nyears])
  
windows=[10,20,30,40,60,80,100]
n_windows=N_ELEMENTS(windows)
windows_strings=strarr(n_windows)
FOR i=0,n_windows-1 DO $
   windows_strings(i)='Using '+STRTRIM(STRING(windows(i)+1),1)+'-year centred window'
windows_strings_short=strarr(n_windows)
FOR i=0,n_windows-1 DO $
   windows_strings_short(i)=STRTRIM(STRING(windows(i)+1),1)+' years'

obs_corr_windows=fltarr(n_windows,silo_nyears)
obs_ninovar_windows=fltarr(n_windows,silo_nyears)
obs_precipvar_windows=fltarr(n_windows,silo_nyears)
obs_corrvar_windows=fltarr(n_windows,silo_nyears)
twentyc_corr_windows=fltarr(n_windows,twentyc_nyears)
twentyc_ninovar_windows=fltarr(n_windows,twentyc_nyears)
twentyc_precipvar_windows=fltarr(n_windows,twentyc_nyears)
twentyc_corrvar_windows=fltarr(n_windows,twentyc_nyears)

FOR i=0,n_windows-1 DO BEGIN
   FOR j=windows(i)/2,silo_nyears-1-windows(i)/2 DO BEGIN
      obs_corr_windows(i,j)=CORRELATE(obs_nino4_amean(j-windows(i)/2:windows(i)/2+j),$
                                      silo_precip_qld_aavg(j-windows(i)/2:windows(i)/2+j)) 
      obs_ninovar_windows(i,j)=STDDEV(obs_nino4_amean(j-windows(i)/2:windows(i)/2+j))
      obs_precipvar_windows(i,j)=STDDEV(silo_precip_qld_aavg(j-windows(i)/2:windows(i)/2+j))
   ENDFOR
;   FOR j=windows(i),silo_nyears-1-windows(i) DO $
;      obs_corrvar_windows(i,j)=CORRELATE(obs_ninovar_windows(i,j-windows(i)/2:windows(i)/2+j),$
;                                         silo_precip_qld_aavg(j-windows(i)/2:windows(i)/2+j))
   FOR j=windows(i),silo_nyears-1-windows(i) DO $
      obs_corrvar_windows(i,j)=CORRELATE(obs_ninovar_windows(i,j-windows(i)/2:windows(i)/2+j),$
                                         obs_precipvar_windows(i,j-windows(i)/2:windows(i)/2+j))
   FOR j=windows(i)/2,twentyc_nyears-1-windows(i)/2 DO BEGIN
      twentyc_corr_windows(i,j)=CORRELATE(obs_nino4_amean(j-windows(i)/2:windows(i)/2+j),$
                                        twentyc_precip_qld_aavg(j-windows(i)/2:windows(i)/2+j))
      twentyc_ninovar_windows(i,j)=STDDEV(obs_nino4_amean(j-windows(i)/2:windows(i)/2+j))
      twentyc_precipvar_windows(i,j)=STDDEV(twentyc_precip_qld_aavg(j-windows(i)/2:windows(i)/2+j))
   ENDFOR
   FOR j=windows(i),twentyc_nyears-1-windows(i) DO $
      twentyc_corrvar_windows(i,j)=CORRELATE(twentyc_ninovar_windows(i,j-windows(i)/2:windows(i)/2+j),$
                                           twentyc_precipvar_windows(i,j-windows(i)/2:windows(i)/2+j))
ENDFOR

psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/nino_correlations/qld_20thc_reanalysis_nino_correlations_compare_silo_sliding_many.qld_region.annmean_may-apr_nino4.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=200,XOFFSET=900,YOFFSET=1200,TFONT=2,TCHARSIZE=80,SPACE3=300,YPLOTS=3,SPACING=1500

POS,YPOS=3
CS,SCALE=26,NCOLS=n_windows,/REV
black=FSC_COLOR("black",1+n_windows)
GSET,XMIN=1891,XMAX=1891+silo_nyears,YMIN=-1.0,YMAX=0.7
AXES,XSTEP=10,YSTEP=0.2,XTITLE='Year in SILO dataset (1891-2007)',YTITLE='Corr QLD rain with Nino 4',NDECS=2,/NOUPPER
FOR i=0,n_windows-1 DO $
   GPLOT,X=indgen(silo_nyears-windows(i))+windows(i)/2+1891,$
   Y=REFORM(obs_corr_windows(i,windows(i)/2:silo_nyears-1-windows(i)/2)),COL=i+2,THICK=150
LEGEND,labels=REVERSE(windows_strings),COL=REVERSE(indgen(n_windows)+2),LEGXOFFSET=-2000,LEGYOFFSET=7000

POS,YPOS=2
CS,SCALE=26,NCOLS=n_windows,/REV
black=FSC_COLOR("black",1+n_windows)
GSET,XMIN=1891,XMAX=1891+silo_nyears,YMIN=-1.0,YMAX=0.7
AXES,XSTEP=10,YSTEP=0.2,XTITLE='Year in 20th Century Reanalysis (v2, 1891-2007)',YTITLE='Corr QLD rain with Nino 4',NDECS=2,/NOUPPER
FOR i=0,n_windows-1 DO $
   GPLOT,X=indgen(twentyc_nyears-windows(i))+windows(i)/2+1891,$
   Y=REFORM(twentyc_corr_windows(i,windows(i)/2:twentyc_nyears-1-windows(i)/2)),COL=i+2,THICK=150

POS,YPOS=1
CS,SCALE=26,NCOLS=n_windows,/REV
black=FSC_COLOR("black",1+n_windows)
GSET,XMIN=1891,XMAX=1891+silo_nyears,YMIN=-0.2,YMAX=0.9
AXES,XSTEP=10,YSTEP=0.2,XTITLE='Year',YTITLE='Diff in corr (20C - SILO)',NDECS=2,/NOUPPER,/HGRID,GSTYLE=1
FOR i=0,n_windows-1 DO $
   GPLOT,X=indgen(twentyc_nyears-windows(i))+windows(i)/2+1891,$
   Y=REFORM(twentyc_corr_windows(i,windows(i)/2:twentyc_nyears-1-windows(i)/2))-REFORM(obs_corr_windows(i,windows(i)/2:silo_nyears-1-windows(i)/2)),COL=i+2,THICK=150
GPLOT,X=indgen(twentyc_nyears)+1891,Y=REPLICATE(0,twentyc_nyears),STYLE=1,THICK=200

GPLOT,X=1950,Y=0.7,TEXT='Correlation of 11-year windowed correlations: '+STRMID(STRTRIM(STRING(CORRELATE(twentyc_corr_windows(0,*),obs_corr_windows(0,*))),1),0,4),ALIGN=0.0

PSCLOSE

psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/nino_correlations/qld_20thc_reanalysis_nino_correlations_compare_silo_sliding_many.qld_region.annmean_may-apr_nino4_var.silo_hadisst.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2500,SPACE2=200,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=80,SPACE3=300,YPLOTS=3,SPACING=800

POS,YPOS=3
CS,SCALE=26,NCOLS=n_windows,/REV
black=FSC_COLOR("black",1+n_windows)
GSET,XMIN=1891,XMAX=1891+silo_nyears,YMIN=-1.0,YMAX=0.8
AXES,XSTEP=10,YSTEP=0.2,XTITLE='Year in SILO dataset (1891-2007)',YTITLE='Corr QLD rain with Nino 4',NDECS=2,/NOUPPER,/NORIGHT
FOR i=0,n_windows-1 DO $
   GPLOT,X=1891+indgen(silo_nyears-windows(i))+windows(i)/2,$
   Y=REFORM(obs_corr_windows(i,windows(i)/2:silo_nyears-1-windows(i)/2)),COL=i+2,THICK=150
LEGEND,labels=REVERSE(windows_strings),COL=REVERSE(indgen(n_windows)+2),LEGXOFFSET=1500,LEGYOFFSET=7000

POS,YPOS=2

GSET,XMIN=1891,XMAX=1891+silo_nyears,YMIN=0,YMAX=0.7
FOR i=0,n_windows-1 DO $
   GPLOT,X=1891+indgen(silo_nyears-windows(i))+windows(i)/2,$
   Y=REFORM(obs_ninovar_windows(i,windows(i)/2:silo_nyears-1-windows(i)/2)),COL=i+2,THICK=150
AXES,XSTEP=10,/ONLYLEFT,YTITLE='Stdev. Nino 4',YVALS=['0.10','0.20','0.30','0.40','0.50','0.60']

GSET,XMIN=1891,XMAX=1891+silo_nyears,YMIN=0.0,YMAX=0.8
FOR i=0,n_windows-1 DO $   
   GPLOT,X=1891+indgen(silo_nyears-windows(i))+windows(i)/2,$
   Y=REFORM(obs_precipvar_windows(i,windows(i)/2:silo_nyears-1-windows(i)/2)),COL=i+2,THICK=150,STYLE=2
AXES,XSTEP=10,YSTEP=0.10,YTITLE='Stdev. May-Apr rain (mm year!U-1!N)',NDECS=2,/ONLYRIGHT
AXES,YSTEP=0.10,/NOLEFT,/NORIGHT,XSTEP=10,/NOUPPER

POS,YPOS=1
GSET,XMIN=0,XMAX=n_windows,YMIN=-1,YMAX=1.5
FOR i=0,n_windows-1 DO BEGIN
   GPLOT,X=i+0.2,Y=CORRELATE(obs_precipvar_windows(i,windows(i)/2:silo_nyears-1-windows(i)/2),$
                             obs_corr_windows(i,windows(i)/2:silo_nyears-1-windows(i)/2)),$
         COL=i+2,THICK=150,SYM=6
   GPLOT,X=i+0.4,Y=CORRELATE(obs_precipvar_windows(i,windows(i)/2:silo_nyears-1-windows(i)/2),$
                             obs_ninovar_windows(i,windows(i)/2:silo_nyears-1-windows(i)/2)),$
         COL=i+2,THICK=150,SYM=3
ENDFOR
GPLOT,X=indgen(n_windows+1),Y=REPLICATE(0,n_windows+1),STYLE=1
AXES,XVALS=indgen(n_windows)+0.5,XLABELS=windows_strings_short,YVALS=['-1.00','-0.75','-0.50','-0.25','0.00','0.25','0.50','0.75','1.00'],$
     NDECS=2,/NORIGHT,/NOUPPER,$
     XTITLE='Length of window',YTITLE='Correlation coefficient'

GSET,XMIN=0,XMAX=n_windows,YMIN=0,YMAX=0.4
FOR i=0,n_windows-1 DO BEGIN
   GPLOT,X=i+0.6,Y=STDDEV(obs_corr_windows(i,windows(i)/2:silo_nyears-1-windows(i)/2)),COL=i+2,THICK=150,SYM=4
   GPLOT,X=i+0.8,Y=STDDEV(obs_precipvar_windows(i,windows(i)/2:silo_nyears-1-windows(i)/2)),COL=i+2,THICK=150,SYM=5
ENDFOR

AXES,XVALS=indgen(n_windows)+0.5,YVALS=['0.00','0.10','0.20','0.30','0.40'],YTITLE='Standard deviation',/ONLYRIGHT,NDECS=2
LEGEND,labels=REVERSE(['Correlation of windowed corr with NINO4 and std.dev. of rain (left)','Correlation of windowed std.dev. of rain and std.dev. of NINO4','Std.dev. of windowed correlation (right)','Std.dev. of windowed std.dev. of rain (right)']),$
       SYM=REVERSE([6,3,4,5]),LENGTH=0,LEGXOFFSET=-7000,LEGYOFFSET=17500
PSCLOSE

psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/nino_correlations/qld_20thc_reanalysis_nino_correlations_compare_silo_sliding_many.qld_region.annmean_may-apr_nino4_var.twentyc.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2500,SPACE2=200,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=80,SPACE3=300,YPLOTS=3,SPACING=800

POS,YPOS=3
CS,SCALE=26,NCOLS=n_windows,/REV
black=FSC_COLOR("black",1+n_windows)
GSET,XMIN=1891,XMAX=1891+twentyc_nyears,YMIN=-1.0,YMAX=0.8
AXES,XSTEP=10,YSTEP=0.2,XTITLE='Year in 20th Century Reanalysis (V2, ensemble-mean)',YTITLE='Corr QLD rain with Nino 4',NDECS=2,/NOUPPER,/NORIGHT
FOR i=0,n_windows-1 DO $
   GPLOT,X=1891+indgen(twentyc_nyears-windows(i))+windows(i)/2,$
   Y=REFORM(twentyc_corr_windows(i,windows(i)/2:twentyc_nyears-1-windows(i)/2)),COL=i+2,THICK=150
LEGEND,labels=REVERSE(windows_strings),COL=REVERSE(indgen(n_windows)+2),LEGXOFFSET=1500,LEGYOFFSET=7500

POS,YPOS=2
GSET,XMIN=1891,XMAX=1891+twentyc_nyears,YMIN=0,YMAX=0.7
FOR i=0,n_windows-1 DO $
   GPLOT,X=1891+indgen(twentyc_nyears-windows(i))+windows(i)/2,$
   Y=REFORM(twentyc_ninovar_windows(i,windows(i)/2:twentyc_nyears-1-windows(i)/2)),COL=i+2,THICK=150
AXES,XSTEP=10,/ONLYLEFT,YTITLE='Stdev. Nino 4',YVALS=['0.10','0.20','0.30','0.40','0.50','0.60']

GSET,XMIN=1891,XMAX=1891+twentyc_nyears,YMIN=0,YMAX=0.8
FOR i=0,n_windows-1 DO $   
   GPLOT,X=1891+indgen(twentyc_nyears-windows(i))+windows(i)/2,$
   Y=REFORM(twentyc_precipvar_windows(i,windows(i)/2:twentyc_nyears-1-windows(i)/2)),COL=i+2,THICK=150,STYLE=2
AXES,XSTEP=10,YSTEP=0.10,YTITLE='Stdev. May-Apr rain',NDECS=2,/ONLYRIGHT
AXES,YSTEP=0.10,/NOLEFT,/NORIGHT,XSTEP=10,/NOUPPER

POS,YPOS=1
GSET,XMIN=0,XMAX=n_windows,YMIN=-1,YMAX=1.5
FOR i=0,n_windows-1 DO BEGIN
   GPLOT,X=i+0.2,Y=CORRELATE(twentyc_precipvar_windows(i,windows(i)/2:twentyc_nyears-1-windows(i)/2),$
                             twentyc_corr_windows(i,windows(i)/2:twentyc_nyears-1-windows(i)/2)),$
         COL=i+2,THICK=150,SYM=6
   GPLOT,X=i+0.4,Y=CORRELATE(twentyc_precipvar_windows(i,windows(i)/2:twentyc_nyears-1-windows(i)/2),$
                             twentyc_ninovar_windows(i,windows(i)/2:twentyc_nyears-1-windows(i)/2)),$
         COL=i+2,THICK=150,SYM=3
ENDFOR
GPLOT,X=indgen(n_windows+1),Y=REPLICATE(0,n_windows+1),STYLE=1
AXES,XVALS=indgen(n_windows)+0.5,XLABELS=windows_strings_short,YVALS=['-1.00','-0.75','-0.50','-0.25','0.00','0.25','0.50','0.75','1.00'],$
     NDECS=2,/NORIGHT,/NOUPPER,$
     XTITLE='Length of window',YTITLE='Correlation coefficient'

GSET,XMIN=0,XMAX=n_windows,YMIN=0,YMAX=0.4
FOR i=0,n_windows-1 DO BEGIN
   GPLOT,X=i+0.6,Y=STDDEV(twentyc_corr_windows(i,windows(i)/2:twentyc_nyears-1-windows(i)/2)),COL=i+2,THICK=150,SYM=4
   GPLOT,X=i+0.8,Y=STDDEV(twentyc_precipvar_windows(i,windows(i)/2:twentyc_nyears-1-windows(i)/2)),COL=i+2,THICK=150,SYM=5
ENDFOR

AXES,XVALS=indgen(n_windows)+0.5,YVALS=['0.00','0.10','0.20','0.30','0.40'],YTITLE='Standard deviation',/ONLYRIGHT,NDECS=2
LEGEND,labels=REVERSE(['Correlation of windowed corr with NINO4 and std.dev. of rain (left)','Correlation of windowed std.dev. of rain and std.dev. of NINO4','Std.dev. of windowed correlation (right)','Std.dev. of windowed std.dev. of rain (right)']),$
       SYM=REVERSE([6,3,4,5]),LENGTH=0,LEGXOFFSET=-7000,LEGYOFFSET=17500
PSCLOSE

STOP

END



