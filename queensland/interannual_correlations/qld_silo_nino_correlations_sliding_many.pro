PRO qld_silo_nino_correlations_sliding_many

; Use sliding windows of many lengths to compare annual or
; seasonal-mean correlations between NINO indices and area-averaged
; Queensland rainfall.

; Timeseries of observed NINO values
obs_nino_infile='/home/ss901165/datasets/NINO/nino4_hadisst.may-apr_ameans.1871-2008.nc'
obs_nino_start_year=1871
obs_nino_end_year=2008
obs_nino_nyears=obs_nino_end_year-obs_nino_start_year+1

; Timeseries of Australian rainfall from SILO
silo_precip_infile='/home/ss901165/datasets_mango/SILO/n144/SILO_precip.may-apr_ameans.1900-2007.n144.nc'
silo_start_year=1900
silo_end_year=2007
silo_nyears=silo_end_year-silo_start_year+1

mask_infile='/home/ss901165/um_output/mask_n144_higam.nc'

qld_box=[-10,138,-29,154]
; Read latitudes and longitudes, restrict to box of interest
silo_longitude=OPEN_AND_EXTRACT(silo_precip_infile,'longitude')
silo_latitude=OPEN_AND_EXTRACT(silo_precip_infile,'latitude')
DEFINE_BOUNDARIES,qld_box,silo_latitude,silo_longitude,silo_box_tx,/LIMIT
silo_nlon=N_ELEMENTS(silo_longitude)
silo_nlat=N_ELEMENTS(silo_latitude)

; Read NINO indices
obs_nino_offset=(silo_start_year-obs_nino_start_year)
obs_nino_count=silo_nyears
obs_nino4_amean=OPEN_AND_EXTRACT(obs_nino_infile,'NINO4',offset=[obs_nino_offset],count=[obs_nino_count])
obs_nino4_amean=obs_nino4_amean-MEAN(obs_nino4_amean)

; Read rainfall
silo_precip_amean=REFORM(OPEN_AND_EXTRACT(silo_precip_infile,'rain',$
                                          offset=[silo_box_tx(1),silo_box_tx(0),0],$
                                          count=[silo_nlon,silo_nlat,silo_nyears]))
; Read land/sea mask
mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
DEFINE_BOUNDARIES,qld_box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)
mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                             offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                             count=[mask_nlon,mask_nlat,1,1]))
mask_latrev=fltarr(mask_nlon,mask_nlat)
FOR i=0,mask_nlat-1 DO $
   mask_latrev(*,i)=mask(*,mask_nlat-i-1)

; Calculate area-weighted mean rainfall
n144_weights_latrev=fltarr(silo_nlat)
FOR i=0,silo_nlat-1 DO BEGIN
   n144_weights_latrev(i)=COS(3.14159*silo_latitude(i)/180.)
ENDFOR
n144_weights_latrev=n144_weights_latrev/TOTAL(n144_weights_latrev)

silo_precip_amean_aavg=fltarr(silo_nyears)
FOR i=0,silo_nyears-1 DO BEGIN
   thisyear_masked=REFORM(silo_precip_amean(*,*,i))
   thisyear_masked[where(mask_latrev lt 1 or thisyear_masked ge 1E20)]=!Values.F_NaN
   FOR j=0,silo_nlon-1 DO $
      silo_precip_amean_aavg(i)=silo_precip_amean_aavg(i)+$
      TOTAL(n144_weights_latrev*REFORM(thisyear_masked(j,*)),/NaN)*1./FLOAT(silo_nlon)
ENDFOR
  
windows=[10,20,30,50,70]
n_windows=N_ELEMENTS(windows)
windows_strings=strarr(n_windows)
FOR i=0,n_windows-1 DO $
   windows_strings(i)='Using a '+STRTRIM(STRING(windows(i)+1),1)+'-year centred window'
windows_strings_short=strarr(n_windows)
FOR i=0,n_windows-1 DO $
   windows_strings_short(i)=STRTRIM(STRING(windows(i)+1),1)+' years'

obs_corr_windows=fltarr(n_windows,silo_nyears)
obs_nino_mean_windows=fltarr(n_windows,silo_nyears)
obs_ninovar_windows=fltarr(n_windows,silo_nyears)
obs_precipvar_windows=fltarr(n_windows,silo_nyears)
obs_corrvar_windows=fltarr(n_windows,silo_nyears)

FOR i=0,n_windows-1 DO BEGIN
   FOR j=windows(i)/2,silo_nyears-1-windows(i)/2 DO BEGIN
      obs_corr_windows(i,j)=CORRELATE(obs_nino4_amean(j-windows(i)/2:windows(i)/2+j),$
                                      silo_precip_amean_aavg(j-windows(i)/2:windows(i)/2+j)) 
      obs_ninovar_windows(i,j)=STDDEV(obs_nino4_amean(j-windows(i)/2:windows(i)/2+j))
      obs_nino_mean_windows(i,j)=MEAN(obs_nino4_amean(j-windows(i)/2:windows(i)/2+j))
      obs_precipvar_windows(i,j)=STDDEV(silo_precip_amean_aavg(j-windows(i)/2:windows(i)/2+j))
   ENDFOR
;   FOR j=windows(i),silo_nyears-1-windows(i) DO $
;      obs_corrvar_windows(i,j)=CORRELATE(obs_ninovar_windows(i,j-windows(i)/2:windows(i)/2+j),$
;                                         silo_precip_amean_aavg(j-windows(i)/2:windows(i)/2+j))
   FOR j=windows(i),silo_nyears-1-windows(i) DO $
      obs_corrvar_windows(i,j)=CORRELATE(obs_ninovar_windows(i,j-windows(i)/2:windows(i)/2+j),$
                                         obs_precipvar_windows(i,j-windows(i)/2:windows(i)/2+j))
ENDFOR

psfile='/home/ss901165/idl/queensland/interannual_correlations/qld_silo_nino_correlations_silo_sliding_many.qld_region.annmean_may-apr_nino4.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2500,SPACE2=200,XOFFSET=1500,YOFFSET=200,TFONT=2,TCHARSIZE=80,SPACE3=300,YPLOTS=2,SPACING=2000
signif_levels=[0.602,0.433,0.355,0.304,0.273]

POS,YPOS=2
CS,SCALE=26,NCOLS=n_windows,/REV
black=FSC_COLOR("black",1+n_windows)
GSET,XMIN=1900,XMAX=1900+silo_nyears,YMIN=-1.0,YMAX=0.4
AXES,XSTEP=10,YSTEP=0.2,XTITLE='Year',YTITLE='Correlation coefficient',NDECS=2,/NOUPPER
FOR i=0,n_windows-1 DO BEGIN
   GPLOT,X=1900+indgen(silo_nyears-windows(i))+windows(i)/2,$
         Y=REFORM(obs_corr_windows(i,windows(i)/2:silo_nyears-1-windows(i)/2)),COL=i+2,THICK=150
   this_window=REFORM(obs_corr_windows(i,windows(i)/2:silo_nyears-1-windows(i)/2))
   windows_signif=where(ABS(this_window) ge signif_levels(i))
   GPLOT,X=1900+windows_signif+windows(i)/2,Y=this_window[windows_signif],SYM=6,SIZE=50,COL=i+2,THICK=150,/NOLINES
ENDFOR

GPLOT,X=1900+indgen(silo_nyears),Y=REPLICATE(0,silo_nyears),COL=1+n_windows,THICK=150,STYLE=1
;GPLOT,X=1900+indgen(silo_nyears-windows(0)),$
;      Y=obs_nino4_amean,COL=2,THICK=150,STYLE=2
LEGEND,labels=REVERSE(windows_strings),COL=REVERSE(indgen(n_windows)+2),LEGXOFFSET=-6500,LEGYOFFSET=9000

POS,YPOS=1
GSET,XMIN=1900,XMAX=1900+silo_nyears,YMIN=30,YMAX=180
FOR i=0,n_windows-1 DO $   
   GPLOT,X=1900+indgen(silo_nyears-windows(i))+windows(i)/2,$
   Y=REFORM(obs_precipvar_windows(i,windows(i)/2:silo_nyears-1-windows(i)/2))*365,COL=i+2,THICK=150,STYLE=0
AXES,XSTEP=10,YSTEP=30,YTITLE='Standard deviation in annual-total rain (mm year!U-1!N)',NDECS=2,/NOUPPER,XTITLE='Year'

;POS,YPOS=1
;GSET,XMIN=0,XMAX=n_windows,YMIN=-1,YMAX=1.5
;FOR i=0,n_windows-1 DO BEGIN
;   GPLOT,X=i+0.2,Y=CORRELATE(obs_precipvar_windows(i,windows(i)/2:silo_nyears-1-windows(i)/2),$
;                             obs_corr_windows(i,windows(i)/2:silo_nyears-1-windows(i)/2)),$
;         COL=i+2,THICK=150,SYM=6
;   GPLOT,X=i+0.4,Y=CORRELATE(obs_precipvar_windows(i,windows(i)/2:silo_nyears-1-windows(i)/2),$
;                             obs_ninovar_windows(i,windows(i)/2:silo_nyears-1-windows(i)/2)),$
;         COL=i+2,THICK=150,SYM=3
;ENDFOR
;GPLOT,X=indgen(n_windows+1),Y=REPLICATE(0,n_windows+1),STYLE=1
;AXES,XVALS=indgen(n_windows)+0.5,XLABELS=windows_strings_short,YVALS=['-1.00','-0.75','-0.50','-0.25','0.00','0.25','0.50','0.75','1;.00'],$
;     NDECS=2,/NORIGHT,/NOUPPER,$
;     XTITLE='Length of window (years)',YTITLE='Correlation coefficient'
;
;GSET,XMIN=0,XMAX=n_windows,YMIN=0,YMAX=0.4
;FOR i=0,n_windows-1 DO BEGIN
;   GPLOT,X=i+0.6,Y=STDDEV(obs_corr_windows(i,windows(i)/2:silo_nyears-1-windows(i)/2)),COL=i+2,THICK=150,SYM=4
;   GPLOT,X=i+0.8,Y=STDDEV(obs_precipvar_windows(i,windows(i)/2:silo_nyears-1-windows(i)/2)),COL=i+2,THICK=150,SYM=5
;ENDFOR

;AXES,XVALS=indgen(n_windows)+0.5,YVALS=['0.00','0.10','0.20','0.30','0.40'],YTITLE='Standard deviation',/ONLYRIGHT,NDECS=2
;LEGEND,labels=REVERSE(['Correlation of windowed correlation and standard deviation of Queensland rain (left axis)','Standard deviation of windowed correlation between Queensland rainfall and Nino 4 (right axis)','Standard deviation of windowed standard deviation of Queensland rain (right axis)']),$
;       SYM=REVERSE([6,4,5]),LENGTH=0,LEGXOFFSET=0,LEGYOFFSET=5000
PSCLOSE

STOP

END



