PRO higem_nino_correlations_compare_silo_sliding_many

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

; Timeseries of NINO indices from HiGEM
xbylr_nino_infile='/home/ss901165/higem_qccce/hpcx_control_xbylr/higem_xbylr.may-apr_ameans.h9-t5.nino_indices.nc'
eafeb_nino_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.may-apr_ameans.h9-w8.nino_indices.nc'

; Timeseries of Australian rainfall from HiGEM
xbylr_precip_infile='/home/ss901165/higem_qccce/hpcx_control_xbylr/higem_xbylr.may-apr_ameans.h9-t5.precip.aus_domain.nc'
eafeb_precip_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.may-apr_ameans.h9-w8.precip.aus_domain.nc'

xbylr_nyears=116
eafeb_nyears=149

mask_infile='/home/ss901165/um_output/mask_n144_higam.nc'

qld_box=[-10,138,-29,154]
; Read latitudes and longitudes, restrict to box of interest
silo_longitude=OPEN_AND_EXTRACT(silo_precip_infile,'longitude')
silo_latitude=OPEN_AND_EXTRACT(silo_precip_infile,'latitude')
DEFINE_BOUNDARIES,qld_box,silo_latitude,silo_longitude,silo_box_tx,/LIMIT
silo_nlon=N_ELEMENTS(silo_longitude)
silo_nlat=N_ELEMENTS(silo_latitude)

higem_longitude=OPEN_AND_EXTRACT(xbylr_precip_infile,'longitude')
higem_latitude=OPEN_AND_EXTRACT(xbylr_precip_infile,'latitude')
DEFINE_BOUNDARIES,qld_box,higem_latitude,higem_longitude,higem_box_tx,/LIMIT
higem_nlon=N_ELEMENTS(higem_longitude)
higem_nlat=N_ELEMENTS(higem_latitude)

; Read NINO indices
obs_nino_offset=(silo_start_year-obs_nino_start_year)
obs_nino_count=silo_nyears
obs_nino4_amean=OPEN_AND_EXTRACT(obs_nino_infile,'NINO4',offset=[obs_nino_offset],count=[obs_nino_count])
xbylr_nino4_amean=OPEN_AND_EXTRACT(xbylr_nino_infile,'nino4')
eafeb_nino4_amean=OPEN_AND_EXTRACT(eafeb_nino_infile,'nino4')

; Read rainfall
silo_precip_amean=REFORM(OPEN_AND_EXTRACT(silo_precip_infile,'rain',$
                                          offset=[silo_box_tx(1),silo_box_tx(0),0],$
                                          count=[silo_nlon,silo_nlat,silo_nyears]))
xbylr_precip_amean=REFORM(OPEN_AND_EXTRACT(xbylr_precip_infile,'precip',$
                                           offset=[higem_box_tx(1),higem_box_tx(0),0,0,0],$
                                           count=[higem_nlon,higem_nlat,1,1,xbylr_nyears]))*86400.
eafeb_precip_amean=REFORM(OPEN_AND_EXTRACT(eafeb_precip_infile,'precip',$
                                           offset=[higem_box_tx(1),higem_box_tx(0),0,0,0],$
                                           count=[higem_nlon,higem_nlat,1,1,eafeb_nyears]))*86400.

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
n144_weights=fltarr(silo_nlat)
n144_weights_latrev=fltarr(higem_nlat)
FOR i=0,silo_nlat-1 DO BEGIN
   n144_weights_latrev(i)=COS(3.14159*silo_latitude(i)/180.)
   n144_weights(i)=COS(3.14159*higem_latitude(i)/180.)
ENDFOR
n144_weights=n144_weights/TOTAL(n144_weights)
n144_weights_latrev=n144_weights_latrev/TOTAL(n144_weights_latrev)

silo_precip_amean_aavg=fltarr(silo_nyears)
xbylr_precip_amean_aavg=fltarr(xbylr_nyears)
eafeb_precip_amean_aavg=fltarr(eafeb_nyears)
FOR i=0,silo_nyears-1 DO BEGIN
   thisyear_masked=REFORM(silo_precip_amean(*,*,i))
   thisyear_masked[where(mask_latrev lt 1 or thisyear_masked ge 1E20)]=!Values.F_NaN
   FOR j=0,silo_nlon-1 DO $
      silo_precip_amean_aavg(i)=silo_precip_amean_aavg(i)+$
      TOTAL(n144_weights_latrev*REFORM(thisyear_masked(j,*)),/NaN)*1./FLOAT(silo_nlon)
ENDFOR
FOR i=0,xbylr_nyears-1 DO BEGIN
   thisyear_masked=REFORM(xbylr_precip_amean(*,*,i))
   thisyear_masked[where(mask lt 1)]=!Values.F_NaN
   FOR j=0,higem_nlon-1 DO $
      xbylr_precip_amean_aavg(i)=xbylr_precip_amean_aavg(i)+$
      TOTAL(n144_weights*REFORM(thisyear_masked(j,*)),/NaN)*1./FLOAT(higem_nlon)
ENDFOR
FOR i=0,eafeb_nyears-1 DO BEGIN
   thisyear_masked=REFORM(eafeb_precip_amean(*,*,i))
   thisyear_masked[where(mask lt 1)]=!Values.F_NaN
   FOR j=0,higem_nlon-1 DO $
      eafeb_precip_amean_aavg(i)=eafeb_precip_amean_aavg(i)+$
      TOTAL(n144_weights*REFORM(thisyear_masked(j,*)),/NaN)*1./FLOAT(higem_nlon)
ENDFOR
  
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
xbylr_corr_windows=fltarr(n_windows,xbylr_nyears)
xbylr_ninovar_windows=fltarr(n_windows,xbylr_nyears)
xbylr_precipvar_windows=fltarr(n_windows,xbylr_nyears)
xbylr_corrvar_windows=fltarr(n_windows,xbylr_nyears)
eafeb_corr_windows=fltarr(n_windows,eafeb_nyears)
eafeb_ninovar_windows=fltarr(n_windows,eafeb_nyears)
eafeb_precipvar_windows=fltarr(n_windows,eafeb_nyears)
eafeb_corrvar_windows=fltarr(n_windows,eafeb_nyears)

FOR i=0,n_windows-1 DO BEGIN
   FOR j=windows(i)/2,silo_nyears-1-windows(i)/2 DO BEGIN
      obs_corr_windows(i,j)=CORRELATE(obs_nino4_amean(j-windows(i)/2:windows(i)/2+j),$
                                      silo_precip_amean_aavg(j-windows(i)/2:windows(i)/2+j)) 
      obs_ninovar_windows(i,j)=STDDEV(obs_nino4_amean(j-windows(i)/2:windows(i)/2+j))
      obs_precipvar_windows(i,j)=STDDEV(silo_precip_amean_aavg(j-windows(i)/2:windows(i)/2+j))
   ENDFOR
;   FOR j=windows(i),silo_nyears-1-windows(i) DO $
;      obs_corrvar_windows(i,j)=CORRELATE(obs_ninovar_windows(i,j-windows(i)/2:windows(i)/2+j),$
;                                         silo_precip_amean_aavg(j-windows(i)/2:windows(i)/2+j))
   FOR j=windows(i),silo_nyears-1-windows(i) DO $
      obs_corrvar_windows(i,j)=CORRELATE(obs_ninovar_windows(i,j-windows(i)/2:windows(i)/2+j),$
                                         obs_precipvar_windows(i,j-windows(i)/2:windows(i)/2+j))
   FOR j=windows(i)/2,xbylr_nyears-1-windows(i)/2 DO BEGIN
      xbylr_corr_windows(i,j)=CORRELATE(xbylr_nino4_amean(j-windows(i)/2:windows(i)/2+j),$
                                        xbylr_precip_amean_aavg(j-windows(i)/2:windows(i)/2+j))
      xbylr_ninovar_windows(i,j)=STDDEV(xbylr_nino4_amean(j-windows(i)/2:windows(i)/2+j))
      xbylr_precipvar_windows(i,j)=STDDEV(xbylr_precip_amean_aavg(j-windows(i)/2:windows(i)/2+j))
   ENDFOR
   FOR j=windows(i),xbylr_nyears-1-windows(i) DO $
      xbylr_corrvar_windows(i,j)=CORRELATE(xbylr_ninovar_windows(i,j-windows(i)/2:windows(i)/2+j),$
                                           xbylr_precipvar_windows(i,j-windows(i)/2:windows(i)/2+j))
   FOR j=windows(i)/2,eafeb_nyears-1-windows(i)/2 DO BEGIN
      eafeb_corr_windows(i,j)=CORRELATE(eafeb_nino4_amean(j-windows(i)/2:windows(i)/2+j),$
                                        eafeb_precip_amean_aavg(j-windows(i)/2:windows(i)/2+j))
      eafeb_ninovar_windows(i,j)=STDDEV(eafeb_nino4_amean(j-windows(i)/2:windows(i)/2+j))
      eafeb_precipvar_windows(i,j)=STDDEV(eafeb_precip_amean_aavg(j-windows(i)/2:windows(i)/2+j))
   ENDFOR
   FOR j=windows(i),eafeb_nyears-1-windows(i) DO $
      eafeb_corrvar_windows(i,j)=CORRELATE(eafeb_ninovar_windows(i,j-windows(i)/2:windows(i)/2+j),$
                                           eafeb_precipvar_windows(i,j-windows(i)/2:windows(i)/2+j))
ENDFOR

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo_sliding_many.qld_region.annmean_may-apr_nino4.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=200,XOFFSET=900,YOFFSET=200,TFONT=2,TCHARSIZE=80,SPACE3=300,YPLOTS=3,SPACING=1000

POS,YPOS=3
CS,SCALE=26,NCOLS=n_windows,/REV
black=FSC_COLOR("black",1+n_windows)
GSET,XMIN=0,XMAX=eafeb_nyears,YMIN=-1.0,YMAX=0.4
AXES,XSTEP=10,YSTEP=0.2,XTITLE='Year in SILO dataset (1900-2007)',YTITLE='Corr QLD rain with Nino 4',NDECS=2,/NOUPPER
FOR i=0,n_windows-1 DO $
   GPLOT,X=indgen(silo_nyears-windows(i))+windows(i)/2,$
   Y=REFORM(obs_corr_windows(i,windows(i)/2:silo_nyears-1-windows(i)/2)),COL=i+2,THICK=150
LEGEND,labels=REVERSE(windows_strings),COL=REVERSE(indgen(n_windows)+2),LEGXOFFSET=0,LEGYOFFSET=7000

POS,YPOS=2
CS,SCALE=26,NCOLS=n_windows,/REV
black=FSC_COLOR("black",1+n_windows)
GSET,XMIN=0,XMAX=eafeb_nyears,YMIN=-1.0,YMAX=0.4
AXES,XSTEP=10,YSTEP=0.2,XTITLE='Year in HiGEM xbylr (HPCx control)',YTITLE='Corr QLD rain with Nino 4',NDECS=2,/NOUPPER
FOR i=0,n_windows-1 DO $
   GPLOT,X=indgen(xbylr_nyears-windows(i))+windows(i)/2,$
   Y=REFORM(xbylr_corr_windows(i,windows(i)/2:xbylr_nyears-1-windows(i)/2)),COL=i+2,THICK=150

POS,YPOS=1
CS,SCALE=26,NCOLS=n_windows,/REV
black=FSC_COLOR("black",1+n_windows)
GSET,XMIN=0,XMAX=eafeb_nyears,YMIN=-1.0,YMAX=0.4
AXES,XSTEP=10,YSTEP=0.2,XTITLE='Year in HiGEM eafeb (ES control)',YTITLE='Corr QLD rain with Nino 4',NDECS=2,/NOUPPER
FOR i=0,n_windows-1 DO $
   GPLOT,X=indgen(eafeb_nyears-windows(i))+windows(i)/2,$
   Y=REFORM(eafeb_corr_windows(i,windows(i)/2:eafeb_nyears-1-windows(i)/2)),COL=i+2,THICK=150

PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo_sliding_many.qld_region.annmean_may-apr_nino4_var.silo_hadisst.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2500,SPACE2=200,XOFFSET=0,YOFFSET=0,TFONT=2,TCHARSIZE=80,SPACE3=300,YPLOTS=3,SPACING=800

POS,YPOS=3
CS,SCALE=26,NCOLS=n_windows,/REV
black=FSC_COLOR("black",1+n_windows)
GSET,XMIN=1900,XMAX=1900+silo_nyears,YMIN=-1.0,YMAX=0.8
AXES,XSTEP=10,YSTEP=0.2,XTITLE='Year in SILO dataset (1900-2007)',YTITLE='Corr QLD rain with Nino 4',NDECS=2,/NOUPPER,/NORIGHT
FOR i=0,n_windows-1 DO $
   GPLOT,X=1900+indgen(silo_nyears-windows(i))+windows(i)/2,$
   Y=REFORM(obs_corr_windows(i,windows(i)/2:silo_nyears-1-windows(i)/2)),COL=i+2,THICK=150
LEGEND,labels=REVERSE(windows_strings),COL=REVERSE(indgen(n_windows)+2),LEGXOFFSET=1500,LEGYOFFSET=7000

POS,YPOS=2

GSET,XMIN=1900,XMAX=1900+silo_nyears,YMIN=0,YMAX=0.7
FOR i=0,n_windows-1 DO $
   GPLOT,X=1900+indgen(silo_nyears-windows(i))+windows(i)/2,$
   Y=REFORM(obs_ninovar_windows(i,windows(i)/2:silo_nyears-1-windows(i)/2)),COL=i+2,THICK=150
AXES,XSTEP=10,/ONLYLEFT,YTITLE='Stdev. Nino 4',YVALS=['0.10','0.20','0.30','0.40','0.50','0.60']

GSET,XMIN=1900,XMAX=1900+silo_nyears,YMIN=0,YMAX=0.55
FOR i=0,n_windows-1 DO $   
   GPLOT,X=1900+indgen(silo_nyears-windows(i))+windows(i)/2,$
   Y=REFORM(obs_precipvar_windows(i,windows(i)/2:silo_nyears-1-windows(i)/2)),COL=i+2,THICK=150,STYLE=2
AXES,XSTEP=10,YSTEP=0.10,YTITLE='Stdev. May-Apr rain',NDECS=2,/ONLYRIGHT
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
       SYM=REVERSE([6,3,4,5]),LENGTH=0,LEGXOFFSET=-7000,LEGYOFFSET=18500
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo_sliding_many.qld_region.annmean_may-apr_nino4_var.higem_xbylr.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2500,SPACE2=200,XOFFSET=0,YOFFSET=0,TFONT=2,TCHARSIZE=80,SPACE3=300,YPLOTS=3,SPACING=800

POS,YPOS=3
CS,SCALE=26,NCOLS=n_windows,/REV
black=FSC_COLOR("black",1+n_windows)
GSET,XMIN=0,XMAX=0+xbylr_nyears,YMIN=-1.0,YMAX=0.8
AXES,XSTEP=10,YSTEP=0.2,XTITLE='Year in HiGEM xbylr',YTITLE='Corr QLD rain with Nino 4',NDECS=2,/NOUPPER,/NORIGHT
FOR i=0,n_windows-1 DO $
   GPLOT,X=0+indgen(xbylr_nyears-windows(i))+windows(i)/2,$
   Y=REFORM(xbylr_corr_windows(i,windows(i)/2:xbylr_nyears-1-windows(i)/2)),COL=i+2,THICK=150
LEGEND,labels=REVERSE(windows_strings),COL=REVERSE(indgen(n_windows)+2),LEGXOFFSET=1500,LEGYOFFSET=7500

POS,YPOS=2

GSET,XMIN=0,XMAX=0+xbylr_nyears,YMIN=0,YMAX=0.7
FOR i=0,n_windows-1 DO $
   GPLOT,X=0+indgen(xbylr_nyears-windows(i))+windows(i)/2,$
   Y=REFORM(xbylr_ninovar_windows(i,windows(i)/2:xbylr_nyears-1-windows(i)/2)),COL=i+2,THICK=150
AXES,XSTEP=10,/ONLYLEFT,YTITLE='Stdev. Nino 4',YVALS=['0.10','0.20','0.30','0.40','0.50','0.60']

GSET,XMIN=0,XMAX=0+xbylr_nyears,YMIN=0,YMAX=0.55
FOR i=0,n_windows-1 DO $   
   GPLOT,X=0+indgen(xbylr_nyears-windows(i))+windows(i)/2,$
   Y=REFORM(xbylr_precipvar_windows(i,windows(i)/2:xbylr_nyears-1-windows(i)/2)),COL=i+2,THICK=150,STYLE=2
AXES,XSTEP=10,YSTEP=0.10,YTITLE='Stdev. May-Apr rain',NDECS=2,/ONLYRIGHT
AXES,YSTEP=0.10,/NOLEFT,/NORIGHT,XSTEP=10,/NOUPPER

POS,YPOS=1
GSET,XMIN=0,XMAX=n_windows,YMIN=-1,YMAX=1.5
FOR i=0,n_windows-1 DO BEGIN
   GPLOT,X=i+0.2,Y=CORRELATE(xbylr_precipvar_windows(i,windows(i)/2:xbylr_nyears-1-windows(i)/2),$
                             xbylr_corr_windows(i,windows(i)/2:xbylr_nyears-1-windows(i)/2)),$
         COL=i+2,THICK=150,SYM=6
   GPLOT,X=i+0.4,Y=CORRELATE(xbylr_precipvar_windows(i,windows(i)/2:xbylr_nyears-1-windows(i)/2),$
                             xbylr_ninovar_windows(i,windows(i)/2:xbylr_nyears-1-windows(i)/2)),$
         COL=i+2,THICK=150,SYM=3
ENDFOR
GPLOT,X=indgen(n_windows+1),Y=REPLICATE(0,n_windows+1),STYLE=1
AXES,XVALS=indgen(n_windows)+0.5,XLABELS=windows_strings_short,YVALS=['-1.00','-0.75','-0.50','-0.25','0.00','0.25','0.50','0.75','1.00'],$
     NDECS=2,/NORIGHT,/NOUPPER,$
     XTITLE='Length of window',YTITLE='Correlation coefficient'

GSET,XMIN=0,XMAX=n_windows,YMIN=0,YMAX=0.4
FOR i=0,n_windows-1 DO BEGIN
   GPLOT,X=i+0.6,Y=STDDEV(xbylr_corr_windows(i,windows(i)/2:xbylr_nyears-1-windows(i)/2)),COL=i+2,THICK=150,SYM=4
   GPLOT,X=i+0.8,Y=STDDEV(xbylr_precipvar_windows(i,windows(i)/2:xbylr_nyears-1-windows(i)/2)),COL=i+2,THICK=150,SYM=5
ENDFOR

AXES,XVALS=indgen(n_windows)+0.5,YVALS=['0.00','0.10','0.20','0.30','0.40'],YTITLE='Standard deviation',/ONLYRIGHT,NDECS=2
LEGEND,labels=REVERSE(['Correlation of windowed corr with NINO4 and std.dev. of rain (left)','Correlation of windowed std.dev. of rain and std.dev. of NINO4','Std.dev. of windowed correlation (right)','Std.dev. of windowed std.dev. of rain (right)']),$
       SYM=REVERSE([6,3,4,5]),LENGTH=0,LEGXOFFSET=-7000,LEGYOFFSET=18500
PSCLOSE

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo_sliding_many.qld_region.annmean_may-apr_nino4_var.higem_eafeb.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2500,SPACE2=200,XOFFSET=0,YOFFSET=0,TFONT=2,TCHARSIZE=80,SPACE3=300,YPLOTS=3,SPACING=800

POS,YPOS=3
CS,SCALE=26,NCOLS=n_windows,/REV
black=FSC_COLOR("black",1+n_windows)
GSET,XMIN=0,XMAX=0+eafeb_nyears,YMIN=-1.0,YMAX=0.8
AXES,XSTEP=10,YSTEP=0.2,XTITLE='Year in HiGEM eafeb',YTITLE='Corr QLD rain with Nino 4',NDECS=2,/NOUPPER,/NORIGHT
FOR i=0,n_windows-1 DO $
   GPLOT,X=0+indgen(eafeb_nyears-windows(i))+windows(i)/2,$
   Y=REFORM(eafeb_corr_windows(i,windows(i)/2:eafeb_nyears-1-windows(i)/2)),COL=i+2,THICK=150
LEGEND,labels=REVERSE(windows_strings),COL=REVERSE(indgen(n_windows)+2),LEGXOFFSET=1500,LEGYOFFSET=7500

POS,YPOS=2

GSET,XMIN=0,XMAX=0+eafeb_nyears,YMIN=0,YMAX=1.2
FOR i=0,n_windows-1 DO $
   GPLOT,X=0+indgen(eafeb_nyears-windows(i))+windows(i)/2,$
   Y=REFORM(eafeb_ninovar_windows(i,windows(i)/2:eafeb_nyears-1-windows(i)/2)),COL=i+2,THICK=150
AXES,XSTEP=10,/ONLYLEFT,YTITLE='Stdev. Nino 4',YVALS=['0.10','0.30','0.50','0.70','0.90','1.10']

GSET,XMIN=0,XMAX=0+eafeb_nyears,YMIN=0,YMAX=0.55
FOR i=0,n_windows-1 DO $   
   GPLOT,X=0+indgen(eafeb_nyears-windows(i))+windows(i)/2,$
   Y=REFORM(eafeb_precipvar_windows(i,windows(i)/2:eafeb_nyears-1-windows(i)/2)),COL=i+2,THICK=150,STYLE=2
AXES,XSTEP=10,YSTEP=0.10,YTITLE='Stdev. May-Apr rain',NDECS=2,/ONLYRIGHT
AXES,YSTEP=0.10,/NOLEFT,/NORIGHT,XSTEP=10,/NOUPPER

POS,YPOS=1
GSET,XMIN=0,XMAX=n_windows,YMIN=-1,YMAX=1.5
FOR i=0,n_windows-1 DO BEGIN
   GPLOT,X=i+0.2,Y=CORRELATE(eafeb_precipvar_windows(i,windows(i)/2:eafeb_nyears-1-windows(i)/2),$
                             eafeb_corr_windows(i,windows(i)/2:eafeb_nyears-1-windows(i)/2)),$
         COL=i+2,THICK=150,SYM=6
   GPLOT,X=i+0.4,Y=CORRELATE(eafeb_precipvar_windows(i,windows(i)/2:eafeb_nyears-1-windows(i)/2),$
                             eafeb_ninovar_windows(i,windows(i)/2:eafeb_nyears-1-windows(i)/2)),$
         COL=i+2,THICK=150,SYM=3
ENDFOR
GPLOT,X=indgen(n_windows+1),Y=REPLICATE(0,n_windows+1),STYLE=1
AXES,XVALS=indgen(n_windows)+0.5,XLABELS=windows_strings_short,YVALS=['-1.00','-0.75','-0.50','-0.25','0.00','0.25','0.50','0.75','1.00'],$
     NDECS=2,/NORIGHT,/NOUPPER,$
     XTITLE='Length of window',YTITLE='Correlation coefficient'

GSET,XMIN=0,XMAX=n_windows,YMIN=0,YMAX=0.4
FOR i=0,n_windows-1 DO BEGIN
   GPLOT,X=i+0.6,Y=STDDEV(eafeb_corr_windows(i,windows(i)/2:eafeb_nyears-1-windows(i)/2)),COL=i+2,THICK=150,SYM=4
   GPLOT,X=i+0.8,Y=STDDEV(eafeb_precipvar_windows(i,windows(i)/2:eafeb_nyears-1-windows(i)/2)),COL=i+2,THICK=150,SYM=5
ENDFOR

AXES,XVALS=indgen(n_windows)+0.5,YVALS=['0.00','0.10','0.20','0.30','0.40'],YTITLE='Standard deviation',/ONLYRIGHT,NDECS=2
LEGEND,labels=REVERSE(['Correlation of windowed corr with NINO4 and std.dev. of rain (left)','Correlation of windowed std.dev. of rain and std.dev. of NINO4','Std.dev. of windowed correlation (right)','Std.dev. of windowed std.dev. of rain (right)']),$
       SYM=REVERSE([6,3,4,5]),LENGTH=0,LEGXOFFSET=-7000,LEGYOFFSET=18500
PSCLOSE

STOP

END



