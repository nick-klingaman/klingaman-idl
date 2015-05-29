PRO mjo_iav_regressions_cwv

n_fields=1

rmm_infile='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmeans.1-77.rmm_indices.nc'
rmm1=OPEN_AND_EXTRACT(rmm_infile,'rmm1')
rmm2=OPEN_AND_EXTRACT(rmm_infile,'rmm2')
rmm1_ts=fltarr(N_ELEMENTS(rmm1))
rmm2_ts=fltarr(N_ELEMENTS(rmm2))
FOR i=0,N_ELEMENTS(rmm1(*,0))-1 DO BEGIN
   rmm1_ts(i*360:(i+1)*360-1)=rmm1(i,*)
   rmm2_ts(i*360:(i+1)*360-1)=rmm2(i,*)
ENDFOR
amp_ts=(rmm1_ts^2+rmm2_ts^2)^0.5

n_time=N_ELEMENTS(rmm1_ts)
n_months=n_time/30
;n_months=852

amp_ts(0:119)=!Values.F_NaN
amp_mmean=fltarr(n_months)
FOR i=0,n_months-1 DO $
   amp_mmean(i)=MEAN(amp_ts(i*30:(i+1)*30-1),/NaN)

amp_running_mean=fltarr(n_months-12)
FOR i=6,n_months-7 DO $
   amp_running_mean(i-6)=MEAN(amp_mmean(i-6:i+6),/NaN)

box=[-30,0,30,360]
cwv_infile='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_mmeans.years1-77.cwv.nc'
longitude=OPEN_AND_EXTRACT(cwv_infile,'longitude')
latitude=OPEN_AND_EXTRACT(cwv_infile,'latitude')
DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)

cwv=REFORM(OPEN_AND_EXTRACT(cwv_infile,'cwv',$
                            offset=[box_tx(1),box_tx(0),0],count=[n_lon,n_lat,n_months]))

cwv_running_mean=fltarr(n_lon,n_lat,n_months-12)
FOR i=0,n_lon-1 DO $
   FOR j=0,n_lat-1 DO $
      FOR k=6,n_months-7 DO $
         cwv_running_mean(i,j,k-6)=MEAN(cwv(i,j,k-6:k+6))

spatial_regression=fltarr(n_lon,n_lat)
spatial_correlation=fltarr(n_lon,n_lat)
FOR i=0,n_lon-1 DO BEGIN
   FOR j=0,n_lat-1 DO BEGIN
      spatial_regression(i,j)=REGRESS(REFORM(amp_running_mean(*)),REFORM(cwv_running_mean(i,j,*)))
      spatial_correlation(i,j)=CORRELATE(REFORM(amp_running_mean(*)),REFORM(cwv_running_mean(i,j,*)))
   ENDFOR
ENDFOR

regress_levs=['-4.5','-3.9','-3.3','-2.7','-2.1','-1.5','-0.9','-0.3','0.3','0.9','1.5','2.1','2.7','3.3','3.9','4.5']
psfile='/home/ss901165/idl/hadgem3-kpp_runs/mjo_iav/regressions/mjo_iav_regressions_cwv.amip2_1.5xentrain.spatial_regression.ps'
PSOPEN,file=psfile,FONT=2,TFONT=2,CHARSIZE=120,SPACE3=400,SPACE2=2000,SPACE1=150,YOFFSET=2500,MARGIN=2000,YSIZE=10000
MAP,LATMIN=MIN(latitude),LATMAX=MAX(latitude),LONMIN=MIN(longitude),LONMAX=MAX(longitude)
CS,SCALE=1,NCOLS=N_ELEMENTS(regress_levs)+1,/REV
LEVS,MANUAL=regress_levs
CON,X=longitude,Y=latitude,FIELD=spatial_regression,$
    CB_TITLE='Regression of CWV on MJO amplitude [mm (unit amplitude)!U-1!N]',$
    CB_WIDTH=115,TITLE='Regression of 361-day mean column water vapor on 361-day MJO amplitude (stipple |corr| > 0.2)',$
    /NOLINES
FOR i=0,n_lon-1 DO $
   FOR j=0,n_lat-1 DO $
      IF (ABS(spatial_correlation(i,j)) ge 0.2) THEN $
         GPLOT,X=longitude(i),Y=latitude(j),SYM=3,SIZE=20
AXES,XSTEP=30,YSTEP=10
PSCLOSE,/NOVIEW

;cwv_box=[-15,0,15,360]
;domain='15S-15N_0-360E'
cwv_box=[-10,40,10,180]
domain='10S-10N_40-180E'
DEFINE_BOUNDARIES,cwv_box,latitude,longitude,cwv_box_tx,/LIMIT
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)

cwv_box_mean=fltarr(n_months)
cwv_box_median=fltarr(n_months)
cwv_box_iqr=fltarr(n_months)
cwv_box_exr=fltarr(n_months)
cwv_box_95=fltarr(n_months)
cwv_box_75=fltarr(n_months)
cwv_box_25=fltarr(n_months)
cwv_box_5=fltarr(n_months)

FOR i=0,n_months-1 DO BEGIN
   temp=cwv(cwv_box_tx(1):cwv_box_tx(3),cwv_box_tx(0):cwv_box_tx(2),i)
   cwv_box_mean(i)=MEAN(temp,/NaN)
   cwv_box_median(i)=MEDIAN(temp)
   sorted=temp[SORT(temp)]
   cwv_box_iqr(i)=sorted(N_ELEMENTS(temp)*3/4)-sorted(N_ELEMENTS(temp)/4)
   cwv_box_exr(i)=sorted(N_ELEMENTS(temp)*95/100)-sorted(N_ELEMENTS(temp)*5/100)
   cwv_box_95(i)=sorted(N_ELEMENTS(temp)*95/100)
   cwv_box_75(i)=sorted(N_ELEMENTS(temp)*3/4)
   cwv_box_25(i)=sorted(N_ELEMENTS(temp)/4)
   cwv_box_5(i)=sorted(N_ELEMENTS(temp)*5/100)
ENDFOR
cwv_box_mean_runningmean=fltarr(n_months-12)
cwv_box_median_runningmean=fltarr(n_months-12)
cwv_box_iqr_runningmean=fltarr(n_months-12)
cwv_box_exr_runningmean=fltarr(n_months-12)
cwv_box_95_runningmean=fltarr(n_months-12)
cwv_box_75_runningmean=fltarr(n_months-12)
cwv_box_25_runningmean=fltarr(n_months-12)
cwv_box_5_runningmean=fltarr(n_months-12)
FOR i=6,n_months-7 DO BEGIN
   cwv_box_mean_runningmean(i-6)=MEAN(cwv_box_mean(i-6:i+6))+0.30*(amp_running_mean(i-6)-1.3)
   cwv_box_median_runningmean(i-6)=MEAN(cwv_box_median(i-6:i+6))+0.22*(amp_running_mean(i-6)-1.3)
   cwv_box_iqr_runningmean(i-6)=MEAN(cwv_box_iqr(i-6:i+6))
   cwv_box_exr_runningmean(i-6)=MEAN(cwv_box_exr(i-6:i+6))
   cwv_box_95_runningmean(i-6)=MEAN(cwv_box_95(i-6:i+6));+0.20*(amp_running_mean(i-6)-1.3)
   cwv_box_75_runningmean(i-6)=MEAN(cwv_box_75(i-6:i+6))
   cwv_box_25_runningmean(i-6)=MEAN(cwv_box_25(i-6:i+6));-0.20*(amp_running_mean(i-6)-1.3)
   cwv_box_5_runningmean(i-6)=MEAN(cwv_box_5(i-6:i+6))
ENDFOR
cwv_box_mean_runningmean_corr=CORRELATE(cwv_box_mean_runningmean,amp_running_mean)
cwv_box_mean_runningmean_regress=REGRESS(amp_running_mean,cwv_box_mean_runningmean,CONST=cwv_box_mean_runningmean_const)
cwv_box_median_runningmean_corr=CORRELATE(cwv_box_median_runningmean,amp_running_mean)
cwv_box_median_runningmean_regress=REGRESS(amp_running_mean,cwv_box_median_runningmean,CONST=cwv_box_median_runningmean_const)
cwv_box_iqr_runningmean_corr=CORRELATE(cwv_box_iqr_runningmean,amp_running_mean)
cwv_box_iqr_runningmean_regress=REGRESS(amp_running_mean,cwv_box_iqr_runningmean,CONST=cwv_box_iqr_runningmean_const)
cwv_box_exr_runningmean_corr=CORRELATE(cwv_box_exr_runningmean,amp_running_mean)
cwv_box_exr_runningmean_regress=REGRESS(amp_running_mean,cwv_box_exr_runningmean,CONST=cwv_box_exr_runningmean_const)
cwv_box_95_runningmean_corr=CORRELATE(cwv_box_95_runningmean,amp_running_mean)
cwv_box_95_runningmean_regress=REGRESS(amp_running_mean,cwv_box_95_runningmean,CONST=cwv_box_95_runningmean_const)
cwv_box_75_runningmean_corr=CORRELATE(cwv_box_75_runningmean,amp_running_mean)
cwv_box_75_runningmean_regress=REGRESS(amp_running_mean,cwv_box_75_runningmean,CONST=cwv_box_75_runningmean_const)
cwv_box_25_runningmean_corr=CORRELATE(cwv_box_25_runningmean,amp_running_mean)
cwv_box_25_runningmean_regress=REGRESS(amp_running_mean,cwv_box_25_runningmean,CONST=cwv_box_25_runningmean_const)
cwv_box_5_runningmean_corr=CORRELATE(cwv_box_5_runningmean,amp_running_mean)
cwv_box_5_runningmean_regress=REGRESS(amp_running_mean,cwv_box_5_runningmean,CONST=cwv_box_5_runningmean_const)

psfile='/home/ss901165/idl/hadgem3-kpp_runs/mjo_iav/regressions/mjo_iav_regressions_cwv.amip2_1.5xentrain.scatter_amp_cwv_runningmean.'+domain+'.ps'
PSOPEN,file=psfile,FONT=2,TFONT=2,CHARSIZE=150,YPLOTS=4,YSPACING=1000,MARGIN=2000
POS,YPOS=1
GSET,XMIN=0.9,XMAX=1.8,YMIN=FLOOR(MIN(cwv_box_mean_runningmean)),YMAX=FLOOR(MAX(cwv_box_mean_runningmean))+0.5;,YMIN=38,YMAX=41
GPLOT,X=amp_running_mean,Y=cwv_box_mean_runningmean,SYM=3,COL=FSC_COLOR('red'),SIZE=30,/NOLINES
GPLOT,X=[1.0,1.7],Y=[1.0,1.7]*cwv_box_mean_runningmean_regress(0)+cwv_box_mean_runningmean_const,COL=FSC_COLOR('red')
GPLOT,X=[1.0],Y=[MAX(cwv_box_mean_runningmean)+0.5],TEXT='r='+STRMID(STRTRIM(STRING(cwv_box_mean_runningmean_corr),1),0,5),COL=FSC_COLOR('red')
AXES,XSTEP=0.1,XMINOR=0.05,YSTEP=1,YMINOR=0.2,/NORIGHT,/NOUPPER,XTITLE='RMM amplitude',YTITLE='Mean CWV (mm)',NDECS=1

POS,YPOS=2
GSET,XMIN=0.9,XMAX=1.8,YMIN=FLOOR(MIN(cwv_box_median_runningmean)),YMAX=FLOOR(MAX(cwv_box_median_runningmean)+0.5);,YMIN=38,YMAX=41
GPLOT,X=amp_running_mean,Y=cwv_box_median_runningmean,SYM=5,COL=FSC_COLOR('blue'),SIZE=30,/NOLINES
GPLOT,X=[1.0,1.7],Y=[1.0,1.7]*cwv_box_median_runningmean_regress(0)+cwv_box_median_runningmean_const,COL=FSC_COLOR('blue')
GPLOT,X=[1.0],Y=[MAX(cwv_box_median_runningmean)+0.5],TEXT='r='+STRMID(STRTRIM(STRING(cwv_box_median_runningmean_corr),1),0,5),COL=FSC_COLOR('blue')
AXES,XSTEP=0.1,XMINOR=0.05,YSTEP=1,YMINOR=0.2,YTITLE='Median CWV (mm)',XTITLE='RMM amplitude',/ONLYLEFT,NDECS=1

POS,YPOS=3
GSET,XMIN=0.9,XMAX=1.8,YMIN=FLOOR(MIN(cwv_box_iqr_runningmean)),YMAX=FLOOR(MAX(cwv_box_iqr_runningmean)+0.5);,YMIN=38,YMAX=41
GPLOT,X=amp_running_mean,Y=cwv_box_iqr_runningmean,SYM=5,COL=FSC_COLOR('purple'),SIZE=30,/NOLINES
GPLOT,X=[1.0,1.7],Y=[1.0,1.7]*cwv_box_iqr_runningmean_regress(0)+cwv_box_iqr_runningmean_const,COL=FSC_COLOR('purple')
GPLOT,X=[1.0],Y=[MAX(cwv_box_iqr_runningmean)+0.5],TEXT='r='+STRMID(STRTRIM(STRING(cwv_box_iqr_runningmean_corr),1),0,5),COL=FSC_COLOR('purple')
AXES,XSTEP=0.1,XMINOR=0.05,YSTEP=1,YMINOR=0.2,YTITLE='IQR of CWV (mm)',XTITLE='RMM amplitude',/ONLYLEFT,NDECS=1

POS,YPOS=4
GSET,XMIN=0.9,XMAX=1.8,YMIN=FLOOR(MIN(cwv_box_exr_runningmean)),YMAX=FLOOR(MAX(cwv_box_exr_runningmean)+1),$
     TITLE='361-day mean RMM amplitude versus 361-day mean CWV statistics ('+domain+')'
GPLOT,X=amp_running_mean,Y=cwv_box_exr_runningmean,SYM=5,COL=FSC_COLOR('violetred'),SIZE=30,/NOLINES
GPLOT,X=[1.0,1.7],Y=[1.0,1.7]*cwv_box_exr_runningmean_regress(0)+cwv_box_exr_runningmean_const,COL=FSC_COLOR('violetred')
GPLOT,X=[1.0],Y=[MAX(cwv_box_exr_runningmean)+0.5],TEXT='r='+STRMID(STRTRIM(STRING(cwv_box_exr_runningmean_corr),1),0,5),COL=FSC_COLOR('violetred')
AXES,XSTEP=0.1,XMINOR=0.05,YSTEP=1,YMINOR=0.2,YTITLE='EXR of CWV (mm)',XTITLE='RMM amplitude',/ONLYLEFT,NDECS=1
PSCLOSE,/NOVIEW


psfile='/home/ss901165/idl/hadgem3-kpp_runs/mjo_iav/regressions/mjo_iav_regressions_cwv.amip2_1.5xentrain.scatter_amp_cwvpct_runningmean.'+domain+'.ps'
PSOPEN,file=psfile,FONT=2,TFONT=2,CHARSIZE=150,YPLOTS=4,YSPACING=1000,MARGIN=2000

POS,YPOS=4
GSET,XMIN=0.9,XMAX=1.8,YMIN=FLOOR(MIN(cwv_box_95_runningmean)),YMAX=FLOOR(MAX(cwv_box_95_runningmean))+0.5,$
     TITLE='361-day mean RMM amplitude vs 361-day mean CWV statistics ('+domain+')'
GPLOT,X=amp_running_mean,Y=cwv_box_95_runningmean,SYM=3,COL=FSC_COLOR('purple'),SIZE=30,/NOLINES
GPLOT,X=[1.0,1.7],Y=[1.0,1.7]*cwv_box_95_runningmean_regress(0)+cwv_box_95_runningmean_const,COL=FSC_COLOR('purple')
GPLOT,X=[1.0],Y=[MAX(cwv_box_95_runningmean)+0.5],TEXT='r='+STRMID(STRTRIM(STRING(cwv_box_95_runningmean_corr),1),0,5),COL=FSC_COLOR('purple')
AXES,XSTEP=0.1,XMINOR=0.05,YSTEP=1,YMINOR=0.2,/ONLYLEFT,XTITLE='RMM amplitude',YTITLE='95%ile of CWV (mm)',NDECS=1

POS,YPOS=3
GSET,XMIN=0.9,XMAX=1.8,YMIN=FLOOR(MIN(cwv_box_75_runningmean)),YMAX=FLOOR(MAX(cwv_box_75_runningmean))+0.5
GPLOT,X=amp_running_mean,Y=cwv_box_75_runningmean,SYM=5,COL=FSC_COLOR('blue'),SIZE=30,/NOLINES
GPLOT,X=[1.0,1.7],Y=[1.0,1.7]*cwv_box_75_runningmean_regress(0)+cwv_box_75_runningmean_const,COL=FSC_COLOR('blue')
GPLOT,X=[1.0],Y=[MAX(cwv_box_75_runningmean)+0.5],TEXT='r='+STRMID(STRTRIM(STRING(cwv_box_75_runningmean_corr),1),0,5),COL=FSC_COLOR('blue')
AXES,XSTEP=0.1,XMINOR=0.05,YSTEP=1,YMINOR=0.2,YTITLE='75%ile of CWV (mm)',XTITLE='RMM amplitude',/ONLYLEFT,NDECS=1

POS,YPOS=2
GSET,XMIN=0.9,XMAX=1.8,YMIN=FLOOR(MIN(cwv_box_25_runningmean)),YMAX=FLOOR(MAX(cwv_box_25_runningmean))+0.5
GPLOT,X=amp_running_mean,Y=cwv_box_25_runningmean,SYM=5,COL=FSC_COLOR('red'),SIZE=30,/NOLINES
GPLOT,X=[1.0,1.7],Y=[1.0,1.7]*cwv_box_25_runningmean_regress(0)+cwv_box_25_runningmean_const,COL=FSC_COLOR('red')
GPLOT,X=[1.0],Y=[MAX(cwv_box_25_runningmean)+0.5],TEXT='r='+STRMID(STRTRIM(STRING(cwv_box_25_runningmean_corr),1),0,5),COL=FSC_COLOR('red')
AXES,XSTEP=0.1,XMINOR=0.05,YSTEP=1,YMINOR=0.2,YTITLE='25%ile of CWV (mm)',XTITLE='RMM amplitude',/ONLYLEFT,NDECS=1

POS,YPOS=1
GSET,XMIN=0.9,XMAX=1.8,YMIN=FLOOR(MIN(cwv_box_5_runningmean)),YMAX=FLOOR(MAX(cwv_box_5_runningmean))+0.5
GPLOT,X=amp_running_mean,Y=cwv_box_5_runningmean,SYM=5,COL=FSC_COLOR('brown'),SIZE=30,/NOLINES
GPLOT,X=[1.0,1.7],Y=[1.0,1.7]*cwv_box_5_runningmean_regress(0)+cwv_box_5_runningmean_const,COL=FSC_COLOR('brown')
GPLOT,X=[1.0],Y=[MAX(cwv_box_5_runningmean)+0.5],TEXT='r='+STRMID(STRTRIM(STRING(cwv_box_5_runningmean_corr),1),0,5),COL=FSC_COLOR('brown')
AXES,XSTEP=0.1,XMINOR=0.05,YSTEP=1,YMINOR=0.2,YTITLE='5%ile of CWV (mm)',XTITLE='RMM amplitude',/NORIGHT,/NOUPPER,NDECS=1
PSCLOSE,/NOVIEW

STOP
END
