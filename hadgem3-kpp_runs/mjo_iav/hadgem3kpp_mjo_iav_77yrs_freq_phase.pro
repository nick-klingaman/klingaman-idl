PRO hadgem3kpp_mjo_iav_77yrs_freq_phase

infile='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmeans.1-77.rmm_indices.nc'

rmm1=OPEN_AND_EXTRACT(infile,'rmm1')
rmm2=OPEN_AND_EXTRACT(infile,'rmm2')
phase=OPEN_AND_EXTRACT(infile,'phase')

rmm1_ts=fltarr(N_ELEMENTS(rmm1))
rmm2_ts=fltarr(N_ELEMENTS(rmm2))
phase_ts=fltarr(N_ELEMENTS(phase))
FOR i=0,N_ELEMENTS(rmm1(*,0))-1 DO BEGIN
   rmm1_ts(i*360:(i+1)*360-1)=rmm1(i,*)
   rmm2_ts(i*360:(i+1)*360-1)=rmm2(i,*)
   phase_ts(i*360:(i+1)*360-1)=phase(i,*)
ENDFOR
amp_ts=(rmm1_ts^2+rmm2_ts^2)^0.5

n_time=N_ELEMENTS(rmm1_ts)
running_mean_rmm1=fltarr(n_time-360)
running_mean_rmm2=fltarr(n_time-360)
running_mean_amp=fltarr(n_time-360)
days_per_phase=fltarr(9,n_time-360)
FOR i=180,n_time-181 DO BEGIN
   running_mean_rmm1(i-180)=MEAN(rmm1_ts(i-180:i+180))
   running_mean_rmm2(i-180)=MEAN(rmm2_ts(i-180:i+180))
   running_mean_amp(i-180)=MEAN(amp_ts(i-180:i+180))
   FOR j=1,8 DO $
      days_per_phase(j,i-180)=N_ELEMENTS(where(amp_ts(i-180:i+180) ge 1 and $
                                           phase_ts(i-180:i+180) eq j))/361.
   days_per_phase(0,i-180)=N_ELEMENTS(where(amp_ts(i-180:i+180) lt 1))/361.
ENDFOR
days_per_phase(*,0:299)=!Values.F_NaN

psfile='/home/ss901165/idl/hadgem3-kpp_runs/mjo_iav/hadgem3kpp_mjo_iav_77yrs_freq_phase.361day_running_mean_allphases.ps'
PSOPEN,file=psfile,TFONT=2,CHARSIZE=110,XOFFSET=1000,YPLOTS=9,YSPACING=400,MARGIN=1500,YOFFSET=500

colors=['black','red','blue','purple','cyan','orange','violetred','dodgerblue','brown','pink']

POS,YPOS=1
GSET,XMIN=0,XMAX=(n_time-360)/360.,YMIN=0.15,YMAX=0.61
GPLOT,X=findgen(n_time-360)/360.,Y=REFORM(days_per_phase(0,*)),COL=FSC_COLOR(colors(0))
AXES,XSTEP=4,XMINOR=1,XTITLE='Year of integration',$
     YTITLE='Phase 0',YSTEP=0.1,YMINOR=0.05,ORIENTATION=30,/NOUPPER,NDECS=2

FOR i=1,8 DO BEGIN
   POS,YPOS=i+1
   GSET,XMIN=0,XMAX=(n_time-360)/360.,YMIN=0.04,YMAX=0.20
   GPLOT,X=findgen(n_time-360)/360.,Y=REFORM(days_per_phase(i,*)),COL=FSC_COLOR(colors(i))
   AXES,XSTEP=4,XMINOR=1,XTITLE='Year of integration',$
        YTITLE='Phase '+STRTRIM(STRING(i),1),YSTEP=0.04,YMINOR=0.02,ORIENTATION=30,/NOLOWER,/NOUPPER,NDECS=2
ENDFOR

PSCLOSE,/NOVIEW

correlation_matrix=fltarr(10,10)
FOR i=0,8 DO $
   FOR j=0,8 DO $
      correlation_matrix(i,j)=CORRELATE(days_per_phase(i,300:n_time-361),days_per_phase(j,300:n_time-361))
FOR j=0,8 DO BEGIN
   correlation_matrix(9,j)=CORRELATE(running_mean_amp(300:n_time-361),days_per_phase(j,300:n_time-361))
   correlation_matrix(j,9)=correlation_matrix(9,j)
ENDFOR
correlation_matrix(9,9)=1.

psfile='/home/ss901165/idl/hadgem3-kpp_runs/mjo_iav/hadgem3kpp_mjo_iav_77yrs_freq_phase.361day_days_per_phase_corr_matrix.ps'
PSOPEN,file=psfile,TFONT=2,CHARSIZE=110,SPACE3=300,SPACE2=1000

GSET,XMIN=0,XMAX=10,YMIN=0,YMAX=10
mylevs=['-0.55','-0.45','-0.35','-0.25','-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55']
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
LEVS,MANUAL=mylevs
CON,X=indgen(10)+0.5,Y=indgen(10)+0.5,FIELD=correlation_matrix,CB_TITLE='Correlation coefficient on days in each phase',/BLOCK,$
    /NOLINES
AXES,XVALS=indgen(10)+0.5,YVALS=indgen(10)+0.5,$
     XLABELS=STRTRIM(STRING(indgen(10)),1),YLABELS=STRTRIM(STRING(indgen(10)),1),$
     XTITLE='Phase (9 = 361-day running mean amplitude)',$
     YTITLE='Phase (9 = 361-day running mean amplitude)'
PSCLOSE

STOP
END
