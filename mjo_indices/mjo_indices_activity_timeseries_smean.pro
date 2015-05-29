PRO mjo_indices_activity_timeseries_smean

file_name='/home/ss901165/um_output6/xihvd/rmm_indices.nc'
obs_file_name='/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2012.index_values.nc'
n_years=60
obs_nyears=34
ndays_per_year=360
obs_ndays_per_year=366
start_day=300
stop_day=119
IF stop_day le start_day THEN BEGIN
   n_days=stop_day+360-start_day
   n_years_mean=n_years-1
   obs_nyears_mean=obs_nyears-1
ENDIF ELSE BEGIN
   n_days=stop_day-start_day
   n_years_mean=n_years
   obs_nyears_mean=obs_nyears
ENDELSE
period_str='ndjfma'

amplitude=OPEN_AND_EXTRACT(file_name,'amplitude',$
                           offset=[0,0],count=[n_years,ndays_per_year])
obs_amplitude=OPEN_AND_EXTRACT(obs_file_name,'amplitude',$
                               offset=[4,0],count=[obs_nyears,obs_ndays_per_year])
obs_amplitude[where(obs_amplitude lt 0)]=!Values.F_NaN

amplitude_ts=fltarr(ndays_per_year*n_years)
FOR i=0,n_years-1 DO $
   FOR j=0,ndays_per_year-1 DO $
      amplitude_ts(i*ndays_per_year+j)=amplitude(i,j)
obs_amplitude_ts=fltarr(obs_ndays_per_year*obs_nyears)
FOR i=0,obs_nyears-1 DO $
   obs_amplitude_ts(i*obs_ndays_per_year:(i+1)*obs_ndays_per_year-1)=obs_amplitude(i,*)

amplitude_mean=fltarr(n_years_mean)
obs_amplitude_mean=fltarr(obs_nyears_mean)
FOR i=0,n_years_mean-1 DO $
   amplitude_mean(i)=MEAN(amplitude_ts(i*ndays_per_year+start_day:i*ndays_per_year+start_day+n_days))
FOR i=0,obs_nyears_mean-1 DO $
   obs_amplitude_mean(i)=MEAN(obs_amplitude_ts(i*obs_ndays_per_year+start_day:i*obs_ndays_per_year+start_day+n_days),/NaN)

psfile='/home/ss901165/idl/mjo_indices/mjo_indices_activity_timeseries_smean.xihvd_'+period_str+'.ps'
PSOPEN,file=psfile,MARGIN=2500,XOFFSET=1000,YOFFSET=1000,FONT=6,TFONT=6,CHARSIZE=150,TCHARSIZE=110,SPACE2=400,SPACE3=400
GSET,XMIN=0,XMAX=n_years_mean,YMIN=0.8,YMAX=2,TITLE='NDJFMA mean MJO amplitude from (black) GA3-KPP and (red) observations (1979-2012)'
GPLOT,X=indgen(n_years_mean)+0.5,Y=amplitude_mean,THICK=150
GPLOT,X=indgen(obs_nyears_mean)+0.5,Y=obs_amplitude_mean,THICK=150,COL=FSC_COLOR('red')
GPLOT,X=50,Y=1.9,TEXT='Std.dev./mean = '+STRMID(STRTRIM(STRING(STDDEV(amplitude_mean)/MEAN(amplitude_mean)),1),0,4)
GPLOT,X=50,Y=1.85,TEXT='Std.dev./mean = '+STRMID(STRTRIM(STRING(STDDEV(obs_amplitude_mean)/MEAN(amplitude_mean)),1),0,4),COL=FSC_COLOR('red')

;GPLOT,X=ndays_per_year/2,Y=4.8,TEXT='Amplitude (daily)',ALIGN=0
;GPLOT,X=ndays_per_year/2,Y=4.6,TEXT='Max: '+STRMID(STRTRIM(STRING(MAX(amplitude_ts,/NaN)),1),0,4),ALIGN=0
;GPLOT,X=ndays_per_year/2,Y=4.4,TEXT='Min: '+STRMID(STRTRIM(STRING(MIN(amplitude_ts,/NaN)),1),0,4),ALIGN=0
;GPLOT,X=ndays_per_year/2,Y=4.2,TEXT='Mean: '+STRMID(STRTRIM(STRING(MEAN(amplitude_ts,/NaN)),1),0,4),ALIGN=0
;GPLOT,X=ndays_per_year/2,Y=4.0,TEXT='Std.dev.: '+STRMID(STRTRIM(STRING(STDDEV(amplitude_ts,/NaN)),1),0,4),ALIGN=0

;GPLOT,X=ndays_per_year*(n_years/3),Y=4.8,TEXT='Amplitude (smoothed, '+STRTRIM(STRING(running_mean_length),1)+' days)',ALIGN=0
;GPLOT,X=ndays_per_year*(n_years/3),Y=4.6,TEXT='Max: '+STRMID(STRTRIM(STRING(MAX(amplitude_smoothed,/NaN)),1),0,4),ALIGN=0
;GPLOT,X=ndays_per_year*(n_years/3),Y=4.4,TEXT='Min: '+STRMID(STRTRIM(STRING(MIN(amplitude_smoothed,/NaN)),1),0,4),ALIGN=0
;GPLOT,X=ndays_per_year*(n_years/3),Y=4.2,TEXT='Mean: '+STRMID(STRTRIM(STRING(MEAN(amplitude_smoothed,/NaN)),1),0,4),ALIGN=0
;GPLOT,X=ndays_per_year*(n_years/3),Y=4.0,TEXT='Std.dev.: '+STRMID(STRTRIM(STRING(STDDEV(amplitude_smoothed,/NaN)),1),0,4),ALIGN=0

;GPLOT,X=ndays_per_year*(n_years*2/3),Y=4.8,TEXT='Variance windowed '+STRTRIM(STRING(running_mean_length),1)+' days',ALIGN=0
;GPLOT,X=ndays_per_year*(n_years*2/3),Y=4.6,TEXT='Max: '+STRMID(STRTRIM(STRING(MAX(amplitude_variance,/NaN)),1),0,4),ALIGN=0
;GPLOT,X=ndays_per_year*(n_years*2/3),Y=4.4,TEXT='Min: '+STRMID(STRTRIM(STRING(MIN(amplitude_variance,/NaN)),1),0,4),ALIGN=0
;GPLOT,X=ndays_per_year*(n_years*2/3),Y=4.2,TEXT='Mean: '+STRMID(STRTRIM(STRING(MEAN(amplitude_variance,/NaN)),1),0,4),ALIGN=0
;GPLOT,X=ndays_per_year*(n_years*2/3),Y=4.0,TEXT='Std.dev.: '+STRMID(STRTRIM(STRING(STDDEV(amplitude_variance,/NaN)),1),0,4),ALIGN=0

AXES,XSTEP=3,XMINOR=1,YSTEP=0.1,YMINOR=0.05,XTITLE='Year',YTITLE='Amplitude',NDECS=2,ORIENTATION=30
PSCLOSE

; amplitude_clim=fltarr(ndays_per_year)
; FOR i=0,ndays_per_year-1 DO $
;    amplitude_clim(i)=MEAN(amplitude(*,i),/NaN)

; amplitude_ndays=fltarr(ndays_per_year)
; FOR i=0,ndays_per_year-1 DO BEGIN
;    IF TOTAL(where(amplitude(*,i) ge 1)) ge 0 THEN $
;       amplitude_ndays(i)=N_ELEMENTS(where(amplitude(*,i) ge 1))
; ENDFOR

; psfile='/home/ss901165/idl/mjo_indices/mjo_indices_activity_timeseries.seasonal_cycle.'+run_id+'.ps'
; PSOPEN,file=psfile,MARGIN=2500,XOFFSET=500,YOFFSET=1000
; GSET,XMIN=0,XMAX=ndays_per_year,YMIN=0.5,YMAX=2
; GPLOT,X=indgen(ndays_per_year)+0.5,Y=SMOOTH(amplitude_clim,5),COL=FSC_COLOR('blue')
; AXES,XSTEP=20,YSTEP=0.1,YMINOR=0.05,NDECS=2,XMINOR=10,XTITLE='Day of year',YTITLE='Climatological daily amplitude',/NORIGHT,ORIENTATION=40
; GSET,XMIN=0,XMAX=ndays_per_year,YMIN=0,YMAX=1
; GPLOT,X=indgen(ndays_per_year)+0.5,Y=SMOOTH(amplitude_ndays,5)/FLOAT(n_years),COL=FSC_COLOR('red')
; AXES,YSTEP=0.1,YMINOR=0.05,YTITLE='Fraction of years with strong MJO on this date',/ONLYRIGHT,NDECS=2
; GLEGEND,labels=['Fraction of years','Mean amplitude'],COL=[FSC_COLOR('red'),FSC_COLOR('blue')],LEGPOS=3
; PSCLOSE,/NOVIEW

; ndays_per_phase=fltarr(9,ndays_per_year*n_years)
; FOR i=running_mean_length/2,ndays_per_year*n_years-running_mean_length/2-1 DO BEGIN
;    FOR j=1,8 DO $
;       ndays_per_phase(j,i)=N_ELEMENTS(where(phase_ts(i-running_mean_length/2:i+running_mean_length/2-1) eq j and $
;                                             amplitude_ts(i-running_mean_length/2:i+running_mean_length/2-1) ge 1))/FLOAT(running_mean_length)
;    ndays_per_phase(0,i)=N_ELEMENTS(where(amplitude_ts(i-running_mean_length/2:i+running_mean_length/2-1) lt 1))/FLOAT(running_mean_length)
; ENDFOR

; psfile='/home/ss901165/idl/mjo_indices/mjo_indices_activity_timeseries.ndays_'+STRTRIM(STRING(running_mean_length),1)+'.'+run_id+'.ps'
; PSOPEN,file=psfile,MARGIN=2500,XOFFSET=500,YOFFSET=1000,TFONT=6,FONT=6,CHARSIZE=150
; GSET,XMIN=0,XMAX=ndays_per_year*n_years,YMIN=0,YMAX=0.8
; CS,SCALE=28,NCOLS=9
; FOR i=0,8 DO $
;    GPLOT,X=findgen(ndays_per_year*n_years)+0.5,Y=SMOOTH(REFORM(ndays_per_phase(i,*)),running_mean_length,/NaN),COL=i+1
; AXES,YSTEP=0.05,YMINOR=0.025,YTITLE='Fraction of days in '+STRTRIM(STRING(running_mean_length),1)+'-day window',XTITLE='Day in integration',NDECS=2,$
;      XSTEP=ndays_per_year,XMINOR=ndays_per_year/2,ORIENTATION=40
; GLEGEND,labels=REVERSE(['Phase 0','Phase 1','Phase 2','Phase 3','Phase 4','Phase 5','Phase 6','Phase 7','Phase 8']),$
;         COL=REVERSE(indgen(9)+1),LEGPOS=9
; PSCLOSE

STOP
END

