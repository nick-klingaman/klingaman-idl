PRO mjo_indices_activity_timeseries,file_name,run_id,n_years,ndays_per_year,running_mean_length,offset_year=offset_year

IF KEYWORD_SET(offset_year) THEN BEGIN
   our_offset_year=offset_year
ENDIF ELSE $
   our_offset_year=0

amplitude=OPEN_AND_EXTRACT(file_name,'amplitude',$
                           offset=[our_offset_year,0],count=[n_years,ndays_per_year])
phase=OPEN_AND_EXTRACT(file_name,'phase',$
                       offset=[our_offset_year,0],count=[n_years,ndays_per_year])

twod_bad=where(amplitude le 0)

amplitude_ts=fltarr(ndays_per_year*n_years)
phase_ts=fltarr(ndays_per_year*n_years)
FOR i=0,n_years-1 DO BEGIN
   FOR j=0,ndays_per_year-1 DO BEGIN
      amplitude_ts(i*ndays_per_year+j)=amplitude(i,j)
      phase_ts(i*ndays_per_year+j)=phase(i,j)
   ENDFOR
ENDFOR
oned_bad=where(amplitude_ts le 0)
amplitude[twod_bad]=!Values.F_NaN
amplitude_ts[oned_bad]=!Values.F_NaN
phase[twod_bad]=!Values.F_NaN
phase_ts[oned_bad]=!Values.F_NaN

amplitude_smoothed=SMOOTH(amplitude_ts,running_mean_length,/NaN)
amplitude_smoothed[oned_bad]=!Values.F_NaN
amplitude_smoothed(0:running_mean_length/2-1)=!Values.F_NaN
amplitude_smoothed[oned_bad+running_mean_length/2-1]=!Values.F_NaN
amplitude_smoothed(ndays_per_year*n_years-running_mean_length/2:ndays_per_year*n_years-1)=!Values.F_NaN

amplitude_variance=fltarr(ndays_per_year*n_years-running_mean_length)
FOR i=running_mean_length/2,ndays_per_year*n_years-running_mean_length/2-2 DO BEGIN
   IF N_ELEMENTS(where(FINITE(amplitude_ts(i-running_mean_length/2:i+running_mean_length/2)) eq 1)) gt 1 THEN BEGIN
      amplitude_variance(i-running_mean_length/2)=STDDEV(amplitude_ts(i-running_mean_length/2:i+running_mean_length/2),/NaN)
   ENDIF ELSE $
      amplitude_variance(i-running_mean_length/2)=!Values.F_NaN
ENDFOR
amplitude_variance[oned_bad]=!Values.F_NaN

psfile='/home/ss901165/idl/mjo_indices/mjo_indices_activity_timeseries.running_mean_'+STRTRIM(STRING(running_mean_length),1)+'.'+run_id+'.ps'
PSOPEN,file=psfile,MARGIN=1500,XOFFSET=1000,YOFFSET=1000
GSET,XMIN=0,XMAX=ndays_per_year*n_years,YMIN=0,YMAX=5
GPLOT,X=indgen(ndays_per_year*n_years)+0.5,Y=amplitude_ts,THICK=70
GPLOT,X=indgen(ndays_per_year*n_years-running_mean_length)+running_mean_length/2+0.5,Y=amplitude_smoothed(running_mean_length/2:ndays_per_year*n_years-running_mean_length/2-1),THICK=150,COL=FSC_COLOR('red')
GPLOT,X=indgen(ndays_per_year*n_years-running_mean_length)+running_mean_length/2+0.5,Y=amplitude_variance,THICK=150,COL=FSC_COLOR('blue')

GPLOT,X=ndays_per_year/2,Y=4.8,TEXT='Amplitude (daily)',ALIGN=0
GPLOT,X=ndays_per_year/2,Y=4.6,TEXT='Max: '+STRMID(STRTRIM(STRING(MAX(amplitude_ts,/NaN)),1),0,4),ALIGN=0
GPLOT,X=ndays_per_year/2,Y=4.4,TEXT='Min: '+STRMID(STRTRIM(STRING(MIN(amplitude_ts,/NaN)),1),0,4),ALIGN=0
GPLOT,X=ndays_per_year/2,Y=4.2,TEXT='Mean: '+STRMID(STRTRIM(STRING(MEAN(amplitude_ts,/NaN)),1),0,4),ALIGN=0
GPLOT,X=ndays_per_year/2,Y=4.0,TEXT='Std.dev.: '+STRMID(STRTRIM(STRING(STDDEV(amplitude_ts,/NaN)),1),0,4),ALIGN=0

GPLOT,X=ndays_per_year*(n_years/3),Y=4.8,TEXT='Amplitude (smoothed, '+STRTRIM(STRING(running_mean_length),1)+' days)',ALIGN=0
GPLOT,X=ndays_per_year*(n_years/3),Y=4.6,TEXT='Max: '+STRMID(STRTRIM(STRING(MAX(amplitude_smoothed,/NaN)),1),0,4),ALIGN=0
GPLOT,X=ndays_per_year*(n_years/3),Y=4.4,TEXT='Min: '+STRMID(STRTRIM(STRING(MIN(amplitude_smoothed,/NaN)),1),0,4),ALIGN=0
GPLOT,X=ndays_per_year*(n_years/3),Y=4.2,TEXT='Mean: '+STRMID(STRTRIM(STRING(MEAN(amplitude_smoothed,/NaN)),1),0,4),ALIGN=0
GPLOT,X=ndays_per_year*(n_years/3),Y=4.0,TEXT='Std.dev.: '+STRMID(STRTRIM(STRING(STDDEV(amplitude_smoothed,/NaN)),1),0,4),ALIGN=0

GPLOT,X=ndays_per_year*(n_years*2/3),Y=4.8,TEXT='Variance windowed '+STRTRIM(STRING(running_mean_length),1)+' days',ALIGN=0
GPLOT,X=ndays_per_year*(n_years*2/3),Y=4.6,TEXT='Max: '+STRMID(STRTRIM(STRING(MAX(amplitude_variance,/NaN)),1),0,4),ALIGN=0
GPLOT,X=ndays_per_year*(n_years*2/3),Y=4.4,TEXT='Min: '+STRMID(STRTRIM(STRING(MIN(amplitude_variance,/NaN)),1),0,4),ALIGN=0
GPLOT,X=ndays_per_year*(n_years*2/3),Y=4.2,TEXT='Mean: '+STRMID(STRTRIM(STRING(MEAN(amplitude_variance,/NaN)),1),0,4),ALIGN=0
GPLOT,X=ndays_per_year*(n_years*2/3),Y=4.0,TEXT='Std.dev.: '+STRMID(STRTRIM(STRING(STDDEV(amplitude_variance,/NaN)),1),0,4),ALIGN=0

AXES,XSTEP=ndays_per_year*(n_years/30+1),YSTEP=0.2,YMINOR=0.1,XTITLE='Day',YTITLE='Amplitude',NDECS=2,ORIENTATION=45
PSCLOSE,/NOVIEW

amplitude_clim=fltarr(ndays_per_year)
FOR i=0,ndays_per_year-1 DO $
   amplitude_clim(i)=MEAN(amplitude(*,i),/NaN)

amplitude_ndays=fltarr(ndays_per_year)
FOR i=0,ndays_per_year-1 DO BEGIN
   IF TOTAL(where(amplitude(*,i) ge 1)) ge 0 THEN $
      amplitude_ndays(i)=N_ELEMENTS(where(amplitude(*,i) ge 1))
ENDFOR

psfile='/home/ss901165/idl/mjo_indices/mjo_indices_activity_timeseries.seasonal_cycle.'+run_id+'.ps'
PSOPEN,file=psfile,MARGIN=2500,XOFFSET=500,YOFFSET=1000
GSET,XMIN=0,XMAX=ndays_per_year,YMIN=0.5,YMAX=2
GPLOT,X=indgen(ndays_per_year)+0.5,Y=SMOOTH(amplitude_clim,5),COL=FSC_COLOR('blue')
AXES,XSTEP=20,YSTEP=0.1,YMINOR=0.05,NDECS=2,XMINOR=10,XTITLE='Day of year',YTITLE='Climatological daily amplitude',/NORIGHT,ORIENTATION=40
GSET,XMIN=0,XMAX=ndays_per_year,YMIN=0,YMAX=1
GPLOT,X=indgen(ndays_per_year)+0.5,Y=SMOOTH(amplitude_ndays,5)/FLOAT(n_years),COL=FSC_COLOR('red')
AXES,YSTEP=0.1,YMINOR=0.05,YTITLE='Fraction of years with strong MJO on this date',/ONLYRIGHT,NDECS=2
GLEGEND,labels=['Fraction of years','Mean amplitude'],COL=[FSC_COLOR('red'),FSC_COLOR('blue')],LEGPOS=3
PSCLOSE,/NOVIEW

ndays_per_phase=fltarr(9,ndays_per_year*n_years)
FOR i=running_mean_length/2,ndays_per_year*n_years-running_mean_length/2-1 DO BEGIN
   FOR j=1,8 DO $
      ndays_per_phase(j,i)=N_ELEMENTS(where(phase_ts(i-running_mean_length/2:i+running_mean_length/2-1) eq j and $
                                            amplitude_ts(i-running_mean_length/2:i+running_mean_length/2-1) ge 1))/FLOAT(running_mean_length)
   ndays_per_phase(0,i)=N_ELEMENTS(where(amplitude_ts(i-running_mean_length/2:i+running_mean_length/2-1) lt 1))/FLOAT(running_mean_length)
ENDFOR

psfile='/home/ss901165/idl/mjo_indices/mjo_indices_activity_timeseries.ndays_'+STRTRIM(STRING(running_mean_length),1)+'.'+run_id+'.ps'
PSOPEN,file=psfile,MARGIN=2500,XOFFSET=500,YOFFSET=1000,TFONT=6,FONT=6,CHARSIZE=150
GSET,XMIN=0,XMAX=ndays_per_year*n_years,YMIN=0,YMAX=0.8
CS,SCALE=28,NCOLS=9
FOR i=0,8 DO $
   GPLOT,X=findgen(ndays_per_year*n_years)+0.5,Y=SMOOTH(REFORM(ndays_per_phase(i,*)),running_mean_length,/NaN),COL=i+1
AXES,YSTEP=0.05,YMINOR=0.025,YTITLE='Fraction of days in '+STRTRIM(STRING(running_mean_length),1)+'-day window',XTITLE='Day in integration',NDECS=2,$
     XSTEP=ndays_per_year,XMINOR=ndays_per_year/2,ORIENTATION=40
GLEGEND,labels=REVERSE(['Phase 0','Phase 1','Phase 2','Phase 3','Phase 4','Phase 5','Phase 6','Phase 7','Phase 8']),$
        COL=REVERSE(indgen(9)+1),LEGPOS=9
PSCLOSE

STOP
END

