PRO mjo_indices_wh04_propagation_probability,input_file,runid,time_per_phase,grace_per_phase

; Read AMP and RMM2
amp=OPEN_AND_EXTRACT(input_file,'amplitude')
phase=OPEN_AND_EXTRACT(input_file,'phase')

s=SIZE(amp)
nyear=MIN([s(1),s(2)])
nday=MAX([s(1),s(2)])
IF s(0) eq 2 THEN BEGIN
   amp_ts=fltarr(s(4))
   phase_ts=fltarr(s(4))
   FOR i=0,nyear-1 DO BEGIN
      FOR j=0,nday-1 DO BEGIN
         IF s(2) gt s(1) THEN BEGIN
            amp_ts(i*nday+j)=amp(i,j)
            phase_ts(i*nday+j)=phase(i,j)
         ENDIF ELSE BEGIN
            amp_ts(i*nday+j)=amp(j,i)
            phase_ts(i*nday+j)=phase(j,i)
         ENDELSE
      ENDFOR
   ENDFOR
ENDIF

amp_ts[where(amp_ts lt -900)]=!Values.F_NaN
phase_ts[where(phase_ts lt -900)]=!Values.F_NaN

probabilities=fltarr(8,8)
n_events=fltarr(8)

FOR i=0,7 DO BEGIN ; starting phase
   days_thisphase=where(amp_ts ge 1 and phase eq i+1)
   last_considered=(-1*time_per_phase)-1
   FOR k=1,N_ELEMENTS(days_thisphase)-1 DO BEGIN
      IF days_thisphase(k) gt last_considered+time_per_phase and days_thisphase(k) lt s(4)-1 THEN BEGIN
         n_events(i)=n_events(i)+1
         target_flag=intarr(8)
         target_flag(*)=0
         FOR j=1,7 DO BEGIN     ; target phase=j+i+1
            target=j+i+1
            IF target gt 8 THEN target=target-8       
            max=time_per_phase*j
            grace=grace_per_phase*j
            lowamp_count=0
            day_count=1
            stop_flag=0
            WHILE stop_flag eq 0 DO BEGIN
               IF day_count eq max or days_thisphase(k)+day_count ge s(4)-1 THEN stop_flag=1
               IF amp_ts(days_thisphase(k)+day_count) lt 1 THEN BEGIN
                  lowamp_count=lowamp_count+1
                  IF lowamp_count gt grace THEN stop_flag=1
               ENDIF ELSE BEGIN
                  IF phase_ts(days_thisphase(k)+day_count) eq target and target_flag(j) eq 0 THEN BEGIN
                     target_flag(j)=1
                     IF j eq 1 THEN BEGIN
                        probabilities(i,j)=probabilities(i,j)+1
                     ENDIF ELSE IF TOTAL(target_flag(1:j-1)) eq j-1 THEN $
                        probabilities(i,j)=probabilities(i,j)+1
                  ENDIF                     
               ENDELSE
               day_count=day_count+1
            ENDWHILE
            last_considered=days_thisphase(k)
         ENDFOR
      ENDIF
   ENDFOR
ENDFOR

psfile='/home/ss901165/idl/mjo_indices/mjo_indices_wh04_propagation_probability.'+runid+'_'+STRTRIM(STRING(time_per_phase),1)+'perphase_'+$
       STRTRIM(STRING(grace_per_phase),1)+'grace.ps'
PSOPEN,file=psfile,FONT=2,TFONT=2,CHARSIZE=120,SPACE2=800,MARGIN=2000

GSET,XMIN=0.5,XMAX=8.5,YMIN=0.5,YMAX=8.5
mylevs=['0.05','0.10','0.15','0.20','0.25','0.30','0.35','0.40','0.45','0.50','0.55','0.60','0.65','0.70','0.75']
CS,SCALE=25,NCOLS=N_ELEMENTS(mylevs)+1
LEVS,MANUAL=mylevs
FOR i=0,7 DO $
   probabilities(i,*)=REFORM(probabilities(i,*))/FLOAT(n_events)
toplot=fltarr(8,8)
FOR i=0,7 DO BEGIN
   FOR j=0,7 DO BEGIN
      IF i eq j THEN BEGIN
         toplot(i,j)=!Values.F_NaN
      ENDIF ELSE BEGIN
         IF j lt i THEN BEGIN
            prob_phase=8-(i-j)
         ENDIF ELSE $
            prob_phase=j-i
         toplot(i,j)=probabilities(i,prob_phase)
      ENDELSE
   ENDFOR
ENDFOR
CON,X=indgen(8)+1,Y=indgen(8)+1,FIELD=toplot*1.5,/BLOCK,/NOLINES,TITLE='Probability of propagation with amp >1 - '+$
    STRTRIM(STRING(time_per_phase),1)+' days/phase to transition with '+STRTRIM(STRING(grace_per_phase),1)+' days/phase grace on amp - '+runid
AXES,XVALS=indgen(8)+1,YVALS=indgen(8)+1,XTITLE='Starting phase',YTITLE='Target phase'
PSCLOSE

STOP
END
