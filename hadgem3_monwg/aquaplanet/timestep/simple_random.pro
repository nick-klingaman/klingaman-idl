PRO simple_random

file='/home/ss901165/um_output3/hadgem3_monwg/anbba/anbbaa.jun-sep_tsmeans.1982.dcvrain.nc'
n_time=10000
model_precip=REFORM(OPEN_AND_EXTRACT(file,'precip_1',$
                              offset=[80,80,0],count=[1,1,n_time]))

lagone_correlation=A_CORRELATE(model_precip,1)

precip_bins=findgen(150)*0.00002+0.00002
n_bins=N_ELEMENTS(precip_bins)
precip_prob=fltarr(n_bins+1)
mean_precip=fltarr(n_bins+1)
FOR i=1,n_bins-1 DO BEGIN
   valid=where(model_precip gt precip_bins(i-1) and model_precip le precip_bins(i))
   IF TOTAL(valid) ge 0 THEN BEGIN
      precip_prob(i)=N_ELEMENTS(valid)
      mean_precip(i)=MEAN(model_precip[valid])
   ENDIF ELSE $
      mean_precip(i)=mean_precip(i-1)
ENDFOR
precip_prob(0)=N_ELEMENTS(where(model_precip eq 0))/FLOAT(n_time)
mean_precip(0)=0.
precip_prob(n_bins)=N_ELEMENTS(where(model_precip ge precip_bins(n_bins-1)))
mean_precip(n_bins)=MEAN(model_precip(where(model_precip ge precip_bins(n_bins-1))))

precip_cdf=fltarr(n_bins+1)
temp=TOTAL(precip_prob(1:n_bins))
FOR i=2,n_bins DO $
   precip_cdf(i)=TOTAL(precip_prob(1:i-1))/temp
precip_cdf(1)=0.
precip_cdf(0)=-999

p_wet=1.-precip_prob(0)
p_dry=precip_prob(0)
p_wetwet=0.
p_drydry=0.
FOR i=1,n_time-1 DO BEGIN
   IF (model_precip(i) gt 0 and model_precip(i-1) gt 0) THEN $
      p_wetwet=p_wetwet+1
   IF (model_precip(i) eq 0 and model_precip(i-1) eq 0) THEN $
      p_drydry=p_drydry+1
ENDFOR
p_drydry=p_drydry/FLOAT(n_time)
p_wetwet=p_wetwet/FLOAT(n_time)
p_wetdry=p_wet-p_wetwet
p_drywet=p_dry-p_drydry

random_ts=fltarr(n_time)
prob=p_wet
FOR i=0,n_time-1 DO BEGIN
   wet=RANDOMU(seed,1,binomial=[1,prob])
   IF wet eq 1 THEN BEGIN
      temp=RANDOMU(seed,1)
      random_ts(i)=mean_precip(NEAREST(precip_cdf,temp(0)))
      prob=p_wetwet  
   ENDIF ELSE BEGIN
      random_ts(i)=0.
      prob=p_wetdry
   ENDELSE
ENDFOR
   

STOP
END

