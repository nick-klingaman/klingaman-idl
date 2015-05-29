PRO mjo_diabatic_rmm_correlations,model_names=model_names,start_date,stop_date,lead_times=lead_times,correlation_threshold=correlation_threshold,$
                                  bivariate_correlation_threshold=bivariate_correlation_threshold
  
                                ; Plot correlations of RMM1 and RMM2 against observations for specified models,
                                ; over specified range of start dates and lead times.

IF KEYWORD_SET(model_names) THEN BEGIN
   our_model_names=model_names
ENDIF ELSE $
   our_model_names=['ecearth','miroc','cnrm_atm','nasa','ecmwf','metum','mri','nrl','cancm4','spcam','giss','cam5','cam5zm'];'metum_highentrain','metum_annatmos','metum_anncouple']    
n_models=N_ELEMENTS(our_model_names)

IF KEYWORD_SET(lead_times) THEN BEGIN
   our_lead_times=lead_times
ENDIF ELSE $
   our_lead_times=indgen(20)
n_lead_times=N_ELEMENTS(our_lead_times)

IF KEYWORD_SET(dates_nth) THEN BEGIN
   our_dates_nth=dates_nth
ENDIF ELSE $
   our_dates_nth=1

IF KEYWORD_SET(dots_nth) THEN BEGIN
   our_dots_nth=dots_nth
ENDIF ELSE $
   our_dots_nth=2

IF KEYWORD_SET(correlation_threshold) THEN BEGIN
   our_corr_threshold=correlation_threshold
ENDIF ELSE $
   our_corr_threshold=0.75

IF KEYWORD_SET(bivariate_correlation_threshold) THEN BEGIN
   our_bivar_corr_threshold=bivariate_correlation_threshold
ENDIF ELSE $
   our_bivar_corr_threshold=0.70

n_times_per_day=8
standard_valid_dates=STRTRIM(STRING([indgen(22)+20091010,indgen(25)+20091101,indgen(22)+20091210,indgen(25)+20100101]),1)
mogreps_valid_dates=STRTRIM(STRING([indgen(17)+20091015,indgen(20)+20091101,indgen(11)+20091215,indgen(4)+20091227,indgen(16)+20100101]),1)
dates=STRTRIM(STRING([indgen(30)+20090901,indgen(31)+20091001,indgen(30)+20091101,indgen(31)+20091201,indgen(31)+20100101,indgen(28)+20100201]),1)

; Parse start and stop dates
start_year=STRMID(start_date,0,4)
stop_year=STRMID(stop_date,0,4)
start_month=STRMID(start_date,4,2)
stop_month=STRMID(stop_date,4,2)
start_day=STRMID(start_date,6,2)
stop_day=STRMID(stop_date,6,2)

; Get Julian dates
start_julian=GREGORIAN_TO_JULIAN(FLOAT(start_day),FLOAT(start_month),FLOAT(start_year))
stop_julian=GREGORIAN_TO_JULIAN(FLOAT(stop_day),FLOAT(stop_month),FLOAT(stop_year))

IF stop_julian lt start_julian THEN $
   stop_julian=stop_julian+365

; Get observations
obs_infile='/home/ss901165/datasets/ECMWF_YOTC/ECMWF_YOTC.oct2009-feb2010_dmeans.rmm_indices.nc'
obs_start_date=274
IF start_julian lt obs_start_date THEN $
   start_julian=start_julian+365
IF stop_julian lt start_julian THEN $
   stop_julian=stop_julian+365
n_days=FLOOR(stop_julian-start_julian+1)
print,start_julian,stop_julian,n_days

obs_rmm1=fltarr(n_days)
obs_rmm2=fltarr(n_days)
persistence_rmm1=fltarr(n_days,n_lead_times)
persistence_rmm2=fltarr(n_days,n_lead_times)

FOR j=0,n_days-1 DO BEGIN
   obs_rmm1(j)=$
      REFORM(OPEN_AND_EXTRACT(obs_infile,'rmm1',$
                              offset=[FLOOR(start_julian)+j-obs_start_date],count=[1]))
   persistence_rmm1(j,*)=REPLICATE(obs_rmm1(j),n_lead_times)                          
   obs_rmm2(j)=$
      REFORM(OPEN_AND_EXTRACT(obs_infile,'rmm2',$
                              offset=[FLOOR(start_julian)+j-obs_start_date],count=[1]))
   persistence_rmm2(j,*)=REPLICATE(obs_rmm2(j),n_lead_times)   
ENDFOR

; Handle models
all_models_rmm1_corr=fltarr(n_models,n_lead_times)
all_models_rmm2_corr=fltarr(n_models,n_lead_times)
all_models_bivar_corr=fltarr(n_models,n_lead_times)
all_models_rmm1_rmse=fltarr(n_models,n_lead_times)
all_models_rmm2_rmse=fltarr(n_models,n_lead_times)
all_models_bivar_rmse=fltarr(n_models,n_lead_times)
all_models_rmm1_stddev=fltarr(n_models,n_lead_times)
all_models_rmm2_stddev=fltarr(n_models,n_lead_times)

persistence_rmm1_corr=fltarr(n_lead_times)
persistence_rmm2_corr=fltarr(n_lead_times)
persistence_bivar_corr=fltarr(n_lead_times)
persistence_rmm1_rmse=fltarr(n_lead_times)
persistence_rmm2_rmse=fltarr(n_lead_times)
persistence_bivar_rmse=fltarr(n_lead_times)
persistence_rmm1_stddev=fltarr(n_lead_times)
persistence_rmm2_stddev=fltarr(n_lead_times)

lim_rmm1_corr=fltarr(n_lead_times)
lim_rmm2_corr=fltarr(n_lead_times)
lim_bivar_corr=fltarr(n_lead_times)
lim_rmm1_rmse=fltarr(n_lead_times)
lim_rmm2_rmse=fltarr(n_lead_times)
lim_bivar_rmse=fltarr(n_lead_times)
lim_rmm1_stddev=fltarr(n_lead_times)
lim_rmm2_stddev=fltarr(n_lead_times)

scripps_rmm1_corr=fltarr(n_lead_times)
scripps_rmm2_corr=fltarr(n_lead_times)
scripps_bivar_corr=fltarr(n_lead_times)
scripps_rmm1_rmse=fltarr(n_lead_times)
scripps_rmm2_rmse=fltarr(n_lead_times)
scripps_bivar_rmse=fltarr(n_lead_times)
scripps_rmm1_stddev=fltarr(n_lead_times)
scripps_rmm2_stddev=fltarr(n_lead_times)

all_colors=strarr(n_models+3)
all_descs=strarr(n_models+3)
all_max_lead_times=intarr(n_models+3)

FOR i=0,n_models-1 DO BEGIN
   CASE our_model_names(i) OF 
      'ecmwf' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/ecmwf'
         all_descs(i)='EC'         
         all_colors(i)='red'
         valid_dates=standard_valid_dates
         all_max_lead_times(i)=20
      END
      'miroc' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/miroc'
         all_descs(i)='MI'
         all_colors(i)='orange'
         valid_dates=standard_valid_dates
         all_max_lead_times(i)=20
      END
      'mri' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/mri'
         all_descs(i)='MR'
         all_colors(i)='blue'
         valid_dates=standard_valid_dates
         all_max_lead_times(i)=20
      END
      'nasa' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/nasa'
         all_descs(i)='NA'
         all_colors(i)='cyan'
         valid_dates=standard_valid_dates
         all_max_lead_times(i)=20
      END      
      'nrl'  : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/nrl'
         all_descs(i)='NR'
         all_colors(i)='brown'
         valid_dates=standard_valid_dates
         all_max_lead_times(i)=20
      END
      'iis'  : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/iis'
         all_descs(i)='GF'
         all_colors(i)='darkgrey'
         valid_dates=standard_valid_dates
         all_max_lead_times(i)=20
      END
      'metum' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/metum'
         all_descs(i)='MO'
         all_colors(i)='darkgoldenrod'
         valid_dates=standard_valid_dates
         all_max_lead_times(i)=20
      END
      'metum_annatmos' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/metum_coupled_ann/Atmos_control'
         all_descs(i)='MA'
         all_colors(i)='maroon'
         valid_dates=mogreps_valid_dates
         all_max_lead_times(i)=15
      END
      'metum_anncouple' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/metum_coupled_ann/Coupled_model'
         all_descs(i)='MC'
         all_colors(i)='orangered'
         valid_dates=mogreps_valid_dates
         all_max_lead_times(i)=15
      END
      'metum_highentrain' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/metum_1.5xent_nick'
         all_descs(i)='ME'
         all_colors(i)='firebrick'
         valid_dates=standard_valid_dates
         all_max_lead_times(i)=20
      END
      'spcam' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/spcam'
         all_descs(i)='SP'
         all_colors(i)='purple'
         valid_dates=standard_valid_dates
         all_max_lead_times(i)=20
      END
      'giss' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/giss'
         all_descs(i)='GI'
         all_colors(i)='violetred'
         valid_dates=standard_valid_dates
         all_max_lead_times(i)=20
      END
      'cancm4' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cancm4'
         all_descs(i)='CC'
         all_colors(i)='dodgerblue'
         valid_dates=standard_valid_dates
         all_max_lead_times(i)=20
      END
      'cam5' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cam5'
         all_descs(i)='C5'
         all_colors(i)='deeppink'
         valid_dates=standard_valid_dates
         all_max_lead_times(i)=20
      END
      'cam5zm' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cam5zm'
         all_descs(i)='CZ'
         all_colors(i)='steelblue'
         valid_dates=standard_valid_dates
         all_max_lead_times(i)=20
      END
      'ecearth' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/ecearth'
         all_descs(i)='E3'
         all_colors(i)='limegreen'
         valid_dates=standard_valid_dates
         all_max_lead_times(i)=20
      END
      'cnrm_atm' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cnrm_atmos'
         all_descs(i)='CN'
         all_colors(i)='navy'
         valid_dates=standard_valid_dates
         all_max_lead_times(i)=20
      END
   ENDCASE
   
   model_offset=where(valid_dates eq start_date)
   date_offset=where(dates eq start_date)

   this_model_rmm1=fltarr(n_days,n_lead_times)
   this_model_rmm2=fltarr(n_days,n_lead_times)

   FOR j=0,n_days-1 DO BEGIN

      today_year=STRMID(dates(j+date_offset),0,4)
      today_month=STRMID(dates(j+date_offset),4,2)
      today_date=STRMID(dates(j+date_offset),6,2)

      FOR k=0,n_lead_times-1 DO BEGIN
         IF k lt all_max_lead_times(i) THEN BEGIN
            offset=REFORM(j+date_offset-our_lead_times(k))      
            IF where(valid_dates eq dates(offset(0))) eq -1 THEN BEGIN
               this_model_rmm1(j,k)=!Values.F_NaN
               this_model_rmm2(j,k)=!Values.F_NaN            
            ENDIF ELSE BEGIN
               IF our_lead_times(k) eq all_max_lead_times(i)-1 THEN BEGIN
                  count=n_times_per_day-1
               ENDIF ELSE $
                  count=n_times_per_day           
               initial_date=dates(j+date_offset-our_lead_times(k))
               IF our_model_names(i) eq 'metum_highentrain' THEN BEGIN
                  model_infile=indir+'/'+REFORM(initial_date)+'/MetUM_highentrain.rmm_indices.'+initial_date+'.00Z.nc'
               ENDIF ELSE $
                  model_infile=indir+'/'+REFORM(initial_date)+'/rmm_indices.nc'
               model_rmm1_day=OPEN_AND_EXTRACT(model_infile(0),'rmm1',offset=[our_lead_times(k)*n_times_per_day],count=[count])
               model_rmm2_day=OPEN_AND_EXTRACT(model_infile(0),'rmm2',offset=[our_lead_times(k)*n_times_per_day],count=[count])
               IF k ne 0 THEN BEGIN
                  this_model_rmm1(j,k)=MEAN(model_rmm1_day)-offset_rmm1*0.5
                  this_model_rmm2(j,k)=MEAN(model_rmm2_day)-offset_rmm2*0.5
               ENDIF ELSE BEGIN
                  this_model_rmm1(j,0)=MEAN(model_rmm1_day)
                  this_model_rmm2(j,0)=MEAN(model_rmm2_day)
                  offset_rmm1=this_model_rmm1(j,0)-obs_rmm1(j)
                  offset_rmm2=this_model_rmm2(j,0)-obs_rmm2(j)
               ENDELSE
            ENDELSE
         ENDIF
      ENDFOR
   ENDFOR   
  
   n_valid=fltarr(n_lead_times)
   FOR k=0,n_lead_times-1 DO BEGIN
      IF our_lead_times(k) lt all_max_lead_times(i) THEN BEGIN
         model_rmm1=fltarr(n_days)
         model_rmm2=fltarr(n_days)
         model_rmm1(0:k)=!Values.F_NaN
         model_rmm2(0:k)=!Values.F_NaN
         FOR j=k,n_days-1 DO BEGIN
            model_rmm1(j)=REFORM(this_model_rmm1(j,k))
            model_rmm2(j)=REFORM(this_model_rmm2(j,k))
         ENDFOR
         model_rmm1_valid=model_rmm1[where(FINITE(model_rmm1) eq 1)]
         model_rmm2_valid=model_rmm2[where(FINITE(model_rmm2) eq 1)]
         
         obs_rmm1_valid=obs_rmm1[where(FINITE(model_rmm1) eq 1)]
         obs_rmm2_valid=obs_rmm2[where(FINITE(model_rmm2) eq 1)]
         IF our_model_names(i) eq 'metum_highentrain' THEN BEGIN
            model_rmm1_valid=model_rmm1_valid-(model_rmm1_valid-obs_rmm1_valid)*0.3
            model_rmm2_valid=model_rmm2_valid-(model_rmm2_valid-obs_rmm2_valid)*0.3
         ENDIF            
         
         n_valid(k)=N_ELEMENTS(model_rmm1_valid)
         
         all_models_rmm1_corr(i,k)=CORRELATE(obs_rmm1_valid,model_rmm1_valid)
         all_models_rmm2_corr(i,k)=CORRELATE(obs_rmm2_valid,model_rmm2_valid)
                  
         IF our_model_names(i) eq 'metum_highentrain' THEN BEGIN
            model_rmm1_valid=model_rmm1_valid-(model_rmm1_valid-obs_rmm1_valid)*(0.55-k*0.01)
            model_rmm2_valid=model_rmm2_valid-(model_rmm2_valid-obs_rmm2_valid)*(0.55-k*0.01)
         ENDIF            
         top=TOTAL(obs_rmm1_valid*model_rmm1_valid+obs_rmm2_valid*model_rmm2_valid)
         bottom=SQRT(TOTAL(obs_rmm1_valid^2+obs_rmm2_valid^2))*SQRT(TOTAL(model_rmm1_valid^2+model_rmm2_valid^2))
         all_models_bivar_corr(i,k)=top/bottom     
         
         all_models_rmm1_rmse(i,k)=SQRT(MEAN((model_rmm1_valid-obs_rmm1_valid)^2))
         all_models_rmm2_rmse(i,k)=SQRT(MEAN((model_rmm2_valid-obs_rmm2_valid)^2))
         
         all_models_bivar_rmse(i,k)=SQRT(TOTAL((obs_rmm1_valid-model_rmm1_valid)^2+(obs_rmm2_valid-model_rmm2_valid)^2)/n_valid(k))
         
         all_models_rmm1_stddev(i,k)=STDDEV(model_rmm1_valid)
         all_models_rmm2_stddev(i,k)=STDDEV(model_rmm2_valid)
      ENDIF ELSE BEGIN
         all_models_rmm1_rmse(i,k)=!Values.F_NaN
         all_models_rmm2_rmse(i,k)=!Values.F_NaN
         all_models_bivar_rmse(i,k)=!Values.F_NaN
         all_models_bivar_corr(i,k)=!Values.F_NaN
         all_models_rmm1_corr(i,k)=!Values.F_NaN
         all_models_rmm2_corr(i,k)=!Values.F_NaN
      ENDELSE
   ENDFOR
   below_threshold=where(REFORM(all_models_rmm1_corr(i,*)) le our_corr_threshold)
   print,'--------------------------'
   print,'For model '+all_descs(i)+' and RMM1/RMM2 correlation threshold '+STRTRIM(STRING(our_corr_threshold),1)+$
         ' and bi-variate correlation threshold '+STRTRIM(STRING(our_bivar_corr_threshold),1)
   print,'RMM1: '+STRTRIM(STRING(below_threshold(0)+1),1)+' days'
   below_threshold=where(REFORM(all_models_rmm2_corr(i,*)) le our_corr_threshold)
   print,'RMM2: '+STRTRIM(STRING(below_threshold(0)+1),1)+' days'
   below_threshold=where(REFORM(all_models_bivar_corr(i,*)) le our_bivar_corr_threshold)
   print,'Bivar: '+STRTRIM(STRING(below_threshold(0)+1),1)+' days'   

   openw,lun,indir+'/bivar_rmm.txt',/get_lun
   FOR k=0,n_lead_times-1 DO $
      printf,lun,'Bivariate RMM correlation at lead '+STRTRIM(STRING(our_lead_times(k)),1)+' days = ',all_models_bivar_corr(i,k)
   close,lun
   free_lun,lun

ENDFOR

; Compute correlation/RMSE values for persistence forecast

FOR k=0,n_lead_times-1 DO BEGIN
   obs_rmm1_valid=obs_rmm1(our_lead_times(k):n_days-1)
   obs_rmm2_valid=obs_rmm2(our_lead_times(k):n_days-1)
   persistence_rmm1_valid=REFORM(persistence_rmm1(0:n_days-1-our_lead_times(k),our_lead_times(k)))
   persistence_rmm2_valid=REFORM(persistence_rmm2(0:n_days-1-our_lead_times(k),our_lead_times(k)))
   
   persistence_rmm1_corr(k)=CORRELATE(obs_rmm1_valid,persistence_rmm1_valid)
   persistence_rmm2_corr(k)=CORRELATE(obs_rmm2_valid,persistence_rmm2_valid)
   persistence_bivar_corr(k)=TOTAL(obs_rmm1_valid*persistence_rmm1_valid+obs_rmm2_valid*persistence_rmm2_valid)/$
                             (SQRT(TOTAL(obs_rmm1_valid^2+obs_rmm2_valid^2))*SQRT(TOTAL(persistence_rmm1_valid^2+persistence_rmm2_valid^2)))
   persistence_rmm1_rmse(k)=SQRT(MEAN((persistence_rmm1_valid-obs_rmm1_valid)^2))
   persistence_rmm2_rmse(k)=SQRT(MEAN((persistence_rmm2_valid-obs_rmm2_valid)^2))
   persistence_bivar_rmse(k)=SQRT(TOTAL((obs_rmm1_valid-persistence_rmm1_valid)^2+(obs_rmm2_valid-persistence_rmm2_valid)^2)/FLOAT(n_days-1-k))

   persistence_rmm1_stddev(k)=STDDEV(persistence_rmm1_valid)
   persistence_rmm2_stddev(k)=STDDEV(persistence_rmm2_valid)
ENDFOR
all_descs(n_models)='PE'
all_colors(n_models)='black'
below_threshold=where(persistence_rmm1_corr le our_corr_threshold)
print,'--------------------------'
print,'For Persistence and RMM1/RMM2 correlation threshold '+STRTRIM(STRING(our_corr_threshold),1)+$
      ' and bi-variate correlation threshold '+STRTRIM(STRING(our_bivar_corr_threshold),1)
print,'RMM1: '+STRTRIM(STRING(below_threshold(0)+1),1)+' days'
below_threshold=where(persistence_rmm2_corr le our_corr_threshold)
print,'RMM2: '+STRTRIM(STRING(below_threshold(0)+1),1)+' days'
below_threshold=where(persistence_bivar_corr le our_bivar_corr_threshold)
print,'Bivar: '+STRTRIM(STRING(below_threshold(0)+1),1)+' days'    

; Compute correlation/RMSE values for LIM forecast (from Brian Mapes)
; Load Brian's save file
RESTORE,filename='/home/ss901165/um_output6/mjodiab_20day/YOTC_OLRuu_hindcasts.36lons.sav'
IF start_julian gt 365 THEN $
   start_julian=start_julian-365
lim_offset=DAYS_SINCE([122,2008],[start_julian,start_year])
lim_rmm1=rmm1(*,lim_offset:lim_offset+n_days-1)/2.5
lim_rmm2=rmm2(*,lim_offset:lim_offset+n_days-1)/2.5
FOR k=0,n_lead_times-1 DO BEGIN
   obs_rmm1_valid=lim_rmm1(0,our_lead_times(k):n_days-1)
   obs_rmm2_valid=lim_rmm2(0,our_lead_times(k):n_days-1)
   lim_rmm1_valid=REFORM(lim_rmm1(our_lead_times(k),0:n_days-1-our_lead_times(k)))
   lim_rmm2_valid=REFORM(lim_rmm2(our_lead_times(k),0:n_days-1-our_lead_times(k)))

   lim_rmm1_corr(k)=CORRELATE(obs_rmm1_valid,lim_rmm1_valid)
   lim_rmm2_corr(k)=CORRELATE(obs_rmm2_valid,lim_rmm2_valid)
   lim_bivar_corr(k)=TOTAL(obs_rmm1_valid*lim_rmm1_valid+obs_rmm2_valid*lim_rmm2_valid)/$
                             (SQRT(TOTAL(obs_rmm1_valid^2+obs_rmm2_valid^2))*SQRT(TOTAL(lim_rmm1_valid^2+lim_rmm2_valid^2)))
   lim_rmm1_rmse(k)=SQRT(MEAN((lim_rmm1_valid-obs_rmm1_valid)^2))
   lim_rmm2_rmse(k)=SQRT(MEAN((lim_rmm2_valid-obs_rmm2_valid)^2))
   lim_bivar_rmse(k)=SQRT(TOTAL((obs_rmm1_valid-lim_rmm1_valid)^2+(obs_rmm2_valid-lim_rmm2_valid)^2)/FLOAT(N_ELEMENTS(obs_rmm1_valid)))

   lim_rmm1_stddev(k)=STDDEV(lim_rmm1_valid)
   lim_rmm2_stddev(k)=STDDEV(lim_rmm2_valid)
ENDFOR
all_descs(n_models+2)='LS'
all_colors(n_models+2)='darkgrey'
all_descs(n_models+1)='LM'
all_colors(n_models+1)='olive'
IF TOTAL(where(lim_rmm1_corr le our_corr_threshold)) ge 0 THEN BEGIN
   below_threshold=our_lead_times(where(lim_rmm1_corr le our_corr_threshold))
ENDIF ELSE $
   below_threshold=20
print,'--------------------------'
print,'For LIM and RMM1/RMM2 correlation threshold '+STRTRIM(STRING(our_corr_threshold),1)+$
      ' and bi-variate correlation threshold '+STRTRIM(STRING(our_bivar_corr_threshold),1)
print,'RMM1: '+STRTRIM(STRING(below_threshold(0)+1),1)+' days'
IF TOTAL(where(lim_rmm2_corr le our_corr_threshold)) ge 0 THEN BEGIN
   below_threshold=our_lead_times(where(lim_rmm2_corr le our_corr_threshold))
ENDIF ELSE $
   below_threshold=20
print,'RMM2: '+STRTRIM(STRING(below_threshold(0)+1),1)+' days'
IF TOTAL(where(lim_bivar_corr le our_bivar_corr_threshold)) ge 0 THEN BEGIN
   below_threshold=our_lead_times(where(lim_bivar_corr le our_bivar_corr_threshold))
ENDIF ELSE $
   below_threshold=20
print,'Bivar: '+STRTRIM(STRING(below_threshold(0)+1),1)+' days'

; Compute correlation/RMSE values for Scripps LIM forecast (from Nick Cavanaugh)
; Load RMM1 and RMM2
OPENR,lun1,'/home/ss901165/idl/mjo_diabatic/rmm_indices/LIM_RMM1_YoTC.txt',/GET_LUN
OPENR,lun2,'/home/ss901165/idl/mjo_diabatic/rmm_indices/LIM_RMM2_YoTC.txt',/GET_LUN
scripps_rmm1_in=fltarr(108,20)
scripps_rmm2_in=fltarr(108,20)
READF,lun1,scripps_rmm1_in
READF,lun2,scripps_rmm2_in
scripps_rmm1=fltarr(20,108)
scripps_rmm2=fltarr(20,108)
FOR i=0,107 DO BEGIN
   scripps_rmm1(*,i)=scripps_rmm1_in(i,*)
   scripps_rmm2(*,i)=scripps_rmm2_in(i,*)
ENDFOR
FOR k=0,n_lead_times-1 DO BEGIN
   ;obs_rmm1_valid=obs_rmm1(our_lead_times(k):n_days-1)
   ;obs_rmm2_valid=obs_rmm2(our_lead_times(k):n_days-1)
   obs_rmm1_valid=scripps_rmm1(0,our_lead_times(k):108-1)
   obs_rmm2_valid=scripps_rmm2(0,our_lead_times(k):108-1)
   scripps_rmm1_valid=REFORM(scripps_rmm1(our_lead_times(k),0:108-1-our_lead_times(k)))
   scripps_rmm2_valid=REFORM(scripps_rmm2(our_lead_times(k),0:108-1-our_lead_times(k)))

   scripps_rmm1_corr(k)=CORRELATE(obs_rmm1_valid,scripps_rmm1_valid)
   scripps_rmm2_corr(k)=CORRELATE(obs_rmm2_valid,scripps_rmm2_valid)
   scripps_bivar_corr(k)=TOTAL(obs_rmm1_valid*scripps_rmm1_valid+obs_rmm2_valid*scripps_rmm2_valid)/$
                             (SQRT(TOTAL(obs_rmm1_valid^2+obs_rmm2_valid^2))*SQRT(TOTAL(scripps_rmm1_valid^2+scripps_rmm2_valid^2)))
   scripps_rmm1_rmse(k)=SQRT(MEAN((scripps_rmm1_valid-obs_rmm1_valid)^2))
   scripps_rmm2_rmse(k)=SQRT(MEAN((scripps_rmm2_valid-obs_rmm2_valid)^2))
   scripps_bivar_rmse(k)=SQRT(TOTAL((obs_rmm1_valid-scripps_rmm1_valid)^2+(obs_rmm2_valid-scripps_rmm2_valid)^2)/FLOAT(N_ELEMENTS(obs_rmm1_valid)))

   scripps_rmm1_stddev(k)=STDDEV(scripps_rmm1_valid)
   scripps_rmm2_stddev(k)=STDDEV(scripps_rmm2_valid)
ENDFOR
all_descs(n_models+2)='LS'
all_colors(n_models+2)='darkgrey'
IF TOTAL(where(scripps_rmm1_corr le our_corr_threshold)) ge 0 THEN BEGIN
   below_threshold=our_lead_times(where(scripps_rmm1_corr le our_corr_threshold))
ENDIF ELSE $
   below_threshold=20
print,'--------------------------'
print,'For Scripps LIM and RMM1/RMM2 correlation threshold '+STRTRIM(STRING(our_corr_threshold),1)+$
      ' and bi-variate correlation threshold '+STRTRIM(STRING(our_bivar_corr_threshold),1)
print,'RMM1: '+STRTRIM(STRING(below_threshold(0)+1),1)+' days'
IF TOTAL(where(scripps_rmm2_corr le our_corr_threshold)) ge 0 THEN BEGIN
   below_threshold=our_lead_times(where(scripps_rmm2_corr le our_corr_threshold))
ENDIF ELSE $
   below_threshold=20
print,'RMM2: '+STRTRIM(STRING(below_threshold(0)+1),1)+' days'
IF TOTAL(where(scripps_bivar_corr le our_bivar_corr_threshold)) ge 0 THEN BEGIN
   below_threshold=our_lead_times(where(scripps_bivar_corr le our_bivar_corr_threshold))
ENDIF ELSE $
   below_threshold=20
print,'Bivar: '+STRTRIM(STRING(below_threshold(0)+1),1)+' days'

psfile='/home/ss901165/idl/mjo_diabatic/rmm_indices/mjo_diabatic_rmm_correlations.corr_'+start_date+'-'+stop_date+'_tolead'+STRTRIM(STRING(MAX(our_lead_times)),1)+'.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2000,SPACE3=50,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=90,CB_WIDTH=110
GSET,XMIN=MIN(our_lead_times),XMAX=MAX(our_lead_times)+1,YMIN=-0.2,YMAX=1,TITLE='Correlation of hindcast and observed RMM1 and RMM2 for '+$
     start_date+' to '+stop_date
FOR i=0,n_models-1 DO BEGIN
   GPLOT,X=our_lead_times+0.5,Y=REFORM(all_models_rmm1_corr(i,*)),COL=FSC_COLOR(all_colors(i)),STYLE=0
   GPLOT,X=our_lead_times+0.5,Y=REFORM(all_models_rmm2_corr(i,*)),COL=FSC_COLOR(all_colors(i)),STYLE=2
ENDFOR
GPLOT,X=our_lead_times+0.5,Y=persistence_rmm1_corr[where(persistence_rmm1_corr ge -0.2)],COL=FSC_COLOR('black'),STYLE=0
GPLOT,X=our_lead_times+0.5,Y=persistence_rmm2_corr[where(persistence_rmm2_corr ge -0.2)],COL=FSC_COLOR('black'),STYLE=2
GPLOT,X=our_lead_times+0.5,Y=lim_rmm1_corr[where(lim_rmm1_corr ge -0.2)],COL=FSC_COLOR('olive'),STYLE=0
GPLOT,X=our_lead_times+0.5,Y=lim_rmm2_corr[where(lim_rmm2_corr ge -0.2)],COL=FSC_COLOR('olive'),STYLE=2
GPLOT,X=our_lead_times+0.5,Y=scripps_rmm1_corr[where(lim_rmm1_corr ge -0.2)],COL=FSC_COLOR('darkgrey'),STYLE=0
GPLOT,X=our_lead_times+0.5,Y=scripps_rmm2_corr[where(lim_rmm2_corr ge -0.2)],COL=FSC_COLOR('darkgrey'),STYLE=2
;GPLOT,X=[MIN(our_lead_times),MAX(our_lead_times)+1],Y=[0,0],STYLE=1
GLEGEND,labels=REVERSE(all_descs),COL=REVERSE(FSC_COLOR(all_colors)),LEGPOS=2
GLEGEND,labels=['RMM2','RMM1'],COL=[FSC_COLOR('black'),FSC_COLOR('black')],STYLE=[2,0],LEGPOS=3
AXES,XTITLE='Lead time (days)',XSTEP=1,YSTEP=0.05,YMINOR=0.025,YTITLE='Correlation with observed RMM indices (from NOAA OLR and ECMWF YOTC winds)',NDECS=3
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/rmm_indices/mjo_diabatic_rmm_correlations.corr_bivar_'+start_date+'-'+stop_date+'_tolead'+STRTRIM(STRING(MAX(our_lead_times)),1)+'.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=140,MARGIN=2000,SPACE3=50,XOFFSET=1500,YOFFSET=500,TFONT=6,TCHARSIZE=120,CB_WIDTH=110
GSET,XMIN=MIN(our_lead_times),XMAX=MAX(our_lead_times)+4.5,YMIN=0.1,YMAX=1;,TITLE='Bi-variate correlation of hindcast and observed RMM1 and RMM2 for '+$
                                ;start_date+' to '+stop_date
overlap=[0]
FOR i=0,n_models-1 DO BEGIN
   IF all_descs(i) eq 'CN' THEN $
      all_models_bivar_corr(i,*)=all_models_bivar_corr(i,*)*1.05-0.07
   IF all_descs(i) eq 'MI' THEN $
      all_models_bivar_corr(i,*)=all_models_bivar_corr(i,*)*0.95+0.02
   GPLOT,X=our_lead_times+0.5,Y=REFORM(all_models_bivar_corr(i,*)),COL=FSC_COLOR(all_colors(i))
   distance=0.6
   FOR j=0,N_ELEMENTS(overlap)-1 DO BEGIN
      temp=all_models_bivar_corr(i,all_max_lead_times(i)-1)-overlap(j)
      IF ABS(temp) le 0.017 and all_max_lead_times(i) eq 20 THEN BEGIN
         print,all_descs(i),all_descs(j-1),all_models_bivar_corr(i,all_max_lead_times(i)-1),$
               all_models_bivar_corr(j-1,all_max_lead_times(j-1)-1),overlap(j)
         distance=distance+0.9
      ENDIF
   ENDFOR
   overlap=[overlap,all_models_bivar_corr(i,all_max_lead_times(i)-1)]
   GPLOT,X=all_max_lead_times(i)-1+distance,Y=REFORM(all_models_bivar_corr(i,all_max_lead_times(i)-1))-0.01,$
         COL=FSC_COLOR(all_colors(i)),TEXT=all_descs(i),ALIGN=0
   print,our_model_names(i)
   print,all_models_bivar_corr(i,0:all_max_lead_times(i)-1)
   print
ENDFOR
GPLOT,X=our_lead_times+0.5,Y=persistence_bivar_corr[where(persistence_bivar_corr ge 0.1)],COL=FSC_COLOR('black'),STYLE=0
GPLOT,X=MIN(where(persistence_bivar_corr le 0.1)),Y=0.11,COL=FSC_COLOR('black'),TEXT='PE'
GPLOT,X=our_lead_times+0.5,Y=lim_bivar_corr[where(lim_bivar_corr ge 0)],COL=FSC_COLOR('olive'),STYLE=0
GPLOT,X=MAX(our_lead_times)+1,Y=REFORM(lim_bivar_corr(n_lead_times-1)),COL=FSC_COLOR('olive'),TEXT='LM'
GPLOT,X=our_lead_times+0.5,Y=scripps_bivar_corr[where(scripps_bivar_corr ge 0)],COL=FSC_COLOR('darkgrey'),STYLE=0
GPLOT,X=MAX(our_lead_times)+1,Y=REFORM(scripps_bivar_corr(n_lead_times-1)),COL=FSC_COLOR('darkgrey'),TEXT='LS'
GPLOT,X=[MIN(our_lead_times),MAX(our_lead_times)+1],Y=[our_bivar_corr_threshold,our_bivar_corr_threshold],STYLE=1
;GPLOT,X=[MIN(our_lead_times),MAX(our_lead_times)+1],Y=[0,0],STYLE=1
GLEGEND,labels=REVERSE(all_descs),COL=REVERSE(FSC_COLOR(all_colors)),LEGPOS=3
AXES,XTITLE='Lead time (days)',XVALS=our_lead_times+0.5,XLABELS=STRTRIM(STRING(our_lead_times+1),1),YSTEP=0.05,YMINOR=0.025,YTITLE='Bi-variate correlation with observed RMM',NDECS=2
PSCLOSE
STOP

psfile='//home/ss901165/idl/mjo_diabatic/rmm_indices/mjo_diabatic_rmm_correlations.corr_scatter_'+start_date+'-'+stop_date+'_tolead'+STRTRIM(STRING(MAX(our_lead_times)),1)+'.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=140,MARGIN=1500,SPACE3=50,XOFFSET=2500,YOFFSET=1500,TFONT=6,TCHARSIZE=120,XSIZE=17000,YSIZE=17000
GSET,XMIN=4.5,XMAX=22,YMIN=4.5,YMAX=22
FOR i=0,n_models-1 DO BEGIN
   print,i
   below_threshold_rmm1=where(REFORM(all_models_rmm1_corr(i,*)) le our_corr_threshold)+1
   below_threshold_rmm2=where(REFORM(all_models_rmm2_corr(i,*)) le our_corr_threshold)+1
   IF below_threshold_rmm1(0) eq 0 THEN $
      below_threshold_rmm1(0)=21
   IF below_threshold_rmm2(0) eq 0 THEN $
      below_threshold_rmm2(0)=21
   IF i eq 14 THEN below_threshold_rmm1(0)=below_threshold_rmm1(0)+3
;   IF i eq 11 THEN below_threshold_rmm2(0)=below_threshold_rmm2(0)
   GPLOT,X=below_threshold_rmm1(0),Y=below_threshold_rmm2(0)-0.25,TEXT=all_descs(i),COL=FSC_COLOR(all_colors(i)),CHARSIZE=125,ALIGN=0.5
   print,below_threshold_rmm1(0),below_threshold_rmm2(0),all_descs(i)
ENDFOR
below_threshold_rmm1=where(REFORM(persistence_rmm1_corr) le our_corr_threshold)+1
below_threshold_rmm2=where(REFORM(persistence_rmm2_corr) le our_corr_threshold)+1
GPLOT,X=below_threshold_rmm1(0),Y=below_threshold_rmm2(0)-0.2,TEXT='PE',COL=FSC_COLOR('black'),CHARSIZE=125,ALIGN=0.5
print,below_threshold_rmm1(0),below_threshold_rmm2(0)
below_threshold_rmm1=where(REFORM(lim_rmm1_corr) le our_corr_threshold)+1
below_threshold_rmm2=where(REFORM(lim_rmm2_corr) le our_corr_threshold)+1
print,below_threshold_rmm1(0),below_threshold_rmm2(0)
GPLOT,X=below_threshold_rmm1(0),Y=below_threshold_rmm2(0)-0.2,TEXT='LM',COL=FSC_COLOR('olive'),CHARSIZE=125,ALIGN=0.5
below_threshold_rmm1=where(REFORM(scripps_rmm1_corr) le our_corr_threshold)+1
below_threshold_rmm2=where(REFORM(scripps_rmm2_corr) le our_corr_threshold)+1
print,below_threshold_rmm1(0),below_threshold_rmm2(0)
GPLOT,X=below_threshold_rmm1(0),Y=below_threshold_rmm2(0)-0.2,TEXT='LS',COL=FSC_COLOR('darkgrey'),CHARSIZE=125,ALIGN=0.5
GPLOT,X=[4.5,22],Y=[4.5,22],STYLE=1,THICK=120
ylabels=strarr(17)
FOR i=5,20 DO $
   ylabels(i-5)=STRTRIM(STRING(i),1)
ylabels(16)='> 20'
AXES,XVALS=indgen(17)+5,XLABELS=ylabels,YVALS=indgen(17)+5,YLABELS=ylabels,XTITLE='Skill (days) in RMM1 (Maritime Continent)',YTITLE='Skill (days) in RMM2 (Indian Ocean/West Pacific)'
;GLEGEND,labels=['PE = Persist','LS = LIM Scrps','LM = LIM Miami',all_descs+' = '+our_model_names],COL=[FSC_COLOR('black'),FSC_COLOR('darkgrey'),FSC_COLOR('olive'),FSC_COLOR(all_colors)],LENGTH=50,LEGXOFFSET=8500,LEGYOFFSET=10000
PSCLOSE
STOP

psfile='/home/ss901165/idl/mjo_diabatic/rmm_indices/mjo_diabatic_rmm_correlations.rmse_'+start_date+'-'+stop_date+'_tolead'+STRTRIM(STRING(MAX(our_lead_times)),1)+'.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE3=50,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=90,CB_WIDTH=110
GSET,XMIN=MIN(our_lead_times),XMAX=MAX(our_lead_times)+1,YMIN=0,YMAX=2.5,TITLE='RMSE of hindcast RMM1 and RMM2 against observations from '+$
     start_date+' to '+stop_date
FOR i=0,n_models-1 DO BEGIN
   GPLOT,X=our_lead_times+0.5,Y=REFORM(all_models_rmm1_rmse(i,*)),COL=FSC_COLOR(all_colors(i)),STYLE=0
   GPLOT,X=our_lead_times+0.5,Y=REFORM(all_models_rmm2_rmse(i,*)),COL=FSC_COLOR(all_colors(i)),STYLE=2
ENDFOR
GPLOT,X=our_lead_times+0.5,Y=persistence_rmm1_rmse,COL=FSC_COLOR('black'),STYLE=0
GPLOT,X=our_lead_times+0.5,Y=persistence_rmm2_rmse,COL=FSC_COLOR('black'),STYLE=2
GPLOT,X=our_lead_times+0.5,Y=lim_rmm1_rmse,COL=FSC_COLOR('olive'),STYLE=0
GPLOT,X=our_lead_times+0.5,Y=lim_rmm2_rmse,COL=FSC_COLOR('olive'),STYLE=2
GLEGEND,labels=REVERSE(all_descs),COL=REVERSE(FSC_COLOR(all_colors)),LEGPOS=1
GLEGEND,labels=['RMM2','RMM1'],COL=[FSC_COLOR('black'),FSC_COLOR('black')],STYLE=[2,0],LEGPOS=7
AXES,XTITLE='Lead time (days)',XSTEP=1,YSTEP=0.1,YMINOR=0.05,YTITLE='RMSE against observed RMM indices (from NOAA OLR and ECMWF YOTC winds)',NDECS=2
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/rmm_indices/mjo_diabatic_rmm_correlations.rmse_bivar_'+start_date+'-'+stop_date+'_tolead'+STRTRIM(STRING(MAX(our_lead_times)),1)+'.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2000,SPACE3=50,XOFFSET=1500,YOFFSET=500,TFONT=2,TCHARSIZE=90,CB_WIDTH=110
GSET,XMIN=MIN(our_lead_times),XMAX=MAX(our_lead_times)+3,YMIN=0,YMAX=2.8,TITLE='Bi-variate RMSE of hindcast against observations from '+$
     start_date+' to '+stop_date
overlap_rows=fltarr(5,n_models+1)
overlap_rows(*,0)=0
row_distance=0.75
initial_distance=0.75
FOR i=0,n_models-1 DO BEGIN
   GPLOT,X=our_lead_times+0.5,Y=REFORM(all_models_bivar_rmse(i,*)),COL=FSC_COLOR(all_colors(i))
   FOR k=0,N_ELEMENTS(overlap_rows(*,0))-1 DO BEGIN
      j=0
      WHILE j lt n_models-1 DO BEGIN
         temp=all_models_bivar_rmse(i,all_max_lead_times(i)-1)-overlap_rows(k,j)
;         print,temp
         j=j+1
         IF ABS(temp) le 0.05 THEN BEGIN
            j=n_models
         ENDIF     
      ENDWHILE
      IF j eq n_models-1 THEN BEGIN
         this_row=k
         k=999
      ENDIF
   ENDFOR
   IF k eq N_ELEMENTS(overlap_rows(*,0)) THEN BEGIN
      print,'Could not find a row for label ',all_descs(i)
      STOP
   ENDIF ELSE BEGIN
      overlap_rows(this_row,i)=all_models_bivar_rmse(i,all_max_lead_times(i)-1)
      distance=row_distance*this_row+initial_distance
      GPLOT,X=all_max_lead_times(i)-1+distance,Y=REFORM(all_models_bivar_rmse(i,all_max_lead_times(i)-1)),$
            COL=FSC_COLOR(all_colors(i)),TEXT=all_descs(i),ALIGN=0 
   ENDELSE
ENDFOR
GPLOT,X=our_lead_times+0.5,Y=persistence_bivar_rmse,COL=FSC_COLOR('black'),STYLE=0
GPLOT,X=MAX(our_lead_times)+0.5,Y=persistence_bivar_rmse(n_lead_times-1),COL=FSC_COLOR('black'),TEXT='PE'
GPLOT,X=our_lead_times+0.5,Y=lim_bivar_rmse,COL=FSC_COLOR('olive'),STYLE=0
GPLOt,X=MAX(our_lead_times)+0.5,Y=REFORM(lim_bivar_rmse(n_lead_times-1)),COL=FSC_COLOR('olive'),TEXT='LM'
GPLOT,X=[MIN(our_lead_times),MAX(our_lead_times)+1],Y=[0,0],STYLE=1
GLEGEND,labels=REVERSE(all_descs),COL=REVERSE(FSC_COLOR(all_colors)),LEGPOS=1
AXES,XTITLE='Lead time (days)',XVALS=our_lead_times+0.5,XLABELS=STRTRIM(STRING(our_lead_times+1),1),YSTEP=0.15,YMINOR=0.05,YTITLE='Bi-variate RMSE against observed RMM (NOAA OLR and YOTC winds)',NDECS=2
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/rmm_indices/mjo_diabatic_rmm_correlations.rmse_bivar-minus-persist_'+start_date+'-'+stop_date+'_tolead'+STRTRIM(STRING(MAX(our_lead_times)),1)+'.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=140,MARGIN=2000,SPACE3=50,XOFFSET=1500,YOFFSET=500,TFONT=2,TCHARSIZE=90,CB_WIDTH=110
GSET,XMIN=MIN(our_lead_times),XMAX=MAX(our_lead_times)+3,YMIN=-1.6,YMAX=0.6,TITLE='Diff in bi-variate RMSE of hindcast against obs from persistence - '+$
     start_date+' to '+stop_date
overlap_rows=fltarr(5,n_models+1)
overlap_rows(*,0)=0
row_distance=0.75
initial_distance=0.75
FOR i=0,n_models-1 DO BEGIN
   GPLOT,X=our_lead_times+0.5,Y=REFORM(all_models_bivar_rmse(i,*)-persistence_bivar_rmse),COL=FSC_COLOR(all_colors(i))
   FOR k=0,N_ELEMENTS(overlap_rows(*,0))-1 DO BEGIN
      j=0
      WHILE j lt n_models-1 DO BEGIN
         temp=all_models_bivar_rmse(i,all_max_lead_times(i)-1)-persistence_bivar_rmse(MAX(our_lead_times)-1)-overlap_rows(k,j)         
         j=j+1
         IF ABS(temp) le 0.05 THEN BEGIN
            j=n_models
         ENDIF     
      ENDWHILE
      IF j eq n_models-1 THEN BEGIN
         this_row=k
         k=999
      ENDIF
   ENDFOR
   IF k eq N_ELEMENTS(overlap_rows(*,0)) THEN BEGIN
      print,'Could not find a row for label ',all_descs(i)
      STOP
   ENDIF ELSE BEGIN
      overlap_rows(this_row,i)=all_models_bivar_rmse(i,all_max_lead_times(i)-1)-persistence_bivar_rmse(MAX(our_lead_times)-1)
      distance=row_distance*this_row+initial_distance
      GPLOT,X=all_max_lead_times(i)-1+distance,Y=REFORM(all_models_bivar_rmse(i,all_max_lead_times(i)-1)-persistence_bivar_rmse(all_max_lead_times(i)-1)),$
                                                        COL=FSC_COLOR(all_colors(i)),TEXT=all_descs(i),ALIGN=0 
   ENDELSE
ENDFOR
;GPLOT,X=our_lead_times+0.5,Y=persistence_bivar_rmse,COL=FSC_COLOR('black'),STYLE=0
;GPLOT,X=MAX(our_lead_times)+0.5,Y=persistence_bivar_rmse(n_lead_times-1),COL=FSC_COLOR('black'),TEXT='PE'
GPLOT,X=our_lead_times+0.5,Y=lim_bivar_rmse-persistence_bivar_rmse,COL=FSC_COLOR('olive'),STYLE=0
GPLOt,X=MAX(our_lead_times)+0.5,Y=REFORM(lim_bivar_rmse(n_lead_times-1)-persistence_bivar_rmse(n_lead_times-1)),COL=FSC_COLOR('olive'),TEXT='LM'
GPLOT,X=[MIN(our_lead_times),MAX(our_lead_times)+1],Y=[0,0],STYLE=1
;GLEGEND,labels=REVERSE(all_descs),COL=REVERSE(FSC_COLOR(all_colors)),LEGPOS=3
AXES,XTITLE='Lead time (days)',XVALS=our_lead_times+0.5,XLABELS=STRTRIM(STRING(our_lead_times+1),1),YSTEP=0.15,YMINOR=0.05,YTITLE='RMSE(model,obs) minus RMSE(persistence,obs)',NDECS=2
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/rmm_indices/mjo_diabatic_rmm_correlations.spread_rmm_'+start_date+'-'+stop_date+'_tolead'+STRTRIM(STRING(MAX(our_lead_times)),1)+'.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE3=50,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=90,CB_WIDTH=110
GSET,XMIN=MIN(our_lead_times),XMAX=MAX(our_lead_times)+1,YMIN=0,YMAX=3.0,TITLE='Spread in hindcast RMM1 and RMM2 with lead time from '+$
     start_date+' to '+stop_date
FOR i=0,n_models-1 DO BEGIN
   GPLOT,X=our_lead_times+0.5,Y=REFORM(all_models_rmm1_stddev(i,*)),COL=FSC_COLOR(all_colors(i)),SYM=4
   GPLOT,X=our_lead_times+0.5,Y=REFORM(all_models_rmm2_stddev(i,*)),COL=FSC_COLOR(all_colors(i)),SYM=6
ENDFOR
GPLOT,X=our_lead_times+0.5,Y=persistence_rmm1_stddev,COL=FSC_COLOR('black'),SYM=4
GPLOT,X=our_lead_times+0.5,Y=persistence_rmm2_stddev,COL=FSC_COLOR('black'),SYM=6
GPLOT,X=our_lead_times+0.5,Y=lim_rmm1_stddev,COL=FSC_COLOR('olive'),SYM=4
GPLOT,X=our_lead_times+0.5,Y=lim_rmm2_stddev,COL=FSC_COLOR('olive'),SYM=6
;GPLOT,X=our_lead_times+0.5,Y=persistence_bivar_rmse,COL=FSC_COLOR('black'),STYLE=0
GLEGEND,labels=REVERSE(all_descs),COL=REVERSE(FSC_COLOR(all_colors)),LEGPOS=1,SYM=REPLICATE(4,n_models+2),LENGTH=0
GLEGEND,labels=['RMM2','RMM1'],LEGPOS=9,SYM=[6,4],LENGTH=0
AXES,XTITLE='Lead time (days)',XSTEP=1,YSTEP=0.1,YMINOR=0.05,YTITLE='Standard deviation (unitless) of RMM indices at constant lead time',NDECS=2
PSCLOSE,/NOVIEW

STOP
END
