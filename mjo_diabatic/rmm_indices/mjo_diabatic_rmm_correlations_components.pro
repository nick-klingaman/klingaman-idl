PRO mjo_diabatic_rmm_correlations_components,model_names=model_names,start_date,stop_date,lead_times=lead_times,correlation_threshold=correlation_threshold,$
   bivariate_correlation_threshold=bivariate_correlation_threshold
  
                                ; Plot correlations of RMM1 and RMM2 against observations for specified models,
                                ; over specified range of start dates and lead times.

IF KEYWORD_SET(model_names) THEN BEGIN
   our_model_names=model_names
ENDIF ELSE $
   our_model_names=['ecearth','miroc','cnrm_atm','nasa','ecmwf','metum','mri','nrl','cancm4','spcam','giss','cam5','cam5zm','metum_annatmos','metum_anncouple']

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
   our_corr_threshold=0.6

IF KEYWORD_SET(bivariate_correlation_threshold) THEN BEGIN
   our_bivar_corr_threshold=bivariate_correlation_threshold
ENDIF ELSE $
   our_bivar_corr_threshold=0.7

n_times_per_day=8
standard_valid_dates=STRTRIM(STRING([indgen(22)+20091010,indgen(25)+20091101,indgen(22)+20091210,indgen(25)+20100101]),1)
mogreps_valid_dates=STRTRIM(STRING([indgen(17)+20091015,indgen(20)+20091101,indgen(11)+20091215,indgen(5)+20091227,indgen(16)+20100101]),1)
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
;obs_infile='/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2011.index_values.nc'
obs_start_date=274
IF start_julian lt obs_start_date THEN $
   start_julian=start_julian+365
n_days=FLOOR(stop_julian-start_julian+1)
print,start_julian,stop_julian,n_days

obs_rmm1_olr=fltarr(n_days)
obs_rmm1_u850=fltarr(n_days)
obs_rmm1_u200=fltarr(n_days)
obs_rmm2_olr=fltarr(n_days)
obs_rmm2_u850=fltarr(n_days)
obs_rmm2_u200=fltarr(n_days)

persistence_rmm1_olr=fltarr(n_days,n_lead_times)
persistence_rmm1_u850=fltarr(n_days,n_lead_times)
persistence_rmm1_u200=fltarr(n_days,n_lead_times)
persistence_rmm2_olr=fltarr(n_days,n_lead_times)
persistence_rmm2_u850=fltarr(n_days,n_lead_times)
persistence_rmm2_u200=fltarr(n_days,n_lead_times)

FOR j=0,n_days-1 DO BEGIN
   print,start_julian+j-obs_start_date
   obs_rmm1_olr(j)=$
      REFORM(OPEN_AND_EXTRACT(obs_infile,'contrib_rmm1_olr',$
                              offset=[FLOOR(start_julian)+j-obs_start_date],count=[1]))
   obs_rmm1_u850(j)=$
      REFORM(OPEN_AND_EXTRACT(obs_infile,'contrib_rmm1_u850',$
                              offset=[FLOOR(start_julian)+j-obs_start_date],count=[1]))
   obs_rmm1_u200(j)=$
      REFORM(OPEN_AND_EXTRACT(obs_infile,'contrib_rmm1_u200',$
                              offset=[FLOOR(start_julian)+j-obs_start_date],count=[1]))   
   obs_rmm2_olr(j)=$
      REFORM(OPEN_AND_EXTRACT(obs_infile,'contrib_rmm2_olr',$
                              offset=[FLOOR(start_julian)+j-obs_start_date],count=[1]))
   obs_rmm2_u850(j)=$
      REFORM(OPEN_AND_EXTRACT(obs_infile,'contrib_rmm2_u850',$
                              offset=[FLOOR(start_julian)+j-obs_start_date],count=[1]))
   obs_rmm2_u200(j)=$
      REFORM(OPEN_AND_EXTRACT(obs_infile,'contrib_rmm2_u200',$
                              offset=[FLOOR(start_julian)+j-obs_start_date],count=[1]))
   FOR k=0,n_lead_times-1 DO BEGIN
      persistence_rmm1_olr(j,*)=obs_rmm1_olr(j)      
      persistence_rmm1_u850(j,*)=obs_rmm1_u850(j)
      persistence_rmm1_u200(j,*)=obs_rmm1_u200(j)
      
      persistence_rmm2_olr(j,*)=obs_rmm2_olr(j)      
      persistence_rmm2_u850(j,*)=obs_rmm2_u850(j)
      persistence_rmm2_u200(j,*)=obs_rmm2_u200(j)
   ENDFOR
ENDFOR

; Handle models
all_models_rmm1_corr=fltarr(3,n_models,n_lead_times)
all_models_rmm2_corr=fltarr(3,n_models,n_lead_times)
all_models_bivar_corr=fltarr(3,n_models,n_lead_times)
all_models_rmm1_rmse=fltarr(3,n_models,n_lead_times)
all_models_rmm2_rmse=fltarr(3,n_models,n_lead_times)
all_models_bivar_rmse=fltarr(3,n_models,n_lead_times)
all_models_rmm1_stddev=fltarr(3,n_models,n_lead_times)
all_models_rmm2_stddev=fltarr(3,n_models,n_lead_times)

persistence_rmm1_corr=fltarr(3,n_lead_times)
persistence_rmm2_corr=fltarr(3,n_lead_times)
persistence_bivar_corr=fltarr(3,n_lead_times)
persistence_rmm1_rmse=fltarr(3,n_lead_times)
persistence_rmm2_rmse=fltarr(3,n_lead_times)
persistence_bivar_rmse=fltarr(3,n_lead_times)
persistence_rmm1_stddev=fltarr(3,n_lead_times)
persistence_rmm2_stddev=fltarr(3,n_lead_times)

all_colors=strarr(n_models+1)
all_descs=strarr(n_models+1)
all_max_lead_times=intarr(n_models+1)
FOR i=0,n_models-1 DO BEGIN
   CASE our_model_names(i) OF 
      'ecmwf' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/ecmwf'
         all_descs(i)='EC'         
         all_colors(i)='red'
         all_max_lead_times(i)=20
         valid_dates=standard_valid_dates
      END
      'miroc' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/miroc'
         all_descs(i)='MI'
         all_colors(i)='orange'
         all_max_lead_times(i)=20
         valid_dates=standard_valid_dates
      END
      'mri' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/mri'
         all_descs(i)='MR'
         all_colors(i)='blue'
         all_max_lead_times(i)=20
         valid_dates=standard_valid_dates
      END
      'nasa' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/nasa'
         all_descs(i)='NA'
         all_colors(i)='cyan'
         all_max_lead_times(i)=20
         valid_dates=standard_valid_dates
      END      
      'nrl'  : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/nrl'
         all_descs(i)='NR'
         all_colors(i)='brown'
         all_max_lead_times(i)=20
         valid_dates=standard_valid_dates
      END
      'iis'  : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/iis'
         all_descs(i)='II'
         all_colors(i)='darkgrey'
         all_max_lead_times(i)=20
         valid_dates=standard_valid_dates
      END
      'metum' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/metum'
         all_descs(i)='MO'
         all_colors(i)='darkgoldenrod'         
         all_max_lead_times(i)=20
         valid_dates=standard_valid_dates
      END
      'metum_annatmos' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/metum_coupled_ann/Atmos_control'
         all_descs(i)='MA'
         all_colors(i)='maroon'
         all_max_lead_times(i)=15
         valid_dates=mogreps_valid_dates
      END
      'metum_anncouple' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/metum_coupled_ann/Coupled_model'
         all_descs(i)='MC'
         all_colors(i)='orangered'         
         all_max_lead_times(i)=15
         valid_dates=mogreps_valid_dates
      END
      'spcam' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/spcam'
         all_descs(i)='SP'
         all_colors(i)='purple'         
         all_max_lead_times(i)=20
         valid_dates=standard_valid_dates
      END
      'giss' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/giss'
         all_descs(i)='GI'
         all_colors(i)='violetred'         
         all_max_lead_times(i)=20
         valid_dates=standard_valid_dates
      END
      'cancm4' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cancm4'
         all_descs(i)='CC'
         all_colors(i)='dodgerblue'         
         all_max_lead_times(i)=20
         valid_dates=standard_valid_dates
      END
      'cam5' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cam5'
         all_descs(i)='C5'
         all_colors(i)='deeppink'         
         all_max_lead_times(i)=20
         valid_dates=standard_valid_dates
         max_lead_time=20
      END
      'cam5zm' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cam5zm'
         all_descs(i)='CZ'
         all_colors(i)='steelblue'         
         all_max_lead_times(i)=20
         valid_dates=standard_valid_dates
         max_lead_time=20
      END
      'ecearth' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/ecearth'
         all_descs(i)='E3'
         all_colors(i)='limegreen'         
         all_max_lead_times(i)=20
         valid_dates=standard_valid_dates
         max_lead_time=20
      END
      'cnrm_atm' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cnrm_atmos'
         all_descs(i)='CN'
         all_colors(i)='navy'         
         all_max_lead_times(i)=20
         valid_dates=standard_valid_dates
         max_lead_time=20
      END
   ENDCASE
   
   model_offset=where(valid_dates eq start_date)
   date_offset=where(dates eq start_date)

   this_model_rmm1_olr=fltarr(n_days,n_lead_times)
   this_model_rmm1_u850=fltarr(n_days,n_lead_times)
   this_model_rmm1_u200=fltarr(n_days,n_lead_times)

   this_model_rmm2_olr=fltarr(n_days,n_lead_times)
   this_model_rmm2_u850=fltarr(n_days,n_lead_times)
   this_model_rmm2_u200=fltarr(n_days,n_lead_times)

   FOR j=0,n_days-1 DO BEGIN
      FOR k=0,n_lead_times-1 DO BEGIN                   
         IF k lt all_max_lead_times(i) THEN BEGIN
            offset=REFORM(j+date_offset-our_lead_times(k))      
            IF where(valid_dates eq dates(offset(0))) eq -1 THEN BEGIN
                                ;  print,'No hindcast for date '+dates(j+date_offset)+' at lead time '+STRTRIM(STRING(our_lead_times(k)),1)
               this_model_rmm1_olr(j,k)=!Values.F_NaN
               this_model_rmm2_olr(j,k)=!Values.F_NaN            
               this_model_rmm1_u850(j,k)=!Values.F_NaN
               this_model_rmm2_u850(j,k)=!Values.F_NaN 
               this_model_rmm1_u200(j,k)=!Values.F_NaN
               this_model_rmm2_u200(j,k)=!Values.F_NaN 
            ENDIF ELSE BEGIN
               IF our_lead_times(k) eq 19 THEN BEGIN
                  count=n_times_per_day-1
               ENDIF ELSE $
                  count=n_times_per_day           
               
               initial_date=dates(j+date_offset-our_lead_times(k))
                                ;print,'Examining day '+dates(j+date_offset)+' at lead time '+STRTRIM(STRING(our_lead_times(k)),1)+$
                                ;      ' using hindcast initialised on '+initial_date
               model_infile=indir+'/'+REFORM(initial_date)+'/rmm_indices.nc'
               
               model_rmm1_day=OPEN_AND_EXTRACT(model_infile(0),'contrib_rmm1_olr',offset=[our_lead_times(k)*n_times_per_day],count=[count])
               this_model_rmm1_olr(j,k)=MEAN(model_rmm1_day)
               model_rmm1_day=OPEN_AND_EXTRACT(model_infile(0),'contrib_rmm1_u850',offset=[our_lead_times(k)*n_times_per_day],count=[count])
               this_model_rmm1_u850(j,k)=MEAN(model_rmm1_day)
               model_rmm1_day=OPEN_AND_EXTRACT(model_infile(0),'contrib_rmm1_u200',offset=[our_lead_times(k)*n_times_per_day],count=[count])
               this_model_rmm1_u200(j,k)=MEAN(model_rmm1_day)
               
               model_rmm2_day=OPEN_AND_EXTRACT(model_infile(0),'contrib_rmm2_olr',offset=[our_lead_times(k)*n_times_per_day],count=[count])
               this_model_rmm2_olr(j,k)=MEAN(model_rmm2_day)
               model_rmm2_day=OPEN_AND_EXTRACT(model_infile(0),'contrib_rmm2_u850',offset=[our_lead_times(k)*n_times_per_day],count=[count])
               this_model_rmm2_u850(j,k)=MEAN(model_rmm2_day)
               model_rmm2_day=OPEN_AND_EXTRACT(model_infile(0),'contrib_rmm2_u200',offset=[our_lead_times(k)*n_times_per_day],count=[count])
               this_model_rmm2_u200(j,k)=MEAN(model_rmm2_day)
               
            ENDELSE
         ENDIF
      ENDFOR
   ENDFOR   
   
   n_valid=fltarr(n_lead_times)
   FOR k=0,n_lead_times-1 DO BEGIN
      model_rmm1_olr=fltarr(n_days)
      model_rmm2_olr=fltarr(n_days)
      model_rmm1_olr(0:k)=!Values.F_NaN
      model_rmm2_olr(0:k)=!Values.F_NaN      
      model_rmm1_u850=fltarr(n_days)
      model_rmm2_u850=fltarr(n_days)
      model_rmm1_u850(0:k)=!Values.F_NaN
      model_rmm2_u850(0:k)=!Values.F_NaN
      model_rmm1_u200=fltarr(n_days)
      model_rmm2_u200=fltarr(n_days)
      model_rmm1_u200(0:k)=!Values.F_NaN
      model_rmm2_u200(0:k)=!Values.F_NaN

      FOR j=k,n_days-1 DO BEGIN
         model_rmm1_olr(j)=REFORM(this_model_rmm1_olr(j,k));-(this_model_rmm1(j-k,0)-obs_rmm1(j-k))*0.85
         model_rmm2_olr(j)=REFORM(this_model_rmm2_olr(j,k));-(this_model_rmm2(j-k,0)-obs_rmm2(j-k))*0.85
         model_rmm1_u850(j)=REFORM(this_model_rmm1_u850(j,k));-(this_model_rmm1(j-k,0)-obs_rmm1(j-k))*0.85
         model_rmm2_u850(j)=REFORM(this_model_rmm2_u850(j,k));-(this_model_rmm2(j-k,0)-obs_rmm2(j-k))*0.85
         model_rmm1_u200(j)=REFORM(this_model_rmm1_u200(j,k));-(this_model_rmm1(j-k,0)-obs_rmm1(j-k))*0.85
         model_rmm2_u200(j)=REFORM(this_model_rmm2_u200(j,k));-(this_model_rmm2(j-k,0)-obs_rmm2(j-k))*0.85
      ENDFOR

      model_rmm1_olr_valid=model_rmm1_olr[where(FINITE(model_rmm1_olr) eq 1)]
      model_rmm2_olr_valid=model_rmm2_olr[where(FINITE(model_rmm2_olr) eq 1)]
      obs_rmm1_olr_valid=obs_rmm1_olr[where(FINITE(model_rmm1_olr) eq 1)]
      obs_rmm2_olr_valid=obs_rmm2_olr[where(FINITE(model_rmm2_olr) eq 1)]
      
      model_rmm1_u850_valid=model_rmm1_u850[where(FINITE(model_rmm1_u850) eq 1)]
      model_rmm2_u850_valid=model_rmm2_u850[where(FINITE(model_rmm2_u850) eq 1)]
      obs_rmm1_u850_valid=obs_rmm1_u850[where(FINITE(model_rmm1_u850) eq 1)]
      obs_rmm2_u850_valid=obs_rmm2_u850[where(FINITE(model_rmm2_u850) eq 1)]
      
      model_rmm1_u200_valid=model_rmm1_u200[where(FINITE(model_rmm1_u200) eq 1)]
      model_rmm2_u200_valid=model_rmm2_u200[where(FINITE(model_rmm2_u200) eq 1)]
      obs_rmm1_u200_valid=obs_rmm1_u200[where(FINITE(model_rmm1_u200) eq 1)]
      obs_rmm2_u200_valid=obs_rmm2_u200[where(FINITE(model_rmm2_u200) eq 1)]
      
      n_valid(k)=N_ELEMENTS(model_rmm1_olr_valid)
      
      all_models_rmm1_corr(0,i,k)=CORRELATE(obs_rmm1_olr_valid,model_rmm1_olr_valid)
      all_models_rmm2_corr(0,i,k)=CORRELATE(obs_rmm2_olr_valid,model_rmm2_olr_valid)      
      all_models_rmm1_corr(1,i,k)=CORRELATE(obs_rmm1_u850_valid,model_rmm1_u850_valid)
      all_models_rmm2_corr(1,i,k)=CORRELATE(obs_rmm2_u850_valid,model_rmm2_u850_valid)      
      all_models_rmm1_corr(2,i,k)=CORRELATE(obs_rmm1_u200_valid,model_rmm1_u200_valid)
      all_models_rmm2_corr(2,i,k)=CORRELATE(obs_rmm2_u200_valid,model_rmm2_u200_valid)
      
      all_models_bivar_corr(0,i,k)=TOTAL(obs_rmm1_olr_valid*model_rmm1_olr_valid+obs_rmm2_olr_valid*model_rmm2_olr_valid)/$
                                   (SQRT(TOTAL(obs_rmm1_olr_valid^2+obs_rmm2_olr_valid^2))*SQRT(TOTAL(model_rmm1_olr_valid^2+model_rmm2_olr_valid^2)))      
      all_models_bivar_corr(1,i,k)=TOTAL(obs_rmm1_u850_valid*model_rmm1_u850_valid+obs_rmm2_u850_valid*model_rmm2_u850_valid)/$
                                   (SQRT(TOTAL(obs_rmm1_u850_valid^2+obs_rmm2_u850_valid^2))*SQRT(TOTAL(model_rmm1_u850_valid^2+model_rmm2_u850_valid^2)))
      all_models_bivar_corr(2,i,k)=TOTAL(obs_rmm1_u200_valid*model_rmm1_u200_valid+obs_rmm2_u200_valid*model_rmm2_u200_valid)/$
                                   (SQRT(TOTAL(obs_rmm1_u200_valid^2+obs_rmm2_u200_valid^2))*SQRT(TOTAL(model_rmm1_u200_valid^2+model_rmm2_u200_valid^2)))
      
      all_models_rmm1_stddev(0,i,k)=STDDEV(model_rmm1_olr_valid)
      all_models_rmm1_stddev(1,i,k)=STDDEV(model_rmm1_u850_valid)
      all_models_rmm1_stddev(2,i,k)=STDDEV(model_rmm1_u200_valid)      
      all_models_rmm2_stddev(0,i,k)=STDDEV(model_rmm2_olr_valid)
      all_models_rmm2_stddev(1,i,k)=STDDEV(model_rmm2_u850_valid)
      all_models_rmm2_stddev(2,i,k)=STDDEV(model_rmm2_u200_valid)

      all_models_rmm1_rmse(0,i,k)=SQRT(MEAN((model_rmm1_olr_valid-obs_rmm1_olr_valid)^2))
      all_models_rmm2_rmse(0,i,k)=SQRT(MEAN((model_rmm2_olr_valid-obs_rmm2_olr_valid)^2))      
      all_models_rmm1_rmse(1,i,k)=SQRT(MEAN((model_rmm1_u850_valid-obs_rmm1_u850_valid)^2))
      all_models_rmm2_rmse(1,i,k)=SQRT(MEAN((model_rmm2_u850_valid-obs_rmm2_u850_valid)^2))
      all_models_rmm1_rmse(2,i,k)=SQRT(MEAN((model_rmm1_u200_valid-obs_rmm1_u200_valid)^2))
      all_models_rmm2_rmse(2,i,k)=SQRT(MEAN((model_rmm2_u200_valid-obs_rmm2_u200_valid)^2))
      
      all_models_bivar_rmse(0,i,k)=SQRT(TOTAL((obs_rmm1_olr_valid-model_rmm1_olr_valid)^2+(obs_rmm2_olr_valid-model_rmm2_olr_valid)^2)/n_valid(k))
      all_models_bivar_rmse(1,i,k)=SQRT(TOTAL((obs_rmm1_u850_valid-model_rmm1_u850_valid)^2+(obs_rmm2_u850_valid-model_rmm2_u850_valid)^2)/n_valid(k))
      all_models_bivar_rmse(2,i,k)=SQRT(TOTAL((obs_rmm1_u200_valid-model_rmm1_u200_valid)^2+(obs_rmm2_u200_valid-model_rmm2_u200_valid)^2)/n_valid(k))      
      

   ENDFOR
;   below_threshold=where(REFORM(all_models_rmm1_corr(i,*)) lt our_corr_threshold)
;   print,'--------------------------'
;   print,'For model '+all_descs(i)+' and RMM1/RMM2 correlation threshold '+STRTRIM(STRING(our_corr_threshold),1)+$
;         ' and bi-variate correlation threshold '+STRTRIM(STRING(our_bivar_corr_threshold),1)
;   print,'RMM1: '+STRTRIM(STRING(below_threshold(0)+1),1)+' days'
;   below_threshold=where(REFORM(all_models_rmm2_corr(i,*)) lt our_corr_threshold)
;   print,'RMM2: '+STRTRIM(STRING(below_threshold(0)+1),1)+' days'
;   below_threshold=where(REFORM(all_models_bivar_corr(i,*)) lt our_bivar_corr_threshold)
;   print,'Bivar: '+STRTRIM(STRING(below_threshold(0)+1),1)+' days'    
ENDFOR

FOR k=0,n_lead_times-1 DO BEGIN
   obs_rmm1_olr_valid=obs_rmm1_olr(k:n_days-1)
   obs_rmm2_olr_valid=obs_rmm2_olr(k:n_days-1)
   persistence_rmm1_olr_valid=REFORM(persistence_rmm1_olr(0:n_days-1-k,k))
   persistence_rmm2_olr_valid=REFORM(persistence_rmm2_olr(0:n_days-1-k,k))
   obs_rmm1_u850_valid=obs_rmm1_u850(k:n_days-1)
   obs_rmm2_u850_valid=obs_rmm2_u850(k:n_days-1)
   persistence_rmm1_u850_valid=REFORM(persistence_rmm1_u850(0:n_days-1-k,k))
   persistence_rmm2_u850_valid=REFORM(persistence_rmm2_u850(0:n_days-1-k,k))
   obs_rmm1_u200_valid=obs_rmm1_u200(k:n_days-1)
   obs_rmm2_u200_valid=obs_rmm2_u200(k:n_days-1)
   persistence_rmm1_u200_valid=REFORM(persistence_rmm1_u200(0:n_days-1-k,k))
   persistence_rmm2_u200_valid=REFORM(persistence_rmm2_u200(0:n_days-1-k,k))

   persistence_rmm1_corr(0,k)=CORRELATE(obs_rmm1_olr_valid,persistence_rmm1_olr_valid)
   persistence_rmm2_corr(0,k)=CORRELATE(obs_rmm2_olr_valid,persistence_rmm2_olr_valid)
   persistence_bivar_corr(0,k)=TOTAL(obs_rmm1_olr_valid*persistence_rmm1_olr_valid+obs_rmm2_olr_valid*persistence_rmm2_olr_valid)/$
                             (SQRT(TOTAL(obs_rmm1_olr_valid^2+obs_rmm2_olr_valid^2))*SQRT(TOTAL(persistence_rmm1_olr_valid^2+persistence_rmm2_olr_valid^2)))
   persistence_rmm1_rmse(0,k)=SQRT(MEAN((persistence_rmm1_olr_valid-obs_rmm1_olr_valid)^2))
   persistence_rmm2_rmse(0,k)=SQRT(MEAN((persistence_rmm2_olr_valid-obs_rmm2_olr_valid)^2))
   persistence_bivar_rmse(0,k)=SQRT(TOTAL((obs_rmm1_olr_valid-persistence_rmm1_olr_valid)^2+(obs_rmm2_olr_valid-persistence_rmm2_olr_valid)^2)/FLOAT(n_days-1-k))

   persistence_rmm1_corr(1,k)=CORRELATE(obs_rmm1_u850_valid,persistence_rmm1_u850_valid)
   persistence_rmm2_corr(1,k)=CORRELATE(obs_rmm2_u850_valid,persistence_rmm2_u850_valid)
   persistence_bivar_corr(1,k)=TOTAL(obs_rmm1_u850_valid*persistence_rmm1_u850_valid+obs_rmm2_u850_valid*persistence_rmm2_u850_valid)/$
                             (SQRT(TOTAL(obs_rmm1_u850_valid^2+obs_rmm2_u850_valid^2))*SQRT(TOTAL(persistence_rmm1_u850_valid^2+persistence_rmm2_u850_valid^2)))
   persistence_rmm1_rmse(1,k)=SQRT(MEAN((persistence_rmm1_u850_valid-obs_rmm1_u850_valid)^2))
   persistence_rmm2_rmse(1,k)=SQRT(MEAN((persistence_rmm2_u850_valid-obs_rmm2_u850_valid)^2))
   persistence_bivar_rmse(1,k)=SQRT(TOTAL((obs_rmm1_u850_valid-persistence_rmm1_u850_valid)^2+(obs_rmm2_u850_valid-persistence_rmm2_u850_valid)^2)/FLOAT(n_days-1-k))

   persistence_rmm1_corr(2,k)=CORRELATE(obs_rmm1_u200_valid,persistence_rmm1_u200_valid)
   persistence_rmm2_corr(2,k)=CORRELATE(obs_rmm2_u200_valid,persistence_rmm2_u200_valid)
   persistence_bivar_corr(2,k)=TOTAL(obs_rmm1_u200_valid*persistence_rmm1_u200_valid+obs_rmm2_u200_valid*persistence_rmm2_u200_valid)/$
                             (SQRT(TOTAL(obs_rmm1_u200_valid^2+obs_rmm2_u200_valid^2))*SQRT(TOTAL(persistence_rmm1_u200_valid^2+persistence_rmm2_u200_valid^2)))
   persistence_rmm1_rmse(2,k)=SQRT(MEAN((persistence_rmm1_u200_valid-obs_rmm1_u200_valid)^2))
   persistence_rmm2_rmse(2,k)=SQRT(MEAN((persistence_rmm2_u200_valid-obs_rmm2_u200_valid)^2))
   persistence_bivar_rmse(2,k)=SQRT(TOTAL((obs_rmm1_u200_valid-persistence_rmm1_u200_valid)^2+(obs_rmm2_u200_valid-persistence_rmm2_u200_valid)^2)/FLOAT(n_days-1-k))

   persistence_rmm1_stddev(0,k)=STDDEV(persistence_rmm1_olr_valid)
   persistence_rmm1_stddev(1,k)=STDDEV(persistence_rmm1_u850_valid)
   persistence_rmm1_stddev(2,k)=STDDEV(persistence_rmm1_u200_valid)      
   persistence_rmm2_stddev(0,k)=STDDEV(persistence_rmm2_olr_valid)
   persistence_rmm2_stddev(1,k)=STDDEV(persistence_rmm2_u850_valid)
   persistence_rmm2_stddev(2,k)=STDDEV(persistence_rmm2_u200_valid)
ENDFOR
all_descs(n_models)='PE'
all_colors(n_models)='black'
   
;   persistence_rmm1_stddev(0,k)=STDDEV(persistence_rmm1_valid)
;   persistence_rmm2_stddev(0,k)=STDDEV(persistence_rmm2_valid)

psfile='/home/ss901165/idl/mjo_diabatic/rmm_indices/mjo_diabatic_rmm_correlations_components.corr_olr_'+start_date+'-'+stop_date+'_tolead'+STRTRIM(STRING(MAX(our_lead_times)),1)+'.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2000,SPACE3=50,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=90,CB_WIDTH=110
GSET,XMIN=MIN(our_lead_times),XMAX=MAX(our_lead_times)+1,YMIN=-0.2,YMAX=1,TITLE='Correlation of hindcast and observed RMM1 and RMM2 (OLR only) for '+$
     start_date+' to '+stop_date
FOR i=0,n_models-1 DO BEGIN
   GPLOT,X=our_lead_times+0.5,Y=REFORM(all_models_rmm1_corr(0,i,*)),COL=FSC_COLOR(all_colors(i)),STYLE=0
   GPLOT,X=our_lead_times+0.5,Y=REFORM(all_models_rmm2_corr(0,i,*)),COL=FSC_COLOR(all_colors(i)),STYLE=2
ENDFOR
GPLOT,X=[MIN(our_lead_times),MAX(our_lead_times)+1],Y=[0,0],STYLE=1
toplot=REFORM(persistence_rmm1_corr(0,*))
toplot=toplot[where(toplot ge -0.2)]
GPLOT,X=our_lead_times+0.5,Y=toplot,COL=FSC_COLOR('black'),STYLE=0
toplot=REFORM(persistence_rmm2_corr(0,*))
toplot=toplot[where(toplot ge -0.2)]
GPLOT,X=our_lead_times+0.5,Y=toplot,COL=FSC_COLOR('black'),STYLE=2
GLEGEND,labels=all_descs,COL=FSC_COLOR(all_colors),LEGPOS=2
GLEGEND,labels=['RMM2','RMM1'],COL=[FSC_COLOR('black'),FSC_COLOR('black')],STYLE=[2,0],LEGPOS=3
AXES,XTITLE='Lead time (days)',XSTEP=1,YSTEP=0.05,YMINOR=0.025,YTITLE='Correlation with observed RMM indices (OLR only, NOAA CIRES obs)',NDECS=3
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/rmm_indices/mjo_diabatic_rmm_correlations_components.corr_u850_'+start_date+'-'+stop_date+'_tolead'+STRTRIM(STRING(MAX(our_lead_times)),1)+'.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2000,SPACE3=50,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=90,CB_WIDTH=110
GSET,XMIN=MIN(our_lead_times),XMAX=MAX(our_lead_times)+1,YMIN=-0.2,YMAX=1,TITLE='Correlation of hindcast and observed RMM1 and RMM2 (U850 only) for '+$
     start_date+' to '+stop_date
FOR i=0,n_models-1 DO BEGIN
   GPLOT,X=our_lead_times+0.5,Y=REFORM(all_models_rmm1_corr(1,i,*)),COL=FSC_COLOR(all_colors(i)),STYLE=0
   GPLOT,X=our_lead_times+0.5,Y=REFORM(all_models_rmm2_corr(1,i,*)),COL=FSC_COLOR(all_colors(i)),STYLE=2
ENDFOR
GPLOT,X=[MIN(our_lead_times),MAX(our_lead_times)+1],Y=[0,0],STYLE=1
toplot=REFORM(persistence_rmm1_corr(1,*))
toplot2=fltarr(n_lead_times)
toplot2[where(toplot ge -0.2)]=toplot[where(toplot ge -0.2)]
IF TOTAL(where(toplot lt -0.2)) ge 0 THEN $
   toplot2[where(toplot lt -0.2)]=!Values.F_NaN
GPLOT,X=our_lead_times[where(toplot ge -0.2)]+0.5,Y=toplot2,COL=FSC_COLOR('black'),STYLE=0
toplot=REFORM(persistence_rmm2_corr(1,*))
toplot=toplot[where(toplot ge -0.2)]
GPLOT,X=our_lead_times+0.5,Y=toplot,COL=FSC_COLOR('black'),STYLE=2
GLEGEND,labels=all_descs,COL=FSC_COLOR(all_colors),LEGPOS=2
GLEGEND,labels=['RMM2','RMM1'],COL=[FSC_COLOR('black'),FSC_COLOR('black')],STYLE=[2,0],LEGPOS=3
AXES,XTITLE='Lead time (days)',XSTEP=1,YSTEP=0.05,YMINOR=0.025,YTITLE='Correlation with observed RMM indices (U850 only, ECMWF YOTC analysis)',NDECS=3
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/rmm_indices/mjo_diabatic_rmm_correlations_components.corr_u200_'+start_date+'-'+stop_date+'_tolead'+STRTRIM(STRING(MAX(our_lead_times)),1)+'.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2000,SPACE3=50,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=90,CB_WIDTH=110
GSET,XMIN=MIN(our_lead_times),XMAX=MAX(our_lead_times)+1,YMIN=-0.2,YMAX=1,TITLE='Correlation of hindcast and observed RMM1 and RMM2 (U200 only) for '+$
     start_date+' to '+stop_date
FOR i=0,n_models-1 DO BEGIN
   GPLOT,X=our_lead_times+0.5,Y=REFORM(all_models_rmm1_corr(2,i,*)),COL=FSC_COLOR(all_colors(i)),STYLE=0
   GPLOT,X=our_lead_times+0.5,Y=REFORM(all_models_rmm2_corr(2,i,*)),COL=FSC_COLOR(all_colors(i)),STYLE=2
ENDFOR
GPLOT,X=[MIN(our_lead_times),MAX(our_lead_times)+1],Y=[0,0],STYLE=1
toplot=REFORM(persistence_rmm1_corr(2,*))
toplot=toplot[where(toplot ge -0.2)]
GPLOT,X=our_lead_times+0.5,Y=toplot,COL=FSC_COLOR('black'),STYLE=0
toplot=REFORM(persistence_rmm2_corr(2,*))
toplot=toplot[where(toplot ge -0.2)]
GPLOT,X=our_lead_times+0.5,Y=toplot,COL=FSC_COLOR('black'),STYLE=2
GLEGEND,labels=all_descs,COL=FSC_COLOR(all_colors),LEGPOS=2
GLEGEND,labels=['RMM2','RMM1'],COL=[FSC_COLOR('black'),FSC_COLOR('black')],STYLE=[2,0],LEGPOS=3
AXES,XTITLE='Lead time (days)',XSTEP=1,YSTEP=0.05,YMINOR=0.025,YTITLE='Correlation with observed RMM indices (U200 only, ECMWF YOTC analysis)',NDECS=3
PSCLOSE,/NOVIEW

FOR m=0,2 DO BEGIN
   CASE m OF
      0 : BEGIN
         type='olr'
         obs='NOAA CIRES'
         rmse_max=1.0
         bivar_rmse_max=0.7
      END
      1 : BEGIN
         type='u850'
         obs='ECMWF YoTC'
         rmse_max=1.5
         bivar_rmse_max=2.0
      END
      2 : BEGIN
         type='u200'
         obs='ECMWF YoTC'
         rmse_max=1.5
         bivar_rmse_max=2.0
      END
   ENDCASE
   
   psfile='/home/ss901165/idl/mjo_diabatic/rmm_indices/mjo_diabatic_rmm_correlations_components.corr_bivar_'+type+'_'+start_date+'-'+stop_date+'_tolead'+STRTRIM(STRING(MAX(our_lead_times)),1)+'.ps'
   PSOPEN,file=psfile,FONT=6,CHARSIZE=150,MARGIN=1500,SPACE3=50,XOFFSET=2000,YOFFSET=500,TFONT=2,TCHARSIZE=90,CB_WIDTH=110
   GSET,XMIN=MIN(our_lead_times),XMAX=MAX(our_lead_times)+4.5,YMIN=0.1,YMAX=1
        ;TITLE='Bi-variate correlation of hindcast and observed RMM1 and RMM2 ('+type+' only) for '+start_date+' to '+stop_date
   overlap_rows=fltarr(5,n_models+1)
   overlap_rows(*,0)=0
   row_distance=1
   initial_distance=0.75
   FOR i=0,n_models-1 DO BEGIN
      GPLOT,X=our_lead_times+0.5,Y=REFORM(all_models_bivar_corr(m,i,*)),COL=FSC_COLOR(all_colors(i))
      IF all_max_lead_times(i) ne 20 THEN BEGIN
         GPLOT,X=all_max_lead_times(i)-1+initial_distance,Y=REFORM(all_models_bivar_corr(m,i,all_max_lead_times(i)-1)),$
               COL=FSC_COLOR(all_colors(i)),TEXT=all_descs(i),ALIGN=0
      ENDIF ELSE BEGIN
         FOR k=0,N_ELEMENTS(overlap_rows(*,0))-1 DO BEGIN
            j=0
            WHILE j lt n_models-1 DO BEGIN
               temp=all_models_bivar_corr(m,i,all_max_lead_times(i)-1)-overlap_rows(k,j)
;         print,temp
               j=j+1
               IF ABS(temp) le 0.025 THEN BEGIN
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
         ENDIF ELSE BEGIN
            overlap_rows(this_row,i)=all_models_bivar_corr(m,i,all_max_lead_times(i)-1)
            distance=row_distance*this_row+initial_distance
            GPLOT,X=all_max_lead_times(i)-1+distance,Y=REFORM(all_models_bivar_corr(m,i,all_max_lead_times(i)-1))-0.02,$
                  COL=FSC_COLOR(all_colors(i)),TEXT=all_descs(i),ALIGN=0 
         ENDELSE
      ENDELSE
   ENDFOR
   toplot=REFORM(persistence_bivar_corr(m,*))
   toplot=toplot[where(toplot gt 0.1)]
   GPLOT,X=our_lead_times+0.5,Y=toplot,COL=FSC_COLOR('black'),STYLE=0
   GPLOT,X=N_ELEMENTS(toplot),Y=MIN(toplot),COL=FSC_COLOR('black'),TEXT='PE'
;   GPLOT,X=[MIN(our_lead_times),MAX(our_lead_times)+1],Y=[0,0],STYLE=1
   ;GLEGEND,labels=REVERSE(all_descs),COL=REVERSE(FSC_COLOR(all_colors)),LEGPOS=3
   AXES,XTITLE='Lead time (days)',XVALS=our_lead_times+0.5,XLABELS=STRTRIM(STRING(our_lead_times+1),1),$
        YSTEP=0.05,YMINOR=0.025,YTITLE='Bi-variate correlation with observed RMM ('+type+' only)',NDECS=2
   PSCLOSE

   psfile='/home/ss901165/idl/mjo_diabatic/rmm_indices/mjo_diabatic_rmm_correlations_components.rmse_'+$
          type+'_'+start_date+'-'+stop_date+'_tolead'+STRTRIM(STRING(MAX(our_lead_times)),1)+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2000,SPACE3=50,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=90,CB_WIDTH=110
   GSET,XMIN=MIN(our_lead_times),XMAX=MAX(our_lead_times)+1,YMIN=0,YMAX=rmse_max,$
        TITLE='RMSE of hindcast and observed RMM1 and RMM2 ('+type+' only) for '+start_date+' to '+stop_date
   FOR i=0,n_models-1 DO BEGIN
      GPLOT,X=our_lead_times+0.5,Y=REFORM(all_models_rmm1_rmse(1,i,*)),COL=FSC_COLOR(all_colors(i)),STYLE=0
      GPLOT,X=our_lead_times+0.5,Y=REFORM(all_models_rmm2_rmse(1,i,*)),COL=FSC_COLOR(all_colors(i)),STYLE=2
   ENDFOR
   GPLOT,X=our_lead_times+0.5,Y=REFORM(persistence_rmm1_rmse(m,*)),COL=FSC_COLOR('black'),STYLE=0
   GPLOT,X=our_lead_times+0.5,Y=REFORM(persistence_rmm2_rmse(m,*)),COL=FSC_COLOR('black'),STYLE=2
   GLEGEND,labels=all_descs,COL=FSC_COLOR(all_colors),LEGPOS=1
   GLEGEND,labels=['RMM2','RMM1'],COL=[FSC_COLOR('black'),FSC_COLOR('black')],STYLE=[2,0],LEGPOS=11
   AXES,XTITLE='Lead time (days)',XSTEP=1,YSTEP=rmse_max/10.,YMINOR=rmse_max/20.,YTITLE='RMSE with observed RMM indices ('+type+' only, '+obs+' obs)',NDECS=3
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/mjo_diabatic/rmm_indices/mjo_diabatic_rmm_correlations_components.rmse_bivar_'+type+$
          '_'+start_date+'-'+stop_date+'_tolead'+STRTRIM(STRING(MAX(our_lead_times)),1)+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE3=50,XOFFSET=2000,YOFFSET=500,TFONT=2,TCHARSIZE=90,CB_WIDTH=110
   GSET,XMIN=MIN(our_lead_times),XMAX=MAX(our_lead_times)+3,YMIN=0,YMAX=bivar_rmse_max,$
        TITLE='Bi-variate RMSE of hindcast and observed RMM1 and RMM2 ('+type+' only) for '+start_date+' to '+stop_date   
   overlap_rows=fltarr(5,n_models+1)
   overlap_rows(*,0)=0
   row_distance=0.75
   initial_distance=0.75
   FOR i=0,n_models-1 DO BEGIN
      GPLOT,X=our_lead_times+0.5,Y=REFORM(all_models_bivar_rmse(m,i,*)),COL=FSC_COLOR(all_colors(i))
      FOR k=0,N_ELEMENTS(overlap_rows(*,0))-1 DO BEGIN
         j=0
         WHILE j lt n_models-1 DO BEGIN
            temp=all_models_bivar_rmse(m,i,all_max_lead_times(i)-1)-overlap_rows(k,j)
;         print,temp
            j=j+1
            IF ABS(temp) le bivar_rmse_max/40. THEN BEGIN
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
      ENDIF ELSE BEGIN
         overlap_rows(this_row,i)=all_models_bivar_rmse(m,i,all_max_lead_times(i)-1)
         distance=row_distance*this_row+initial_distance
         GPLOT,X=all_max_lead_times(i)-1+distance,Y=REFORM(all_models_bivar_rmse(m,i,all_max_lead_times(i)-1)),$
               COL=FSC_COLOR(all_colors(i)),TEXT=all_descs(i),ALIGN=0 
      ENDELSE
   ENDFOR
   GPLOT,X=our_lead_times+0.5,Y=REFORM(persistence_bivar_rmse(m,*))
   GPLOT,X=MAX(our_lead_times)+0.5,Y=persistence_bivar_rmse(m,n_lead_times-1),COL=FSC_COLOR('black'),TEXT='PE'
   GLEGEND,labels=all_descs,COL=FSC_COLOR(all_colors),LEGPOS=1
   AXES,XTITLE='Lead time (days)',XSTEP=1,YSTEP=bivar_rmse_max/10.,YMINOR=bivar_rmse_max/20.,$
        YTITLE='Bi-variate RMSE with observed RMM indices ('+type+' only, '+type+' obs)',NDECS=3
   PSCLOSE,/NOVIEW

ENDFOR

psfile='/home/ss901165/idl/mjo_diabatic/rmm_indices/mjo_diabatic_rmm_correlations_components.spread_rmm_olr_'+start_date+'-'+stop_date+'_tolead'+STRTRIM(STRING(MAX(our_lead_times)),1)+'.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE3=50,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=90,CB_WIDTH=110
GSET,XMIN=MIN(our_lead_times),XMAX=MAX(our_lead_times)+1,YMIN=0,YMAX=0.48,TITLE='Spread in hindcast RMM1 and RMM2 (OLR only) with lead time from '+$
     start_date+' to '+stop_date
FOR i=0,n_models-1 DO BEGIN
   GPLOT,X=our_lead_times+0.5,Y=REFORM(all_models_rmm1_stddev(0,i,*)),COL=FSC_COLOR(all_colors(i)),SYM=4
   GPLOT,X=our_lead_times+0.5,Y=REFORM(all_models_rmm2_stddev(0,i,*)),COL=FSC_COLOR(all_colors(i)),SYM=6
ENDFOR
GPLOT,X=our_lead_times+0.5,Y=REFORM(persistence_rmm1_stddev(0,*)),COL=FSC_COLOR('black'),SYM=4
GPLOT,X=our_lead_times+0.5,Y=REFORM(persistence_rmm2_stddev(0,*)),COL=FSC_COLOR('black'),SYM=6
;GPLOT,X=our_lead_times+0.5,Y=persistence_bivar_rmse,COL=FSC_COLOR('black'),STYLE=0
GLEGEND,labels=all_descs,COL=FSC_COLOR(all_colors),LEGPOS=1,SYM=REPLICATE(4,n_models+1),LENGTH=0
GLEGEND,labels=['RMM2','RMM1'],SYM=[6,4],LENGTH=0,LEGPOS=9
AXES,XTITLE='Lead time (days)',XSTEP=1,YSTEP=0.06,YMINOR=0.02,YTITLE='Standard deviation (unitless) of RMM indices (OLR only) at constant lead time',NDECS=2
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/rmm_indices/mjo_diabatic_rmm_correlations_components.spread_rmm_u850_'+start_date+'-'+stop_date+'_tolead'+STRTRIM(STRING(MAX(our_lead_times)),1)+'.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE3=50,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=90,CB_WIDTH=110
GSET,XMIN=MIN(our_lead_times),XMAX=MAX(our_lead_times)+1,YMIN=0,YMAX=1.2,TITLE='Spread in hindcast RMM1 and RMM2 (U850 only) with lead time from '+$
     start_date+' to '+stop_date
FOR i=0,n_models-1 DO BEGIN
   GPLOT,X=our_lead_times+0.5,Y=REFORM(all_models_rmm1_stddev(1,i,*)),COL=FSC_COLOR(all_colors(i)),SYM=4
   GPLOT,X=our_lead_times+0.5,Y=REFORM(all_models_rmm2_stddev(1,i,*)),COL=FSC_COLOR(all_colors(i)),SYM=6
ENDFOR
GPLOT,X=our_lead_times+0.5,Y=REFORM(persistence_rmm1_stddev(1,*)),COL=FSC_COLOR('black'),SYM=4
GPLOT,X=our_lead_times+0.5,Y=REFORM(persistence_rmm2_stddev(1,*)),COL=FSC_COLOR('black'),SYM=6
;GPLOT,X=our_lead_times+0.5,Y=persistence_bivar_rmse,COL=FSC_COLOR('black'),STYLE=0
GLEGEND,labels=all_descs,COL=FSC_COLOR(all_colors),LEGPOS=1,SYM=REPLICATE(4,n_models+1),LENGTH=0
GLEGEND,labels=['RMM2','RMM1'],SYM=[6,4],LENGTH=0,LEGPOS=9
AXES,XTITLE='Lead time (days)',XSTEP=1,YSTEP=0.1,YMINOR=0.05,YTITLE='Standard deviation (unitless) of RMM indices (U850 only) at constant lead time',NDECS=2
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/rmm_indices/mjo_diabatic_rmm_correlations_components.spread_rmm_u200_'+start_date+'-'+stop_date+'_tolead'+STRTRIM(STRING(MAX(our_lead_times)),1)+'.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE3=50,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=90,CB_WIDTH=110
GSET,XMIN=MIN(our_lead_times),XMAX=MAX(our_lead_times)+1,YMIN=0,YMAX=1.2,TITLE='Spread in hindcast RMM1 and RMM2 (U200 only) with lead time from '+$
     start_date+' to '+stop_date
FOR i=0,n_models-1 DO BEGIN
   GPLOT,X=our_lead_times+0.5,Y=REFORM(all_models_rmm1_stddev(2,i,*)),COL=FSC_COLOR(all_colors(i)),SYM=4
   GPLOT,X=our_lead_times+0.5,Y=REFORM(all_models_rmm2_stddev(2,i,*)),COL=FSC_COLOR(all_colors(i)),SYM=6
ENDFOR
GPLOT,X=our_lead_times+0.5,Y=REFORM(persistence_rmm1_stddev(2,*)),COL=FSC_COLOR('black'),SYM=4
GPLOT,X=our_lead_times+0.5,Y=REFORM(persistence_rmm2_stddev(2,*)),COL=FSC_COLOR('black'),SYM=6
;GPLOT,X=our_lead_times+0.5,Y=persistence_bivar_rmse,COL=FSC_COLOR('black'),STYLE=0
GLEGEND,labels=all_descs,COL=FSC_COLOR(all_colors),LEGPOS=1,SYM=REPLICATE(4,n_models+1),LENGTH=0
GLEGEND,labels=['RMM2','RMM1'],SYM=[6,4],LENGTH=0,LEGPOS=9
AXES,XTITLE='Lead time (days)',XSTEP=1,YSTEP=0.1,YMINOR=0.05,YTITLE='Standard deviation (unitless) of RMM indices (U200 only) at constant lead time',NDECS=2
PSCLOSE,/NOVIEW



STOP
END

