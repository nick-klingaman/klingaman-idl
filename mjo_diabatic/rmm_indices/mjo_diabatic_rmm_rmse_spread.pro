PRO mjo_diabatic_rmm_rmse_spread,model_names=model_names,start_date,stop_date,lead_time

IF KEYWORD_SET(model_names) THEN BEGIN
   our_model_names=model_names
ENDIF ELSE $
   our_model_names=['cam5zm','cam5','ecmwf','mri','metum','nasa','cnrm_atmos','ecearth','miroc','giss','spcam','nrl','cancm4']

CASE lead_time OF
   10 : BEGIN
      ymax=3.2
      ystep=0.2
      yminor=0.1
      diff_ymin=-2.7
      diff_ymax=1.5
      diff_ystep=0.3
      diff_yminor=0.15
      persist_ymax=2.5
   END
   20 : BEGIN
      ymax=4.8
      ystep=0.4
      yminor=0.2
      diff_ymin=-2.7
      diff_ymax=1.8
      diff_ystep=0.3
      diff_yminor=0.15
      persist_ymax=3.0
   END  
ENDCASE

n_models=N_ELEMENTS(our_model_names)

n_times_per_day=8
dates=STRTRIM(STRING([indgen(22)+20091010,indgen(30)+20091101,indgen(31)+20091201,indgen(31)+20100101]),1)
standard_valid_dates=STRTRIM(STRING([indgen(22)+20091010,indgen(25)+20091101,indgen(22)+20091210,indgen(25)+20100101]),1)
mogreps_valid_dates=STRTRIM(STRING([indgen(17)+20091015,indgen(20)+20091101,indgen(11)+20091215,indgen(5)+20091227,indgen(16)+20100101]),1)
entrain_valid_dates=STRTRIM(STRING([indgen(15)+20091010,indgen(25)+20100101]),1)

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
IF stop_julian lt start_julian THEN $;BEGIN
   stop_julian=stop_julian+365
;ENDIF ELSE $
n_days=FLOOR(stop_julian-start_julian+1)
   
; Get observations
obs_infile='/home/ss901165/datasets/ECMWF_YOTC/ECMWF_YOTC.oct2009-feb2010_dmeans.rmm_indices.nc'
obs_start_date=274
IF start_julian lt obs_start_date THEN $
   start_julian=start_julian+365
obs_rmm1=fltarr(n_days,lead_time)
obs_rmm2=fltarr(n_days,lead_time)
persist_rmm1=fltarr(n_days,lead_time)
persist_rmm2=fltarr(n_days,lead_time)
rmse_persist=fltarr(n_days)

FOR j=0,n_days-1 DO BEGIN
   FOR k=0,lead_time-1 DO BEGIN
      obs_rmm1(j,k)=$
         REFORM(OPEN_AND_EXTRACT(obs_infile,'rmm1',$
                                 offset=[FLOOR(start_julian)+j-obs_start_date+k],count=[1]))
      obs_rmm2(j,k)=$
         REFORM(OPEN_AND_EXTRACT(obs_infile,'rmm2',$
                                 offset=[FLOOR(start_julian)+j-obs_start_date+k],count=[1]))
   ENDFOR
   persist_rmm1(j,*)=obs_rmm1(j,0)
   persist_rmm2(j,*)=obs_rmm2(j,0)
   rmse_persist(j)=SQRT(TOTAL((obs_rmm1(j,*)-persist_rmm1(j,*))^2+$
                              (obs_rmm2(j,*)-persist_rmm2(j,*))^2)/FLOAT(lead_time-1))
ENDFOR
sort_rmse_persist=SORT(rmse_persist)

all_models_rmse_obs=fltarr(n_models,6)
all_models_rmse_persist=fltarr(n_models,6)
all_models_diff_persist=fltarr(n_models,6)
all_models_rmse_bypersist=fltarr(n_models,3,6)
all_descs=strarr(n_models)
FOR i=0,n_models-1 DO BEGIN
   CASE our_model_names(i) OF 
      'ecmwf' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/ecmwf'
         all_descs(i)='ECMWF_IFS'
         valid_dates=standard_valid_dates
      END
      'miroc' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/miroc'
         all_descs(i)='MIROC5'
         valid_dates=standard_valid_dates
      END
      'mri' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/mri'
         all_descs(i)='MRI-AGCM'
         valid_dates=standard_valid_dates
      END
      'nasa' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/nasa'
         all_descs(i)='GEOS5_AGCM'
         valid_dates=standard_valid_dates
      END      
      'nrl'  : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/nrl'
         all_descs(i)='NRL_NGEM01'
         valid_dates=standard_valid_dates
      END
      'iis'  : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/iis'
         all_descs(i)='gfs2_iis'
         valid_dates=standard_valid_dates
      END
      'metum' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/metum'
         all_descs(i)='MetUM_GA3'
         valid_dates=standard_valid_dates
      END
      'metum_annatmos' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/metum_coupled_ann/Atmos_control'
         all_descs(i)='metum_mogreps_atmos'
         valid_dates=mogreps_valid_dates
      END
      'metum_anncouple' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/metum_coupled_ann/Coupled_model'
         all_descs(i)='metum_mogreps_couple'
         valid_dates=mogreps_valid_dates
      END
      'metum_highentrain' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/metum_1.5xent_nick'
         all_descs(i)='metum_1.5xent'
         valid_dates=standard_valid_dates
      END
      'spcam' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/spcam'
         all_descs(i)='SPCAM3'
         valid_dates=standard_valid_dates
      END
      'giss' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/giss'
         all_descs(i)='GISS_ModelE'
         valid_dates=standard_valid_dates
      END
      'cancm4' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cancm4'
         all_descs(i)='CCCma_CanCM4'
         valid_dates=standard_valid_dates
      END
      'nicam' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/nicam'
         all_descs(i)='NICAM'
         valid_dates=STRTRIM(['20091015','20091020','20091025','20091030',$
                              '20091104','20091109','20091215','20091220',$
                              '20091225','20091230','20100104','20100109'])
      END
      'cam5zm' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cam5zm'
         all_descs(i)='CAM5_ZM'
         valid_dates=standard_valid_dates
      END
      'cam5' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cam5'
         all_descs(i)='CAM5'
         valid_dates=standard_valid_dates
      END
      'ecearth' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/ecearth'
         all_descs(i)='ECEARTH'
         valid_dates=standard_valid_dates
      END
      'cnrm_atmos' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cnrm_atmos'
         all_descs(i)='CNRM_ATM'
         valid_dates=standard_valid_dates
      END
   ENDCASE

   start_position=REFORM(where(dates eq start_date))
   this_model_rmm1=fltarr(lead_time)
   this_model_rmm2=fltarr(lead_time)
   this_model_rmse_obs=fltarr(n_days)
   this_model_rmse_persist=fltarr(n_days)
   this_model_diff_persist=fltarr(n_days)
   this_model_rmse_bypersist=fltarr(3,n_days/3+1)
   
   FOR j=0,n_days-1 DO BEGIN
      IF TOTAL(where(valid_dates eq dates(j+start_position(0)))) ne -1 THEN BEGIN         
         FOR k=0,lead_time-1 DO BEGIN
            initial_date=valid_dates(where(valid_dates eq dates(j+start_position(0))))            
            model_infile=indir+'/'+REFORM(initial_date)+'/rmm_indices.nc'
            model_rmm1_day=OPEN_AND_EXTRACT(model_infile(0),'rmm1',offset=[k*n_times_per_day],count=[n_times_per_day])
            this_model_rmm1(k)=MEAN(model_rmm1_day)
            model_rmm2_day=OPEN_AND_EXTRACT(model_infile(0),'rmm2',offset=[k*n_times_per_day],count=[n_times_per_day])
            this_model_rmm2(k)=MEAN(model_rmm2_day)      
         ENDFOR
         today_obs_rmm1=REFORM(obs_rmm1(start_position(0)+j,*))
         today_obs_rmm2=REFORM(obs_rmm2(start_position(0)+j,*))
         today_persist_rmm1=REFORM(persist_rmm1(start_position(0)+j,*))
         today_persist_rmm2=REFORM(persist_rmm2(start_position(0)+j,*))
         this_model_rmse_obs(j)=SQRT(TOTAL((today_obs_rmm1-this_model_rmm1)^2+(today_obs_rmm2-this_model_rmm2)^2)/FLOAT(lead_time-1))
         this_model_rmse_persist(j)=SQRT(TOTAL((today_persist_rmm1-this_model_rmm1)^2+(today_persist_rmm2-this_model_rmm2)^2)/FLOAT(lead_time-1))
         this_model_diff_persist(j)=this_model_rmse_obs(j)-rmse_persist(start_position(0)+j)
      ENDIF ELSE BEGIN
         print,'No hindcast for date '+standard_valid_dates(j+start_position)      
         this_model_rmse_obs(j)=!Values.F_NaN
         this_model_rmse_persist(j)=!Values.F_NaN
         this_model_diff_persist(j)=!Values.F_NaN
      ENDELSE
   ENDFOR
   n_valid_days=N_ELEMENTS(where(FINITE(this_model_rmse_obs) eq 1))
   this_sort_rmse_persist=sort_rmse_persist[where(FINITE(this_model_rmse_obs) eq 1)]
   ; Mean/median/iqr_low/iqr_high/range_low/range_high
   all_models_rmse_obs(i,0)=MEAN(this_model_rmse_obs,/NaN)
   all_models_rmse_obs(i,1)=MEDIAN(this_model_rmse_obs)
   all_models_rmse_obs(i,5)=MAX(this_model_rmse_obs,/NaN)
   all_models_rmse_obs(i,4)=MIN(this_model_rmse_obs,/NaN)
   sorted=SORT(this_model_rmse_obs)
   all_models_rmse_obs(i,2)=this_model_rmse_obs(sorted(n_valid_days/4))
   all_models_rmse_obs(i,3)=this_model_rmse_obs(sorted(n_valid_days*3/4))
   ; Mean/median/iqr_low/iqr_high/range_low/range_high
   all_models_rmse_persist(i,0)=MEAN(this_model_rmse_persist,/NaN)
   all_models_rmse_persist(i,1)=MEDIAN(this_model_rmse_persist)
   all_models_rmse_persist(i,5)=MAX(this_model_rmse_persist,/NaN)
   all_models_rmse_persist(i,4)=MIN(this_model_rmse_persist,/NaN)
   sorted=SORT(this_model_rmse_persist)
   all_models_rmse_persist(i,2)=this_model_rmse_persist(sorted(n_valid_days/4))
   all_models_rmse_persist(i,3)=this_model_rmse_persist(sorted(n_valid_days*3/4))     
   ; Mean/median/iqr_low/iqr_high/range_low/range_high
   all_models_diff_persist(i,0)=MEAN(this_model_diff_persist,/NaN)
   all_models_diff_persist(i,1)=MEDIAN(this_model_diff_persist)
   all_models_diff_persist(i,5)=MAX(this_model_diff_persist,/NaN)
   all_models_diff_persist(i,4)=MIN(this_model_diff_persist,/NaN)
   sorted=SORT(this_model_diff_persist)
   all_models_diff_persist(i,2)=this_model_diff_persist(sorted(n_valid_days/4))
   all_models_diff_persist(i,3)=this_model_diff_persist(sorted(n_valid_days*3/4))

   valid=this_model_rmse_obs[where(FINITE(this_model_rmse_obs) eq 1)]
   this_model_rmse_bypersist(0,0:n_valid_days/3-1)=valid[this_sort_rmse_persist(0:N_ELEMENTS(this_sort_rmse_persist)/3-1)]
   this_model_rmse_bypersist(1,0:(n_valid_days*2/3-n_valid_days/3-1))=valid[this_sort_rmse_persist(N_ELEMENTS(this_sort_rmse_persist)/3:$
                                                                                                 N_ELEMENTS(this_sort_rmse_persist)*2/3-1)]
   this_model_rmse_bypersist(2,0:(n_valid_days-n_valid_days*2/3-1))=valid[this_sort_rmse_persist(N_ELEMENTS(this_sort_rmse_persist)*2/3:$
                                                                                                 N_ELEMENTS(this_sort_rmse_persist)-1)]
   this_model_rmse_bypersist[where(this_model_rmse_bypersist eq 0)]=!Values.F_NaN
   FOR j=0,2 DO BEGIN
      temp=REFORM(this_model_rmse_bypersist(j,*))
      all_models_rmse_bypersist(i,j,0)=MEAN(temp,/NaN)
      all_models_rmse_bypersist(i,j,1)=MEDIAN(temp)
      all_models_rmse_bypersist(i,j,5)=MAX(temp,/NaN)
      all_models_rmse_bypersist(i,j,4)=MIN(temp,/NaN)
      sorted=SORT(temp)     
      all_models_rmse_bypersist(i,j,2)=temp(sorted(N_ELEMENTS(where(FINITE(temp) eq 1))/4))
      all_models_rmse_bypersist(i,j,3)=temp(sorted(N_ELEMENTS(where(FINITE(temp) eq 1))*3/4))      
      IF all_models_rmse_bypersist(i,j,2) ge all_models_rmse_bypersist(i,j,3) THEN $
         STOP
   ENDFOR
ENDFOR

psfile='/home/ss901165/idl/mjo_diabatic/rmm_indices/mjo_diabatic_rmm_rmse_spread.'+$
       STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'.bivar_rmse_'+STRTRIM(STRING(lead_time),1)+'days.ps'
PSOPEN,file=psfile,TFONT=2,CHARSIZE=120,FONT=2,YOFFSET=1000,XOFFSET=1000,MARGIN=2000
GSET,XMIN=0,XMAX=n_models,YMIN=0,YMAX=ymax
FOR i=0,n_models-1 DO BEGIN
   EBAR,X=i+0.37,BOX=[all_models_rmse_obs(i,4),all_models_rmse_obs(i,2),$
                     all_models_rmse_obs(i,1),all_models_rmse_obs(i,3),$
                     all_models_rmse_obs(i,5)],COL=FSC_COLOR('blue')
   EBAR,X=i+0.63,BOX=[all_models_rmse_persist(i,4),all_models_rmse_persist(i,2),$
                     all_models_rmse_persist(i,1),all_models_rmse_persist(i,3),$
                     all_models_rmse_persist(i,5)],COL=FSC_COLOR('red')
ENDFOR
AXES,XVALS=findgen(n_models)+0.5,XLABELS=all_descs,ORIENTATION=45,$
     YTITLE='Bivariate RMSE over first '+STRTRIM(STRING(lead_time),1)+' days of forecast',YSTEP=ystep,YMINOR=yminor,NDECS=2
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/rmm_indices/mjo_diabatic_rmm_rmse_spread.'+$
       STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'.diff_persist_'+STRTRIM(STRING(lead_time),1)+'days.ps'
PSOPEN,file=psfile,TFONT=2,CHARSIZE=120,FONT=2,YOFFSET=1000,XOFFSET=1000,MARGIN=2000
GSET,XMIN=0,XMAX=n_models,YMIN=diff_ymin,YMAX=diff_ymax
FOR i=0,n_models-1 DO BEGIN
   EBAR,X=i+0.5,BOX=[all_models_diff_persist(i,4),all_models_diff_persist(i,2),$
                     all_models_diff_persist(i,1),all_models_diff_persist(i,3),$
                     all_models_diff_persist(i,5)],COL=FSC_COLOR('blue')
ENDFOR
GPLOT,X=[0,n_models],Y=[0,0],STYLE=1
AXES,XVALS=findgen(n_models)+0.5,XLABELS=all_descs,ORIENTATION=45,$
     YTITLE='Difference to persistence RMSE over first '+STRTRIM(STRING(lead_time),1)+' days of forecast',$
     YSTEP=diff_ystep,YMINOR=diff_yminor,NDECS=2
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/rmm_indices/mjo_diabatic_rmm_rmse_spread.'+$
       STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'.rmse_bypersist_'+STRTRIM(STRING(lead_time),1)+'days.ps'
PSOPEN,file=psfile,TFONT=2,CHARSIZE=120,FONT=2,YOFFSET=1000,XOFFSET=1000,MARGIN=2000
GSET,XMIN=0,XMAX=n_models,YMIN=0,YMAX=persist_ymax
colors=['purple','blue','red']
FOR i=0,n_models-1 DO $
   FOR j=0,2 DO $
      EBAR,X=i+0.3+0.2*j,BOX=[all_models_rmse_bypersist(i,j,4),all_models_rmse_bypersist(i,j,2),$
                              all_models_rmse_bypersist(i,j,1),all_models_rmse_bypersist(i,j,3),$
                              all_models_rmse_bypersist(i,j,5)],COL=FSC_COLOR(colors(j)),width=60

GPLOT,X=[0,n_models],Y=[0,0],STYLE=1
AXES,XVALS=findgen(n_models)+0.5,XLABELS=all_descs,ORIENTATION=45,$
     YTITLE='RMSE over first '+STRTRIM(STRING(lead_time),1)+' days of forecast, grouped by RMSE of persistence',$
     YSTEP=ystep,YMINOR=yminor,NDECS=2
PSCLOSE

STOP
END

