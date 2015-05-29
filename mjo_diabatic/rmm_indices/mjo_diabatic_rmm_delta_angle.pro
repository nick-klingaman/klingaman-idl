PRO mjo_diabatic_rmm_delta_angle,model_names=model_names,start_date,stop_date,delta_time

IF KEYWORD_SET(model_names) THEN BEGIN
   our_model_names=model_names
ENDIF ELSE $
   our_model_names=['ecmwf','mri','nasa','nrl','metum','cancm4','spcam','miroc','giss','cam5','cam5zm','ecearth','cnrm_atm']

n_models=N_ELEMENTS(our_model_names)

n_times_per_day=8
standard_valid_dates=STRTRIM(STRING([indgen(22)+20091010,indgen(25)+20091101,indgen(22)+20091210,indgen(25)+20100101]),1)
mogreps_valid_dates=STRTRIM(STRING([indgen(17)+20091015,indgen(20)+20091101,indgen(11)+20091215,indgen(4)+20091227,indgen(16)+20100101]),1)
dates=STRTRIM(STRING([indgen(30)+20090901,indgen(31)+20091001,indgen(30)+20091101,indgen(31)+20091201,indgen(31)+20100101,indgen(28)+20100201]),1)

CASE delta_time OF 
   10 : BEGIN
      angle_bins=[-70,-50,-30,-10,10,30,50,70,90,110,130,150,170,190]
      raw_ymax=0.17
      raw_ystep=0.02
      diff_ymin=-0.08
      diff_ymax=0.12
      diff_ystep=0.01
   END
   5 : BEGIN
      angle_bins=[-65,-55,-45,-35,-25,-15,-5,5,15,25,35,45,55,65,75,85,95]
      raw_ymax=0.17
      raw_ystep=0.02
      diff_ymin=-0.12
      diff_ymax=0.12
      diff_ystep=0.02
   END
   15 : BEGIN
      angle_bins=[-105,-75,-45,-15,15,45,75,105,135,165,195,225,255]     
      raw_ymax=0.21
      raw_ystep=0.02
      diff_ymin=-0.10
      diff_ymax=0.16
      diff_ystep=0.02
   END
ENDCASE
n_bins=N_ELEMENTS(angle_bins)

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

obs_delta_angle=fltarr(n_days)
obs_binned_angle=fltarr(n_bins+1)
date_offset=where(dates eq start_date)
FOR j=0,n_days-1 DO BEGIN
   offset=REFORM(j+date_offset)
   IF where(standard_valid_dates eq dates(offset(0))) eq -1 THEN BEGIN
      obs_delta_angle(j)=!Values.F_NaN
   ENDIF ELSE BEGIN
      local_delta=fltarr(delta_time)
      FOR k=0,delta_time-1 DO BEGIN
         obs_angle=fltarr(2)
         FOR m=0,1 DO BEGIN
            CASE m OF 
               0 : BEGIN
                  obs_rmm1=$
                     REFORM(OPEN_AND_EXTRACT(obs_infile,'rmm1',$
                                             offset=[FLOOR(start_julian)+j+k-obs_start_date],count=[1]))   
                  obs_rmm2=$
                     REFORM(OPEN_AND_EXTRACT(obs_infile,'rmm2',$
                                             offset=[FLOOR(start_julian)+j+k-obs_start_date],count=[1]))
               END
               1 : BEGIN
                  obs_rmm1=$
                     REFORM(OPEN_AND_EXTRACT(obs_infile,'rmm1',$
                                             offset=[FLOOR(start_julian)+j+k+1-obs_start_date],count=[1]))   
                  obs_rmm2=$
                     REFORM(OPEN_AND_EXTRACT(obs_infile,'rmm2',$
                                             offset=[FLOOR(start_julian)+j+k+1-obs_start_date],count=[1]))
               END
            ENDCASE
            obs_angle(m)=ATAN(obs_rmm2/obs_rmm1)
            IF obs_rmm2 lt 0 and obs_rmm1 lt 0 THEN BEGIN
               obs_angle(m)=obs_angle(m)+!Pi
            ENDIF ELSE IF obs_rmm2 lt 0 and obs_rmm1 ge 0 THEN BEGIN
               obs_angle(m)=obs_angle(m)+2*!Pi
            ENDIF ELSE IF obs_rmm2 ge 0 and obs_rmm1 lt 0 THEN $
               obs_angle(m)=obs_angle(m)+!Pi
            obs_angle(m)=obs_angle(m)*180./!Pi
         ENDFOR       
         local_delta(k)=obs_angle(1)-obs_angle(0)
         IF local_delta(k) lt -180 THEN $
            local_delta(k)=local_delta(k)+360.
      ENDFOR
      obs_delta_angle(j)=TOTAL(local_delta)
   ENDELSE
ENDFOR
temp=obs_delta_angle[where(FINITE(obs_delta_angle) eq 1)]
FOR j=0,n_bins-2 DO $
   IF TOTAL(where(temp ge angle_bins(j) and temp lt angle_bins(j+1))) ge 0 THEN $
      obs_binned_angle(j+1)=N_ELEMENTS(where(temp ge angle_bins(j) and temp lt angle_bins(j+1)))
IF TOTAL(where(temp lt angle_bins(0))) ge 0 THEN $
   obs_binned_angle(0)=N_ELEMENTS(where(temp lt angle_bins(0)))
IF TOTAL(where(temp ge angle_bins(n_bins-1))) ge 0 THEN $
   obs_binned_angle(n_bins)=N_ELEMENTS(where(temp ge angle_bins(n_bins-1)))
obs_binned_angle(*)=obs_binned_angle/FLOAT(N_ELEMENTS(temp))

; Handle models

all_models_delta_angle=fltarr(n_models,n_days)
all_models_binned_angle=fltarr(n_models,n_bins+1)
all_colors=strarr(n_models)
all_descs=strarr(n_models)

FOR i=0,n_models-1 DO BEGIN
   CASE our_model_names(i) OF
      'ecmwf' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/ecmwf'
         all_descs(i)='EC'         
         all_colors(i)='red'
         valid_dates=standard_valid_dates
         max_lead_time=20
      END
      'miroc' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/miroc'
         all_descs(i)='MI'
         all_colors(i)='orange'
         valid_dates=standard_valid_dates
         max_lead_time=20
      END
      'mri' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/mri'
         all_descs(i)='MR'
         all_colors(i)='blue'
         valid_dates=standard_valid_dates
         max_lead_time=20
      END
      'nasa' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/nasa'
         all_descs(i)='G5'
         all_colors(i)='cyan'
         valid_dates=standard_valid_dates
         max_lead_time=20
      END      
      'nrl'  : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/nrl'
         all_descs(i)='NR'
         all_colors(i)='brown'
         valid_dates=standard_valid_dates
         max_lead_time=20
      END
      'iis'  : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/iis'
         all_descs(i)='II'
         all_colors(i)='darkgrey'
         valid_dates=standard_valid_dates
         max_lead_time=20
      END
      'metum' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/metum'
         all_descs(i)='MO'
         all_colors(i)='darkgoldenrod'
         valid_dates=standard_valid_dates
         max_lead_time=20
      END
      'metum_annatmos' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/metum_coupled_ann/Atmos_control'
         all_descs(i)='MA'
         all_colors(i)='maroon'
         valid_dates=mogreps_valid_dates
         max_lead_time=14
      END
      'metum_anncouple' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/metum_coupled_ann/Coupled_model'
         all_descs(i)='MC'
         all_colors(i)='orangered'
         valid_dates=mogreps_valid_dates
         max_lead_time=14
      END
      'spcam' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/spcam'
         all_descs(i)='SP'
         all_colors(i)='purple'
         valid_dates=standard_valid_dates
         max_lead_time=20
      END
      'giss' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/giss'
         all_descs(i)='GI'
         all_colors(i)='violetred'
         valid_dates=standard_valid_dates
         max_lead_time=20
      END
      'cancm4' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cancm4'
         all_descs(i)='CC'
         all_colors(i)='dodgerblue'
         valid_dates=standard_valid_dates
         max_lead_time=20
      END
      'cam5' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cam5'
         all_descs(i)='C5'
         all_colors(i)='deeppink'
         valid_dates=standard_valid_dates
         max_lead_time=20
      END
      'cam5zm' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cam5zm'
         all_descs(i)='CZ'
         all_colors(i)='steelblue'
         valid_dates=standard_valid_dates
         max_lead_time=20
      END
      'ecearth' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/ecearth'
         all_descs(i)='EE'
         all_colors(i)='limegreen'
         valid_dates=standard_valid_dates
         max_lead_time=20
      END
      'cnrm_atm' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cnrm_atmos'
         all_descs(i)='CA'
         all_colors(i)='navy'
         valid_dates=standard_valid_dates
         max_lead_time=20
      END
   ENDCASE
   
   date_offset=where(dates eq start_date)
   FOR j=0,n_days-1 DO BEGIN
      today_year=STRMID(dates(j+date_offset),0,4)
      today_month=STRMID(dates(j+date_offset),4,2)
      today_date=STRMID(dates(j+date_offset),6,2)
      ;print,'Today = ',today_year+today_month+today_date
      
      offset=REFORM(j+date_offset)      
      IF where(valid_dates eq dates(offset(0))) eq -1 THEN BEGIN
         all_models_delta_angle(i,j)=!Values.F_NaN
      ENDIF ELSE BEGIN
         initial_date=dates(j+date_offset)
         ;print,'file date = ',initial_date
         model_infile=indir+'/'+REFORM(initial_date)+'/rmm_indices.nc'
         local_delta=fltarr(delta_time)
         FOR k=0,delta_time-1 DO BEGIN
            model_angle=fltarr(2)
            FOR m=0,1 DO BEGIN
               CASE m OF 
                  0 : BEGIN
                     model_rmm1_day=OPEN_AND_EXTRACT(model_infile(0),'rmm1',offset=[k*n_times_per_day],count=[n_times_per_day])
                     model_rmm2_day=OPEN_AND_EXTRACT(model_infile(0),'rmm2',offset=[k*n_times_per_day],count=[n_times_per_day])
                  END
                  1 : BEGIN
                     model_rmm1_day=OPEN_AND_EXTRACT(model_infile(0),'rmm1',offset=[(k+1)*n_times_per_day],count=[n_times_per_day])
                     model_rmm2_day=OPEN_AND_EXTRACT(model_infile(0),'rmm2',offset=[(k+1)*n_times_per_day],count=[n_times_per_day])
                  END
               ENDCASE              
               model_rmm1=MEAN(model_rmm1_day)
               model_rmm2=MEAN(model_rmm2_day)
               model_angle(m)=ATAN(model_rmm2/model_rmm1)
               IF model_rmm2 lt 0 and model_rmm1 lt 0 THEN BEGIN
                  model_angle(m)=model_angle(m)+!Pi
               ENDIF ELSE IF model_rmm2 lt 0 and model_rmm1 ge 0 THEN BEGIN
                  model_angle(m)=model_angle(m)+2*!Pi
               ENDIF ELSE IF model_rmm2 ge 0 and model_rmm1 lt 0 THEN $
                  model_angle(m)=model_angle(m)+!Pi
               model_angle(m)=model_angle(m)*180./!Pi
            ENDFOR       
            local_delta(k)=model_angle(1)-model_angle(0)
            IF local_delta(k) lt -180 THEN $
               local_delta(k)=local_delta(k)+360.
         ENDFOR
         all_models_delta_angle(i,j)=TOTAL(local_delta)
      ENDELSE
   ENDFOR

   all_models_binned_angle(i,*)=0.
   temp=REFORM(all_models_delta_angle(i,*))
   temp=temp[where(FINITE(temp) eq 1)]
   FOR j=0,n_bins-2 DO $
      IF TOTAL(where(temp ge angle_bins(j) and temp lt angle_bins(j+1))) ge 0 THEN $
         all_models_binned_angle(i,j+1)=N_ELEMENTS(where(temp ge angle_bins(j) and temp lt angle_bins(j+1)))
   IF TOTAL(where(temp lt angle_bins(0))) ge 0 THEN $
      all_models_binned_angle(i,0)=N_ELEMENTS(where(temp lt angle_bins(0)))
   IF TOTAL(where(temp ge angle_bins(n_bins-1))) ge 0 THEN $
      all_models_binned_angle(i,n_bins)=N_ELEMENTS(where(temp ge angle_bins(n_bins-1)))
   all_models_binned_angle(i,*)=all_models_binned_angle(i,*)/FLOAT(N_ELEMENTS(temp))

ENDFOR
         
psfile='/home/ss901165/idl/mjo_diabatic/rmm_indices/mjo_diabatic_rmm_delta_angle.'+start_date+'-'+stop_date+'_'+STRTRIM(STRING(delta_time),1)+'days_pdf.ps'
PSOPEN,file=psfile,TFONT=2,FONT=2,CHARSIZE=130,MARGIN=3500,XOFFSET=-800
GSET,XMIN=0,XMAX=n_bins+1,YMIN=0,YMAX=raw_ymax
GPLOT,X=indgen(n_bins+1)+0.5,Y=SMOOTH(obs_binned_angle,3),COL=FSC_COLOR('black'),THICK=200
FOR i=0,n_models-1 DO $
   GPLOT,X=indgen(n_bins+1)+0.5,Y=SMOOTH(REFORM(all_models_binned_angle(i,*)),3),COL=FSC_COLOR(all_colors(i)),THICK=100
AXES,XVALS=indgen(n_bins+2),XLABELS=['< '+STRMID(STRTRIM(STRING(angle_bins(0)),1),0,4),$
                                     STRMID(STRTRIM(STRING(angle_bins),1),0,4),$
                                     '>= '+STRMID(STRTRIM(STRING(angle_bins(n_bins-1)),1),0,3)],YSTEP=raw_ystep,YMINOR=raw_ystep/2.,$
     XTITLE='Change in phase angle over '+STRTRIM(STRING(delta_time),1)+' days (degrees, positive anti-clockwise)',YTITLE='Probability',NDECS=2
GLEGEND,labels=REVERSE([all_descs,'OB']),COL=REVERSE([FSC_COLOR(all_colors),FSC_COLOR('black')]),LEGXOFFSET=4000,LEGYOFFSET=12000,LENGTH=20
PSCLOSE

psfile='/home/ss901165/idl/mjo_diabatic/rmm_indices/mjo_diabatic_rmm_delta_angle.'+start_date+'-'+stop_date+'_'+STRTRIM(STRING(delta_time),1)+'days_pdf_diff.ps'
PSOPEN,file=psfile,TFONT=2,FONT=2,CHARSIZE=130,MARGIN=3500,XOFFSET=-800
; For 10 days
GSET,XMIN=0,XMAX=n_bins+1,YMIN=diff_ymin,YMAX=diff_ymax
;GPLOT,X=indgen(n_bins+1)+0.5,Y=SMOOTH(obs_binned_angle,3),COL=FSC_COLOR('black'),THICK=200
FOR i=0,n_models-1 DO $
   GPLOT,X=indgen(n_bins+1)+0.5,Y=SMOOTH(REFORM(all_models_binned_angle(i,*)),3)-SMOOTH(obs_binned_angle,3),COL=FSC_COLOR(all_colors(i)),THICK=100
GPLOT,X=[0,n_bins+1],Y=[0,0],STYLE=1
AXES,XVALS=indgen(n_bins+2),XLABELS=['< '+STRMID(STRTRIM(STRING(angle_bins(0)),1),0,4),$
                                     STRMID(STRTRIM(STRING(angle_bins),1),0,4),$
                                     '>= '+STRMID(STRTRIM(STRING(angle_bins(n_bins-1)),1),0,3)],YSTEP=diff_ystep,YMINOR=diff_ystep/2.,$
     XTITLE='Change in phase angle over '+STRTRIM(STRING(delta_time),1)+' days (degrees, positive anti-clockwise)',$
     YTITLE='Difference in probability from observations',NDECS=2
GLEGEND,labels=REVERSE([all_descs,'OB']),COL=REVERSE([FSC_COLOR(all_colors),FSC_COLOR('black')]),LEGXOFFSET=4000,LEGYOFFSET=12000,LENGTH=20
PSCLOSE
                                     

STOP
END
