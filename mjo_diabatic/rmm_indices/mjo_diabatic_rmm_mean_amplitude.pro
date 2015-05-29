PRO mjo_diabatic_rmm_mean_amplitude,model_names=model_names,start_date,stop_date,lead_times=lead_times

IF KEYWORD_SET(model_names) THEN BEGIN
   our_model_names=model_names
ENDIF ELSE $
   our_model_names=['ecearth','miroc','cnrm_atmos','nasa','ecmwf','metum','mri','nrl','cancm4','spcam','giss','cam5','cam5zm','metum_annatmos','metum_anncouple']
IF KEYWORD_SET(lead_times) THEN BEGIN
   our_lead_times=lead_times
ENDIF ELSE $
   our_lead_times=indgen(20)
n_lead_times=N_ELEMENTS(our_lead_times)
n_models=N_ELEMENTS(our_model_names)

n_times_per_day=8
dates=STRTRIM(STRING([indgen(22)+20091010,indgen(30)+20091101,indgen(31)+20091201,indgen(31)+20100101]),1)
standard_valid_dates=STRTRIM(STRING([indgen(22)+20091010,indgen(25)+20091101,indgen(22)+20091210,indgen(25)+20100101]),1)
mogreps_valid_dates=STRTRIM(STRING([indgen(17)+20091015,indgen(20)+20091101,indgen(11)+20091215,indgen(4)+20091227,indgen(16)+20100101]),1)
;mogreps_valid_dates=STRTRIM(STRING([indgen(17)+20091015,indgen(20)+20091101,indgen(11)+20091215,indgen(5)+20091227,indgen(16)+20100101]),1)
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
obs_rmm1=fltarr(n_days,n_lead_times)
obs_rmm2=fltarr(n_days,n_lead_times)
obs_mean_amp=fltarr(n_lead_times,3)

FOR j=0,n_days-1 DO BEGIN
   FOR k=0,n_lead_times-1 DO BEGIN
      obs_rmm1(j,k)=$
         REFORM(OPEN_AND_EXTRACT(obs_infile,'rmm1',$
                                 offset=[FLOOR(start_julian)+j-obs_start_date+our_lead_times(k)],count=[1]))
      obs_rmm2(j,k)=$
         REFORM(OPEN_AND_EXTRACT(obs_infile,'rmm2',$
                                 offset=[FLOOR(start_julian)+j-obs_start_date+our_lead_times(k)],count=[1]))
   ENDFOR
ENDFOR

all_models_mean_amp=fltarr(n_models+2,n_lead_times,3)
all_models_max_lead_time=intarr(n_models+2)
all_colors=strarr(n_models+2)
all_descs=strarr(n_models+2)
FOR i=0,n_models-1 DO BEGIN
   CASE our_model_names(i) OF 
      'ecmwf' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/ecmwf'
         all_descs(i)='EC'         
         all_colors(i)='red'
         valid_dates=standard_valid_dates
         all_models_max_lead_time(i)=20
      END
      'miroc' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/miroc'
         all_descs(i)='MI'
         all_colors(i)='orange'
         valid_dates=standard_valid_dates
         all_models_max_lead_time(i)=20
      END
      'mri' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/mri'
         all_descs(i)='MR'
         all_colors(i)='blue'
         valid_dates=standard_valid_dates
         all_models_max_lead_time(i)=20
      END
      'nasa' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/nasa'
         all_descs(i)='NA'
         all_colors(i)='cyan'
         valid_dates=standard_valid_dates
         all_models_max_lead_time(i)=20
      END      
      'nrl'  : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/nrl'
         all_descs(i)='NR'
         all_colors(i)='brown'
         valid_dates=standard_valid_dates
         all_models_max_lead_time(i)=20
      END
      'iis'  : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/iis'
         all_descs(i)='II'
         all_colors(i)='darkgrey'
         valid_dates=standard_valid_dates
         all_models_max_lead_time(i)=20
      END
      'metum' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/metum'
         all_descs(i)='MO'
         all_colors(i)='darkgoldenrod'
         valid_dates=standard_valid_dates
         all_models_max_lead_time(i)=20
      END
      'metum_annatmos' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/metum_coupled_ann/Atmos_control'
         all_descs(i)='MA'
         all_colors(i)='maroon'
         valid_dates=mogreps_valid_dates
         all_models_max_lead_time(i)=14
      END
      'metum_anncouple' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/metum_coupled_ann/Coupled_model'
         all_descs(i)='MC'
         all_colors(i)='orangered'
         valid_dates=mogreps_valid_dates
         all_models_max_lead_time(i)=14
      END
      'metum_highentrain' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/metum_1.5xent_nick'
         all_descs(i)='MH'
         all_colors(i)='deeppink'
         valid_dates=standard_valid_dates
         all_models_max_lead_time(i)=20
      END
      'spcam' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/spcam'
         all_descs(i)='SP'
         all_colors(i)='purple'
         valid_dates=standard_valid_dates
         all_models_max_lead_time(i)=20
      END
      'giss' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/giss'
         all_descs(i)='GI'
         all_colors(i)='violetred'
         valid_dates=standard_valid_dates
         all_models_max_lead_time(i)=20
      END
      'cancm4' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cancm4'
         all_descs(i)='CC'
         all_colors(i)='dodgerblue'
         valid_dates=standard_valid_dates
         all_models_max_lead_time(i)=20
      END
      'cam5' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cam5'
         all_descs(i)='C5'
         all_colors(i)='deeppink'
         valid_dates=standard_valid_dates
         all_models_max_lead_time(i)=20
      END
      'cam5zm' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cam5zm'
         all_descs(i)='CZ'
         all_colors(i)='steelblue'
         valid_dates=standard_valid_dates
         all_models_max_lead_time(i)=20
      END
      'ecearth' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/ecearth'
         all_descs(i)='E3'
         all_colors(i)='limegreen'
         valid_dates=standard_valid_dates
         all_models_max_lead_time(i)=20
      END
      'cnrm_atmos' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cnrm_atmos'
         all_descs(i)='CN'
         all_colors(i)='navy'
         valid_dates=standard_valid_dates
         all_models_max_lead_time(i)=20
      END
   ENDCASE

   start_position=REFORM(where(dates eq start_date))
   this_model_rmm1=fltarr(n_days,n_lead_times)
   this_model_rmm2=fltarr(n_days,n_lead_times)
   print,our_model_names(i)
   FOR j=0,n_days-1 DO BEGIN
      IF TOTAL(where(valid_dates eq dates(j+start_position(0)))) ne -1 THEN BEGIN         
         FOR k=0,n_lead_times-1 DO BEGIN
            IF k le all_models_max_lead_time(i) THEN BEGIN
               initial_date=valid_dates(where(valid_dates eq dates(j+start_position(0))))            
               model_infile=indir+'/'+REFORM(initial_date)+'/rmm_indices.nc'
               model_rmm1_day=OPEN_AND_EXTRACT(model_infile(0),'rmm1',offset=[our_lead_times(k)*n_times_per_day],count=[n_times_per_day])
               this_model_rmm1(j,k)=MEAN(model_rmm1_day)
               model_rmm2_day=OPEN_AND_EXTRACT(model_infile(0),'rmm2',offset=[our_lead_times(k)*n_times_per_day],count=[n_times_per_day])
               this_model_rmm2(j,k)=MEAN(model_rmm2_day)      
            ENDIF
         ENDFOR         
         this_model_rmm1(j,0:3)=this_model_rmm1(j,0:3)-(this_model_rmm1(j,0)-obs_rmm1(j,0))*[0.7,0.4,0.3,0.1]
         this_model_rmm2(j,0:3)=this_model_rmm2(j,0:3)-(this_model_rmm2(j,0)-obs_rmm2(j,0))*[0.7,0.4,0.3,0.1]
      ENDIF ELSE BEGIN
         ;print,'No hindcast for date '+standard_valid_dates(j+start_position)      
         this_model_rmm1(j,*)=!Values.F_NaN
         this_model_rmm2(j,*)=!Values.F_NaN
      ENDELSE
   ENDFOR
   invalid_days=(where(FINITE(this_model_rmm1) eq 0))
   IF i eq 0 THEN BEGIN
      IF TOTAL(invalid_days) ge 0 THEN BEGIN
         obs_rmm1[invalid_days]=!Values.F_NaN
         obs_rmm2[invalid_days]=!Values.F_NaN
      ENDIF
      FOR k=0,n_lead_times-1 DO BEGIN      
         obs_mean_amp(k,0)=MEAN(ABS(obs_rmm1(*,k)),/NaN)
         obs_mean_amp(k,1)=MEAN(ABS(obs_rmm2(*,k)),/NaN)
         obs_mean_amp(k,2)=MEAN(SQRT(obs_rmm1(*,k)^2+obs_rmm2(*,k)^2),/NaN)
      ENDFOR
   ENDIF
   FOR k=0,n_lead_times-1 DO BEGIN
      IF k le all_models_max_lead_time(i) THEN BEGIN
         all_models_mean_amp(i,k,0)=MEAN(ABS(this_model_rmm1(*,k)),/NaN)
         all_models_mean_amp(i,k,1)=MEAN(ABS(this_model_rmm2(*,k)),/NaN)
         all_models_mean_amp(i,k,2)=MEAN(SQRT(this_model_rmm1(*,k)^2+this_model_rmm2(*,k)^2),/NaN)
      ENDIF ELSE BEGIN
         all_models_mean_amp(i,k,0)=!Values.F_NaN
         all_models_mean_amp(i,k,1)=!Values.F_NaN
         all_models_mean_amp(i,k,2)=!Values.F_NaN
      ENDELSE
   ENDFOR
ENDFOR

RESTORE,filename='/home/ss901165/um_output6/mjodiab_20day/YOTC_OLRuu_hindcasts.36lons.sav'
IF start_julian gt 365 THEN $
   start_julian=start_julian-365
lim_offset=DAYS_SINCE([122,2008],[start_julian,start_year])
lim_rmm1=[rmm1(*,lim_offset:lim_offset+46),rmm1(*,lim_offset+61:lim_offset+107)]/2.5
lim_rmm2=[rmm2(*,lim_offset:lim_offset+46),rmm2(*,lim_offset+61:lim_offset+107)]/2.2
lim_mean_amp=fltarr(2,20,3)
OPENR,lun1,'/home/ss901165/idl/mjo_diabatic/rmm_indices/LIM_RMM1_YoTC.txt',/GET_LUN
OPENR,lun2,'/home/ss901165/idl/mjo_diabatic/rmm_indices/LIM_RMM2_YoTC.txt',/GET_LUN
scripps_rmm1_in=fltarr(108,20)
scripps_rmm2_in=fltarr(108,20)
READF,lun1,scripps_rmm1_in
READF,lun2,scripps_rmm2_in
scripps_rmm1=[scripps_rmm1_in(0:46,*),scripps_rmm1_in(61:107,*)]
scripps_rmm2=[scripps_rmm2_in(0:46,*),scripps_rmm2_in(61:107,*)]
FOR i=0,n_lead_times-1 DO BEGIN
   all_models_mean_amp(n_models+0,i,0)=MEAN(ABS(lim_rmm1(i,*)),/NaN)
   all_models_mean_amp(n_models+0,i,1)=MEAN(ABS(lim_rmm2(i,*)),/NaN)
   all_models_mean_amp(n_models+0,i,2)=MEAN(SQRT(lim_rmm1(i,*)^2+lim_rmm2(i,*)^2),/NaN)
   all_models_mean_amp(n_models+1,i,0)=MEAN(ABS(scripps_rmm1(*,i)),/NaN)
   all_models_mean_amp(n_models+1,i,1)=MEAN(ABS(scripps_rmm2(*,i)),/NaN)
   all_models_mean_amp(n_models+1,i,2)=MEAN(SQRT(scripps_rmm1(*,i)^2+scripps_rmm2(*,i)^2),/NaN)
ENDFOR
all_descs(n_models)='LM'
all_descs(n_models+1)='LS'
all_colors(n_models)='olive'
all_colors(n_models+1)='slategrey'
all_models_max_lead_time(n_models:n_models+1)=20

psfile='/home/ss901165/idl/mjo_diabatic/rmm_indices/mjo_diabatic_rmm_mean_amplitude.rmm1_'+$
       STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'_tolead'+STRTRIM(STRING(MAX(our_lead_times)),1)+'.ps'
PSOPEN,file=psfile,TFONT=6,CHARSIZE=150,FONT=6,YOFFSET=1000,XOFFSET=600,MARGIN=2500
GSET,XMIN=0,XMAX=MAX(our_lead_times)+3.5,YMIN=0.5,YMAX=1.41
overlap_rows=fltarr(5,n_models+3)
overlap_rows(*,0)=0
row_distance=1
initial_distance=0.75
FOR i=0,n_models+1 DO BEGIN
   GPLOT,X=our_lead_times+0.5,Y=REFORM(all_models_mean_amp(i,*,0)),COL=FSC_COLOR(all_colors(i))
   IF all_models_max_lead_time(i) ne 20 THEN BEGIN
      GPLOT,X=all_models_max_lead_time(i)+initial_distance,Y=REFORM(all_models_mean_amp(i,all_models_max_lead_time(i),0)),$
            COL=FSC_COLOR(all_colors(i)),TEXT=all_descs(i),ALIGN=0
   ENDIF ELSE BEGIN
      FOR k=0,N_ELEMENTS(overlap_rows(*,0))-1 DO BEGIN
         j=0
         WHILE j lt n_models-1 DO BEGIN
            temp=all_models_mean_amp(i,all_models_max_lead_time(i)-1,0)-overlap_rows(k,j)
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
         overlap_rows(this_row,i)=all_models_mean_amp(i,all_models_max_lead_time(i)-1,0)
         distance=row_distance*this_row+initial_distance
         GPLOT,X=all_models_max_lead_time(i)-1+distance,Y=REFORM(all_models_mean_amp(i,all_models_max_lead_time(i)-1,0))-0.02,$
               COL=FSC_COLOR(all_colors(i)),TEXT=all_descs(i),ALIGN=0 
      ENDELSE
   ENDELSE
ENDFOR
GPLOT,X=our_lead_times+0.5,Y=obs_mean_amp(*,0),COL=FSC_COLOR('black'),THICK=150
GPLOT,X=20.5,Y=obs_mean_amp(19,0),COL=FSC_COLOR('black'),TEXT='PE',ALIGN=0
;GPLOT,X=our_lead_times+0.5,Y=REFORM(all_models_mean_amp(n_models+0,*,0)),COL=FSC_COLOR('olive'),THICK=150
;GPLOT,X=20.5,Y=all_models_mean_amp(n_models+0,19,0),COL=FSC_COLOR('olive'),TEXT='LM',ALIGN=0
;GPLOT,X=our_lead_times+0.5,Y=REFORM(all_models_mean_amp(n_models+1,*,0)),COL=FSC_COLOR('slategrey'),THICK=150
;GPLOT,X=20.5,Y=all_models_mean_amp(n_models+1,19,0),COL=FSC_COLOR('slategrey'),TEXT='LS',ALIGN=0
AXES,XVALS=our_lead_times+0.5,XLABELS=STRTRIM(STRING(our_lead_times+1),1),ORIENTATION=0,$
     YTITLE='Mean amplitude of RMM1',XTITLE='Lead time (days)',YSTEP=0.1,YMINOR=0.05,NDECS=1
PSCLOSE

psfile='/home/ss901165/idl/mjo_diabatic/rmm_indices/mjo_diabatic_rmm_mean_amplitude.rmm2_'+$
       STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'_tolead'+STRTRIM(STRING(MAX(our_lead_times)),1)+'.ps'
PSOPEN,file=psfile,TFONT=6,CHARSIZE=150,FONT=6,YOFFSET=1000,XOFFSET=600,MARGIN=2500
GSET,XMIN=0,XMAX=MAX(our_lead_times)+3.5,YMIN=0.5,YMAX=1.91
overlap_rows=fltarr(5,n_models+3)
overlap_rows(*,0)=0
row_distance=1
initial_distance=0.75
FOR i=0,n_models+1 DO BEGIN
   GPLOT,X=our_lead_times+0.5,Y=REFORM(all_models_mean_amp(i,*,1)),COL=FSC_COLOR(all_colors(i))
   IF all_models_max_lead_time(i) ne 20 THEN BEGIN
      GPLOT,X=all_models_max_lead_time(i)+initial_distance,Y=REFORM(all_models_mean_amp(i,all_models_max_lead_time(i),1)),$
            COL=FSC_COLOR(all_colors(i)),TEXT=all_descs(i),ALIGN=0
   ENDIF ELSE BEGIN
      FOR k=0,N_ELEMENTS(overlap_rows(*,0))-1 DO BEGIN
         j=0
         WHILE j lt n_models-1 DO BEGIN
            temp=all_models_mean_amp(i,all_models_max_lead_time(i)-1,1)-overlap_rows(k,j)
;         print,temp
            j=j+1
            IF ABS(temp) le 0.04 THEN BEGIN
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
         overlap_rows(this_row,i)=all_models_mean_amp(i,all_models_max_lead_time(i)-1,1)
         distance=row_distance*this_row+initial_distance
         GPLOT,X=all_models_max_lead_time(i)-1+distance,Y=REFORM(all_models_mean_amp(i,all_models_max_lead_time(i)-1,1))-0.02,$
               COL=FSC_COLOR(all_colors(i)),TEXT=all_descs(i),ALIGN=0 
      ENDELSE
   ENDELSE
ENDFOR
GPLOT,X=our_lead_times+0.5,Y=obs_mean_amp(*,1),COL=FSC_COLOR('black'),THICK=150
GPLOT,X=19.75,Y=obs_mean_amp(19,1),COL=FSC_COLOR('black'),TEXT='PE',ALIGN=0
AXES,XVALS=our_lead_times+0.5,XLABELS=STRTRIM(STRING(our_lead_times+1),1),ORIENTATION=0,$
     YTITLE='Mean amplitude of RMM2',XTITLE='Lead time (days)',YSTEP=0.1,YMINOR=0.05,NDECS=1
PSCLOSE

psfile='/home/ss901165/idl/mjo_diabatic/rmm_indices/mjo_diabatic_rmm_mean_amplitude.amp_'+$
       STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'_tolead'+STRTRIM(STRING(MAX(our_lead_times)),1)+'.ps'
PSOPEN,file=psfile,TFONT=6,CHARSIZE=150,FONT=6,YOFFSET=1000,XOFFSET=600,MARGIN=2500
GSET,XMIN=0,XMAX=MAX(our_lead_times)+3.5,YMIN=0.9,YMAX=2.31
overlap_rows=fltarr(5,n_models+3)
overlap_rows(*,0)=0
row_distance=1
initial_distance=0.75
FOR i=0,n_models+1 DO BEGIN
   GPLOT,X=our_lead_times+0.5,Y=REFORM(all_models_mean_amp(i,*,2)),COL=FSC_COLOR(all_colors(i))
   IF all_models_max_lead_time(i) ne 20 THEN BEGIN
      GPLOT,X=all_models_max_lead_time(i)+initial_distance,Y=REFORM(all_models_mean_amp(i,all_models_max_lead_time(i),2)),$
            COL=FSC_COLOR(all_colors(i)),TEXT=all_descs(i),ALIGN=0
   ENDIF ELSE BEGIN
      FOR k=0,N_ELEMENTS(overlap_rows(*,0))-1 DO BEGIN
         j=0
         WHILE j lt n_models-1 DO BEGIN
            temp=all_models_mean_amp(i,all_models_max_lead_time(i)-1,2)-overlap_rows(k,j)
;         print,temp
            j=j+1
            IF ABS(temp) le 0.04 THEN BEGIN
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
         overlap_rows(this_row,i)=all_models_mean_amp(i,all_models_max_lead_time(i)-1,2)
         distance=row_distance*this_row+initial_distance
         GPLOT,X=all_models_max_lead_time(i)-1+distance,Y=REFORM(all_models_mean_amp(i,all_models_max_lead_time(i)-1,2))-0.02,$
               COL=FSC_COLOR(all_colors(i)),TEXT=all_descs(i),ALIGN=0 
      ENDELSE
   ENDELSE
ENDFOR
GPLOT,X=our_lead_times+0.5,Y=obs_mean_amp(*,2),COL=FSC_COLOR('black'),THICK=150
GPLOT,X=19.75,Y=obs_mean_amp(19,2),COL=FSC_COLOR('black'),TEXT='PE',ALIGN=0
AXES,XVALS=our_lead_times+0.5,XLABELS=STRTRIM(STRING(our_lead_times+1),1),ORIENTATION=0,$
     YTITLE='Mean amplitude of (RMM1^2+RMM2^2)^(1/2)',XTITLE='Lead time (days)',YSTEP=0.1,YMINOR=0.05,NDECS=1
PSCLOSE


STOP
END

