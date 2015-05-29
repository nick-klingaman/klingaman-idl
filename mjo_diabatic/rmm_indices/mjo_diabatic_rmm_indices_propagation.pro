PRO mjo_diabatic_rmm_indices_propagation,model_names=model_names,lead_times=lead_times,start_date,stop_date

lags=indgen(41)-21

; Read observed RMM indices
obs_rmm_file='/home/ss901165/datasets/ECMWF_YOTC/ECMWF_YOTC.oct2009-feb2010_dmeans.rmm_indices.nc'
obs_rmm1=OPEN_AND_EXTRACT(obs_rmm_file,'rmm1')
obs_rmm2=OPEN_AND_EXTRACT(obs_rmm_file,'rmm2')

obs_ccorr=C_CORRELATE(obs_rmm1,obs_rmm2,lags)
print,MAX(ABS(obs_ccorr)),lags[where(ABS(obs_ccorr) eq MAX(ABS(obs_ccorr)))]

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

obs_start_date=274
IF start_julian lt obs_start_date THEN $
   start_julian=start_julian+365
IF stop_julian lt start_julian THEN $
   stop_julian=stop_julian+365
n_days=FLOOR(stop_julian-start_julian+1)
print,start_julian,stop_julian,n_days

n_times_per_day=8
standard_valid_dates=STRTRIM(STRING([indgen(22)+20091010,indgen(25)+20091101,indgen(22)+20091210,indgen(25)+20100101]),1)
mogreps_valid_dates=STRTRIM(STRING([indgen(17)+20091015,indgen(20)+20091101,indgen(11)+20091215,indgen(4)+20091227,indgen(16)+20100101]),1)
dates=STRTRIM(STRING([indgen(30)+20090901,indgen(31)+20091001,indgen(30)+20091101,indgen(31)+20091201,indgen(31)+20100101,indgen(28)+20100201]),1)

all_max_ccorr=fltarr(n_models,n_lead_times/5)

all_colors=strarr(n_models)
all_descs=strarr(n_models)
all_max_lead_times=intarr(n_models)

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

   FOR k=0,n_lead_times-1,5 DO BEGIN
      temp_rmm1=REFORM(this_model_rmm1(*,k:k+4))
      temp_rmm2=REFORM(this_model_rmm2(*,k:k+4))
      valid_rmm1=temp_rmm1[where(FINITE(temp_rmm1) eq 1)]
      valid_rmm2=temp_rmm2[where(FINITE(temp_rmm2) eq 1)]
      model_ccorr=C_CORRELATE(REFORM(valid_rmm1),REFORM(valid_rmm2),lags)
      all_max_ccorr(i,k/5)=ABS(lags[where(ABS(model_ccorr) eq MAX(ABS(model_ccorr)))])
   ENDFOR
   print,our_model_names(i),MAX(all_max_ccorr(i,*))
ENDFOR

psfile='/home/ss901165/idl/mjo_diabatic/rmm_indices/mjo_diabatic_rmm_indices_propagation.rmm_lag_corr.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=160,MARGIN=2000,SPACE3=50,XOFFSET=1500,YOFFSET=500,TFONT=6,TCHARSIZE=120,CB_WIDTH=110
GSET,XMIN=-1,XMAX=MAX(our_lead_times)+3.5,YMIN=2,YMAX=18
overlap=[0]
FOR i=0,n_models-1 DO BEGIN
   GPLOT,X=[2.5,7.5,12.5,17.5],Y=REFORM(all_max_ccorr(i,*)),COL=FSC_COLOR(all_colors(i)),STYLE=2,SYM=3
   distance=0.6
   FOR j=0,N_ELEMENTS(overlap)-1 DO BEGIN
      temp=all_max_ccorr(i,3)-overlap(j)
      IF ABS(temp) lt 1 THEN BEGIN
         print,all_descs(i),all_descs(j-1),all_max_ccorr(i,3),$
               all_max_ccorr(j-1,3),overlap(j)
         distance=distance+1.1
      ENDIF
   ENDFOR
   overlap=[overlap,all_max_ccorr(i,3)]
   GPLOT,X=17.5+distance,Y=REFORM(all_max_ccorr(i,3))-0.01,$
         COL=FSC_COLOR(all_colors(i)),TEXT=all_descs(i),ALIGN=0
ENDFOR
GPLOT,X=[2.5,22.5],Y=[7,7],STYLE=2
GPLOT,X=3.5,Y=7.3,TEXT='OBS'
;GPLOT,X=our_lead_times+0.5,Y=persistence_bivar_corr[where(persistence_bivar_corr ge 0.3)],COL=FSC_COLOR('black'),STYLE=0
;GPLOT,X=MIN(where(persistence_bivar_corr le 0.3)),Y=0.31,COL=FSC_COLOR('black'),TEXT='PE'
;GPLOT,X=our_lead_times+0.5,Y=lim_bivar_corr[where(lim_bivar_corr ge 0)],COL=FSC_COLOR('olive'),STYLE=0
;GPLOT,X=MAX(our_lead_times)+1,Y=REFORM(lim_bivar_corr(n_lead_times-1)),COL=FSC_COLOR('olive'),TEXT='LM'
;GPLOT,X=our_lead_times+0.5,Y=scripps_bivar_corr[where(scripps_bivar_corr ge 0)],COL=FSC_COLOR('darkgrey'),STYLE=0
;GPLOT,X=MAX(our_lead_times)+1,Y=REFORM(scripps_bivar_corr(n_lead_times-1)),COL=FSC_COLOR('darkgrey'),TEXT='LS'
;GPLOT,X=[MIN(our_lead_times),MAX(our_lead_times)+1],Y=[our_bivar_corr_threshold,our_bivar_corr_threshold],STYLE=1
;GPLOT,X=[MIN(our_lead_times),MAX(our_lead_times)+1],Y=[0,0],STYLE=1
GLEGEND,labels=REVERSE(all_descs),COL=REVERSE(FSC_COLOR(all_colors)),LEGPOS=3,LENGTH=0,SYM=REPLICATE(3,n_models)
AXES,XTITLE='Lead time (days)',XVALS=[2.5,7.5,12.5,17.5],XLABELS=['Days 1-5','Days 6-10','Days 11-15','Days 16-20'],YSTEP=2,YMINOR=1,YTITLE='Lag of maximum correlation of RMM1 and RMM2 (days)',NDECS=2
PSCLOSE

STOP
END

