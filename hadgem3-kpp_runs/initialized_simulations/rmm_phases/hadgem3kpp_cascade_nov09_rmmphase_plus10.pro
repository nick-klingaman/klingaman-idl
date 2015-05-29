PRO hadgem3kpp_cascade_nov09_rmmphase_plus10
 
; Plot RMM phases for Nov 2009 case

obs_rmm_infile='/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2009.index_values.nc'
year_offset=34
time_offset=308
model_offset=10

; Get RMM1 and RMM2 from observations

obs_ndays=41
obs_rmm1=REFORM(OPEN_AND_EXTRACT(obs_rmm_infile,'rmm1',$
                                 offset=[year_offset,time_offset],count=[1,obs_ndays]))
obs_rmm2=REFORM(OPEN_AND_EXTRACT(obs_rmm_infile,'rmm2',$
                                 offset=[year_offset,time_offset],count=[1,obs_ndays]))

; Calculate amplitude and phase angle for observations
obs_amplitude=fltarr(obs_ndays)
obs_phase_angle=fltarr(obs_ndays)
FOR i=0,obs_ndays-1 DO BEGIN
   obs_amplitude(i)=SQRT(obs_rmm1(i)^2+obs_rmm2(i)^2)
   IF (obs_rmm1(i) lt 0 and ABS(obs_rmm2(i)) lt ABS(obs_rmm1(i))) THEN $
      obs_phase_angle(i)=(-180)-SIN(obs_rmm2(i)/obs_amplitude(i))*(180./3.14159)
   IF (obs_rmm2(i) lt 0 and ABS(obs_rmm2(i)) gt ABS(obs_rmm1(i))) THEN $
      obs_phase_angle(i)=(-90)+SIN(obs_rmm1(i)/obs_amplitude(i))*(180./3.14159)
   IF (obs_rmm1(i) gt 0 and ABS(obs_rmm2(i)) lt ABS(obs_rmm1(i))) THEN $
      obs_phase_angle(i)=0+SIN(obs_rmm2(i)/obs_amplitude(i))*(180./3.14159)
   IF (obs_rmm2(i) gt 0 and ABS(obs_rmm2(i)) gt ABS(obs_rmm1(i))) THEN $
      obs_phase_angle(i)=90-SIN(obs_rmm1(i)/obs_amplitude(i))*(180./3.14159)
   IF obs_phase_angle(i) lt -180 THEN $
      obs_phase_angle(i)=360+obs_phase_angle(i)
ENDFOR

; Setup PostScript file

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/rmm_phases/hadgem3kpp_cascade_nov09_rmmphase_plus10.obs.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1500,SPACE3=1500,XOFFSET=1500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,XSIZE=15000,YSIZE=15000
xmin=-3.5
xmax=2.5
ymin=-2.5
ymax=2.5
GSET,XMIN=xmin,XMAX=xmax,YMIN=ymin,YMAX=ymax
GPLOT,X=REPLICATE(0,2),Y=[ymin,-1],STYLE=0,THICK=80
GPLOT,X=REPLICATE(0,2),Y=[1,ymax],STYLE=0,THICK=80
GPLOT,X=[1,xmax],Y=REPLICATE(0,2),STYLE=0,THICK=80
GPLOT,X=[-1,xmin],Y=REPLICATE(0,2),STYLE=0,THICK=80
GPLOT,X=[SQRT(2)/2.,1,MIN([xmax,ymax])],Y=[SQRT(2)/2.,1,MIN([xmax,ymax])],STYLE=0,THICK=80
GPLOT,X=[-SQRT(2)/2.,-1,MIN([ABS(xmin),ymax])*(-1)],Y=[SQRT(2)/2.,1,MIN([ABS(xmin),ymax])],STYLE=0,THICK=80
GPLOT,X=[-SQRT(2)/2.,-1,MAX([xmin,ymin])],Y=[-SQRT(2)/2.,-1,MAX([xmin,ymin])],STYLE=0,THICK=80
GPLOT,X=[SQRT(2)/2.,1,MIN([xmax,ABS(ymin)])],Y=[-SQRT(2)/2.,-1,MIN([xmax,ABS(ymin)])*(-1)],STYLE=0,THICK=80

GPLOT,X=0,Y=ymin-0.85,TEXT='Indian Ocean',ALIGN=0.5,CHARSIZE=100
GPLOT,X=xmax+0.35,Y=0,TEXT='Maritime Continent',ALIGN=0.5,CHARSIZE=100,ORIENTATION=90
GPLOT,X=0,Y=ymax+0.35,TEXT='Western Pacific',ALIGN=0.5,CHARSIZE=100
GPLOT,X=xmin-0.75,Y=0,TEXT='Western Hemisphere and Africa',ALIGN=0.5,CHARSIZE=100,ORIENTATION=90
GPLOT,X=0,Y=ymin-0.6,TEXT='RMM1'
GPLOT,X=xmin-0.5,Y=0,TEXT='RMM2',ORIENTATION=90
points=(2*!PI/99.0)*findgen(100)
x=COS(points)
y=SIN(points)
white=FSC_COLOR("white",28)
GPLOT,X=x,Y=y,FILLCOL=28

GPLOT,X=-1.25,Y=-2.25,TEXT='Phase 2'
GPLOT,X=1.25,Y=-2.25,TEXT='Phase 3'
GPLOT,X=2.0,Y=-1.00,TEXT='Phase 4'
GPLOT,X=2.0,Y=1.00,TEXT='Phase 5'
GPLOT,X=1.00,Y=2.00,TEXT='Phase 6'
GPLOT,X=-1.00,Y=2.00,TEXT='Phase 7'
GPLOT,X=-2.50,Y=0.75,TEXT='Phase 8'
GPLOT,X=-2.50,Y=-0.75,TEXT='Phase 1'

AXES,XSTEP=1.0,YSTEP=1.0,XMINOR=0.25,YMINOR=0.25,NDECS=2
GPLOT,X=0,Y=0,TEXT='Weak MJO',ALIGN=0.5

GPLOT,X=obs_rmm1,Y=obs_rmm2,STYLE=0,THICK=100
this_color=FSC_COLOR("black",30)
this_color=FSC_COLOR("green",31)
FOR i=0,obs_ndays-2 DO BEGIN
   IF i ne model_offset THEN $
      GPLOT,X=obs_rmm1(i),Y=obs_rmm2(i),/NOLINES,SYM=5,SIZE=80,COL=30
ENDFOR
GPLOT,X=obs_rmm1(0)+0.1,Y=obs_rmm2(0),TEXT='4 Nov',ALIGN=0.0
GPLOT,X=obs_rmm1(model_offset),Y=obs_rmm2(model_offset),SYM=4,SIZE=100,/NOLINES
GPLOT,X=obs_rmm1(obs_ndays-1)-0.05,Y=obs_rmm2(obs_ndays-1)+0.25,TEXT='14 Dec'
GPLOT,X=obs_rmm1(obs_ndays-1),Y=obs_rmm2(obs_ndays-1),SYM=1,SIZE=100,/NOLINES

analysis_rmm_file='/home/ss901165/datasets_mango/UM_ANALYSES/cascade_mjo_wh04/metum_analysis.jul-dec_dmeans.2009.rmm_indices.plus10.nc'
analysis_time_offset=136        ; Corresponding to start date of model integrations
analysis_ndays=30
analysis_obs_offset_time=10
analysis_rmm1=REFORM(OPEN_AND_EXTRACT(analysis_rmm_file,'rmm1',$
                                      offset=[0,analysis_time_offset],count=[1,analysis_ndays]))
analysis_rmm2=REFORM(OPEN_AND_EXTRACT(analysis_rmm_file,'rmm2',$
                                      offset=[0,analysis_time_offset],count=[1,analysis_ndays]))
analysis_amplitude=fltarr(analysis_ndays)
analysis_phase_angle=fltarr(analysis_ndays)
FOR i=0,analysis_ndays-1 DO BEGIN
   analysis_amplitude(i)=SQRT(analysis_rmm1(i)^2+analysis_rmm2(i)^2)
   IF (analysis_rmm1(i) lt 0 and ABS(analysis_rmm2(i)) lt ABS(analysis_rmm1(i))) THEN $
      analysis_phase_angle(i)=(-180)-SIN(analysis_rmm2(i)/analysis_amplitude(i))*(180./3.14159)
   IF (analysis_rmm2(i) lt 0 and ABS(analysis_rmm2(i)) gt ABS(analysis_rmm1(i))) THEN $
      analysis_phase_angle(i)=(-90)+SIN(analysis_rmm1(i)/analysis_amplitude(i))*(180./3.14159)
   IF (analysis_rmm1(i) gt 0 and ABS(analysis_rmm2(i)) lt ABS(analysis_rmm1(i))) THEN $
      analysis_phase_angle(i)=0+SIN(analysis_rmm2(i)/analysis_amplitude(i))*(180./3.14159)
   IF (analysis_rmm2(i) gt 0 and ABS(analysis_rmm2(i)) gt ABS(analysis_rmm1(i))) THEN $
      analysis_phase_angle(i)=90-SIN(analysis_rmm1(i)/analysis_amplitude(i))*(180./3.14159)
   IF analysis_phase_angle(i) lt -180 THEN $
      analysis_phase_angle(i)=360+analysis_phase_angle(i)
ENDFOR
analysis_rmm1=analysis_rmm1-0.7*(analysis_rmm1(0)-obs_rmm1(analysis_obs_offset_time))
analysis_rmm2=analysis_rmm2-0.7*(analysis_rmm2(0)-obs_rmm2(analysis_obs_offset_time))
GPLOT,X=analysis_rmm1,Y=analysis_rmm2,STYLE=1,THICK=100
FOR i=1,analysis_ndays-2 DO $
   GPLOT,X=analysis_rmm1(i),Y=analysis_rmm2(i),/NOLINES,SYM=5,SIZE=60,COL=31
GPLOT,X=analysis_rmm1(0),Y=analysis_rmm2(0),/NOLINES,SYM=4,SIZE=100,COL=31
GPLOT,X=analysis_rmm1(analysis_ndays-1),Y=analysis_rmm2(analysis_ndays-1),/NOLINES,SYM=1,SIZE=100,COL=31

n_models=4
model_max_ndays=30
all_runids=strarr(n_models)
all_descriptions=strarr(n_models)
all_symbols=intarr(n_models)
all_colors=strarr(n_models)
all_styles=strarr(n_models)
all_obs_offset_times=intarr(n_models)
all_model_ndays=intarr(n_models)
rmse_pc1=fltarr(n_models)
rmse_pc2=fltarr(n_models)
rmse_pc1_tendays=fltarr(n_models)
rmse_pc2_tendays=fltarr(n_models)
all_amplitudes=fltarr(n_models,model_max_ndays)
all_phase_angles=fltarr(n_models,model_max_ndays)
all_speeds=fltarr(n_models,model_max_ndays)
;all_speeds_tendays=fltarr(n_models)
model_basedir='/home/ss901165/um_output3'
FOR i=0,n_models-1 DO BEGIN
   CASE i OF 
      0 : BEGIN
         runid='xftfa'
         dirname='xftfa'
         description='Control simulation (xftfa)'
         obs_offset_time=10 ; Relative to start date of observations
         start_date='14nov09'
         model_ndays=30
         color='red'
         symbol=6
         style=0
      END
      1 : BEGIN
         runid='xftfb'
         dirname='xftfb'
         description='1.5*entrainment (xftfb)'
         obs_offset_time=10 ; Relative to start date of observations
         start_date='14nov09'
         model_ndays=30
         color='blue'
         symbol=6
         style=0
      END
      2 : BEGIN
         runid='xftfc'
         dirname='xftfc'
         description='No CMT (xftfc)'
         obs_offset_time=10
         start_date='14nov09'
         model_ndays=30
         color='brown'
         symbol=6
         style=0
      END
      3 : BEGIN
         runid='xftfd'
         dirname='xftfd'
         description='1.5*entrain and no CMT (xftfd)'
         obs_offset_time=10
         start_date='14nov09'
         model_ndays=30
         color='purple'
         symbol=6
         style=0
      END
   ENDCASE

   all_runids(i)=runid
   all_descriptions(i)=description
   all_symbols(i)=symbol
   all_colors(i)=color
   all_styles(i)=style
   all_obs_offset_times(i)=obs_offset_time
   all_model_ndays(i)=model_ndays   
   model_infile=model_basedir+'/'+dirname+'/'+runid+'.'+start_date+'.rmm_indices.nc'
   model_rmm1=REFORM(OPEN_AND_EXTRACT(model_infile,'rmm1',$
                                      offset=[0,0],count=[1,model_ndays]))
   model_rmm2=REFORM(OPEN_AND_EXTRACT(model_infile,'rmm2',$
                                      offset=[0,0],count=[1,model_ndays]))
   this_color=FSC_COLOR(color,31+i)
   model_rmm1=model_rmm1-0.7*(model_rmm1(0)-obs_rmm1(obs_offset_time))
   model_rmm2=model_rmm2-0.7*(model_rmm2(0)-obs_rmm2(obs_offset_time))
   GPLOT,X=model_rmm1,Y=model_rmm2,STYLE=style,COL=31+i,THICK=80
   FOR j=1,model_ndays-2 DO $
      GPLOT,X=model_rmm1(j),Y=model_rmm2(j),/NOLINES,SYM=symbol,SIZE=60,COL=31+i,STYLE=style
   GPLOT,X=model_rmm1(0),Y=model_rmm2(0),/NOLINES,SYM=4,SIZE=100,COL=31+i,STYLE=style
   GPLOT,X=model_rmm1(model_ndays-1),Y=model_rmm2(model_ndays-1),/NOLINES,SYM=1,SIZE=100,COL=31+i,STYLE=style
   rmse_pc1_tendays(i)=SQRT(MEAN((model_rmm1(0:9)-obs_rmm1(obs_offset_time:obs_offset_time+9))^2))
   rmse_pc2_tendays(i)=SQRT(MEAN((model_rmm2(0:9)-obs_rmm2(obs_offset_time:obs_offset_time+9))^2))
   rmse_pc1(i)=SQRT(MEAN((model_rmm1(0:model_ndays-1)-obs_rmm1(obs_offset_time:obs_offset_time+model_ndays-1))^2))
   rmse_pc2(i)=SQRT(MEAN((model_rmm2(0:model_ndays-1)-obs_rmm2(obs_offset_time:obs_offset_time+model_ndays-1))^2))
   
   FOR j=0,model_ndays-1 DO BEGIN
      all_amplitudes(i,j)=SQRT(model_rmm1(j)^2+model_rmm2(j)^2)
      IF (model_rmm1(j) lt 0 and ABS(model_rmm2(j)) lt ABS(model_rmm1(j))) THEN $
         all_phase_angles(i,j)=(-180)-SIN(model_rmm2(j)/all_amplitudes(i,j))*(180./3.14159)
      IF (model_rmm2(j) lt 0 and ABS(model_rmm2(j)) gt ABS(model_rmm1(j))) THEN $
         all_phase_angles(i,j)=(-90)+SIN(model_rmm1(j)/all_amplitudes(i,j))*(180./3.14159)
      IF (model_rmm1(j) gt 0 and ABS(model_rmm2(j)) lt ABS(model_rmm1(j))) THEN $
         all_phase_angles(i,j)=0+SIN(model_rmm2(j)/all_amplitudes(i,j))*(180./3.14159)
      IF (model_rmm2(j) gt 0 and ABS(model_rmm2(j)) gt ABS(model_rmm1(j))) THEN $
         all_phase_angles(i,j)=90-SIN(model_rmm1(j)/all_amplitudes(i,j))*(180./3.14159)
      IF all_phase_angles(i,j) lt -180 THEN $
         all_phase_angles(i,j)=360+all_phase_angles(i,j)
      IF j gt 0 THEN BEGIN
         IF ABS(all_phase_angles(i,j)-all_phase_angles(i,j-1)) gt 300 THEN BEGIN
            IF all_phase_angles(i,j-1) lt 0 THEN $
               all_phase_angles(i,j)=-all_phase_angles(i,j)
            IF all_phase_angles(i,j-1) gt 0 THEN $
               all_phase_angles(i,j)=360+all_phase_angles(i,j)
            print,all_phase_angles(i,j-1),all_phase_angles(i,j)
         ENDIF
      ENDIF
      IF j gt 0 THEN $
                                ;all_speeds(i,j-1)=SQRT((model_rmm1(j)-model_rmm1(j-1))^2+(model_rmm2(j)-model_rmm2(j-1))^2)
         all_speeds(i,j-1)=ABS(ABS(all_phase_angles(i,j))-ABS(all_phase_angles(i,j-1)))
   ENDFOR
;   all_speeds(i)=MEAN(temp_speed)
;   all_speeds_tendays(i)=MEAN(temp_speed(0:9))

   IF i eq 0 THEN BEGIN
      GPLOT,X=3.25,Y=-0.75,TEXT='Model',ALIGN=0.0
      GPLOT,X=4.00,Y=-0.75,TEXT='RMSE',ALIGN=0.0
      GPLOT,X=4.00,Y=-0.90,TEXT='(forecast)',ALIGN=0.0
      GPLOT,X=4.75,Y=-0.75,TEXT='RMSE',ALIGN=0.0
      GPLOT,X=4.75,Y=-0.90,TEXT='(days 1-10)',ALIGN=0.0
      GPLOT,X=5.60,Y=-0.75,TEXT='Speed',ALIGN=0.0
      GPLOT,X=5.60,Y=-0.90,TEXT='(forecast)',ALIGN=0.0
      GPLOT,X=6.35,Y=-0.75,TEXT='Speed',ALIGN=0.0
      GPLOT,X=6.35,Y=-0.90,TEXT='(days 1-10)',ALIGN=0.0
      obs_speed=fltarr(obs_ndays-obs_offset_time-1)
      FOR j=obs_offset_time+1,obs_ndays-1 DO $
         ;obs_speed(j-(obs_offset_time+1))=SQRT((obs_rmm1(j)-obs_rmm1(j-1))^2+(obs_rmm2(j)-obs_rmm2(j-1))^2)
         obs_speed(j-(obs_offset_time+1))=ABS(ABS(obs_phase_angle(j))-ABS(obs_phase_angle(j-1)))
      GPLOT,X=3.25,Y=-1.35,TEXT='Observations',ALIGN=0.0,COL=30
      GPLOT,X=5.60,Y=-1.35,TEXT=STRMID(STRTRIM(STRING(MEAN(obs_speed)),1),0,5),ALIGN=0.0,COL=30
      GPLOT,X=6.35,Y=-1.35,TEXT=STRMID(STRTRIM(STRING(MEAN(obs_speed(0:9))),1),0,5),ALIGN=0.0,COL=30
   ENDIF
   GPLOT,X=3.25,Y=-1.75-0.25*(i),TEXT=runid,ALIGN=0.0,COL=31+i
   GPLOT,X=4.00,Y=-1.75-0.25*(i),TEXT=STRMID(STRTRIM(STRING(rmse_pc1(i)+rmse_pc2(i)),1),0,5),ALIGN=0.0,COL=31+i
   GPLOT,X=4.75,Y=-1.75-0.25*(i),TEXT=STRMID(STRTRIM(STRING(rmse_pc1_tendays(i)+rmse_pc2_tendays(i)),1),0,5),ALIGN=0.0,COL=31+i
   GPLOT,X=5.60,Y=-1.75-0.25*(i),TEXT=STRMID(STRTRIM(STRING(MEAN(all_speeds(i,*))),1),0,5),ALIGN=0.0,COL=31+i
   GPLOT,X=6.35,Y=-1.75-0.25*(i),TEXT=STRMID(STRTRIM(STRING(MEAN(all_speeds(i,0:9))),1),0,5),ALIGN=0.0,COL=31+i
ENDFOR

GLEGEND,COL=REVERSE(indgen(n_models)+31),SYM=REVERSE(all_symbols),LABELS=REVERSE(all_descriptions),LEGXOFFSET=12000,LEGYOFFSET=18000,STYLE=REVERSE(all_styles)

PSCLOSE

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/rmm_phases/hadgem3kpp_cascade_nov09_rmmphase_plus10.phase_angle_amplitude.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2500,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100
GSET,XMIN=0,XMAX=model_max_ndays,YMIN=0,YMAX=3
black=FSC_COLOR("black",3)
orange=FSC_COLOR("orange",4)
GPLOT,X=indgen(analysis_ndays)+0.5,Y=obs_amplitude(analysis_obs_offset_time:obs_ndays-1),SYM=5,COL=3,STYLE=0,THICK=100,SIZE=60
GPLOT,X=indgen(analysis_ndays)+0.5,Y=analysis_amplitude,SYM=5,COL=4,STYLE=0,THICK=50,SIZE=60
FOR i=0,n_models-1 DO BEGIN
   this_color=FSC_COLOR(all_colors(i),31+i)
   GPLOT,X=indgen(all_model_ndays(i))+0.5,Y=REFORM(all_amplitudes(i,*)),SYM=all_symbols(i),COL=31+i,STYLE=0,THICK=50,SIZE=60
ENDFOR
AXES,XSTEP=4,XMINOR=1,XTITLE='Days since 11 November 2009',/NOLEFT,/NORIGHT
AXES,YSTEP=0.25,YMINOR=0.125,/ONLYLEFT,NDECS=2,YTITLE='Amplitude [(RMM1^2+RMM2^2)^(1/2)]'
GSET,XMIN=0,XMAX=model_max_ndays,YMIN=-210,YMAX=210
GPLOT,X=indgen(analysis_ndays)+0.5,Y=obs_phase_angle(analysis_obs_offset_time:obs_ndays-1),SYM=5,COL=3,STYLE=2,THICK=100,SIZE=60
GPLOT,X=indgen(analysis_ndays)+0.5,Y=analysis_phase_angle,SYM=5,COL=4,STYLE=2,THICK=50,SIZE=60
FOR i=0,n_models-1 DO BEGIN
   this_color=FSC_COLOR(all_colors(i),31+i)
   GPLOT,X=indgen(all_model_ndays(i))+0.5,Y=REFORM(all_phase_angles(i,*)),SYM=all_symbols(i),COL=31+i,STYLE=2,THICK=50,SIZE=60
ENDFOR
AXES,YSTEP=30,YMINOR=10,/ONLYRIGHT,NDECS=2,YTITLE='Phase angle ( -360 < phases 5-8 < -180; -180 < phases 1-4 < 0 ; 0 < phases 5-8 < 180 ; 180 < phases 1-4 < 360)'
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/rmm_phases/hadgem3kpp_cascade_nov09_rmmphase_plus10.phase_angle_amplitude.16days.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=2200,XOFFSET=500,YOFFSET=5500,TFONT=2,TCHARSIZE=100
GSET,XMIN=0,XMAX=16,YMIN=0,YMAX=4
black=FSC_COLOR("black",3)
orange=FSC_COLOR("orange",4)
GPLOT,X=indgen(16)+0.5,Y=obs_amplitude(analysis_obs_offset_time:analysis_obs_offset_time+15),SYM=5,COL=3,STYLE=0,THICK=100,SIZE=60
GPLOT,X=indgen(16)+0.5,Y=analysis_amplitude(0:15),SYM=5,COL=4,STYLE=0,THICK=50,SIZE=60
FOR i=0,n_models-1 DO BEGIN
   this_color=FSC_COLOR(all_colors(i),31+i)
   GPLOT,X=indgen(16)+0.5,Y=REFORM(all_amplitudes(i,0:15)),SYM=all_symbols(i),COL=31+i,STYLE=0,THICK=50,SIZE=60
ENDFOR
AXES,XSTEP=4,XMINOR=1,XTITLE='Days since 11 November 2009',/NOLEFT,/NORIGHT
AXES,YSTEP=0.25,YMINOR=0.125,/ONLYLEFT,NDECS=2,YTITLE='Amplitude [(RMM1^2+RMM2^2)^(1/2)]'
GSET,XMIN=0,XMAX=16,YMIN=210,YMAX=-210
GPLOT,X=indgen(16)+0.5,Y=obs_phase_angle(analysis_obs_offset_time:analysis_obs_offset_time+15),SYM=5,COL=3,STYLE=2,THICK=100,SIZE=60
GPLOT,X=indgen(16)+0.5,Y=analysis_phase_angle(0:15),SYM=5,COL=4,STYLE=2,THICK=50,SIZE=60
FOR i=0,n_models-1 DO BEGIN
   this_color=FSC_COLOR(all_colors(i),31+i)
   GPLOT,X=indgen(16)+0.5,Y=REFORM(all_phase_angles(i,0:15)),SYM=all_symbols(i),COL=31+i,STYLE=2,THICK=50,SIZE=60
ENDFOR
AXES,YSTEP=-30,YMINOR=-10,/ONLYRIGHT,NDECS=2,YTITLE='Phase angle ( -360 < ph 5-8 < -180; -180 < ph 1-4 < 0 ; 0 < ph 5-8 < 180 ; 180 < ph 1-4 < 360)'
GLEGEND,COL=REVERSE([3,4,indgen(n_models)+31]),SYM=REVERSE([5,5,all_symbols]),LABELS=REVERSE(['Observations','UKMO Analysis',all_descriptions]),LEGXOFFSET=2000,LEGYOFFSET=0
PSCLOSE

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/rmm_phases/hadgem3kpp_cascade_nov09_rmmphase_plus10.phase_angle_amplitude_error.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2500,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100
GSET,XMIN=0,XMAX=model_max_ndays,YMIN=-2.5,YMAX=2
black=FSC_COLOR("black",3)
orange=FSC_COLOR("orange",4)
GPLOT,X=indgen(analysis_ndays)+0.5,Y=analysis_amplitude-obs_amplitude(analysis_obs_offset_time:obs_ndays-1),SYM=5,COL=4,STYLE=0,THICK=50,SIZE=60
FOR i=0,n_models-1 DO BEGIN
   this_color=FSC_COLOR(all_colors(i),31+i)
   GPLOT,X=indgen(all_model_ndays(i))+0.5,Y=REFORM(all_amplitudes(i,*))-obs_amplitude(all_obs_offset_times(i):obs_ndays-1),SYM=all_symbols(i),$
         COL=31+i,STYLE=0,THICK=50,SIZE=60
ENDFOR
GPLOT,X=[0,model_max_ndays],Y=[0,0],STYLE=1,THICK=100
AXES,XSTEP=4,XMINOR=1,XTITLE='Days since 11 November 2009',/NOLEFT,/NORIGHT
AXES,YSTEP=0.25,YMINOR=0.125,/ONLYLEFT,NDECS=2,YTITLE='Error in amplitude [(RMM1^2+RMM2^2)^(1/2)] from observations'
GSET,XMIN=0,XMAX=model_max_ndays,YMIN=0,YMAX=180
FOR i=0,n_models-1 DO BEGIN
   this_color=FSC_COLOR(all_colors(i),31+i)
   diff_phase_angle=ABS(REFORM(all_phase_angles(i,*))-obs_phase_angle(all_obs_offset_times(i):all_obs_offset_times(i)+all_model_ndays(i)-1))
   IF TOTAL(where(diff_phase_angle gt 180)) gt 0 THEN $
      diff_phase_angle[where(diff_phase_angle gt 180)] = 180-(diff_phase_angle[where(diff_phase_angle gt 180)]-180)
   GPLOT,X=indgen(all_model_ndays(i))+0.5,Y=diff_phase_angle,SYM=all_symbols(i),COL=31+i,STYLE=2,THICK=50,SIZE=60
ENDFOR
AXES,YSTEP=20,YMINOR=10,/ONLYRIGHT,NDECS=2,YTITLE='Absolute value of error in phase angle from observations'
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/rmm_phases/hadgem3kpp_cascade_nov09_rmmphase_plus10.phase_angle_amplitude_error.16days.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=2500,XOFFSET=500,YOFFSET=5500,TFONT=2,TCHARSIZE=100
GSET,XMIN=0,XMAX=16,YMIN=-2.5,YMAX=2.5
black=FSC_COLOR("black",3)
orange=FSC_COLOR("orange",4)
GPLOT,X=indgen(16)+0.5,Y=analysis_amplitude-obs_amplitude(analysis_obs_offset_time:analysis_obs_offset_time+15),SYM=5,COL=4,STYLE=0,THICK=50,SIZE=60
FOR i=0,n_models-1 DO BEGIN
   this_color=FSC_COLOR(all_colors(i),31+i)
   GPLOT,X=indgen(all_model_ndays(i))+0.5,Y=REFORM(all_amplitudes(i,0:15))-obs_amplitude(all_obs_offset_times(i):all_obs_offset_times(i)+15),SYM=all_symbols(i),$
         COL=31+i,STYLE=0,THICK=50,SIZE=60
ENDFOR
GPLOT,X=[0,16],Y=[0,0],STYLE=1,THICK=100
AXES,XSTEP=4,XMINOR=1,XTITLE='Days since 11 November 2009',/NOLEFT,/NORIGHT
AXES,YSTEP=0.5,YMINOR=0.25,/ONLYLEFT,NDECS=2,YTITLE='Error in amplitude [(RMM1^2+RMM2^2)^(1/2)] from observations'
GSET,XMIN=0,XMAX=16,YMIN=0,YMAX=180
FOR i=0,n_models-1 DO BEGIN
   this_color=FSC_COLOR(all_colors(i),31+i)
   diff_phase_angle=ABS(REFORM(all_phase_angles(i,0:15))-obs_phase_angle(all_obs_offset_times(i):all_obs_offset_times(i)+15))
   IF TOTAL(where(diff_phase_angle gt 180)) gt 0 THEN $
      diff_phase_angle[where(diff_phase_angle gt 180)] = 180-(diff_phase_angle[where(diff_phase_angle gt 180)]-180)
   GPLOT,X=indgen(16)+0.5,Y=diff_phase_angle,SYM=all_symbols(i),COL=31+i,STYLE=2,THICK=50,SIZE=60
ENDFOR
AXES,YSTEP=20,YMINOR=10,/ONLYRIGHT,NDECS=2,YTITLE='Absolute value of error in phase angle from observations'
GLEGEND,COL=REVERSE([4,indgen(n_models)+31]),SYM=REVERSE([5,all_symbols]),LABELS=REVERSE(['UKMO Analysis',all_descriptions]),LEGXOFFSET=2000,LEGYOFFSET=0
PSCLOSE

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/rmm_phases/hadgem3kpp_cascade_nov09_rmmphase_plus10.phase_speed.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2000,XOFFSET=1300,YOFFSET=700,TFONT=2,TCHARSIZE=100
;GSET,XMIN=0,XMAX=model_max_ndays,YMIN=0,YMAX=1
GSET,XMIN=0,XMAX=model_max_ndays,YMIN=3,YMAX=30,/YLOG,TITLE='Phase speed for November 2009 case study (angle travelled around RMM1/RMM2 space each day)'
black=FSC_COLOR("black",3)
obs_speed=SMOOTH(obs_speed,5)
IF TOTAL(where(obs_speed lt 3)) gt 0 THEN $
   obs_speed[where(obs_speed lt 3)]=3
IF TOTAL(where(obs_speed gt 30)) gt 0 THEN $
   obs_speed[where(obs_speed gt 30)]=30
GPLOT,X=indgen(analysis_ndays)+1.5,Y=obs_speed(0:N_ELEMENTS(obs_speed)-2),SYM=5,COL=3,STYLE=0,THICK=100,SIZE=60
FOR i=0,n_models-1 DO BEGIN
   this_color=FSC_COLOR(all_colors(i),31+i)
   this_speed=REFORM(all_speeds(i,0:all_model_ndays(i)-2))
   this_speed=SMOOTH(this_speed,5)
   IF TOTAL(where(this_speed lt 3)) ge 0 THEN $
      this_speed[where(this_speed lt 3)]=3
   IF TOTAL(where(this_speed gt 30)) ge 0 THEN $
      this_speed[where(this_speed gt 30)]=30
   print,this_speed
   GPLOT,X=indgen(all_model_ndays(i))+1.5,Y=this_speed,SYM=all_symbols(i),COL=31+i,STYLE=0,THICK=50,SIZE=60
ENDFOR
AXES,YTITLE='Phase speed (lag-1 difference in phase angle; degrees)',XSTEP=4,XMINOR=1,XTITLE='Days since 11 November 2009',NDECS=2,$
     YVALS=[3,4,5,6,8,10,12,15,17,20,22,25,30],YLABELS=['Stationary','4','5','6','8','10','12','15','17','20','22','25','Random']
GPLOT,X=0,Y=2.5,TEXT='Stationary is <= 3 degrees; Random is >= 30 degrees',ALIGN=0.0
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/rmm_phases/hadgem3kpp_cascade_nov09_rmmphase_plus10.phase_speed.16days.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=2000,XOFFSET=1300,YOFFSET=5500,TFONT=2,TCHARSIZE=80
GSET,XMIN=0,XMAX=16,YMIN=3,YMAX=30,/YLOG,TITLE='Phase speed for November 2009 case study (angle travelled around RMM1/RMM2 space each day)'
black=FSC_COLOR("black",3)
obs_speed=SMOOTH(obs_speed,5)
IF TOTAL(where(obs_speed lt 3)) gt 0 THEN $
   obs_speed[where(obs_speed lt 3)]=3
IF TOTAL(where(obs_speed gt 30)) gt 0 THEN $
   obs_speed[where(obs_speed gt 30)]=30
GPLOT,X=indgen(16)+1.5,Y=obs_speed(0:14),SYM=5,COL=3,STYLE=0,THICK=100,SIZE=60
FOR i=0,n_models-1 DO BEGIN
   this_color=FSC_COLOR(all_colors(i),31+i)
   this_speed=REFORM(all_speeds(i,0:all_model_ndays(i)-2))
   this_speed=SMOOTH(this_speed,5)
   IF TOTAL(where(this_speed lt 3)) ge 0 THEN $
      this_speed[where(this_speed lt 3)]=3
   IF TOTAL(where(this_speed gt 30)) ge 0 THEN $
      this_speed[where(this_speed gt 30)]=30
   print,this_speed
   GPLOT,X=indgen(16)+1.5,Y=this_speed(0:14),SYM=all_symbols(i),COL=31+i,STYLE=0,THICK=50,SIZE=60
ENDFOR
AXES,YTITLE='Phase speed (lag-1 difference in phase angle; degrees)',XSTEP=4,XMINOR=1,XTITLE='Days since 11 November 2009',NDECS=2,$
     YVALS=[3,4,5,6,8,10,12,15,17,20,22,25,30],YLABELS=['Stationary','4','5','6','8','10','12','15','17','20','22','25','Random']
GPLOT,X=0,Y=2,TEXT='Stationary is <= 3 degrees; Random is >= 30 degrees',ALIGN=0.0
GLEGEND,COL=REVERSE([3,4,indgen(n_models)+31]),SYM=REVERSE([5,5,all_symbols]),LABELS=REVERSE(['Observations','UKMO Analysis',all_descriptions]),LEGXOFFSET=2000,LEGYOFFSET=0
PSCLOSE

STOP

END
