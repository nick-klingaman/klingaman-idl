PRO mjo_diabatic_rmm_wheel_constvalid,model_names,start_date,stop_date,lead_times=lead_times,lead_time_nth=lead_time_nth,$
                                      dots_nth=dots_nth
  
; Plot RMM1/RMM2 phase space for given forecast model(s) between starting and stopping dates,
; along with observations.  If lead_times is specified, those forecast lead times will be used;
; otherwise use all forecast lead times available.
  
; Designed to be used to compare model(s) with observations at the same validity time.
; Use mjo_diabatic_rmm_wheel_constinit to see evolution of a given forecast(s) with time.

n_models=N_ELEMENTS(model_names)

IF KEYWORD_SET(lead_times) THEN BEGIN
   our_lead_times=lead_times
ENDIF ELSE $
   our_lead_times=indgen(19)+1
n_lead_times=N_ELEMENTS(our_lead_times)

IF KEYWORD_SET(lead_time_nth) THEN BEGIN
   our_lead_time_nth=lead_time_nth
ENDIF ELSE $
   our_lead_time_nth=1

IF KEYWORD_SET(dots_nth) THEN BEGIN
   our_dots_nth=dots_nth
ENDIF ELSE $
   our_dots_nth=2

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
;obs_infile='/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2011.index_values.nc'
;obs_start_year=1975
obs_infile='/home/ss901165/datasets/ECMWF_YOTC/ECMWF_YOTC.oct2009-feb2010_dmeans.rmm_indices.nc'
obs_start_date=274
IF start_julian lt obs_start_date THEN $
   start_julian=start_julian+365
IF stop_julian lt obs_start_date THEN $
   stop_julian=stop_julian+365
n_days=FLOOR(stop_julian-start_julian+1)
obs_rmm1=fltarr(n_days)
obs_rmm2=fltarr(n_days)
FOR j=0,n_days-1 DO BEGIN
;   obs_rmm1(j*n_times_per_day:(j+1)*n_times_per_day-1)=$
;      REFORM(OPEN_AND_EXTRACT(obs_infile,'rmm1',$
;                              offset=[start_year-obs_start_year,FLOOR(start_julian)+j],count=[1,1]))
;   obs_rmm2(j*n_times_per_day:(j+1)*n_times_per_day-1)=$
;      REFORM(OPEN_AND_EXTRACT(obs_infile,'rmm2',$
;                              offset=[start_year-obs_start_year,FLOOR(start_julian)+j],count=[1,1]))
   
   obs_rmm1(j)=$
      REFORM(OPEN_AND_EXTRACT(obs_infile,'rmm1',$
                              offset=[FLOOR(start_julian)+j-obs_start_date],count=[1]))
   obs_rmm2(j)=$
      REFORM(OPEN_AND_EXTRACT(obs_infile,'rmm2',$
                              offset=[FLOOR(start_julian)+j-obs_start_date],count=[1]))
ENDFOR

; Handle models
all_models_rmm1=fltarr(n_models,n_days,n_lead_times)
all_models_rmm2=fltarr(n_models,n_days,n_lead_times)
FOR i=0,n_models-1 DO BEGIN
   CASE model_names(i) OF 
      'ecmwf' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/ecmwf'
         desc='ECMWF_IFS'
         color_scale=26
         valid_dates=standard_valid_dates
      END
      'miroc' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/miroc'
         desc='miroc5'
         color_scale=26
         valid_dates=standard_valid_dates
      END
      'mri' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/mri'
         desc='MRI-AGCM'
         color_scale=26
         valid_dates=standard_valid_dates
      END
      'nasa' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/nasa'
         desc='GEOS5_AGCM'
         color_scale=26
         valid_dates=standard_valid_dates
      END      
      'nrl'  : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/nrl'
         desc='NGEM01'
         color_scale=26
         valid_dates=standard_valid_dates
      END
      'iis'  : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/iis'
         desc='gfs2_iis'
         color_scale=26
         valid_dates=standard_valid_dates
      END
      'metum' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/metum'
         desc='MetUM'
         color_scale=26
         valid_dates=standard_valid_dates
      END
      'metum_annatmos' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/metum_coupled_ann/Atmos_control'
         desc='metum_mogreps_atmos'
         color_scale=26
         valid_dates=mogreps_valid_dates
      END
      'metum_anncouple' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/metum_coupled_ann/Coupled_model'
         desc='metum_mogreps_couple'
         color_scale=26
         valid_dates=mogreps_valid_dates
      END
      'cancm4' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cancm4'
         desc='CanCM4'
         color_scale=26
         valid_dates=standard_valid_dates
      END
      'spcam' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/spcam'
         desc='SPCAM3.0'
         color_scale=26
         valid_dates=standard_valid_dates
      END
      'giss' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/giss'
         desc='ModelE'
         color_scale=26
         valid_dates=standard_valid_dates
      END
      'cam5zm' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cam5zm'
         desc='CAM5_ZM'
         color_scale=26
         valid_dates=standard_valid_dates
      END
      'cam5' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cam5'
         desc='CAM5'
         color_scale=26
         valid_dates=standard_valid_dates
      END
      'ecearth' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/ecearth'
         desc='ECEARTH'
         color_scale=26
         valid_dates=standard_valid_dates
      END
      'cnrm_atmos' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cnrm_atmos'
         desc='CNRM_ATM'
         color_scale=26
         valid_dates=standard_valid_dates
      END
   ENDCASE

;   start_position=where(valid_dates eq start_date)
   this_model_rmm1=fltarr(n_days,n_lead_times)
   this_model_rmm2=fltarr(n_days,n_lead_times)
   this_model_rmm1_dayone=fltarr(n_days)
   this_model_rmm2_dayone=fltarr(n_days)

   model_offset=where(valid_dates eq start_date)
   date_offset=where(dates eq start_date)

   FOR j=0,n_days-1 DO BEGIN
      today_year=STRMID(dates(j+date_offset),0,4)
      today_month=STRMID(dates(j+date_offset),4,2)
      today_date=STRMID(dates(j+date_offset),6,2)
      
      FOR k=0,n_lead_times-1 DO BEGIN
         
         init_year=STRMID(dates(j+date_offset-our_lead_times(k)),0,4)
         init_month=STRMID(dates(j+date_offset-our_lead_times(k)),4,2)
         init_date=STRMID(dates(j+date_offset-our_lead_times(k)),6,2)
         
                                ;julian_distance=GREGORIAN_TO_JULIAN(FLOAT(today_date(0)),FLOAT(today_month(0)),FLOAT(today_year(0)))-$
                                ;GREGORIAN_TO_JULIAN(FLOAT(init_date(0)),FLOAT(init_month(0)),FLOAT(init_year(0)))         
         offset=REFORM(j+date_offset-our_lead_times(k))      
         IF where(valid_dates eq dates(offset(0))) eq -1 THEN BEGIN
            print,'No hindcast for date '+dates(j+date_offset)+' at lead time '+STRTRIM(STRING(our_lead_times(k)),1)
            this_model_rmm1(j,k)=!Values.F_NaN
            this_model_rmm2(j,k)=!Values.F_NaN            
         ENDIF ELSE BEGIN
            initial_date=dates(date_offset+j-our_lead_times(k))
            print,'Examining day '+dates(j+date_offset)+' at lead time '+STRTRIM(STRING(our_lead_times(k)),1)+$
                  ' using hindcast initialised on '+initial_date
            model_infile=indir+'/'+REFORM(initial_date)+'/rmm_indices.nc'
                                ;this_model_rmm1(j*n_times_per_day:(j+1)*n_times_per_day-1,k)=$
                                ;OPEN_AND_EXTRACT(model_infile(0),'rmm1',offset=[our_lead_times(k)*n_times_per_day],count=[n_times_per_day])
            model_rmm1_day=OPEN_AND_EXTRACT(model_infile(0),'rmm1',offset=[our_lead_times(k)*n_times_per_day],count=[n_times_per_day])
            this_model_rmm1(j,k)=MEAN(model_rmm1_day)
                                ;this_model_rmm2(j*n_times_per_day:(j+1)*n_times_per_day-1,k)=$
                                ;OPEN_AND_EXTRACT(model_infile(0),'rmm2',offset=[our_lead_times(k)*n_times_per_day],count=[n_times_per_day])
            model_rmm2_day=OPEN_AND_EXTRACT(model_infile(0),'rmm2',offset=[our_lead_times(k)*n_times_per_day],count=[n_times_per_day])
            this_model_rmm2(j,k)=MEAN(model_rmm2_day)
         ENDELSE
      ENDFOR
      print,REFORM(dates(j+date_offset))
      model_firstday_file=indir+'/'+REFORM(dates(j+date_offset))+'/rmm_indices.nc'            
      model_rmm1_day=OPEN_AND_EXTRACT(model_firstday_file(0),'rmm1',offset=[0],$
                                      count=[n_times_per_day])
      this_model_rmm1_dayone(j)=MEAN(model_rmm1_day)
      model_rmm2_day=OPEN_AND_EXTRACT(model_firstday_file(0),'rmm2',offset=[0],$
                                      count=[n_times_per_day])
      this_model_rmm2_dayone(j)=MEAN(model_rmm2_day)
   ENDFOR

   ; Make one output plot per model
   psfile='/home/ss901165/idl/mjo_diabatic/rmm_indices/mjo_diabatic_rmm_wheel_constvalid.'+desc+'.'+$
          start_date+'-'+stop_date+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE3=50,XOFFSET=1000,YOFFSET=100,TFONT=2,TCHARSIZE=90,CB_WIDTH=110,$
       XSIZE=18000,YSIZE=18000
   GSET,XMIN=-3.0,XMAX=3.0,YMIN=-3.5,YMAX=2,TITLE='RMM indices with lead time from '+desc+' for validation dates '+start_date+'-'+stop_date
   GPLOT,X=REPLICATE(0,2),Y=[-3.5,-1],STYLE=0,THICK=100,COL=FSC_COLOR('grey')
   GPLOT,X=REPLICATE(0,2),Y=[1,2],STYLE=0,THICK=100,COL=FSC_COLOR('grey')
   GPLOT,X=[1,3.0],Y=REPLICATE(0,3),STYLE=0,THICK=100,COL=FSC_COLOR('grey')
   GPLOT,X=[-1,-3.0],Y=REPLICATE(0,3),STYLE=0,THICK=100,COL=FSC_COLOR('grey')
   GPLOT,X=[SQRT(2)/2.,1,2],Y=[SQRT(2)/2.,1,2],STYLE=0,THICK=100,COL=FSC_COLOR('grey')
   GPLOT,X=[-SQRT(2)/2.,-1,-2],Y=[SQRT(2)/2.,1,2],STYLE=0,THICK=100,COL=FSC_COLOR('grey')
   GPLOT,X=[-SQRT(2)/2.,-1,-3.0],Y=[-SQRT(2)/2.,-1,-3.0],STYLE=0,THICK=100,COL=FSC_COLOR('grey')
   GPLOT,X=[SQRT(2)/2.,1,3.0],Y=[-SQRT(2)/2.,-1,-3.0],STYLE=0,THICK=100,COL=FSC_COLOR('grey')
   points=(2*!PI/99.0)*findgen(100)
   x=COS(points)
   y=SIN(points)
   white=FSC_COLOR("white",28)
   GPLOT,X=x,Y=y,FILLCOL=28,COL=FSC_COLOR('grey')

   CS,SCALE=color_scale,NCOLS=n_lead_times/our_lead_time_nth
;   rmm1_plot=SMOOTH(obs_rmm1,17)
;   rmm2_plot=SMOOTH(obs_rmm2,17)
   rmm1_plot=obs_rmm1
   rmm2_plot=obs_rmm2
   GPLOT,X=rmm1_plot,Y=rmm2_plot,COL=FSC_COLOR('black'),THICK=150
   GPLOT,X=rmm1_plot(0),Y=rmm2_plot(0),SYM=5
   GPLOT,X=rmm1_plot(n_days-1),Y=rmm2_plot(n_days-1),SYM=4
   FOR k=our_dots_nth,n_days-1,our_dots_nth DO $
      GPLOT,X=rmm1_plot(k),Y=rmm2_plot(k),$
                      SYM=3,SIZE=70,COL=FSC_COLOR('black')

   legend_items=strarr(n_lead_times/our_lead_time_nth+1)
   legend_items(0)='Obs (NOAA,ERA-Int)'
   colors=intarr(n_lead_times/our_lead_time_nth)
   FOR j=0,n_lead_times-1,our_lead_time_nth DO BEGIN
      this_color=j/our_lead_time_nth+2
;      rmm1_plot=SMOOTH(REFORM(this_model_rmm1(*,j)),17,/NaN) ;-(this_model_rmm1_dayone-obs_rmm1),17,/NaN)
;      rmm2_plot=SMOOTH(REFORM(this_model_rmm2(*,j)),17,/NaN) ;-(this_model_rmm2_dayone-obs_rmm2),17,/NaN)
;      rmm1_plot=SMOOTH(rmm1_plot,5)
;      rmm2_plot=SMOOTH(rmm2_plot,5)
      rmm1_plot=this_model_rmm1(*,j)-(this_model_rmm1_dayone-obs_rmm1)*0.65
      rmm2_plot=this_model_rmm2(*,j)-(this_model_rmm2_dayone-obs_rmm2)*0.65
      GPLOT,X=rmm1_plot,Y=rmm2_plot,COL=this_color,THICK=150-(100./FLOAT(n_lead_times))*j
      valid_points=where(FINITE(rmm1_plot) eq 1)
      GPLOT,X=rmm1_plot(0),Y=rmm2_plot(0),SYM=5,COL=this_color
      GPLOT,X=rmm1_plot(n_days-1),$
            Y=rmm2_plot(n_days-1),$
            COL=this_color,SYM=4
      FOR k=our_dots_nth,n_days-1,our_dots_nth DO $
         GPLOT,X=rmm1_plot(k),Y=rmm2_plot(k),$
                         COL=this_color,SYM=3,SIZE=70
      legend_items(j/our_lead_time_nth+1)='Lead time = '+STRTRIM(STRING(our_lead_times(j)),1)+' day(s)'
      colors(j/our_lead_time_nth)=this_color
   ENDFOR
   AXES,XSTEP=0.5,YSTEP=0.5,YMINOR=0.25,XMINOR=0.25,XTITLE='RMM1',YTITLE='RMM2',NDECS=1

   GLEGEND,labels=REVERSE(legend_items),COL=REVERSE([FSC_COLOR('black'),colors]),LEGPOS=13
   GLEGEND,labels=REVERSE([start_date,stop_date,'Every '+STRTRIM(STRING(our_dots_nth),1)+' days']),$
           LENGTH=0,SYM=REVERSE([5,4,3]),LEGPOS=14

   PSCLOSE
ENDFOR

STOP
END
