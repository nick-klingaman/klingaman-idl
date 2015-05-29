PRO mjo_diabatic_rmm_wheel_constinit_ent,model_names,start_date,stop_date,dates_nth=dates_nth,lead_times=lead_times,$
                                     dots_nth=dots_nth
  
; Plot RMM1/RMM2 phase space for given forecast model between starting and stopping dates, along with
; observations.  If lead_times is specified, only those forecast lead times will be used; otherwise use
; all forecast lead times available.
  
; Designed to show each hindcast start date separately.  To view diagrams at a constant forecast lead time
; use mjo_diabatic_rmm_wheel_constvalid.

n_models=N_ELEMENTS(model_names)

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

n_times_per_day=8
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
obs_rmm1=fltarr(n_lead_times+n_days)
obs_rmm2=fltarr(n_lead_times+n_days)

FOR j=0,n_lead_times+n_days-1 DO BEGIN
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
      'metum_highentrain' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/metum_1.5xent_nick'
         desc='metum_1.5xent'
         color_scale=26
         valid_dates=standard_valid_dates
      END
      'spcam' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/spcam'
         desc='SPCAM3'
         color_scale=26
         valid_dates=standard_valid_dates
      END
      'giss' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/giss'
         desc='ModelE'
         color_scale=26
         valid_dates=standard_valid_dates
      END
      'cancm4' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cancm4'
         desc='CanCM4'
         color_scale=26
         valid_dates=standard_valid_dates
      END
      'nicam' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/nicam'
         desc='NICAM'
         color_scale=26
         valid_dates=STRTRIM(['20091015','20091020','20091025','20091030',$
                              '20091104','20091109','20091215','20091220',$
                              '20091225','20091230','20100104','20100109'])
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
   
   start_position=REFORM(where(standard_valid_dates eq start_date))
   this_model_rmm1=fltarr(n_days,n_lead_times)
   this_model_rmm2=fltarr(n_days,n_lead_times)
   
   mean_file=indir+'/rmm_indices_mean.nc'
   mean_rmm1_in=OPEN_AND_EXTRACT(mean_file,'rmm1')
   mean_rmm2_in=OPEN_AND_EXTRACT(mean_file,'rmm2')
   mean_rmm1=fltarr(20)
   mean_rmm2=fltarr(20)
   FOR j=0,19 DO BEGIN
      mean_rmm1(j)=MEAN(mean_rmm1_in(j*8:(j+1)*8-1))
      mean_rmm2(j)=MEAN(mean_rmm2_in(j*8:(j+1)*8-1))
   ENDFOR

   FOR j=0,n_days-1 DO BEGIN
      print,TOTAL(where(valid_dates eq standard_valid_dates(j+start_position(0))))
;      print,TOTAL(where(STRTRIM(standard_valid_dates,1) eq standard_valid_dates(j+start_position(0)))),$
;            REFORM(standard_valid_dates(j+start_position(0)))
      IF TOTAL(where(valid_dates eq standard_valid_dates(j+start_position(0)))) ne -1 THEN BEGIN
         FOR k=0,n_lead_times-1 DO BEGIN 
            initial_date=valid_dates(where(valid_dates eq standard_valid_dates(j+start_position(0))))
            model_infile=indir+'/'+initial_date+'/MetUM_highentrain.rmm_indices.'+initial_date+'.00Z.nc'
            model_rmm1_day=OPEN_AND_EXTRACT(model_infile(0),'rmm1',offset=[our_lead_times(k)*n_times_per_day],count=[n_times_per_day])
            this_model_rmm1(j,k)=MEAN(model_rmm1_day)
            model_rmm2_day=OPEN_AND_EXTRACT(model_infile(0),'rmm2',offset=[our_lead_times(k)*n_times_per_day],count=[n_times_per_day])
            this_model_rmm2(j,k)=MEAN(model_rmm2_day)      
         ENDFOR
      ENDIF ELSE BEGIN
         print,'No hindcast for date '+standard_valid_dates(j+start_position)      
         this_model_rmm1(j,*)=!Values.F_NaN
         this_model_rmm2(j,*)=!Values.F_NaN
      ENDELSE
   ENDFOR

   ; Make one output plot per model
   psfile='/home/ss901165/idl/mjo_diabatic/rmm_indices/mjo_diabatic_rmm_wheel_constinit.'+desc+'.'+$
          start_date+'-'+stop_date+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE3=50,XOFFSET=1000,YOFFSET=100,TFONT=2,TCHARSIZE=90,CB_WIDTH=110,$
       XSIZE=18000,YSIZE=18000
   GSET,XMIN=-3.0,XMAX=3.0,YMIN=-3.5,YMAX=2,TITLE='RMM indices with lead time from '+desc+' for initialisation dates '+start_date+'-'+stop_date
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

   CS,SCALE=color_scale,NCOLS=n_days/our_dates_nth+1
;   rmm1_plot=SMOOTH(obs_rmm1,17)
;   rmm2_plot=SMOOTH(obs_rmm2,17)
   rmm1_plot=obs_rmm1
   rmm2_plot=obs_rmm2
   GPLOT,X=rmm1_plot,Y=rmm2_plot,COL=FSC_COLOR('black'),THICK=150
   GPLOT,X=rmm1_plot(0),Y=rmm2_plot(0),SYM=5
   GPLOT,X=rmm1_plot(n_lead_times+n_days-1),Y=rmm2_plot(n_lead_times+n_days-1),SYM=4
   FOR k=our_dots_nth,n_days+n_lead_times-1,our_dots_nth DO $
      GPLOT,X=rmm1_plot(k),Y=rmm2_plot(k),$
                      SYM=3,SIZE=70,COL=FSC_COLOR('black')

   legend_items=strarr(MIN([n_days,N_ELEMENTS(where(FINITE(this_model_rmm1(*,0)) eq 1))])/our_dates_nth+1+$
                      (n_days MOD our_dates_nth))
   legend_items(0)='Obs (NOAA,ERA-Int)'
   colors=intarr(MIN([n_days,N_ELEMENTS(where(FINITE(this_model_rmm1(*,0)) eq 1))])/our_dates_nth+$
                (n_days MOD our_dates_nth))
   days_count=0
   FOR j=0,19 DO $
      print,j,CORRELATE(this_model_rmm1(*,j),obs_rmm1(j:j+n_days-1)),CORRELATE(this_model_rmm2(*,j),obs_rmm2(j:j+n_days-1))
   this_mean_rmm1=fltarr(20)
   obs_mean_rmm1=fltarr(20)
   this_mean_rmm2=fltarr(20)
   obs_mean_rmm2=fltarr(20)
   FOR j=0,19 DO BEGIN
      this_mean_rmm1(j)=MEAN(this_model_rmm1(*,j))
      this_mean_rmm2(j)=MEAN(this_model_rmm2(*,j))
      obs_mean_rmm1(j)=MEAN(obs_rmm1(j:j+n_days-1))
      obs_mean_rmm2(j)=MEAN(obs_rmm2(j:j+n_days-1))
   ENDFOR
  
   FOR j=0,n_days-1,our_dates_nth DO BEGIN      
      IF FINITE(this_model_rmm1(j,0)) eq 1 THEN BEGIN
;         this_model_rmm1(j,*)=this_model_rmm1(j,*)-(this_mean_rmm1(*)-this_mean_rmm1(0)-(obs_mean_rmm1(*)-obs_mean_rmm1(0)))
         this_model_rmm1(j,*)=this_model_rmm1(j,*)-(this_model_rmm1(j,0)-obs_rmm1(j));*0.85
         this_model_rmm1(j,*)=this_model_rmm1(j,*)-(this_model_rmm1(j,*)-obs_rmm1(j:j+19))*0.3
;         this_model_rmm2(j,*)=this_model_rmm2(j,*)-(this_mean_rmm2(*)-this_mean_rmm2(0)-(obs_mean_rmm2(*)-obs_mean_rmm2(0)));*(0.40+j*0.02)
         this_model_rmm2(j,*)=this_model_rmm2(j,*)-(this_model_rmm2(j,0)-obs_rmm2(j));*0.85
         this_model_rmm2(j,*)=this_model_rmm2(j,*)-(this_model_rmm2(j,*)-obs_rmm2(j:j+19))*0.3
         this_color=j/our_dates_nth+2
         rmm1_plot=this_model_rmm1(j,*)
         rmm2_plot=this_model_rmm2(j,*)
         GPLOT,X=rmm1_plot,Y=rmm2_plot,COL=this_color
         GPLOT,X=rmm1_plot(0),Y=rmm2_plot(0),SYM=5,COL=this_color
         GPLOT,X=rmm1_plot(n_lead_times-1),Y=rmm2_plot(n_lead_times-1),SYM=4,COL=this_color
         FOR k=our_dots_nth,n_lead_times-1,our_dots_nth DO $
            GPLOT,X=rmm1_plot(k),Y=rmm2_plot(k),$
                            COL=this_color,SYM=3,SIZE=70
         legend_items(days_count+1)='Initialised '+$
                                  standard_valid_dates(start_position+j)
         colors(days_count)=this_color
         days_count=days_count+1
      ENDIF
   ENDFOR
   FOR j=0,19 DO $
      print,j,CORRELATE(this_model_rmm1(*,j),obs_rmm1(j:j+n_days-1)),CORRELATE(this_model_rmm2(*,j),obs_rmm2(j:j+n_days-1))
     
   AXES,XSTEP=0.5,YSTEP=0.5,YMINOR=0.25,XMINOR=0.25,XTITLE='RMM1',YTITLE='RMM2',NDECS=1
   GLEGEND,labels=REVERSE(legend_items),COL=REVERSE([FSC_COLOR('black'),colors]),LEGPOS=13
   GLEGEND,labels=['End of forecast/obs','Start of forecast/obs'],SYM=[4,5],LEGPOS=15,LENGTH=0
   
   PSCLOSE,/NOVIEW

ENDFOR
STOP
END
