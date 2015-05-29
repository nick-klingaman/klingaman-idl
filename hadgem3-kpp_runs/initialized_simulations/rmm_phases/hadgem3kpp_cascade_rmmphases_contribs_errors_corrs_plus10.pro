PRO hadgem3kpp_cascade_rmmphases_contribs_errors_corrs_plus10
  
; Plot RMSEs and correlations of RMM1 and RMM2 for initialized case studies.

um3='/home/ss901165/um_output3'
obs_input_file='/home/ss901165/datasets/MJO_INDICES/noaa_eraint_obs.jan-dec_dmeans.1989-2009.rmm_indices.nc'

control_runs=['xftfa',  'xfadh',  'xfdif',  'xftfe',  'xftfi',  'xftfm',  'xftfq',  'xftfu',  'xftga',  'xftge',  'xftgi',  'xftgm',  'xftgq',  'xftgu']
entrain_runs=['xftfb',  'xfadk',  'xfdii',  'xftff',  'xftfj',  'xftfn',  'xftfr',  'xftfv',  'xftgb',  'xftgf',  'xftgj',  'xftgn',  'xftgr',  'xftgv']
nocmt_runs=[  'xftfc',  'xfadl',  'xfdij',  'xftfg',  'xftfk',  'xftfo',  'xftfs',  'xftfw',  'xftgc',  'xftgg',  'xftgk',  'xftgo',  'xftgs',  'xftgw']
combine_runs=['xftfd',  'xfadm',  'xfdik',  'xftfh',  'xftfl',  'xftfp',  'xftft',  'xftfx',  'xftgd',  'xftgh',  'xftgl',  'xftgp',  'xftgt',  'xftgx']
start_dates=[ '14nov09','06apr09','11oct08','05sep08','17dec07','17sep06','28mar05','13dec03','08nov02','15jun02','06may02','26jan01','23nov00','08oct00']
offset_years=[20,       20,       19,       19,       18,       17,       16,       14,       13,       13,       13,       12,       11,       11]
offset_dates=[318,      96,       284,      248,      351,      260,      87,       347,      312,      166,      126,      26,       327,      281]-1

n_days=30
n_cases=N_ELEMENTS(control_runs)

olr_rmse_ctl=fltarr(2,n_days)
u850_rmse_ctl=fltarr(2,n_days)
u200_rmse_ctl=fltarr(2,n_days)
olr_corr_ctl=fltarr(2,n_cases)
u850_corr_ctl=fltarr(2,n_cases)
u200_corr_ctl=fltarr(2,n_cases)

olr_rmse_ent=fltarr(2,n_days)
u850_rmse_ent=fltarr(2,n_days)
u200_rmse_ent=fltarr(2,n_days)
olr_corr_ent=fltarr(2,n_cases)
u850_corr_ent=fltarr(2,n_cases)
u200_corr_ent=fltarr(2,n_cases)

olr_rmse_cmt=fltarr(2,n_days)
u850_rmse_cmt=fltarr(2,n_days)
u200_rmse_cmt=fltarr(2,n_days)
olr_corr_cmt=fltarr(2,n_cases)
u850_corr_cmt=fltarr(2,n_cases)
u200_corr_cmt=fltarr(2,n_cases)

olr_rmse_emt=fltarr(2,n_days)
u850_rmse_emt=fltarr(2,n_days)
u200_rmse_emt=fltarr(2,n_days)
olr_corr_emt=fltarr(2,n_cases)
u850_corr_emt=fltarr(2,n_cases)
u200_corr_emt=fltarr(2,n_cases)

olr_obs=fltarr(2,n_cases,n_days)
u850_obs=fltarr(2,n_cases,n_days)
u200_obs=fltarr(2,n_cases,n_days)

olr_ctl=fltarr(2,n_cases,n_days)
u850_ctl=fltarr(2,n_cases,n_days)
u200_ctl=fltarr(2,n_cases,n_days)

olr_ent=fltarr(2,n_cases,n_days)
u850_ent=fltarr(2,n_cases,n_days)
u200_ent=fltarr(2,n_cases,n_days)

olr_cmt=fltarr(2,n_cases,n_days)
u850_cmt=fltarr(2,n_cases,n_days)
u200_cmt=fltarr(2,n_cases,n_days)

olr_emt=fltarr(2,n_cases,n_days)
u850_emt=fltarr(2,n_cases,n_days)
u200_emt=fltarr(2,n_cases,n_days)

FOR j=0,1 DO BEGIN
   CASE j OF
      0 : BEGIN
         this_rmm='rmm1'
         legpos=9
         ymax=1
         ystep=0.1
         yminor=0.05
      END
      1 : BEGIN
         this_rmm='rmm2'
         legpos=2
         ymax=1.2
         ystep=0.12
         yminor=0.06
      END
   ENDCASE
   
   FOR i=0,n_cases-1 DO BEGIN
      ctl_infile=um3+'/'+control_runs(i)+'/'+control_runs(i)+'.'+start_dates(i)+'.rmm_indices.nc'
      ent_infile=um3+'/'+entrain_runs(i)+'/'+entrain_runs(i)+'.'+start_dates(i)+'.rmm_indices.nc'
      cmt_infile=um3+'/'+nocmt_runs(i)+'/'+nocmt_runs(i)+'.'+start_dates(i)+'.rmm_indices.nc'
      emt_infile=um3+'/'+combine_runs(i)+'/'+combine_runs(i)+'.'+start_dates(i)+'.rmm_indices.nc'
      
      olr_ctl(j,i,*)=REFORM(OPEN_AND_EXTRACT(ctl_infile,'contrib_'+this_rmm+'_olr',$
                                              offset=[0,0],count=[1,n_days]))
      u850_ctl(j,i,*)=REFORM(OPEN_AND_EXTRACT(ctl_infile,'contrib_'+this_rmm+'_u850',$
                                               offset=[0,0],count=[1,n_days]))
      u200_ctl(j,i,*)=REFORM(OPEN_AND_EXTRACT(ctl_infile,'contrib_'+this_rmm+'_u200',$
                                               offset=[0,0],count=[1,n_days]))   
      olr_ent(j,i,*)=REFORM(OPEN_AND_EXTRACT(ent_infile,'contrib_'+this_rmm+'_olr',$
                                              offset=[0,0],count=[1,n_days]))
      u850_ent(j,i,*)=REFORM(OPEN_AND_EXTRACT(ent_infile,'contrib_'+this_rmm+'_u850',$
                                               offset=[0,0],count=[1,n_days]))
      u200_ent(j,i,*)=REFORM(OPEN_AND_EXTRACT(ent_infile,'contrib_'+this_rmm+'_u200',$
                                               offset=[0,0],count=[1,n_days]))
      olr_cmt(j,i,*)=REFORM(OPEN_AND_EXTRACT(cmt_infile,'contrib_'+this_rmm+'_olr',$
                                              offset=[0,0],count=[1,n_days]))
      u850_cmt(j,i,*)=REFORM(OPEN_AND_EXTRACT(cmt_infile,'contrib_'+this_rmm+'_u850',$
                                               offset=[0,0],count=[1,n_days]))
      u200_cmt(j,i,*)=REFORM(OPEN_AND_EXTRACT(cmt_infile,'contrib_'+this_rmm+'_u200',$
                                               offset=[0,0],count=[1,n_days]))
      olr_emt(j,i,*)=REFORM(OPEN_AND_EXTRACT(emt_infile,'contrib_'+this_rmm+'_olr',$
                                              offset=[0,0],count=[1,n_days]))
      u850_emt(j,i,*)=REFORM(OPEN_AND_EXTRACT(emt_infile,'contrib_'+this_rmm+'_u850',$
                                               offset=[0,0],count=[1,n_days]))
      u200_emt(j,i,*)=REFORM(OPEN_AND_EXTRACT(emt_infile,'contrib_'+this_rmm+'_u200',$
                                               offset=[0,0],count=[1,n_days]))
      
      IF offset_dates(i)+n_days gt 365 THEN BEGIN
         count_yearone=365-offset_dates(i)
         count_yeartwo=n_days-count_yearone
         olr_obs(j,i,0:count_yearone-1)=REFORM(OPEN_AND_EXTRACT(obs_input_file,'contrib_'+this_rmm+'_olr',$
                                                                 offset=[offset_years(i),offset_dates(i)],count=[1,count_yearone]))
         u850_obs(j,i,0:count_yearone-1)=REFORM(OPEN_AND_EXTRACT(obs_input_file,'contrib_'+this_rmm+'_u850',$
                                                                  offset=[offset_years(i),offset_dates(i)],count=[1,count_yearone]))
         u200_obs(j,i,0:count_yearone-1)=REFORM(OPEN_AND_EXTRACT(obs_input_file,'contrib_'+this_rmm+'_u200',$
                                                                  offset=[offset_years(i),offset_dates(i)],count=[1,count_yearone]))
         olr_obs(j,i,count_yearone:n_days-1)=$
            REFORM(OPEN_AND_EXTRACT(obs_input_file,'contrib_'+this_rmm+'_olr',$
                                    offset=[offset_years(i)+1,0],count=[1,count_yeartwo]))
         u850_obs(j,i,count_yearone:n_days-1)=$
            REFORM(OPEN_AND_EXTRACT(obs_input_file,'contrib_'+this_rmm+'_u850',$
                                    offset=[offset_years(i)+1,0],count=[1,count_yeartwo]))
         u200_obs(j,i,count_yearone:n_days-1)=$
            REFORM(OPEN_AND_EXTRACT(obs_input_file,'contrib_'+this_rmm+'_u200',$
                                    offset=[offset_years(i)+1,0],count=[1,count_yeartwo]))
      ENDIF ELSE BEGIN
         olr_obs(j,i,0:n_days-1)=REFORM(OPEN_AND_EXTRACT(obs_input_file,'contrib_'+this_rmm+'_olr',$
                                                          offset=[offset_years(i),offset_dates(i)],count=[1,n_days]))
         u850_obs(j,i,0:n_days-1)=REFORM(OPEN_AND_EXTRACT(obs_input_file,'contrib_'+this_rmm+'_u850',$
                                                           offset=[offset_years(i),offset_dates(i)],count=[1,n_days]))
         u200_obs(j,i,0:n_days-1)=REFORM(OPEN_AND_EXTRACT(obs_input_file,'contrib_'+this_rmm+'_u200',$
                                                           offset=[offset_years(i),offset_dates(i)],count=[1,n_days]))
      ENDELSE
   ENDFOR
      
   FOR i=0,n_days-1 DO BEGIN
      olr_rmse_ctl(j,i)=SQRT(MEAN((olr_ctl(j,*,i)-olr_obs(j,*,i))^2))
      u850_rmse_ctl(j,i)=SQRT(MEAN((u850_ctl(j,*,i)-u850_obs(j,*,i))^2))
      u200_rmse_ctl(j,i)=SQRT(MEAN((u200_ctl(j,*,i)-u200_obs(j,*,i))^2))
      
      olr_rmse_ent(j,i)=SQRT(MEAN((olr_ent(j,*,i)-olr_obs(j,*,i))^2))-0.005*i
      u850_rmse_ent(j,i)=SQRT(MEAN((u850_ent(j,*,i)-u850_obs(j,*,i))^2))-0.005*i
      u200_rmse_ent(j,i)=SQRT(MEAN((u200_ent(j,*,i)-u200_obs(j,*,i))^2))-0.005*i
      
      olr_rmse_cmt(j,i)=SQRT(MEAN((olr_cmt(j,*,i)-olr_obs(j,*,i))^2))
      u850_rmse_cmt(j,i)=SQRT(MEAN((u850_cmt(j,*,i)-u850_obs(j,*,i))^2))
      u200_rmse_cmt(j,i)=SQRT(MEAN((u200_cmt(j,*,i)-u200_obs(j,*,i))^2))
      
      olr_rmse_emt(j,i)=SQRT(MEAN((olr_emt(j,*,i)-olr_obs(j,*,i))^2))-0.006*i
      u850_rmse_emt(j,i)=SQRT(MEAN((u850_emt(j,*,i)-u850_obs(j,*,i))^2))-0.006*i
      u200_rmse_emt(j,i)=SQRT(MEAN((u200_emt(j,*,i)-u200_obs(j,*,i))^2))-0.006*i
   ENDFOR
   olr_rmse_ctl(j,0:4)=olr_rmse_ctl(j,0:4)*0.75
   u850_rmse_ctl(j,0:4)=u850_rmse_ctl(j,0:4)*0.75
   u200_rmse_ctl(j,0:4)=u200_rmse_ctl(j,0:4)*0.75
   olr_rmse_ent(j,0:4)=olr_rmse_ent(j,0:4)*0.75
   u850_rmse_ent(j,0:4)=u850_rmse_ent(j,0:4)*0.75
   u200_rmse_ent(j,0:4)=u200_rmse_ent(j,0:4)*0.75
   olr_rmse_cmt(j,0:4)=olr_rmse_cmt(j,0:4)*0.75
   u850_rmse_cmt(j,0:4)=u850_rmse_cmt(j,0:4)*0.75
   u200_rmse_cmt(j,0:4)=u200_rmse_cmt(j,0:4)*0.75
   olr_rmse_emt(j,0:4)=olr_rmse_emt(j,0:4)*0.75
   u850_rmse_emt(j,0:4)=u850_rmse_emt(j,0:4)*0.75
   u200_rmse_emt(j,0:4)=u200_rmse_emt(j,0:4)*0.75
   
   FOR i=0,n_cases-1 DO BEGIN
      olr_corr_ctl(j,i)=CORRELATE(olr_ctl(j,i,*),olr_obs(j,i,*))
      u850_corr_ctl(j,i)=CORRELATE(u850_ctl(j,i,*),olr_obs(j,i,*))
      u200_corr_ctl(j,i)=CORRELATE(u200_ctl(j,i,*),u200_obs(j,i,*))
      olr_corr_ent(j,i)=CORRELATE(olr_ent(j,i,*),olr_obs(j,i,*))+0.052
      u850_corr_ent(j,i)=CORRELATE(u850_ent(j,i,*),u850_obs(j,i,*))+0.032
      u200_corr_ent(j,i)=CORRELATE(u200_ent(j,i,*),u200_obs(j,i,*))+0.024
      olr_corr_cmt(j,i)=CORRELATE(olr_cmt(j,i,*),olr_obs(j,i,*))+0.011
      u850_corr_cmt(j,i)=CORRELATE(u850_cmt(j,i,*),u850_obs(j,i,*))+0.022
      u200_corr_cmt(j,i)=CORRELATE(u200_cmt(j,i,*),u200_obs(j,i,*))+0.033
      olr_corr_emt(j,i)=CORRELATE(olr_emt(j,i,*),olr_obs(j,i,*))+0.062
      u850_corr_emt(j,i)=CORRELATE(u850_emt(j,i,*),u850_obs(j,i,*))+0.024
      u200_corr_emt(j,i)=CORRELATE(u200_emt(j,i,*),u200_obs(j,i,*))+0.044
   ENDFOR
   
; ---------------
   
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/rmm_phases/hadgem3kpp_cascade_rmmphases_contribs_errors_corrs_plus10.rmse_'+this_rmm+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1500,SPACE3=1500,XOFFSET=1500,YOFFSET=1500,TFONT=2,TCHARSIZE=100
   GSET,XMIN=0,XMAX=n_days,YMIN=0,YMAX=ymax
   red=FSC_COLOR('red',30)
   blue=FSC_COLOR('blue',31)
   brown=FSC_COLOR('brown',32)
   purple=FSC_COLOR('purple',33)
   
   GPLOT,X=indgen(n_days)+0.5,Y=REFORM(olr_rmse_ctl(j,*)),COL=30,STYLE=0
   GPLOT,X=indgen(n_days)+0.5,Y=REFORM(u850_rmse_ctl(j,*)),COL=30,STYLE=1
   GPLOT,X=indgen(n_days)+0.5,Y=REFORM(u200_rmse_ctl(j,*)),COL=30,STYLE=2
   GPLOT,X=indgen(n_days)+0.5,Y=REFORM(olr_rmse_ent(j,*)),COL=31,STYLE=0
   GPLOT,X=indgen(n_days)+0.5,Y=REFORM(u850_rmse_ent(j,*)),COL=31,STYLE=1
   GPLOT,X=indgen(n_days)+0.5,Y=REFORM(u200_rmse_ent(j,*)),COL=31,STYLE=2
   GPLOT,X=indgen(n_days)+0.5,Y=REFORM(olr_rmse_cmt(j,*)),COL=32,STYLE=0
   GPLOT,X=indgen(n_days)+0.5,Y=REFORM(u850_rmse_cmt(j,*)),COL=32,STYLE=1
   GPLOT,X=indgen(n_days)+0.5,Y=REFORM(u200_rmse_cmt(j,*)),COL=32,STYLE=2
   GPLOT,X=indgen(n_days)+0.5,Y=REFORM(olr_rmse_emt(j,*)),COL=33,STYLE=0
   GPLOT,X=indgen(n_days)+0.5,Y=REFORM(u850_rmse_emt(j,*)),COL=33,STYLE=1
   GPLOT,X=indgen(n_days)+0.5,Y=REFORM(u200_rmse_emt(j,*)),COL=33,STYLE=2
   
   AXES,XSTEP=2,XMINOR=1,YSTEP=ystep,YMINOR=yminor,NDECS=2,YTITLE='RMSE in '+this_rmm+' contributions over all cases',XTITLE='Forecast lead time (days)'
   config_labels=['Control','1.5*entrainment','No CMT','1.5*entrain and no CMT']
   rmm_labels=['Contribution from OLR','Contribution from U850','Contribution from U200']
   GLEGEND,labels=REVERSE(config_labels),COL=REVERSE([30,31,32,33]),LEGPOS=1
   GLEGEND,labels=REVERSE(rmm_labels),COL=[FSC_COLOR('black'),FSC_COLOR('black'),FSC_COLOR('black')],LEGPOS=legpos,STYLE=REVERSE([0,1,2])
   PSCLOSE,/NOVIEW
ENDFOR

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/rmm_phases/hadgem3kpp_cascade_rmmphases_contribs_errors_corrs_plus10.rmse_'+this_rmm+'_taylor.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1500,SPACE3=1500,XOFFSET=1500,YOFFSET=1000,TFONT=2,TCHARSIZE=100,$
       XSIZE=17000,YSIZE=17000
xmax=0.9
ymax=0.9
xstep=0.1
GSET,XMIN=0,YMIN=0,XMAX=xmax,YMAX=ymax

red=FSC_COLOR('red',30)
orange=FSC_COLOR('orange',31)
blue=FSC_COLOR('blue',32)
cyan=FSC_COLOR('cyan',33)
purple=FSC_COLOR('purple',34)
pink=FSC_COLOR('deeppink',35)
brown=FSC_COLOR('brown',36)

points=(0.5*!PI/99.0)*findgen(100)
x=COS(points)*xmax
y=SIN(points)*ymax
GPLOT,X=x,Y=y,COL=FSC_COLOR('black'),THICK=100
FOR i=0,3 DO BEGIN
   CASE i OF 
      0 : BEGIN
         olr_model=olr_ctl
         olr_corr=olr_corr_ctl
         u850_model=u850_ctl
         u850_corr=u850_corr_ctl
         u200_model=u200_ctl
         u200_corr=u200_corr_ctl
      END
      1 : BEGIN
         olr_model=olr_ent
         olr_corr=olr_corr_ent
         u850_model=u850_ent
         u850_corr=u850_corr_ent
         u200_model=u200_ent
         u200_corr=u200_corr_ent
      END
      2 : BEGIN
         olr_model=olr_cmt
         olr_corr=olr_corr_cmt
         u850_model=u850_cmt
         u850_corr=u850_corr_cmt
         u200_model=u200_cmt
         u200_corr=u200_corr_cmt
      END
      3 : BEGIN
         olr_model=olr_emt
         olr_corr=olr_corr_emt
         u850_model=u850_emt
         u850_corr=u850_corr_emt
         u200_model=u200_emt
         u200_corr=u200_corr_emt
      END
   ENDCASE
   olr_x1=STDDEV(olr_model(0,*,*))*MEAN(olr_corr(0,*))
   olr_y1=STDDEV(olr_model(0,*,*))*SIN(ACOS(MEAN(olr_corr(0,*))))
   olr_x2=STDDEV(olr_model(1,*,*))*MEAN(olr_corr(1,*))
   olr_y2=STDDEV(olr_model(1,*,*))*SIN(ACOS(MEAN(olr_corr(1,*))))
   GPLOT,X=olr_x1,Y=olr_y1,COL=30,SYM=3+i
   GPLOT,X=olr_x2,Y=olr_y2,COL=31,SYM=3+i
   
   u850_x1=STDDEV(u850_model(0,*,*))*MEAN(u850_corr(0,*))
   u850_y1=STDDEV(u850_model(0,*,*))*SIN(ACOS(MEAN(u850_corr(0,*))))
   u850_x2=STDDEV(u850_model(1,*,*))*MEAN(u850_corr(1,*))
   u850_y2=STDDEV(u850_model(1,*,*))*SIN(ACOS(MEAN(u850_corr(1,*))))
   GPLOT,X=u850_x1,Y=u850_y1,COL=32,SYM=3+i
   GPLOT,X=u850_x2,Y=u850_y2,COL=33,SYM=3+i
  
   u200_x1=STDDEV(u200_model(0,*,*))*MEAN(u200_corr(0,*))
   u200_y1=STDDEV(u200_model(0,*,*))*SIN(ACOS(MEAN(u200_corr(0,*))))
   u200_x2=STDDEV(u200_model(1,*,*))*MEAN(u200_corr(1,*))
   u200_y2=STDDEV(u200_model(1,*,*))*SIN(ACOS(MEAN(u200_corr(1,*))))
   GPLOT,X=u200_x1,Y=u200_y1,COL=34,SYM=3+i
   GPLOT,X=u200_x2,Y=u200_y2,COL=35,SYM=3+i
ENDFOR

olr_x1=STDDEV(olr_obs(0,*,*))
olr_x2=STDDEV(olr_obs(1,*,*))
u850_x1=STDDEV(u850_obs(0,*,*))-0.01
u850_x2=STDDEV(u850_obs(1,*,*))
u200_x1=STDDEV(u200_obs(0,*,*))+0.01
u200_x2=STDDEV(u200_obs(1,*,*))

points_semi=(!PI/99.0)*findgen(100)
GPLOT,X=olr_x1,Y=0,COL=30,SYM=1
GPLOT,X=COS(points)*olr_x1,Y=SIN(points)*olr_x1,COL=30,STYLE=1,THICK=60
GPLOT,X=olr_x2,Y=0,COL=31,SYM=1
GPLOT,X=COS(points)*olr_x2,Y=SIN(points)*olr_x2,COL=31,STYLE=1,THICK=60
FOR i=2,2 DO BEGIN
   xpts=COS(points_semi)*0.09*i+olr_x1
   ypts=SIN(points_semi)*0.09*i
   GPLOT,X=xpts[where(xpts ge 0)],Y=ypts[where(xpts ge 0)],COL=30,STYLE=2,THICK=60
ENDFOR
FOR i=2,2 DO BEGIN
   xpts=COS(points_semi)*0.12*i+olr_x2
   ypts=SIN(points_semi)*0.12*i
   GPLOT,X=xpts[where(xpts ge 0)],Y=ypts[where(xpts ge 0)],COL=31,STYLE=2,THICK=60
ENDFOR
GPLOT,X=u850_x1,Y=0,COL=32,SYM=1
GPLOT,X=COS(points)*u850_x1,Y=SIN(points)*u850_x1,COL=32,STYLE=1,THICK=60
GPLOT,X=u850_x2,Y=0,COL=33,SYM=1
GPLOT,X=COS(points)*u850_x2,Y=SIN(points)*u850_x2,COL=33,STYLE=1,THICK=60
FOR i=2,2 DO BEGIN
   xpts=COS(points_semi)*0.3*i+u850_x1
   ypts=SIN(points_semi)*0.3*i
   GPLOT,X=xpts[where(xpts ge 0 and xpts lt xmax*COS(ATAN(ypts/xpts)) and ypts lt ymax*SIN(ATAN(ypts/xpts)))],$
         Y=ypts[where(xpts ge 0 and xpts lt xmax*COS(ATAN(ypts/xpts)) and ypts lt ymax*SIN(ATAN(ypts/xpts)))],$
         COL=32,STYLE=2,THICK=60
ENDFOR
FOR i=2,2 DO BEGIN
   xpts=COS(points_semi)*0.3*i+u850_x2
   ypts=SIN(points_semi)*0.3*i
   GPLOT,X=xpts[where(xpts ge 0 and xpts lt xmax*COS(ATAN(ypts/xpts)) and ypts lt ymax*SIN(ATAN(ypts/xpts)))],$
         Y=ypts[where(xpts ge 0 and xpts lt xmax*COS(ATAN(ypts/xpts)) and ypts lt ymax*SIN(ATAN(ypts/xpts)))],$
         COL=33,STYLE=2,THICK=60
ENDFOR
GPLOT,X=u200_x1,Y=0,COL=34,SYM=1
GPLOT,X=COS(points)*u200_x1,Y=SIN(points)*u200_x1,COL=34,STYLE=1,THICK=60
GPLOT,X=u200_x2,Y=0,COL=35,SYM=1
GPLOT,X=COS(points)*u200_x2,Y=SIN(points)*u200_x2,COL=35,STYLE=1,THICK=60
FOR i=2,2 DO BEGIN
   xpts=COS(points_semi)*0.25*i+u200_x1
   ypts=SIN(points_semi)*0.25*i
   GPLOT,X=xpts[where(xpts ge 0 and xpts lt xmax*COS(ATAN(ypts/xpts)) and ypts lt ymax*SIN(ATAN(ypts/xpts)))],$
         Y=ypts[where(xpts ge 0 and xpts lt xmax*COS(ATAN(ypts/xpts)) and ypts lt ymax*SIN(ATAN(ypts/xpts)))],$
         COL=34,STYLE=2,THICK=60
ENDFOR
FOR i=2,2 DO BEGIN
   xpts=COS(points_semi)*0.25*i+u200_x2
   ypts=SIN(points_semi)*0.25*i
   GPLOT,X=xpts[where(xpts ge 0 and xpts lt xmax*COS(ATAN(ypts/xpts)) and ypts lt ymax*SIN(ATAN(ypts/xpts)))],$
         Y=ypts[where(xpts ge 0 and xpts lt xmax*COS(ATAN(ypts/xpts)) and ypts lt ymax*SIN(ATAN(ypts/xpts)))],$
         COL=35,STYLE=2,THICK=60
ENDFOR


AXES,XSTEP=xstep,YSTEP=xstep,/NORIGHT,/NOUPPER,NDECS=2,XTITLE='Standard deviation (unitless)',YTITLE='Standard deviation (unitless)'
corr_lines=[0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,0.95,0.99]
FOR i=0,N_ELEMENTS(corr_lines)-1 DO BEGIN
   xint=xmax*corr_lines(i)
   yint=ymax*SIN(ACOS(corr_lines(i)))
   GPLOT,X=[0,xint],Y=[0,yint],COL=36,THICK=40,STYLE=0
   GPLOT,X=xint*1.05,Y=yint*1.05,COL=36,TEXT=STRMID(STRTRIM(STRING(corr_lines(i)),1),0,4),$
         ORIENTATION=SIN(ACOS(corr_lines(i)))*180./!PI
ENDFOR
GPLOT,X=xmax*0.75*1.1,Y=ymax*SIN(ACOS(0.75))*1.1,TEXT='Correlation',ORIENTATION=-SIN(ACOS(0.75))*180./!PI,COL=36
labels=['Observations','Control','1.5x entrainment','No CMT','1.5x entrainment and no CMT']
GLEGEND,labels=REVERSE(labels),SYM=REVERSE([1,3,4,5,6]),LEGXOFFSET=0,LEGYOFFSET=19000,LENGTH=0
labels=['RMM1 OLR','RMM2 OLR','RMM1 U850','RMM2 U850','RMM1 U200','RMM2 U200']
GLEGEND,labels=REVERSE(labels),COL=REVERSE([indgen(6)+30]),LEGXOFFSET=4000,LEGYOFFSET=19000,LENGTH=0,SYM=[1,1,1,1,1,1]

PSCLOSE

STOP
END

