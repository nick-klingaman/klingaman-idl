PRO hadgem3kpp_dcyc_search_suppressed

xjnoa='/home/ss901165/um_output6/xjnoa'
rmm_file=xjnoa+'/rmm_indices.nc'
n_times_per_day=8  
n_years=4
n_days_per_year=360
year_offset=0
n_days=n_years*n_days_per_year

rmm_amp_in=OPEN_AND_EXTRACT(rmm_file,'amplitude',offset=[year_offset,0],count=[n_years,n_days_per_year*n_times_per_day])
rmm1_in=OPEN_AND_EXTRACT(rmm_file,'rmm1',offset=[year_offset,0],count=[n_years,n_days_per_year*n_times_per_day])
rmm2_in=OPEN_AND_EXTRACT(rmm_file,'rmm2',offset=[year_offset,0],count=[n_years,n_days_per_year*n_times_per_day])
rmm_phase_in=OPEN_AND_EXTRACT(rmm_file,'phase',offset=[year_offset,0],count=[n_years,n_days_per_year*n_times_per_day])
rmm_phase_in[where(rmm_amp_in le 0)]=!Values.F_NaN
rmm_amp_in[where(rmm_amp_in le 0)]=!Values.F_NaN
rmm_amp=fltarr(n_days)
rmm1=fltarr(n_days)
rmm2=fltarr(n_days)
rmm_phase=fltarr(n_days)
FOR i=0,n_years-1 DO BEGIN
   FOR j=0,n_days_per_year-1 DO BEGIN
      rmm1(i*n_days_per_year+j)=MEAN(rmm1_in(i,j*n_times_per_day:(j+1)*n_times_per_day-1),/NaN)
      rmm2(i*n_days_per_year+j)=MEAN(rmm2_in(i,j*n_times_per_day:(j+1)*n_times_per_day-1),/NaN)
      rmm_amp(i*n_days_per_year+j)=MEAN(rmm_amp_in(i,j*n_times_per_day:(j+1)*n_times_per_day-1),/NaN)
      rmm_phase(i*n_days_per_year+j)=rmm_phase_in(i,j*n_times_per_day)
   ENDFOR
ENDFOR

consecutive=0
max_events=20
n_events=0
n_days_before=20
n_days_after=50
n_days_per_event=n_days_before+n_days_after+1
allevents_rmm1=fltarr(max_events,n_days_per_event)
allevents_rmm2=fltarr(max_events,n_days_per_event)
event_start=intarr(max_events)
FOR i=0,n_days-1 DO BEGIN
   IF consecutive eq 0 THEN $
      this_run=[0]
   IF (rmm_amp(i) ge 1 and rmm_phase(i) eq 1) or $
      (rmm_amp(i) ge 1 and rmm_phase(i) eq 2) THEN BEGIN
      consecutive=consecutive+1
      this_run=[this_run,i]
   ENDIF ELSE BEGIN
      IF consecutive ge 8 THEN BEGIN
         print,'Phase 5/6 event > 8 days on days ...'
         print,STRTRIM(STRING(this_run(1:consecutive)),1)
         print,'------'         
         allevents_rmm1(n_events,*)=rmm1(this_run(1)-n_days_before:this_run(1)+n_days_after)
         allevents_rmm2(n_events,*)=rmm2(this_run(1)-n_days_before:this_run(1)+n_days_after)      
         event_start(n_events)=this_run(1)
         n_events=n_events+1
      ENDIF
      consecutive=0
   ENDELSE
ENDFOR
   
psfile='/home/ss901165/idl/hadgem3-kpp_runs/diurnal_cycle/hadgem3kpp_dcyc_search_suppressed.phases1+2_all_events_8days_pt1.ps'
   
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2000,SPACE3=700,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
       XSIZE=16000,YSIZE=16000,SPACE2=100
GSET,XMIN=-3.5,XMAX=3.5,YMIN=-3.5,YMAX=3.5,TITLE='Evolution of all phase 1+2 events >= 8 days in xjnoa'
GPLOT,X=REPLICATE(0,2),Y=[-3.5,-1],STYLE=0,THICK=80
GPLOT,X=REPLICATE(0,2),Y=[1,3.5],STYLE=0,THICK=80
GPLOT,X=[1,3.5],Y=REPLICATE(0,2),STYLE=0,THICK=80
GPLOT,X=[-1,-3.5],Y=REPLICATE(0,2),STYLE=0,THICK=80
GPLOT,X=[SQRT(2)/2.,1,3.5],Y=[SQRT(2)/2.,1,3.5],STYLE=0,THICK=80
GPLOT,X=[-SQRT(2)/2.,-1,-3.5],Y=[SQRT(2)/2.,1,3.5],STYLE=0,THICK=80
GPLOT,X=[-SQRT(2)/2.,-1,-3.5],Y=[-SQRT(2)/2.,-1,-3.5],STYLE=0,THICK=80
GPLOT,X=[SQRT(2)/2.,1,3.5],Y=[-SQRT(2)/2.,-1,-3.5],STYLE=0,THICK=80
points=(2*!PI/99.0)*findgen(100)
x=COS(points)
y=SIN(points)
white=FSC_COLOR("white",28)
GPLOT,X=x,Y=y,FILLCOL=28
CS,SCALE=28,NCOLS=n_events/2+1
;lowamp=where(SQRT(allevents_rmm1^2+allevents_rmm2^2) le 0.5)
;allevents_rmm1[lowamp]=!Values.F_NaN
;allevents_rmm2[lowamp]=!Values.F_NaN
FOR i=0,n_events/2-1 DO BEGIN
   GPLOT,X=allevents_rmm1(i,0:n_days_before-1),Y=allevents_rmm2(i,0:n_days_before-1),COL=i+2,THICK=125,SYM=3,SIZE=50
   GPLOT,X=allevents_rmm1(i,n_days_before-1:n_days_per_event-1),Y=allevents_rmm2(i,n_days_before-1:n_days_per_event-1),$
         THICK=125,SYM=6,SIZE=50,COL=i+2
ENDFOR
GPLOT,X=0,Y=-4.3,TEXT='Indian Ocean',ALIGN=0.5,CHARSIZE=100
GPLOT,X=4.0,Y=0,TEXT='Maritime Continent',ALIGN=0.5,CHARSIZE=100,ORIENTATION=90
GPLOT,X=0,Y=3.9,TEXT='Western Pacific',ALIGN=0.5,CHARSIZE=100
GPLOT,X=-4.3,Y=0,TEXT='Western Hemisphere and Africa',ALIGN=0.5,CHARSIZE=100,ORIENTATION=90
AXES,XSTEP=1,YSTEP=1,XMINOR=0.5,YMINOR=0.5,NDECS=2
GPLOT,X=0,Y=-4.1,TEXT='RMM1'
GPLOT,X=-4.0,Y=0,TEXT='RMM2',ORIENTATION=90
GLEGEND,labels=REVERSE('Starts day '+STRTRIM(STRING(event_start(0:n_events/2-1)),1)),COL=REVERSE(indgen(n_events/2)+2),$
        LEGYOFFSET=17000,LEGXOFFSET=7000
PSCLOSE

psfile='/home/ss901165/idl/hadgem3-kpp_runs/diurnal_cycle/hadgem3kpp_dcyc_search_suppressed.phases1+2_all_events_8days_pt2.ps'
   
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2000,SPACE3=700,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
       XSIZE=16000,YSIZE=16000,SPACE2=100
GSET,XMIN=-3.5,XMAX=3.5,YMIN=-3.5,YMAX=3.5,TITLE='Evolution of all phase 1+2 events >= 8 days in xjnoa'
GPLOT,X=REPLICATE(0,2),Y=[-3.5,-1],STYLE=0,THICK=80
GPLOT,X=REPLICATE(0,2),Y=[1,3.5],STYLE=0,THICK=80
GPLOT,X=[1,3.5],Y=REPLICATE(0,2),STYLE=0,THICK=80
GPLOT,X=[-1,-3.5],Y=REPLICATE(0,2),STYLE=0,THICK=80
GPLOT,X=[SQRT(2)/2.,1,3.5],Y=[SQRT(2)/2.,1,3.5],STYLE=0,THICK=80
GPLOT,X=[-SQRT(2)/2.,-1,-3.5],Y=[SQRT(2)/2.,1,3.5],STYLE=0,THICK=80
GPLOT,X=[-SQRT(2)/2.,-1,-3.5],Y=[-SQRT(2)/2.,-1,-3.5],STYLE=0,THICK=80
GPLOT,X=[SQRT(2)/2.,1,3.5],Y=[-SQRT(2)/2.,-1,-3.5],STYLE=0,THICK=80
points=(2*!PI/99.0)*findgen(100)
x=COS(points)
y=SIN(points)
white=FSC_COLOR("white",28)
GPLOT,X=x,Y=y,FILLCOL=28
CS,SCALE=28,NCOLS=n_events/2+2
;lowamp=where(SQRT(allevents_rmm1^2+allevents_rmm2^2) le 0.5)
;allevents_rmm1[lowamp]=!Values.F_NaN
;allevents_rmm2[lowamp]=!Values.F_NaN
FOR i=n_events/2,n_events-1 DO BEGIN
   GPLOT,X=allevents_rmm1(i,0:n_days_before-1),Y=allevents_rmm2(i,0:n_days_before-1),COL=(i-n_events/2)+2,THICK=125,SYM=3,SIZE=50
   GPLOT,X=allevents_rmm1(i,n_days_before-1:n_days_per_event-1),Y=allevents_rmm2(i,n_days_before-1:n_days_per_event-1),$
         THICK=125,SYM=6,SIZE=50,COL=(i-n_events/2)+2
ENDFOR
GPLOT,X=0,Y=-4.3,TEXT='Indian Ocean',ALIGN=0.5,CHARSIZE=100
GPLOT,X=4.0,Y=0,TEXT='Maritime Continent',ALIGN=0.5,CHARSIZE=100,ORIENTATION=90
GPLOT,X=0,Y=3.9,TEXT='Western Pacific',ALIGN=0.5,CHARSIZE=100
GPLOT,X=-4.3,Y=0,TEXT='Western Hemisphere and Africa',ALIGN=0.5,CHARSIZE=100,ORIENTATION=90
AXES,XSTEP=1,YSTEP=1,XMINOR=0.5,YMINOR=0.5,NDECS=2
GPLOT,X=0,Y=-4.1,TEXT='RMM1'
GPLOT,X=-4.0,Y=0,TEXT='RMM2',ORIENTATION=90
GLEGEND,labels=REVERSE('Starts day '+STRTRIM(STRING(event_start(n_events/2:n_events-1)),1)),COL=REVERSE(indgen(n_events/2+1)+1),$
        LEGYOFFSET=17000,LEGXOFFSET=7000
PSCLOSE

STOP
END
