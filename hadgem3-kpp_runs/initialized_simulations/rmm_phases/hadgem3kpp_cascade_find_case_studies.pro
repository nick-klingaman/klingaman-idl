PRO hadgem3kpp_cascade_find_case_studies

rmm_infile='/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2009.index_values.nc'
start_year_offset=25
n_years=10
n_days_per_year=366

; Get RMM1 and RMM2 from observations
rmm1=REFORM(OPEN_AND_EXTRACT(rmm_infile,'rmm1',$
                             offset=[start_year_offset,0],count=[n_years,n_days_per_year]))
rmm2=REFORM(OPEN_AND_EXTRACT(rmm_infile,'rmm2',$
                             offset=[start_year_offset,0],count=[n_years,n_days_per_year]))
phase=REFORM(OPEN_AND_EXTRACT(rmm_infile,'phase',$
                              offset=[start_year_offset,0],count=[n_years,n_days_per_year]))
amplitude=REFORM(OPEN_AND_EXTRACT(rmm_infile,'amplitude',$
                                  offset=[start_year_offset,0],count=[n_years,n_days_per_year]))

rmm1_ts=fltarr(n_years*n_days_per_year)
rmm2_ts=fltarr(n_years*n_days_per_year)
phase_ts=fltarr(n_years*n_days_per_year)
amplitude_ts=fltarr(n_years*n_days_per_year)

FOR i=0,n_years-1 DO BEGIN
   rmm1_ts(i*n_days_per_year:(i+1)*n_days_per_year-1)=rmm1(i,*)
   rmm2_ts(i*n_days_per_year:(i+1)*n_days_per_year-1)=rmm2(i,*)
   phase_ts(i*n_days_per_year:(i+1)*n_days_per_year-1)=phase(i,*)
   amplitude_ts(i*n_days_per_year:(i+1)*n_days_per_year-1)=amplitude(i,*)
ENDFOR

max_events=30
event_count=0
events_rmm1=fltarr(max_events,40)
events_rmm2=fltarr(max_events,40)
events_year=intarr(max_events)
events_day=intarr(max_events)
events_rmm1(*,*)=-9999
events_rmm2(*,*)=-9999
events_year(*)=-9999
events_day(*)=-9999
FOR i=10,n_years*n_days_per_year-31 DO BEGIN
;   IF TOTAL(where(amplitude_ts(i-5:i-1) ge 1)) eq -1 and TOTAL(where(amplitude_ts(i:i+10) le 1)) eq -1 THEN BEGIN
   IF TOTAL(where(amplitude_ts(i:i+30) le 1)) eq -1 THEN BEGIN
      IF phase_ts(i) eq 2 and TOTAL(where(phase_ts(i:i+29) eq 6)) ne -1 THEN BEGIN
         events_year(event_count)=i/366+start_year_offset+1975
         events_day(event_count)=(i MOD 366)
         print,'Found potential event at year '+STRTRIM(STRING(events_year(event_count)),1)+' day '+STRTRIM(STRING(events_day(event_count)),1),$
               ' amp=',amplitude_ts(i),' phase=',phase_ts(i),' rmm1=',rmm1_ts(i),' rmm2=',rmm2_ts(i)
         events_rmm1(event_count,*)=rmm1_ts(i-10:i+29)
         events_rmm2(event_count,*)=rmm2_ts(i-10:i+29)
         event_count=event_count+1
         i=i+30
      ENDIF
   ENDIF
ENDFOR

; Setup PostScript file

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/rmm_phases/hadgem3kpp_cascade_find_case_studies.2000-2009_phase2_next30_phase5.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1500,SPACE3=1500,XOFFSET=1500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,XSIZE=15000,YSIZE=15000
GSET,XMIN=-3,XMAX=3,YMIN=-3,YMAX=3
GPLOT,X=REPLICATE(0,2),Y=[-3,-1],STYLE=0,THICK=80
GPLOT,X=REPLICATE(0,2),Y=[1,3],STYLE=0,THICK=80
GPLOT,X=[1,3],Y=REPLICATE(0,2),STYLE=0,THICK=80
GPLOT,X=[-1,-3],Y=REPLICATE(0,2),STYLE=0,THICK=80
GPLOT,X=[SQRT(2)/2.,1,3],Y=[SQRT(2)/2.,1,3],STYLE=0,THICK=80
GPLOT,X=[-SQRT(2)/2.,-1,-3],Y=[SQRT(2)/2.,1,3],STYLE=0,THICK=80
GPLOT,X=[-SQRT(2)/2.,-1,-3],Y=[-SQRT(2)/2.,-1,-3],STYLE=0,THICK=80
GPLOT,X=[SQRT(2)/2.,1,3],Y=[-SQRT(2)/2.,-1,-3],STYLE=0,THICK=80

GPLOT,X=0,Y=-3.45,TEXT='Indian Ocean',ALIGN=0.5,CHARSIZE=100
GPLOT,X=3.35,Y=0,TEXT='Maritime Continent',ALIGN=0.5,CHARSIZE=100,ORIENTATION=90
GPLOT,X=0,Y=3.15,TEXT='Western Pacific',ALIGN=0.5,CHARSIZE=100
GPLOT,X=-3.65,Y=0,TEXT='Western Hemisphere and Africa',ALIGN=0.5,CHARSIZE=100,ORIENTATION=90
GPLOT,X=0,Y=-3.7,TEXT='RMM1'
GPLOT,X=-3.85,Y=0,TEXT='RMM2',ORIENTATION=90
points=(2*!PI/99.0)*findgen(100)
x=COS(points)
y=SIN(points)
white=FSC_COLOR("white",28)
GPLOT,X=x,Y=y,FILLCOL=28

GPLOT,X=-1.5,Y=-2.5,TEXT='Phase 2'
GPLOT,X=1.5,Y=-2.5,TEXT='Phase 3'
GPLOT,X=2.5,Y=-1.5,TEXT='Phase 4'
GPLOT,X=2.5,Y=1.5,TEXT='Phase 5'
GPLOT,X=1.5,Y=2.5,TEXT='Phase 6'
GPLOT,X=-1.5,Y=2.5,TEXT='Phase 7'
GPLOT,X=-2.5,Y=1.5,TEXT='Phase 8'
GPLOT,X=-2.5,Y=-1.5,TEXT='Phase 1'

AXES,XSTEP=1.0,YSTEP=1.0,XMINOR=0.25,YMINOR=0.25,NDECS=2
GPLOT,X=0,Y=0,TEXT='Weak MJO',ALIGN=0.5

colors=['blue','cyan','purple','orange','green','red','pink','brown','black','grey','maroon','turquoise','cornflower blue','dark red','gold',$
       'crimson','forest green','royal blue','olive','sienna']
FOR i=0,max_events-1 DO BEGIN
   IF events_rmm1(i,0) ne -9999 THEN BEGIN
      GPLOT,X=events_rmm1(i,0:10),Y=events_rmm2(i,0:10),THICK=100,COL=FSC_COLOR(colors(i)),STYLE=2
      GPLOT,X=events_rmm1(i,10:39),Y=events_rmm2(i,10:39),THICK=100,COL=FSC_COLOR(colors(i))
      GPLOT,X=events_rmm1(i,10),Y=events_rmm2(i,10),SYM=2,COL=FSC_COLOR(colors(i))
      GPLOT,X=events_rmm1(i,39),Y=events_rmm2(i,39),SYM=5,COL=FSC_COLOR(colors(i))
      gregorian_date=JULIAN_TO_GREGORIAN(events_day(i))
      GPLOT,X=4.5,Y=3.25,TEXT='Event start dates (diamonds): ',ALIGN=0.0
      GPLOT,X=4.5,Y=3.0-0.25*i,TEXT=STRTRIM(STRING(gregorian_date(0)),1)+$
            '/'+STRTRIM(STRING(gregorian_date(1)),1)+'/'+STRTRIM(STRING(events_year(i)),1),COL=FSC_COLOR(colors(i)),ALIGN=0.0
   ENDIF
ENDFOR

PSCLOSE

STOP
END

