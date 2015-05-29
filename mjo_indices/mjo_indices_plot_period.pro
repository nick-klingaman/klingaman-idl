PRO mjo_indices_plot_period

; Input file
input_file='/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2010.index_values.nc'

;start_year=2009
;stop_year=2009
;start_day=284 ; As Julian date
;stop_day=344  ; As Julian date
;date_range='11oct2009-10dec2009'

start_year=2009
stop_year=2010
start_day=345 ; As Julian date
stop_day=31   ; As Julian date
date_range='11dec2009-31jan2010'

start_time=DAYS_SINCE([1,1975],[start_day,start_year])
stop_time=DAYS_SINCE([1,1975],[stop_day,stop_year])
;start_time=365*(start_year-1975)+start_day
;stop_time=365*(stop_year-1975)+stop_day
n_days=stop_time-start_time+1

print,start_time,stop_time

;marks=[0,5,10,15,20,25,30,35,40,45,50,55,60]
;marks_text=['11/10','16/10','21/10','26/10','31/10','5/11','10/11','15/11','20/11','25/11','30/11','5/12','10/12']

marks=[0,5,10,15,20,25,30,35,40,45,50]
marks_text=['11/12','16/12','21/12','26/12','31/12','5/1','10/1','15/1','20/1','25/1','30/1']

rmm1_ts=OPEN_AND_EXTRACT(input_file,'rmm1_ts',$
                         offset=[start_time],count=[n_days])
rmm2_ts=OPEN_AND_EXTRACT(input_file,'rmm2_ts',$
                         offset=[start_time],count=[n_days])

psfile='/home/ss901165/idl/mjo_indices/mjo_indices_plot_period.'+date_range+'.ps'

PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE3=1500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
       XSIZE=17000,YSIZE=17000

ymin=-2.5
ymax=2.5
xmin=-2.5
xmax=2.5
GSET,XMIN=xmin,XMAX=xmax,YMIN=ymin,YMAX=ymax,TITLE='Wheeler and Hendon RMM phase diagram for '+date_range

GPLOT,X=[0,0],Y=[ymin,-1],STYLE=0,THICK=80
GPLOT,X=[0,0],Y=[1,ymax],STYLE=0,THICK=80
GPLOT,X=[1,xmax],Y=[0,0],STYLE=0,THICK=80
GPLOT,X=[-1,xmin],Y=[0,0],STYLE=0,THICK=80
GPLOT,X=[SQRT(2)/2.,xmax],Y=[SQRT(2)/2.,ymax],STYLE=0,THICK=80
GPLOT,X=[-SQRT(2)/2.,xmin],Y=[SQRT(2)/2.,ymax],STYLE=0,THICK=80
GPLOT,X=[-SQRT(2)/2.,xmin],Y=[-SQRT(2)/2.,ymin],STYLE=0,THICK=80
GPLOT,X=[SQRT(2)/2.,xmax],Y=[-SQRT(2)/2.,ymin],STYLE=0,THICK=80

GPLOT,X=0,Y=-2.8,TEXT='Indian Ocean',ALIGN=0.5,CHARSIZE=100
GPLOT,X=2.8,Y=0,TEXT='Maritime Continent',ALIGN=0.5,CHARSIZE=100,ORIENTATION=90
GPLOT,X=0,Y=2.8,TEXT='Western Pacific',ALIGN=0.5,CHARSIZE=100
GPLOT,X=-2.8,Y=0,TEXT='Western Hemisphere and Africa',ALIGN=0.5,CHARSIZE=100,ORIENTATION=90

points=(2*!PI/99.0)*findgen(100)
x=COS(points)
y=SIN(points)
GPLOT,X=x,Y=y,FILLCOL=28

GPLOT,X=rmm1_ts,Y=rmm2_ts
;GPLOT,X=rmm1_ts(0),Y=rmm2_ts(0),SYM=3

GPLOT,X=rmm1_ts,Y=rmm2_ts,SYM=3,/NOLINES,SIZE=50
GPLOT,X=rmm1_ts[marks],Y=rmm2_ts[marks],SYM=3,/NOLINES
GPLOT,X=rmm1_ts[marks],Y=rmm2_ts[marks]-0.2,TEXT=marks_text,/NOLINES,COL=FSC_COLOR('red')

;GPLOT,X=rmm1_ts(0)-0.0,Y=rmm2_ts(0)-0.4,TEXT=start_day_text
;GPLOT,X=rmm1_ts(mark1),Y=rmm2_ts(mark1),SYM=3
;GPLOT,X=rmm1_ts(mark1)-0.0,Y=rmm2_ts(mark1)-0.4,TEXT=mark1_text
;GPLOT,X=rmm1_ts(n_days-1),Y=rmm2_ts(n_days-1),SYM=3
;GPLOT,X=rmm1_ts(n_days-1)-0.0,Y=rmm2_ts(n_days-1)-0.4,TEXT=stop_day_text
;GPLOT,X=rmm1_ts(mark2),Y=rmm2_ts(mark2),SYM=3
;GPLOT,X=rmm1_ts(mark2)-0.0,Y=rmm2_ts(mark2)-0.4,TEXT=mark2_text
;FOR i=0,n_days-1,5 DO $
;  GPLOT,X=rmm1_ts(i),Y=rmm2_ts(i),SYM=4

AXES,XSTEP=1,YSTEP=1,XMINOR=0.5,YMINOR=0.5,XTITLE='RMM1',YTITLE='RMM2'

PSCLOSE

STOP
END

