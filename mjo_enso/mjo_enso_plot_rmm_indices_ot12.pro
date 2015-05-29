PRO mjo_enso_plot_rmm_indices_ot12

; Plot RMM1/RMM2 traces for given period(s).

rmm_infile='/home/ss901165/datasets/MJO_INDICES/MJO_ihr1_ihr2.jan-dec_dmeans_ts.1905-2008.index_values.nc'
; rmm_infile='/home/ss901165/datasets/MJO_INDICES/
infile_start_year=1905

; What periods do you want to plot?  All periods will be shown on one plot, in different colours.
years=[2006]   
start_days=[182] ; Julian dates
stop_days=[212]  ; Julian dates

; Colours to use (for a list of colors available, see http://solarmuri.ssl.berkeley.edu/~welsch/public/software/ssw_stuff/fsc_color.pro)
colors=['blue','red','turquoise','violetred','orange','brown']

; Get RMM1 and RMM2 from observations
n_periods=N_ELEMENTS(years)
counts=stop_days-start_days+1
max_count=MAX(counts)
obs_rmm1=fltarr(n_periods,max_count)
obs_rmm2=fltarr(n_periods,max_count)

FOR i=0,n_periods-1 DO BEGIN
   
   this_start=DAYS_SINCE([1,infile_start_year],[start_days(i),years(i)])
   print,this_start
   
                                ; Change rmm1_ts and rmm2_ts to ihr1 and ihr2 for the Oliver and Thompson index
   obs_rmm1(i,0:counts(i)-1)=REFORM(OPEN_AND_EXTRACT(rmm_infile,'IHR1',$
                                                     offset=[this_start],count=[counts(i)]))
   obs_rmm2(i,0:counts(i)-1)=REFORM(OPEN_AND_EXTRACT(rmm_infile,'IHR2',$
                                                     offset=[this_start],count=[counts(i)]))
   IF counts(i) ne max_count THEN BEGIN
      obs_rmm1(i,counts(i):max_count-1)=!Values.F_NaN
      obs_rmm2(i,counts(i):max_count-1)=!Values.F_NaN
   ENDIF
ENDFOR

; Setup PostScript file
psfile='/home/ss901165/idl/mjo_enso/mjo_enso_plot_rmm_indices_ot12.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1500,SPACE3=1500,XOFFSET=1500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,XSIZE=15000,YSIZE=15000
GSET,XMIN=-3,XMAX=3,YMIN=-3,YMAX=3

; Draw the Wheeler and Hendon wheel - don't modify this bit.
GPLOT,X=REPLICATE(0,2),Y=[-3,-1],STYLE=0,THICK=60
GPLOT,X=REPLICATE(0,2),Y=[1,3],STYLE=0,THICK=60
GPLOT,X=[1,3],Y=REPLICATE(0,2),STYLE=0,THICK=60
GPLOT,X=[-1,-3],Y=REPLICATE(0,2),STYLE=0,THICK=60
GPLOT,X=[SQRT(2)/2.,1,3],Y=[SQRT(2)/2.,1,3],STYLE=0,THICK=60
GPLOT,X=[-SQRT(2)/2.,-1,-3],Y=[SQRT(2)/2.,1,3],STYLE=0,THICK=60
GPLOT,X=[-SQRT(2)/2.,-1,-3],Y=[-SQRT(2)/2.,-1,-3],STYLE=0,THICK=60
GPLOT,X=[SQRT(2)/2.,1,3],Y=[-SQRT(2)/2.,-1,-3],STYLE=0,THICK=60
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
; End drawing of the wheel

; Plot the traces for each period
FOR i=0,n_periods-1 DO BEGIN
   GPLOT,X=REFORM(obs_rmm1(i,*)),Y=REFORM(obs_rmm2(i,*)),STYLE=0,THICK=150,COL=FSC_COLOR(colors(i))
   FOR j=0,counts(i)-1,5 DO $    
      GPLOT,X=obs_rmm1(i,j),Y=obs_rmm2(i,j),/NOLINES,SYM=5,SIZE=100,COL=FSC_COLOR(colors(i))
   GPLOT,X=obs_rmm1(i,0),Y=obs_rmm2(i,0),SYM=3,SIZE=100,/NOLINES,COL=FSC_COLOR(colors(i))
   GPLOT,X=obs_rmm1(i,counts(i)-1),Y=obs_rmm2(i,counts(i)-1),SYM=1,SIZE=100,/NOLINES,COL=FSC_COLOR(colors(i))
ENDFOR

; Draw a legend to the right of the plot, using the years as labels
GLEGEND,labels=REVERSE(STRTRIM(STRING(years))),COL=REVERSE(FSC_COLOR(colors(0:n_periods-1))),LEGXOFFSET=5000,LEGYOFFSET=15000
PSCLOSE

STOP

END
