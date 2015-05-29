PRO mjo_indices_wh04_plot_interannual

infile='/home/ss901165/um_output6/xihvd/rmm_indices.nc'
n_years=60
day=180
day_str='1Jul'

rmm1=REFORM(OPEN_AND_EXTRACT(infile,'rmm1',$
                             offset=[0,day],count=[n_years,1]))
rmm2=REFORM(OPEN_AND_EXTRACT(infile,'rmm2',$
                             offset=[0,day],count=[n_years,1]))

psfile='/home/ss901165/idl/mjo_indices/mjo_indices_wh04_plot_interannual.xihvd_'+day_str+'.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=160,MARGIN=2000,SPACE3=1500,XOFFSET=500,YOFFSET=500,TFONT=6,TCHARSIZE=120,CB_WIDTH=110,$
       XSIZE=16000,YSIZE=16000
GSET,XMIN=-3,XMAX=3,YMIN=-3,YMAX=3,TITLE='Position of the MJO every '+day_str+' in xihvd ('+STRTRIM(STRING(n_years),1)+'years)'
GPLOT,X=REPLICATE(0,2),Y=[-3,-1],STYLE=0,THICK=80
GPLOT,X=REPLICATE(0,2),Y=[1,3],STYLE=0,THICK=80
GPLOT,X=[1,3],Y=REPLICATE(0,2),STYLE=0,THICK=80
GPLOT,X=[-1,-3],Y=REPLICATE(0,2),STYLE=0,THICK=80
GPLOT,X=[SQRT(2)/2.,1,3],Y=[SQRT(2)/2.,1,3],STYLE=0,THICK=80
GPLOT,X=[-SQRT(2)/2.,-1,-3],Y=[SQRT(2)/2.,1,3],STYLE=0,THICK=80
GPLOT,X=[-SQRT(2)/2.,-1,-3],Y=[-SQRT(2)/2.,-1,-3],STYLE=0,THICK=80
GPLOT,X=[SQRT(2)/2.,1,3],Y=[-SQRT(2)/2.,-1,-3],STYLE=0,THICK=80
points=(2*!PI/99.0)*findgen(100)
x=COS(points)
y=SIN(points)
white=FSC_COLOR("white",28)
GPLOT,X=x,Y=y,FILLCOL=28
GPLOT,X=0,Y=-3.85,TEXT='Indian Ocean',ALIGN=0.5,CHARSIZE=100
GPLOT,X=3.5,Y=0,TEXT='Maritime Continent',ALIGN=0.5,CHARSIZE=100,ORIENTATION=90
GPLOT,X=0,Y=3.35,TEXT='Western Pacific',ALIGN=0.5,CHARSIZE=100
GPLOT,X=-3.6,Y=0,TEXT='Western Hemisphere and Africa',ALIGN=0.5,CHARSIZE=100,ORIENTATION=90

CS,SCALE=2,NCOLS=n_years+1
FOR i=0,n_years-1 DO $
   GPLOT,X=rmm1(i),Y=rmm2(i),TEXT=STRTRIM(STRING(i+1),1),COL=i+3
AXES,XSTEP=1,YSTEP=1,XMINOR=0.25,YMINOR=0.25

PSCLOSE

STOP
END
