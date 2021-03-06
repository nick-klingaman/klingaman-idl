PRO ex23
PSOPEN, /PORTRAIT, XSIZE=15000, YSIZE=15000;, YOFFSET=9000
MAP, LONMIN=-6, LONMAX=2, LATMIN=50, LATMAX=56, /HIRES, /ISOTROPIC
CS, COLS=[453, 534], NCOLS=10
LEVS, MIN=200, MAX=1400, STEP=200
d=NCREAD('ukterrain.nc')
CON, F=d.hgt, X=d.lon, Y=d.lat, /NOLINES, /NOAXES, /NOCOLBAR

CS, COLS=[98, 174, 224, 252, 329, 370, 27, 453]
LEVS, MANUAL=['0.25','0.5','1','2','4','8','16','32'], /UPPER
CON, F=SF('rainfall_rate.nc', 'rrate'), /NOLINES, $
     /CB_RIGHT, CB_TITLE='mm/hr', TITLE='15/06/2009-1500'

GPLOT, X=-1.92, Y=52.5, SYM=13, SIZE=100,COL=1
GPLOT, X=-1.77, Y=52.5, TEXT="Birmingham", ALIGN=0.0, VALIGN=0.5
GPLOT, X=0.17, Y=51.5, SYM=13, SIZE=100, COL=1
GPLOT, X=0.0, Y=51.5, TEXT="London", ALIGN=1.0, VALIGN=0.5
GPLOT, X=-2.24, Y=53.48, SYM=13, SIZE=100, COL=1
GPLOT, X=-2.09, Y=53.48, TEXT="Manchester", ALIGN=0.0, VALIGN=0.5
GPLOT, X=-3.0, Y=53.25, SYM=13, SIZE=100, COL=1
GPLOT, X=-2.85, Y=53.25, TEXT="Liverpool", ALIGN=0.0, VALIGN=0.5
PSCLOSE
END

