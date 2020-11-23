PRO ex54
d=NCREAD('rgp.nc')
PSOPEN
GSET, XMIN=0, XMAX=N_ELEMENTS(d.x)-1, YMIN=0, YMAX=N_ELEMENTS(d.y)-1 
RGAXES, XIN=d.x, YIN=d.y, XPOLE=160, YPOLE=30, /CONTINENTS
RGROT, XIN=0.0, YIN=51.5, XPOLE=160, YPOLE=30, XOUT=xout, YOUT=yout
lons=((d.x + 180) MOD 360)-180 
xpt=MAX(WHERE(xout GE lons))
xval=xpt+(xout-lons(xpt))/(lons(xpt+1)-lons(xpt))
lats=REVERSE(d.y)
ypt=MIN(WHERE(yout GE lats))
yval=ypt+(yout-lats(ypt))/(lats(ypt+1)-lats(ypt))
GPLOT, X=xval, Y=yval, SYM=1, COL=6
PSCLOSE
END

