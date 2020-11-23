PRO gd8
PSOPEN
d=NCREAD('ukmo_test.nc', VARS=['u', 'v', 'temp'])
tnew=REGRID(d.temp, d.longitude, d.latitude, d.longitude_1, d.latitude_1)
col=FLTARR(96, 72)+2
icol=2
FOR i=-32, 32,4 DO BEGIN
 pts=WHERE(tnew-273.15 GT i, count)
 IF (count GT 1 ) THEN col(pts)=icol
 icol=icol+1
ENDFOR
MAP
VECT, X=d.longitude_1, Y=d.latitude_1, U=d.u(*,*, 0), V=d.v(*,*,0), MAG=10, COL=col
AXES
LEVS, MIN=-32, MAX=32, STEP=4
COLBAR, COORDS=[3000, 1000, 26700, 1500]
PSCLOSE
END

