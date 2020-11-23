PRO ex20
d=NCREAD('moll_all.nc')
step=(26700-3000)/35

PSOPEN, YOFFSET=2000
MAP, /MOLLWEIDE

;Find ocean and land points.
ocean=WHERE(d.lsm LE 0.0)
land=WHERE(d.lsm GE 1.0)

;Land temperature .
ptarr=d.temp
ptarr(ocean)=-5000.
LEVS, MANUAL=[-70,INDGEN(14)*5-30], /EXACT
CS, SCALE=1, NCOLS=15
CON, FIELD=ptarr, X=d.longitude, Y=d.latitude, /NOLINES, /NOCOLBAR, /NOMAP
COLBAR, COORDS=[3000+step, 2500, 3000+16*step, 3000], TITLE='Land temperature [degC]', /ALT

;Ocean temperature.
ptarr=d.temp
ptarr(land)=-5000.
LEVS, MIN=-3, MAX=30, STEP=3, /EXACT
CS, COLS=[204,230], NCOLS=11
CON, FIELD=ptarr, X=d.longitude, Y=d.latitude, /NOLINES, /NOCOLBAR, /NOMAP
COLBAR, COORDS=[3000+16*step, 2500, 3000+27*step, 3000], TITLE='Ocean temperature [degC]'

;Ice.
ptarr=d.ice
LEVS, MANUAL=[0.1, 1.0], /EXACT
CS, COLS=[399]
CON, FIELD=ptarr, X=d.longitude, Y=d.latitude, /NOLINES, /NOCOLBAR, /NOMAP
COLBAR, COORDS=[3000, 2500, 3000+step, 3000], /NOTEXT, TITLE='Ice'

;Clouds.
ptarr=d.cld
LEVS, MANUAL=findgen(9)/20+0.15, /EXACT
CS, COLS=[490, 462], NCOLS=8
CON, FIELD=ptarr, X=d.longitude, Y=d.latitude, /NOLINES, /NOCOLBAR, /NOMAP
COLBAR, COORDS=[3000+27*step, 2500, 3000+35*step,3000], TITLE='Cloud', /ALT, /NOTEXT

PSCLOSE

END


