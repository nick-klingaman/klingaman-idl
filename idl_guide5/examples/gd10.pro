PRO gd10
PSOPEN
d=NCREAD('gdata.nc')
sydneyt=REGRID(d.temp(*,*,0), d.lon, d.lat, 151.0, -34.0)
title='Sydney temperature is '+SCROP(sydneyt)
CS, SCALE=1, NCOLS=22
MAP, LONMIN=110, LONMAX=160, LATMIN=-40, LATMAX=-10
LEVS, MIN=12, MAX=32, STEP=1
CON, F=d.temp(*,*,0), X=d.lon, Y=d.lat, TITLE=title
GPLOT, X=151.0, Y=-34.0, SYM=3, COL=16
GPLOT,  X=152.0, Y=-34.0, TEXT='Sydney', ALIGN=0.0, VALIGN=0.5
PSCLOSE
END

