PRO gd11
PSOPEN, XPLOTS=2, YPLOTS=2
d=NCREAD('gdata.nc')
lons_new=INDGEN(361)-180
lats_new=INDGEN(181)-90
temp_new=REGRID(d.temp(*,*,0), d.lon, d.lat, lons_new, lats_new)
CS, SCALE=1, NCOLS=22
LEVS, MIN=12, MAX=32, STEP=1
MAP, LONMIN=110, LONMAX=160, LATMIN=-40, LATMAX=-10
CON, F=d.temp(*,*,0), X=d.lon, Y=d.lat, TITLE='Grid spacing=2.5 degree'

POS, XPOS=2
MAP, LONMIN=110, LONMAX=160, LATMIN=-40, LATMAX=-10
CON, F=temp_new, X=lons_new, Y=lats_new, TITLE='Grid spacing=1.0 degree'
PSCLOSE
END

