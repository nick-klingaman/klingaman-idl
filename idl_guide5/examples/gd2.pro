PRO gd2
d=NCREAD('gdata.nc')
PSOPEN, XPLOTS=2, YPLOTS=2, MARGIN=2000
CS, SCALE=1

POS, YPOS=2
LEVS, MIN=-32, MAX=32, STEP=4
MAP
CON, FIELD=d.temp(*,*,0), X=d.lon, Y=d.lat, TITLE='Jan 1987', $
     CB_TITLE='Temperature (Celsius)', /NOLINES

POS, XPOS=2, YPOS=2
LEVS, MIN=-16, MAX=16, STEP=2
MAP
CON, FIELD=d.u(*,*,0), X=d.lon, Y=d.lat, TITLE='Jan 1987', $
     CB_TITLE='Zonal Wind (m/s)', /NOLINES

POS, XOFFSET=8600, YOFFSET=2000, XSIZE=11800, YSIZE=6900
LEVS, MIN=-8, MAX=8, STEP=1
MAP
CON, FIELD=d.v(*,*,0), X=d.lon, Y=d.lat, TITLE='Jan 1987', $
     CB_TITLE='Meridional Wind (m/s)', /NOLINES

PSCLOSE
END


