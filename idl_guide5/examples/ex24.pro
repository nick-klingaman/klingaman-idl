PRO ex24
d=NCREAD('eddies.nc')
PSOPEN
LEVS, MIN=-4, MAX=28, STEP=1
CS, SCALE=26, NCOLS=36
MAP, LONMIN=-100, LONMAX=10, LATMIN=0, LATMAX=70, /HIRES, /ISOTROPIC
CON, F=d.temperature(*,*), X=d.longitude, Y=d.latitude, /NOLINES
PSCLOSE
END

