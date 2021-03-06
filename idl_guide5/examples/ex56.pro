PRO ex56
d=NCREAD('wrf_test.nc')
PSOPEN
MAP, LONMIN=-60, LONMAX=20, LATMIN=30, LATMAX=70
GSET, XMIN=-60, XMAX=20, YMIN=30, YMAX=70
LEVS, MIN=250, MAX=290, STEP=2
CS, SCALE=1, NCOLS=28
CON, F=d.tmn(*,*),X=d.xlong(*,0), Y=REFORM(d.xlat(0,*)), TITLE='SOIL TEMPERATURE AT LOWER BOUNDARY'
AXES
PSCLOSE
END
