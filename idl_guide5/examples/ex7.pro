PRO ex7
PSOPEN, XPLOTS=2, YPLOTS=2
LEVS, MIN=-32, MAX=32, STEP=4
f=SF('gdata.nc', 'temp', p=1000)
MAP
CON, F=f, TITLE='Jan 1987'

POS, XPOS=1, YPOS=2
MAP, /MOLLWEIDE
CON, F=f, TITLE='Jan 1987'

POS, XPOS=2, YPOS=1
MAP, /ROBINSON, LONMIN=0, LONMAX=360
CON, F=f, TITLE='Jan 1987'

POS, XPOS=2, YPOS=2
MAP, SATELLITE=[0, 50]
CON, F=f, TITLE='Jan 1987'
PSCLOSE
END

