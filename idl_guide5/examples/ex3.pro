PRO ex3
PSOPEN 
CS, SCALE=33, NCOLS=12
MAP, /NH
LEVS, MIN=15150, MAX=16650, STEP=150
CON, F=SF('gdata.nc', 'ht', p=100), CB_TITLE='Height (m)', $
     TITLE='Jan 1987 - 100mb Geopotential Height'
PSCLOSE
END

