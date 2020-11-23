PRO ex1
PSOPEN 
CS, SCALE=1
MAP
LEVS, MIN=-32, MAX=32, STEP=4
CON, F=SF('gdata.nc', 'temp', p=1000), TITLE='Jan 1987', CB_TITLE='Temperature (Celsius)'
PSCLOSE
END

