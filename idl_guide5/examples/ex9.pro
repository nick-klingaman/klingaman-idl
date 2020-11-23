PRO ex9
PSOPEN
CS, COLS=[95, 125, 93, 269, 274, 369, 411, 421, 409]	  
MAP, LONMIN=110, LONMAX=300, LATMIN=-50, LATMAX=40
LEVS, MANUAL=['-1', '-0.5', '0.5', '1', '1.5', '2', '2.5', '3']
CON, FIELD=SF('elnino.nc', 'sst'), TITLE='October SST Anomaly', $
     CB_TITLE='Temperature (Celsius)'
AXES, STEP=10
PSCLOSE
END
