PRO ex42
d=NCREAD('three_axes.nc')
PSOPEN, YOFFSET=6000, /PORTRAIT
CS, SCALE=28
GSET, XMIN=-2.0, XMAX=0.0, YMIN=2200, YMAX=0 
GPLOT, X=d.temperature, Y=d.pressure
AXES, YTITLE='Pressure (decibar)', XTITLE='Temperature (Celsius)', $
      YSTEP=-200, NDECS=1
      
GSET, XMIN=34.5, XMAX=35, YMIN=2200, YMAX=0 
GPLOT, X=d.psal, Y=d.pressure, COL=2
AXES, /ONLYLOWER, OFFSET=2000, STEP=0.1, NDECS=1, COL=2, XTITLE='Salinity (psu)'

GSET, XMIN=27, XMAX=38, YMIN=2200, YMAX=0 
GPLOT, X=d.density, Y=d.pressure, COL=4
AXES, /ONLYLOWER, STEP=1, OFFSET=4000, COL=4, XTITLE='Density'

GLEGEND, LEGPOS=3, LABELS=['Temperature', 'Salinity', 'Density'],$
        COL=[1,2,4]
PSCLOSE
END

