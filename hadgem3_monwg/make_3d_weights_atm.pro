PRO make_3d_weights_atm

infile='/home/ss901165/test.nc'
longitude=OPEN_AND_EXTRACT(infile,'longitude')
latitude=OPEN_AND_EXTRACT(infile,'latitude')
z=OPEN_AND_EXTRACT(infile,'eta_hsp')
n_lon=N_ELEMENTS(

plevs=OPEN_AND_EXTRACT(infile,'data')
pstar=OPEN_AND_EXTRACT(infile,'p')

pdiff=fltarr(

STOP
END

