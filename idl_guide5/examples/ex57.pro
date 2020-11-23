PRO ex57
d=NCREAD('wrf_test.nc')
PSOPEN
MAP, LONMIN=-60, LONMAX=20, LATMIN=30, LATMAX=70, /DRAW
newv=REGRID(d.v(*,*, 0, 0), d.xlong_v(*,0,0), d.xlat_v(0,*,0), d.xlong_u(*,0,0), d.xlat_u(0,*,0))
VECT, U=d.u(*,*, 0, 0), V=newv, X=d.xlong_u(*,0,0), Y=REFORM(d.xlat_u(0,*,0)), STRIDE=10, MAG=10
AXES
PSCLOSE
END
