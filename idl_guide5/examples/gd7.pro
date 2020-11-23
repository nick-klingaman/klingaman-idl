PRO gd7
d=NCREAD('ukmo_test.nc', VARS=['u', 'v'])
PSOPEN, YPLOTS=2
GSET, XMIN=-90, XMAX=90, YMIN=1000, YMAX=0
VECT, X=d.latitude_1, Y=d.hybrid_p_x1000, U=REFORM(d.u(0, *,*)), V=REFORM(d.v(0,*,*)), MAG=30
AXES, XSTEP=30, YSTEP=-200

POS, YPOS=2
GSET, XMIN=-90, XMAX=90, YMIN=1000, YMAX=0
VECT, X=d.latitude_1, Y=d.hybrid_p_x1000, U=REFORM(d.u(0, *,*)), V=REFORM(d.v(0,*,*)), SIZE=100, /NOLEGEND
AXES, XSTEP=30, YSTEP=-200

PSCLOSE
END
