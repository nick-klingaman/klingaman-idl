PRO ex32
d=NCREAD('gwinds.nc')
uv=SF('gwinds.nc', ['u', 'v'], hybrid_p_x1000=99.2468)
PSOPEN, XPLOTS=2
CS, SCALE=1
MAP, /NH
VECT, F=uv, MAG=10, MUNITS='ms!E-1!N'      
AXES, STEP=30

POS, XPOS=2
MAP, /NH
VECT, F=uv, MAG=10, MUNITS='ms!E-1!N', PTS=40
AXES, STEP=30
PSCLOSE
END


