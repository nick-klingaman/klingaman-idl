FUNCTION div_latlon, u, v, lat, lon

ON_ERROR,2

IF N_PARAMS(0) NE 4 THEN $
  MESSAGE, 'Div LatLon : Need 4 input parameters, got '+STRING(strtrim(N_PARAMS(0),1))

nv = SIZE(v)
ny = nv(2)
nx = nv(1)

IF ny NE N_ELEMENTS(u(0,*)) OR nx NE N_ELEMENTS(u(*,0)) THEN BEGIN
    print,ny,nx,N_ELEMENTS(u(0,*)),N_ELEMENTS(u(*,0))
    MESSAGE, 'Div LatLon : The input fields must have an equal number of elements in each dimension.'
ENDIF

dvdy = fltarr(nx,ny)
dudx = fltarr(nx,ny)

dvdy = SHIFT(v,0,-1) - SHIFT(v,0,1)
dvdy(0,*) = -3.*v(0,*) + 4.*v(1,*) - v(2,*)
dvdy(nx-1,0) = 3.*v(nx-1,*) - 4.*v(nx-2,*) + v(nx-3,*)
dvdy = TEMPORARY(dvdy)/(111325.*(ABS(lat(2)-lat(1))))

dudx = SHIFT(u,-1,0) - SHIFT(u,1,0)
dudx(*,0) = -3.*u(*,0) + 4.*u(*,1) - u(*,2)
dudx(0,ny-1) = -(3.*u(*,ny-1) - 4.*u(*,ny-1) + u(*,ny-3))

FOR i=0,N_ELEMENTS(lat)-1 DO BEGIN
    dudx(*,i) = dudx(*,i)/(111325.*ABS(cos(lat(i)))*(ABS(lon(2)-lon(1))))
ENDFOR

div = dudx + dvdy
div = TEMPORARY(div)/2.

; STOP

RETURN,div

END
