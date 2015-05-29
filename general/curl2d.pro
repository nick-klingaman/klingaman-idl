FUNCTION curl2d, u, v,dx,dy

;+
; NAME:
;       CURL2D
;
; PURPOSE:
;       Compute the two-dimensional curl of two two-dimensional input
;       fields.
; 
; CALLING SEQUENCE:
;       result = CURL2D(u,v)
;
; INPUTS:
;       u - X component of the two-dimensional input field
;       v - Y component of the two-dimensional input field
;
; OUTPUTS:
;       result - the curl (vorticity) of the two-dimensional field,
;                computed as D(V)/Dx - D(U)/Dy
;
; RESTRICTIONS:
;       None
;
; SIDE EFFECTS:
;       None
;
; COMMON BLOCKS:
;       None
;
; MODIFICATION HISTORY:
;       1.0 - Stable version (NPK) (21/04/06 Euro)
;       0.2 - Replace "PRINT" with "MESSAGE" for error statements
;             (NPK) (19/04/06 Euro)
;       0.1 - Written by Nicholas Klingaman (03/04/06 Euro)
;
;-

ON_ERROR,2

IF N_PARAMS(0) NE 4 THEN $
  MESSAGE,'CURL2D : Need 4 input parameters, got '+STRING(strtrim(N_PARAMS(0),1))

nv = SIZE(v)
ny = nv(2)
nx = nv(1)
IF nv(1) NE N_ELEMENTS(u(*,0)) OR nv(2) NE N_ELEMENTS(u(0,*)) THEN $
  MESSAGE,'CURL2D : The input fields must have an equal number of elements in each dimension'

curl = SHIFT(v,-1,0) - SHIFT(v,1,0)
curl(0,*) = -3.*v(0,*) + 4.*v(1,*) - v(2,*)
curl(nx-1,0) = 3.*v(nx-1,*) - 4.*v(nx-2,*) + v(nx-3,*)
FOR i=0,ny-1 DO BEGIN
    curl(*,i) = curl(*,i)/dx(i)
ENDFOR

curl = TEMPORARY(curl) - SHIFT(u,0,-1) + SHIFT(u,0,1)
curl(*,0) = - (-3.*u(*,0) + 4.*u(*,1) - u(*,2))
curl(0,ny-1) = -(3.*u(*,ny-1) - 4.*u(*,ny-1) + u(*,ny-3))
FOR i=0,ny-1 DO BEGIN
    curl(*,i) = curl(*,i)/dy(i)
ENDFOR

curl = TEMPORARY(curl)/2.

RETURN,curl

END
