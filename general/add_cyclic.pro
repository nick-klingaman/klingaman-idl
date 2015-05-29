PRO add_cyclic,x,longitudes, remove=remove

;+
; NAME: 
;       ADD CYCLIC
;
; AUTHOR:
;       Nicholas P. Klingaman
;       Dept. of Meteorology, Univ. of Reading
;
; PURPOSE :
;       Adds a new longitude point to an existing field so that global
;       contour plots do not have an annoying white line down the
;       Prime Meridian.
;
; CALLING SEQUENCE:
;       ADD_CYCLIC,x,longitudes
;
; INPUTS:
;       x          - the two-dimensional array of data to be modified
;                    (longitudes by latitudes)
;       longitudes - the one-dimensional array of longitudes to be
;                    modified
;
; OUTPUTS:
;       x          - the modified x array
;       longitudes - the modified longitudes array
;
; KEYWORD PARAMETERS:
;       remove     - remove the cyclic data point added by a previous
;                    call to ADD_CYCLIC
;
; RESTRICTIONS:
;       None
;
; SIDE EFFECTS:
;       Modifies x and longitudes directly.  Running this procedure
;       iteratively (i.e., within a loop) with the same longitudes
;       variable is silly.
;
; COMMON BLOCKS:
;       None
;
; MODIFICATION HISTORY;
;       0.1 - Initial version (NPK) (8/11/06 Euro)
;       0.2 - Added option to remove the cyclic data point (NPK) (20/7/07 Euro)
;-

n_lon = N_ELEMENTS(x(*,0))
n_lat = N_ELEMENTS(x(0,*))

IF KEYWORD_SET(remove) THEN BEGIN
    new_x = fltarr(n_lon-1,n_lat)
    new_longitudes = fltarr(n_lon-1)    
    new_x(*,*) = x(0:n_lon-2,*)
    new_longitudes(*) = longitudes(0:n_lon-2)
ENDIF ELSE BEGIN
    new_x = fltarr(n_lon+1,n_lat)
    new_longitudes = fltarr(n_lon+1)   
    new_x(0:n_lon-1,*) = x(*,*)
    new_x(n_lon,*) = x(0,*)
    new_longitudes = [longitudes,longitudes(0)]
ENDELSE

x = new_x
longitudes = new_longitudes

END
