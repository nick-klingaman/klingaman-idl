PRO define_boundaries,box,lat,lon,box_tx,limit=limit

;+
;  NAME:
;        DEFINE BOUNDARIES
; 
;  PURPOSE:
;        This function returns the boundaries of the input latitude
;        and longitude arrays that match the input corners of a box.
;        This function is useful for determining the starting and
;        ending points for a read command, or for limiting the amount
;        of information retained from a global dataset.
;  
;  CALLING SEQUENCE:
;        result = DEFINE_BOUNDARIES(box,lat,lon)
;
;  INPUTS:
;        box    - the corners of the box from which to define the
;                 boundaries of lat and lon.  A one-dimensional array
;                 of [min_lat,min_lon,max_lat,max_lon]
;        lat    - a one-dimensional array of latitude values
;        lon    - a one-dimensional array of longitude values
;
;  OUTPUTS:
;        box_tx - a one-dimensional array containing the boundary
;                 positions in the lat and lon array; defined as
;                 [min_lat,min_lon,max_lat,max_lon].
;
;  KEYWORD PARAMETERS:
;        LIMIT  - do not return the full lat and lon arrays, but
;                 return only the portions of the arrays within the
;                 defined boundaries.
; 
;  PROCEDURE:
;        Find the nearest points in latitude and longitude to the
;        corners of the box specified.
;  
;  DEPENDENCIES:
;        Requires the function NEAREST.
;
;  SIDE EFFECTS:
;        If /LIMIT is specified, lat and lon will be overwritten in
;        the main procedure.
;
;  MODIFICATION HISTORY
;        0.1  - Written by Nicholas Klingaman (30/4/07 Euro)
;
;-

; Do some rudimentary error-checking
s = SIZE(box)
IF s(0) ne 1 THEN MESSAGE, 'Input array [box] must be one-dimensional'
IF N_ELEMENTS(box) ne 4 THEN MESSAGE, 'Input array [box] must have four elements: [min_lat,min_lon,max_lat,max_lon]'
s = SIZE(lat)
IF s(0) ne 1 THEN MESSAGE, 'Input array [lat] must be one-dimensional.'
s = SIZE(lon)
IF s(0) ne 1 THEN MESSAGE, 'Input array [lon] must be one-dimensional.'

; Find the minimum and maximum latitude and longitude points
min_lat = MIN([NEAREST(lat,box(0)),NEAREST(lat,box(2))])
;min_lon = MIN([NEAREST(lon,box(1)),NEAREST(lon,box(3))])
max_lat = MAX([NEAREST(lat,box(0)),NEAREST(lat,box(2))])
;max_lon = MAX([NEAREST(lon,box(1)),NEAREST(lon,box(3))])
;min_lat=NEAREST(lat,box(0))
min_lon=NEAREST(lon,box(1))
;max_lat=NEAREST(lat,box(2))
max_lon=NEAREST(lon,box(3))

; Package the result as box_tx
box_tx = [min_lat,min_lon,max_lat,max_lon]

; If we're limiting the returned lat and lon, do so.
IF KEYWORD_SET(limit) THEN BEGIN
   IF min_lon lt max_lon THEN BEGIN
      lon = lon(min_lon:max_lon)
   ENDIF ELSE BEGIN
      n_lon=(N_ELEMENTS(lon))
      n_lon_reduced=(N_ELEMENTS(lon)-min_lon)+max_lon+1
      temp_lon = fltarr(n_lon_reduced)
      temp_lon(0:n_lon-min_lon-1) = lon(min_lon:n_lon-1)
      temp_lon(n_lon-min_lon:n_lon_reduced-1) = lon(0:max_lon)
      lon=temp_lon
   ENDELSE
   lat = lat(min_lat:max_lat)
ENDIF

RETURN

END

