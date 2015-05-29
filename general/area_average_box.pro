FUNCTION area_average_box, x, lon, lat, box, missing

;+
; NAME:
;       AREA AVERAGE BOX
;
; PURPOSE:
;       Take the area average (non-weighted) of an input variable 
;       over a specified box, ignoring missing values.
;
; CALLING SEQUENCE:
;       result = AREA_AVERAGE_BOX(x,lon,lat,box,missing)
;
; INPUTS:
;       x       - the variable to be area-averaged, dimensioned as (lon,lat)
;       lon     - a one-dimensional array of the longitude points in x
;       lat     - a one-dimensional array of the latitude points in x
;       box     - the box over which to take the average.  This is a
;                 four-element array: [start_lon,start_lat,stop_lon,stop_lat]
;       missing - the value you have assigned to missing data in the input dataset
;
; OUTPUTS:
;       result - the area-average (scalar) of variable x in the
;                specified box
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
;       1.0 - Tested and working; cleanup comments (NPK) (23/10/06 Euro)
;       0.1 - Written by Nicholas Klingaman (05/04/06 Euro)
;
;-

; Do some rudimentary error-checking
s = size(x)
IF s(0) ne 2 THEN print, 'AREA AVERAGE BOX : Input array must have 2 dimensions'
IF s(1) ne N_ELEMENTS(lon) THEN $
  print, 'AREA AVERAGE BOX : Longitude array is incorrect size'
IF s(2) ne N_ELEMENTS(lat) THEN $
  print, 'AREA AVERAGE BOX : Latitude array is incorrect size'

; Select only the box of interest.  This is a conservative process:
; only points inside the box or along its edges will be selected.
test_lon = lon-box(0)
good = where(test_lon ge 0)
start_lon = where(lon eq min(test_lon[good])+box(0))

test_lat = lat-box(1)
good = where(test_lat ge 0)
start_lat = where(lat eq min(test_lat[good])+box(1))

test_lon = box(2)-lon
good = where(test_lon ge 0)
stop_lon = where(lon eq box(2)-min(test_lon[good]))

test_lat = box(3)-lat
good = where(test_lat ge 0)
stop_lat = where(lat eq box(3)-min(test_lat[good]))

box_start_lon = MIN([start_lon,stop_lon])
box_stop_lon = MAX([start_lon,stop_lon])
box_start_lat = MIN([start_lat,stop_lat])
box_stop_lat = MAX([start_lat,stop_lat])

x_select = x(box_start_lon:box_stop_lon,box_start_lat:box_stop_lat)

; Get rid of missing data
good = where(x_select ne missing)
IF TOTAL(good) eq -1 THEN BEGIN
;    print, 'AREA AVERAGE BOX : All data in selected box are missing, returning missing value'
    result = missing
    RETURN,result
ENDIF

; Take area average
result = MEAN(x_select[good], /DOUBLE, /NaN)

RETURN, result
END
