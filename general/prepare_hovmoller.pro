FUNCTION prepare_hovmoller,x,lon,lat,trans_lon,trans_lat

;+
; NAME:
;       PREPARE HOVMOLLER
;
; PURPOSE:
;       Transform a three-dimensional input field into a
;       two-dimensional field, usually for use in a Hovmoller diagram.
;
; CALLING SEQUENCE:
;       result = PREPARE_HOVMOLLER(x,lon,lat,trans_lon,trans_lat)
;
; INPUTS:
;       x         - the three-dimensional input field, dimensioned as
;                   (time,longitude,latitude)
;       lon       - the longitude values that correspond to the
;                   longitude dimension of x
;       lat       - the latitude values that correspond to the latitude
;                   dimension of x
;       trans_lon - if trans_lon has one element, it is the longitude
;                   value to use for the longitude transect.  If it
;                   has two values, it is the range of longitudes over
;                   which the latitude transect will be plotted.
;       trans_lat - same rules as trans_lon, but for latitude values
;
; OUTPUTS:
;       result    - the two-dimensional field that can be used for a
;                   Hovmoller plot, dimensioned as either
;                   (time,latitude) for a longitude transect or
;                   (time,longitude) for a latitude transect
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
;       0.2 - Fixed bug in calculation for dimensions of "result"
;       0.1 - Written by Nicholas Klingaman (06/04/06 Euro)
;
;-

; Do some rudimentary error-checking
s = size(x)
IF s(0) ne 3 THEN $
  message, 'PREPARE HOVMOLLER : Input array must have 3 dimensions'
IF s(2) ne N_ELEMENTS(lon) THEN $
  message, 'PREPARE HOVMOLLER : Longitude array is incompatible with dimension 1 of input array'
IF s(3) ne N_ELEMENTS(lat) THEN $
  message, 'PREPARE HOVMOLLER : Latitude array is incompatible with dimension 2 of input array'
IF N_ELEMENTS(trans_lon) eq N_ELEMENTS(trans_lat) THEN BEGIN
  message, 'PREPARE HOVMOLLER : One (and only one) of trans_lat and trans_lat must have two elements'
ENDIF ELSE IF N_ELEMENTS(trans_lon ne 2) and N_ELEMENTS(trans_lat ne 2) THEN $
  message, 'PREPARE HOVMOLLER : One (and only one) of trans_lat and trans_lat must have two elements'

; Select the transect of interest.  This is a conservative process:
; only points within the bounds of the transect or at its edges will
; be selected.
lat_lon=0            ; 0 for a longitude transect (latitude vs. time),
                     ; 1 for a latitude transect (longitude vs time)
IF N_ELEMENTS(trans_lon) eq 2 THEN lat_lon = 1
CASE lat_lon OF
    0 : BEGIN                   ; We are doing a longitude transect
                                ; (time vs. latitude)
        lon_select = WHERE(ABS(lon-trans_lon) eq MIN(abs(lon-trans_lon)))
        lat_select_start = WHERE(lat eq MIN(lat(WHERE(lat-trans_lat(0) ge 0))))
        lat_select_stop = WHERE(lat eq MAX(lat(WHERE(trans_lat(1)-lat ge 0))))
        x_select = x(*,lon_select,MIN([lat_select_start,lat_select_stop]):$
                     MAX([lat_select_start,lat_select_stop]))
        result = fltarr(s(1),ABS(lat_select_stop(0)-lat_select_start(0))+1)
        FOR i=0,N_ELEMENTS(result(*,0))-1 DO $
            FOR j=0,N_ELEMENTS(result(0,*))-1 DO $
              result(i,j) = MEAN(x_select(i,*,j))
    END
    1 : BEGIN                   ; We are doing a latitude transect
                                ; (time vs. longitude)
        lat_select = WHERE(ABS(lat-trans_lat) eq MIN(abs(lat-trans_lat)))
        lon_select_start = WHERE(lon eq MIN(lon(WHERE(lon-trans_lon(0) ge 0))))
        lon_select_stop = WHERE(lon eq MAX(lon(WHERE(trans_lon(1)-lon ge 0))))
        x_select = x(*,MIN([lon_select_start,lon_select_stop]):$
                     MAX([lon_select_start,lon_select_stop]),lat_select)
        result = x_select
    END
ENDCASE

return,result
END
