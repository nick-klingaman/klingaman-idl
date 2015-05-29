function con_coordxy,coin,dvsize,axis,$
  data=data,normal=normal,raw=raw,$
  to_data=to_data,to_normal=to_normal,to_raw=to_raw,distance=distance
;
; NAME: con_coordxy
; CATEGORY: Ancillary for con_coords
; CALLING SEQUENCE: coout=con_coordxy(coin,dvsize,axis)
; INPUTS:
;   coin -- input coordinate
;   dvsize -- visible device size for the approprimate dimension
;   axis -- axis structure for the approximate dimension
; KEYWORD PARAMETERS: as for con_coords
; OUTPUTS: coout -- converted coordinate
;
author_name='$Author: pvwave $'
date_name='$Date: 1998/07/15 08:48:41 $'
version_name='$Revision: 1.3 $'
@comm_error

; No conversion
if (keyword_set(data) and keyword_set(to_data)) or $
  (keyword_set(normal) and keyword_set(to_normal)) or $
  (keyword_set(raw) and keyword_set(to_raw)) then return,coin
if keyword_set(distance) and axis.type ne 0 $
  then message,'/DISTANCE is valid only for linear axes'

; Convert to normal coordinates
if keyword_set(data) then begin
  if axis.type eq 0 then coout=coin else coout=alog10(coin)
  coout=axis.s(1)*coout
  if not keyword_set(distance) then coout=coout+axis.s(0)
endif else if keyword_set(raw) then coout=coin/dvsize $
else coout=coin
  
; Convert to desired coordinates
if keyword_set(to_data) then begin
  if not keyword_set(distance) then coout=coout-axis.s(0)
  coout=coout/axis.s(1)
  if axis.type ne 0 then coout=10^coout
endif else if keyword_set(to_raw) then coout=long(coout*dvsize)

return,coout
end

function con_coords,coin,data=data,normal=normal,raw=raw,$
  to_data=to_data,to_normal=to_normal,to_raw=to_raw,$
  distance=distance,x=x,y=y
;+
; NAME: CON_COORDS
; PURPOSE: Convert between coordinate systems
; CATEGORY: Support
; CALLING SEQUENCE: out_coordinate=con_coords(in_coordinate)
; INPUTS: in_coordinate -- (vector of) input coordinate(s), x by default,
;   assumed by default to be a data coordinate
; KEYWORD PARAMETERS:
;   /x -- coordinate to be converted is an x-coordinate (default)
;   /y -- coordinate to be converted is a y-coordinate
;   /data -- in_coordinate is a data coordinate (default)
;   /normal -- in_coordinate is a normal coordinate
;   /raw -- in_coordinate is a device coordinate
;   /to_data -- out_coordinate is a data coordinate (default)
;   /to_normal -- out_coordinate is a normal coordinate
;   /to_raw -- out_coordinate is a device coordinate
;   /distance -- coordinate is a distance, not a position. Only relevant if
;     data coordinates are involved, since device and normal have the same
;     origin. Not allowed if logarithmic axes are involved.
; OUTPUTS: out_coordinate -- output coordinate(s), converted as requested
; MODIFICATION HISTORY: J.M.Gregory 25.11.92
;-
; Year 2000 compliant: Yes
; Checked by Roger Milton on 15th July 1998

author_name='$Author: pvwave $'
date_name='$Date: 1998/07/15 08:48:41 $'
version_name='$Revision: 1.3 $'
@comm_error

; Check consistencies
if keyword_set(x) and keyword_set(y) $
  then message,'/X and /Y are incompatible'
if not keyword_set(y) then x=1
sum=(keyword_set(normal))+(keyword_set(raw))
if sum+(keyword_set(data)) gt 1 $
  then message,'Only one of /DATA /NORMAL /RAW is allowed'
if sum eq 0 then data=1
sum=(keyword_set(to_normal))+(keyword_set(to_raw))
if sum+(keyword_set(to_data)) gt 1 $
  then message,'Only one of /TO_DATA /TO_NORMAL /TO_RAW is allowed'
if sum eq 0 then to_data=1

print,!x

if keyword_set(x) $
  then return,con_coordxy(float(coin),!d.x_vsize,!x,$
  data=keyword_set(data),normal=keyword_set(normal),raw=keyword_set(raw),$
  to_data=keyword_set(to_data),to_normal=keyword_set(to_normal),$
  to_raw=keyword_set(to_raw),distance=distance) $
  else return,con_coordxy(float(coin),!d.y_vsize,!y,$
  data=keyword_set(data),normal=keyword_set(normal),raw=keyword_set(raw),$
  to_data=keyword_set(to_data),to_normal=keyword_set(to_normal),$
  to_raw=keyword_set(to_raw),distance=distance)
end
