FUNCTION nearest_low,array,value

;+
; NAME:
;      NEAREST LOW
;
; PURPOSE:
;      Find the element in an input array that is nearest 
;      to a specified value without being greater than that value.  
;
; CALLING SEQUENCE:
;      result = NEAREST_LOW(array,value)
;
; INPUTS:
;      array - Input array to be searched
;      value - Value used as search key
;
; OUTPUTS:
;      result - The position in input array "array" that holds a value
;               closest to the specified value "value"
;
; RESTRICTIONS:
;      None.
;
; SIDE EFFECTS:
;      None.
; 
; COMMON BLOCKS:
;      None.
;
; MODIFICATION HISTORY:
;      0.3 - Branch from NEAREST
;      0.2 - Modifications for arrays of length > 32-bit integer size
;            (NPK 8/2/10 Euro)
;      0.1 - Written by Nicholas Klingaman (4/8/06 Euro)
;
;-

ON_ERROR,2

IF N_PARAMS(0) NE 2 THEN $
  MESSAGE,'NEAREST : Need exactly 2 input parameters, got '+STRING(STRTRIM(N_PARAMS(0),1))

array_diff = array - value
IF TOTAL(where(array_diff gt 0) ne -1) THEN $ 
   array_diff[where(array_diff gt 0)]=9999
array_diff_min = MIN(ABS(array_diff))
; Version 0.2 - specify 64-bit integer (where available)
result = WHERE(ABS(array_diff) eq array_diff_min,/L64)
IF N_ELEMENTS(result) gt 1 THEN BEGIN
   result = result[SORT(result)]
   result = result(0)
ENDIF
; Version 0.2 - return longword integer
result = FIX(TOTAL(result),TYPE=3)

RETURN,result
END
