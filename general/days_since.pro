FUNCTION days_since, start_date, stop_date, noleap=noleap

; +
;  NAME:
;        DAYS SINCE
;
;  PURPOSE:
;        Calculate the number of days between two specified Julian dates and
;        years.  If only Gregorian dates are available, you can use the
;        gregorian_to_julian.pro procedure first.
;  
;  CALLING SEQUENCE:
;        result = DAYS_SINCE(start_date,stop_date)
;
;  INPUT
;        start_date - a two-element array containing [date, year] of
;                     the starting date.
;        stop_date  - a two-element array containing [date, year] of
;                     the ending date.
;
;  OUTPUTS:
;        result     - the number of days in between start_date and
;                     stop_date.
;
;  KEYWORDS:
;        noleap     - assume that all years have 365 days
;
;  RESTRICTIONS:
;        None.
;
;  SIDE EFFECTS:
;        None.
;
;  COMMON BLOCKS:
;        None.
;
;  MODIFICATION HISTORY:
;        0.1 - Written by Nicholas Klingaman (18/4/07)
;        0.2 - Added NOLEAP keyword (NPK 25/6/07)
; 
;-

; Do some rudimentary error-checking
s = N_ELEMENTS(start_date)
IF s ne 2 THEN MESSAGE, 'Input start_date must have 2 elements.'
s = N_ELEMENTS(stop_date)
IF s ne 2 THEN MESSAGE, 'Input stop_date must have 2 elements.'
IF stop_date(1) lt start_date(1) THEN MESSAGE, 'stop_date must be after start_date.'
IF stop_date(1) eq start_date(1) and stop_date(0) lt start_date(0) THEN MESSAGE, 'stop_date must be after start_date.'

; If the years are the same, calculate the time between the two
; dates.  
IF start_date(1) eq stop_date(1) THEN BEGIN
    result = LONG(stop_date(0) - start_date(0))
ENDIF ELSE BEGIN
    IF KEYWORD_SET(noleap) THEN BEGIN
        this_days_in_year = 365
    ENDIF ELSE BEGIN
                                ; Calculate the number of days in this year
        this_days_in_year = DAYS_IN_YEAR(start_date(1))
    ENDELSE
                                ; Calculate the time until the end of this year
    result = this_days_in_year - start_date(0)
                                ; Add the time in all intervening years until the last
    FOR i=1,stop_date(1)-start_date(1)-1 DO BEGIN
        IF KEYWORD_SET(noleap) THEN BEGIN
            result = LONG(result+365)
        ENDIF ELSE $
          result = LONG(result+DAYS_IN_YEAR(start_date(1)+i))
    ENDFOR 
                                ; Add the time in the last year
    result = LONG(result+stop_date(0))
ENDELSE

RETURN,result

STOP

END

