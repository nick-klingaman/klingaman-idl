FUNCTION days_in_year, year

;+
;  NAME:
;        DAYS_IN_YEAR
;
;  PURPOSE:
;        Calculate the number of days in a given year.
;  
;  CALLING SEQUENCE:
;        result = DAYS_IN_YEAR(year)
;
;  INPUT
;        year   - an integer year greater than zero.
;
;  OUTPUTS:
;        result - the number of days in the given year.
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
; 
;-

; Do some rudimentary error checking
IF N_ELEMENTS(year) ne 1 THEN MESSAGE, 'Input year may have only one element.'
IF year le 0 THEN MESSAGE, 'Input year must be greater than zero.'
IF year MOD 1 ne 0 THEN MESSAGE, 'Input year must be an integer.'

result = 365

IF year MOD 4 eq 0 THEN BEGIN
    result = 366
    IF year MOD 100 eq 0 THEN $
      result = 365
    IF year MOD 400 eq 0 THEN $
      result = 366
ENDIF

RETURN,result

END

