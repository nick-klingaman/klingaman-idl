FUNCTION gregorian_to_julian, date, month, year

month_end_noleap = [31,28,31,30,31,30,31,31,30,31,30,31]
month_end_leap = [31,29,31,30,31,30,31,31,30,31,30,31]

IF (year MOD 4 eq 0) THEN BEGIN
   IF ((year MOD 100) eq 0) and ((year MOD 400) ne 0) THEN BEGIN
       month_end = month_end_noleap
   ENDIF ELSE $
     month_end = month_end_leap
ENDIF ELSE $
  month_end = month_end_noleap

julian = 0
IF month gt 1 THEN BEGIN
    FOR i=1,month-1 DO BEGIN
        julian = julian+month_end(i-1)
    ENDFOR
ENDIF

julian = julian+date

RETURN,julian

END
