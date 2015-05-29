FUNCTION julian_to_gregorian, julian, string=string, cal360=cal360, leapyear=leapyear, start_month=start_month

; This procedure converts a Julian date into a Gregorian day and month.
; Written by Nicholas Klingaman
; Last updated 14/9/06

; Arguments:
;               julian - a Julian date in the range 1 to 365
;               
; Returns: 
;               gregorian - a Gregorian date (element 0) and month
;                           (element 1) which corresponds the given
;                           Julian date.
;
; Options:
;               character   - return the Gregorian date as a string, use
;                             letters for the month (e.g., "1 Jun")
;               cal360      - use 360 day calendar, as in the UM
;               leapyear    - indicates that this year is a leap year
;               start_month - use a different start month than
;                             January. Specify the month as its number
;                             in the year (e.g., "2" for "February")

IF KEYWORD_SET(start_month) THEN BEGIN
   start_month=start_month
ENDIF ELSE $
   start_month=1

IF Keyword_Set(cal360) THEN BEGIN
    month_end_greg = [30,30,30,30,30,30,30,30,30,30,30,30]
    month_end_juli = [30,60,90,120,150,180,210,240,270,300,330,360]
ENDIF ELSE BEGIN      
    month_end_greg = [31,28,31,30,31,30,31,31,30,31,30,31]
    month_end_juli = [31,59,90,120,151,181,212,243,273,304,334,365]
    IF KEYWORD_SET(start_month) and start_month ne 1 THEN BEGIN
       temp=month_end_greg(start_month-1:11)
       temp2=month_end_greg(0:start_month-2)
       month_end_greg=[temp,temp2]
       month_end_juli(0)=month_end_greg(0)
       FOR i=1,11 DO $
          month_end_juli(i)=month_end_juli(i-1)+month_end_greg(i)
       ;print,month_end_greg,month_end_juli
    ENDIF
ENDELSE
month_string = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',$
                'Sep','Oct','Nov','Dec']

IF julian le month_end_juli(0) THEN BEGIN
    ; Well that was easy.
    gregorian = [julian, 1]
ENDIF ELSE BEGIN
    FOR i = 0,10 DO BEGIN
        IF julian gt month_end_juli(i) and $
          julian le month_end_juli(i+1) THEN BEGIN
            gregorian = [julian-month_end_juli(i),i+2]
        ENDIF        
    ENDFOR
ENDELSE

IF Keyword_Set(leapyear) THEN BEGIN
    IF julian gt 59 THEN BEGIN
        gregorian(0) = gregorian(0)-1
        IF gregorian(0) le 0 THEN BEGIN
            gregorian(1) = gregorian(1)-1
            gregorian(0) = month_end_greg(gregorian(1)-1)
        ENDIF
    ENDIF
    IF julian eq 60 THEN $
      gregorian = [29,2]
ENDIF

IF Keyword_Set(string) THEN $
  gregorian=strtrim(string(gregorian(0)),1)+' '+month_string(gregorian(1)-1)

RETURN,gregorian

END

