FUNCTION hours_to_julian, hours, start

;+
; NAME:
;       Hours to Julian
;
; PURPOSE:
;       Convert a given number of hours from a given start time to a
;       Julian day and year.  This function was conceived because I
;       got really annoyed that a netCDF file for which I wanted to
;       write a read routine had its time dimension defined as "Hours
;       since 1/1/1 00:00:00"
;
; CALLING SEQUENCE:
;       result = HOURS_TO_JULIAN(hours, start)
;
; INPUTS:
;       hours - the number of hours from the given start time (see
;               below) that defines the current time.
;       start - a three-element array of the following (in order):
;               starting hour, starting date (Julian), and starting
;               year.
; 
; OUTPUTS:
;       The function returns the current time as a three-element array
;       of the following (in order): the current hour, the current
;       date (Julian), and the current year.
;
; PROCEDURE:
;       The function computes the current time as the starting time
;       plus the given number of hours from that starting time.
;
; RESTRICTIONS:
;       This function does not support Gregorian dates, but may be
;       used in conjunction with julian_to_gregorian and
;       gregorian_to_julian to convert as necessary.
;
; SIDE EFFECTS:
;       None
;
; COMMON BLOCKS:
;       None
;
; MODIFICATION HISTORY:
;       0.2 - Preliminary tests and error checking (NPK) (18/4/06 Euro)
;       0.1 - Written by Nicholas Klingaman (18/4/06 Euro)
;
;-

ON_ERROR,2

; Do some rudimentary error checking
IF N_ELEMENTS(start) ne 3 THEN $
  MESSAGE, 'Hours to Julian : Input variable "start" must have three elements.'
IF start(0) ge 24 or start(0) lt 0 THEN $
  MESSAGE, 'Hours to Julian : Starting hour is out of range (0-23).'
IF start(1) gt 366 or start(1) lt 1 THEN $
  MESSAGE, 'Hours to Julian : Starting date is out of range (1-366).'
IF hours lt 0 THEN $
  MESSAGE, 'Hours to Julian : Input variable "hours" cannot be negative.'

; Decompose the start array
current_hour = start(0)
current_date = start(1)
current_year = start(2)

; First, convert the given number of hours into whole days using
; integer division.
days = hours/24
temp_days = hours/24

; Now take what remains and save it to increment the hour later.
leftover_hours = hours MOD 24

; Figure out whether we need to add 365 or 366 days in order to get to
; the same date next year.
IF ((current_year MOD 4) eq 3 and current_date gt 59) $
  or ((current_year MOD 4) eq 0 and current_date le 59) THEN BEGIN
                                ; This is possibly a leap year, but we
                                ; don't know for sure yet.
    IF ((current_year MOD 400) eq 399 and current_date gt 59)  $
      or ((current_year MOD 400) eq 0 and current_date le 59) THEN BEGIN
                                ; This is definitely a leap year.
        full_year = 366
        print,current_year
    ENDIF ELSE IF ((current_year MOD 100) eq 99) or $
      (current_year MOD 100 eq 0) THEN BEGIN
                                ; This is definitely not a leap year.
        full_year = 365
    ENDIF ELSE BEGIN     
                                ; This is definitely a leap year.
        full_year = 366
        print,current_year
    ENDELSE
ENDIF ELSE full_year = 365  

; Take the remaining days and add them one full year at a time until
; we run out of years.
WHILE days gt full_year DO BEGIN
    current_year = current_year + 1
    days = days - full_year
                                ; Find the number of days we need to
                                ; add to get to the same point next
                                ; year.
    
    IF ((current_year MOD 4) eq 3 and current_date gt 59) $
      or ((current_year MOD 4) eq 0 and current_date le 59) THEN BEGIN
                                ; This is possibly a leap year, but we
                                ; don't know for sure yet.
        IF ((current_year MOD 400) eq 399 and current_date gt 59)  $
          or ((current_year MOD 400) eq 0 and current_date le 59) THEN BEGIN
                                ; This is definitely a leap year.
            full_year = 366
            print,current_year
        ENDIF ELSE IF ((current_year MOD 100) eq 99) or $
          (current_year MOD 100 eq 0) THEN BEGIN
                                ; This is definitely not a leap year.
            full_year = 365
        ENDIF ELSE BEGIN     
                                ; This is definitely a leap year.
            full_year = 366
            print,current_year
        ENDELSE
    ENDIF ELSE full_year = 365                          
ENDWHILE

; Now we no longer have any full years, it's safe to add the leftover
; hours.
current_hour = current_hour + leftover_hours
IF current_hour gt 23 THEN BEGIN
    current_hour = current_hour-24
    current_date = current_date+1
ENDIF

; Now it's safe to add the leftover days
current_date = current_date + days
IF current_date gt full_year THEN BEGIN
    current_date = current_date-full_year
    current_year = current_year+1
ENDIF

; Pack up the output array and exit.
output_array = intarr(3)
output_array(0) = current_hour
output_array(1) = current_date
output_array(2) = current_year

STOP

RETURN,output_array

END
