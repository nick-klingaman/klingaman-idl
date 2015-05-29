FUNCTION julian_to_hours, date, start

;+
; NAME:
;       Julian to Hours
;
; PURPOSE:
;       Convert a given Julian date and year to the number of hours
;       since a given starting date and year. This function was 
;       conceived because I got really annoyed that a netCDF file for 
;       which I wanted to write a read routine had its time dimension 
;       defined as "Hours since 1/1/1 00:00:00"
; 
; CALLING SEQUENCE:
;       result = JULIAN_TO_HOURS(date,start)
;
; INPUTS:
;       date  - a three-element array consisting of the following (in
;               order): the hour, date (Julian), and year that you wish
;               to convert
;       start - a three-element array consisting of the following (in
;               order): the hour, date (Julian), and year to use as
;               the starting point (i.e., "Hours since [start]")
;
; OUTPUTS:
;       The function returns the number of hours between the current
;       date and the starting date.
;
; PROCEDURE:
;       The function computes the number of hours as the difference
;       between the current time (represented by [date]) and the
;       starting point ([start]), accounting for leap years.
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
  MESSAGE, 'Julian to Hours : Input variable "start" must have three elements.'
IF N_ELEMENTS(date) ne 3 THEN $
  MESSAGE, 'Julian to Hours : Input variable "date" must have three elements.'
IF start(0) ge 24 or start(0) lt 0 THEN $
  MESSAGE, 'Julian to Hours : Starting hour is out of range (0-23).'
IF start(1) gt 366 or start(1) lt 1 THEN $
  MESSAGE, 'Julian to Hours : Starting date is out of range (1-366).'
IF date(0) ge 24 or date(0) lt 0 THEN $
  MESSAGE, 'Julian to Hours : Current hour is out of range (0-23).'
IF date(1) ge 366 or start(1) lt 1 THEN $
  MESSAGE, 'Julian to Hours : Current date is out of range (1-366).'
IF (date(2) lt start(2)) or (date(2) eq start(2) and date(1) lt start(1)) $
  or (date(2) eq start(2) and date(1) eq start(1) and date(0) lt start(0)) THEN $
  MESSAGE, 'Julian to Hours : Current date cannot be before starting date.'

; Decompose the start array
start_hour = start(0)
start_date = start(1)
start_year = start(2)

; Decompose the date array
current_hour = date(0)
current_date = date(1)
current_year = date(2)

CASE current_year-start_year OF
    0 : BEGIN
        days = current_date-start_date
    END
    1 : BEGIN
        days = current_date-1
        ; Current position: 1 January of the year after the starting year.
        ; Figure out if the starting year is a leap year.
        IF (start_year MOD 4 eq 0) THEN BEGIN
                                ; This may be a leap year, but we
                                ; can't be sure yet.
            IF (start_year MOD 400 eq 0) THEN BEGIN
                                ; This is definitely a leap year.
                full_year = 366
            ENDIF ELSE IF (start_year MOD 100 eq 0) THEN BEGIN               
                                ; This is definitely NOT a leap year.            
                full_year = 365
            ENDIF ELSE BEGIN
                                ; This is definitely a leap year.
                full_year = 366
            ENDELSE
        ENDIF ELSE BEGIN
                                ; This is definitely NOT a leap year.
            full_year = 365
        ENDELSE
        ; Add the days going back to the starting date, adding one because the
        ; current position is actually 1 January of the year after the
        ; starting year
        days = days+(full_year-start_date)+1
    END
    ELSE : BEGIN
        ; We work from the current date backwards to the starting date to get
        ; the total number of days in this period.  First, get the number of
        ; full days in the current year.
        days = FLOAT(current_date - 1)

        ; Current position: 1 January of the current year.
        ; Work backwards to the year after the start year, adding one full
        ; year's worth of days after appropriate leap year tests.
        FOR i=FLOAT(current_year-1),FLOAT(start_year+1),-1 DO BEGIN
            IF (i MOD 4 eq 0) THEN BEGIN
                                ; This may be a leap year, but we
                                ; can't be sure yet.
                IF (i MOD 400 eq 0) THEN BEGIN
                                ; This is definitely a leap year.
                    full_year = 366
                ENDIF ELSE IF (i MOD 100 eq 0) THEN BEGIN               
                                ; This is definitely NOT a leap year.            
                    full_year = 365
                ENDIF ELSE BEGIN
                                ; This is definitely a leap year.
                    full_year = 366
                ENDELSE
            ENDIF ELSE BEGIN
                                ; This is definitely NOT a leap year.
                full_year = 365
            ENDELSE
            days = days+full_year
        ENDFOR
        
        ; Current position: 1 January of the year after the starting year.
        ; Figure out if the starting year is a leap year.
        IF (start_year MOD 4 eq 0) THEN BEGIN
                                ; This may be a leap year, but we
                                ; can't be sure yet.
            IF (start_year MOD 400 eq 0) THEN BEGIN
                                ; This is definitely a leap year.
                full_year = 366
            ENDIF ELSE IF (start_year MOD 100 eq 0) THEN BEGIN               
                                ; This is definitely NOT a leap year.            
                full_year = 365
            ENDIF ELSE BEGIN
                                ; This is definitely a leap year.
                full_year = 366
            ENDELSE
        ENDIF ELSE BEGIN
                                ; This is definitely NOT a leap year.
            full_year = 365
        ENDELSE
        
        ; Add the days going back to the starting date, adding one because the
        ; current position is actually 1 January of the year after the
        ; starting year
        days = days+(full_year-start_date)+1
    END
ENDCASE
        
; Convert days to hours.
hours = days*24.

; Add any hours between the current date and the starting date
hours = hours+(current_hour-start_hour)

RETURN, hours

END
