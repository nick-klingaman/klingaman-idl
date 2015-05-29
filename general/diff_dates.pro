FUNCTION diff_dates, start, stop

;+
; NAME:
;       Difference Dates
;
; PURPOSE:
;       Calculate the number of days between a given start and
;       stop.  Accounts for leap years.
;
; CALLING SEQUENCE:
;       result = DIFF_DATES(start,stop)
;
; INPUTS:
;       start - a two-element array consisting of the following
;                    (in order): the Julian date and the year of the starting
;                    date.
;       stop   - a two-element array consisting of the following
;                    (in order): the Julian date and the year of the ending
;                    date.
;
; OUTPUTS:
;       The function returns the number of days between the starting
;       date and the ending date.
;
; PROCEDURE:
;       The function computes the number of days between the starting
;       date and the ending date, accounting properly for all leap
;       years.
;
; RESTRICTIONS:
;       There is no adjustment for bizarre calendars (e.g., the
;       elimination of 11 days in the English calendar in 1752)
;
; SIDE EFFECTS:
;       None
;
; COMMON BLOCKS:
;       None
;
; MODIFICATION HISTORY:
;       0.1 - Written by Nicholas Klingaman (13/7/06 Euro)
;
;-

ON_ERROR,2

; Do some rudimentary error-checking
IF N_ELEMENTS(start) ne 2 THEN $
  MESSAGE, 'Diff Dates : Input variable "start" must have two elements.'
IF N_ELEMENTS(stop) ne 2 THEN $
  MESSAGE, 'Diff Dates : Input variable "stop" must have two elements.'
IF (start(0)/1000. + start(1)) gt (stop(0)/1000. + stop(1)) THEN $
  MESSAGE, 'Diff Dates : start may not be later than stop.'
IF (start(0)) gt 366 or (start(0)) le 0 THEN $
  MESSAGE, 'Diff Dates : Julian date in start is out of range (1-366)'
IF (stop(0)) gt  366 or (stop(0)) le 0 THEN $
  MESSAGE, 'Diff Dates : Julian date in stop is out of range (1-366)'

; Decompose the start and stop arrays
start_date = start(0)
start_year = start(1)
current_date = stop(0)
current_year = stop(1)

CASE current_year - start_year OF
    0 : BEGIN
        days = current_date-start_date
    END
    1 : BEGIN
        days = current_date-1
                                ; Current position: 1 January of the
                                ; year after the starting year.
                                ; Figure out of the starting year is a
                                ; leap year.
        IF (start_year MOD 4 eq 0) THEN BEGIN
                                ; This may be a leap year, but we
                                ; can't be sure yet.
            IF (start_year MOD 400 eq 0) THEN BEGIN
                                ; This is definitely a leap year
                full_year = 366
            ENDIF ELSE IF (start_year MOD 100 eq 0) THEN BEGIN
                                ; This is definitely NOT a leap year
                full_year = 365
            ENDIF ELSE BEGIN
                                ; This is definitely a leap year
                full_year = 366
            ENDELSE
        ENDIF ELSE BEGIN        ; This is definitely NOT a leap year
            full_year = 365
        ENDELSE
                                ; Add the days going back to the
                                ; starting date, adding one because
                                ; the current position is actually 1
                                ; January of the year after the
                                ; starting year
            days = days+(full_year-start_date)+1
        END
        ELSE : BEGIN
                                ; We work from the current date
                                ; backwards to the starting date to
                                ; get the total number of days in this
                                ; period.  First, get the number of
                                ; full days in the current year
            days = FLOAT(current_date-1)
                                ; Current position: 1 January of the
                                ; current year.
                                ; Work backwards to the year after the
                                ; start year, adding one full year's
                                ; worth of days after the appropriate
                                ; leap year tests.
            FOR i=FLOAT(current_year-1),FLOAT(start_year+1),-1 DO BEGIN
                IF (i MOD 4 eq 0) THEN BEGIN
                                ; This may be a leap year, but we
                                ; can't be sure yet.
                    IF (i MOD 400 eq 0) THEN BEGIN
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
                days = days+full_year
            ENDFOR
            
                                ; Current position: 1 January of the
                                ; year after the starting year.
                                ; Figure out if the starting year is a
                                ; leap year.
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
            
                                ; Add the days going back to the
                                ; starting date, adding one because the
                                ; current position is actually 1
                                ; January of the year after the
                                ; starting year.
            days = days+(full_year-start_date)+1
        END
    ENDCASE

RETURN, days

END
