PRO daily_to_monthly, daily, start_date, end_date, monthly

; This procedure converts daily data to monthly average data.  It is
; designed to make monthly climatologies from daily data.

; Arguments:
;             daily - the daily data to be converted
;             start_date - the date the data starts.  If start_date
;                          has two elements, it is assumed to contain
;                          a Gregorian date (element 0) and month
;                          (element 1).  If start_date
;                          has one element, it is assumed to contain a
;                          Julian date.
;             end_date - the date the data ends.  See start_date for format.
;             monthly - the monthly data

month_end_greg = [31,28,31,30,31,30,31,31,30,31,30,31]
n_days = n_elements(daily)
start_date_greg = intarr(2)
end_date_greg = intarr(2)

IF n_elements(start_date) eq 1 THEN BEGIN
    
    ; We are working in Julian dates.
    
    ; Find the starting date in Gregorian

    julian_to_gregorian,start_date,start_date_greg
    julian_to_gregorian,end_date,end_date_greg
    start_date_greg(1) = start_date_greg(1)-1
    end_date_greg(1) = end_date_greg(1)-1

ENDIF ELSE BEGIN

    start_date_greg = start_date
    end_date_greg = end_date

ENDELSE

; Find the average for the first month

IF start_date_greg(1) ne end_date_greg(1) THEN BEGIN
    n_days_tmp = month_end_greg(start_date_greg(1))-start_date_greg(0)+1
    month_avg = MEAN(daily(0:n_days_tmp-1))
    monthly(0:n_days_tmp-1) = REPLICATE(month_avg,n_days_tmp)
    start_marker = n_days_tmp
ENDIF ELSE BEGIN
    n_days_tmp = end_date_greg(0)-start_date_greg(0)+1
    month_avg = MEAN(daily(0:n_days_tmp-1))
    monthly(0:n_days_tmp-1) = REPLICATE(month_avg,n_days_tmp)
    RETURN
ENDELSE

; Find the average for the intermediate months

FOR i = start_date_greg(1)+1, end_date_greg(1)-1 DO BEGIN
    n_days_tmp = month_end_greg(i)
    stop_marker = start_marker + n_days_tmp-1
    month_avg = MEAN(daily(start_marker:stop_marker))
    monthly(start_marker:stop_marker) = REPLICATE(month_avg,n_days_tmp)
    start_marker = start_marker + n_days_tmp
ENDFOR

; Find the average for the last month

n_days_tmp = end_date_greg(0)
stop_marker = start_marker + n_days_tmp-1
month_avg = MEAN(daily(start_marker:stop_marker))
monthly(start_marker:stop_marker) = REPLICATE(month_avg,n_days_tmp)
start_marker = start_marker + n_days_tmp

END

    
    

    
