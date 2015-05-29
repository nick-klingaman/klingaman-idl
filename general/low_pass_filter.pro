FUNCTION low_pass_filter,x,shortest_period

; This function accepts a one-dimensional array and performs a
; low-pass filter on it to eliminate all periods shorter (all
; frequencies higher) than the specified "shortest period."  It
; returns the filtered, one-dimensional array.

; Required arguments:
;   x :: the one-dimensional array to be filtered.
;
;   shortest_period :: the shortest period (in units of the interval
;   between the data points in x) that will be retained by the
;   low-pass filter.  For example, if x has a sampling frequency of 3
;   days and you want to retain all periods longer than 30 days, then
;   shortest_period should be 10, not 30.

nx = n_elements(x)

IF nx le shortest_period THEN BEGIN
    PRINT, 'Low_pass_filter : The number of elements in x cannot be smaller '+$
      'than the shortest period you wish to keep.'
    RETURN, x
ENDIF

f_low = 0.                           ; Frequencies higher than f_low will be passed.   
f_high = 1./FLOAT(shortest_period)   ; Frequencies lower than f_high will be passed.
a = 50.                              ; Gibbs wiggles in decibels

IF (nx-1)/2. lt 10. THEN BEGIN
    n_terms = 2
ENDIF ELSE IF (nx-1)/2. lt 100 THEN BEGIN
    n_terms = 20
ENDIF ELSE BEGIN
    n_terms = 40
ENDELSE

x = [REVERSE(x(0:n_terms*2-1)),x,REVERSE(x(nx-n_terms*2:nx-1))]

; Note that we must pass 2*f_high to DIGITAL_FILTER because
; DIGITAL_FILTER requires f_high as a factor of the Nyquist frequency,
; which is twice the sampling rate of the dataset.

coeff = DIGITAL_FILTER(f_low,2*f_high,a,n_terms)

result = CONVOL(x,coeff,/NaN)
nx_filter = N_ELEMENTS(result)

x_filter = result(n_terms*2:nx_filter-n_terms*2-1)

RETURN, x_filter

END
