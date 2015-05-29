FUNCTION correlate_missing, x_input, y_input, missing

; This function calculates Pearson's R, dropping all missing values
; from the analysis.

; Required arguments:
;       X_INPUT :: the data to be correlated with Y.
;       Y_INPUT :: the data to be correlated with X.
;       MISSING :: the value of "missing" data in X and Y.

; At the moment this function only operates with one-dimensional
; vectors.  Sorry.

IF N_ELEMENTS(x_input) ne N_ELEMENTS(y_input) THEN BEGIN
    PRINT, 'Error : Two vectors input to correlate_missing must be of equal length'
    RETURN, x_input
ENDIF

; Scan x_input and y_input for missing values and flag.
x_good = where(x_input ne missing)
y_good = where(y_input ne missing)

; Get the intersection of these points
good = SETINTERSECTION(x_good,y_good)

; Place all non-missing values into new arrays
x = x_input[good]
y = y_input[good]
nx = N_ELEMENTS(x)
ny = N_ELEMENTS(y)
IF nx ne ny THEN BEGIN
    PRINT, 'Error : correlate_missing is malfunctioning . . . thinks X and Y are different sizes'
    RETURN, x_input
ENDIF

; Calculate correlation terms
x_mean = MEAN(x)
y_mean = MEAN(y)
x_dev = x - x_mean
y_dev = y - y_mean
xy = fltarr(nx)
x2 = fltarr(nx)
y2 = fltarr(ny)
FOR i=0,nx-1 DO BEGIN
    xy(i) = x_dev(i) * y_dev(i)
    x2(i) = x_dev(i)^2
    y2(i) = y_dev(i)^2
ENDFOR
sum_xy = TOTAL(xy)
sum_x2 = TOTAL(x2)
sum_y2 = TOTAL(y2)

r = sum_xy/(SQRT(sum_x2*sum_y2))

RETURN, r

END
