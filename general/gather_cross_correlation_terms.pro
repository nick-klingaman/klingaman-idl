PRO gather_cross_correlation_terms, x, y, y_lag, sum_x, sum_y, sum_x2, $
                                    sum_y2, sum_xylag, nx, ny
                                   
; This procedure should be called once per day.

; It is assumed that X is one value (i.e., a one-dimensional field at
; a single timestep) and the Y is a two-dimensional field of values
; (i.e., a two-dimensional field at a single timestep.)

; This routine accumulates the terms required for cross-correlation.
; It accepts a single timestep's worth of data for two fields, then
; checks to make sure none of the data are missing, then keeps a
; running sum of the terms required for the cross-correlation
; equation.

; Note that this procedure DOES NOT CALCULATE THE CROSS-CORRELATION.

; X and Y are the fields being cross-correlated.  These should be the
; fields at the CURRENT TIMESTEP (i.e., no lag should be applied to
; these fields.

; Y_LAG is the field with the lag applied.  It can be either a lead
; (negative lag) or a lag (positive lag).

ny1 = N_ELEMENTS(y(*,0))
ny2 = N_ELEMENTS(y(0,*))

; If X is missing everywhere, then check to see if Y is also missing
; everywhere.  If Y is present, then update the Y variables and
; RETURN.  If Y is also missing everywhere, then RETURN.

a = FINITE(x)
b = N_ELEMENTS(where(FINITE(y) eq 1))
c = N_ELEMENTS(where(FINITE(y_lag) eq 1))

IF (a eq 0) or (b eq 0) or (c eq 0) THEN BEGIN
;    print, '* Gather Cross Correlate Terms: Data Missing . . . Returning'
    RETURN
ENDIF ELSE BEGIN
    nx = nx+1
    ny = ny+1
    sum_x = sum_x + x
    sum_x2 = sum_x2 + x^2
    FOR i=0,ny1-1 DO BEGIN
        FOR j=0,ny2-1 DO BEGIN
            sum_y(i,j) = sum_y(i,j) + y(i,j)
            sum_y2(i,j) = sum_y2(i,j) + y(i,j)^2
            sum_xylag(i,j) = sum_xylag(i,j) + x * y_lag(i,j)
        ENDFOR
    ENDFOR
ENDELSE

END
