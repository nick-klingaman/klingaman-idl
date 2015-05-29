PRO cross_correlate_v3, sum_x, sum_y, sum_x2, sum_y2, sum_xylag, $
                          nx, ny, stddev_x, stddev_y, result

; It is assumed that all the X variables are scalar and that all the Y
; variables are two-dimensional.

; This routine accepts the accumulated terms from
; gather_cross_correlation_terms and calculates the cross correlation
; at every point in the two-dimensional Y field.

ny1 = N_ELEMENTS(sum_y(*,0))
ny2 = N_ELEMENTS(sum_y(0,*))

mean_x = FLOAT(sum_x)/FLOAT(nx)
denomenator_x = sum_x2 - sum_x^2/FLOAT(nx)
stddev_x = SQRT((sum_x2 - FLOAT(nx)*(mean_x)^2)/FLOAT(nx-1))

mean_y = fltarr(ny1,ny2)

FOR i=0,ny1-1 DO BEGIN
    FOR j=0,ny2-1 DO BEGIN
        mean_y = FLOAT(sum_y(i,j))/FLOAT(ny)
        IF sum_y2(i,j) eq 0 THEN sum_y2(i,j) = 0.00000001
	numerator = sum_xylag(i,j) - mean_x*mean_y
	denomenator_y = sum_y2(i,j) - sum_y(i,j)^2/FLOAT(ny)
	denomenator = SQRT(denomenator_x*denomenator_y)
        stddev_y(i,j) = SQRT((sum_y2(i,j) - FLOAT(ny)*(mean_y)^2)/FLOAT(ny-1))
	result(i,j) = numerator / denomenator
    ENDFOR
ENDFOR

END
