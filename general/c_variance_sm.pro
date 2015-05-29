FUNCTION c_covariance_sm, x, y, xmean, ymean, lags

;+
;
; NAME:
;       C_COVARIANCE_SM
;
; PURPOSE:
;       Calculate the covariance of two timeseries using a specified
;       mean for each series.  This is useful when you want to
;       calculate the covariance of only a portion of timeseries, but
;       using the means from the entire timeseries.  One example of
;       this would be if you had timeseries containing
;       June-September data for a number of years.
;
; CALLING SEQUENCE:
;       result = C_VARIANCE_SM(x,y,xmean,ymean,lags)
;
; INPUTS:
;       x      - The timeseries to be "fixed" (i.e., not lead-lag).
;       y      - The timeseries to be lagged with the x timeseries.
;       xmean  - The mean of the COMPLETE x timeseries.
;       ymean  - The mean of the COMPLETE y timeseries.
;       lags   - An array of lags to be considered
;
; OUTPUTS:
;       result - An array of the same length as "lags" containing the
;                covariance of the two timeseries at each lag.
;
; RESTRICTIONS:
;       None.
; 
; SIDE EFFECTS:
;       None.
;
; COMMON BLOCKS:
;       None.
;
; REVISION HISTORY:
;       0.1 - Written by Nicholas Klingaman (19/4/07) 
;
;-

n_lags = N_ELEMENTS(lags)
c_covariance = fltarr(n_lags)

nx = N_ELEMENTS(x)
ny = N_ELEMENTS(y)
IF nx ne ny THEN MESSAGE, 'x and y input arrays must have the same length'

FOR i=0,n_lags-1 DO BEGIN
    this_lag = lags(i)
    IF this_lag lt 0 THEN BEGIN
        this_lag = ABS(this_lag)
        x_anom = x(this_lag:nx-1)-xmean
        y_anom = y(0:nx-this_lag-1)-ymean
    ENDIF ELSE BEGIN
        x_anom = x(0:nx-this_lag-1)-xmean
        y_anom = y(this_lag:nx-1)-ymean
    ENDELSE
    a = fltarr(N_ELEMENTS(x_anom))
    FOR j=0,N_ELEMENTS(x_anom)-1 DO $
        a(j)=x_anom(j)*y_anom(j)
    c_covariance(i) = TOTAL(a)*1./FLOAT(nx)
ENDFOR

RETURN,c_covariance

END
