FUNCTION c_variance_sm, x, y, xmean, ymean, lags

;+
;
; NAME:
;       C_VARIANCE_SM
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
;       x     - The timeseries to be "fixed" (i.e., not lead-lag).
;       y     - The timeseries to be lagged with the x timeseries.
;       xmean - The mean of the COMPLETE x timeseries.
;       ymean - The mean of the COMPLETE y timeseries.
;       lags  - An array of lags to be considered
;
; OUTPUTS:
;       
;

