FUNCTION t_test_onesample, x, assumed_mean, t_value

;+
; NAME:
;       ONE SAMPLE T-TEST
;
; PURPOSE:
;       Computes the probability that the given data sample has a mean
;       that is different from the assumed mean of the underlying
;       normal distribution.
;
; CALLING SEQUENCE:
;       result = T_TEST_ONESAMPLE(x,assumed_mean,t_value)
;
; INPUTS:
;       x            - the data sample to be tested (one-dimensional
;                      array).  Set any missing data to !Values.F_NaN
;                      so that they can be ignored.
;       assumed_mean - the assumed mean of the underlying normal
;                      distribution.
;       t_value      - the lowest T value that you are willing to
;                      tolerate.  All points with T values above this
;                      value will pass the T-test.  This value must be
;                      taken from a statistical table using your known
;                      number of degrees of freedom and the
;                      significance level for which you wish to test.
;
; OUTPUTS:
;       result       - A scalar that is equal to either 0
;                      (x did not pass the test) or 1 (x passed the
;                      test)
;
;                
; RESTRICTIONS:
;       None
;
; SIDE EFFECTS:
;       None
;
; COMMON BLOCKS:
;       None
;
; MODIFICATION HISTORY:
;       1.0 - Tested and working; cleanup comments (NPK) (23/10/06 Euro)
;       0.2 - Return an array of equal size as x, not just the
;             locations of points in x that passed the test (NPK)
;             (16/03/06 Euro).
;       0.1 - Written by Nicholas Klingaman (13/03/06 Euro)
;
;-

nx = N_ELEMENTS(x)
IF TOTAL(WHERE(FINITE(x) ne 1)) ne -1 THEN BEGIN
    missing = N_ELEMENTS(WHERE(FINITE(x) ne 1))
    IF missing gt nx-2 THEN BEGIN
        result = 0
        RETURN, result
    ENDIF ELSE nx = nx-missing
ENDIF

mean_x = MEAN(x, /NaN, /double)
stddev_x = STDDEV(x, /NaN, /double)

t_score = abs((mean_x - assumed_mean))/(stddev_x / SQRT(FLOAT(nx)))
;print,t_score

IF t_score ge t_value THEN BEGIN
    result = 1
ENDIF ELSE result = 0

RETURN, result

END
