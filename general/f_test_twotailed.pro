FUNCTION f_test_twotailed,x1,dof1,x2,dof2,sig_level,takevar=takevar

;+
;
;  NAME:
;        F TEST TWO-TAILED
;
;  AUTHOR:
;        Nicholas P. Klingaman
;        Dept. of Meteorology, Univ. of Reading
;
;  PURPOSE:
;        Conducts a two-tailed F-test on the sample variances of two
;        timeseries to determine if those timeseries came from the
;        same origin.
;
;  CALLING SEQUENCE:
;        result = F_TEST_TWOTAILED(x1,x2,sig_level)
;
;  INPUTS:
;        x1         - The first timeseries (1D array) or the variance of
;                     the timeseries (scalar)
;        dof1       - Degrees of freedom in the first timeseries
;        x2         - The second timeseries (1D array) or the variance of
;                     the timeseries (scalar)
;        dof2       - Degrees of freedom in the second timeseries
;        sig_level  - The significance level which you want to test
;                     (alpha; scalar).  Note that this value will be
;                     divided by two for a two-tailed test.
; OUTPUTS:
;        result     - Either zero (did not pass F-test) or one (passed
;                     F-test)
;
; RESTRICTIONS:
;        None
;
; SIDE EFFECTS:
;        None
;
; COMMON BLOCKS:
;        None
;
; MODIFICATION HISTORY:
;        0.1 - Initial version (NPK) (8/11/06 Euro)
;
;-

IF KEYWORD_SET(takevar) THEN BEGIN
    s1 = STDDEV(x1)
    s2 = STDDEV(x2)
ENDIF ELSE BEGIN
    s1 = x1
    s2 = x2
ENDELSE

f_statistic = (s1^2)/(s2^2)
high_critical_value = F_CVF(sig_level/2.,dof1,dof2)
low_critical_value = F_CVF(1.-(sig_level/2.),dof1,dof2)

result = fltarr(N_ELEMENTS(x1(*,0)),N_ELEMENTS(x1(0,*)))
FOR i=0,N_ELEMENTS(x1(*,0))-1 DO BEGIN
    FOR j=0,N_ELEMENTS(x1(0,*))-1 DO BEGIN        
        IF f_statistic(i,j) gt high_critical_value $
          or f_statistic (i,j) lt low_critical_value THEN BEGIN
            result(i,j) = 1
        ENDIF ELSE $
          result(i,j) = 0
    ENDFOR
ENDFOR

RETURN,result

END
