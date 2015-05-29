FUNCTION tukey_power_spectrum, x, mm

mean_x = MEAN(x)
n_time = N_ELEMENTS(x)
lfreq = FLOOR(n_time/2)

; Compute auto-covariance cofficients
cx = fltarr(n_time)
FOR i=-1,n_time-2 DO BEGIN
    FOR j=1,n_time-2-i DO $
      cx(i+1) = cx(i+1) + (x(j)-mean_x)*(x(i+j)-mean_x)
ENDFOR
cx = cx/FLOAT(n_time)

; Compute the spectrum with Tukey windowing
two_pi_n = 2*3.14159/FLOAT(n_time)
pi_rm = 3.14159/FLOAT(mm)
; Compute the weights for the Tukey window
w = 0.5*(1+cos(pi_rm*indgen(mm+1)))
ones = REPLICATE(1,lfreq)
tukSpec = w(1)*cx(1)*ones
FOR i=0,lfreq-1 DO BEGIN
    omega = two_pi_n*i
    FOR j=0,mm-1 DO BEGIN
        tukSpec(i) = tukSpec(i) + 2*w(j+1)*cx(j+1)*cos(omega*j)
    ENDFOR
ENDFOR
tukSpec = tukSpec/3.14159

RETURN,tukSpec

STOP

END
