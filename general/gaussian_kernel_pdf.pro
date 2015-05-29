FUNCTION gaussian_kernel_pdf, x, y, xaxis, yaxis, missing

; Calculates the 2D Gaussian kernel estimate of the PDF of x and y

x = x
y = y
xaxis = xaxis
yaxis = yaxis

help,x

my_x = x[where(x ne missing)]
my_y = y[where(y ne missing)]

my_x = my_x[where(FINITE(my_x) eq 1)]
my_y = my_y[where(FINITE(my_y) eq 1)]

; Statistics on the input data
mean_x = MEAN(my_x)
mean_y = MEAN(my_y)
std_x = STDDEV(my_x)
std_y = STDDEV(my_y)
nx = N_ELEMENTS(my_x)
n = nx

; Standardize the data and the axes to have a mean of 0 and a standard
; deviation of 1.
my_x = (my_x-mean_x)/std_x
my_y = (my_y-mean_y)/std_y
my_xaxis = (xaxis-mean_x)/std_x
my_yaxis = (yaxis-mean_y)/std_y

; Dimensions and standard and maximum window size
d = 2.
;h = (4./(2.*d+1))^(1./(d+4.))*nx^(-1./(d+4.))
h = (4/(2*d+1))^(1/(d+4))*n^(-1/(d+4))
hmax = 5*h

; Calculate constant for the summation
pdf = fltarr(N_ELEMENTS(xaxis),N_ELEMENTS(yaxis))
pdf(*,*) = 0.
scale_Pdf = 1/(n*h^d)*(2*3.14159)^(-d/2)/(std_x*std_y)
scale_exp = -0.5/h^d

; Loop over all of the observations
FOR i=0,nx-1 DO BEGIN
    ; Loop over xaxis and yaxis
    FOR j=0,N_ELEMENTS(xaxis)-1 DO BEGIN
        dx = my_xaxis(j)-my_x(i)
        IF abs(dx) lt hmax THEN BEGIN
            FOR k=0,N_ELEMENTS(yaxis)-1 DO BEGIN
                dy = my_yaxis(k)-my_y(i)
                IF abs(dy) lt hmax THEN $
                  pdf(j,k) = pdf(j,k) + exp(scale_exp*(dx*dx+dy*dy))
            ENDFOR
        ENDIF
    ENDFOR
ENDFOR

pdf = pdf*scale_pdf

; Check that the integral of the PDF equals 1.

pdf2 = fltarr(N_ELEMENTS(xaxis)-1,N_ELEMENTS(yaxis)-1)
FOR i=0,N_ELEMENTS(xaxis)-2 DO BEGIN
    FOR j=0,N_ELEMENTS(yaxis)-2 DO BEGIN
        pdf2(i,j) = 0.25*(pdf(i,j)+pdf(i+1,j)+pdf(i,j+1)+pdf(i+1,j+1))
    ENDFOR
ENDFOR

dx = xaxis(1:N_ELEMENTS(xaxis)-1)-xaxis(0:N_ELEMENTS(xaxis)-2)
dy = yaxis(1:N_ELEMENTS(yaxis)-1)-yaxis(0:N_ELEMENTS(yaxis)-2)

print,'Integral of pdf dx dy = ',dx*pdf2*dy,'*sdx*sdy'

RETURN,pdf

STOP

END
