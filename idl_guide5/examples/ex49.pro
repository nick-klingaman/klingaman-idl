PRO ex49
hzero=NCREAD('deltaTT2_amjjason.nc',VARS=['deltaTT','stdev'], /NOMOD)
h1000=NCREAD('deltaTT2_amjjason_wrtzero.nc', VARS=['deltaTT','stdev','signif'], /NOMOD)

PSOPEN, YPLOTS=2, /PORTRAIT, THICK=200, CHARSIZE=140
months=['Apr-1','May-1','Jun-1','Jul-1','Aug-1','Sep-1','Oct-1','Nov-1','Dec-1']
xvals=[0,30,60,90,120,150,180,210,240]
CS, COLS=[370, 125, 36]
time=INDGEN(240)
xpts=time
POS, YPOS=2
GSET, XMIN=0, XMAX=240, YMIN=-5, YMAX=4

ypts1=h1000.deltaTT-h1000.stdev
ypts2=h1000.deltaTT+h1000.stdev
FOR i=0, 238, 4 DO BEGIN
 GPLOT, X=[xpts(i), xpts(i)], Y=[ypts1(i), ypts2(i)], COL=3, THICK=250
ENDFOR

ypts1=hzero.deltaTT-hzero.stdev
ypts2=hzero.deltaTT+hzero.stdev
FOR i=2, 238, 4 DO BEGIN
 GPLOT, X=[xpts(i), xpts(i)], Y=[ypts1(i), ypts2(i)], COL=4, THICK=250
ENDFOR

GPLOT, X=[0,240], Y=[0,0], COL=1, THICK=50

GLEGEND, LEGPOS=7, COL=[3,4], LABELS=['HimTP1000','HimTPzero'], THICK=[250,250]
AXES, XVALS=xvals, XLABELS=months, YTITLE='tropospheric temperature gradient (!Eo!NC)', YSTEP=1
signif=REFORM(h1000.signif(2,*))
signif(WHERE(signif EQ 0))=!values.f_nan
signif(WHERE(signif EQ 1))=-5
GPLOT, X=time, Y=signif, COL=2, THICK=200

PSCLOSE
END
