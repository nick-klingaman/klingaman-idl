PRO plot_globmean_ts_atm

vars=['field1560','field1562','field1563','temp','precip','olr','field201','field1523','field1397',$
     'wind','field1564']
ctl=NCREAD('/home/ss901165/um_output6/xhfad/xhfada.py.1851-4850.globalmean.nc',$
           vars=vars,/SILENT,/NOMOD)
exp=NCREAD('/home/ss901165/um_output6/xhhba/xhhbaa.py.1851-4850.globalmean.nc',$
           vars=vars,/SILENT,/NOMOD)

nyears_exp=3000
nyears_ctl=3000

nyears_smooth=51

ctl_smooth=SMOOTH(ctl.field1563,nyears_smooth)
exp_smooth=SMOOTH(exp.field1563,nyears_smooth)

ctl_ts=REFORM(ctl.field1563)
exp_ts=REFORM(exp.field1563)

labels=['CTL yearly','25*MIX yearly','CTL 21yr smoothed','25*MIX 21yr smoothed']

PSOPEN,file='/home/ss901165/idl/FAMOUS_co2flxatmos.ps',MARGIN=2000,XOFFSET=2000
CS,SCALE=1,NCOLS=12
GSET,XMIN=1850,XMAX=1850+nyears_exp,YMIN=-6.E-9,YMAX=3.E-9,TITLE='Global-mean flux of carbon to atmosphere'
GPLOT,X=1850+indgen(nyears_ctl),Y=ctl_ts,COL=2
GPLOT,X=1850+indgen(nyears_exp),Y=exp_ts,COL=10
GPLOT,X=1855+indgen(nyears_ctl-nyears_smooth),Y=ctl_smooth(nyears_smooth/2:nyears_ctl-(nyears_smooth/2+1)),COL=FSC_COLOR('cyan')
GPLOT,X=1855+indgen(nyears_exp-nyears_smooth),Y=exp_smooth(nyears_smooth/2:nyears_exp-(nyears_smooth/2+1)),COL=FSC_COLOR('red')
GPLOT,X=[1850,1850+nyears_exp],Y=[0,0],STYLE=2
GLEGEND,labels=REVERSE(labels),COL=REVERSE([2,10,FSC_COLOR('cyan'),FSC_COLOR('red')]),LEGPOS=11
AXES,XSTEP=200,XMINOR=100,YSTEP=5.E-10,XTITLE='Year',YTITLE='Carbon flux (kgC m!U-2!N s!U-1!N)'
PSCLOSE,/NOVIEW

ctl_smooth=SMOOTH(ctl.field1560,nyears_smooth)*1./0.7
exp_smooth=SMOOTH(exp.field1560,nyears_smooth)*1./0.7

ctl_ts=REFORM(ctl.field1560)*1./0.7
exp_ts=REFORM(exp.field1560)*1./0.7

labels=['CTL yearly','25*MIX yearly','CTL 21yr smoothed','25*MIX 21yr smoothed']

PSOPEN,file='/home/ss901165/idl/FAMOUS_co2flxocean.ps',MARGIN=2000,XOFFSET=2000
CS,SCALE=1,NCOLS=12
GSET,XMIN=1850,XMAX=1850+nyears_exp,YMIN=-4.E-9,YMAX=1.E-9,TITLE='Global-mean flux of carbon to ocean'
GPLOT,X=1850+indgen(nyears_ctl),Y=ctl_ts,COL=2
GPLOT,X=1850+indgen(nyears_exp),Y=exp_ts,COL=10
GPLOT,X=1855+indgen(nyears_ctl-nyears_smooth),Y=ctl_smooth(nyears_smooth/2:nyears_ctl-(nyears_smooth/2+1)),COL=FSC_COLOR('cyan')
GPLOT,X=1855+indgen(nyears_exp-nyears_smooth),Y=exp_smooth(nyears_smooth/2:nyears_exp-(nyears_smooth/2+1)),COL=FSC_COLOR('red')
GPLOT,X=[1850,1850+nyears_exp],Y=[0,0],STYLE=2
GLEGEND,labels=REVERSE(labels),COL=REVERSE([2,10,FSC_COLOR('cyan'),FSC_COLOR('red')]),LEGPOS=11
AXES,XSTEP=200,XMINOR=100,YSTEP=5.E-10,XTITLE='Year',YTITLE='Carbon flux (kgC m!U-2!N s!U-1!N)'
PSCLOSE,/NOVIEW

ctl_smooth=SMOOTH(ctl.field1562,nyears_smooth)
exp_smooth=SMOOTH(exp.field1562,nyears_smooth)

ctl_ts=REFORM(ctl.field1562)
exp_ts=REFORM(exp.field1562)

labels=['CTL yearly','25*MIX yearly','CTL 21yr smoothed','25*MIX 21yr smoothed']

PSOPEN,file='/home/ss901165/idl/FAMOUS_co2flxland.ps',MARGIN=2000,XOFFSET=2000
CS,SCALE=1,NCOLS=12
GSET,XMIN=1850,XMAX=1850+nyears_exp,YMIN=-3.E-8,YMAX=3.E-8,TITLE='Global-mean flux of carbon to land'
GPLOT,X=1850+indgen(nyears_ctl),Y=ctl_ts,COL=2
GPLOT,X=1850+indgen(nyears_exp),Y=exp_ts,COL=10
GPLOT,X=1855+indgen(nyears_ctl-nyears_smooth),Y=ctl_smooth(nyears_smooth/2:nyears_ctl-(nyears_smooth/2+1)),COL=FSC_COLOR('cyan')
GPLOT,X=1855+indgen(nyears_exp-nyears_smooth),Y=exp_smooth(nyears_smooth/2:nyears_exp-(nyears_smooth/2+1)),COL=FSC_COLOR('red')
GPLOT,X=[1850,1850+nyears_exp],Y=[0,0],STYLE=2
GLEGEND,labels=REVERSE(labels),COL=REVERSE([2,10,FSC_COLOR('cyan'),FSC_COLOR('red')]),LEGPOS=11
AXES,XSTEP=200,XMINOR=100,YSTEP=5.E-9,XTITLE='Year',YTITLE='Carbon flux (kgC m!U-2!N s!U-1!N)'
PSCLOSE,/NOVIEW

ctl_smooth=SMOOTH(REFORM(ctl.field1564(10,*)),nyears_smooth)*6.57E5
exp_smooth=SMOOTH(REFORM(exp.field1564(10,*)),nyears_smooth)*6.57E5

ctl_ts=REFORM(ctl.field1564(10,*))*6.57E5
exp_ts=REFORM(exp.field1564(10,*))*6.57E5

ctl_rate=fltarr(nyears_ctl)
exp_rate=fltarr(nyears_exp)

FOR i=50,nyears_ctl-51 DO $
   ctl_rate(i)=REGRESS(indgen(101),ctl_ts(i-50:i+50))
FOR i=50,nyears_exp-51 DO $
   exp_rate(i)=REGRESS(indgen(101),exp_ts(i-50:i+50))
ctl_rate(0:49)=!Values.F_NaN
ctl_rate(nyears_ctl-50:nyears_ctl-1)=!Values.F_NaN
exp_rate(0:49)=!Values.F_NaN
exp_rate(nyears_exp-50:nyears_exp-1)=!Values.F_NaN

labels=['CTL yearly','25*MIX yearly','CTL 21yr smoothed','25*MIX 21yr smoothed']

PSOPEN,file='/home/ss901165/idl/FAMOUS_co2atmosppm.ps',MARGIN=3000,XOFFSET=500
CS,SCALE=1,NCOLS=12
GSET,XMIN=1850,XMAX=1850+nyears_exp,YMIN=220,YMAX=900,TITLE='Global-mean atmospheric CO2'
GPLOT,X=1850+indgen(nyears_ctl),Y=ctl_ts,COL=2
GPLOT,X=1850+indgen(nyears_exp),Y=exp_ts,COL=10
GPLOT,X=1855+indgen(nyears_ctl-nyears_smooth),Y=ctl_smooth(nyears_smooth/2:nyears_ctl-(nyears_smooth/2+1)),COL=FSC_COLOR('cyan')
GPLOT,X=1855+indgen(nyears_exp-nyears_smooth),Y=exp_smooth(nyears_smooth/2:nyears_exp-(nyears_smooth/2+1)),COL=FSC_COLOR('red')
GPLOT,X=[1850,1850+nyears_exp],Y=[0,0],STYLE=2
AXES,XSTEP=200,YSTEP=50,YMINOR=25,XMINOR=100,XTITLE='Year',YTITLE='Carbon dioxide (ppm)',/NORIGHT

GLEGEND,labels=REVERSE([labels,'Rate of change-101yr centred']),$
                       COL=REVERSE([2,10,FSC_COLOR('cyan'),FSC_COLOR('red'),FSC_COLOR('black')]),$
                       LEGPOS=11
GSET,XMIN=1850,XMAX=1850+nyears_exp,YMIN=-0.6,YMAX=1.6
GPLOT,X=1850+indgen(nyears_ctl),Y=ctl_rate,COL=2,STYLE=2,THICK=200
GPLOT,X=1850+indgen(nyears_exp),Y=exp_rate,COL=10,STYLE=2,THICK=200
GPLOT,X=[1850,1850+nyears_exp],Y=[0,0],STYLE=2
AXES,XSTEP=200,XMINOR=100,YSTEP=0.1,YMINOR=0.05,$
     YTITLE='Rate of CO2 change (ppm yr!U-1!N) estimated from 101 years (centred)',/ONLYRIGHT,NDECS=2
PSCLOSE,/NOVIEW

ctl_ts=REFORM(ctl.temp)
exp_ts=REFORM(exp.temp)
ctl_smooth=SMOOTH(ctl_ts,nyears_smooth)
exp_smooth=SMOOTH(exp_ts,nyears_smooth)

ctl_rate=fltarr(nyears_ctl)
exp_rate=fltarr(nyears_exp)

FOR i=50,nyears_ctl-51 DO $
   ctl_rate(i)=REGRESS(indgen(101),ctl_ts(i-50:i+50))
FOR i=50,nyears_exp-51 DO $
   exp_rate(i)=REGRESS(indgen(101),exp_ts(i-50:i+50))
ctl_rate(0:49)=!Values.F_NaN
ctl_rate(nyears_ctl-50:nyears_ctl-1)=!Values.F_NaN
exp_rate(0:49)=!Values.F_NaN
exp_rate(nyears_exp-50:nyears_exp-1)=!Values.F_NaN

PSOPEN,file='/home/ss901165/idl/FAMOUS_surftemp.ps',MARGIN=3000,XOFFSET=0
CS,SCALE=1,NCOLS=12
GSET,XMIN=1850,XMAX=1850+nyears_exp,YMIN=284,YMAX=307,TITLE='Global-mean surface temperature'
GPLOT,X=1850+indgen(nyears_ctl),Y=ctl_ts,COL=2
GPLOT,X=1850+indgen(nyears_exp),Y=exp_ts,COL=10
GPLOT,X=1855+indgen(nyears_ctl-nyears_smooth),Y=ctl_smooth(nyears_smooth/2:nyears_ctl-(nyears_smooth/2+1)),COL=FSC_COLOR('cyan')
GPLOT,X=1855+indgen(nyears_exp-nyears_smooth),Y=exp_smooth(nyears_smooth/2:nyears_exp-(nyears_smooth/2+1)),COL=FSC_COLOR('red')
AXES,XSTEP=200,XMINOR=100,YSTEP=2,XTITLE='Year',YMINOR=0.5,YTITLE='Surface temperature (K)',/NORIGHT
GLEGEND,labels=REVERSE([labels,'Rate of change-101yr centred']),$
                       COL=REVERSE([2,10,FSC_COLOR('cyan'),FSC_COLOR('red'),FSC_COLOR('black')]),$
                       LEGPOS=11
GSET,XMIN=1850,XMAX=1850+nyears_exp,YMIN=-0.02,YMAX=0.056
GPLOT,X=1850+indgen(nyears_ctl),Y=ctl_rate,COL=2,STYLE=2,THICK=200
GPLOT,X=1850+indgen(nyears_exp),Y=exp_rate,COL=10,STYLE=2,THICK=200
GPLOT,X=[1850,1850+nyears_exp],Y=[0,0],STYLE=2
AXES,XSTEP=200,XMINOR=100,YSTEP=0.004,YMINOR=0.001,$
     YTITLE='Rate of temperature change (K yr!U-1!N) estimated from 101 yrs (centred)',/ONLYRIGHT,NDECS=3
PSCLOSE,/NOVIEW

ctl_ts=REFORM(ctl.field1397)
exp_ts=REFORM(exp.field1397)
ctl_smooth=SMOOTH(ctl_ts,nyears_smooth)
exp_smooth=SMOOTH(exp_ts,nyears_smooth)

PSOPEN,file='/home/ss901165/idl/FAMOUS_soilcarbon.ps',MARGIN=2000,XOFFSET=2000
CS,SCALE=1,NCOLS=12
GSET,XMIN=1850,XMAX=1850+nyears_exp,YMIN=3,YMAX=12.5,TITLE='Global-mean soil carbon'
GPLOT,X=1850+indgen(nyears_ctl),Y=ctl_ts,COL=2
GPLOT,X=1850+indgen(nyears_exp),Y=exp_ts,COL=10
GPLOT,X=1855+indgen(nyears_ctl-nyears_smooth),Y=ctl_smooth(nyears_smooth/2:nyears_ctl-(nyears_smooth/2+1)),COL=FSC_COLOR('cyan')
GPLOT,X=1855+indgen(nyears_exp-nyears_smooth),Y=exp_smooth(nyears_smooth/2:nyears_exp-(nyears_smooth/2+1)),COL=FSC_COLOR('red')
GPLOT,X=[1850,1850+nyears_exp],Y=[0,0],STYLE=2
GLEGEND,labels=REVERSE(labels),COL=REVERSE([2,10,FSC_COLOR('cyan'),FSC_COLOR('red')]),LEGPOS=9
AXES,XSTEP=200,XMINOR=100,YSTEP=0.5,XTITLE='Year',YMINOR=0.25,YTITLE='Soil carbon (kg C m!U-2!N)'
PSCLOSE,/NOVIEW

ctl_ts=REFORM(ctl.field1523)
exp_ts=REFORM(exp.field1523)
ctl_smooth=SMOOTH(ctl_ts,nyears_smooth)
exp_smooth=SMOOTH(exp_ts,nyears_smooth)

PSOPEN,file='/home/ss901165/idl/FAMOUS_soilresp.ps',MARGIN=2000,XOFFSET=2000
CS,SCALE=1,NCOLS=12
GSET,XMIN=1850,XMAX=1850+nyears_exp,YMIN=1.E-8,YMAX=3.5E-8,TITLE='Global-mean soil respiration'
GPLOT,X=1850+indgen(nyears_ctl),Y=ctl_ts,COL=2
GPLOT,X=1850+indgen(nyears_exp),Y=exp_ts,COL=10
GPLOT,X=1855+indgen(nyears_ctl-nyears_smooth),Y=ctl_smooth(nyears_smooth/2:nyears_ctl-(nyears_smooth/2+1)),COL=FSC_COLOR('cyan')
GPLOT,X=1855+indgen(nyears_exp-nyears_smooth),Y=exp_smooth(nyears_smooth/2:nyears_exp-(nyears_smooth/2+1)),COL=FSC_COLOR('red')
GPLOT,X=[1850,1850+nyears_exp],Y=[0,0],STYLE=2
GLEGEND,labels=REVERSE(labels),COL=REVERSE([2,10,FSC_COLOR('cyan'),FSC_COLOR('red')]),LEGPOS=11
AXES,XSTEP=200,XMINOR=100,YSTEP=1.5E-9,XTITLE='Year',YMINOR=0.5E-9,YTITLE='Soil respiration (kg C m!U-2!N)'
PSCLOSE,/NOVIEW

ctl_ts=REFORM(ctl.wind)
exp_ts=REFORM(exp.wind)
ctl_smooth=SMOOTH(ctl_ts,nyears_smooth)
exp_smooth=SMOOTH(exp_ts,nyears_smooth)

PSOPEN,file='/home/ss901165/idl/FAMOUS_tenmwind.ps',MARGIN=2000,XOFFSET=2000
CS,SCALE=1,NCOLS=12
GSET,XMIN=1850,XMAX=1850+nyears_exp,YMIN=4.6,YMAX=5.8,TITLE='Global-mean 10 metre wind speed'
GPLOT,X=1850+indgen(nyears_ctl),Y=ctl_ts,COL=2
GPLOT,X=1850+indgen(nyears_exp),Y=exp_ts,COL=10
GPLOT,X=1855+indgen(nyears_ctl-nyears_smooth),Y=ctl_smooth(nyears_smooth/2:nyears_ctl-(nyears_smooth/2+1)),COL=FSC_COLOR('cyan')
GPLOT,X=1855+indgen(nyears_exp-nyears_smooth),Y=exp_smooth(nyears_smooth/2:nyears_exp-(nyears_smooth/2+1)),COL=FSC_COLOR('red')
GPLOT,X=[1850,1850+nyears_exp],Y=[0,0],STYLE=2
GLEGEND,labels=REVERSE(labels),COL=REVERSE([2,10,FSC_COLOR('cyan'),FSC_COLOR('red')]),LEGPOS=9
AXES,XSTEP=200,XMINOR=100,YSTEP=0.1,XTITLE='Year',YMINOR=0.05,YTITLE='10-metre wind speed',NDECS=2
PSCLOSE,/NOVIEW

ctl_ts=REFORM(ctl.precip)*86400.
exp_ts=REFORM(exp.precip)*86400.
ctl_smooth=SMOOTH(ctl_ts,nyears_smooth)
exp_smooth=SMOOTH(exp_ts,nyears_smooth)

PSOPEN,file='/home/ss901165/idl/FAMOUS_precip.ps',MARGIN=2000,XOFFSET=2000
CS,SCALE=1,NCOLS=12
GSET,XMIN=1850,XMAX=1850+nyears_exp,YMIN=2.4,YMAX=4.21,TITLE='Global-mean precipitaton rate'
GPLOT,X=1850+indgen(nyears_ctl),Y=ctl_ts,COL=2
GPLOT,X=1850+indgen(nyears_exp),Y=exp_ts,COL=10
GPLOT,X=1855+indgen(nyears_ctl-nyears_smooth),Y=ctl_smooth(nyears_smooth/2:nyears_ctl-(nyears_smooth/2+1)),COL=FSC_COLOR('cyan')
GPLOT,X=1855+indgen(nyears_exp-nyears_smooth),Y=exp_smooth(nyears_smooth/2:nyears_exp-(nyears_smooth/2+1)),COL=FSC_COLOR('red')
GPLOT,X=[1850,1850+nyears_exp],Y=[0,0],STYLE=2
GLEGEND,labels=REVERSE(labels),COL=REVERSE([2,10,FSC_COLOR('cyan'),FSC_COLOR('red')]),LEGPOS=11
AXES,XSTEP=200,XMINOR=100,YSTEP=0.15,XTITLE='Year',YMINOR=0.05,YTITLE='Precipitation rate (mm day!U-1!N)',NDECS=2
PSCLOSE,/NOVIEW

ctl_ts=REFORM(-1.*ctl.field201-ctl.olr)+341.65
exp_ts=REFORM(-1.*exp.field201-exp.olr)+341.65
ctl_smooth=SMOOTH(ctl_ts,nyears_smooth)
exp_smooth=SMOOTH(exp_ts,nyears_smooth)

PSOPEN,file='/home/ss901165/idl/FAMOUS_toaimbalance.ps',MARGIN=2000,XOFFSET=2000
CS,SCALE=1,NCOLS=12
GSET,XMIN=1850,XMAX=1850+nyears_exp,YMIN=-2,YMAX=12,TITLE='Global-mean TOA radiative imbalance'
GPLOT,X=1850+indgen(nyears_ctl),Y=ctl_ts,COL=2
GPLOT,X=1850+indgen(nyears_exp),Y=exp_ts,COL=10
GPLOT,X=1855+indgen(nyears_ctl-nyears_smooth),Y=ctl_smooth(nyears_smooth/2:nyears_ctl-(nyears_smooth/2+1)),COL=FSC_COLOR('cyan')
GPLOT,X=1855+indgen(nyears_exp-nyears_smooth),Y=exp_smooth(nyears_smooth/2:nyears_exp-(nyears_smooth/2+1)),COL=FSC_COLOR('red')
GPLOT,X=[1850,1850+nyears_exp],Y=[0,0],STYLE=2
GLEGEND,labels=REVERSE(labels),COL=REVERSE([2,10,FSC_COLOR('cyan'),FSC_COLOR('red')]),LEGPOS=11
AXES,XSTEP=200,XMINOR=100,YSTEP=1,XTITLE='Year',YMINOR=0.5,YTITLE='TOA imbalance (W m!U-2!N)'
PSCLOSE,/NOVIEW

ctl_netsolar=REFORM(-1.*ctl.field201)+341.65
ctl_olr=REFORM(ctl.olr)
exp_netsolar=REFORM(-1.*exp.field201)+341.65
exp_olr=REFORM(exp.olr)

PSOPEN,file='/home/ss901165/idl/FAMOUS_toaimbalance_components.ps',MARGIN=2000,XOFFSET=2000
CS,SCALE=1,NCOLS=12
GSET,XMIN=1850,XMAX=1850+nyears_exp,YMIN=225,YMAX=260,TITLE='Global-mean TOA radiative fluxes'
GPLOT,X=1850+indgen(nyears_ctl),Y=ctl_netsolar,COL=2
GPLOT,X=1850+indgen(nyears_exp),Y=exp_netsolar,COL=10
ctl_smooth=SMOOTH(ctl_netsolar,nyears_smooth)
exp_smooth=SMOOTH(exp_netsolar,nyears_smooth)
GPLOT,X=1855+indgen(nyears_ctl-nyears_smooth),Y=ctl_smooth(nyears_smooth/2:nyears_ctl-(nyears_smooth/2+1)),COL=FSC_COLOR('cyan')
GPLOT,X=1855+indgen(nyears_exp-nyears_smooth),Y=exp_smooth(nyears_smooth/2:nyears_exp-(nyears_smooth/2+1)),COL=FSC_COLOR('red')

GPLOT,X=1850+indgen(nyears_ctl),Y=ctl_olr,COL=3
GPLOT,X=1850+indgen(nyears_exp),Y=exp_olr,COL=9
ctl_smooth=SMOOTH(ctl_olr,nyears_smooth)
exp_smooth=SMOOTH(exp_olr,nyears_smooth)
GPLOT,X=1855+indgen(nyears_ctl-nyears_smooth),Y=ctl_smooth(nyears_smooth/2:nyears_ctl-(nyears_smooth/2+1)),COL=FSC_COLOR('cyan')
GPLOT,X=1855+indgen(nyears_exp-nyears_smooth),Y=exp_smooth(nyears_smooth/2:nyears_exp-(nyears_smooth/2+1)),COL=FSC_COLOR('red')
AXES,XSTEP=200,XMINOR=100,YSTEP=5,YTITLE='TOA fluxes (W m!U-2!N; positive up for OLR, down for net solar radiation)'

GLEGEND,labels=REVERSE(['CTL net solar yearly','25*MIX net solar yearly',$
                        'CTL OLR yearly','25*MIX OLR yearly','CTL 21-year smoothed','25*MIX 21-year smoothed']),$
        COL=REVERSE([2,10,3,9,FSC_COLOR('cyan'),FSC_COLOR('red')]),LEGPOS=11
PSCLOSE,/NOVIEW

STOP
END

