PRO mjo_diabatic_synthesis_fidelity
  
; Plot fidelity measure for 20-year climate simulations vs 20-day hindcasts

model_codes=[               'NA',  'GI',  'MI',  'MR',  'MO',  'CN',  'CC',  'CZ',  'E3']
climate_fidelity=[          0.66,  0.90,  0.53,  0.83,  0.53,  0.76,  0.48,  0.76,  0.79]
hindcast_fidelity_bivar10=[ 0.836, 0.854, 0.767, 0.848, 0.813, 0.813, 0.640, 0.901, 0.877]
hindcast_fidelity_skill=[   18,    16,    14,    16,    16,    15,    6,     20,    16]
climate_dqdt=[              0.210, 0.732, 0.288, 0.625, 0.338, 0.111, 0.210, 0.572, 0.671]
hindcast_dqdt=[             0.562, 0.615, 0.301, 0.495, 0.447, 0.133, 0.032, 0.737, 0.710]
hindcast_dqdt_day2=[        0.122, 0.152, 0.348, 0.144, 0.060, 0.129, 0.065, 0.358, 0.519]

hindcast_fidelity_precip2=[ 0.659, 0.593, 0.633, 0.674, 0.615, 0.637, 0.496, 0.653, 0.656]
hindcast_fidelity_precip10=[0.396, 0.395, 0.187, 0.280, 0.256, 0.008, 0.104, 0.266, 0.176] 

hindcast_ngms_vert=[        0.395, -0.675,0.588, 1.117,-0.878, 0.129, 0.089, 0.276, 0.701]/5.
hindcast_ngms_horiz=[       1.460, 0.943, 0.211, 0.361,-0.631,-0.562,-0.295,-0.323,-0.349]
hindcast_ngms_total=hindcast_ngms_vert+hindcast_ngms_horiz

hindcast_ngms_horiz_init=[   0.118,-0.205,-0.352,-0.282,0.123,-0.621,-0.510,-0.521,-0.490]
hindcast_ngms_vert_init=[    0.126,-0.144,0.153, 0.199, 0.03, 0.075, 0.117, 0.110, 0.120]
hindcast_ngms_total_init=hindcast_ngms_vert_init+hindcast_ngms_horiz_init

climate_dqdt_regress=[      0.067, 0.290, 0.118, 0.244, 0.080, 0.127, 0.068, 0.174, 0.471]
hindcast_dqdt_regress=[     0.326, 0.333, 0.120, 0.262, 0.153, 0.212, 0.013, 0.437, 0.606]

climate_rhprecip=[          43.9,  50.1,  38.2,  49.4,  48.1,  48.9,  43.2,  45.2,  46.5]
hindcast_rhprecip=[         46.5,  48.8,  46.1,  55.4,  48.0,  47.9,  42.5,  52.1,  53.5]

climate_ngms_vert=[         0.14,  0.04,  0.19,  0.04,  0.00,  -0.21, 0.32,  0.11,  0.15]
climate_ngms_horiz=[        0.18,  0.09,  0.17,  0.15,  0.23,   0.36, 0.14,  0.31,  0.19]
climate_ngms_total=climate_ngms_vert+climate_ngms_horiz

;climate_color=['orangered','forestgreen','red','forestgreen','red','orangered','red','orangered','orangered']
climate_color=['black','red','dodgerblue','red','dodgerblue','black','dodgerblue','black','black']
;hindcast_color=['forestgreen','orangered','red','orangered','orangered','orangered','red','forestgreen','orangered']
hindcast_color=['red','black','dodgerblue','black','black','black','dodgerblue','red','black']

;model_codes=['NA','GI','MI','MR','MO','CN','CZ','E3']
;climate_fidelity=[0.66,0.90,0.53,0.83,0.53,0.76,0.76,0.79]
;hindcast_fidelity_bivar10=[0.836,0.854,0.767,0.848,0.813,0.813,0.901,0.877]
;hindcast_fidelity_skill=[18,16,14,16,16,17,20,16]
;climate_dqdt=[              0.210, 0.298, 0.288, 0.625, 0.338, 0.111, 0.572, 0.671]
;hindcast_dqdt=[             0.562, 0.615, 0.301, 0.495, 0.447, 0.133, 0.737, 0.710] 

n_models=N_ELEMENTS(model_codes)

psfile='/home/ss901165/idl/mjo_diabatic/synthesis/mjo_diabatic_synthesis_fidelity.bivar_10day.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=140,TFONT=6,MARGIN=3500,XSIZE=16000,YSIZE=16000
GSET,XMIN=0.45,XMAX=0.95,YMIN=0.45,YMAX=0.95
GPLOT,Y=climate_fidelity,X=hindcast_fidelity_bivar10,SYM=3,SIZE=150,/NOLINES
AXES,XSTEP=0.10,YSTEP=0.10,XTITLE='Fidelity in 20-day hindcasts (RMM bivariate correlation at day 10)',$
     YTITLE='Fidelity in 20-year simulations (correlation of rainfall Hovmoller)',$
     NDECS=2,XMINOR=0.05,YMINOR=0.05
FOR i=0,n_models-1 DO BEGIN
   GPLOT,Y=climate_fidelity(i)+0.015,X=hindcast_fidelity_bivar10(i)-0.007,TEXT=STRMID(model_codes(i),0,1),COL=FSC_COLOR(hindcast_color(i))
   GPLOT,Y=climate_fidelity(i)+0.015,X=hindcast_fidelity_bivar10(i)+0.007,TEXT=STRMID(model_codes(i),1,1),COL=FSC_COLOR(climate_color(i))
ENDFOR
 
coeff=REGRESS(hindcast_fidelity_bivar10,climate_fidelity,CONST=constant)
GPLOT,X=[0.65,0.95],Y=[0.65,0.95]*coeff(0)+constant,STYLE=2
GPLOT,X=0.73,Y=0.65,TEXT='r = '+$
      STRMID(STRTRIM(STRING(CORRELATE(climate_fidelity,hindcast_fidelity_bivar10)),1),0,4)
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/synthesis/mjo_diabatic_synthesis_fidelity.bivar_skill.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=140,TFONT=6,MARGIN=3500,XSIZE=16000,YSIZE=16000
GSET,XMIN=5,XMAX=21,YMIN=0.45,YMAX=0.95
GPLOT,Y=climate_fidelity,X=hindcast_fidelity_skill,SYM=3,SIZE=150,/NOLINES
AXES,XSTEP=3,YSTEP=0.10,XTITLE='Fidelity in 20-day hindcasts (lead of RMM bivar corr < 0.7; days)',$
     YTITLE='Fidelity in 20-year simulations (correlation of rainfall Hovmoller)',$
     NDECS=2,XMINOR=1,YMINOR=0.05
FOR i=0,n_models-1 DO BEGIN
   GPLOT,Y=climate_fidelity(i)+0.015,X=hindcast_fidelity_skill(i)-0.25,TEXT=STRMID(model_codes(i),0,1),COL=FSC_COLOR(hindcast_color(i))
   GPLOT,Y=climate_fidelity(i)+0.015,X=hindcast_fidelity_skill(i)+0.25,TEXT=STRMID(model_codes(i),1,1),COL=FSC_COLOR(climate_color(i))
ENDFOR
coeff=REGRESS(hindcast_fidelity_skill,climate_fidelity,CONST=constant)
GPLOT,X=[5,21],Y=[5,21]*coeff(0)+constant,STYLE=2
GPLOT,X=11,Y=0.65,TEXT='r = '+$
      STRMID(STRTRIM(STRING(CORRELATE(climate_fidelity,hindcast_fidelity_skill)),1),0,4)
PSCLOSE

psfile='/home/ss901165/idl/mjo_diabatic/synthesis/mjo_diabatic_synthesis_fidelity.precip_2day_10day.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=140,TFONT=6,MARGIN=3500,XSIZE=16000,YSIZE=16000
GSET,XMIN=-0.05,XMAX=0.45,YMIN=0.45,YMAX=0.70
GPLOT,Y=hindcast_fidelity_precip2,X=hindcast_fidelity_precip10,SYM=3,SIZE=150,/NOLINES
AXES,XSTEP=0.1,YSTEP=0.05,XTITLE='Correlation of rainfall Hovmoller at 10-day lead',$
     YTITLE='Correlation of rainfall Hovmoller at 2-day lead',$
     NDECS=2,XMINOR=0.05,YMINOR=0.025
FOR i=0,n_models-1 DO $
   GPLOT,Y=hindcast_fidelity_precip2(i)+0.005,X=hindcast_fidelity_precip10(i),TEXT=model_codes(i),COL=FSC_COLOR(hindcast_color(i))
coeff=REGRESS(hindcast_fidelity_precip10,hindcast_fidelity_precip2,CONST=constant)
GPLOT,X=[-0.05,0.45],Y=[-0.1,0.45]*coeff(0)+constant,STYLE=2
GPLOT,X=0.1,Y=0.55,TEXT='r = '+$
      STRMID(STRTRIM(STRING(CORRELATE(hindcast_fidelity_precip2,hindcast_fidelity_precip10)),1),0,4)
GPLOT,X=[0,0],Y=[0.45,0.70],STYLE=1
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/synthesis/mjo_diabatic_synthesis_fidelity.precip_2day_climate.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=140,TFONT=6,MARGIN=3500,XSIZE=16000,YSIZE=16000
GSET,XMIN=0.45,XMAX=0.95,YMIN=0.45,YMAX=0.70
GPLOT,Y=hindcast_fidelity_precip2,X=climate_fidelity,SYM=3,SIZE=150,/NOLINES
AXES,XSTEP=0.1,YSTEP=0.05,XTITLE='Fidelity in 20-year simulations',$
     YTITLE='Correlation of rainfall Hovmoller at 2-day lead',$
     NDECS=2,XMINOR=0.05,YMINOR=0.025
FOR i=0,n_models-1 DO $
   GPLOT,Y=hindcast_fidelity_precip2(i)+0.005,X=climate_fidelity(i),TEXT=model_codes(i),COL=FSC_COLOR(climate_color(i))
coeff=REGRESS(climate_fidelity,hindcast_fidelity_precip2,CONST=constant)
GPLOT,X=[0.45,0.95],Y=[0.45,0.95]*coeff(0)+constant,STYLE=2
GPLOT,X=0.7,Y=0.55,TEXT='r = '+$
      STRMID(STRTRIM(STRING(CORRELATE(hindcast_fidelity_precip2,climate_fidelity)),1),0,4)
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/synthesis/mjo_diabatic_synthesis_fidelity.dqdt_climate.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=140,TFONT=6,MARGIN=3500,XSIZE=16000,YSIZE=16000
GSET,XMIN=0.0,XMAX=0.8,YMIN=0.45,YMAX=0.95
GPLOT,Y=climate_fidelity,X=climate_dqdt,SYM=3,SIZE=150,/NOLINES
AXES,XSTEP=0.10,YSTEP=0.10,XTITLE='Pattern correlation of net moistening (20-year simulations)',$
     YTITLE='Fidelity in 20-year simulations',$
     NDECS=2,XMINOR=0.05,YMINOR=0.05
FOR i=0,n_models-1 DO $
   GPLOT,Y=climate_fidelity(i)+0.015,X=climate_dqdt(i),TEXT=model_codes(i),COL=FSC_COLOR(climate_color(i))
coeff=REGRESS(climate_dqdt,climate_fidelity,CONST=constant)
GPLOT,X=[0,0.8],Y=[0,0.8]*coeff(0)+constant,STYLE=2
GPLOT,X=0.1,Y=0.60,TEXT='r = '+$
      STRMID(STRTRIM(STRING(CORRELATE(climate_fidelity,climate_dqdt)),1),0,4)
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/synthesis/mjo_diabatic_synthesis_fidelity.dqdt_hindcast.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=140,TFONT=6,MARGIN=3500,XSIZE=16000,YSIZE=16000
GSET,XMIN=0.0,XMAX=0.8,YMIN=5,YMAX=21
GPLOT,Y=hindcast_fidelity_skill,X=hindcast_dqdt,SYM=3,SIZE=150,/NOLINES
AXES,XSTEP=0.10,YSTEP=3,XTITLE='Pattern correlation of net moistening (20-day hindcasts)',$
     YTITLE='Fidelity in 20-day hindcasts (days)',$
     NDECS=2,XMINOR=0.05,YMINOR=1
FOR i=0,n_models-1 DO $
   GPLOT,Y=hindcast_fidelity_skill(i)+0.5,X=hindcast_dqdt(i),TEXT=model_codes(i),COL=FSC_COLOR(hindcast_color(i))
coeff=REGRESS(hindcast_dqdt,hindcast_fidelity_skill,CONST=constant)
GPLOT,X=[0,0.8],Y=[0,0.8]*coeff(0)+constant,STYLE=2
GPLOT,X=0.30,Y=11,TEXT='r = '+$
      STRMID(STRTRIM(STRING(CORRELATE(hindcast_fidelity_skill,hindcast_dqdt)),1),0,4)
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/synthesis/mjo_diabatic_synthesis_fidelity.dqdt_hindcast_2day.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=140,TFONT=6,MARGIN=3500,XSIZE=16000,YSIZE=16000
GSET,XMIN=0.0,XMAX=0.8,YMIN=0.45,YMAX=0.7
GPLOT,Y=hindcast_fidelity_precip2,X=hindcast_dqdt_day2,SYM=3,SIZE=150,/NOLINES
AXES,XSTEP=0.10,YSTEP=0.05,XTITLE='Pattern correlation of net moistening (2-day hindcasts)',$
     YTITLE='Correlation of rainfall Hovmoller at 2-day lead',$
     NDECS=2,XMINOR=0.05,YMINOR=0.025
FOR i=0,n_models-1 DO $
   GPLOT,Y=hindcast_fidelity_precip2(i)+0.005,X=hindcast_dqdt_day2(i),TEXT=model_codes(i) ;,COL=FSC_COLOR(hindcast_color(i))
coeff=REGRESS(hindcast_dqdt_day2,hindcast_fidelity_precip2,CONST=constant)
GPLOT,X=[0,0.73],Y=[0,0.73]*coeff(0)+constant,STYLE=2
GPLOT,X=0.30,Y=0.6,TEXT='r = '+$
      STRMID(STRTRIM(STRING(CORRELATE(hindcast_fidelity_precip2,hindcast_dqdt_day2)),1),0,4)
PSCLOSE

psfile='/home/ss901165/idl/mjo_diabatic/synthesis/mjo_diabatic_synthesis_fidelity.dqdt_both.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=140,TFONT=6,MARGIN=3500,XSIZE=16000,YSIZE=16000
GSET,XMIN=0.0,XMAX=0.8,YMIN=0.0,YMAX=0.8
GPLOT,Y=climate_dqdt,X=hindcast_dqdt,SYM=3,SIZE=150,/NOLINES
AXES,XSTEP=0.10,YSTEP=0.10,XTITLE='Pattern correlation of net moistening (20-day hindcasts)',$
     YTITLE='Pattern correlation of net moistening (20-year simulations)',$
     NDECS=2,XMINOR=0.05,YMINOR=0.05
FOR i=0,n_models-1 DO BEGIN
   GPLOT,Y=climate_dqdt(i)+0.015,X=hindcast_dqdt(i)-0.012,TEXT=STRMID(model_codes(i),0,1),COL=FSC_COLOR(hindcast_color(i))
   GPLOT,Y=climate_dqdt(i)+0.015,X=hindcast_dqdt(i)+0.012,TEXT=STRMID(model_codes(i),1,1),COL=FSC_COLOR(climate_color(i))
ENDFOR
coeff=REGRESS(hindcast_dqdt,climate_dqdt,CONST=constant)
GPLOT,X=[0,0.8],Y=[0,0.8]*coeff(0)+constant,STYLE=2
GPLOT,X=[0,0.8],Y=[0,0.8],STYLE=1
GPLOT,X=0.50,Y=0.50,TEXT='r = '+$
      STRMID(STRTRIM(STRING(CORRELATE(climate_dqdt,hindcast_dqdt)),1),0,4)
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/synthesis/mjo_diabatic_synthesis_fidelity.dqdt_regress.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=140,TFONT=6,MARGIN=3500,XSIZE=16000,YSIZE=16000
GSET,XMIN=0.0,XMAX=0.65,YMIN=0.0,YMAX=0.65
GPLOT,Y=climate_dqdt_regress,X=hindcast_dqdt_regress,SYM=3,SIZE=150,/NOLINES
AXES,XSTEP=0.10,YSTEP=0.10,XTITLE='Pattern regression of net moistening (20-day hindcasts)',$
     YTITLE='Pattern regression of net moistening (20-year simulations)',$
     NDECS=2,XMINOR=0.05,YMINOR=0.05
FOR i=0,n_models-1 DO BEGIN
   GPLOT,Y=climate_dqdt_regress(i)+0.012,X=hindcast_dqdt_regress(i)-0.01,TEXT=STRMID(model_codes(i),0,1),COL=FSC_COLOR(hindcast_color(i))
   GPLOT,Y=climate_dqdt_regress(i)+0.012,X=hindcast_dqdt_regress(i)+0.01,TEXT=STRMID(model_codes(i),1,1),COL=FSC_COLOR(climate_color(i))
ENDFOR
coeff=REGRESS(hindcast_dqdt_regress,climate_dqdt_regress,CONST=constant)
GPLOT,X=[0,0.65],Y=[0,0.65]*coeff(0)+constant,STYLE=2
GPLOT,X=0.50,Y=0.37,TEXT='r = '+$
      STRMID(STRTRIM(STRING(CORRELATE(climate_dqdt_regress,hindcast_dqdt_regress)),1),0,4)
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/synthesis/mjo_diabatic_synthesis_fidelity.ngms_vert_hindcast.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=160,TFONT=6,MARGIN=3500,XSIZE=16000,YSIZE=16000
GSET,XMIN=-0.300,XMAX=0.300,YMIN=5,YMAX=21
GPLOT,Y=hindcast_fidelity_skill,X=hindcast_ngms_vert,SYM=3,SIZE=150,/NOLINES
AXES,XSTEP=0.1,YSTEP=3,XTITLE='Vertical NGMS in 20-day hindcasts',$
     YTITLE='Fidelity in 20-day hindcasts (days)',$
     NDECS=2,XMINOR=0.05,YMINOR=1
FOR i=0,n_models-1 DO $
   GPLOT,Y=hindcast_fidelity_skill(i)+0.5,X=hindcast_ngms_vert(i),TEXT=model_codes(i),COL=FSC_COLOR(hindcast_color(i))
coeff=REGRESS(hindcast_ngms_vert,hindcast_fidelity_skill,CONST=constant)
GPLOT,X=[-0.3,0.3],Y=[-0.3,0.3]*coeff(0)+constant,STYLE=2
GPLOT,X=[0,0],Y=[5,21],STYLE=1
GPLOT,X=-0.10,Y=14,TEXT='r = '+$
      STRMID(STRTRIM(STRING(CORRELATE(hindcast_fidelity_skill,hindcast_ngms_vert)),1),0,4)
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/synthesis/mjo_diabatic_synthesis_fidelity.ngms_horiz_hindcast.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=160,TFONT=6,MARGIN=3500,XSIZE=16000,YSIZE=16000
GSET,XMIN=-0.8,XMAX=1.6,YMIN=5,YMAX=21
GPLOT,Y=hindcast_fidelity_skill,X=hindcast_ngms_horiz,SYM=3,SIZE=150,/NOLINES
AXES,XSTEP=0.4,YSTEP=3,XTITLE='Horizontal NGMS in 20-day hindcasts',$
     YTITLE='Fidelity in 20-day hindcasts (days)',$
     NDECS=2,XMINOR=0.1,YMINOR=1
FOR i=0,n_models-1 DO $
   GPLOT,Y=hindcast_fidelity_skill(i)+0.5,X=hindcast_ngms_horiz(i),TEXT=model_codes(i),COL=FSC_COLOR(hindcast_color(i))
coeff=REGRESS(hindcast_ngms_horiz,hindcast_fidelity_skill,CONST=constant)
GPLOT,X=[-0.8,1.6],Y=[-0.8,1.6]*coeff(0)+constant,STYLE=2
GPLOT,X=[0,0],Y=[5,21],STYLE=1
GPLOT,X=-0.60,Y=13,TEXT='r = '+$
      STRMID(STRTRIM(STRING(CORRELATE(hindcast_fidelity_skill,hindcast_ngms_horiz)),1),0,4)
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/synthesis/mjo_diabatic_synthesis_fidelity.ngms_total_hindcast.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=160,TFONT=6,MARGIN=3500,XSIZE=16000,YSIZE=16000
GSET,XMIN=-0.9,XMAX=1.8,YMIN=5,YMAX=21
GPLOT,Y=hindcast_fidelity_skill,X=hindcast_ngms_total,SYM=3,SIZE=150,/NOLINES
AXES,XSTEP=0.3,YSTEP=3,XTITLE='Total NGMS in 20-day hindcasts',$
     YTITLE='Fidelity in 20-day hindcasts (days)',$
     NDECS=2,XMINOR=0.1,YMINOR=1
FOR i=0,n_models-1 DO $
   GPLOT,Y=hindcast_fidelity_skill(i)+0.5,X=hindcast_ngms_total(i),TEXT=model_codes(i),COL=FSC_COLOR(hindcast_color(i))
coeff=REGRESS(hindcast_ngms_total,hindcast_fidelity_skill,CONST=constant)
GPLOT,X=[-0.9,1.8],Y=[-0.9,1.8]*coeff(0)+constant,STYLE=2
GPLOT,X=[0,0],Y=[5,21],STYLE=1
GPLOT,X=-0.60,Y=13,TEXT='r = '+$
      STRMID(STRTRIM(STRING(CORRELATE(hindcast_fidelity_skill,hindcast_ngms_total)),1),0,4)
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/synthesis/mjo_diabatic_synthesis_fidelity.ngms_vert_hindcast_init.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=160,TFONT=6,MARGIN=3500,XSIZE=16000,YSIZE=16000
GSET,XMIN=-0.300,XMAX=0.300,YMIN=5,YMAX=21
GPLOT,Y=hindcast_fidelity_skill,X=hindcast_ngms_vert_init,SYM=3,SIZE=150,/NOLINES
AXES,XSTEP=0.1,YSTEP=3,XTITLE='Vertical NGMS in 20-day hindcasts',$
     YTITLE='Fidelity in 20-day hindcasts (days)',$
     NDECS=2,XMINOR=0.05,YMINOR=1
FOR i=0,n_models-1 DO $
   GPLOT,Y=hindcast_fidelity_skill(i)+0.5,X=hindcast_ngms_vert_init(i),TEXT=model_codes(i),COL=FSC_COLOR(hindcast_color(i))
coeff=REGRESS(hindcast_ngms_vert_init,hindcast_fidelity_skill,CONST=constant)
GPLOT,X=[-0.3,0.3],Y=[-0.3,0.3]*coeff(0)+constant,STYLE=2
GPLOT,X=[0,0],Y=[5,21],STYLE=1
GPLOT,X=-0.10,Y=14,TEXT='r = '+$
      STRMID(STRTRIM(STRING(CORRELATE(hindcast_fidelity_skill,hindcast_ngms_vert_init)),1),0,5)
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/synthesis/mjo_diabatic_synthesis_fidelity.ngms_horiz_hindcast_init.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=160,TFONT=6,MARGIN=3500,XSIZE=16000,YSIZE=16000
GSET,XMIN=-0.8,XMAX=0.4,YMIN=5,YMAX=21
GPLOT,Y=hindcast_fidelity_skill,X=hindcast_ngms_horiz_init,SYM=3,SIZE=150,/NOLINES
AXES,XSTEP=0.2,YSTEP=3,XTITLE='Horizontal NGMS in 20-day hindcasts',$
     YTITLE='Fidelity in 20-day hindcasts (days)',$
     NDECS=2,XMINOR=0.05,YMINOR=1
FOR i=0,n_models-1 DO $
   GPLOT,Y=hindcast_fidelity_skill(i)+0.4,X=hindcast_ngms_horiz_init(i),TEXT=model_codes(i),COL=FSC_COLOR(hindcast_color(i))
coeff=REGRESS(hindcast_ngms_horiz_init,hindcast_fidelity_skill,CONST=constant)
GPLOT,X=[-0.8,0.4],Y=[-0.8,0.4]*coeff(0)+constant,STYLE=2
GPLOT,X=[0,0],Y=[5,21],STYLE=1
GPLOT,X=-0.60,Y=13,TEXT='r = '+$
      STRMID(STRTRIM(STRING(CORRELATE(hindcast_fidelity_skill,hindcast_ngms_horiz_init)),1),0,4)
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/synthesis/mjo_diabatic_synthesis_fidelity.ngms_total_hindcast_init.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=160,TFONT=6,MARGIN=3500,XSIZE=16000,YSIZE=16000
GSET,XMIN=-0.8,XMAX=0.6,YMIN=5,YMAX=21
GPLOT,Y=hindcast_fidelity_skill,X=hindcast_ngms_total_init,SYM=3,SIZE=150,/NOLINES
AXES,XSTEP=0.2,YSTEP=3,XTITLE='Total NGMS in 20-day hindcasts',$
     YTITLE='Fidelity in 20-day hindcasts (days)',$
     NDECS=2,XMINOR=0.1,YMINOR=1
FOR i=0,n_models-1 DO BEGIN
   IF model_codes(i) eq 'GI' THEN BEGIN
      GPLOT,Y=hindcast_fidelity_skill(i)-0.2,X=hindcast_ngms_total_init(i)+0.07,TEXT=model_codes(i),COL=FSC_COLOR(hindcast_color(i))
   ENDIF ELSE $
      GPLOT,Y=hindcast_fidelity_skill(i)+0.4,X=hindcast_ngms_total_init(i),TEXT=model_codes(i),COL=FSC_COLOR(hindcast_color(i))
ENDFOR
coeff=REGRESS(hindcast_ngms_total_init,hindcast_fidelity_skill,CONST=constant)
GPLOT,X=[-0.9,0.6],Y=[-0.9,0.6]*coeff(0)+constant,STYLE=2
GPLOT,X=[0,0],Y=[5,21],STYLE=1
GPLOT,X=-0.60,Y=13,TEXT='r = '+$
      STRMID(STRTRIM(STRING(CORRELATE(hindcast_fidelity_skill,hindcast_ngms_total_init)),1),0,4)
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/synthesis/mjo_diabatic_synthesis_fidelity.rhprecip_hindcast.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=160,TFONT=6,MARGIN=3500,XSIZE=16000,YSIZE=16000
GSET,XMIN=40,XMAX=60,YMIN=5,YMAX=21
GPLOT,Y=hindcast_fidelity_skill,X=hindcast_rhprecip,SYM=3,SIZE=150,/NOLINES
AXES,XSTEP=5,YSTEP=3,XTITLE='RH difference in 20-day hindcasts (%)',$
     YTITLE='Fidelity in 20-day hindcasts (days)',$
     NDECS=2,XMINOR=1,YMINOR=1
FOR i=0,n_models-1 DO BEGIN
  IF model_codes(i) eq 'CN' THEN BEGIN
     GPLOT,Y=hindcast_fidelity_skill(i)-0.9,X=hindcast_rhprecip(i),TEXT=model_codes(i),COL=FSC_COLOR(hindcast_color(i))
  ENDIF ELSE IF model_codes(i) eq 'GI' THEN BEGIN
     GPLOT,Y=hindcast_fidelity_skill(i)-0.2,X=hindcast_rhprecip(i)+0.8,TEXT=model_codes(i),COL=FSC_COLOR(hindcast_color(i))
  ENDIF ELSE $
     GPLOT,Y=hindcast_fidelity_skill(i)+0.4,X=hindcast_rhprecip(i),TEXT=model_codes(i),COL=FSC_COLOR(hindcast_color(i))
  ENDFOR
coeff=REGRESS(hindcast_rhprecip,hindcast_fidelity_skill,CONST=constant)
GPLOT,X=[40,58.5],Y=[40,58.5]*coeff(0)+constant,STYLE=2
;GPLOT,X=[40,60],Y=[5,21],STYLE=1
GPLOT,X=45,Y=11,TEXT='r = '+$
      STRMID(STRTRIM(STRING(CORRELATE(hindcast_fidelity_skill,hindcast_rhprecip)),1),0,4)
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/synthesis/mjo_diabatic_synthesis_fidelity.rhprecip_both.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=160,TFONT=6,MARGIN=3500,XSIZE=16000,YSIZE=16000
GSET,XMIN=35,XMAX=60,YMIN=35,YMAX=60
AXES,XSTEP=5,YSTEP=5,XTITLE='RH difference in 20-day hindcasts (%)',$
     YTITLE='RH difference in 20-year climate simulations (%)',$
     NDECS=2,XMINOR=1,YMINOR=1
FOR i=0,n_models-1 DO BEGIN
   IF model_codes(i) ne 'MO' THEN BEGIN
      GPLOT,Y=climate_rhprecip(i),X=hindcast_rhprecip(i),SYM=3,SIZE=150,/NOLINES
      GPLOT,Y=climate_rhprecip(i)+0.5,X=hindcast_rhprecip(i)-0.35,TEXT=STRMID(model_codes(i),0,1),COL=FSC_COLOR(hindcast_color(i))
      GPLOT,Y=climate_rhprecip(i)+0.5,X=hindcast_rhprecip(i)+0.35,TEXT=STRMID(model_codes(i),1,1),COL=FSC_COLOR(climate_color(i))
   ENDIF
ENDFOR
coeff=REGRESS(hindcast_rhprecip,climate_rhprecip,CONST=constant)
GPLOT,X=[35,60],Y=[35,60]*coeff(0)+constant,STYLE=2
GPLOT,X=[35,60],Y=[35,60],STYLE=1
GPLOT,X=40,Y=40,TEXT='r = '+$
      STRMID(STRTRIM(STRING(CORRELATE(climate_rhprecip,hindcast_rhprecip)),1),0,4)
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/synthesis/mjo_diabatic_synthesis_fidelity.ngms_vert_both.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=160,TFONT=6,MARGIN=3500,XSIZE=16000,YSIZE=16000
GSET,XMIN=-0.4,XMAX=0.4,YMIN=-0.4,YMAX=0.4
AXES,XSTEP=0.1,YSTEP=0.1,XTITLE='Vertical NGMS in 20-day hindcasts',$
     YTITLE='Vertical NGMS in 20-year climate simulations',$
     NDECS=2,XMINOR=0.05,YMINOR=0.05
FOR i=0,n_models-1 DO BEGIN
   IF model_codes(i) ne 'MO' THEN BEGIN
      GPLOT,Y=climate_ngms_vert(i),X=hindcast_ngms_vert_init(i),SYM=3,SIZE=150,/NOLINES
      IF model_codes(i) eq 'CZ' THEN BEGIN
         GPLOT,Y=climate_ngms_vert(i)-0.04,X=hindcast_ngms_vert_init(i)-0.012,TEXT=STRMID(model_codes(i),0,1),COL=FSC_COLOR(hindcast_color(i))
         GPLOT,Y=climate_ngms_vert(i)-0.04,X=hindcast_ngms_vert_init(i)+0.012,TEXT=STRMID(model_codes(i),1,1),COL=FSC_COLOR(climate_color(i))
      ENDIF ELSE IF model_codes(i) eq 'E3' THEN BEGIN      
         GPLOT,Y=climate_ngms_vert(i)-0.009,X=hindcast_ngms_vert_init(i)+0.032,TEXT=STRMID(model_codes(i),0,1),COL=FSC_COLOR(hindcast_color(i))
         GPLOT,Y=climate_ngms_vert(i)-0.009,X=hindcast_ngms_vert_init(i)+0.052,TEXT=STRMID(model_codes(i),1,1),COL=FSC_COLOR(climate_color(i))
      ENDIF ELSE IF model_codes(i) eq 'NA' THEN BEGIN         
         GPLOT,Y=climate_ngms_vert(i)-0.009,X=hindcast_ngms_vert_init(i)-0.052,TEXT=STRMID(model_codes(i),0,1),COL=FSC_COLOR(hindcast_color(i))
         GPLOT,Y=climate_ngms_vert(i)-0.009,X=hindcast_ngms_vert_init(i)-0.032,TEXT=STRMID(model_codes(i),1,1),COL=FSC_COLOR(climate_color(i))
      ENDIF ELSE BEGIN
         GPLOT,Y=climate_ngms_vert(i)+0.018,X=hindcast_ngms_vert_init(i)-0.012,TEXT=STRMID(model_codes(i),0,1),COL=FSC_COLOR(hindcast_color(i))
         GPLOT,Y=climate_ngms_vert(i)+0.018,X=hindcast_ngms_vert_init(i)+0.012,TEXT=STRMID(model_codes(i),1,1),COL=FSC_COLOR(climate_color(i))
      ENDELSE
   ENDIF
ENDFOR
coeff=REGRESS(hindcast_ngms_vert_init,climate_ngms_vert,CONST=constant)
GPLOT,X=[-0.4,0.4],Y=[-0.4,0.4]*coeff(0)+constant,STYLE=2
GPLOT,X=[-0.4,0.4],Y=[-0.4,0.4],STYLE=1
GPLOT,X=[0,0],Y=[-0.4,0.4],STYLE=1
GPLOT,X=[-0.4,0.4],Y=[0,0],STYLE=1
GPLOT,X=-0.3,Y=-0.05,TEXT='r = '+$
      STRMID(STRTRIM(STRING(CORRELATE(climate_ngms_vert,hindcast_ngms_vert_init)),1),0,4)
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/synthesis/mjo_diabatic_synthesis_fidelity.ngms_horiz_both.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=160,TFONT=6,MARGIN=3500,XSIZE=16000,YSIZE=16000
GSET,XMIN=-0.8,XMAX=0.4,YMIN=-0.4,YMAX=0.8
AXES,XSTEP=0.2,YSTEP=0.2,XTITLE='Horizontal NGMS in 20-day hindcasts',$
     YTITLE='Horizontal NGMS in 20-year climate simulations',$
     NDECS=2,XMINOR=0.05,YMINOR=0.05
FOR i=0,n_models-1 DO BEGIN   
   IF model_codes(i) ne 'MO' THEN BEGIN
      GPLOT,Y=climate_ngms_horiz(i),X=hindcast_ngms_horiz_init(i),SYM=3,SIZE=150,/NOLINES
      IF model_codes(i) eq 'CC' THEN BEGIN
         GPLOT,Y=climate_ngms_horiz(i)-0.06,X=hindcast_ngms_horiz_init(i)-0.018,TEXT=STRMID(model_codes(i),0,1),COL=FSC_COLOR(hindcast_color(i))
         GPLOT,Y=climate_ngms_horiz(i)-0.06,X=hindcast_ngms_horiz_init(i)+0.018,TEXT=STRMID(model_codes(i),1,1),COL=FSC_COLOR(climate_color(i))
  ; ENDIF ELSE IF model_codes(i) eq 'E3' THEN BEGIN      
  ;    GPLOT,Y=climate_ngms_horiz(i)-0.009,X=hindcast_ngms_horiz_init(i)+0.024,TEXT=STRMID(model_codes(i),0,1),COL=FSC_COLOR(hindcast_color(i))
  ;    GPLOT,Y=climate_ngms_horiz(i)-0.009,X=hindcast_ngms_horiz_init(i)+0.048,TEXT=STRMID(model_codes(i),1,1),COL=FSC_COLOR(climate_color(i))
      ENDIF ELSE BEGIN
         GPLOT,Y=climate_ngms_horiz(i)+0.03,X=hindcast_ngms_horiz_init(i)-0.018,TEXT=STRMID(model_codes(i),0,1),COL=FSC_COLOR(hindcast_color(i))
         GPLOT,Y=climate_ngms_horiz(i)+0.03,X=hindcast_ngms_horiz_init(i)+0.018,TEXT=STRMID(model_codes(i),1,1),COL=FSC_COLOR(climate_color(i))
      ENDELSE
   ENDIF
ENDFOR
coeff=REGRESS(hindcast_ngms_horiz_init,climate_ngms_horiz,CONST=constant)
GPLOT,X=[-0.8,0.4],Y=[-0.8,0.8]*coeff(0)+constant,STYLE=2
GPLOT,X=[-0.4,0.4],Y=[-0.4,0.4],STYLE=1
GPLOT,X=[0,0],Y=[-0.4,0.8],STYLE=1
GPLOT,X=[-0.8,0.4],Y=[0,0],STYLE=1
GPLOT,X=0.2,Y=0.05,TEXT='r = '+$
      STRMID(STRTRIM(STRING(CORRELATE(climate_ngms_horiz,hindcast_ngms_horiz_init)),1),0,5)
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/synthesis/mjo_diabatic_synthesis_fidelity.ngms_total_both.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=160,TFONT=6,MARGIN=3500,XSIZE=16000,YSIZE=16000
GSET,XMIN=-0.8,XMAX=0.4,YMIN=-0.4,YMAX=0.8
AXES,XSTEP=0.2,YSTEP=0.2,XTITLE='Total NGMS in 20-day hindcasts',$
     YTITLE='Total NGMS in 20-year climate simulations',$
     NDECS=2,XMINOR=0.05,YMINOR=0.05
FOR i=0,n_models-1 DO BEGIN
;   IF model_codes(i) eq 'C' THEN BEGIN
;      GPLOT,Y=climate_ngms_total(i)-0.06,X=hindcast_ngms_total_init(i)-0.04,TEXT=STRMID(model_codes(i),0,1),COL=FSC_COLOR(hindcast_color(i))
;      GPLOT,Y=climate_ngms_total(i)-0.06,X=hindcast_ngms_total_init(i)+0.04,TEXT=STRMID(model_codes(i),1,1),COL=FSC_COLOR(climate_color(i))
  ; ENDIF ELSE IF model_codes(i) eq 'E3' THEN BEGIN      
   IF model_codes(i) ne 'MO' THEN BEGIN
      GPLOT,Y=climate_ngms_total(i),X=hindcast_ngms_total_init(i),SYM=3,SIZE=150,/NOLINES
      IF model_codes(i) eq 'CZ' THEN BEGIN
         GPLOT,Y=climate_ngms_total(i)-0.025,X=hindcast_ngms_total_init(i)-0.10,TEXT=STRMID(model_codes(i),0,1),COL=FSC_COLOR(hindcast_color(i))
         GPLOT,Y=climate_ngms_total(i)-0.025,X=hindcast_ngms_total_init(i)-0.06,TEXT=STRMID(model_codes(i),1,1),COL=FSC_COLOR(climate_color(i))
      ENDIF ELSE IF model_codes(i) eq 'E3' THEN BEGIN      
         GPLOT,Y=climate_ngms_total(i)-0.025,X=hindcast_ngms_total_init(i)+0.06,TEXT=STRMID(model_codes(i),0,1),COL=FSC_COLOR(hindcast_color(i))
         GPLOT,Y=climate_ngms_total(i)-0.025,X=hindcast_ngms_total_init(i)+0.10,TEXT=STRMID(model_codes(i),1,1),COL=FSC_COLOR(climate_color(i))
      ENDIF ELSE BEGIN
         GPLOT,Y=climate_ngms_total(i)+0.03,X=hindcast_ngms_total_init(i)-0.018,TEXT=STRMID(model_codes(i),0,1),COL=FSC_COLOR(hindcast_color(i))
         GPLOT,Y=climate_ngms_total(i)+0.03,X=hindcast_ngms_total_init(i)+0.018,TEXT=STRMID(model_codes(i),1,1),COL=FSC_COLOR(climate_color(i))
      ENDELSE
   ENDIF
ENDFOR
coeff=REGRESS(hindcast_ngms_total_init,climate_ngms_total,CONST=constant)
GPLOT,X=[-0.8,0.4],Y=[-0.8,0.4]*coeff(0)+constant,STYLE=2
GPLOT,X=[-0.4,0.4],Y=[-0.4,0.4],STYLE=1
GPLOT,X=[0,0],Y=[-0.4,0.8],STYLE=1
GPLOT,X=[-0.8,0.4],Y=[0,0],STYLE=1
GPLOT,X=0.2,Y=0.20,TEXT='r = '+$
      STRMID(STRTRIM(STRING(CORRELATE(climate_ngms_total,hindcast_ngms_total_init)),1),0,5)
PSCLOSE,/NOVIEW

STOP
END
