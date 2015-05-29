PRO mjo_diabatic_synthesis_preciprmse,models

n_models=N_ELEMENTS(models)
basedir='/home/ss901165/um_output6/mjodiab_2day'

all_codes=strarr(n_models)
all_colors=strarr(n_models)
all_climate_colors=strarr(n_models)
all_hindcast_fidelity=intarr(n_models)
all_climate_fidelity=fltarr(n_models)
all_lagone=fltarr(n_models)

FOR m=0,n_models-1 DO BEGIN
   CASE models(m) OF
      'cam5' : BEGIN
         dir=basedir+'/cam5'
         name='NCAR.CAM5'
         all_codes(m)='C5'
         all_colors(m)='red'
         all_climate_colors(m)='black'
         z_name='levels'
         lon_name='lon'
         lat_name='lat'
         z_mult=1.
         nt=3168
         all_hindcast_fidelity(m)=20
         all_climate_fidelity(m)=0.67
         pr_mult=86400.*1000.
      END
      'cancm4' : BEGIN
         dir=basedir+'/cancm4'
         name='CanCM4'
         all_codes(m)='CC'
         all_colors(m)='blue'
         all_climate_colors(m)='blue'
         z_name='level'
         lon_name='longitude'
         lat_name='latitude'
         z_mult=0.01
         nt=1548
         all_hindcast_fidelity(m)=6
         all_climate_fidelity(m)=0.48
         pr_mult=86400.
      END
      'cnrm_atmos' : BEGIN
         dir=basedir+'/cnrm_atmos'
         name='CNRM'
         all_codes(m)='CN'
         all_colors(m)='black'
         all_climate_colors(m)='black'
         z_name='lev'
         lon_name='lon'
         lat_name='lat'
         z_mult=1.
         nt=3168         
         all_climate_fidelity(m)=0.76
         all_hindcast_fidelity(m)=15
      END
      'ecearth' : BEGIN
         dir=basedir+'/ecearth'
         name='ecearth3'
         all_codes(m)='E3'
         all_colors(m)='black'
         all_climate_colors(m)='black'
         z_name='lev'
         lon_name='lon'
         lat_name='lat'
         z_mult=0.01
         nt=2112
         pr_mult=86400.
         all_climate_fidelity(m)=0.79
         all_hindcast_fidelity(m)=16
      END      
      'giss' : BEGIN
         dir=basedir+'/giss'
         name='ModelE'
         all_codes(m)='GI'
         all_colors(m)='black'
         all_climate_colors(m)='red'
         z_name='level'
         lon_name='longitude'
         lat_name='latitude'
         z_mult=1
         nt=3168
         pr_mult=1.
         all_climate_fidelity(m)=0.90
         all_hindcast_fidelity(m)=16
      END
      'metum' : BEGIN
         dir=basedir+'/metum'
         name='MetUM'
         all_codes(m)='MO'
         all_colors(m)='black'
         all_climate_colors(m)='blue'
         z_name='level'
         lon_name='longitude'
         lat_name='latitude'
         z_mult=1
         nt=7920
         pr_mult=86400.
         all_climate_fidelity(m)=0.53
         all_hindcast_fidelity(m)=16
      END
      'miroc' : BEGIN
         dir=basedir+'/miroc'
         name='miroc5'
         all_codes(m)='MI'
         all_colors(m)='blue'
         all_climate_colors(m)='blue'
         z_name='level'
         lon_name='longitude'
         lat_name='latitude'
         z_mult=1
         nt=7920
         pr_mult=86400.
         all_climate_fidelity(m)=0.53
         all_hindcast_fidelity(m)=14
      END
      'mri' : BEGIN
         dir=basedir+'/mri'
         name='MRI-AGCM'
         all_codes(m)='MR'
         all_colors(m)='black'
         all_climate_colors(m)='red'
         z_name='plev'
         lon_name='lon'
         lat_name='lat'
         z_mult=0.01
         nt=3168
         pr_mult=86400.
         all_climate_fidelity(m)=0.83
         all_hindcast_fidelity(m)=16
      END
      'nasa' : BEGIN
         dir=basedir+'/nasa'
         name='GEOS5_AGCM'
         all_codes(m)='NA'
         all_colors(m)='red'
         all_climate_colors(m)='black'
         z_name='lev'
         lon_name='lon'
         lat_name='lat'
         z_mult=1
         nt=4752
         pr_mult=86400.
         all_climate_fidelity(m)=0.66
         all_hindcast_fidelity(m)=18
      END
   ENDCASE

   box=[0,75,5,80]
   pr_infile=basedir+'/'+name+'.pr.20091020-20100110.lead_12-48hrs.nc'
   lon=OPEN_AND_EXTRACT(pr_infile,lon_name)
   lat=OPEN_AND_EXTRACT(pr_infile,lat_name)
   DEFINE_BOUNDARIES,box,lat,lon,box_tx,/LIMIT
   nlon=N_ELEMENTS(lon)
   nlat=N_ELEMENTS(lat)

   precip=OPEN_AND_EXTRACT(pr_infile,'pr',$
                           offset=[box_tx(1),box_tx(0),0],count=[nlon,nlat,nt])*pr_mult
   precip_aavg=fltarr(nt)
   FOR t=0,nt-1 DO $
      precip_aavg(t)=MEAN(precip(*,*,t))

;   all_lagone(m)=A_CORRELATE(precip_aavg,1)
   all_lagone(m)=SQRT(MEAN((precip_aavg(0:nt-2)-precip_aavg(1:nt-1))^2))

   print,all_codes(m),all_lagone(m),pr_mult

ENDFOR

psfile='/home/ss901165/idl/mjo_diabatic/synthesis/mjo_diabatic_synthesis_preciprmse.hindcast_fidelity.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=150,MARGIN=3500,XSIZE=16000,YSIZE=16000
GSET,XMIN=0,XMAX=9,YMIN=5,YMAX=21
FOR m=0,n_models-1 DO BEGIN
   GPLOT,X=all_lagone(m),Y=all_hindcast_fidelity(m),SYM=3,/NOLINES
   GPLOT,X=all_lagone(m),Y=all_hindcast_fidelity(m)+0.2,TEXT=all_codes(m),$
         COL=FSC_COLOR(all_colors(m))
ENDFOR
coeff=REGRESS(all_lagone,all_hindcast_fidelity,CONST=constant)
GPLOT,X=[0,9],Y=[0,9]*coeff(0)+constant,STYLE=2
GPLOT,X=6,Y=12,TEXT='r = '+STRMID(STRTRIM(STRING(CORRELATE(all_lagone,all_hindcast_fidelity)),1),0,5)
AXES,XSTEP=1,XMINOR=0.5,YSTEP=3,YMINOR=1,$
     XTITLE='RMSD in timestep rainfall in 2-day hindcasts (mm day!U-1!N)',$
     YTITLE='Fidelity in 20-day hindcasts (days)'
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/synthesis/mjo_diabatic_synthesis_preciprmse.climate_fidelity.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=150,MARGIN=3500,XSIZE=16000,YSIZE=16000
GSET,XMIN=0,XMAX=9,YMIN=0.4,YMAX=1
FOR m=0,n_models-1 DO BEGIN
   GPLOT,Y=all_climate_fidelity(m),X=all_lagone(m),SYM=3,SIZE=150,/NOLINES
   IF all_codes(m) eq 'NA' THEN BEGIN
      GPLOT,X=all_lagone(m)+0.5,Y=all_climate_fidelity(m)-0.015,TEXT=all_codes(m),$
         COL=FSC_COLOR(all_climate_colors(m))
   ENDIF ELSE $
      GPLOT,X=all_lagone(m),Y=all_climate_fidelity(m)+0.015,TEXT=all_codes(m),$
            COL=FSC_COLOR(all_climate_colors(m))
ENDFOR
coeff=REGRESS(all_lagone,all_climate_fidelity,CONST=constant)
GPLOT,X=[0,9],Y=[0,9]*coeff(0)+constant,STYLE=2
GPLOT,X=6,Y=0.7,TEXT='r = '+STRMID(STRTRIM(STRING(CORRELATE(all_lagone,all_climate_fidelity)),1),0,5)
AXES,XSTEP=1,XMINOR=0.5,YSTEP=0.1,YMINOR=0.05,$
     XTITLE='RMSD in timestep rainfall in 2-day hindcasts (mm day!U-1!N)',$
     YTITLE='Fidelity in 20-year climate simulations (days)',NDECS=2
PSCLOSE

STOP
END
