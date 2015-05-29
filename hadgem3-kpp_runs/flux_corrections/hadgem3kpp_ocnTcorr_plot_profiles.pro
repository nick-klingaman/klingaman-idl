PRO hadgem3kpp_ocnTcorr_plot_profiles

analysis_infile='/home/ss901165/um_output6/kpp_ocean/3D_kpp_r4b1/test_run/ocnT_clim.nc'
forced_15_infile='/home/ss901165/um_output6/kpp_ocean/3D_kpp_r4b1/test_run/15_day/KPPocean.jan-dec_mmeans.i2-j1.nc'
forced_1_infile='/home/ss901165/um_output6/kpp_ocean/3D_kpp_r4b1/test_run/1_day/KPPocean.jan-dec_mmeans.i2-j1.nc'
coupled_1_infile='/home/ss901165/um_output6/xgspt/KPPocean.jan-dec_mmeans.i2-i6.nc'
coupled_15_infile='/home/ss901165/um_output6/xgspt/15_day/KPPocean.jan-dec_mmeans.i2-i6.nc'
coupled_relax_15_infile='/home/ss901165/um_output6/xihva/KPPocean.jan-dec_mmeans.i2-i6.nc'
old_coupled_infile='/home/ss901165/um_output6/xgspm/kpp_ocean/KPPocean.jan-dec_mmeans.i2-k1.nc'

pt=[-10,70]
forced_nyears=10
coupled_15_nyears=5
coupled_1_nyears=3
coupled_relax_15_nyears=4
old_coupled_nyears=20
month=5 ; as offset

analysis_lon=OPEN_AND_EXTRACT(analysis_infile,'longitude')
analysis_lat=OPEN_AND_EXTRACT(analysis_infile,'latitude')
analysis_z=OPEN_AND_EXTRACT(analysis_infile,'z')
analysis_lonpt=NEAREST(analysis_lon,pt(1))
analysis_latpt=NEAREST(analysis_lat,pt(0))
analysis_nz=N_ELEMENTS(analysis_z)

forced_lon=OPEN_AND_EXTRACT(forced_15_infile,'longitude')
forced_lat=OPEN_AND_EXTRACT(forced_15_infile,'latitude')
forced_z=OPEN_AND_EXTRACT(forced_15_infile,'z')
forced_lonpt=NEAREST(forced_lon,pt(1))
forced_latpt=NEAREST(forced_lat,pt(0))
forced_nz=N_ELEMENTS(forced_z)

coupled_lon=OPEN_AND_EXTRACT(coupled_15_infile,'longitude')
coupled_lat=OPEN_AND_EXTRACT(coupled_15_infile,'latitude')
coupled_z=OPEN_AND_EXTRACT(coupled_15_infile,'z')
coupled_lonpt=NEAREST(coupled_lon,pt(1))
coupled_latpt=NEAREST(coupled_lat,pt(0))
coupled_nz=N_ELEMENTS(coupled_z)

coupled_relax_lon=OPEN_AND_EXTRACT(coupled_relax_15_infile,'longitude')
coupled_relax_lat=OPEN_AND_EXTRACT(coupled_relax_15_infile,'latitude')
coupled_relax_z=OPEN_AND_EXTRACT(coupled_relax_15_infile,'z')
coupled_relax_lonpt=NEAREST(coupled_relax_lon,pt(1))
coupled_relax_latpt=NEAREST(coupled_relax_lat,pt(0))
coupled_relax_z=coupled_relax_z[where(coupled_relax_z ge -200)]
coupled_relax_nz=N_ELEMENTS(coupled_relax_z)

analysis_T_dmean=REFORM(OPEN_AND_EXTRACT(analysis_infile,'temperature',$
                                         offset=[analysis_lonpt,analysis_latpt,0,month*30],$
                                         count=[1,1,analysis_nz,30]))
analysis_T=fltarr(analysis_nz)
FOR i=0,analysis_nz-1 DO $
   analysis_T(i)=MEAN(analysis_T_dmean(i,*))

forced_15_T=REFORM(OPEN_AND_EXTRACT(forced_15_infile,'T',$
                                    offset=[forced_lonpt,forced_latpt,0,month,0],$
                                    count=[1,1,forced_nz,1,forced_nyears]))
forced_1_T=REFORM(OPEN_AND_EXTRACT(forced_1_infile,'T',$
                                   offset=[forced_lonpt,forced_latpt,0,month,0],$
                                   count=[1,1,forced_nz,1,forced_nyears]))

coupled_15_T=fltarr(coupled_nz,coupled_15_nyears)
coupled_relax_15_T=fltarr(coupled_relax_nz,coupled_relax_15_nyears)
coupled_1_T=fltarr(coupled_nz,coupled_1_nyears)
FOR i=0,coupled_15_nyears-1 DO $
   coupled_15_T(*,i)=REFORM(OPEN_AND_EXTRACT(coupled_15_infile,'T',$
                                             offset=[coupled_lonpt,coupled_latpt,0,i*12+month],$
                                             count=[1,1,forced_nz,1]))
FOR i=0,coupled_1_nyears-1 DO $
   coupled_1_T(*,i)=REFORM(OPEN_AND_EXTRACT(coupled_1_infile,'T',$
                                            offset=[coupled_lonpt,coupled_latpt,0,i*12+month],$
                                            count=[1,1,forced_nz,1]))

FOR i=0,coupled_relax_15_nyears-1 DO $
   coupled_relax_15_T(*,i)=REFORM(OPEN_AND_EXTRACT(coupled_relax_15_infile,'T',$
                                                   offset=[coupled_relax_lonpt,coupled_relax_latpt,0,i*12+month],$
                                                   count=[1,1,coupled_relax_nz,1]))

old_coupled_T=fltarr(coupled_nz,old_coupled_nyears)
FOR i=0,old_coupled_nyears-1 DO $
   old_coupled_T(*,i)=REFORM(OPEN_AND_EXTRACT(old_coupled_infile,'T',$
                                              offset=[coupled_lonpt,coupled_latpt,0,i*12+month],$
                                              count=[1,1,forced_nz,1]))

psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_ocnTcorr_plot_profiles.pt_10S_70E_month'+STRTRIM(STRING(month),1)+'.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,/PORTRAIT
GSET,XMIN=12,XMAX=28,YMIN=MAX(ABS(analysis_z)),YMAX=MIN(ABS(analysis_z))
GPLOT,X=analysis_T,Y=ABS(analysis_z),COL=FSC_COLOR('black'),THICK=200
AXES,XSTEP=2,XMINOR=1,YSTEP=-10,YMINOR=-5,YTITLE='Depth',XTITLE='Analysis temperature',/NOUPPER
GSET,XMIN=-3,XMAX=3,YMIN=MAX(ABS(analysis_Z)),YMAX=MIN(ABS(analysis_z))
FOR i=0,forced_nyears-1 DO BEGIN
   GPLOT,X=forced_15_T(*,i)-analysis_T,Y=ABS(forced_z),COL=FSC_COLOR('black'),THICK=50,STYLE=2
   GPLOT,X=forced_1_T(*,i)-analysis_T,Y=ABS(forced_z),COL=FSC_COLOR('red'),THICK=50,STYLE=2
ENDFOR
;CS,SCALE=2,NCOLS=old_coupled_nyears
;FOR i=0,old_coupled_nyears-1 DO $
;   GPLOT,X=old_coupled_T(*,i),Y=ABS(coupled_z),COL=i+2,THICK=50,STYLE=1
CS,SCALE=2,NCOLS=coupled_15_nyears
FOR i=0,coupled_15_nyears-1 DO $
   GPLOT,X=coupled_15_T(*,i)-analysis_T,Y=ABS(coupled_z),COL=i+2,THICK=100,STYLE=1
CS,SCALE=2,NCOLS=coupled_1_nyears
FOR i=0,coupled_1_nyears-1 DO $
   GPLOT,X=coupled_1_T(*,i)-analysis_T,Y=ABS(coupled_z),COL=i+2,THICK=100,STYLE=2
CS,SCALE=2,NCOLS=coupled_relax_15_nyears
FOR i=0,coupled_relax_15_nyears-1 DO BEGIN
   diff=fltarr(analysis_nz)
   FOR j=0,analysis_nz-1 DO BEGIN
      lev=NEAREST(coupled_relax_z,analysis_z(j))
      diff(j)=coupled_relax_15_T(lev,i)-analysis_T(j)
   ENDFOR
   GPLOT,X=diff,Y=ABS(analysis_z),COL=i+2,THICK=100,STYLE=0
ENDFOR
AXES,XSTEP=0.5,XMINOR=0.25,YSTEP=-10,YMINOR=-5,YTITLE='Depth',XTITLE='Difference in temperature from analysis',/ONLYUPPER,NDECS=1
GLEGEND,labels=REVERSE(['Analysis','Forced 15-day','Forced 1-day','Coupled 15-day','Coupled 1-day','Coupled xact 15-day']),STYLE=REVERSE([0,2,2,1,2,0]),COL=REVERSE([FSC_COLOR('black'),FSC_COLOR('black'),FSC_COLOR('red'),FSC_COLOR('purple'),FSC_COLOR('purple'),FSC_COLOR('purple')]),LEGPOS=11
PSCLOSE

STOP
END
