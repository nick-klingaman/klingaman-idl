PRO hadgem3kpp_ocnTcorr_plot_seasonal_cycle

analysis_infile='/home/ss901165/um_output6/kpp_ocean/3D_kpp_r4b1/test_run/ocnT_clim.nc'
forced_1_infile='/home/ss901165/um_output6/kpp_ocean/3D_kpp_r4b1/test_run/1_day/KPPocean.jan-dec_mmeans.i2-j1.nc'
forced_5_infile='/home/ss901165/um_output6/kpp_ocean/3D_kpp_r4b1/test_run/5_day/KPPocean.jan-dec_mmeans.i2-j1.nc'
forced_15_infile='/home/ss901165/um_output6/kpp_ocean/3D_kpp_r4b1/test_run/15_day/KPPocean.jan-dec_mmeans.i2-j1.nc'
coupled_infile='/home/ss901165/um_output6/xgspt/KPPocean.jan-dec_mmeans.i2-i6.nc'
old_coupled_infile='/home/ss901165/um_output6/xgspm/kpp_ocean/KPPocean.jan-dec_mmeans.i2-k1.nc'

pt=[-10,70,0]
forced_nyears=10
coupled_nmonths=60
old_coupled_nyears=20

analysis_lon=OPEN_AND_EXTRACT(analysis_infile,'longitude')
analysis_lat=OPEN_AND_EXTRACT(analysis_infile,'latitude')
analysis_z=OPEN_AND_EXTRACT(analysis_infile,'z')
analysis_lonpt=NEAREST(analysis_lon,pt(1))
analysis_latpt=NEAREST(analysis_lat,pt(0))
analysis_zpt=NEAREST(analysis_z,pt(2))

forced_lon=OPEN_AND_EXTRACT(forced_15_infile,'longitude')
forced_lat=OPEN_AND_EXTRACT(forced_15_infile,'latitude')
forced_z=OPEN_AND_EXTRACT(forced_15_infile,'z')
forced_lonpt=NEAREST(forced_lon,pt(1))
forced_latpt=NEAREST(forced_lat,pt(0))
forced_zpt=NEAREST(forced_z,pt(2))

coupled_lon=OPEN_AND_EXTRACT(coupled_infile,'longitude')
coupled_lat=OPEN_AND_EXTRACT(coupled_infile,'latitude')
coupled_z=OPEN_AND_EXTRACT(coupled_infile,'z')
coupled_lonpt=NEAREST(coupled_lon,pt(1))
coupled_latpt=NEAREST(coupled_lat,pt(0))
coupled_zpt=NEAREST(coupled_z,pt(2))

analysis_T=REFORM(OPEN_AND_EXTRACT(analysis_infile,'temperature',$
                                   offset=[analysis_lonpt,analysis_latpt,analysis_zpt,0],$
                                   count=[1,1,1,360]))
analysis_T_mmean=fltarr(12)
FOR i=0,11 DO $
   analysis_T_mmean(i)=MEAN(analysis_T(i*30:(i+1)*30-1))

forced_15_T=REFORM(OPEN_AND_EXTRACT(forced_15_infile,'T',$
                                 offset=[forced_lonpt,forced_latpt,forced_zpt,0,0],$
                                 count=[1,1,1,12,forced_nyears]))
forced_5_T=REFORM(OPEN_AND_EXTRACT(forced_5_infile,'T',$
                                   offset=[forced_lonpt,forced_latpt,forced_zpt,0,0],$
                                   count=[1,1,1,12,forced_nyears]))
forced_1_T=REFORM(OPEN_AND_EXTRACT(forced_1_infile,'T',$
                                   offset=[forced_lonpt,forced_latpt,forced_zpt,0,0],$
                                   count=[1,1,1,12,forced_nyears]))
coupled_T=REFORM(OPEN_AND_EXTRACT(coupled_infile,'T',$
                                  offset=[coupled_lonpt,coupled_latpt,coupled_zpt,0],$
                                  count=[1,1,1,coupled_nmonths]))
coupled_nyears=coupled_nmonths/12+1
IF coupled_nmonths MOD 12 eq 0 THEN $
   coupled_nyears=coupled_nyears-1
coupled_T_reform=fltarr(12,coupled_nyears)
FOR i=0,coupled_nyears-1 DO BEGIN
   IF (i+1)*12 le coupled_nmonths THEN BEGIN
      coupled_T_reform(*,i)=coupled_T(i*12:(i+1)*12-1)
   ENDIF ELSE BEGIN
      coupled_T_reform(0:(coupled_nmonths MOD 12)-1,i)=coupled_T(i*12:coupled_nmonths-1)
      coupled_T_reform((coupled_nmonths MOD 12):11,i)=!Values.F_NaN
   ENDELSE
ENDFOR

old_coupled_T=REFORM(OPEN_AND_EXTRACT(old_coupled_infile,'T',$
                                      offset=[coupled_lonpt,coupled_latpt,coupled_zpt,0],$
                                      count=[1,1,1,old_coupled_nyears*12]))
old_coupled_T_reform=fltarr(12,old_coupled_nyears)
FOR i=0,old_coupled_nyears-1 DO $
   old_coupled_T_reform(*,i)=old_coupled_T(i*12:(i+1)*12-1)

psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_ocnTcorr_plot_seasonal_cycle.pt_10S_70E_sst.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120
GSET,XMIN=0,XMAX=13,YMIN=25,YMAX=30
;GPLOT,X=findgen(360)/30.+0.5,Y=analysis_T,COL=FSC_COLOR('black'),THICK=200
GPLOT,X=indgen(13)+0.5,Y=REFORM([analysis_T_mmean(*),analysis_T_mmean(0)]),COL=FSC_COLOR('black'),THICK=200
FOR i=0,forced_nyears-1 DO BEGIN
   IF i ne forced_nyears-1 THEN BEGIN
      GPLOT,X=indgen(13)+0.5,Y=REFORM([forced_15_T(*,i),forced_5_T(0,i+1)]),$
            COL=FSC_COLOR('black'),THICK=80,STYLE=2
   ENDIF ELSE $
      GPLOT,X=indgen(12)+0.5,Y=REFORM(forced_15_T(*,i)),COL=FSC_COLOR('black'),THICK=80,STYLE=2
ENDFOR
FOR i=0,forced_nyears-1 DO BEGIN
   IF i ne forced_nyears-1 THEN BEGIN
      GPLOT,X=indgen(13)+0.5,Y=REFORM([forced_5_T(*,i),forced_5_T(0,i+1)]),$
            COL=FSC_COLOR('blue'),THICK=80,STYLE=2
   ENDIF ELSE $
      GPLOT,X=indgen(12)+0.5,Y=REFORM(forced_5_T(*,i)),COL=FSC_COLOR('blue'),THICK=80,STYLE=2
ENDFOR
FOR i=0,forced_nyears-1 DO BEGIN
   IF i ne forced_nyears-1 THEN BEGIN
      GPLOT,X=indgen(13)+0.5,Y=REFORM([forced_1_T(*,i),forced_1_T(0,i+1)]),$
            COL=FSC_COLOR('red'),THICK=80,STYLE=2
   ENDIF ELSE $
      GPLOT,X=indgen(12)+0.5,Y=REFORM(forced_1_T(*,i)),COL=FSC_COLOR('blue'),THICK=80,STYLE=2
ENDFOR

CS,SCALE=2,NCOLS=coupled_nyears+1
FOR i=0,coupled_nyears-1 DO BEGIN
   IF i ne coupled_nyears-1 THEN BEGIN
      GPLOT,X=indgen(13)+0.5,Y=REFORM([coupled_T_reform(*,i),coupled_T_reform(0,i+1)]),$
            COL=i+2,THICK=100
   ENDIF ELSE $
      GPLOT,X=indgen(12)+0.5,Y=REFORM(coupled_T_reform(*,i)),COL=i+2,THICK=100
ENDFOR
AXES,XVALS=indgen(13)+0.5,XLABELS=['jan','feb','mar','apr','may','jun','jul','aug','sep','oct',$
                                   'nov','dec','jan'],YSTEP=0.5,YMINOR=0.25
;CS,SCALE=2,NCOLS=old_coupled_nyears+1
;FOR i=0,old_coupled_nyears-1 DO BEGIN
;   IF i ne old_coupled_nyears-1 THEN BEGIN
;      GPLOT,X=indgen(13)+0.5,Y=REFORM([old_coupled_T_reform(*,i),old_coupled_T_reform(0,i+1)]),$
;            COL=i+2,THICK=50,STYLE=2
;   ENDIF ELSE $
;      GPLOT,X=indgen(12)+0.5,Y=REFORM(old_coupled_T_reform(*,i)),COL=i+2,THICK=50,STYLE=2
;ENDFOR
PSCLOSE

STOP
END
