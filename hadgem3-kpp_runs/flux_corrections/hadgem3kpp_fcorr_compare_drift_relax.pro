PRO hadgem3kpp_fcorr_compare_drift_relax

drift_file='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/heat_corrections/hadgem3_1.5xentrain_allmembers_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421_ind.jan-dec_mmeans.flxcorr.n96.nc'
relax_file='/home/ss901165/um_output6/kpp_ocean/3D_kpp_r4b1/test_run/temp.nc'

pt=[-10,70]

longitude=OPEN_AND_EXTRACT(drift_file,'longitude')
latitude=OPEN_AND_EXTRACT(drift_file,'latitude')
lat_pt=NEAREST(latitude,pt(0))
lon_pt=NEAREST(longitude,pt(1))

relax_time=OPEN_AND_EXTRACT(relax_file,'time')
relax_ntime=N_ELEMENTS(relax_time)

drift_fcorr=REFORM(OPEN_AND_EXTRACT(drift_file,'expcorr',offset=[lon_pt,lat_pt,0,0,0],count=[1,1,60,12,10]))
relax_fcorr=REFORM(OPEN_AND_EXTRACT(relax_file,'tinc_fcorr',offset=[lon_pt,lat_pt,0,0],count=[1,1,60,relax_ntime]))*720.

relax_nyears=relax_ntime/360
relax_sfc_fcorr_mmean=fltarr(12,relax_nyears)
FOR i=0,relax_nyears-1 DO BEGIN
   FOR j=0,11 DO $
      relax_sfc_fcorr_mmean(j,i)=MEAN(relax_fcorr(0,j*30+i*360:(j+1)*30+i*360-1))
ENDFOR
drift_sfc_fcorr_mmean=REFORM(drift_fcorr(0,*,*))

psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_drift_relax.sfc_10S70E.ps'
PSOPEN,file=psfile,TFONT=2,CHARSIZE=120
GSET,XMIN=1,XMAX=13,YMIN=-4,YMAX=4
CS,SCALE=26,NCOLS=12
FOR i=0,9 DO $
   GPLOT,X=indgen(12)+1.3,Y=REFORM(drift_sfc_fcorr_mmean(*,i)),SYM=4,COL=i+2,/NOLINES
ensmean=fltarr(12)
FOR i=0,11 DO $
   ensmean(i)=MEAN(drift_sfc_fcorr_mmean(i,*))
print,ensmean
GPLOT,X=indgen(12)+1.5,Y=ensmean,COL=FSC_COLOR('black'),SYM=4
FOR i=0,relax_nyears-1 DO $
   GPLOT,X=indgen(12)+1.7,Y=REFORM(relax_sfc_fcorr_mmean(*,i)),SYM=3,COL=i+2,/NOLINES
FOR i=0,11 DO $
   ensmean(i)=MEAN(relax_sfc_fcorr_mmean(i,*))
FOR i=1,12 DO $
   GPLOT,X=[i,i],Y=[-4,4],STYLE=1,COL=FSC_COLOR('black'),THICK=50
GPLOT,X=indgen(12)+1.5,Y=ensmean,SYM=3,COL=FSC_COLOR('black')
AXES,XVALS=indgen(12)+1.5,XLABELS=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'],$
     XTITLE='Month',YTITLE='Monthly surface temperature correction (K)',YSTEP=1,YMINOR=0.5,NDECS=2
PSCLOSE

STOP
END

