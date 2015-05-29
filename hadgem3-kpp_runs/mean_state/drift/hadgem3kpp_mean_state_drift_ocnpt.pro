PRO hadgem3kpp_mean_state_drift_ocnpt
  
; Plot time-depth cross-section of temperatures at a single ocean gridpoint.

T_infile='/home/ss901165/um_output6/xgspe/kpp_ocean/kpp_concat_T.nc'
hmix_infile='/home/ss901165/um_output6/xgspe/kpp_ocean/kpp_concat_hmix.nc'

pt=[-12.5,85]
time_offset=0
n_days=720
levels=['15','16','17','18','19','20','21','22','23','24','25','26','27','28','29','30','31']

longitude=OPEN_AND_EXTRACT(T_infile,'longitude')
latitude=OPEN_AND_EXTRACT(T_infile,'latitude')
z=OPEN_AND_EXTRACT(T_infile,'z')
lonpt=NEAREST(longitude,pt(1))
latpt=NEAREST(latitude,pt(0))
n_z=N_ELEMENTS(z)

ocnT=REFORM(OPEN_AND_EXTRACT(T_infile,'T',$
                             offset=[lonpt,latpt,0,time_offset],$
                             count=[1,1,n_z,n_days]))
ocnhmix=REFORM(OPEN_AND_EXTRACT(hmix_infile,'hmix',$
                                offset=[lonpt,latpt,time_offset],$
                                count=[1,1,n_days]))


ocnT_rev=fltarr(n_days,n_z)
FOR i=0,n_days-1 DO $
   ocnT_rev(i,*)=ocnT(*,i)

psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/drift/hadgem3kpp_mean_state_drift_ocnpt.ocnT.lon'+STRTRIM(STRING(pt(1)),1)+'_lat'+STRTRIM(STRING(pt(0)),1)+'.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=400
GSET,XMIN=0,XMAX=n_days,YMIN=MIN(z),YMAX=MAX(z)
CS,SCALE=1,NCOLS=N_ELEMENTS(levels)+1
LEVS,MANUAL=levels
CON,X=indgen(n_days)+0.5,Y=z,FIELD=ocnT_rev,/NOLINES
GPLOT,X=indgen(n_days)+0.5,Y=(-1.)*ocnhmix,STYLE=2
AXES,XSTEP=90,XMINOR=30,YSTEP=10,YMINOR=5
PSCLOSE

STOP
END

