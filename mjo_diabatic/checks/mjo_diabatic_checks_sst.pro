PRO mjo_diabatic_checks_sst

;ecmwf,metum,spcam
;ECMWF_IFS,MetUM,SPCAM3.0

model_dirs=['cancm4','giss','iis','miroc','mri','nasa','nicam','nrl','cnrm_atmos','cam5','cam5zm','ecearth']
model_descs=['CanCM4','ModelE','gfs2_iis','miroc5','MRI-AGCM','GEOS5_AGCM','nicam','NGEM01','CNRM','NCAR.CAM5','CAM5ZMMicroCAPT','ecearth3']
model_colors=['dodgerblue','violetred','darkgrey','orange','blue','cyan','olive','brown','navy','deeppink','steelblue','limegreen']

model_lonnames=['longitude','longitude','longitude','longitude','lon','lon','longitude','longitude','longitude','longitude','longitude','lon']
model_latnames=['latitude','latitude','latitude','latitude','lat','lat','latitude','latitude','latitude','latitude','latitude','lat']

n_models=N_ELEMENTS(model_dirs)
n_times=160
sst_ts=fltarr(n_models,n_times)
FOR i=0,n_models-1 DO BEGIN
   infile='/home/ss901165/um_output6/mjodiab_20day/'+model_dirs(i)+'/20091015/'+model_descs(i)+'.tos.20091015.00Z.nc'

   longitude=OPEN_AND_EXTRACT(infile,model_lonnames(i))
   latitude=OPEN_AND_EXTRACT(infile,model_latnames(i))
   lonpt=NEAREST(longitude,75.)
   latpt=NEAREST(latitude,0.)
   print,lonpt,latpt

   sst_ts(i,*)=REFORM(OPEN_AND_EXTRACT(infile,'tos',$
                                       offset=[lonpt,latpt,0],count=[1,1,n_times]))
   IF sst_ts(i,0) lt 273 THEN $
      sst_ts(i,*)=sst_ts(i,*)+273.15
ENDFOR

psfile='/home/ss901165/idl/mjo_diabatic/checks/mjo_diabatic_checks_sst.all_models_ts.ps'
PSOPEN,file=psfile,TFONT=6,CHARSIZE=150,FONT=6,XOFFSET=1000,MARGIN=2500
GSET,XMIN=0,XMAX=n_times,YMIN=301,YMAX=305,TITLE='SST timeseries for init 20091015 at 0N,75E'
FOR i=0,n_models-1 DO $
   GPLOT,X=indgen(n_times)+0.5,Y=REFORM(sst_ts(i,*)),COL=FSC_COLOR(model_colors(i))
AXES,XSTEP=8,YSTEP=0.4,YMINOR=0.2,XTITLE='Output timestep (every 3 hours)',YTITLE='SST (K)'
PSCLOSE

STOP
END

