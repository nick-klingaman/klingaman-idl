PRO kpp_scm_sfcorr_tests_sst

; Plot SST from SCM test of [s/f]corr timescales

basedir='/home/ss901165/um_output6/kpp_ocean/3D_kpp_r4b3/test_run_scm'
files=['/TS_relax_output/clim.nc',$
       '/TS_free_output/clim.nc',$
       '/TS_relaxind_output/clim.nc',$
       '/TS_freeind_output/clim.nc',$
       '/TS_free_360relax_output/clim.nc',$
       '/TS_free_720relax_output/clim.nc']
names=['TS_15day_relax','TS_15day_free','TS_15day_relax_1yr','TS_15day_free_1yr','TS_15day_free_360day_relax','TS_15day_free_720day_relax']
colors=['blue','cyan','red','orange','dodgerblue','purple']

longitude=219.375
latitude=51.25

clim_file=basedir+'/ocnT_clim.nc'
clim_lon=OPEN_AND_EXTRACT(clim_file,'longitude')
clim_lat=OPEN_AND_EXTRACT(clim_file,'latitude')
clim_lonpt=NEAREST(clim_lon,longitude)
clim_latpt=NEAREST(clim_lat,latitude)
clim_sst=REFORM(OPEN_AND_EXTRACT(clim_file,'temperature',$
                                 offset=[clim_lonpt,clim_latpt,0,0],count=[1,1,1,360]))

n_files=N_ELEMENTS(files)
kpp_sst=fltarr(n_files,360)
FOR i=0,n_files-1 DO BEGIN
   this_file=basedir+files(i)
   kpp_sst(i,*)=REFORM(OPEN_AND_EXTRACT(this_file,'T',$
                                        offset=[0,0,0,0],count=[1,1,1,360]))
ENDFOR

psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_sfcorr_tests_sst.sst_ts.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,TFONT=2
GSET,XMIN=0,XMAX=360,YMIN=5,YMAX=18,TITLE='Clim SST at '+STRTRIM(STRING(longitude),1)+'E, '+$
     STRTRIM(STRING(latitude),1)+'N from forced SCM exps'
GPLOT,X=indgen(360)+0.5,Y=clim_sst,COL=FSC_COLOR('black')
FOR i=0,n_files-1 DO $
   GPLOT,X=indgen(360)+0.5,Y=REFORM(kpp_sst(i,*)),COL=FSC_COLOR(colors(i))
AXES,XSTEP=30,XMINOR=15,YMINOR=0.5,YSTEP=1,YTITLE='Sea-surface temperature (degrees Celsius)',$
     XTITLE='Day of year'
GLEGEND,LEGPOS=1,LABELS=REVERSE(names),COL=REVERSE(FSC_COLOR(colors))
PSCLOSE

STOP
END
