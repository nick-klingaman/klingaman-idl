PRO hadgem3kpp_make_dmean_clim_sst

kpp_infile='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-20.sst.nc'
outfile='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim_forUM.years1-20.sst.nc'

kpp_sst=OPEN_AND_EXTRACT(kpp_infile,'sst')
longitude=OPEN_AND_EXTRACT(kpp_infile,'longitude')
latitude=OPEN_AND_EXTRACT(kpp_infile,'latitude')
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)

out_sst=fltarr(n_lon,n_lat,360)
FOR i=0,n_lon-1 DO BEGIN
   FOR j=0,n_lat-1 DO BEGIN
      temp=[REFORM(kpp_sst(i,j,*)),REFORM(kpp_sst(i,j,*))]
      temp=SMOOTH(temp,31)
      out_sst(i,j,*)=[temp(360:374),temp(15:359)]
   ENDFOR
ENDFOR
out_sst[where(kpp_sst le 275)]=kpp_sst[where(kpp_sst le 275)]

id=NCDF_CREATE(outfile,/CLOBBER)
dimids=intarr(3)
dimids(0)=NCDF_DIMDEF(id,'longitude',n_lon)
dimids(1)=NCDF_DIMDEF(id,'latitude',n_lat)
dimids(2)=NCDF_DIMDEF(id,'t',360)
varids=intarr(4)
varids(0)=NCDF_VARDEF(id,'longitude',[dimids(0)])
varids(1)=NCDF_VARDEF(id,'latitude',[dimids(1)])
varids(2)=NCDF_VARDEF(id,'t',[dimids(2)])
varids(3)=NCDF_VARDEF(id,'sst',[dimids(0),dimids(1),dimids(2)])
NCDF_CONTROL,id,/ENDEF
NCDF_VARPUT,id,varids(0),longitude
NCDF_VARPUT,id,varids(1),latitude
NCDF_VARPUT,id,varids(2),findgen(360)+0.5
NCDF_VARPUT,id,varids(3),out_sst
NCDF_CLOSE,id

STOP
END
