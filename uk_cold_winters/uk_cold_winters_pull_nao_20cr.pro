PRO uk_cold_winters_pull_nao_20cr

; Input file
twentyc_infile='/home/ss901165/datasets/20THC_REANALYSIS/mslp/20thc_reanalysis.jan-dec_mmeans.1871-2010.mslp.twod.nc'

; Box to use for area-averaging
ice_box=[63,339,68,343]
gbr_box=[33,352,39,357]

; Land/sea mask
mask_infile='/home/ss901165/datasets/20THC_REANALYSIS/mask_t62.nc'

; Get latitude and longitude
latitude=OPEN_AND_EXTRACT(twentyc_infile,'latitude')
longitude=OPEN_AND_EXTRACT(twentyc_infile,'longitude')
DEFINE_BOUNDARIES,ice_box,latitude,longitude,ice_box_tx
DEFINE_BOUNDARIES,gbr_box,latitude,longitude,gbr_box_tx
n_lat=N_ELEMENTS(latitude)
n_lon=N_ELEMENTS(longitude)

; Get temperatures
n_time=12
n_year=140
ice_mslp=REFORM(OPEN_AND_EXTRACT(twentyc_infile,'PMSL',$
                                 offset=[ice_box_tx(1),ice_box_tx(0),0,0],$
                                 count=[ice_box_tx(3)-ice_box_tx(1)+1,$
                                        ice_box_tx(2)-ice_box_tx(0)+1,$
                                        n_time,n_year]))/100.
gbr_mslp=REFORM(OPEN_AND_EXTRACT(twentyc_infile,'PMSL',$
                                 offset=[gbr_box_tx(1),gbr_box_tx(0),0,0],$
                                 count=[gbr_box_tx(3)-gbr_box_tx(1)+1,$
                                        gbr_box_tx(2)-gbr_box_tx(0)+1,$
                                        n_time,n_year]))/100.

; Take area-average over all land points
ice_aavg=fltarr(n_time,n_year)
ice_aavg_anom=fltarr(n_time,n_year)
gbr_aavg=fltarr(n_time,n_year)
gbr_aavg_anom=fltarr(n_time,n_year)
FOR i=0,n_time-1 DO BEGIN
   FOR j=0,n_year-1 DO BEGIN
      ice_aavg(i,j)=MEAN(ice_mslp(*,*,i,j),/NaN)
      gbr_aavg(i,j)=MEAN(gbr_mslp(*,*,i,j),/Nan)
   ENDFOR
   ice_aavg_anom(i,*)=ice_aavg(i,*)-MEAN(ice_aavg(i,*))
   gbr_aavg_anom(i,*)=gbr_aavg(i,*)-MEAN(gbr_aavg(i,*))
   ice_aavg_anom(i,*)=ice_aavg_anom(i,*)/STDDEV(ice_aavg_anom(i,*))
   gbr_aavg_anom(i,*)=gbr_aavg_anom(i,*)/STDDEV(gbr_aavg_anom(i,*))
ENDFOR

nao_index=gbr_aavg_anom-ice_aavg_anom

outfile='/home/ss901165/datasets/20THC_REANALYSIS/mslp/20thc_reanalysis.jan-dec_mmeans.1871-2010.nao_index.twod.nc'
id=NCDF_CREATE(outfile,/CLOBBER)
tdimid=NCDF_DIMDEF(id,'month',n_time)
ydimid=NCDF_DIMDEF(id,'year',n_year)
tvarid=NCDF_VARDEF(id,'month',[tdimid])
yvarid=NCDF_VARDEF(id,'year',[ydimid])
cetvarid=NCDF_VARDEF(id,'nao_index',[tdimid,ydimid])
NCDF_CONTROL,id,/ENDEF
NCDF_VARPUT,id,tvarid,indgen(n_time)
NCDF_VARPUT,id,yvarid,indgen(n_year)
NCDF_VARPUT,id,cetvarid,nao_index

NCDF_CLOSE,id

STOP
END

