PRO qld_higem_make_qld_region_aavg_precip_ts

; Make an area-averaged precipitation timeseries for Queensland

infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.may-apr_dmeans.h9-w8.precip.aus_domain.nc'
outfile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.may-apr_dmeans.h9-w8.precip.qld_region_aavg.nc'
mask_file='/home/ss901165/datasets/SILO/n144/SILO_qld_region_mask.n144.may-apr.nc'
lsm_file='/home/ss901165/um_output/mask_n144_higam.nc'

higem_nyears=149
higem_ntime=360

; Read regional mask data
mask_longitude=OPEN_AND_EXTRACT(mask_file,'longitude')
mask_latitude=OPEN_AND_EXTRACT(mask_file,'latitude')
mask=OPEN_AND_EXTRACT(mask_file,'silo_regions')
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)
n_regions=MAX(mask)

mask_revlat=fltarr(mask_nlon,mask_nlat)
FOR i=0,mask_nlat-1 DO $
   mask_revlat(*,i)=mask(*,mask_nlat-i-1)

; Read land/sea mask data
lsm_longitude=OPEN_AND_EXTRACT(lsm_file,'longitude')
lsm_latitude=OPEN_AND_EXTRACT(lsm_file,'latitude')
DEFINE_BOUNDARIES,[mask_latitude(mask_nlat-1),mask_longitude(0),mask_latitude(0),mask_longitude(mask_nlon-1)],$
                  lsm_latitude,lsm_longitude,lsm_box_tx,/LIMIT
lsm_nlon=N_ELEMENTS(lsm_longitude)
lsm_nlat=N_ELEMENTS(lsm_latitude)
lsm=OPEN_AND_EXTRACT(lsm_file,'lsm',$
                     offset=[lsm_box_tx(1),lsm_box_tx(0),0,0],$
                     count=[lsm_nlon,lsm_nlat,1,1])

higem_qldaavg_byregion=fltarr(higem_ntime,higem_nyears,n_regions)
higem_qldaavg=fltarr(higem_ntime,higem_nyears)

longitude=OPEN_AND_EXTRACT(infile,'longitude')
latitude=OPEN_AND_EXTRACT(infile,'latitude')
DEFINE_BOUNDARIES,[mask_latitude(mask_nlat-1),mask_longitude(0),mask_latitude(0),mask_longitude(mask_nlon-1)],$
                  latitude,longitude,qld_box_tx,/LIMIT
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)

higem_precip=REFORM(OPEN_AND_EXTRACT(infile,'precip',offset=[qld_box_tx(1),qld_box_tx(0),0,0],$
                                     count=[n_lon,n_lat,higem_ntime,higem_nyears]))

FOR j=0,n_regions-1 DO BEGIN
   FOR k=0,higem_ntime-1 DO BEGIN
      FOR m=0,higem_nyears-1 DO BEGIN
         temp=REFORM(higem_precip(*,*,k,m))
         temp[where(mask_revlat ne j+1 or lsm eq 0)]=!Values.F_NaN
         higem_qldaavg_byregion(k,m,j)=MEAN(temp,/NaN)
      ENDFOR
   ENDFOR
ENDFOR

FOR k=0,higem_ntime-1 DO BEGIN
   FOR m=0,higem_nyears-1 DO BEGIN
      temp=REFORM(higem_precip(*,*,k,m))
      temp[where(mask_revlat eq 0 or lsm eq 0)]=!Values.F_NaN
      higem_qldaavg(k,m)=MEAN(temp,/NAN)
   ENDFOR
ENDFOR
   
; Export the results to a netCDF file
out_id=NCDF_CREATE(outfile,/CLOBBER)
out_dimids=intarr(3)
out_varids=intarr(5)
out_dimids(0)=NCDF_DIMDEF(out_id,'year',higem_nyears)
out_dimids(1)=NCDF_DIMDEF(out_id,'time',higem_ntime)
out_dimids(2)=NCDF_DIMDEF(out_id,'region',n_regions)
out_varids(0)=NCDF_VARDEF(out_id,'year',[out_dimids(0)])
out_varids(1)=NCDF_VARDEF(out_id,'time',[out_dimids(1)])
out_varids(2)=NCDF_VARDEF(out_id,'region',[out_dimids(2)])
out_varids(3)=NCDF_VARDEF(out_id,'rain_aavg_region',[out_dimids(1),out_dimids(0),out_dimids(2)])
out_varids(4)=NCDF_VARDEF(out_id,'rain_aavg_allqld',[out_dimids(1),out_dimids(0)])

NCDF_CONTROL,out_id,/ENDEF
NCDF_VARPUT,out_id,out_varids(0),indgen(higem_nyears)
NCDF_VARPUT,out_id,out_varids(1),indgen(higem_ntime)
NCDF_VARPUT,out_id,out_varids(2),indgen(n_regions)+1
NCDF_VARPUT,out_id,out_varids(3),higem_qldaavg_byregion
NCDF_VARPUT,out_id,out_varids(4),higem_qldaavg
NCDF_CLOSE,out_id

STOP

END

