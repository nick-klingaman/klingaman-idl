PRO qld_higem_make_qld_aavg_precip_ts

; Make an area-averaged precipitation timeseries for Queensland

infile='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.may-apr_dmeans.o2-r3.precip.nc'
outfile='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.may-apr_dmeans.o2-r3.precip.qld_region_aavg.nc'
mask_file='/home/ss901165/um_output/mask_n144_higam.nc'

higem_nyears=31
qld_box=[-30,138,-10,154]

longitude=OPEN_AND_EXTRACT(infile,'longitude')
latitude=OPEN_AND_EXTRACT(infile,'latitude')
DEFINE_BOUNDARIES,qld_box,latitude,longitude,qld_box_tx,/LIMIT
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)

mask_longitude=OPEN_AND_EXTRACT(mask_file,'longitude')
mask_latitude=OPEN_AND_EXTRACT(mask_file,'latitude')
DEFINE_BOUNDARIES,qld_box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)
mask=REFORM(OPEN_AND_EXTRACT(mask_file,'lsm',$
                             offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                             count=[mask_nlon,mask_nlat,1,1]))

weights=fltarr(n_lat)
FOR i=0,n_lat-1 DO $
   weights(i)=COS(3.14159*latitude(i)/180.)
weights=weights/TOTAL(weights)
n_valid_points=N_ELEMENTS(where(mask eq 1))
mask_weight=fltarr(mask_nlon,mask_nlat)
FOR k=0,mask_nlon-1 DO BEGIN
   FOR m=0,mask_nlat-1 DO BEGIN
      IF mask(k,m) ge 0.5 THEN $
         mask_weight(k,m)=1./FLOAT(n_valid_points)*weights(m)
   ENDFOR
ENDFOR
mask_weight=mask_weight/TOTAL(mask_weight)

; Read precipitation
higem_precip=REFORM(OPEN_AND_EXTRACT(infile,'rain',offset=[qld_box_tx(1),qld_box_tx(0),0],$
                                     count=[n_lon,n_lat,higem_nyears]))
higem_precip[where(higem_precip ge 2e20)]=!Values.F_NaN
higem_precip_qld_aavg=fltarr(higem_nyears)
FOR i=0,higem_nyears-1 DO $
   higem_precip_qld_aavg(i)=TOTAL(REFORM(higem_precip(*,*,i))*mask_weight,/NaN)

; Export the results to a netCDF file
out_id=NCDF_CREATE(outfile,/CLOBBER)
out_dimids=intarr(3)
out_varids=intarr(5)
out_dimids(0)=NCDF_DIMDEF(out_id,'year',/UNLIMITED)
out_varids(0)=NCDF_VARDEF(out_id,'year',[out_dimids(0)])
out_varids(1)=NCDF_VARDEF(out_id,'rain_aavg_allqld',[out_dimids(0)])

NCDF_CONTROL,out_id,/ENDEF
NCDF_VARPUT,out_id,out_varids(0),indgen(higem_nyears)
NCDF_VARPUT,out_id,out_varids(1),higem_precip_qld_aavg
NCDF_CLOSE,out_id

STOP

END

