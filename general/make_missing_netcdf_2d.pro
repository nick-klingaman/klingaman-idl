PRO make_missing_netcdf_2d,filename,varname,$
                           nlon,start_lon,lon_res,$
                           nlat,start_lat,lat_res,$
                           missing_value

; Preserve inputs
my_filename=filename
my_varname=varname
my_nlon=nlon
my_start_lon=start_lon
my_lon_res=lon_res
my_nlat=nlat
my_start_lat=start_lat
my_lat_res=lat_res
my_missing_value=missing_value

; Create output file
ncid=NCDF_CREATE(my_filename,/CLOBBER)
lon_dimid=NCDF_DIMDEF(ncid,'nlon',my_nlon)
lat_dimid=NCDF_DIMDEF(ncid,'nlat',my_nlat)
;surf_dimid=NCDF_DIMDEF(ncid,'surface',1)
;time_dimid=NCDF_DIMDEF(ncid,'t',/UNLIMITED)

lon_varid=NCDF_VARDEF(ncid,'nlon',[lon_dimid])
lat_varid=NCDF_VARDEF(ncid,'nlat',[lat_dimid])
;time_varid=NCDF_VARDEF(ncid,'t',[time_dimid])
var_varid=NCDF_VARDEF(ncid,my_varname,[lon_dimid,lat_dimid])

NCDF_CONTROL,ncid,/ENDEF

longitude=indgen(my_nlon)*my_lon_res+my_start_lon
latitude=indgen(my_nlat)*my_lat_res+my_start_lat
;time=1

NCDF_VARPUT,ncid,lon_varid,longitude
NCDF_VARPUT,ncid,lat_varid,latitude
;NCDF_VARPUT,ncid,time_varid,time

variable=fltarr(my_nlon,my_nlat)
variable(*,*)=missing_value

NCDF_VARPUT,ncid,var_varid,variable

NCDF_CLOSE,ncid

STOP

END
