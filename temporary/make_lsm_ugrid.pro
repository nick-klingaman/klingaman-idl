PRO make_lsm_ugrid

; Make a land-sea mask on the UM N144 U grid using an output file
; defined over land points only.

infile='/home/ss901165/um_output/xcxaz_wind.nc'
outfile='/home/ss901165/um_output/mask_n144_ugrid.nc'

longitude = OPEN_AND_EXTRACT(infile,'longitude')
latitude = OPEN_AND_EXTRACT(infile,'latitude')
data = REFORM(OPEN_AND_EXTRACT(infile,'wind'))

n_lon = N_ELEMENTS(longitude)
n_lat = N_ELEMENTS(latitude)

id = NCDF_CREATE(outfile,/CLOBBER)
dimids = intarr(2)
dimids(0) = NCDF_DIMDEF(id,'longitude',n_lon)
dimids(1) = NCDF_DIMDEF(id,'latitude',n_lat)
varids = intarr(3)
varids(0) = NCDF_VARDEF(id,'longitude',[dimids(0)])
varids(1) = NCDF_VARDEF(id,'latitude',[dimids(1)])
varids(2) = NCDF_VARDEF(id,'lsm',[dimids(0),dimids(1)])
NCDF_CONTROL,id,/ENDEF

mask = intarr(n_lon,n_lat)
mask(*,*) = 1
mask[where(ABS(data) ge 100)] = 0

NCDF_VARPUT,id,varids(0),longitude
NCDF_VARPUT,id,varids(1),latitude
NCDF_VARPUT,id,varids(2),mask

NCDF_CLOSE,id

STOP
END
