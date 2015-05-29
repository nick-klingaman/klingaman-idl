PRO hadgem3_monwg_aquaplanet_forcing_sin2sin4_warmpool

; Make an idealised SST forcing for aquaplanet experiments
; as a combination of a sin^2(lat) and sin^4(lat) functions.
; Add a warm pool with a COS function in a specified box.

; Maximum SST (equator) in Celsius
sst_max=29.
; Latitude (in both hemispheres) polewards of which SST is zero
lat_zero=60.
; Warm Pool box as [start_lat,start_lon,stop_lat,stop_lon]
warm_pool_box=[-10,80,10,140]

; File from which to read grid information
grid_file='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/mask_n96_hadgem3-7.3.nc'

longitude=OPEN_AND_EXTRACT(grid_file,'longitude')
latitude=OPEN_AND_EXTRACT(grid_file,'latitude')
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)

sst_out=fltarr(n_lon,n_lat)
FOR i=0,n_lat-1 DO BEGIN
   IF latitude(i) le -1.*lat_zero or latitude(i) ge lat_zero THEN BEGIN
      sst_out(*,i)=0.
   ENDIF ELSE $
      sst_out(*,i)=sst_max-sst_max*(sin(ABS(latitude(i))/60.*!Pi/2.)^2+sin(ABS(latitude(i))/60.*!Pi/2.)^4)/2.
ENDFOR

DEFINE_BOUNDARIES,warm_pool_box,latitude,longitude,warm_pool_box_tx
warm_pool_nlon=warm_pool_box_tx(3)-warm_pool_box_tx(1)+1
warm_pool_nlat=warm_pool_box_tx(2)-warm_pool_box_tx(0)+1
FOR i=warm_pool_box_tx(1),warm_pool_box_tx(3) DO BEGIN
   FOR j=warm_pool_box_tx(0),warm_pool_box_tx(2) DO BEGIN
      distance_x=(warm_pool_nlon/2-(i-warm_pool_box_tx(1)))/FLOAT(warm_pool_nlon/2)
      distance_y=(warm_pool_nlat/2-(j-warm_pool_box_tx(0)))/FLOAT(warm_pool_nlat/2)
      print,i,j,distance_x,distance_y
      sst_out(i,j)=COS(ABS(distance_x)*(!Pi/2))*COS(ABS(distance_y)*(!Pi/2))+sst_out(i,j)
   ENDFOR
ENDFOR

; Convert from Celsius to Kelvin
sst_out=sst_out+273.15

; Write output netCDF file
outfile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/aquaplanet_sst.sin2sin4.warmpool.nc'
id=NCDF_CREATE(outfile,/CLOBBER)
dimids=fltarr(2)
dimids(0)=NCDF_DIMDEF(id,'longitude',n_lon)
dimids(1)=NCDF_DIMDEF(id,'latitude',n_lat)
varids=fltarr(3)
varids(0)=NCDF_VARDEF(id,'longitude',[dimids(0)])
varids(1)=NCDF_VARDEF(id,'latitude',[dimids(1)])
varids(2)=NCDF_VARDEF(id,'sst',[dimids(0),dimids(1)])

NCDF_CONTROL,id,/ENDEF

NCDF_VARPUT,id,varids(0),longitude
NCDF_VARPUT,id,varids(1),latitude
NCDF_VARPUT,id,varids(2),sst_out

NCDF_CLOSE,id

STOP
END

