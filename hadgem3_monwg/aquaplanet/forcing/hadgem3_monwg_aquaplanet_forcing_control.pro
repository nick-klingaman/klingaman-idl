PRO hadgem3_monwg_aquaplanet_forcing_control

; Make an idealised SST forcing for aquaplanet experiments
; as a combination of a sin^2(lat) and sin^4(lat) functions.

; Maximum SST (equator) in Celsius
sst_max=27.
; Latitude (in both hemispheres) polewards of which SST is zero
lat_zero=60.

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
   ENDIF ELSE BEGIN
      sst1=sst_max*(1-sin(latitude(i)*!Pi/180.*3/2.)^2)
      sst_out(*,i)=(sst1)
   ENDELSE
ENDFOR

; Convert from Celsius to Kelvin
sst_out=sst_out+273.15

; Write output netCDF file
outfile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/aquaplanet_sst.control.nc'
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

