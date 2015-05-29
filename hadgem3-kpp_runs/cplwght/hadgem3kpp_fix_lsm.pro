PRO hadgem3kpp_fix_lsm

infile='lsm_ocndepth.nc'
outfile='lsm_ocndepth_fix.nc'
maskfile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/landfrac_n216_hadgem3-7.8.nc'

mask=OPEN_AND_EXTRACT(maskfile,'lsm')
lsm_in=OPEN_AND_EXTRACT(infile,'lsm')
longitude=OPEN_AND_EXTRACT(infile,'longitude')
latitude=OPEN_AND_EXTRACT(infile,'latitude')
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)
max_depth=OPEN_AND_EXTRACT(infile,'max_depth')

lsm_out=fltarr(n_lon,n_lat)
FOR i=0,n_lon-1 DO BEGIN
   FOR j=0,n_lat-1 DO BEGIN
      IF mask(i,j) lt 1 THEN BEGIN
         IF lsm_in(i,j) eq 1 THEN BEGIN
            lsm_out(i,j)=1
         ENDIF ELSE $
            lsm_out(i,j)=0
      ENDIF ELSE $
         lsm_out(i,j)=1
   ENDFOR
ENDFOR

id=NCDF_CREATE(outfile,/CLOBBER)
dimids=intarr(2)
dimids(0)=NCDF_DIMDEF(id,'longitude',n_lon)
dimids(1)=NCDF_DIMDEF(id,'latitude',n_lat)
varids=intarr(4)
varids(0)=NCDF_VARDEF(id,'longitude',[dimids(0)])
varids(1)=NCDF_VARDEF(id,'latitude',[dimids(1)])
varids(2)=NCDF_VARDEF(id,'lsm',[dimids(0),dimids(1)])
varids(3)=NCDF_VARDEF(id,'max_depth',[dimids(0),dimids(1)])
NCDF_CONTROL,id,/ENDEF
NCDF_VARPUT,id,varids(0),longitude
NCDF_VARPUT,id,varids(1),latitude
NCDF_VARPUT,id,varids(2),lsm_out
NCDF_VARPUT,id,varids(3),max_depth
NCDF_CLOSE,id
            
STOP
END
