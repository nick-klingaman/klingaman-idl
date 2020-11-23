PRO hadgem3kpp_flux_corrections_columbia_aquaplanet

grid_file='/home/users/npklingaman/datasets/HADGEM3-KPP_ANCIL/landfrac_n96e_hadgem3-8.5.nc'
outfile='/home/users/npklingaman/datasets/HADGEM3-KPP_ANCIL/fcorr_columbia_aqua_n96e.nc'

longitude=OPEN_AND_EXTRACT(grid_file,'longitude')
latitude=OPEN_AND_EXTRACT(grid_file,'latitude')
nlon=N_ELEMENTS(longitude)
nlat=N_ELEMENTS(latitude)

fcorr=fltarr(nlon,nlat,1)
FOR i=0,nlat-1 DO BEGIN
   IF latitude(i) ge 0 THEN BEGIN      
      fcorr(*,i,0)=-50.1685+4.9755*latitude(i)-1.4162e-1*latitude(i)^2+$
                 1.6743e-3*latitude(i)^3-6.8650e-6*latitude(i)^4
   ENDIF ELSE $
      fcorr(*,i,0)=-56.0193-6.4824*latitude(i)-2.3494e-1*latitude(i)^2-$
                 3.4685e-3*latitude(i)^3-1.7732e-5*latitude(i)^4   
ENDFOR

id=NCDF_CREATE(outfile,/CLOBBER)
dimids=intarr(3)
dimids(0)=NCDF_DIMDEF(id,'longitude',nlon)
dimids(1)=NCDF_DIMDEF(id,'latitude',nlat)
dimids(2)=NCDF_DIMDEF(id,'t',1)
varids=intarr(4)
varids(0)=NCDF_VARDEF(id,'longitude',[dimids(0)])
varids(1)=NCDF_VARDEF(id,'latitude',[dimids(1)])
varids(2)=NCDF_VARDEF(id,'t',[dimids(2)])
varids(3)=NCDF_VARDEF(id,'fcorr',[dimids(0),dimids(1),dimids(2)])
NCDF_CONTROL,id,/ENDEF
NCDF_VARPUT,id,varids(0),longitude
NCDF_VARPUT,id,varids(1),latitude
NCDF_VARPUT,id,varids(2),0.5
NCDF_VARPUT,id,varids(3),fcorr
NCDF_CLOSE,id

STOP
END
