PRO uk_cold_winters_make_swwind_index

box=[50,352,55,358]
n_days_per_year=365
n_years=140

u_infile='/home/ss901165/datasets/20THC_REANALYSIS/zonal_wind/20thc_reanalysis.jan-dec_dmeans.1871-2010.uvel850.nc'
v_infile='/home/ss901165/datasets/20THC_REANALYSIS/meridional_wind/20thc_reanalysis.jan-dec_dmeans.1871-2010.vvel850.nc'

u_longitude=OPEN_AND_EXTRACT(u_infile,'longitude')
u_latitude=OPEN_AND_EXTRACT(u_infile,'latitude')
DEFINE_BOUNDARIES,box,u_latitude,u_longitude,u_box_tx,/LIMIT
u_nlon=N_ELEMENTS(u_longitude)
u_nlat=N_ELEMENTS(u_latitude)

v_longitude=OPEN_AND_EXTRACT(v_infile,'longitude')
v_latitude=OPEN_AND_EXTRACT(v_infile,'latitude')
DEFINE_BOUNDARIES,box,v_latitude,v_longitude,v_box_tx,/LIMIT
v_nlon=N_ELEMENTS(v_longitude)
v_nlat=N_ELEMENTS(v_latitude)

u_comp = REFORM(OPEN_AND_EXTRACT(u_infile,'U',$
                                 offset=[u_box_tx(1),u_box_tx(0),0,0,0],$
                                 count=[u_nlon,u_nlat,1,n_days_per_year,n_years]))

v_comp = REFORM(OPEN_AND_EXTRACT(v_infile,'V',$
                                 offset=[v_box_tx(1),v_box_tx(0),0,0,0],$
                                 count=[v_nlon,v_nlat,1,n_days_per_year,n_years]))

;swwind_index=fltarr(u_nlon,u_nlat,n_days_per_year,n_years)
swwind_index=fltarr(n_days_per_year,n_years)
FOR i=0,n_years-1 DO BEGIN
   print,'Year '+STRTRIM(STRING(i+1),1)+' ...'
   FOR j=0,n_days_per_year-1 DO BEGIN
;      FOR k=0,u_nlat-1 DO BEGIN
;         FOR m=0,v_nlon-1 DO BEGIN
            u_comp_aavg=MEAN(u_comp(*,*,j,i))
            v_comp_aavg=MEAN(v_comp(*,*,j,i))
            IF (u_comp_aavg lt 0 and v_comp_aavg lt 0) THEN $
               swwind_index(j,i)=(u_comp_aavg^2+v_comp_aavg^2)^0.5*COS(ATAN(v_comp_aavg/u_comp_aavg)-5*!Pi/4.)          
            IF (u_comp_aavg gt 0 and v_comp_aavg gt 0) THEN $
               swwind_index(j,i)=(u_comp_aavg^2+v_comp_aavg^2)^0.5*COS(ATAN(v_comp_aavg/u_comp_aavg)-!Pi/4.)
            IF (u_comp_aavg lt 0 and v_comp_aavg gt 0) THEN $
               swwind_index(j,i)=(u_comp_aavg^2+v_comp_aavg^2)^0.5*COS(ATAN(v_comp_aavg/u_comp_aavg)-3*!Pi/4.)
            IF (u_comp_aavg gt 0 and v_comp_aavg lt 0) THEN $
               swwind_index(j,i)=(u_comp_aavg^2+v_comp_aavg^2)^0.5*COS(ATAN(v_comp_aavg/u_comp_aavg)-7*!Pi/4.)
;         ENDFOR
;      ENDFOR
   ENDFOR
ENDFOR

outfile='/home/ss901165/datasets/20THC_REANALYSIS/wind_speed/20thc_reanalysis.jan-dec_dmeans.1871-2010.swwind_index_850.nc'
id=NCDF_CREATE(outfile,/CLOBBER)
dimids=intarr(4)
varids=intarr(5)
dimids(0)=NCDF_DIMDEF(id,'day_in_year',n_days_per_year)
dimids(1)=NCDF_DIMDEF(id,'year',n_years)
;dimids(2)=NCDF_DIMDEF(id,'longitude',u_nlon)
;dimids(3)=NCDF_DIMDEF(id,'latitude',u_nlat)
varids(0)=NCDF_VARDEF(id,'day_in_year',[dimids(0)])
varids(1)=NCDF_VARDEF(id,'year',[dimids(1)])
;varids(2)=NCDF_VARDEF(id,'longitude',[dimids(2)])
;varids(3)=NCDF_VARDEF(id,'latitude',[dimids(3)])
;varids(4)=NCDF_VARDEF(id,'swwind_index',[dimids(2),dimids(3),dimids(0),dimids(1)])
varids(4)=NCDF_VARDEF(id,'swwind_index',[dimids(0),dimids(1)])

NCDF_CONTROL,id,/ENDEF

NCDF_VARPUT,id,varids(0),indgen(n_days_per_year)+1
NCDF_VARPUT,id,varids(1),indgen(n_years)+1871
;NCDF_VARPUT,id,varids(2),u_longitude
;NCDF_VARPUT,id,varids(3),u_latitude
NCDF_VARPUT,id,varids(4),swwind_index

NCDF_CLOSE,id

STOP
END

