PRO qld_20thc_reanalysis_make_qld_aavg_precip_ts

; Make an area-averaged precipitation timeseries for Queensland

infile='/home/ss901165/datasets_mango/20THC_REANALYSIS/precip/20thc_reanalysis.may-apr_dmeans.1891-2007.precip.nc'
outfile='/home/ss901165/datasets_mango/20THC_REANALYSIS/precip/20thc_reanalysis.may-apr_dmeans.1891-2007.precip.qld_region_aavg.nc'
mask_file='/home/ss901165/datasets_mango/SILO/t62/SILO_qld_region_mask.t62.may-apr.nc'

n_year=117
n_time=365
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


; Read number of regions
n_regions=N_ELEMENTS(OPEN_AND_EXTRACT(mask_file,'region'))

weights=fltarr(n_lat)
FOR i=0,n_lat-1 DO $
   weights(i)=COS(3.14159*latitude(i)/180.)
weights=weights/TOTAL(weights)

; For each region, mask rainfall and compute area-averaged timeseries
region_aavg_ts=fltarr(n_regions,n_year,n_time)
FOR i=0,n_regions-1 DO BEGIN
   ; Read rainfall mask
   this_region_mask=OPEN_AND_EXTRACT(mask_file,'mask_region'+STRTRIM(STRING(i+1),1),$
                                     offset=[0,0],count=[mask_nlon,mask_nlat])
                                ; For each year, read rainfall
                                ; timeseries, mask and take
                                ; area-average
   FOR j=0,n_year-1 DO BEGIN
      this_year_precip=REFORM(OPEN_AND_EXTRACT(infile,'PRATE',$
                                               offset=[qld_box_tx(1),qld_box_tx(0),0,0,j],$
                                               count=[mask_nlon,mask_nlat,1,n_time,1]))
      FOR k=0,n_time-1 DO BEGIN
         temp=REFORM(this_year_precip(*,*,k))
         temp[where(this_region_mask ne 1)]=!Values.F_NaN
         this_year_precip(*,*,k)=temp
      ENDFOR
      n_valid_points=N_ELEMENTS(where(this_region_mask eq 1))
      mask_weight=fltarr(mask_nlon,mask_nlat)
      FOR k=0,mask_nlon-1 DO BEGIN
         FOR m=0,mask_nlat-1 DO BEGIN
            IF this_region_mask(k,m) eq 1 THEN $
               mask_weight(k,m)=1./FLOAT(n_valid_points)*weights(m)
         ENDFOR
      ENDFOR
      mask_weight=mask_weight/TOTAL(mask_weight)
      this_year_precip_aavg=fltarr(n_time)
      this_year_precip_aavg(*)=0.      
      FOR m=0,n_time-1 DO $
         this_year_precip_aavg(m)=this_year_precip_aavg(m)+$
         TOTAL(REFORM(this_year_precip(*,*,m))*mask_weight,/NaN)
      region_aavg_ts(i,j,*)=this_year_precip_aavg(*)
   ENDFOR
ENDFOR
qld_aavg_ts=fltarr(n_year,n_time)
qld_mask=OPEN_AND_EXTRACT(mask_file,'silo_regions',$
                          offset=[0,0],count=[mask_nlon,mask_nlat])
FOR j=0,n_year-1 DO BEGIN
   this_year_precip=REFORM(OPEN_AND_EXTRACT(infile,'PRATE',$
                                               offset=[qld_box_tx(1),qld_box_tx(0),0,0,j],$
                                               count=[mask_nlon,mask_nlat,1,n_time,1]))   
   FOR k=0,n_time-1 DO BEGIN
      temp=REFORM(this_year_precip(*,*,k))
      temp[where(qld_mask eq 0 or qld_mask gt n_regions)]=!Values.F_NaN
      this_year_precip(*,*,k)=temp
   ENDFOR
   n_valid_points=N_ELEMENTS(where(qld_mask gt 0 and qld_mask le n_regions))
   mask_weight=fltarr(mask_nlon,mask_nlat)
   FOR k=0,mask_nlon-1 DO BEGIN
      FOR m=0,mask_nlat-1 DO BEGIN
         IF qld_mask(k,m) gt 0 and qld_mask(k,m) le n_regions THEN $
            mask_weight(k,m)=1./FLOAT(n_valid_points)*weights(m)
      ENDFOR
   ENDFOR
   mask_weight=mask_weight/TOTAL(mask_weight)
   this_year_precip_aavg=fltarr(n_time)
   this_year_precip_aavg(*)=0.      
   FOR m=0,n_time-1 DO $
      this_year_precip_aavg(m)=this_year_precip_aavg(m)+$
      TOTAL(REFORM(this_year_precip(*,*,m))*mask_weight,/NaN)
   qld_aavg_ts(j,*)=this_year_precip_aavg(*)
ENDFOR

; Export the results to a netCDF file
out_id=NCDF_CREATE(outfile,/CLOBBER)
out_dimids=intarr(3)
out_varids=intarr(5)
out_dimids(0)=NCDF_DIMDEF(out_id,'region',n_regions)
out_dimids(1)=NCDF_DIMDEF(out_id,'year',n_year)
out_dimids(2)=NCDF_DIMDEF(out_id,'time',/UNLIMITED)
out_varids(0)=NCDF_VARDEF(out_id,'region',[out_dimids(0)])
out_varids(1)=NCDF_VARDEF(out_id,'year',[out_dimids(1)])
out_varids(2)=NCDF_VARDEF(out_id,'time',[out_dimids(2)])
out_varids(3)=NCDF_VARDEF(out_id,'rain_aavg_region',[out_dimids(0),out_dimids(1),out_dimids(2)])
out_varids(4)=NCDF_VARDEF(out_id,'rain_aavg_allqld',[out_dimids(1),out_dimids(2)])

NCDF_CONTROL,out_id,/ENDEF

NCDF_VARPUT,out_id,out_varids(0),indgen(n_regions)+1
NCDF_VARPUT,out_id,out_varids(1),indgen(n_year)
NCDF_VARPUT,out_id,out_varids(2),indgen(n_time)
NCDF_VARPUT,out_id,out_varids(3),region_aavg_ts
NCDF_VARPUT,out_id,out_varids(4),qld_aavg_ts

NCDF_CLOSE,out_id


STOP

END

