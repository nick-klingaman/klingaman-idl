PRO uk_cold_winters_pull_cet_20cr_ensmem

; Input file
twentyc_infile='/home/ss901165/datasets/20THC_REANALYSIS/every_member/surf_temp/20thc_reanalysis_allmembers.jan-dec_mmeans.1871-2008.t9950.nc'

; Box to use for area-averaging
box=[52,356,57,0]

; Land/sea mask
mask_infile='/home/ss901165/datasets/20THC_REANALYSIS/mask_t62.nc'

; Get latitude and longitude
latitude=OPEN_AND_EXTRACT(twentyc_infile,'lat')
longitude=OPEN_AND_EXTRACT(twentyc_infile,'lon')
DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
n_lat=N_ELEMENTS(latitude)
n_lon=N_ELEMENTS(longitude)

; Get temperatures
n_member=56
n_time=12
n_year=138
temp_2m=REFORM(OPEN_AND_EXTRACT(twentyc_infile,'t9950',$
                                offset=[box_tx(1),box_tx(0),0,0],$
                                count=[n_lon,n_lat,n_member,n_time*n_year],/WRAP))

; Get land/sea mask
mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlat=N_ELEMENTS(mask_latitude)
mask_nlon=N_ELEMENTS(mask_longitude)
mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                             offset=[mask_box_tx(1),mask_box_tx(0)],$
                             count=[n_lon,n_lat],/WRAP))
mask_revlat=fltarr(mask_nlon,mask_nlat)
FOR i=0,mask_nlat-1 DO $
   mask_revlat(*,i)=mask(*,mask_nlat-i-1)

; Take area-average over all land points
temp_2m_aavg=fltarr(n_member,n_time,n_year)
FOR k=0,n_member-1 DO BEGIN
   FOR i=0,n_time-1 DO BEGIN
      FOR j=0,n_year-1 DO BEGIN
         x=REFORM(temp_2m(*,*,k,j*n_time+i))
         x[where(mask_revlat eq 0)]=!Values.F_NaN
         temp_2m_aavg(k,i,j)=MEAN(x,/NaN)
      ENDFOR
   ENDFOR
ENDFOR

outfile='/home/ss901165/datasets/20THC_REANALYSIS/temp_2m/20thc_reanalysis_allmembers.jan-dec_mmeans.1870-2008.ctl_eng_temp.twod.nc'
id=NCDF_CREATE(outfile,/CLOBBER)
mdimid=NCDF_DIMDEF(id,'member',n_member)
tdimid=NCDF_DIMDEF(id,'month',n_time)
ydimid=NCDF_DIMDEF(id,'year',n_year)
mvarid=NCDF_VARDEF(id,'member',[mdimid])
tvarid=NCDF_VARDEF(id,'month',[tdimid])
yvarid=NCDF_VARDEF(id,'year',[ydimid])
cetvarid=NCDF_VARDEF(id,'ctl_eng_temp',[mdimid,tdimid,ydimid])
NCDF_CONTROL,id,/ENDEF
NCDF_VARPUT,id,mvarid,indgen(n_member)
NCDF_VARPUT,id,tvarid,indgen(n_time)
NCDF_VARPUT,id,yvarid,indgen(n_year)
NCDF_VARPUT,id,cetvarid,temp_2m_aavg

NCDF_CLOSE,id

STOP
END

