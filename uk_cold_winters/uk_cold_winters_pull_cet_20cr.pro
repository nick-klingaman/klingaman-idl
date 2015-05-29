PRO uk_cold_winters_pull_cet_20cr

; Input file
twentyc_infile='/home/ss901165/datasets/20THC_REANALYSIS/temp_2m/20thc_reanalysis.jan-dec_mmeans.1870-2010.temp2m.twod.nc'

; Box to use for area-averaging
box=[52,356,57,0]

; Land/sea mask
mask_infile='/home/ss901165/datasets/20THC_REANALYSIS/mask_t62.nc'

; Get latitude and longitude
latitude=OPEN_AND_EXTRACT(twentyc_infile,'latitude')
longitude=OPEN_AND_EXTRACT(twentyc_infile,'longitude')
DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
n_lat=N_ELEMENTS(latitude)
n_lon=N_ELEMENTS(longitude)

; Get temperatures
n_time=12
n_year=141
temp_2m=REFORM(OPEN_AND_EXTRACT(twentyc_infile,'TMP',$
                                offset=[box_tx(1),box_tx(0),0,0],$
                                count=[n_lon,n_lat,n_time,n_year],/WRAP))

; Get land/sea mask
mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                             offset=[box_tx(1),box_tx(0)],$
                             count=[n_lon,n_lat],/WRAP))

; Take area-average over all land points
temp_2m_aavg=fltarr(n_time,n_year)
FOR i=0,n_time-1 DO BEGIN
   FOR j=0,n_year-1 DO BEGIN
      x=REFORM(temp_2m(*,*,i,j))
      x[where(mask eq 0)]=!Values.F_NaN
      temp_2m_aavg(i,j)=MEAN(x,/NaN)
   ENDFOR
ENDFOR

outfile='/home/ss901165/datasets/20THC_REANALYSIS/temp_2m/20thc_reanalysis.jan-dec_mmeans.1870-2010.ctl_eng_temp.twod.nc'
id=NCDF_CREATE(outfile,/CLOBBER)
tdimid=NCDF_DIMDEF(id,'month',n_time)
ydimid=NCDF_DIMDEF(id,'year',n_year)
tvarid=NCDF_VARDEF(id,'month',[tdimid])
yvarid=NCDF_VARDEF(id,'year',[ydimid])
cetvarid=NCDF_VARDEF(id,'ctl_eng_temp',[tdimid,ydimid])
NCDF_CONTROL,id,/ENDEF
NCDF_VARPUT,id,tvarid,indgen(n_time)
NCDF_VARPUT,id,yvarid,indgen(n_year)
NCDF_VARPUT,id,cetvarid,temp_2m_aavg

NCDF_CLOSE,id

STOP
END

