PRO uk_cold_winters_pull_nao_20cr_ensmem

; Input file
twentyc_infile='/home/ss901165/datasets/20THC_REANALYSIS/every_member/mslp/20thc_reanalysis_allmembers.jan-dec_mmeans.1871-2008.mslp.nc'
n_member=56

; Box to use for area-averaging
ice_box=[63,339,68,343]
gbr_box=[33,352,39,357]

; Land/sea mask
mask_infile='/home/ss901165/datasets/20THC_REANALYSIS/mask_t62.nc'

; Get latitude and longitude
latitude=OPEN_AND_EXTRACT(twentyc_infile,'lat')
longitude=OPEN_AND_EXTRACT(twentyc_infile,'lon')
DEFINE_BOUNDARIES,ice_box,latitude,longitude,ice_box_tx
DEFINE_BOUNDARIES,gbr_box,latitude,longitude,gbr_box_tx
n_lat=N_ELEMENTS(latitude)
n_lon=N_ELEMENTS(longitude)

; Get temperatures
n_months_per_year=12
n_year=138
n_time=n_months_per_year*n_year
ice_mslp=REFORM(OPEN_AND_EXTRACT(twentyc_infile,'prmsl',$
                                 offset=[ice_box_tx(1),ice_box_tx(0),0,0],$
                                 count=[ice_box_tx(3)-ice_box_tx(1)+1,$
                                        ice_box_tx(2)-ice_box_tx(0)+1,$
                                        n_member,n_time]))/100.
gbr_mslp=REFORM(OPEN_AND_EXTRACT(twentyc_infile,'prmsl',$
                                 offset=[gbr_box_tx(1),gbr_box_tx(0),0,0],$
                                 count=[gbr_box_tx(3)-gbr_box_tx(1)+1,$
                                        gbr_box_tx(2)-gbr_box_tx(0)+1,$
                                        n_member,n_time]))/100.

; Take area-average over all land points
ice_aavg=fltarr(n_member,n_months_per_year,n_year)
ice_aavg_anom=fltarr(n_member,n_months_per_year,n_year)
gbr_aavg=fltarr(n_member,n_months_per_year,n_year)
gbr_aavg_anom=fltarr(n_member,n_months_per_year,n_year)
FOR k=0,n_member-1 DO BEGIN
   FOR i=0,n_months_per_year-1 DO BEGIN
      FOR j=0,n_year-1 DO BEGIN
         ice_aavg(k,i,j)=MEAN(ice_mslp(*,*,k,j*n_months_per_year+i),/NaN)
         gbr_aavg(k,i,j)=MEAN(gbr_mslp(*,*,k,j*n_months_per_year+i),/Nan)
      ENDFOR
      ice_aavg_anom(k,i,*)=ice_aavg(k,i,*)-MEAN(ice_aavg(k,i,*))
      gbr_aavg_anom(k,i,*)=gbr_aavg(k,i,*)-MEAN(gbr_aavg(k,i,*))
      ice_aavg_anom(k,i,*)=ice_aavg_anom(k,i,*)/STDDEV(ice_aavg_anom(k,i,*))
      gbr_aavg_anom(k,i,*)=gbr_aavg_anom(k,i,*)/STDDEV(gbr_aavg_anom(k,i,*))
   ENDFOR
ENDFOR

nao_index=gbr_aavg_anom-ice_aavg_anom

outfile='/home/ss901165/datasets/20THC_REANALYSIS/mslp/20thc_reanalysis_allmembers.jan-dec_mmeans.1871-2008.nao_index.twod.nc'
id=NCDF_CREATE(outfile,/CLOBBER)
tdimid=NCDF_DIMDEF(id,'month',n_months_per_year)
ydimid=NCDF_DIMDEF(id,'year',n_year)
mdimid=NCDF_DIMDEF(id,'ensemble_member',n_member)
tvarid=NCDF_VARDEF(id,'month',[tdimid])
yvarid=NCDF_VARDEF(id,'year',[ydimid])
mvarid=NCDF_VARDEF(id,'ensemble_member',[mdimid])
cetvarid=NCDF_VARDEF(id,'nao_index',[mdimid,tdimid,ydimid])
NCDF_CONTROL,id,/ENDEF
NCDF_VARPUT,id,tvarid,indgen(n_months_per_year)
NCDF_VARPUT,id,yvarid,indgen(n_year)
NCDF_VARPUT,id,mvarid,indgen(n_member)
NCDF_VARPUT,id,cetvarid,nao_index

NCDF_CLOSE,id

STOP
END

