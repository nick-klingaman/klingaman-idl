PRO hadgem3_monwg_aquaplanet_forcing_zonalsym_hemissym

; Create zonally and hemispherically symmetric SSTs from a dataset

;input_file='/home/ss901165/um_output6/xjhva/kpp_ocean/ga60_aqua_1.0xentrain.sfcorr_withz.nc'
;output_file='/home/ss901165/um_output6/xjhva/kpp_ocean/ga60_aqua_1.0xentrain.zsym_hsym.sfcorr_withz.nc'
;input_file='/home/ss901165/datasets/METO_OCEAN_ANALYSIS/n96/meto_ocean_analysis.jan-dec_amean_clim.1980-2009.pot_temp_1000m.n96e.nc'
input_file='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/kpp_sep1_initcond_n96e_ukmotemp_ukmosal_1000m.nc'
output_file='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/kpp_sep1_initcond_n96e_aquaV_1000m.nc'
mask_file='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/landfrac_n96e_hadgem3-8.5.nc'

longitude=OPEN_AND_EXTRACT(input_file,'longitude')
latitude=OPEN_AND_EXTRACT(input_file,'latitude')
z=OPEN_AND_EXTRACT(input_file,'zvel')
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)
n_z=N_ELEMENTS(z)

input=REFORM(OPEN_AND_EXTRACT(input_file,'v',$
                              offset=[0,0,0],$
                              count=[n_lon,n_lat,n_z]))
;input[where(input ge 1e10)]=!Values.F_NaN

mask=REFORM(OPEN_AND_EXTRACT(mask_file,'lsm'))


sst_zonalsym=fltarr(n_lon,n_lat,n_z)
;sst_zonalsym=fltarr(n_lon,n_lat)
FOR k=0,n_z-1 DO BEGIN
   temp=REFORM(input(*,*,k))
   temp[where(mask eq 1)]=!Values.F_NaN
   FOR i=0,n_lat-1 DO $
      ;sst_zonalsym(*,i)=MEAN(input(*,i),/NaN)
      sst_zonalsym(*,i,k)=MEAN(temp(*,i),/NaN)
ENDFOR

sst_zonalsym_hemissym=fltarr(n_lon,n_lat,n_z)
;sst_zonalsym_hemissym=fltarr(n_lon,n_lat)
FOR k=0,n_z-1 DO BEGIN
   FOR i=0,n_lat/2-1 DO BEGIN
                                ;IF sst_zonalsym(0,i,k) ne 0 and sst_zonalsym(0,n_lat-i-1,k) ne 0 THEN BEGIN
      sst_zonalsym_hemissym(*,i,k)=(sst_zonalsym(*,i,k)+sst_zonalsym(*,n_lat-i-1,k))/2.
                                ;ENDIF ELSE $
                                ;  sst_zonalsym_hemissym(*,i,k)=!Values.F_NaN
      sst_zonalsym_hemissym(*,n_lat-i-1,k)=sst_zonalsym_hemissym(*,i,k)
   ENDFOR
ENDFOR
sst_zonalsym_hemissym[where(FINITE(sst_zonalsym_hemissym) eq 0)]=0.
;FOR k=0,n_z-1 DO $
;   FOR m=0,n_lon-1 DO $
;      sst_zonalsym_hemissym(m,*,k)=SMOOTH(sst_zonalsym_hemissym(m,*,k),5)
;IF ODD(n_lat) eq 1 THEN $
;   sst_zonalsym_hemissym(*,n_lat/2)=sst_zonalsym(*,n_lat/2,k)

;FOR i=0,n_lat/2-1 DO BEGIN
                                ;  IF sst_zonalsym(0,i) ne 0 and sst_zonalsym(0,n_lat-i-1) ne 0 THEN BEGIN
;   sst_zonalsym_hemissym(*,i)=(sst_zonalsym(*,i)+sst_zonalsym(*,n_lat-i-1))/2.
                                ;  ENDIF ELSE $
                                ;     sst_zonalsym_hemissym(*,i)=!Values.F_NaN
;   sst_zonalsym_hemissym(*,n_lat-i-1)=sst_zonalsym_hemissym(*,i)
;ENDFOR
;sst_zonalsym_hemissym[where(FINITE(sst_zonalsym_hemissym) eq 0)]=0.
;FOR m=0,n_lon-1 DO $
;   sst_zonalsym_hemissym(m,*)=SMOOTH(sst_zonalsym_hemissym(m,*),5)
;IF ODD(n_lat) eq 1 THEN $
;   sst_zonalsym_hemissym(*,n_lat/2)=sst_zonalsym(*,n_lat/2)

id=NCDF_CREATE(output_file,/CLOBBER)
dimids=intarr(4)
dimids(0)=NCDF_DIMDEF(id,'longitude',n_lon)
dimids(1)=NCDF_DIMDEF(id,'latitude',n_lat)
dimids(2)=NCDF_DIMDEF(id,'zvel',n_z)
;dimids(3)=NCDF_DIMDEF(id,'t')
varids=intarr(5)
varids(0)=NCDF_VARDEF(id,'longitude',dimids(0))
varids(1)=NCDF_VARDEF(id,'latitude',dimids(1))
varids(2)=NCDF_VARDEF(id,'zvel',dimids(2))
;varids(3)=NCDF_VARDEF(id,'t',dimids(3))
varids(4)=NCDF_VARDEF(id,'v',[dimids(0),dimids(1),dimids(2)])
;varids(4)=NCDF_VARDEF(id,'T',[dimids(0),dimids(1),dimids(3)])
NCDF_CONTROL,id,/ENDEF
NCDF_VARPUT,id,varids(0),longitude
NCDF_VARPUT,id,varids(1),latitude
NCDF_VARPUT,id,varids(2),z
;NCDF_VARPUT,id,varids(3),[0.5]
NCDF_VARPUT,id,varids(4),sst_zonalsym_hemissym
NCDF_CLOSE,id

STOP
END

