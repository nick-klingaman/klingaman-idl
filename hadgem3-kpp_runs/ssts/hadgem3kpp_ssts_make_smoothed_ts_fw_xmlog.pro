PRO hadgem3kpp_ssts_make_smoothed_ts_fw

; Make a smoothed timeseries of SST
; Need to combine KPP and obs SST using coupling weight file

kpp_sst_file='/group_workspaces/jasmin2/bobble/metum/climate_n96/xmlog/kpp_ocean/sst.nc'
obs_sst_file='/home/users/npklingaman/datasets/METO_OCEAN_ANALYSIS/n96e/meto_ocean_analysis.jan-dec_dmean_clim_for_um_lininterp.1980-2009.sst.n96e.nc'
cplwght_file='/home/users/npklingaman/datasets/HADGEM3-KPP_ANCIL/cplwght_for_kpp.n96e.5ptblend.IndOcn.nc'
mask_file='/home/users/npklingaman/datasets/HADGEM3-KPP_ANCIL/landfrac_n96e_hadgem3-8.5.nc'
;obs_ice_file='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/sice_amip2_5108_n216_v7.0.jan-dec_dmeans_clim.nc'
obs_ice_file='/home/users/npklingaman/datasets/HADGEM3-KPP_ANCIL/sice_amip2_5106_n96e_v7.0.jan-dec_dmeans_clim.nc'

smooth_length=31
out_sst_file='/group_workspaces/jasmin2/bobble/metum/climate_n96/xmlog/smoothed_sst_xmlog_31day_years1-29.nc'

; Get global grid from cplwght file
gbl_longitude=OPEN_AND_EXTRACT(cplwght_file,'longitude')
gbl_latitude=OPEN_AND_EXTRACT(cplwght_file,'latitude')
n_lon=N_ELEMENTS(gbl_longitude)
n_lat=N_ELEMENTS(gbl_latitude)

; Get KPP grid from KPP file
kpp_longitude=OPEN_AND_EXTRACT(kpp_sst_file,'longitude')
kpp_latitude=OPEN_AND_EXTRACT(kpp_sst_file,'latitude')
kpp_nlon=N_ELEMENTS(kpp_longitude)
kpp_nlat=N_ELEMENTS(kpp_latitude)

start=0
count=29*360
; Get SSTs
kpp_sst=OPEN_AND_EXTRACT(kpp_sst_file,'T',offset=[0,0,start],count=[kpp_nlon,kpp_nlat,count])
n_time=N_ELEMENTS(kpp_sst(0,0,*))
obs_sst=OPEN_AND_EXTRACT(obs_sst_file,'sst')-273.15
obs_ice=OPEN_AND_EXTRACT(obs_ice_file,'iceconc')

kpp_lonoff=NEAREST(gbl_longitude,kpp_longitude(0))
kpp_latoff=NEAREST(gbl_latitude,kpp_latitude(0))

; Get cplwght
cplwght=OPEN_AND_EXTRACT(cplwght_file,'alpha')
; Get land fraction
landfrac=REFORM(OPEN_AND_EXTRACT(mask_file,'lsm'))

sst_out=fltarr(n_lon,n_lat,n_time)
; Note this code assumes the KPP grid is global in longitude
FOR i=0,n_lon-1 DO BEGIN
   print,'Running for longitude '+STRTRIM(STRING(i+1),1)+' of '+STRTRIM(STRING(n_lon),1)
   FOR j=0,n_lat-1 DO BEGIN
      IF landfrac(i,j) ne 1 THEN BEGIN
         IF gbl_latitude(j) lt kpp_latitude(0) or gbl_latitude(j) gt kpp_latitude(kpp_nlat-1) or $
	    gbl_longitude(i) lt kpp_longitude(0) or gbl_longitude(i) gt kpp_longitude(kpp_nlon-1) THEN BEGIN
            FOR k=0,n_time/360-1 DO $
               sst_out(i,j,k*360:(k+1)*360-1)=obs_sst(i,j,*)
            IF n_time MOD 360 ne 0 THEN $
               sst_out(i,j,n_time/360*360:n_time-1)=obs_sst(i,j,0:(n_time MOD 360)-1)
         ENDIF ELSE IF kpp_sst(i-kpp_lonoff,j-kpp_latoff,0) eq 1e20 THEN BEGIN
            FOR k=0,n_time/360-1 DO $
               sst_out(i,j,k*360:(k+1)*360-1)=obs_sst(i,j,*)
            IF n_time MOD 360 ne 0 THEN $
               sst_out(i,j,n_time/360*360:n_time-1)=obs_sst(i,j,0:(n_time MOD 360)-1)
         ENDIF ELSE BEGIN
            FOR k=0,n_time/360-1 DO $
               sst_out(i,j,k*360:(k+1)*360-1)=kpp_sst(i-kpp_lonoff,j-kpp_latoff,k*360:(k+1)*360-1)*cplwght(i,j)+$
               obs_sst(i,j,*)*(1.-cplwght(i,j))
            IF n_time MOD 360 ne 0 THEN $
               sst_out(i,j,n_time/360*360:n_time-1)=kpp_sst(i-kpp_lonoff,j-kpp_latoff,n_time/360*360:n_time-1)$
                                                    *cplwght(i,j)+obs_sst(i,j,0:(n_time MOD 360)-1)*$
                                                    (1.-cplwght(i,j))
         ENDELSE         
      ENDIF ELSE $
         sst_out(i,j,*)=2e20
      sst_out(i,j,*)=SMOOTH(sst_out(i,j,*),smooth_length)
      IF TOTAL(where(obs_ice(i,j,*) gt 0.)) gt 0 and landfrac(i,j) ne 1 THEN BEGIN
         FOR k=0,n_time/360-1 DO BEGIN
            temp_sst=REFORM(sst_out(i,j,k*360:(k+1)*360-1))
            temp_ice=REFORM(obs_ice(i,j,*))
            ;temp_sst[where(temp_ice gt 0.0)]=-1.80*temp_ice[where(temp_ice gt 0.0)]+$
                                ;temp_sst[where(temp_ice gt 0.0)]*(1.0-temp_ice[where(temp_ice gt 0.0)])
            temp_sst[where(temp_ice gt 0.)]=-1.80
            sst_out(i,j,k*360:(k+1)*360-1)=temp_sst
         ENDFOR
      ENDIF
      temp_sst=REFORM(sst_out(i,j,*))
      IF TOTAL(where(temp_sst lt -1.80)) ge 0 THEN BEGIN
         temp_sst[where(temp_sst lt -1.80)]=-1.80
         sst_out(i,j,*)=temp_sst
      ENDIF
   ENDFOR
ENDFOR

sst_out = sst_out+273.15

id=NCDF_CREATE(out_sst_file,/CLOBBER)
dimids=intarr(3)
dimids(0)=NCDF_DIMDEF(id,'longitude',n_lon)
dimids(1)=NCDF_DIMDEF(id,'latitude',n_lat)
dimids(2)=NCDF_DIMDEF(id,'time',n_time)
varids=intarr(4)
varids(0)=NCDF_VARDEF(id,'longitude',dimids(0))
varids(1)=NCDF_VARDEF(id,'latitude',dimids(1))
varids(2)=NCDF_VARDEF(id,'time',dimids(2))
varids(3)=NCDF_VARDEF(id,'sst',[dimids(0),dimids(1),dimids(2)])
NCDF_ATTPUT,id,varids(3),'missing_value',2e20
NCDF_ATTPUT,id,varids(3),'_FillValue',2e20
NCDF_CONTROL,id,/ENDEF
NCDF_VARPUT,id,varids(0),gbl_longitude
NCDF_VARPUT,id,varids(1),gbl_latitude
NCDF_VARPUT,id,varids(2),findgen(n_time)+0.5
NCDF_VARPUT,id,varids(3),sst_out
NCDF_CLOSE,id

STOP
END


