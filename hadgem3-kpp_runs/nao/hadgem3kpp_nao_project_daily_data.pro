PRO hadgem3kpp_nao_project_daily_data

box=[20,270,80,40]
nmodels=1

FOR m=0,nmodels-1 DO BEGIN
   print,m
   CASE m OF      
;      0 : BEGIN
;         nao_file='/home/ss901165/public_html/cvdp_output/fwgbl_n216_all/20CR_V2.cvdp_data.1871-2008.nc'
;         mslp_dmean_file='/home/ss901165/um_output6/xjhwb/hadgem3kpp_nrglobal_n216.jan-dec_dmeans_ts.years1-106.mslp.nc'
;         outfile='/home/ss901165/um_output6/xjhwb/hadgem3kpp_nrglobal_n216.jan-dec_dmeans_ts.years1-106.nao_index_20CR.nc'
;         ntime=LONG(106)*LONG(360)
;      END
      1 : BEGIN
         nao_file='/home/ss901165/public_html/cvdp_output/fwgbl_n216_all/GOML1_N216.cvdp_data.1992-2097.nc'
         mslp_dmean_file='/home/ss901165/um_output6/xjhwb/hadgem3kpp_nrglobal_n216.jan-dec_dmeans_ts.years1-106.mslp.nc'
         outfile='/home/ss901165/um_output6/xjhwb/hadgem3kpp_nrglobal_n216.jan-dec_dmeans_ts.years1-106.nao_index_goml_djf.nc'
         ntime=LONG(106)*LONG(360)
         ndays_per_year=360
         varname='air_pressure_at_sea_level'
         nao_varname='nao_djf'
      END
      0 : BEGIN
         nao_file='/home/ss901165/um_output6/cvdp_output/fwgbl_n216_all/20CR_V2.cvdp_data.1871-2008.nc'
         mslp_dmean_file='/home/ss901165/datasets/20THC_REANALYSIS/every_member/mslp/20thc_reanalysis_member1.jan-dec_dmeans.1871-2008.mslp.nc'
         outfile='/home/ss901165/datasets/20THC_REANALYSIS/every_member/mslp/20thc_reanalysis_member1.jan-dec_dmeans_ts.1871-2008.nao_index_20CR.nc'
         ntime=LONG(50404)
         ndays_per_year=365
         varname='prmsl'
         nao_varname='nao_ann'
      END
   ENDCASE
   

                                ; Read the NAO spatial pattern (for all months) produced by NCAR CVDP
   lon=OPEN_AND_EXTRACT(nao_file,'lon')
   lat=OPEN_AND_EXTRACT(nao_file,'lat')
   DEFINE_BOUNDARIES,box,lat,lon,box_tx,/LIMIT
   nlat=N_ELEMENTS(lat)
   nlon=N_ELEMENTS(lon)
   
   nao_pattern=OPEN_AND_EXTRACT(nao_file,nao_varname,offset=[box_tx(1),box_tx(0)],$
                                count=[nlon,nlat],/WRAP)
   oned_pattern=REFORM(nao_pattern,[nlon*nlat])
   
                                ; Read daily MSLP from model
   mslp_dmean=OPEN_AND_EXTRACT(mslp_dmean_file,varname,$
                               offset=[box_tx(1),box_tx(0),0],$
                               count=[nlon,nlat,ntime],/WRAP)/100.
; Convert to anomalies

;   FOR i=0,nlon-1 DO $
;      FOR j=0,nlat-1 DO $
;         FOR k=0,359 DO $
;            mslp_dmean(i,j,k:ntime-1:360)=mslp_dmean(i,j,k:ntime-1:360)-MEAN(mslp_dmean(i,j,k:ntime-1:360))
   

   FOR i=0,nlon-1 DO $
      FOR j=0,nlat-1 DO $
         FOR k=0,ndays_per_year-1 DO $
            mslp_dmean(i,j,k:ntime-1:ndays_per_year)=mslp_dmean(i,j,k:ntime-1:ndays_per_year)-MEAN(mslp_dmean(i,j,k:ntime-1:ndays_per_year))
   
;   FOR i=0,nlon-1 DO $
;      FOR j=0,nlat-1 DO $
;         FOR k=0,ntime-1 DO $
;            mslp_dmean(i,j,k:ntime-1:360)=mslp_dmean(i,j,k:ntime-1:360)/STDDEV(mslp_dmean(i,j,k:ntime-1:360))

; Compute projection onto pattern
   nao_daily=fltarr(ntime)
   FOR k=LONG(0),LONG(ntime)-1 DO BEGIN
      oned_today=REFORM(mslp_dmean(*,*,k),[nlon*nlat])
      nao_daily(k)=REGRESS(oned_today,oned_pattern)
   ENDFOR   

   id=NCDF_CREATE(outfile,/CLOBBER)
   dimid=NCDF_DIMDEF(id,'time',ntime)
   tvarid=NCDF_VARDEF(id,'time',[dimid])
   nvarid=NCDF_VARDEF(id,'nao_index',[dimid])
   NCDF_CONTROL,id,/ENDEF
   NCDF_VARPUT,id,tvarid,findgen(ntime)+0.5
   NCDF_VARPUT,id,nvarid,nao_daily
   NCDF_CLOSE,id
   
ENDFOR

STOP
END
