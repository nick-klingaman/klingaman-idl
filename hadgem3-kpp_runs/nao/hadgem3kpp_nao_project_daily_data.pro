PRO hadgem3kpp_nao_project_daily_data

box=[20,270,80,40]
nmodels=2

FOR m=0,nmodels-1 DO BEGIN
   print,m
   CASE m OF
      ;1 : BEGIN
      ;   nao_file='/group_workspaces/jasmin/futureweather/cvdp_output/fwgbl_n216/ERA-Interim.cvdp_data.1979-2009.nc'
      ;   mslp_dmean_file='/group_workspaces/jasmin/futureweather/xjhwb/hadgem3kpp_nrglobal_n216.jan-dec_dmeans_ts.years1-106.mslp.nc'
      ;   outfile='/group_workspaces/jasmin/futureweather/xjhwb/hadgem3kpp_nrglobal_n216.jan-dec_dmeans_ts.years1-106.nao_index_eraint.nc'
      ;   ntime=LONG(106)*LONG(360)
      ;   varname='air_pressure_at_sea_level'
      ;END
      ;0 : BEGIN
      ;   nao_file='/group_workspaces/jasmin/futureweather/cvdp_output/fwgbl_n96/MetUM-GOML1_N96_1.5xent.cvdp_data.1992-2060.nc'
      ;   mslp_dmean_file='/group_workspaces/jasmin2/klingaman/metum/xihvp/hadgem3kpp_fwgbl_1.5xentrain_ga30_n96.jan-dec_dmeans_ts.years1-72.mslp.nc'
      ;   outfile='/group_workspaces/jasmin2/klingaman/metum/xihvp/hadgem3kpp_fwgbl_1.5xentrain_ga30_n96.jan-dec_dmeans_ts.years1-72.nao_index.nc'
      ;   ntime=LONG(72)*LONG(360)
      ;   varname='p'
      ;END

      ; 1 : BEGIN
      ;    nao_file='/group_workspaces/jasmin/futureweather/cvdp_output/MetUM-GOML1_N216.cvdp_data.1992-2057.nc'
      ;    mslp_dmean_file='/group_workspaces/jasmin/futureweather/xjhwe/hadgem3a_kppnrglobalsmooth31_n216.jan-dec_dmeans_ts.years1-66.mslp.nc'
      ;    outfile='/group_workspaces/jasmin/futureweather/xjhwe/hadgem3a_kppnrglobalsmooth31_n216.jan-dec_dmeans_ts.years1-66.nao_index_goml.nc'
      ;    ntime=66*360
      ; END
      
;      2 : BEGIN
;         nao_file='/group_workspaces/jasmin/futureweather/cvdp_output/fwgbl_n216_all/GA3_N216_clim.cvdp_data.1992-2097.nc'
;         mslp_dmean_file='/group_workspaces/jasmin/futureweather/xjhwh/metum-ga3_fwn216-clim.jan-dec_dmeans_ts.years1-100.mslp.nc'
;         outfile='/group_workspaces/jasmin/futureweather/xjhwh/metum-ga3_fwn216-clim.jan-dec_dmeans_ts.years1-100.nao_index_ga3-clim.nc'
;         ntime=LONG(100)*LONG(360)
;         varname='air_pressure_at_sea_level'
;      END
;      1 : BEGIN
;         nao_file='/group_workspaces/jasmin/futureweather/cvdp_output/fwgbl_n216_all/GOML1_N216.cvdp_data.1992-2097.nc'
;         mslp_dmean_file='/group_workspaces/jasmin/futureweather/xjhwb/metum-goml1_fwn216.jan-dec_dmeans_ts.years1-100.mslp.nc'
;         outfile='/group_workspaces/jasmin/futureweather/xjhwb/metum-goml1_fwn216.jan-dec_dmeans_ts.years1-100.nao_index_goml.nc'
;         ntime=LONG(100)*LONG(360)
;         varname='air_pressure_at_sea_level'
;      END
;      0 : BEGIN
;         nao_file='/group_workspaces/jasmin/futureweather/cvdp_output/fwgbl_n216_all/GA3_N216_31d.cvdp_data.1992-2097.nc'
;         mslp_dmean_file='/group_workspaces/jasmin/futureweather/xjhwe/metum-ga3_fwn216-31day.jan-dec_dmeans_ts.years1-100.mslp.nc'
;         outfile='/group_workspaces/jasmin/futureweather/xjhwe/metum-ga3_fwn216-31day.jan-dec_dmeans_ts.years1-100.nao_index_ga3-31d.nc'
;         ntime=LONG(100)*LONG(360)
;         varname='air_pressure_at_sea_level'
;      END     

;      0 : BEGIN
;         nao_file='/group_workspaces/jasmin/futureweather/cvdp_output/getoml_n216_all/ETOML1_N216.cvdp_data.1992-2051.nc'
;         mslp_dmean_file='/group_workspaces/jasmin2/klingaman/metum/xlhvf/metum-etoml1_fwn216.jan-dec_dmeans_ts.years1-60.mslp.nc'
;         outfile='/group_workspaces/jasmin2/klingaman/metum/xlhvf/metum-etoml1_fwn216.jan-dec_dmeans_ts.years1-60.nao_index_etoml1.nc'
;         ntime=LONG(60)*LONG(360)
;         varname='air_pressure_at_sea_level'
;      END
;      0 : BEGIN
;         nao_file='/group_workspaces/jasmin/futureweather/cvdp_output/getoml_n216_all/GOML1_N216_1.5F.cvdp_data.1992-2051.nc'
;         mslp_dmean_file='/group_workspaces/jasmin/futureweather/fwgbl_n216_1p5F/xihvu/metum-goml1_fwn216-1p5.jan-dec_dmeans_ts.years1-61.mslp.nc'
;         outfile='/group_workspaces/jasmin/futureweather/fwgbl_n216_1p5F/metum-goml1_fwn216-1p5.jan-dec_dmeans_ts.years1-61.nao_index_goml1-1p5.nc'
;         ntime=LONG(61)*LONG(360)
;         varname='p'
;      END
;      1 : BEGIN
;         nao_file='/group_workspaces/jasmin/futureweather/cvdp_output/getoml_n216_all/GA3_N216_1.5F.cvdp_data.1992-2051.nc'
;         mslp_dmean_file='/group_workspaces/jasmin/futureweather/fwgbl_n216_1p5F/metum-ga3_fwn216-31day-1p5.jan-dec_dmeans_ts.years1-61.mslp.nc'
;         outfile='/group_workspaces/jasmin/futureweather/fwgbl_n216_1p5F/metum-ga3_fwn216-31day-1p5.jan-dec_dmeans_ts.nao_index_ga3-1p5.nc'
;         ntime=LONG(61)*LONG(360)
;         varname='p'
;      END
      
      0 : BEGIN
         nao_file='/group_workspaces/jasmin/futureweather/cvdp_output/getoml_n216_all/TOML1_N216.cvdp_data.1992-2051.nc'
         mslp_dmean_file='/group_workspaces/jasmin/futureweather/fwtoml_n216/metum-toml1_fwn216.jan-dec_dmeans_ts.years1-66.mslp.nc'
         outfile='/group_workspaces/jasmin/futureweather/fwtoml_n216/metum-toml1_fwn216.jan-dec_dmeans_ts.years1-66.nao_index_toml1.nc'
         ntime=LONG(66)*LONG(360)
         varname='air_pressure_at_sea_level'
      END

      ;0 : BEGIN
      ;   nao_file='/group_workspaces/jasmin/futureweather/cvdp_output/ERA-Interim.cvdp_data.1979-2013.nc'
      ;   mslp_dmean_file='/home/ss901165/datasets/ERA-INTERIM/MSL/MSL.jan-dec_dmeans_ts.1979-2013.n216.nc'
      ;   outfile='/home/ss901165/datasets/ERA-INTERIM/MSL/ERA-Interim.jan-dec_dmeans_ts.1979-2013.nao_index.nc'
      ;   ntime=35*365
      ;END

      ;0 : BEGIN
      ;   nao_file='/group_workspaces/jasmin/futureweather/cvdp_output/MetUM-GOML1_N216.cvdp_data.1992-2057.nc'
      ;   mslp_dmean_file='/home/ss901165/datasets/ERA-INTERIM/MSL/MSL.jan-dec_dmeans_ts.1979-2013.n216.nc'
      ;   outfile='/home/ss901165/datasets/ERA-INTERIM/MSL/ERA-Interim.jan-dec_dmeans_ts.1979-2013.nao_index_goml.nc'
      ;   ntime=35*365
      ;END

   ENDCASE
   

                                ; Read the NAO spatial pattern (for all months) produced by NCAR CVDP
   lon=OPEN_AND_EXTRACT(nao_file,'lon')
   lat=OPEN_AND_EXTRACT(nao_file,'lat')
   DEFINE_BOUNDARIES,box,lat,lon,box_tx,/LIMIT
   nlat=N_ELEMENTS(lat)
   nlon=N_ELEMENTS(lon)

   
;   STOP
   nao_pattern=OPEN_AND_EXTRACT(nao_file,'nao_mon',offset=[box_tx(1),box_tx(0)],$
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
         FOR k=0,359 DO $
            mslp_dmean(i,j,k:ntime-1:360)=mslp_dmean(i,j,k:ntime-1:360)-MEAN(mslp_dmean(i,j,k:ntime-1:360))
   
;   FOR i=0,nlon-1 DO $
;      FOR j=0,nlat-1 DO $
;         FOR k=0,ntime-1 DO $
;            mslp_dmean(i,j,k:ntime-1:360)=mslp_dmean(i,j,k:ntime-1:360)/STDDEV(mslp_dmean(i,j,k:ntime-1:360))

; Compute projection onto pattern
   nao_daily=fltarr(ntime)
   FOR k=0,ntime-1 DO BEGIN
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
