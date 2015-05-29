PRO hadgem3kpp_ao_project_daily_data

box=[20,0,88,360]
nmodels=1

FOR m=0,nmodels-1 DO BEGIN
   print,m
   CASE m OF
       ;0 : BEGIN
       ;   ao_file='/home/ss901165/um_output6/xihvi/hadgem3kpp_fwgbl_1.0xentrain_ga30_n96.jan-dec_mmeans_ts.years1-75.ao_eofs.nc'
       ;   Z_dmean_file='/home/ss901165/um_output6/xihvi/hadgem3kpp_fwgbl_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.Z.nc'
       ;   outfile='/home/ss901165/um_output6/xihvi/hadgem3kpp_fwgbl_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.ao_index_goml.nc'
       ;   ntime=75*360
       ;END
      
      ;0 : BEGIN
      ;   ao_file='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgbl31day_1.0xentrain_ga30_n96.jan-dec_mmeans_ts.years1-75.ao_eofs.nc'
      ;   Z_dmean_file='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgbl31day_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.Z.nc'
      ;   outfile='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgbl31day_1.0xentrain_ga30_n96.jan-dec_mmeans_ts.years1-75.ao_index_ga3-31d.nc'
      ;   ntime=75*360
      ;END
      ;1 : BEGIN
      ;   ao_file='/home/ss901165/um_output6/xihvi/hadgem3kpp_fwgbl_1.0xentrain_ga30_n96.jan-dec_mmeans_ts.years1-75.ao_eofs.nc'
      ;   Z_dmean_file='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgbl31day_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.Z.nc'
      ;   outfile='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgbl31day_1.0xentrain_ga30_n96.jan-dec_mmeans_ts.years1-75.ao_index_goml.nc'
      ;   ntime=75*360
      ;END
     

     ; 0 : BEGIN
     ;    ao_file='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgblclim_1.0xentrain_ga30_n96.jan-dec_mmeans_ts.years1-75.ao_eofs.nc'
     ;    Z_dmean_file='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgblclim_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.Z.nc'
     ;    outfile='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgblclim_1.0xentrain_ga30_n96.jan-dec_mmeans_ts.years1-75.ao_index_ga3clim.nc'
     ;    ntime=75*360
     ; END
     ; 1 : BEGIN
     ;    ao_file='/home/ss901165/um_output6/xihvi/hadgem3kpp_fwgbl_1.0xentrain_ga30_n96.jan-dec_mmeans_ts.years1-75.ao_eofs.nc'
     ;    Z_dmean_file='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgblclim_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.Z.nc'
     ;    outfile='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgblclim_1.0xentrain_ga30_n96.jan-dec_mmeans_ts.years1-75.ao_index_goml.nc'
     ;    ntime=75*360
     ; END

     ; 0 : BEGIN
     ;    ao_file='/home/ss901165/datasets/ERA-INTERIM/Z/ERA_interim.jan-dec_mmeans.1979-2013.ao_eofs.n96.nc'
     ;    Z_dmean_file='/home/ss901165/datasets/ERA-INTERIM/Z/ERA_interim.jan-dec_dmeans_ts.1979-2013.Z.nh_domain.n96.nc'
     ;    outfile='/home/ss901165/datasets/ERA-INTERIM/Z/ERA_interim.jan-dec_dmeans_ts.1979-2013.ao_index_eraint_n96.nc'
     ;    ntime=35*365
     ;    ndpy=365
     ; END
     ; 1 : BEGIN
     ;    ao_file='/home/ss901165/um_output6/xihvi/hadgem3kpp_fwgbl_1.0xentrain_ga30_n96.jan-dec_mmeans_ts.years1-75.ao_eofs.nc'
     ;    Z_dmean_file='/home/ss901165/datasets/ERA-INTERIM/Z/ERA_interim.jan-dec_dmeans_ts.1979-2013.Z.nh_domain.n96.nc'
     ;    outfile='/home/ss901165/datasets/ERA-INTERIM/Z/ERA_interim.jan-dec_dmeans_ts.1979-2013.ao_index_goml_n96.nc'
     ;    ntime=35*365
     ;    ndpy=365
     ; END

     ; 0 : BEGIN
     ;    ao_file='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_mmeans_clim.years1-29.ao_eofs.nc'
     ;    Z_dmean_file='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans_ts.years1-29.Z.nc'
     ;    outfile='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans_ts.years1-29.ao_index_goml_1p5-50NS.nc'
     ;    ntime=29*360
     ;    ndpy=360
     ; END
     ; 1 : BEGIN
     ;    ao_file='/home/ss901165/um_output6/xihvi/hadgem3kpp_fwgbl_1.0xentrain_ga30_n96.jan-dec_mmeans_ts.years1-75.ao_eofs.nc'
     ;    Z_dmean_file='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans_ts.years1-29.Z.nc'
     ;    outfile='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans_ts.years1-29.ao_index_goml.nc'
     ;    ntime=29*360
     ;    ndpy=360
     ; END
     ; 2 : BEGIN
     ;    ao_file='/home/ss901165/datasets/ERA-INTERIM/Z/ERA_interim.jan-dec_mmeans.1979-2013.ao_eofs.n96.nc'
     ;    Z_dmean_file='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans_ts.years1-29.Z.nc'
     ;    outfile='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans_ts.years1-29.ao_index_eraint.nc'
     ;    ntime=29*360
     ;    ndpy=360
     ; END
      

      0 : BEGIN
         ao_file='/home/ss901165/datasets/ERA-INTERIM/Z/ERA_interim.jan-dec_mmeans.1979-2013.ao_eofs.n216.nc'
         Z_dmean_file='/home/ss901165/datasets/ERA-INTERIM/Z/ERA_interim.jan-dec_dmeans_ts.1979-2013.Z.nh_domain.n216.nc'
         outfile='/home/ss901165/datasets/ERA-INTERIM/Z/ERA_interim.jan-dec_dmeans_ts.1979-2013.ao_index_eraint_n216.nc'
         ntime=35*365
         ndpy=365
      END

      ; 1 : BEGIN
      ;    nao_file='/home/ss901165/um_output6/cvdp_output/fwgbl_n96/MetUM-GA3_N96_GOML1_31-day_SST.cvdp_data.1992-2066.nc'
      ;    mslp_dmean_file='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgbl31day_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.mslp.nc'
      ;    outfile='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgbl31day_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_ga3-31d.nc'
      ;    ntime=75*360
      ; END
      ; 2 : BEGIN
      ;    nao_file='/home/ss901165/um_output6/cvdp_output/fwgbl_n96/MetUM-GA3_N96_GOML1_clim_SST.cvdp_data.1992-2066.nc'
      ;    mslp_dmean_file='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgblclim_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.mslp.nc'
      ;    outfile='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgblclim_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_ga3-clim.nc'
      ;    ntime=75*360
      ; END
      ; 3 : BEGIN
      ;    nao_file='/home/ss901165/um_output6/cvdp_output/fwgbl_n96/MetUM-GOML1_N96.cvdp_data.1992-2066.nc'
      ;    mslp_dmean_file='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgbl31day_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.mslp.nc'
      ;    outfile='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgbl31day_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_goml.nc'
      ;    ntime=75*360
      ; END
      ; 4 : BEGIN
      ;    nao_file='/home/ss901165/um_output6/cvdp_output/fwgbl_n96/MetUM-GOML1_N96.cvdp_data.1992-2066.nc'
      ;    mslp_dmean_file='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgblclim_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.mslp.nc'
      ;    outfile='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgblclim_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_goml.nc'
      ;    ntime=75*360
      ; END
      ; 5 : BEGIN
      ;    nao_file='/home/ss901165/um_output6/cvdp_output/fwgbl_n96/ERA-Interim.cvdp_data.1979-2013.nc'
      ;    mslp_dmean_file='/home/ss901165/um_output6/xihvi/hadgem3kpp_fwgbl_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.mslp.nc'
      ;    outfile='/home/ss901165/um_output6/xihvi/hadgem3kpp_fwgbl_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_eraint.nc'
      ;    ntime=75*360
      ; END
      ; 6 : BEGIN
      ;    nao_file='/home/ss901165/um_output6/cvdp_output/fwgbl_n96/ERA-Interim.cvdp_data.1979-2013.nc'
      ;    mslp_dmean_file='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgbl31day_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.mslp.nc'
      ;    outfile='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgbl31day_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_eraint.nc'
      ;    ntime=75*360
      ; END
      ; 7 : BEGIN
      ;    nao_file='/home/ss901165/um_output6/cvdp_output/fwgbl_n96/ERA-Interim.cvdp_data.1979-2013.nc'
      ;    mslp_dmean_file='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgblclim_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.mslp.nc'
      ;    outfile='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgblclim_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_eraint.nc'
      ;    ntime=75*360
      ; END
         
      
      ;0 : BEGIN
     ;    nao_file='/home/ss901165/um_output6/cvdp_output/fwgbl_n216/ERA-Interim.cvdp_data.1979-2013.nc'
    ;     mslp_dmean_file='/home/ss901165/datasets/ERA-INTERIM/MSL/MSL.jan-dec_dmeans_ts.1979-2013.n216.nc'
   ;      outfile='/home/ss901165/datasets/ERA-INTERIM/MSL/ERA-Interim.jan-dec_dmeans_ts.1979-2013.nao_index_n216.nc'
  ;       ntime=35*365
 ;     END
;      1 : BEGIN
;         nao_file='/home/ss901165/um_output6/cvdp_output/fwgbl_n216/MetUM-GOML1_N216.cvdp_data.1992-2057.nc'
;         mslp_dmean_file='/home/ss901165/datasets/ERA-INTERIM/MSL/MSL.jan-dec_dmeans_ts.1979-2013.n216.nc'
;         outfile='/home/ss901165/datasets/ERA-INTERIM/MSL/ERA-Interim.jan-dec_dmeans_ts.1979-2013.nao_index_goml_n216.nc'
;         ntime=35*365
;      END

;      0 : BEGIN
;         nao_file='/home/ss901165/um_output6/cvdp_output/fwgbl_n96/MetUM-GOML1_N96_1.5xent.cvdp_data.1992-2060.nc'
;         mslp_dmean_file='/home/ss901165/um_output6/xihvp/hadgem3kpp_fwgbl_1.5xentrain_ga30_n96.jan-dec_dmeans_ts.years1-69.mslp.nc'
;         outfile='/home/ss901165/um_output6/xihvp/hadgem3kpp_fwgbl_1.5xentrain_ga30_n96.jan-dec_dmeans_ts.years1-69.nao_index_goml_1p5.nc'
;         ntime=69*360
;      END
;      1 : BEGIN
;         nao_file='/home/ss901165/um_output6/cvdp_output/fwgbl_n96/ERA-Interim.cvdp_data.1979-2013.nc'
;         mslp_dmean_file='/home/ss901165/um_output6/xihvp/hadgem3kpp_fwgbl_1.5xentrain_ga30_n96.jan-dec_dmeans_ts.years1-69.mslp.nc'
;         outfile='/home/ss901165/um_output6/xihvp/hadgem3kpp_fwgbl_1.5xentrain_ga30_n96.jan-dec_dmeans_ts.years1-69.nao_index_eraint.nc'
;         ntime=69*360
;      END
;      2 : BEGIN
;         nao_file='/home/ss901165/um_output6/cvdp_output/fwgbl_n96/MetUM-GOML1_N96.cvdp_data.1992-2066.nc'
;         mslp_dmean_file='/home/ss901165/um_output6/xihvp/hadgem3kpp_fwgbl_1.5xentrain_ga30_n96.jan-dec_dmeans_ts.years1-69.mslp.nc'
;         outfile='/home/ss901165/um_output6/xihvp/hadgem3kpp_fwgbl_1.5xentrain_ga30_n96.jan-dec_dmeans_ts.years1-69.nao_index_goml.nc'
;         ntime=69*360
;      END
         
      ; 0 : BEGIN
      ;    nao_file='/home/ss901165/um_output6/cvdp_output/fwgbl_n96/MetUM-GOML1_50NS_N96_1.5xent.cvdp_data.1982-2010.nc'
      ;    mslp_dmean_file='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans_ts.years1-29.mslp.nc'
      ;    outfile='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans_ts.years1-29.nao_index_goml_1p5_50NS.nc'
      ;    ntime=29*360
      ; END
      ; 1 : BEGIN
      ;    nao_file='/home/ss901165/um_output6/cvdp_output/fwgbl_n96/ERA-Interim.cvdp_data.1979-2013.nc'
      ;    mslp_dmean_file='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans_ts.years1-29.mslp.nc'
      ;    outfile='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans_ts.years1-29.nao_index_eraint.nc'
      ;    ntime=29*360
      ; END
      ; 2 : BEGIN
      ;    nao_file='/home/ss901165/um_output6/cvdp_output/fwgbl_n96/MetUM-GOML1_N96.cvdp_data.1992-2066.nc'
      ;    mslp_dmean_file='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans_ts.years1-29.mslp.nc'
      ;    outfile='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans_ts.years1-29.nao_index_goml.nc'
      ;    ntime=29*360
      ; END

      ; 3 : BEGIN
      ;    nao_file='/home/ss901165/um_output6/cvdp_output/fwgbl_n96/MetUM-GA3_N96_50NS_31d_SST_1.5xent.cvdp_data.1982-2010.nc'
      ;    mslp_dmean_file='/home/ss901165/um_output6/xihvx/hadgem3a_kpp50N50Ssmooth31_1.5xentrain_ga30.jan-dec_dmeans_ts.years1-29.mslp.nc'
      ;    outfile='/home/ss901165/um_output6/xihvx/hadgem3a_kpp50N50Ssmooth31_1.5xentrain_ga30.jan-dec_dmeans_ts.years1-29.nao_index_ga3-50ns-31d.nc'
      ;    ntime=29*360
      ; END
      ; 4 : BEGIN
      ;    nao_file='/home/ss901165/um_output6/cvdp_output/fwgbl_n96/ERA-Interim.cvdp_data.1979-2013.nc'
      ;    mslp_dmean_file='/home/ss901165/um_output6/xihvx/hadgem3a_kpp50N50Ssmooth31_1.5xentrain_ga30.jan-dec_dmeans_ts.years1-29.mslp.nc'
      ;    outfile='/home/ss901165/um_output6/xihvx/hadgem3a_kpp50N50Ssmooth31_1.5xentrain_ga30.jan-dec_dmeans_ts.years1-29.nao_index_eraint.nc'
      ;    ntime=29*360
      ; END
      ; 5 : BEGIN
      ;    nao_file='/home/ss901165/um_output6/cvdp_output/fwgbl_n96/MetUM-GOML1_N96.cvdp_data.1992-2066.nc'
      ;    mslp_dmean_file='/home/ss901165/um_output6/xihvx/hadgem3a_kpp50N50Ssmooth31_1.5xentrain_ga30.jan-dec_dmeans_ts.years1-29.mslp.nc'
      ;    outfile='/home/ss901165/um_output6/xihvx/hadgem3a_kpp50N50Ssmooth31_1.5xentrain_ga30.jan-dec_dmeans_ts.years1-29.nao_index_goml.nc'
      ;    ntime=29*360
      ; END

      ; 6 : BEGIN
      ;    nao_file='/home/ss901165/um_output6/cvdp_output/fwgbl_n96/MetUM-GA3_N96_50NS_15d_SST_1.5xent.cvdp_data.1982-2010.nc'
      ;    mslp_dmean_file='/home/ss901165/um_output6/xihvy/hadgem3a_kpp50N50Ssmooth15_1.5xentrain_ga30.jan-dec_dmeans_ts.years1-29.mslp.nc'
      ;    outfile='/home/ss901165/um_output6/xihvy/hadgem3a_kpp50N50Ssmooth15_1.5xentrain_ga30.jan-dec_dmeans_ts.years1-29.nao_index_ga3-50ns-15d.nc'
      ;    ntime=29*360
      ; END
      ; 7 : BEGIN
      ;    nao_file='/home/ss901165/um_output6/cvdp_output/fwgbl_n96/ERA-Interim.cvdp_data.1979-2013.nc'
      ;    mslp_dmean_file='/home/ss901165/um_output6/xihvy/hadgem3a_kpp50N50Ssmooth15_1.5xentrain_ga30.jan-dec_dmeans_ts.years1-29.mslp.nc'
      ;    outfile='/home/ss901165/um_output6/xihvy/hadgem3a_kpp50N50Ssmooth15_1.5xentrain_ga30.jan-dec_dmeans_ts.years1-29.nao_index_eraint.nc'
      ;    ntime=29*360
      ; END
      ; 8 : BEGIN
      ;    nao_file='/home/ss901165/um_output6/cvdp_output/fwgbl_n96/MetUM-GOML1_N96.cvdp_data.1992-2066.nc'
      ;    mslp_dmean_file='/home/ss901165/um_output6/xihvy/hadgem3a_kpp50N50Ssmooth15_1.5xentrain_ga30.jan-dec_dmeans_ts.years1-29.mslp.nc'
      ;    outfile='/home/ss901165/um_output6/xihvy/hadgem3a_kpp50N50Ssmooth15_1.5xentrain_ga30.jan-dec_dmeans_ts.years1-29.nao_index_goml.nc'
      ;    ntime=29*360
      ; END

      ; 0 : BEGIN
      ;    nao_file='/home/ss901165/public_html/cvdp_output/ERA-Interim.cvdp_data.1979-2013.nc'
      ;    mslp_dmean_file='/home/ss901165/um_output6/xjhwb/hadgem3kpp_nrglobal_n216.jan-dec_dmeans_ts.years1-66.mslp.nc'
      ;    outfile='/home/ss901165/um_output6/xjhwb/hadgem3kpp_nrglobal_n216.jan-dec_dmeans_ts.years1-66.nao_index_eraint.nc'
      ;    ntime=66*360
      ; END
      ; 1 : BEGIN
      ;    nao_file='/home/ss901165/public_html/cvdp_output/ERA-Interim.cvdp_data.1979-2013.nc'
      ;    mslp_dmean_file='/home/ss901165/um_output6/xjhwe/hadgem3a_kppnrglobalsmooth31_n216.jan-dec_dmeans_ts.years1-66.mslp.nc'
      ;    outfile='/home/ss901165/um_output6/xjhwe/hadgem3a_kppnrglobalsmooth31_n216.jan-dec_dmeans_ts.years1-66.nao_index_eraint.nc'
      ;    ntime=66*360
      ; END
      ; 2 : BEGIN
      ;    nao_file='/home/ss901165/public_html/cvdp_output/ERA-Interim.cvdp_data.1979-2013.nc'
      ;    mslp_dmean_file='/home/ss901165/um_output6/xjhwh/hadgem3a_nrglobal_n216.jan-dec_dmeans_ts.years1-66.mslp.nc'
      ;    outfile='/home/ss901165/um_output6/xjhwh/hadgem3a_nrglobal_n216.jan-dec_dmeans_ts.years1-66.nao_index_eraint.nc'
      ;    ntime=66*360
      ; END         

      ; 0 : BEGIN
      ;    nao_file='/home/ss901165/public_html/cvdp_output/MetUM-GOML1_N216.cvdp_data.1992-2057.nc'
      ;    mslp_dmean_file='/home/ss901165/um_output6/xjhwb/hadgem3kpp_nrglobal_n216.jan-dec_dmeans_ts.years1-66.mslp.nc'
      ;    outfile='/home/ss901165/um_output6/xjhwb/hadgem3kpp_nrglobal_n216.jan-dec_dmeans_ts.years1-66.nao_index_goml.nc'
      ;    ntime=66*360
      ; END
      ; 1 : BEGIN
      ;    nao_file='/home/ss901165/public_html/cvdp_output/MetUM-GOML1_N216.cvdp_data.1992-2057.nc'
      ;    mslp_dmean_file='/home/ss901165/um_output6/xjhwe/hadgem3a_kppnrglobalsmooth31_n216.jan-dec_dmeans_ts.years1-66.mslp.nc'
      ;    outfile='/home/ss901165/um_output6/xjhwe/hadgem3a_kppnrglobalsmooth31_n216.jan-dec_dmeans_ts.years1-66.nao_index_goml.nc'
      ;    ntime=66*360
      ; END
      ; 2 : BEGIN
      ;    nao_file='/home/ss901165/public_html/cvdp_output/MetUM-GOML1_N216.cvdp_data.1992-2057.nc'
      ;    mslp_dmean_file='/home/ss901165/um_output6/xjhwh/hadgem3a_nrglobal_n216.jan-dec_dmeans_ts.years1-66.mslp.nc'
      ;    outfile='/home/ss901165/um_output6/xjhwh/hadgem3a_nrglobal_n216.jan-dec_dmeans_ts.years1-66.nao_index_goml.nc'
      ;    ntime=66*360
      ; END    
      
      ;0 : BEGIN
      ;   nao_file='/home/ss901165/public_html/cvdp_output/ERA-Interim.cvdp_data.1979-2013.nc'
      ;   mslp_dmean_file='/home/ss901165/datasets/ERA-INTERIM/MSL/MSL.jan-dec_dmeans_ts.1979-2013.n216.nc'
      ;   outfile='/home/ss901165/datasets/ERA-INTERIM/MSL/ERA-Interim.jan-dec_dmeans_ts.1979-2013.nao_index.nc'
      ;   ntime=35*365
      ;END

      ;0 : BEGIN
      ;   nao_file='/home/ss901165/public_html/cvdp_output/MetUM-GOML1_N216.cvdp_data.1992-2057.nc'
      ;   mslp_dmean_file='/home/ss901165/datasets/ERA-INTERIM/MSL/MSL.jan-dec_dmeans_ts.1979-2013.n216.nc'
      ;   outfile='/home/ss901165/datasets/ERA-INTERIM/MSL/ERA-Interim.jan-dec_dmeans_ts.1979-2013.nao_index_goml.nc'
      ;   ntime=35*365
      ;END

   ENDCASE
   

                                ; Read the NAO spatial pattern (for all months) produced by NCAR CVDP
   lon=OPEN_AND_EXTRACT(ao_file,'longitude')
   lat=OPEN_AND_EXTRACT(ao_file,'latitude')
   lev=OPEN_AND_EXTRACT(ao_file,'evn')
   DEFINE_BOUNDARIES,box,lat,lon,box_tx,/LIMIT
   nlat=N_ELEMENTS(lat)
   nlon=N_ELEMENTS(lon)
   nlev=N_ELEMENTS(lev)
   
   ao_pattern=OPEN_AND_EXTRACT(ao_file,'ao_eofs',offset=[box_tx(1),box_tx(0),0],$
                               count=[nlon,nlat,nlev])
   ao_daily=fltarr(nlev,ntime)

   FOR p=0,nlev-1 DO BEGIN
      print,'---> ',p
      oned_pattern=REFORM(ao_pattern(*,*,p),[nlon*nlat])
                                ; Read daily Z from model
      Z_dmean=REFORM(OPEN_AND_EXTRACT(Z_dmean_file,'Z',$
                                      offset=[box_tx(1),box_tx(0),p,0],$
                                      count=[nlon,nlat,1,ntime]))
      FOR i=0,nlon-1 DO $
         FOR j=0,nlat-1 DO $
            FOR k=0,ndpy-1 DO $
               Z_dmean(i,j,k:ntime-1:ndpy)=Z_dmean(i,j,k:ntime-1:ndpy)-MEAN(Z_dmean(i,j,k:ntime-1:ndpy))
                                ; Compute projection onto pattern
      FOR k=0,ntime-1 DO BEGIN
         oned_today=REFORM(Z_dmean(*,*,k),[nlon*nlat])
         ao_daily(p,k)=REGRESS(oned_today,oned_pattern)
      ENDFOR   
   ENDFOR
   
   id=NCDF_CREATE(outfile,/CLOBBER)
   tdimid=NCDF_DIMDEF(id,'time',ntime)
   pdimid=NCDF_DIMDEF(id,'lev',nlev)
   pvarid=NCDF_VARDEF(id,'lev',[pdimid])
   tvarid=NCDF_VARDEF(id,'time',[tdimid])
   avarid=NCDF_VARDEF(id,'ao_index',[pdimid,tdimid])   
   NCDF_CONTROL,id,/ENDEF
;   plevs=[1000,975,950,925,900,850,800,750,700,650,600,550,500,450,400,350,300,250,200,150,100,50,30,10,5,1]
   plevs=[1000,975,950,925,900,875,850,825,800,750,700,650,600,550,500,450,400,350,300,250,200,150,100,50]
   NCDF_VARPUT,id,pvarid,plevs
   NCDF_VARPUT,id,tvarid,findgen(ntime)+0.5
   NCDF_VARPUT,id,avarid,ao_daily
   NCDF_CLOSE,id
   
ENDFOR

STOP
END
