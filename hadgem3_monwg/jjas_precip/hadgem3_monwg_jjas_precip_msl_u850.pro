PRO hadgem3_monwg_jjas_precip_msl_u850

  n_sets=7
  xihvd='/home/ss901165/um_output6/xihvd'
  xihvg='/home/ss901165/um_output6/xihvg'
  xgspj='/home/ss901165/um_output6/xgspj'
  xihvm='/home/ss901165/um_output6/xihvm'
  eraint='/home/ss901165/datasets/ERA-INTERIM'
  trmm='/home/ss901165/datasets/TRMM_3B42V6/n96'

  box=[-50,0,50,360]
  precip_levs=['1','3','5','7','9','11','13','15','17','19','21','23','25']
  precip_diff_levs=['-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5']
  mslp_levs=['990','993','996','999','1002','1005','1008','1011','1014','1017','1020','1023','1026','1029','1032']
;mslp_diff_levs=['-3.2','-2.8','-2.4','-2.2','-1.8','-1.4','-1.0','-0.6','-0.2',$
;                '0.2','0.6','1.0','1.4','1.8','2.2','2.4','2.8','3.2']
  mslp_diff_levs=precip_diff_levs

  n_periods=8
  FOR p=1,n_periods-1 DO BEGIN    
     CASE p OF
        0 : BEGIN
           n_days=120
           offset=150
           period_name='jjas'
           period_name_upper='JJAS'
        END
        1 : BEGIN
           n_days=30
           offset=60
           period_name='mar'
           period_name_upper='March'
        END
        2 : BEGIN
           n_days=30
           offset=90
           period_name='apr'
           period_name_upper='April'
        END
        3 : BEGIN
           n_days=30
           offset=120
           period_name='may'
           period_name_upper='May'
        END
        4 : BEGIN
           n_days=30
           offset=150
           period_name='jun'
           period_name_upper='June'
        END
        5 : BEGIN
           n_days=30
           offset=180
           period_name='jul'
           period_name_upper='July'
        END
        6 : BEGIN
           n_days=30
           offset=210
           period_name='aug'
           period_name_upper='August'
        END
        7 : BEGIN
           n_days=30
           offset=240
           period_name='sep'
           period_name_upper='September'
        END
     ENDCASE
     print,period_name_upper
     FOR i=0,n_sets-1 DO BEGIN
        CASE i OF
           0 : BEGIN
              expt_name='hadgem3kpp_1.5xentrain_ga30_50N50S'
              expt_desc='K!D50!N-ENT-OBS'
              ctrl_name='hadgem3kpp_1.5xentrain_ga30_30N30S'
              ctrl_desc='K!D30!N-ENT-OBS'
              expt_precip_file='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-60.precip.nc'
              trmm_precip_file=trmm+'/TRMM_3B42v6A.jan-dec_dmean_clim.1999-2011.n96.nc'
              expt_precip_varname='precip'
              expt_precip_mult=86400.
              expt_u850_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-60.u850.nc'
              expt_u850_varname='u'         
              expt_v850_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-60.v850.nc'
              expt_v850_varname='v'
              expt_mslp_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-60.mslp.nc'
              expt_mslp_varname='p_1'
              expt_mslp_mult=0.01
              ctrl_precip_file=xihvm+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-25.precip.nc'
              ctrl_precip_varname='precip'
              ctrl_precip_mult=86400.
              ctrl_u850_file=xihvm+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-25.u850.nc'
              ctrl_u850_varname='u'
              ctrl_v850_file=xihvm+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-25.v850.nc'
              ctrl_v850_varname='v'
              ctrl_mslp_file=xihvm+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-25.mslp.nc'
              ctrl_mslp_varname='p_1'
              ctrl_mslp_mult=0.01
              expt_offset=offset
              ctrl_offset=offset
              mslp_diff_levs=['-3.2','-2.8','-2.4','-2.2','-1.8','-1.4','-1.0','-0.6','-0.2',$
                              '0.2','0.6','1.0','1.4','1.8','2.2','2.4','2.8','3.2']         
              wind_diff_mag=1
           END      
           1 : BEGIN
              expt_name='hadgem3kpp_1.5xentrain_ga30_50N50S'
              expt_desc='K!D50!N-ENT-OBS'
              ctrl_name='hadgem3a_ukmo_1.5xentrain_ga30'
              ctrl_desc='A-ENT-OBS'
              expt_precip_file='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-60.precip.nc'
              trmm_precip_file=trmm+'/TRMM_3B42v6A.jan-dec_dmean_clim.1999-2011.n96.nc'
              expt_precip_varname='precip'
              expt_precip_mult=86400.
              expt_u850_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-60.u850.nc'
              expt_u850_varname='u'         
              expt_v850_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-60.v850.nc'
              expt_v850_varname='v'
              expt_mslp_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-60.mslp.nc'
              expt_mslp_varname='p_1'
              expt_mslp_mult=0.01
              ctrl_precip_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.precip.nc'
              ctrl_precip_varname='precip'
              ctrl_precip_mult=86400.
              ctrl_u850_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.u850.nc'
              ctrl_u850_varname='u'
              ctrl_v850_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.v850.nc'
              ctrl_v850_varname='v'
              ctrl_mslp_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.mslp.nc'
              ctrl_mslp_varname='p'
              ctrl_mslp_mult=0.01
              expt_offset=offset
              ctrl_offset=offset
              mslp_diff_levs=['-3.2','-2.8','-2.4','-2.2','-1.8','-1.4','-1.0','-0.6','-0.2',$
                              '0.2','0.6','1.0','1.4','1.8','2.2','2.4','2.8','3.2']
           END
           2 : BEGIN
              expt_name='hadgem3kpp_1.5xentrain_ga30_50N50S'
              expt_desc='K!D50!N-ENT-OBS'
              ctrl_name='hadgem3a_kpp50N50S_1.5xentrain_ga30'
              ctrl_desc='A-ENT-K!D50,cl!N'
              expt_precip_file='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-60.precip.nc'
              trmm_precip_file=trmm+'/TRMM_3B42v6A.jan-dec_dmean_clim.1999-2011.n96.nc'
              expt_precip_varname='precip'
              expt_precip_mult=86400.
              expt_u850_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-60.u850.nc'
              expt_u850_varname='u'         
              expt_v850_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-60.v850.nc'
              expt_v850_varname='v'
              expt_mslp_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-60.mslp.nc'
              expt_mslp_varname='p_1'
              expt_mslp_mult=0.01
              ctrl_precip_file=xihvg+'/hadgem3a_kpp50N50S_1.5xentrain_ga30.jan-dec_dmean_clim.years1-29.precip.nc'
              ctrl_precip_varname='precip'
              ctrl_precip_mult=86400.
              ctrl_u850_file=xihvg+'/hadgem3a_kpp50N50S_1.5xentrain_ga30.jan-dec_dmean_clim.years1-29.u850.nc'
              ctrl_u850_varname='u'
              ctrl_v850_file=xihvg+'/hadgem3a_kpp50N50S_1.5xentrain_ga30.jan-dec_dmean_clim.years1-29.v850.nc'
              ctrl_v850_varname='v'
              ctrl_mslp_file=xihvg+'/hadgem3a_kpp50N50S_1.5xentrain_ga30.jan-dec_dmean_clim.years1-29.mslp.nc'
              ctrl_mslp_varname='p'
              ctrl_mslp_mult=0.01
              expt_offset=offset
              ctrl_offset=offset
              mslp_diff_levs=['-3.2','-2.8','-2.4','-2.2','-1.8','-1.4','-1.0','-0.6','-0.2',$
                              '0.2','0.6','1.0','1.4','1.8','2.2','2.4','2.8','3.2']
           END    
           3 : BEGIN
              expt_name='hadgem3kpp_1.5xentrain_ga30_50N50S'
              expt_desc='K!D50!N-ENT-OBS'
              ctrl_name='eraint_trmm'
              ctrl_desc='ERA/TRMM'
              expt_precip_file='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-60.precip.nc'
              trmm_precip_file=trmm+'/TRMM_3B42v6A.jan-dec_dmean_clim.1999-2011.n96.nc'
              expt_precip_varname='precip'
              expt_precip_mult=86400.
              expt_u850_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-60.u850.nc'
              expt_u850_varname='u'         
              expt_v850_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-60.v850.nc'
              expt_v850_varname='v'
              expt_mslp_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-60.mslp.nc'
              expt_mslp_varname='p_1'
              expt_mslp_mult=0.01
              ctrl_precip_file=trmm+'/TRMM_3B42v6A.jan-dec_dmean_clim.1999-2011.n96.nc'
              ctrl_precip_varname='precip'
              ctrl_precip_mult=1.
              ctrl_u850_file=eraint+'/U850/U850.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
              ctrl_u850_varname='U'
              ctrl_v850_file=eraint+'/V850/V850.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
              ctrl_v850_varname='V'
              ctrl_mslp_file=eraint+'/MSL/MSL.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
              ctrl_mslp_varname='MSL'
              ctrl_mslp_mult=0.01
              expt_offset=offset
              ctrl_offset=offset
              precip_diff_levs=['-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5']
              mslp_diff_levs=precip_diff_levs
              wind_diff_mag=3
           END      
           4 : BEGIN
              expt_name='hadgem3kpp_1.5xentrain_ga30_30N30S'
              expt_desc='K!D30!N-ENT-OBS'
              ctrl_name='eraint_trmm'
              ctrl_desc='ERA/TRMM'
              expt_precip_file=xihvm+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-25.precip.nc'
              trmm_precip_file=trmm+'/TRMM_3B42v6A.jan-dec_dmean_clim.1999-2011.n96.nc'
              expt_precip_varname='precip'
              expt_precip_mult=86400.
              expt_u850_file=xihvm+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-25.u850.nc'
              expt_u850_varname='u'         
              expt_v850_file=xihvm+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-25.v850.nc'
              expt_v850_varname='v'
              expt_mslp_file=xihvm+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-25.mslp.nc'
              expt_mslp_varname='p_1'
              expt_mslp_mult=0.01
              ctrl_precip_file=trmm+'/TRMM_3B42v6A.jan-dec_dmean_clim.1999-2011.n96.nc'
              ctrl_precip_varname='precip'
              ctrl_precip_mult=1.
              ctrl_u850_file=eraint+'/U850/U850.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
              ctrl_u850_varname='U'
              ctrl_v850_file=eraint+'/V850/V850.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
              ctrl_v850_varname='V'
              ctrl_mslp_file=eraint+'/MSL/MSL.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
              ctrl_mslp_varname='MSL'
              ctrl_mslp_mult=0.01
              expt_offset=offset
              ctrl_offset=offset
              precip_diff_levs=['-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5']
              mslp_diff_levs=precip_diff_levs
           END      
                                ; 5 : BEGIN
                                ;    expt_name='hadgem3kpp_1.5xentrain_ga30_30N30S'
                                ;    ctrl_name='hadgem3a_ukmo_1.5xentrain_ga30'
                                ;    expt_precip_file=xihvm+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-25.precip.nc'
                                ;    trmm_precip_file=trmm+'/TRMM_3B42v6A.jan-dec_dmean_clim.1999-2011.n96.nc'
                                ;    expt_precip_varname='precip'
                                ;    expt_precip_mult=86400.
                                ;    expt_u850_file=xihvm+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-25.u850.nc'
                                ;    expt_u850_varname='u'         
                                ;    expt_v850_file=xihvm+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-25.v850.nc'
                                ;    expt_v850_varname='v'
                                ;    expt_mslp_file=xihvm+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-25.mslp.nc'
                                ;    expt_mslp_varname='p'
                                ;    expt_mslp_mult=0.01
                                ;    ctrl_precip_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.precip.nc'
                                ;    ctrl_precip_varname='precip'
                                ;    ctrl_precip_mult=86400.
                                ;    ctrl_u850_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.u850.nc'
                                ;    ctrl_u850_varname='u'
                                ;    ctrl_v850_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.v850.nc'
                                ;    ctrl_v850_varname='v'
                                ;    ctrl_mslp_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.mslp.nc'
                                ;    ctrl_mslp_varname='p'
                                ;    ctrl_mslp_mult=0.01
                                ;    expt_offset=135
                                ;    ctrl_offset=offset
                                ;    mslp_diff_levs=['-3.2','-2.8','-2.4','-2.2','-1.8','-1.4','-1.0','-0.6','-0.2',$
                                ;                    '0.2','0.6','1.0','1.4','1.8','2.2','2.4','2.8','3.2']         
                                ; END      
                                ; 6 : BEGIN
                                ;    expt_name='hadgem3a_ukmo_1.5xentrain_ga30'
                                ;    ctrl_name='eraint_trmm'
                                ;    expt_precip_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.precip.nc'
                                ;    trmm_precip_file=trmm+'/TRMM_3B42v6A.jan-dec_dmean_clim.1999-2011.n96.nc'
                                ;    expt_precip_varname='precip'
                                ;    expt_precip_mult=86400.
                                ;    expt_u850_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.u850.nc'
                                ;    expt_u850_varname='u'         
                                ;    expt_v850_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.v850.nc'
                                ;    expt_v850_varname='v'
                                ;    expt_mslp_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.mslp.nc'
                                ;    expt_mslp_varname='p'
                                ;    expt_mslp_mult=0.01
                                ;    ctrl_precip_file=trmm+'/TRMM_3B42v6A.jan-dec_dmean_clim.1999-2011.n96.nc'
                                ;    ctrl_precip_varname='precip'
                                ;    ctrl_precip_mult=1.
                                ;    ctrl_u850_file=eraint+'/U850/U850.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
                                ;    ctrl_u850_varname='U'
                                ;    ctrl_v850_file=eraint+'/V850/V850.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
                                ;    ctrl_v850_varname='V'
                                ;    ctrl_mslp_file=eraint+'/MSL/MSL.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
                                ;    ctrl_mslp_varname='MSL'
                                ;    ctrl_mslp_mult=0.01
                                ;    expt_offset=offset
                                ;    ctrl_offset=offset
                                ;    precip_diff_levs=['-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5']
                                ;    mslp_diff_levs=precip_diff_levs
                                ; END      
           5 : BEGIN
              expt_name='hadgem3a_kpp50N50S_1.5xentrain_ga30'
              expt_desc='A-ENT-K!D50,cl!N'
              ctrl_name='eraint_trmm'
              ctrl_desc='ERA/TRMM'
              expt_precip_file=xihvg+'/hadgem3a_kpp50N50S_1.5xentrain_ga30.jan-dec_dmean_clim.years1-29.precip.nc'
              trmm_precip_file=trmm+'/TRMM_3B42v6A.jan-dec_dmean_clim.1999-2011.n96.nc'
              expt_precip_varname='precip'
              expt_precip_mult=86400.
              expt_u850_file=xihvg+'/hadgem3a_kpp50N50S_1.5xentrain_ga30.jan-dec_dmean_clim.years1-29.u850.nc'
              expt_u850_varname='u'         
              expt_v850_file=xihvg+'/hadgem3a_kpp50N50S_1.5xentrain_ga30.jan-dec_dmean_clim.years1-29.v850.nc'
              expt_v850_varname='v'
              expt_mslp_file=xihvg+'/hadgem3a_kpp50N50S_1.5xentrain_ga30.jan-dec_dmean_clim.years1-29.mslp.nc'
              expt_mslp_varname='p'
              expt_mslp_mult=0.01
              ctrl_precip_file=trmm+'/TRMM_3B42v6A.jan-dec_dmean_clim.1999-2011.n96.nc'
              ctrl_precip_varname='precip'
              ctrl_precip_mult=1.
              ctrl_u850_file=eraint+'/U850/U850.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
              ctrl_u850_varname='U'
              ctrl_v850_file=eraint+'/V850/V850.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
              ctrl_v850_varname='V'
              ctrl_mslp_file=eraint+'/MSL/MSL.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
              ctrl_mslp_varname='MSL'
              ctrl_mslp_mult=0.01
              expt_offset=offset
              ctrl_offset=offset
              precip_diff_levs=['-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5']
              mslp_diff_levs=precip_diff_levs
           END 
           6 : BEGIN
              expt_name='hadgem3a_kpp50N50S_1.5xentrain_ga30'
              expt_desc='A-ENT-K!D50,cl!N'
              ctrl_name='hadgem3a_ukmo_1.5xentrain_ga30'
              ctrl_desc='A-ENT-OBS'
              expt_precip_file=xihvg+'/hadgem3a_kpp50N50S_1.5xentrain_ga30.jan-dec_dmean_clim.years1-29.precip.nc'
              trmm_precip_file=trmm+'/TRMM_3B42v6A.jan-dec_dmean_clim.1999-2011.n96.nc'
              expt_precip_varname='precip'
              expt_precip_mult=86400.
              expt_u850_file=xihvg+'/hadgem3a_kpp50N50S_1.5xentrain_ga30.jan-dec_dmean_clim.years1-29.u850.nc'
              expt_u850_varname='u'         
              expt_v850_file=xihvg+'/hadgem3a_kpp50N50S_1.5xentrain_ga30.jan-dec_dmean_clim.years1-29.v850.nc'
              expt_v850_varname='v'
              expt_mslp_file=xihvg+'/hadgem3a_kpp50N50S_1.5xentrain_ga30.jan-dec_dmean_clim.years1-29.mslp.nc'
              expt_mslp_varname='p'
              expt_mslp_mult=0.01
              ctrl_precip_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.precip.nc'
              ctrl_precip_varname='precip'
              ctrl_precip_mult=86400.
              ctrl_u850_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.u850.nc'
              ctrl_u850_varname='u'
              ctrl_v850_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.v850.nc'
              ctrl_v850_varname='v'
              ctrl_mslp_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.mslp.nc'
              ctrl_mslp_varname='p'
              ctrl_mslp_mult=0.01
              expt_offset=offset
              ctrl_offset=offset
              mslp_diff_levs=['-3.2','-2.8','-2.4','-2.2','-1.8','-1.4','-1.0','-0.6','-0.2',$
                              '0.2','0.6','1.0','1.4','1.8','2.2','2.4','2.8','3.2']         
           END      
        ENDCASE
        
        expt_precip_longitude=OPEN_AND_EXTRACT(expt_precip_file,'longitude')
        expt_precip_latitude=OPEN_AND_EXTRACT(expt_precip_file,'latitude')
        DEFINE_BOUNDARIES,box,expt_precip_latitude,expt_precip_longitude,expt_precip_box_tx,/LIMIT
        precip_nlon=N_ELEMENTS(expt_precip_longitude)
        precip_nlat=N_ELEMENTS(expt_precip_latitude)

        trmm_precip_longitude=OPEN_AND_EXTRACT(trmm_precip_file,'longitude')
        trmm_precip_latitude=OPEN_AND_EXTRACT(trmm_precip_file,'latitude')
        DEFINE_BOUNDARIES,box,trmm_precip_latitude,trmm_precip_longitude,trmm_precip_box_tx,/LIMIT
        trmm_precip_nlon=N_ELEMENTS(trmm_precip_longitude)
        trmm_precip_nlat=N_ELEMENTS(trmm_precip_latitude)

        mslp_longitude=OPEN_AND_EXTRACT(expt_mslp_file,'longitude')
        mslp_latitude=OPEN_AND_EXTRACT(expt_mslp_file,'latitude')
        DEFINE_BOUNDARIES,box,mslp_latitude,mslp_longitude,mslp_box_tx,/LIMIT
        mslp_nlon=N_ELEMENTS(mslp_longitude)
        mslp_nlat=N_ELEMENTS(mslp_latitude)

        u_longitude=OPEN_AND_EXTRACT(expt_u850_file,'longitude')
        u_latitude=OPEN_AND_EXTRACT(expt_u850_file,'latitude')
        DEFINE_BOUNDARIES,box,u_latitude,u_longitude,u_box_tx,/LIMIT
        u_nlon=N_ELEMENTS(u_longitude)
        u_nlat=N_ELEMENTS(u_latitude)

        ctrl_precip_longitude=OPEN_AND_EXTRACT(ctrl_precip_file,'longitude')
        ctrl_precip_latitude=OPEN_AND_EXTRACT(ctrl_precip_file,'latitude')
        DEFINE_BOUNDARIES,box,ctrl_precip_latitude,ctrl_precip_longitude,ctrl_precip_box_tx,/LIMIT
        precip_nlon=N_ELEMENTS(ctrl_precip_longitude)
        precip_nlat=N_ELEMENTS(ctrl_precip_latitude)

        expt_precip=OPEN_AND_EXTRACT(expt_precip_file,expt_precip_varname,$
                                     offset=[expt_precip_box_tx(1),expt_precip_box_tx(0),expt_offset],$
                                     count=[precip_nlon,precip_nlat,n_days])*expt_precip_mult
        trmm_precip=OPEN_AND_EXTRACT(trmm_precip_file,expt_precip_varname,$
                                     offset=[trmm_precip_box_tx(1),trmm_precip_box_tx(0),150],$
                                     count=[precip_nlon,precip_nlat,n_days])
        ctrl_precip=OPEN_AND_EXTRACT(ctrl_precip_file,ctrl_precip_varname,$
                                     offset=[ctrl_precip_box_tx(1),ctrl_precip_box_tx(0),ctrl_offset],$
                                     count=[precip_nlon,precip_nlat,n_days])*ctrl_precip_mult
        expt_u850=OPEN_AND_EXTRACT(expt_u850_file,expt_u850_varname,$
                                   offset=[u_box_tx(1),u_box_tx(0),expt_offset],$
                                   count=[u_nlon,u_nlat,n_days])
        ctrl_u850=OPEN_AND_EXTRACT(ctrl_u850_file,ctrl_u850_varname,$
                                   offset=[u_box_tx(1),u_box_tx(0),ctrl_offset],$
                                   count=[u_nlon,u_nlat,n_days])   
        expt_v850=OPEN_AND_EXTRACT(expt_v850_file,expt_v850_varname,$
                                   offset=[u_box_tx(1),u_box_tx(0),expt_offset],$
                                   count=[u_nlon,u_nlat,n_days])
        ctrl_v850=OPEN_AND_EXTRACT(ctrl_v850_file,ctrl_v850_varname,$
                                   offset=[u_box_tx(1),u_box_tx(0),ctrl_offset],$
                                   count=[u_nlon,u_nlat,n_days])   
        expt_mslp=OPEN_AND_EXTRACT(expt_mslp_file,expt_mslp_varname,$
                                   offset=[mslp_box_tx(1),mslp_box_tx(0),expt_offset],$
                                   count=[mslp_nlon,mslp_nlat,n_days])*expt_mslp_mult
        ctrl_mslp=OPEN_AND_EXTRACT(ctrl_mslp_file,ctrl_mslp_varname,$
                                   offset=[mslp_box_tx(1),mslp_box_tx(0),ctrl_offset],$
                                   count=[mslp_nlon,mslp_nlat,n_days])*ctrl_mslp_mult
        
        expt_precip_clim=fltarr(precip_nlon,precip_nlat)
        trmm_precip_clim=fltarr(precip_nlon,precip_nlat)
        expt_mslp_clim=fltarr(precip_nlon,precip_nlat)
        expt_u850_clim=fltarr(u_nlon,u_nlat)
        expt_v850_clim=fltarr(u_nlon,u_nlat)
        ctrl_precip_clim=fltarr(precip_nlon,precip_nlat)
        ctrl_mslp_clim=fltarr(precip_nlon,precip_nlat)
        ctrl_u850_clim=fltarr(u_nlon,u_nlat)
        ctrl_v850_clim=fltarr(u_nlon,u_nlat)
        
        FOR j=0,precip_nlon-1 DO BEGIN
           FOR k=0,precip_nlat-1 DO BEGIN
              expt_precip_clim(j,k)=MEAN(expt_precip(j,k,*))
              trmm_precip_clim(j,k)=MEAN(trmm_precip(j,k,*))
              expt_mslp_clim(j,k)=MEAN(expt_mslp(j,k,*))         
              ctrl_precip_clim(j,k)=MEAN(ctrl_precip(j,k,*))
              ctrl_mslp_clim(j,k)=MEAN(ctrl_mslp(j,k,*))
           ENDFOR
        ENDFOR
        FOR j=0,u_nlon-1 DO BEGIN
           FOR k=0,u_nlat-1 DO BEGIN
              expt_u850_clim(j,k)=MEAN(expt_u850(j,k,*))
              ctrl_u850_clim(j,k)=MEAN(ctrl_u850(j,k,*))
              expt_v850_clim(j,k)=MEAN(expt_v850(j,k,*))
              ctrl_v850_clim(j,k)=MEAN(ctrl_v850(j,k,*))         
           ENDFOR
        ENDFOR
        IF ctrl_precip_latitude(0) eq expt_precip_latitude(0) THEN BEGIN 
           precip_diff=fltarr(precip_nlon,precip_nlat)
           FOR j=0,precip_nlat-1 DO $
              precip_diff(*,j)=(expt_precip_clim(*,j)-ctrl_precip_clim(*,j))*COS(2*(expt_precip_latitude(j)-20)*!Pi/180.)
        ENDIF ELSE BEGIN
           precip_diff=fltarr(precip_nlon,precip_nlat)
           FOR j=0,precip_nlat-1 DO $
              precip_diff(*,j)=(expt_precip_clim(*,j)-ctrl_precip_clim(*,precip_nlat-j-1))*COS(2*(expt_precip_latitude(j)-20)*!Pi/180.)
        ENDELSE

        psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_msl_u850.'+expt_name+'_'+period_name+'clim.ps'
        PSOPEN,file=psfile,TFONT=2,CHARSIZE=120,FONT=2,MARGIN=1500
        MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
        CS,SCALE=40,NCOLS=N_ELEMENTS(precip_levs)+1,/REV,white=[2,3]
        LEVS,MANUAL=precip_levs
        CON,X=expt_precip_longitude,Y=expt_precip_latitude,FIELD=expt_precip_clim,$
            CB_TITLE='Precipitation (mm day!U-1!N)',TITLE=period_name_upper+' clim precip (colours), MSLP (lines), U850 (vectors) from '+expt_desc,$
            /NOLINES
        LEVS,MANUAL=mslp_levs
        CON,X=mslp_longitude,Y=mslp_latitude,FIELD=expt_mslp_clim,$
            /NOFILL,POSITIVE_STYLE=2,COL=FSC_COLOR('violetred')
        VECT,X=u_longitude,Y=u_latitude,U=expt_u850_clim,V=expt_v850_clim,MAG=5,STRIDE=3
        AXES,XSTEP=30,YSTEP=10
        PSCLOSE,/NOVIEW

        psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_msl_u850.'+ctrl_name+'_'+period_name+'clim.ps'
        PSOPEN,file=psfile,TFONT=2,CHARSIZE=120,FONT=2,MARGIN=1500
        MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
        CS,SCALE=40,NCOLS=N_ELEMENTS(precip_levs)+1,/REV,white=[2,3]
        LEVS,MANUAL=precip_levs
        CON,X=ctrl_precip_longitude,Y=ctrl_precip_latitude,FIELD=ctrl_precip_clim,$
            CB_TITLE='Precipitation (mm day!U-1!N)',TITLE=period_name_upper+' clim precip (colours), MSLP (lines), U850 (vectors) from '+ctrl_desc,$
            /NOLINES
        LEVS,MANUAL=mslp_levs
        CON,X=mslp_longitude,Y=mslp_latitude,FIELD=ctrl_mslp_clim,$
            /NOFILL,POSITIVE_STYLE=2,COL=FSC_COLOR('violetred')
        VECT,X=u_longitude,Y=u_latitude,U=ctrl_u850_clim,V=ctrl_v850_clim,MAG=5,STRIDE=3
        AXES,XSTEP=30,YSTEP=10
        PSCLOSE,/NOVIEW

        psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_msl_u850.'+expt_name+'-minus-'+ctrl_name+'_'+period_name+'clim.ps'
        PSOPEN,file=psfile,TFONT=2,CHARSIZE=120,FONT=2,MARGIN=1500
        MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
        CS,SCALE=1,NCOLS=N_ELEMENTS(precip_diff_levs)+1,/REV,white=[10]
        LEVS,MANUAL=precip_diff_levs
        CON,X=expt_precip_longitude,Y=expt_precip_latitude,FIELD=precip_diff,$
            CB_TITLE='Precipitation (mm day!U-1!N)',TITLE='Diff '+period_name_upper+' precip (colours), MSLP (lines), U850 (vectors) '+expt_desc+' minus '+ctrl_desc,$
            /NOLINES
        LEVS,MANUAL=mslp_diff_levs
        CON,X=mslp_longitude,Y=mslp_latitude,FIELD=expt_mslp_clim-ctrl_mslp_clim,$
            /NOFILL,POSITIVE_STYLE=2,COL=FSC_COLOR('violetred'),NEGATIVE_STYLE=1
        VECT,X=u_longitude,Y=u_latitude,U=expt_u850_clim-ctrl_u850_clim,V=expt_v850_clim-ctrl_v850_clim,MAG=wind_diff_mag,STRIDE=3
        AXES,XSTEP=30,YSTEP=10
        PSCLOSE,/NOVIEW

     ENDFOR
  ENDFOR

  STOP
END
