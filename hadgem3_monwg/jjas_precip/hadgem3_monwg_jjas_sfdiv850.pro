PRO hadgem3_monwg_jjas_sfdiv850

n_sets=2
xihvd='/home/ss901165/um_output6/xihvd'
xgspj='/home/ss901165/um_output6/xgspj'
xhwob='/home/ss901165/um_output6/xhwob'
eraint='/home/ss901165/datasets/ERA-INTERIM'
trmm='/home/ss901165/datasets/TRMM_3B42V6/n96'

box=[-50,0,50,360]
;precip_levs=['1','3','5','7','9','11','13','15','17','19','21','23','25']
;precip_diff_levs=['-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5']
sf_levs=['-16','-14','-12','-10','-8','-6','-4','-2','0','2','4','6','8','10','12','14','16']
div_levs=['-15','-13','-11','-9','-7','-5','-3','-1','1','3','5','7','9','11','13','15']
sf_levs_diff=['-2.2','-1.8','-1.4','-1.0','-0.6','-0.2','0.2','0.6','1.0','1.4','1.8','2.2']
div_levs_diff=['-3.3','-2.7','-2.1','-1.5','-0.9','-0.3','0.3','0.9','1.5','2.1','2.7','3.3']

n_days=120
FOR i=1,n_sets-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         expt_name='hadgem3kpp_1.5xentrain_ga30_50N50S_free'
         ctrl_name='hadgem3kpp_1.5xentrain_ga30_30N30S'
         expt_precip_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-23.precip.nc'
         trmm_precip_file=trmm+'/TRMM_3B42v6A.jan-dec_dmean_clim.1999-2011.n96.nc'
         expt_precip_varname='precip'
         expt_precip_mult=86400.
         expt_u850_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-20.u850.nc'
         expt_u850_varname='u'         
         expt_v850_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-20.v850.nc'
         expt_v850_varname='v'
         expt_mslp_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-20.mslp.nc'
         expt_mslp_varname='p_1'
         expt_mslp_mult=0.01
         ctrl_precip_file=xhwob+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-15.precip.nc'
         ctrl_precip_varname='precip'
         ctrl_precip_mult=86400.
         ctrl_u850_file=xhwob+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-14.u850.nc'
         ctrl_u850_varname='u'
         ctrl_v850_file=xhwob+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-14.v850.nc'
         ctrl_v850_varname='v'
         ctrl_mslp_file=xhwob+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-15.mslp.nc'
         ctrl_mslp_varname='p'
         ctrl_mslp_mult=0.01
         expt_offset=150
         ctrl_offset=135
         mslp_diff_levs=['-3.2','-2.8','-2.4','-2.2','-1.8','-1.4','-1.0','-0.6','-0.2',$
                         '0.2','0.6','1.0','1.4','1.8','2.2','2.4','2.8','3.2']         
      END      
      1 : BEGIN
         expt_name='hadgem3kpp_1.5xentrain_ga30_50N50S_free'
         ctrl_name='hadgem3a_ukmo_1.5xentrain_ga30'
         expt_sf_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-23.sf850.nc'
         expt_sf_varname='sf'
         expt_sf_mult=1./(1E6)
         expt_div_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-23.div850.nc'
         expt_div_varname='div' 
         expt_div_mult=1E6
         ctrl_sf_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.sf850.nc'
         ctrl_sf_varname='sf'
         ctrl_sf_mult=1./(1E6)
         ctrl_div_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.div850.nc'
         ctrl_div_varname='div'
         ctrl_div_mult=1E6
         expt_offset=150
         ctrl_offset=150
      END      
      2 : BEGIN
         expt_name='hadgem3kpp_1.5xentrain_ga30_50N50S_free'
         ctrl_name='eraint_trmm'
         expt_sf_file='/home/ss901165/um_output6/xihvf/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-23.precip.nc'
         trmm_sf_file=trmm+'/TRMM_3B42v6A.jan-dec_dmean_clim.1999-2011.n96.nc'
         expt_sf_varname='precip'
         expt_sf_mult=86400.
         expt_div_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-20.u850.nc'
         expt_div_varname='u'         
         expt_v850_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-20.v850.nc'
         expt_v850_varname='v'
         expt_mslp_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-20.mslp.nc'
         expt_mslp_varname='p_1'
         expt_mslp_mult=0.01
         ctrl_sf_file=trmm+'/TRMM_3B42v6A.jan-dec_dmean_clim.1999-2011.n96.nc'
         ctrl_sf_varname='precip'
         ctrl_sf_mult=1.
         ctrl_div_file=eraint+'/U850/U850.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
         ctrl_div_varname='U'
         ctrl_v850_file=eraint+'/V850/V850.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
         ctrl_v850_varname='V'
         ctrl_mslp_file=eraint+'/MSL/MSL.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
         ctrl_mslp_varname='MSL'
         ctrl_mslp_mult=0.01
         expt_offset=150
         ctrl_offset=150
         sf_diff_levs=['-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5']
         mslp_diff_levs=sf_diff_levs
      END      
      3 : BEGIN
         expt_name='hadgem3kpp_1.5xentrain_ga30_30N30S'
         ctrl_name='eraint_trmm'
         expt_sf_file=xhwob+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-15.precip.nc'
         trmm_sf_file=trmm+'/TRMM_3B42v6A.jan-dec_dmean_clim.1999-2011.n96.nc'
         expt_sf_varname='precip'
         expt_sf_mult=86400.
         expt_div_file=xhwob+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-14.u850.nc'
         expt_div_varname='u'         
         expt_v850_file=xhwob+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-14.v850.nc'
         expt_v850_varname='v'
         expt_mslp_file=xhwob+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-15.mslp.nc'
         expt_mslp_varname='p'
         expt_mslp_mult=0.01
         ctrl_sf_file=trmm+'/TRMM_3B42v6A.jan-dec_dmean_clim.1999-2011.n96.nc'
         ctrl_sf_varname='precip'
         ctrl_sf_mult=1.
         ctrl_div_file=eraint+'/U850/U850.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
         ctrl_div_varname='U'
         ctrl_v850_file=eraint+'/V850/V850.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
         ctrl_v850_varname='V'
         ctrl_mslp_file=eraint+'/MSL/MSL.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
         ctrl_mslp_varname='MSL'
         ctrl_mslp_mult=0.01
         expt_offset=135
         ctrl_offset=150
         sf_diff_levs=['-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5']
         mslp_diff_levs=sf_diff_levs
      END      
      4 : BEGIN
         expt_name='hadgem3kpp_1.5xentrain_ga30_30N30S'
         ctrl_name='hadgem3a_ukmo_1.5xentrain_ga30'
         expt_sf_file=xhwob+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-15.precip.nc'
         trmm_sf_file=trmm+'/TRMM_3B42v6A.jan-dec_dmean_clim.1999-2011.n96.nc'
         expt_sf_varname='precip'
         expt_sf_mult=86400.
         expt_div_file=xhwob+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-14.u850.nc'
         expt_div_varname='u'         
         expt_v850_file=xhwob+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-14.v850.nc'
         expt_v850_varname='v'
         expt_mslp_file=xhwob+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-15.mslp.nc'
         expt_mslp_varname='p'
         expt_mslp_mult=0.01
         ctrl_sf_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.precip.nc'
         ctrl_sf_varname='precip'
         ctrl_sf_mult=86400.
         ctrl_div_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.u850.nc'
         ctrl_div_varname='u'
         ctrl_v850_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.v850.nc'
         ctrl_v850_varname='v'
         ctrl_mslp_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.mslp.nc'
         ctrl_mslp_varname='p'
         ctrl_mslp_mult=0.01
         expt_offset=135
         ctrl_offset=150
         mslp_diff_levs=['-3.2','-2.8','-2.4','-2.2','-1.8','-1.4','-1.0','-0.6','-0.2',$
                         '0.2','0.6','1.0','1.4','1.8','2.2','2.4','2.8','3.2']         
      END      
      5 : BEGIN
         expt_name='hadgem3a_ukmo_1.5xentrain_ga30'
         ctrl_name='eraint_trmm'
         expt_sf_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.precip.nc'
         trmm_sf_file=trmm+'/TRMM_3B42v6A.jan-dec_dmean_clim.1999-2011.n96.nc'
         expt_sf_varname='precip'
         expt_sf_mult=86400.
         expt_div_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.u850.nc'
         expt_div_varname='u'         
         expt_v850_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.v850.nc'
         expt_v850_varname='v'
         expt_mslp_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.mslp.nc'
         expt_mslp_varname='p'
         expt_mslp_mult=0.01
         ctrl_sf_file=trmm+'/TRMM_3B42v6A.jan-dec_dmean_clim.1999-2011.n96.nc'
         ctrl_sf_varname='precip'
         ctrl_sf_mult=1.
         ctrl_div_file=eraint+'/U850/U850.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
         ctrl_div_varname='U'
         ctrl_v850_file=eraint+'/V850/V850.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
         ctrl_v850_varname='V'
         ctrl_mslp_file=eraint+'/MSL/MSL.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
         ctrl_mslp_varname='MSL'
         ctrl_mslp_mult=0.01
         expt_offset=150
         ctrl_offset=150
         sf_diff_levs=['-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5']
         mslp_diff_levs=sf_diff_levs
      END      
   ENDCASE
  
   expt_sf_longitude=OPEN_AND_EXTRACT(expt_sf_file,'longitude')
   expt_sf_latitude=OPEN_AND_EXTRACT(expt_sf_file,'latitude')
   DEFINE_BOUNDARIES,box,expt_sf_latitude,expt_sf_longitude,expt_sf_box_tx,/LIMIT
   sf_nlon=N_ELEMENTS(expt_sf_longitude)
   sf_nlat=N_ELEMENTS(expt_sf_latitude)

   ctrl_sf_longitude=OPEN_AND_EXTRACT(ctrl_sf_file,'longitude')
   ctrl_sf_latitude=OPEN_AND_EXTRACT(ctrl_sf_file,'latitude')
   DEFINE_BOUNDARIES,box,ctrl_sf_latitude,ctrl_sf_longitude,ctrl_sf_box_tx,/LIMIT
   sf_nlon=N_ELEMENTS(ctrl_sf_longitude)
   sf_nlat=N_ELEMENTS(ctrl_sf_latitude)

   expt_sf=OPEN_AND_EXTRACT(expt_sf_file,expt_sf_varname,$
                                     offset=[expt_sf_box_tx(1),expt_sf_box_tx(0),expt_offset],$
                                     count=[sf_nlon,sf_nlat,n_days])*expt_sf_mult
   ctrl_sf=OPEN_AND_EXTRACT(ctrl_sf_file,ctrl_sf_varname,$
                                offset=[ctrl_sf_box_tx(1),ctrl_sf_box_tx(0),ctrl_offset],$
                                count=[sf_nlon,sf_nlat,n_days])*ctrl_sf_mult
   expt_div=OPEN_AND_EXTRACT(expt_div_file,expt_div_varname,$
                              offset=[expt_sf_box_tx(1),expt_sf_box_tx(0),expt_offset],$
                              count=[sf_nlon,sf_nlat,n_days])*expt_div_mult
   ctrl_div=OPEN_AND_EXTRACT(ctrl_div_file,ctrl_div_varname,$
                              offset=[ctrl_sf_box_tx(1),ctrl_sf_box_tx(0),ctrl_offset],$
                              count=[sf_nlon,sf_nlat,n_days])*ctrl_div_mult
      
   expt_sf_clim=fltarr(sf_nlon,sf_nlat)
   expt_div_clim=fltarr(sf_nlon,sf_nlat)
   ctrl_sf_clim=fltarr(sf_nlon,sf_nlat)
   ctrl_div_clim=fltarr(sf_nlon,sf_nlat)
  
   FOR j=0,sf_nlon-1 DO BEGIN
      FOR k=0,sf_nlat-1 DO BEGIN
         expt_sf_clim(j,k)=MEAN(expt_sf(j,k,*))       
         ctrl_sf_clim(j,k)=MEAN(ctrl_sf(j,k,*))
      ENDFOR
   ENDFOR
   FOR j=0,sf_nlon-1 DO BEGIN
      FOR k=0,sf_nlat-1 DO BEGIN
         expt_div_clim(j,k)=MEAN(expt_div(j,k,*))
         ctrl_div_clim(j,k)=MEAN(ctrl_div(j,k,*))
      ENDFOR
   ENDFOR
   sf_diff=expt_sf_clim-ctrl_sf_clim
   div_diff=expt_div_clim-ctrl_div_clim

   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_sfdiv850.'+expt_name+'_jjasclim.ps'
   PSOPEN,file=psfile,TFONT=2,CHARSIZE=120,FONT=2,MARGIN=1500
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)      
   CS,SCALE=1,NCOLS=N_ELEMENTS(div_levs)+1,/REV,white=[10]
   LEVS,MANUAL=div_levs
   CON,X=expt_sf_longitude,Y=expt_sf_latitude,FIELD=expt_div_clim,/NOLINES,CB_TITLE='Divergence (positive for divergence) * 1E6'
   LEVS,MANUAL=sf_levs
   CON,X=expt_sf_longitude,Y=expt_sf_latitude,FIELD=expt_sf_clim,$
       CB_TITLE='Precipitation (mm day!U-1!N)',TITLE='JJAS clim 850 hPa SF (lines) and DIV (colours) from '+expt_name,$
       /NOFILL,POSITIVE_COL=FSC_COLOR('red'),NEGATIVE_COL=FSC_COLOR('blue'),/NOCOLBAR,ZERO_COL=FSC_COLOR('black')
   AXES,XSTEP=30,YSTEP=10
   PSCLOSE,/NOVIEW 

   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_sfdiv850.'+ctrl_name+'_jjasclim.ps'
   PSOPEN,file=psfile,TFONT=2,CHARSIZE=120,FONT=2,MARGIN=1500
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)      
   CS,SCALE=1,NCOLS=N_ELEMENTS(div_levs)+1,/REV,white=[10]
   LEVS,MANUAL=div_levs
   CON,X=ctrl_sf_longitude,Y=ctrl_sf_latitude,FIELD=ctrl_div_clim,/NOLINES,CB_TITLE='Divergence (positive for divergence) * 1E6'
   LEVS,MANUAL=sf_levs
   CON,X=ctrl_sf_longitude,Y=ctrl_sf_latitude,FIELD=ctrl_sf_clim,$
       CB_TITLE='Precipitation (mm day!U-1!N)',TITLE='JJAS clim 850 hPa SF (lines) and DIV (colours) from '+ctrl_name,$
       /NOFILL,POSITIVE_COL=FSC_COLOR('red'),NEGATIVE_COL=FSC_COLOR('blue'),/NOCOLBAR,ZERO_COL=FSC_COLOR('black')
   AXES,XSTEP=30,YSTEP=10
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_sfdiv850.'+expt_name+'-minus-'+ctrl_name+'_jjasclim.ps'
   PSOPEN,file=psfile,TFONT=2,CHARSIZE=120,FONT=2,MARGIN=1500,TCHARSIZE=80
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
   CS,SCALE=1,NCOLS=N_ELEMENTS(div_levs_diff)+1,/REV,white=[8]
   LEVS,MANUAL=div_levs_diff
   CON,X=ctrl_sf_longitude,Y=ctrl_sf_latitude,FIELD=div_diff,/NOLINES,CB_TITLE='Divergence (positive for divergence) * 1E6'
   LEVS,MANUAL=sf_levs_diff
   CON,X=ctrl_sf_longitude,Y=ctrl_sf_latitude,FIELD=sf_diff,$
       CB_TITLE='Precipitation (mm day!U-1!N)',TITLE='JJAS clim 850 hPa SF (lines) and DIV (colours) from '+expt_name+' minus '+ctrl_name,$
       /NOFILL,POSITIVE_COL=FSC_COLOR('red'),NEGATIVE_COL=FSC_COLOR('blue'),/NOCOLBAR,ZERO_COL=FSC_COLOR('black')
   AXES,XSTEP=30,YSTEP=10
   PSCLOSE


END

STOP
END
