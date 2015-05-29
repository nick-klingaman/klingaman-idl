PRO hadgem3_monwg_jjas_sfuv250

n_sets=6
xihvd='/home/ss901165/um_output6/xihvd'
xgspj='/home/ss901165/um_output6/xgspj'
xhwob='/home/ss901165/um_output6/xhwob'
eraint='/home/ss901165/datasets/ERA-INTERIM'
trmm='/home/ss901165/datasets/TRMM_3B42V6/n96'

box=[-50,0,50,360]
;precip_levs=['1','3','5','7','9','11','13','15','17','19','21','23','25']
;precip_diff_levs=['-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5']
;sf_levs=['-80','-72','-64','-56','-48','-40','-32','-24','-16','-8','0','8','16','24','32','40','48','56','64','72','80']
sf_levs_diff_era=indgen(25)*2-24
sf_levs_diff_hg3=indgen(25)*2-24
;raw_mag=15
diff_mag_era=3
diff_mag_hg3=3
sf_levs=['-100','-90','-80','-70','-60','-50','-40','-30','-20','-10','0','10','20','30','40','50','60','70','80','90','100']
raw_mag=20
;diff_mag=4

n_days=30
FOR i=0,n_sets-1 DO BEGIN
   CASE i OF      
      2 : BEGIN
         expt_name='hadgem3kpp_1.5xentrain_ga30_50N50S_free'
         ctrl_name='hadgem3a_ukmo_1.5xentrain_ga30'
         expt_sf_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-23.sf250.nc'
         expt_sf_varname='sf'
         expt_sf_mult=1./(1E6)
         expt_u250_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-27.u250.nc'
         expt_u250_varname='u' 
         expt_v250_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-27.v250.nc'
         expt_v250_varname='v'          
         ctrl_sf_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.sf250.nc'
         ctrl_sf_varname='sf'
         ctrl_sf_mult=1./(1E6)
         ctrl_u250_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.u250.nc'
         ctrl_u250_varname='u' 
         ctrl_v250_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.v250.nc'
         ctrl_v250_varname='v'          
         expt_offset=240
         ctrl_offset=240
         expt_longitude_name='longitude'
         expt_latitude_name='latitude'
         ctrl_longitude_name='longitude_1'
         ctrl_latitude_name='latitude_1'
         sf_levs_diff=sf_levs_diff_hg3
         diff_mag=diff_mag_hg3
      END      
      1 : BEGIN
         expt_name='hadgem3kpp_1.5xentrain_ga30_50N50S_free'
         ctrl_name='hadgem3kpp_1.5xentrain_ga30_30N30S'
         expt_sf_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-23.sf250.nc'
         expt_sf_varname='sf'
         expt_sf_mult=1./(1E6)
         expt_u250_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-27.u250.nc'
         expt_u250_varname='u' 
         expt_v250_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-27.v250.nc'
         expt_v250_varname='v'          
         ctrl_sf_file=xhwob+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-14.sf250.nc'
         ctrl_sf_varname='sf'
         ctrl_sf_mult=1./(1E6)
         ctrl_u250_file=xhwob+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-14.u250.nc'
         ctrl_u250_varname='u' 
         ctrl_v250_file=xhwob+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-14.v250.nc'
         ctrl_v250_varname='v'          
         expt_offset=240
         ctrl_offset=225
         expt_longitude_name='longitude_1'
         expt_latitude_name='latitude_1'
         ctrl_longitude_name='longitude'
         ctrl_latitude_name='latitude'
         sf_levs_diff=sf_levs_diff_hg3
         diff_mag=diff_mag_hg3
      END      
      3 : BEGIN
         expt_name='hadgem3kpp_1.5xentrain_ga30_30N30S'
         ctrl_name='hadgem3a_ukmo_1.5xentrain_ga30'
         expt_sf_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-14.sf250.nc'
         expt_sf_varname='sf'
         expt_sf_mult=1./(1E6)
         expt_u250_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-14.u250.nc'
         expt_u250_varname='u' 
         expt_v250_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-14.v250.nc'
         expt_v250_varname='v'          
         ctrl_sf_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.sf250.nc'
         ctrl_sf_varname='sf'
         ctrl_sf_mult=1./(1E6)
         ctrl_u250_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.u250.nc'
         ctrl_u250_varname='u' 
         ctrl_v250_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.v250.nc'
         ctrl_v250_varname='v'          
         expt_offset=225
         ctrl_offset=240
         expt_longitude_name='longitude_1'
         expt_latitude_name='latitude_1'
         ctrl_longitude_name='longitude'
         ctrl_latitude_name='latitude'
         sf_levs_diff=sf_levs_diff_hg3
         diff_mag=diff_mag_hg3
      END
      0 : BEGIN
         expt_name='hadgem3kpp_1.5xentrain_ga30_50N50S_free'
         ctrl_name='eraint'
         expt_sf_file=xhwob+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-23.sf250.nc'
         expt_sf_varname='sf'
         expt_sf_mult=1./(1E6)
         expt_u250_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-27.u250.nc'
         expt_u250_varname='u' 
         expt_v250_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-27.v250.nc'
         expt_v250_varname='v'          
         ctrl_sf_file=eraint+'/STRF250/STRF250.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
         ctrl_sf_varname='STRF'
         ctrl_sf_mult=1./(1E6)
         ctrl_u250_file=eraint+'/U250/U250.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
         ctrl_u250_varname='U' 
         ctrl_v250_file=eraint+'/V250/V250.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
         ctrl_v250_varname='V'          
         expt_offset=240
         ctrl_offset=240
         expt_longitude_name='longitude_1'
         expt_latitude_name='latitude_1'
         ctrl_longitude_name='longitude'
         ctrl_latitude_name='latitude'
         sf_levs_diff=sf_levs_diff_era
         diff_mag=diff_mag_era
      END  
      4 : BEGIN
         expt_name='hadgem3kpp_1.5xentrain_ga30_30N30S'
         ctrl_name='eraint'
         expt_sf_file=xhwob+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-14.sf250.nc'
         expt_sf_varname='sf'
         expt_sf_mult=1./(1E6)
         expt_u250_file=xhwob+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-14.u250.nc'
         expt_u250_varname='u' 
         expt_v250_file=xhwob+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-14.v250.nc'
         expt_v250_varname='v' 
         ctrl_sf_file=eraint+'/STRF250/STRF250.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
         ctrl_sf_varname='STRF'
         ctrl_sf_mult=1./(1E6)
         ctrl_u250_file=eraint+'/U250/U250.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
         ctrl_u250_varname='U' 
         ctrl_v250_file=eraint+'/V250/V250.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
         ctrl_v250_varname='V'          
         expt_offset=225
         ctrl_offset=240
         expt_longitude_name='longitude_1'
         expt_latitude_name='latitude_1'
         ctrl_longitude_name='longitude'
         ctrl_latitude_name='latitude'
         sf_levs_diff=sf_levs_diff_era
         diff_mag=diff_mag_era
      END  
      5 : BEGIN
         expt_name='hadgem3a_ukmo_1.5xentrain_ga30'
         ctrl_name='eraint'
         expt_sf_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.sf250.nc'
         expt_sf_varname='sf'
         expt_sf_mult=1./(1E6)
         expt_u250_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.u250.nc'
         expt_u250_varname='u'
         expt_v250_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.v250.nc'
         expt_v250_varname='v'          
         ctrl_sf_file=eraint+'/STRF250/STRF250.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
         ctrl_sf_varname='STRF'
         ctrl_sf_mult=1./(1E6)
         ctrl_u250_file=eraint+'/U250/U250.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
         ctrl_u250_varname='U' 
         ctrl_v250_file=eraint+'/V250/V250.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
         ctrl_v250_varname='V'          
         expt_offset=240
         ctrl_offset=240
         expt_longitude_name='longitude'
         expt_latitude_name='latitude'
         ctrl_longitude_name='longitude'
         ctrl_latitude_name='latitude'
         sf_levs_diff=sf_levs_diff_era
         diff_mag=diff_mag_era
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

   expt_u_longitude=OPEN_AND_EXTRACT(expt_u250_file,expt_longitude_name)
   expt_u_latitude=OPEN_AND_EXTRACT(expt_u250_file,expt_latitude_name)
   DEFINE_BOUNDARIES,box,expt_u_latitude,expt_u_longitude,expt_u_box_tx,/LIMIT
   sf_nlon=N_ELEMENTS(expt_u_longitude)
   sf_nlat=N_ELEMENTS(expt_u_latitude)

   ctrl_u_longitude=OPEN_AND_EXTRACT(ctrl_u250_file,ctrl_longitude_name)
   ctrl_u_latitude=OPEN_AND_EXTRACT(ctrl_u250_file,ctrl_latitude_name)
   DEFINE_BOUNDARIES,box,ctrl_u_latitude,ctrl_u_longitude,ctrl_u_box_tx,/LIMIT
   u_nlon=N_ELEMENTS(ctrl_u_longitude)
   u_nlat=N_ELEMENTS(ctrl_u_latitude)

   expt_sf=OPEN_AND_EXTRACT(expt_sf_file,expt_sf_varname,$
                                     offset=[expt_sf_box_tx(1),expt_sf_box_tx(0),expt_offset],$
                                     count=[sf_nlon,sf_nlat,n_days])*expt_sf_mult
   ctrl_sf=OPEN_AND_EXTRACT(ctrl_sf_file,ctrl_sf_varname,$
                                offset=[ctrl_sf_box_tx(1),ctrl_sf_box_tx(0),ctrl_offset],$
                            count=[sf_nlon,sf_nlat,n_days])*ctrl_sf_mult
   expt_u250=OPEN_AND_EXTRACT(expt_u250_file,expt_u250_varname,$
                              offset=[expt_u_box_tx(1),expt_u_box_tx(0),expt_offset],$
                              count=[u_nlon,u_nlat,n_days])
   ctrl_u250=OPEN_AND_EXTRACT(ctrl_u250_file,ctrl_u250_varname,$
                              offset=[ctrl_u_box_tx(1),ctrl_u_box_tx(0),ctrl_offset],$
                              count=[u_nlon,u_nlat,n_days])
   expt_v250=OPEN_AND_EXTRACT(expt_v250_file,expt_v250_varname,$
                              offset=[expt_u_box_tx(1),expt_u_box_tx(0),expt_offset],$
                              count=[u_nlon,u_nlat,n_days])
   ctrl_v250=OPEN_AND_EXTRACT(ctrl_v250_file,ctrl_v250_varname,$
                              offset=[ctrl_u_box_tx(1),ctrl_u_box_tx(0),ctrl_offset],$
                              count=[u_nlon,u_nlat,n_days])
      
   expt_sf_clim=fltarr(sf_nlon,sf_nlat)
   expt_u250_clim=fltarr(u_nlon,u_nlat)
   expt_v250_clim=fltarr(u_nlon,u_nlat)
   ctrl_sf_clim=fltarr(sf_nlon,sf_nlat)
   ctrl_u250_clim=fltarr(u_nlon,u_nlat)
   ctrl_v250_clim=fltarr(u_nlon,u_nlat)

   FOR j=0,sf_nlon-1 DO BEGIN
      FOR k=0,sf_nlat-1 DO BEGIN
         expt_sf_clim(j,k)=MEAN(expt_sf(j,k,*))       
         ctrl_sf_clim(j,k)=MEAN(ctrl_sf(j,k,*))
      ENDFOR
   ENDFOR
   FOR j=0,u_nlon-1 DO BEGIN
      FOR k=0,u_nlat-1 DO BEGIN
         expt_u250_clim(j,k)=MEAN(expt_u250(j,k,*))
         ctrl_u250_clim(j,k)=MEAN(ctrl_u250(j,k,*))
         expt_v250_clim(j,k)=MEAN(expt_v250(j,k,*))
         ctrl_v250_clim(j,k)=MEAN(ctrl_v250(j,k,*))         
      ENDFOR
   ENDFOR
   
   sf_diff=expt_sf_clim-ctrl_sf_clim
   u250_diff=expt_u250_clim-ctrl_u250_clim
   v250_diff=expt_v250_clim-ctrl_v250_clim

   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_sfdiv250.'+expt_name+'_sepclim.ps'
   PSOPEN,file=psfile,TFONT=2,CHARSIZE=120,FONT=2,MARGIN=1500
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)      
   LEVS,MANUAL=sf_levs
   CON,X=expt_sf_longitude,Y=expt_sf_latitude,FIELD=expt_sf_clim,$
       TITLE='Sep clim 250 hPa SF (lines) and winds (vectors) from '+expt_name,$
       /NOFILL,POSITIVE_COL=FSC_COLOR('red'),NEGATIVE_COL=FSC_COLOR('blue'),/NOCOLBAR,ZERO_COL=FSC_COLOR('black')
   VECT,X=expt_u_longitude,Y=expt_u_latitude,U=expt_u250_clim,V=expt_v250_clim,MAG=raw_mag,STRIDE=3
   AXES,XSTEP=30,YSTEP=10
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_sfdiv250.'+ctrl_name+'_sepclim.ps'
   PSOPEN,file=psfile,TFONT=2,CHARSIZE=120,FONT=2,MARGIN=1500
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)      
   LEVS,MANUAL=sf_levs
   CON,X=ctrl_sf_longitude,Y=ctrl_sf_latitude,FIELD=ctrl_sf_clim,$
       TITLE='Sep clim 250 hPa SF (lines) and winds (vectors) from '+ctrl_name,$
       /NOFILL,POSITIVE_COL=FSC_COLOR('red'),NEGATIVE_COL=FSC_COLOR('blue'),/NOCOLBAR,ZERO_COL=FSC_COLOR('black')
   VECT,X=ctrl_u_longitude,Y=ctrl_u_latitude,U=ctrl_u250_clim,V=ctrl_v250_clim,MAG=raw_mag,STRIDE=3
   AXES,XSTEP=30,YSTEP=10
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_sfdiv250.'+expt_name+'-minus-'+ctrl_name+'_sepclim.ps'
   PSOPEN,file=psfile,TFONT=2,CHARSIZE=120,FONT=2,MARGIN=1500,TCHARSIZE=80
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
   CS,SCALE=1,NCOLS=N_ELEMENTS(sf_levs_diff)+1,/REV,white=[8]
   LEVS,MANUAL=sf_levs_diff
   CON,X=ctrl_sf_longitude,Y=ctrl_sf_latitude,FIELD=sf_diff,$
       TITLE='Sep clim 250 hPa SF (lines) and winds (vectors) from '+expt_name+' minus '+ctrl_name,$
       /NOFILL,POSITIVE_COL=FSC_COLOR('red'),NEGATIVE_COL=FSC_COLOR('blue'),/NOCOLBAR,ZERO_COL=FSC_COLOR('black')
   VECT,X=expt_u_longitude,Y=expt_u_latitude,U=u250_diff,V=v250_diff,MAG=diff_mag,STRIDE=3
   AXES,XSTEP=30,YSTEP=10
   PSCLOSE,/NOVIEW


END

STOP
END
