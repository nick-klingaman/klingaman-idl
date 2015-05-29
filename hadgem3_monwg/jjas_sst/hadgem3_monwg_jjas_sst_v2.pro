PRO hadgem3_monwg_jjas_sst_v2

  
nick_ga30_actl='/home/ss901165/um_output6/xgspo/hadgem3a_ukmo_1.0xentrain_vn78.jan-dec_dmean_clim.years1-20.surf_temp.nc'
nick_ga30_actl_ga30cpl='/home/ss901165/um_output6/xgspr/hadgem3a_ga30cpl_1.0xentrain_vn78.jan-dec_dmean_clim.years1-20.surf_temp.nc'
nick_ga30_actl_ga30cpl='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/ga30_coupled_TandS/hadgem3ao_ajtzr_ga30.jan-dec_dmean-interp-from-mmean_clim.sst.nc'
nick_ga30_kctl='/home/ss901165/um_output6/xgspp/hadgem3kpp_1.0xentrain_ga30.jan-dec_dmean_clim.years1-20.surf_temp.nc'
nick_ga30_kctl_ga30cpl='/home/ss901165/um_output6/xgsps/hadgem3kpp_1.0xentrain_ga30cpl.jan-dec_dmean_clim.years1-20.surf_temp.nc'
nick_ga30_kent='/home/ss901165/um_output6/xgspm/hadgem3kpp_1.5xentrain_ga30.jan-dec_dmean_clim.i2-k1.surf_temp.nc'
nick_ga30_kent_50n50s_relax='/home/ss901165/um_output6/xihvc/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-10.surf_temp.nc'
nick_ga30_kent_50n50s_free='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-60.sst.nc'
nick_ga30_kent_30n30s='/home/ss901165/um_output6/xihvm/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-25.sst.nc'
linda_ga30_kent_30n30s='/net/niagara/export/niagara/data-06/cy000010/um_output/xhwob/hadgem3kpp_1.5xentrain_vn78_360E.jan-dec_dmean_clim.years1-15.surf_temp.nc'

gc2_n96='/home/ss901165/um_output6/gc2/anqjm/hadgem3_gc2_n96_orca025.jan-dec_dmean_clim.years1-29.surf_temp.nc'
ga6_n96='/home/ss901165/um_output6/gc2/antia/hadgem3_ga6_n96.jan-dec_dmean_clim.years1-27.surf_temp.nc'
goml2_n96='/home/ss901165/um_output6/xjhvd/metum-goml2_fwgbln96_1.0xent.jan-dec_dmean_clim.years1-20.surf_temp.nc'

mask_file='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/landfrac_n96e_hadgem3-8.5.nc'

mylevs_diff=['-3.4','-3.0','-2.6','-2.2','-1.8','-1.4','-1.0','-0.6','-0.2','0.2','0.6','1.0','1.4','1.8','2.2','2.6','3.0','3.4']
;mylevs_diff=['-1.7','-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5','1.7']

box=[-50,0,50,360]

mask_lon=OPEN_AND_EXTRACT(mask_file,'longitude')
mask_lat=OPEN_AND_EXTRACT(mask_file,'latitude')
DEFINE_BOUNDARIES,box,mask_lat,mask_lon,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_lon)
mask_nlat=N_ELEMENTS(mask_lat)
mask=REFORM(OPEN_AND_EXTRACT(mask_file,'lsm',$
                             offset=[mask_box_tx(1),mask_box_tx(0)],$
                             count=[mask_nlon,mask_nlat]))

n_sets=9
FOR m=7,n_sets-1 DO BEGIN
   CASE m OF
      0 : BEGIN
         expt_infile=nick_ga30_kctl
         ctrl_infile=nick_ga30_actl
         expt_name='hadgem3kpp_1.0xentrain_ga30'
         expt_varname='temp'
         ctrl_name='meto_analysis'
         expt_desc='K-CTL-OBS'
         ctrl_desc='Obs (1980-2009)'
         expt_offset=135
         ctrl_offset=150
         expt_count=120
         ctrl_count=120
         ctrl_revlat=0
         expt_mult=1.
         ctrl_mult=1.
         this_mylevs_diff=mylevs_diff
         cpl_box=[-30,20,30,200]
      END
      1 : BEGIN
         expt_infile=nick_ga30_kent
         ctrl_infile=nick_ga30_actl
         expt_name='hadgem3kpp_1.5xentrain_ga30'
         ctrl_name='meto_analysis'
         expt_desc='K-ENT-OBS'
         ctrl_desc='Obs (1980-2009)'
         expt_offset=135
         ctrl_offset=150
         expt_count=120
         ctrl_count=120
         ctrl_revlat=0
         expt_mult=1.
         ctrl_mult=1.
         this_mylevs_diff=mylevs_diff
         cpl_box=[-30,20,30,200]
      END   
      2 : BEGIN
         expt_infile=nick_ga30_actl_ga30cpl
         ctrl_infile=nick_ga30_actl
         expt_name='hadgem3a_1.0xentrain_ga30cpl'
         expt_varname='sst'
         ctrl_name='meto_analysis'
         expt_desc='A-CTL-CPL'
         ctrl_desc='Obs (1980-2009)'
         expt_offset=0
         ctrl_offset=0
         expt_count=360
         ctrl_count=360
         ctrl_revlat=0
         expt_mult=1.
         ctrl_mult=1.
         this_mylevs_diff=mylevs_diff
         cpl_box=box
      END   
      3 : BEGIN
         expt_infile=nick_ga30_kctl_ga30cpl
         ctrl_infile=nick_ga30_actl
         expt_name='hadgem3kpp_1.0xentrain_ga30cpl'
         expt_varname='temp'
         ctrl_name='meto_analysis'
         expt_desc='K-CTL-CPL'
         ctrl_desc='Obs (1980-2009)'
         expt_offset=135
         ctrl_offset=150
         expt_count=120
         ctrl_count=120
         ctrl_revlat=0
         expt_mult=1.
         ctrl_mult=1.
         this_mylevs_diff=mylevs_diff
         cpl_box=[-30,20,30,200]
      END   
      4 : BEGIN
         expt_infile=nick_ga30_kent_30n30s
         ctrl_infile=nick_ga30_actl
         expt_name='hadgem3kpp_1.5xentrain_ga30_30N30S'
         expt_varname='temp_2'
         ctrl_name='meto_analysis'
         expt_desc='K!D30!N-ENT-OBS'
         ctrl_desc='Obs (1980-2009)'
         expt_offset=150
         ctrl_offset=150
         expt_count=120
         ctrl_count=120
         ctrl_revlat=0
         expt_mult=1.
         ctrl_mult=1.
         this_mylevs_diff=mylevs_diff
         cpl_box=[-32,0,32,360]
      END
      5 : BEGIN
         expt_infile=nick_ga30_kent_50n50s_free
         ctrl_infile=nick_ga30_actl
         expt_name='hadgem3kpp_1.5xentrain_ga30_50N50S'
         expt_varname='temp_2'
         ctrl_name='meto_analysis'
         expt_desc='K!D50!N-ENT-OBS'
         ctrl_desc='Obs (1980-2009)'
         expt_offset=0
         ctrl_offset=0
         expt_count=360
         ctrl_count=360
         ctrl_revlat=0
         expt_mult=1.
         ctrl_mult=1.
         this_mylevs_diff=mylevs_diff
         cpl_box=[-50,0,54,360]
      END
      6 : BEGIN
         expt_infile=nick_ga30_kent_50n50s_relax
         ctrl_infile=nick_ga30_actl
         expt_name='hadgem3kpp_1.5xentrain_ga30_50N50S_relax'
         expt_varname='temp_1'
         ctrl_name='meto_analysis'
         expt_desc='K-ENT-OBS-50_relax'
         ctrl_desc='Obs (1980-2009)'
         expt_offset=150
         ctrl_offset=150
         expt_count=120
         ctrl_count=120
         ctrl_revlat=0
         expt_mult=1.
         ctrl_mult=1.
         this_mylevs_diff=mylevs_diff
         cpl_box=[-50,0,54,360]
      END
      7 : BEGIN
         expt_infile=goml2_n96
         ctrl_infile=ga6_n96
         expt_name='goml2_n96'
         expt_varname='temp'
         ctrl_name='ga6_n96'
         ctrl_varname='temp'
         expt_desc='MetUM GOML2'
         ctrl_desc='MetUM GA6'
         expt_offset=150
         ctrl_offset=150
         expt_count=120
         ctrl_count=120
         expt_mult=1
         ctrl_mult=1
         this_mylevs_diff=mylevs_diff
         cpl_box=[-50,0,50,360]
         ctrl_revlat=0
      END
      8 : BEGIN
         expt_infile=gc2_n96
         ctrl_infile=ga6_n96
         expt_name='gc2_n96'
         expt_varname='temp'
         ctrl_name='ga6_n96'
         ctrl_varname='temp'
         expt_desc='MetUM GC2'
         ctrl_desc='MetUM GA6'
         expt_offset=150
         ctrl_offset=150
         expt_count=120
         ctrl_count=120
         expt_mult=1
         ctrl_mult=1
         this_mylevs_diff=mylevs_diff
         cpl_box=[-50,0,50,360]
         ctrl_revlat=0
      END
   ENDCASE

                                ; Precip grid information
   expt_longitude=OPEN_AND_EXTRACT(expt_infile,'longitude')
   expt_latitude=OPEN_AND_EXTRACT(expt_infile,'latitude')
   DEFINE_BOUNDARIES,box,expt_latitude,expt_longitude,expt_box_tx,/LIMIT
   expt_nlon=N_ELEMENTS(expt_longitude)
   expt_nlat=N_ELEMENTS(expt_latitude)
   
   ctrl_longitude=OPEN_AND_EXTRACT(ctrl_infile,'longitude')
   ctrl_latitude=OPEN_AND_EXTRACT(ctrl_infile,'latitude')
   DEFINE_BOUNDARIES,box,ctrl_latitude,ctrl_longitude,ctrl_box_tx,/LIMIT
   ctrl_nlon=N_ELEMENTS(ctrl_longitude)
   ctrl_nlat=N_ELEMENTS(ctrl_latitude)

   expt_sst_mean=fltarr(expt_nlon,expt_nlat)
   ctrl_sst_mean=fltarr(ctrl_nlon,ctrl_nlat)
   
   this_sst=REFORM(OPEN_AND_EXTRACT(expt_infile,expt_varname,$
                                       offset=[expt_box_tx(1),expt_box_tx(0),expt_offset],$
                                       count=[expt_nlon,expt_nlat,expt_count]))*expt_mult;+273.15
   FOR j=0,expt_nlon-1 DO $
      FOR k=0,expt_nlat-1 DO $
         expt_sst_mean(j,k)=MEAN(this_sst(j,k,*))
    
    this_sst=REFORM(OPEN_AND_EXTRACT(ctrl_infile,'temp',$
                                        offset=[ctrl_box_tx(1),ctrl_box_tx(0),ctrl_offset],$
                                        count=[ctrl_nlon,ctrl_nlat,ctrl_count]))*ctrl_mult
    FOR j=0,ctrl_nlon-1 DO $
       FOR k=0,ctrl_nlat-1 DO $
          ctrl_sst_mean(j,k)=MEAN(this_sst(j,k,*))    
    IF ctrl_revlat eq 1 THEN BEGIN
       temp=fltarr(ctrl_nlon,ctrl_nlat)
       FOR j=0,ctrl_nlat-1 DO $
          temp(*,j)=ctrl_sst_mean(*,ctrl_nlat-j-1)
       ctrl_sst_mean=temp
    ENDIF

    diff_expt_ctrl=(expt_sst_mean-ctrl_sst_mean)
    diff_expt_ctrl[where(mask eq 1)]=!Values.F_NaN
    FOR j=0,expt_nlon-1 DO BEGIN
       temp=REFORM(diff_expt_ctrl(j,*))
       temp[where(expt_latitude le cpl_box(0) or expt_latitude ge cpl_box(2))]=!Values.F_NaN
       diff_expt_ctrl(j,*)=temp
    ENDFOR

    weights=fltarr(expt_nlon,expt_nlat)
    FOR j=0,expt_nlat-1 DO $
       FOR k=0,expt_nlon-1 DO $
          IF mask(k,j) ne 1 and FINITE(diff_expt_ctrl(k,j)) eq 1 THEN $
             weights(k,j)=COS(expt_latitude(j)*!Pi/180.)
    weights=weights/TOTAL(weights)
    rmse=SQRT(TOTAL(diff_expt_ctrl^2*weights,/NaN))
    print,rmse

;    IF m eq 4 THEN $
;       diff_expt_ctrl=diff_expt_ctrl-MEAN(diff_expt_ctrl,/NaN)

    psfile='/home/ss901165/idl/hadgem3_monwg/jjas_sst/hadgem3_monwg_jjas_sst.'+expt_name+'-minus-'+ctrl_name+'.clim_sst_global.ps'
    PSOPEN,file=psfile,FONT=6,CHARSIZE=150,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=2500,TFONT=6,TCHARSIZE=100,YSIZE=12000,SPACE3=500
    CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1,white=[11]
    MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
    LEVS,MANUAL=this_mylevs_diff
    CON,FIELD=diff_expt_ctrl,X=expt_longitude,Y=expt_latitude,$
        TITLE="Diff in clim SST for "+expt_desc+" minus "+ctrl_desc,CB_TITLE='Kelvin',$
        /NOLINES,/BLOCK
    grey=FSC_COLOR('grey',30)
    FOR j=0,ctrl_nlon-1 DO $
       FOR k=0,ctrl_nlat-1 DO $
          IF mask(j,k) eq 1 THEN $
             GPLOT,X=ctrl_longitude(j),Y=ctrl_latitude(k),SYM=1,SIZE=60,COL=30
;    GPLOT,X=[cpl_box(1),cpl_box(1)],Y=[cpl_box(0),cpl_box(2)],STYLE=1
;    GPLOT,X=[cpl_box(3),cpl_box(3)],Y=[cpl_box(0),cpl_box(2)],STYLE=1
;    GPLOT,X=[cpl_box(1),cpl_box(3)/2],Y=[cpl_box(0),cpl_box(0)],STYLE=1
;    GPLOT,X=[cpl_box(3)/2.+1,cpl_box(3)],Y=[cpl_box(0),cpl_box(0)],STYLE=1
;    GPLOT,X=[cpl_box(1),cpl_box(3)/2],Y=[cpl_box(2),cpl_box(2)],STYLE=1
;    GPLOT,X=[cpl_box(3)/2.+1,cpl_box(3)],Y=[cpl_box(2),cpl_box(2)],STYLE=1
    AXES,XSTEP=30,YSTEP=10;,YMINOR=10,XMINOR=15
    PSCLOSE,/NOVIEW
 ENDFOR

STOP
END
