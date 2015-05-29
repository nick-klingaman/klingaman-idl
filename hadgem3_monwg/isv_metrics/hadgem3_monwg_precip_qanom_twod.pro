PRO hadgem3_monwg_precip_qanom_twod

box=[10,70,30,90]
box_name='india'
n_models=1
mask_type='land' ; Points to use: 'land' or 'sea' or 'both'

precip_bins=[0.1,0.2,0.3,0.4,0.6,0.8,1.0,1.5,2.0,2.5,3.0,4.0,5.0,6.0,8.0,10.0,12.0,16.,20,27,35]
n_precip_bins=N_ELEMENTS(precip_bins)

; For India land points
;mylevs=['-1.1','-0.9','-0.7','-0.6','-0.5','-0.4','-0.32','-0.24','-0.18','-0.12','-0.06','-0.04','-0.02',$
;        '0.02','0.04','0.06','0.12','0.18','0.24','0.32','0.4','0.5','0.6','0.7','0.9','1.1']

; For WeqIO ocean points
mylevs=['-2.2','-1.8','-1.4','-1.2','-1.0','-0.8','-0.64','-0.46','-0.36','-0.24','-0.12','-0.08','-0.04',$
        '0.04','0.08','0.12','0.24','0.36','0.48','0.64','0.8','1.0','1.2','1.4','1.8','2.2']

FOR i=0,n_models-1 DO BEGIN
   CASE i OF 
      1 : BEGIN
         q_infile='/home/ss901165/datasets/ERA-INTERIM/Q/era_interim.jan-dec_dmeans.1999-2010.q.monsoon_domain.n96.nc'
         q_clim_infile='/home/ss901165/datasets/ERA-INTERIM/Q/era_interim.jan-dec_dmean_clim.1999-2010.q.monsoon_domain.n96.nc'
         q_varname='Q'
         precip_infile='/home/ss901165/datasets/TRMM_3B42V6/n96/TRMM_3B42v6A.jan-dec_dmeans.1999-2010.n96.nc'
         n_years=11
         q_offset_day=150
         precip_offset_day=150
         n_days=122
         q_levs_varname='p'
         q_minlev=100.
         model_name='eraint_trmm'
         mask_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/mask_n96_hadgem3-7.1.nc'
         precip_revlat=1
         precip_mult=1.
      END
      0 : BEGIN
         q_infile='/home/ss901165/um_output3/hadgem3_monwg/akkvi/hadgem3a_captivate_final_n96_amip2_akkvi.jun-sep_dmeans.1979-1989.q.tgrid.nc'
         q_clim_infile='/home/ss901165/um_output3/hadgem3_monwg/akkvi/hadgem3a_captivate_final_n96_amip2_akkvi.jun-sep_dmean_clim.1979-1989.q.tgrid.nc'
         q_varname='q'
         precip_infile='/home/ss901165/um_output3/hadgem3_monwg/akkvi/hadgem3a_captivate_final_n96_amip2_akkvi.jan-dec_dmeans.1979-1989.precip.nc'
         n_years=11
         q_offset_day=0
         precip_offset_day=150
         n_days=120
         q_levs_varname='p'
         q_minlev=100.
         model_name='captivate_final_amip2'
         mask_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/mask_n96_hadgem3-7.1.nc'
         precip_revlat=0
         precip_mult=86400.
      END
      2 : BEGIN
         q_infile='/home/ss901165/um_output4/hadgem3a_amip2_1.0xentrain_rerun_vn74/hadgem3a_amip2_1.0xentrain_rerun_vn74.mar-feb_dmeans.years1-14.q.tgrid.nc'
         q_clim_infile='/home/ss901165/um_output4/hadgem3a_amip2_1.0xentrain_rerun_vn74/hadgem3a_amip2_1.0xentrain_rerun_vn74.mar-feb_dmean_clim.years1-14.q.tgrid.nc'
         q_varname='q'
         precip_infile='/home/ss901165/um_output4/hadgem3a_amip2_1.0xentrain_rerun_vn74/hadgem3a_amip2_1.0xentrain_vn74.jun-sep_dmeans.years1-14.precip.nc'
         n_years=14
         q_offset_day=90
         precip_offset_day=0
         n_days=120
         q_levs_varname='p'
         q_minlev=100.
         model_name='nick_control_climsst'
         mask_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/mask_n96_hadgem3-7.1.nc'
         precip_revlat=0
         precip_mult=86400.
      END
      3 : BEGIN
         q_infile='/home/ss901165/um_output4/hadgem3a_amip2_1.5xentrain_rerun_vn74/hadgem3a_amip2_1.5xentrain_rerun_vn74.mar-feb_dmeans.years1-14.q.tgrid.nc'
         q_clim_infile='/home/ss901165/um_output4/hadgem3a_amip2_1.5xentrain_rerun_vn74/hadgem3a_amip2_1.5xentrain_rerun_vn74.mar-feb_dmean_clim.years1-13.q.tgrid.nc'
         q_varname='q'
         precip_infile='/home/ss901165/um_output4/hadgem3a_amip2_1.5xentrain_rerun_vn74/hadgem3a_amip2_1.5xentrain_vn74.jun-sep_dmeans.years1-13.precip.nc'
         n_years=13
         q_offset_day=90
         precip_offset_day=0
         n_days=120
         q_levs_varname='p'
         q_minlev=100.
         model_name='nick_1.5xentrain_climsst'
         mask_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/mask_n96_hadgem3-7.1.nc'
         precip_revlat=0
         precip_mult=86400.
      END
   ENDCASE

   q_longitude=OPEN_AND_EXTRACT(q_infile,'longitude')
   q_latitude=OPEN_AND_EXTRACT(q_infile,'latitude')
   DEFINE_BOUNDARIES,box,q_latitude,q_longitude,q_box_tx,/LIMIT
   q_nlon=N_ELEMENTS(q_longitude)
   q_nlat=N_ELEMENTS(q_latitude)
   q_levs=OPEN_AND_EXTRACT(q_infile,q_levs_varname)
   q_levs=q_levs[where(q_levs ge q_minlev)]
   q_nlevs=N_ELEMENTS(q_levs)

   precip_longitude=OPEN_AND_EXTRACT(precip_infile,'longitude')
   precip_latitude=OPEN_AND_EXTRACT(precip_infile,'latitude')
   DEFINE_BOUNDARIES,box,precip_latitude,precip_longitude,precip_box_tx,/LIMIT
   precip_nlon=N_ELEMENTS(precip_longitude)
   precip_nlat=N_ELEMENTS(precip_latitude)

   mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
   mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
   DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
   mask_nlon=N_ELEMENTS(mask_longitude)
   mask_nlat=N_ELEMENTS(mask_latitude)
   mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                         offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                         count=[mask_nlon,mask_nlat,1,1]))

   q_allyears = OPEN_AND_EXTRACT(q_infile,q_varname,$
                                 offset=[q_box_tx(1),q_box_tx(0),0,q_offset_day,0],$
                                 count=[q_nlon,q_nlat,q_nlevs,n_days,n_years])*1000.

   q_clim = OPEN_AND_EXTRACT(q_clim_infile,q_varname,$
                             offset=[q_box_tx(1),q_box_tx(0),0,q_offset_day],$
                             count=[q_nlon,q_nlat,q_nlevs,n_days])*1000.
   
   precip_allyears = OPEN_AND_EXTRACT(precip_infile,'precip',$
                                      offset=[precip_box_tx(1),precip_box_tx(0),precip_offset_day,0],$
                                      count=[precip_nlon,precip_nlat,n_days,n_years])*precip_mult

   IF precip_revlat eq 1 THEN BEGIN
      temp=fltarr(precip_nlon,precip_nlat,n_days,n_years)
      FOR j=0,precip_nlat-1 DO $
         temp(*,j,*,*)=precip_allyears(*,mask_nlat-j-1,*,*)
      precip_allyears=temp
   ENDIF

   q_anom_allyears = fltarr(q_nlon,q_nlat,q_nlevs,n_days,n_years)
   FOR j=0,n_years-1 DO $
      q_anom_allyears(*,*,*,*,j)=REFORM(q_allyears(*,*,*,*,j))-q_clim(*,*,*,*)

   FOR j=0,n_years-1 DO BEGIN
      FOR k=0,n_days-1 DO BEGIN
         temp_precip=REFORM(precip_allyears(*,*,k,j))
         IF mask_type eq 'land' THEN BEGIN
            temp_precip[where(mask eq 0)]=-999.
         ENDIF ELSE IF mask_type eq 'ocean' THEN BEGIN
            IF TOTAL(where(mask ne 0)) ne -1 THEN $
               temp_precip[where(mask ne 0)]=0.
         ENDIF
         precip_allyears(*,*,k,j)=temp_precip
      ENDFOR
   ENDFOR
            
   q_binned = fltarr(n_precip_bins,q_nlevs)
   n_pts = fltarr(n_precip_bins)
   FOR j=0,q_nlevs-1 DO BEGIN
      this_qlev = REFORM(q_anom_allyears(*,*,j,*,*))
      FOR k=0,n_precip_bins-1 DO BEGIN
         IF k ne n_precip_bins-1 THEN BEGIN
            valid_points = where(precip_allyears ge precip_bins(k) and precip_allyears le precip_bins(k+1))
         ENDIF ELSE $
            valid_points = where(precip_allyears ge precip_bins(k))
         IF TOTAL(valid_points) ne -1 THEN BEGIN
            q_binned(k,j)=MEAN(this_qlev[valid_points])
            IF j eq 0 THEN $
               n_pts(k)=N_ELEMENTS(valid_points)
         ENDIF ELSE BEGIN
            q_binned(k,j)=!Values.F_NaN
            n_pts(k)=0
         ENDELSE         
      ENDFOR   
   ENDFOR

   psfile='/home/ss901165/idl/hadgem3_monwg/isv_metrics/hadgem3_monwg_precip_anom_twod.'+model_name+'_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2500,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=0,TFONT=3,$
          TCHARSIZE=100,SPACE3=600,CB_WIDTH=110
   GSET,XMIN=0,XMAX=n_precip_bins,YMIN=MAX(q_levs),YMAX=MIN(q_levs)
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   LEVS,MANUAL=mylevs
   CON,Y=q_levs,X=indgen(n_precip_bins)+0.5,FIELD=q_binned,/BLOCK,/NOLINES,$
       CB_TITLE='Specific humidity (g kg!U-1!N) anomaly from daily climatology',$
       TITLE='Specific humidity anomalies binned by precip rate over '+box_name+' '+mask_type+' - '+model_name
   AXES,YVALS=q_levs,XVALS=indgen(n_precip_bins)+0.5,XLABELS=STRMID(STRTRIM(STRING(precip_bins),1),0,4),$
        ORIENTATION=30,/NORIGHT,YTITLE='Pressure',XTITLE='Daily-mean precipitation rate (mm/day)',NDECS=2        
   GSET,XMIN=0,XMAX=n_precip_bins,YMIN=0,YMAX=0.2
   GPLOT,X=indgen(n_precip_bins)+0.5,Y=n_pts/TOTAL(n_pts),STYLE=2
   AXES,YSTEP=0.02,/ONLYRIGHT,YTITLE='Fraction of points',NDECS=2
   PSCLOSE

ENDFOR

STOP
END

   
