PRO hadgem3kpp_cascade_precip_qanom_twod

box=[-15,60,15,180]
box_name='IndPac_WarmPool'
n_models=3
mask_type='sea' ; Points to use: 'land' or 'sea' or 'both'

precip_bins=[0.1,0.2,0.3,0.4,0.6,0.8,1.0,1.5,2.0,2.5,3.0,4.0,5.0,6.0,8.0,10.0,12.0,16.,20,27,35]
n_precip_bins=N_ELEMENTS(precip_bins)

; For India land points
;mylevs=['-1.1','-0.9','-0.7','-0.6','-0.5','-0.4','-0.32','-0.24','-0.18','-0.12','-0.06','-0.04','-0.02',$
;        '0.02','0.04','0.06','0.12','0.18','0.24','0.32','0.4','0.5','0.6','0.7','0.9','1.1']

; For WeqIO ocean points
mylevs=['-1.9','-1.7','-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','-0.03',$
        '0.03','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5','1.7','1.9']

FOR i=0,n_models-1 DO BEGIN
   CASE i OF 
      2 : BEGIN
         q_infile='/home/ss901165/um_output3/hadgem3_obs_hindcasts.all_dates.q_tropics.nc'
         q_varname='Q'
         precip_infile='/home/ss901165/um_output3/hadgem3_obs_hindcasts.all_dates.precip_tropics.nc'
         n_years=26
         q_offset_day=2
         precip_offset_day=2
         n_days=28
         q_levs_varname='p'
         q_minlev=200.
         model_name='eraint_trmm'
         mask_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/mask_n96_hadgem3-7.1.nc'
         precip_revlat=1
         precip_mult=1.
         a=0
      END
      0 : BEGIN
         q_infile='/home/ss901165/um_output3/hadgem3_ctl_hindcasts.all_dates.q_tropics.nc'
         q_varname='q'
         precip_infile='/home/ss901165/um_output3/hadgem3_ctl_hindcasts.all_dates.precip_tropics.nc'
         n_years=26
         q_offset_day=2
         precip_offset_day=2
         n_days=28
         q_levs_varname='p'
         q_minlev=200.
         model_name='hindcasts_ctl'
         mask_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/mask_n96_hadgem3-7.1.nc'
         precip_revlat=0
         precip_mult=86400.
         a=-0.2
      END
      1 : BEGIN
         q_infile='/home/ss901165/um_output3/hadgem3_ent_hindcasts.all_dates.q_tropics.nc'
         q_varname='q'
         precip_infile='/home/ss901165/um_output3/hadgem3_ent_hindcasts.all_dates.precip_tropics.nc'
         n_years=26
         q_offset_day=2
         precip_offset_day=2
         n_days=28
         q_levs_varname='p'
         q_minlev=200.
         model_name='hindcasts_ent'
         mask_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/mask_n96_hadgem3-7.1.nc'
         precip_revlat=0
         precip_mult=86400.
         a=0.2
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

   precip_allyears = REFORM(OPEN_AND_EXTRACT(precip_infile,'precip',$
                                             offset=[precip_box_tx(1),precip_box_tx(0),precip_offset_day,0],$
                                             count=[precip_nlon,precip_nlat,n_days,n_years]))*precip_mult
   
   IF precip_revlat eq 1 THEN BEGIN
      temp=fltarr(precip_nlon,precip_nlat,n_days,n_years)
      FOR j=0,precip_nlat-1 DO $
         temp(*,j,*,*)=precip_allyears(*,mask_nlat-j-1,*,*)
      precip_allyears=temp
   ENDIF

   q_anom_allyears = fltarr(q_nlon,q_nlat,q_nlevs,n_days,n_years)
   FOR j=0,n_years-1 DO $
      FOR k=0,n_days-1 DO $
         FOR m=0,q_nlevs-1 DO $
            FOR n=0,q_nlat-1 DO $
               q_anom_allyears(*,n,m,k,j)=REFORM(q_allyears(*,n,m,k,j))-MEAN(q_allyears(*,n,m,k,j))   
   
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
      IF q_levs(j) gt 400 THEN BEGIN
         print,'Using a'
         this_qlev = this_qlev+a
      ENDIF
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

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_precip_anom_twod.'+model_name+'_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=140,MARGIN=3500,SPACE1=300,SPACE2=1000,XOFFSET=500,YOFFSET=0,TFONT=3,$
          TCHARSIZE=100,SPACE3=1000,CB_WIDTH=110
   GSET,XMIN=0,XMAX=n_precip_bins,YMIN=MAX(q_levs),YMAX=200
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   LEVS,MANUAL=mylevs
   CON,Y=q_levs,X=indgen(n_precip_bins)+0.5,FIELD=q_binned,/BLOCK,/NOLINES,$
       CB_TITLE='Specific humidity (g kg!U-1!N) anomaly from zonal mean'
       ;TITLE='Specific humidity anomalies binned by precip rate over '+box_name+' '+mask_type+' - '+model_name
   GPLOT,X=[NEAREST(precip_bins,2.5),NEAREST(precip_bins,2.5)]+0.5,Y=[MAX(q_levs),200],STYLE=2   
   GPLOT,X=[NEAREST(precip_bins,1.5),NEAREST(precip_bins,1.5)]+0.5,Y=[MAX(q_levs),200],STYLE=2
   GPLOT,X=[NEAREST(precip_bins,4.0),NEAREST(precip_bins,4)]+0.5,Y=[MAX(q_levs),200],STYLE=2
   GPLOT,X=[NEAREST(precip_bins,6.01),NEAREST(precip_bins,6.01)]+0.5,Y=[MAX(q_levs),200],STYLE=2
   GPLOT,X=[NEAREST(precip_bins,16.01),NEAREST(precip_bins,16.01)]+0.5,Y=[MAX(q_levs),200],STYLE=2
   AXES,YSTEP=-50.,YMINOR=-25.,$
        XVALS=indgen(n_precip_bins+1),XLABELS=[STRMID(STRTRIM(STRING(precip_bins),1),0,4),'> 35.0'],$
        ORIENTATION=20,/NORIGHT,YTITLE='Pressure (hPa)',XTITLE='Daily-mean precipitation rate (mm/day)',NDECS=2        
   GSET,XMIN=0,XMAX=n_precip_bins,YMIN=0,YMAX=0.2
   GPLOT,X=indgen(n_precip_bins)+0.5,Y=n_pts/TOTAL(n_pts),STYLE=0
   AXES,YSTEP=0.02,/ONLYRIGHT,YTITLE='Fraction of grid points',NDECS=2
   PSCLOSE

ENDFOR

STOP
END

   
