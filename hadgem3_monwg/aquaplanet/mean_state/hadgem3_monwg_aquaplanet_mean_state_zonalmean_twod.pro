PRO hadgem3_monwg_aquaplanet_mean_state_zonalmean_twod

n_plots=5
lat_range=[-50,50]

FOR i=0,n_plots-1 DO BEGIN
   CASE i OF 
      2 : BEGIN
         plot_name='u_plvl'
         nc_field='u_1'
         field_name='zonal wind'
         z_name='p_1'
         multipliers=1.
         ctl_file='/home/ss901165/um_output5/xhccs/time/xhccsa.pm2006-2008.timemean.nc'
         ctl_name='fixnhsol_sin2sin45N_1.5xent'
         expt_file='/home/ss901165/um_output5/xhcct/time/xhccta.pm2006-2008.timemean.nc'                   
         expt_name='fixnhsol_sin2sin45N_2.5xent'
         yaxis_label='Pressure (hPa)'
         levels=['-57','-51','-45','-39','-33','-27','-21','-15','-9','-3','3','9','15','21','27','33','39','45','51','57']
         plot_diff=12
         ;diff_levels=['-11.4','-10.2','-9.0','-7.8','-6.6','-5.4','-4.2','-3.0','-1.8','-0.6','0.6','1.8','3.0','4.2','5.4','6.6','7.8','9.0','10.2','11.4']
         diff_levels=['-5.7','-5.1','-4.5','-3.9','-3.3','-2.7','-2.1','-1.5','-0.9','-0.3','0.3','0.9','1.5','2.1','2.7','3.3','3.9','4.5','5.1','5.7']
         diff_white=[12]
         scale=1
         ystep=-80
         yminor=-40
         rev_color=0
         cb_title='Zonal wind velocity (m s!U-1!N)'
      END
      1 : BEGIN
         plot_name='v_plvl'
         nc_field='v_1'
         field_name='meridional wind'
         z_name='p_1'
         multipliers=1.
         ctl_file='/home/ss901165/um_output5/xhccs/time/xhccsa.pm2006-2008.timemean.nc'
         ctl_name='fixnhsol_sin2sin45N_1.5xent'
         expt_file='/home/ss901165/um_output5/xhcct/time/xhccta.pm2006-2008.timemean.nc'                   
         expt_name='fixnhsol_sin2sin45N_2.5xent'
         yaxis_label='Pressure (hPa)'
         levels=['-4.5','-3.9','-3.3','-2.7','-2.1','-1.5','-0.9','-0.3','0.3','0.9','1.5','2.1','2.7','3.3','3.9','4.5']
         plot_diff=1
                                ;diff_levels=['-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5']
         diff_levels=['-0.75','-0.65','-0.55','-0.45','-0.35','-0.25','-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75']
         diff_white=[10]
         scale=1
         ystep=-80
         yminor=-40
         rev_color=0
         cb_title='Meridional wind velocity (m s!U-1!N)'
      END
      3 : BEGIN
         plot_name='q_plvl'
         nc_field='q'
         field_name='specific humidity'
         z_name='p_1'
         multipliers=1000.
         ctl_file='/home/ss901165/um_output5/xhccs/time/xhccsa.pm2006-2008.timemean.nc'
         ctl_name='fixnhsol_sin2sin45N_1.5xent'
         expt_file='/home/ss901165/um_output5/xhcct/time/xhccta.pm2006-2008.timemean.nc'                   
         expt_name='fixnhsol_sin2sin45N_2.5xent'
         yaxis_label='Pressure (hPa)'
         levels=['0','0.125','0.25','0.5','1','1.5','2','2.5','3','4','5','6','7','8','10','12','14','16','18','20']
         rev_color=1
         plot_diff=1
                                ;diff_levels=['-1.14','-1.02','-0.90','-0.78','-0.66','-0.54','-0.42','-0.30','-0.18','-0.06','0.06','0.18','0.30','0.42','0.54','0.66','0.78','0.90','1.02','1.14']
         diff_levels=['-0.57','-0.51','-0.45','-0.39','-0.33','-0.27','-0.21','-0.15','-0.09','-0.03','0.03','0.09','0.15','0.21','0.27','0.33','0.39','0.45','0.51','0.57']
         diff_white=[12]
         scale=1
         ystep=-80
         yminor=-40
         latitude_name='latitude'
         cb_title='Specific humidity (g kg!U-1!N)'
      END
      4: BEGIN
         plot_name='omega'
         nc_field='omega'
         field_name='vertical velocity'
         z_name='p_1'
         multipliers=1.
         ctl_file='/home/ss901165/um_output5/xhccs/time/xhccsa.pm2006-2008.timemean.nc'
         ctl_name='fixnhsol_sin2sin45N_1.5xent'
         expt_file='/home/ss901165/um_output5/xhcct/time/xhccta.pm2006-2008.timemean.nc'                   
         expt_name='fixnhsol_sin2sin45N_2.5xent'
         yaxis_label='Pressure (hPa)'
         levels=['-0.102','-0.09','-0.078','-0.066','-0.054','-0.042','-0.03','-0.018','-0.006',$
                 '0.006','0.018','0.03','0.042','0.054','0.066','0.078','0.09','0.102']
         rev_color=0
         plot_diff=1
                                ;diff_levels=['-0.060','-0.052','-0.044','-0.036','-0.028','-0.020','-0.012','-0.004',$
                                ;'0.004','0.012','0.020','0.028','0.036','0.044','0.052','0.060']
         diff_levels=['-0.030','-0.026','-0.022','-0.018','-0.014','-0.010','-0.006','-0.002','0.002','0.006','0.010','0.014','0.018','0.022','0.026','0.030']
         diff_white=[10]
         scale=1
         ystep=-80
         yminor=-40
         latitude_name='latitude'
         cb_title='Vertical velocity (Pa s!U-1!N)'
      END
      0 : BEGIN
         plot_name='temp_plvl'
         nc_field='temp_1'
         field_name='temperature'
         z_name='p_1'
         multipliers=1.
         ctl_file='/home/ss901165/um_output5/xhccs/time/xhccsa.pm2006-2008.timemean.nc'
         ctl_plvl_file=ctl_file
         ctl_name='fixnhsol_sin2sin45N_1.5xent'
         expt_file='/home/ss901165/um_output5/xhcct/time/xhccta.pm2006-2008.timemean.nc'                   
         expt_plvl_file=expt_file
         expt_name='fixnhsol_sin2sin45N_2.5xent'         
         yaxis_label='Pressure (hPa)'
         levels=indgen(18)*5+210
         rev_color=0
         plot_diff=1
                                ;diff_levels=['-3.8','-3.4','-3.0','-2.6','-2.2','-1.8','-1.4','-1.0','-0.6','-0.2',$
                                ;'0.2','0.6','1.0','1.4','1.8','2.2','2.6','3.0','3.4','3.8']
         diff_levels=['-1.9','-1.7','-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5','1.7','1.9']
         diff_white=[12]
         scale=1
         ystep=-80
         yminor=-40
         latitude_name='latitude'
         cb_title='Temperature'
      END
   ENDCASE
   
   ctl_longitude=OPEN_AND_EXTRACT(ctl_file,'longitude_1')
   ctl_latitude=OPEN_AND_EXTRACT(ctl_file,'latitude_1')+1.25
   ctl_start_lat=NEAREST(ctl_latitude,lat_range(0))
   ctl_stop_lat=NEAREST(ctl_latitude,lat_range(1))
   ctl_latitude=ctl_latitude(ctl_start_lat:ctl_stop_lat)
   z=OPEN_AND_EXTRACT(ctl_file,z_name)
   n_lon=N_ELEMENTS(ctl_longitude)
   n_lat=N_ELEMENTS(ctl_latitude)
   n_z=N_ELEMENTS(z)

   plvl_longitude=OPEN_AND_EXTRACT(ctl_plvl_file,'longitude')
   plvl_latitude=OPEN_AND_EXTRACT(ctl_plvl_file,'latitude')
   DEFINE_BOUNDARIES,[MIN(ctl_latitude),MIN(ctl_longitude),MAX(ctl_latitude),MAX(ctl_longitude)],plvl_latitude,plvl_longitude,$
                     plvl_box_tx,/LIMIT
   plvl_nlon=N_ELEMENTS(plvl_longitude)
   plvl_nlat=N_ELEMENTS(plvl_latitude)
   ctl_plvl_sfc=REFORM(OPEN_AND_EXTRACT(ctl_plvl_file,'p',$
                                        offset=[plvl_box_tx(1),plvl_box_tx(0),0,0],$
                                        count=[plvl_nlon,plvl_nlat,1,1]))/100.

   exp_longitude=OPEN_AND_EXTRACT(expt_file,'longitude_1')
   exp_latitude=OPEN_AND_EXTRACT(expt_file,'latitude_1')+1.25
   exp_start_lat=NEAREST(exp_latitude,lat_range(0))
   exp_stop_lat=NEAREST(exp_latitude,lat_range(1))
   exp_latitude=exp_latitude(exp_start_lat:exp_stop_lat)
   
   field=fltarr(n_lon,n_lat,n_z)
   ctl_fields=fltarr(n_lat,n_z)
   expt_fields=fltarr(n_lat,n_z)
   
   field=REFORM(OPEN_AND_EXTRACT(ctl_file,nc_field,$
                                 offset=[0,ctl_start_lat,0],count=[n_lon,n_lat,n_z]))*multipliers
   FOR k=0,n_lat-1 DO $
      FOR m=0,n_z-1 DO $
         ctl_fields(k,m)=MEAN(field(*,k,m))

   field=REFORM(OPEN_AND_EXTRACT(expt_file,nc_field,$
                                 offset=[0,exp_start_lat,0,0],count=[n_lon,n_lat,n_z,1]))*multipliers
   FOR k=0,n_lat-1 DO $
      FOR m=0,n_z-1 DO $
         expt_fields(k,m)=MEAN(field(*,k,m))      
  
   temp=REFORM(ctl_fields(*,0))
   temp[where(ABS(ctl_latitude) gt 33)]=!Values.F_NaN
   ctl_fields(*,0)=temp
   temp=REFORM(ctl_fields(*,1))
   temp[where(ABS(ctl_latitude) gt 45)]=!Values.F_NaN
   ctl_fields(*,1)=temp
   
   temp=REFORM(expt_fields(*,0))
   temp[where(ABS(exp_latitude) gt 33)]=!Values.F_NaN
   expt_fields(*,0)=temp
   temp=REFORM(expt_fields(*,1))
   temp[where(ABS(exp_latitude) gt 45)]=!Values.F_NaN
   expt_fields(*,1)=temp
   
   
   psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/mean_state/hadgem3_monwg_aquaplanet_mean_state_zonalmean_twod.'+plot_name+'.'+ctl_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=3000,SPACE2=1000,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=500,CB_WIDTH=120
   GSET,XMIN=MIN(ctl_latitude),XMAX=MAX(ctl_latitude),YMIN=MAX(z),YMAX=MIN(z),$
        TITLE='Time-mean, zonal-mean '+field_name+' from '+ctl_name
   IF rev_color eq 1 THEN BEGIN
      CS,SCALE=scale,NCOLS=N_ELEMENTS(levels)+1,/REV
   ENDIF ELSE $
      CS,SCALE=scale,NCOLS=N_ELEMENTS(levels)+1
   LEVS,MANUAL=levels
   CON,X=ctl_latitude,Y=z,FIELD=REFORM(ctl_fields(*,*)),$
       /NOLINELABELS,cb_title=cb_title
   AXES,XVALS=indgen(21)*5-50,YSTEP=ystep,YMINOR=yminor,XTITLE='Latitude (degrees north)',YTITLE=yaxis_label
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/mean_state/hadgem3_monwg_aquaplanet_mean_state_zonalmean_twod.'+plot_name+'.'+expt_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=3000,SPACE2=1000,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=500,CB_WIDTH=120
   GSET,XMIN=MIN(ctl_latitude),XMAX=MAX(ctl_latitude),YMIN=MAX(z),YMAX=MIN(z),$
        TITLE='Time-mean, zonal-mean '+field_name+' from '+expt_name
    IF rev_color eq 1 THEN BEGIN
      CS,SCALE=scale,NCOLS=N_ELEMENTS(levels)+1,/REV
   ENDIF ELSE $
      CS,SCALE=scale,NCOLS=N_ELEMENTS(levels)+1
   LEVS,MANUAL=levels
   CON,X=ctl_latitude,Y=z,FIELD=REFORM(expt_fields(*,*)),$
       /NOLINELABELS,cb_title=cb_title
   AXES,XVALS=indgen(21)*5-50,YSTEP=ystep,YMINOR=yminor,XTITLE='Latitude (degrees north)',YTITLE=yaxis_label
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/mean_state/hadgem3_monwg_aquaplanet_mean_state_zonalmean_twod.'+plot_name+'.'+expt_name+'-minus-'+ctl_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=3000,SPACE2=1000,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=500,CB_WIDTH=120
   GSET,XMIN=MIN(ctl_latitude),XMAX=MAX(ctl_latitude),YMIN=MAX(z),YMAX=MIN(z),$
        TITLE='Diff in time-mean, zonal-mean '+field_name+' from '+expt_name+' minus '+ctl_name
   IF rev_color eq 1 THEN BEGIN
      CS,SCALE=scale,NCOLS=N_ELEMENTS(diff_levels)+1,/REV,white=[diff_white]
   ENDIF ELSE $
      CS,SCALE=scale,NCOLS=N_ELEMENTS(diff_levels)+1,white=[diff_white]
   LEVS,MANUAL=diff_levels
   CON,X=ctl_latitude,Y=z,FIELD=REFORM(expt_fields(*,*)-ctl_fields(*,*)),$
       /NOLINELABELS,CB_TITLE=cb_title
   AXES,XVALS=indgen(21)*5-50,YSTEP=ystep,YMINOR=yminor,XTITLE='Latitude (degrees north)',YTITLE=yaxis_label
   PSCLOSE,/NOVIEW

ENDFOR

STOP
END
