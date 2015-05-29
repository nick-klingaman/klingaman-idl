PRO mjo_iav_regressions_runningmean

n_fields=1

rmm_infile='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmeans.1-77.rmm_indices.nc'
rmm1=OPEN_AND_EXTRACT(rmm_infile,'rmm1')
rmm2=OPEN_AND_EXTRACT(rmm_infile,'rmm2')
rmm1_ts=fltarr(N_ELEMENTS(rmm1))
rmm2_ts=fltarr(N_ELEMENTS(rmm2))
FOR i=0,N_ELEMENTS(rmm1(*,0))-1 DO BEGIN
   rmm1_ts(i*360:(i+1)*360-1)=rmm1(i,*)
   rmm2_ts(i*360:(i+1)*360-1)=rmm2(i,*)
ENDFOR
amp_ts=(rmm1_ts^2+rmm2_ts^2)^0.5

n_time=N_ELEMENTS(rmm1_ts)
running_mean_rmm1=fltarr(n_time)
running_mean_rmm2=fltarr(n_time)
running_mean_amp=fltarr(n_time)
running_var_amp=fltarr(n_time)
FOR i=180,n_time-181 DO BEGIN
   running_mean_rmm1(i)=MEAN(rmm1_ts(i-180:i+180))
   running_mean_rmm2(i)=MEAN(rmm2_ts(i-180:i+180))
   running_mean_amp(i)=MEAN(amp_ts(i-180:i+180))
   running_var_amp(i)=STDDEV(amp_ts(i-180:i+180))
ENDFOR

FOR i=0,n_fields-1 DO BEGIN
   CASE i OF
      8 : BEGIN
         infile='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmeans.years1-77.precip.nc'
         nc_varname='precip'
         file_varname='precip'
         plot_varname='precipitation'
         units='mm day!U-1!N'
         box=[-30,0,30,360]
         xname='longitude'
         yname='latitude'
         nyears=77
         ndays_per_year=360
         use_map=1
         field_multiplier=86400.
         regression_levels=['-4.5','-3.9','-3.3','-2.7','-2.1','-1.5','-0.9','-0.3','0.3','0.9','1.5','2.1','2.7','3.3','3.9','4.5']
         correlation_levels=['-0.55','-0.45','-0.35','-0.25','-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55']
         colrev=1
         yrev=0
         ystep=10
         xstep=30
      END
      0 : BEGIN
         infile='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmeans.years1-77.u200.nc'
         nc_varname='u'
         file_varname='u200'
         plot_varname='200 hPa zonal wind'         
         units='m s!U-1!N'
         box=[-90,0,90,360]
         xname='longitude'
         yname='latitude'
         nyears=77
         ndays_per_year=360
         use_map=1
         field_multiplier=1.
         regression_levels=['-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5']
         correlation_levels=['-0.55','-0.45','-0.35','-0.25','-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55']
         colrev=0
         yrev=0
         ystep=10
         xstep=30
      END
      9 : BEGIN
         infile='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmeans.years1-77.q_lonavg.nc'
         nc_varname='q'
         file_varname='q_lonavg'
         plot_varname='specific humidity (lonavg)'
         units='g kg!U-1!N'
         box=[925,-40,200,40]
         xname='latitude'
         yname='p'
         nyears=77
         ndays_per_year=360
         use_map=0
         field_multiplier=1000.
         regression_levels=['-0.55','-0.45','-0.35','-0.30','-0.25','-0.20','-0.15','-0.10','-0.06','-0.02',$
                            '0.02','0.06','0.10','0.15','0.20','0.25','0.30','0.35','0.45','0.55']
         colrev=1
         yrev=1
         ystep=-50
         xstep=10
         ytitle='Pressure (hPa)'
         xtitle='Latitude (degrees north)'
      END
      1 : BEGIN
         infile='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmeans.years1-77.q_latavg_15S-15N.nc'
         nc_varname='q'
         file_varname='q_latavg15S-15N'
         plot_varname='specific humidity (latavg 15S-15N)'
         units='g kg!U-1!N'
         box=[925,0,200,360]
         xname='longitude'
         yname='p'
         nyears=77
         ndays_per_year=360
         use_map=0         
         field_multiplier=1000.
         regression_levels=['-0.55','-0.45','-0.35','-0.30','-0.25','-0.20','-0.15','-0.10','-0.06','-0.02',$
                            '0.02','0.06','0.10','0.15','0.20','0.25','0.30','0.35','0.45','0.55']
         colrev=1
         yrev=1
         ystep=-50
         xstep=30
         ytitle='Pressure (hPa)'
         xtitle='Longitude (degrees east)'
      END
      2 : BEGIN
         infile='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmeans.years1-77.omega_latavg15S-15N.nc'
         nc_varname='omega'
         file_varname='omega_latavg15S-15N'
         plot_varname='omega (latavg 15S-15N)'         
         units='Pa s!U-1!N'
         box=[925,0,200,360]
         xname='longitude'
         yname='p'
         nyears=77
         ndays_per_year=360
         use_map=0
         field_multiplier=1.
         regression_levels=['-0.026','-0.022','-0.018','-0.014','-0.010','-0.006','-0.002',$
                            '0.002','0.006','0.010','0.014','0.018','0.022','0.026']
         colrev=0
         yrev=1
         ystep=-50
         xstep=30
         ytitle='Pressure (hPa)'
         xtitle='Longitude (degrees east)'
      END
      3 : BEGIN
         infile='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmeans.years1-77.omega_lonavg.nc'
         nc_varname='omega'
         file_varname='omega_lonavg'
         plot_varname='omega (lonavg)'         
         units='Pa s!U-1!N'
         box=[925,-40,200,40]
         xname='latitude'
         yname='p'
         nyears=77
         ndays_per_year=360
         use_map=0
         field_multiplier=1.
         regression_levels=['-0.013','-0.011','-0.009','-0.007','-0.005','-0.003','-0.001',$
                            '0.001','0.003','0.005','0.007','0.009','0.011','0.013']
         colrev=0
         yrev=1
         ystep=-50
         xstep=10
         ytitle='Pressure (hPa)'
         xtitle='Latitude (degrees north)'
      END
      4 : BEGIN
         infile='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmeans.years1-77.u_latavg15S-15N.nc'
         nc_varname='u'
         file_varname='u_latavg15S-15N'
         plot_varname='zonal wind (latavg 15S-15N)'         
         units='m s!U-1!N'
         box=[925,0,200,360]
         xname='longitude'
         yname='p'
         nyears=77
         ndays_per_year=360
         use_map=0
         field_multiplier=1.
         regression_levels=['-3.0','-2.6','-2.2','-1.8','-1.4','-1.0','-0.6','-0.2',$
                            '0.2','0.6','1.0','1.4','1.8','2.2','2.6','3.0']
         colrev=0
         yrev=1
         ystep=-50
         xstep=30
         ytitle='Pressure (hPa)'
         xtitle='Longitude (degrees east)'
      END
      5 : BEGIN
         infile='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmeans.years1-77.u_lonavg.nc'
         nc_varname='u'
         file_varname='u_lonavg'
         plot_varname='zonal wind (lonavg)'         
         units='m s!U-1!N'
         box=[925,-40,200,40]
         xname='latitude'
         yname='p'
         nyears=77
         ndays_per_year=360
         use_map=0
         field_multiplier=1.
         regression_levels=['-3.0','-2.6','-2.2','-1.8','-1.4','-1.0','-0.6','-0.2',$
                            '0.2','0.6','1.0','1.4','1.8','2.2','2.6','3.0']       
         colrev=0
         yrev=1
         ystep=-50
         xstep=10
         ytitle='Pressure (hPa)'
         xtitle='Latitude (degrees north)'
      END      
      6 : BEGIN
         infile='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmeans.years1-77.v_latavg15S-15N.nc'
         nc_varname='v'
         file_varname='v_latavg15S-15N'
         plot_varname='meridional wind (latavg 15S-15N)'         
         units='m s!U-1!N'
         box=[925,0,200,360]
         xname='longitude'
         yname='p'
         nyears=77
         ndays_per_year=360
         use_map=0
         field_multiplier=1.
         regression_levels=['-0.75','-0.65','-0.55','-0.45','-0.35','-0.25','-0.15','-0.05',$
                            '0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75']
         colrev=0
         yrev=1
         ystep=-50
         xstep=30
         ytitle='Pressure (hPa)'
         xtitle='Longitude (degrees east)'
      END
      7 : BEGIN
         infile='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmeans.years1-77.v_lonavg.nc'
         nc_varname='v'
         file_varname='v_lonavg'
         plot_varname='meridional wind (lonavg)'         
         units='m s!U-1!N'
         box=[925,-40,200,40]
         xname='latitude'
         yname='p'
         nyears=77
         ndays_per_year=360
         use_map=0
         field_multiplier=1.
         regression_levels=['-0.75','-0.65','-0.55','-0.45','-0.35','-0.25','-0.15','-0.05',$
                            '0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75']
         colrev=0
         yrev=1
         ystep=-50
         xstep=10
         ytitle='Pressure (hPa)'
         xtitle='Latitude (degrees north)'
      END      
   ENDCASE
   
   xdim=OPEN_AND_EXTRACT(infile,xname)
   ydim=OPEN_AND_EXTRACT(infile,yname)
   DEFINE_BOUNDARIES,box,ydim,xdim,box_tx,/LIMIT
   nx=N_ELEMENTS(xdim)
   ny=N_ELEMENTS(ydim)
   
   field=OPEN_AND_EXTRACT(infile,nc_varname,$
                          offset=[box_tx(1),box_tx(0),0,0],count=[nx,ny,ndays_per_year,nyears])*field_multiplier
   field_running_mean=fltarr(nyears*ndays_per_year)
   correlation_with_amplitude=fltarr(nx,ny)
   regression_with_amplitude=fltarr(nx,ny)
   correlation_with_amplitude_variance=fltarr(nx,ny)
   regression_with_amplitude_variance=fltarr(nx,ny)
   FOR j=0,nx-1 DO BEGIN
      print,'x value ',+STRTRIM(STRING(j+1),1)+' of '+STRTRIM(STRING(nx),1)
      FOR k=0,ny-1 DO BEGIN
         temp=fltarr(nyears*ndays_per_year)
         FOR m=0,nyears-1 DO $
            temp(m*ndays_per_year:(m+1)*ndays_per_year-1)=REFORM(field(j,k,*,m))
         FOR m=180,nyears*ndays_per_year-181 DO $
            field_running_mean(m)=MEAN(temp(m-180:m+180))
         correlation_with_amplitude(j,k)=CORRELATE(running_mean_amp(180:nyears*ndays_per_year-181),$
                                              field_running_mean(180:nyears*ndays_per_year-181))
         regression_with_amplitude(j,k)=REGRESS(running_mean_amp(180:nyears*ndays_per_year-181),$
                                                field_running_mean(180:nyears*ndays_per_year-181))
         correlation_with_amplitude_variance(j,k)=CORRELATE(running_var_amp(180:nyears*ndays_per_year-181),$
                                                            field_running_mean(180:nyears*ndays_per_year-181))
         regression_with_amplitude_variance(j,k)=REGRESS(running_var_amp(180:nyears*ndays_per_year-181),$
                                                         field_running_mean(180:nyears*ndays_per_year-181))         
      ENDFOR
   ENDFOR
  
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mjo_iav/regressions/mjo_iav_regressions_runningmean.amip2_1.5xentrain.'+file_varname+'.ps'
   PSOPEN,file=psfile,FONT=2,TFONT=2,CHARSIZE=120,SPACE3=400,SPACE2=500,SPACE1=150,YOFFSET=500,MARGIN=2000;,YSIZE=10000
   IF use_map eq 1 THEN BEGIN
      MAP,LONMIN=MIN(xdim),LONMAX=MAX(xdim),LATMIN=MIN(ydim),LATMAX=MAX(ydim)
   ENDIF ELSE IF yrev eq 1 THEN BEGIN
      GSET,XMIN=MIN(xdim),XMAX=MAX(xdim),YMIN=MAX(ydim),YMAX=MIN(ydim)
   ENDIF ELSE $
      GSET,XMIN=MIN(xdim),XMAX=MAX(ydim),YMIN=MIN(ydim),YMAX=MAX(xdim)
   IF colrev eq 1 THEN BEGIN
      CS,SCALE=1,NCOLS=N_ELEMENTS(regression_levels)+1,/REV
   ENDIF ELSE $
      CS,SCALE=1,NCOLS=N_ELEMENTS(regression_levels)+1
   LEVS,MANUAL=regression_levels
   CON,X=xdim,Y=ydim,FIELD=regression_with_amplitude,$
       CB_TITLE='Regression coefficient of '+plot_varname+' on MJO amplitude ['+units+' (unit amplitude)!U-1!N ]',CB_WIDTH=115,$
       TITLE='Regression of 361-day mean '+plot_varname+' on 361-day mean MJO amplitude (stipple |corr| > 0.2)',/NOLINES
   FOR j=0,nx-1 DO $
      FOR k=0,ny-1 DO $
         IF (ABS(correlation_with_amplitude(j,k)) ge 0.15) THEN $
            GPLOT,X=xdim(j),Y=ydim(k),SYM=3,SIZE=20
;   LEVS,MANUAL=correlation_levels
;   CON,X=xdim,Y=ydim,FIELD=correlation_with_amplitude,/NOFILL,POSITIVE_STYLE=2,NEGATIVE_STYLE=1
   IF use_map eq 1 THEN BEGIN
      AXES,XSTEP=xstep,YSTEP=ystep
   ENDIF ELSE $
      AXES,XSTEP=xstep,YSTEP=ystep,YTITLE=ytitle,XTITLE=xtitle
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mjo_iav/regressions/mjo_iav_regressions_runningvar.amip2_1.5xentrain.'+file_varname+'.ps'
   PSOPEN,file=psfile,FONT=2,TFONT=2,CHARSIZE=120,SPACE3=400,SPACE2=500,SPACE1=150,YOFFSET=500,MARGIN=2000;,YSIZE=10000
   IF use_map eq 1 THEN BEGIN
      MAP,LONMIN=MIN(xdim),LONMAX=MAX(xdim),LATMIN=MIN(ydim),LATMAX=MAX(ydim)
   ENDIF ELSE IF yrev eq 1 THEN BEGIN
      GSET,XMIN=MIN(xdim),XMAX=MAX(xdim),YMIN=MAX(ydim),YMAX=MIN(ydim)
   ENDIF ELSE $
      GSET,XMIN=MIN(xdim),XMAX=MAX(ydim),YMIN=MIN(ydim),YMAX=MAX(xdim)
   IF colrev eq 1 THEN BEGIN
      CS,SCALE=1,NCOLS=N_ELEMENTS(regression_levels)+1,/REV
   ENDIF ELSE $
      CS,SCALE=1,NCOLS=N_ELEMENTS(regression_levels)+1
   LEVS,MANUAL=regression_levels
   CON,X=xdim,Y=ydim,FIELD=regression_with_amplitude_variance,$
       CB_TITLE='Regression coefficient of '+plot_varname+' on MJO amplitude stddev ['+units+' (unit amplitude)!U-1!N ]',CB_WIDTH=115,$
       TITLE='Regression of 361-day mean '+plot_varname+' on 361-day stddev in MJO amplitude (stipple |corr| > 0.2)',/NOLINES
   FOR j=0,nx-1 DO $
      FOR k=0,ny-1 DO $
         IF (ABS(correlation_with_amplitude_variance(j,k)) ge 0.15) THEN $
            GPLOT,X=xdim(j),Y=ydim(k),SYM=3,SIZE=20
;   LEVS,MANUAL=correlation_levels
;   CON,X=xdim,Y=ydim,FIELD=correlation_with_amplitude_variance,/NOFILL,POSITIVE_STYLE=2,NEGATIVE_STYLE=1
   IF use_map eq 1 THEN BEGIN
      AXES,XSTEP=xstep,YSTEP=ystep
   ENDIF ELSE $
      AXES,XSTEP=xstep,YSTEP=ystep,YTITLE=ytitle,XTITLE=xtitle
   PSCLOSE,/NOVIEW
   
ENDFOR

STOP
END
