PRO hadgem3_monwg_aquaplanet_daily_twodvar

n_plots=1
n_runs=2
FOR i=0,n_plots-1 DO BEGIN
   CASE i OF 
      0 : BEGIN
         plot_name='u_plvl'
         nc_field='u'
         field_name='zonal wind'
         lon_name='longitude_1'
         lat_name='latitude_1'
         z_name='p'
         multipliers=1.
         yaxis_label='Pressure (hPa)'
         levels=['0','0.3','0.6','0.9','1.2','1.5','1.8','2.1','2.4','2.7','3.0','3.3','3.6','3.9','4.2','4.5','4.8','5.1']
         ratio_levels=['0.70','0.74','0.78','0.82','0.86','0.90','0.94','0.98','1.02','1.06','1.11','1.16','1.22','1.28','1.35','1.43']
         rev_color=0
         scale=1
         latmin=-50
         latmax=50
         ystep=-80
         yminor=-40
         cb_title='Standard deviation in daily zonal wind velocity (m s!U-1!N)'
      END
   ENDCASE
   
   model_names=strarr(n_runs)
   colors=strarr(n_runs)
   FOR j=0,n_runs-1 DO BEGIN
      CASE j OF
         0 : BEGIN
            model_file='/home/ss901165/um_output5/xhccj/daily/hadgem3a_ga30_aqua_fixeqnx_sin2sin4_ctlent.jan-dec_dmeans.years1-3.u_plvl.nc'
            model_names(j)='fixeqnx_sin2sin4_ctlent'
            colors(j)='blue'
         END
         1 : BEGIN
            model_file='/home/ss901165/um_output5/xhcck/daily/hadgem3a_ga30_aqua_fixeqnx_sin2sin4_1.5xent.jan-dec_dmeans.years1-3.u_plvl.nc'
            model_names(j)='fixeqnx_sin2sin4_1.5xent'
            colors(j)='red'
         END         
      ENDCASE

      longitude=OPEN_AND_EXTRACT(model_file,lon_name)
      latitude=OPEN_AND_EXTRACT(model_file,lat_name)+1.25
      lat_start=NEAREST(latitude,latmin)
      lat_stop=NEAREST(latitude,latmax)
      latitude=latitude(lat_start:lat_stop)
      time=OPEN_AND_EXTRACT(model_file,'t')
      z=OPEN_AND_EXTRACT(model_file,z_name)
      n_lon=N_ELEMENTS(longitude)
      n_lat=N_ELEMENTS(latitude)
      n_time=N_ELEMENTS(time)
      n_z=N_ELEMENTS(z)
      
      field=REFORM(OPEN_AND_EXTRACT(model_file,nc_field,$
                                    offset=[0,lat_start,0,0],count=[n_lon,n_lat,n_z,n_time]))*multipliers
      zonalmean_field=fltarr(n_time,n_lat,n_z)
      print,j
      IF j eq 0 THEN BEGIN
         zonalmean_var=fltarr(n_runs,n_lat,n_z)
;         zonalmax_field=fltarr(n_runs,n_z,n_lat)
      ENDIF
      FOR k=0,n_lat-1 DO BEGIN
         FOR n=0,n_z-1 DO BEGIN
            FOR m=0,n_time-1 DO $
               zonalmean_field(m,k,n)=MEAN(ABS(field(*,k,n,m)))
            zonalmean_var(j,k,n)=STDDEV(zonalmean_field(*,k,n))
         ENDFOR
      ENDFOR      

;   FOR m=0,n_time-1 DO BEGIN
;      temp=REFORM(zonalmean_field(m,*))
;      max=where(temp eq MAX(temp))
;      zonalmax_field(j,max)=zonalmax_field(j,max)+1
;   ENDFOR
;   zonalmax_field(j,*)=zonalmax_field(j,*)/FLOAT(n_time)

      psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/daily/hadgem3_monwg_aquaplanet_daily_twodvar.'+plot_name+'.'+$
             model_names(j)+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=3000,SPACE2=1000,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=500,CB_WIDTH=110
      GSET,YMIN=MAX(z),YMAX=MIN(z),XMIN=latmin,XMAX=latmax,$
           TITLE='Standard deviation in zonal-mean '+field_name+' from '+model_names(j)
      IF rev_color eq 1 THEN BEGIN
         CS,SCALE=scale,NCOLS=N_ELEMENTS(levels)+1,/REV
      ENDIF ELSE $
         CS,SCALE=scale,NCOLS=N_ELEMENTS(levels)+1
      LEVS,MANUAL=levels
      CON,X=latitude,Y=z,FIELD=REFORM(zonalmean_var(j,*,*)),$
          /NOLINELABELS,cb_title=cb_title,/NOLINES
      AXES,XSTEP=5,XMINOR=2.5,YSTEP=ystep,YMINOR=yminor,XTITLE='Latitude (degrees north)',YTITLE=yaxis_label,NDECS=2
      PSCLOSE,/NOVIEW 
   ENDFOR

   psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/daily/hadgem3_monwg_aquaplanet_daily_twodvar.'+plot_name+'.'+$
          model_names(1)+'-ratio-'+model_names(0)+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=3000,SPACE2=1000,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=500,CB_WIDTH=110
   GSET,YMIN=MAX(z),YMAX=MIN(z),XMIN=latmin,XMAX=latmax,$
        TITLE='Ratio of stddev in zonal-mean '+field_name+' - '+model_names(1)+' div '+model_names(0)
   IF rev_color eq 1 THEN BEGIN
      CS,SCALE=scale,NCOLS=N_ELEMENTS(ratio_levels)+1,/REV
   ENDIF ELSE $
      CS,SCALE=scale,NCOLS=N_ELEMENTS(ratio_levels)+1
   LEVS,MANUAL=ratio_levels
   CON,X=latitude,Y=z,FIELD=REFORM(zonalmean_var(1,*,*))/REFORM(zonalmean_var(0,*,*)),$
       /NOLINELABELS,cb_title='Ratio (unitless)',/NOLINES
   AXES,XSTEP=5,XMINOR=2.5,YSTEP=ystep,YMINOR=yminor,XTITLE='Latitude (degrees north)',YTITLE=yaxis_label,NDECS=2
   PSCLOSE   
ENDFOR

STOP
END

