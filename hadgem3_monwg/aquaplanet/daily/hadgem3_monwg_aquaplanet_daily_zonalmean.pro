PRO hadgem3_monwg_aquaplanet_daily_zonalmean

n_plots=1
n_runs=5
FOR i=0,n_plots-1 DO BEGIN
   CASE i OF 
      0 : BEGIN
         plot_name='precip'
         nc_field='precip'
         field_name='precipitation'
         z_name='latitude'
         multipliers=86400.
         yaxis_label='Latitude (degrees north)'
         levels=['0','1.5','3.0','4.5','6.0','7.5','9.0','10.5','12.0','13.5','15.0','16.5','18.0','19.5','21.0']
         rev_color=1
         scale=1
         latmin=-10
         latmax=10
         ystep=1.25
         yminor=0.625
         cb_title='Precipitation (mm day!U-1!N)'
         var_ymin=0
         var_ymax=3
         var_ystep=0.3
         var_yminor=0.1
         var_yaxis_label='Standard deviation in precipition (mm day!U-1!N)'
      END
   ENDCASE
   
   model_names=strarr(n_runs)
   colors=strarr(n_runs)
   styles=strarr(n_runs)
   FOR j=0,n_runs-1 DO BEGIN
      CASE j OF
         2 : BEGIN
            model_file='/home/ss901165/um_output5/xhccr/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_ctlent.jan-dec_dmeans.years1-3.precip.nc'
            model_names(j)='fixnhsol5N_sin2sin4_ctlent'
            colors(j)='blue'
            styles(j)=2
         END
         3 : BEGIN
            model_file='/home/ss901165/um_output5/xhccs/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_1.5xent.jan-dec_dmeans.years1-3.precip.nc'
            model_names(j)='fixnhsol5N_sin2sin4_1.5xent'                 
            colors(j)='red'
            styles(j)=2
         END         
         0 : BEGIN
            model_file='/home/ss901165/um_output5/xhccj/daily/hadgem3a_ga30_aqua_fixeqnx_sin2sin4_ctlent.jan-dec_dmeans.years1-3.precip.nc'
            model_names(j)='fixeqnx_sin2sin4_ctlent'
            colors(j)='blue'
            styles(j)=0
         END
         1 : BEGIN
            model_file='/home/ss901165/um_output5/xhcck/daily/hadgem3a_ga30_aqua_fixeqnx_sin2sin4_1.5xent.jan-dec_dmeans.years1-3.precip.nc'
            model_names(j)='fixeqnx_sin2sin4_1.5xent'                 
            colors(j)='red'
            styles(j)=0
         END         
         4 : BEGIN
            model_file='/home/ss901165/um_output5/xhccp/daily/hadgem3a_ga30_aqua_fixnhsol_sin2sin4_ctlent.jan-dec_dmeans.years1-3.precip.nc'
            model_names(j)='fixnhsol_sin2sin4_ctlent'
            colors(j)='blue'
            styles(j)=1
         END
      ENDCASE
   
      longitude=OPEN_AND_EXTRACT(model_file,'longitude')
      latitude=OPEN_AND_EXTRACT(model_file,'latitude')
      lat_start=NEAREST(latitude,latmin)
      lat_stop=NEAREST(latitude,latmax)
      latitude=latitude(lat_start:lat_stop)
      time=OPEN_AND_EXTRACT(model_file,'t')
      n_lon=N_ELEMENTS(longitude)
      n_lat=N_ELEMENTS(latitude)
      n_time=N_ELEMENTS(time)

      field=REFORM(OPEN_AND_EXTRACT(model_file,nc_field,$
                                    offset=[0,lat_start,0],count=[n_lon,n_lat,n_time]))*multipliers
      zonalmean_field=fltarr(n_time,n_lat)
      IF j eq 0 THEN BEGIN
         zonalmean_var=fltarr(n_runs,n_lat)
         zonalmax_field=fltarr(n_runs,n_lat)
      ENDIF
      FOR k=0,n_lat-1 DO BEGIN
         FOR m=0,n_time-1 DO $
            zonalmean_field(m,k)=MEAN(field(*,k,m))
         zonalmean_var(j,k)=STDDEV(zonalmean_field(*,k))
      ENDFOR
      FOR m=0,n_time-1 DO BEGIN
         temp=REFORM(zonalmean_field(m,*))
         max=where(temp eq MAX(temp))
         zonalmax_field(j,max)=zonalmax_field(j,max)+1
      ENDFOR
      zonalmax_field(j,*)=zonalmax_field(j,*)/FLOAT(n_time)

      psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/daily/hadgem3_monwg_aquaplanet_daily_zonalmean.lattime.'+plot_name+'.'+$
             model_names(j)+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=3000,SPACE2=1000,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=500,CB_WIDTH=120
      GSET,YMIN=latmin,YMAX=latmax,XMIN=MIN(time),XMAX=MAX(time)
           TITLE='Zonal-mean '+field_name+' from '+model_names(j)
      IF rev_color eq 1 THEN BEGIN
         CS,SCALE=scale,NCOLS=N_ELEMENTS(levels)+1,/REV
      ENDIF ELSE $
         CS,SCALE=scale,NCOLS=N_ELEMENTS(levels)+1
      LEVS,MANUAL=levels
      CON,X=time,Y=latitude,FIELD=REFORM(zonalmean_field(*,*)),$
          /NOLINELABELS,cb_title=cb_title,/NOLINES
      AXES,XSTEP=100,XMINOR=50,YSTEP=ystep,YMINOR=yminor,XTITLE='Time (days since initialization)',YTITLE=yaxis_label,NDECS=2
      PSCLOSE,/NOVIEW
   ENDFOR
   
   psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/daily/hadgem3_monwg_aquaplanet_daily_zonalmean.latvar.'+plot_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=3000,SPACE2=1000,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=500,CB_WIDTH=105
   GSET,XMIN=MIN(latitude),XMAX=MAX(latitude),YMIN=var_ymin,YMAX=var_ymax,$
        TITLE='Time variance in zonal mean '+field_name
   FOR j=0,n_runs-1 DO $
      GPLOT,X=latitude,Y=REFORM(zonalmean_var(j,*)),COL=FSC_COLOR(colors(j))
   AXES,XSTEP=ystep,XMINOR=yminor,YSTEP=var_ystep,YMINOR=var_yminor,XTITLE='Latitude (degrees north)',YTITLE=var_yaxis_label,NDECS=2
   GLEGEND,labels=model_names,COL=FSC_COLOR(colors),LEGPOS=1
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/daily/hadgem3_monwg_aquaplanet_daily_zonalmean.latmax.'+plot_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2000,SPACE2=500,XOFFSET=1500,YOFFSET=3000,TFONT=2,TCHARSIZE=90,SPACE3=200,CB_WIDTH=105
   GSET,XMIN=MIN(latitude),XMAX=MAX(latitude),YMIN=0,YMAX=0.6,$
        TITLE='Latitude of maximum zonal-mean '+field_name
   FOR j=0,n_runs-1 DO $
      GPLOT,X=latitude,Y=REFORM(zonalmax_field(j,*)),COL=FSC_COLOR(colors(j)),STYLE=styles(j)
   AXES,XSTEP=ystep,XMINOR=yminor,YSTEP=0.05,YMINOR=0.01,XTITLE='Latitude (degrees north)',YTITLE='Probability (unitless)',NDECS=2
   FOR j=0,n_runs-1 DO $
      model_names(j)=model_names(j)+' - SH: '+STRMID(STRTRIM(STRING(TOTAL(zonalmax_field(j,0:n_lat/2-1))),1),0,4)+'   EQ: '+$
      STRMID(STRTRIM(STRING(zonalmax_field(j,n_lat/2)),1),0,4)+'    NH: '+$
      STRMID(STRTRIM(STRING(TOTAL(zonalmax_field(j,n_lat/2+1:n_lat-1))),1),0,4)
   GLEGEND,labels=REVERSE(model_names),COL=REVERSE(FSC_COLOR(colors)),LEGXOFFSET=2000,LEGYOFFSET=-1000,STYLE=REVERSE(styles)
   PSCLOSE
ENDFOR

STOP
END
