PRO hadgem3_monwg_aquaplanet_mean_state_zonalmean_oned

n_plots=3

FOR i=0,n_plots-1 DO BEGIN
   CASE i OF 
      0 : BEGIN
         plot_name='surf_temp'
         fields=['temp']
         field_names=['surf_temp']
         multipliers=[1]
         colors=['black']
         n_fields=N_ELEMENTS(fields)
         ctl_files=['/home/ss901165/um_output5/xhccj/time/xhccja.pm2006-2008.timemean.nc',$
                    '/home/ss901165/um_output5/xhccj/time/xhccja.pm2006-2008.timemean.nc',$
                    '/home/ss901165/um_output5/xhccj/time/xhccja.pm2006-2008.timemean.nc']
         ctl_name='fixeqnx_sin2sin4_ctlent'         
         ctl_style=0
         expt_files=['/home/ss901165/um_output6/xhccp/time/xhccpa.pm2006-2008.timemean.nc',$
                     '/home/ss901165/um_output6/xhccp/time/xhccpa.pm2006-2008.timemean.nc',$
                     '/home/ss901165/um_output6/xhccp/time/xhccpa.pm2006-2008.timemean.nc']
         expt_name='fixnhsol_sin2sin4_ctlent'
         expt_style=2
         ymin=270
         ymax=303
         yaxis_label='Surface temperature (Kelvin)'
         diff_yaxis_label='Difference in surface temperature (Kelvin)'
         plot_diff=1
         diff_style=3
         diff_ymin=-5
         diff_ymax=5         
      END
      2 : BEGIN
         plot_name='precip_cvls'
         fields=['precip','cvrain','lsrain']
         field_names=['precipitation','convective rainfall','large-scale rainfall']
         multipliers=[86400.,86400.,86400.]
         colors=['black','red','blue']
         n_fields=N_ELEMENTS(fields)
         ctl_files=['/home/ss901165/um_output5/xhccj/time/xhccja.pm2006-2008.timemean.nc',$
                    '/home/ss901165/um_output5/xhccj/time/xhccja.pm2006-2008.timemean.nc',$
                    '/home/ss901165/um_output5/xhccj/time/xhccja.pm2006-2008.timemean.nc']
         ctl_name='fixeqnx_sin2sin4_ctlent'         
         ctl_style=0
         expt_files=['/home/ss901165/um_output6/xhccp/time/xhccpa.pm2006-2008.timemean.nc',$
                     '/home/ss901165/um_output6/xhccp/time/xhccpa.pm2006-2008.timemean.nc',$
                     '/home/ss901165/um_output6/xhccp/time/xhccpa.pm2006-2008.timemean.nc']
         expt_name='fixnhsol_sin2sin4_ctlent'
         expt_style=2
         ymin=0
         ymax=18
         yaxis_label='Precipitation (mm day!U-1!N)'
         diff_yaxis_label='Difference in precipitation (mm day!U-1!N)'
         plot_diff=1
         diff_style=3
         diff_ymin=-12
         diff_ymax=12        
      END
      1 : BEGIN
         plot_name='precip_convtype'
         fields=['precip_1','precip_3','precip_2']
         field_names=['deep-conv precip','mid-conv precip','shallow-conv precip']
         multipliers=[86400.,86400.,86400.]
         colors=['black','red','blue']
         n_fields=N_ELEMENTS(fields)
         ctl_files=['/home/ss901165/um_output5/xhccj/time/xhccja.pm2006-2008.timemean.nc',$
                    '/home/ss901165/um_output5/xhccj/time/xhccja.pm2006-2008.timemean.nc',$
                    '/home/ss901165/um_output5/xhccj/time/xhccja.pm2006-2008.timemean.nc']
         ctl_name='fixeqnx_sin2sin4_ctlent'         
         ctl_style=0
         expt_files=['/home/ss901165/um_output6/xhccp/time/xhccpa.pm2006-2008.timemean.nc',$
                     '/home/ss901165/um_output6/xhccp/time/xhccpa.pm2006-2008.timemean.nc',$
                     '/home/ss901165/um_output6/xhccp/time/xhccpa.pm2006-2008.timemean.nc']
         expt_name='fixnhsol_sin2sin4_ctlent'
         expt_style=2
         ymin=0
         ymax=18
         yaxis_label='Precipitation (mm day!U-1!N)'
         diff_yaxis_label='Difference in precipitation (mm day!U-1!N)'
         plot_diff=1
         diff_style=3
         diff_ymin=-12
         diff_ymax=12     
      END
      3 : BEGIN
         plot_name='precip_convcall'
         fields=['unspecified','field1934','field1598']
         field_names=['deep convection','mid-level convection','shallow convection']
         multipliers=[1,1,1]
         colors=['black','red','blue']
         n_fields=N_ELEMENTS(fields)
         ctl_files=['/home/ss901165/um_output5/xhccj/time/xhccja.pm2006-2008.timemean.nc',$
                    '/home/ss901165/um_output5/xhccj/time/xhccja.pm2006-2008.timemean.nc',$
                    '/home/ss901165/um_output5/xhccj/time/xhccja.pm2006-2008.timemean.nc']
         ctl_name='fixeqnx_sin2sin4_ctlent'         
         ctl_style=0
         expt_files=['/home/ss901165/um_output6/xhccp/time/xhccpa.pm2006-2008.timemean.nc',$
                     '/home/ss901165/um_output6/xhccp/time/xhccpa.pm2006-2008.timemean.nc',$
                     '/home/ss901165/um_output6/xhccp/time/xhccpa.pm2006-2008.timemean.nc']
         expt_name='fixnhsol_sin2sin4_ctlent'
         expt_style=2
         ymin=0
         ymax=1
         yaxis_label='Fraction of timesteps (unitless)'
         diff_yaxis_label='Difference in fraction of timesteps (unitless)'
         plot_diff=1
         diff_style=3
         diff_ymin=-0.5
         diff_ymax=0.5    
      END
   ENDCASE
   
   longitude=OPEN_AND_EXTRACT(ctl_files(0),'longitude')
   latitude=OPEN_AND_EXTRACT(ctl_files(0),'latitude')
   n_lon=N_ELEMENTS(longitude)
   n_lat=N_ELEMENTS(latitude)

   field=fltarr(n_lon,n_lat)
   ctl_fields=fltarr(n_fields,n_lat)
   expt_fields=fltarr(n_fields,n_lat)

   FOR j=0,n_fields-1 DO BEGIN
      
      field=REFORM(OPEN_AND_EXTRACT(ctl_files(j),fields(j)))*multipliers(j)
      FOR k=0,n_lat-1 DO $
         ctl_fields(j,k)=MEAN(field(*,k))
      field=REFORM(OPEN_AND_EXTRACT(expt_files(j),fields(j)))*multipliers(j)
      FOR k=0,n_lat-1 DO $
         expt_fields(j,k)=MEAN(field(*,k))
      
   ENDFOR
     
   psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/mean_state/hadgem3_monwg_aquaplanet_mean_state_zonalmean_oned.'+plot_name+'.'+ctl_name+'-and-'+expt_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=3000,SPACE2=1000,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=500
   GSET,XMIN=MIN(latitude),XMAX=MAX(latitude),YMIN=ymin,YMAX=ymax,$
        TITLE='Time-mean, zonal-mean from aquaplanet integrations - '+ctl_name+' and '+expt_name
   FOR j=0,n_fields-1 DO BEGIN
      GPLOT,X=latitude,Y=REFORM(ctl_fields(j,*)),STYLE=ctl_style,COL=FSC_COLOR(colors(j))
      GPLOT,X=latitude,Y=REFORM(expt_fields(j,*)),STYLE=expt_style,COL=FSC_COLOR(colors(j))
   ENDFOR
   model_legend_items=[ctl_name,expt_name]
   model_legend_styles=[ctl_style,expt_style]
   IF plot_diff eq 0 THEN BEGIN
      AXES,YSTEP=FLOAT(ymax-ymin)/10.,YMINOR=FLOAT(ymax-ymin)/20.,XSTEP=10,XMINOR=5,$
           YTITLE=yaxis_label,XTITLE='Latitude (positive north)',NDECS=2      
   ENDIF ELSE IF plot_diff eq 1 THEN BEGIN
      AXES,YSTEP=FLOAT(ymax-ymin)/10.,YMINOR=FLOAT(ymax-ymin)/20.,XSTEP=10,XMINOR=5,$
           YTITLE=yaxis_label,XTITLE='Latitude (positive north)',/NORIGHT,NDECS=2
   
      GSET,XMIN=MIN(latitude),XMAX=MAX(latitude),YMIN=diff_ymin,ymax=diff_ymax
      FOR j=0,n_fields-1 DO $
         GPLOT,X=latitude,Y=REFORM(expt_fields(j,*))-REFORM(ctl_fields(j,*)),$
               STYLE=diff_style,COL=FSC_COLOR(colors(j))
      GPLOT,X=[MIN(latitude),MAX(latitude)],Y=[0,0],COL=FSC_COLOR('black'),STYLE=1
      AXES,XSTEP=10,XMINOR=5,YSTEP=FLOAT(diff_ymax-diff_ymin)/10.,$
           YMINOR=FLOAT(diff_ymax-diff_ymin)/20.,YTITLE=diff_yaxis_label,NDECS=2,$
           /ONLYRIGHT
      model_legend_items=[model_legend_items,'Difference']
      model_legend_styles=[model_legend_styles,diff_style]
   ENDIF   

   GLEGEND,labels=REVERSE(field_names),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=4
   GLEGEND,labels=REVERSE(model_legend_items),STYLE=REVERSE(model_legend_styles),LEGPOS=12

   PSCLOSE

ENDFOR

STOP
END
