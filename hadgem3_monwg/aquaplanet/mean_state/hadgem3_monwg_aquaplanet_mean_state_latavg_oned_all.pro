PRO hadgem3_monwg_aquaplanet_mean_state_latavg_oned_all

n_plots=2
latavg_range=[-15,15]
lon_range=[0,359]

infiles=['/home/ss901165/um_output5/xhccj/time/xhccja.pf2006-2008.timemean.nc',$
         '/home/ss901165/um_output5/xhcck/time/xhccka.pf2006-2008.timemean.nc',$
         '/home/ss901165/um_output5/xhccn/time/xhccna.pf2006-2008.timemean.nc',$
         '/home/ss901165/um_output5/xhcco/time/xhccoa.pf2006-2008.timemean.nc',$
         '/home/ss901165/um_output5/xhccp/time/xhccpa.pf2006-2008.timemean.nc',$
         '/home/ss901165/um_output5/xhccq/time/xhccqa.pm2006-2008.timemean.nc',$
         '/home/ss901165/um_output5/xhccr/time/xhccra.pm2006-2008.timemean.nc',$
         '/home/ss901165/um_output5/xhccs/time/xhccsa.pm2006-2008.timemean.nc',$
         '/home/ss901165/um_output5/xhcct/time/xhccta.pm2006-2008.timemean.nc']
names=['fixeqnx_sin2sin4eq_ctlent',$
       'fixeqnx_sin2sin4eq_1.5xent',$
       'fixeqnx_nodcyc_sin2sin4_ctlent',$
       'fixeqnx_nodcyc_sin2sin4_1.5xent',$
       'fixnhsol_sin2sin4eq_ctlent',$
       'fixnhsol_sin2sin4eq_1.5xent',$
       'fixnhsol_sin2sin45N_ctlent',$
       'fixnhsol_sin2sin45N_1.5xent']
colors=['blue','blue','cyan','cyan','orange','orange','red','red']
styles=[0,2,0,2,0,2,0,2]

FOR i=0,n_plots-1 DO BEGIN
   CASE i OF      
      0 : BEGIN
         plot_name='total_precip'
         field='precip'
         field_name='total precipitation'
         multiplier=86400.
         expt_style=2
         ymin=0
         ymax=16
         yaxis_label='Precipitation (mm day!U-1!N)'
         diff_yaxis_label='Difference in precipitation (mm day!U-1!N)'
         plot_diff=1
         diff_style=3
         diff_ymin=-1
         diff_ymax=1         
      END
   ENDCASE
   
   longitude=OPEN_AND_EXTRACT(ctl_files(0),'longitude')
   latitude=OPEN_AND_EXTRACT(ctl_files(0),'latitude')
   DEFINE_BOUNDARIES,[latavg_range(0),lon_range(0),latavg_range(1),lon_range(1)],latitude,longitude,box_tx,/LIMIT
   n_lon=N_ELEMENTS(longitude)
   n_lat=N_ELEMENTS(latitude)

   field=fltarr(n_lon,n_lat)
   ctl_fields=fltarr(n_fields,n_lon)
   expt_fields=fltarr(n_fields,n_lon)

   FOR j=0,n_fields-1 DO BEGIN      
      field=REFORM(OPEN_AND_EXTRACT(ctl_files(j),fields(j),$
                                    offset=[box_tx(1),box_tx(0),0,0],$
                                    count=[n_lon,n_lat,1,1]))*multipliers(j)
      FOR k=0,n_lon-1 DO $
         ctl_fields(j,k)=MEAN(field(k,*))

      field=REFORM(OPEN_AND_EXTRACT(expt_files(j),fields(j),$
                                    offset=[box_tx(1),box_tx(0),0,0],$
                                    count=[n_lon,n_lat,1,1]))*multipliers(j)
      FOR k=0,n_lon-1 DO $
         expt_fields(j,k)=MEAN(field(k,*))
   ENDFOR
     
   psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/mean_state/hadgem3_monwg_aquaplanet_mean_state_latavg_oned.'+plot_name+'.'+ctl_name+'-and-'+expt_name+'.'+$
          STRTRIM(STRING(latavg_range(0)),1)+'_to_'+STRTRIM(STRING(latavg_range(1)),1)+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=3000,SPACE2=300,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=300
   GSET,XMIN=MIN(longitude),XMAX=MAX(longitude),YMIN=ymin,YMAX=ymax,$
        TITLE='Time-mean, lat-avg ('+STRTRIM(STRING(latavg_range(0)),1)+' to '+STRTRIM(STRING(latavg_range(1)),1)+$
        ') from aquaplanet integrations - '+expt_name+' and '+ctl_name
   FOR j=0,n_fields-1 DO BEGIN
      GPLOT,X=longitude,Y=REFORM(ctl_fields(j,*)),STYLE=ctl_style,COL=FSC_COLOR(colors(j))
      GPLOT,X=longitude,Y=REFORM(expt_fields(j,*)),STYLE=expt_style,COL=FSC_COLOR(colors(j))
   ENDFOR
   model_legend_items=[ctl_name,expt_name]
   model_legend_styles=[ctl_style,expt_style]
   IF plot_diff eq 0 THEN BEGIN
      AXES,YSTEP=FLOAT(ymax-ymin)/10.,YMINOR=FLOAT(ymax-ymin)/20.,XSTEP=30,XMINOR=10,$
           YTITLE=yaxis_label,XTITLE='Longitude (degrees east)',NDECS=2      
   ENDIF ELSE IF plot_diff eq 1 THEN BEGIN
      AXES,YSTEP=FLOAT(ymax-ymin)/10.,YMINOR=FLOAT(ymax-ymin)/20.,XSTEP=30,XMINOR=10,$
           YTITLE=yaxis_label,XTITLE='Longitude (degrees east)',/NORIGHT,NDECS=2

      GSET,XMIN=MIN(longitude),XMAX=MAX(longitude),YMIN=diff_ymin,ymax=diff_ymax
      FOR j=0,n_fields-1 DO $
         GPLOT,X=longitude,Y=REFORM(expt_fields(j,*))-REFORM(ctl_fields(j,*)),$
               STYLE=diff_style,COL=FSC_COLOR(colors(j))
      GPLOT,X=[MIN(longitude),MAX(longitude)],Y=[0,0],COL=FSC_COLOR('black'),STYLE=1
      AXES,XSTEP=30,XMINOR=10,YSTEP=FLOAT(diff_ymax-diff_ymin)/10.,$
           YMINOR=FLOAT(diff_ymax-diff_ymin)/20.,YTITLE=diff_yaxis_label,NDECS=2,$
           /ONLYRIGHT
      model_legend_items=[model_legend_items,'Diff: dashed minus solid']
      model_legend_styles=[model_legend_styles,diff_style]
   ENDIF   

   GLEGEND,labels=REVERSE(field_names),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=4
   GLEGEND,labels=REVERSE(model_legend_items),STYLE=REVERSE(model_legend_styles),LEGPOS=12

   PSCLOSE

ENDFOR

STOP
END
