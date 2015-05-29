PRO hadgem3_monwg_aquaplanet_mean_state_lonavg_oned_all

n_plots=1
lat_range=[-25,25]
lonavg_range=[0,359]

;infiles=['/home/ss901165/um_output5/xhccj/time/xhccja.pf2006-2008.timemean.nc',$
;         '/home/ss901165/um_output5/xhcck/time/xhccka.pf2006-2008.timemean.nc',$
;         '/home/ss901165/um_output5/xhccn/time/xhccna.pf2006-2008.timemean.nc',$
;         '/home/ss901165/um_output5/xhcco/time/xhccoa.pf2006-2008.timemean.nc',$
;         '/home/ss901165/um_output5/xhccu/time/xhccua.pm2006-2008.timemean.nc',$
;         '/home/ss901165/um_output5/xhccv/time/xhccva.pm2006-2008.timemean.nc']
;infiles=[;'/home/ss901165/um_output5/xhccp/time/xhccpa.pf2006-2008.timemean.nc',$
;'/home/ss901165/um_output5/xhccq/time/xhccqa.pm2006-2008.timemean.nc',$
;infiles=['/home/ss901165/um_output5/xhccr/time/xhccra.pm2006-2008.timemean.nc',$
;         '/home/ss901165/um_output5/xhccs/time/xhccsa.pf2006-2008.timemean.nc',$
;         '/home/ss901165/um_output5/xhcct/time/xhccta.pm2006-2008.timemean.nc',$
;         '/home/ss901165/um_output5/xhccw/time/xhccwa.pm2006-2008.timemean.nc',$
;         '/home/ss901165/um_output5/xhccx/time/xhccxa.pm2006-2008.timemean.nc',$
;         '/home/ss901165/um_output5/xhccy/time/xhccya.pm2006-2008.timemean.nc']
infiles=['/home/ss901165/um_output5/xiura/xiura.pm2006-2008.timemean.nc',$
         '/home/ss901165/um_output5/xiurb/xiurb.pm2006-2008.timemean.nc',$
         '/home/ss901165/um_output5/xiurc/xiurc.pm2006-2008.timemean.nc',$
         '/home/ss901165/um_output5/xiurd/xiurd.pm2006-2008.timemean.nc',$
         '/home/ss901165/um_output5/xiure/xiure.pm2006-2008.timemean.nc',$
         '/home/ss901165/um_output5/xiurf/xiurf.pm2006-2008.timemean.nc']
;names=['fixeqnx_sin2sin4eq_ctlent',$
;       'fixeqnx_sin2sin4eq_1.5xent',$
;       'fixeqnx_nodcyc_sin2sin4_ctlent',$
;       'fixeqnx_nodcyc_sin2sin4_1.5xent',$
;       'fixeqnx_sin2sin4eq_2xcape',$
;       'fixeqnx_sin2sin4eq_0.5xcape']

;names=['fixnhsol_sin2sin4eq_ctlent',$
;       'fixnhsol_sin2sin4eq_1.5xent',$
;names=['fixnhsol_sin2sin45N_ctlent',$
;       'fixnhsol_sin2sin45N_1.5xent',$
;       'fixnhsol_sin2sin45N_2.5xent',$
;       'fixnhsol_sin2sin45N_2xcape',$
;       'fixnhsol_sin2sin45N_0.5xcape',$
;       'fixnhsol_sin2sin45N_2xcape_1.5xent']
names=['fixeqnx_qobs_ctlent',$
       'fixeqnx_qobs_1.5xent',$
       'fixeqnx_flat_ctlent',$
       'fixeqnx_flat_1.5xent',$
       'fixeqnx_peak_ctlent',$
       'fixeqnx_peak_1.5xent']

first_control_number=[0,0,2,2,4,4]
second_control_number=[0,1,0,1,0,1]

;colors=['blue','red','blue','red','orange','orange']
;styles=[0,0,2,2,0,2]
;colors=['blue','blue','red','red','red','orange','orange','orange'];,'red','red','red']
colors=['black','black','red','red','blue','blue']
;styles=[0,2,0,2,1,0,2,1];,0,2,1
styles=[0,2,0,2,0,2]
n_runs=N_ELEMENTS(infiles)

FOR i=0,n_plots-1 DO BEGIN
   CASE i OF      
      0 : BEGIN
         plot_name='total_precip_eqnx_varysst'
         field='precip'
         field_name='total precipitation'
         multiplier=86400.
         expt_style=2
         ymin=0
         ymax=20
         first_diff_ymin=-4
         first_diff_ymax=4
         second_diff_ymin=-12
         second_diff_ymax=12
         yaxis_label='Precipitation (mm day!U-1!N)'
      END
   ENDCASE
   
   longitude=OPEN_AND_EXTRACT(infiles(0),'longitude')
   latitude=OPEN_AND_EXTRACT(infiles(0),'latitude')
   DEFINE_BOUNDARIES,[lat_range(0),lonavg_range(0),lat_range(1),lonavg_range(1)],latitude,longitude,box_tx,/LIMIT
   n_lon=N_ELEMENTS(longitude)
   n_lat=N_ELEMENTS(latitude)

   this_field=fltarr(n_lon,n_lat)
   fields=fltarr(n_runs,n_lon)   

   FOR j=0,n_runs-1 DO BEGIN      
      this_field=REFORM(OPEN_AND_EXTRACT(infiles(j),field,$
                                         offset=[box_tx(1),box_tx(0),0,0],$
                                         count=[n_lon,n_lat,1,1]))*multiplier
      FOR k=0,n_lat-1 DO $
         fields(j,k)=MEAN(this_field(*,k))     
   ENDFOR
     
   psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/mean_state/hadgem3_monwg_aquaplanet_mean_state_lonavg_oned_all.'+plot_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2000,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=90,SPACE3=300
   GSET,XMIN=MIN(latitude),XMAX=MAX(latitude),YMIN=ymin,YMAX=ymax,$
        TITLE='Time-mean, zonal-mean '+field_name+' from aquaplanet integrations'
   FOR j=0,n_runs-1 DO $
      GPLOT,X=latitude,Y=REFORM(fields(j,*)),STYLE=styles(j),COL=FSC_COLOR(colors(j))
   
   GPLOT,X=[0,0],Y=[ymin,ymax],STYLE=1,COL=FSC_COLOR('black')
   GLEGEND,labels=REVERSE(names),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=1,STYLE=REVERSE(styles)
   AXES,XSTEP=2.5,XMINOR=1.25,YSTEP=2,YMINOR=1,XTITLE='Latitude (degrees north; minor tickmarks are gridpoints)',$
        YTITLE='Zonal-mean, time-mean '+field_name+' (mm day!U-1!N)',NDECS=1
   PSCLOSE

   psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/mean_state/hadgem3_monwg_aquaplanet_mean_state_lonavg_oned_all.'+plot_name+'-minus-'+$
      'ctlent.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2000,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=90,SPACE3=300
   GSET,XMIN=MIN(latitude),XMAX=MAX(latitude),YMIN=first_diff_ymin,YMAX=first_diff_ymax,$
        TITLE='Time-mean, zonal-mean '+field_name+', difference against control entrainment'
   FOR j=0,n_runs-1 DO BEGIN
      IF j ne first_control_number(j) THEN $
         GPLOT,X=latitude,Y=REFORM(fields(j,*)-fields(first_control_number(j),*)),STYLE=styles(j),COL=FSC_COLOR(colors(j))
   ENDFOR
   GPLOT,X=[0,0],Y=[first_diff_ymin,first_diff_ymax],STYLE=1,COL=FSC_COLOR('black')
   GPLOT,X=[-25,25],Y=[0,0],STYLE=1,COL=FSC_COLOR('black')
   GLEGEND,labels=REVERSE(names),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=3,STYLE=REVERSE(styles)
   AXES,XSTEP=2.5,XMINOR=1.25,YSTEP=2,YMINOR=1,XTITLE='Latitude (degrees north; minor tickmarks are gridpoints)',$
        YTITLE='Zonal-mean, time-mean '+field_name+' (mm day!U-1!N)',NDECS=1
   PSCLOSE

   psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/mean_state/hadgem3_monwg_aquaplanet_mean_state_lonavg_oned_all.'+plot_name+'-minus-'+$
          'qobssst.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2000,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=90,SPACE3=300
   GSET,XMIN=MIN(latitude),XMAX=MAX(latitude),YMIN=second_diff_ymin,YMAX=second_diff_ymax,$
        TITLE='Time-mean, zonal-mean '+field_name+', difference against Qobs SST at same entrainment'
   FOR j=0,n_runs-1 DO BEGIN
      IF j ne second_control_number(j) THEN $
         GPLOT,X=latitude,Y=REFORM(fields(j,*)-fields(second_control_number(j),*)),STYLE=styles(j),COL=FSC_COLOR(colors(j))
   ENDFOR
   GPLOT,X=[0,0],Y=[second_diff_ymin,second_diff_ymax],STYLE=1,COL=FSC_COLOR('black')
   GPLOT,X=[-25,25],Y=[0,0],STYLE=1,COL=FSC_COLOR('black')
   GLEGEND,labels=REVERSE(names),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=3,STYLE=REVERSE(styles)
   AXES,XSTEP=2.5,XMINOR=1.25,YSTEP=2,YMINOR=1,XTITLE='Latitude (degrees north; minor tickmarks are gridpoints)',$
        YTITLE='Zonal-mean, time-mean '+field_name+' (mm day!U-1!N)',NDECS=1
   PSCLOSE

ENDFOR

STOP
END
