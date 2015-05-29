PRO hadgem3kpp_clim_diabatic_heating

n_sets=2

box_aavg=[-10,90,10,110]

FOR i=0,n_sets-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         indir='/home/ss901165/um_output4/hadgem3a_amip2_1.0xentrain_rerun_vn74'
         prefix='hadgem3a_amip2_1.0xentrain_rerun_vn74'
      END
      1 : BEGIN
         indir='/home/ss901165/um_output4/hadgem3a_amip2_1.5xentrain_rerun_vn74'
         prefix='hadgem3a_amip2_1.5xentrain_rerun_vn74'
      END
   ENDCASE

   q1_minus_qr_file=indir+'/'+prefix+'.jan_mmean_clim.years1-14.q1-minus-qr.nc'
   bdylr_file=indir+'/'+prefix+'.jan_mmean_clim.years1-14.Tinc_bdylr.nc'
   conv_file=indir+'/'+prefix+'.jan_mmean_clim.years1-14.Tinc_conv.nc'
   lsrain_file=indir+'/'+prefix+'.jan_mmean_clim.years1-14.Tinc_lsrain.nc'
   p_file=indir+'/'+prefix+'.jan_mmean_clim.years1-14.p_thlvl.nc'

   mask_file='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/mask_n96_hadgem3-7.3.nc'
   
   longitude=OPEN_AND_EXTRACT(q1_minus_qr_file,'longitude')
   latitude=OPEN_AND_EXTRACT(q1_minus_qr_file,'latitude')
   DEFINE_BOUNDARIES,box_aavg,latitude,longitude,box_aavg_tx,/LIMIT
   n_lon=N_ELEMENTS(longitude)
   n_lat=N_ELEMENTS(latitude)
   z_thlvl=OPEN_AND_EXTRACT(q1_minus_qr_file,'hybrid_ht')
   nz=N_ELEMENTS(z_thlvl)

   q1_minus_qr=REFORM(OPEN_AND_EXTRACT(q1_minus_qr_file,'q1_minus_qr',$
                                       offset=[box_aavg_tx(1),box_aavg_tx(0),0],$
                                       count=[n_lon,n_lat,nz]))
   tinc_bdylr=REFORM(OPEN_AND_EXTRACT(bdylr_file,'unspecified_4',$
                                      offset=[box_aavg_tx(1),box_aavg_tx(0),0],$
                                      count=[n_lon,n_lat,nz]))
   tinc_conv=REFORM(OPEN_AND_EXTRACT(conv_file,'unspecified_8',$
                                     offset=[box_aavg_tx(1),box_aavg_tx(0),0],$
                                     count=[n_lon,n_lat,nz]))
   tinc_lsrain=REFORM(OPEN_AND_EXTRACT(lsrain_file,'unspecified',$
                                       offset=[box_aavg_tx(1),box_aavg_tx(0),0],$
                                       count=[n_lon,n_lat,nz]))

   mask=REFORM(OPEN_AND_EXTRACT(mask_file,'lsm',$
                                offset=[box_aavg_tx(1),box_aavg_tx(0),0,0],$
                                count=[n_lon,n_lat,1,1]))

   p_thlvl=OPEN_AND_EXTRACT(p_file,'p_1',$
                            offset=[box_aavg_tx(1),box_aavg_tx(0),0],$
                            count=[n_lon,n_lat,nz])

   tinc_bdylr_aavg=fltarr(nz)
   tinc_conv_aavg=fltarr(nz)
   tinc_lsrain_aavg=fltarr(nz)
   q1_minus_qr_aavg=fltarr(nz)
   p_thlvl_aavg=fltarr(nz)
 
   weight_total=0
   FOR j=0,n_lon-1 DO BEGIN
      FOR k=0,n_lat-1 DO BEGIN
         IF mask(j,k) ne 0 THEN BEGIN
            weight=ABS(COS(3.14159*latitude(k)/180.))
            weight_total=weight_total+weight
            FOR m=0,nz-1 DO BEGIN
               tinc_bdylr_aavg(m)=tinc_bdylr(j,k,m)*weight+tinc_bdylr_aavg(m)
               tinc_conv_aavg(m)=tinc_conv(j,k,m)*weight+tinc_conv_aavg(m)
               tinc_lsrain_aavg(m)=tinc_lsrain(j,k,m)*weight+tinc_lsrain_aavg(m)
               q1_minus_qr_aavg(m)=q1_minus_qr(j,k,m)*weight+q1_minus_qr_aavg(m)
               p_thlvl_aavg(m)=p_thlvl(j,k,m)*weight+p_thlvl_aavg(m)
            ENDFOR
         ENDIF
      ENDFOR
   ENDFOR
   q1_minus_qr_aavg=q1_minus_qr_aavg/weight_total
   tinc_conv_aavg=tinc_conv_aavg/weight_total
   tinc_bdylr_aavg=tinc_bdylr_aavg/weight_total
   tinc_lsrain_aavg=tinc_lsrain_aavg/weight_total
   p_thlvl_aavg=p_thlvl_aavg/weight_total

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/diabatic_heating/hadgem3kpp_clim_diabatic_heating.jan_mmean_marcon.'+$
          prefix+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2500,SPACE2=500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=200
   GSET,XMIN=-4.5,XMAX=3.5,YMIN=1025.,YMAX=10.
   GPLOT,X=q1_minus_qr_aavg*72.,Y=p_thlvl_aavg/100.,COL=FSC_COLOR('black')
   GPLOT,X=tinc_conv_aavg*72.,Y=p_thlvl_aavg/100.,COL=FSC_COLOR('red')
   GPLOT,X=tinc_bdylr_aavg*72.,Y=p_thlvl_aavg/100.,COL=FSC_COLOR('purple')
   GPLOT,X=tinc_lsrain_aavg*72.,Y=p_thlvl_aavg/100.,COL=FSC_COLOR('blue')
   GPLOT,X=[0,0],Y=[1025,10],STYLE=1
   
   AXES,XSTEP=0.5,XMINOR=0.25,YSTEP=-50,YMINOR=-25,YTITLE='Pressure (hPa)',XTITLE='Area-averaged temperature increment (K day!U-1!N)',$
        NDECS=1

   GLEGEND,labels=REVERSE(['Q1 minus QR','Convection','Boundary-layer + large-scale cloud','Large-scale rain']),$
           COL=REVERSE([FSC_COLOR('black'),FSC_COLOR('red'),FSC_COLOR('purple'),FSC_COLOR('blue')]),$
           LEGPOS=1
   PSCLOSE

ENDFOR

STOP
END

