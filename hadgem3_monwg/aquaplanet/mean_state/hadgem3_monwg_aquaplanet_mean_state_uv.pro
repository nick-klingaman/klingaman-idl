PRO hadgem3_monwg_aquaplanet_mean_state_uv

; Plot mean precip and differences from 1.0x and 1.5x entrainment
; aquaplanet simulations

n_diffs=1
u_varname='u'
v_varname='v'
vardesc='10 m winds'
varunits='m s!U-1!N'

levels_raw=['1','2','3','4','5','6','8','10','12','14','16','18','20','22']
levels_diff=['-4.5','-3.9','-3.3','-2.7','-2.1','-1.5','-0.9','-0.3','0.3','0.9','1.5','2.1','2.7','3.3','3.9','4.5']

box=[-60,0,60,360]
box_name='60N60S'

FOR i=0,n_diffs-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         ctl_infile='/home/ss901165/um_output5/xhccj/xhccja_pf.2006-2008_timemean.nc'
         ctl_name='fixeqnx_sin2sin4_ctlent'
         ctl_desc='fixed eqnx, sin2sin4 SSTs, 1.0x entrainment'
         exp_infile='/home/ss901165/um_output5/xhcck/xhccka_pf.2006-2008_timemean.nc'
         exp_name='fixeqnx_sin2sin4_1.5xent'
         exp_desc='fixed eqnx, sin2sin4 SSTs, 1.5x entrainment'         
      END
   ENDCASE

   ctl_longitude=OPEN_AND_EXTRACT(ctl_infile,'longitude_1')
   ctl_latitude=OPEN_AND_EXTRACT(ctl_infile,'latitude_2')
   DEFINE_BOUNDARIES,box,ctl_latitude,ctl_longitude,ctl_box_tx,/LIMIT
   ctl_nlon=N_ELEMENTS(ctl_longitude)
   ctl_nlat=N_ELEMENTS(ctl_latitude)

   exp_longitude=OPEN_AND_EXTRACT(exp_infile,'longitude_1')
   exp_latitude=OPEN_AND_EXTRACT(exp_infile,'latitude_2')
   DEFINE_BOUNDARIES,box,exp_latitude,exp_longitude,exp_box_tx,/LIMIT
   exp_nlon=N_ELEMENTS(exp_longitude)
   exp_nlat=N_ELEMENTS(exp_latitude)

   ctl_u_var=REFORM(OPEN_AND_EXTRACT(ctl_infile,u_varname,$
                                     offset=[ctl_box_tx(1),ctl_box_tx(0),0,0],$
                                     count=[ctl_nlon,ctl_nlat,1,1]))
   exp_u_var=REFORM(OPEN_AND_EXTRACT(exp_infile,u_varname,$
                                     offset=[exp_box_tx(1),exp_box_tx(0),0,0],$
                                     count=[exp_nlon,exp_nlat,1,1]))

   ctl_v_var=REFORM(OPEN_AND_EXTRACT(ctl_infile,v_varname,$
                                     offset=[ctl_box_tx(1),ctl_box_tx(0),0,0],$
                                     count=[ctl_nlon,ctl_nlat,1,1]))
   exp_v_var=REFORM(OPEN_AND_EXTRACT(exp_infile,v_varname,$
                                     offset=[exp_box_tx(1),exp_box_tx(0),0,0],$
                                     count=[exp_nlon,exp_nlat,1,1]))

   psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/mean_state/hadgem3_monwg_aquaplanet_mean_state_uv.uv10m_'+ctl_name+'_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2000,SPACE2=1000,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=90,SPACE3=500
;   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2),/SET
   GSET,XMIN=box(1),XMAX=box(3),YMIN=box(0),YMAX=box(2)
;   CS,SCALE=1,NCOLS=N_ELEMENTS(levels_raw)+1,/REV
;   LEVS,MANUAL=levels_raw
   VECT,X=ctl_longitude,Y=ctl_latitude,U=ctl_u_var,V=ctl_v_var,$
        TITLE='Clim '+vardesc+' ('+varunits+') from '+ctl_desc,MAG=5,STRIDE=3
   AXES,XSTEP=20,YSTEP=10,YTITLE='Latitude (degrees north)',XTITLE='Longitude (degrees east)'
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/mean_state/hadgem3_monwg_aquaplanet_mean_state_uv.uv10m_'+exp_name+'_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2000,SPACE2=1000,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=90,SPACE3=500
;   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2),/SET
   GSET,XMIN=box(1),XMAX=box(3),YMIN=box(0),YMAX=box(2)
;   CS,SCALE=1,NCOLS=N_ELEMENTS(levels_raw)+1,/REV
;   LEVS,MANUAL=levels_raw
   VECT,X=exp_longitude,Y=exp_latitude,U=exp_u_var,V=exp_v_var,$
       TITLE='Clim '+vardesc+' ('+varunits+') from '+exp_desc,MAG=5,STRIDE=3
   AXES,XSTEP=20,YSTEP=10,YTITLE='Latitude (degrees north)',XTITLE='Longitude (degrees east)'
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/mean_state/hadgem3_monwg_aquaplanet_mean_state_uv.uv10m_'+exp_name+'-minus-'+$
          ctl_name+'_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2000,SPACE2=1000,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=90,SPACE3=500
;   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2),/SET
   GSET,XMIN=box(1),XMAX=box(3),YMIN=box(0),YMAX=box(2)
;   CS,SCALE=1,NCOLS=N_ELEMENTS(levels_diff)+1,/REV
;   LEVS,MANUAL=levels_diff
   VECT,X=ctl_longitude,Y=ctl_latitude,U=exp_u_var-ctl_u_var,V=exp_v_var-ctl_v_var,$
       TITLE='Diff in clim '+vardesc+' for '+exp_desc+' minus '+ctl_desc,MAG=1,STRIDE=3
   AXES,XSTEP=20,YSTEP=10,YTITLE='Latitude (degrees north)',XTITLE='Longitude (degrees east)'
   PSCLOSE

ENDFOR

STOP
END
