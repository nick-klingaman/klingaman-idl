PRO hadgem3_monwg_aquaplanet_mean_state_precip

; Plot mean precip and differences from 1.0x and 1.5x entrainment
; aquaplanet simulations

n_diffs=1
n_vars=1
box=[-30,0,30,360]
box_name='30N30S'
FOR j=0,n_vars-1 DO BEGIN   
   CASE j OF 
      0 : BEGIN
         varname='precip'
         vardesc='Total precipitation'
         varunits='mm day!U-1!N'         
;         levels_raw=['1','2','3','4','5','6','8','10','12','14','16','18','20','22']
         levels_raw=['1','2','3','4','5','6','8','10','12','15','18','21','24','28']
         levels_diff=['-4.5','-3.9','-3.3','-2.7','-2.1','-1.5','-0.9','-0.3','0.3','0.9','1.5','2.1','2.7','3.3','3.9','4.5']
;         levels_diff=['-6.6','-5.8','-5.0','-4.2','-3.6','-2.8','-2.0','-1.2','-0.4','0.4','1.2','2.0','2.8','3.6','4.2','5.0','5.8','6.6']
         multiplier=86400.
      END
      1 : BEGIN
         varname='lsrain'
         vardesc='large-scale rainfall'
         varunits='mm day!U-1!N'
         levels_raw=['0','0.3','0.6','0.9','1.2','1.5','1.8','2.1','2.4','2.7','3.0','3.3','3.6','3.9']
         levels_diff=['-0.85','-0.75','-0.65','-0.55','-0.45','-0.35','-0.25','-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85']
         multiplier=86400.
      END
      3 : BEGIN
         varname='precip_1'
         vardesc='deep-conv precip'
         varunits='mm day!U-1!N'
         levels_raw=['1','2','3','4','5','6','7','8','9','10','11','12','13','14','15']
         levels_diff=['-4.5','-3.9','-3.3','-2.7','-2.1','-1.5','-0.9','-0.3','0.3','0.9','1.5','2.1','2.7','3.3','3.9','4.5']
      END
      4 : BEGIN
         varname='precip_2'
         vardesc='shallow-conv precip'
         varunits='mm day!U-1!N'
         levels_raw=['0','0.05','0.10','0.15','0.20','0.25','0.30','0.35','0.40','0.45','0.50','0.55','0.60','0.65','0.70']
         levels_diff=['-0.45','-0.39','-0.33','-0.27','-0.21','-0.15','-0.09','-0.03','0.03','0.09','0.15','0.21','0.27','0.33','0.39','0.45']
      END
      5 : BEGIN
         varname='precip_3'
         vardesc='mid-conv precip'
         varunits='mm day!U-1!N'
         levels_raw=['0','0.1','0.2','0.3','0.4','0.5','0.6','0.7','0.8','0.9','1.0','1.1','1.2','1.3','1.4']
         levels_diff=['-0.65','-0.55','-0.45','-0.35','-0.25','-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55','0.65']
      END
      6 : BEGIN
         varname='unspecified_2'
         vardesc='fracts_deepconv'
         varunits='unitless'
         levels_raw=['0','0.03','0.06','0.09','0.12','0.15','0.18','0.21','0.24','0.27','0.30','0.33','0.36','0.39','0.42']
         levels_diff=['-0.17','-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15','0.17']
         multiplier=1.
      END
      7 : BEGIN
         varname='field1598'
         vardesc='fracts_shallowconv'
         varunits='unitless'
         levels_raw=['0','0.07','0.14','0.21','0.28','0.35','0.42','0.49','0.56','0.63','0.70','0.77','0.84','0.91','0.98']
         levels_diff=['-0.17','-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15','0.17']
         multiplier=1.
      END
      8 : BEGIN
         varname='field1934'
         vardesc='fracts_midconv'
         varunits='unitless'
         levels_raw=['0','0.02','0.04','0.06','0.08','0.10','0.12','0.14','0.16','0.18','0.20','0.22','0.24','0.28']
         levels_diff=['-0.065','-0.055','-0.045','-0.035','-0.025','-0.015','-0.005','0.005','0.015','0.025','0.035','0.045','0.055','0.065']
         multiplier=1.
      END
      2 : BEGIN
         varname='cvrain'
         vardesc='convective rainfall'
         varunits='mm day!U-1!N'
;         levels_raw=['1','2','3','4','5','6','8','10','12','14','16','18','20','22']
         levels_raw=['1','2','3','4','5','6','8','10','12','15','18','21','24','28']
         levels_diff=['-4.5','-3.9','-3.3','-2.7','-2.1','-1.5','-0.9','-0.3','0.3','0.9','1.5','2.1','2.7','3.3','3.9','4.5']
         multiplier=86400.
      END
   ENDCASE
   
   FOR i=0,n_diffs-1 DO BEGIN
      CASE i OF
         0 : BEGIN
            ctl_infile='/home/ss901165/um_output5/xhccj/time/xhccja.pf2006-2008.timemean.nc'
            ctl_name='fixeqnx_sin2sin4_ctlent'
            ctl_desc='fixed eqnx, sin2sin4 SSTs, ctl ent'
            exp_infile='/home/ss901165/um_output5/xhcck/time/xhccka.pf2006-2008.timemean.nc'
            exp_name='fixeqnx_sin2sin4_1.5xent'
            exp_desc='fixed eqnx, sin2sin4 SSTs, 1.5x ent'
         END
         4 : BEGIN
            ctl_infile='/home/ss901165/um_output5/xhccl/monthly/timemean.nc'
            ctl_name='fixeqnx_sin2sin4_warmpool_ctlent'
            ctl_desc='fixed eqnx, sin2sin4 + warm pool, ctl ent'
            exp_infile='/home/ss901165/um_output5/xhccm/monthly/timemean.nc'
            exp_name='fixeqnx_sin2sin4_warmpool_1.5xent'
            exp_desc='fixed eqnx, sin2sin4 + warm pool, 1.5x ent'
         END
         6 : BEGIN
            ctl_infile='/home/ss901165/um_output5/xhccj/time/xhccja.pf2006-2008.timemean.nc'
            ctl_name='fixeqnx_sin2sin4_ctlent'
            ctl_desc='fixed eqnx, sin2sin4 SSTs, ctl ent'
            exp_infile='/home/ss901165/um_output5/xhccl/monthly/timemean.nc'
            exp_name='fixeqnx_sin2sin4_warmpool_ctlent'
            exp_desc='fixed eqnx, sin2sin4 + warm pool, ctl ent'
         END
         3 : BEGIN
            ctl_infile='/home/ss901165/um_output5/xhcck/time/xhccka.pf2006-2008.timemean.nc'
            ctl_name='fixeqnx_sin2sin4_1.5xent'
            ctl_desc='fixed eqnx, sin2sin4 SSTs, 1.5x ent'
            exp_infile='/home/ss901165/um_output5/xhccm/monthly/timemean.nc'
            exp_name='fixeqnx_sin2sin4_warmpool_1.5xent'
            exp_desc='fixed eqnx, sin2sin4 + warm pool, 1.5x ent'
         END
         8 : BEGIN
            ctl_infile='/home/ss901165/um_output5/xhccj/time/xhccja.pf2006-2008.timemean.nc'
            ctl_name='fixeqnx_sin2sin4_ctlent'
            ctl_desc='fixed eqnx, sin2sin4 SSTs, ctl ent'
            exp_infile='/home/ss901165/um_output5/xhccr/time/xhccra.pm2006-2008.timemean.nc'
            exp_name='fixnhsol_sin2sin45N_ctlent'
            exp_desc='fixed NHsol, sin2sin4_5N SSTs, ctl ent'
         END
         2 : BEGIN
            ctl_infile='/home/ss901165/um_output5/xhcck/time/xhccka.pf2006-2008.timemean.nc'
            ctl_name='fixeqnx_sin2sin4_1.5xent'
            ctl_desc='fixed eqnx, sin2sin4 SSTs, 1.5x ent'
            exp_infile='/home/ss901165/um_output5/xhccs/time/xhccsa.pm2006-2008.timemean.nc'
            exp_name='fixnhsol_sin2sin45N_1.5xent'
            exp_desc='fixed NHsol, sin2sin4_5N SSTs, 1.5x ent'
         END
         5 : BEGIN
            ctl_infile='/home/ss901165/um_output5/xhccj/time/xhccja.pf2006-2008.timemean.nc'
            ctl_name='fixeqnx_sin2sin4_ctlent'
            ctl_desc='fixed eqnx, sin2sin4 SSTs, ctl ent'
            exp_infile='/home/ss901165/um_output5/xhccp/time/xhccpa.pm2006-2008.timemean.nc'
            exp_name='fixnhsol_sin2sin4_ctlent'
            exp_desc='fixed NHsol, sin2sin4 SSTs, ctl ent'
         END
         1 : BEGIN
            ctl_infile='/home/ss901165/um_output5/xhccr/time/xhccra.pm2006-2008.timemean.nc'
            ctl_name='fixnhsol_sin2sin45N_ctlent'
            ctl_desc='fixed NHsol, sin2sin4_5N SSTs, ctl ent'
            exp_infile='/home/ss901165/um_output5/xhccs/time/xhccsa.pm2006-2008.timemean.nc'
            exp_name='fixnhsol_sin2sin45N_1.5xent'
            exp_desc='fixed NHsol, sin2sin4_5N SSTs, 1.5x ent'
         END
         7 : BEGIN
            ctl_infile='/home/ss901165/um_output5/xhccj/time/xhccja.pf2006-2008.timemean.nc'
            ctl_name='fixeqnx_sin2sin4_ctlent'
            ctl_desc='fixed eqnx, sin2sin4 SSTs, ctl ent'
            exp_infile='/home/ss901165/um_output6/xhccn/time/xhccna.pf2006-2008.timemean.nc'
            exp_name='fixeqnx_sin2sin4_ctlent_nodcyc'
            exp_desc='fixed eqnx, sin2sin4 SSTs, ctl ent, no dcyc'
         END
         9 : BEGIN
            ctl_infile='/home/ss901165/um_output5/xhcck/time/xhccka.pf2006-2008.timemean.nc'
            ctl_name='fixeqnx_sin2sin4_1.5xent'
            ctl_desc='fixed eqnx, sin2sin4 SSTs, 1.5x ent'
            exp_infile='/home/ss901165/um_output6/xhcco/time/xhccoa.pf2006-2008.timemean.nc'
            exp_name='fixeqnx_sin2sin4_1.5xent_nodcyc'
            exp_desc='fixed eqnx, sin2sin4 SSTs, 1.5x ent, no dcyc'
         END
         9 : BEGIN
            ctl_infile='/home/ss901165/um_output6/xhccn/time/xhccna.pf2006-2008.timemean.nc'
            ctl_name='fixeqnx_sin2sin4_ctlent_nodcyc'
            ctl_desc='fixed eqnx, sin2sin4 SSTs, ctl ent, no dcyc'
            exp_infile='/home/ss901165/um_output6/xhcco/time/xhccoa.pf2006-2008.timemean.nc'
            exp_name='fixeqnx_sin2sin4_1.5xent_nodcyc'
            exp_desc='fixed eqnx, sin2sin4 SSTs, 1.5x ent, no dcyc'
         END
      ENDCASE
      
      ctl_longitude=OPEN_AND_EXTRACT(ctl_infile,'longitude')
      ctl_latitude=OPEN_AND_EXTRACT(ctl_infile,'latitude')
      DEFINE_BOUNDARIES,box,ctl_latitude,ctl_longitude,ctl_box_tx,/LIMIT
      ctl_nlon=N_ELEMENTS(ctl_longitude)
      ctl_nlat=N_ELEMENTS(ctl_latitude)
      
      exp_longitude=OPEN_AND_EXTRACT(exp_infile,'longitude')
      exp_latitude=OPEN_AND_EXTRACT(exp_infile,'latitude')
      DEFINE_BOUNDARIES,box,exp_latitude,exp_longitude,exp_box_tx,/LIMIT
      exp_nlon=N_ELEMENTS(exp_longitude)
      exp_nlat=N_ELEMENTS(exp_latitude)
      
      ctl_var=REFORM(OPEN_AND_EXTRACT(ctl_infile,varname,$
                                      offset=[ctl_box_tx(1),ctl_box_tx(0),0,0],$
                                      count=[ctl_nlon,ctl_nlat,1,1]))*multiplier
      exp_var=REFORM(OPEN_AND_EXTRACT(exp_infile,varname,$
                                      offset=[exp_box_tx(1),exp_box_tx(0),0,0],$
                                      count=[exp_nlon,exp_nlat,1,1]))*multiplier

      ctl_mean=MEAN(ctl_var)
      exp_mean=MEAN(exp_var)
      
      psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/mean_state/hadgem3_monwg_aquaplanet_mean_state_precip.'+varname+'_'+ctl_name+'_'+box_name+'.ps'
      PSOPEN,file=psfile,FONT=6,CHARSIZE=150,MARGIN=2500,SPACE2=2000,XOFFSET=500,YOFFSET=2000,TFONT=6,TCHARSIZE=90,SPACE3=500,YSIZE=10000
;   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2),/SET
      GSET,XMIN=box(1),XMAX=box(3),YMIN=box(0),YMAX=box(2)
      CS,SCALE=1,NCOLS=N_ELEMENTS(levels_raw)+1,/REV
      LEVS,MANUAL=levels_raw
      CON,X=ctl_longitude,Y=ctl_latitude,FIELD=ctl_var,/NOLINES,/BLOCK,$
          TITLE='Clim '+vardesc+' from '+ctl_desc,CB_TITLE=vardesc+' ('+varunits+')'
      GPLOT,X=(box(3)-box(1))*0.9,Y=box(2)*1.2,TEXT='Mean = '+STRMID(STRTRIM(STRING(ctl_mean),1),0,6)
      AXES,XSTEP=20,YSTEP=10,YTITLE='Latitude (degrees north)',XTITLE='Longitude (degrees east)'
      PSCLOSE,/NOVIEW
      
      psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/mean_state/hadgem3_monwg_aquaplanet_mean_state_precip.'+varname+'_'+exp_name+'_'+box_name+'.ps'
      PSOPEN,file=psfile,FONT=6,CHARSIZE=150,MARGIN=2500,SPACE2=2000,XOFFSET=500,YOFFSET=2000,TFONT=6,TCHARSIZE=90,SPACE3=500,YSIZE=10000
;   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2),/SET
      GSET,XMIN=box(1),XMAX=box(3),YMIN=box(0),YMAX=box(2)
      CS,SCALE=1,NCOLS=N_ELEMENTS(levels_raw)+1,/REV
      LEVS,MANUAL=levels_raw
      CON,X=exp_longitude,Y=exp_latitude,FIELD=exp_var,/NOLINES,/BLOCK,$
          TITLE='Clim '+vardesc+' from '+exp_desc,CB_TITLE=vardesc+' ('+varunits+')'     
      GPLOT,X=(box(3)-box(1))*0.9,Y=box(2)*1.2,TEXT='Mean = '+STRMID(STRTRIM(STRING(exp_mean),1),0,6)
      AXES,XSTEP=20,YSTEP=10,YTITLE='Latitude (degrees north)',XTITLE='Longitude (degrees east)'
      PSCLOSE,/NOVIEW
      
      psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/mean_state/hadgem3_monwg_aquaplanet_mean_state_precip.'+varname+'_'+exp_name+'-minus-'+$
             ctl_name+'_'+box_name+'.ps'
      PSOPEN,file=psfile,FONT=6,CHARSIZE=150,MARGIN=2500,SPACE2=2000,XOFFSET=500,YOFFSET=2000,TFONT=6,TCHARSIZE=90,SPACE3=500,YSIZE=10000
;   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2),/SET
      GSET,XMIN=box(1),XMAX=box(3),YMIN=box(0),YMAX=box(2)
      CS,SCALE=1,NCOLS=N_ELEMENTS(levels_diff)+1,/REV
      LEVS,MANUAL=levels_diff
      CON,X=ctl_longitude,Y=ctl_latitude,FIELD=exp_var-ctl_var,/NOLINES,/BLOCK,$
          TITLE='Diff in clim '+vardesc+' for '+exp_desc+' minus '+ctl_desc,CB_TITLE=vardesc+' ('+varunits+')'      
      GPLOT,X=(box(3)-box(1))*0.9,Y=box(2)*1.25,TEXT='Mean = '+STRMID(STRTRIM(STRING(exp_mean-ctl_mean),1),0,6)
      AXES,XSTEP=20,YSTEP=10,YTITLE='Latitude (degrees north)',XTITLE='Longitude (degrees east)'
      PSCLOSE
   ENDFOR
ENDFOR

STOP
END
