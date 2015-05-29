PRO hadgem3kpp_phase_composites_olr_rain_sst_snapshot

  olr_var_levels=indgen(16)*15+100
;  olr_var_levels=indgen(41)*1-20
;  precip_var_levels=findgen(13)*0.4-2.6
  precip_var_levels=['1','2','3','4','6','8','10','12','15','18','21','24','28','32','36','40','45','50','55','60']
  sst_var_levels=['-0.33','-0.27','-0.21','-0.15','-0.09','-0.03','0.03','0.09','0.15','0.21','0.27','0.33']
  box=[-20,40,20,200]

;  rmm_infile='/home/ss901165/um_output6/xgspm/rmm_indices.nc'
;  olr_infile='/home/ss901165/um_output6/xgspm/hadgem3kpp_1.5xentrain_ga30.jan-dec_dmeans.i2-k1.olr.nc'
;  olr_clim_infile='/home/ss901165/um_output6/xgspm/hadgem3kpp_1.5xentrain_ga30.jan-dec_dmean_clim.i2-k1.olr.nc'
;  precip_infile='/home/ss901165/um_output6/xgspm/hadgem3kpp_1.5xentrain_ga30.jan-dec_dmeans.i2-k1.precip.nc'
;  precip_clim_infile='/home/ss901165/um_output6/xgspm/hadgem3kpp_1.5xentrain_ga30.jan-dec_dmean_clim.i2-k1.precip.nc'
;  sst_infile='/home/ss901165/um_output6/xgspm/hadgem3kpp_1.5xentrain_ga30.jan-dec_dmeans.i2-k1.sst.nc'
;  sst_clim_infile='/home/ss901165/um_output6/xgspm/hadgem3kpp_1.5xentrain_ga30.jan-dec_dmean_clim.i2-k1.sst.nc'

;  rmm_infile='/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2012.index_values.nc'
;  olr_infile='/home/ss901165/datasets/NOAA_CIRES_OLR/daily/temp.nc';NOAA_CIRES_OLR.jan-dec_dmeans.1978-2012.nc'
;  olr_clim_infile='/home/ss901165/datasets/NOAA_CIRES_OLR/daily/NOAA_CIRES_OLR.jan-dec_dmeans_clim.1978-2012.nc'
;  sst_infile='/home/ss901165/datasets/TMI_AMSRE/n96/tmi_fusion.jan-dec_dmeans.1998-2012.n96.nc'
;  sst_clim_infile='/home/ss901165/datasets/TMI_AMSRE/n96/tmi_fusion.jan-dec_dmean_clim.1998-2012.n96.nc'
  
  FOR i=0,1 DO BEGIN
     CASE i OF
        0 : BEGIN
           olr_infile='/home/ss901165/um_output6/xjgxc/i4/olr.nc'
           precip_infile='/home/ss901165/um_output6/xjgxc/i4/precip.nc'
           res='n512'
        END
        1 : BEGIN
           olr_infile='/home/ss901165/um_output6/xjgxb/i4/olr.nc'
           precip_infile='/home/ss901165/um_output6/xjgxb/i4/precip.nc'
           res='n96'
        END
     ENDCASE   
     olr_longitude=OPEN_AND_EXTRACT(olr_infile,'longitude')
     olr_latitude=OPEN_AND_EXTRACT(olr_infile,'latitude')
     DEFINE_BOUNDARIES,box,olr_latitude,olr_longitude,olr_box_tx,/LIMIT
     olr_nlon=N_ELEMENTS(olr_longitude)
     olr_nlat=N_ELEMENTS(olr_latitude)
     
     olr_in=OPEN_AND_EXTRACT(olr_infile,'olr',offset=[olr_box_tx(1),olr_box_tx(0),0,5],$
                             count=[olr_nlon,olr_nlat,1,1])
     precip=REFORM(OPEN_AND_EXTRACT(precip_infile,'precip',offset=[olr_box_tx(1),olr_box_tx(0),0,5],$
                                    count=[olr_nlon,olr_nlat,1,1]))*86400.
     
     olr=fltarr(olr_nlon,olr_nlat)
     
     psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/composites/hadgem3kpp_phase_composites_olr_rain_sst_snapshot.xjgxc_jun10i4_'+res+'.ps'
     PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1700,SPACE3=1200,XOFFSET=300,YOFFSET=2000,TFONT=2,TCHARSIZE=100,$
            SPACE2=1500,YSIZE=12000
     ;CS,SCALE=7,NCOLS=N_ELEMENTS(olr_var_levels)+1,white=[1]  
     ;LEVS,MANUAL=olr_var_levels
     ;white=FSC_COLOR('white',1)
     MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3)
     ;white=FSC_COLOR('black',1)
     ;CON,X=olr_longitude,Y=olr_latitude,FIELD=olr_in,$
     ;    /NOLINES,CB_TITLE='Outgoing longwave radiation (W m!U-2!N)',/NOAXES,/BLOCK
     CS,SCALE=1,NCOLS=N_ELEMENTS(precip_var_levels)+1,/REV,white=[2]
     LEVS,MANUAL=precip_var_levels
     CON,X=olr_longitude,Y=olr_latitude,FIELD=precip,$
         CB_WIDTH=105,/NOFILL,/NOLINELABELS,/BLOCK,/NOLINES,CB_TITLE='Precipitation (mm day!U-1!N)'
     ;AXES,XSTEP=10,YSTEP=5
     PSCLOSE,/NOVIEW
     print,N_ELEMENTS(precip),N_ELEMENTS(where(precip gt 1)),N_ELEMENTS(where(precip gt 1))/FLOAT(N_ELEMENTS(precip))
  ENDFOR
   
STOP
END
