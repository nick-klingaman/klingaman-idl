PRO qld_nugam_dblco2_precip_stbl

; Directories
higam_control_indir='/home/ss901165/higam_qccce/es_higemctl_eagsl'
higam_stbl_indir='/home/ss901165/higam_qccce/es_higem2xco2_eagsm'

higem_control_indir='/home/ss901165/higem_qccce/es_control_eafeb'
higem_stbl_indir='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu'

higam_control_nyears=28
higam_stbl_nyears=28

nugam_control_nyears=28
nugam_stbl_nyears=28

higem_control_offset=20
higem_control_nyears=28
higem_stbl_nyears=31

mask_n144_infile='/home/ss901165/um_output/mask_n144_higam.nc'
mask_n216_infile='/home/ss901165/um_output/mask_n216_nugam.nc'

box=[-50,90,-1,180]
qldbox=[-32,137,-11,154]

mylevs_lintrend_annual=['-13','-11','-9','-7','-5','-3','-1','1','3','5','7','9','11','13']
mylevs_lintrend_season=['-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5']

mylevs_diff_season=['-156','-132','-108','-84','-60','-36','-12','12','36','60','84','108','132','156']
mylevs_diff_annual=['-325','-275','-225','-175','-125','-75','-25','25','75','125','175','225','275','325']

mylevs_ratio_season=['0.35','0.45','0.55','0.65','0.75','0.85','0.95','1.05','1.18','1.33','1.54','1.82','2.22','2.86']
mylevs_ratio_annual=['0.35','0.45','0.55','0.65','0.75','0.85','0.95','1.05','1.18','1.33','1.54','1.82','2.22','2.86']

n_seasons=5
FOR i=0,n_seasons-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         season_name='dec-feb'
         season_type='smean'
         season_ndays=90.
         ts_min=100
         ts_max=600
         ts_step=25
      END
      1 : BEGIN
         season_name='mar-may'
         season_type='smean'
         season_ndays=90.
         ts_min=0
         ts_max=400
         ts_step=25
      END
      2 : BEGIN
         season_name='jun-aug'
         season_type='smean'
         season_ndays=90.
         ts_min=0
         ts_max=150
         ts_step=15
      END
      3 : BEGIN
         season_name='sep-nov'
         season_type='smean'
         season_ndays=90.
         ts_min=0
         ts_max=220
         ts_step=20         
      END
      4 : BEGIN
         season_name='may-apr'
         season_type='amean'
         season_ndays=360.
         ts_min=200
         ts_max=1000
         ts_step=100
      END
   ENDCASE

   higam_control_infile=higam_control_indir+'/higam_eagsl.'+season_name+'_'+season_type+'s.k9-n7.precip.global_domain.nc'
   higam_stbl_infile=higam_stbl_indir+'/higam_eagsm.'+season_name+'_'+season_type+'s.o3-r1.precip.global_domain.nc'
   
   higem_control_infile=higem_control_indir+'/higem_eafeb.'+season_name+'_'+season_type+'s.h9-w8.precip.global_domain.nc'
   higem_stbl_infile=higem_stbl_indir+'/higem_eadwu.'+season_name+'_'+season_type+'s.o2-r3.precip.global_domain.nc'

   higam_control_longitude=OPEN_AND_EXTRACT(higam_control_infile,'longitude')
   higam_control_latitude=OPEN_AND_EXTRACT(higam_control_infile,'latitude')
   DEFINE_BOUNDARIES,box,higam_control_latitude,higam_control_longitude,higam_control_box_tx,/LIMIT
   higam_control_nlon=N_ELEMENTS(higam_control_longitude)
   higam_control_nlat=N_ELEMENTS(higam_control_latitude)
   
   higam_stbl_longitude=OPEN_AND_EXTRACT(higam_stbl_infile,'longitude')
   higam_stbl_latitude=OPEN_AND_EXTRACT(higam_stbl_infile,'latitude')
   DEFINE_BOUNDARIES,box,higam_stbl_latitude,higam_stbl_longitude,higam_stblbox_tx,/LIMIT
   higam_stbl_nlon=N_ELEMENTS(higam_stbl_longitude)
   higam_stbl_nlat=N_ELEMENTS(higam_stbl_latitude)
   
   higem_control_longitude=OPEN_AND_EXTRACT(higem_control_infile,'longitude')
   higem_control_latitude=OPEN_AND_EXTRACT(higem_control_infile,'latitude')
   DEFINE_BOUNDARIES,box,higem_control_latitude,higem_control_longitude,higem_control_box_tx,/LIMIT
   higem_control_nlon=N_ELEMENTS(higem_control_longitude)
   higem_control_nlat=N_ELEMENTS(higem_control_latitude)
   
   higem_stbl_longitude=OPEN_AND_EXTRACT(higem_stbl_infile,'longitude')
   higem_stbl_latitude=OPEN_AND_EXTRACT(higem_stbl_infile,'latitude')
   DEFINE_BOUNDARIES,box,higem_stbl_latitude,higem_stbl_longitude,higem_stblbox_tx,/LIMIT
   higem_stbl_nlon=N_ELEMENTS(higem_stbl_longitude)
   higem_stbl_nlat=N_ELEMENTS(higem_stbl_latitude)
   
   higam_control_precip=OPEN_AND_EXTRACT(higam_control_infile,'precip',$
                                         offset=[higam_control_box_tx(1),higam_control_box_tx(0),0],$
                                         count=[higam_control_nlon,higam_control_nlat,higam_control_nyears])*86400.
   
   higam_stbl_precip=OPEN_AND_EXTRACT(higam_stbl_infile,'precip',$
                                      offset=[higam_stblbox_tx(1),higam_stblbox_tx(0),0],$
                                      count=[higam_stbl_nlon,higam_stbl_nlat,higam_stbl_nyears])*86400.
   
   higem_control_precip=OPEN_AND_EXTRACT(higem_control_infile,'precip',$
                                         offset=[higem_control_box_tx(1),higem_control_box_tx(0),higem_control_offset],$
                                         count=[higem_control_nlon,higem_control_nlat,higem_control_nyears])*86400.
   
   higem_stbl_precip=OPEN_AND_EXTRACT(higem_stbl_infile,'precip',$
                                      offset=[higem_stblbox_tx(1),higem_stblbox_tx(0),0],$
                                      count=[higem_stbl_nlon,higem_stbl_nlat,higem_stbl_nyears])*86400.

   higam_stbl_precip_clim=fltarr(higam_stbl_nlon,higam_stbl_nlat)
   FOR j=0,higam_stbl_nlon-1 DO $
      FOR k=0,higam_stbl_nlat-1 DO $
         higam_stbl_precip_clim(j,k)=MEAN(higam_stbl_precip(j,k,*))
   
   higam_control_precip_clim=fltarr(higam_control_nlon,higam_control_nlat)
   FOR j=0,higam_control_nlon-1 DO $
      FOR k=0,higam_control_nlat-1 DO $
         higam_control_precip_clim(j,k)=MEAN(higam_control_precip(j,k,*))
   
   higem_stbl_precip_clim=fltarr(higem_stbl_nlon,higem_stbl_nlat)
   FOR j=0,higem_stbl_nlon-1 DO $
      FOR k=0,higem_stbl_nlat-1 DO $
         higem_stbl_precip_clim(j,k)=MEAN(higem_stbl_precip(j,k,*))

   higem_control_precip_clim=fltarr(higem_control_nlon,higem_control_nlat)
   FOR j=0,higem_control_nlon-1 DO $
      FOR k=0,higem_control_nlat-1 DO $
         higem_control_precip_clim(j,k)=MEAN(higem_control_precip(j,k,*))
   
   n_nugam_members=9
   FOR m=0,n_nugam_members-1 DO BEGIN
      CASE m OF
         0 : BEGIN
            nugam_control_indir='/home/ss901165/nugam_qccce/hpcx_higemctl_xdxpm'
            nugam_stbl_indir='/home/ss901165/nugam_qccce/hpcx_higem2xco2_xdxpn'
            nugam_control_id='xdxpm'
            nugam_stbl_id='xdxpn'
         END
         1 : BEGIN
            nugam_control_indir='/home/ss901165/nugam_qccce/hpcx_higemctl_xfzwa'
            nugam_stbl_indir='/home/ss901165/nugam_qccce/hpcx_higem2xco2_xfzwb'
            nugam_control_id='xfzwa'
            nugam_stbl_id='xfzwb'
         END
         2 : BEGIN
            nugam_control_indir='/home/ss901165/nugam_qccce/hpcx_higemctl_xfzwd'
            nugam_stbl_indir='/home/ss901165/nugam_qccce/hpcx_higem2xco2_xdxpp'
            nugam_control_id='xfzwd'
            nugam_stbl_id='xdxpp'
         END
         3 : BEGIN
            nugam_control_indir='/home/ss901165/nugam_qccce/hpcx_higemctl_xdxpm'
            nugam_stbl_indir='/home/ss901165/nugam_qccce/hpcx_higem2xco2_xfzwb'
            nugam_control_id='xdxpm'
            nugam_stbl_id='xfzwb'
         END
         4 : BEGIN
            nugam_control_indir='/home/ss901165/nugam_qccce/hpcx_higemctl_xdxpm'
            nugam_stbl_indir='/home/ss901165/nugam_qccce/hpcx_higem2xco2_xdxpp'
            nugam_control_id='xdxpm'
            nugam_stbl_id='xdxpp'
         END
         5 : BEGIN          
            nugam_control_indir='/home/ss901165/nugam_qccce/hpcx_higemctl_xfzwa'
            nugam_stbl_indir='/home/ss901165/nugam_qccce/hpcx_higem2xco2_xdxpn'
            nugam_control_id='xfzwa'
            nugam_stbl_id='xdxpn'
         END  
         6 : BEGIN
            nugam_control_indir='/home/ss901165/nugam_qccce/hpcx_higemctl_xfzwa'
            nugam_stbl_indir='/home/ss901165/nugam_qccce/hpcx_higem2xco2_xdxpp'
            nugam_control_id='xfzwa'
            nugam_stbl_id='xdxpp'
         END
         7 : BEGIN
            nugam_control_indir='/home/ss901165/nugam_qccce/hpcx_higemctl_xfzwd'
            nugam_stbl_indir='/home/ss901165/nugam_qccce/hpcx_higem2xco2_xdxpn'
            nugam_control_id='xfzwd'
            nugam_stbl_id='xdxpn'
         END
         8 :BEGIN
            nugam_control_indir='/home/ss901165/nugam_qccce/hpcx_higemctl_xfzwd'
            nugam_stbl_indir='/home/ss901165/nugam_qccce/hpcx_higem2xco2_xfzwb'
            nugam_control_id='xfzwd'
            nugam_stbl_id='xfzwb'
         END
      ENDCASE
      nugam_control_infile=nugam_control_indir+'/nugam_'+nugam_control_id+'.'+season_name+'_'+season_type+'s.k9-n7.precip.global_domain.nc'
      nugam_stbl_infile=nugam_stbl_indir+'/nugam_'+nugam_stbl_id+'.'+season_name+'_'+season_type+'s.o3-r1.precip.global_domain.nc'
      
      nugam_n144_control_infile=nugam_control_indir+'/nugam_'+nugam_control_id+'.'+season_name+'_'+season_type+'s.k9-n7.precip.global_domain.n144.nc'
      nugam_n144_stbl_infile=nugam_stbl_indir+'/nugam_'+nugam_stbl_id+'.'+season_name+'_'+season_type+'s.o3-r1.precip.global_domain.n144.nc'        
      
      print,nugam_n144_control_infile,nugam_n144_stbl_infile

      nugam_control_longitude=OPEN_AND_EXTRACT(nugam_control_infile,'longitude_1')
      nugam_control_latitude=OPEN_AND_EXTRACT(nugam_control_infile,'latitude_1')
      DEFINE_BOUNDARIES,box,nugam_control_latitude,nugam_control_longitude,nugam_control_box_tx,/LIMIT
      nugam_control_nlon=N_ELEMENTS(nugam_control_longitude)
      nugam_control_nlat=N_ELEMENTS(nugam_control_latitude)
      
      nugam_stbl_longitude=OPEN_AND_EXTRACT(nugam_stbl_infile,'longitude_1')
      nugam_stbl_latitude=OPEN_AND_EXTRACT(nugam_stbl_infile,'latitude_1')
      DEFINE_BOUNDARIES,box,nugam_stbl_latitude,nugam_stbl_longitude,nugam_stblbox_tx,/LIMIT
      nugam_stbl_nlon=N_ELEMENTS(nugam_stbl_longitude)
      nugam_stbl_nlat=N_ELEMENTS(nugam_stbl_latitude)
      
      nugam_n144_control_longitude=OPEN_AND_EXTRACT(nugam_n144_control_infile,'longitude')
      nugam_n144_control_latitude=OPEN_AND_EXTRACT(nugam_n144_control_infile,'latitude')
      DEFINE_BOUNDARIES,box,nugam_n144_control_latitude,nugam_n144_control_longitude,nugam_n144_control_box_tx,/LIMIT
      nugam_n144_control_nlon=N_ELEMENTS(nugam_n144_control_longitude)
      nugam_n144_control_nlat=N_ELEMENTS(nugam_n144_control_latitude)
   
      nugam_n144_stbl_longitude=OPEN_AND_EXTRACT(nugam_n144_stbl_infile,'longitude')
      nugam_n144_stbl_latitude=OPEN_AND_EXTRACT(nugam_n144_stbl_infile,'latitude')
      DEFINE_BOUNDARIES,box,nugam_n144_stbl_latitude,nugam_n144_stbl_longitude,nugam_n144_stblbox_tx,/LIMIT
      nugam_n144_stbl_nlon=N_ELEMENTS(nugam_n144_stbl_longitude)
      nugam_n144_stbl_nlat=N_ELEMENTS(nugam_n144_stbl_latitude)
       
      nugam_control_precip=OPEN_AND_EXTRACT(nugam_control_infile,'precip',$
                                            offset=[nugam_control_box_tx(1),nugam_control_box_tx(0),0],$
                                            count=[nugam_control_nlon,nugam_control_nlat,nugam_control_nyears])*86400.
      
      nugam_stbl_precip=OPEN_AND_EXTRACT(nugam_stbl_infile,'precip',$
                                         offset=[nugam_stblbox_tx(1),nugam_stblbox_tx(0),0],$
                                         count=[nugam_stbl_nlon,nugam_stbl_nlat,nugam_stbl_nyears])*86400.
      
      nugam_n144_control_precip=OPEN_AND_EXTRACT(nugam_n144_control_infile,'precip',$
                                                 offset=[nugam_n144_control_box_tx(1),nugam_n144_control_box_tx(0),0],$
                                                 count=[nugam_n144_control_nlon,nugam_n144_control_nlat,nugam_control_nyears])*86400.
      
      nugam_n144_stbl_precip=OPEN_AND_EXTRACT(nugam_n144_stbl_infile,'precip',$
                                              offset=[nugam_n144_stblbox_tx(1),nugam_n144_stblbox_tx(0),0],$
                                              count=[nugam_n144_stbl_nlon,nugam_n144_stbl_nlat,nugam_stbl_nyears])*86400. 
       
      nugam_stbl_precip_clim=fltarr(nugam_stbl_nlon,nugam_stbl_nlat)
      FOR j=0,nugam_stbl_nlon-1 DO $
         FOR k=0,nugam_stbl_nlat-1 DO $
            nugam_stbl_precip_clim(j,k)=MEAN(nugam_stbl_precip(j,k,*))
      
      nugam_control_precip_clim=fltarr(nugam_control_nlon,nugam_control_nlat)
      FOR j=0,nugam_control_nlon-1 DO $
         FOR k=0,nugam_control_nlat-1 DO $
            nugam_control_precip_clim(j,k)=MEAN(nugam_control_precip(j,k,*))
      
      nugam_n144_stbl_precip_clim=fltarr(nugam_n144_stbl_nlon,nugam_n144_stbl_nlat)
      FOR j=0,nugam_n144_stbl_nlon-1 DO $
         FOR k=0,nugam_n144_stbl_nlat-1 DO $
            nugam_n144_stbl_precip_clim(j,k)=MEAN(nugam_n144_stbl_precip(j,k,*))
      
      nugam_n144_control_precip_clim=fltarr(nugam_n144_control_nlon,nugam_n144_control_nlat)
      FOR j=0,nugam_n144_control_nlon-1 DO $
         FOR k=0,nugam_n144_control_nlat-1 DO $
            nugam_n144_control_precip_clim(j,k)=MEAN(nugam_n144_control_precip(j,k,*))
      
      diff_nugam_stbl_control=nugam_stbl_precip_clim-nugam_control_precip_clim
      ratio_nugam_stbl_control=nugam_stbl_precip_clim/nugam_control_precip_clim
      
      diff_nugam_higem_stbl=nugam_n144_stbl_precip_clim-higem_stbl_precip_clim
      ratio_nugam_higem_stbl=nugam_n144_stbl_precip_clim/higem_stbl_precip_clim
   
      diff_nugam_higem_control=nugam_n144_control_precip_clim-higem_control_precip_clim
      ratio_nugam_higem_control=nugam_n144_control_precip_clim/higem_control_precip_clim
      
      diff_nugam_higam_control=nugam_n144_control_precip_clim-higam_control_precip_clim
      ratio_nugam_higam_control=nugam_n144_control_precip_clim/higam_control_precip_clim
      
      diff_nugam_higam_stbl=nugam_n144_stbl_precip_clim-higam_stbl_precip_clim
      ratio_nugam_higam_stbl=nugam_n144_stbl_precip_clim/higam_stbl_precip_clim
      
      psfile='/home/ss901165/idl/queensland/nugam/double_co2/precip/qld_nugam_dblco2_precip_stbl.diff_nugam_stbl_'+nugam_stbl_id+'-minus-nugam_control_'+nugam_control_id+'.'+$
             season_name+'_'+season_type+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
             TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
      IF season_type eq 'amean' THEN BEGIN
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_annual)+1,/REV,white=[9]
      ENDIF ELSE $
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_season)+1,/REV,white=[9] 
      MAP,LONMIN=MIN(nugam_stbl_longitude),LONMAX=MAX(nugam_stbl_longitude),LATMIN=MIN(nugam_stbl_latitude),LATMAX=MAX(nugam_stbl_latitude),/hires ;,/SET
      IF season_type eq 'amean' THEN BEGIN
         LEVS,MANUAL=mylevs_diff_annual
         cb_label='mm year!U-1!N'
      ENDIF ELSE BEGIN
         LEVS,MANUAL=mylevs_diff_season
         cb_label='mm season!U-1!N'
      ENDELSE
      CON,FIELD=diff_nugam_stbl_control*season_ndays,X=nugam_stbl_longitude,Y=nugam_stbl_latitude,$
          TITLE='Diff in clim rainfall between NuGAM 2xCO2 '+nugam_stbl_id+' (30 years) and control '+nugam_control_id+' (30 years) '+$
          '- '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label          
      AXES
      PSCLOSE,/NOVIEW
      
      psfile='/home/ss901165/idl/queensland/nugam/double_co2/precip/qld_nugam_dblco2_precip_stbl.ratio_nugam_stbl_'+nugam_stbl_id+'-div-nugam_control_'+nugam_control_id+'.'+$
             season_name+'_'+season_type+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
             TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
      IF season_type eq 'amean' THEN BEGIN
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_annual)+1,/REV,white=[9]
      ENDIF ELSE $
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_season)+1,/REV,white=[9] 
      MAP,LONMIN=MIN(nugam_stbl_longitude),LONMAX=MAX(nugam_stbl_longitude),LATMIN=MIN(nugam_stbl_latitude),LATMAX=MAX(nugam_stbl_latitude),/hires ;,/SET
      IF season_type eq 'amean' THEN BEGIN
         LEVS,MANUAL=mylevs_ratio_annual      
      ENDIF ELSE $
         LEVS,MANUAL=mylevs_ratio_season
      CON,FIELD=ratio_nugam_stbl_control,X=nugam_stbl_longitude,Y=nugam_stbl_latitude,$
          TITLE='Ratio in clim rainfall between NuGAM 2xCO2 '+nugam_stbl_id+' (30 years) and control '+nugam_control_id+' (30 years) '+$
          '- '+season_name,/NOLINES,/BLOCK,CB_TITLE='Ratio of rainfall (unitless)'
      AXES
      PSCLOSE,/NOVIEW
      
      psfile='/home/ss901165/idl/queensland/nugam/double_co2/precip/qld_nugam_dblco2_precip_stbl.diff_nugam_stbl_'+nugam_stbl_id+'-minus-higem_stbl.'+$
             season_name+'_'+season_type+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
             TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
      IF season_type eq 'amean' THEN BEGIN
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_annual)+1,/REV,white=[9]
      ENDIF ELSE $
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_season)+1,/REV,white=[9] 
      MAP,LONMIN=MIN(nugam_stbl_longitude),LONMAX=MAX(nugam_stbl_longitude),LATMIN=MIN(nugam_stbl_latitude),LATMAX=MAX(nugam_stbl_latitude),/hires ;,/SET
      IF season_type eq 'amean' THEN BEGIN
         LEVS,MANUAL=mylevs_diff_annual
         cb_label='mm year!U-1!N'
      ENDIF ELSE BEGIN
         LEVS,MANUAL=mylevs_diff_season
         cb_label='mm season!U-1!N'
      ENDELSE
      CON,FIELD=diff_nugam_higem_stbl*season_ndays,X=higem_stbl_longitude,Y=higem_stbl_latitude,$
          TITLE='Diff in clim rainfall between NuGAM 2xCO2 '+nugam_stbl_id+' (30 years) and HiGEM 2xCO2 (30 years) '+$
          '- '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label          
      AXES
      PSCLOSE,/NOVIEW
      
      psfile='/home/ss901165/idl/queensland/nugam/double_co2/precip/qld_nugam_dblco2_precip_stbl.ratio_nugam_stbl_'+nugam_stbl_id+'-div-higem_stbl.'+$
             season_name+'_'+season_type+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
             TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
      IF season_type eq 'amean' THEN BEGIN
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_annual)+1,/REV,white=[9]
      ENDIF ELSE $
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_season)+1,/REV,white=[9] 
      MAP,LONMIN=MIN(nugam_stbl_longitude),LONMAX=MAX(nugam_stbl_longitude),LATMIN=MIN(nugam_stbl_latitude),LATMAX=MAX(nugam_stbl_latitude),/hires ;,/SET
      IF season_type eq 'amean' THEN BEGIN
         LEVS,MANUAL=mylevs_ratio_annual      
      ENDIF ELSE $
         LEVS,MANUAL=mylevs_ratio_season
      CON,FIELD=ratio_nugam_higem_stbl,X=higem_stbl_longitude,Y=higem_stbl_latitude,$
          TITLE='Ratio in clim rainfall between NuGAM 2xCO2 '+nugam_stbl_id+' (30 years) and HiGEM 2xCO2 (30 years) '+$
          '- '+season_name,/NOLINES,/BLOCK,CB_TITLE='Ratio of rainfall (unitless)'
      AXES
      PSCLOSE,/NOVIEW
      
      psfile='/home/ss901165/idl/queensland/nugam/double_co2/precip/qld_nugam_dblco2_precip_stbl.diff_nugam_control_'+nugam_control_id+'-minus-higem_control.'+$
             season_name+'_'+season_type+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
             TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
      IF season_type eq 'amean' THEN BEGIN
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_annual)+1,/REV,white=[9]
      ENDIF ELSE $
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_season)+1,/REV,white=[9] 
      MAP,LONMIN=MIN(nugam_control_longitude),LONMAX=MAX(nugam_control_longitude),LATMIN=MIN(nugam_control_latitude),LATMAX=MAX(nugam_control_latitude),/hires ;,/SET
      IF season_type eq 'amean' THEN BEGIN
         LEVS,MANUAL=mylevs_diff_annual
         cb_label='mm year!U-1!N'
      ENDIF ELSE BEGIN
         LEVS,MANUAL=mylevs_diff_season
         cb_label='mm season!U-1!N'
      ENDELSE
      CON,FIELD=diff_nugam_higem_control*season_ndays,X=higam_control_longitude,Y=higam_control_latitude,$
          TITLE='Diff in clim rainfall between NuGAM control (30 years) and HiGEM control (30 years) '+$
          '- '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label          
      AXES
      PSCLOSE,/NOVIEW
      
      psfile='/home/ss901165/idl/queensland/nugam/double_co2/precip/qld_nugam_dblco2_precip_stbl.ratio_nugam_control_'+nugam_control_id+'-div-higem_control.'+$
             season_name+'_'+season_type+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
             TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
      IF season_type eq 'amean' THEN BEGIN
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_annual)+1,/REV,white=[9]
      ENDIF ELSE $
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_season)+1,/REV,white=[9] 
      MAP,LONMIN=MIN(nugam_control_longitude),LONMAX=MAX(nugam_control_longitude),LATMIN=MIN(nugam_control_latitude),LATMAX=MAX(nugam_control_latitude),/hires ;,/SET
      IF season_type eq 'amean' THEN BEGIN
         LEVS,MANUAL=mylevs_ratio_annual      
      ENDIF ELSE $
         LEVS,MANUAL=mylevs_ratio_season
      CON,FIELD=ratio_nugam_higem_control,X=higam_control_longitude,Y=higam_control_latitude,$
          TITLE='Ratio in clim rainfall between NuGAM control (30 years) and HiGEM control (30 years) '+$
          '- '+season_name,/NOLINES,/BLOCK,CB_TITLE='Ratio of rainfall (unitless)'
      AXES
      PSCLOSE,/NOVIEW
      
      psfile='/home/ss901165/idl/queensland/nugam/double_co2/precip/qld_nugam_dblco2_precip_stbl.diff_nugam_control_'+nugam_control_id+'-minus-higam_control.'+$
             season_name+'_'+season_type+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
             TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
      IF season_type eq 'amean' THEN BEGIN
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_annual)+1,/REV,white=[9]
      ENDIF ELSE $
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_season)+1,/REV,white=[9] 
      MAP,LONMIN=MIN(nugam_control_longitude),LONMAX=MAX(nugam_control_longitude),LATMIN=MIN(nugam_control_latitude),LATMAX=MAX(nugam_control_latitude),/hires ;,/SET
      IF season_type eq 'amean' THEN BEGIN
         LEVS,MANUAL=mylevs_diff_annual
         cb_label='mm year!U-1!N'
      ENDIF ELSE BEGIN
         LEVS,MANUAL=mylevs_diff_season
         cb_label='mm season!U-1!N'
      ENDELSE
      CON,FIELD=diff_nugam_higam_control*season_ndays,X=higam_control_longitude,Y=higam_control_latitude,$
          TITLE='Diff in clim rainfall between NuGAM control (30 years) and HiGAM control (30 years) '+$
          '- '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label          
      AXES
      PSCLOSE,/NOVIEW
      
      psfile='/home/ss901165/idl/queensland/nugam/double_co2/precip/qld_nugam_dblco2_precip_stbl.ratio_nugam_control_'+nugam_control_id+'-div-higam_control.'+$
             season_name+'_'+season_type+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
             TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
      IF season_type eq 'amean' THEN BEGIN
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_annual)+1,/REV,white=[9]
      ENDIF ELSE $
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_season)+1,/REV,white=[9] 
      MAP,LONMIN=MIN(nugam_control_longitude),LONMAX=MAX(nugam_control_longitude),LATMIN=MIN(nugam_control_latitude),LATMAX=MAX(nugam_control_latitude),/hires ;,/SET
      IF season_type eq 'amean' THEN BEGIN
         LEVS,MANUAL=mylevs_ratio_annual      
      ENDIF ELSE $
         LEVS,MANUAL=mylevs_ratio_season
      CON,FIELD=ratio_nugam_higam_control,X=higam_control_longitude,Y=higam_control_latitude,$
          TITLE='Ratio in clim rainfall between NuGAM control (30 years) and HiGAM control (30 years) '+$
          '- '+season_name,/NOLINES,/BLOCK,CB_TITLE='Ratio of rainfall (unitless)'
      AXES
      PSCLOSE,/NOVIEW
      
      psfile='/home/ss901165/idl/queensland/nugam/double_co2/precip/qld_nugam_dblco2_precip_stbl.diff_nugam_stbl_'+nugam_stbl_id+'-minus-higam_stbl.'+$
             season_name+'_'+season_type+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
             TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
      IF season_type eq 'amean' THEN BEGIN
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_annual)+1,/REV,white=[9]
      ENDIF ELSE $
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_season)+1,/REV,white=[9] 
      MAP,LONMIN=MIN(nugam_stbl_longitude),LONMAX=MAX(nugam_stbl_longitude),LATMIN=MIN(nugam_stbl_latitude),LATMAX=MAX(nugam_stbl_latitude),/hires ;,/SET
      IF season_type eq 'amean' THEN BEGIN
         LEVS,MANUAL=mylevs_diff_annual
         cb_label='mm year!U-1!N'
      ENDIF ELSE BEGIN
         LEVS,MANUAL=mylevs_diff_season
         cb_label='mm season!U-1!N'
      ENDELSE
      CON,FIELD=diff_nugam_higam_stbl*season_ndays,X=higam_stbl_longitude,Y=higam_stbl_latitude,$
          TITLE='Diff in clim rainfall between NuGAM 2xCO2 '+nugam_stbl_id+' (30 years) and HiGAM 2xCO2 (30 years) '+$
          '- '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label          
      AXES
      PSCLOSE,/NOVIEW
      
      psfile='/home/ss901165/idl/queensland/nugam/double_co2/precip/qld_nugam_dblco2_precip_stbl.ratio_nugam_stbl_'+nugam_stbl_id+'-div-higam_stbl.'+$
             season_name+'_'+season_type+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
             TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
      IF season_type eq 'amean' THEN BEGIN
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_annual)+1,/REV,white=[9]
      ENDIF ELSE $
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_season)+1,/REV,white=[9] 
      MAP,LONMIN=MIN(nugam_stbl_longitude),LONMAX=MAX(nugam_stbl_longitude),LATMIN=MIN(nugam_stbl_latitude),LATMAX=MAX(nugam_stbl_latitude),/hires ;,/SET
      IF season_type eq 'amean' THEN BEGIN
         LEVS,MANUAL=mylevs_ratio_annual      
      ENDIF ELSE $
         LEVS,MANUAL=mylevs_ratio_season
      CON,FIELD=ratio_nugam_higam_stbl,X=higam_stbl_longitude,Y=higam_stbl_latitude,$
          TITLE='Ratio in clim rainfall between NuGAM 2xCO2 '+nugam_stbl_id+' (30 years) and HiGAM 2xCO2 (30 years) '+$
          '- '+season_name,/NOLINES,/BLOCK,CB_TITLE='Ratio of rainfall (unitless)'
      AXES
      PSCLOSE,/NOVIEW
   ENDFOR
ENDFOR

STOP
END

