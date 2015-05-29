PRO qld_higam_dblco2_precip_stbl

; Directories
higam_control_indir='/home/ss901165/higam_qccce/es_higemctl_eagsl'
higam_stbl_indir='/home/ss901165/higam_qccce/es_higem2xco2_eagsm'
higem_control_indir='/home/ss901165/higem_qccce/es_control_eafeb'
higem_stbl_indir='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu'

higam_control_nyears=28

higem_control_offset=20
higem_control_nyears=28

higam_stbl_nyears=28

higem_stbl_nyears=31

mask_infile='/home/ss901165/um_output/mask_n144_higam.nc'

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

   diff_higam_stbl_control=higam_stbl_precip_clim-higam_control_precip_clim
   ratio_higam_stbl_control=higam_stbl_precip_clim/higam_control_precip_clim

   diff_higam_higem_stbl=higam_stbl_precip_clim-higem_stbl_precip_clim
   ratio_higam_higem_stbl=higam_stbl_precip_clim/higem_stbl_precip_clim
   
   diff_higam_higem_control=higam_control_precip_clim-higem_control_precip_clim
   ratio_higam_higem_control=higam_control_precip_clim/higem_control_precip_clim

   diff_higem_stbl_control=higem_stbl_precip_clim-higem_control_precip_clim
   ratio_higem_stbl_control=higem_stbl_precip_clim/higem_control_precip_clim
   
   psfile='/home/ss901165/idl/queensland/higam/double_co2/precip/qld_higam_dblco2_precip_stbl.diff_higam_stbl-minus-higam_control.'+$
          season_name+'_'+season_type+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
   IF season_type eq 'amean' THEN BEGIN
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_annual)+1,/REV,white=[9]
   ENDIF ELSE $
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_season)+1,/REV,white=[9] 
   MAP,LONMIN=MIN(higam_stbl_longitude),LONMAX=MAX(higam_stbl_longitude),LATMIN=MIN(higam_stbl_latitude),LATMAX=MAX(higam_stbl_latitude),/hires ;,/SET
   IF season_type eq 'amean' THEN BEGIN
      LEVS,MANUAL=mylevs_diff_annual
      cb_label='mm year!U-1!N'
   ENDIF ELSE BEGIN
      LEVS,MANUAL=mylevs_diff_season
      cb_label='mm season!U-1!N'
   ENDELSE
   CON,FIELD=diff_higam_stbl_control*season_ndays,X=higam_stbl_longitude,Y=higam_stbl_latitude,$
       TITLE='Diff in clim rainfall between HiGAM 2xCO2 (30 years) and control (30 years) '+$
       '- '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label          
   AXES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higam/double_co2/precip/qld_higam_dblco2_precip_stbl.ratio_higam_stbl-div-higam_control.'+$
          season_name+'_'+season_type+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
   IF season_type eq 'amean' THEN BEGIN
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_annual)+1,/REV,white=[9]
   ENDIF ELSE $
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_season)+1,/REV,white=[9] 
   MAP,LONMIN=MIN(higam_stbl_longitude),LONMAX=MAX(higam_stbl_longitude),LATMIN=MIN(higam_stbl_latitude),LATMAX=MAX(higam_stbl_latitude),/hires ;,/SET
   IF season_type eq 'amean' THEN BEGIN
      LEVS,MANUAL=mylevs_ratio_annual      
   ENDIF ELSE $
      LEVS,MANUAL=mylevs_ratio_season
   CON,FIELD=ratio_higam_stbl_control,X=higam_stbl_longitude,Y=higam_stbl_latitude,$
       TITLE='Ratio in clim rainfall between HiGAM 2xCO2 (30 years) and control (30 years) '+$
       '- '+season_name,/NOLINES,/BLOCK,CB_TITLE='Ratio of rainfall (unitless)'
   AXES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higam/double_co2/precip/qld_higam_dblco2_precip_stbl.diff_higam_stbl-minus-higem_stbl.'+$
          season_name+'_'+season_type+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
   IF season_type eq 'amean' THEN BEGIN
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_annual)+1,/REV,white=[9]
   ENDIF ELSE $
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_season)+1,/REV,white=[9] 
   MAP,LONMIN=MIN(higam_stbl_longitude),LONMAX=MAX(higam_stbl_longitude),LATMIN=MIN(higam_stbl_latitude),LATMAX=MAX(higam_stbl_latitude),/hires ;,/SET
   IF season_type eq 'amean' THEN BEGIN
      LEVS,MANUAL=mylevs_diff_annual
      cb_label='mm year!U-1!N'
   ENDIF ELSE BEGIN
      LEVS,MANUAL=mylevs_diff_season
      cb_label='mm season!U-1!N'
   ENDELSE
   CON,FIELD=diff_higam_higem_stbl*season_ndays,X=higam_stbl_longitude,Y=higam_stbl_latitude,$
       TITLE='Diff in clim rainfall between HiGAM 2xCO2 (30 years) and HiGEM 2xCO2 (30 years) '+$
       '- '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label          
   AXES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higam/double_co2/precip/qld_higam_dblco2_precip_stbl.ratio_higam_stbl-div-higem_stbl.'+$
          season_name+'_'+season_type+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
   IF season_type eq 'amean' THEN BEGIN
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_annual)+1,/REV,white=[9]
   ENDIF ELSE $
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_season)+1,/REV,white=[9] 
   MAP,LONMIN=MIN(higam_stbl_longitude),LONMAX=MAX(higam_stbl_longitude),LATMIN=MIN(higam_stbl_latitude),LATMAX=MAX(higam_stbl_latitude),/hires ;,/SET
   IF season_type eq 'amean' THEN BEGIN
      LEVS,MANUAL=mylevs_ratio_annual      
   ENDIF ELSE $
      LEVS,MANUAL=mylevs_ratio_season
   CON,FIELD=ratio_higam_higem_stbl,X=higam_stbl_longitude,Y=higam_stbl_latitude,$
       TITLE='Ratio in clim rainfall between HiGAM 2xCO2 (30 years) and HiGEM 2xCO2 (30 years) '+$
       '- '+season_name,/NOLINES,/BLOCK,CB_TITLE='Ratio of rainfall (unitless)'
   AXES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higam/double_co2/precip/qld_higam_dblco2_precip_stbl.diff_higam_control-minus-higem_control.'+$
          season_name+'_'+season_type+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
   IF season_type eq 'amean' THEN BEGIN
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_annual)+1,/REV,white=[9]
   ENDIF ELSE $
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_season)+1,/REV,white=[9] 
   MAP,LONMIN=MIN(higam_control_longitude),LONMAX=MAX(higam_control_longitude),LATMIN=MIN(higam_control_latitude),LATMAX=MAX(higam_control_latitude),/hires ;,/SET
   IF season_type eq 'amean' THEN BEGIN
      LEVS,MANUAL=mylevs_diff_annual
      cb_label='mm year!U-1!N'
   ENDIF ELSE BEGIN
      LEVS,MANUAL=mylevs_diff_season
      cb_label='mm season!U-1!N'
   ENDELSE
   CON,FIELD=diff_higam_higem_control*season_ndays,X=higam_control_longitude,Y=higam_control_latitude,$
       TITLE='Diff in clim rainfall between HiGAM control (30 years) and HiGEM control (30 years) '+$
       '- '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label          
   AXES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higam/double_co2/precip/qld_higam_dblco2_precip_stbl.ratio_higam_control-div-higem_control.'+$
          season_name+'_'+season_type+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
   IF season_type eq 'amean' THEN BEGIN
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_annual)+1,/REV,white=[9]
   ENDIF ELSE $
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_season)+1,/REV,white=[9] 
   MAP,LONMIN=MIN(higam_control_longitude),LONMAX=MAX(higam_control_longitude),LATMIN=MIN(higam_control_latitude),LATMAX=MAX(higam_control_latitude),/hires ;,/SET
   IF season_type eq 'amean' THEN BEGIN
      LEVS,MANUAL=mylevs_ratio_annual      
   ENDIF ELSE $
      LEVS,MANUAL=mylevs_ratio_season
   CON,FIELD=ratio_higam_higem_control,X=higam_control_longitude,Y=higam_control_latitude,$
       TITLE='Ratio in clim rainfall between HiGAM control (30 years) and HiGEM control (30 years) '+$
       '- '+season_name,/NOLINES,/BLOCK,CB_TITLE='Ratio of rainfall (unitless)'
   AXES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higam/double_co2/precip/qld_higam_dblco2_precip_stbl.diff_higem_stbl-minus-higem_control.'+$
          season_name+'_'+season_type+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
   IF season_type eq 'amean' THEN BEGIN
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_annual)+1,/REV,white=[9]
   ENDIF ELSE $
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_season)+1,/REV,white=[9] 
   MAP,LONMIN=MIN(higem_control_longitude),LONMAX=MAX(higem_control_longitude),LATMIN=MIN(higem_control_latitude),LATMAX=MAX(higem_control_latitude),/hires ;,/SET
   IF season_type eq 'amean' THEN BEGIN
      LEVS,MANUAL=mylevs_diff_annual
      cb_label='mm year!U-1!N'
   ENDIF ELSE BEGIN
      LEVS,MANUAL=mylevs_diff_season
      cb_label='mm season!U-1!N'
   ENDELSE
   CON,FIELD=diff_higem_stbl_control*season_ndays,X=higem_control_longitude,Y=higem_control_latitude,$
       TITLE='Diff in clim rainfall between HiGEM 2xCO2 (30 years) and HiGEM control (30 years) '+$
       '- '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label          
   AXES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higam/double_co2/precip/qld_higam_dblco2_precip_stbl.ratio_higem_stbl-div-higem_control.'+$
          season_name+'_'+season_type+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
   IF season_type eq 'amean' THEN BEGIN
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_annual)+1,/REV,white=[9]
   ENDIF ELSE $
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_season)+1,/REV,white=[9] 
   MAP,LONMIN=MIN(higem_control_longitude),LONMAX=MAX(higem_control_longitude),LATMIN=MIN(higem_control_latitude),LATMAX=MAX(higem_control_latitude),/hires ;,/SET
   IF season_type eq 'amean' THEN BEGIN
      LEVS,MANUAL=mylevs_ratio_annual      
   ENDIF ELSE $
      LEVS,MANUAL=mylevs_ratio_season
   CON,FIELD=ratio_higem_stbl_control,X=higem_control_longitude,Y=higem_control_latitude,$
       TITLE='Ratio in clim rainfall between HiGEM 2xCO2 (30 years) and HiGEM control (30 years) '+$
       '- '+season_name,/NOLINES,/BLOCK,CB_TITLE='Ratio of rainfall (unitless)'
   AXES
   PSCLOSE,/NOVIEW

   mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
   mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
   DEFINE_BOUNDARIES,qldbox,mask_latitude,mask_longitude,mask_qldbox_tx,/LIMIT
   mask_nlon=N_ELEMENTS(mask_longitude)
   mask_nlat=N_ELEMENTS(mask_latitude)

   DEFINE_BOUNDARIES,qldbox,higam_control_latitude,higam_control_longitude,higam_control_qldbox_tx
   DEFINE_BOUNDARIES,qldbox,higam_stbl_latitude,higam_stbl_longitude,trans_qldbox_tx
   
   mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                                offset=[mask_qldbox_tx(1),mask_qldbox_tx(0),0,0],$
                                count=[mask_nlon,mask_nlat,1,1]))
   
   higam_control_qld_aavg=fltarr(higam_control_nyears)
   higam_stbl_qld_aavg=fltarr(higam_stbl_nyears)
   FOR j=0,higam_control_nyears-1 DO BEGIN
      thisyear_precip=REFORM(higam_control_precip(higam_control_qldbox_tx(1):higam_control_qldbox_tx(3),higam_control_qldbox_tx(0):higam_control_qldbox_tx(2),j))
      thisyear_precip[where(mask eq 0)]=!Values.F_NaN
      higam_control_qld_aavg(j)=MEAN(thisyear_precip,/NaN)      
   ENDFOR
   FOR j=0,higam_stbl_nyears-1 DO BEGIN
      thisyear_precip=REFORM(higam_stbl_precip(trans_qldbox_tx(1):trans_qldbox_tx(3),trans_qldbox_tx(0):trans_qldbox_tx(2),j))
      thisyear_precip[where(mask eq 0)]=!Values.F_NaN
      higam_stbl_qld_aavg(j)=MEAN(thisyear_precip,/NaN)     
   ENDFOR

   psfile='/home/ss901165/idl/queensland/higam/double_co2/precip/qld_higam_dblco2_precip_stbl.qld_aavg_ts_'+$
          season_name+'_'+season_type+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=200,XOFFSET=1000,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,SPACE3=200
   GSET,XMIN=0,XMAX=higam_control_nyears,YMIN=ts_min,YMAX=ts_max,$
        TITLE='Queensland area-average precipitation in Higam control, transient and stabilized simulations - '+season_name
   GPLOT,X=indgen(higam_control_nyears),Y=higam_control_qld_aavg*season_ndays,COL=FSC_COLOR('black'),STYLE=0,THICK=40
   GPLOT,X=indgen(higam_stbl_nyears),Y=higam_stbl_qld_aavg*season_ndays,COL=FSC_COLOR('blue'),STYLE=0,THICK=40
   
   temp=SMOOTH(higam_control_qld_aavg*season_ndays,5)
   GPLOT,X=indgen(higam_control_nyears-4)+3,Y=temp(2:higam_control_nyears-3),COL=FSC_COLOR('black'),STYLE=0,THICK=150
   temp=SMOOTH(higam_stbl_qld_aavg*season_ndays,5)
   GPLOT,X=indgen(higam_stbl_nyears-4)+3,Y=temp(2:higam_stbl_nyears-3),COL=FSC_COLOR('blue'),STYLE=0,THICK=150

   GPLOT,X=[0,higam_control_nyears],Y=[MEAN(higam_control_qld_aavg),MEAN(higam_control_qld_aavg)]*season_ndays,STYLE=2
;   GPLOT,X=[0,higam_control_nyears],Y=[MEAN(control_qld_aavg)+STDDEV(control_qld_aavg),$
;                                       MEAN(control_qld_aavg)+STDDEV(control_qld_aavg)]*season_ndays,STYLE=1
;   GPLOT,X=[0,higam_control_nyears],Y=[MEAN(control_qld_aavg)-STDDEV(control_qld_aavg),$
;                                       MEAN(control_qld_aavg)-STDDEV(control_qld_aavg)]*season_ndays,STYLE=1

   AXES,XSTEP=10,YSTEP=ts_step,YMINOR=ts_step/2.,XMINOR=2,XTITLE='Year of Higam integration',YTITLE='Total precipitation (mm) averaged over Queensland land points'
   
   GLEGEND,labels=['Five-year running mean','Yearly timeseries','HiGAM 2xCO2 fixed','HiGAM control'],$
           COL=[FSC_COLOR('black'),FSC_COLOR('black'),FSC_COLOR('blue'),FSC_COLOR('black')],$
           THICK=[150,40,100,100,100],LEGPOS=1
                

   PSCLOSE,/NOVIEW
ENDFOR

STOP
END

