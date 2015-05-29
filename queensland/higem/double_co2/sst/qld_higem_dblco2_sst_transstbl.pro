PRO qld_higem_dblco2_sst_transstbl

; Directories
higem_control_indir='/home/ss901165/higem_qccce/es_control_eafeb'
higem_trans_indir='/home/ss901165/higem_qccce/es_2pctco2_eafee'
higem_stbl_indir='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu'

higem_control_nyears=149
higem_trans_nyears=70
trans_offset=20
trans_stop_offset=31
higem_stbl_nyears=31
stbl_offset=33

mask_infile='/home/ss901165/um_output/mask_n144_higam.nc'

;box=[-70,0,70,360]
;qldbox=[-32,137,-11,154]
;region_name='global'

box=[-70,50,10,300]
region_name='sind_spac'

;mylevs_lintrend_annual=['-0.22','-0.18','-0.14','-0.10','-0.06','-0.02','0.02','0.06','0.10','0.14','0.18','0.22']
mylevs_lintrend_annual=['-0.035','-0.025','-0.015','-0.005','0.005','0.015','0.025','0.035','0.045','0.055','0.065','0.075','0.085','0.095','0.105']
;mylevs_lintrend_annual=['-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15','0.17','0.19','0.21']
mylevs_lintrend_season=mylevs_lintrend_annual

;mylevs_diff_season=['-6.0','-5.2','-4.4','-3.6','-2.8','-2.0','-1.2','-0.4','0.4','1.2','2.0','2.8','3.6','4.4','5.2','6.0']
mylevs_diff_season=['-1.4','-1.0','-0.6','-0.2','0.2','0.6','1.0','1.4','1.8','2.2','2.6','3.0','3.4','3.8','4.2']
;mylevs_diff_season=['-2.8','-2.0','-1.2','-0.4','0.4','1.2','2.0','2.8','3.6','4.4','5.2','6.0','6.8','7.6','8.4']
mylevs_diff_annual=mylevs_diff_season

;mylevs_clim_season=['2','3','4','5','6','7','8','9','10','11','12','13','14']
;mylevs_clim_annual=mylevs_clim_season

;mylevs_lintrend_annual=['-13','-11','-9','-7','-5','-3','-1','1','3','5','7','9','11','13']
;mylevs_lintrend_season=['-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5']

;mylevs_diff_season=['-156','-132','-108','-84','-60','-36','-12','12','36','60','84','108','132','156']
;mylevs_diff_annual=['-325','-275','-225','-175','-125','-75','-25','25','75','125','175','225','275','325']

;mylevs_ratio_season=['0.35','0.45','0.55','0.65','0.75','0.85','0.95','1.05','1.18','1.33','1.54','1.82','2.22','2.86']
;mylevs_ratio_annual=['0.35','0.45','0.55','0.65','0.75','0.85','0.95','1.05','1.18','1.33','1.54','1.82','2.22','2.86']

n_seasons=5
FOR i=0,n_seasons-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         season_name='dec-feb'
         season_type='smean'
;         ts_min=100
;         ts_max=600
;         ts_step=25
      END
      1 : BEGIN
         season_name='mar-may'
         season_type='smean'
         season_ndays=90.
;         ts_min=0
;         ts_max=400
;         ts_step=25
      END
      2 : BEGIN
         season_name='jun-aug'
         season_type='smean'
         season_ndays=90.
;         ts_min=0
;         ts_max=150
;         ts_step=15
      END
      3 : BEGIN
         season_name='sep-nov'
         season_type='smean'
         season_ndays=90.
;         ts_min=0
;         ts_max=220
;         ts_step=20         
      END
      4 : BEGIN
         season_name='may-apr'
         season_type='amean'
         season_ndays=360.
;         ts_min=200
;         ts_max=1000
;         ts_step=100
      END
   ENDCASE
   
   higem_control_sst_infile=higem_control_indir+'/higem_eafeb.'+season_name+'_'+season_type+'s.h9-w8.surf_temp.global_domain.nc'
   higem_trans_sst_infile=higem_trans_indir+'/higem_eafee.'+season_name+'_'+season_type+'s.k9-u7.surf_temp.global_domain.nc'
   higem_stbl_sst_infile=higem_stbl_indir+'/higem_eadwu.'+season_name+'_'+season_type+'s.o2-r3.surf_temp.global_domain.nc'
   
   control_sst_longitude=OPEN_AND_EXTRACT(higem_control_sst_infile,'longitude')
   control_sst_latitude=OPEN_AND_EXTRACT(higem_control_sst_infile,'latitude')
   DEFINE_BOUNDARIES,box,control_sst_latitude,control_sst_longitude,control_sst_box_tx,/LIMIT
   control_sst_nlon=N_ELEMENTS(control_sst_longitude)
   control_sst_nlat=N_ELEMENTS(control_sst_latitude)
   
   trans_sst_longitude=OPEN_AND_EXTRACT(higem_trans_sst_infile,'longitude')
   trans_sst_latitude=OPEN_AND_EXTRACT(higem_trans_sst_infile,'latitude')
   DEFINE_BOUNDARIES,box,trans_sst_latitude,trans_sst_longitude,trans_sst_box_tx,/LIMIT
   trans_sst_nlon=N_ELEMENTS(trans_sst_longitude)
   trans_sst_nlat=N_ELEMENTS(trans_sst_latitude)

   control_sst=OPEN_AND_EXTRACT(higem_control_sst_infile,'temp',$
                                   offset=[control_sst_box_tx(1),control_sst_box_tx(0),0],$
                                   count=[control_sst_nlon,control_sst_nlat,higem_control_nyears])
   
   trans_sst=OPEN_AND_EXTRACT(higem_trans_sst_infile,'temp_2',$
                                 offset=[trans_sst_box_tx(1),trans_sst_box_tx(0),0],$
                                 count=[trans_sst_nlon,trans_sst_nlat,higem_trans_nyears])

   stbl_sst=OPEN_AND_EXTRACT(higem_stbl_sst_infile,'temp_2',$
                                offset=[trans_sst_box_tx(1),trans_sst_box_tx(0),0],$
                                count=[trans_sst_nlon,trans_sst_nlat,higem_stbl_nyears])

   mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
   mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
   DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
   mask_nlon=N_ELEMENTS(mask_longitude)
   mask_nlat=N_ELEMENTS(mask_latitude)

   mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                         count=[mask_nlon,mask_nlat,1,1]))
   
   trans_sst_lintrend=fltarr(trans_sst_nlon,trans_sst_nlat)
   trans_sst_lincorr=fltarr(trans_sst_nlon,trans_sst_nlat)
   FOR j=0,trans_sst_nlon-1 DO BEGIN
      FOR k=0,trans_sst_nlat-1 DO BEGIN
         trans_sst_lintrend(j,k)=REGRESS(indgen(higem_trans_nyears),REFORM(trans_sst(j,k,*)),CORRELATION=temp)
         trans_sst_lincorr(j,k)=temp
      ENDFOR
   ENDFOR
   
   psfile='/home/ss901165/idl/queensland/higem/double_co2/sst/qld_higem_dblco2_sst_transstbl.trans_lintrend.'+season_name+'_'+$
          season_type+'_'+region_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=600,XOFFSET=500,YOFFSET=500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110
   IF season_type eq 'amean' THEN BEGIN
      CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_lintrend_annual)+1;,white=[8]
   ENDIF ELSE $
      CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_lintrend_season)+1;,white=[8] 
   MAP,LONMIN=MIN(trans_sst_longitude),LONMAX=MAX(trans_sst_longitude),LATMIN=MIN(trans_sst_latitude),LATMAX=MAX(trans_sst_latitude),/hires ;,/SET
   IF season_type eq 'amean' THEN BEGIN
      LEVS,MANUAL=mylevs_lintrend_annual
      cb_label='K year!U-1!N'
   ENDIF ELSE BEGIN
      LEVS,MANUAL=mylevs_lintrend_season
      cb_label='K year!U-1!N'
   ENDELSE
   trans_sst_lintrend[where(ABS(trans_sst_lincorr) le 0.274)]=!Values.F_NaN
   CON,FIELD=trans_sst_lintrend,X=trans_sst_longitude,Y=trans_sst_latitude,TITLE='Linear trend in surface temperature during first 32 years '+$
       'of HiGEM 2% year!U-1!N transient integration (to 2x CO2) - '+season_name+' - 5% sig only',/NOLINES,/BLOCK,CB_TITLE=cb_label

   ;FOR j=0,trans_sst_nlon-1 DO $
   ;   FOR k=0,trans_sst_nlat-1 DO $
   ;      IF ABS(trans_sst_lincorr(j,k)) gt 0.249 THEN $
   ;         GPLOT,X=trans_sst_longitude(j),Y=trans_sst_latitude(k),SYM=3,SIZE=20,/NOLINES
   AXES
   PSCLOSE,/NOVIEW;,/NOVIEW

   trans_sst_clim=fltarr(trans_sst_nlon,trans_sst_nlat)
   stbl_sst_clim=fltarr(trans_sst_nlon,trans_sst_nlat)
   FOR j=0,trans_sst_nlon-1 DO BEGIN
      FOR k=0,trans_sst_nlat-1 DO BEGIN
         trans_sst_clim(j,k)=MEAN(trans_sst(j,k,*))
         stbl_sst_clim(j,k)=MEAN(stbl_sst(j,k,*))
      ENDFOR
   ENDFOR
   
   ; Number of 30 year chunks in the HiGEM control integration
   n_chunks=5
   n_years_per_chunk=29
   diff_trans_control_sst_chunks=fltarr(trans_sst_nlon,trans_sst_nlat)
   ratio_trans_control_sst_chunks=fltarr(trans_sst_nlon,trans_sst_nlat)
   diff_stbl_control_sst_chunks=fltarr(trans_sst_nlon,trans_sst_nlat)
   ratio_stbl_control_sst_chunks=fltarr(trans_sst_nlon,trans_sst_nlat)
  
   FOR j=0,n_chunks-1 DO BEGIN
      control_sst_chunk=fltarr(control_sst_nlon,control_sst_nlat)
      FOR k=0,trans_sst_nlon-1 DO BEGIN
         FOR m=0,trans_sst_nlat-1 DO BEGIN
            control_sst_chunk(k,m)=MEAN(control_sst(k,m,j*n_years_per_chunk:(j+1)*n_years_per_chunk-1))
         ENDFOR
      ENDFOR

      diff_trans_control_sst_chunks=trans_sst_clim-control_sst_chunk      
      diff_stbl_control_sst_chunks=stbl_sst_clim-control_sst_chunk
      
      ;psfile='/home/ss901165/idl/queensland/higem/double_co2/uv850/qld_higem_dblco2_uv850_transstbl.control_chunk'+$
      ;       STRTRIM(STRING(j+1),1)+'.'+season_name+'_'+season_type+'.ps'
      ;PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=600,XOFFSET=500,YOFFSET=500,TFONT=2,$
      ;    TCHARSIZE=80,CB_WIDTH=110
      ;IF season_type eq 'amean' THEN BEGIN
      ;   CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_clim_annual)+1,white=[2]
      ;ENDIF ELSE $
      ;   CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_clim_season)+1,white=[2] 
      ;MAP,LONMIN=MIN(control_sst_longitude),LONMAX=MAX(control_sst_longitude),LATMIN=MIN(control_sst_latitude),$
      ;    LATMAX=MAX(control_sst_latitude),/hires
      ;IF season_type eq 'amean' THEN BEGIN
      ;   LEVS,MANUAL=mylevs_clim_annual
      ;   cb_label='m s!U-1!N'
      ;ENDIF ELSE BEGIN
      ;   LEVS,MANUAL=mylevs_clim_season
      ;   cb_label='m s!U-1!N'
      ;ENDELSE
      ;CON,FIELD=control_uv850mag_chunk,X=control_sst_longitude,Y=control_sst_latitude,$
      ;    TITLE='Clim 850 hPa wind for control (30 years, chunk '+$
      ;    STRTRIM(STRING(j+1),1)+') - '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label
      ;VECT,X=control_sst_longitude,Y=control_sst_latitude,U=control_sst_chunk,V=control_v850_chunk,MAG=5,STRIDE=3
      ;AXES
      ;PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/higem/double_co2/sst/qld_higem_dblco2_sst_transstbl.diff_trans-minus-control_chunk'+$
             STRTRIM(STRING(j+1),1)+'.'+season_name+'_'+season_type+'_'+region_name+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=600,XOFFSET=500,YOFFSET=500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110
      IF season_type eq 'amean' THEN BEGIN
         CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_diff_annual)+1,white=[6]
      ENDIF ELSE $
         CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_diff_season)+1,white=[6] 
      MAP,LONMIN=MIN(trans_sst_longitude),LONMAX=MAX(trans_sst_longitude),LATMIN=MIN(trans_sst_latitude),$
          LATMAX=MAX(trans_sst_latitude),/hires
      IF season_type eq 'amean' THEN BEGIN
         LEVS,MANUAL=mylevs_diff_annual
         cb_label='K'
      ENDIF ELSE BEGIN
         LEVS,MANUAL=mylevs_diff_season
         cb_label='K'
      ENDELSE
      CON,FIELD=diff_trans_control_sst_chunks,X=trans_sst_longitude,Y=trans_sst_latitude,$
          TITLE='Diff in surface temperature between 2% year!U-1!N CO2 transient (30 years) and control (30 years, chunk '+$
          STRTRIM(STRING(j+1),1)+') - '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/higem/double_co2/sst/qld_higem_dblco2_sst_transstbl.diff_stbl-minus-control_chunk'+$
             STRTRIM(STRING(j+1),1)+'.'+season_name+'_'+season_type+'_'+region_name+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=600,XOFFSET=500,YOFFSET=500,TFONT=2,$
             TCHARSIZE=80,CB_WIDTH=110
      IF season_type eq 'amean' THEN BEGIN
         CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_diff_annual)+1,white=[6]
      ENDIF ELSE $
         CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_diff_season)+1,white=[6] 
      MAP,LONMIN=MIN(trans_sst_longitude),LONMAX=MAX(trans_sst_longitude),LATMIN=MIN(trans_sst_latitude),$
          LATMAX=MAX(trans_sst_latitude),/hires
      IF season_type eq 'amean' THEN BEGIN
         LEVS,MANUAL=mylevs_diff_annual
         cb_label='K'
      ENDIF ELSE BEGIN
         LEVS,MANUAL=mylevs_diff_season
         cb_label='K'
      ENDELSE
      CON,FIELD=diff_stbl_control_sst_chunks,X=trans_sst_longitude,Y=trans_sst_latitude,$
          TITLE='Diff in surface temperature between 2x CO2 fixed  (30 years) and control (30 years, chunk '+$
          STRTRIM(STRING(j+1),1)+') - '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label      
      AXES
      PSCLOSE,/NOVIEW;,/NOVIEW

   ENDFOR
   
   FOR k=0,trans_sst_nlon-1 DO $
      FOR m=0,trans_sst_nlat-1 DO $
         control_sst_chunk(k,m)=MEAN(control_sst(k,m,*))
   
   diff_trans_control_sst_chunks=trans_sst_clim-control_sst_chunk
   diff_stbl_control_sst_chunks=stbl_sst_clim-control_sst_chunk

;   psfile='/home/ss901165/idl/queensland/higem/double_co2/sst/qld_higem_dblco2_sst_transstbl.control'+$
;          '.'+season_name+'_'+season_type+'_'+region_name+'.ps'
;   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=600,XOFFSET=500,YOFFSET=500,TFONT=2,$
;          TCHARSIZE=80,CB_WIDTH=110
;   IF season_type eq 'amean' THEN BEGIN
;      CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_clim_annual)+1,white=[2]
;   ENDIF ELSE $
;      CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_clim_season)+1,white=[2] 
;   MAP,LONMIN=MIN(control_sst_longitude),LONMAX=MAX(control_sst_longitude),LATMIN=MIN(control_sst_latitude),$
;       LATMAX=MAX(control_sst_latitude),/hires
;   IF season_type eq 'amean' THEN BEGIN
;      LEVS,MANUAL=mylevs_clim_annual
;      cb_label='m s!U-1!N'
;   ENDIF ELSE BEGIN
;      LEVS,MANUAL=mylevs_clim_season
;      cb_label='m s!U-1!N'
;   ENDELSE
;   CON,FIELD=control_sst_chunk,X=control_sst_longitude,Y=control_sst_latitude,$
;       TITLE='Clim 850 hPa wind for control (150 years) - '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label
;   VECT,X=control_sst_longitude,Y=control_sst_latitude,U=control_sst_chunk,V=control_v850_chunk,MAG=5,STRIDE=3
;   AXES
;   PSCLOSE,/NOVIEW
;
;   psfile='/home/ss901165/idl/queensland/higem/double_co2/sst/qld_higem_dblco2_uv850_transstbl.trans'+$
;          '.'+season_name+'_'+season_type+'_'+region_name+'.ps'
;   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=600,XOFFSET=500,YOFFSET=500,TFONT=2,$
;          TCHARSIZE=80,CB_WIDTH=110
;   IF season_type eq 'amean' THEN BEGIN
;      CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_clim_annual)+1,white=[2]
;   ENDIF ELSE $
;      CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_clim_season)+1,white=[2] 
;   MAP,LONMIN=MIN(trans_u850_longitude),LONMAX=MAX(trans_u850_longitude),LATMIN=MIN(trans_u850_latitude),$
;       LATMAX=MAX(trans_u850_latitude),/hires
;   IF season_type eq 'amean' THEN BEGIN
;      LEVS,MANUAL=mylevs_clim_annual
;      cb_label='m s!U-1!N'
;   ENDIF ELSE BEGIN
;      LEVS,MANUAL=mylevs_clim_season
;      cb_label='m s!U-1!N'
;   ENDELSE
;   CON,FIELD=trans_uv850mag_clim,X=trans_u850_longitude,Y=trans_u850_latitude,$
;       TITLE='Clim 850 hPa wind for 2% year!U-1!N CO2 transient (30 years) - '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label
;   VECT,X=trans_u850_longitude,Y=trans_u850_latitude,U=trans_u850_clim,V=trans_v850_clim,MAG=5,STRIDE=3
;   AXES
;   PSCLOSE,/NOVIEW
;
;   psfile='/home/ss901165/idl/queensland/higem/double_co2/uv850/qld_higem_dblco2_uv850_transstbl.stbl'+$
;          '.'+season_name+'_'+season_type+'_'+region_name+'.ps'
;   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=600,XOFFSET=500,YOFFSET=500,TFONT=2,$
;          TCHARSIZE=80,CB_WIDTH=110
;   IF season_type eq 'amean' THEN BEGIN
;      CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_clim_annual)+1,white=[2]
;   ENDIF ELSE $
;      CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_clim_season)+1,white=[2] 
;   MAP,LONMIN=MIN(trans_u850_longitude),LONMAX=MAX(trans_u850_longitude),LATMIN=MIN(trans_u850_latitude),$
;       LATMAX=MAX(trans_u850_latitude),/hires
;   IF season_type eq 'amean' THEN BEGIN
;      LEVS,MANUAL=mylevs_clim_annual
;      cb_label='m s!U-1!N'
;   ENDIF ELSE BEGIN
;      LEVS,MANUAL=mylevs_clim_season
;      cb_label='m s!U-1!N'
;   ENDELSE
;   CON,FIELD=stbl_uv850mag_clim,X=trans_u850_longitude,Y=trans_u850_latitude,$
;       TITLE='Clim 850 hPa wind for 2x fixed CO2 (30 years) - '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label
;   VECT,X=trans_u850_longitude,Y=trans_u850_latitude,U=stbl_u850_clim,V=stbl_v850_clim,MAG=5,STRIDE=3
;   AXES
;   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/queensland/higem/double_co2/sst/qld_higem_dblco2_sst_transstbl.diff_trans-minus-control.'+$
          season_name+'_'+season_type+'_'+region_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=600,XOFFSET=500,YOFFSET=500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110
   IF season_type eq 'amean' THEN BEGIN
      CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_diff_annual)+1,white=[6]
   ENDIF ELSE $
      CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_diff_season)+1,white=[6] 
   MAP,LONMIN=MIN(trans_sst_longitude),LONMAX=MAX(trans_sst_longitude),LATMIN=MIN(trans_sst_latitude),$
       LATMAX=MAX(trans_sst_latitude),/hires
   IF season_type eq 'amean' THEN BEGIN
      LEVS,MANUAL=mylevs_diff_annual
      cb_label='K'
   ENDIF ELSE BEGIN
      LEVS,MANUAL=mylevs_diff_season
      cb_label='K'
   ENDELSE
   CON,FIELD=diff_trans_control_sst_chunks,X=trans_sst_longitude,Y=trans_sst_latitude,$
       TITLE='Diff in surface temperature between 2% year!U-1!N CO2 transient (30 years) and control (150 years) - '+$
       season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label
   AXES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/sst/qld_higem_dblco2_sst_transstbl.diff_stbl-minus-control.'+$
          season_name+'_'+season_type+'_'+region_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=600,XOFFSET=500,YOFFSET=500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110
   IF season_type eq 'amean' THEN BEGIN
      CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_diff_annual)+1,white=[6]
   ENDIF ELSE $
      CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_diff_season)+1,white=[6] 
   MAP,LONMIN=MIN(trans_sst_longitude),LONMAX=MAX(trans_sst_longitude),LATMIN=MIN(trans_sst_latitude),$
       LATMAX=MAX(trans_sst_latitude),/hires
   IF season_type eq 'amean' THEN BEGIN
      LEVS,MANUAL=mylevs_diff_annual
      cb_label='K'
   ENDIF ELSE BEGIN
      LEVS,MANUAL=mylevs_diff_season
      cb_label='K'
   ENDELSE
   CON,FIELD=diff_stbl_control_sst_chunks,X=trans_sst_longitude,Y=trans_sst_latitude,$
       TITLE='Diff in surface temperature between 2x CO2 fixed (30 years) and control (150 years) - '+$
       season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label
   AXES
   PSCLOSE,/NOVIEW
   
ENDFOR



STOP
END

