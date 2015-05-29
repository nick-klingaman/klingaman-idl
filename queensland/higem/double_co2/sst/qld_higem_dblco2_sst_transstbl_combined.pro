PRO qld_higem_dblco2_sst_transstbl_combined

; Directories
higem_control_indir='/home/ss901165/higem_qccce/es_control_eafeb'
higem_stbl_indir='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu'

higem_control_nyears=149
higem_transstbl_nyears=31

mask_infile='/home/ss901165/um_output/mask_n144_higam.nc'

box=[-90,0,90,360]
qldbox=[-32,137,-11,154]
region_name='global'

;box=[-70,40,20,300]
;region_name='sind_spac'

mylevs_diff_season=['-6.0','-5.2','-4.4','-3.6','-2.8','-2.0','-1.2','-0.4','0.4','1.2','2.0','2.8','3.6','4.4','5.2','6.0']
;mylevs_diff_season=['-1.4','-1.0','-0.6','-0.2','0.2','0.6','1.0','1.4','1.8','2.2','2.6','3.0','3.4','3.8','4.2']
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

n_seasons=4
FOR i=0,n_seasons-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         season_name='dec-feb'
         season_type='smean'
      END
      1 : BEGIN
         season_name='mar-may'
         season_type='smean'
         season_ndays=90.
      END
      2 : BEGIN
         season_name='jun-aug'
         season_type='smean'
         season_ndays=90.
      END
      3 : BEGIN
         season_name='sep-nov'
         season_type='smean'
         season_ndays=90.
      END
;      4 : BEGIN
;         season_name='may-apr'
;         season_type='amean'
;         season_ndays=360.
;      END
   ENDCASE
   
   higem_control_sst_infile=higem_control_indir+'/higem_eafeb.'+season_name+'_'+season_type+'s.h9-w8.surf_temp.global_domain.nc'   
;   higem_transstbl_sst_infile=higem_stbl_indir+'/higem_eafee_eadwu.'+season_name+'_'+season_type+'s.m9-s0.surf_temp.global_domain.nc'
   higem_transstbl_sst_infile=higem_stbl_indir+'/higem_eadwu.'+season_name+'_'+season_type+'s.o2-r3.surf_temp.global_domain.nc'
   
   
   control_sst_longitude=OPEN_AND_EXTRACT(higem_control_sst_infile,'longitude')
   control_sst_latitude=OPEN_AND_EXTRACT(higem_control_sst_infile,'latitude')
   DEFINE_BOUNDARIES,box,control_sst_latitude,control_sst_longitude,control_sst_box_tx,/LIMIT
   control_sst_nlon=N_ELEMENTS(control_sst_longitude)
   control_sst_nlat=N_ELEMENTS(control_sst_latitude)
   
   transstbl_sst_longitude=OPEN_AND_EXTRACT(higem_transstbl_sst_infile,'longitude')
   transstbl_sst_latitude=OPEN_AND_EXTRACT(higem_transstbl_sst_infile,'latitude')
   DEFINE_BOUNDARIES,box,transstbl_sst_latitude,transstbl_sst_longitude,transstbl_sst_box_tx,/LIMIT
   transstbl_sst_nlon=N_ELEMENTS(transstbl_sst_longitude)
   transstbl_sst_nlat=N_ELEMENTS(transstbl_sst_latitude)

   control_sst=OPEN_AND_EXTRACT(higem_control_sst_infile,'temp',$
                                   offset=[control_sst_box_tx(1),control_sst_box_tx(0),0],$
                                   count=[control_sst_nlon,control_sst_nlat,higem_control_nyears])
   
   transstbl_sst=OPEN_AND_EXTRACT(higem_transstbl_sst_infile,'temp_2',$
                                 offset=[transstbl_sst_box_tx(1),transstbl_sst_box_tx(0),0],$
                                 count=[transstbl_sst_nlon,transstbl_sst_nlat,higem_transstbl_nyears])

   mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
   mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
   DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
   mask_nlon=N_ELEMENTS(mask_longitude)
   mask_nlat=N_ELEMENTS(mask_latitude)

   mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                         count=[mask_nlon,mask_nlat,1,1]))
   
   transstbl_sst_clim=fltarr(transstbl_sst_nlon,transstbl_sst_nlat)
   FOR j=0,transstbl_sst_nlon-1 DO $
      FOR k=0,transstbl_sst_nlat-1 DO $
         transstbl_sst_clim(j,k)=MEAN(transstbl_sst(j,k,*))   
 
   ; Number of 50 year chunks in the HiGEM control integration
   n_chunks=3
   n_years_per_chunk=49
   diff_trans_control_sst_chunks=fltarr(transstbl_sst_nlon,transstbl_sst_nlat)
   ratio_trans_control_sst_chunks=fltarr(transstbl_sst_nlon,transstbl_sst_nlat)
  
   FOR j=0,n_chunks-1 DO BEGIN
      control_sst_chunk=fltarr(control_sst_nlon,control_sst_nlat)
      FOR k=0,transstbl_sst_nlon-1 DO BEGIN
         FOR m=0,transstbl_sst_nlat-1 DO BEGIN
            control_sst_chunk(k,m)=MEAN(control_sst(k,m,j*n_years_per_chunk:(j+1)*n_years_per_chunk-1))
         ENDFOR
      ENDFOR

      diff_transstbl_control_sst_chunks=transstbl_sst_clim-control_sst_chunk      

      psfile='/home/ss901165/idl/queensland/higem/double_co2/sst/qld_higem_dblco2_sst_transstbl_combined.diff_transstbl-minus-control_chunk'+$
             STRTRIM(STRING(j+1),1)+'.'+season_name+'_'+season_type+'_'+region_name+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=600,XOFFSET=500,YOFFSET=500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110
      IF season_type eq 'amean' THEN BEGIN
         CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_diff_annual)+1,white=[6]
      ENDIF ELSE $
         CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_diff_season)+1,white=[6] 
      MAP,LONMIN=MIN(transstbl_sst_longitude),LONMAX=MAX(transstbl_sst_longitude),LATMIN=MIN(transstbl_sst_latitude),$
          LATMAX=MAX(transstbl_sst_latitude),/hires
      IF season_type eq 'amean' THEN BEGIN
         LEVS,MANUAL=mylevs_diff_annual
         cb_label='K'
      ENDIF ELSE BEGIN
         LEVS,MANUAL=mylevs_diff_season
         cb_label='K'
      ENDELSE
      CON,FIELD=diff_transstbl_control_sst_chunks,X=transstbl_sst_longitude,Y=transstbl_sst_latitude,$
          TITLE='Diff in surface temperature between HiGEM 2%+2x (50 years) and control (50 years, chunk '+$
          STRTRIM(STRING(j+1),1)+') - '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label
      AXES
      PSCLOSE,/NOVIEW
   ENDFOR
   
   FOR k=0,transstbl_sst_nlon-1 DO $
      FOR m=0,transstbl_sst_nlat-1 DO $
         control_sst_chunk(k,m)=MEAN(control_sst(k,m,*))
   
   weights=fltarr(transstbl_sst_nlon,transstbl_sst_nlat)
   FOR k=0,transstbl_sst_nlon-1 DO BEGIN
      FOR m=0,transstbl_sst_nlat-1 DO BEGIN
         weights(k,m)=ABS(COS(3.14159*transstbl_sst_latitude(m)/180.))
      ENDFOR
   ENDFOR
   weights=weights/TOTAL(weights)
   print,'Difference in area-average surface temperature for '+season_name+':',$
         TOTAL(transstbl_sst_clim*weights)-TOTAL(control_sst_chunk*weights)

   diff_transstbl_control_sst_chunks=transstbl_sst_clim-control_sst_chunk  
   
   psfile='/home/ss901165/idl/queensland/higem/double_co2/sst/qld_higem_dblco2_sst_transstbl_combined.diff_transstbl-minus-control.'+$
          season_name+'_'+season_type+'_'+region_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=600,XOFFSET=500,YOFFSET=500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110
   IF season_type eq 'amean' THEN BEGIN
      CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_diff_annual)+1,white=[6]
   ENDIF ELSE $
      CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_diff_season)+1,white=[6] 
   MAP,LONMIN=MIN(transstbl_sst_longitude),LONMAX=MAX(transstbl_sst_longitude),LATMIN=MIN(transstbl_sst_latitude),$
       LATMAX=MAX(transstbl_sst_latitude),/hires
   IF season_type eq 'amean' THEN BEGIN
      LEVS,MANUAL=mylevs_diff_annual
      cb_label='K'
   ENDIF ELSE BEGIN
      LEVS,MANUAL=mylevs_diff_season
      cb_label='K'
   ENDELSE
   CON,FIELD=diff_transstbl_control_sst_chunks,X=transstbl_sst_longitude,Y=transstbl_sst_latitude,$
       TITLE='Diff in surface temperature between 2%+2x CO2 (50 years) and control (150 years) - '+$
       season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label
   AXES
   PSCLOSE,/NOVIEW
   
ENDFOR



STOP
END

