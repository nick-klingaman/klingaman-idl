PRO qld_higem_dblco2_uv850_transstbl_combined

; Directories
higem_control_indir='/home/ss901165/higem_qccce/es_control_eafeb'
higem_transstbl_indir='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu'

higem_control_nyears=149
higem_transstbl_nyears=51

box=[-59,80,10,240]
qldbox=[-32,137,-11,154]

mylevs_lintrend_annual=['-0.055','-0.045','-0.035','-0.025','-0.015','-0.005','0.005','0.015','0.025','0.035','0.045','0.055']
mylevs_lintrend_season=mylevs_lintrend_annual

mylevs_diff_season=['-3.0','-2.6','-2.2','-1.8','-1.4','-1.0','-0.6','-0.2','0.2','0.6','1.0','1.4','1.8','2.2','2.6','3.0']
mylevs_diff_annual=['-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5']

mylevs_clim_season=['2','3','4','5','6','7','8','9','10','11','12','13','14']
mylevs_clim_annual=mylevs_clim_season

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
      4 : BEGIN
         season_name='may-apr'
         season_type='amean'
         season_ndays=360.
      END
   ENDCASE
   
   higem_control_u850_infile=higem_control_indir+'/higem_eafeb.'+season_name+'_'+season_type+'s.h9-w8.u850.mjo_domain.nc'
   higem_transstbl_u850_infile=higem_transstbl_indir+'/higem_eafee_eadwu.'+season_name+'_'+season_type+'s.m9-s0.u850.global_domain.nc'
   
   control_u850_longitude=OPEN_AND_EXTRACT(higem_control_u850_infile,'longitude_1')
   control_u850_latitude=OPEN_AND_EXTRACT(higem_control_u850_infile,'latitude_1')
   DEFINE_BOUNDARIES,box,control_u850_latitude,control_u850_longitude,control_u850_box_tx,/LIMIT
   control_u850_nlon=N_ELEMENTS(control_u850_longitude)
   control_u850_nlat=N_ELEMENTS(control_u850_latitude)
   
   transstbl_u850_longitude=OPEN_AND_EXTRACT(higem_transstbl_u850_infile,'longitude_1')
   transstbl_u850_latitude=OPEN_AND_EXTRACT(higem_transstbl_u850_infile,'latitude_1')
   DEFINE_BOUNDARIES,box,transstbl_u850_latitude,transstbl_u850_longitude,transstbl_u850_box_tx,/LIMIT
   transstbl_u850_nlon=N_ELEMENTS(transstbl_u850_longitude)
   transstbl_u850_nlat=N_ELEMENTS(transstbl_u850_latitude)

   control_u850=OPEN_AND_EXTRACT(higem_control_u850_infile,'u',$
                                   offset=[control_u850_box_tx(1),control_u850_box_tx(0),0],$
                                   count=[control_u850_nlon,control_u850_nlat,higem_control_nyears])
   
   transstbl_u850=OPEN_AND_EXTRACT(higem_transstbl_u850_infile,'u',$
                                 offset=[transstbl_u850_box_tx(1),transstbl_u850_box_tx(0),0],$
                                 count=[transstbl_u850_nlon,transstbl_u850_nlat,higem_transstbl_nyears])

   higem_control_v850_infile=higem_control_indir+'/higem_eafeb.'+season_name+'_'+season_type+'s.h9-w8.v850.pac_domain.nc'
   higem_transstbl_v850_infile=higem_transstbl_indir+'/higem_eafee_eadwu.'+season_name+'_'+season_type+'s.m9-s0.v850.global_domain.nc'
   
   control_v850_longitude=OPEN_AND_EXTRACT(higem_control_v850_infile,'longitude_1')
   control_v850_latitude=OPEN_AND_EXTRACT(higem_control_v850_infile,'latitude_1')
   DEFINE_BOUNDARIES,box,control_v850_latitude,control_v850_longitude,control_v850_box_tx,/LIMIT
   control_v850_nlon=N_ELEMENTS(control_v850_longitude)
   control_v850_nlat=N_ELEMENTS(control_v850_latitude)
   
   transstbl_v850_longitude=OPEN_AND_EXTRACT(higem_transstbl_v850_infile,'longitude_1')
   transstbl_v850_latitude=OPEN_AND_EXTRACT(higem_transstbl_v850_infile,'latitude_1')
   DEFINE_BOUNDARIES,box,transstbl_v850_latitude,transstbl_v850_longitude,transstbl_v850_box_tx,/LIMIT
   transstbl_v850_nlon=N_ELEMENTS(transstbl_v850_longitude)
   transstbl_v850_nlat=N_ELEMENTS(transstbl_v850_latitude)

   control_v850=OPEN_AND_EXTRACT(higem_control_v850_infile,'v',$
                                   offset=[control_v850_box_tx(1),control_v850_box_tx(0),0],$
                                   count=[control_v850_nlon,control_v850_nlat,higem_control_nyears])
   
   transstbl_v850=OPEN_AND_EXTRACT(higem_transstbl_v850_infile,'v',$
                                 offset=[transstbl_v850_box_tx(1),transstbl_v850_box_tx(0),0],$
                                 count=[transstbl_v850_nlon,transstbl_v850_nlat,higem_transstbl_nyears])

   control_uv850mag=fltarr(control_u850_nlon,control_u850_nlat,higem_control_nyears)
   transstbl_uv850mag=fltarr(transstbl_u850_nlon,transstbl_u850_nlat,higem_transstbl_nyears)
   FOR j=0,control_u850_nlon-1 DO $
      FOR k=0,control_u850_nlat-1 DO $
         control_uv850mag(j,k,*)=SQRT(control_u850(j,k,*)^2+control_v850(j,k,*)^2)
   FOR j=0,transstbl_u850_nlon-1 DO $
      FOR k=0,transstbl_u850_nlat-1 DO $
         transstbl_uv850mag(j,k,*)=SQRT(transstbl_u850(j,k,*)^2+transstbl_v850(j,k,*)^2)

   transstbl_u850_clim=fltarr(transstbl_u850_nlon,transstbl_u850_nlat)
   transstbl_v850_clim=fltarr(transstbl_u850_nlon,transstbl_u850_nlat)
   transstbl_uv850mag_clim=fltarr(transstbl_u850_nlon,transstbl_u850_nlat)
   FOR j=0,transstbl_u850_nlon-1 DO BEGIN
      FOR k=0,transstbl_u850_nlat-1 DO BEGIN
         transstbl_u850_clim(j,k)=MEAN(transstbl_u850(j,k,*))
         transstbl_v850_clim(j,k)=MEAN(transstbl_v850(j,k,*))
         transstbl_uv850mag_clim(j,k)=MEAN(transstbl_uv850mag(j,k,*))
      ENDFOR
   ENDFOR
   
   ; Number of 30 year chunks in the HiGEM control integration
   n_chunks=3
   n_years_per_chunk=49
   diff_transstbl_control_u850_chunks=fltarr(transstbl_u850_nlon,transstbl_u850_nlat)
   ratio_transstbl_control_u850_chunks=fltarr(transstbl_u850_nlon,transstbl_u850_nlat)
  
   diff_transstbl_control_v850_chunks=fltarr(transstbl_v850_nlon,transstbl_v850_nlat)
   ratio_transstbl_control_v850_chunks=fltarr(transstbl_v850_nlon,transstbl_v850_nlat)
  
   diff_transstbl_control_uv850mag_chunks=fltarr(transstbl_u850_nlon,transstbl_u850_nlat)
   ratio_transstbl_control_uv850mag_chunks=fltarr(transstbl_u850_nlon,transstbl_u850_nlat)
   
   FOR j=0,n_chunks-1 DO BEGIN
      control_u850_chunk=fltarr(control_u850_nlon,control_u850_nlat)
      control_v850_chunk=fltarr(control_v850_nlon,control_v850_nlat)
      control_uv850mag_chunk=fltarr(control_u850_nlon,control_u850_nlat)
      FOR k=0,transstbl_u850_nlon-1 DO BEGIN
         FOR m=0,transstbl_u850_nlat-1 DO BEGIN
            control_u850_chunk(k,m)=MEAN(control_u850(k,m,j*n_years_per_chunk:(j+1)*n_years_per_chunk-1))
            control_v850_chunk(k,m)=MEAN(control_v850(k,m,j*n_years_per_chunk:(j+1)*n_years_per_chunk-1))
            control_uv850mag_chunk(k,m)=MEAN(control_uv850mag(k,m,j*n_years_per_chunk:(j+1)*n_years_per_chunk-1))
         ENDFOR
      ENDFOR

      diff_transstbl_control_u850_chunks=transstbl_u850_clim-control_u850_chunk
      ratio_transstbl_control_u850_chunks=transstbl_u850_clim/control_u850_chunk
      
      diff_transstbl_control_v850_chunks=transstbl_v850_clim-control_v850_chunk
      ratio_transstbl_control_v850_chunks=transstbl_v850_clim/control_v850_chunk
      
      diff_transstbl_control_uv850mag_chunks=transstbl_uv850mag_clim-control_uv850mag_chunk
      ratio_transstbl_control_uv850mag_chunks=transstbl_uv850mag_clim/control_uv850mag_chunk
         
      psfile='/home/ss901165/idl/queensland/higem/double_co2/uv850/qld_higem_dblco2_uv850_transstbl_combined.control_chunk'+$
             STRTRIM(STRING(j+1),1)+'.'+season_name+'_'+season_type+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=600,XOFFSET=500,YOFFSET=500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110
      IF season_type eq 'amean' THEN BEGIN
         CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_clim_annual)+1,white=[2]
      ENDIF ELSE $
         CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_clim_season)+1,white=[2] 
      MAP,LONMIN=MIN(control_u850_longitude),LONMAX=MAX(control_u850_longitude),LATMIN=MIN(control_u850_latitude),$
          LATMAX=MAX(control_u850_latitude),/hires
      IF season_type eq 'amean' THEN BEGIN
         LEVS,MANUAL=mylevs_clim_annual
         cb_label='m s!U-1!N'
      ENDIF ELSE BEGIN
         LEVS,MANUAL=mylevs_clim_season
         cb_label='m s!U-1!N'
      ENDELSE
      CON,FIELD=control_uv850mag_chunk,X=control_u850_longitude,Y=control_u850_latitude,$
          TITLE='Clim 850 hPa wind for control (50 years, chunk '+$
          STRTRIM(STRING(j+1),1)+') - '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label
      VECT,X=control_u850_longitude,Y=control_u850_latitude,U=control_u850_chunk,V=control_v850_chunk,MAG=5,STRIDE=3
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/higem/double_co2/uv850/qld_higem_dblco2_uv850_transstbl_combined.diff_transstbl-minus-control_chunk'+$
             STRTRIM(STRING(j+1),1)+'.'+season_name+'_'+season_type+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=600,XOFFSET=500,YOFFSET=500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110
      IF season_type eq 'amean' THEN BEGIN
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_annual)+1,/REV,white=[10]
      ENDIF ELSE $
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_season)+1,/REV,white=[10] 
      MAP,LONMIN=MIN(transstbl_u850_longitude),LONMAX=MAX(transstbl_u850_longitude),LATMIN=MIN(transstbl_u850_latitude),$
          LATMAX=MAX(transstbl_u850_latitude),/hires
      IF season_type eq 'amean' THEN BEGIN
         LEVS,MANUAL=mylevs_diff_annual
         cb_label='m s!U-1!N'
      ENDIF ELSE BEGIN
         LEVS,MANUAL=mylevs_diff_season
         cb_label='m s!U-1!N'
      ENDELSE
      CON,FIELD=diff_transstbl_control_uv850mag_chunks,X=transstbl_u850_longitude,Y=transstbl_u850_latitude,$
          TITLE='Diff in 850 hPa wind between 2% year!U-1!N CO2 transient (30 years) and control (30 years, chunk '+$
          STRTRIM(STRING(j+1),1)+') - '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label
      VECT,X=transstbl_u850_longitude,Y=transstbl_u850_latitude,U=diff_transstbl_control_u850_chunks,V=diff_transstbl_control_v850_chunks,MAG=0.5,STRIDE=3
      AXES
      PSCLOSE,/NOVIEW
   ENDFOR
   
   FOR k=0,transstbl_u850_nlon-1 DO BEGIN
      FOR m=0,transstbl_u850_nlat-1 DO BEGIN         
         control_u850_chunk(k,m)=MEAN(control_u850(k,m,*))
         control_v850_chunk(k,m)=MEAN(control_v850(k,m,*))
         control_uv850mag_chunk(k,m)=MEAN(control_uv850mag(k,m,*))
      ENDFOR
   ENDFOR
   
   diff_transstbl_control_u850_chunks=transstbl_u850_clim-control_u850_chunk
   ratio_transstbl_control_u850_chunks=transstbl_u850_clim/control_u850_chunk
   
   diff_transstbl_control_v850_chunks=transstbl_v850_clim-control_v850_chunk
   ratio_transstbl_control_v850_chunks=transstbl_v850_clim/control_v850_chunk
   
   diff_transstbl_control_uv850mag_chunks=transstbl_uv850mag_clim-control_uv850mag_chunk
   ratio_transstbl_control_uv850mag_chunks=transstbl_uv850mag_clim/control_uv850mag_chunk

   psfile='/home/ss901165/idl/queensland/higem/double_co2/uv850/qld_higem_dblco2_uv850_transstbl_combined.control'+$
          '.'+season_name+'_'+season_type+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=600,XOFFSET=500,YOFFSET=500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110
   IF season_type eq 'amean' THEN BEGIN
      CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_clim_annual)+1,white=[2]
   ENDIF ELSE $
      CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_clim_season)+1,white=[2] 
   MAP,LONMIN=MIN(control_u850_longitude),LONMAX=MAX(control_u850_longitude),LATMIN=MIN(control_u850_latitude),$
       LATMAX=MAX(control_u850_latitude),/hires
   IF season_type eq 'amean' THEN BEGIN
      LEVS,MANUAL=mylevs_clim_annual
      cb_label='m s!U-1!N'
   ENDIF ELSE BEGIN
      LEVS,MANUAL=mylevs_clim_season
      cb_label='m s!U-1!N'
   ENDELSE
   CON,FIELD=control_uv850mag_chunk,X=control_u850_longitude,Y=control_u850_latitude,$
       TITLE='Clim 850 hPa wind for control (150 years) - '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label
   VECT,X=control_u850_longitude,Y=control_u850_latitude,U=control_u850_chunk,V=control_v850_chunk,MAG=5,STRIDE=3
   AXES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/uv850/qld_higem_dblco2_uv850_transstbl_combined.transstbl'+$
          '.'+season_name+'_'+season_type+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=600,XOFFSET=500,YOFFSET=500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110
   IF season_type eq 'amean' THEN BEGIN
      CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_clim_annual)+1,white=[2]
   ENDIF ELSE $
      CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_clim_season)+1,white=[2] 
   MAP,LONMIN=MIN(transstbl_u850_longitude),LONMAX=MAX(transstbl_u850_longitude),LATMIN=MIN(transstbl_u850_latitude),$
       LATMAX=MAX(transstbl_u850_latitude),/hires
   IF season_type eq 'amean' THEN BEGIN
      LEVS,MANUAL=mylevs_clim_annual
      cb_label='m s!U-1!N'
   ENDIF ELSE BEGIN
      LEVS,MANUAL=mylevs_clim_season
      cb_label='m s!U-1!N'
   ENDELSE
   CON,FIELD=transstbl_uv850mag_clim,X=transstbl_u850_longitude,Y=transstbl_u850_latitude,$
       TITLE='Clim 850 hPa wind for HiGEM 2%+2x CO2 (50 years) - '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label
   VECT,X=transstbl_u850_longitude,Y=transstbl_u850_latitude,U=transstbl_u850_clim,V=transstbl_v850_clim,MAG=5,STRIDE=3
   AXES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/uv850/qld_higem_dblco2_uv850_transstbl_combined.diff_transstbl-minus-control.'+$
          season_name+'_'+season_type+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=600,XOFFSET=500,YOFFSET=500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110
   IF season_type eq 'amean' THEN BEGIN
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_annual)+1,/REV,white=[10]
   ENDIF ELSE $
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_season)+1,/REV,white=[10] 
   MAP,LONMIN=MIN(transstbl_u850_longitude),LONMAX=MAX(transstbl_u850_longitude),LATMIN=MIN(transstbl_u850_latitude),$
       LATMAX=MAX(transstbl_u850_latitude),/hires
   IF season_type eq 'amean' THEN BEGIN
      LEVS,MANUAL=mylevs_diff_annual
      cb_label='m s!U-1!N'
   ENDIF ELSE BEGIN
      LEVS,MANUAL=mylevs_diff_season
      cb_label='m s!U-1!N'
   ENDELSE
   CON,FIELD=diff_transstbl_control_uv850mag_chunks,X=transstbl_u850_longitude,Y=transstbl_u850_latitude,$
       TITLE='Diff in 850 hPa wind between 2%+2x CO2 (50 years) and control (150 years) - '+$
       season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label
   VECT,X=transstbl_u850_longitude,Y=transstbl_u850_latitude,U=diff_transstbl_control_u850_chunks,V=diff_transstbl_control_v850_chunks,MAG=0.5,STRIDE=3
   AXES
   PSCLOSE,/NOVIEW

ENDFOR



STOP
END

