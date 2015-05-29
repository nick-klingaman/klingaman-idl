PRO qld_higem_dblco2_precip_freq_transstbl_combined

; Directories
higem_control_indir='/home/ss901165/higem_qccce/es_control_eafeb'
higem_transstbl_indir='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu'

higem_control_nyears=149
higem_transstbl_nyears=51

mask_infile='/home/ss901165/um_output/mask_n144_higam.nc'

box=[-50,90,-1,180]
qldbox=[-32,137,-11,154]

; threshold for rainfall in mm/day
threshold=5

; multiplier for season_days for gridpoint to be considered 'valid' 
; (i.e., gridpoints with fewer than season_ndays*valid_mult days of rainfall above the threshold will be shaded white in plots)
valid_mult=0.05

;mylevs_lintrend_annual=['-13','-11','-9','-7','-5','-3','-1','1','3','5','7','9','11','13']
;mylevs_lintrend_season=['-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5']

mylevs_lintrend_freq_annual=['-0.78','-0.66','-0.54','-0.42','-0.30','-0.18','-0.06','0.06','0.18','0.30','0.42','0.54','0.66','0.78']
mylevs_lintrend_freq_season=['-0.52','-0.44','-0.36','-0.28','-0.20','-0.12','-0.04','0.04','0.12','0.20','0.28','0.36','0.44','0.52']

mylevs_lintrend_ints_annual=['-0.39','-0.33','-0.27','-0.21','-0.15','-0.09','-0.03','0.03','0.09','0.15','0.21','0.27','0.33','0.39']
mylevs_lintrend_ints_season=mylevs_lintrend_ints_annual

;mylevs_diff_season=['-156','-132','-108','-84','-60','-36','-12','12','36','60','84','108','132','156']
;mylevs_diff_annual=['-325','-275','-225','-175','-125','-75','-25','25','75','125','175','225','275','325']

mylevs_diff_freq_season=['-9.1','-7.7','-6.3','-4.9','-3.5','-2.1','-0.7','0.7','2.1','3.5','4.9','6.3','7.7','9.1']
mylevs_diff_freq_annual=['-26','-22','-18','-14','-10','-6','-2','2','6','10','14','18','22','26']

mylevs_ratio_freq_season=['0.48','0.56','0.64','0.72','0.80','0.88','0.96','1.04','1.13','1.25','1.39','1.56','1.79','2.08']
mylevs_ratio_freq_annual=mylevs_ratio_freq_season

mylevs_diff_ints_season=['-5.0','-4.2','-3.6','-2.8','-2.0','-1.2','-0.4','0.4','1.2','2.0','2.8','3.6','4.2','5.0']
mylevs_diff_ints_annual=mylevs_diff_ints_season

mylevs_ratio_ints_season=mylevs_ratio_freq_season
mylevs_ratio_ints_annual=mylevs_ratio_freq_annual

n_seasons=5
FOR i=0,n_seasons-1 DO BEGIN
   CASE i OF
      4 : BEGIN
         season_name='dec-feb'
         season_type='dmean'
         season_ndays=90.
      END
      1 : BEGIN
         season_name='mar-may'
         season_type='dmean'
         season_ndays=90.
      END
      2 : BEGIN
         season_name='jun-aug'
         season_type='dmean'
         season_ndays=90.
      END
      3 : BEGIN
         season_name='sep-nov'
         season_type='dmean'
         season_ndays=90.
      END
      0 : BEGIN
         season_name='may-apr'
         season_type='dmean'
         season_ndays=360.
      END
   ENDCASE
   print,season_name
   
   higem_control_infile=higem_control_indir+'/higem_eafeb.'+season_name+'_'+season_type+'s.h9-w8.precip.global_domain.nc'
   higem_transstbl_infile=higem_transstbl_indir+'/higem_eafee_eadwu.'+season_name+'_'+season_type+'s.m9-s0.precip.global_domain.nc'
   
   control_longitude=OPEN_AND_EXTRACT(higem_control_infile,'longitude')
   control_latitude=OPEN_AND_EXTRACT(higem_control_infile,'latitude')
   DEFINE_BOUNDARIES,box,control_latitude,control_longitude,control_box_tx,/LIMIT
   control_nlon=N_ELEMENTS(control_longitude)
   control_nlat=N_ELEMENTS(control_latitude)
   
   transstbl_longitude=OPEN_AND_EXTRACT(higem_transstbl_infile,'longitude')
   transstbl_latitude=OPEN_AND_EXTRACT(higem_transstbl_infile,'latitude')
   DEFINE_BOUNDARIES,box,transstbl_latitude,transstbl_longitude,transstbl_box_tx,/LIMIT
   transstbl_nlon=N_ELEMENTS(transstbl_longitude)
   transstbl_nlat=N_ELEMENTS(transstbl_latitude)

   control_precip=OPEN_AND_EXTRACT(higem_control_infile,'precip',$
                                   offset=[control_box_tx(1),control_box_tx(0),0,0],$
                                   count=[control_nlon,control_nlat,season_ndays,higem_control_nyears])*86400.
   IF TOTAL(where(control_precip ge 1e10)) ge 0 THEN $
      control_precip[where(control_precip ge 1e10)]=!Values.F_NaN
   
   transstbl_precip=OPEN_AND_EXTRACT(higem_transstbl_infile,'precip',$
                                 offset=[transstbl_box_tx(1),transstbl_box_tx(0),0,0],$
                                 count=[transstbl_nlon,transstbl_nlat,season_ndays,higem_transstbl_nyears])*86400.

   control_precip_freq=fltarr(control_nlon,control_nlat,higem_control_nyears)
   control_precip_ints=fltarr(control_nlon,control_nlat,higem_control_nyears)
   transstbl_precip_freq=fltarr(transstbl_nlon,transstbl_nlat,higem_transstbl_nyears)
   transstbl_precip_ints=fltarr(transstbl_nlon,transstbl_nlat,higem_transstbl_nyears)

   FOR j=0,control_nlon-1 DO BEGIN
      FOR k=0,control_nlat-1 DO BEGIN
         FOR m=0,higem_control_nyears-1 DO BEGIN
            thisyear_precip=REFORM(control_precip(j,k,*,m))
            IF TOTAL(where(thisyear_precip ge threshold)) ge 0 THEN BEGIN
               control_precip_freq(j,k,m)=N_ELEMENTS(where(thisyear_precip ge threshold))
               control_precip_ints(j,k,m)=MEAN(thisyear_precip[where(thisyear_precip ge threshold)])
            ENDIF ELSE BEGIN
               control_precip_freq(j,k,m)=0
               control_precip_ints(j,k,m)=!Values.F_NaN
            ENDELSE
         ENDFOR

         FOR m=0,higem_transstbl_nyears-1 DO BEGIN
            thisyear_precip=REFORM(transstbl_precip(j,k,*,m))
            IF TOTAL(where(thisyear_precip ge threshold)) ge 0 THEN BEGIN
               transstbl_precip_freq(j,k,m)=N_ELEMENTS(where(thisyear_precip ge threshold))
               transstbl_precip_ints(j,k,m)=MEAN(thisyear_precip[where(thisyear_precip ge threshold)])
            ENDIF ELSE BEGIN
               transstbl_precip_freq(j,k,m)=0
               transstbl_precip_ints(j,k,m)=!Values.F_NaN
            ENDELSE
         ENDFOR
;         IF MEAN(transstbl_precip_freq(j,k,*)) lt 9 THEN BEGIN
;            transstbl_precip_freq(j,k,*)=!Values.F_NaN
;            transstbl_precip_ints(j,k,*)=!Values.F_NaN
;         ENDIF
      ENDFOR
   ENDFOR
            
   transstbl_precip_freq_clim=fltarr(transstbl_nlon,transstbl_nlat)
   transstbl_precip_ints_clim=fltarr(transstbl_nlon,transstbl_nlat)
   FOR j=0,transstbl_nlon-1 DO BEGIN
      FOR k=0,transstbl_nlat-1 DO BEGIN
         transstbl_precip_freq_clim(j,k)=MEAN(transstbl_precip_freq(j,k,*),/NaN)
         transstbl_precip_ints_clim(j,k)=TOTAL(transstbl_precip_ints(j,k,*)*transstbl_precip_freq(j,k,*),/NaN)/TOTAL(transstbl_precip_freq(j,k,*))
      ENDFOR
   ENDFOR
   
   ; Number of 30 year chunks in the HiGEM control integration
   n_chunks=3
   n_years_per_chunk=49
   diff_transstbl_control_freq_chunks=fltarr(transstbl_nlon,transstbl_nlat)
   ratio_transstbl_control_freq_chunks=fltarr(transstbl_nlon,transstbl_nlat)
   
   diff_transstbl_control_ints_chunks=fltarr(transstbl_nlon,transstbl_nlat)
   ratio_transstbl_control_ints_chunks=fltarr(transstbl_nlon,transstbl_nlat)
   FOR j=0,n_chunks-1 DO BEGIN
      FOR k=0,transstbl_nlon-1 DO BEGIN
         FOR m=0,transstbl_nlat-1 DO BEGIN
            control_freq_mean=MEAN(control_precip_freq(k,m,j*n_years_per_chunk:(j+1)*n_years_per_chunk-1))
            control_ints_mean=TOTAL(control_precip_freq(k,m,j*n_years_per_chunk:(j+1)*n_years_per_chunk-1)*$
                                    control_precip_ints(k,m,j*n_years_per_chunk:(j+1)*n_years_per_chunk-1),/NaN)/$
                              TOTAL(control_precip_freq(k,m,j*n_years_per_chunk:(j+1)*n_years_per_chunk-1))            
            IF control_freq_mean ge season_ndays*valid_mult THEN BEGIN               
               diff_transstbl_control_freq_chunks(k,m)=transstbl_precip_freq_clim(k,m)-control_freq_mean
               ratio_transstbl_control_freq_chunks(k,m)=transstbl_precip_freq_clim(k,m)/control_freq_mean               
               diff_transstbl_control_ints_chunks(k,m)=transstbl_precip_ints_clim(k,m)-control_ints_mean
               ratio_transstbl_control_ints_chunks(k,m)=transstbl_precip_ints_clim(k,m)/control_ints_mean
            ENDIF ELSE BEGIN
               ratio_transstbl_control_freq_chunks(k,m)=!Values.F_NaN
               diff_transstbl_control_freq_chunks(k,m)=!Values.F_NaN
               diff_transstbl_control_ints_chunks(k,m)=!Values.F_NaN
               ratio_transstbl_control_ints_chunks(k,m)=!Values.F_NaN
            ENDELSE
         ENDFOR
      ENDFOR
         
      psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_freq_transstbl_combined.diff_freq_trans-minus-control_chunk'+$
             STRTRIM(STRING(j+1),1)+'.'+season_name+'_'+season_type+'.'+STRTRIM(STRING(threshold),1)+'mm_threshold.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
      IF season_name eq 'may-apr' THEN BEGIN
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_freq_annual)+1,/REV,white=[9]
      ENDIF ELSE $
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_freq_season)+1,/REV,white=[9] 
      MAP,LONMIN=MIN(transstbl_longitude),LONMAX=MAX(transstbl_longitude),LATMIN=MIN(transstbl_latitude),LATMAX=MAX(transstbl_latitude),/hires ;,/SET
      IF season_name eq 'may-apr' THEN BEGIN
         LEVS,MANUAL=mylevs_diff_freq_annual
         cb_label='days year!U-1!N'
      ENDIF ELSE BEGIN
         LEVS,MANUAL=mylevs_diff_freq_season
         cb_label='days season!U-1!N'
      ENDELSE
      CON,FIELD=diff_transstbl_control_freq_chunks,X=transstbl_longitude,Y=transstbl_latitude,$
          TITLE='Diff in freq of > '+STRTRIM(STRING(threshold),1)+' mm day!U-1!N rain between 2%+2x CO2 (50 years) and ctl (50 years, chunk '+$
          STRTRIM(STRING(j+1),1)+') - '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_freq_transstbl_combined.ratio_freq_trans-minus-control_chunk'+$
             STRTRIM(STRING(j+1),1)+'.'+season_name+'_'+season_type+'.'+STRTRIM(STRING(threshold),1)+'mm_threshold.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
      IF season_name eq 'may-apr' THEN BEGIN
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_freq_annual)+1,/REV;,white=[9]
      ENDIF ELSE $
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_freq_season)+1,/REV;,white=[9] 
      MAP,LONMIN=MIN(transstbl_longitude),LONMAX=MAX(transstbl_longitude),LATMIN=MIN(transstbl_latitude),LATMAX=MAX(transstbl_latitude),/hires ;,/SET
      IF season_name eq 'may-apr' THEN BEGIN
         LEVS,MANUAL=mylevs_ratio_freq_annual
      ENDIF ELSE $
         LEVS,MANUAL=mylevs_ratio_freq_season
      CON,FIELD=ratio_transstbl_control_freq_chunks,X=transstbl_longitude,Y=transstbl_latitude,$
          TITLE='Ratio of freq of > '+STRTRIM(STRING(threshold),1)+' mm day!U-1!N for 2%+2x CO2 (50 years) divided by ctl (50 years, chunk '+$
          STRTRIM(STRING(j+1),1)+') - '+season_name,/NOLINES,/BLOCK,CB_TITLE='Ratio (unitless)'
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_freq_transstbl_combined.diff_ints_trans-minus-control_chunk'+$
             STRTRIM(STRING(j+1),1)+'.'+season_name+'_'+season_type+'.'+STRTRIM(STRING(threshold),1)+'mm_threshold.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
      IF season_name eq 'may-apr' THEN BEGIN
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_ints_annual)+1,/REV;,white=[8]
      ENDIF ELSE $
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_ints_season)+1,/REV;,white=[8] 
      MAP,LONMIN=MIN(transstbl_longitude),LONMAX=MAX(transstbl_longitude),LATMIN=MIN(transstbl_latitude),LATMAX=MAX(transstbl_latitude),/hires ;,/SET
      IF season_name eq 'may-apr' THEN BEGIN
         LEVS,MANUAL=mylevs_diff_ints_annual
         cb_label='mm day!U-1!N'
      ENDIF ELSE BEGIN
         LEVS,MANUAL=mylevs_diff_ints_season
         cb_label='mm day!U-1!N'
      ENDELSE
      CON,FIELD=diff_transstbl_control_ints_chunks,X=transstbl_longitude,Y=transstbl_latitude,$
          TITLE='Diff in intensity of > '+STRTRIM(STRING(threshold),1)+' mm day!U-1!N rain between 2%+2x CO2 (50 years) and ctl (50 years, chunk '+$
          STRTRIM(STRING(j+1),1)+') - '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_freq_transstbl_combined.ratio_ints_trans-minus-control_chunk'+$
             STRTRIM(STRING(j+1),1)+'.'+season_name+'_'+season_type+'.'+STRTRIM(STRING(threshold),1)+'mm_threshold.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
      IF season_name eq 'may-apr' THEN BEGIN
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_ints_annual)+1,/REV;,white=[9]
      ENDIF ELSE $
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_ints_season)+1,/REV;,white=[9] 
      MAP,LONMIN=MIN(transstbl_longitude),LONMAX=MAX(transstbl_longitude),LATMIN=MIN(transstbl_latitude),LATMAX=MAX(transstbl_latitude),/hires ;,/SET
      IF season_name eq 'may-apr' THEN BEGIN
         LEVS,MANUAL=mylevs_ratio_ints_annual
      ENDIF ELSE $
         LEVS,MANUAL=mylevs_ratio_ints_season
      CON,FIELD=ratio_transstbl_control_ints_chunks,X=transstbl_longitude,Y=transstbl_latitude,$
          TITLE='Ratio of ints of > '+STRTRIM(STRING(threshold),1)+' mm day!U-1!N for 2%+2x CO2 (50 years) divided by ctl (50 years, chunk '+$
          STRTRIM(STRING(j+1),1)+') - '+season_name,/NOLINES,/BLOCK,CB_TITLE='Ratio (unitless)'
      AXES
      PSCLOSE,/NOVIEW

   ENDFOR

   FOR k=0,transstbl_nlon-1 DO BEGIN
      FOR m=0,transstbl_nlat-1 DO BEGIN
         control_freq_mean=MEAN(control_precip_freq(k,m,*))
         control_ints_mean=TOTAL(control_precip_freq(k,m,*)*$
                                 control_precip_ints(k,m,*),/NaN)/$
                           TOTAL(control_precip_freq(k,m,*),/NaN)

         IF control_freq_mean ge season_ndays*valid_mult THEN BEGIN            
            diff_transstbl_control_freq_chunks(k,m)=transstbl_precip_freq_clim(k,m)-control_freq_mean
            diff_transstbl_control_ints_chunks(k,m)=transstbl_precip_ints_clim(k,m)-control_ints_mean
            ratio_transstbl_control_freq_chunks(k,m)=transstbl_precip_freq_clim(k,m)/control_freq_mean            
            ratio_transstbl_control_ints_chunks(k,m)=transstbl_precip_ints_clim(k,m)/control_ints_mean
         ENDIF ELSE BEGIN
            ratio_transstbl_control_freq_chunks(k,m)=!Values.F_NaN            
            diff_transstbl_control_ints_chunks(k,m)=!Values.F_NaN
            diff_transstbl_control_freq_chunks(k,m)=!Values.F_NaN
            ratio_transstbl_control_ints_chunks(k,m)=!Values.F_NaN
         ENDELSE
      ENDFOR
   ENDFOR

   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_freq_transstbl_combined.diff_freq_trans-minus-control.'+$
          season_name+'_'+season_type+'.'+STRTRIM(STRING(threshold),1)+'mm_threshold.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
   IF season_name eq 'may-apr' THEN BEGIN
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_freq_annual)+1,/REV;,white=[9]
   ENDIF ELSE $
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_freq_season)+1,/REV;,white=[9] 
   MAP,LONMIN=MIN(transstbl_longitude),LONMAX=MAX(transstbl_longitude),LATMIN=MIN(transstbl_latitude),LATMAX=MAX(transstbl_latitude),/hires ;,/SET
   IF season_name eq 'may-apr' THEN BEGIN
      LEVS,MANUAL=mylevs_diff_freq_annual
      cb_label='days year!U-1!N'
   ENDIF ELSE BEGIN
      LEVS,MANUAL=mylevs_diff_freq_season
      cb_label='days season!U-1!N'
   ENDELSE
   CON,FIELD=diff_transstbl_control_freq_chunks,X=transstbl_longitude,Y=transstbl_latitude,$
       TITLE='Diff in freq of > '+STRTRIM(STRING(threshold),1)+' mm day!U-1!N for 2%+2x CO2 (50 years) minus ctl (150 years) '+$
       '- '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label                                
   AXES
   PSCLOSE,/NOVIEW   

   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_freq_transstbl_combined.ratio_freq_trans-minus-control.'+$
          season_name+'_'+season_type+'.'+STRTRIM(STRING(threshold),1)+'mm_threshold.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
   IF season_name eq 'may-apr' THEN BEGIN
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_freq_annual)+1,/REV;,white=[9]
   ENDIF ELSE $
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_freq_season)+1,/REV;,white=[9] 
   MAP,LONMIN=MIN(transstbl_longitude),LONMAX=MAX(transstbl_longitude),LATMIN=MIN(transstbl_latitude),LATMAX=MAX(transstbl_latitude),/hires ;,/SET
   IF season_name eq 'may-apr' THEN BEGIN
      LEVS,MANUAL=mylevs_ratio_freq_annual
   ENDIF ELSE $
      LEVS,MANUAL=mylevs_ratio_freq_season
   CON,FIELD=ratio_transstbl_control_freq_chunks,X=transstbl_longitude,Y=transstbl_latitude,$
       TITLE='Ratio in freq of > '+STRTRIM(STRING(threshold),1)+' mm day!U-1!N for 2%+2x CO2 (50 years) divded by ctl (150 years) '+$
       '- '+season_name,/NOLINES,/BLOCK,CB_TITLE='Ratio (unitless)'
   AXES
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_freq_transstbl_combined.diff_ints_trans-minus-control.'+$
          season_name+'_'+season_type+'.'+STRTRIM(STRING(threshold),1)+'mm_threshold.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
   IF season_name eq 'may-apr' THEN BEGIN
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_ints_annual)+1,/REV;,white=[8]
   ENDIF ELSE $
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_ints_season)+1,/REV;,white=[8] 
   MAP,LONMIN=MIN(transstbl_longitude),LONMAX=MAX(transstbl_longitude),LATMIN=MIN(transstbl_latitude),LATMAX=MAX(transstbl_latitude),/hires ;,/SET
   IF season_name eq 'may-apr' THEN BEGIN
      LEVS,MANUAL=mylevs_diff_ints_annual
      cb_label='mm day!U-1!N'
   ENDIF ELSE BEGIN
      LEVS,MANUAL=mylevs_diff_ints_season
      cb_label='mm day!U-1!N'
   ENDELSE
   CON,FIELD=diff_transstbl_control_ints_chunks,X=transstbl_longitude,Y=transstbl_latitude,$
       TITLE='Diff in ints of > '+STRTRIM(STRING(threshold),1)+' mm day!U-1!N for 2%+2x CO2 (50 years) minus ctl (150 years) '+$
       '- '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label                                
   AXES
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_freq_transstbl_combined.ratio_ints_trans-minus-control.'+$
          season_name+'_'+season_type+'.'+STRTRIM(STRING(threshold),1)+'mm_threshold.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
   IF season_name eq 'may-apr' THEN BEGIN
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_ints_annual)+1,/REV;,white=[9]
   ENDIF ELSE $
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_ints_season)+1,/REV;,white=[9] 
   MAP,LONMIN=MIN(transstbl_longitude),LONMAX=MAX(transstbl_longitude),LATMIN=MIN(transstbl_latitude),LATMAX=MAX(transstbl_latitude),/hires ;,/SET
   IF season_name eq 'may-apr' THEN BEGIN
      LEVS,MANUAL=mylevs_ratio_ints_annual
   ENDIF ELSE $
      LEVS,MANUAL=mylevs_ratio_ints_season
   CON,FIELD=ratio_transstbl_control_ints_chunks,X=transstbl_longitude,Y=transstbl_latitude,$
       TITLE='Ratio in ints of > '+STRTRIM(STRING(threshold),1)+' mm day!U-1!N for 2%+2x CO2 (50 years) divded by ctl (150 years) '+$
       '- '+season_name,/NOLINES,/BLOCK,CB_TITLE='Ratio (unitless)'
   AXES
   PSCLOSE,/NOVIEW
   
;   mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
;   mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
;   DEFINE_BOUNDARIES,qldbox,mask_latitude,mask_longitude,mask_qldbox_tx,/LIMIT
;   mask_nlon=N_ELEMENTS(mask_longitude)
;   mask_nlat=N_ELEMENTS(mask_latitude)
;
;   DEFINE_BOUNDARIES,qldbox,control_latitude,control_longitude,control_qldbox_tx
;   DEFINE_BOUNDARIES,qldbox,transstbl_latitude,transstbl_longitude,transstbl_qldbox_tx
;   
;   mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
;                                offset=[mask_qldbox_tx(1),mask_qldbox_tx(0),0,0],$
;                                count=[mask_nlon,mask_nlat,1,1]))
;   
;   control_qld_aavg=fltarr(higem_control_nyears)
;   transstbl_qld_aavg=fltarr(higem_transstbl_nyears)
;   stbl_qld_aavg=fltarr(higem_stbl_nyears)
;   FOR j=0,higem_control_nyears-1 DO BEGIN
;      thisyear_precip=REFORM(control_precip(control_qldbox_tx(1):control_qldbox_tx(3),control_qldbox_tx(0):control_qldbox_tx(2),j))
;      thisyear_precip[where(mask eq 0)]=!Values.F_NaN
;      control_qld_aavg(j)=MEAN(thisyear_precip,/NaN)      
;   ENDFOR
;   FOR j=0,higem_transstbl_nyears-1 DO BEGIN
;      thisyear_precip=REFORM(transstbl_precip(transstbl_qldbox_tx(1):transstbl_qldbox_tx(3),transstbl_qldbox_tx(0):transstbl_qldbox_tx(2),j))
;      thisyear_precip[where(mask eq 0)]=!Values.F_NaN
;      transstbl_qld_aavg(j)=MEAN(thisyear_precip,/NaN)     
;   ENDFOR
;   FOR j=0,higem_stbl_nyears-1 DO BEGIN
;      thisyear_precip=REFORM(stbl_precip(transstbl_qldbox_tx(1):transstbl_qldbox_tx(3),transstbl_qldbox_tx(0):transstbl_qldbox_tx(2),j))
;      thisyear_precip[where(mask eq 0)]=!Values.F_NaN
;      stbl_qld_aavg(j)=MEAN(thisyear_precip,/NaN)     
;   ENDFOR
;
;   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_freq_transstbl.qld_aavg_ts_'+$
;          season_name+'_'+season_type+'.'+STRTRIM(STRING(threshold),1)+'mm_threshold.ps'
;   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=200,XOFFSET=1000,YOFFSET=1500,TFONT=2,$
;          TCHARSIZE=80,SPACE3=200
;   GSET,XMIN=0,XMAX=higem_control_nyears,YMIN=ts_min,YMAX=ts_max,$
;        TITLE='Queensland area-average precipitation in HiGEM control, transient and stabilized simulations - '+season_name
;   GPLOT,X=indgen(higem_control_nyears),Y=control_qld_aavg*season_ndays,COL=FSC_COLOR('black'),STYLE=0,THICK=40
;   GPLOT,X=indgen(higem_transstbl_nyears)+transstbl_offset,Y=transstbl_qld_aavg*season_ndays,COL=FSC_COLOR('red'),STYLE=0,THICK=40
;   GPLOT,X=indgen(higem_stbl_nyears)+transstbl_offset+stbl_offset,Y=stbl_qld_aavg*season_ndays,COL=FSC_COLOR('blue'),STYLE=0,THICK=40
;   
;   temp=SMOOTH(control_qld_aavg*season_ndays,5)
;   GPLOT,X=indgen(higem_control_nyears-4)+3,Y=temp(2:higem_control_nyears-3),COL=FSC_COLOR('black'),STYLE=0,THICK=150
;   temp=SMOOTH(transstbl_qld_aavg*season_ndays,5)
;   GPLOT,X=indgen(higem_transstbl_nyears-4)+3+transstbl_offset,Y=temp(2:higem_transstbl_nyears-3),COL=FSC_COLOR('red'),STYLE=0,THICK=150
;   temp=SMOOTH(stbl_qld_aavg*season_ndays,5)
;   GPLOT,X=indgen(higem_stbl_nyears-4)+3+transstbl_offset+stbl_offset,Y=temp(2:higem_stbl_nyears-3),COL=FSC_COLOR('blue'),STYLE=0,THICK=150
;
;   GPLOT,X=[0,higem_control_nyears],Y=[MEAN(control_qld_aavg),MEAN(control_qld_aavg)]*season_ndays,STYLE=2
;   GPLOT,X=[0,higem_control_nyears],Y=[MEAN(control_qld_aavg)+STDDEV(control_qld_aavg),$
;                                       MEAN(control_qld_aavg)+STDDEV(control_qld_aavg)]*season_ndays,STYLE=1
;   GPLOT,X=[0,higem_control_nyears],Y=[MEAN(control_qld_aavg)-STDDEV(control_qld_aavg),$
;                                       MEAN(control_qld_aavg)-STDDEV(control_qld_aavg)]*season_ndays,STYLE=1
;
;   AXES,XSTEP=10,YSTEP=ts_step,YMINOR=ts_step/2.,XMINOR=2,XTITLE='Year of HiGEM integration',YTITLE='Total precipitation (mm) averaged over Queensland land points'
;   
;   GLEGEND,labels=['Five-year running mean','Yearly timeseries','HiGEM 2xCO2 fixed','HiGEM 2% CO2 year!U-1!N transient','HiGEM control'],$
;           COL=[FSC_COLOR('black'),FSC_COLOR('black'),FSC_COLOR('blue'),FSC_COLOR('red'),FSC_COLOR('black')],$
;           THICK=[150,40,100,100,100],LEGPOS=1
;                
;
;   PSCLOSE,/NOVIEW
ENDFOR

STOP
END

