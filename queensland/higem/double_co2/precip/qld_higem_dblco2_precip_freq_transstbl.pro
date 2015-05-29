PRO qld_higem_dblco2_precip_freq_transstbl

; Directories
higem_control_indir='/home/ss901165/higem_qccce/es_control_eafeb'
higem_trans_indir='/home/ss901165/higem_qccce/es_2pctco2_eafee'
higem_stbl_indir='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu'

higem_control_nyears=149
higem_trans_nyears=32
trans_offset=20
trans_stop_offset=31
higem_stbl_nyears=31
stbl_offset=33

mask_infile='/home/ss901165/um_output/mask_n144_higam.nc'

box=[-50,90,-1,180]
qldbox=[-32,137,-11,154]

; threshold for rainfall in mm/day
threshold=1

; multiplier for season_days for gridpoint to be considered 'valid' 
; (i.e., gridpoints with fewer than season_ndays*valid_mult days of rainfall above the threshold will be shaded white in plots)
valid_mult=0.10

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

n_seasons=1
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
   higem_trans_infile=higem_trans_indir+'/higem_eafee.'+season_name+'_'+season_type+'s.k9-u7.precip.global_domain.nc'
   higem_stbl_infile=higem_stbl_indir+'/higem_eadwu.'+season_name+'_'+season_type+'s.o2-r3.precip.global_domain.nc'
   
   control_longitude=OPEN_AND_EXTRACT(higem_control_infile,'longitude')
   control_latitude=OPEN_AND_EXTRACT(higem_control_infile,'latitude')
   DEFINE_BOUNDARIES,box,control_latitude,control_longitude,control_box_tx,/LIMIT
   control_nlon=N_ELEMENTS(control_longitude)
   control_nlat=N_ELEMENTS(control_latitude)
   
   trans_longitude=OPEN_AND_EXTRACT(higem_trans_infile,'longitude')
   trans_latitude=OPEN_AND_EXTRACT(higem_trans_infile,'latitude')
   DEFINE_BOUNDARIES,box,trans_latitude,trans_longitude,trans_box_tx,/LIMIT
   trans_nlon=N_ELEMENTS(trans_longitude)
   trans_nlat=N_ELEMENTS(trans_latitude)

   control_precip=OPEN_AND_EXTRACT(higem_control_infile,'precip',$
                                   offset=[control_box_tx(1),control_box_tx(0),0,0],$
                                   count=[control_nlon,control_nlat,season_ndays,higem_control_nyears])*86400.
   IF TOTAL(where(control_precip ge 1e10)) ge 0 THEN $
      control_precip[where(control_precip ge 1e10)]=!Values.F_NaN
   
   trans_precip=OPEN_AND_EXTRACT(higem_trans_infile,'precip',$
                                 offset=[trans_box_tx(1),trans_box_tx(0),0,0],$
                                 count=[trans_nlon,trans_nlat,season_ndays,higem_trans_nyears])*86400.

   stbl_precip=OPEN_AND_EXTRACT(higem_stbl_infile,'precip',$
                                offset=[trans_box_tx(1),trans_box_tx(0),0,0],$
                                count=[trans_nlon,trans_nlat,season_ndays,higem_stbl_nyears])*86400.

   control_precip_freq=fltarr(control_nlon,control_nlat,higem_control_nyears)
   control_precip_ints=fltarr(control_nlon,control_nlat,higem_control_nyears)
   trans_precip_freq=fltarr(trans_nlon,trans_nlat,higem_trans_nyears)
   trans_precip_ints=fltarr(trans_nlon,trans_nlat,higem_trans_nyears)
   stbl_precip_freq=fltarr(trans_nlon,trans_nlat,higem_stbl_nyears)
   stbl_precip_ints=fltarr(trans_nlon,trans_nlat,higem_stbl_nyears)

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

         FOR m=0,higem_trans_nyears-1 DO BEGIN
            thisyear_precip=REFORM(trans_precip(j,k,*,m))
            IF TOTAL(where(thisyear_precip ge threshold)) ge 0 THEN BEGIN
               trans_precip_freq(j,k,m)=N_ELEMENTS(where(thisyear_precip ge threshold))
               trans_precip_ints(j,k,m)=MEAN(thisyear_precip[where(thisyear_precip ge threshold)])
            ENDIF ELSE BEGIN
               trans_precip_freq(j,k,m)=0
               trans_precip_ints(j,k,m)=!Values.F_NaN
            ENDELSE
         ENDFOR
;         IF MEAN(trans_precip_freq(j,k,*)) lt 9 THEN BEGIN
;            trans_precip_freq(j,k,*)=!Values.F_NaN
;            trans_precip_ints(j,k,*)=!Values.F_NaN
;         ENDIF

         FOR m=0,higem_stbl_nyears-1 DO BEGIN
            thisyear_precip=REFORM(stbl_precip(j,k,*,m))
            IF TOTAL(where(thisyear_precip ge threshold)) ge 0 THEN BEGIN
               stbl_precip_freq(j,k,m)=N_ELEMENTS(where(thisyear_precip ge threshold))
               stbl_precip_ints(j,k,m)=MEAN(thisyear_precip[where(thisyear_precip ge threshold)])
            ENDIF ELSE BEGIN
               stbl_precip_freq(j,k,m)=0
               stbl_precip_ints(j,k,m)=!Values.F_NaN
            ENDELSE
         ENDFOR
      ENDFOR
   ENDFOR
            
   trans_precip_freq_lintrend=fltarr(trans_nlon,trans_nlat)
   trans_precip_ints_lintrend=fltarr(trans_nlon,trans_nlat)
   trans_precip_freq_lincorr=fltarr(trans_nlon,trans_nlat)
   trans_precip_ints_lincorr=fltarr(trans_nlon,trans_nlat)
   FOR j=0,trans_nlon-1 DO BEGIN
      FOR k=0,trans_nlat-1 DO BEGIN
         IF MEAN(trans_precip_freq(j,k,*)) ge season_ndays*valid_mult THEN BEGIN
            trans_precip_freq_thispt=REFORM(trans_precip_freq(j,k,*))
            IF TOTAL(where(FINITE(trans_precip_freq_thispt) eq 0)) ge 0 and N_ELEMENTS(where(FINITE(trans_precip_freq_thispt) eq 1)) gt 1 THEN BEGIN
               trans_precip_freq_thispt=trans_precip_freq_thispt[where(FINITE(trans_precip_freq_thispt) eq 1)]
               regression_x=where(FINITE(trans_precip_freq_thispt) eq 1)
            ENDIF ELSE $
               regression_x=indgen(higem_trans_nyears)
            trans_precip_freq_lintrend(j,k)=REGRESS(regression_x,REFORM(trans_precip_freq_thispt),CORRELATION=temp)
            trans_precip_freq_lincorr(j,k)=temp
            
            trans_precip_ints_thispt=REFORM(trans_precip_ints(j,k,*))
            IF TOTAL(where(FINITE(trans_precip_ints_thispt) eq 0)) ge 0 and N_ELEMENTS(where(FINITE(trans_precip_ints_thispt) eq 1)) gt 1 THEN BEGIN
               trans_precip_ints_thispt=trans_precip_ints_thispt[where(FINITE(trans_precip_ints_thispt) eq 1)]
               regression_x=where(FINITE(trans_precip_ints_thispt) eq 1)
            ENDIF ELSE $
               regression_x=indgen(higem_trans_nyears)
            trans_precip_ints_lintrend(j,k)=REGRESS(regression_x,REFORM(trans_precip_ints_thispt),CORRELATION=temp)
            trans_precip_ints_lincorr(j,k)=temp
         ENDIF ELSE BEGIN
            trans_precip_freq_lintrend(j,k)=!Values.F_NaN
            trans_precip_ints_lintrend(j,k)=!Values.F_NaN
            trans_precip_freq_lincorr(j,k)=!Values.F_NaN
            trans_precip_ints_lincorr(j,k)=!Values.F_NaN
         ENDELSE
      ENDFOR
   ENDFOR
   
   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_freq_transstbl.trans_freq_lintrend.'+season_name+'_'+$
          season_type+'.'+STRTRIM(STRING(threshold),1)+'mm_threshold.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
   IF season_name eq 'may-apr' THEN BEGIN
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_lintrend_freq_annual)+1,/REV;,white=[9]
   ENDIF ELSE $
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_lintrend_freq_season)+1,/REV;,white=[9] 
   MAP,LONMIN=MIN(trans_longitude),LONMAX=MAX(trans_longitude),LATMIN=MIN(trans_latitude),LATMAX=MAX(trans_latitude),/hires ;,/SET
   IF season_name eq 'may-apr' THEN BEGIN
      LEVS,MANUAL=mylevs_lintrend_freq_annual
      cb_label='days year!U-1!N year!U-1!N'
   ENDIF ELSE BEGIN
      LEVS,MANUAL=mylevs_lintrend_freq_season
      cb_label='days season!U-1!N year!U-1!N'
   ENDELSE
   CON,FIELD=trans_precip_freq_lintrend,X=trans_longitude,Y=trans_latitude,TITLE='Linear trend in number of days > '+STRTRIM(STRING(threshold),1)+' mm day!U-1!N '+$
       'rainfall during first 32 years of HiGEM 2% year!U-1!N transient integration (to 2x CO2) - '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label
   FOR j=0,trans_nlon-1 DO $
      FOR k=0,trans_nlat-1 DO $
         IF ABS(trans_precip_freq_lincorr(j,k)) gt 0.249 THEN $
            GPLOT,X=trans_longitude(j),Y=trans_latitude(k),SYM=3,SIZE=40,/NOLINES   
   AXES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_freq_transstbl.trans_ints_lintrend.'+season_name+'_'+$
          season_type+'.'+STRTRIM(STRING(threshold),1)+'mm_threshold.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
   IF season_name eq 'may-apr' THEN BEGIN
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_lintrend_ints_annual)+1,/REV;,white=[9]
   ENDIF ELSE $
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_lintrend_ints_season)+1,/REV;,white=[9] 
   MAP,LONMIN=MIN(trans_longitude),LONMAX=MAX(trans_longitude),LATMIN=MIN(trans_latitude),LATMAX=MAX(trans_latitude),/hires ;,/SET
   IF season_name eq 'may-apr' THEN BEGIN
      LEVS,MANUAL=mylevs_lintrend_ints_annual
      cb_label='mm day!U-1!N year!U-1!N'
   ENDIF ELSE BEGIN
      LEVS,MANUAL=mylevs_lintrend_ints_season
      cb_label='mm day!U-1!N season!U-1!N'
   ENDELSE
   CON,FIELD=trans_precip_ints_lintrend,X=trans_longitude,Y=trans_latitude,TITLE='Linear trend in intensity on days with > '+STRTRIM(STRING(threshold),1)+' mm day!U-1!N '+$
       'rainfall during first 32 years of HiGEM 2% year!U-1!N transient integration (to 2x CO2) - '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label
   FOR j=0,trans_nlon-1 DO $
      FOR k=0,trans_nlat-1 DO $
         IF ABS(trans_precip_ints_lincorr(j,k)) gt 0.249 THEN $
            GPLOT,X=trans_longitude(j),Y=trans_latitude(k),SYM=3,SIZE=40,/NOLINES   
   AXES
   PSCLOSE,/NOVIEW
      
   trans_precip_freq_clim=fltarr(trans_nlon,trans_nlat)
   trans_precip_ints_clim=fltarr(trans_nlon,trans_nlat)
   stbl_precip_freq_clim=fltarr(trans_nlon,trans_nlat)
   stbl_precip_ints_clim=fltarr(trans_nlon,trans_nlat)
   FOR j=0,trans_nlon-1 DO BEGIN
      FOR k=0,trans_nlat-1 DO BEGIN
         trans_precip_freq_clim(j,k)=MEAN(trans_precip_freq(j,k,*),/NaN)
         stbl_precip_freq_clim(j,k)=MEAN(stbl_precip_freq(j,k,*),/NaN)
         trans_precip_ints_clim(j,k)=TOTAL(trans_precip_ints(j,k,*)*trans_precip_freq(j,k,*),/NaN)/TOTAL(trans_precip_freq(j,k,*))
         stbl_precip_ints_clim(j,k)=TOTAL(stbl_precip_ints(j,k,*)*stbl_precip_freq(j,k,*),/NaN)/TOTAL(stbl_precip_freq(j,k,*))
      ENDFOR
   ENDFOR
   
   ; Number of 30 year chunks in the HiGEM control integration
   n_chunks=5
   n_years_per_chunk=29
   diff_trans_control_freq_chunks=fltarr(trans_nlon,trans_nlat)
   ratio_trans_control_freq_chunks=fltarr(trans_nlon,trans_nlat)
   diff_stbl_control_freq_chunks=fltarr(trans_nlon,trans_nlat)
   ratio_stbl_control_freq_chunks=fltarr(trans_nlon,trans_nlat)
   
   diff_trans_control_ints_chunks=fltarr(trans_nlon,trans_nlat)
   ratio_trans_control_ints_chunks=fltarr(trans_nlon,trans_nlat)
   diff_stbl_control_ints_chunks=fltarr(trans_nlon,trans_nlat)
   ratio_stbl_control_ints_chunks=fltarr(trans_nlon,trans_nlat)
   FOR j=0,n_chunks-1 DO BEGIN
      FOR k=0,trans_nlon-1 DO BEGIN
         FOR m=0,trans_nlat-1 DO BEGIN
            control_freq_mean=MEAN(control_precip_freq(k,m,j*n_years_per_chunk:(j+1)*n_years_per_chunk-1))
            control_ints_mean=TOTAL(control_precip_freq(k,m,j*n_years_per_chunk:(j+1)*n_years_per_chunk-1)*$
                                    control_precip_ints(k,m,j*n_years_per_chunk:(j+1)*n_years_per_chunk-1),/NaN)/$
                              TOTAL(control_precip_freq(k,m,j*n_years_per_chunk:(j+1)*n_years_per_chunk-1))
            
            diff_trans_control_freq_chunks(k,m)=trans_precip_freq_clim(k,m)-control_freq_mean
            diff_stbl_control_freq_chunks(k,m)=stbl_precip_freq_clim(k,m)-control_freq_mean

            IF control_freq_mean ge season_ndays*valid_mult THEN BEGIN
               ratio_trans_control_freq_chunks(k,m)=trans_precip_freq_clim(k,m)/control_freq_mean
               ratio_stbl_control_freq_chunks(k,m)=stbl_precip_freq_clim(k,m)/control_freq_mean
               
               diff_trans_control_ints_chunks(k,m)=trans_precip_ints_clim(k,m)-control_ints_mean
               ratio_trans_control_ints_chunks(k,m)=trans_precip_ints_clim(k,m)/control_ints_mean
               diff_stbl_control_ints_chunks(k,m)=stbl_precip_ints_clim(k,m)-control_ints_mean
               ratio_stbl_control_ints_chunks(k,m)=stbl_precip_ints_clim(k,m)/control_ints_mean            
            ENDIF ELSE BEGIN
               ratio_trans_control_freq_chunks(k,m)=!Values.F_NaN
               ratio_stbl_control_freq_chunks(k,m)=!Values.F_NaN
               
               diff_trans_control_ints_chunks(k,m)=!Values.F_NaN
               ratio_trans_control_ints_chunks(k,m)=!Values.F_NaN
               diff_stbl_control_ints_chunks(k,m)=!Values.F_NaN
               ratio_stbl_control_ints_chunks(k,m)=!Values.F_NaN
            ENDELSE
         ENDFOR
      ENDFOR
         
      psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_freq_transstbl.diff_freq_trans-minus-control_chunk'+$
             STRTRIM(STRING(j+1),1)+'.'+season_name+'_'+season_type+'.'+STRTRIM(STRING(threshold),1)+'mm_threshold.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
      IF season_name eq 'may-apr' THEN BEGIN
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_freq_annual)+1,/REV,white=[9]
      ENDIF ELSE $
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_freq_season)+1,/REV,white=[9] 
      MAP,LONMIN=MIN(trans_longitude),LONMAX=MAX(trans_longitude),LATMIN=MIN(trans_latitude),LATMAX=MAX(trans_latitude),/hires ;,/SET
      IF season_name eq 'may-apr' THEN BEGIN
         LEVS,MANUAL=mylevs_diff_freq_annual
         cb_label='days year!U-1!N'
      ENDIF ELSE BEGIN
         LEVS,MANUAL=mylevs_diff_freq_season
         cb_label='days season!U-1!N'
      ENDELSE
      CON,FIELD=diff_trans_control_freq_chunks,X=trans_longitude,Y=trans_latitude,$
          TITLE='Diff in freq of > '+STRTRIM(STRING(threshold),1)+' mm day!U-1!N rain between 2% year!U-1!N CO2 trans (30 years) and ctl (30 years, chunk '+$
          STRTRIM(STRING(j+1),1)+') - '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_freq_transstbl.diff_freq_stbl-minus-control_chunk'+$
             STRTRIM(STRING(j+1),1)+'.'+season_name+'_'+season_type+'.'+STRTRIM(STRING(threshold),1)+'mm_threshold.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
      IF season_name eq 'may-apr' THEN BEGIN
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_freq_annual)+1,/REV,white=[9]
      ENDIF ELSE $
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_freq_season)+1,/REV,white=[9] 
      MAP,LONMIN=MIN(trans_longitude),LONMAX=MAX(trans_longitude),LATMIN=MIN(trans_latitude),LATMAX=MAX(trans_latitude),/hires ;,/SET
      IF season_name eq 'may-apr' THEN BEGIN
         LEVS,MANUAL=mylevs_diff_freq_annual
         cb_label='days year!U-1!N'
      ENDIF ELSE BEGIN
         LEVS,MANUAL=mylevs_diff_freq_season
         cb_label='days season!U-1!N'
      ENDELSE
      CON,FIELD=diff_stbl_control_freq_chunks,X=trans_longitude,Y=trans_latitude,$
          TITLE='Diff in freq of > '+STRTRIM(STRING(threshold),1)+' mm day!U-1!N rain between 2x CO2 (30 years) and ctl (30 years, chunk '+$
          STRTRIM(STRING(j+1),1)+') - '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_freq_transstbl.ratio_freq_trans-minus-control_chunk'+$
             STRTRIM(STRING(j+1),1)+'.'+season_name+'_'+season_type+'.'+STRTRIM(STRING(threshold),1)+'mm_threshold.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
      IF season_name eq 'may-apr' THEN BEGIN
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_freq_annual)+1,/REV;,white=[9]
      ENDIF ELSE $
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_freq_season)+1,/REV;,white=[9] 
      MAP,LONMIN=MIN(trans_longitude),LONMAX=MAX(trans_longitude),LATMIN=MIN(trans_latitude),LATMAX=MAX(trans_latitude),/hires ;,/SET
      IF season_name eq 'may-apr' THEN BEGIN
         LEVS,MANUAL=mylevs_ratio_freq_annual
      ENDIF ELSE $
         LEVS,MANUAL=mylevs_ratio_freq_season
      CON,FIELD=ratio_trans_control_freq_chunks,X=trans_longitude,Y=trans_latitude,$
          TITLE='Ratio of freq of > '+STRTRIM(STRING(threshold),1)+' mm day!U-1!N for 2% year!U-1!N CO2 trans (30 years) divided by ctl (30 years, chunk '+$
          STRTRIM(STRING(j+1),1)+') - '+season_name,/NOLINES,/BLOCK,CB_TITLE='Ratio (unitless)'
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_freq_transstbl.ratio_freq_stbl-minus-control_chunk'+$
             STRTRIM(STRING(j+1),1)+'.'+season_name+'_'+season_type+'.'+STRTRIM(STRING(threshold),1)+'mm_threshold.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
      IF season_name eq 'may-apr' THEN BEGIN
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_freq_annual)+1,/REV;,white=[9]
      ENDIF ELSE $
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_freq_season)+1,/REV;,white=[9] 
      MAP,LONMIN=MIN(trans_longitude),LONMAX=MAX(trans_longitude),LATMIN=MIN(trans_latitude),LATMAX=MAX(trans_latitude),/hires ;,/SET
      IF season_name eq 'may-apr' THEN BEGIN
         LEVS,MANUAL=mylevs_ratio_freq_annual
      ENDIF ELSE $
         LEVS,MANUAL=mylevs_ratio_freq_season
      CON,FIELD=ratio_stbl_control_freq_chunks,X=trans_longitude,Y=trans_latitude,$
          TITLE='Ratio in freq of > '+STRTRIM(STRING(threshold),1)+' mm day!U-1!N for 2x CO2 HiGEM (30 years) divided by ctl (30 years, chunk '+$
          STRTRIM(STRING(j+1),1)+') - '+season_name,/NOLINES,/BLOCK,CB_TITLE='Ratio (unitless)'
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_freq_transstbl.diff_ints_trans-minus-control_chunk'+$
             STRTRIM(STRING(j+1),1)+'.'+season_name+'_'+season_type+'.'+STRTRIM(STRING(threshold),1)+'mm_threshold.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
      IF season_name eq 'may-apr' THEN BEGIN
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_ints_annual)+1,/REV;,white=[8]
      ENDIF ELSE $
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_ints_season)+1,/REV;,white=[8] 
      MAP,LONMIN=MIN(trans_longitude),LONMAX=MAX(trans_longitude),LATMIN=MIN(trans_latitude),LATMAX=MAX(trans_latitude),/hires ;,/SET
      IF season_name eq 'may-apr' THEN BEGIN
         LEVS,MANUAL=mylevs_diff_ints_annual
         cb_label='mm day!U-1!N'
      ENDIF ELSE BEGIN
         LEVS,MANUAL=mylevs_diff_ints_season
         cb_label='mm day!U-1!N'
      ENDELSE
      CON,FIELD=diff_trans_control_ints_chunks,X=trans_longitude,Y=trans_latitude,$
          TITLE='Diff in intensity of > '+STRTRIM(STRING(threshold),1)+' mm day!U-1!N rain between 2% year!U-1!N CO2 trans (30 years) and ctl (30 years, chunk '+$
          STRTRIM(STRING(j+1),1)+') - '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_freq_transstbl.diff_ints_stbl-minus-control_chunk'+$
             STRTRIM(STRING(j+1),1)+'.'+season_name+'_'+season_type+'.'+STRTRIM(STRING(threshold),1)+'mm_threshold.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
      IF season_name eq 'may-apr' THEN BEGIN
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_ints_annual)+1,/REV;,white=[8]
      ENDIF ELSE $
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_ints_season)+1,/REV;,white=[8] 
      MAP,LONMIN=MIN(trans_longitude),LONMAX=MAX(trans_longitude),LATMIN=MIN(trans_latitude),LATMAX=MAX(trans_latitude),/hires ;,/SET
      IF season_name eq 'may-apr' THEN BEGIN
         LEVS,MANUAL=mylevs_diff_ints_annual
         cb_label='mm day!U-1!N'
      ENDIF ELSE BEGIN
         LEVS,MANUAL=mylevs_diff_ints_season
         cb_label='mm day!U-1!N'
      ENDELSE
      CON,FIELD=diff_stbl_control_ints_chunks,X=trans_longitude,Y=trans_latitude,$
          TITLE='Diff in ints of > '+STRTRIM(STRING(threshold),1)+' mm day!U-1!N rain between 2x CO2 (30 years) and ctl (30 years, chunk '+$
          STRTRIM(STRING(j+1),1)+') - '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_freq_transstbl.ratio_ints_trans-minus-control_chunk'+$
             STRTRIM(STRING(j+1),1)+'.'+season_name+'_'+season_type+'.'+STRTRIM(STRING(threshold),1)+'mm_threshold.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
      IF season_name eq 'may-apr' THEN BEGIN
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_ints_annual)+1,/REV;,white=[9]
      ENDIF ELSE $
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_ints_season)+1,/REV;,white=[9] 
      MAP,LONMIN=MIN(trans_longitude),LONMAX=MAX(trans_longitude),LATMIN=MIN(trans_latitude),LATMAX=MAX(trans_latitude),/hires ;,/SET
      IF season_name eq 'may-apr' THEN BEGIN
         LEVS,MANUAL=mylevs_ratio_ints_annual
      ENDIF ELSE $
         LEVS,MANUAL=mylevs_ratio_ints_season
      CON,FIELD=ratio_trans_control_ints_chunks,X=trans_longitude,Y=trans_latitude,$
          TITLE='Ratio of ints of > '+STRTRIM(STRING(threshold),1)+' mm day!U-1!N for 2% year!U-1!N CO2 trans (30 years) divided by ctl (30 years, chunk '+$
          STRTRIM(STRING(j+1),1)+') - '+season_name,/NOLINES,/BLOCK,CB_TITLE='Ratio (unitless)'
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_freq_transstbl.ratio_ints_stbl-minus-control_chunk'+$
             STRTRIM(STRING(j+1),1)+'.'+season_name+'_'+season_type+'.'+STRTRIM(STRING(threshold),1)+'mm_threshold.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
      IF season_name eq 'may-apr' THEN BEGIN
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_ints_annual)+1,/REV;,white=[9]
      ENDIF ELSE $
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_ints_season)+1,/REV;,white=[9] 
      MAP,LONMIN=MIN(trans_longitude),LONMAX=MAX(trans_longitude),LATMIN=MIN(trans_latitude),LATMAX=MAX(trans_latitude),/hires ;,/SET
      IF season_name eq 'may-apr' THEN BEGIN
         LEVS,MANUAL=mylevs_ratio_ints_annual
      ENDIF ELSE $
         LEVS,MANUAL=mylevs_ratio_ints_season
      CON,FIELD=ratio_stbl_control_ints_chunks,X=trans_longitude,Y=trans_latitude,$
          TITLE='Ratio in ints of > '+STRTRIM(STRING(threshold),1)+' mm day!U-1!N for 2x CO2 HiGEM (30 years) divided by ctl (30 years, chunk '+$
          STRTRIM(STRING(j+1),1)+') - '+season_name,/NOLINES,/BLOCK,CB_TITLE='Ratio (unitless)'
      AXES
      PSCLOSE,/NOVIEW
      
   ENDFOR

   FOR k=0,trans_nlon-1 DO BEGIN
      FOR m=0,trans_nlat-1 DO BEGIN
         control_freq_mean=MEAN(control_precip_freq(k,m,*))
         control_ints_mean=TOTAL(control_precip_freq(k,m,*)*$
                                 control_precip_ints(k,m,*),/NaN)/$
                           TOTAL(control_precip_freq(k,m,*),/NaN)

         diff_trans_control_freq_chunks(k,m)=trans_precip_freq_clim(k,m)-control_freq_mean
         diff_stbl_control_freq_chunks(k,m)=stbl_precip_freq_clim(k,m)-control_freq_mean

         IF control_freq_mean ge season_ndays*valid_mult THEN BEGIN
            ratio_trans_control_freq_chunks(k,m)=trans_precip_freq_clim(k,m)/control_freq_mean
            ratio_stbl_control_freq_chunks(k,m)=stbl_precip_freq_clim(k,m)/control_freq_mean
            
            diff_trans_control_ints_chunks(k,m)=trans_precip_ints_clim(k,m)-control_ints_mean
            ratio_trans_control_ints_chunks(k,m)=trans_precip_ints_clim(k,m)/control_ints_mean
            diff_stbl_control_ints_chunks(k,m)=stbl_precip_ints_clim(k,m)-control_ints_mean
            ratio_stbl_control_ints_chunks(k,m)=stbl_precip_ints_clim(k,m)/control_ints_mean            
            IF ratio_stbl_control_ints_chunks(k,m) le 0.0001 THEN BEGIN
               print,k,m
               STOP
            ENDIF
         ENDIF ELSE BEGIN
            ratio_trans_control_freq_chunks(k,m)=!Values.F_NaN
            ratio_stbl_control_freq_chunks(k,m)=!Values.F_NaN
            
            diff_trans_control_ints_chunks(k,m)=!Values.F_NaN
            ratio_trans_control_ints_chunks(k,m)=!Values.F_NaN
            diff_stbl_control_ints_chunks(k,m)=!Values.F_NaN
            ratio_stbl_control_ints_chunks(k,m)=!Values.F_NaN
         ENDELSE
      ENDFOR
   ENDFOR

   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_freq_transstbl.diff_freq_trans-minus-control.'+$
          season_name+'_'+season_type+'.'+STRTRIM(STRING(threshold),1)+'mm_threshold.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
   IF season_name eq 'may-apr' THEN BEGIN
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_freq_annual)+1,/REV,white=[9]
   ENDIF ELSE $
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_freq_season)+1,/REV,white=[9] 
   MAP,LONMIN=MIN(trans_longitude),LONMAX=MAX(trans_longitude),LATMIN=MIN(trans_latitude),LATMAX=MAX(trans_latitude),/hires ;,/SET
   IF season_name eq 'may-apr' THEN BEGIN
      LEVS,MANUAL=mylevs_diff_freq_annual
      cb_label='days year!U-1!N'
   ENDIF ELSE BEGIN
      LEVS,MANUAL=mylevs_diff_freq_season
      cb_label='days season!U-1!N'
   ENDELSE
   CON,FIELD=diff_trans_control_freq_chunks,X=trans_longitude,Y=trans_latitude,$
       TITLE='Diff in freq of > '+STRTRIM(STRING(threshold),1)+' mm day!U-1!N for 2% year!U-1!N CO2 trans (30 years) minus ctl (150 years) '+$
       '- '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label                                
   AXES
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_freq_transstbl.diff_freq_stbl-minus-control.'+$
          season_name+'_'+season_type+'.'+STRTRIM(STRING(threshold),1)+'mm_threshold.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
   IF season_name eq 'may-apr' THEN BEGIN
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_freq_annual)+1,/REV,white=[9]
   ENDIF ELSE $
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_freq_season)+1,/REV,white=[9] 
   MAP,LONMIN=MIN(trans_longitude),LONMAX=MAX(trans_longitude),LATMIN=MIN(trans_latitude),LATMAX=MAX(trans_latitude),/hires ;,/SET
   IF season_name eq 'may-apr' THEN BEGIN
      LEVS,MANUAL=mylevs_diff_freq_annual
      cb_label='days year!U-1!N'
   ENDIF ELSE BEGIN
      LEVS,MANUAL=mylevs_diff_freq_season
      cb_label='days season!U-1!N'
   ENDELSE
   CON,FIELD=diff_stbl_control_freq_chunks,X=trans_longitude,Y=trans_latitude,$
       TITLE='Diff in freq of > '+STRTRIM(STRING(threshold),1)+' mm day!U-1!N for 2x CO2 (30 years) minus ctl (150 years) '+$
       '- '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label                                
   AXES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_freq_transstbl.ratio_freq_trans-minus-control.'+$
          season_name+'_'+season_type+'.'+STRTRIM(STRING(threshold),1)+'mm_threshold.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
   IF season_name eq 'may-apr' THEN BEGIN
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_freq_annual)+1,/REV;,white=[9]
   ENDIF ELSE $
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_freq_season)+1,/REV;,white=[9] 
   MAP,LONMIN=MIN(trans_longitude),LONMAX=MAX(trans_longitude),LATMIN=MIN(trans_latitude),LATMAX=MAX(trans_latitude),/hires ;,/SET
   IF season_name eq 'may-apr' THEN BEGIN
      LEVS,MANUAL=mylevs_ratio_freq_annual
   ENDIF ELSE $
      LEVS,MANUAL=mylevs_ratio_freq_season
   CON,FIELD=ratio_trans_control_freq_chunks,X=trans_longitude,Y=trans_latitude,$
       TITLE='Ratio in freq of > '+STRTRIM(STRING(threshold),1)+' mm day!U-1!N for 2% year!U-1!N CO2 trans (30 years) divded by ctl (150 years) '+$
       '- '+season_name,/NOLINES,/BLOCK,CB_TITLE='Ratio (unitless)'
   AXES
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_freq_transstbl.ratio_freq_stbl-minus-control.'+$
          season_name+'_'+season_type+'.'+STRTRIM(STRING(threshold),1)+'mm_threshold.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
   IF season_name eq 'may-apr' THEN BEGIN
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_freq_annual)+1,/REV;,white=[9]
   ENDIF ELSE $
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_freq_season)+1,/REV;,white=[9] 
   MAP,LONMIN=MIN(trans_longitude),LONMAX=MAX(trans_longitude),LATMIN=MIN(trans_latitude),LATMAX=MAX(trans_latitude),/hires ;,/SET
   IF season_name eq 'may-apr' THEN BEGIN
      LEVS,MANUAL=mylevs_ratio_freq_annual      
   ENDIF ELSE $
      LEVS,MANUAL=mylevs_ratio_freq_season
   CON,FIELD=ratio_stbl_control_freq_chunks,X=trans_longitude,Y=trans_latitude,$
       TITLE='Ratio in freq of > '+STRTRIM(STRING(threshold),1)+' mm day!U-1!N for 2x CO2 (30 years) divded by ctl (150 years) '+$
       '- '+season_name,/NOLINES,/BLOCK,CB_TITLE='Ratio (unitless)'
   AXES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_freq_transstbl.diff_ints_trans-minus-control.'+$
          season_name+'_'+season_type+'.'+STRTRIM(STRING(threshold),1)+'mm_threshold.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
   IF season_name eq 'may-apr' THEN BEGIN
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_ints_annual)+1,/REV;,white=[8]
   ENDIF ELSE $
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_ints_season)+1,/REV;,white=[8] 
   MAP,LONMIN=MIN(trans_longitude),LONMAX=MAX(trans_longitude),LATMIN=MIN(trans_latitude),LATMAX=MAX(trans_latitude),/hires ;,/SET
   IF season_name eq 'may-apr' THEN BEGIN
      LEVS,MANUAL=mylevs_diff_ints_annual
      cb_label='mm day!U-1!N'
   ENDIF ELSE BEGIN
      LEVS,MANUAL=mylevs_diff_ints_season
      cb_label='mm day!U-1!N'
   ENDELSE
   CON,FIELD=diff_trans_control_ints_chunks,X=trans_longitude,Y=trans_latitude,$
       TITLE='Diff in ints of > '+STRTRIM(STRING(threshold),1)+' mm day!U-1!N for 2% year!U-1!N CO2 trans (30 years) minus ctl (150 years) '+$
       '- '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label                                
   AXES
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_freq_transstbl.diff_ints_stbl-minus-control.'+$
          season_name+'_'+season_type+'.'+STRTRIM(STRING(threshold),1)+'mm_threshold.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
   IF season_name eq 'may-apr' THEN BEGIN
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_ints_annual)+1,/REV;,white=[8]
   ENDIF ELSE $
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_ints_season)+1,/REV;,white=[8] 
   MAP,LONMIN=MIN(trans_longitude),LONMAX=MAX(trans_longitude),LATMIN=MIN(trans_latitude),LATMAX=MAX(trans_latitude),/hires ;,/SET
   IF season_name eq 'may-apr' THEN BEGIN
      LEVS,MANUAL=mylevs_diff_ints_annual
      cb_label='mm day!U-1!N'
   ENDIF ELSE BEGIN
      LEVS,MANUAL=mylevs_diff_ints_season
      cb_label='mm day!U-1!N'
   ENDELSE
   CON,FIELD=diff_stbl_control_ints_chunks,X=trans_longitude,Y=trans_latitude,$
       TITLE='Diff in ints of > '+STRTRIM(STRING(threshold),1)+' mm day!U-1!N for 2x CO2 (30 years) minus ctl (150 years) '+$
       '- '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label                                
   AXES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_freq_transstbl.ratio_ints_trans-minus-control.'+$
          season_name+'_'+season_type+'.'+STRTRIM(STRING(threshold),1)+'mm_threshold.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
   IF season_name eq 'may-apr' THEN BEGIN
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_ints_annual)+1,/REV;,white=[9]
   ENDIF ELSE $
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_ints_season)+1,/REV;,white=[9] 
   MAP,LONMIN=MIN(trans_longitude),LONMAX=MAX(trans_longitude),LATMIN=MIN(trans_latitude),LATMAX=MAX(trans_latitude),/hires ;,/SET
   IF season_name eq 'may-apr' THEN BEGIN
      LEVS,MANUAL=mylevs_ratio_ints_annual
   ENDIF ELSE $
      LEVS,MANUAL=mylevs_ratio_ints_season
   CON,FIELD=ratio_trans_control_ints_chunks,X=trans_longitude,Y=trans_latitude,$
       TITLE='Ratio in ints of > '+STRTRIM(STRING(threshold),1)+' mm day!U-1!N for 2% year!U-1!N CO2 trans (30 years) divded by ctl (150 years) '+$
       '- '+season_name,/NOLINES,/BLOCK,CB_TITLE='Ratio (unitless)'
   AXES
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_freq_transstbl.ratio_ints_stbl-minus-control.'+$
          season_name+'_'+season_type+'.'+STRTRIM(STRING(threshold),1)+'mm_threshold.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
   IF season_name eq 'may-apr' THEN BEGIN
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_ints_annual)+1,/REV;,white=[9]
   ENDIF ELSE $
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_ints_season)+1,/REV;,white=[9] 
   MAP,LONMIN=MIN(trans_longitude),LONMAX=MAX(trans_longitude),LATMIN=MIN(trans_latitude),LATMAX=MAX(trans_latitude),/hires ;,/SET
   IF season_name eq 'may-apr' THEN BEGIN
      LEVS,MANUAL=mylevs_ratio_ints_annual      
   ENDIF ELSE $
      LEVS,MANUAL=mylevs_ratio_ints_season
   CON,FIELD=ratio_stbl_control_ints_chunks,X=trans_longitude,Y=trans_latitude,$
       TITLE='Ratio in ints of > '+STRTRIM(STRING(threshold),1)+' mm day!U-1!N for 2x CO2 (30 years) divded by ctl (150 years) '+$
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
;   DEFINE_BOUNDARIES,qldbox,trans_latitude,trans_longitude,trans_qldbox_tx
;   
;   mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
;                                offset=[mask_qldbox_tx(1),mask_qldbox_tx(0),0,0],$
;                                count=[mask_nlon,mask_nlat,1,1]))
;   
;   control_qld_aavg=fltarr(higem_control_nyears)
;   trans_qld_aavg=fltarr(higem_trans_nyears)
;   stbl_qld_aavg=fltarr(higem_stbl_nyears)
;   FOR j=0,higem_control_nyears-1 DO BEGIN
;      thisyear_precip=REFORM(control_precip(control_qldbox_tx(1):control_qldbox_tx(3),control_qldbox_tx(0):control_qldbox_tx(2),j))
;      thisyear_precip[where(mask eq 0)]=!Values.F_NaN
;      control_qld_aavg(j)=MEAN(thisyear_precip,/NaN)      
;   ENDFOR
;   FOR j=0,higem_trans_nyears-1 DO BEGIN
;      thisyear_precip=REFORM(trans_precip(trans_qldbox_tx(1):trans_qldbox_tx(3),trans_qldbox_tx(0):trans_qldbox_tx(2),j))
;      thisyear_precip[where(mask eq 0)]=!Values.F_NaN
;      trans_qld_aavg(j)=MEAN(thisyear_precip,/NaN)     
;   ENDFOR
;   FOR j=0,higem_stbl_nyears-1 DO BEGIN
;      thisyear_precip=REFORM(stbl_precip(trans_qldbox_tx(1):trans_qldbox_tx(3),trans_qldbox_tx(0):trans_qldbox_tx(2),j))
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
;   GPLOT,X=indgen(higem_trans_nyears)+trans_offset,Y=trans_qld_aavg*season_ndays,COL=FSC_COLOR('red'),STYLE=0,THICK=40
;   GPLOT,X=indgen(higem_stbl_nyears)+trans_offset+stbl_offset,Y=stbl_qld_aavg*season_ndays,COL=FSC_COLOR('blue'),STYLE=0,THICK=40
;   
;   temp=SMOOTH(control_qld_aavg*season_ndays,5)
;   GPLOT,X=indgen(higem_control_nyears-4)+3,Y=temp(2:higem_control_nyears-3),COL=FSC_COLOR('black'),STYLE=0,THICK=150
;   temp=SMOOTH(trans_qld_aavg*season_ndays,5)
;   GPLOT,X=indgen(higem_trans_nyears-4)+3+trans_offset,Y=temp(2:higem_trans_nyears-3),COL=FSC_COLOR('red'),STYLE=0,THICK=150
;   temp=SMOOTH(stbl_qld_aavg*season_ndays,5)
;   GPLOT,X=indgen(higem_stbl_nyears-4)+3+trans_offset+stbl_offset,Y=temp(2:higem_stbl_nyears-3),COL=FSC_COLOR('blue'),STYLE=0,THICK=150
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

