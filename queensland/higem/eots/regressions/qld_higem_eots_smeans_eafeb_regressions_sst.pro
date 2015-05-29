PRO qld_higem_eots_smeans_eafeb_regressions_sst

eots_basedir='/home/ss901165/higem_qccce/es_control_eafeb'
higem_basedir='/home/ss901165/higem_qccce/es_control_eafeb'
mask_infile='/home/ss901165/um_output/mask_n144_higam.nc'
n_seasons=4
n_years=149
n_eots=4                        ; Number of EOTs to analyze

mylevs_corr=['-0.52','-0.44','-0.36','-0.28','-0.20','-0.12','-0.04','0.04','0.12','0.20','0.28','0.36','0.44','0.52']
mylevs_regress=['-0.375','-0.325','-0.275','-0.225','-0.175','-0.125','-0.075','-0.025','0.025','0.075','0.125','0.175','0.225','0.275','0.325','0.375']

FOR i=0,n_seasons-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         season_name='dec-feb'
      END
      1 : BEGIN
         season_name='mar-may'
      END
      2 : BEGIN
         season_name='jun-aug'
      END
      3 : BEGIN
         season_name='sep-nov'
      END
   ENDCASE
   
   eots_infile=eots_basedir+'/higem_eafeb.'+season_name+'_smeans.h9-w8.eots.nc'
   higem_infile=higem_basedir+'/higem_eafeb.'+season_name+'_smeans.h9-w8.surf_temp.global_domain.nc'

   higem_longitude=OPEN_AND_EXTRACT(higem_infile,'longitude')
   higem_latitude=OPEN_AND_EXTRACT(higem_infile,'latitude')
   higem_nlon=N_ELEMENTS(higem_longitude)
   higem_nlat=N_ELEMENTS(higem_latitude)

   higem_smeans=REFORM(OPEN_AND_EXTRACT(higem_infile,'temp',$
                                        offset=[0,0,0],count=[higem_nlon,higem_nlat,n_years]))
   eots_loadings=REFORM(OPEN_AND_EXTRACT(eots_infile,'loading',$
                                         offset=[0,0],count=[n_years,n_eots]))

   mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
   mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
   DEFINE_BOUNDARIES,[higem_latitude(0),higem_longitude(0),higem_latitude(higem_nlat-1),higem_longitude(higem_nlon-1)],$
                     mask_latitude,mask_longitude,mask_box_tx,/LIMIT
   mask_nlon=N_ELEMENTS(mask_longitude)
   mask_nlat=N_ELEMENTS(mask_latitude)
   
   mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                                offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                                count=[mask_nlon,mask_nlat,1,1]))
   
   higem_detrended_smeans=fltarr(higem_nlon,higem_nlat,n_years)
   FOR k=0,higem_nlon-1 DO BEGIN
      FOR m=0,higem_nlat-1 DO BEGIN
         IF higem_smeans(k,m,0) ge 0 THEN BEGIN
            trend=REGRESS(indgen(n_years),REFORM(higem_smeans(k,m,*)-MEAN(higem_smeans(k,m,*))))
            trend=trend(0)
            higem_detrended_smeans(k,m,*)=REFORM(higem_smeans(k,m,*))+indgen(n_years)*trend
         ENDIF
      ENDFOR
   ENDFOR

   eot_sst_regressions=fltarr(n_eots,higem_nlon,higem_nlat)
   eot_sst_correlations=fltarr(n_eots,higem_nlon,higem_nlat)
   FOR j=0,n_eots-1 DO BEGIN
      FOR k=0,higem_nlon-1 DO BEGIN
         FOR m=0,higem_nlat-1 DO BEGIN
            IF higem_smeans(k,m,0) ge 0 and mask(k,m) lt 1 THEN BEGIN               
               eot_sst_regressions(j,k,m)=REGRESS(REFORM(eots_loadings(*,j)),REFORM(higem_smeans(k,m,*)),CORRELATION=temp)*$
                                          STDDEV(eots_loadings(*,j))
               eot_sst_correlations(j,k,m)=temp(0)
            ENDIF ELSE BEGIN
               eot_sst_regressions(j,k,m)=!Values.F_NaN
               eot_sst_correlations(j,k,m)=!Values.F_NaN
            ENDELSE
         ENDFOR
      ENDFOR
   ENDFOR

   FOR j=0,n_eots-1 DO BEGIN
      psfile='/home/ss901165/idl/queensland/higem/eots/regressions/qld_higem_eots_smeans_eafeb_regressions_sst.'+season_name+'_eot'+$
             STRTRIM(STRING(j+1),1)+'_higem_corr.h9-w8.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=105,YSIZE=18000,$
             XSIZE=24000
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_corr)+1,white=[9]
      MAP,/hires
      LEVS,MANUAL=mylevs_corr
      CON,X=higem_longitude,Y=higem_latitude,FIELD=REFORM(eot_sst_correlations(j,*,*)),/NOLINES,/BLOCK,$
          TITLE='Correlation of EOT'+STRTRIM(STRING(j+1),1)+' of '+season_name+' rainfall with HiGEM SSTs for h9-w8',$
          CB_TITLE='Correlation coefficient'
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/higem/eots/regressions/qld_higem_eots_smeans_eafeb_regressions_sst_regional.'+season_name+'_eot'+$
             STRTRIM(STRING(j+1),1)+'_higem_regress.h9-w8.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=105,XSIZE=24000,$
             YSIZE=18000
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_regress)+1,white=[10]
      MAP,/hires,LONMIN=40,LONMAX=300,LATMIN=-60,LATMAX=30
      LEVS,MANUAL=mylevs_regress
      eot_sst_regressions[where(ABS(eot_sst_correlations) lt 0.198)]=!Values.F_NaN
      CON,X=higem_longitude,Y=higem_latitude,FIELD=REFORM(eot_sst_regressions(j,*,*)),/NOLINES,/BLOCK,$
;          TITLE='Regression of EOT'+STRTRIM(STRING(j+1),1)+' of '+season_name+' rainfall with HiGEM SSTs for h9-w8',$
          CB_TITLE='!Uo!NC per '+STRTRIM(STRING(FLOOR(STDDEV(eots_loadings(*,j)))),1)+' mm season!U-1!N (1 stddev) change in rainfall EOT timeseries'
      PSCLOSE,/NOVIEW
   ENDFOR
ENDFOR

STOP
END
