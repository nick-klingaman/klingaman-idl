PRO qld_higem_eots_smeans_eafeb_regressions_precip_monthly

eots_basedir='/home/ss901165/higem_qccce/es_control_eafeb'
higem_basedir='/home/ss901165/higem_qccce/es_control_eafeb'
n_seasons=4
n_eots=3                        ; Number of EOTs to analyze
higem_siglevel=0.190

mask_file='/home/ss901165/um_output/mask_n144_higam.nc'

mylevs_corr=['-0.52','-0.44','-0.36','-0.28','-0.20','-0.12','-0.04','0.04','0.12','0.20','0.28','0.36','0.44','0.52']

FOR i=0,n_seasons-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         season_name='dec-feb'
         months=['dec','jan','feb']
         year_offset=[0,1,1]
         n_years_higem=149
         n_days=[31,31,28]
         mylevs_regress=['-25','-15','-5','5','15','25','35','45','55','65','75','85','95','105','115']
         mylevs_mean=['30','60','90','120','150','180','210','240','270','300','330','360','390','420']
         max_col=165
      END
      1 : BEGIN
         season_name='mar-may'
         months=['mar','apr','may']
         year_offset=[0,0,0]
         n_years_higem=149
         n_days=[31,30,31]
         mylevs_regress=['-25','-15','-5','5','15','25','35','45','55','65','75','85','95','105','115']
         mylevs_mean=['20','40','60','80','100','120','140','160','180','200','220','240','260','280']
         max_col=165
      END
      2 : BEGIN
         season_name='jun-aug'
         months=['jun','jul','aug']
         year_offset=[0,0,0]
         n_years_era40=44
         n_years_higem=149
         n_days=[30,31,31]
         mylevs_regress=['-15','-9','-3','3','9','15','21','27','33','39','45','51','57','63','69']
         mylevs_mean=['15','30','45','60','75','90','105','120','135','150','165','180','195','210']
         max_col=165
      END
      3 : BEGIN
         season_name='sep-nov'
         months=['sep','oct','nov']
         year_offset=[0,0,0]
         n_years_higem=149
         n_days=[30,31,30]
         mylevs_regress=['-15','-9','-3','3','9','15','21','27','33','39','45','51','57','63','69']
         mylevs_mean=['15','30','45','60','75','90','105','120','135','150','165','180','195','210']
         max_col=165
      END
   ENDCASE

   eots_infile=eots_basedir+'/higem_eafeb.'+season_name+'_smeans.h9-w8.eots.nc'

   FOR j=0,2 DO BEGIN
      higem_infile=higem_basedir+'/higem_eafeb.'+months(j)+'_mmeans.h9-w8.precip.aus_domain.nc'
      IF j eq 0 THEN BEGIN
         higem_longitude=OPEN_AND_EXTRACT(higem_infile,'longitude')
         higem_latitude=OPEN_AND_EXTRACT(higem_infile,'latitude')
         higem_nlon=N_ELEMENTS(higem_longitude)
         higem_nlat=N_ELEMENTS(higem_latitude)
         higem_mmeans=fltarr(higem_nlon,higem_nlat,n_years_higem,3)
      ENDIF      
      higem_mmeans(*,*,*,j)=REFORM(OPEN_AND_EXTRACT(higem_infile,'precip',$
                                          offset=[0,0,0,year_offset(j),0],count=[higem_nlon,higem_nlat,1,n_years_higem,1]))*n_days(j)*86400.
   ENDFOR
   
   mask_longitude=OPEN_AND_EXTRACT(mask_file,'longitude')
   mask_latitude=OPEN_AND_EXTRACT(mask_file,'latitude')
   DEFINE_BOUNDARIES,[higem_latitude(0),higem_longitude(0),higem_latitude(higem_nlat-1),higem_longitude(higem_nlon-1)],$
                     mask_latitude,mask_longitude,mask_box_tx,/LIMIT
   mask=REFORM(OPEN_AND_EXTRACT(mask_file,'lsm',$
                                offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                                count=[higem_nlon,higem_nlat,1,1]))

   FOR j=0,n_years_higem-1 DO BEGIN
      FOR k=0,2 DO BEGIN
         temp=REFORM(higem_mmeans(*,*,j,k))
         temp[where(mask eq 0)]=!Values.F_NaN
         higem_mmeans(*,*,j,k)=temp
      ENDFOR
   ENDFOR
   higem_mmeans_clim=fltarr(3,higem_nlon,higem_nlat)
   FOR j=0,higem_nlon-1 DO $
      FOR k=0,higem_nlat-1 DO $
         FOR n=0,2 DO $
            higem_mmeans_clim(n,j,k)=MEAN(higem_mmeans(j,k,*,n),/NaN)

   eots_loadings=REFORM(OPEN_AND_EXTRACT(eots_infile,'loading',$
                                         offset=[0,0],count=[n_years_higem,n_eots]))
   eot_precip_regressions_higem=fltarr(3,n_eots,higem_nlon,higem_nlat)
   eot_precip_correlations_higem=fltarr(3,n_eots,higem_nlon,higem_nlat)

   FOR j=0,n_eots-1 DO BEGIN
      FOR k=0,higem_nlon-1 DO BEGIN
         FOR m=0,higem_nlat-1 DO BEGIN
            FOR n=0,2 DO BEGIN
               eot_precip_regressions_higem(n,j,k,m)=REGRESS(REFORM(eots_loadings(*,j)),$
                                                            REFORM(higem_mmeans(k,m,*,n)),$
                                                            CORRELATION=temp)*STDDEV(eots_loadings(*,j))
               eot_precip_correlations_higem(n,j,k,m)=temp(0)
            ENDFOR
         ENDFOR
      ENDFOR
   ENDFOR

   FOR n=0,2 DO BEGIN
      FOR j=0,n_eots-1 DO BEGIN
         psfile='/home/ss901165/idl/queensland/higem/eots/regressions/qld_higem_eots_smeans_eafeb_regressions_precip.'+season_name+'_eot'+$
                STRTRIM(STRING(j+1),1)+'_higem_corr_'+months(n)+'.h9-w8.ps'
         PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1000,XOFFSET=500,YOFFSET=2000,TFONT=2,TCHARSIZE=80,CB_WIDTH=105,SPACE3=1000,$
                XSIZE=18000,YSIZE=15000
         CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_corr)+1,white=[9]
         MAP,/hires,LATMIN=-45,LATMAX=-8,LONMIN=110,LONMAX=154 ;,LATMIN=-45,LATMAX=-8,LONMIN=105,LONMAX=160
         LEVS,MANUAL=mylevs_corr
         CON,X=higem_longitude,Y=higem_latitude,FIELD=REFORM(eot_precip_correlations_higem(n,j,*,*)),/NOLINES,/BLOCK,$
             TITLE='Correlation of EOT'+STRTRIM(STRING(j+1),1)+' of '+season_name+' rainfall with '+months(n)+' total HiGEM eafeb rain for h9-w8',$
             CB_TITLE='Correlation coefficient'
         FOR k=0,higem_nlon-1 DO BEGIN
            FOR m=0,higem_nlat-1 DO BEGIN
               IF ABS(eot_precip_correlations_higem(n,j,k,m)) gt higem_siglevel and higem_latitude(m) lt 0 THEN $
                  GPLOT,X=higem_longitude(k),Y=higem_latitude(m),SYM=3,SIZE=10
            ENDFOR
         ENDFOR                               
         PSCLOSE,/NOVIEW

         psfile='/home/ss901165/idl/queensland/higem/eots/regressions/qld_higem_eots_smeans_eafeb_regressions_precip_monthly.'+season_name+'_eot'+$
                STRTRIM(STRING(j+1),1)+'_higem_regress_'+months(n)+'.h9-w8.ps'
         PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1000,XOFFSET=500,YOFFSET=2500,TFONT=2,TCHARSIZE=80,CB_WIDTH=105,SPACE3=1000,$
                XSIZE=15000,YSIZE=15000
         CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_regress)+1,MAX=max_col,/REV
         MAP,/hires,LATMIN=-45,LATMAX=-8,LONMIN=110,LONMAX=154
         LEVS,MANUAL=mylevs_regress
         FOR k=0,higem_nlon-1 DO BEGIN
            FOR m=0,higem_nlat-1 DO BEGIN
               IF ABS(eot_precip_correlations_higem(n,j,k,m)) lt higem_siglevel THEN $
                  eot_precip_regressions_higem(n,j,k,m)=!Values.F_NaN
            ENDFOR
         ENDFOR
         CON,X=higem_longitude,Y=higem_latitude,FIELD=REFORM(eot_precip_regressions_higem(n,j,*,*)),/NOLINES,/BLOCK,$
             TITLE='Regression of EOT'+STRTRIM(STRING(j+1),1)+' of '+season_name+' rain onto '+months(n)+' total HiGEM eafeb rain for h9-w8',$
             CB_TITLE='mm month!U-1!N per '+STRTRIM(STRING(FLOOR(STDDEV(eots_loadings(*,j)))),1)+$
             ' mm season!U-1!N (1 stddev) change in rainfall EOT timeseries'         
         PSCLOSE,/NOVIEW
      ENDFOR
      psfile='/home/ss901165/idl/queensland/eots/regressions/qld_higem_eots_smeans_eafeb_regressions_precip.'+months(n)+'_mean_precip_higem.h9-w8.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1000,XOFFSET=500,YOFFSET=2500,TFONT=2,TCHARSIZE=80,CB_WIDTH=105,SPACE3=1000,$
             XSIZE=15000,YSIZE=15000
      CS,SCALE=25,NCOLS=N_ELEMENTS(mylevs_mean)+1,white=[2]
      MAP,/hires,LATMIN=-45,LATMAX=-8,LONMIN=110,LONMAX=154
      LEVS,MANUAL=mylevs_mean
      CON,X=higem_longitude,Y=higem_latitude,FIELD=REFORM(higem_mmeans_clim(n,*,*)),/NOLINELABELS,POSITIVE_STYLE=2,$
          TITLE='Clim precip for '+months(n)+' from HiGEM eafeb 0.25 - h9-w8',$
          CB_TITLE='mm month!U-1!N'
      PSCLOSE,/NOVIEW
   ENDFOR
ENDFOR

STOP
END
