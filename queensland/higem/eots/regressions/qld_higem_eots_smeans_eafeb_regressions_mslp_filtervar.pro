PRO qld_higem_eots_smeans_eafeb_regressions_mslp_filtervar

eots_basedir='/home/ss901165/higem_qccce/es_control_eafeb'
higem_basedir='/home/ss901165/higem_qccce/es_control_eafeb'
n_seasons=4
higem_offset=0
n_years=149
n_eots=4                        ; Number of EOTs to analyze
higem_siglevel=0.162

mylevs_corr=['-0.52','-0.44','-0.36','-0.28','-0.20','-0.12','-0.04','0.04','0.12','0.20','0.28','0.36','0.44','0.52']
mylevs_regress=['-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15']

FOR i=0,n_seasons-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         season_name='dec-feb'
         mylevs_regress=[['-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15'],$
                         ['-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15'],$
                         ['-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15'],$
                         ['-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15']]
      END
      1 : BEGIN
         season_name='mar-may'
         mylevs_regress=[['-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15'],$
                         ['-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15'],$
                         ['-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15'],$
                         ['-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15']]
      END
      2 : BEGIN
         season_name='jun-aug'
         mylevs_regress=[['-0.45','-0.39','-0.33','-0.27','-0.21','-0.15','-0.09','-0.03','0.03','0.09','0.15','0.21','0.27','0.33','0.39','0.45'],$
                         ['-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15'],$
                         ['-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15'],$
                         ['-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15']]
      END
      3 : BEGIN
         season_name='sep-nov'
         mylevs_regress=[['-0.30','-0.26','-0.22','-0.18','-0.14','-0.10','-0.06','-0.02','0.02','0.06','0.10','0.14','0.18','0.22','0.26','0.30'],$
                         ['-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15'],$
                         ['-0.30','-0.26','-0.22','-0.18','-0.14','-0.10','-0.06','-0.02','0.02','0.06','0.10','0.14','0.18','0.22','0.26','0.30'],$
                         ['-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15']]
      END
   ENDCASE

   eots_infile=eots_basedir+'/higem_eafeb.'+season_name+'_smeans.h9-w8.eots.nc'
   higem_infile=higem_basedir+'/higem_eafeb.'+season_name+'_smeans.h9-w8.msl_filter210_stddev.nc'
   
   higem_longitude=OPEN_AND_EXTRACT(higem_infile,'longitude')
   higem_latitude=OPEN_AND_EXTRACT(higem_infile,'latitude')
   higem_nlon=N_ELEMENTS(higem_longitude)
   higem_nlat=N_ELEMENTS(higem_latitude)

   higem_smeans=REFORM(OPEN_AND_EXTRACT(higem_infile,'MSL',$
                                          offset=[0,0,higem_offset],count=[higem_nlon,higem_nlat,n_years]))/100.
   eots_loadings=REFORM(OPEN_AND_EXTRACT(eots_infile,'loading',$
                                        offset=[0,0],count=[n_years,n_eots]))

   eot_mslp_regressions_higem=fltarr(n_eots,higem_nlon,higem_nlat)
   eot_mslp_correlations_higem=fltarr(n_eots,higem_nlon,higem_nlat)

   FOR j=0,n_eots-1 DO BEGIN
      FOR k=0,higem_nlon-1 DO BEGIN
         FOR m=0,higem_nlat-1 DO BEGIN            
            eot_mslp_regressions_higem(j,k,m)=REGRESS(REFORM(eots_loadings(*,j)),$
                                                      REFORM(higem_smeans(k,m,*)),$
                                                      CORRELATION=temp)*STDDEV(eots_loadings(*,j))
            eot_mslp_correlations_higem(j,k,m)=temp(0)
         ENDFOR
      ENDFOR
   ENDFOR
   
   FOR j=0,n_eots-1 DO BEGIN
      psfile='/home/ss901165/idl/queensland/higem/eots/regressions/qld_higem_eots_smeans_eafeb_regressions_mslp_filtervar.'+season_name+'_eot'+$
             STRTRIM(STRING(j+1),1)+'_higem_corr.h9-w8.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1500,SPACE2=800,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=90,CB_WIDTH=105,SPACE3=1000
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_corr)+1,white=[9]
      MAP,/hires,LATMIN=-90,LATMAX=0,/SH
      LEVS,MANUAL=mylevs_corr
      CON,X=higem_longitude,Y=higem_latitude,FIELD=REFORM(eot_mslp_correlations_higem(j,*,*)),/NOLINES,/BLOCK,$
          TITLE='Correlation of EOT'+STRTRIM(STRING(j+1),1)+' of '+season_name+' rainfall with HiGEM MSLP for h9-w8',$
          CB_TITLE='Correlation coefficient'
      FOR k=0,higem_nlon-1 DO BEGIN
         FOR m=0,higem_nlat-1 DO BEGIN
            IF ABS(eot_mslp_correlations_higem(j,k,m)) gt higem_siglevel and higem_latitude(m) lt 0 THEN $
               GPLOT,X=higem_longitude(k),Y=higem_latitude(m),SYM=3,SIZE=30
         ENDFOR
      ENDFOR    
      FOR k=0,3 DO $
         GPLOT,X=higem_longitude,Y=REPLICATE(-20*k,higem_nlon),STYLE=0,COL=FSC_COLOR('black'),THICK=70
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/higem/eots/regressions/qld_higem_eots_smeans_eafeb_regressions_mslp_filtervar.'+season_name+'_eot'+$
             STRTRIM(STRING(j+1),1)+'_higem_regress.h9-w8.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1500,SPACE2=800,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=90,CB_WIDTH=105,SPACE3=1000
      CS,SCALE=1,NCOLS=N_ELEMENTS(REFORM(mylevs_regress(*,j)))+1 ;,white=[10]
      MAP,/hires,LATMIN=-90,LATMAX=0,/SH,LONMIN=60,LONMAX=200,/SECTOR
      LEVS,MANUAL=REFORM(mylevs_regress(*,j))
      eot_mslp_regressions_higem[where(ABS(eot_mslp_correlations_higem) lt higem_siglevel)]=!Values.F_NaN
      CON,X=higem_longitude,Y=higem_latitude,FIELD=REFORM(eot_mslp_regressions_higem(j,*,*)),/NOLINES,/BLOCK,$
          TITLE='Regression of EOT'+STRTRIM(STRING(j+1),1)+' of '+season_name+' rainfall with HiGEM MSLP for h9-w8'
;          CB_TITLE='hPa per '+STRTRIM(STRING(FLOOR(STDDEV(eots_loadings(*,j)))),1)+' mm season!U-1!N (1 stddev) change in rainfall EOT timeseries'
;      FOR k=0,higem_nlon-1 DO BEGIN
;         FOR m=0,higem_nlat-1 DO BEGIN
;            IF ABS(eot_mslp_correlations_higem(j,k,m)) gt higem_siglevel and higem_latitude(m) lt 0 THEN $
;               GPLOT,X=higem_longitude(k),Y=higem_latitude(m),SYM=3,SIZE=30
;         ENDFOR
;      ENDFOR   
      FOR k=1,3 DO $
         GPLOT,X=higem_longitude,Y=REPLICATE(-20*k,higem_nlon),STYLE=0,COL=FSC_COLOR('black'),THICK=70
         ;GPLOT,X=120,Y=-20*k+3,TEXT=STRTRIM(STRING(20*k),1)+'S'
      ;ENDFOR
      PSCLOSE,/NOVIEW
   ENDFOR
ENDFOR

STOP
END
