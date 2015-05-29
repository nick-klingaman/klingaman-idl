PRO qld_higem_eots_smeans_eafeb_regressions_850200shear

eots_basedir='/home/ss901165/higem_qccce/es_control_eafeb'
higem_basedir='/home/ss901165/higem_qccce/es_control_eafeb'
n_seasons=2
higem_offset=0
n_years_higem=149
n_eots=4                        ; Number of EOTs to analyze
higem_siglevel=0.190

;lat_range=[-90,30]
box=[-40,110,0,210]

mylevs_corr=['-0.52','-0.44','-0.36','-0.28','-0.20','-0.12','-0.04','0.04','0.12','0.20','0.28','0.36','0.44','0.52']
mylevs_regress=['-1.70','-1.50','-1.30','-1.10','-0.90','-0.70','-0.50','-0.30','-0.10',$
                '0.10','0.30','0.50','0.70','0.90','1.10','1.30','1.50','1.70']

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
   higem_shear_infile=higem_basedir+'/higem_eafeb.'+season_name+'_smeans.h9-w8.shear_200_850.nc'
   
   higem_longitude=OPEN_AND_EXTRACT(higem_shear_infile,'longitude_1')
   higem_latitude=OPEN_AND_EXTRACT(higem_shear_infile,'latitude_1')
   DEFINE_BOUNDARIES,box,higem_latitude,higem_longitude,higem_box_tx,/LIMIT
   higem_nlon=N_ELEMENTS(higem_longitude)
   higem_nlat=N_ELEMENTS(higem_latitude)

   higem_shear_smeans=REFORM(OPEN_AND_EXTRACT(higem_shear_infile,'shear',$
                                                offset=[higem_box_tx(1),higem_box_tx(0),0,0],$
                                                count=[higem_nlon,higem_nlat,1,n_years_higem]))
   eots_loadings=REFORM(OPEN_AND_EXTRACT(eots_infile,'loading',$
                                        offset=[0,0],count=[n_years_higem,n_eots]))

   eot_shear_regressions_higem=fltarr(n_eots,higem_nlon,higem_nlat)
   eot_shear_correlations_higem=fltarr(n_eots,higem_nlon,higem_nlat)

   FOR j=0,n_eots-1 DO BEGIN
      FOR k=0,higem_nlon-1 DO BEGIN
         FOR m=0,higem_nlat-1 DO BEGIN            
            eot_shear_regressions_higem(j,k,m)=REGRESS(REFORM(eots_loadings(higem_offset:n_years_higem-1,j)),$
                                                         REFORM(higem_shear_smeans(k,m,*)),$
                                                         CORRELATION=temp)*STDDEV(eots_loadings(*,j))
            eot_shear_correlations_higem(j,k,m)=temp(0)
         ENDFOR
      ENDFOR
   ENDFOR

   FOR j=0,n_eots-1 DO BEGIN
      psfile='/home/ss901165/idl/queensland/higem/eots/regressions/qld_higem_eots_smeans_eafeb_regressions_850200shear.'+season_name+'_eot'+$
             STRTRIM(STRING(j+1),1)+'_higem_corr.h9-w8.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=105
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_corr)+1,white=[9]
      MAP,/hires,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2),/SECTOR     
      LEVS,MANUAL=mylevs_corr
      CON,X=higem_longitude,Y=higem_latitude,FIELD=REFORM(eot_shear_correlations_higem(j,*,*)),/NOLINES,/BLOCK,$
          TITLE='Correlation of EOT'+STRTRIM(STRING(j+1),1)+' of '+season_name+' rainfall with HiGEM eafeb shear for h9-w8',$
          CB_TITLE='Correlation coefficient'
      FOR k=0,higem_nlon-1 DO BEGIN
         FOR m=0,higem_nlat-1 DO BEGIN
            IF ABS(eot_shear_correlations_higem(j,k,m)) gt higem_siglevel THEN $
               GPLOT,X=higem_longitude(k),Y=higem_latitude(m),SYM=3,SIZE=20
         ENDFOR
      ENDFOR    
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/higem/eots/regressions/qld_higem_eots_smeans_eafeb_regressions_850200shear.'+season_name+'_eot'+$
             STRTRIM(STRING(j+1),1)+'_higem_regress.h9-w8.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1500,SPACE2=1000,XOFFSET=500,YOFFSET=2500,TFONT=2,TCHARSIZE=100,CB_WIDTH=105,$
             YSIZE=15000,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_regress)+1;,white=[10]
      MAP,/hires,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
      LEVS,MANUAL=mylevs_regress
      eot_shear_regressions_higem[where(ABS(eot_shear_correlations_higem)*1.2 lt higem_siglevel)]=!Values.F_NaN
      CON,X=higem_longitude,Y=higem_latitude,FIELD=REFORM(eot_shear_regressions_higem(j,*,*)),/NOLINES,/BLOCK,$
          TITLE='Regression of EOT'+STRTRIM(STRING(j+1),1)+' of '+season_name+' rainfall with HiGEM eafeb shear for h9-w8',$
          CB_TITLE='m s!U-1!N per '+STRTRIM(STRING(FLOOR(STDDEV(eots_loadings(*,j)))),1)+' mm season!U-1!N (1 stddev) change in rainfall EOT timeseries'
      AXES
      PSCLOSE,/NOVIEW
   ENDFOR
ENDFOR

STOP
END
