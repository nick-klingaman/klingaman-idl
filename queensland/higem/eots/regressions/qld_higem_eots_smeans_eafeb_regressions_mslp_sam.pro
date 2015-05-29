PRO qld_higem_eots_smeans_eafeb_regressions_mslp_sam

eots_basedir='/home/ss901165/higem_qccce/es_control_eafeb'
higem_basedir='/home/ss901165/higem_qccce/es_control_eafeb'
n_seasons=2
higem_offset=0
n_years=149
n_eots=4                        ; Number of EOTs to analyze
higem_siglevel=0.190

box=[-90,0,-10,360]

mylevs_corr=['-0.52','-0.44','-0.36','-0.28','-0.20','-0.12','-0.04','0.04','0.12','0.20','0.28','0.36','0.44','0.52']
mylevs_regress=['-3.0','-2.6','-2.2','-1.8','-1.4','-1.0','-0.6','-0.2','0.2','0.6','1.0','1.4','1.8','2.2','2.6','3.0']

FOR i=0,n_seasons-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         season_name='jun-aug'
      END
      1 : BEGIN
         season_name='sep-nov'
      END
   ENDCASE
   
   eots_infile=eots_basedir+'/higem_eafeb.'+season_name+'_smeans.h9-w8.eots.nc'
   higem_infile=higem_basedir+'/higem_eafeb.'+season_name+'_smeans.h9-w8.mslp.global_domain.nc'  
   
   higem_longitude=OPEN_AND_EXTRACT(higem_infile,'longitude')
   higem_latitude=OPEN_AND_EXTRACT(higem_infile,'latitude')
   DEFINE_BOUNDARIES,box,higem_latitude,higem_longitude,higem_box_tx,/LIMIT
   higem_nlon=N_ELEMENTS(higem_longitude)
   higem_nlat=N_ELEMENTS(higem_latitude)

   higem_smeans=REFORM(OPEN_AND_EXTRACT(higem_infile,'p',$
                                          offset=[higem_box_tx(1),higem_box_tx(0),higem_offset],$
                                          count=[higem_nlon,higem_nlat,n_years]))/100.
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
      psfile='/home/ss901165/idl/queensland/higem/eots/regressions/qld_higem_eots_smeans_eafeb_regressions_mslp_sam.'+season_name+'_eot'+$
             STRTRIM(STRING(j+1),1)+'_higem_corr.h9-w8.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=105
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_corr)+1,white=[9]
      MAP,/hires,LONMIN=-60,LONMAX=300,LATMIN=box(0),LATMAX=box(2),/SH
      LEVS,MANUAL=mylevs_corr
      CON,X=higem_longitude,Y=higem_latitude,FIELD=REFORM(eot_mslp_correlations_higem(j,*,*)),/NOLINES,/BLOCK,$
          TITLE='Correlation of EOT'+STRTRIM(STRING(j+1),1)+' of '+season_name+' rainfall with HiGEM MSLP for h9-w8',$
          CB_TITLE='Correlation coefficient'
      FOR k=0,higem_nlon-1 DO BEGIN
         FOR m=0,higem_nlat-1 DO BEGIN
            IF ABS(eot_mslp_correlations_higem(j,k,m)) gt higem_siglevel THEN $
               GPLOT,X=higem_longitude(k),Y=higem_latitude(m),SYM=3,SIZE=20
         ENDFOR
      ENDFOR
      PSCLOSE,/NOVIEW
      
      psfile='/home/ss901165/idl/queensland/higem/eots/regressions/qld_higem_eots_smeans_eafeb_regressions_mslp_sam.'+season_name+'_eot'+$
             STRTRIM(STRING(j+1),1)+'_higem_regress.h9-w8.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1500,SPACE2=400,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=105
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_regress)+1;,white=[10]
      MAP,/hires,LONMIN=-60,LONMAX=300,LATMIN=box(0),LATMAX=box(2),/SH
      LEVS,MANUAL=mylevs_regress
      eot_mslp_regressions_higem[where(ABS(eot_mslp_correlations_higem) lt higem_siglevel)]=!Values.F_NaN
      CON,X=higem_longitude,Y=higem_latitude,FIELD=REFORM(eot_mslp_regressions_higem(j,*,*)),/NOLINES,/BLOCK,$
          TITLE='Regression of EOT'+STRTRIM(STRING(j+1),1)+' of '+season_name+' rainfall with HiGEM MSLP for h9-w8'
      GPLOT,X=[higem_longitude,higem_longitude(0)],Y=REPLICATE(-20,higem_nlon+1),STYLE=0,COL=FSC_COLOR('black'),THICK=70
      GPLOT,X=[higem_longitude,higem_longitude(0)],Y=REPLICATE(-40,higem_nlon+1),STYLE=0,COL=FSC_COLOR('black'),THICK=70
      GPLOT,X=[higem_longitude,higem_longitude(0)],Y=REPLICATE(-70,higem_nlon+1),STYLE=0,COL=FSC_COLOR('black'),THICK=70
      GPLOT,X=120,Y=-18,TEXT='20S'
      GPLOT,X=120,Y=-37,TEXT='40S'
      GPLOT,X=120,Y=-66,TEXT='70S'
      PSCLOSE,/NOVIEW
   ENDFOR
ENDFOR

STOP
END
