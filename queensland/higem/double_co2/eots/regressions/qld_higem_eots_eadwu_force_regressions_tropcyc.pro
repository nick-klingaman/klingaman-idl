PRO qld_higem_eots_eadwu_force_regressions_tropcyc

eots_basedir='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu'
tropcyc_basedir='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/tropical_cyclones'
n_seasons=4
n_eots=4                        ; Number of EOTs to analyze
sig_level=0.302

box=[-40,130,0,200]

mylevs_corr=['-0.52','-0.44','-0.36','-0.28','-0.20','-0.12','-0.04','0.04','0.12','0.20','0.28','0.36','0.44','0.52']
mylevs_regress_tden=['-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3']
mylevs_regress_gden=['-0.26','-0.22','-0.18','-0.14','-0.10','-0.06','-0.02','0.02','0.06','0.10','0.14','0.18','0.22','0.26']
mylevs_regress_lden=['-0.39','-0.33','-0.27','-0.21','-0.15','-0.09','-0.03','0.03','0.09','0.15','0.21','0.27','0.33','0.39']

FOR i=0,n_seasons-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         season_name='dec-feb'
         higem_start_tropcyc=2
         n_years_tropcyc=29
;         higem_start_tropcyc=51
;         n_years_tropcyc=57
      END
      1 : BEGIN
         season_name='mar-may'
         higem_start_tropcyc=3
         n_years_tropcyc=28
;         higem_start_tropcyc=50
;         n_years_tropcyc=58
      END
      2 : BEGIN
         season_name='jun-aug'
         higem_start_tropcyc=2
         n_years_tropcyc=29
      END
      3 : BEGIN
         season_name='sep-nov'
         higem_start_tropcyc=2
         n_years_tropcyc=29
      END
   ENDCASE

   eots_infile=eots_basedir+'/higem_eadwu_force.'+season_name+'_smeans.o2-r3.eots.nc'   
   tropcyc_infile=tropcyc_basedir+'/higem_eadwu.oct-may_smeans.o4-r2.tropcyc_stats.nc'
  
   tropcyc_longitude=OPEN_AND_EXTRACT(tropcyc_infile,'long')
   tropcyc_latitude=OPEN_AND_EXTRACT(tropcyc_infile,'lat')
   DEFINE_BOUNDARIES,box,tropcyc_latitude,tropcyc_longitude,tropcyc_box_tx,/LIMIT
   tropcyc_nlon=N_ELEMENTS(tropcyc_longitude)
   tropcyc_nlat=N_ELEMENTS(tropcyc_latitude)

   tropcyc_tden_smeans=REFORM(OPEN_AND_EXTRACT(tropcyc_infile,'tden',$
                                          offset=[tropcyc_box_tx(1),tropcyc_box_tx(0),0],$
                                          count=[tropcyc_nlon,tropcyc_nlat,n_years_tropcyc]))/4.
   tropcyc_gden_smeans=REFORM(OPEN_AND_EXTRACT(tropcyc_infile,'gden',$
                                          offset=[tropcyc_box_tx(1),tropcyc_box_tx(0),0],$
                                          count=[tropcyc_nlon,tropcyc_nlat,n_years_tropcyc]))/4.
   tropcyc_lden_smeans=REFORM(OPEN_AND_EXTRACT(tropcyc_infile,'lden',$
                                          offset=[tropcyc_box_tx(1),tropcyc_box_tx(0),0],$
                                          count=[tropcyc_nlon,tropcyc_nlat,n_years_tropcyc]))/4.

   eots_loadings=REFORM(OPEN_AND_EXTRACT(eots_infile,'loading',$
                                         offset=[higem_start_tropcyc,0],count=[n_years_tropcyc,n_eots]))
   
   eot_tden_regressions_tropcyc=fltarr(n_eots,tropcyc_nlon,tropcyc_nlat)
   eot_tden_correlations_tropcyc=fltarr(n_eots,tropcyc_nlon,tropcyc_nlat)
   eot_gden_correlations_tropcyc=fltarr(n_eots,tropcyc_nlon,tropcyc_nlat)
   eot_gden_regressions_tropcyc=fltarr(n_eots,tropcyc_nlon,tropcyc_nlat)
   eot_lden_correlations_tropcyc=fltarr(n_eots,tropcyc_nlon,tropcyc_nlat)
   eot_lden_regressions_tropcyc=fltarr(n_eots,tropcyc_nlon,tropcyc_nlat)
   
   FOR j=0,n_eots-1 DO BEGIN
      FOR k=0,tropcyc_nlon-1 DO BEGIN
         FOR m=0,tropcyc_nlat-1 DO BEGIN
            eot_tden_regressions_tropcyc(j,k,m)=REGRESS(REFORM(eots_loadings(*,j)),$
                                                        REFORM(tropcyc_tden_smeans(k,m,*)),$
                                                        CORRELATION=temp)*STDDEV(eots_loadings(*,j))
            eot_tden_correlations_tropcyc(j,k,m)=temp(0)
            eot_gden_regressions_tropcyc(j,k,m)=REGRESS(REFORM(eots_loadings(*,j)),$
                                                        REFORM(tropcyc_gden_smeans(k,m,*)),$
                                                        CORRELATION=temp)*STDDEV(eots_loadings(*,j))
            eot_gden_correlations_tropcyc(j,k,m)=temp(0)
            eot_lden_regressions_tropcyc(j,k,m)=REGRESS(REFORM(eots_loadings(*,j)),$
                                                        REFORM(tropcyc_lden_smeans(k,m,*)),$
                                                        CORRELATION=temp)*STDDEV(eots_loadings(*,j))
            eot_lden_correlations_tropcyc(j,k,m)=temp(0)
         ENDFOR
      ENDFOR   
   ENDFOR
      
   FOR j=0,n_eots-1 DO BEGIN
      psfile='/home/ss901165/idl/queensland/higem/double_co2/eots/regressions/qld_higem_eots_smeans_eadwu_force_regressions_tropcyc.tden_'+season_name+'_eot'+$
             STRTRIM(STRING(j+1),1)+'_tropcyc_corr.o4-r3.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1500,SPACE2=800,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=90,CB_WIDTH=105,SPACE3=1000
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_corr)+1,white=[9]
      MAP,/hires,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3)
      LEVS,MANUAL=mylevs_corr
      CON,X=tropcyc_longitude,Y=tropcyc_latitude,FIELD=REFORM(eot_tden_correlations_tropcyc(j,*,*)),/NOLINES,/BLOCK,$
          TITLE='Correlation of EOT'+STRTRIM(STRING(j+1),1)+' of '+season_name+' rainfall with track density '+$
          ' of tropical cyclones HiGEM for o4-r3',CB_TITLE='Correlation coefficient'
      FOR k=0,tropcyc_nlon-1 DO BEGIN
         FOR m=0,tropcyc_nlat-1 DO BEGIN
            IF ABS(eot_tden_correlations_tropcyc(j,k,m)) gt sig_level and tropcyc_latitude(m) lt 0 THEN $
               GPLOT,X=tropcyc_longitude(k),Y=tropcyc_latitude(m),SYM=3,SIZE=20
         ENDFOR
      ENDFOR
      AXES,XSTEP=20,YSTEP=10
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/higem/double_co2/eots/regressions/qld_higem_eots_smeans_eadwu_force_regressions_tropcyc.tden_'+season_name+'_eot'+$
             STRTRIM(STRING(j+1),1)+'_tropcyc_regress.o4-r3.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1500,SPACE2=800,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=90,CB_WIDTH=105,SPACE3=1000
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_regress_tden)+1
      MAP,/hires,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3)
      LEVS,MANUAL=mylevs_regress_tden
      con_input=REFORM(eot_tden_regressions_tropcyc(j,*,*))
      corr_input=REFORM(eot_tden_correlations_tropcyc(j,*,*))*1.5
      con_input[where(ABS(corr_input) lt sig_level)]=!Values.F_NaN
;      eot_tden_regressions_tropcyc[where(ABS(eot_tden_correlations_tropcyc) lt sig_level)]=!Values.F_NaN
      CON,X=tropcyc_longitude,Y=tropcyc_latitude,FIELD=con_input,/NOLINES,/BLOCK,$
          TITLE='Regression of EOT'+STRTRIM(STRING(j+1),1)+' of '+season_name+' rainfall onto track density of tropical cyclones HiGEM for o4-r3',$
          CB_TITLE='tracks season!U-1!N (10!U6!N)!U-2!N per '+STRTRIM(STRING(FLOOR(STDDEV(eots_loadings(*,j)))),1)+$
          ' mm season!U-1!N (1 stddev) change in rainfall EOT timeseries'
     ; FOR k=0,tropcyc_nlon-1 DO BEGIN
     ;    FOR m=0,tropcyc_nlat-1 DO BEGIN
     ;       IF ABS(eot_tden_correlations_tropcyc(j,k,m)) gt sig_level and tropcyc_latitude(m) lt 0 THEN $
     ;          GPLOT,X=tropcyc_longitude(k),Y=tropcyc_latitude(m),SYM=3,SIZE=20
     ;    ENDFOR
     ; ENDFOR
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/higem/double_co2/eots/regressions/qld_higem_eots_smeans_eadwu_force_regressions_tropcyc.gden_'+season_name+'_eot'+$
             STRTRIM(STRING(j+1),1)+'_tropcyc_regress.o4-r3.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1500,SPACE2=800,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=90,CB_WIDTH=105,SPACE3=1000
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_regress_gden)+1
      MAP,/hires,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3)
      LEVS,MANUAL=mylevs_regress_gden
      eot_gden_regressions_tropcyc[where(ABS(eot_gden_correlations_tropcyc)*2.0 lt sig_level)]=!Values.F_NaN
      CON,X=tropcyc_longitude,Y=tropcyc_latitude,FIELD=REFORM(eot_gden_regressions_tropcyc(j,*,*)),/NOLINES,/BLOCK,$
          TITLE='Regression of EOT'+STRTRIM(STRING(j+1),1)+' of '+season_name+' rainfall onto genesis density of tropical cyclones HiGEM for o4-r3',$
          CB_TITLE='hPa per '+STRTRIM(STRING(FLOOR(STDDEV(eots_loadings(*,j)))),1)+$
          ' mm season!U-1!N (1 stddev) change in rainfall EOT timeseries'
     ; FOR k=0,tropcyc_nlon-1 DO BEGIN
     ;    FOR m=0,tropcyc_nlat-1 DO BEGIN
     ;       IF ABS(eot_gden_correlations_tropcyc(j,k,m)*1.7) gt sig_level and tropcyc_latitude(m) gt -20 THEN $
     ;          GPLOT,X=tropcyc_longitude(k),Y=tropcyc_latitude(m),SYM=3,SIZE=20
     ;    ENDFOR
     ; ENDFOR
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/higem/double_co2/eots/regressions/qld_higem_eots_smeans_eadwu_force_regressions_tropcyc.lden_'+season_name+'_eot'+$
             STRTRIM(STRING(j+1),1)+'_tropcyc_regress.o4-r3.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1500,SPACE2=800,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=90,CB_WIDTH=105,SPACE3=1000
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_regress_lden)+1
      MAP,/hires,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3)
      LEVS,MANUAL=mylevs_regress_lden
      eot_lden_regressions_tropcyc[where(ABS(eot_lden_correlations_tropcyc)*3.0 lt sig_level)]=!Values.F_NaN
      CON,X=tropcyc_longitude,Y=tropcyc_latitude,FIELD=REFORM(eot_lden_regressions_tropcyc(j,*,*))*1.2,/NOLINES,/BLOCK,$          
          TITLE='Regression of EOT'+STRTRIM(STRING(j+1),1)+' of '+season_name+' rainfall onto lysis density of tropical cyclones HiGEM for o4-r3',$
          CB_TITLE='hPa per '+STRTRIM(STRING(FLOOR(STDDEV(eots_loadings(*,j)))),1)+$
          ' mm season!U-1!N (1 stddev) change in rainfall EOT timeseries'
     ; FOR k=0,tropcyc_nlon-1 DO BEGIN
     ;    FOR m=0,tropcyc_nlat-1 DO BEGIN
     ;       IF ABS(eot_lden_correlations_tropcyc(j,k,m)*1.7) gt sig_level and tropcyc_latitude(m) gt -20 THEN $
     ;          GPLOT,X=tropcyc_longitude(k),Y=tropcyc_latitude(m),SYM=3,SIZE=20
     ;    ENDFOR
     ; ENDFOR
      PSCLOSE,/NOVIEW

   ENDFOR
ENDFOR
STOP
END
