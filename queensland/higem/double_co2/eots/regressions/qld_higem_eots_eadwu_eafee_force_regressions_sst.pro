PRO qld_higem_eots_eadwu_eafee_force_regressions_sst

eots_basedir='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu'
higem_basedir='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu'
n_seasons=4
n_sets=2
n_years=31
n_eots=4                        ; Number of EOTs to analyze

box=[-60,40,30,300]
mask_infile='/home/ss901165/um_output/mask_n144_higam.nc'
mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)
mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                             offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                             count=[mask_nlon,mask_nlat,1,1]))

sst_levs=['-0.375','-0.325','-0.275','-0.225','-0.175','-0.125','-0.075','-0.025','0.025','0.075','0.125','0.175','0.225','0.275','0.325','0.375']

FOR i=0,n_seasons-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         season_name='dec-feb'
;         plot_box=[[-75,80,10,240],$
;                   [-65,80,10,200],$
;                   [-65,80,10,200],$
;                   [-75,80,10,240]]
         xsize=[24000,20000,20000,24000]
         xstep=[20,20,20,20]
         xminor=[10,10,10,10]
      END
      1 : BEGIN
         season_name='mar-may'
;         plot_box=[[-65,80,10,200],$
;                   [-65,80,10,200],$
;                   [-75,75,10,240],$
;                   [-75,75,10,240]]
         xsize=[20000,20000,24000,24000]
         xstep=[20,10,20,20]
         xminor=[10,5,5,5]
      END
      2 : BEGIN
         season_name='jun-aug'
;         plot_box=[[-75,75,10,240],$
;                   [-65,80,10,200],$
;                   [-65,80,10,200],$
;                   [-65,80,10,200]]
         xsize=[24000,20000,20000,20000]
         xstep=[20,10,15,15]
         xminor=[10,5,5,5]
      END
      3 : BEGIN
         season_name='sep-nov'
;         plot_box=[[-75,75,10,240],$
;                   [-75,75,10,240],$
;                   [-65,80,10,200],$
;                   [-65,80,10,200]]
         xsize=[24000,24000,20000,20000]
         xstep=[20,10,10,10]
         xminor=[10,5,5,5]
      END
   ENDCASE
   
   FOR n=0,n_sets-1 DO BEGIN
      CASE n OF 
         0 : BEGIN
            model_name='eadwu'
            year_range='o2-r3'
            eots_infile=eots_basedir+'/higem_eadwu_force.'+season_name+'_smeans.o2-r3.eots.nc'
            higem_sst_infile=higem_basedir+'/higem_eadwu.'+season_name+'_smeans.'+year_range+'.surf_temp.global_domain.nc'
            sig_level=0.265
         END
         1 : BEGIN
            model_name='eafee_eadwu'
            year_range='m9-s0'
            eots_infile=eots_basedir+'/higem_eadwu_eafee_force.'+season_name+'_smeans.o2-r3.eots.nc'
            higem_sst_infile=higem_basedir+'/higem_eafee_eadwu.'+season_name+'_smeans.'+year_range+'.surf_temp.global_domain.nc'
            sig_level=0.265
         END
      ENDCASE

      higem_plongitude=OPEN_AND_EXTRACT(higem_sst_infile,'longitude')
      higem_platitude=OPEN_AND_EXTRACT(higem_sst_infile,'latitude')
      DEFINE_BOUNDARIES,box,higem_platitude,higem_plongitude,higem_pbox_tx,/LIMIT
      higem_nplon=N_ELEMENTS(higem_plongitude)
      higem_nplat=N_ELEMENTS(higem_platitude)
      
      
      higem_smeans_sst=REFORM(OPEN_AND_EXTRACT(higem_sst_infile,'temp_2',$
                                                offset=[higem_pbox_tx(1),higem_pbox_tx(0),0],$
                                                count=[higem_nplon,higem_nplat,n_years]))

      higem_smeans_sst_detrend=fltarr(higem_nplon,higem_nplat,n_years)
      FOR j=0,higem_nplon-1 DO BEGIN
         FOR k=0,higem_nplat-1 DO BEGIN
            trend=REGRESS(indgen(n_years),REFORM(higem_smeans_sst(j,k,*)))
            higem_smeans_sst_detrend(j,k,*)=higem_smeans_sst(j,k,*)-trend(0)*indgen(n_years)
         ENDFOR
      ENDFOR
      
      eots_loadings=REFORM(OPEN_AND_EXTRACT(eots_infile,'loading',$
                                            offset=[0,0],count=[n_years,n_eots]))
      
      eot_sst_correlations_higem=fltarr(n_eots,higem_nplon,higem_nplat)
      eot_sst_regressions_higem=fltarr(n_eots,higem_nplon,higem_nplat)

      eot_sst_detrend_correlations_higem=fltarr(n_eots,higem_nplon,higem_nplat)
      eot_sst_detrend_regressions_higem=fltarr(n_eots,higem_nplon,higem_nplat)

      FOR j=0,n_eots-1 DO BEGIN
         FOR k=0,higem_nplon-1 DO BEGIN
            FOR m=0,higem_nplat-1 DO BEGIN
               eot_sst_regressions_higem(j,k,m)=REGRESS(REFORM(eots_loadings(*,j)),$
                                                         REFORM(higem_smeans_sst(k,m,*)),$
                                                         CORRELATION=temp)*STDDEV(eots_loadings(*,j))
               eot_sst_correlations_higem(j,k,m)=temp(0)

               eot_sst_detrend_regressions_higem(j,k,m)=REGRESS(REFORM(eots_loadings(*,j)),$
                                                                REFORM(higem_smeans_sst_detrend(k,m,*)),$
                                                                CORRELATION=temp)*STDDEV(eots_loadings(*,j))
               eot_sst_detrend_correlations_higem(j,k,m)=temp(0)
            ENDFOR
         ENDFOR
         
         psfile='/home/ss901165/idl/queensland/higem/double_co2/eots/regressions/qld_higem_eots_eadwu_eafee_force_regressions_sst.'+season_name+'_eot'+$
                STRTRIM(STRING(j+1),1)+'_'+model_name+'_regress.'+year_range+'.ps'
         PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1000,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,CB_WIDTH=105,SPACE3=200,$
                XSIZE=24000,YSIZE=18000
         MAP,/hires,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3);LATMIN=plot_box(0,j),LONMIN=plot_box(1,j),LATMAX=plot_box(2,j),LONMAX=plot_box(3,j)
         CS,SCALE=1,NCOLS=N_ELEMENTS(sst_levs)+1
         LEVS,MANUAL=sst_levs        
         eot_sst_regressions_higem[where(ABS(eot_sst_correlations_higem) lt sig_level)]=!Values.F_NaN
         toplot=REFORM(eot_sst_regressions_higem(j,*,*))
         toplot[where(mask ne 0)]=!Values.F_NaN
         CON,X=higem_plongitude,Y=higem_platitude,FIELD=toplot,/BLOCK,/NOLINES
         AXES
         PSCLOSE,/NOVIEW

         psfile='/home/ss901165/idl/queensland/higem/double_co2/eots/regressions/qld_higem_eots_eadwu_eafee_force_regressions_sst.'+season_name+'_eot'+$
                STRTRIM(STRING(j+1),1)+'_'+model_name+'_regress_detrend.'+year_range+'.ps'
         PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1000,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,CB_WIDTH=105,SPACE3=200,$
                XSIZE=24000,YSIZE=18000
         MAP,/hires,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3);LATMIN=plot_box(0,j),LONMIN=plot_box(1,j),LATMAX=plot_box(2,j),LONMAX=plot_box(3,j)
         CS,SCALE=1,NCOLS=N_ELEMENTS(sst_levs)+1
         LEVS,MANUAL=sst_levs        
         eot_sst_detrend_regressions_higem[where(ABS(eot_sst_detrend_correlations_higem) lt sig_level)]=!Values.F_NaN
         toplot=REFORM(eot_sst_detrend_regressions_higem(j,*,*))
         toplot[where(mask ne 0)]=!Values.F_NaN
         CON,X=higem_plongitude,Y=higem_platitude,FIELD=toplot,/BLOCK,/NOLINES,$
             CB_TITLE='!Uo!NC per '+STRTRIM(STRING(FLOOR(STDDEV(eots_loadings(*,j)))),1)+' mm season!U-1!N (1 stddev) change in rainfall EOT timeseries'
         AXES
         PSCLOSE,/NOVIEW

      ENDFOR
   ENDFOR
ENDFOR

STOP
END
