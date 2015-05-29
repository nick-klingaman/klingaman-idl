PRO qld_higem_eots_eadwu_eafee_force_regressions_uvq500

eots_basedir='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu'
higem_basedir='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu'
n_seasons=4
n_sets=2
n_years=31
n_eots=4                        ; Number of EOTs to analyze

box=[-75,75,10,240]
plot_box=[-65,80,10,240]

q500_levs=['-0.30','-0.26','-0.22','-0.18','-0.14','-0.10','-0.06','-0.02','0.02','0.06','0.10','0.14','0.18','0.22','0.26','0.30']

FOR i=0,n_seasons-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         season_name='dec-feb'
;         plot_box=[[-75,80,10,240],$
;                   [-65,80,10,200],$
;                   [-65,80,10,200],$
;                   [-75,80,10,240]]
;         xsize=[24000,20000,20000,24000]
         xstep=[20,20,20,20]
         xminor=[10,10,10,10]
      END
      1 : BEGIN
         season_name='mar-may'
;         plot_box=[[-65,80,10,200],$
;                   [-65,80,10,200],$
;                   [-75,75,10,240],$
;                   [-75,75,10,240]]
;         xsize=[20000,20000,24000,24000]
         xstep=[20,10,20,20]
         xminor=[10,5,5,5]
      END
      2 : BEGIN
         season_name='jun-aug'
;         plot_box=[[-75,75,10,240],$
;                   [-65,80,10,200],$
;                   [-65,80,10,200],$
;                   [-65,80,10,200]]
;         xsize=[24000,20000,20000,20000]
         xstep=[20,10,15,15]
         xminor=[10,5,5,5]
      END
      3 : BEGIN
         season_name='sep-nov'
;         plot_box=[[-75,75,10,240],$
;                   [-75,75,10,240],$
;                   [-65,80,10,200],$
;                   [-65,80,10,200]]
;         xsize=[24000,24000,20000,20000]
         xstep=[20,10,10,10]
         xminor=[10,5,5,5]
      END
   ENDCASE
   
   FOR n=0,n_sets-1 DO BEGIN
      CASE n OF 
         0 : BEGIN
            model_name='eadwu'
            year_range='o2-r3'
            eots_infile=eots_basedir+'/higem_eadwu_force.'+season_name+'_smeans.'+year_range+'.eots.nc'
            higem_q500_infile=higem_basedir+'/higem_eadwu.'+season_name+'_smeans.'+year_range+'.q500.global_domain.nc'
            higem_u500_infile=higem_basedir+'/higem_eadwu.'+season_name+'_smeans.'+year_range+'.u500.global_domain.nc'
            higem_v500_infile=higem_basedir+'/higem_eadwu.'+season_name+'_smeans.'+year_range+'.v500.global_domain.nc'
            sig_level=0.265
         END
         1 : BEGIN
            model_name='eafee_eadwu'
            year_range='m9-s0'
            eots_infile=eots_basedir+'/higem_eadwu_eafee_force.'+season_name+'_smeans.o2-r3.eots.nc'
            higem_q500_infile=higem_basedir+'/higem_eafee_eadwu.'+season_name+'_smeans.'+year_range+'.q500.global_domain.nc'
            higem_u500_infile=higem_basedir+'/higem_eafee_eadwu.'+season_name+'_smeans.'+year_range+'.u500.global_domain.nc'
            higem_v500_infile=higem_basedir+'/higem_eafee_eadwu.'+season_name+'_smeans.'+year_range+'.v500.global_domain.nc'
            sig_level=0.265
         END
      ENDCASE

      higem_plongitude=OPEN_AND_EXTRACT(higem_q500_infile,'longitude')
      higem_platitude=OPEN_AND_EXTRACT(higem_q500_infile,'latitude_1')
      DEFINE_BOUNDARIES,box,higem_platitude,higem_plongitude,higem_pbox_tx,/LIMIT
      higem_nplon=N_ELEMENTS(higem_plongitude)
      higem_nplat=N_ELEMENTS(higem_platitude)
      
      higem_ulongitude=OPEN_AND_EXTRACT(higem_u500_infile,'longitude')
      higem_ulatitude=OPEN_AND_EXTRACT(higem_u500_infile,'latitude_1')
      DEFINE_BOUNDARIES,box,higem_ulatitude,higem_ulongitude,higem_ubox_tx,/LIMIT
      higem_nulon=N_ELEMENTS(higem_ulongitude)
      higem_nulat=N_ELEMENTS(higem_ulatitude)
      
      higem_vlongitude=OPEN_AND_EXTRACT(higem_u500_infile,'longitude')
      higem_vlatitude=OPEN_AND_EXTRACT(higem_u500_infile,'latitude_1')
      DEFINE_BOUNDARIES,box,higem_vlatitude,higem_vlongitude,higem_vbox_tx,/LIMIT
      higem_nvlon=N_ELEMENTS(higem_vlongitude)
      higem_nvlat=N_ELEMENTS(higem_vlatitude)
      
      higem_smeans_q500=REFORM(OPEN_AND_EXTRACT(higem_q500_infile,'q_1',$
                                                offset=[higem_pbox_tx(1),higem_pbox_tx(0),0],$
                                                count=[higem_nplon,higem_nplat,n_years]))*1000.
      higem_smeans_u500=REFORM(OPEN_AND_EXTRACT(higem_u500_infile,'u_2',$
                                                offset=[higem_ubox_tx(1),higem_ubox_tx(0),0],$
                                                count=[higem_nulon,higem_nulat,n_years]))   
      higem_smeans_v500=REFORM(OPEN_AND_EXTRACT(higem_v500_infile,'v_2',$
                                                offset=[higem_vbox_tx(1),higem_vbox_tx(0),0],$
                                                count=[higem_nvlon,higem_nvlat,n_years]))
      
      eots_loadings=REFORM(OPEN_AND_EXTRACT(eots_infile,'loading',$
                                            offset=[0,0],count=[n_years,n_eots]))
      
      eot_q500_correlations_higem=fltarr(n_eots,higem_nplon,higem_nplat)
      eot_q500_regressions_higem=fltarr(n_eots,higem_nplon,higem_nplat)
      eot_u500_correlations_higem=fltarr(n_eots,higem_nulon,higem_nulat)
      eot_u500_regressions_higem=fltarr(n_eots,higem_nulon,higem_nulat)
      eot_v500_correlations_higem=fltarr(n_eots,higem_nvlon,higem_nvlat)
      eot_v500_regressions_higem=fltarr(n_eots,higem_nvlon,higem_nvlat)
      
      FOR j=0,n_eots-1 DO BEGIN
         FOR k=0,higem_nplon-1 DO BEGIN
            FOR m=0,higem_nplat-1 DO BEGIN
               eot_q500_regressions_higem(j,k,m)=REGRESS(REFORM(eots_loadings(*,j)),$
                                                         REFORM(higem_smeans_q500(k,m,*)),$
                                                         CORRELATION=temp)*STDDEV(eots_loadings(*,j))
               eot_q500_correlations_higem(j,k,m)=temp(0)
            ENDFOR
         ENDFOR
         FOR k=0,higem_nulon-1 DO BEGIN
            FOR m=0,higem_nulat-1 DO BEGIN
               eot_u500_regressions_higem(j,k,m)=REGRESS(REFORM(eots_loadings(*,j)),$
                                                         REFORM(higem_smeans_u500(k,m,*)),$
                                                         CORRELATION=temp)*STDDEV(eots_loadings(*,j))
               eot_u500_correlations_higem(j,k,m)=temp(0)
            ENDFOR
         ENDFOR
         FOR k=0,higem_nvlon-1 DO BEGIN
            FOR m=0,higem_nvlat-1 DO BEGIN
               eot_v500_regressions_higem(j,k,m)=REGRESS(REFORM(eots_loadings(*,j)),$
                                                         REFORM(higem_smeans_v500(k,m,*)),$
                                                         CORRELATION=temp)*STDDEV(eots_loadings(*,j))
               eot_v500_correlations_higem(j,k,m)=temp(0)            
            ENDFOR
         ENDFOR
         
         psfile='/home/ss901165/idl/queensland/higem/double_co2/eots/regressions/qld_higem_eots_eadwu_eafee_force_regressions_uvq500.'+season_name+'_eot'+$
                STRTRIM(STRING(j+1),1)+'_'+model_name+'_regress.'+year_range+'.ps'
         PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1000,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,CB_WIDTH=105,SPACE3=200,$
                XSIZE=24000,YSIZE=18000
;         MAP,/hires,LATMIN=plot_box(0,j),LONMIN=plot_box(1,j),LATMAX=plot_box(2,j),LONMAX=plot_box(3,j)
         MAP,/hires,LATMIN=plot_box(0),LONMIN=plot_box(1),LATMAX=plot_box(2),LONMAX=plot_box(3)
         CS,SCALE=1,NCOLS=N_ELEMENTS(q500_levs)+1,/REV
         LEVS,MANUAL=q500_levs
         black=FSC_COLOR("black",20)
         grey=FSC_COLOR("grey",22)

         eot_q500_regressions_higem[where(ABS(eot_q500_correlations_higem) lt sig_level)]=!Values.F_NaN
         CON,X=higem_plongitude,Y=higem_platitude,FIELD=REFORM(eot_q500_regressions_higem(j,*,*)),/BLOCK,/NOLINES
         VECT,U=REFORM(eot_u500_regressions_higem(j,*,*)),V=REFORM(eot_v500_regressions_higem(j,*,*)),$
              X=higem_ulongitude,Y=higem_ulatitude,MAG='0.5',COL=22,MUNITS='m s!U-1!N',LEGPOS=15,STRIDE=3,/MAP
         eot_u500_regressions_higem[where(ABS(eot_v500_correlations_higem) lt sig_level and ABS(eot_u500_correlations_higem) lt sig_level)]=!Values.F_NaN
         VECT,U=REFORM(eot_u500_regressions_higem(j,*,*)),V=REFORM(eot_v500_regressions_higem(j,*,*)),$
              X=higem_ulongitude,Y=higem_ulatitude,MAG='0.5',COL=20,MUNITS='m s!U-1!N',LEGPOS=15,STRIDE=3,/MAP
                                ; FOR k=0,higem_nlon-1,2 DO BEGIN
                                ;    FOR m=0,higem_nlat-1,2 DO BEGIN
                                ;       IF ABS(eot_u500_correlations_higem(j,k,m)) gt sig_level $
                                ;          or ABS(eot_v500_correlations_higem(j,k,m)) gt sig_level THEN $
                                ;          VECT,U=REFORM(eot_u500_regressions_higem(j,k,m)),V=REFORM(eot_v500_regressions_higem(j,k,m)),$
                                ;               X=higem_longitude(k),Y=higem_latitude(m),MAG=0.5,COL=20,/NOLEGEND
                                ;       ;IF ABS(eot_q500_correlations_higem(j,k,m)) gt sig_level THEN $
                                ;       ;   GPLOT,X=higem_longitude(k),Y=higem_latitude(m),SYM=3,SIZE=25,COL=21
                                ;    ENDFOR
                                ; ENDFOR
         AXES,XSTEP=10,YSTEP=10,XMINOR=5,YMINOR=5
         PSCLOSE,/NOVIEW
      ENDFOR
   ENDFOR
ENDFOR

STOP
END
