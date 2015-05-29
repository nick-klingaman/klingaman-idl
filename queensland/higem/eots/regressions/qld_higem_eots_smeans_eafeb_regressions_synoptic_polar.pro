PRO qld_higem_eots_smeans_eafeb_regressions_synoptic_polar

eots_basedir='/home/ss901165/higem_qccce/es_control_eafeb'
higem_basedir='/home/ss901165/higem_qccce/es_control_eafeb'
n_seasons=4
n_years=149
n_eots=4                        ; Number of EOTs to analyze
sig_level=0.184

box=[-90,0,0,360]

mslp_levs=['-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1']

FOR i=0,n_seasons-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         season_name='dec-feb'
         plot_box=[[-90,0,0,360],$
                   [-90,0,0,360],$
                   [-90,0,0,360],$
                   [-90,0,0,360]]
         xsize=[24000,24000,24000,24000]
         xstep=[20,10,10,0]
         xminor=[10,5,5,5]
      END
      1 : BEGIN
         season_name='mar-may'
         plot_box=[[-90,0,0,360],$
                   [-90,0,0,360],$
                   [-90,0,0,360],$
                   [-90,0,0,360]]
         xsize=[24000,24000,24000,24000]
         xstep=[20,10,20,20]
         xminor=[10,5,5,5]
      END
      2 : BEGIN
         season_name='jun-aug'
         plot_box=[[-90,0,0,360],$
                   [-90,0,0,360],$
                   [-90,0,0,360],$
                   [-90,0,0,360]]
         xsize=[24000,24000,24000,24000]
         xstep=[20,10,15,15]
         xminor=[10,5,5,5]
      END
      3 : BEGIN
         season_name='sep-nov'
         plot_box=[[-90,0,0,360],$
                   [-90,0,0,360],$
                   [-90,0,0,360],$
                   [-90,0,0,360]]
         xsize=[24000,24000,24000,24000]
         xstep=[20,10,10,10]
         xminor=[10,5,5,5]
      END
   ENDCASE
   
   eots_infile=eots_basedir+'/higem_eafeb.'+season_name+'_smeans.h9-w8.eots.nc'
   
   higem_mslp_infile=higem_basedir+'/higem_eafeb.'+season_name+'_smeans.h9-w8.mslp.sam_domain.nc'
   higem_u850_infile=higem_basedir+'/higem_eafeb.'+season_name+'_smeans.h9-w8.u850.mjo_domain.nc'
   higem_v850_infile=higem_basedir+'/higem_eafeb.'+season_name+'_smeans.h9-w8.v850.pac_domain.nc'

   higem_plongitude=OPEN_AND_EXTRACT(higem_mslp_infile,'longitude')
   higem_platitude=OPEN_AND_EXTRACT(higem_mslp_infile,'latitude')
   DEFINE_BOUNDARIES,box,higem_platitude,higem_plongitude,higem_pbox_tx,/LIMIT
   higem_nplon=N_ELEMENTS(higem_plongitude)
   higem_nplat=N_ELEMENTS(higem_platitude)
   
  ; higem_ulongitude=OPEN_AND_EXTRACT(higem_u850_infile,'longitude_1')
  ; higem_ulatitude=OPEN_AND_EXTRACT(higem_u850_infile,'latitude_1')
  ; DEFINE_BOUNDARIES,box,higem_ulatitude,higem_ulongitude,higem_ubox_tx,/LIMIT
  ; higem_nulon=N_ELEMENTS(higem_ulongitude)
  ; higem_nulat=N_ELEMENTS(higem_ulatitude)

  ; higem_vlongitude=OPEN_AND_EXTRACT(higem_u850_infile,'longitude_1')
  ; higem_vlatitude=OPEN_AND_EXTRACT(higem_u850_infile,'latitude_1')
  ; DEFINE_BOUNDARIES,box,higem_vlatitude,higem_vlongitude,higem_vbox_tx,/LIMIT
  ; higem_nvlon=N_ELEMENTS(higem_vlongitude)
  ; higem_nvlat=N_ELEMENTS(higem_vlatitude)

   higem_smeans_mslp=REFORM(OPEN_AND_EXTRACT(higem_mslp_infile,'p',$
                                             offset=[higem_pbox_tx(1),higem_pbox_tx(0),0],$
                                             count=[higem_nplon,higem_nplat,n_years]))/100.
  ; higem_smeans_u850=REFORM(OPEN_AND_EXTRACT(higem_u850_infile,'u',$
  ;                                           offset=[higem_ubox_tx(1),higem_ubox_tx(0),0],$
  ;                                           count=[higem_nulon,higem_nulat,n_years]))   
  ; higem_smeans_v850=REFORM(OPEN_AND_EXTRACT(higem_v850_infile,'v',$
  ;                                           offset=[higem_vbox_tx(1),higem_vbox_tx(0),0],$
  ;                                           count=[higem_nvlon,higem_nvlat,n_years]))
   
   eots_loadings=REFORM(OPEN_AND_EXTRACT(eots_infile,'loading',$
                                         offset=[0,0],count=[n_years,n_eots]))
   
   eot_mslp_correlations_higem=fltarr(n_eots,higem_nplon,higem_nplat)
   eot_mslp_regressions_higem=fltarr(n_eots,higem_nplon,higem_nplat)
  ; eot_u850_correlations_higem=fltarr(n_eots,higem_nulon,higem_nulat)
  ; eot_u850_regressions_higem=fltarr(n_eots,higem_nulon,higem_nulat)
  ; eot_v850_correlations_higem=fltarr(n_eots,higem_nvlon,higem_nvlat)
  ; eot_v850_regressions_higem=fltarr(n_eots,higem_nvlon,higem_nvlat)
   
   FOR j=0,n_eots-1 DO BEGIN
      FOR k=0,higem_nplon-1 DO BEGIN
         FOR m=0,higem_nplat-1 DO BEGIN
            eot_mslp_regressions_higem(j,k,m)=REGRESS(REFORM(eots_loadings(*,j)),$
                                                        REFORM(higem_smeans_mslp(k,m,*)),$
                                                        CORRELATION=temp)*STDDEV(eots_loadings(*,j))
            eot_mslp_correlations_higem(j,k,m)=temp(0)
         ENDFOR
      ENDFOR
  ;    FOR k=0,higem_nulon-1 DO BEGIN
  ;       FOR m=0,higem_nulat-1 DO BEGIN
  ;          eot_u850_regressions_higem(j,k,m)=REGRESS(REFORM(eots_loadings(*,j)),$
  ;                                                    REFORM(higem_smeans_u850(k,m,*)),$
  ;                                                    CORRELATION=temp)*STDDEV(eots_loadings(*,j))
  ;          eot_u850_correlations_higem(j,k,m)=temp(0)
  ;       ENDFOR
  ;    ENDFOR
  ;    FOR k=0,higem_nvlon-1 DO BEGIN
  ;       FOR m=0,higem_nvlat-1 DO BEGIN
  ;          eot_v850_regressions_higem(j,k,m)=REGRESS(REFORM(eots_loadings(*,j)),$
  ;                                                    REFORM(higem_smeans_v850(k,m,*)),$
  ;                                                    CORRELATION=temp)*STDDEV(eots_loadings(*,j))
  ;          eot_v850_correlations_higem(j,k,m)=temp(0)            
                                ;    ENDFOR
                                ;   ENDFOR
      
      psfile='/home/ss901165/idl/queensland/higem/eots/regressions/qld_higem_eots_smeans_eafeb_regressions_synoptic_polar.'+season_name+'_eot'+$
             STRTRIM(STRING(j+1),1)+'_higem_regress.h9-w8.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1000,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,CB_WIDTH=105,SPACE3=200,XSIZE=xsize(j),YSIZE=18000
      MAP,/hires,LATMIN=plot_box(0,j),LONMIN=plot_box(1,j),LATMAX=plot_box(2,j),LONMAX=plot_box(3,j),/SH
      CS,SCALE=1,NCOLS=N_ELEMENTS(mslp_levs)+1
      LEVS,MANUAL=mslp_levs
      black=FSC_COLOR("black",20)
      grey=FSC_COLOR("grey",22)
;      red=FSC_COLOR("red",21)
;      CON,X=higem_longitude,Y=higem_latitude,FIELD=REFORM(eot_mslp_regressions_higem(j,*,*)),/NOFILL,$
;          NEGATIVE_STYLE=2,POSITIVE_STYLE=0,COL=21,THICK=150
      eot_mslp_regressions_higem[where(ABS(eot_mslp_correlations_higem) lt sig_level)]=!Values.F_NaN
      CON,X=higem_plongitude,Y=higem_platitude,FIELD=REFORM(eot_mslp_regressions_higem(j,*,*)),/BLOCK,/NOLINES
;      VECT,U=REFORM(eot_u850_regressions_higem(j,*,*)),V=REFORM(eot_v850_regressions_higem(j,*,*)),$
;           X=higem_ulongitude,Y=higem_ulatitude,MAG='0.5',COL=22,MUNITS='m s!U-1!N',LEGPOS=15,STRIDE=3
;      eot_u850_regressions_higem[where(ABS(eot_v850_correlations_higem) lt sig_level and ABS(eot_u850_correlations_higem) lt sig_level)]=!Values.F_NaN
;      VECT,U=REFORM(eot_u850_regressions_higem(j,*,*)),V=REFORM(eot_v850_regressions_higem(j,*,*)),$
;           X=higem_ulongitude,Y=higem_ulatitude,MAG='0.5',COL=20,MUNITS='m s!U-1!N',LEGPOS=15,STRIDE=3
                                ; FOR k=0,higem_nlon-1,2 DO BEGIN
                                ;    FOR m=0,higem_nlat-1,2 DO BEGIN
                                ;       IF ABS(eot_u850_correlations_higem(j,k,m)) gt sig_level $
                                ;          or ABS(eot_v850_correlations_higem(j,k,m)) gt sig_level THEN $
                                ;          VECT,U=REFORM(eot_u850_regressions_higem(j,k,m)),V=REFORM(eot_v850_regressions_higem(j,k,m)),$
                                ;               X=higem_longitude(k),Y=higem_latitude(m),MAG=0.5,COL=20,/NOLEGEND
                                ;       ;IF ABS(eot_mslp_correlations_higem(j,k,m)) gt sig_level THEN $
                                ;       ;   GPLOT,X=higem_longitude(k),Y=higem_latitude(m),SYM=3,SIZE=25,COL=21
                                ;    ENDFOR
                                ; ENDFOR
      GPLOT,X=[higem_plongitude,higem_plongitude(0)],Y=REPLICATE(-40,higem_nplon+1),COL=FSC_COLOR("black"),THICK=70
      GPLOT,X=[higem_plongitude,higem_plongitude(0)],Y=REPLICATE(-65,higem_nplon+1),COL=FSC_COLOR("black"),THICK=70
      AXES,XSTEP=10,YSTEP=10,XMINOR=5,YMINOR=5
      PSCLOSE
   ENDFOR
ENDFOR

STOP
END
