PRO qld_higem_eots_smeans_eafeb_regressions_blocking

eots_basedir='/home/ss901165/higem_qccce/es_control_eafeb'
higem_basedir='/home/ss901165/higem_qccce/es_control_eafeb'
n_seasons=4
n_eots=4                        ; Number of EOTs to analyze
sig_level=0.182
n_years=134
eot_start=15
block_start=0

lonavg_break=[120,150,180]

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
   higem_infile=higem_basedir+'/higem_eafeb.'+season_name+'_smeans.j4-w8.bom_blocking_index.nc'
   higem_longitude=OPEN_AND_EXTRACT(higem_infile,'longitude')
   higem_lonavg_break=intarr(3)
   FOR k=0,2 DO $
      higem_lonavg_break(k)=NEAREST(higem_longitude,lonavg_break(k)) 
   higem_nlon=N_ELEMENTS(higem_longitude)
   
   higem_smeans=REFORM(OPEN_AND_EXTRACT(higem_infile,'bom_index',$
                                        offset=[0,block_start],count=[higem_nlon,n_years]))
   
   eots_loadings=REFORM(OPEN_AND_EXTRACT(eots_infile,'loading',$
                                         offset=[eot_start,0],count=[n_years,n_eots]))

   eot_blocking_regressions_higem_smeans=fltarr(n_eots,higem_nlon)
   eot_blocking_correlations_higem_smeans=fltarr(n_eots,higem_nlon)

   higem_smeans_lonavg=fltarr(2,n_years)
   eot_blocking_regressions_higem_smeans_lonavg=fltarr(n_eots,2)
   eot_blocking_correlations_higem_smeans_lonavg=fltarr(n_eots,2)

   FOR j=0,1 DO $
      FOR k=0,n_years-1 DO $
         higem_smeans_lonavg(j,k)=MEAN(higem_smeans(higem_lonavg_break(j):higem_lonavg_break(j+1),k))

   FOR j=0,n_eots-1 DO BEGIN
      FOR k=0,higem_nlon-1 DO BEGIN
         eot_blocking_regressions_higem_smeans(j,k)=REGRESS(REFORM(eots_loadings(*,j)),$
                                                            REFORM(higem_smeans(k,*)),$
                                                            CORRELATION=temp)*STDDEV(eots_loadings(*,j))
         eot_blocking_correlations_higem_smeans(j,k)=temp(0)
      ENDFOR
      FOR k=0,1 DO BEGIN
         eot_blocking_regressions_higem_smeans_lonavg(j,k)=REGRESS(REFORM(eots_loadings(*,j)),$
                                                                   REFORM(higem_smeans_lonavg(k,*)),$
                                                                   CORRELATION=temp)*STDDEV(eots_loadings(*,j))
         eot_blocking_correlations_higem_smeans_lonavg(j,k)=temp(0)         
      ENDFOR       
   ENDFOR
   
   colors=['blue','red','purple','brown']
   styles=[0,1,2,3]
   symbols=[2,3,4,5]

   psfile='/home/ss901165/idl/queensland/higem/eots/regressions/qld_higem_eots_eafeb_silo025_regressions_blocking.'+season_name+'_alleots'+$
          '_regress.1900-2007.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=500,XOFFSET=1000,YOFFSET=1000,TFONT=2,TCHARSIZE=100,CB_WIDTH=105,SPACE3=200
   GSET,XMIN=MIN(higem_longitude),XMAX=MAX(higem_longitude),YMIN=-3.5,YMAX=3.5,TITLE='Regression of EOTs for '+season_name+' onto BoM blocking index'
   FOR j=0,n_eots-1 DO BEGIN
      ;FOR n=0,2 DO BEGIN    
      ;   GPLOT,X=era40_longitude,Y=REFORM(eot_blocking_regressions_era40_mmeans(n,j,*)),COL=FSC_COLOR(colors(n)),STYLE=styles(j),THICK=80
      ;   FOR k=0,era40_nlon-1 DO $
      ;      IF ABS(eot_blocking_correlations_era40_mmeans(n,j,k)) gt era40_siglevel THEN $
      ;         GPLOT,X=era40_longitude(k),Y=eot_blocking_regressions_era40_mmeans(n,j,k),SYM=symbols(j),/NOLINES,COL=FSC_COLOR(colors(n)),SIZE=50
      ;ENDFOR
      GPLOT,X=higem_longitude,Y=REFORM(eot_blocking_regressions_higem_smeans(j,*)),COL=FSC_COLOR(colors(j)),STYLE=styles(j)
      FOR k=0,higem_nlon-1 DO BEGIN
         IF ABS(eot_blocking_correlations_higem_smeans(j,k)) gt sig_level THEN $
            GPLOT,X=higem_longitude(k),Y=eot_blocking_regressions_higem_smeans(j,k),SYM=symbols(j),/NOLINES,COL=FSC_COLOR(colors(j)),SIZE=50
      ENDFOR
   ENDFOR
   AXES,XSTEP=10,YSTEP=0.5,YMINOR=0.25,XTITLE='Longitude (degrees east)',NDECS=2,$
        YTITLE='Regression coefficient (m s!U-1!N per one standard deviation in EOT timeseries)'
   GLEGEND,labels=REVERSE(['EOT1','EOT2','EOT3','EOT4']),STYLE=REVERSE(styles),SYM=REVERSE(symbols),LEGPOS=3,COL=REVERSE(FSC_COLOR(colors))
   PSCLOSE
   
   print,season_name
   FOR j=0,n_eots-1 DO BEGIN
      print,'EOT '+STRTRIM(STRING(j+1),1)+': '
      print,'Correlation with HiGEM blocking for '+STRTRIM(STRING(lonavg_break(0)),1)+'-'+STRTRIM(STRING(lonavg_break(1)),1)+'E: '+$
            STRTRIM(STRING(eot_blocking_correlations_higem_smeans_lonavg(j,0)),1)
      print,'Correlation with HiGEM blocking for '+STRTRIM(STRING(lonavg_break(1)),1)+'-'+STRTRIM(STRING(lonavg_break(2)),1)+'E: '+$
            STRTRIM(STRING(eot_blocking_correlations_higem_smeans_lonavg(j,1)),1)
   ENDFOR
   print,''

   ;psfile='/home/ss901165/idl/queensland/eots/regressions/qld_eots_smeans_silo025_regressions_blocking.'+season_name+'_alleots'+$
   ;       '_higem_regress.1900-2007.ps'
   ;PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1000,XOFFSET=1000,YOFFSET=1000,TFONT=2,TCHARSIZE=80,CB_WIDTH=105,SPACE3=1000
   ;GSET,XMIN=MIN(higem_longitude),XMAX=210,YMIN=-5,YMAX=5
   ;FOR j=0,n_eots-1 DO BEGIN
   ;   ;FOR n=0,2 DO BEGIN    
   ;   ;   GPLOT,X=higem_longitude,Y=REFORM(eot_blocking_regressions_higem_mmeans(n,j,*)),COL=FSC_COLOR(colors(n)),STYLE=styles(j),THICK=80
   ;   ;   FOR k=0,higem_nlon-1 DO $
   ;   ;      IF ABS(eot_blocking_correlations_higem_mmeans(n,j,k)) gt sig_level THEN $
   ;   ;         GPLOT,X=higem_longitude(k),Y=eot_blocking_regressions_higem_mmeans(n,j,k),SYM=symbols(j),/NOLINES,COL=FSC_COLOR(colors(n)),SIZE=50
   ;   ;ENDFOR
   ;   GPLOT,X=higem_longitude,Y=REFORM(eot_blocking_regressions_higem_smeans(j,*)),COL=FSC_COLOR("black"),STYLE=styles(j)
   ;   FOR k=0,higem_nlon-1 DO $
   ;      IF ABS(eot_blocking_correlations_higem_smeans(j,k)) gt sig_level THEN $
   ;         GPLOT,X=higem_longitude(k),Y=eot_blocking_regressions_higem_smeans(j,k),SYM=symbols(j),/NOLINES,COL=FSC_COLOR("black"),SIZE=50
   ;ENDFOR
   ;AXES,XSTEP=10,YSTEP=0.5,YMINOR=0.25,XTITLE='Longitude (degrees east)',NDECS=2,$
   ;     YTITLE='Regression coefficient (m s!U-1!N per one standard deviation in EOT timeseries)'
   ;LEGEND,labels=REVERSE(['EOT1','EOT2','EOT3']),STYLE=REVERSE(styles),SYM=REVERSE(symbols),LEGPOS=3
   ;LEGEND,labels=REVERSE(months),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=7
   ;PSCLOSE

ENDFOR


STOP
END
