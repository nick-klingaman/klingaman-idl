PRO qld_higem_eots_smeans_eafeb_regressions_uv500

eots_basedir='/home/ss901165/higem_qccce/es_control_eafeb'
higem_basedir='/home/ss901165/higem_qccce/es_control_eafeb'
n_seasons=4
higem_offset=0
n_years=149
n_eots=4                        ; Number of EOTs to analyze
higem_siglevel=0.190

box=[-75,75,10,240]

;q_levels=['-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01',$
;          '0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15']
q_levels=['-0.30','-0.26','-0.22','-0.18','-0.14','-0.10','-0.06','-0.02',$
          '0.02','0.06','0.10','0.14','0.18','0.22','0.26','0.30']

plot_box=[-65,80,10,240]

FOR i=0,n_seasons-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         season_name='dec-feb'
;         plot_box=[[-75,80,10,240],$
;                   [-65,80,10,200],$
;                   [-65,80,10,200],$
;                   [-65,80,10,200]]
;         xsize=[24000,20000,20000,20000]
;         ysize=[18000,18000,18000,18000]
         xstep=[20,10,10,10]
         xminor=[10,5,5,5]
      END
      1 : BEGIN
         season_name='mar-may'
;         plot_box=[[-65,80,10,200],$
;                   [-65,80,10,200],$
;                   [-75,75,10,240],$
;                   [-75,75,10,240]]
;         xsize=[20000,20000,24000,20000]
;         ysize=[18000,18000,18000,18000]
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
;         ysize=[18000,18000,18000,18000]
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
;         ysize=[18000,18000,18000,18000]
         xstep=[20,10,10,10]
         xminor=[10,5,5,5]
      END
   ENDCASE
   print,season_name

   eots_infile=eots_basedir+'/higem_eafeb.'+season_name+'_smeans.h9-w8.eots.nc'
  
   higem_u500_infile=higem_basedir+'/higem_eafeb.'+season_name+'_smeans.h9-w8.u500.pac_domain.nc'   
   higem_v500_infile=higem_basedir+'/higem_eafeb.'+season_name+'_smeans.h9-w8.v500.pac_domain.nc'
   higem_q500_infile=higem_basedir+'/higem_eafeb.'+season_name+'_smeans.h9-w8.q500.pac_domain.nc'

   higem_longitude=OPEN_AND_EXTRACT(higem_u500_infile,'longitude')
   higem_latitude=OPEN_AND_EXTRACT(higem_u500_infile,'latitude_1')
   DEFINE_BOUNDARIES,box,higem_latitude,higem_longitude,higem_box_tx,/LIMIT
   higem_nlon=N_ELEMENTS(higem_longitude)
   higem_nlat=N_ELEMENTS(higem_latitude)
   
   higem_smeans_u500=REFORM(OPEN_AND_EXTRACT(higem_u500_infile,'u_2',$
                                               offset=[higem_box_tx(1),higem_box_tx(0),higem_offset],$
                                               count=[higem_nlon,higem_nlat,n_years]))   
   higem_smeans_v500=REFORM(OPEN_AND_EXTRACT(higem_v500_infile,'v_2',$
                                               offset=[higem_box_tx(1),higem_box_tx(0),higem_offset],$
                                               count=[higem_nlon,higem_nlat,n_years]))
   higem_smeans_q500=REFORM(OPEN_AND_EXTRACT(higem_q500_infile,'q_1',$
                                               offset=[higem_box_tx(1),higem_box_tx(0),higem_offset],$
                                               count=[higem_nlon,higem_nlat,n_years]))*1000.
                                            
   eots_loadings=REFORM(OPEN_AND_EXTRACT(eots_infile,'loading',$
                                         offset=[0,0],count=[n_years,n_eots]))
      
   eot_u500_correlations_higem=fltarr(n_eots,higem_nlon,higem_nlat)
   eot_u500_regressions_higem=fltarr(n_eots,higem_nlon,higem_nlat)
   eot_v500_correlations_higem=fltarr(n_eots,higem_nlon,higem_nlat)
   eot_v500_regressions_higem=fltarr(n_eots,higem_nlon,higem_nlat)
   eot_q500_correlations_higem=fltarr(n_eots,higem_nlon,higem_nlat)
   eot_q500_regressions_higem=fltarr(n_eots,higem_nlon,higem_nlat)

   FOR j=0,n_eots-1 DO BEGIN
      FOR k=0,higem_nlon-1 DO BEGIN
         FOR m=0,higem_nlat-1 DO BEGIN
            eot_u500_regressions_higem(j,k,m)=REGRESS(REFORM(eots_loadings(*,j)),$
                                                        REFORM(higem_smeans_u500(k,m,*)),$
                                                        CORRELATION=temp)*STDDEV(eots_loadings(*,j))
            eot_u500_correlations_higem(j,k,m)=temp(0)
            eot_v500_regressions_higem(j,k,m)=REGRESS(REFORM(eots_loadings(*,j)),$
                                                        REFORM(higem_smeans_v500(k,m,*)),$
                                                        CORRELATION=temp)*STDDEV(eots_loadings(*,j))
            eot_v500_correlations_higem(j,k,m)=temp(0)
            eot_q500_regressions_higem(j,k,m)=REGRESS(REFORM(eots_loadings(*,j)),$
                                                        REFORM(higem_smeans_q500(k,m,*)),$
                                                        CORRELATION=temp)*STDDEV(eots_loadings(*,j))
            eot_q500_correlations_higem(j,k,m)=temp(0)          
         ENDFOR
      ENDFOR
      
      psfile='/home/ss901165/idl/queensland/higem/eots/regressions/qld_higem_eots_smeans_eafeb_regressions_uv500.'+season_name+'_eot'+$
             STRTRIM(STRING(j+1),1)+'_regress.h9-w8.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1000,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,CB_WIDTH=105,SPACE3=200,XSIZE=24000,YSIZE=18000
      MAP,/hires,LATMIN=plot_box(0),LONMIN=plot_box(1),LATMAX=plot_box(2),LONMAX=plot_box(3)
;      MAP,/hires,LATMIN=plot_box(0,j),LONMIN=plot_box(1,j),LATMAX=plot_box(2,j),LONMAX=plot_box(3,j)      
      CS,SCALE=1,NCOLS=N_ELEMENTS(q_levels)+1,/REV
      LEVS,MANUAL=q_levels
      eot_q500_regressions_higem[where(ABS(eot_q500_correlations_higem) lt higem_siglevel)]=!Values.F_NaN
      CON,X=higem_longitude,Y=higem_latitude,FIELD=REFORM(eot_q500_regressions_higem(j,*,*)),/NOLINES,/BLOCK
      black=FSC_COLOR("black",20)
      grey=FSC_COLOR("grey",22)      
      VECT,U=REFORM(eot_u500_regressions_higem(j,*,*)),V=REFORM(eot_v500_regressions_higem(j,*,*)),$
           X=higem_longitude,Y=higem_latitude,MAG='0.5',COL=22,MUNITS='m s!U-1!N',LEGPOS=15,STRIDE=3,/MAP
      eot_u500_regressions_higem[where(ABS(eot_v500_correlations_higem) lt higem_siglevel and ABS(eot_u500_correlations_higem) lt higem_siglevel)]=!Values.F_NaN
      eot_v500_regressions_higem[where(ABS(eot_v500_correlations_higem) lt higem_siglevel and ABS(eot_u500_correlations_higem) lt higem_siglevel)]=!Values.F_NaN      
      VECT,U=REFORM(eot_u500_regressions_higem(j,*,*)),V=REFORM(eot_v500_regressions_higem(j,*,*)),$
           X=higem_longitude,Y=higem_latitude,MAG='0.5',COL=20,MUNITS='m s!U-1!N',LEGPOS=15,STRIDE=3
      AXES,XSTEP=10,YSTEP=10,XMINOR=5,YMINOR=5
      PSCLOSE,/NOVIEW
   ENDFOR
ENDFOR

STOP
END
