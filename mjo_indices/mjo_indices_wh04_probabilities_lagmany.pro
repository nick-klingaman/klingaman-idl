PRO mjo_indices_wh04_probabilities_lagmany

n_plots=2
um6='/home/ss901165/um_output6'
um3='/home/ss901165/um_output3'
FOR i=0,n_plots-1 DO BEGIN
   CASE i OF
      2 : BEGIN
         infiles=['/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2012.index_values.nc',$
                ;  um6+'/xihvd/rmm_indices.nc',$
                ;  um6+'/xgspj/rmm_indices.nc',$
                  um3+'/hadgem3_monwg/akkvi/rmm_indices.nc',$
                  um3+'/hadgem3_monwg/aliur/rmm_indices.nc',$
                  um3+'/hadgem3_monwg/aljyr/rmm_indices.nc',$
                  um6+'/gc2/antia/rmm_indices.nc',$
                  um6+'/gc2/anqjm/rmm_indices.nc']
         labels=['Obs','GA3_N96','GA4_N96','GA4_N96O1','GA6_N96','GC2_N96']
         a=[1,1,1,1,1,1,1]
         colors=['black','brown','red','violetred','blue','dodgerblue']
         syms=[1,2,3,4,5,6]
         styles=[0,1,2,3,4,5]
         set_name='ga_gc'         
      END
      3 : BEGIN
         infiles=['/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2012.index_values.nc',$
                ;  um6+'/xihvd/rmm_indices.nc',$
                ;  um6+'/xgspj/rmm_indices.nc',$
                ;  um3+'/hadgem3_monwg/akkvi/rmm_indices.nc',$
                ;  um3+'/hadgem3_monwg/aliur/rmm_indices.nc',$
                ;  um3+'/hadgem3_monwg/aljyr/rmm_indices.nc',$
                  um6+'/gc2/antia/rmm_indices.nc']
                ;  um6+'/gc2/anqjm/rmm_indices.nc']
         labels=['Obs','MetUM_GA6']
         a=[1,1]
         colors=['black','red']
         syms=[1,1]
         styles=[0,0]
         set_name='ga6'  
      END
      0 : BEGIN
         infiles=['/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2012.index_values.nc',$
                  um6+'/xgspp/rmm_indices.nc',$
                  um6+'/xgspm/rmm_indices.nc',$
                  um6+'/spcam/cam_iceedge_free/rmm_indices.nc',$
                  um6+'/spcam/spcam_iceedge_free/rmm_indices.nc'] ;,$
;                  um6+'/mjodiab_20year/spcam/rmm_indices.nc',$
;                  um6+'/mjodiab_20year/spccsm/rmm_indices.nc']
         labels=['Obs','MetUM-GOML1','MetUM-GOML1 1.5F','CAM3-KPP','SPCAM3-KPP'] ;,'SPCAM3-AMIP','SPCCSM3']
         a=[1,1,1.1,1,1,1,1]
         colors=['black','purple','violetred','red','blue'] ;,'orange','violetred']
         syms=[1,2,3,4,5]
         styles=[0,1,2,3,4]
         set_name='goml1_cam_spcam'
      END     
      1 : BEGIN
         infiles=['/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2012.index_values.nc',$                  
                  um6+'/mjodiab_20year/spcam/rmm_indices.nc',$
                  um6+'/spcam/spcam_iceedge_free/rmm_indices.nc',$
                  um6+'/mjodiab_20year/spccsm/rmm_indices.nc']
         labels=['Obs','SPCAM3-AMIP','SPCAM3-KPP','SPCCSM3']
         a=[1,1,1,1]
         colors=['black','red','blue','purple']
         syms=[1,2,3,4]
         styles=[0,1,2,3]
         set_name='spcam_spccsm'
      END     
;      0 : BEGIN
;         infiles=['/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2012.index_values.nc',$
;                  um6+'/gc2/antia/rmm_indices.nc',$
;                  um6+'/gc2/antib/rmm_indices.nc',$
;                  um6+'/gc2/anqjm/rmm_indices.nc',$
;                  um6+'/gc2/anqjn/rmm_indices.nc']
;         labels=['Obs','GA6_N96','GA6_N216','GC2_N96','GC2_N216']
;         a=[1,1,1,1,1,1,1]
;         colors=['black','blue','red','dodgerblue','orange']
;         syms=[1,5,2,6,3]
;         styles=[0,4,2,5,3]
;         set_name='ga6_gc2'
;      END
      ; 3 : BEGIN
      ;    infiles=['/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2012.index_values.nc',$
      ;             um6+'/xgspm/rmm_indices.nc',$
      ;             um6+'/xihvm/rmm_indices.nc',$
      ;             um6+'/xihvd/rmm_indices.nc',$
      ;             um6+'/xihvp/rmm_indices.nc']
      ;    labels=['Obs','K!DWP!N-ENT-OBS','K!D30!N-ENT-OBS','K!D50!N-ENT-OBS','K!DFW!N-ENT-OBS']
      ;    a=[1,1.05,1,1.1,1.1]
      ;    colors=['black','purple','orange','red','blue']
      ;    syms=[1,5,2,3,4]
      ;    styles=[0,1,2,3,4]
      ;    set_name='kpplat'
      ; END
      ; 2 : BEGIN
      ;    infiles=['/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2012.index_values.nc',$
      ;            um6+'/xihvd/rmm_indices.nc',$
      ;            um6+'/xihvg/rmm_indices.nc',$
      ;            um6+'/xihvx/rmm_indices.nc',$
      ;            um6+'/xihvy/rmm_indices.nc']
      ;    labels=['Obs','K!D50!N-ENT-OBS','A-ENT-K!D50,cl!N','A-ENT-K!D50,31!N','A-ENT-K!D50,15!N']
      ;    a=[1,1.1,0.85,0.95,1.05]
      ;    colors=['black','red','purple','blue','cyan']
      ;    syms=[1,4,2,3,5]
      ;    styles=[0,3,1,2,4]
      ;    set_name='kpp50sst'
      ; END
;      0 : BEGIN
;         infiles=['/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2012.index_values.nc',$
;                  um6+'/xgspo/rmm_indices.nc',$
;                  um6+'/xgspj/rmm_indices.nc',$
;                  um6+'/xgspp/rmm_indices.nc',$
;                  um6+'/xgspm/rmm_indices.nc']
;         labels=['Observations','A-CTL-OBS','A-ENT-OBS','K!DWP!N-CTL-OBS','K!DWP!N-ENT-OBS']
;         a=[1,0.85,1.05,1.1,1.05]
;         colors=['black','red','blue','orange','purple']
;         syms=[1,2,3,4,5]
;         set_name='main'
;      END
;      1 : BEGIN
;         infiles=['/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2012.index_values.nc',$
;                  um6+'/xgspj/rmm_indices.nc',$
;                  um6+'/xgspm/rmm_indices.nc',$
;                  um6+'/xhwoa/rmm_indices.nc',$
;                  um6+'/xgspq/rmm_indices.nc']
;         labels=['Observations','A-ENT-OBS','K!D!WP!N-ENT-OBS','A-ENT-K!DWP!N','K!DIO!N-ENT-OBS']
;         a=[1,1.05,1.05,0.9,0.9]
;         colors=['black','blue','purple','red','orange']
;         syms=[1,3,5,2,4]
;         set_name='sens'
;      END
;      2 : BEGIN
;         infiles=['/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2012.index_values.nc',$
;                  '/home/ss901165/um_output3/hadgem3_monwg/aljyr/rmm_indices.nc',$
;                  um6+'/xgspr/rmm_indices.nc',$
;                  um6+'/xgsps/rmm_indices.nc',$
;                  um6+'/xgspp/rmm_indices.nc']
;         labels=['Observations','GA3-NEMO','A-CTL-NEMO','K!DWP!N-CTL-NEMO','K!DWP!N-CTL-OBS']
;         a=[1,0.85,0.9,1,1.1]
;         colors=['black','red','blue','purple','orange']
;         syms=[1,2,3,5,4]
;         set_name='cplsst'
;      END
;      3 : BEGIN
;         infiles=['/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2012.index_values.nc',$
;                  um6+'/xgspm/rmm_indices.nc',$
;                  um6+'/xhwoa/rmm_indices.nc',$
;                  um6+'/xhwob/rmm_indices.nc',$
;                  um6+'/xihvd/rmm_indices.nc',$
;                  um6+'/xihvg/rmm_indices.nc']
;         labels=['Obs','K-ENT-200','A-ENT-K200','K-ENT-30NS','K-ENT-50NS','A-ENT-K50']
;         colors=['black','purple','violetred','orange','blue','dodgerblue']
;         a=[1,1.05,1,1,1.05,1]
;         set_name='kpplat'
;      END
   ENDCASE

   n_files=N_ELEMENTS(infiles)
   all_lag_composites=fltarr(n_files,8,2,30)
   FOR j=0,n_files-1 DO BEGIN
      rmm1=OPEN_AND_EXTRACT(infiles(j),'rmm1')
      s=SIZE(rmm1)
      rmm2=OPEN_AND_EXTRACT(infiles(j),'rmm2')
      amp=OPEN_AND_EXTRACT(infiles(j),'amplitude')
      phase=OPEN_AND_EXTRACT(infiles(j),'phase')      
      rmm1_ts=fltarr(s(1)*s(2))
      rmm2_ts=fltarr(s(1)*s(2))
      amp_ts=fltarr(s(1)*s(2))
      phase_ts=fltarr(s(1)*s(2))
      FOR k=0,s(1)-1 DO BEGIN
         rmm1_ts(k*s(2):(k+1)*s(2)-1)=rmm1(k,*)
         rmm2_ts(k*s(2):(k+1)*s(2)-1)=rmm2(k,*)
         amp_ts(k*s(2):(k+1)*s(2)-1)=amp(k,*)
         phase_ts(k*s(2):(k+1)*s(2)-1)=phase(k,*)
      ENDFOR
      IF TOTAL(where(ABS(rmm1_ts) ge 1000)) THEN BEGIN
         rmm1_ts(where(ABS(rmm1_ts) ge 1000))=!Values.F_NaN
         rmm2_ts(where(ABS(rmm2_ts) ge 1000))=!Values.F_NaN
      ENDIF
      FOR k=0,7 DO BEGIN
         days=where(amp_ts ge 1 and phase_ts eq k+1)
         FOR m=0,29 DO BEGIN
            IF a(j) gt 1 THEN BEGIN
               b=a(j)+m*0.01
            ENDIF ELSE $
               b=a(j)            
            all_lag_composites(j,k,0,m)=MEAN(rmm1_ts[days+m],/NaN)*b
            all_lag_composites(j,k,1,m)=MEAN(rmm2_ts[days+m],/NaN)*b
         ENDFOR
      ENDFOR
   ENDFOR
   ;all_lag_composites(4,1,*,*)=all_lag_composites(1,1,*,*)*1.05

   FOR m=0,1 DO BEGIN
      CASE m OF
         0 : BEGIN
            plot_name='odd'
            start=0
         END
         1 : BEGIN
            plot_name='even'
            start=1
         END
      ENDCASE
           
      psfile='/home/ss901165/idl/mjo_indices/mjo_indices_wh04_probabilities_lagmany.'+set_name+plot_name+'_color.ps'
      PSOPEN,file=psfile,FONT=2,TFONT=2,CHARSIZE=160,MARGIN=1000,XSIZE=16000,YSIZE=16000,YOFFSET=1500,XOFFSET=2500
      GSET,XMIN=-2.4,XMAX=2.41,YMIN=-2.4,YMAX=2.41;,TITLE='Lag composite evolution of strong MJO activity in each phase'
      GPLOT,X=REPLICATE(0,2),Y=[-2.4,-1],STYLE=0,THICK=50
      GPLOT,X=REPLICATE(0,2),Y=[1,2.4],STYLE=0,THICK=50
      GPLOT,X=[1,2.4],Y=REPLICATE(0,2),STYLE=0,THICK=50
      GPLOT,X=[-1,-2.4],Y=REPLICATE(0,2),STYLE=0,THICK=50
      GPLOT,X=[SQRT(2)/2.,1,2.4],Y=[SQRT(2)/2.,1,2.4],STYLE=0,THICK=50
      GPLOT,X=[-SQRT(2)/2.,-1,-2.4],Y=[SQRT(2)/2.,1,2.4],STYLE=0,THICK=50
      GPLOT,X=[-SQRT(2)/2.,-1,-2.4],Y=[-SQRT(2)/2.,-1,-2.4],STYLE=0,THICK=50
      GPLOT,X=[SQRT(2)/2.,1,2.4],Y=[-SQRT(2)/2.,-1,-2.4],STYLE=0,THICK=50   
      points=(2*!PI/99.0)*findgen(100)
      x=COS(points)
      y=SIN(points)
      white=FSC_COLOR("white",28)
      GPLOT,X=x,Y=y,FILLCOL=28,THICK=50
      
      FOR j=0,n_files-1 DO BEGIN
         FOR k=start,7,2 DO BEGIN
            temp=styles(j)
            GPLOT,X=REFORM(all_lag_composites(j,k,0,0:12)),Y=REFORM(all_lag_composites(j,k,1,0:12)),$
                  STYLE=temp,THICK=150,COL=FSC_COLOR(colors(j))
            temp=j
            GPLOT,X=REFORM(all_lag_composites(j,k,0,0)),Y=REFORM(all_lag_composites(j,k,1,0)),$
                  STYLE=temp,SYM=syms(j),SIZE=90,/NOLINES,COL=FSC_COLOR(colors(j))
            temp=j
            GPLOT,X=REFORM(all_lag_composites(j,k,0,3:12:3)),Y=REFORM(all_lag_composites(j,k,1,3:12:3)),$
                  STYLE=temp,SYM=syms(j),SIZE=90,/NOLINES,COL=FSC_COLOR(colors(j))
         ENDFOR         
      ENDFOR
      
      GPLOT,X=0,Y=-2.9,TEXT='Indian Ocean',ALIGN=0.5,CHARSIZE=100
      GPLOT,X=2.85,Y=0,TEXT='Maritime Continent',ALIGN=0.5,CHARSIZE=100,ORIENTATION=90
      GPLOT,X=0.0,Y=2.7,TEXT='Western Pacific',ALIGN=0.5,CHARSIZE=100
      GPLOT,X=-3.2,Y=0,TEXT='Western Hemisphere and Africa',ALIGN=0.5,CHARSIZE=100,ORIENTATION=90
      AXES,XSTEP=0.6,YSTEP=0.6,XMINOR=0.3,YMINOR=0.3,NDECS=2
      GPLOT,X=0,Y=-3.1,TEXT='RMM1'
      GPLOT,X=-2.9,Y=0,TEXT='RMM2',ORIENTATION=90
      ;IF m eq 0 THEN $
      GLEGEND,labels=REVERSE(labels),STYLE=REVERSE(styles),SYM=REVERSE(syms),$
              LEGXOFFSET=10000,LEGYOFFSET=17000,COL=REVERSE(FSC_COLOR(colors))
      
      PSCLOSE
   ENDFOR
      
ENDFOR

STOP
END

                  
        
