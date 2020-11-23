PRO mjo_indices_wh04_probabilities_lagmany

n_plots=3
futureweather='/group_workspaces/jasmin/futureweather'
klingaman='/group_workspaces/jasmin2/klingaman'
gc2='/group_workspaces/jasmin2/klingaman/metum/gc2'
gc3='/group_workspaces/jasmin2/klingaman/metum/gc3'
gc4='/group_workspaces/jasmin2/klingaman/metum/gc4'
FOR i=0,n_plots-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         infiles=['/home/users/npklingaman/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2014.index_values.nc',$
                  gc4+'/u-as650/rmm_indices.nc',$
                  gc4+'/u-av802/rmm_indices.nc',$
                  gc4+'/u-au509/rmm_indices.nc',$
		  gc4+'/u-au939/rmm_indices.nc']
         labels=['Obs','GA8-N96','GA8-N216','GC4-N96','GC4-N216']
         a=[1,1,1,1,1]
         colors=['black','blue','cyan','red','orange']
         syms=[1,1,1,1,1]
         styles=[0,0,0,0,0]
         set_name='gc4'
      END
      1 : BEGIN
         infiles=['/home/users/npklingaman/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2014.index_values.nc',$
                  gc4+'/u-au509/rmm_indices.nc',$
                  gc3+'/u-ai183/rmm_indices.nc',$
		  gc4+'/u-au939/rmm_indices.nc',$
	 	  gc3+'/u-ai599/rmm_indices.nc']
         labels=['Obs','GC4-N96','GC3-N96','GC4-N216','GC3-N216']
         a=[1,1,1,1,1]
         colors=['black','red','red','orange','orange']
         syms=[1,1,2,1,2]
         styles=[0,0,1,0,1]
         set_name='gc4-vs-gc3'
      END
      2 : BEGIN
	 infiles=['/home/users/npklingaman/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2014.index_values.nc',$
		 gc4+'/u-as650/rmm_indices.nc',$
		 gc3+'/u-ab642/mjo/rmm_indices.nc',$
		 gc4+'/u-av802/rmm_indices.nc',$
		 gc3+'/u-ab680/mjo/rmm_indices.nc']
	 labels=['Obs','GA8-N96','GA7-N96','GA8-N216','GA7-N216']
	 a=[1,1,1,1,1]
	 colors=['black','blue','blue','cyan','cyan']
	 syms=[1,1,2,1,2]
	 styles=[0,0,1,0,1]
	 set_name='ga8-vs-ga7'
      END
         
;      0 : BEGIN
;         infiles=['/home/users/npklingaman/datasets/MJO_INDICES/MJO_rmm1_rmm2.dec-feb_dmeans.1975-2009.index_values.nc',$
;                  '/home/users/npklingaman/datasets/MJO_INDICES/MJO_rmm1_rmm2.mar-may_dmeans.1975-2009.index_values.nc',$
;                  '/home/users/npklingaman/datasets/MJO_INDICES/MJO_rmm1_rmm2.jun-aug_dmeans.1975-2009.index_values.nc',$
;                  '/home/users/npklingaman/datasets/MJO_INDICES/MJO_rmm1_rmm2.sep-nov_dmeans.1975-2009.index_values.nc']
;         labels=['DJF','MAM','JJA','SON']
;         a=[1.1,1,0.9,1]
;         colors=['blue','purple','red','brown']
;         syms=[1,1,1,1]
;         styles=[0,0,0,0]
;         set_name='rmm_byseason'
;      END
;      0 : BEGIN
;         infiles=['/home/users/npklingaman/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2014.index_values.nc',$
;                  gc2+'/antia/rmm_indices.nc',$
;                  gc2+'/anqjm/rmm_indices.nc',$
;                  gc3+'/u-ab642/mjo/rmm_indices.nc',$
;                  gc3+'/u-ab673/mjo/rmm_indices.nc']
;         labels=['Obs','GA6 N96','GC2 N96','GA7 N96','GC3 N96']
;         a=[1,1,1,1,1]
;         colors=['black','red','orange','blue','cyan']
;         syms=[1,2,3,4,5]
;         styles=[0,0,0,0,0]
;         set_name='gc2_gc3_n96'
;      END         
;      1 : BEGIN
;         infiles=['/home/users/npklingaman/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2014.index_values.nc',$
;                  gc2+'/antib/rmm_indices.nc',$
;                  gc2+'/anqjn/rmm_indices.nc',$
;                  gc3+'/u-ab680/mjo/rmm_indices.nc',$
;                  gc3+'/u-ab674/mjo/rmm_indices.nc']
;         labels=['Obs','GA6 N216','GC2 N216','GA7 N216','GC3 N216']
;         a=[1,1,1,1,1]
;         colors=['black','red','orange','blue','cyan']
;         syms=[1,2,3,4,5]
;         styles=[0,0,0,0,0]
;         set_name='gc2_gc3_n216'
;      END
      ;0 : BEGIN
      ;   infiles=['/home/users/npklingaman/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2012.index_values.nc',$
      ;            futureweather+'/xjhwb/rmm_indices.nc',$
      ;            futureweather+'/xjhwe/rmm_indices.nc',$
      ;            futureweather+'/xjhwh/rmm_indices.nc']
      ;   labels=['Obs','GOML1-N216','GA3_31d-N216','GA3_clim-N216']
      ;   a=[1,1.1,1,1]
      ;   colors=['black','purple','red','blue']
      ;   syms=[1,2,3,4,5]
      ;   styles=[0,0,0,0]
      ;   set_name='goml1_n216'
      ;END     
      ;1 : BEGIN
      ;   infiles=['/home/users/npklingaman/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2012.index_values.nc',$
      ;            futureweather+'/xjhwb/rmm_indices.nc',$
      ;            klingaman+'/metum/xlhvc/rmm_indices.nc',$
      ;            klingaman+'/metum/xlhvf/rmm_indices.nc',$
      ;	  klingaman+'/metum/xihvu/rmm_indices.nc']
      ;   labels=['Obs','GOML1-N216','TOML1-N216','ETOML-N216','GOML1-N216-1p5F']
      ;	 a=[1,1.1,1.1,1,1.1]
      ;  colors=['black','purple','cyan','blue','violetred']
      ; syms=[1,2,3,4,5]
      ;styles=[0,0,0,0,0]
;	 set_name='getoml_n216'
;      END
;      2 : BEGIN
;         infiles=['/home/users/npklingaman/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2012.index_values.nc',$                       ;             ;
;		  klingaman+'/cam/spcam_iceedge_free/rmm_indices.nc',$
;                  klingaman+'/cam/spcam_atm_iceedge_31day/rmm_indices.nc',$
;		  klingaman+'/cam/spcam_iceedge_free_spccsm/rmm_indices.nc',$
 ;                 klingaman+'/mjodiab_20year/spccsm/rmm_indices.nc']
  ;       labels=['Obs','SP3-K_OBS','SP3-A_KOBS','SP3-K_SPCCSM','SPCCSM3']
   ;      a=[1,1,1,1,1]
    ;     colors=['black','red','orange','blue','brown']
     ;    syms=[1,2,3,4,5]
      ;   styles=[0,1,2,3,4]
       ;  set_name='spcam_spccsm'
     ; END     
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
      IF j eq 0 THEN BEGIN
         start_year=4
      ENDIF ELSE $
         start_year=0
      FOR k=start_year,s(1)-1 DO BEGIN
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
               b=a(j)+m*0.02
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
           
      psfile='/home/users/npklingaman/plots/mjo_indices/mjo_indices_wh04_probabilities_lagmany.'+set_name+'_'+plot_name+'_color.ps'
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
      
      PSCLOSE,/NOVIEW
   ENDFOR
      
ENDFOR

STOP
END

                  
        
