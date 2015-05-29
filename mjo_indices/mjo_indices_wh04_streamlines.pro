PRO mjo_indices_wh04_streamlines,input_file,runid,grid_spacing,lags

; Read RMM1 and RMM2
rmm1=OPEN_AND_EXTRACT(input_file,'rmm1')
rmm2=OPEN_AND_EXTRACT(input_file,'rmm2')

s=SIZE(rmm1)
nyear=MIN([s(1),s(2)])
nday=MAX([s(1),s(2)])
IF s(0) eq 2 THEN BEGIN
   rmm1_ts=fltarr(s(4))
   rmm2_ts=fltarr(s(4))
   FOR i=0,nyear-1 DO BEGIN
      FOR j=0,nday-1 DO BEGIN
         IF s(2) gt s(1) THEN BEGIN
            rmm1_ts(i*nday+j)=rmm1(i,j)
            rmm2_ts(i*nday+j)=rmm2(i,j)
         ENDIF ELSE BEGIN
            rmm1_ts(i*nday+j)=rmm1(j,i)
            rmm2_ts(i*nday+j)=rmm2(j,i)
         ENDELSE
      ENDFOR
   ENDFOR
ENDIF

rmm1_ts[where(rmm1_ts lt -900)]=!Values.F_NaN
rmm2_ts[where(rmm2_ts lt -900)]=!Values.F_NaN
rmm_min=-3.
rmm_max=3.
rmm_bins=findgen((rmm_max-rmm_min)/grid_spacing+1)*grid_spacing+rmm_min
n_bins=N_ELEMENTS(rmm_bins)

n_lags=N_ELEMENTS(lags)

trajectory=fltarr(n_bins,n_bins,n_lags,2)
n_pts=fltarr(n_bins,n_bins)
transitions=fltarr(n_bins,n_bins,4)
n_transitions=fltarr(n_bins,n_bins)
FOR i=1,n_bins-2 DO BEGIN
   FOR j=1,n_bins-2 DO BEGIN
      IF ABS(rmm_bins(i)) ge 0.5 or ABS(rmm_bins(j)) ge 0.5 THEN BEGIN
                                ;pts=where(rmm1_ts ge rmm_bins(i) and rmm1_ts le rmm_bins(i+1) and $
                                ;          rmm2_ts ge rmm_bins(j) and rmm2_ts le rmm_bins(j+1))
         pts=where(SQRT((rmm1_ts-rmm_bins(i))^2+(rmm2_ts-rmm_bins(j))^2) le 1)
         n_pts(i,j)=N_ELEMENTS(pts)
         
         IF n_pts(i,j) gt 1 THEN BEGIN
            next=pts(0)-1
            FOR k=0,n_pts(i,j)-1 DO BEGIN
               IF pts(k) gt next THEN BEGIN
                  flag=0
                  m=0
                  WHILE flag ne 1 and m+pts(k) lt s(4) DO BEGIN
                     IF SQRT((rmm1_ts(pts(k)+m)-rmm_bins(i))^2+(rmm2_ts(pts(k)+m)-rmm_bins(j))^2) gt 1 THEN BEGIN
                        IF rmm1_ts(pts(k)+m)-rmm_bins(i) lt 0 and rmm2_ts(pts(k)+m)-rmm_bins(j) lt 0 THEN $
                           transitions(i,j,0)=transitions(i,j,0)+1
                        IF rmm1_ts(pts(k)+m)-rmm_bins(i) lt 0 and rmm2_ts(pts(k)+m)-rmm_bins(j) gt 0 THEN $
                           transitions(i,j,1)=transitions(i,j,1)+1
                        IF rmm1_ts(pts(k)+m)-rmm_bins(i) gt 0 and rmm2_ts(pts(k)+m)-rmm_bins(j) gt 0 THEN $
                           transitions(i,j,2)=transitions(i,j,2)+1
                        IF rmm1_ts(pts(k)+m)-rmm_bins(i) gt 0 and rmm2_ts(pts(k)+m)-rmm_bins(j) lt 0 THEN $
                           transitions(i,j,3)=transitions(i,j,3)+1                  
                        flag=1
                     ENDIF
                     m=m+1
                     IF m eq 100 THEN BEGIN
                        print,'Error - stuck in transition loop'
                        STOP
                     ENDIF
                  ENDWHILE
                  next=m+pts(k)
               ENDIF
            ENDFOR
            n_transitions(i,j)=TOTAL(transitions(i,j,*))
            IF n_transitions(i,j) ne 0 THEN $
               transitions(i,j,*)=transitions(i,j,*)/FLOAT(n_transitions(i,j))
         ENDIF
      ENDIF ELSE $
         n_pts(i,j)=0.
      ;print,rmm_bins(i),rmm_bins(j),n_pts(i,j)
               
;      IF TOTAL(pts) ge 0 THEN BEGIN
;         FOR k=0,n_lags-1 DO BEGIN
;            lag_pts=pts+lags(k)
;            IF TOTAL(where(lag_pts ge s(4))) ge 0 THEN $
;               lag_pts[where(lag_pts ge s(4))]=-999
;            IF TOTAL(where(lag_pts lt 120)) ge 0 THEN $
;               lag_pts[where(lag_pts lt 120)]=-999
;            IF N_ELEMENTS(where(lag_pts eq -999)) ne N_ELEMENTS(lag_pts) THEN BEGIN
;               trajectory(i,j,k,0)=MEAN(rmm1_ts[lag_pts(where(lag_pts ne -999))],/NaN)
;               trajectory(i,j,k,1)=MEAN(rmm2_ts[lag_pts(where(lag_pts ne -999))],/NaN)
;            ENDIF            
;         ENDFOR
;      ENDIF ELSE BEGIN
;         trajectory(i,j,*,0)=(rmm_bins(i)+rmm_bins(i+1))/2.
;         trajectory(i,j,*,1)=(rmm_bins(j)+rmm_bins(j+1))/2.
;      ENDELSE
   ENDFOR   
ENDFOR

;FOR i=0,n_bins-2 DO BEGIN
;   FOR j=0,n_bins-2 DO BEGIN
;     FOR k=0,n_lags-1 DO BEGIN
;         IF trajectory(i,j,k,0) gt rmm_bins(i+1) or trajectory(i,j,k,1) gt rmm_bins(j+1) or $
;            trajectory(i,j,k,0) lt rmm_bins(i) or trajectory(i,j,k,1) lt rmm_bins(j) THEN BEGIN
;            new_i_bin=NEAREST(rmm_bins,trajectory(i,j,k,0))
;            new_j_bin=NEAREST(rmm_bins,trajectory(i,j,k,1))
;            lag_rmse=SQRT((trajectory(i,j,k,0)-trajectory(new_i_bin,new_j_bin,*,0))^2+$
;                          (trajectory(i,j,k,1)-trajectory(new_i_bin,new_j_bin,*,1))^2)
;            matching_lag=NEAREST(lag_rmse,MIN(lag_rmse))
;            trajectory(i,j,k,*)=trajectory(new_i_bin,new_j_bin,matching_lag,*)
;         ENDIF
;      ENDFOR
;   ENDFOR
;ENDFOR   

psfile='/home/ss901165/idl/mjo_indices/mjo_indices_wh04_streamlines.'+runid+'_grid'+STRMID(STRTRIM(STRING(grid_spacing),1),0,4)+'.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE3=1500,XOFFSET=1500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
       XSIZE=17000,YSIZE=17000
GSET,XMIN=rmm_min,XMAX=rmm_max,YMIN=rmm_min,YMAX=rmm_max,$
     TITLE='Streamlines of MJO activity for '+runid+' with grid spacing '+STRMID(STRTRIM(STRING(grid_spacing),1),0,4)+' RMM units'
GPLOT,X=[0,0],Y=[rmm_min,-1],STYLE=1,THICK=60
GPLOT,X=[0,0],Y=[1,rmm_max],STYLE=1,THICK=60
GPLOT,X=[1,rmm_max],Y=[0,0],STYLE=1,THICK=60
GPLOT,X=[rmm_min,1],Y=[0,0],STYLE=1,THICK=60
GPLOT,X=[SQRT(2)/2.,rmm_max],Y=[SQRT(2)/2.,rmm_max],STYLE=1,THICK=60
GPLOT,X=[-SQRT(2)/2.,rmm_min],Y=[SQRT(2)/2.,rmm_max],STYLE=1,THICK=60
GPLOT,X=[-SQRT(2)/2.,rmm_min],Y=[-SQRT(2)/2.,rmm_min],STYLE=1,THICK=60
GPLOT,X=[SQRT(2)/2.,rmm_max],Y=[-SQRT(2)/2.,rmm_min],STYLE=1,THICK=60

circle_points=(2*!PI/99.0)*findgen(100)
GPLOT,X=COS(circle_points),Y=SIN(circle_points),FILLCOL=FSC_COLOR('white')

CS,SCALE=28,NCOLS=8
FOR i=1,n_bins-2 DO BEGIN
   FOR j=1,n_bins-2 DO BEGIN
      IF rmm_bins(i) le 0 THEN BEGIN
         IF rmm_bins(j) le 0 THEN BEGIN
            IF ABS(rmm_bins(i)) gt ABS(rmm_bins(j)) THEN BEGIN
               color=2
            ENDIF ELSE color=3
         ENDIF ELSE BEGIN
            IF ABS(rmm_bins(i)) gt ABS(rmm_bins(j)) THEN BEGIN
               color=4
            ENDIF ELSE color=5
         ENDELSE
      ENDIF ELSE BEGIN
         IF rmm_bins(j) le 0 THEN BEGIN
            IF ABS(rmm_bins(i)) gt ABS(rmm_bins(j)) THEN BEGIN
               color=6
            ENDIF ELSE color=7
         ENDIF ELSE BEGIN
            IF ABS(rmm_bins(i)) gt ABS(rmm_bins(j)) THEN BEGIN
               color=8
            ENDIF ELSE color=9
         ENDELSE
      ENDELSE

      IF ABS(rmm_bins(i)) ge 0.5 or ABS(rmm_bins(j)) ge 0.5 THEN BEGIN
         GPLOT,X=rmm_bins(i),Y=rmm_bins(j),SIZE=60*n_pts(i,j)/TOTAL(n_pts)*n_bins^2+35,SYM=3,COL=color
         IF transitions(i,j,0) gt 0 THEN $
            GPLOT,X=[rmm_bins(i),rmm_bins(i)-grid_spacing/2.],Y=[rmm_bins(j),rmm_bins(j)-grid_spacing/2.],COL=color,THICK=transitions(i,j,0)*400.
         IF transitions(i,j,1) gt 0 THEN $
            GPLOT,X=[rmm_bins(i),rmm_bins(i)-grid_spacing/2.],Y=[rmm_bins(j),rmm_bins(j)+grid_spacing/2.],COL=color,THICK=transitions(i,j,1)*400.      
         IF transitions(i,j,2) gt 0 THEN $
            GPLOT,X=[rmm_bins(i),rmm_bins(i)+grid_spacing/2.],Y=[rmm_bins(j),rmm_bins(j)+grid_spacing/2.],COL=color,THICK=transitions(i,j,2)*400.      
         IF transitions(i,j,3) gt 0 THEN $
            GPLOT,X=[rmm_bins(i),rmm_bins(i)+grid_spacing/2.],Y=[rmm_bins(j),rmm_bins(j)-grid_spacing/2.],COL=color,THICK=transitions(i,j,3)*400.
      ENDIF

      multiples=[0.25,0.5,0.75,1.0,1.5,2.0,3.0,4.0]      
      sizes=FLOOR(60*multiples+35)      
      GLEGEND_NEW,labels=[STRMID(STRTRIM(STRING(multiples),1),0,4)],$
                  SYM=[3,3,3,3,3,3,3,3],SIZE=sizes,LEGXOFFSET=5000,LEGYOFFSET=10000,LENGTH=0
      
      GLEGEND_NEW,labels=['100','200'],SYM=[3,3],SIZE=[100,200],LEGPOS=1

      GLEGEND_NEW,labels=['0.125','0.25','0.375','0.50','0.625','0.75','0.875','1.00'],$
                  THICK=[0.125*400,0.25*400,0.375*400,0.5*400,0.625*400,0.75*400,0.875*400,1.00*400],LEGXOFFSET=5000,LEGYOFFSET=17000
     
;      IF trajectory(i,j,where(lags eq 0),0) le 0 THEN BEGIN
;         IF trajectory(i,j,where(lags eq 0),1) le 0 THEN BEGIN
;            IF ABS(trajectory(i,j,where(lags eq 0),0)) gt ABS(trajectory(i,j,where(lags eq 0),1)) THEN BEGIN
;               color=2
;            ENDIF ELSE color=3
;         ENDIF ELSE BEGIN
;            IF ABS(trajectory(i,j,where(lags eq 0),0)) gt ABS(trajectory(i,j,where(lags eq 0),1)) THEN BEGIN
;               color=4
;            ENDIF ELSE color=5
;         ENDELSE
;      ENDIF ELSE BEGIN
;         IF trajectory(i,j,where(lags eq 0),1) le 0 THEN BEGIN
;            IF ABS(trajectory(i,j,where(lags eq 0),0)) gt ABS(trajectory(i,j,where(lags eq 0),1)) THEN BEGIN
;               color=6
;            ENDIF ELSE color=7            
;         ENDIF ELSE BEGIN
;            IF ABS(trajectory(i,j,where(lags eq 0),0)) gt ABS(trajectory(i,j,where(lags eq 0),1)) THEN BEGIN
;               color=8
;            ENDIF ELSE color=9
;         ENDELSE
;      ENDELSE
;      GPLOT,X=REFORM(trajectory(i,j,*,0)),Y=REFORM(trajectory(i,j,*,1)),THICK=n_pts(i,j)/(s(4))*3000.,$
;            COL=color
;      IF trajectory(i,j,where(lags eq 0),0) ne trajectory(i,j,1,0) THEN $
;         GPLOT,X=REFORM(trajectory(i,j,where(lags eq 0),0)),Y=REFORM(trajectory(i,j,where(lags eq 0),1)),SIZE=50,SYM=3,COL=color

   ENDFOR
ENDFOR
AXES,XSTEP=0.5,YSTEP=0.5,XMINOR=0.25,YMINOR=0.25,XTITLE='RMM1',YTITLE='RMM2',NDECS=2

PSCLOSE

STOP
END
