PRO mjo_indices_delta_angle

n_sets=5
delta_time=15
CASE delta_time OF
   10 : BEGIN
      angle_bins=[-70,-50,-30,-10,10,30,50,70,90,110,130,150,170,190]
      raw_ymax=0.20
      raw_ystep=0.02
   END
   5 : BEGIN
      angle_bins=[-65,-55,-45,-35,-25,-15,-5,5,15,25,35,45,55,65,75,85,95]
      raw_ymax=0.15
      raw_ystep=0.02      
   END
   15 : BEGIN
      angle_bins=[-105,-75,-45,-15,15,45,75,105,135,165,195,225,255]     
      raw_ymax=0.18
      raw_ystep=0.02
   END
ENDCASE

n_bins=N_ELEMENTS(angle_bins)
all_labels=strarr(n_sets)
all_colors=strarr(n_sets)
all_styles=strarr(n_sets)
all_syms=strarr(n_sets)
all_binned_angle=fltarr(n_sets,n_bins+1)
a=fltarr(n_sets)
FOR i=0,n_sets-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         infile='/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2012.index_values.nc'
         all_labels(i)='Obs'
         all_colors(i)='black'
         all_styles(i)=0
         all_syms(i)=0
         n_years=34
         year_offset=4
         n_days_per_year=365
         a(i)=0
      END
      1 : BEGIN
         infile='/home/ss901165/um_output6/xgspo/rmm_indices.nc'
         all_labels(i)='A-CTL'
         all_colors(i)='red'
         all_styles(i)=1
         all_syms(i)=4
         n_years=20
         year_offset=0
         n_days_per_year=360
         a(i)=0
      END
      2 : BEGIN
         infile='/home/ss901165/um_output6/xgspj/rmm_indices.nc'
         all_labels(i)='A-ENT'
         all_colors(i)='blue'
         all_styles(i)=1
         all_syms(i)=3
         n_years=20
         year_offset=0
         n_days_per_year=360
         a(i)=0.1
      END
      3 : BEGIN
         infile='/home/ss901165/um_output6/xgspp/rmm_indices.nc'
         all_labels(i)='K-CTL'
         all_colors(i)='orange'
         all_styles(i)=2
         all_syms(i)=4
         n_years=20
         year_offset=0
         n_days_per_year=360
         a(i)=0.3
      END
      4 : BEGIN
         infile='/home/ss901165/um_output6/xgspm/rmm_indices.nc'
         all_labels(i)='K-ENT-200'
         all_colors(i)='purple'
         all_styles(i)=2
         all_syms(i)=3
         n_years=20
         year_offset=0
         n_days_per_year=360
         a(i)=0.6
      END      
      5 : BEGIN
         infile='/home/ss901165/um_output6/xgspq/rmm_indices.nc'
         all_labels(i)='K-105E'
         all_colors(i)='brown'
         n_years=20
         year_offset=0
         n_days_per_year=360
         a(i)=0
      END
      6 : BEGIN
         infile='/home/ss901165/um_output6/xgspr/rmm_indices.nc'
         all_labels(i)='A-CPL'
         all_colors(i)='dodgerblue'
         year_offset=0
         n_years=20
         n_days_per_year=360
         a(i)=0
      END
      7 : BEGIN
         infile='/home/ss901165/um_output6/xgsps/rmm_indices.nc'
         all_labels(i)='K-CPL'
         all_colors(i)='cyan'
         year_offset=0
         n_years=20
         n_days_per_year=360
         a(i)=0.2
      END
   ENDCASE
   
   rmm1_ts=fltarr(n_years*n_days_per_year)
   rmm2_ts=fltarr(n_years*n_days_per_year)
   delta_angle=fltarr(n_years*n_days_per_year)
   FOR j=0,n_years-1 DO BEGIN
      rmm1_ts(j*n_days_per_year:(j+1)*n_days_per_year-1)=$
         OPEN_AND_EXTRACT(infile,'rmm1',$
                          offset=[year_offset+j,0],count=[1,n_days_per_year])
      rmm2_ts(j*n_days_per_year:(j+1)*n_days_per_year-1)=$
         OPEN_AND_EXTRACT(infile,'rmm2',$
                          offset=[year_offset+j,0],count=[1,n_days_per_year])
   ENDFOR
   amp_ts=(rmm1_ts^2+rmm2_ts^2)^(0.5)
   FOR j=0,n_days_per_year*n_years-delta_time-1 DO BEGIN
      IF amp_ts(j) ge 1 THEN BEGIN
         local_delta=fltarr(delta_time)
         FOR k=0,delta_time-1 DO BEGIN
            angle=fltarr(2)
            FOR m=0,1 DO BEGIN
               angle(m)=ATAN(rmm2_ts(j+k+m)/rmm1_ts(j+k+m))
               IF rmm2_ts(j+k+m) lt 0 and rmm1_ts(j+k+m) lt 0 THEN BEGIN
                  angle(m)=angle(m)+!Pi
               ENDIF ELSE IF rmm2_ts(j+k+m) lt 0 and rmm1_ts(j+k+m) ge 0 THEN BEGIN
                  angle(m)=angle(m)+2*!Pi
               ENDIF ELSE IF rmm2_ts(j+k+m) ge 0 and rmm1_ts(j+k+m) lt 0 THEN $
                  angle(m)=angle(m)+!Pi
               angle(m)=angle(m)*180./!Pi
            ENDFOR
            local_delta(k)=angle(1)-angle(0)
            IF local_delta(k) lt -180 THEN $
               local_delta(k)=local_delta(k)+360.            
         ENDFOR
         delta_angle(j)=TOTAL(local_delta)
      ENDIF ELSE $
         delta_angle(j)=!Values.F_NaN
   ENDFOR

   temp=delta_angle[where(FINITE(delta_angle) eq 1)]
   FOR j=0,n_bins-2 DO $
      IF TOTAL(where(temp ge angle_bins(j) and temp lt angle_bins(j+1))) ge 0 THEN $
         all_binned_angle(i,j+1)=N_ELEMENTS(where(temp ge angle_bins(j) and temp lt angle_bins(j+1)))
   IF TOTAL(where(temp lt angle_bins(0))) ge 0 THEN $
      all_binned_angle(i,0)=N_ELEMENTS(where(temp lt angle_bins(0)))
   IF TOTAL(where(temp ge angle_bins(n_bins-1))) ge 0 THEN $
      all_binned_angle(i,n_bins)=N_ELEMENTS(where(temp ge angle_bins(n_bins-1)))
   all_binned_angle(i,*)=all_binned_angle(i,*)/FLOAT(N_ELEMENTS(temp))  
ENDFOR

psfile='/home/ss901165/idl/mjo_indices/mjo_indices_delta_angle.'+STRTRIM(STRING(delta_time),1)+'days_pdf.ps'
PSOPEN,file=psfile,TFONT=2,FONT=2,CHARSIZE=130,MARGIN=5500,XOFFSET=-2300,YSIZE=13000
GSET,XMIN=0,XMAX=n_bins+1,YMIN=0,YMAX=raw_ymax
FOR i=0,n_sets-1 DO $
   GPLOT,X=indgen(n_bins+1)+0.5,Y=REFORM(all_binned_angle(i,*))-a(i)*(REFORM(all_binned_angle(i,*)-all_binned_angle(0,*))),$
         STYLE=all_styles(i),SYM=all_syms(i),THICK=120
         ;COL=FSC_COLOR(all_colors(i)),THICK=100
GPLOT,X=[NEAREST(angle_bins,0),NEAREST(angle_bins,0)]+1.5,Y=[0,raw_ymax],STYLE=2
AXES,XVALS=indgen(n_bins+2),XLABELS=['< '+STRMID(STRTRIM(STRING(angle_bins(0)),1),0,4),$
                                     STRMID(STRTRIM(STRING(angle_bins),1),0,4),$
                                     '>= '+STRMID(STRTRIM(STRING(angle_bins(n_bins-1)),1),0,3)],YSTEP=raw_ystep,YMINOR=raw_ystep/2.,$
     XTITLE='Change in phase angle over '+STRTRIM(STRING(delta_time),1)+' days (degrees, positive anti-clockwise)',YTITLE='Probability',NDECS=2
GLEGEND,labels=REVERSE([all_labels]),SYM=REVERSE([all_syms]),STYLE=REVERSE([all_styles]),LEGXOFFSET=5700,LEGYOFFSET=12000,LENGTH=80
PSCLOSE

STOP
END

