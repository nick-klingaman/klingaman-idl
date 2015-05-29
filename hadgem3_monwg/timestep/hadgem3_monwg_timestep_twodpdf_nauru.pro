PRO hadgem3_monwg_timestep_twodpdf_nauru

infile='/home/ss901165/datasets/NAURU_RAINFALL/precip1min.asc'
n_steps=LONG(4070880)

precip_1min=fltarr(n_steps)
OPENR,lun,infile,/GET_LUN
FOR i=LONG(0),n_steps-1 DO BEGIN
   READF,lun,temp
   precip_1min(i)=temp
ENDFOR
precip_1min[where(precip_1min lt 0)]=!Values.F_NaN

mean_periods=[1,5,10,15,20]
n_means=N_ELEMENTS(mean_periods)
heaviside=[1,2,4,6,9,12,16,20,25,30,40,60,90,130,180]
row_map=[0]
column_map=[12,13,14,15]
n_heavi=N_ELEMENTS(heaviside)
twod_pdf_pwet=fltarr(n_heavi+1,n_heavi+1)
twod_pdf_prct=fltarr(n_heavi+1,n_heavi+1)
oned_pdf=fltarr(n_heavi+1)
oned_pdf_prct=fltarr(n_heavi+1)
intermittent_freq=0
FOR i=0,n_means-1 DO BEGIN
   n_means=LONG(n_steps/mean_periods(i))
   ts=fltarr(n_means)
   FOR j=LONG(0),n_means-1 DO $
      ts(j)=MEAN(precip_1min(j*mean_periods(i):(j+1)*mean_periods(i)-1),/NaN)   
                                ; Convert from mm/hr to mm/day
   ts=ts*12
   FOR m=0,n_heavi-1 DO BEGIN
      IF TOTAL(where(ts ge heaviside(m))) ge 0 THEN BEGIN
         valid=where(ts ge heaviside(m))
         oned_pdf(m+1)=oned_pdf(m+1)+N_ELEMENTS(valid)
         next=ts[valid+1]                
         FOR n=0,n_heavi-1 DO BEGIN
            IF TOTAL(where(next ge heaviside(n))) ge 0 THEN $
               twod_pdf_pwet(m+1,n+1)=N_ELEMENTS(where(next ge heaviside(n)))+twod_pdf_pwet(m+1,n+1)
         ENDFOR
         IF TOTAL(where(next lt heaviside(0))) ge 0 THEN $
            twod_pdf_pwet(m+1,0)=N_ELEMENTS(where(next lt heaviside(0)))+twod_pdf_pwet(m+1,0)
      ENDIF
   ENDFOR
   FOR m=0,n_heavi-2 DO BEGIN
      IF TOTAL(where(ts ge heaviside(m) and ts lt heaviside(m+1))) ge 0 THEN BEGIN
         valid=where(ts ge heaviside(m) and ts lt heaviside(m+1))
         oned_pdf_prct(m+1)=oned_pdf_prct(m+1)+N_ELEMENTS(valid)
         next=ts[valid+1]
         FOR n=0,n_heavi-2 DO BEGIN
            IF TOTAL(where(next ge heaviside(n) and next lt heaviside(n+1))) ge 0 THEN $
               twod_pdf_prct(m+1,n+1)=N_ELEMENTS(where(next ge heaviside(n) and next lt heaviside(n+1)))+twod_pdf_prct(m+1,n+1)
         ENDFOR
         IF TOTAL(where(next lt heaviside(0))) ge 0 THEN BEGIN
            twod_pdf_prct(m+1,0)=N_ELEMENTS(where(next lt heaviside(0)))+twod_pdf_prct(m+1,0)
            IF where(row_map eq m) ge 0 THEN $
               intermittent_freq=intermittent_freq+N_ELEMENTS(where(next lt heaviside(0)))
         ENDIF
         IF TOTAL(where(next ge heaviside(n_heavi-1))) ge 0 THEN $
            twod_pdf_prct(m+1,n_heavi)=N_ELEMENTS(where(next ge heaviside(n_heavi-1)))+twod_pdf_prct(m+1,n_heavi)
      ENDIF 
   ENDFOR
   IF TOTAL(where(ts lt heaviside(0))) ge 0 THEN BEGIN
      valid=where(ts lt heaviside(0))
      oned_pdf(0)=oned_pdf(0)+N_ELEMENTS(valid)
      oned_pdf_prct(0)=oned_pdf_prct(0)+N_ELEMENTS(valid)
      next=ts[valid+1]
      FOR n=0,n_heavi-1 DO BEGIN
         IF TOTAL(where(next ge heaviside(n))) ge 0 THEN $
            twod_pdf_pwet(0,n+1)=N_ELEMENTS(where(next ge heaviside(n)))+twod_pdf_pwet(0,n+1)
      ENDFOR
      FOR n=0,n_heavi-2 DO BEGIN
         IF TOTAL(where(next ge heaviside(n) and next lt heaviside(n+1))) ge 0 THEN BEGIN
            twod_pdf_prct(0,n+1)=N_ELEMENTS(where(next ge heaviside(n) and next lt heaviside(n+1)))+twod_pdf_prct(0,n+1)                
            IF where(row_map eq n) ge 0 THEN $
               intermittent_freq=intermittent_freq+N_ELEMENTS(where(next ge heaviside(n) and next lt heaviside(n+1)))
         ENDIF
      ENDFOR
      IF TOTAL(where(next lt heaviside(0))) ge 0 THEN $
         twod_pdf_prct(0,0)=N_ELEMENTS(where(next lt heaviside(0)))+twod_pdf_prct(0,0)
      IF TOTAL(where(next ge heaviside(n_heavi-1))) ge 0 THEN $
         twod_pdf_prct(0,n_heavi)=N_ELEMENTS(where(next ge heaviside(n_heavi-1)))+twod_pdf_prct(0,n_heavi)
      IF TOTAL(where(next lt heaviside(0))) ge 0 THEN $
         twod_pdf_pwet(0,0)=N_ELEMENTS(where(next lt heaviside(0)))+twod_pdf_pwet(0,0)
   ENDIF           
   IF TOTAL(where(ts ge heaviside(n_heavi-1))) ge 0 THEN BEGIN
      valid=where(ts ge heaviside(n_heavi-1))
      oned_pdf_prct(n_heavi)=oned_pdf_prct(n_heavi)+N_ELEMENTS(valid)
      next=ts[valid+1]
      FOR n=0,n_heavi-2 DO BEGIN
         IF TOTAL(where(next ge heaviside(n) and next lt heaviside(n+1))) ge 0 THEN $
            twod_pdf_prct(n_heavi,n+1)=N_ELEMENTS(where(next ge heaviside(n) and next lt heaviside(n+1)))+twod_pdf_prct(n_heavi,n+1)             
      ENDFOR
      IF TOTAL(where(next lt heaviside(0))) ge 0 THEN $
         twod_pdf_prct(n_heavi,0)=N_ELEMENTS(where(next lt heaviside(0)))+twod_pdf_prct(n_heavi,0)
      IF TOTAL(where(next ge heaviside(n_heavi-1))) ge 0 THEN $
         twod_pdf_prct(n_heavi,n_heavi)=N_ELEMENTS(where(next ge heaviside(n_heavi-1)))+twod_pdf_prct(n_heavi,n_heavi)
   ENDIF   

   FOR m=0,n_heavi DO $
       twod_pdf_pwet(m,*)=twod_pdf_pwet(m,*)/FLOAT(oned_pdf(m))
   oned_pdf=oned_pdf/(FLOAT(n_means))
   oned_pdf_prct=oned_pdf_prct/TOTAL(oned_pdf_prct)
    
   twod_pdf_prct_norm_col=fltarr(n_heavi+1,n_heavi+1)
   twod_pdf_prct_norm_row=fltarr(n_heavi+1,n_heavi+1)
   FOR m=0,n_heavi DO BEGIN
      twod_pdf_prct_norm_col(m,*)=twod_pdf_prct(m,*)/TOTAL(twod_pdf_prct(m,*))
      twod_pdf_prct_norm_row(*,m)=twod_pdf_prct(*,m)/TOTAL(twod_pdf_prct(*,m))
   ENDFOR
   twod_pdf_prct=twod_pdf_prct/TOTAL(twod_pdf_prct)
   
   mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
   mylevs_prct=['1e-5','2e-5','4e-5','7e-5','1e-4','2e-4','4e-4','7e-4','1e-3','2e-3','4e-3','7e-3','1e-2','2e-2','4e-2','7e-2','1e-1']
   mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95','1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']
   psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_twodpdf_nauru.'+STRTRIM(STRING(mean_periods(i)),1)+'mean'+'_prct.ps'
   PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=125,TCHARSIZE=100,XSIZE=16500,YSIZE=16500,SPACE2=2000,YOFFSET=1000
   GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0,YMAX=n_heavi+1
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_prct)+1,white=[2]
   LEVS,MANUAL=mylevs_prct
   CON,X=indgen(n_heavi+1)+0.5,Y=indgen(n_heavi+1)+0.5,FIELD=twod_pdf_prct,/BLOCK,/NOLINES,CB_WIDTH=135,CB_TITLE='Probability';,TITLE=psfile_title
   GPLOT,X=[0,n_heavi+1],Y=[0,n_heavi+1],STYLE=1
   AXES,XVALS=indgen(n_heavi+2),YVALS=indgen(n_heavi+2),$
        YLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
        XLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
        XTITLE='Precipitation on timestep t (mm day!U-1!N)',YTITLE='Precipitation on timestep t+1 (mm day!U-1!N)',/NORIGHT    
   GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0.001,YMAX=1,/YLOG
   GPLOT,X=indgen(n_heavi+1)+0.5,Y=oned_pdf_prct,STYLE=2
   AXES,YVALS=['0.001','0.0015','0.002','0.003','0.004','0.006','0.008','0.01','0.015','0.02','0.03','0.04','0.06','0.08','0.10','0.15','0.2','0.3','0.4','0.6','0.8','1.0'],YTITLE='Probability of precipitation in column',/ONLYRIGHT,NDECS=2
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_twodpdf_nauru.'+STRTRIM(STRING(mean_periods(i)),1)+'mean'+'_prct_norm_col.ps'
   PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=125,TCHARSIZE=110,XSIZE=16500,YSIZE=16500,SPACE2=2000,YOFFSET=1000
   GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0,YMAX=n_heavi+1
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,white=[2]
   LEVS,MANUAL=mylevs
   CON,X=indgen(n_heavi+1)+0.5,Y=indgen(n_heavi+1)+0.5,FIELD=twod_pdf_prct_norm_col,/BLOCK,/NOLINES,CB_WIDTH=135,CB_TITLE='Normalised probability (by column)'
   GPLOT,X=[0,n_heavi+1],Y=[0,n_heavi+1],STYLE=1
   AXES,XVALS=indgen(n_heavi+2),YVALS=indgen(n_heavi+2),$
        YLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
        XLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
        XTITLE='Precipitation on timestep t (mm day!U-1!N)',YTITLE='Precipitation on timestep t+1 (mm day!U-1!N)',/NORIGHT         
   GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0.001,YMAX=1,/YLOG
   GPLOT,X=indgen(n_heavi+1)+0.5,Y=oned_pdf_prct,STYLE=2
   AXES,YVALS=['0.001','0.0015','0.002','0.003','0.004','0.006','0.008','0.01','0.015','0.02','0.03','0.04','0.06','0.08','0.10','0.15','0.2','0.3','0.4','0.6','0.8','1.0'],YTITLE='Probability of precipitation in column',/ONLYRIGHT,NDECS=2
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_twodpdf_nauru.'+STRTRIM(STRING(mean_periods(i)),1)+'mean'+'_prct_norm_row.ps'
   PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=125,TCHARSIZE=110,XSIZE=16500,YSIZE=16500,SPACE2=2000,YOFFSET=1000
   GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0,YMAX=n_heavi+1
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,white=[2]
   LEVS,MANUAL=mylevs
   CON,X=indgen(n_heavi+1)+0.5,Y=indgen(n_heavi+1)+0.5,FIELD=twod_pdf_prct_norm_row,/BLOCK,/NOLINES,CB_WIDTH=135,CB_TITLE='Normalised probability (by row)'
   GPLOT,X=[0,n_heavi+1],Y=[0,n_heavi+1],STYLE=1
   AXES,XVALS=indgen(n_heavi+2),YVALS=indgen(n_heavi+2),$
        YLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
        XLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
        XTITLE='Precipitation on timestep t (mm day!U-1!N)',YTITLE='Precipitation on timestep t+1 (mm day!U-1!N)',/NORIGHT         
   GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0.001,YMAX=1,/YLOG
   GPLOT,X=indgen(n_heavi+1)+0.5,Y=oned_pdf_prct,STYLE=2
   AXES,YVALS=['0.001','0.0015','0.002','0.003','0.004','0.006','0.008','0.01','0.015','0.02','0.03','0.04','0.06','0.08','0.10','0.15','0.2','0.3','0.4','0.6','0.8','1.0'],YTITLE='Probability of precipitation in column',/ONLYRIGHT,NDECS=2
   PSCLOSE,/NOVIEW
   
   FOR m=0,n_heavi DO BEGIN
      twod_pdf_prct_norm_col(m,*)=twod_pdf_prct_norm_col(m,*)/oned_pdf_prct
      twod_pdf_prct_norm_row(*,m)=twod_pdf_prct_norm_row(*,m)/oned_pdf_prct
   ENDFOR
   
   psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_twodpdf_nauru.'+STRTRIM(STRING(mean_periods(i)),1)+'mean'+'_prct_norm_col2.ps'
   PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=125,TCHARSIZE=110,XSIZE=16500,YSIZE=16500,SPACE2=2000,YOFFSET=1000
   GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0,YMAX=n_heavi+1
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_norm)+1,white=[12]
   LEVS,MANUAL=mylevs_norm
   CON,X=indgen(n_heavi+1)+0.5,Y=indgen(n_heavi+1)+0.5,FIELD=twod_pdf_prct_norm_col,/BLOCK,/NOLINES,CB_WIDTH=135,CB_TITLE='Normalised probability (by column and PDF)'
   GPLOT,X=[0,n_heavi+1],Y=[0,n_heavi+1],STYLE=1
   AXES,XVALS=indgen(n_heavi+2),YVALS=indgen(n_heavi+2),$
        YLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
        XLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
        XTITLE='Precipitation on timestep t (mm day!U-1!N)',YTITLE='Precipitation on timestep t+1 (mm day!U-1!N)',/NORIGHT         
   GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0.001,YMAX=1,/YLOG
   GPLOT,X=indgen(n_heavi+1)+0.5,Y=oned_pdf_prct,STYLE=2
   AXES,YVALS=['0.001','0.0015','0.002','0.003','0.004','0.006','0.008','0.01','0.015','0.02','0.03','0.04','0.06','0.08','0.10','0.15','0.2','0.3','0.4','0.6','0.8','1.0'],YTITLE='Probability of precipitation in column',/ONLYRIGHT,NDECS=2
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_twodpdf_nauru.'+STRTRIM(STRING(mean_periods(i)),1)+'mean'+'_prct_norm_row2.ps'
   PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=125,TCHARSIZE=110,XSIZE=16500,YSIZE=16500,SPACE2=2000,YOFFSET=1000
   GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0,YMAX=n_heavi+1
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_norm)+1,white=[12]
   LEVS,MANUAL=mylevs_norm
   CON,X=indgen(n_heavi+1)+0.5,Y=indgen(n_heavi+1)+0.5,FIELD=twod_pdf_prct_norm_row,/BLOCK,/NOLINES,CB_WIDTH=135,CB_TITLE='Normalised probability (by row and PDF)'
   GPLOT,X=[0,n_heavi+1],Y=[0,n_heavi+1],STYLE=1
   AXES,XVALS=indgen(n_heavi+2),YVALS=indgen(n_heavi+2),$
        YLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
        XLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
        XTITLE='Precipitation on timestep t (mm day!U-1!N)',YTITLE='Precipitation on timestep t+1 (mm day!U-1!N)',/NORIGHT         
   GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0.001,YMAX=1,/YLOG
   GPLOT,X=indgen(n_heavi+1)+0.5,Y=oned_pdf_prct,STYLE=2
   AXES,YVALS=['0.001','0.0015','0.002','0.003','0.004','0.006','0.008','0.01','0.015','0.02','0.03','0.04','0.06','0.08','0.10','0.15','0.2','0.3','0.4','0.6','0.8','1.0'],YTITLE='Probability of precipitation in column',/ONLYRIGHT,NDECS=2
   PSCLOSE,/NOVIEW
ENDFOR

STOP
END


