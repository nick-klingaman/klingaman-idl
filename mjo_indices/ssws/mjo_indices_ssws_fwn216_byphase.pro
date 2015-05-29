PRO mjo_indices_ssws_fwn216_byphase

n_sets=7
leads=indgen(63)-48
nleads=N_ELEMENTS(leads)

phase_colors=['black','purple','blue','dodgerblue','cyan','orange','red','firebrick','deeppink']
twophase_colors=['black','purple','dodgerblue','orange','red']

max_ssw=100
rmm1_ssw=fltarr(n_sets,max_ssw,nleads)
rmm2_ssw=fltarr(n_sets,max_ssw,nleads)
nssw=fltarr(n_sets)   
plot_names=strarr(n_sets)

FOR i=0,n_sets-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         ssw_day=   [  9,  8,  5, 23, 26, 27, 14,  3, 13,  9, 25, 23,  1,  3, 19, 20, 16, 23, 27, 10,  9,  1, 23,  3,  1, 11,  2, 20, 29, 28, 22, 21, 18,  1,  3, 29,  1, 30]
         ssw_month= [  1,  1,  1,  2,  1,  2,  3,  3,  1,  1, 11, 12,  3,  1, 12,  1,  3, 11, 11,  1, 12,  2,  1,  2,  1,  2,  2, 12,  3, 12,  3,  1,  3,  1,  3,  2,  2,  3]
         ssw_year=  [  1,  3,  4,  5,  6,  6, 10, 11, 14, 17, 18, 22, 25, 27, 29, 30, 30, 31, 32, 32, 34, 34, 37, 38, 40, 41, 42, 46, 46, 47, 48, 50, 50, 51, 52, 54, 55, 55]
         nssw(i)=N_ELEMENTS(ssw_day)
         rmm_file='/home/linda/export/um_output/xjhwb/hadgem3kpp_nrglobal123_n216.jan-dec_dmeans.years1-60.rmm_indices.nc'
         ndays_per_year=360
         n_years=60
         psfile_name='metum-goml1_n216'
         plot_names(i)='GOML1-N216'
         true_date=0
      END
      1 : BEGIN
         ssw_day=   [ 26,  4, 27, 26,  2, 18, 16,  5,  8, 28, 16,  8,  9, 26, 18, 24,  1, 27, 30, 30,  7, 26, 26,  7,  2, 19,  8]
         ssw_month= [ 11,  2,  2, 12,  2,  3,  2,  3,  1, 12,  2,  2,  3, 11,  1,  2, 12,  2,  3,  1,  1, 11,  2,  3,  1,  2,  2]
         ssw_year=  [  1,  3,  9, 10, 11, 14, 18, 19, 20, 26, 27, 30, 34, 38, 38, 39, 40, 40, 42, 45, 47, 48, 48, 49, 52, 52, 56]
         nssw(i)=N_ELEMENTS(ssw_day)
         rmm_file='/home/linda/export/um_output/xjhwe/hadgem3a_kppnrglobal123smooth31_n216.jan-dec_dmeans.years1-60.rmm_indices.nc'
         ndays_per_year=360
         n_years=60
         psfile_name='metum-ga3-31d_n216'
         plot_names(i)='GA3_31d-N216'
         true_date=0
      END
      2 : BEGIN
         ssw_day=   [ 29, 18,  3,  4,  1, 24, 18, 30, 18, 18, 23,  2, 29, 17,  6, 24, 20, 19, 25, 16, 30, 29, 10,  5, 23]
         ssw_month= [  3,  2,  1,  3,  2,  2,  1,  3,  3,  3,  3,  2,  1,  2,  2,  2,  3,  3, 12,  3,  2,  2,  3,  1, 11]
         ssw_year=  [  0,  2,  6,  6,  7,  7,  9, 12, 20, 21, 22, 25, 27, 32, 37, 39, 42, 43, 44, 44, 45, 46, 51, 55, 56]
         nssw(i)=N_ELEMENTS(ssw_day)
         rmm_file='/home/linda/export/um_output/xjhwh/hadgem3a_nrglobal123_n216.jan-dec_dmeans.years1-60.rmm_indices.nc'         
         ndays_per_year=360
         n_years=60
         psfile_name='metum-ga3-clim_n216'
         plot_names(i)='GA3_clim-N216'
         true_date=0
      END
      3 : BEGIN
         ssw_day=   [ 29, 23, 21, 21, 24, 27,  6, 12, 12, 30, 22, 24,  7, 29, 23,  2,  4,  3, 24, 25, 13,  1, 16, 24,  3, 12, 28, 19, 11, 15, 29, 17, 11, 16, 12,  3, 19, 29, 14, 17, 23, 29, 10,  5, 13, 23, 20, 26,  6, 29, 28, 29]
         ssw_month= [ 11,  3, 12,  2,  1, 12,  2,  2,  3, 11,  2, 11,  2,  3, 11,  1,  2,  3, 12,  2,  3,  3,  1,  1,  2,  3, 11,  2, 12,  2,  1,  1,  1,  2,  3,  3,  2,  2,  3,  2,  3,  3,  3,  2,  3,  1,  3, 12,  1,  1, 12,  2]
         ssw_year=  [  0,  0,  5,  6,  7,  8,  8,  9,  9, 10, 12, 13, 13, 13, 16, 17, 17, 18, 19, 20, 21, 22, 23, 24, 25, 25, 26, 26, 29, 29, 30, 31, 32, 32, 33, 35, 37, 28, 39, 40, 41, 42, 43, 44, 44, 45, 45, 52, 54, 55, 56, 56]
         nssw(i)=N_ELEMENTS(ssw_day)
         rmm_file='/home/linda/export/um_output/xilai/process/hadgem3kpp_nrglobal123_n96_v2.jan-dec_dmeans.years1-60.rmm_indices.nc'
         ndays_per_year=360
         n_years=60
         psfile_name='metum-goml1_n96'
         plot_names(i)='GOML1-N96'
         true_date=0
      END
      4 : BEGIN
         ssw_day=   [ 22, 26, 26, 10, 23, 19, 24, 28,  3,  3, 24, 10, 23, 11, 26, 16,  3, 12, 23, 16,  9, 13,  3, 28, 25, 24, 28, 26, 21,  5, 21, 19, 22,  2, 13,  6, 13,  6,  2,  9,  1, 15, 21,  8,  1,  3, 26]
         ssw_month= [  3,  3,  2,  1, 11,  3,  1,  2,  3,  1,  2,  2,  2,  2,  1,  1,  2,  2,  1,  3,  1,  2,  2,  1,  3, 11,  3,  1,  1,  3,  2,  1,  2,  2,  1,  2,  1,  2,  2,  2,  2,  1,  2,  2,  3,  3,  2]
         ssw_year=  [  1,  3,  5,  7,  9,  9, 10, 10, 11, 13, 14, 15, 19, 20, 21, 22, 23, 24, 25, 27, 28, 28, 29, 30, 30, 31, 31, 33, 34, 34, 36, 38, 39, 40, 41, 41, 42, 42, 43, 44, 46, 47, 47, 48, 50, 53, 55]
         nssw(i)=N_ELEMENTS(ssw_day)
         rmm_file='/home/linda/export/um_output/xilas/process/hadgem3a_kppnrglobal123smooth31_n96_v2.jan-dec_dmeans.years1-60.rmm_indices.nc'
         ndays_per_year=360
         n_years=60
         psfile_name='metum-ga3-31d_n96'
         plot_names(i)='GA3_31d-N96'
         true_date=0
      END
      5 : BEGIN
         ssw_day=   [ 15, 13, 11, 17, 20, 28, 27, 22,  7, 28, 15,  2, 28, 25, 14, 20, 28, 10, 27, 25, 15,  8,  9, 21, 12, 21,  2, 11, 26, 28,  9,  7, 18, 16, 27, 23, 16, 15, 10,  3, 19, 30, 16, 18]
         ssw_month= [  2,  1,  3,  1,  2, 12,  2, 11,  1,  1,  3,  3,  1,  2,  1,  2,  2,  2, 12,  1,  2, 12,  2,  1,  1,  3,  3,  2,  1,  1,  3,  2,  2,  2,  2,  2,  1,  1,  1,  2,  2,  1,  2, 12]
         ssw_year=  [  0,  1,  2,  3,  3,  4,  5,  6,  8, 10, 10, 12, 16, 17, 18, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 27, 31, 32, 33, 34, 34, 36, 39, 40, 45, 46, 48, 49, 49, 50, 51, 53, 54, 55]
         nssw(i)=N_ELEMENTS(ssw_day)
         rmm_file='/home/linda/export/um_output/xilap/process/hadgem3a_nrglobal123_n96_v2.jan-dec_dmeans.years1-60.rmm_indices.nc'
         ndays_per_year=360
         n_years=60
         psfile_name='metum-ga3-clim_n96'
         plot_names(i)='GA3_clim-N96'
         true_date=0
      END
      6 : BEGIN
         ssw_day=   [ 53, 60, 63,338, 55,  1, 23,341, 73, 52,349, 57, 79, 42,364, 48, 18,  5, 21, 55, 53, 24, 40, 83,  6]
         ;ssw_day=   [ 22, 29,  4,  4, 24,  1, 23,  7, 14, 21, 15, 26, 20, 11, 30, 17, 18,  5, 21, 24, 22, 24,  9, 24]    ;,  6]
         ;ssw_month= [  2,  2,  3, 12,  2,  1,  1, 12,  3,  2, 12,  2,  3,  2, 12,  2,  1,  1,  1,  2,  2,  1,  2,  3] ;,  1]
         ssw_year=  [ 79, 80, 81, 81, 84, 85, 87, 87, 88, 89, 98, 99,100,101,101,102,103,104,106,107,108,109,110,110,113]-75 ;,113]
         nssw(i)=N_ELEMENTS(ssw_day)
         rmm_file='/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2014.index_values.nc'
         ndays_per_year=365
         n_years=40
         psfile_name='obs'
         plot_names(i)='Observations' 
         true_date=1
      END
   ENDCASE

   model_year=fltarr(nssw(i))     
   ;model_year=ssw_year+1
   IF true_date eq 0 THEN BEGIN      
      FOR j=0,nssw(i)-1 DO BEGIN
         IF ssw_month(j) le 3 THEN BEGIN
            model_year(j)=ssw_year(j)+1
         ENDIF ELSE $
            model_year(j)=ssw_year(j)
      ENDFOR  
      model_year[where(model_year gt 19)]=model_year[where(model_year gt 19)]+1      
      model_year[where(model_year gt 38)]=model_year[where(model_year gt 38)]+1 ; Effect is cumulative with previous command   
      model_day=(ssw_month-1)*30+ssw_day-1+model_year*ndays_per_year
   ENDIF ELSE BEGIN
      model_year=ssw_year
      model_day=model_year*ndays_per_year+ssw_day-1
   ENDELSE

   rmm1=OPEN_AND_EXTRACT(rmm_file,'rmm1')
   rmm2=OPEN_AND_EXTRACT(rmm_file,'rmm2')
   phase=OPEN_AND_EXTRACT(rmm_file,'phase')

   rmm1_ts=fltarr(n_years*ndays_per_year)
   rmm2_ts=fltarr(n_years*ndays_per_year)
   phase_ts=fltarr(n_years*ndays_per_year)
   IF true_date eq 1 THEN BEGIN
      FOR j=0,n_years-1 DO BEGIN
         rmm1_ts(j*ndays_per_year:(j+1)*ndays_per_year-1)=[REFORM(rmm1(j,0:57)),REFORM(rmm1(j,59:365))]
         rmm2_ts(j*ndays_per_year:(j+1)*ndays_per_year-1)=[REFORM(rmm2(j,0:57)),REFORM(rmm2(j,59:365))]
         phase_ts(j*ndays_per_year:(j+1)*ndays_per_year-1)=[REFORM(phase(j,0:57)),REFORM(phase(j,59:365))]
      ENDFOR
   ENDIF ELSE BEGIN
      FOR j=0,n_years-1 DO BEGIN
         rmm1_ts(j*ndays_per_year:(j+1)*ndays_per_year-1)=rmm1(j,*)
         rmm2_ts(j*ndays_per_year:(j+1)*ndays_per_year-1)=rmm2(j,*)
         phase_ts(j*ndays_per_year:(j+1)*ndays_per_year-1)=phase(j,*)
      ENDFOR
   ENDELSE

   IF TOTAL(where(rmm1_ts le -100)) gt 0 THEN $
      rmm1_ts[where(rmm1_ts le -100)]=!Values.F_NaN
   IF TOTAL(where(rmm2_ts le -100)) gt 0 THEN $
      rmm2_ts[where(rmm2_ts le -100)]=!Values.F_NaN
   IF TOTAL(where(phase_ts le -100)) gt 0 THEN $
      phase_ts[where(phase_ts le -100)]=!Values.F_NaN
   
   ;rmm1_ssw=fltarr(nssw(i),nleads)
   ;rmm2_ssw=fltarr(nssw(i),nleads)
   phase_ssw=fltarr(nssw(i),nleads)

   FOR j=0,nssw(i)-1 DO BEGIN
      FOR k=0,nleads-1 DO BEGIN
         rmm1_ssw(i,j,k)=rmm1_ts[model_day(j)+leads(k)]
         rmm2_ssw(i,j,k)=rmm2_ts[model_day(j)+leads(k)]   
         phase_ssw(j,k)=phase_ts[model_day(j)+leads(k)]
      ENDFOR
   ENDFOR

   rmm1_ssw_mean=fltarr(nleads)
   rmm2_ssw_mean=fltarr(nleads)
   FOR j=0,nleads-1 DO BEGIN
      rmm1_ssw_mean(j)=MEAN(rmm1_ssw(*,j),/NaN)
      rmm2_ssw_mean(j)=MEAN(rmm2_ssw(*,j),/NaN)
   ENDFOR
   ;print,rmm1_ssw_mean,rmm2_ssw_mean
   
   rmm_phase=fltarr(9,nleads)
   FOR j=0,nssw(i)-1 DO BEGIN
      FOR k=0,nleads-1 DO BEGIN
         IF (phase_ssw(j,k) ge 0) THEN BEGIN
            IF SQRT(rmm1_ssw(i,j,k)^2+rmm2_ssw(i,j,k)^2) lt 1 THEN BEGIN
               rmm_phase(0,k)=rmm_phase(0,k)+1
            ENDIF ELSE $
               rmm_phase(phase_ssw(j,k),k)=rmm_phase(phase_ssw(j,k),k)+1
         ENDIF
      ENDFOR
   ENDFOR
   FOR k=0,nleads-1 DO $
      rmm_phase(*,k)=rmm_phase(*,k)/FLOAT(nssw(i))

   clim_prob=fltarr(9)
   amp_ts=SQRT(rmm1_ts^2+rmm2_ts^2)
   clim_prob(0)=N_ELEMENTS(where(amp_ts lt 1))
   FOR k=1,8 DO $
      clim_prob(k)=N_ELEMENTS(where(amp_ts ge 1 and phase_ts eq k))   
   clim_prob=clim_prob/FLOAT(ndays_per_year*n_years)

   psfile='/home/ss901165/idl/mjo_indices/ssws/mjo_indices_ssws_fwn216_byphase.phase_probabilities_'+psfile_name+'.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,MARGIN=1800,YOFFSET=1000,XOFFSET=2000,XSIZE=21000,SPACE3=0,SPACE2=100
   GSET,XMIN=MIN(leads),XMAX=MAX(leads),YMIN=0.2,YMAX=5.0,/YLOG,TITLE='Relationship between SSWs (n='+STRTRIM(STRING(nssw(i)),1)+') and MJO phase in '+plot_names(i)
   FOR k=0,8 DO BEGIN
      y_toplot=fltarr(nleads/6)  
      x_toplot=fltarr(nleads/6)
      FOR j=0,N_ELEMENTS(x_toplot)-1 DO BEGIN
         y_toplot(j)=MEAN(rmm_phase(k,j*6:(j+1)*6-1))
         x_toplot(j)=MEAN(leads(j*6:(j+1)*6-1))
                                ;ENDFOR
                                ;GPLOT,X=x_toplot,Y=y_toplot/clim_prob(k),COL=FSC_COLOR(phase_colors(k)),SYM=3,SIZE=50,THICK=150      
         HIST,X=x_toplot(j)-1.0+k*0.4,Y=y_toplot(j)/clim_prob(k)-1,FILLCOL=FSC_COLOR(phase_colors(k)),WIDTH=40,OFFSET=1,/NOBORDER
      ENDFOR
   ENDFOR
   GPLOT,X=[MIN(leads),MAX(leads)],Y=[1,1],STYLE=2
   GPLOT,X=[0,0],Y=[0.2,5.0],STYLE=2
   AXES,XSTEP=6,NDECS=2,YTITLE='Frequency relative to climatology (ratio)',XTITLE='Lead-lag relative to SSW (negative leads)',$
        YVALS=['0.2','0.25','0.3','0.4','0.5','0.6','0.8','1.0','1.3','1.6','2.0','2.5','3.0','4.0','5.0'] ;,YVALS=['0.2','0.25','0.3','0.4','0.5','0.6','0.8','1.0','1.3','1.6','2.0','2.5','3.0','4.0','5.0']
   GLEGEND,labels=REVERSE(['No MJO','P1','P2','P3','P4','P5','P6','P7','P8']),COL=REVERSE(FSC_COLOR(phase_colors)),SYM=REPLICATE(1,9),LENGTH=0,LEGXOFFSET=5000,LEGYOFFSET=17000
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/mjo_indices/ssws/mjo_indices_ssws_fwn216_byphase.twophase_probabilities_'+psfile_name+'.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,MARGIN=1800,YOFFSET=1000,XOFFSET=2000,XSIZE=21000,SPACE3=0
   GSET,XMIN=MIN(leads),XMAX=MAX(leads),YMIN=0.20,YMAX=5.0,/YLOG,TITLE='Relationship between SSWs (n='+STRTRIM(STRING(nssw(i)),1)+') and MJO phase in '+plot_names(i)
   FOR k=0,4 DO BEGIN
      y_toplot=fltarr(nleads/6)  
      x_toplot=fltarr(nleads/6)
      FOR j=0,N_ELEMENTS(x_toplot)-1 DO BEGIN
         IF k gt 0 THEN BEGIN
            CASE k OF 
               1 : BEGIN
                  y_toplot(j)=MEAN(rmm_phase(2:3,j*6:(j+1)*6-1))
               END
               2 : BEGIN
                  y_toplot(j)=MEAN(rmm_phase(4:5,j*6:(j+1)*6-1))
               END
               3 : BEGIN
                  y_toplot(j)=MEAN(rmm_phase(6:7,j*6:(j+1)*6-1))
               END
               4 : BEGIN
                  y_toplot(j)=MEAN([rmm_phase(8,j*6:(j+1)*6-1),rmm_phase(1,j*6:(j+1)*6-1)])
               END
            ENDCASE
         ENDIF ELSE $
            y_toplot(j)=MEAN(rmm_phase(k,j*6:(j+1)*6-1))
         x_toplot(j)=MEAN(leads(j*6:(j+1)*6-1))
                                ;ENDFOR
                                ;GPLOT,X=x_toplot,Y=y_toplot/clim_prob(k),COL=FSC_COLOR(phase_colors(k)),SYM=3,SIZE=50,THICK=150      
         HIST,X=x_toplot(j)-1.0+k*0.8,Y=y_toplot(j)/clim_prob(k)-1,FILLCOL=FSC_COLOR(twophase_colors(k)),WIDTH=70,OFFSET=1,/NOBORDER
      ENDFOR
   ENDFOR
   GPLOT,X=[MIN(leads),MAX(leads)],Y=[1,1],STYLE=2
   GPLOT,X=[0,0],Y=[0.2,5.0],STYLE=2
   GLEGEND,labels=REVERSE(['No MJO','P2+3','P4+5','P6+7','P8+1']),COL=REVERSE(FSC_COLOR(twophase_colors)),SYM=REPLICATE(1,5),LENGTH=0,LEGXOFFSET=5000,LEGYOFFSET=17000

   AXES,XSTEP=6,NDECS=2,YTITLE='Frequency relative to climatology (ratio)',XTITLE='Lead-lag relative to SSW (negative leads)',$
        YVALS=['0.20','0.25','0.30','0.35','0.40','0.50','0.60','0.70','0.80','1.00','1.25','1.43','1.66','2.0','2.5','2.86','3.33','4.00','5.00'] ;,YVALS=['0.2','0.25','0.3','0.4','0.5','0.6','0.8','1.0','1.3','1.6','2.0','2.5','3.0','4.0','5.0']
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/mjo_indices/ssws/mjo_indices_ssws_fwn216_byphase.wh04_wheel.'+psfile_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE3=1500,XOFFSET=1500,YOFFSET=1000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
          XSIZE=17000,YSIZE=17000
   points=(2*!PI/99.0)*findgen(100)
   x=COS(points)
   y=SIN(points)
   GSET,XMIN=-3,XMAX=3,YMIN=-3,YMAX=3;,TITLE='Fractional frequency of occurrence for Wheeler and Hendon phases - '+runid
   GPLOT,X=x,Y=y,FILLCOL=28
   GPLOT,X=[0,0],Y=[-3,-1],STYLE=0,THICK=80
   GPLOT,X=[0,0],Y=[1,3],STYLE=0,THICK=80
   GPLOT,X=[1,3],Y=[0,0],STYLE=0,THICK=80
   GPLOT,X=[-1,-3],Y=[0,0],STYLE=0,THICK=80
   GPLOT,X=[SQRT(2)/2.,3],Y=[SQRT(2)/2.,3],STYLE=0,THICK=80
   GPLOT,X=[-SQRT(2)/2.,-3],Y=[SQRT(2)/2.,3],STYLE=0,THICK=80
   GPLOT,X=[-SQRT(2)/2.,-3],Y=[-SQRT(2)/2.,-3],STYLE=0,THICK=80
   GPLOT,X=[SQRT(2)/2.,3],Y=[-SQRT(2)/2.,-3],STYLE=0,THICK=80
   
   title_xpos=[-2.0,-0.15,0.15, 2.00,2.00,0.15,-0.15,-2.0]
   title_ypos=[-0.60,-2.50,-2.50,-0.60,0.50,2.50, 2.50, 0.50]
   title_align=[1.0,1.0,0.0,0.0,0.0,0.0,1.0,1.0]
   FOR j=0,7 DO BEGIN
      phase=j+1
      GPLOT,X=title_xpos(j),Y=title_ypos(j),TEXT='Phase '+STRTRIM(STRING(phase),1),ALIGN=title_align(j),CHARSIZE=130
   ENDFOR
   
   GPLOT,X=0,Y=-3.5,TEXT='Indian Ocean',ALIGN=0.5,CHARSIZE=120
   GPLOT,X=3.4,Y=0,TEXT='Maritime Continent',ALIGN=0.5,CHARSIZE=120,ORIENTATION=90
   GPLOT,X=0,Y=3.25,TEXT='Western Pacific',ALIGN=0.5,CHARSIZE=120
   GPLOT,X=-3.4,Y=0,TEXT='Western Hemisphere and Africa',ALIGN=0.5,CHARSIZE=120,ORIENTATION=90
   AXES,XSTEP=1,YSTEP=1,XMINOR=0.5,YMINOR=0.5
   GPLOT,X=0,Y=-3.75,TEXT='RMM1',ALIGN=0.5,CHARSIZE=120
   GPLOT,X=-3.65,Y=0,TEXT='RMM2',ALIGN=0.5,CHARSIZE=120,ORIENTATION=90

   CS,SCALE=2,NCOLS=nleads/6
   FOR j=0,nssw(i)-1 DO BEGIN
      consec=0
      last_plot=0
      last_rmm1=0
      last_rmm2=0      
      FOR k=0,nleads-7,6 DO BEGIN
         rmm1_window=rmm1_ssw(i,j,k:k+5)
         rmm2_window=rmm2_ssw(i,j,k:k+5)
         amp_window=SQRT(rmm1_window^2+rmm2_window^2)
         IF N_ELEMENTS(where(amp_window ge 1)) ge 5 THEN BEGIN
            GPLOT,X=MEAN(rmm1_window),Y=MEAN(rmm2_window),SYM=3,COL=k/6+2
            ;IF last_plot eq 1 THEN $
            ;   GPLOT,X=[last_rmm1,MEAN(rmm1_window)],Y=[last_rmm2,MEAN(rmm2_window)],STYLE=2
            ;last_plot=1
            ;last_rmm1=MEAN(rmm1_window)
            ;last_rmm2=MEAN(rmm2_window)
         ENDIF ;ELSE BEGIN
            ;last_plot=0
            ;last_rmm1=0
            ;last_rmm2=0
                                ;ENDELSE
      ENDFOR
   ENDFOR
   
   labels=strarr(nleads/6)
   FOR k=0,N_ELEMENTS(labels)-1 DO $
      labels(k)='Days '+STRTRIM(STRING(leads(k*6)),1)+' to '+STRTRIM(STRING(leads((k+1)*6-1)),1)
   GLEGEND,labels=REVERSE(labels),COL=REVERSE(indgen(N_ELEMENTS(labels))+2),LEGXOFFSET=8000,LEGYOFFSET=17000,SYM=REPLICATE(3,N_ELEMENTS(labels)),LENGTH=0   
   PSCLOSE,/NOVIEW
   
ENDFOR

colors=['purple','blue','firebrick','pink','cyan','red','black']
FOR k=0,nleads-7,6 DO BEGIN
   psfile='/home/ss901165/idl/mjo_indices/ssws/mjo_indices_ssws_fwn216_byphase.wh04_wheel_allmodels.lag'+STRTRIM(STRING(leads(k)),1)+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE3=1500,XOFFSET=1500,YOFFSET=1000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
          XSIZE=17000,YSIZE=17000
   points=(2*!PI/99.0)*findgen(100)
   x=COS(points)
   y=SIN(points)
   GSET,XMIN=-4,XMAX=4,YMIN=-4,YMAX=4 ;,TITLE='Fractional frequency of occurrence for Wheeler and Hendon phases - '+runid
   GPLOT,X=x,Y=y,FILLCOL=28
   GPLOT,X=[0,0],Y=[-4,-1],STYLE=0,THICK=80
   GPLOT,X=[0,0],Y=[1,4],STYLE=0,THICK=80
   GPLOT,X=[1,4],Y=[0,0],STYLE=0,THICK=80
   GPLOT,X=[-1,-4],Y=[0,0],STYLE=0,THICK=80
   GPLOT,X=[SQRT(2)/2.,4],Y=[SQRT(2)/2.,4],STYLE=0,THICK=80
   GPLOT,X=[-SQRT(2)/2.,-4],Y=[SQRT(2)/2.,4],STYLE=0,THICK=80
   GPLOT,X=[-SQRT(2)/2.,-4],Y=[-SQRT(2)/2.,-4],STYLE=0,THICK=80
   GPLOT,X=[SQRT(2)/2.,4],Y=[-SQRT(2)/2.,-4],STYLE=0,THICK=80
   
   title_xpos=[-2.5,-0.15,0.15, 2.50,2.50,0.15,-0.15,-2.5]
   title_ypos=[-0.60,-3.00,-3.00,-0.60,0.50,3.00, 3.00, 0.50]
   title_align=[1.0,1.0,0.0,0.0,0.0,0.0,1.0,1.0]
   FOR j=0,7 DO BEGIN
      phase=j+1
      GPLOT,X=title_xpos(j),Y=title_ypos(j),TEXT='Phase '+STRTRIM(STRING(phase),1),ALIGN=title_align(j),CHARSIZE=140
   ENDFOR
   
   GPLOT,X=0,Y=-4.7,TEXT='Indian Ocean',ALIGN=0.5,CHARSIZE=120
   GPLOT,X=4.6,Y=0,TEXT='Maritime Continent',ALIGN=0.5,CHARSIZE=120,ORIENTATION=90
   GPLOT,X=0,Y=4.25,TEXT='Western Pacific',ALIGN=0.5,CHARSIZE=120
   GPLOT,X=-4.6,Y=0,TEXT='Western Hemisphere and Africa',ALIGN=0.5,CHARSIZE=120,ORIENTATION=90
   AXES,XSTEP=1,YSTEP=1,XMINOR=0.5,YMINOR=0.5
   GPLOT,X=0,Y=-4.95,TEXT='RMM1',ALIGN=0.5,CHARSIZE=120
   GPLOT,X=-4.95,Y=0,TEXT='RMM2',ALIGN=0.5,CHARSIZE=120,ORIENTATION=90
   
   FOR i=0,n_sets-1 DO BEGIN
      FOR j=0,nssw(i)-1 DO BEGIN
         rmm1_window=rmm1_ssw(i,j,k:k+5)
         rmm2_window=rmm2_ssw(i,j,k:k+5)
         amp_window=SQRT(rmm1_window^2+rmm2_window^2)
         IF N_ELEMENTS(where(amp_window ge 1)) ge 5 THEN $
            GPLOT,X=MEAN(rmm1_window),Y=MEAN(rmm2_window),SYM=3,COL=FSC_COLOR(colors(i))
      ENDFOR
   ENDFOR     
   GLEGEND,labels=REVERSE(plot_names),COL=REVERSE(FSC_COLOR(colors)),SYM=REPLICATE(3,n_sets),LEGXOFFSET=8000,LEGYOFFSET=17000,LENGTH=0
   PSCLOSE,/NOVIEW
ENDFOR

STOP
END
