PRO hadgem3kpp_nao_mjo_probabilities

nmodels=3

all_names=strarr(nmodels)
all_colors=strarr(nmodels)
all_symbols=intarr(nmodels)
all_styles=intarr(nmodels)
all_mynyears=intarr(nmodels)

nao_thresh=1.0
nphases=9
lags=[3,6,9,12,15,18,21,24]
nlags=N_ELEMENTS(lags)
naop_bymjo=fltarr(nmodels,nphases,nlags)
naon_bymjo=fltarr(nmodels,nphases,nlags)

phase_colors=['black','purple','blue','dodgerblue','cyan','orange','red','firebrick','deeppink']
twophase_colors=['black','purple','dodgerblue','orange','red']

FOR m=2,nmodels-1 DO BEGIN
   CASE m OF
      0 : BEGIN         
         nao_file='/home/ss901165/datasets/ERA-INTERIM/MSL/ERA-Interim.jan-dec_dmeans_ts.1979-2013.nao_index_n216.nc'
         ntime=35*365
         mjo_file='/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2014.index_values.nc'
         mjo_offset=4
         all_names(m)='ERA-Int'
         all_colors(m)='black'
         all_symbols(m)=2
         all_styles(m)=0
         days=[0,365]
         ndays_per_year=365
      END
      1 : BEGIN         
         nao_file='/home/ss901165/um_output6/xjhwb/hadgem3kpp_nrglobal_n216.jan-dec_dmeans_ts.years1-106.nao_index_goml.nc'
         ntime=LONG(106)*LONG(360)
         mjo_file='/home/ss901165/um_output6/xjhwb/hadgem3kpp_nrglobal_n216.jan-dec_dmeans.years1-106.rmm_indices.nc'
         mjo_offset=0
         all_names(m)='goml1-n216'
         all_colors(m)='purple'
         all_symbols(m)=2
         all_styles(m)=0
         days=[0,360]
         ndays_per_year=360
      END
      2 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvp/hadgem3kpp_fwgbl_1.5xentrain_ga30_n96.jan-dec_dmeans_ts.years1-69.nao_index_goml_1p5.nc'
         ntime=LONG(24)*LONG(360)
         mjo_file='/home/ss901165/um_output6/xihvp/hadgem3kpp_fwgbl_1.5xentrain_ga30.jan-dec_dmeans.years1-25.rmm_indices.nc'
         mjo_offset=2
         all_names(m)='goml1-n96-1p5ent'
         all_colors(m)='violetred'
         all_symbols(m)=2
         all_styles(m)=0
         days=[0,360]
         ndays_per_year=360
      END
   ENDCASE
   
   nao_index=OPEN_AND_EXTRACT(nao_file,'nao_index',offset=[0],count=[ntime])
   nyears=ntime/ndays_per_year
   ndpy=days(1)-days(0)
   
   IF days(1) le 360 THEN BEGIN
      my_nyears=nyears
   ENDIF ELSE $
      my_nyears=nyears-1
   all_mynyears(m)=my_nyears
   
   nao_index_subset=fltarr(ndpy)
   nao_index_allyears=fltarr(ndpy*my_nyears)
   FOR i=0,my_nyears-1 DO BEGIN
      nao_index_subset(*)=nao_index(LONG(i)*LONG(ndays_per_year)+LONG(days(0)):LONG(i)*LONG(ndays_per_year)+LONG(days(1))-LONG(1))
      nao_index_allyears(LONG(i)*LONG(ndpy):LONG((i+1))*LONG(ndpy)-LONG(1))=nao_index_subset
   ENDFOR   
   nao_index_allyears=nao_index_allyears-MEAN(nao_index_allyears)
   nao_index_allyears=nao_index_allyears/STDDEV(nao_index_allyears)
   
   amp=OPEN_AND_EXTRACT(mjo_file,'amplitude')
   phase=OPEN_AND_EXTRACT(mjo_file,'phase')
   mjo_amplitude_allyears=fltarr(ndays_per_year*my_nyears)
   mjo_phase_allyears=fltarr(ndays_per_year*my_nyears)
   FOR i=0,my_nyears-1 DO BEGIN
      temp=REFORM(amp(mjo_offset+i,*))
      IF m eq 0 THEN BEGIN
         mjo_amplitude_allyears(i*ndays_per_year:(i+1)*ndays_per_year-1)=[temp(0:58),temp(60:365)]
      ENDIF ELSE $
         mjo_amplitude_allyears(LONG(i)*LONG(ndays_per_year):LONG((i+1))*LONG(ndays_per_year)-1)=temp
      temp=REFORM(phase(mjo_offset+i,*))      
      IF m eq 0 THEN BEGIN
         mjo_phase_allyears(i*ndays_per_year:(i+1)*ndays_per_year-1)=[temp(0:58),temp(60:365)]
      ENDIF ELSE $
         mjo_phase_allyears(LONG(i)*LONG(ndays_per_year):LONG((i+1))*LONG(ndays_per_year)-1)=temp
   ENDFOR

   naop_prob=N_ELEMENTS(where(nao_index_allyears gt nao_thresh))/FLOAT(ntime)
   naon_prob=N_ELEMENTS(where(nao_index_allyears lt (-1.0)*nao_thresh))/FLOAT(ntime)
   
   FOR p=0,8 DO BEGIN
      IF p eq 0 THEN BEGIN
          this_phase=where(mjo_amplitude_allyears lt 1.0)
      ENDIF ELSE $
         this_phase=where(mjo_amplitude_allyears ge 1.0 and mjo_phase_allyears eq p)
      FOR i=0,nlags-1 DO BEGIN
         nao_lagged=nao_index_allyears[this_phase+lags(i)]
         naop_bymjo(m,p,i)=N_ELEMENTS(where(nao_lagged ge nao_thresh))/FLOAT(N_ELEMENTS(this_phase))/naop_prob
         naon_bymjo(m,p,i)=N_ELEMENTS(where(nao_lagged le (-1.0)*nao_thresh))/FLOAT(N_ELEMENTS(this_phase))/naon_prob
      ENDFOR
   ENDFOR
    
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/nao/hadgem3kpp_nao_mjo_probabilities.'+all_names(m)+'_naop_thresh'+STRMID(STRTRIM(STRING(nao_thresh),1),0,3)+'.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,MARGIN=2000,XOFFSET=1500,XPLOTS=2,YPLOTS=4,YSPACING=1500
   phase=[0,4,3,2,1,8,7,6,5]
   FOR i=1,8 DO BEGIN
      print,phase(i),(i-1)/4+1,((i-1) MOD 4)+1
      POS,XPOS=(i-1)/4+1,YPOS=((i-1) MOD 4)+1
      GSET,XMIN=0,XMAX=nlags,YMIN=0.7,YMAX=1.0/0.7,/YLOG
      HIST,X=indgen(nlags)+0.5,Y=REFORM(naop_bymjo(m,phase(i),*))-1,FILLCOL=FSC_COLOR(phase_colors(phase(i))),/NOBORDER,OFFSET=1
      GPLOT,X=0.5,Y=0.75,TEXT='RMM phase '+STRTRIM(STRING(phase(i)),1),ALIGN=0.0
      GPLOT,X=[0,nlags],Y=[1,1],STYLE=1
      IF ((i-1) MOD 4)+1 eq 1 THEN BEGIN
         AXES,XVALS=indgen(nlags)+0.5,XLABELS=STRTRIM(STRING(lags),1),YVALS=['0.70','0.80','0.90','1.0','1.11','1.25','1.43'],$
              XTITLE='Lag (days) after MJO phase',YTITLE='Frequency of NAO+'
      ENDIF ELSE $
         AXES,XVALS=indgen(nlags)+0.5,XLABELS=STRTRIM(STRING(lags),1),YVALS=['0.70','0.80','0.90','1.0','1.11','1.25','1.43'],$
              YTITLE='Frequency of NAO+'      
   ENDFOR
   PSCLOSE   

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/nao/hadgem3kpp_nao_mjo_probabilities.'+all_names(m)+'_naon_thresh'+STRMID(STRTRIM(STRING(nao_thresh),1),0,3)+'.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,MARGIN=2000,XOFFSET=1500,XPLOTS=2,YPLOTS=4,YSPACING=1500
   phase=[0,4,3,2,1,8,7,6,5]
   FOR i=1,8 DO BEGIN
      print,phase(i),(i-1)/4+1,((i-1) MOD 4)+1
      POS,XPOS=(i-1)/4+1,YPOS=((i-1) MOD 4)+1
      GSET,XMIN=0,XMAX=nlags,YMIN=0.7,YMAX=1.0/0.7,/YLOG
      HIST,X=indgen(nlags)+0.5,Y=REFORM(naon_bymjo(m,phase(i),*))-1,FILLCOL=FSC_COLOR(phase_colors(phase(i))),/NOBORDER,OFFSET=1
      GPLOT,X=0.5,Y=0.75,TEXT='RMM phase '+STRTRIM(STRING(phase(i)),1),ALIGN=0.0
      GPLOT,X=[0,nlags],Y=[1,1],STYLE=1
      IF ((i-1) MOD 4)+1 eq 1 THEN BEGIN
         AXES,XVALS=indgen(nlags)+0.5,XLABELS=STRTRIM(STRING(lags),1),YVALS=['0.70','0.80','0.90','1.0','1.11','1.25','1.43'],$
              XTITLE='Lag (days) after MJO phase',YTITLE='Frequency of NAO-'
      ENDIF ELSE $
         AXES,XVALS=indgen(nlags)+0.5,XLABELS=STRTRIM(STRING(lags),1),YVALS=['0.70','0.80','0.90','1.0','1.11','1.25','1.43'],$
              YTITLE='Frequency of NAO-'      
   ENDFOR
   PSCLOSE   

ENDFOR

STOP
END
