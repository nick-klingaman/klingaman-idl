PRO hadgem3kpp_nao_mjo_probabilities

nmodels=2

all_names=strarr(nmodels)
all_colors=strarr(nmodels)
all_symbols=intarr(nmodels)
all_styles=intarr(nmodels)
all_mynyears=intarr(nmodels)

nao_thresh=1.0
nphases=9
;lags=[3,6,9,12,15,18,21,24]
lags=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]
nlags=N_ELEMENTS(lags)
naop_bymjo=fltarr(nmodels,nphases,nlags)
naon_bymjo=fltarr(nmodels,nphases,nlags)

phase_colors=['black','purple','blue','dodgerblue','cyan','orange','red','firebrick','deeppink']
twophase_colors=['black','purple','dodgerblue','orange','red']

FOR m=0,nmodels-1 DO BEGIN
   CASE m OF
;      0 : BEGIN         
;         nao_file='/home/users/npklingaman/datasets/ERA-INTERIM/MSL/ERA-Interim.jan-dec_dmeans_ts.1979-2013.nao_index_n216.nc'
;         ntime=35*365
;         mjo_file='/home/users/npklingaman/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2014.index_values.nc'
;         mjo_offset=4
;         all_names(m)='ERA-Int'
;         all_colors(m)='black'
;         all_symbols(m)=2
;         all_styles(m)=0
;         days=[304,455]
;         ndays_per_year=365
;      END
;      1 : BEGIN         
;         nao_file='/group_workspaces/jasmin/futureweather/xjhwb/metum-goml1_fwn216.jan-dec_dmeans_ts.years1-100.nao_index_goml.nc'
;         ntime=LONG(100)*LONG(360)
;         mjo_file='/group_workspaces/jasmin/futureweather/xjhwb/metum-goml1_fwn216.jan-dec_dmeans.years1-100.rmm_indices.nc'
;         mjo_offset=0
;         all_names(m)='goml1-n216'
;         all_colors(m)='purple'
;         all_symbols(m)=2
;         all_styles(m)=0
;         days=[300,449]
;         ndays_per_year=360
;      END
;      2 : BEGIN
;         nao_file='/group_workspaces/jasmin/futureweather/xjhwe/metum-ga3_fwn216-31day.jan-dec_dmeans_ts.years1-100.nao_index_ga3-31d.nc'
;         ntime=LONG(100)*LONG(360)
;         mjo_file='/group_workspaces/jasmin/futureweather/xjhwe/rmm_indices.nc'
;         mjo_offset=0
;         all_names(m)='ga3-31d-n216'
;         all_colors(m)='violetred'
;         all_symbols(m)=2
;         all_styles(m)=0
;         days=[300,449]
;         ndays_per_year=360
;      END
;      3 : BEGIN
;         nao_file='/group_workspaces/jasmin/futureweather/xjhwh/metum-ga3_fwn216-clim.jan-dec_dmeans_ts.years1-100.nao_index_ga3-clim.nc'
;         ntime=LONG(100)*LONG(360)
;         mjo_file='/group_workspaces/jasmin/futureweather/xjhwh/rmm_indices.nc'
;         mjo_offset=0
;         all_names(m)='ga3-clim-n216'
;         all_colors(m)='violetred'
;         all_symbols(m)=2
;         all_styles(m)=0
;         days=[300,449]
;         ndays_per_year=360
;      END
      0 : BEGIN
         nao_file='/group_workspaces/jasmin/futureweather/fwgbl_n216_1p5F/xihvu/metum-goml1_fwn216-1p5.jan-dec_dmeans_ts.years1-61.nao_index_goml1-1p5.nc'
         ntime=LONG(61)*LONG(360)
         mjo_file='/group_workspaces/jasmin/futureweather/fwgbl_n216_1p5F/xihvu/rmm_indices.nc'
         mjo_offset=0
         all_names(m)='goml1-n216-1p5F'
         all_colors(m)='violetred'
         all_symbols(m)=2
         all_styles(m)=0
         days=[300,449]
         ndays_per_year=360
      END
       1 : BEGIN
         nao_file='/group_workspaces/jasmin/futureweather/fwgbl_n216_1p5F/metum-ga3_fwn216-31day-1p5.jan-dec_dmeans_ts.years1-61.nao_index_ga3-1p5.nc'
         ntime=LONG(61)*LONG(360)
         mjo_file='/group_workspaces/jasmin/futureweather/fwgbl_n216_1p5F/rmm_indices.nc'
         mjo_offset=0
         all_names(m)='ga3-n216-1p5F'
         all_colors(m)='orange'
         all_symbols(m)=2
         all_styles(m)=0
         days=[300,449]
         ndays_per_year=360
      END
;      5 : BEGIN
;         nao_file='/group_workspaces/jasmin2/klingaman/metum/xlhvc/metum-toml1_fwn216.jan-dec_dmeans_ts.years1-60.nao_index_toml1.nc'
;         ntime=LONG(60)*LONG(360)
;         mjo_filee='/group_workspaces/jasmin/futureweather/xlhvc/rmm_indices.nc'
;         mjo_offset=0
;         all_names(m)='toml1-n216'
;         all_colors(m)='violetred'
;         all_symbols(m)=2
;         all_styles(m)=0
;         days=[300,449]
;         ndays_per_year=360
;      END
;      6 : BEGIN
;         nao_file='/group_workspaces/jasmin2/klingaman/metum/xlhvf/metum-etoml1_fwn216.jan-dec_dmeans_ts.years1-60.nao_index_etoml1.nc'
;         ntime=LONG(60)*LONG(360)
;         mjo_filee='/group_workspaces/jasmin/futureweather/xlhvf/rmm_indices.nc'
;         mjo_offset=0
;         all_names(m)='etoml1-n216'
;         all_colors(m)='violetred'
;         all_symbols(m)=2
;         all_styles(m)=0
;         days=[300,449]
;         ndays_per_year=360
;      END
   ENDCASE
   
   nao_index=OPEN_AND_EXTRACT(nao_file,'nao_index',offset=[0],count=[ntime])
   nyears=ntime/ndays_per_year
   ndpy=days(1)-days(0)
   
   IF days(1) le ndays_per_year THEN BEGIN
      my_nyears=nyears
   ENDIF ELSE $
      my_nyears=nyears-1
   all_mynyears(m)=my_nyears
   
   nao_index_subset=fltarr(ndpy)
   nao_index_allyears=fltarr(ndpy*my_nyears)
   FOR i=0,my_nyears-1 DO BEGIN
      nao_index_subset(*)=nao_index(LONG(i)*LONG(ndays_per_year)+LONG(days(0)):LONG(i)*LONG(ndays_per_year)+LONG(days(1))-LONG(1))
      nao_index_allyears(LONG(i)*LONG(ndpy):LONG(i+1)*LONG(ndpy)-LONG(1))=nao_index_subset
   ENDFOR   
   nao_index_allyears=nao_index_allyears-MEAN(nao_index)
   nao_index_allyears=nao_index_allyears/STDDEV(nao_index)
   
   amp_ts=fltarr(ndays_per_year*nyears)
   phase_ts=fltarr(ndays_per_year*nyears)
   IF m eq 0 THEN BEGIN
      amp=OPEN_AND_EXTRACT(mjo_file,'amplitude',offset=[mjo_offset,0],count=[nyears,ndays_per_year])
      phase=OPEN_AND_EXTRACT(mjo_file,'phase',offset=[mjo_offset,0],count=[nyears,ndays_per_year])
      FOR i=0,nyears-1 DO BEGIN
         amp_ts(LONG(i)*LONG(ndays_per_year):LONG(i+1)*LONG(ndays_per_year)-LONG(1))=amp(i,*)
         phase_ts(LONG(i)*LONG(ndays_per_year):LONG(i+1)*LONG(ndays_per_year)-LONG(1))=phase(i,*)
      ENDFOR
   ENDIF ELSE BEGIN
      amp=OPEN_AND_EXTRACT(mjo_file,'amplitude')      
      phase=OPEN_AND_EXTRACT(mjo_file,'phase')
      FOR i=0,nyears-1 DO BEGIN
         amp_ts(LONG(i)*LONG(ndays_per_year):LONG(i+1)*LONG(ndays_per_year)-LONG(1))=amp(i,*)
         phase_ts(LONG(i)*LONG(ndays_per_year):LONG(i+1)*LONG(ndays_per_year)-LONG(1))=phase(i,*)
      ENDFOR
   ENDELSE
   mjo_amplitude_allyears=fltarr(ndpy*my_nyears)
   mjo_phase_allyears=fltarr(ndpy*my_nyears)
   FOR i=0,my_nyears-1 DO BEGIN    
      mjo_amplitude_allyears(LONG(i)*LONG(ndpy):LONG(i+1)*LONG(ndpy)-LONG(1))=amp_ts(LONG(i)*LONG(ndays_per_year)+LONG(days(0)):$
                                                                                     LONG(i)*LONG(ndays_per_year)+LONG(days(1))-LONG(1))
      mjo_phase_allyears(LONG(i)*LONG(ndpy):LONG(i+1)*LONG(ndpy)-LONG(1))=phase_ts(LONG(i)*LONG(ndays_per_year)+LONG(days(0)):$
                                                                                   LONG(i)*LONG(ndays_per_year)+LONG(days(1))-LONG(1))
   ENDFOR

   naop_prob=N_ELEMENTS(where(nao_index_allyears gt nao_thresh))/FLOAT(ndpy*nyears)
   naon_prob=N_ELEMENTS(where(nao_index_allyears lt (-1.0)*nao_thresh))/FLOAT(ndpy*nyears)
   
   total_phase=intarr(9)
   FOR j=0,my_nyears-1 DO BEGIN
      thisyear_amp=mjo_amplitude_allyears(LONG(j)*LONG(ndpy):LONG(j+1)*LONG(ndpy)-LONG(MAX(lags))-LONG(1))
      thisyear_phase=mjo_phase_allyears(LONG(j)*LONG(ndpy):LONG(j+1)*LONG(ndpy)-LONG(MAX(lags))-LONG(1))
      thisyear_nao=nao_index_allyears(LONG(j)*LONG(ndpy):LONG(j+1)*LONG(ndpy)-LONG(1))
      FOR p=0,8 DO BEGIN     
         IF p eq 0 THEN BEGIN
            this_phase=where(thisyear_amp lt 1.0)
         ENDIF ELSE $
            this_phase=where(thisyear_amp ge 1.0 and thisyear_phase eq p)
         IF TOTAL(this_phase) ge 0 THEN BEGIN
            total_phase(p)=total_phase(p)+N_ELEMENTS(this_phase)
            FOR i=0,nlags-1 DO BEGIN
               nao_lagged=thisyear_nao[this_phase+lags(i)]
               IF TOTAL(where(nao_lagged ge nao_thresh)) ge 0 THEN $
                  naop_bymjo(m,p,i)=N_ELEMENTS(where(nao_lagged ge nao_thresh))+naop_bymjo(m,p,i)
               IF TOTAL(where(nao_lagged le (-1.0)*nao_thresh)) ge 0 THEN $
                  naon_bymjo(m,p,i)=N_ELEMENTS(where(nao_lagged le (-1.0)*nao_thresh))+naon_bymjo(m,p,i)
            ENDFOR
         ENDIF
      ENDFOR
   ENDFOR
   FOR p=0,8 DO BEGIN
      naop_bymjo(m,p,*)=naop_bymjo(m,p,*)/FLOAT(total_phase(p))/FLOAT(naop_prob)
      naon_bymjo(m,p,*)=naon_bymjo(m,p,*)/FLOAT(total_phase(p))/FLOAT(naon_prob)
   ENDFOR
   
    
   psfile='/home/users/npklingaman/plots/hadgem3-kpp_runs/nao/hadgem3kpp_nao_mjo_probabilities.'+all_names(m)+'_naop_thresh'+STRMID(STRTRIM(STRING(nao_thresh),1),0,3)+'.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,MARGIN=2000,XOFFSET=1500,XPLOTS=2,YPLOTS=4,YSPACING=1500
   phase=[0,4,3,2,1,8,7,6,5]
   FOR i=1,8 DO BEGIN
      print,phase(i),(i-1)/4+1,((i-1) MOD 4)+1
      POS,XPOS=(i-1)/4+1,YPOS=((i-1) MOD 4)+1
      GSET,XMIN=0,XMAX=nlags,YMIN=0.4,YMAX=2.50,/YLOG
      HIST,X=indgen(nlags)+0.5,Y=REFORM(naop_bymjo(m,phase(i),*))-1,FILLCOL=FSC_COLOR(phase_colors(phase(i))),/NOBORDER,OFFSET=1
      GPLOT,X=0.5,Y=0.5,TEXT='RMM phase '+STRTRIM(STRING(phase(i)),1),ALIGN=0.0
      GPLOT,X=[0,nlags],Y=[1,1],STYLE=1
      IF ((i-1) MOD 4)+1 eq 1 THEN BEGIN
         AXES,XVALS=indgen(nlags)+0.5,XLABELS=STRTRIM(STRING(lags),1),YVALS=['0.40','0.50','0.60','0.80','1.00','1.25','1.66','2.00','2.50'],$
              XTITLE='Lag (days) after MJO phase',YTITLE='Frequency of NAO+'
      ENDIF ELSE $
         AXES,XVALS=indgen(nlags)+0.5,XLABELS=STRTRIM(STRING(lags),1),YVALS=['0.40','0.50','0.60','0.80','1.00','1.25','1.66','2.00','2.50'],$
              YTITLE='Frequency of NAO+'      
   ENDFOR
   PSCLOSE,/NOVIEW

   psfile='/home/users/npklingaman/plots/hadgem3-kpp_runs/nao/hadgem3kpp_nao_mjo_probabilities.'+all_names(m)+'_naon_thresh'+STRMID(STRTRIM(STRING(nao_thresh),1),0,3)+'.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,MARGIN=2000,XOFFSET=1500,XPLOTS=2,YPLOTS=4,YSPACING=1500
   phase=[0,4,3,2,1,8,7,6,5]
   FOR i=1,8 DO BEGIN
      print,phase(i),(i-1)/4+1,((i-1) MOD 4)+1
      POS,XPOS=(i-1)/4+1,YPOS=((i-1) MOD 4)+1
      GSET,XMIN=0,XMAX=nlags,YMIN=0.4,YMAX=2.50,/YLOG
      HIST,X=indgen(nlags)+0.5,Y=REFORM(naon_bymjo(m,phase(i),*))-1,FILLCOL=FSC_COLOR(phase_colors(phase(i))),/NOBORDER,OFFSET=1
      GPLOT,X=0.5,Y=0.5,TEXT='RMM phase '+STRTRIM(STRING(phase(i)),1),ALIGN=0.0
      GPLOT,X=[0,nlags],Y=[1,1],STYLE=1
      IF ((i-1) MOD 4)+1 eq 1 THEN BEGIN
         AXES,XVALS=indgen(nlags)+0.5,XLABELS=STRTRIM(STRING(lags),1),YVALS=['0.40','0.50','0.60','0.80','1.00','1.25','1.66','2.00','2.50'],$
              XTITLE='Lag (days) after MJO phase',YTITLE='Frequency of NAO-'
      ENDIF ELSE $
         AXES,XVALS=indgen(nlags)+0.5,XLABELS=STRTRIM(STRING(lags),1),YVALS=['0.40','0.50','0.60','0.80','1.00','1.25','1.66','2.00','2.50'],$
              YTITLE='Frequency of NAO-'      
   ENDFOR
   PSCLOSE,/NOVIEW   

ENDFOR

STOP
END
