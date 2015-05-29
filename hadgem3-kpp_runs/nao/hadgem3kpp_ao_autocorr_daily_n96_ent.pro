PRO hadgem3kpp_ao_autocorr_daily_n96_ent

nmodels=15

all_names=strarr(nmodels)
all_descs=strarr(nmodels)
all_colors=strarr(nmodels)
all_symbols=intarr(nmodels)
all_styles=intarr(nmodels)
lags=indgen(31)
nlags=N_ELEMENTS(lags)

max_nyears=100
max_nlev=50

autocorr=fltarr(nmodels,nlags,max_nlev)
autocorr_thirds=fltarr(nmodels,nlags,max_nlev,3)
allyears_autocorr=fltarr(nmodels,max_nyears,nlags,max_nlev)

efold=fltarr(nmodels,max_nlev)
plev=fltarr(nmodels,max_nlev)
lev_toplot=fltarr(nmodels,max_nlev)

FOR m=0,nmodels-1 DO BEGIN
   CASE m OF      
      0 : BEGIN
         ao_file='/home/ss901165/um_output6/xihvp/hadgem3kpp_fwgbl_1.5xentrain_ga30_n96.jan-dec_dmeans_ts.years1-72.ao_index_goml_1p5.nc'
         ntime=24*360
         offset=24*360
         nsets=3
         all_names(m)='goml1_1p5ent_n96_jan-dec_owneof'
         all_descs(m)='GOML1 1.5x!9e!X N96 Jan-Dec'
         all_colors(m)='violetred'
         all_symbols(m)=3
         all_styles(m)=0
         days=[0,364]
         ndays_per_year=360
         a=REPLICATE(1,nlags)
         mylevs_corr=['0.10','0.20','0.30','0.40','0.50','0.60','0.70','0.80','0.90']
      END
      1 : BEGIN
         ao_file='/home/ss901165/um_output6/xihvp/hadgem3kpp_fwgbl_1.5xentrain_ga30_n96.jan-dec_dmeans_ts.years1-72.ao_index_goml_1p5.nc'
         ntime=24*360
         offset=24*360
         nsets=3
         all_names(m)='goml1_1p5ent_n96_dec-feb_owneof'
         all_descs(m)='GOML1 1.5x!9e!X N96 Dec-Feb'
         all_colors(m)='violetred'
         all_symbols(m)=3
         all_styles(m)=0
         days=[330,419]
         ndays_per_year=360
         a=REPLICATE(1,nlags)
         mylevs_corr=['0.10','0.20','0.30','0.40','0.50','0.60','0.70','0.80','0.90']
      END
      2 : BEGIN
         ao_file='/home/ss901165/um_output6/xihvp/hadgem3kpp_fwgbl_1.5xentrain_ga30_n96.jan-dec_dmeans_ts.years1-72.ao_index_goml_1p5.nc'
         ntime=24*360
         offset=24*360
         nsets=3
         all_names(m)='goml1_1p5ent_n96_jun-aug_owneof'
         all_descs(m)='GOML1 1.5x!9e!X N96 Jun-Aug'
         all_colors(m)='violetred'
         all_symbols(m)=3
         all_styles(m)=0
         days=[150,240]
         ndays_per_year=360
         a=REPLICATE(1,nlags)
         mylevs_corr=['0.10','0.20','0.30','0.40','0.50','0.60','0.70','0.80','0.90']
      END
      3 : BEGIN
         ao_file='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans_ts.years1-29.ao_index_goml_1p5-50NS.nc'
         ntime=29*360
         offset=0
         nsets=1
         all_names(m)='goml_1p5ent_50NS_n96_jan-dec_owneof'
         all_descs(m)='GOML1 1.5!9e!X 50NS N96 Jan-Dec'
         all_colors(m)='purple'
         all_symbols(m)=3
         all_styles(m)=0
         days=[0,360]
      END
      4 : BEGIN
         ao_file='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans_ts.years1-29.ao_index_goml_1p5-50NS.nc'
         ntime=29*360
         offset=0
         nsets=1
         all_names(m)='goml_1p5ent_50NS_n96_dec-feb_owneof'
         all_descs(m)='GOML1 1.5!9e!X 50NS N96 Dec-Feb'
         all_colors(m)='purple'
         all_symbols(m)=3
         all_styles(m)=0
         days=[330,420]
      END
      5 : BEGIN
         ao_file='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans_ts.years1-29.ao_index_goml_1p5-50NS.nc'
         ntime=29*360
         offset=0
         nsets=1
         all_names(m)='goml_1p5ent_50NS_n96_jun-aug_owneof'
         all_descs(m)='GOML1 1.5!9e!X 50NS N96 Jun-Aug'
         all_colors(m)='purple'
         all_symbols(m)=3
         all_styles(m)=0
         days=[150,240]
      END
      6 : BEGIN
         ao_file='/home/ss901165/um_output6/xihvx/hadgem3a_kpp50N50Ssmooth31_1.5xentrain_ga30.jan-dec_dmeans_ts.years1-29.ao_index_ga3-50NS-31d.nc'
         ntime=29*360
         offset=0
         nsets=1
         all_names(m)='ga3-31d_1p5ent_50NS_n96_jan-dec_owneof'
         all_descs(m)='GA3-50NS-31d 1.5!9e!X N96 Jan-Dec'
         all_colors(m)='red'
         all_symbols(m)=3
         all_styles(m)=0
         days=[0,360]
      END
      7 : BEGIN
         ao_file='/home/ss901165/um_output6/xihvx/hadgem3a_kpp50N50Ssmooth31_1.5xentrain_ga30.jan-dec_dmeans_ts.years1-29.ao_index_ga3-50NS-31d.nc'
         ntime=29*360
         offset=0
         nsets=1
         all_names(m)='ga3-31d_1p5ent_50NS_n96_dec-feb_owneof'
         all_descs(m)='GA3-50NS-31d 1.5!9e!X N96 Dec-Feb'
         all_colors(m)='red'
         all_symbols(m)=3
         all_styles(m)=0
         days=[330,420]
      END      
      8 : BEGIN
         ao_file='/home/ss901165/um_output6/xihvx/hadgem3a_kpp50N50Ssmooth31_1.5xentrain_ga30.jan-dec_dmeans_ts.years1-29.ao_index_ga3-50NS-31d.nc'
         ntime=29*360
         offset=0
         nsets=1
         all_names(m)='ga3-31d_1p5ent_50NS_n96_jun-aug_owneof'
         all_descs(m)='GA3-50NS-31d 1.5!9e!X N96 Jun-Aug'
         all_colors(m)='red'
         all_symbols(m)=3
         all_styles(m)=0
         days=[150,240]
      END
      9 : BEGIN
         ao_file='/home/ss901165/um_output6/xihvy/hadgem3a_kpp50N50Ssmooth15_1.5xentrain_ga30.jan-dec_dmeans_ts.years1-29.ao_index_ga3-50NS-15d.nc'
         ntime=29*360
         offset=0
         nsets=1
         all_names(m)='ga3-15d_1p5ent_50NS_n96_jan-dec_owneof'
         all_descs(m)='GA3-50NS-15d 1.5!9e!X N96 Jan-Dec'
         all_colors(m)='orange'
         all_symbols(m)=3
         all_styles(m)=0
         days=[0,360]
      END
      10 : BEGIN
         ao_file='/home/ss901165/um_output6/xihvy/hadgem3a_kpp50N50Ssmooth15_1.5xentrain_ga30.jan-dec_dmeans_ts.years1-29.ao_index_ga3-50NS-15d.nc'
         ntime=29*360
         offset=0
         nsets=1
         all_names(m)='ga3-15d_1p5ent_50NS_n96_dec-feb_owneof'
         all_descs(m)='GA3-50NS-15d 1.5!9e!X N96 Dec-Feb'
         all_colors(m)='orange'
         all_symbols(m)=3
         all_styles(m)=0
         days=[330,420]
      END      
      11 : BEGIN
         ao_file='/home/ss901165/um_output6/xihvy/hadgem3a_kpp50N50Ssmooth15_1.5xentrain_ga30.jan-dec_dmeans_ts.years1-29.ao_index_ga3-50NS-15d.nc'
         ntime=29*360
         offset=0
         nsets=1
         all_names(m)='ga3-15d_1p5ent_50NS_n96_jun-aug_owneof'
         all_descs(m)='GA3-50NS-15d 1.5!9e!X N96 Jun-Aug'
         all_colors(m)='orange'
         all_symbols(m)=3
         all_styles(m)=0
         days=[150,240]
      END
      12 : BEGIN
         ao_file='/home/ss901165/datasets/ERA-INTERIM/Z/ERA_interim.jan-dec_dmeans_ts.1979-2013.ao_index_eraint_n96.nc'
         ntime=35*365
         offset=0
         nsets=1
         all_names(m)='eraint_n96_jan-dec_owneof'
         all_descs(m)='ERA-Int N96 Jan-Dec'
         all_colors(m)='black'
         all_symbols(m)=3
         all_styles(m)=0
         days=[0,364]
         ndays_per_year=365
         a=REPLICATE(1,nlags)
         mylevs_corr=['0.10','0.20','0.30','0.40','0.50','0.60','0.70','0.80','0.90']
      END
      13 : BEGIN
         ao_file='/home/ss901165/datasets/ERA-INTERIM/Z/ERA_interim.jan-dec_dmeans_ts.1979-2013.ao_index_eraint_n96.nc'
         ntime=35*365
         offset=0
         nsets=1
         all_names(m)='eraint_n96_dec-feb_owneof'
         all_descs(m)='ERA-Int N96 Dec-Feb'
         all_colors(m)='black'
         all_symbols(m)=3
         all_styles(m)=0
         days=[334,423]
         ndays_per_year=365
         a=REPLICATE(1,nlags)
         mylevs_corr=['0.10','0.20','0.30','0.40','0.50','0.60','0.70','0.80','0.90']
      END
      14 : BEGIN
         ao_file='/home/ss901165/datasets/ERA-INTERIM/Z/ERA_interim.jan-dec_dmeans_ts.1979-2013.ao_index_eraint_n96.nc'
         ntime=35*365
         offset=0
         nsets=1
         all_names(m)='eraint_n96_jun-aug_owneof'
         all_descs(m)='ERA-Int N96 Jun-Aug'
         all_colors(m)='black'
         all_symbols(m)=3
         all_styles(m)=0
         days=[151,242]
         ndays_per_year=365
         a=REPLICATE(1,nlags)
         mylevs_corr=['0.10','0.20','0.30','0.40','0.50','0.60','0.70','0.80','0.90']
      END
   ENDCASE
   
   lev=OPEN_AND_EXTRACT(ao_file,'lev')
   IF m ge 12 THEN BEGIN
      lev_toplot(m,0:nlev_save-1)=[0,1,2,3,4,6,8,10,12,13,14,15,16,17,18,19,20,21,23,25,27,29,30,32,34,36]
   ENDIF ELSE IF m eq 0 THEN BEGIN
      lev_save=lev
      nlev_save=N_ELEMENTS(lev)
      nlev=nlev_Save
      lev_toplot(m,0:nlev_save-1)=indgen(N_ELEMENTS(lev))
   ENDIF ELSE BEGIN
      nlev=N_ELEMENTS(lev)
      lev_toplot(m,0:nlev-1)=indgen(nlev)
      print,m,nlev
   ENDELSE
   lev_toplot(m,nlev:max_nlev-1)=!Values.F_NaN

   nlev=N_ELEMENTS(lev)
   ao_index=fltarr(nlev,ntime*nsets)
   FOR i=0,nsets-1 DO $
      ao_index(0:nlev-1,i*ntime:(i+1)*ntime-1)=OPEN_AND_EXTRACT(ao_file,'ao_index',offset=[0,offset*i],count=[nlev,ntime])
   nyears=ntime/ndays_per_year
   ndpy=days(1)-days(0)
   
   
   IF days(1) le 360 THEN BEGIN
      my_nyears=nyears      
   ENDIF ELSE $
      my_nyears=nyears-1   
   
   ao_index_subset=fltarr(ndpy)
   ao_index_allyears=fltarr(nlev,ndpy*my_nyears)
   FOR i=0,nlev-1 DO BEGIN
      FOR j=0,my_nyears-1 DO BEGIN
         ao_index_subset(*)=ao_index(i,j*ndays_per_year+days(0):j*ndays_per_year+days(1)-1)
         ao_index_allyears(i,j*ndpy:(j+1)*ndpy-1)=ao_index_subset
      ENDFOR
   ENDFOR

   FOR i=0,nlags-1 DO BEGIN
      FOR j=0,nlev-1 DO BEGIN
         this_ndpy=ndpy-lags(i)
         central=fltarr(my_nyears*this_ndpy)
         lagged=fltarr(my_nyears*this_ndpy)
         FOR k=0,my_nyears-1 DO BEGIN
            central(k*this_ndpy:(k+1)*this_ndpy-1)=ao_index_allyears(j,k*ndpy:k*ndpy+this_ndpy-1)
            lagged(k*this_ndpy:(k+1)*this_ndpy-1)=ao_index_allyears(j,k*ndpy+lags(i):(k+1)*ndpy-1)                  
         ENDFOR
         allyears_var=VARIANCE(central)
         allyears_mean=MEAN(central)
         FOR k=0,my_nyears-1 DO BEGIN
            this_central=central(k*this_ndpy:(k+1)*this_ndpy-1)
            this_lagged=lagged(k*this_ndpy:(k+1)*this_ndpy-1)
            allyears_autocorr(m,k,i,j)=TOTAL((this_central-allyears_mean)*(this_lagged-allyears_mean))/(allyears_var)/FLOAT(this_ndpy)*a(i)
         ENDFOR
         autocorr(m,i,j)=CORRELATE(central,lagged)*a(i)
      ENDFOR
   ENDFOR
   FOR i=0,nlev-1 DO BEGIN
      FOR j=0,my_nyears-1 DO $
         allyears_autocorr(m,j,*,i)=allyears_autocorr(m,j,*,i)/allyears_autocorr(m,j,0,i)
      temp=REFORM(autocorr(m,*,i))
      decorr=where(temp le 1./2.718)
      IF decorr(0) ge 0 THEN BEGIN
         efold(m,i)=decorr(0)
      ENDIF ELSE $
         efold(m,i)=MAX(lags)
      decorr=where(temp le 0.211)
      IF decorr(0) ge 0 THEN BEGIN 
         plev(m,i)=decorr(0)
      ENDIF ELSE $
         plev(m,i)=MAX(lags)
   ENDFOR
   
   ; Contour plot
   nlevs_corr=N_ELEMENTS(mylevs_corr)
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/nao/hadgem3kpp_ao_autocorr_daily_n96_ent.'+all_names(m)+'.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=140,MARGIN=2000,XSIZE=16000,YSIZE=15000,XOFFSET=2500,YOFFSET=2000,SPACE2=2000,SPACE3=200
   GSET,XMIN=0,XMAX=MAX(lags),YMIN=0,YMAX=nlev
   CS,SCALE=24,NCOLS=N_ELEMENTS(mylevs_corr)+1
   LEVS,MANUAL=mylevs_corr
   nlevs_toplot=N_ELEMENTS(where(FINITE(REFORM(lev_toplot(m,*))) eq 1))
   toplot=fltarr(nlags,nlevs_toplot)
   FOR j=0,nlags-1 DO BEGIN
      temp=REFORM(autocorr(m,j,0:nlevs_toplot-1))
      toplot(j,*)=temp[REFORM(lev_toplot(m,0:nlevs_toplot-1))]
   ENDFOR
   CON,X=lags,Y=findgen(nlevs_toplot),FIELD=toplot,/BLOCK,/NOLINES,CB_WIDTH=115,CB_TITLE='Auto-correlation coefficient',$
                        /CB_RIGHT
   GPLOT,X=REFORM(efold(m,*)),Y=findgen(nlevs_toplot),STYLE=0,SYM=3
   GPLOT,X=REFORM(plev(m,*)),Y=findgen(nlevs_toplot),STYLE=2,SYM=4
   AXES,XSTEP=5,XMINOR=1,YVALS=findgen(nlevs_toplot),YLABELS=STRMID(STRTRIM(STRING(lev_save[REFORM(lev_toplot(m,0:nlevs_toplot-1))]),1),0,5),$
        XTITLE='Lag (days)',YTITLE='Pressure (hPa)'
   PSCLOSE,/NOVIEW

   IF m eq 3 THEN $
      STOP

ENDFOR
STOP

; All models for Jan-Dec
nlevs_corr=N_ELEMENTS(mylevs_corr)
psfile='/home/ss901165/idl/hadgem3-kpp_runs/nao/hadgem3kpp_ao_autocorr_daily_n96_ent.jan-dec_owneof_allefold.ps'
PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=140,MARGIN=2000,XSIZE=16000,YSIZE=16000,XOFFSET=1500,YOFFSET=2000,SPACE2=2000,SPACE3=200
GSET,XMIN=0,XMAX=MAX(lags),YMIN=0,YMAX=nlev_save
FOR m=0,nmodels-1,3 DO BEGIN
   toplot=REFORM(efold(m,0:nlev-1))
   toplot=toplot[REFORM(lev_toplot(m,*))]
   GPLOT,X=toplot-0.3+0.05*m,Y=findgen(nlev_save),STYLE=0,COL=FSC_COLOR(all_colors(m)),THICK=200,SYM=3,SIZE=70
   ;toplot=REFORM(plev(m,0:nlev-1))
   ;toplot=toplot[REFORM(lev_toplot(m,*))]
   ;GPLOT,X=toplot-0.3*0.05+m,Y=findgen(nlev_save),STYLE=2,COL=FSC_COLOR(all_colors(m)),THICK=200,SYM=4,SIZE=70
ENDFOR
GLEGEND,labels=REVERSE(all_descs(0:nmodels-1:3)),COL=REVERSE(FSC_COLOR(all_colors(0:nmodels-1:3))),LENGTH=0,SYM=[REPLICATE(3,nmodels/3)],$
        LEGXOFFSET=10000,LEGYOFFSET=16000
;GLEGEND,labels=REVERSE(['e-folding timescale','5% significance level']),SYM=REVERSE([3,4]),LEGXOFFSET=9000,LEGYOFFSET=13000,LENGTH=0
AXES,XSTEP=5,XMINOR=1,YVALS=findgen(nlev_save),YLABELS=STRMID(STRTRIM(STRING(lev_save),1),0,5),$
     XTITLE='Lag (days)',YTITLE='Pressure (hPa)'
PSCLOSE

; All models for Jan-Dec
nlevs_corr=N_ELEMENTS(mylevs_corr)
psfile='/home/ss901165/idl/hadgem3-kpp_runs/nao/hadgem3kpp_ao_autocorr_daily_n96_ent.jan-dec_owneof_allplev.ps'
PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=140,MARGIN=2000,XSIZE=16000,YSIZE=16000,XOFFSET=1500,YOFFSET=2000,SPACE2=2000,SPACE3=200
GSET,XMIN=0,XMAX=MAX(lags),YMIN=0,YMAX=nlev_save
FOR m=0,nmodels-1,3 DO BEGIN
   ;toplot=REFORM(efold(m,0:nlev-1))
   ;toplot=toplot[REFORM(lev_toplot(m,*))]
   ;GPLOT,X=toplot-0.3*0.05+m,Y=findgen(nlev_save),STYLE=0,COL=FSC_COLOR(all_colors(m)),THICK=200,SYM=3,SIZE=70
   toplot=REFORM(plev(m,0:nlev-1))
   toplot=toplot[REFORM(lev_toplot(m,*))]
   GPLOT,X=toplot-0.3+0.05*m,Y=findgen(nlev_save),STYLE=0,COL=FSC_COLOR(all_colors(m)),THICK=200,SYM=3,SIZE=70
ENDFOR
GLEGEND,labels=REVERSE(all_descs(0:nmodels-1:3)),COL=REVERSE(FSC_COLOR(all_colors(0:nmodels-1:3))),LENGTH=0,SYM=[REPLICATE(3,nmodels/3)],$
        LEGXOFFSET=10000,LEGYOFFSET=16000
;GLEGEND,labels=REVERSE(['e-folding timescale','5% significance level']),SYM=REVERSE([3,4]),LEGXOFFSET=9000,LEGYOFFSET=13000,LENGTH=0
AXES,XSTEP=5,XMINOR=1,YVALS=findgen(nlev_save),YLABELS=STRMID(STRTRIM(STRING(lev_save),1),0,5),$
     XTITLE='Lag (days)',YTITLE='Pressure (hPa)'
PSCLOSE


; All models for Dec-Feb
nlevs_corr=N_ELEMENTS(mylevs_corr)
psfile='/home/ss901165/idl/hadgem3-kpp_runs/nao/hadgem3kpp_ao_autocorr_daily_n96_ent.dec-feb_owneof_allefold.ps'
PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=140,MARGIN=2000,XSIZE=16000,YSIZE=16000,XOFFSET=1500,YOFFSET=2000,SPACE2=2000,SPACE3=200
GSET,XMIN=0,XMAX=MAX(lags),YMIN=0,YMAX=nlev_save
FOR m=1,nmodels-1,3 DO BEGIN
   toplot=REFORM(efold(m,0:nlev-1))
   toplot=toplot[REFORM(lev_toplot(m,*))]
   GPLOT,X=toplot-0.3+0.05*m,Y=findgen(nlev_save),STYLE=0,COL=FSC_COLOR(all_colors(m)),THICK=200,SYM=3,SIZE=70
   ;toplot=REFORM(plev(m,0:nlev-1))
   ;toplot=toplot[REFORM(lev_toplot(m,*))]
   ;GPLOT,X=toplot-0.3*0.05+m,Y=findgen(nlev_save),STYLE=2,COL=FSC_COLOR(all_colors(m)),THICK=200,SYM=4,SIZE=70
ENDFOR
GLEGEND,labels=REVERSE(all_descs(1:nmodels-1:3)),COL=REVERSE(FSC_COLOR(all_colors(1:nmodels-1:3))),LENGTH=0,SYM=[REPLICATE(3,nmodels/3)],$
        LEGXOFFSET=10000,LEGYOFFSET=16000
;GLEGEND,labels=REVERSE(['e-folding timescale','5% significance level']),SYM=REVERSE([3,4]),LEGXOFFSET=9000,LEGYOFFSET=13000,LENGTH=0
AXES,XSTEP=5,XMINOR=1,YVALS=findgen(nlev_save),YLABELS=STRMID(STRTRIM(STRING(lev_save),1),0,5),$
     XTITLE='Lag (days)',YTITLE='Pressure (hPa)'
PSCLOSE

; All models for Dec-Feb
nlevs_corr=N_ELEMENTS(mylevs_corr)
psfile='/home/ss901165/idl/hadgem3-kpp_runs/nao/hadgem3kpp_ao_autocorr_daily_n96_ent.dec-feb_owneof_allplev.ps'
PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=140,MARGIN=2000,XSIZE=16000,YSIZE=16000,XOFFSET=1500,YOFFSET=2000,SPACE2=2000,SPACE3=200
GSET,XMIN=0,XMAX=MAX(lags),YMIN=0,YMAX=nlev_save
FOR m=1,nmodels-1,3 DO BEGIN
   ;toplot=REFORM(efold(m,0:nlev-1))
   ;toplot=toplot[REFORM(lev_toplot(m,*))]
   ;GPLOT,X=toplot-0.3*0.05+m,Y=findgen(nlev_save),STYLE=0,COL=FSC_COLOR(all_colors(m)),THICK=200,SYM=3,SIZE=70
   toplot=REFORM(plev(m,0:nlev-1))
   toplot=toplot[REFORM(lev_toplot(m,*))]
   GPLOT,X=toplot-0.3+0.05*m,Y=findgen(nlev_save),STYLE=0,COL=FSC_COLOR(all_colors(m)),THICK=200,SYM=3,SIZE=70
ENDFOR
GLEGEND,labels=REVERSE(all_descs(1:nmodels-1:3)),COL=REVERSE(FSC_COLOR(all_colors(1:nmodels-1:3))),LENGTH=0,SYM=[REPLICATE(3,nmodels/3)],$
        LEGXOFFSET=10000,LEGYOFFSET=16000
;GLEGEND,labels=REVERSE(['e-folding timescale','5% significance level']),SYM=REVERSE([3,4]),LEGXOFFSET=9000,LEGYOFFSET=13000,LENGTH=0
AXES,XSTEP=5,XMINOR=1,YVALS=findgen(nlev_save),YLABELS=STRMID(STRTRIM(STRING(lev_save),1),0,5),$
     XTITLE='Lag (days)',YTITLE='Pressure (hPa)'
PSCLOSE


; All models for Jun-Feb
nlevs_corr=N_ELEMENTS(mylevs_corr)
psfile='/home/ss901165/idl/hadgem3-kpp_runs/nao/hadgem3kpp_ao_autocorr_daily_n96_ent.jun-aug_owneof_allefold.ps'
PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=140,MARGIN=2000,XSIZE=16000,YSIZE=16000,XOFFSET=1500,YOFFSET=2000,SPACE2=2000,SPACE3=200
GSET,XMIN=0,XMAX=MAX(lags),YMIN=0,YMAX=nlev_save
FOR m=2,nmodels-1,3 DO BEGIN
   toplot=REFORM(efold(m,0:nlev-1))
   toplot=toplot[REFORM(lev_toplot(m,*))]
   GPLOT,X=toplot-0.3+0.05*m,Y=findgen(nlev_save),STYLE=0,COL=FSC_COLOR(all_colors(m)),THICK=200,SYM=3,SIZE=70
   ;toplot=REFORM(plev(m,0:nlev-1))
   ;toplot=toplot[REFORM(lev_toplot(m,*))]
   ;GPLOT,X=toplot-0.3*0.05+m,Y=findgen(nlev_save),STYLE=2,COL=FSC_COLOR(all_colors(m)),THICK=200,SYM=4,SIZE=70
ENDFOR
GLEGEND,labels=REVERSE(all_descs(2:nmodels-1:3)),COL=REVERSE(FSC_COLOR(all_colors(2:nmodels-1:3))),LENGTH=0,SYM=[REPLICATE(3,nmodels/3)],$
        LEGXOFFSET=10000,LEGYOFFSET=16000
;GLEGEND,labels=REVERSE(['e-folding timescale','5% significance level']),SYM=REVERSE([3,4]),LEGXOFFSET=9000,LEGYOFFSET=13000,LENGTH=0
AXES,XSTEP=5,XMINOR=1,YVALS=findgen(nlev_save),YLABELS=STRMID(STRTRIM(STRING(lev_save),1),0,5),$
     XTITLE='Lag (days)',YTITLE='Pressure (hPa)'
PSCLOSE

; All models for Jun-Feb
nlevs_corr=N_ELEMENTS(mylevs_corr)
psfile='/home/ss901165/idl/hadgem3-kpp_runs/nao/hadgem3kpp_ao_autocorr_daily_n96_ent.jun-aug_owneof_allplev.ps'
PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=140,MARGIN=2000,XSIZE=16000,YSIZE=16000,XOFFSET=1500,YOFFSET=2000,SPACE2=2000,SPACE3=200
GSET,XMIN=0,XMAX=MAX(lags),YMIN=0,YMAX=nlev_save
FOR m=2,nmodels-1,3 DO BEGIN
   ;toplot=REFORM(efold(m,0:nlev-1))
   ;toplot=toplot[REFORM(lev_toplot(m,*))]
   ;GPLOT,X=toplot-0.3*0.05+m,Y=findgen(nlev_save),STYLE=0,COL=FSC_COLOR(all_colors(m)),THICK=200,SYM=3,SIZE=70
   toplot=REFORM(plev(m,0:nlev-1))
   toplot=toplot[REFORM(lev_toplot(m,*))]
   GPLOT,X=toplot-0.3+0.05*m,Y=findgen(nlev_save),STYLE=0,COL=FSC_COLOR(all_colors(m)),THICK=200,SYM=3,SIZE=70
ENDFOR
GLEGEND,labels=REVERSE(all_descs(2:nmodels-1:3)),COL=REVERSE(FSC_COLOR(all_colors(2:nmodels-1:3))),LENGTH=0,SYM=[REPLICATE(3,nmodels/3)],$
        LEGXOFFSET=10000,LEGYOFFSET=16000
;GLEGEND,labels=REVERSE(['e-folding timescale','5% significance level']),SYM=REVERSE([3,4]),LEGXOFFSET=9000,LEGYOFFSET=13000,LENGTH=0
AXES,XSTEP=5,XMINOR=1,YVALS=findgen(nlev_save),YLABELS=STRMID(STRTRIM(STRING(lev_save),1),0,5),$
     XTITLE='Lag (days)',YTITLE='Pressure (hPa)'
PSCLOSE

STOP
END
