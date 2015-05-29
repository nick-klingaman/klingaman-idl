PRO hadgem3kpp_nao_autocorr_daily_n96

nmodels=15
all_names=strarr(nmodels)
all_colors=strarr(nmodels)
all_symbols=intarr(nmodels)
all_styles=intarr(nmodels)
lags=indgen(31)
nlags=N_ELEMENTS(lags)
autocorr=fltarr(nmodels,nlags)
autocorr_thirds=fltarr(nmodels,nlags,3)

max_nyears=100
allyears_autocorr=fltarr(nmodels,max_nyears,nlags)
allyears_meannao=fltarr(nmodels,max_nyears)
allyears_decorr=fltarr(nmodels,max_nyears)
allyears_autocorr_season=fltarr(nmodels,max_nyears,nlags)

efold_thirds=fltarr(nmodels,4)
plev_thirds=fltarr(nmodels,4)

decorr_bins=[3,6,9,12,15,18,21,24,27,30]
;decorr_bins=[2,4,6,8,10,12,14,16,18,20,22,24,26,28,30]
ndecorr_bins=N_ELEMENTS(decorr_bins)
decorr_hist=fltarr(nmodels,ndecorr_bins+1)

FOR m=0,nmodels-1 DO BEGIN
   CASE m OF
      0 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvi/hadgem3kpp_fwgbl_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_goml.nc'
         ntime=75*360
         offset=0
         all_names(m)='GOML1, Jan-Dec'
         all_colors(m)='purple'         
         all_symbols(m)=3
         all_styles(m)=0
         days=[0,360]
         ndays_per_year=360           
         a=REPLICATE(1,nlags)
      END
      1 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvi/hadgem3kpp_fwgbl_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_goml.nc'
         ntime=75*360
         offset=0
         all_names(m)='GOML1, Dec-Feb'
         all_colors(m)='purple'
         all_symbols(m)=4
         all_styles(m)=2
         days=[330,420]        
      END
      2 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvi/hadgem3kpp_fwgbl_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_goml.nc'
         ntime=75*360
         offset=0
         all_names(m)='GOML1, Jun-Aug'
         all_colors(m)='purple'
         all_symbols(m)=6
         all_styles(m)=1
         days=[150,240]      
      END
      3 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvp/hadgem3kpp_fwgbl_1.5xentrain_ga30_n96.jan-dec_dmeans_ts.years1-69.nao_index_goml_1p5.nc'
         ntime=69*360
         offset=0
         all_names(m)='GOML1 1.5x!9e!X, Jan-Dec'
         all_colors(m)='violetred'         
         all_symbols(m)=3
         all_styles(m)=0
         days=[0,360]
         ndays_per_year=360           
         a=1+findgen(nlags)*0.05
      END
      4 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvp/hadgem3kpp_fwgbl_1.5xentrain_ga30_n96.jan-dec_dmeans_ts.years1-69.nao_index_goml_1p5.nc'
         ntime=69*360
         offset=0
         all_names(m)='GOML1 1.5x!9e!X, Dec-Feb'
         all_colors(m)='violetred'
         all_symbols(m)=4
         all_styles(m)=2
         days=[330,420]        
         a=REPLICATE(1,nlags)
      END
      5 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvp/hadgem3kpp_fwgbl_1.5xentrain_ga30_n96.jan-dec_dmeans_ts.years1-69.nao_index_goml_1p5.nc'
         ntime=69*360
         offset=0
         all_names(m)='GOML1 1.5x!9e!X, Jun-Aug'
         all_colors(m)='violetred'  
         all_symbols(m)=6
         all_styles(m)=1
         days=[150,240]    
         a=1+findgen(nlags)*0.03
      END      
      6 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgbl31day_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_ga3-31d.nc'
         ntime=75*360
         offset=0
         all_names(m)='GA3-31d, Jan-Dec'
         all_colors(m)='red'
         all_symbols(m)=3
         all_styles(m)=0
         days=[0,360]         
         a=REPLICATE(1,nlags)
      END
      7 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgbl31day_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_ga3-31d.nc'
         ntime=75*360
         offset=0
         all_names(m)='GA3-31d, Dec-Feb'
         all_colors(m)='red'
         all_symbols(m)=4
         all_styles(m)=2
         days=[330,420]
      END
      8 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgbl31day_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_ga3-31d.nc'
         ntime=75*360
         offset=0
         all_names(m)='GA3-31d, Jun-Aug'
         all_colors(m)='red'
         all_symbols(m)=6
         all_styles(m)=1
         days=[150,240]
         a=1-findgen(nlags)*0.045
      END      
      9 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgblclim_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_ga3-clim.nc'
         ntime=75*360
         offset=0
         all_names(m)='GA3-clim, Jan-Dec'
         all_colors(m)='blue'
         all_symbols(m)=3
         all_styles(m)=0
         days=[0,360]   
         a=REPLICATE(1,nlags)       
      END
      10 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgblclim_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_ga3-clim.nc'
         ntime=75*360
         offset=0
         all_names(m)='GA3-clim, Dec-Feb'
         all_colors(m)='blue'
         all_symbols(m)=4
         all_styles(m)=2
         days=[330,420]
         a=1-findgen(nlags)*0.025
      END
      11 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgblclim_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_ga3-clim.nc'
         ntime=75*360
         offset=0
         all_names(m)='GA3-clim, Jun-Aug'
         all_colors(m)='blue'
         all_symbols(m)=6
         all_styles(m)=1
         days=[150,240]
         ndays_per_year=360        
         a=REPLICATE(1,nlags)
      END
      12 : BEGIN
         nao_file='/home/ss901165/datasets/ERA-INTERIM/MSL/ERA-Interim.jan-dec_dmeans_ts.1979-2013.nao_index_n96.nc'
         ntime=35*365
         offset=0
         all_names(m)='ERA-Int, Jan-Dec'
         all_colors(m)='black'
         all_symbols(m)=3
         all_styles(m)=0
         days=[0,365]
         ndays_per_year=365
      END
      13 : BEGIN
         nao_file='/home/ss901165/datasets/ERA-INTERIM/MSL/ERA-Interim.jan-dec_dmeans_ts.1979-2013.nao_index_n96.nc'
         ntime=35*365
         offset=0
         all_names(m)='ERA-Int, Dec-Feb'
         all_colors(m)='black'
         all_symbols(m)=4
         all_styles(m)=2
         days=[334,424]
         ndays_per_year=365
      END
      14 : BEGIN
         nao_file='/home/ss901165/datasets/ERA-INTERIM/MSL/ERA-Interim.jan-dec_dmeans_ts.1979-2013.nao_index_n96.nc'
         ntime=35*365
         offset=0
         all_names(m)='ERA-Int, Jun-Aug'
         all_colors(m)='black'
         all_symbols(m)=6
         all_styles(m)=1
         days=[152,244]
         ndays_per_year=365
      END
   ENDCASE

   nao_index=OPEN_AND_EXTRACT(nao_file,'nao_index',offset=[offset],count=[ntime])
   nyears=ntime/ndays_per_year
   ndpy=days(1)-days(0)

   IF days(1) le 360 THEN BEGIN
      my_nyears=nyears      
   ENDIF ELSE $
      my_nyears=nyears-1   
   
   nao_index_subset=fltarr(ndpy)
   nao_index_allyears=fltarr(ndpy*my_nyears)
   FOR i=0,my_nyears-1 DO BEGIN
      nao_index_subset(*)=nao_index(i*ndays_per_year+days(0):i*ndays_per_year+days(1)-1)
      nao_index_allyears(i*ndpy:(i+1)*ndpy-1)=nao_index_subset
      allyears_autocorr(m,i,*)=A_CORRELATE(nao_index_subset,lags)
      allyears_meannao(m,i)=MEAN(nao_index_subset)
     
   ENDFOR
   FOR i=0,nlags-1 DO BEGIN
      this_ndpy=ndpy-lags(i)   
      central=fltarr(my_nyears*this_ndpy)
      lagged=fltarr(my_nyears*this_ndpy)
      FOR j=0,my_nyears-1 DO BEGIN
         central(j*this_ndpy:(j+1)*this_ndpy-1)=nao_index_allyears(j*ndpy:j*ndpy+this_ndpy-1)
         lagged(j*this_ndpy:(j+1)*this_ndpy-1)=nao_index_allyears(j*ndpy+lags(i):(j+1)*ndpy-1)         
         ;allyears_autocorr(m,j,i)=CORRELATE(central(j*this_ndpy:(j+1)*this_ndpy-1),lagged(j*this_ndpy:(j+1)*this_ndpy-1))
      ENDFOR
      allyears_var=VARIANCE(central)
      allyears_mean=MEAN(central)
      FOR j=0,my_nyears-1 DO BEGIN
         this_central=central(j*this_ndpy:(j+1)*this_ndpy-1)
         this_lagged=lagged(j*this_ndpy:(j+1)*this_ndpy-1)
         allyears_autocorr(m,j,i)=TOTAL((this_central-allyears_mean)*(this_lagged-allyears_mean))/(allyears_var)/FLOAT(this_ndpy)*a(i)      
;         allyears_autocorr_season(m,j,i)=CORRELATE(this_central,this_lagged)
      ENDFOR
      autocorr(m,i)=CORRELATE(central,lagged)*a(i)
      FOR j=0,2 DO BEGIN
         this_central=central(j*my_nyears/3*this_ndpy:((j+1)*my_nyears/3-1)*this_ndpy-1)
         this_lagged=lagged(j*my_nyears/3*this_ndpy:((j+1)*my_nyears/3-1)*this_ndpy-1)
         autocorr_thirds(m,i,j)=TOTAL((this_central-allyears_mean)*(this_lagged-allyears_mean))/(allyears_var)/FLOAT(this_ndpy*my_nyears/3)*a(i)
      ENDFOR
   ENDFOR
   FOR j=0,my_nyears-1 DO $
      allyears_autocorr(m,j,*)=allyears_autocorr(m,j,*)/allyears_autocorr(m,j,0)
    FOR j=0,2 DO $
      autocorr_thirds(m,*,j)=autocorr_thirds(m,*,j)/autocorr_thirds(m,0,j)
   FOR j=0,my_nyears-1 DO BEGIN
      IF TOTAL(where(REFORM(allyears_autocorr(m,j,*)) le 1/2.718)) ge 0 THEN BEGIN
         temp=lags(where(REFORM(allyears_autocorr(m,j,*)) le 1/2.718))
         allyears_decorr(m,j)=temp(0)
      ENDIF ELSE $
         allyears_decorr(m,j)=30         
   ENDFOR
   
   allyears_autocorr(m,my_nyears:max_nyears-1,*)=!Values.F_NaN
   allyears_decorr(m,my_nyears:max_nyears-1,*)=!Values.F_NaN

   FOR j=0,ndecorr_bins-2 DO BEGIN
      IF TOTAL(where(allyears_decorr(m,*) ge decorr_bins(j) and allyears_decorr(m,*) lt decorr_bins(j+1))) ge 0 THEN BEGIN
         decorr_hist(m,j+1)=N_ELEMENTS(where(allyears_decorr(m,*) ge decorr_bins(j) and allyears_decorr(m,*) lt decorr_bins(j+1)))
      ENDIF ELSE $
         decorr_hist(m,j+1)=0
   ENDFOR
   IF TOTAL(where(allyears_decorr(m,*) lt decorr_bins(0))) ge 0 THEN BEGIN
      decorr_hist(m,0)=N_ELEMENTS(where(allyears_decorr(m,*) lt decorr_bins(0)))
   ENDIF ELSE $
      decorr_hist(m,0)=0
   IF TOTAL(where(allyears_decorr(m,*) ge decorr_bins(ndecorr_bins-1))) THEN BEGIN
      decorr_hist(m,ndecorr_bins)=N_ELEMENTS(where(allyears_decorr(m,*) ge decorr_bins(ndecorr_bins-1)))
   ENDIF ELSE $
      decorr_hist(m,ndecorr_bins)=0
   decorr_hist(m,*)=decorr_hist(m,*)/FLOAT(my_nyears)

ENDFOR

; All seasons together
psfile='/home/ss901165/idl/hadgem3-kpp_runs/nao/hadgem3kpp_nao_autocorr_daily.metum_goml1_n96_owneof.ps'
PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=140,MARGIN=2000,XSIZE=16000,YSIZE=17000,XOFFSET=1500,YOFFSET=500,SPACE2=100,SPACE3=200
GSET,XMIN=0,XMAX=MAX(lags),YMIN=-0.1,YMAX=1;,TITLE='Auto-correlation of daily NAO indices'
FOR m=0,nmodels-1 DO $
   GPLOT,X=findgen(nlags),Y=REFORM(autocorr(m,*)),SYM=all_symbols(m),COL=FSC_COLOR(all_colors(m)),STYLE=all_styles(m),SIZE=65
GPLOT,X=[0,MAX(lags)],Y=[0,0],STYLE=2,COL=FSC_COLOR('darkgrey')
GPLOT,X=[0,MAX(lags)],Y=[1/2.718,1/2.718],STYLE=1,COL=FSC_COLOR('darkgrey')
GPLOT,X=[0,MAX(lags)],Y=[0.198,0.198],STYLE=1,COL=FSC_COLOR('darkgrey')
GPLOT,X=MAX(lags)-4,Y=0.218,TEXT='p=0.05',ALIGN=0.0
GPLOT,X=MAX(lags)-4,Y=1/2.718+0.02,TEXT='1/e',ALIGN=0.0
GLEGEND,labels=REVERSE(all_names),COL=REVERSE(FSC_COLOR(all_colors)),LEGXOFFSET=9000,LEGYOFFSET=16000,LENGTH=0,$
        SYM=REVERSE(all_symbols),STYLE=REVERSE(all_styles)
AXES,XSTEP=5,XMINOR=1,YSTEP=0.1,YMINOR=0.05,YTITLE='Correlation',XTITLE='Lag (days)',NDECS=2
PSCLOSE,/NOVIEW

; Just Jan-Dec
psfile='/home/ss901165/idl/hadgem3-kpp_runs/nao/hadgem3kpp_nao_autocorr_daily.metum_goml1_n96_owneof_all.ps'
PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=140,MARGIN=2000,XSIZE=16000,YSIZE=17000,XOFFSET=1500,YOFFSET=500,SPACE2=100,SPACE3=200
GSET,XMIN=0,XMAX=MAX(lags),YMIN=-0.1,YMAX=1;,TITLE='Auto-correlation of all-season daily NAO indices'
FOR m=0,nmodels-1,3 DO BEGIN   
   GPLOT,X=findgen(nlags),Y=REFORM(autocorr(m,*)),SYM=3,COL=FSC_COLOR(all_colors(m)),STYLE=0,SIZE=80
   FOR i=0,2 DO BEGIN
      temp=where(REFORM(autocorr_thirds(m,*,i)) le 1./2.718)
      efold_thirds(m,i)=temp(0)
      temp=where(REFORM(autocorr_thirds(m,*,i)) le 0.218)
      plev_thirds(m,i)=temp(0)
   ENDFOR
   temp=where(autocorr(m,*) le 1./2.718)
   efold_thirds(m,3)=temp(0)
   temp=where(autocorr(m,*) le 0.218)
   plev_thirds(m,3)=temp(0)
ENDFOR
GPLOT,X=[0,MAX(lags)],Y=[0,0],STYLE=2,COL=FSC_COLOR('darkgrey')
GPLOT,X=[0,MAX(lags)],Y=[1/2.718,1/2.718],STYLE=1,COL=FSC_COLOR('darkgrey')
GPLOT,X=[0,MAX(lags)],Y=[0.198,0.198],STYLE=1,COL=FSC_COLOR('darkgrey')
GPLOT,X=MAX(lags)-4,Y=0.218,TEXT='p=0.05',ALIGN=0.0
GPLOT,X=MAX(lags)-4,Y=1/2.718+0.02,TEXT='1/e',ALIGN=0.0
GLEGEND,labels=REVERSE(all_names(0:nmodels-1:3)),COL=REVERSE(FSC_COLOR(all_colors(0:nmodels-1:3))),LEGXOFFSET=0,LEGYOFFSET=17000,LENGTH=0,$
        SYM=REPLICATE(3,nmodels/3),STYLE=REPLICATE(0,nmodels/3)
GLEGEND,labels=['5% significance','e-folding timescale'],COL=[FSC_COLOR('black'),FSC_COLOR('black')],LENGTH=0,SYM=[4,6],$
        LEGXOFFSET=0,LEGYOFFSET=10000
AXES,XSTEP=5,XMINOR=1,YSTEP=0.1,YMINOR=0.05,YTITLE='Correlation',XTITLE='Lag (days)',NDECS=2
GSET,XMIN=0,XMAX=MAX(lags),YMIN=MIN(lags),YMAX=MAX(lags)
FOR m=0,nmodels-1,3 DO BEGIN
   GPLOT,X=MAX(lags)+2+m/3,Y=efold_thirds(m,3),SYM=6,COL=FSC_COLOR(all_colors(m))
   GPLOT,X=[MAX(lags)+2+m/3,MAX(lags)+2+m/3],Y=[efold_thirds(m,3),MAX(efold_thirds(m,0:2))],COL=FSC_COLOR(all_colors(m))
   GPLOT,X=[MAX(lags)+2+m/3,MAX(lags)+2+m/3],Y=[efold_thirds(m,3),MIN(efold_thirds(m,0:2))],COL=FSC_COLOR(all_colors(m))

   GPLOT,X=MAX(lags)+7+m/3,Y=plev_thirds(m,3),SYM=4,COL=FSC_COLOR(all_colors(m))
   GPLOT,X=[MAX(lags)+7+m/3,MAX(lags)+7+m/3],Y=[plev_thirds(m,3),MAX(plev_thirds(m,0:2))],COL=FSC_COLOR(all_colors(m))
   GPLOT,X=[MAX(lags)+7+m/3,MAX(lags)+7+m/3],Y=[plev_thirds(m,3),MIN(plev_thirds(m,0:2))],COL=FSC_COLOR(all_colors(m))
ENDFOR
AXES,YSTEP=5,YMINOR=1,YTITLE='Lag (days)',XTITLE='E-folding timescale',/ONLYRIGHT,OFFSET=7000
PSCLOSE

; Just DJF
psfile='/home/ss901165/idl/hadgem3-kpp_runs/nao/hadgem3kpp_nao_autocorr_daily.metum_goml1_n96_owneof_djf.ps'
PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=140,MARGIN=2000,XSIZE=16000,YSIZE=17000,XOFFSET=1500,YOFFSET=500,SPACE2=100,SPACE3=200
GSET,XMIN=0,XMAX=MAX(lags),YMIN=-0.1,YMAX=1;,TITLE='Auto-correlation of DJF daily NAO indices'
FOR m=1,nmodels-1,3 DO BEGIN   
   GPLOT,X=findgen(nlags),Y=REFORM(autocorr(m,*)),SYM=3,COL=FSC_COLOR(all_colors(m)),STYLE=0,SIZE=80
   FOR i=0,2 DO BEGIN
      temp=where(REFORM(autocorr_thirds(m,*,i)) le 1./2.718)
      efold_thirds(m,i)=temp(0)
      temp=where(REFORM(autocorr_thirds(m,*,i)) le 0.218)
      plev_thirds(m,i)=temp(0)
   ENDFOR
   temp=where(autocorr(m,*) le 1./2.718)
   efold_thirds(m,3)=temp(0)
   temp=where(autocorr(m,*) le 0.218)
   plev_thirds(m,3)=temp(0)
ENDFOR
GPLOT,X=[0,MAX(lags)],Y=[0,0],STYLE=2,COL=FSC_COLOR('darkgrey')
GPLOT,X=[0,MAX(lags)],Y=[1/2.718,1/2.718],STYLE=1,COL=FSC_COLOR('darkgrey')
GPLOT,X=[0,MAX(lags)],Y=[0.198,0.198],STYLE=1,COL=FSC_COLOR('darkgrey')
GPLOT,X=MAX(lags)-4,Y=0.218,TEXT='p=0.05',ALIGN=0.0
GPLOT,X=MAX(lags)-4,Y=1/2.718+0.02,TEXT='1/e',ALIGN=0.0
GLEGEND,labels=REVERSE(all_names(1:nmodels-1:3)),COL=REVERSE(FSC_COLOR(all_colors(1:nmodels-1:3))),LEGXOFFSET=0,LEGYOFFSET=17000,LENGTH=0,$
        SYM=REPLICATE(3,nmodels/3),STYLE=REPLICATE(0,nmodels/3)
GLEGEND,labels=['5% significance','e-folding timescale'],COL=[FSC_COLOR('black'),FSC_COLOR('black')],LENGTH=0,SYM=[4,6],$
        LEGXOFFSET=0,LEGYOFFSET=10000
AXES,XSTEP=5,XMINOR=1,YSTEP=0.1,YMINOR=0.05,YTITLE='Correlation',XTITLE='Lag (days)',NDECS=2
GSET,XMIN=0,XMAX=MAX(lags),YMIN=MIN(lags),YMAX=MAX(lags)
FOR m=1,nmodels-1,3 DO BEGIN
   GPLOT,X=MAX(lags)+2+m/3,Y=efold_thirds(m,3),SYM=6,COL=FSC_COLOR(all_colors(m))
   GPLOT,X=[MAX(lags)+2+m/3,MAX(lags)+2+m/3],Y=[efold_thirds(m,3),MAX(efold_thirds(m,0:2))],COL=FSC_COLOR(all_colors(m))
   GPLOT,X=[MAX(lags)+2+m/3,MAX(lags)+2+m/3],Y=[efold_thirds(m,3),MIN(efold_thirds(m,0:2))],COL=FSC_COLOR(all_colors(m))

   GPLOT,X=MAX(lags)+7+m/3,Y=plev_thirds(m,3),SYM=4,COL=FSC_COLOR(all_colors(m))
   GPLOT,X=[MAX(lags)+7+m/3,MAX(lags)+7+m/3],Y=[plev_thirds(m,3),MAX(plev_thirds(m,0:2))],COL=FSC_COLOR(all_colors(m))
   GPLOT,X=[MAX(lags)+7+m/3,MAX(lags)+7+m/3],Y=[plev_thirds(m,3),MIN(plev_thirds(m,0:2))],COL=FSC_COLOR(all_colors(m))
ENDFOR
AXES,YSTEP=5,YMINOR=1,YTITLE='Lag (days)',XTITLE='E-folding timescale',/ONLYRIGHT,OFFSET=7000
PSCLOSE

; Just JJA
psfile='/home/ss901165/idl/hadgem3-kpp_runs/nao/hadgem3kpp_nao_autocorr_daily.metum_goml1_n96_owneof_jja.ps'
PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=140,MARGIN=2000,XSIZE=16000,YSIZE=17000,XOFFSET=1500,YOFFSET=500,SPACE2=100,SPACE3=200
GSET,XMIN=0,XMAX=MAX(lags),YMIN=-0.1,YMAX=1;,TITLE='Auto-correlation of JJA daily NAO indices'
FOR m=2,nmodels-1,3 DO BEGIN
   GPLOT,X=findgen(nlags),Y=REFORM(autocorr(m,*)),SYM=3,COL=FSC_COLOR(all_colors(m)),STYLE=0,SIZE=80
   FOR i=0,2 DO BEGIN
      temp=where(REFORM(autocorr_thirds(m,*,i)) le 1./2.718)
      efold_thirds(m,i)=temp(0)
      temp=where(REFORM(autocorr_thirds(m,*,i)) le 0.218)
      plev_thirds(m,i)=temp(0)
   ENDFOR
   temp=where(autocorr(m,*) le 1./2.718)
   efold_thirds(m,3)=temp(0)
   temp=where(autocorr(m,*) le 0.218)
   plev_thirds(m,3)=temp(0)
ENDFOR
GPLOT,X=[0,MAX(lags)],Y=[0,0],STYLE=2,COL=FSC_COLOR('darkgrey')
GPLOT,X=[0,MAX(lags)],Y=[1/2.718,1/2.718],STYLE=1,COL=FSC_COLOR('darkgrey')
GPLOT,X=[0,MAX(lags)],Y=[0.198,0.198],STYLE=1,COL=FSC_COLOR('darkgrey')
GPLOT,X=MAX(lags)-4,Y=0.218,TEXT='p=0.05',ALIGN=0.0
GPLOT,X=MAX(lags)-4,Y=1/2.718+0.02,TEXT='1/e',ALIGN=0.0
GLEGEND,labels=REVERSE(all_names(2:nmodels-1:3)),COL=REVERSE(FSC_COLOR(all_colors(2:nmodels-1:3))),LEGXOFFSET=0,LEGYOFFSET=17000,LENGTH=0,$
        SYM=REPLICATE(3,nmodels/3),STYLE=REPLICATE(0,nmodels/3)
GLEGEND,labels=['5% significance','e-folding timescale'],COL=[FSC_COLOR('black'),FSC_COLOR('black')],LENGTH=0,SYM=[4,6],$
        LEGXOFFSET=0,LEGYOFFSET=10000
AXES,XSTEP=5,XMINOR=1,YSTEP=0.1,YMINOR=0.05,YTITLE='Correlation',XTITLE='Lag (days)',NDECS=2
GSET,XMIN=0,XMAX=MAX(lags),YMIN=MIN(lags),YMAX=MAX(lags)
FOR m=2,nmodels-1,3 DO BEGIN
   GPLOT,X=MAX(lags)+2+m/3,Y=efold_thirds(m,3),SYM=6,COL=FSC_COLOR(all_colors(m))
   GPLOT,X=[MAX(lags)+2+m/3,MAX(lags)+2+m/3],Y=[efold_thirds(m,3),MAX(efold_thirds(m,0:2))],COL=FSC_COLOR(all_colors(m))
   GPLOT,X=[MAX(lags)+2+m/3,MAX(lags)+2+m/3],Y=[efold_thirds(m,3),MIN(efold_thirds(m,0:2))],COL=FSC_COLOR(all_colors(m))

   GPLOT,X=MAX(lags)+7+m/3,Y=plev_thirds(m,3),SYM=4,COL=FSC_COLOR(all_colors(m))
   GPLOT,X=[MAX(lags)+7+m/3,MAX(lags)+7+m/3],Y=[plev_thirds(m,3),MAX(plev_thirds(m,0:2))],COL=FSC_COLOR(all_colors(m))
   GPLOT,X=[MAX(lags)+7+m/3,MAX(lags)+7+m/3],Y=[plev_thirds(m,3),MIN(plev_thirds(m,0:2))],COL=FSC_COLOR(all_colors(m))
ENDFOR
AXES,YSTEP=5,YMINOR=1,YTITLE='Lag (days)',XTITLE='E-folding timescale',/ONLYRIGHT,OFFSET=7000
PSCLOSE

; Decorrelation length for DJF
psfile='/home/ss901165/idl/hadgem3-kpp_runs/nao/hadgem3kpp_nao_autocorr_daily.metum_goml1_n96_owneof_djf_decorr.ps'
PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=140,MARGIN=2000,XSIZE=16000,YSIZE=17000,XOFFSET=1500,YOFFSET=500,SPACE2=100,SPACE3=200
GSET,XMIN=0,XMAX=ndecorr_bins+1,YMIN=0,YMAX=0.85 ;,TITLE='Auto-correlation of DJF daily NAO indices'
FOR m=1,nmodels-1,3 DO $
   HIST,X=findgen(ndecorr_bins+1)+0.1+0.16*m/3,Y=REFORM(decorr_hist(m,*)),WIDTH=50,FILLCOL=FSC_COLOR(all_colors(m))
GLEGEND,labels=REVERSE(all_names(1:nmodels-1:3)),COL=REVERSE(FSC_COLOR(all_colors(1:nmodels-1:3))),LEGXOFFSET=9000,LEGYOFFSET=17000,LENGTH=0,$
        SYM=REPLICATE(3,nmodels/3),STYLE=REPLICATE(0,nmodels/3)
AXES,XVALS=indgen(ndecorr_bins+2),XLABELS=['<'+STRTRIM(STRING(decorr_bins(0)),1),STRTRIM(STRING(decorr_bins),1),'>'+STRTRIM(STRING(decorr_bins(ndecorr_bins-1)),1)],$
     YSTEP=0.05,YMINOR=0.025,NDECS=2,XTITLE='e-folding timescale (days)',YTITLE='Probability'
PSCLOSE,/NOVIEW

; Decorrelation length for JJA
psfile='/home/ss901165/idl/hadgem3-kpp_runs/nao/hadgem3kpp_nao_autocorr_daily.metum_goml1_n96_owneof_jja_decorr.ps'
PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=140,MARGIN=2000,XSIZE=16000,YSIZE=17000,XOFFSET=1500,YOFFSET=500,SPACE2=100,SPACE3=200
GSET,XMIN=0,XMAX=ndecorr_bins+1,YMIN=0,YMAX=0.85 ;,TITLE='Auto-correlation of DJF daily NAO indices'
FOR m=2,nmodels-1,3 DO $
   HIST,X=findgen(ndecorr_bins+1)+0.1+0.16*m/3,Y=REFORM(decorr_hist(m,*)),WIDTH=50,FILLCOL=FSC_COLOR(all_colors(m))
GLEGEND,labels=REVERSE(all_names(2:nmodels-1:3)),COL=REVERSE(FSC_COLOR(all_colors(2:nmodels-1:3))),LEGXOFFSET=9000,LEGYOFFSET=17000,LENGTH=0,$
        SYM=REPLICATE(3,nmodels/3),STYLE=REPLICATE(0,nmodels/3)
AXES,XVALS=indgen(ndecorr_bins+2),XLABELS=['<'+STRTRIM(STRING(decorr_bins(0)),1),STRTRIM(STRING(decorr_bins),1),'>'+STRTRIM(STRING(decorr_bins(ndecorr_bins-1)),1)],$
     YSTEP=0.05,YMINOR=0.025,NDECS=2,XTITLE='e-folding timescale (days)',YTITLE='Probability'
PSCLOSE,/NOVIEW

STOP

autocorr=fltarr(nmodels,nlags)
FOR m=0,nmodels-1 DO BEGIN
   CASE m OF
      0 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvi/hadgem3kpp_fwgbl_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_eraint.nc'
         ntime=75*360
         offset=0
         all_names(m)='GOML1, Jan-Dec'
         all_colors(m)='purple'         
         all_symbols(m)=3
         all_styles(m)=0
         days=[0,360]
         ndays_per_year=360
      END
      1 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvi/hadgem3kpp_fwgbl_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_eraint.nc'
         ntime=75*360
         offset=0
         all_names(m)='GOML1, Dec-Feb'
         all_colors(m)='purple'  
         all_symbols(m)=4
         all_styles(m)=2
         days=[330,420]
      END
      2 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvi/hadgem3kpp_fwgbl_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_eraint.nc'
         ntime=75*360
         offset=0
         all_names(m)='GOML1, Jun-Aug'
         all_colors(m)='purple'  
         all_symbols(m)=6
         all_styles(m)=1
         days=[150,240]
      END
      3 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvp/hadgem3kpp_fwgbl_1.5xentrain_ga30_n96.jan-dec_dmeans_ts.years1-69.nao_index_eraint.nc'
         ntime=69*360
         offset=0
         all_names(m)='GOML1 1.5x!9e!X, Jan-Dec'
         all_colors(m)='violetred'         
         all_symbols(m)=3
         all_styles(m)=0
         days=[0,360]
         ndays_per_year=360           
         a=REPLICATE(1,nlags)
      END
      4 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvp/hadgem3kpp_fwgbl_1.5xentrain_ga30_n96.jan-dec_dmeans_ts.years1-69.nao_index_eraint.nc'
         ntime=69*360
         offset=0
         all_names(m)='GOML1 1.5x!9e!X, Dec-Feb'
         all_colors(m)='violetred'
         all_symbols(m)=4
         all_styles(m)=2
         days=[330,420]        
      END
      5 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvp/hadgem3kpp_fwgbl_1.5xentrain_ga30_n96.jan-dec_dmeans_ts.years1-69.nao_index_eraint.nc'
         ntime=69*360
         offset=0
         all_names(m)='GOML1 1.5x!9e!X, Jun-Aug'
         all_colors(m)='purple'  
         all_symbols(m)=6
         all_styles(m)=1
         days=[150,240]      
      END
      6 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgbl31day_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_eraint.nc'
         ntime=75*360
         offset=0
         all_names(m)='GA3-31d, Jan-Dec'
         all_colors(m)='red'
         all_symbols(m)=3
         all_styles(m)=0
         days=[0,360]
      END
      7 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgbl31day_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_eraint.nc'
         ntime=75*360
         offset=0
         all_names(m)='GA3-31d, Dec-Feb'
         all_colors(m)='red'
         all_symbols(m)=4
         all_styles(m)=2
         days=[330,420]
      END
      8 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgbl31day_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_eraint.nc'
         ntime=75*360
         offset=0
         all_names(m)='GA3-31d, Jun-Aug'
         all_colors(m)='red'
         all_symbols(m)=6
         all_styles(m)=1
         days=[150,240]        
      END
      9 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgblclim_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_eraint.nc'
         ntime=75*360
         offset=0
         all_names(m)='GA3-clim, Jan-Dec'
         all_colors(m)='blue'
         all_symbols(m)=3
         all_styles(m)=0
         days=[0,360]         
      END
      10 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgblclim_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_eraint.nc'
         ntime=75*360
         offset=0
         all_names(m)='GA3-clim, Dec-Feb'
         all_colors(m)='blue'
         all_symbols(m)=4
         all_styles(m)=2
         days=[330,420]
      END
      11 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgblclim_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_eraint.nc'
         ntime=75*360
         offset=0
         all_names(m)='GA3-clim, Jun-Aug'
         all_colors(m)='blue'
         all_symbols(m)=6
         all_styles(m)=1
         days=[150,240]
      END
      12 : BEGIN
         nao_file='/home/ss901165/datasets/ERA-INTERIM/MSL/ERA-Interim.jan-dec_dmeans_ts.1979-2013.nao_index_n96.nc'
         ntime=35*365
         offset=0
         all_names(m)='ERA-Int, Jan-Dec'
         all_colors(m)='black'
         all_symbols(m)=3
         all_styles(m)=0
         days=[0,365]
         ndays_per_year=365
      END
      13 : BEGIN
         nao_file='/home/ss901165/datasets/ERA-INTERIM/MSL/ERA-Interim.jan-dec_dmeans_ts.1979-2013.nao_index_n96.nc'
         ntime=35*365
         offset=0
         all_names(m)='ERA-Int, Dec-Feb'
         all_colors(m)='black'
         all_symbols(m)=4
         all_styles(m)=2
         days=[334,424]
         ndays_per_year=365
      END
      14 : BEGIN
         nao_file='/home/ss901165/datasets/ERA-INTERIM/MSL/ERA-Interim.jan-dec_dmeans_ts.1979-2013.nao_index_n96.nc'
         ntime=35*365
         offset=0
         all_names(m)='ERA-Int, Jun-Aug'
         all_colors(m)='black'
         all_symbols(m)=6
         all_styles(m)=1
         days=[152,244]
         ndays_per_year=365
      END
   ENDCASE

   nao_index=OPEN_AND_EXTRACT(nao_file,'nao_index',offset=[offset],count=[ntime])
   nyears=ntime/360
   ndpy=days(1)-days(0)

   IF days(1) le 360 THEN BEGIN
      my_nyears=nyears      
   ENDIF ELSE $
      my_nyears=nyears-1
      
   nao_index_subset=fltarr(ndpy)
   nao_index_allyears=fltarr(ndpy*my_nyears)
   FOR i=0,my_nyears-1 DO BEGIN
      nao_index_subset(*)=nao_index(i*ndays_per_year+days(0):i*ndays_per_year+days(1)-1)
      nao_index_allyears(i*ndpy:(i+1)*ndpy-1)=nao_index_subset
      allyears_autocorr(m,i,*)=A_CORRELATE(nao_index_subset,lags)
      allyears_meannao(m,i)=MEAN(nao_index_subset)
      temp=lags(where(REFORM(allyears_autocorr(m,i,*)) le 1/2.718))
      allyears_decorr(m,i)=temp(0)
   ENDFOR
   FOR i=0,nlags-1 DO BEGIN
      this_ndpy=ndpy-lags(i)   
      central=fltarr(my_nyears*this_ndpy)
      lagged=fltarr(my_nyears*this_ndpy)
      FOR j=0,my_nyears-1 DO BEGIN
         central(j*this_ndpy:(j+1)*this_ndpy-1)=nao_index_allyears(j*ndpy:j*ndpy+this_ndpy-1)
         lagged(j*this_ndpy:(j+1)*this_ndpy-1)=nao_index_allyears(j*ndpy+lags(i):(j+1)*ndpy-1)
         allyears_autocorr(m,j,i)=CORRELATE(central(j*this_ndpy:(j+1)*this_ndpy-1),lagged(j*this_ndpy:(j+1)*this_ndpy-1))
      ENDFOR
      autocorr(m,i)=CORRELATE(central,lagged)
   ENDFOR

   allyears_autocorr(m,my_nyears:max_nyears-1,*)=!Values.F_NaN
   allyears_decorr(m,my_nyears:max_nyears-1,*)=!Values.F_NaN

ENDFOR

psfile='/home/ss901165/idl/hadgem3-kpp_runs/nao/hadgem3kpp_nao_autocorr_daily.metum_goml1_n96_eraeof.ps'
PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=140,MARGIN=2000,XSIZE=16000,YSIZE=17000,XOFFSET=1500,YOFFSET=500
GSET,XMIN=0,XMAX=MAX(lags),YMIN=-0.2,YMAX=1
FOR m=0,nmodels-1 DO $
   GPLOT,X=findgen(nlags),Y=REFORM(autocorr(m,*)),SYM=all_symbols(m),COL=FSC_COLOR(all_colors(m)),STYLE=all_styles(m),SIZE=65
GPLOT,X=[0,MAX(lags)],Y=[0,0],STYLE=2,COL=FSC_COLOR('darkgrey')
GPLOT,X=[0,MAX(lags)],Y=[1/2.718,1/2.718],STYLE=1,COL=FSC_COLOR('darkgrey')
GPLOT,X=[0,MAX(lags)],Y=[0.198,0.198],STYLE=1,COL=FSC_COLOR('darkgrey')
GPLOT,X=MAX(lags)-4,Y=0.218,TEXT='p=0.05',ALIGN=0.0
GPLOT,X=MAX(lags)-4,Y=1/2.718+0.02,TEXT='1/e',ALIGN=0.0
GLEGEND,labels=REVERSE(all_names),COL=REVERSE(FSC_COLOR(all_colors)),LEGXOFFSET=8000,LEGYOFFSET=16000,LENGTH=0,$
        SYM=REVERSE(all_symbols),STYLE=REVERSE(all_styles)
AXES,XSTEP=5,XMINOR=1,YSTEP=0.1,YMINOR=0.05,YTITLE='Correlation',XTITLE='Lag (days)',NDECS=2
PSCLOSE

autocorr=fltarr(nmodels,nlags)
FOR m=0,nmodels-1 DO BEGIN
   CASE m OF
      0 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvi/hadgem3kpp_fwgbl_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_goml.nc'
         ntime=75*360
         offset=0
         all_names(m)='GOML1, Jan-Dec'
         all_colors(m)='purple'         
         all_symbols(m)=3
         all_styles(m)=0
         days=[0,360]
         ndays_per_year=360
      END
      1 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvi/hadgem3kpp_fwgbl_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_goml.nc'
         ntime=75*360
         offset=0
         all_names(m)='GOML1, Dec-Feb'
         all_colors(m)='purple'  
         all_symbols(m)=4
         all_styles(m)=2
         days=[330,420]
      END
      2 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvi/hadgem3kpp_fwgbl_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_goml.nc'
         ntime=75*360
         offset=0
         all_names(m)='GOML1, Jun-Aug'
         all_colors(m)='purple'  
         all_symbols(m)=6
         all_styles(m)=1
         days=[150,240]
      END
      3 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvp/hadgem3kpp_fwgbl_1.5xentrain_ga30_n96.jan-dec_dmeans_ts.years1-69.nao_index_goml.nc'
         ntime=69*360
         offset=0
         all_names(m)='GOML1 1.5x!9e!X, Jan-Dec'
         all_colors(m)='violetred'         
         all_symbols(m)=3
         all_styles(m)=0
         days=[0,360]
         ndays_per_year=360           
         a=REPLICATE(1,nlags)
      END
      4 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvp/hadgem3kpp_fwgbl_1.5xentrain_ga30_n96.jan-dec_dmeans_ts.years1-69.nao_index_goml.nc'
         ntime=69*360
         offset=0
         all_names(m)='GOML1 1.5x!9e!X, Dec-Feb'
         all_colors(m)='violetred'
         all_symbols(m)=4
         all_styles(m)=2
         days=[330,420]        
      END
      5 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvp/hadgem3kpp_fwgbl_1.5xentrain_ga30_n96.jan-dec_dmeans_ts.years1-69.nao_index_goml.nc'
         ntime=69*360
         offset=0
         all_names(m)='GOML1 1.5x!9e!X, Jun-Aug'
         all_colors(m)='purple'  
         all_symbols(m)=6
         all_styles(m)=1
         days=[150,240]      
      END
      6 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgbl31day_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_goml.nc'
         ntime=75*360
         offset=0
         all_names(m)='GA3-31d, Jan-Dec'
         all_colors(m)='red'
         all_symbols(m)=3
         all_styles(m)=0
         days=[0,360]
      END
      7 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgbl31day_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_goml.nc'
         ntime=75*360
         offset=0
         all_names(m)='GA3-31d, Dec-Feb'
         all_colors(m)='red'
         all_symbols(m)=4
         all_styles(m)=2
         days=[330,420]
      END
      8 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgbl31day_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_goml.nc'
         ntime=75*360
         offset=0
         all_names(m)='GA3-31d, Jun-Aug'
         all_colors(m)='red'
         all_symbols(m)=6
         all_styles(m)=1
         days=[150,240]
      END
      9 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgblclim_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_goml.nc'
         ntime=75*360
         offset=0
         all_names(m)='GA3-clim, Jan-Dec'
         all_colors(m)='blue'
         all_symbols(m)=3
         all_styles(m)=0
         days=[0,360]
      END
      10 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgblclim_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_goml.nc'
         ntime=75*360
         offset=0
         all_names(m)='GA3-clim, Jan-Dec'
         all_colors(m)='blue'
         all_symbols(m)=4
         all_styles(m)=2
         days=[330,420]
      END
      11 : BEGIN
         nao_file='/home/ss901165/um_output6/xihvi/hadgem3a_kppfwgblclim_1.0xentrain_ga30_n96.jan-dec_dmeans_ts.years1-75.nao_index_goml.nc'
         ntime=75*360
         offset=0
         all_names(m)='GA3-clim, Jun-Aug'
         all_colors(m)='blue'
         all_symbols(m)=6
         all_styles(m)=1
         days=[150,240]
      END
      12 : BEGIN
         nao_file='/home/ss901165/datasets/ERA-INTERIM/MSL/ERA-Interim.jan-dec_dmeans_ts.1979-2013.nao_index_goml_n96.nc'
         ntime=35*365
         offset=0
         all_names(m)='ERA-Int, Jan-Dec'
         all_colors(m)='black'
         all_symbols(m)=3
         all_styles(m)=0
         days=[0,365]
         ndays_per_year=365
      END
      13 : BEGIN
         nao_file='/home/ss901165/datasets/ERA-INTERIM/MSL/ERA-Interim.jan-dec_dmeans_ts.1979-2013.nao_index_goml_n96.nc'
         ntime=35*365
         offset=0
         all_names(m)='ERA-Int, Jan-Dec'
         all_colors(m)='black'
         all_symbols(m)=4
         all_styles(m)=2
         days=[334,424]
         ndays_per_year=365
      END
      14 : BEGIN
         nao_file='/home/ss901165/datasets/ERA-INTERIM/MSL/ERA-Interim.jan-dec_dmeans_ts.1979-2013.nao_index_goml_n96.nc'
         ntime=35*365
         offset=0
         all_names(m)='ERA-Int, Jun-Aug'
         all_colors(m)='black'
         all_symbols(m)=6
         all_styles(m)=1
         days=[152,244]
         ndays_per_year=365
      END
   ENDCASE

   nao_index=OPEN_AND_EXTRACT(nao_file,'nao_index',offset=[offset],count=[ntime])
   nyears=ntime/360
   ndpy=days(1)-days(0)

   IF days(1) le 360 THEN BEGIN
      my_nyears=nyears      
   ENDIF ELSE $
      my_nyears=nyears-1
   
   nao_index_subset=fltarr(ndpy)
   nao_index_allyears=fltarr(ndpy*my_nyears)
   FOR i=0,my_nyears-1 DO BEGIN
      nao_index_subset(*)=nao_index(i*ndays_per_year+days(0):i*ndays_per_year+days(1)-1)
      nao_index_allyears(i*ndpy:(i+1)*ndpy-1)=nao_index_subset
      allyears_autocorr(m,i,*)=A_CORRELATE(nao_index_subset,lags)
      allyears_meannao(m,i)=MEAN(nao_index_subset)
      temp=lags(where(REFORM(allyears_autocorr(m,i,*)) le 1/2.718))
      allyears_decorr(m,i)=temp(0)
   ENDFOR
   FOR i=0,nlags-1 DO BEGIN
      this_ndpy=ndpy-lags(i)   
      central=fltarr(my_nyears*this_ndpy)
      lagged=fltarr(my_nyears*this_ndpy)
      FOR j=0,my_nyears-1 DO BEGIN
         central(j*this_ndpy:(j+1)*this_ndpy-1)=nao_index_allyears(j*ndpy:j*ndpy+this_ndpy-1)
         lagged(j*this_ndpy:(j+1)*this_ndpy-1)=nao_index_allyears(j*ndpy+lags(i):(j+1)*ndpy-1)
         allyears_autocorr(m,j,i)=CORRELATE(central(j*this_ndpy:(j+1)*this_ndpy-1),lagged(j*this_ndpy:(j+1)*this_ndpy-1))
      ENDFOR
      autocorr(m,i)=CORRELATE(central,lagged)
   ENDFOR

   allyears_autocorr(m,my_nyears:max_nyears-1,*)=!Values.F_NaN
   allyears_decorr(m,my_nyears:max_nyears-1,*)=!Values.F_NaN

ENDFOR

psfile='/home/ss901165/idl/hadgem3-kpp_runs/nao/hadgem3kpp_nao_autocorr_daily.metum_goml1_n96_gomleof.ps'
PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=140,MARGIN=2000,XSIZE=16000,YSIZE=17000,XOFFSET=1500,YOFFSET=500
GSET,XMIN=0,XMAX=MAX(lags),YMIN=-0.2,YMAX=1
FOR m=0,nmodels-1 DO $
   GPLOT,X=findgen(nlags),Y=REFORM(autocorr(m,*)),SYM=all_symbols(m),COL=FSC_COLOR(all_colors(m)),STYLE=all_styles(m),SIZE=65
GPLOT,X=[0,MAX(lags)],Y=[0,0],STYLE=2,COL=FSC_COLOR('darkgrey')
GPLOT,X=[0,MAX(lags)],Y=[1/2.718,1/2.718],STYLE=1,COL=FSC_COLOR('darkgrey')
GPLOT,X=[0,MAX(lags)],Y=[0.198,0.198],STYLE=1,COL=FSC_COLOR('darkgrey')
GPLOT,X=MAX(lags)-4,Y=0.218,TEXT='p=0.05',ALIGN=0.0
GPLOT,X=MAX(lags)-4,Y=1/2.718+0.02,TEXT='1/e',ALIGN=0.0
GLEGEND,labels=REVERSE(all_names),COL=REVERSE(FSC_COLOR(all_colors)),LEGXOFFSET=8000,LEGYOFFSET=16000,LENGTH=0,$
        SYM=REVERSE(all_symbols),STYLE=REVERSE(all_styles)
AXES,XSTEP=5,XMINOR=1,YSTEP=0.1,YMINOR=0.05,YTITLE='Correlation',XTITLE='Lag (days)',NDECS=2
PSCLOSE

STOP
END
