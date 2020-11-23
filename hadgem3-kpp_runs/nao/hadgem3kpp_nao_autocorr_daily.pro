PRO hadgem3kpp_nao_autocorr_daily

nmodels=12
all_names=strarr(nmodels)
all_colors=strarr(nmodels)
all_symbols=intarr(nmodels)
all_styles=intarr(nmodels)
lags=indgen(31)
nlags=N_ELEMENTS(lags)
autocorr=fltarr(nmodels,nlags)
autocorr_ssw=fltarr(nmodels,nlags)
autocorr_nossw=fltarr(nmodels,nlags)
autocorr_noiav=fltarr(nmodels,nlags)
autocorr_sarah=fltarr(nmodels,nlags)
autocorr_noiav_sarah=fltarr(nmodels,nlags)
autocorr_thirds=fltarr(nmodels,nlags,3)
autocorr_noiav_thirds=fltarr(nmodels,nlags,3)
test_autocorr=fltarr(nmodels,nlags)

max_nyears=150
allyears_autocorr=fltarr(nmodels,max_nyears,nlags)
allyears_autocorr_sarah=fltarr(nmodels,max_nyears,nlags)
allyears_autocorr_noiav=fltarr(nmodels,max_nyears,nlags)
allyears_autocorr_noiav_sarah=fltarr(nmodels,max_nyears,nlags)
allyears_meannao=fltarr(nmodels,max_nyears)
allyears_decorr=fltarr(nmodels,max_nyears)
allyears_decorr_noiav=fltarr(nmodels,max_nyears)
allyears_autocorr_season=fltarr(nmodels,max_nyears,nlags)

all_mynyears=intarr(nmodels)
efold_thirds=fltarr(nmodels,4)
plev_thirds=fltarr(nmodels,4)

decorr_bins=[3,6,9,12,15,18,21,24,27,30]
;decorr_bins=[2,4,6,8,10,12,14,16,18,20,22,24,26,28,30]
ndecorr_bins=N_ELEMENTS(decorr_bins)
decorr_hist=fltarr(nmodels,ndecorr_bins+1)
decorr_noiav_hist=fltarr(nmodels,ndecorr_bins+1)

futweat='/group_workspaces/jasmin/futureweather'

FOR m=0,nmodels-1 DO BEGIN
   CASE m OF
      0 : BEGIN
         nao_file=futweat+'/xjhwb/metum-goml1_fwn216.jan-dec_dmeans_ts.years1-100.nao_index_goml.nc'
         ntime=LONG(100)*LONG(360)
         offset=0
         all_names(m)='GOML1, Jan-Dec'
         all_colors(m)='purple'         
         all_symbols(m)=3
         all_styles(m)=0
         days=[0,360]
         ndays_per_year=360   
         a=REPLICATE(1,nlags)
         ssw_years=[3,6,8,10,11,12,13,15,18,19,20,22,24,25,28,31,34,38,41,43,44,45,46,50,51,54,57,58,59,60,63,$
                    64,67,68,69,70,72,73,74,78,79,80,82,83,84,86,87,90,92,98]
      END
      1 : BEGIN
         nao_file=futweat+'/xjhwb/metum-goml1_fwn216.jan-dec_dmeans_ts.years1-100.nao_index_goml.nc'
         ntime=LONG(100)*LONG(360)
         offset=0
         all_names(m)='GOML1, Dec-Feb'
         all_colors(m)='purple'  
         all_symbols(m)=4
         all_styles(m)=2
         days=[330,450]        
         a=1+findgen(nlags)*0.01
         ;ssw_years=[3,6,8,10,11,12,13,15,18,19,20,22,24,25,28,31,34,38,41,43,44,45,46,50,51,54,57,58,59,60,63,$
         ;           64,67,68,69,70,72,73,74,78,79,80,82,83,84,86,87,90,92,98]
      END
      2 : BEGIN
         nao_file=futweat+'/xjhwb/metum-goml1_fwn216.jan-dec_dmeans_ts.years1-100.nao_index_goml.nc'
         ntime=LONG(100)*LONG(360)
         offset=0
         all_names(m)='GOML1, Jun-Aug'
         all_colors(m)='purple'  
         all_symbols(m)=6
         all_styles(m)=1
         days=[150,240]      
      END  
      3 : BEGIN
         nao_file=futweat+'/xjhwe/metum-ga3_fwn216-31day.jan-dec_dmeans_ts.years1-100.nao_index_ga3-31d.nc'
         ntime=LONG(100)*LONG(360)
         offset=0
         all_names(m)='GA3-31d, Jan-Dec'
         all_colors(m)='red'
         all_symbols(m)=3
         all_styles(m)=0
         days=[0,360] 
         ssw_years=[1,3,9,10,11,14,20,21,23,24,26,27,28,29,32,34,35,36,41,47,48,51,55,59,60,61,64,65,67,69,$
                    71,72,74,77,79,80,81,84,88,89,90,91,94,96]
      END
      4 : BEGIN
         nao_file=futweat+'/xjhwe/metum-ga3_fwn216-31day.jan-dec_dmeans_ts.years1-100.nao_index_ga3-31d.nc'
         ntime=LONG(100)*LONG(360)
         offset=0
         all_names(m)='GA3-31d, Dec-Feb'
         all_colors(m)='red'
         all_symbols(m)=4
         all_styles(m)=2
         days=[330,450]  
         a=1+findgen(nlags)*0.02
      END
      5 : BEGIN
         nao_file=futweat+'/xjhwe/metum-ga3_fwn216-31day.jan-dec_dmeans_ts.years1-100.nao_index_ga3-31d.nc'
         ntime=LONG(100)*LONG(360)
         offset=0
         all_names(m)='GA3-31d, Jun-Aug'
         all_colors(m)='red'
         all_symbols(m)=6
         all_styles(m)=1
         days=[150,240]
         a=1-findgen(nlags)*0.025
      END
      6 : BEGIN
         nao_file=futweat+'/xjhwh/metum-ga3_fwn216-clim.jan-dec_dmeans_ts.years1-100.nao_index_ga3-clim.nc'
         ntime=LONG(100)*LONG(360)
         offset=0
         all_names(m)='GA3-clim, Jan-Dec'
         all_colors(m)='blue'
         all_symbols(m)=3
         all_styles(m)=0
         days=[0,360]   
         a=REPLICATE(1,nlags)
         ssw_years=[1,2,3,6,8,13,18,19,20,21,22,24,25,27,29,31,34,37,41,44,45,46,47,48,53,57,58,60,61,$
                    62,64,66,70,72,76,77,79,82,89,91,92,93,94,97,98]
      END
      7 : BEGIN
         nao_file=futweat+'/xjhwh/metum-ga3_fwn216-clim.jan-dec_dmeans_ts.years1-100.nao_index_ga3-clim.nc'
         ntime=LONG(100)*LONG(360)
         offset=0
         all_names(m)='GA3-clim, Dec-Feb'
         all_colors(m)='blue'
         all_symbols(m)=4
         all_styles(m)=2
         days=[330,450]
      END
      8 : BEGIN
         nao_file=futweat+'/xjhwh/metum-ga3_fwn216-clim.jan-dec_dmeans_ts.years1-100.nao_index_ga3-clim.nc'
         ntime=LONG(100)*LONG(360)
         offset=0
         all_names(m)='GA3-clim, Jun-Aug'
         all_colors(m)='blue'
         all_symbols(m)=6
         all_styles(m)=1
         days=[150,240]
         ndays_per_year=360         
      END
      9 : BEGIN
         nao_file='/home/users/npklingaman/datasets/ERA-INTERIM/MSL/ERA-Interim.jan-dec_dmeans_ts.1979-2013.nao_index_n216.nc'
         ntime=35*365
         offset=0
         all_names(m)='ERA-Int, Jan-Dec'
         all_colors(m)='black'
         all_symbols(m)=3
         all_styles(m)=0
         days=[0,365]
         ndays_per_year=365
         ssw_years=[79,80,81,84,85,87,88,89,98,99,100,101,102,103,104,106,107,108,109,110]-79
      END
      10 : BEGIN
         nao_file='/home/users/npklingaman/datasets/ERA-INTERIM/MSL/ERA-Interim.jan-dec_dmeans_ts.1979-2013.nao_index_n216.nc'
         ntime=35*365
         offset=0
         all_names(m)='ERA-Int, Dec-Feb'
         all_colors(m)='black'
         all_symbols(m)=4
         all_styles(m)=2
         days=[334,424]
         ndays_per_year=365
      END
      11 : BEGIN
         nao_file='/home/users/npklingaman/datasets/ERA-INTERIM/MSL/ERA-Interim.jan-dec_dmeans_ts.1979-2013.nao_index_n216.nc'
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

   nssw_years=N_ELEMENTS(ssw_years)
   
   nao_index=OPEN_AND_EXTRACT(nao_file,'nao_index',offset=[offset],count=[ntime])
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
      allyears_autocorr(m,i,*)=A_CORRELATE(nao_index_subset,lags)
      allyears_meannao(m,i)=MEAN(nao_index_subset)     
   ENDFOR

   FOR i=0,nlags-1 DO BEGIN
      this_ndpy=ndpy-MAX(lags)
      central=fltarr(my_nyears*this_ndpy)
      central_noiav=fltarr(my_nyears*this_ndpy)
      central_ssw=fltarr(nssw_years*this_ndpy)
      central_nossw=fltarr((my_nyears-nssw_years)*this_ndpy)
      lagged=fltarr(my_nyears*this_ndpy)
      lagged_noiav=fltarr(my_nyears*this_ndpy)
      lagged_ssw=fltarr(nssw_years*this_ndpy)
      lagged_nossw=fltarr((my_nyears-nssw_years)*this_ndpy)
      ssw_count=0
      nossw_count=0
      FOR j=0,my_nyears-1 DO BEGIN
         start_offset=LONG(j)*LONG(this_ndpy)
         stop_offset=LONG(j+1)*LONG(this_ndpy)-LONG(1)
         central(start_offset:stop_offset)=nao_index_allyears(LONG(j)*LONG(ndpy):LONG(j)*LONG(ndpy)+LONG(this_ndpy)-1)
         central_noiav(start_offset:stop_offset)=nao_index_allyears(LONG(j)*LONG(ndpy):LONG(j)*LONG(ndpy)+LONG(this_ndpy)-1)-allyears_meannao(m,j)
         
         start_offset=LONG(j)*LONG(this_ndpy)
         stop_offset=LONG(j+1)*LONG(this_ndpy)-1
         lagged(start_offset:stop_offset)=$
            nao_index_allyears(LONG(j)*LONG(ndpy)+lags(i):LONG(j+1)*LONG(ndpy)-(MAX(lags)-lags(i))-1)
         lagged_noiav(start_offset:stop_offset)=$
            nao_index_allyears(LONG(j)*LONG(ndpy)+lags(i):LONG(j+1)*LONG(ndpy)-(MAX(lags)-lags(i))-1)-allyears_meannao(m,j)

         IF where(ssw_years eq j) ge 0 THEN BEGIN
            start_offset=LONG(ssw_count)*LONG(this_ndpy)
            stop_offset=LONG(ssw_count+1)*LONG(this_ndpy)-LONG(1)
            central_ssw(start_offset:stop_offset)=nao_index_allyears(LONG(j)*LONG(ndpy):LONG(j)*LONG(ndpy)+LONG(this_ndpy)-1)-allyears_meannao(m,j)
            lagged_ssw(start_offset:stop_offset)=nao_index_allyears(LONG(j)*LONG(ndpy)+lags(i):LONG(j+1)*LONG(ndpy)-(MAX(lags)-lags(i))-1)-allyears_meannao(m,j)
            ssw_count=ssw_count+1
         ENDIF ELSE BEGIN
            start_offset=LONG(nossw_count)*LONG(this_ndpy)
            stop_offset=LONG(nossw_count+1)*LONG(this_ndpy)-LONG(1)
            central_nossw(start_offset:stop_offset)=nao_index_allyears(LONG(j)*LONG(ndpy):LONG(j)*LONG(ndpy)+LONG(this_ndpy)-1)-allyears_meannao(m,j)
            lagged_nossw(start_offset:stop_offset)=nao_index_allyears(LONG(j)*LONG(ndpy)+lags(i):LONG(j+1)*LONG(ndpy)-(MAX(lags)-lags(i))-1)-allyears_meannao(m,j)
            nossw_count=nossw_count+1
         ENDELSE

      ENDFOR
      allyears_var=VARIANCE(central)
      allyears_mean=MEAN(central)
      FOR j=0,my_nyears-1 DO BEGIN
         start_offset=LONG(j)*LONG(this_ndpy)
         stop_offset=LONG(j+1)*LONG(this_ndpy)-1

         this_central=central(start_offset:stop_offset)-allyears_mean
         this_lagged=lagged(start_offset:stop_offset)-allyears_mean
         allyears_autocorr(m,j,i)=TOTAL(this_central*this_lagged)/(allyears_var)/FLOAT(this_ndpy)*a(i)
         allyears_autocorr_sarah(m,j,i)=TOTAL(this_central*this_lagged)/(TOTAL(this_central^2))*a(i)

         this_central=central_noiav(start_offset:stop_offset)-allyears_mean
         this_lagged=lagged_noiav(start_offset:stop_offset)-allyears_mean
         allyears_autocorr_noiav(m,j,i)=TOTAL(this_central*this_lagged)/(allyears_var)/FLOAT(this_ndpy)*a(i)
         allyears_autocorr_noiav_sarah(m,j,i)=TOTAL(this_central*this_lagged)/TOTAL(this_central^2)*a(i)
      ENDFOR
      autocorr(m,i)=CORRELATE(central-allyears_mean,lagged-allyears_mean)*a(i)
      autocorr_noiav(m,i)=CORRELATE(central_noiav-allyears_mean,lagged_noiav-allyears_mean)*a(i)
      autocorr_ssw(m,i)=CORRELATE(central_ssw-MEAN(central_ssw),lagged_ssw-MEAN(lagged_ssw))*a(i)
      autocorr_nossw(m,i)=CORRELATE(central_nossw-MEAN(central_nossw),lagged_nossw-MEAN(lagged_ssw))*a(i)
      autocorr_sarah(m,i)=TOTAL(allyears_autocorr_sarah(m,*,i))/FLOAT(my_nyears)
      autocorr_noiav_sarah(m,i)=TOTAL(allyears_autocorr_noiav_sarah(m,*,i))/FLOAT(my_nyears)

      FOR j=0,2 DO BEGIN
         this_central=central(j*my_nyears/3*this_ndpy:((j+1)*my_nyears/3-1)*this_ndpy-1)
         this_lagged=lagged(j*my_nyears/3*this_ndpy:((j+1)*my_nyears/3-1)*this_ndpy-1)
         autocorr_thirds(m,i,j)=CORRELATE(this_central-allyears_mean,this_lagged-allyears_mean)
         
         this_central=central_noiav(j*my_nyears/3*this_ndpy:((j+1)*my_nyears/3-1)*this_ndpy-1)
         this_lagged=lagged_noiav(j*my_nyears/3*this_ndpy:((j+1)*my_nyears/3-1)*this_ndpy-1)
         autocorr_noiav_thirds(m,i,j)=CORRELATE(this_central-allyears_mean,this_lagged-allyears_mean)         
      ENDFOR
   ENDFOR
   FOR j=0,my_nyears-1 DO BEGIN
      allyears_autocorr(m,j,*)=allyears_autocorr(m,j,*)/allyears_autocorr(m,j,0)
      allyears_autocorr_noiav(m,j,*)=allyears_autocorr_noiav(m,j,*)/allyears_autocorr_noiav(m,j,0)
   ENDFOR
   
   FOR j=0,my_nyears-1 DO BEGIN
      IF TOTAL(where(REFORM(allyears_autocorr(m,j,*)) le 1/2.718)) ge 0 THEN BEGIN
         temp=lags(where(REFORM(allyears_autocorr(m,j,*)) le 1/2.718))
         allyears_decorr(m,j)=temp(0)
      ENDIF ELSE $
         allyears_decorr(m,j)=30
      
      IF TOTAL(where(REFORM(allyears_autocorr_noiav(m,j,*)) le 1/2.718)) ge 0 THEN BEGIN
         temp=lags(where(REFORM(allyears_autocorr_noiav(m,j,*)) le 1/2.718))
         allyears_decorr_noiav(m,j)=temp(0)
      ENDIF ELSE $
         allyears_decorr_noiav(m,j)=30
   ENDFOR
   
   allyears_autocorr(m,my_nyears:max_nyears-1,*)=!Values.F_NaN
   allyears_autocorr_noiav(m,my_nyears:max_nyears-1,*)=!Values.F_NaN
   allyears_decorr(m,my_nyears:max_nyears-1,*)=!Values.F_NaN
   allyears_decorr_noiav(m,my_nyears:max_nyears-1,*)=!Values.F_NaN

   FOR j=0,ndecorr_bins-2 DO BEGIN
      IF TOTAL(where(allyears_decorr(m,*) ge decorr_bins(j) and allyears_decorr(m,*) lt decorr_bins(j+1))) ge 0 THEN BEGIN
         decorr_hist(m,j+1)=N_ELEMENTS(where(allyears_decorr(m,*) ge decorr_bins(j) and allyears_decorr(m,*) lt decorr_bins(j+1)))
      ENDIF ELSE $
         decorr_hist(m,j+1)=0

      IF TOTAL(where(allyears_decorr_noiav(m,*) ge decorr_bins(j) and allyears_decorr_noiav(m,*) lt decorr_bins(j+1))) ge 0 THEN BEGIN
         decorr_noiav_hist(m,j+1)=N_ELEMENTS(where(allyears_decorr_noiav(m,*) ge decorr_bins(j) and allyears_decorr_noiav(m,*) lt decorr_bins(j+1)))
      ENDIF ELSE $
         decorr_noiav_hist(m,j+1)=0
      
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

   IF TOTAL(where(allyears_decorr_noiav(m,*) lt decorr_bins(0))) ge 0 THEN BEGIN
      decorr_noiav_hist(m,0)=N_ELEMENTS(where(allyears_decorr_noiav(m,*) lt decorr_bins(0)))
   ENDIF ELSE $
      decorr_noiav_hist(m,0)=0
   IF TOTAL(where(allyears_decorr_noiav(m,*) ge decorr_bins(ndecorr_bins-1))) THEN BEGIN
      decorr_noiav_hist(m,ndecorr_bins)=N_ELEMENTS(where(allyears_decorr_noiav(m,*) ge decorr_bins(ndecorr_bins-1)))
   ENDIF ELSE $
      decorr_noiav_hist(m,ndecorr_bins)=0
   decorr_noiav_hist(m,*)=decorr_noiav_hist(m,*)/FLOAT(my_nyears)
   
ENDFOR

; All seasons together
psfile='/home/users/npklingaman/plots/hadgem3-kpp_runs/nao/hadgem3kpp_nao_autocorr_daily.metum_goml1_n216_owneof.ps'
PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=140,MARGIN=2000,XSIZE=16000,YSIZE=16000,XOFFSET=1500,YOFFSET=500,SPACE2=100,SPACE3=200
GSET,XMIN=0,XMAX=MAX(lags),YMIN=-0.1,YMAX=1;,TITLE='Auto-correlation of daily NAO indices'
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
PSCLOSE,/NOVIEW

; Just Jan-Dec
psfile='/home/users/npklingaman/plots/hadgem3-kpp_runs/nao/hadgem3kpp_nao_autocorr_daily.metum_goml1_n216_owneof_all.ps'
PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=140,MARGIN=2000,XSIZE=16000,YSIZE=16000,XOFFSET=1500,YOFFSET=500,SPACE2=100,SPACE3=200
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
GLEGEND,labels=REVERSE(all_names(0:nmodels-1:3)),COL=REVERSE(FSC_COLOR(all_colors(0:nmodels-1:3))),LEGXOFFSET=0,LEGYOFFSET=16000,LENGTH=0,$
        SYM=REPLICATE(3,nmodels/3),STYLE=REPLICATE(0,nmodels/3)
GLEGEND,labels=['5% significance','e-folding timescale'],COL=[FSC_COLOR('black'),FSC_COLOR('black')],LENGTH=0,SYM=[4,6],$
        LEGXOFFSET=0,LEGYOFFSET=12000
AXES,XSTEP=5,XMINOR=1,YSTEP=0.1,YMINOR=0.05,YTITLE='Correlation',XTITLE='Lag (days)',NDECS=2
GSET,XMIN=0,XMAX=MAX(lags),YMIN=MIN(lags),YMAX=MAX(lags)
FOR m=0,nmodels-1,3 DO BEGIN
   GPLOT,X=MAX(lags)+1+m/4.,Y=efold_thirds(m,3),SYM=6,COL=FSC_COLOR(all_colors(m))
   IF m ne 9 THEN BEGIN
      GPLOT,X=[MAX(lags)+1+m/4.,MAX(lags)+1+m/4.],Y=[efold_thirds(m,3),MAX(efold_thirds(m,0:2))],COL=FSC_COLOR(all_colors(m))
      GPLOT,X=[MAX(lags)+1+m/4.,MAX(lags)+1+m/4.],Y=[efold_thirds(m,3),MIN(efold_thirds(m,0:2))],COL=FSC_COLOR(all_colors(m))
   ENDIF 
   
   GPLOT,X=MAX(lags)+7+m/4.,Y=plev_thirds(m,3),SYM=4,COL=FSC_COLOR(all_colors(m))
   IF m ne 9 THEN BEGIN
      GPLOT,X=[MAX(lags)+7+m/4.,MAX(lags)+7+m/4.],Y=[plev_thirds(m,3),MAX(plev_thirds(m,0:2))],COL=FSC_COLOR(all_colors(m))
      GPLOT,X=[MAX(lags)+7+m/4.,MAX(lags)+7+m/4.],Y=[plev_thirds(m,3),MIN(plev_thirds(m,0:2))],COL=FSC_COLOR(all_colors(m))
   ENDIF
ENDFOR
AXES,YSTEP=5,YMINOR=1,YTITLE='Lag (days)',XTITLE='E-folding timescale',/ONLYRIGHT,OFFSET=6000
PSCLOSE,/NOVIEW

; Just DJF
psfile='/home/users/npklingaman/plots/hadgem3-kpp_runs/nao/hadgem3kpp_nao_autocorr_daily.metum_goml1_n216_owneof_djf.ps'
PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=140,MARGIN=2000,XSIZE=16000,YSIZE=16000,XOFFSET=1500,YOFFSET=500,SPACE2=100,SPACE3=200
GSET,XMIN=0,XMAX=MAX(lags),YMIN=-0.2,YMAX=1;,TITLE='Auto-correlation of DJF daily NAO indices'
FOR m=1,nmodels-1,3 DO BEGIN
   GPLOT,X=findgen(nlags),Y=SMOOTH(REFORM(autocorr(m,*)),7),SYM=3,COL=FSC_COLOR(all_colors(m)),STYLE=0,SIZE=70
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
GLEGEND,labels=REVERSE(all_names(1:nmodels-1:3)),COL=REVERSE(FSC_COLOR(all_colors(1:nmodels-1:3))),LEGXOFFSET=0,LEGYOFFSET=16000,LENGTH=0,$
        SYM=REPLICATE(3,nmodels/3),STYLE=REPLICATE(0,nmodels/3)
GLEGEND,labels=['5% significance','e-folding timescale'],COL=[FSC_COLOR('black'),FSC_COLOR('black')],LENGTH=0,SYM=[4,6],$
        LEGXOFFSET=0,LEGYOFFSET=12000
AXES,XSTEP=5,XMINOR=1,YSTEP=0.1,YMINOR=0.05,YTITLE='Correlation',XTITLE='Lag (days)',NDECS=2
GSET,XMIN=0,XMAX=MAX(lags),YMIN=MIN(lags),YMAX=MAX(lags)
FOR m=1,nmodels-1,3 DO BEGIN
   GPLOT,X=MAX(lags)+1+m/4.,Y=efold_thirds(m,3),SYM=6,COL=FSC_COLOR(all_colors(m))
   IF m ne 10 THEN BEGIN
      GPLOT,X=[MAX(lags)+1+m/4.,MAX(lags)+1+m/4.],Y=[efold_thirds(m,3),MAX(efold_thirds(m,0:2))],COL=FSC_COLOR(all_colors(m))
      GPLOT,X=[MAX(lags)+1+m/4.,MAX(lags)+1+m/4.],Y=[efold_thirds(m,3),MIN(efold_thirds(m,0:2))],COL=FSC_COLOR(all_colors(m))
   ENDIF 
      
   GPLOT,X=MAX(lags)+7+m/4.,Y=plev_thirds(m,3),SYM=4,COL=FSC_COLOR(all_colors(m))
   IF m ne 10 THEN BEGIN
      GPLOT,X=[MAX(lags)+7+m/4.,MAX(lags)+7+m/4.],Y=[plev_thirds(m,3),MAX(plev_thirds(m,0:2))],COL=FSC_COLOR(all_colors(m))
      GPLOT,X=[MAX(lags)+7+m/4.,MAX(lags)+7+m/4.],Y=[plev_thirds(m,3),MIN(plev_thirds(m,0:2))],COL=FSC_COLOR(all_colors(m))
   ENDIF
ENDFOR
AXES,YSTEP=5,YMINOR=1,YTITLE='Lag (days)',XTITLE='E-folding timescale',/ONLYRIGHT,OFFSET=6000
PSCLOSE,/NOVIEW

; Just DJF without inter-annual variability
psfile='/home/users/npklingaman/plots/hadgem3-kpp_runs/nao/hadgem3kpp_nao_autocorr_daily.metum_goml1_n216_owneof_djf_noiav.ps'
PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=140,MARGIN=2000,XSIZE=16000,YSIZE=16000,XOFFSET=1500,YOFFSET=500,SPACE2=100,SPACE3=200
GSET,XMIN=0,XMAX=MAX(lags),YMIN=-0.2,YMAX=1;,TITLE='Auto-correlation of DJF daily NAO indices'
FOR m=1,nmodels-1,3 DO BEGIN
   GPLOT,X=findgen(nlags),Y=SMOOTH(REFORM(autocorr_noiav(m,*)),7),SYM=3,COL=FSC_COLOR(all_colors(m)),STYLE=0,SIZE=70
   FOR i=0,2 DO BEGIN
      temp=where(REFORM(autocorr_noiav_thirds(m,*,i)) le 1./2.718)
      efold_thirds(m,i)=temp(0)
      temp=where(REFORM(autocorr_noiav_thirds(m,*,i)) le 0.218)
      plev_thirds(m,i)=temp(0)
   ENDFOR
   temp=where(autocorr_noiav(m,*) le 1./2.718)
   efold_thirds(m,3)=temp(0)
   temp=where(autocorr_noiav(m,*) le 0.218)
   plev_thirds(m,3)=temp(0)
ENDFOR
GPLOT,X=[0,MAX(lags)],Y=[0,0],STYLE=2,COL=FSC_COLOR('darkgrey')
GPLOT,X=[0,MAX(lags)],Y=[1/2.718,1/2.718],STYLE=1,COL=FSC_COLOR('darkgrey')
GPLOT,X=[0,MAX(lags)],Y=[0.198,0.198],STYLE=1,COL=FSC_COLOR('darkgrey')
GPLOT,X=MAX(lags)-4,Y=0.218,TEXT='p=0.05',ALIGN=0.0
GPLOT,X=MAX(lags)-4,Y=1/2.718+0.02,TEXT='1/e',ALIGN=0.0
GLEGEND,labels=REVERSE(all_names(1:nmodels-1:3)),COL=REVERSE(FSC_COLOR(all_colors(1:nmodels-1:3))),LEGXOFFSET=0,LEGYOFFSET=16000,LENGTH=0,$
        SYM=REPLICATE(3,nmodels/3),STYLE=REPLICATE(0,nmodels/3)
GLEGEND,labels=['5% significance','e-folding timescale'],COL=[FSC_COLOR('black'),FSC_COLOR('black')],LENGTH=0,SYM=[4,6],$
        LEGXOFFSET=0,LEGYOFFSET=12000
AXES,XSTEP=5,XMINOR=1,YSTEP=0.1,YMINOR=0.05,YTITLE='Correlation',XTITLE='Lag (days)',NDECS=2
GSET,XMIN=0,XMAX=MAX(lags),YMIN=MIN(lags),YMAX=MAX(lags)
FOR m=1,nmodels-1,3 DO BEGIN
   GPLOT,X=MAX(lags)+1+m/4.,Y=efold_thirds(m,3),SYM=6,COL=FSC_COLOR(all_colors(m))
   IF m ne 10 THEN BEGIN
      GPLOT,X=[MAX(lags)+1+m/4.,MAX(lags)+1+m/4.],Y=[efold_thirds(m,3),MAX(efold_thirds(m,0:2))],COL=FSC_COLOR(all_colors(m))
      GPLOT,X=[MAX(lags)+1+m/4.,MAX(lags)+1+m/4.],Y=[efold_thirds(m,3),MIN(efold_thirds(m,0:2))],COL=FSC_COLOR(all_colors(m))
   ENDIF 
      
   GPLOT,X=MAX(lags)+7+m/4.,Y=plev_thirds(m,3),SYM=4,COL=FSC_COLOR(all_colors(m))
   IF m ne 10 THEN BEGIN
      GPLOT,X=[MAX(lags)+7+m/4.,MAX(lags)+7+m/4.],Y=[plev_thirds(m,3),MAX(plev_thirds(m,0:2))],COL=FSC_COLOR(all_colors(m))
      GPLOT,X=[MAX(lags)+7+m/4.,MAX(lags)+7+m/4.],Y=[plev_thirds(m,3),MIN(plev_thirds(m,0:2))],COL=FSC_COLOR(all_colors(m))
   ENDIF
ENDFOR
AXES,YSTEP=5,YMINOR=1,YTITLE='Lag (days)',XTITLE='E-folding timescale',/ONLYRIGHT,OFFSET=6000
PSCLOSE,/NOVIEW

; Just JJA
psfile='/home/users/npklingaman/plots/hadgem3-kpp_runs/nao/hadgem3kpp_nao_autocorr_daily.metum_goml1_n216_owneof_jja.ps'
PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=140,MARGIN=2000,XSIZE=16000,YSIZE=16000,XOFFSET=1500,YOFFSET=500,SPACE2=100,SPACE3=200
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
GLEGEND,labels=REVERSE(all_names(2:nmodels-1:3)),COL=REVERSE(FSC_COLOR(all_colors(2:nmodels-1:3))),LEGXOFFSET=0,LEGYOFFSET=16000,LENGTH=0,$
        SYM=REPLICATE(3,nmodels/3),STYLE=REPLICATE(0,nmodels/3)
GLEGEND,labels=['5% significance','e-folding timescale'],COL=[FSC_COLOR('black'),FSC_COLOR('black')],LENGTH=0,SYM=[4,6],$
        LEGXOFFSET=0,LEGYOFFSET=12000
AXES,XSTEP=5,XMINOR=1,YSTEP=0.1,YMINOR=0.05,YTITLE='Correlation',XTITLE='Lag (days)',NDECS=2
GSET,XMIN=0,XMAX=MAX(lags),YMIN=MIN(lags),YMAX=MAX(lags)
FOR m=2,nmodels-1,3 DO BEGIN
   GPLOT,X=MAX(lags)+1+m/4.,Y=efold_thirds(m,3),SYM=6,COL=FSC_COLOR(all_colors(m))
   GPLOT,X=[MAX(lags)+1+m/4.,MAX(lags)+1+m/4.],Y=[efold_thirds(m,3),MAX(efold_thirds(m,0:2))],COL=FSC_COLOR(all_colors(m))
   GPLOT,X=[MAX(lags)+1+m/4.,MAX(lags)+1+m/4.],Y=[efold_thirds(m,3),MIN(efold_thirds(m,0:2))],COL=FSC_COLOR(all_colors(m))

   GPLOT,X=MAX(lags)+7+m/4.,Y=plev_thirds(m,3),SYM=4,COL=FSC_COLOR(all_colors(m))
   GPLOT,X=[MAX(lags)+7+m/4.,MAX(lags)+7+m/4.],Y=[plev_thirds(m,3),MAX(plev_thirds(m,0:2))],COL=FSC_COLOR(all_colors(m))
   GPLOT,X=[MAX(lags)+7+m/4.,MAX(lags)+7+m/4.],Y=[plev_thirds(m,3),MIN(plev_thirds(m,0:2))],COL=FSC_COLOR(all_colors(m))
ENDFOR
AXES,YSTEP=5,YMINOR=1,YTITLE='Lag (days)',XTITLE='E-folding timescale',/ONLYRIGHT,OFFSET=6000
PSCLOSE,/NOVIEW

; Decorrelation length for DJF
psfile='/home/users/npklingaman/plots/hadgem3-kpp_runs/nao/hadgem3kpp_nao_autocorr_daily.metum_goml1_n216_owneof_djf_decorr.ps'
PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=140,MARGIN=2000,XSIZE=16000,YSIZE=16000,XOFFSET=1500,YOFFSET=500,SPACE2=100,SPACE3=200
GSET,XMIN=0,XMAX=ndecorr_bins+1,YMIN=0,YMAX=0.85 ;,TITLE='Auto-correlation of DJF daily NAO indices'
FOR m=1,nmodels-1,3 DO $
   HIST,X=findgen(ndecorr_bins+1)+0.22+0.16*m/3,Y=REFORM(decorr_hist(m,*)),WIDTH=50,FILLCOL=FSC_COLOR(all_colors(m))
GLEGEND,labels=REVERSE(all_names(1:nmodels-1:3)),COL=REVERSE(FSC_COLOR(all_colors(1:nmodels-1:3))),LEGXOFFSET=8000,LEGYOFFSET=16000,LENGTH=0,$
        SYM=REPLICATE(3,nmodels/3),STYLE=REPLICATE(0,nmodels/3)
AXES,XVALS=indgen(ndecorr_bins+2),XLABELS=['<'+STRTRIM(STRING(decorr_bins(0)),1),STRTRIM(STRING(decorr_bins),1),'>'+STRTRIM(STRING(decorr_bins(ndecorr_bins-1)),1)],$
     YSTEP=0.05,YMINOR=0.025,NDECS=2,XTITLE='e-folding timescale (days)',YTITLE='Probability'
PSCLOSE,/NOVIEW

; Decorrelation length for DJF without interannual variability
psfile='/home/users/npklingaman/plots/hadgem3-kpp_runs/nao/hadgem3kpp_nao_autocorr_daily.metum_goml1_n216_owneof_djf_noiav_decorr.ps'
PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=140,MARGIN=2000,XSIZE=16000,YSIZE=16000,XOFFSET=1500,YOFFSET=500,SPACE2=100,SPACE3=200
GSET,XMIN=0,XMAX=ndecorr_bins+1,YMIN=0,YMAX=0.85 ;,TITLE='Auto-correlation of DJF daily NAO indices'
FOR m=1,nmodels-1,3 DO $
   HIST,X=findgen(ndecorr_bins+1)+0.2+0.13*m/3,Y=REFORM(decorr_noiav_hist(m,*)),WIDTH=40,FILLCOL=FSC_COLOR(all_colors(m))
GLEGEND,labels=REVERSE(all_names(1:nmodels-1:3)),COL=REVERSE(FSC_COLOR(all_colors(1:nmodels-1:3))),LEGXOFFSET=8000,LEGYOFFSET=16000,LENGTH=0,$
        SYM=REPLICATE(3,nmodels/3),STYLE=REPLICATE(0,nmodels/3)
AXES,XVALS=indgen(ndecorr_bins+2),XLABELS=['<'+STRTRIM(STRING(decorr_bins(0)),1),STRTRIM(STRING(decorr_bins),1),'>'+STRTRIM(STRING(decorr_bins(ndecorr_bins-1)),1)],$
     YSTEP=0.05,YMINOR=0.025,NDECS=2,XTITLE='e-folding timescale (days)',YTITLE='Probability'
PSCLOSE,/NOVIEW

; Decorrelation length for JJA
psfile='/home/users/npklingaman/plots/hadgem3-kpp_runs/nao/hadgem3kpp_nao_autocorr_daily.metum_goml1_n216_owneof_jja_decorr.ps'
PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=140,MARGIN=2000,XSIZE=16000,YSIZE=16000,XOFFSET=1500,YOFFSET=500,SPACE2=100,SPACE3=200
GSET,XMIN=0,XMAX=ndecorr_bins+1,YMIN=0,YMAX=0.85 ;,TITLE='Auto-correlation of DJF daily NAO indices'
FOR m=2,nmodels-1,3 DO $
   HIST,X=findgen(ndecorr_bins+1)+0.22+0.16*m/3,Y=REFORM(decorr_hist(m,*)),WIDTH=50,FILLCOL=FSC_COLOR(all_colors(m))
GLEGEND,labels=REVERSE(all_names(2:nmodels-1:3)),COL=REVERSE(FSC_COLOR(all_colors(2:nmodels-1:3))),LEGXOFFSET=8000,LEGYOFFSET=16000,LENGTH=0,$
        SYM=REPLICATE(3,nmodels/3),STYLE=REPLICATE(0,nmodels/3)
AXES,XVALS=indgen(ndecorr_bins+2),XLABELS=['<'+STRTRIM(STRING(decorr_bins(0)),1),STRTRIM(STRING(decorr_bins),1),'>'+STRTRIM(STRING(decorr_bins(ndecorr_bins-1)),1)],$
     YSTEP=0.05,YMINOR=0.025,NDECS=2,XTITLE='e-folding timescale (days)',YTITLE='Probability'
PSCLOSE,/NOVIEW

STOP
END
