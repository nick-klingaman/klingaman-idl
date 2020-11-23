PRO mjo_indices_ssws_fwn216_polarcapT

box=[65,0,90,360]
plev=10

n_models=3
futweat='/group_workspaces/jasmin/futureweather'

lags=indgen(51)
n_lags=N_ELEMENTS(lags)

mylevs=['-0.55','-0.45','-0.35','-0.25','-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55']
nlevs=N_ELEMENTS(mylevs)

FOR i=0,n_models-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         t_infiles=[futweat+'/xjhwb/metum-goml1_fwn216_mem1.jan-dec_dmeans_ts.years1-41.zmean_T.nc',$
                    futweat+'/xjhwb/metum-goml1_fwn216_mem2.jan-dec_dmeans_ts.years1-31.zmean_T.nc',$
                    futweat+'/xjhwb/metum-goml1_fwn216_mem3.jan-dec_dmeans_ts.years1-31.zmean_T.nc']
         t_varname='air_temperature'
         p_varname='air_pressure'
         n_years_infiles=[40,30,30]
         rmm_infile=futweat+'/xjhwb/rmm_indices.nc'
         ndays_per_year=LONG(360)
         model_name='metum-goml1_fwn216'
      END
      1 : BEGIN
         t_infiles=[futweat+'/xjhwe/metum-ga3_fwn216-31day_mem1.jan-dec_dmeans_ts.years1-41.zmean_T.nc',$
                    futweat+'/xjhwe/metum-ga3_fwn216-31day_mem2.jan-dec_dmeans_ts.years1-31.zmean_T.nc',$
                    futweat+'/xjhwe/metum-ga3_fwn216-31day_mem3.jan-dec_dmeans_ts.years1-31.zmean_T.nc']
         t_varname='air_temperature'
         p_varname='air_pressure'
         n_years_infiles=[40,30,30]
         rmm_infile=futweat+'/xjhwe/rmm_indices.nc'
         ndays_per_year=LONG(360)
         model_name='metum-ga3_fwn216-31day'
      END      
      2 : BEGIN
         t_infiles=[futweat+'/xjhwh/metum-ga3_fwn216-clim_mem1.jan-dec_dmeans_ts.years1-41.zmean_T.nc',$
                    futweat+'/xjhwh/metum-ga3_fwn216-clim_mem2.jan-dec_dmeans_ts.years1-31.zmean_T.nc',$
                    futweat+'/xjhwh/metum-ga3_fwn216-clim_mem3.jan-dec_dmeans_ts.years1-31.zmean_T.nc']
         t_varname='air_temperature'
         p_varname='air_pressure'
         n_years_infiles=[40,30,30]
         rmm_infile=futweat+'/xjhwh/rmm_indices.nc'
         ndays_per_year=LONG(360)
         model_name='metum-ga3_fwn216-clim'
      END
   ENDCASE

   n_years = LONG(TOTAL(n_years_infiles))
   p = OPEN_AND_EXTRACT(t_infiles(0),p_varname)
   plev_offset = NEAREST(p,plev)
   
   lat = OPEN_AND_EXTRACT(t_infiles(0),'latitude')
   lat_start = NEAREST(lat,box(0))
   lat_stop = NEAREST(lat,box(2))   
   nlat = lat_stop-lat_start+1
   weights = COS(lat*!Pi/180.)
   weights = weights/TOTAL(weights)

   t_dmean = fltarr(nlat,ndays_per_year*n_years)
   FOR f=0,N_ELEMENTS(t_infiles)-1 DO BEGIN
      print,f
      IF f eq 0 THEN BEGIN
         t_dmean(*,0:n_years_infiles(0)*ndays_per_year-1)=$
            OPEN_AND_EXTRACT(t_infiles(0),t_varname,offset=[lat_start,plev_offset,0],$
                             count=[nlat,1,n_years_infiles(f)*ndays_per_year])
      ENDIF ELSE $
         t_dmean(*,TOTAL(n_years_infiles(0:f-1))*ndays_per_year:TOTAL(n_years_infiles(0:f))*ndays_per_year-1)=$
         OPEN_AND_EXTRACT(t_infiles(f),t_varname,offset=[lat_start,plev_offset,0],$
                          count=[nlat,1,n_years_infiles(f)*ndays_per_year])
   ENDFOR

   t_clim = fltarr(nlat,ndays_per_year)
   FOR t=0,ndays_per_year-1 DO $
      FOR y=0,nlat-1 DO $
         t_clim(y,t)=MEAN(t_dmean(y,t:ndays_per_year*n_years-1:ndays_per_year))
   t_anom = fltarr(nlat,ndays_per_year*n_years)
   FOR t=0,n_years-1 DO $
      t_anom(*,t*ndays_per_year:(t+1)*ndays_per_year-1) = t_dmean(*,t*ndays_per_year:(t+1)*ndays_per_year-1)-t_clim
   t_anom_latavg = fltarr(ndays_per_year*n_years)
   FOR t=0,ndays_per_year*n_years-1 DO $
      t_anom_latavg(t)=TOTAL(t_anom(*,t)*weights)

   phase = OPEN_AND_EXTRACT(rmm_infile,'phase')
   amp = OPEN_AND_EXTRACT(rmm_infile,'amplitude')

   phase_ts = fltarr(ndays_per_year*n_years)
   amp_ts = fltarr(ndays_per_year*n_years)
   FOR t=0,n_years-1 DO BEGIN
      phase_ts(t*ndays_per_year:(t+1)*ndays_per_year-1)=phase(t,*)
      amp_ts(t*ndays_per_year:(t+1)*ndays_per_year-1)=amp(t,*)
   ENDFOR

   polart_comp = fltarr(16,n_lags)
   FOR j=1,8 DO BEGIN
      this_phase = where(phase_ts ge j and amp_ts ge 1.5)
      nt=0
      FOR t=0,N_ELEMENTS(this_phase)-1 DO BEGIN
         IF this_phase(t)+MAX(lags) lt ndays_per_year*n_years and $
            (this_phase(t) MOD ndays_per_year gt 300 or this_phase(t) MOD ndays_per_year lt 90) THEN BEGIN
            polart_comp(j-1,*)=polart_comp(j-1,*)+t_anom_latavg[this_phase(t)+lags]         
            nt=nt+1
         ENDIF
      ENDFOR
      polart_comp(j-1,*)=polart_comp(j-1,*)/FLOAT(nt)
   ENDFOR
   polart_comp(8:15,*)=polart_comp(0:7,*)
   polart_comp = polart_comp - MEAN(polart_comp)

   psfile='/home/users/npklingaman/plots/mjo_indices/ssws/mjo_indices_ssws_fwn216_polarcapT.'+model_name+'_byphase.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,MARGIN=3000,SPACE3=1000,SPACE2=1000
   GSET,XMIN=0,XMAX=15,YMIN=0,YMAX=MAX(lags)
   CS,SCALE=1,NCOLS=nlevs+1,white=[8]
   LEVS,MANUAL=mylevs
   CON,X=indgen(16),Y=lags,FIELD=polart_comp,/NOLINELABELS,POSITIVE_STYLE=2,NEGATIVE_STYLE=1,$
       CB_TITLE='Anomalies in polar cap temperature (10hPa, 65-90N)'
   AXES,XVALS=indgen(16),XLABELS=['1','2','3','4','5','6','7','8','1','2','3','4','5','6','7','8'],$
        YSTEP=10,YMINOR=2,YTITLE='Days after MJO',XTITLE='MJO phase'
   PSCLOSE,/NOVIEW
   
ENDFOR

STOP
END
