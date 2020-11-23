PRO mjo_indices_latavg_lag_regressions_gagc

n_models=2
um6='/home/ss901165/um_output6'
um3='/home/ss901165/um_output3/hadgem3_monwg'

box=[-10,40,10,180]
base_points=[70,100,130]
n_base_points=N_ELEMENTS(base_points)
lags=indgen(51)-25
n_lags=N_ELEMENTS(lags)
mylevs=['-0.75','-0.65','-0.55','-0.45','-0.35','-0.25','-0.15','-0.05',$
        '0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75']

FOR i=0,n_models-1 DO BEGIN
   print,i
   CASE i OF
      0 : BEGIN
         
      2: BEGIN
         infile=um3+'/aliur/hadgem3a_amip_n96_ga40_aliur.jan-dec_dmeans_anom-3harm_filter2080.1982-2008.precip.2.5x2.5.nc'
         varname='precip'
         model_name='ga4-n96'
         n_years=27
         n_days_per_year=360
         a=0
         view=0
      END
      1: BEGIN
         infile=um3+'/akkvi/hadgem3a_captivate_final_n96_amip2_akkvi.jan-dec_dmeans_anom-3harm_filter2080.1979-2005.precip.2.5x2.5.nc'
         varname='precip'
         model_name='ga3-n96'
         n_years=27
         n_days_per_year=360
         a=0
         view=0
      END
      0: BEGIN
         infile='/home/ss901165/um_output6/dlhbi/metum_ga60_6aconv_dlhbi.jan-dec_dmeans_anom-3harm_filter2080.1989-2007.precip.2.5x2.5.nc'
         varname='precip'
         model_name='ga6-n96-6aconv'
         n_years=19
         n_days_per_year=360
         a=0
         view=1
      END      
      10 : BEGIN
         infile='/home/ss901165/datasets/MJO_INDICES/noaa_eraint_trmm_obs.jan-dec_dmeans_anom-3harm_filter2080.1999-2008.precip.2.5x2.5.nc'
         varname='precip'
         model_name='trmm'
         n_years=10
         n_days_per_year=365
         a=0
         view=0
      END
      3: BEGIN
         infile=um3+'/aljyr/hadgem3ao_orca1_n96_ga40_aljyr.jan-dec_dmeans_anom-3harm_filter2080.1982-2008.precip.2.5x2.5.nc'
         varname='precip'
         model_name='ga4-n96-orca1'
         n_years=27
         n_days_per_year=360
         a=0
         view=0
      END
      4: BEGIN
         infile=um6+'/gc2/antia/hadgem3_ga6_n96.jan-dec_dmeans_anom-3harm_filter2080.years1-27.precip.2.5x2.5.nc'
         varname='precip'
         model_name='ga6-n96'
         n_years=27
         n_days_per_year=360
         a=0
         view=0
      END      
      5: BEGIN
         infile=um6+'/gc2/antib/hadgem3_ga6_n216.jan-dec_dmeans_anom-3harm_filter2080.years1-27.precip.2.5x2.5.nc'
         varname='precip'
         model_name='ga6-n216'
         n_years=27
         n_days_per_year=360
         a=0
         view=0
      END      
      6: BEGIN
         infile=um6+'/gc2/anqjm/hadgem3_gc2_n96_orca025.jan-dec_dmeans_anom-3harm_filter2080.years1-41.precip.2.5x2.5.nc'
         varname='precip'
         model_name='gc2-n96'
         n_years=41
         n_days_per_year=360
         a=0
         view=0
      END      
      7: BEGIN         
         infile=um6+'/gc2/anqjn/hadgem3_gc2_n216.jan-dec_dmeans_anom-3harm_filter2080.years1-29.precip.2.5x2.5.nc'
         varname='precip'
         model_name='gc2-n216'
         n_years=29
         n_days_per_year=360
         a=0
         view=0
      END      
   ENDCASE
   longitude=OPEN_AND_EXTRACT(infile,'longitude')
   latitude=OPEN_AND_EXTRACT(infile,'latitude')
   DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
   n_lon=N_ELEMENTS(longitude)
   n_lat=N_ELEMENTS(latitude)
   
   field=OPEN_AND_EXTRACT(infile,varname,$
                          offset=[box_tx(1),box_tx(0),0,0],count=[n_lon,n_lat,n_years,n_days_per_year])
   IF TOTAL(where(field eq 2e20)) ge 0 THEN $
      field(where(field eq 2e20))=0;!Values.F_NaN
   n_time=n_days_per_year*n_years
   latavg_field=fltarr(n_lon,n_days_per_year*n_years)
   FOR j=0,n_lon-1 DO $
      FOR k=0,n_years-1 DO $
         FOR m=0,n_days_per_year-1 DO $
            latavg_field(j,n_days_per_year*k+m)=MEAN(field(j,*,k,m))
   
   IF i eq 0 THEN BEGIN
      lag_regressions=fltarr(n_models,n_base_points,n_lon,n_lags)
      lag_correlations=fltarr(n_models,n_base_points,n_lon,n_lags)
   ENDIF
   FOR j=0,n_base_points-1 DO BEGIN
      pt=NEAREST(longitude,base_points(j))
      base_ts=REFORM(latavg_field(pt,*))
      FOR k=0,n_lon-1 DO BEGIN
         this_ts=REFORM(latavg_field(k,*))
         FOR m=0,n_lags-1 DO BEGIN
            IF lags(m) lt 0 THEN BEGIN
               lag_regressions(i,j,k,m)=REGRESS(base_ts(ABS(lags(m))+50:n_time-51),this_ts(50:n_time-51-ABS(lags(m))))
               lag_correlations(i,j,k,m)=CORRELATE(base_ts(ABS(lags(m))+50:n_time-51),this_ts(50:n_time-51-ABS(lags(m))))
            ENDIF ELSE BEGIN
               lag_regressions(i,j,k,m)=REGRESS(base_ts(50:n_time-lags(m)-51),this_ts(lags(m)+50:n_time-51))
               lag_correlations(i,j,k,m)=CORRELATE(base_ts(50:n_time-lags(m)-51),this_ts(lags(m)+50:n_time-51))
            ENDELSE
         ENDFOR
      ENDFOR
      psfile='/home/ss901165/idl/mjo_indices/plots/mjo_indices_latavg_lag_regressions.'+model_name+'.'+varname+'.base'+$
             STRTRIM(STRING(FLOOR(base_points(j))),1)+'E'+'.ps'
      PSOPEN,file=psfile,TFONT=2,CHARSIZE=200,FONT=6,YOFFSET=1000,SPACE2=1000,XOFFSET=1000
      GSET,XMIN=box(1),XMAX=box(3),YMIN=MIN(lags),YMAX=MAX(lags)+0.1
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[10]
      LEVS,MANUAL=mylevs
      IF i gt 0 and a ne 0 THEN $
         lag_regressions(i,j,*,*)=lag_regressions(i,j,*,*)
      CON,X=longitude,Y=lags,FIELD=REFORM(lag_regressions(i,j,*,*)),CB_TITLE='Regression coefficient (unitless)',$
          /NOLINES,CB_WIDTH=115,/NOCOLBAR
      xend=box(3)
      yend=(180-base_points(j))/4.3
      IF yend ge MAX(lags) THEN BEGIN
         xend=MAX(lags)*4.3+base_points(j)
         yend=MAX(lags)
      ENDIF  
      xbeg=box(1)
      ybeg=-(base_points(j)-40)/4.3
      IF ybeg lt MIN(lags) THEN BEGIN
         xbeg=base_points-MIN(lags)*4.3
         ybeg=MIN(lags)
      ENDIF        
      GPLOT,X=[base_points(j),xend],Y=[0,yend],STYLE=2
      GPLOT,X=[xbeg,base_points(j)],Y=[ybeg,0],STYLE=2
      AXES,XSTEP=20,YSTEP=5,YTITLE='<--- Lead     Lag (days)   Lag --->',$
           XTITLE='Longitude (degrees east)'
      IF view eq 1 THEN BEGIN
         PSCLOSE
      ENDIF ELSE $
         PSCLOSE,/NOVIEW
   ENDFOR
ENDFOR

STOP
END
