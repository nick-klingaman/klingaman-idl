PRO mjo_indices_latavg_lag_regressions_spcam

n_models=6
um6='/home/ss901165/um_output6'
um3='/home/ss901165/um_output3/hadgem3_monwg'

box=[-10,40,10,180]
base_points=[70,100,130]
n_base_points=N_ELEMENTS(base_points)
lags=indgen(51)-25
n_lags=N_ELEMENTS(lags)
mylevs=['-0.95','-0.85','-0.75','-0.65','-0.55','-0.45','-0.35','-0.25','-0.15','-0.05',$
        '0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95']

FOR i=0,n_models-1 DO BEGIN
   print,i
   CASE i OF
      4 : BEGIN
         infile=um6+'/mjodiab_20year/spcam/SPCAM3_AMIP.jan-dec_dmeans_anom-3harm_filter2080.1986-2003.olr.2.5x2.5.nc'
         varname='olr'
         model_name='spcam3_amip_mjodiab'
         n_years=18
         n_days_per_year=365
         a=0
         view=1
      END
      5 : BEGIN
         infile=um6+'/mjodiab_20year/spccsm/SPCCSM.jan-dec_dmeans_anom-3harm_filter2080.4-23.olr.2.5x2.5.nc'
         varname='olr'
         model_name='spccsm_mjodiab'
         n_years=20
         n_days_per_year=365
         a=0
         view=1
      END
      3 : BEGIN
         infile=um6+'/spcam/spcam_iceedge_free/spcam_iceedge_free.jan-dec_dmeans_anom-3harm_filter2080.1-19.olr.2.5x2.5.nc'
         varname='olr'         
         model_name='spcam3-kpp'
         n_years=19
         n_days_per_year=365
         a=0
         view=0
      END
      2: BEGIN
         infile=um6+'/spcam/cam_iceedge_free/cam_iceedge_free.jan-dec_dmeans_anom-3harm_filter2080.1-100.olr.2.5x2.5.nc'
         varname='olr'
         model_name='cam3-kpp'
         n_years=50
         n_days_per_year=365
         a=0
         view=0
      END
;      1: BEGIN
;         infile=um6+'/xgspp/hadgem3kpp_1.0xentrain_ga30.jan-dec_dmeans_anom-3harm_filter2080.i2-k1.olr.2.5x2.5.nc'
;         varname='precip'
;         model_name='goml1_30N-30S_1.0xent'
;         n_years=20
;         n_days_per_year=360
;         a=0
;         view=0
;      END
      1 : BEGIN
         infile='/home/ss901165/datasets/MJO_INDICES/noaa_eraint_obs.jan-dec_dmeans_anom-3harm_filter2080.1989-2008.olr.2.5x2.5.nc'
         varname='olr'
         model_name='noaa'
         n_years=20
         ndays_per_year=365
         a=0
         view=0
      END
      0: BEGIN
         infile=um6+'/xgspm/hadgem3kpp_1.5xentrain_ga30.jan-dec_dmeans_anom-3harm_filter2080.i2-k1.olr.2.5x2.5.nc'
         varname='olr'
         model_name='goml1_30N-30S_1.5xent'
         n_years=20
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
   latavg_field=fltarr(n_lon,LONG(n_days_per_year)*LONG(n_years))
   FOR j=0,n_lon-1 DO $
      FOR k=LONG(0),LONG(n_years)-1 DO $
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
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[12]
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
