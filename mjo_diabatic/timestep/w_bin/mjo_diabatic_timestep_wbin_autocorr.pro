PRO mjo_diabatic_timestep_wbin_autocorr

metum='/home/ss901165/um_output6/mjodiab_2day/metum'
; Density and vertical velocity
rho_file=metum+'/MetUM.rho.20091020-20100110.lead_12-48hrs.nc'
wap_file=metum+'/MetUM.wap.20091020-20100110.lead_12-48hrs.nc'

lags=indgen(21)-10
n_lags=N_ELEMENTS(lags)
thresholds=[-0.03,-0.02,-0.01,0.01,0.02,0.03,0.04,0.06]
;thresholds=[-0.06,-0.04,-0.02,0.02,0.04,0.06,0.08,0.12]
colors=['brown','red','orange','cyan','dodgerblue','blue','steelblue','purple']
n_thresholds=N_ELEMENTS(thresholds)
lag_correlations=fltarr(n_thresholds+2,n_lags)

box=[-10,60,10,90]
; Read longitude, latitude, height
lat=OPEN_AND_EXTRACT(rho_file,'latitude')
lon=OPEN_AND_EXTRACT(rho_file,'longitude')
DEFINE_BOUNDARIES,box,lat,lon,box_tx,/LIMIT
nlon=N_ELEMENTS(lon)
nlat=N_ELEMENTS(lat)
z=OPEN_AND_EXTRACT(rho_file,'level')
nz=50
offset_time=0
nt=180*22

; Read wap and rho
rho=OPEN_AND_EXTRACT(rho_file,'rho',offset=[box_tx(1),box_tx(0),0,offset_time],$
                     count=[nlon,nlat,nz,nt])
wap=OPEN_AND_EXTRACT(wap_file,'wap',offset=[box_tx(1),box_tx(0),0,offset_time],$
                     count=[nlon,nlat,nz,nt])
p=REFORM(OPEN_AND_EXTRACT(metum+'/MetUM.p.20100110.00Z.nc','p',offset=[box_tx(1),box_tx(0),0,0],$
                          count=[nlon,nlat,nz,1]))

; Compute rho-weighted wap (0-48 for troposphere, 20-26 for 3-5km, 27-48 for > 5km)
int_bot=0
int_top=48
wap_intrho=fltarr(nlon,nlat,nt)
p_bot=MEAN(p(*,*,int_bot))/100.
p_top=MEAN(p(*,*,int_top))/100.
FOR i=0,nlon-1 DO BEGIN
   FOR j=0,nlat-1 DO BEGIN
      FOR k=0,nt-1 DO BEGIN
         rhosum=TOTAL(rho(i,j,int_bot:int_top,k))
         wap_intrho(i,j,k)=TOTAL(rho(i,j,int_bot:int_top,k)*wap(i,j,int_bot:int_top,k))/rhosum
      ENDFOR
   ENDFOR
ENDFOR

FOR i=0,n_thresholds+1 DO BEGIN
   total_ncorr=LONG(0)
   FOR j=0,nlon-1 DO BEGIN
      FOR k=0,nlat-1 DO BEGIN
         ts=REFORM(wap_intrho(j,k,*))
         CASE i OF
            n_thresholds : BEGIN
               valid=where(ts gt -0.04)               
            END
            n_thresholds+1 : BEGIN
               valid=where(ts lt 0.02 and ts gt -0.02)
            END
            ELSE : BEGIN
               this_threshold=thresholds(i)
               IF this_threshold(0) lt 0 THEN BEGIN
                  valid=where(ts lt this_threshold(0))
               ENDIF ELSE $
                  valid=where(ts gt this_threshold(0))
            END
         ENDCASE
         n_valid=N_ELEMENTS(valid)
         n_corr=0
         IF n_valid gt 1 THEN BEGIN
            corr_ts=fltarr(n_valid,n_lags)
            FOR m=0,n_valid-1 DO BEGIN
               IF valid(m) gt n_lags/2 and valid(m) lt nt-n_lags/2 THEN BEGIN
                  corr_ts(n_corr,*)=ts(valid(m)-n_lags/2:valid(m)+n_lags/2)
                  n_corr=n_corr+1
               ENDIF
            ENDFOR
            IF n_corr ge 2 THEN BEGIN
               FOR m=0,n_lags-1 DO $
                  lag_correlations(i,m)=CORRELATE(corr_ts(0:n_corr-1,n_lags/2),corr_ts(0:n_corr-1,m))*n_corr+lag_correlations(i,m)
               ;total_ncorr=LONG(total_ncorr)+LONG(n_corr)
            ENDIF
            IF TOTAL(where(FINITE(lag_correlations(i,*)) eq 0)) ge 0 THEN $
               STOP
         ENDIF
      ENDFOR
   ENDFOR      
   print,i,lag_correlations(i,n_lags/2)
   lag_correlations(i,*)=lag_correlations(i,*)/FLOAT(lag_correlations(i,n_lags/2))
ENDFOR

lag_correlations(*,n_lags/2)=!Values.F_NaN
psfile='/home/ss901165/idl/mjo_diabatic/timestep/w_bin/mjo_diabatic_timestep_wbin_autocorr.metum_ga3_intlev'+STRTRIM(STRING(int_bot),1)+'-'+STRTRIM(STRING(int_top),1)+'.ps'
PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=150,TCHARSIZE=100,SPACE2=500,YOFFSET=1500,XOFFSET=2000,SPACE1=400,MARGIN=2500,XSIZE=18000
GSET,XMIN=MIN(lags),XMAX=MAX(lags),YMIN=-0.55,YMAX=0.65
labels=strarr(n_thresholds+2)
FOR i=0,n_thresholds-1 DO BEGIN
   GPLOT,X=lags,Y=REFORM(lag_correlations(i,*)),COL=FSC_COLOR(colors(i)),SYM=3
   IF thresholds(i) lt 0 THEN BEGIN
      labels(i)='W < '+STRMID(STRTRIM(STRING(thresholds(i)),1),0,6)
   ENDIF ELSE $
      labels(i)='W > '+STRMID(STRTRIM(STRING(thresholds(i)),1),0,5)
ENDFOR
labels(n_thresholds)='All values'
labels(n_thresholds+1)='|W| < 0.01'
GPLOT,X=lags,Y=REFORM(lag_correlations(n_thresholds,*)),COL=FSC_COLOR('black'),SYM=3
GPLOT,X=lags,Y=REFORM(lag_correlations(n_thresholds+1,*)),COL=FSC_COLOR('darkgrey'),SYM=3
GPLOT,X=[0,0],Y=[-0.55,0.65],STYLE=1
GPLOT,X=[MIN(lags),MAX(lags)],Y=[0,0],STYLE=1
AXES,XSTEP=1,YSTEP=0.1,XTITLE='Lag (timesteps)',YTITLE='Correlation coefficient',NDECS=3,YMINOR=0.05
all_colors=strarr(n_thresholds+2)
all_colors(0:2)=colors(0:2)
all_colors(3)='darkgrey'
all_colors(4:n_thresholds)=colors(3:n_thresholds-1)
all_colors(n_thresholds+1)='black'
GLEGEND,labels=[labels(0:2),labels(n_thresholds+1),labels(3:n_thresholds-1),labels(n_thresholds)],$
        COL=FSC_COLOR(all_colors),SYM=[REPLICATE(3,n_thresholds),3,3],LENGTH=0,LEGXOFFSET=7000,LEGYOFFSET=16000
PSCLOSE

STOP
END
