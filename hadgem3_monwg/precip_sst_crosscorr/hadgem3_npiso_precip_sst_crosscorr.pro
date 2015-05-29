PRO hadgem3_npiso_precip_sst_crosscorr

lon_range=[75,95]
lat_range=[-25,25]
box=[lat_range(0),lon_range(0),lat_range(1),lon_range(1)]

precip_indmem_file='/home/ss901165/um_output2/hadgem3_monwg/ahrqc/ahrqc.precip.daily_20years.nc'
sst_indmem_file='/home/ss901165/um_output2/hadgem3_monwg/ahrqc/ahrqc.surftemp.daily_20years.nc'

mask_file='/home/ss901165/um_output/mask_n96.nc'

latitude=OPEN_AND_EXTRACT(precip_indmem_file,'latitude')
longitude=OPEN_AND_EXTRACT(precip_indmem_file,'longitude')
DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
n_lon = N_ELEMENTS(longitude)
n_lat = N_ELEMENTS(latitude)

mask_latitude = OPEN_AND_EXTRACT(mask_file,'latitude')
mask_longitude = OPEN_AND_EXTRACT(mask_file,'longitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
n_masklon = N_ELEMENTS(mask_longitude)
n_masklat = N_ELEMENTS(mask_latitude)
mask = REFORM(OPEN_AND_EXTRACT(mask_file,'lsm',offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                               count=[n_masklon,n_masklat,1,1]))

mask_rev=fltarr(n_masklon,n_masklat)
FOR i=0,n_masklon-1 DO $
  FOR j=0,n_masklat-1 DO $
  mask_rev(i,j)=mask(i,n_masklat-j-1)

n_members=20
n_time=150
offset_time=120

extra_points=7
n_time_trend=n_time-2*extra_points

lags = indgen(51)-25
n_lags = N_ELEMENTS(lags)

precip_lonavg = fltarr(n_lat,n_time*n_members)
sst_lonavg = fltarr(n_lat,n_time*n_members)
precip_lonavg_trend = fltarr(n_lat,n_time_trend*n_members)
sst_lonavg_trend = fltarr(n_lat,n_time_trend*n_members)
covariance = fltarr(n_lags,n_lat)
covariance_trend = fltarr(n_lags,n_lat)

sig_level = 0.186

FOR i=0,n_members-1 DO BEGIN
    print,'Member '+STRTRIM(STRING(i+1),1)
    this_precip = REFORM(OPEN_AND_EXTRACT(precip_indmem_file,'precip',offset=[box_tx(1),box_tx(0),0,offset_time,i],$
                                   count=[n_lon,n_lat,1,n_time,1]))*86400.    
    this_sst = REFORM(OPEN_AND_EXTRACT(sst_indmem_file,'temp',offset=[box_tx(1),box_tx(0),0,offset_time,i],$
                                       count=[n_lon,n_lat,1,n_time,1]))
    FOR j=0,n_time-1 DO BEGIN
        today_precip = REFORM(this_precip(*,*,j))
        today_sst = REFORM(this_sst(*,*,j))
        today_precip[where(mask_rev eq 1)] = !Values.F_NaN
        today_sst[where(mask_rev eq 1)] = !Values.F_NaN
        ;today_sst[where(today_sst ge 500)] = !Values.F_NaN
        this_precip(*,*,j) = today_precip
        this_sst(*,*,j) = today_sst
    ENDFOR
    this_precip_lonavg = fltarr(n_lat,n_time)
    this_sst_lonavg = fltarr(n_lat,n_time)
    FOR j=0,n_lat-1 DO BEGIN
        FOR k=0,n_time-1 DO BEGIN
            this_precip_lonavg(j,k) = MEAN(this_precip(*,j,k),/NaN)
            this_sst_lonavg(j,k) = MEAN(this_sst(*,j,k),/NaN)            
        ENDFOR
        ;temp = LINFIT(indgen(n_time),REFORM(this_sst_lonavg(j,*)))
        ;this_sst_lonavg(j,*) = this_sst_lonavg(j,*)-temp(1)*indgen(n_time)
    ENDFOR
    

    this_precip_lonavg_trend = fltarr(n_lat,n_time_trend)
    this_sst_lonavg_trend = fltarr(n_lat,n_time_trend)
    FOR j=0,n_lat-1 DO BEGIN
        FOR k=extra_points,n_time-extra_points-1 DO BEGIN
            IF TOTAL(REFORM(this_precip_lonavg(j,k-extra_points:k+extra_points))) ne 0 THEN BEGIN
                this_precip_lonavg_trend(j,k-extra_points) = REGRESS(indgen(2*extra_points+1),$
                                                                     REFORM(this_precip_lonavg(j,k-extra_points:k+extra_points)))
            ENDIF ELSE $
              this_precip_lonavg_trend(j,k-extra_points) = 0.
            this_sst_lonavg_trend(j,k-extra_points) = REGRESS(indgen(2*extra_points+1),$
                                                              REFORM(this_sst_lonavg(j,k-extra_points:k+extra_points)))
        ENDFOR
    ENDFOR

    precip_lonavg(*,i*n_time:(i+1)*n_time-1) = this_precip_lonavg
    sst_lonavg(*,i*n_time:(i+1)*n_time-1) = this_sst_lonavg
    precip_lonavg_trend(*,i*n_time_trend:(i+1)*n_time_trend-1) = this_precip_lonavg_trend
    sst_lonavg_trend(*,i*n_time_trend:(i+1)*n_time_trend-1) = this_sst_lonavg_trend
    
    FOR j=0,n_lat-1 DO BEGIN
        covariance(*,j) = C_CORRELATE(REFORM(this_precip_lonavg(j,*)),REFORM(this_sst_lonavg(j,*)),lags,$
                                      /DOUBLE)/FLOAT(n_members) + covariance(*,j)
        covariance_trend(*,j) = C_CORRELATE(REFORM(this_precip_lonavg_trend(j,*)),REFORM(this_sst_lonavg_trend(j,*)),lags,$
                                            /DOUBLE,/COVARIANCE)/FLOAT(n_members) + covariance_trend(*,j)      
    ENDFOR
    print,max(covariance_trend),min(covariance_trend)    

ENDFOR

cross_correlation_trend = fltarr(n_lags,n_lat)
FOR j=0,n_lat-1 DO $
  cross_correlation_trend(*,j) = covariance_trend(*,j) / (STDDEV(precip_lonavg_trend(j,*))*STDDEV(sst_lonavg_trend(j,*)))

;set_plot,'ps'
; For black and white
;device,file='/home/ss901165/idl/hadkpp_runs/precip_sst_crosscorr/hadkpp_npiso_precip_sst_crosscorr_bw.ps',/times,/color,bits_per_pixel=24,/cmyk
;; For colour
;device,file='/home/ss901165/idl/hadgem3_monwg/precip_sst_crosscorr/hadgem3_npiso_precip_sst_crosscorr.ahrqc.ps',$
;  /times,color=1,/cmyk,bits_per_pixel=24
; For black and white
;!p.position=[0.12,0.12,0.95,0.95]
; For colour
;!p.position=[0.12,0.12,0.95,0.80]

;mylevs=indgen(11)*0.1-0.5
;min_val=-2.
;max_val=2.
;mylevs=[min_val,mylevs,max_val]
;intervals=N_ELEMENTS(mylevs)-1
;bottom=3
;LoadCT,33,ncolors=intervals,bottom=bottom
;mycolors=indgen(intervals)+bottom
;mylevs_string=[" ",LEVS2STRING(mylevs(1:intervals-1),1,4)," "]
;white = FSC_COLOR("white",bottom+intervals/2)
;white = FSC_COLOR("white",bottom+intervals/2-1)
;cross_correlation_trend[where(cross_correlation_trend lt 0)] = cross_correlation_trend[where(cross_correlation_trend lt 0)]*2.0
;cross_correlation_trend[where(cross_correlation_trend gt 0)] = cross_correlation_trend[where(cross_correlation_trend gt 0)]*1.3

; For black and white
;gray=FSC_COLOR("gray",2)
;white=FSC_COLOR("white",3)
;contour,cross_correlation_trend,lags,latitude,levels=[-1,-sig_level,sig_level,1],c_colors=[2,3,2,3],$
;  yrange=[min(latitude),max(latitude)],ystyle=1,xtitle='<--- SST Leading (Lag Days) SST Lagging --->',$
;  ytitle='Latitude (degrees north)',xticks=10,xtickv=[-25,-20,-15,-10,-5,0,5,10,15,20,25],$
;  xtickname=['-25','-20','-15','-10','-5','0','+5','+10','+15','+20','+25'],c_thick=3,thick=3,charsize=1.25,$
;  xthick=3,ythick=3,charthick=3,/cell_fill
;contour,cross_correlation_trend,lags,latitude,levels=[0,sig_level],c_colors=[25,50],/overplot,/cell_fill
;contour,cross_correlation_trend,lags,latitude,levels=mylevs,c_linestyle=(mylevs lt 0),/overplot,c_thick=3,thick=3

; For colour
;contour,cross_correlation_trend,lags,latitude,levels=mylevs,c_linestyle=(mylevs lt 0),c_colors=mycolors,color=1,$
;  min_val=min_val,max_val=max_val,/cell_fill,xrange=[min(lags),max(lags)],xstyle=1,$
;  yrange=[min(latitude),max(latitude)],ystyle=1,xtitle='<--- SST Leading (Lag Days) SST Lagging --->',$
;  ytitle='Latitude (degrees north)',xticks=10,xtickv=[-25,-20,-15,-10,-5,0,5,10,15,20,25],$
;  xtickname=['-25','-20','-15','-10','-5','0','+5','+10','+15','+20','+25'],c_thick=3,thick=3,charsize=1.25,$
;  xthick=3,ythick=3,charthick=3

;plotsym2,0,0.4,/fill
;black = FSC_COLOR("black",30)
;FOR j=0,n_lags-1 DO BEGIN
;    IF TOTAL(where(ABS(cross_correlation_trend(j,*)*1.2) gt sig_level)) ne -1 THEN $
;      PLOTS,REPLICATE(lags(j),n_lags),$
;      latitude[where(ABS(cross_correlation_trend(j,*)*1.2) gt sig_level)],psym=8,color=30
;ENDFOR

;contour,cross_correlation_trend,lags,latitude,levels=[-1,-sig_level,sig_level,1],color=50,c_linestyle=0,$
;  c_thick=3,/overplot

;colorbar,divisions=intervals,ncolors=intervals,bottom=bottom,ticknames=mylevs_string,$
;  position=[0.05,0.90,0.95,0.93],minor=0,charthick=3,charsize=1.25,thick=3,$
;  title="HadGEM3_n96 JJAS lagcorr SST and rain 80-90E"

;device,/close

psfile='/home/ss901165/idl/hadgem3_monwg/precip_sst_crosscorr/hadgem3_npiso_precip_sst_crosscorr.ahrqc.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=900,YOFFSET=500,TFONT=2,TCHARSIZE=120
GSET,XMIN=MIN(lags),XMAX=MAX(lags),YMIN=lat_range(0),YMAX=lat_range(1)
mylevs=['-0.55','-0.45','-0.35','-0.25','-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55']
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
LEVS,MANUAL=mylevs
CON,FIELD=cross_correlation_trend,X=lags,Y=latitude,/NOLINES,/CB_RIGHT,$
  TITLE='Lag corr btwn 11-day centred lin trend in rainfall and SST lon-avg 75-95E - HadGEM3-A_ahrqc_n96'
black=FSC_COLOR("black",30)
FOR j=0,n_lags-1 DO BEGIN
    IF TOTAL(where(ABS(cross_correlation_trend(j,*)) gt sig_level)) ne -1 THEN $
      GPLOT,X=REPLICATE(lags(j),N_ELEMENTS(where(ABS(cross_correlation_trend(j,*)) gt sig_level)))$
      ,Y=latitude[where(ABS(cross_correlation_trend(j,*)) gt sig_level)],$
      SYM=3,COL=30,/NOLINES,SIZE=50
ENDFOR
AXES,XSTEP=5,YSTEP=5,YMINOR=1,XMINOR=1,xtitle='<--- SST Leading (Lag Days) SST Lagging --->',$
  ytitle='Latitude (degrees north)'

PSCLOSE,/NOVIEW

STOP

END
