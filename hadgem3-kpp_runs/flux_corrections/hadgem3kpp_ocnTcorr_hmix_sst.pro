PRO hadgem3kpp_ocnTcorr_hmix_sst

; Plot maximum hmix, mean hmix and SST biases

;kpp_sst_infile='/home/ss901165/um_output6/xihvc/kpp_i2-j1_cat.nc'
;analysis_sst_infile='/home/ss901165/um_output6/xihvc/ocnT_clim.nc'
kpp_hmix_infile='/home/ss901165/um_output6/xihvf/hmix_ts.nc'
lsm_ocndepth_file='/home/ss901165/um_output6/xihvf/lsm_ocndepth.nc'
kpp_ntime=720

kpp_longitude=OPEN_AND_EXTRACT(kpp_hmix_infile,'longitude')
kpp_latitude=OPEN_AND_EXTRACT(kpp_hmix_infile,'latitude')
kpp_nlon=N_ELEMENTS(kpp_longitude)
kpp_nlat=N_ELEMENTS(kpp_latitude)

box=[kpp_latitude(0),kpp_longitude(0),kpp_latitude(kpp_nlat-1),kpp_longitude(kpp_nlon-1)]

;analysis_longitude=OPEN_AND_EXTRACT(analysis_sst_infile,'longitude')
;analysis_latitude=OPEN_AND_EXTRACT(analysis_sst_infile,'latitude')
;DEFINE_BOUNDARIES,box,analysis_latitude,analysis_longitude,analysis_box_tx,/LIMIT
;analysis_nlon=N_ELEMENTS(analysis_longitude)
;analysis_nlat=N_ELEMENTS(analysis_latitude)

hmix=OPEN_AND_EXTRACT(kpp_hmix_infile,'hmix',offset=[0,0,0],count=[kpp_nlon,kpp_nlat,kpp_ntime])
;amean_sst=REFORM(OPEN_AND_EXTRACT(kpp_sst_infile,'T',$
;                            offset=[0,0,0,0,0],count=[kpp_nlon,kpp_nlat,1,1,kpp_ntime]))
;analysis_sst=REFORM(OPEN_AND_EXTRACT(analysis_sst_infile,'temperature',$
;                                     offset=[analysis_box_tx(1),analysis_box_tx(0),0,0],count=[analysis_nlon,analysis_nlat,1,360]))

mask_longitude=OPEN_AND_EXTRACT(lsm_ocndepth_file,'longitude')
mask_latitude=OPEN_AND_EXTRACT(lsm_ocndepth_file,'latitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)
mask=OPEN_AND_EXTRACT(lsm_ocndepth_file,'lsm',$
                      offset=[mask_box_tx(1),mask_box_tx(0)],count=[mask_nlon,mask_nlat])

max_hmix=fltarr(kpp_nlon,kpp_nlat)
count_max_hmix=fltarr(kpp_nlon,kpp_nlat)
mean_hmix=fltarr(kpp_nlon,kpp_nlat)
;amean_sst=fltarr(kpp_nlon,kpp_nlat,kpp_ntime/12)
;analysis_amean_sst=fltarr(analysis_nlon,analysis_nlat)

FOR i=0,kpp_nlon-1 DO BEGIN
   FOR j=0,kpp_nlat-1 DO BEGIN
      IF mask(i,j) eq 0 THEN BEGIN
         max_hmix(i,j)=MAX(hmix(i,j,*))
         mean_hmix(i,j)=MEAN(hmix(i,j,*))
	 IF TOTAL(where(hmix(i,j,*) gt 960)) ge 0 THEN BEGIN 
         count_max_hmix(i,j)=N_ELEMENTS(where(hmix(i,j,*) gt 960))
	 ENDIF ELSE count_max_hmix(i,j)=0
                                ;FOR k=0,kpp_ntime/12-1 DO $
                                ;amean_sst(i,j,k)=MEAN(kpp_sst(i,j,k*12:(k+1)*12-1))
      ENDIF ELSE BEGIN
         count_max_hmix(i,j)=!Values.F_NaN
         max_hmix(i,j)=!Values.F_NaN
         mean_hmix(i,j)=!Values.F_NaN
      ENDELSE
   ENDFOR
ENDFOR
;FOR i=0,analysis_nlon-1 DO $
;   FOR j=0,analysis_nlat-1 DO $
;      analysis_amean_sst(i,j)=MEAN(analysis_sst(i,j,*))

psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_ocnTcorr_hmix_sst.xihvf_max_hmix.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110
MAP,LATMIN=-60,LATMAX=60,LONMIN=1,LONMAX=359
mylevs=['0','50','100','150','200','250','300','350','400','450','500','550','600','650','700','750','800','850','900','950','1000']
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
LEVS,MANUAL=mylevs
CON,X=kpp_longitude,Y=kpp_latitude,FIELD=max_hmix,CB_TITLE='Maximum mixing depth (m)',$
    TITLE='Maximum mixing depth in xihvf (15-day T,S)',/NOLINES
AXES,XSTEP=30,YSTEP=15
PSCLOSE

psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_ocnTcorr_hmix_sst.xihvf_max_hmix_count.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110
MAP,LATMIN=-60,LATMAX=60
mylevs=['1','2','4','8','16','32','64','128','256','512','1024']
CS,SCALE=25,NCOLS=N_ELEMENTS(mylevs)+1,white=[2]
LEVS,MANUAL=mylevs
CON,X=kpp_longitude,Y=kpp_latitude,FIELD=count_max_hmix,CB_TITLE='Number of timesteps',$
    TITLE='Number of timesteps that mixing depth reaches bottom of domain (1000 metres)',/NOLINES,/BLOCK
AXES,XSTEP=30,YSTEP=15
PSCLOSE

psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_ocnTcorr_hmix_sst.xihvf_mean_hmix.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110
MAP,LATMIN=-60,LATMAX=60
mylevs=['0','15','30','45','60','75','90','105','120','135','150','165','180','195','210','225','240','255','270','285','300']
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
LEVS,MANUAL=mylevs
CON,X=kpp_longitude,Y=kpp_latitude,FIELD=mean_hmix,CB_TITLE='Average mixing depth (m)',$
    TITLE='Mean mixing depth in xihvf (15-day T,S)',/NOLINES
AXES,XSTEP=30,YSTEP=15
PSCLOSE,/NOVIEW

;FOR k=0,kpp_ntime-1 DO BEGIN
;   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_ocnTcorr_hmix_sst.xihvd_sst_bias_year'+STRTRIM(STRING(k+1),1)+'.ps'
;   PSOPEN,file=psfile,FONT=2,CHARSIZE=110
;   MAP,LATMIN=-60,LATMAX=60
;   mylevs=['-2.2','-1.8','-1.4','-1.0','-0.6','-0.2','0.2','0.6','1.0','1.4','1.8','2.2']
;   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,white=[8]
;   LEVS,MANUAL=mylevs
;   CON,X=kpp_longitude,Y=kpp_latitude,FIELD=REFORM(amean_sst(*,*,k)-analysis_amean_sst(*,*)),CB_TITLE='Difference in SST (K)',$
;       TITLE='Difference in annual mean SST xihvd-analysis - year '+STRTRIM(STRING(k+1),1),/NOLINES
;   AXES,XSTEP=30,YSTEP=15
;   PSCLOSE,/NOVIEW
;ENDFOR

STOP
END
