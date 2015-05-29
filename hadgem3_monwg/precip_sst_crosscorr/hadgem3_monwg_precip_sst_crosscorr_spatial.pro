PRO hadgem3_monwg_precip_sst_crosscorr_spatial

; ------------ Observations (TMI and GPCP) --------------
gpcp_basedir='/home/ss901165/datasets/GPCP/one_degree'
tmi_basedir='/home/ss901165/datasets/TMI_AMSRE'
gpcp_clim_file=gpcp_basedir+'/gpcp1dd.1997-2007.mean.n144.nc'
tmi_clim_file=tmi_basedir+'/tmi.fusion.1998-2008.amjjaso_clim.monsoon_domain.n144.nc'
obs_start_year=1998
obs_stop_year=2007
obs_nyears=obs_stop_year-obs_start_year+1

mask_n144_file='/home/ss901165/um_output/mask_n144.nc'

precip_box=[15,70,25,85]
gpcp_gridfile=gpcp_basedir+'/gpcp1dd.1998.n144.nc'
gpcp_longitude=OPEN_AND_EXTRACT(gpcp_gridfile,'longitude')
gpcp_latitude=OPEN_AND_EXTRACT(gpcp_gridfile,'latitude')
DEFINE_BOUNDARIES,precip_box,gpcp_latitude,gpcp_longitude,gpcp_box_tx,/LIMIT
gpcp_nlon=N_ELEMENTS(gpcp_longitude)
gpcp_nlat=N_ELEMENTS(gpcp_latitude)

sst_box=[-10,40,30,162]
tmi_gridfile=tmi_basedir+'/tmi.fusion.1998.amjjaso.monsoon_domain.n144.nc'
tmi_longitude=OPEN_AND_EXTRACT(tmi_gridfile,'longitude')
tmi_latitude=OPEN_AND_EXTRACT(tmi_gridfile,'latitude')
DEFINE_BOUNDARIES,sst_box,tmi_latitude,tmi_longitude,tmi_box_tx,/LIMIT
tmi_nlon=N_ELEMENTS(tmi_longitude)
tmi_nlat=N_ELEMENTS(tmi_latitude)

mask_n144_longitude=OPEN_AND_EXTRACT(mask_n144_file,'longitude')
mask_n144_latitude=OPEN_AND_EXTRACT(mask_n144_file,'latitude')
DEFINE_BOUNDARIES,sst_box,mask_n144_latitude,mask_n144_longitude,mask_n144_box_tx,/LIMIT
mask_n144_nlon=N_ELEMENTS(mask_n144_longitude)
mask_n144_nlat=N_ELEMENTS(mask_n144_latitude)
mask_n144=REFORM(OPEN_AND_EXTRACT(mask_n144_file,'lsm',$
                                  offset=[mask_n144_box_tx(1),mask_n144_box_tx(0),0,0],$
                                  count=[mask_n144_nlon,mask_n144_nlat,1,1]))

gpcp_offset_may1=120
gpcp_offset_jun1=150
gpcp_offset_sep30=271
gpcp_offset_oct31=302
gpcp_ntime=gpcp_offset_oct31-gpcp_offset_jun1+1
gpcp_ntime_filter=gpcp_ntime-61
gpcp_ntime_year=gpcp_offset_sep30-gpcp_offset_jun1+1
gpcp_ntime_year_filter=gpcp_ntime_year-60

tmi_offset_may1=30
tmi_offset_jun1=60
tmi_offset_sep30=181
tmi_offset_oct31=212
tmi_ntime=tmi_offset_oct31-tmi_offset_jun1+1
tmi_ntime_filter=tmi_ntime-61
tmi_ntime_year=tmi_offset_sep30-tmi_offset_jun1+1
tmi_ntime_year_filter=tmi_ntime_year-60

; Get climatologies, area-average rainfall and filter
gpcp_clim=REFORM(OPEN_AND_EXTRACT(gpcp_clim_file,'precip',$
                                  offset=[gpcp_box_tx(1),gpcp_box_tx(0),gpcp_offset_jun1],$
                                  count=[gpcp_nlon,gpcp_nlat,gpcp_ntime]))
tmi_clim=REFORM(OPEN_AND_EXTRACT(tmi_clim_file,'sst',$
                                 offset=[tmi_box_tx(1),tmi_box_tx(0),tmi_offset_jun1],$
                                 count=[tmi_nlon,tmi_nlat,tmi_ntime]))

gpcp_clim_aavg=fltarr(gpcp_ntime)
print,'Now filtering GPCP climatology ...'
FOR i=0,gpcp_ntime-1 DO $
  gpcp_clim_aavg(i)=MEAN(gpcp_clim(*,*,i))
gpcp_clim_filter=fltarr(gpcp_ntime_filter)
temp_filter=LANCZOS_BANDPASS(gpcp_clim_aavg,0.,1./60.,101,/DETREND)
gpcp_clim_filter(*)=temp_filter(30:gpcp_ntime-32)

tmi_clim_filter=fltarr(tmi_nlon,tmi_nlat,tmi_ntime_filter)
print,'Now filtering TMI climatology ...'
FOR i=0,tmi_nlon-1 DO BEGIN
    FOR j=0,tmi_nlat-1 DO BEGIN
        temp=REFORM(tmi_clim(i,j,*))
        temp_filter=LANCZOS_BANDPASS(temp,0.,1./60.,101,/DETREND)
        tmi_clim_filter(i,j,*)=temp_filter(30:tmi_ntime-32)
    ENDFOR
ENDFOR

lags=[-12,-8,-4,0,4,8,12]
n_lags=N_ELEMENTS(lags)
covariance=fltarr(tmi_nlon,tmi_nlat,n_lags)
allyears_gpcp=fltarr(gpcp_ntime_year_filter*obs_nyears)
allyears_tmi=fltarr(tmi_nlon,tmi_nlat,tmi_ntime_year_filter*obs_nyears)
FOR i=0,obs_nyears-1 DO BEGIN
    this_year_str=STRTRIM(STRING(obs_start_year+i),1)
    print,'Now computing correlations for observations in '+this_year_str+' ...'
    gpcp_file=gpcp_basedir+'/gpcp1dd.'+this_year_str+'.n144.nc'
    tmi_file=tmi_basedir+'/tmi.fusion.'+this_year_str+'.amjjaso.monsoon_domain.n144.nc'
    
    gpcp_year=REFORM(OPEN_AND_EXTRACT(gpcp_file,'precip',$
                                      offset=[gpcp_box_tx(1),gpcp_box_tx(0),gpcp_offset_jun1],$
                                      count=[gpcp_nlon,gpcp_nlat,gpcp_ntime_year]))
    tmi_year=REFORM(OPEN_AND_EXTRACT(tmi_file,'sst',$
                                     offset=[tmi_box_tx(1),tmi_box_tx(0),tmi_offset_jun1],$
                                     count=[tmi_nlon,tmi_nlat,tmi_ntime_year]))

    gpcp_year_aavg=fltarr(gpcp_ntime_year)
    FOR j=0,gpcp_ntime_year-1 DO $
      gpcp_year_aavg(j)=MEAN(gpcp_year(*,*,j))-gpcp_clim_aavg(j);_filter(j)
    gpcp_year_aavg=gpcp_year_aavg-MEAN(gpcp_year_aavg)
    gpcp_year_aavg_filter=fltarr(gpcp_ntime_year_filter)
    temp_filter=LANCZOS_BANDPASS(gpcp_year_aavg,1./50.,1./30.,101,/DETREND)
    gpcp_year_aavg_filter(*)=temp_filter(30:gpcp_ntime_year-31)
    allyears_gpcp(i*gpcp_ntime_year_filter:(i+1)*gpcp_ntime_year_filter-1)=gpcp_year_aavg_filter

    tmi_year=tmi_year-tmi_clim;_filter    
    FOR j=0,tmi_nlon-1 DO BEGIN
        FOR k=0,tmi_nlat-1 DO BEGIN
            IF mask_n144(j,k) eq 0 THEN BEGIN
                temp_tmi=REFORM(tmi_year(j,k,*))-MEAN(tmi_year(j,k,*))
                temp_filter=LANCZOS_BANDPASS(temp_tmi,1./50.,1./30.,101,/DETREND)
                temp_tmi=temp_filter(30:tmi_ntime_year-31)
                allyears_tmi(j,k,i*tmi_ntime_year_filter:(i+1)*tmi_ntime_year_filter-1)=temp_tmi
                covariance(j,k,*)=covariance(j,k,*)+C_CORRELATE(gpcp_year_aavg_filter,temp_tmi,lags+3,/DOUBLE,/COVARIANCE)/FLOAT(obs_nyears)
            ENDIF ELSE $
              covariance(j,k,*)=!Values.F_NaN
        ENDFOR
    ENDFOR
ENDFOR

corr_coeff=fltarr(tmi_nlon,tmi_nlat,n_lags)
regression_sst_tmi=fltarr(tmi_nlon,tmi_nlat,n_lags)
FOR i=0,tmi_nlon-1 DO BEGIN
    FOR j=0,tmi_nlat-1 DO BEGIN
        corr_coeff(i,j,*)=covariance(i,j,*)/(STDDEV(allyears_gpcp)*STDDEV(allyears_tmi(i,j,*)))
        regression_sst_tmi(i,j,*)=corr_coeff(i,j,*)*STDDEV(allyears_tmi(i,j,*))/STDDEV(allyears_gpcp)
    ENDFOR
ENDFOR
        
corr_coeff_conf=fltarr(tmi_nlon,tmi_nlat,n_lags)
; Significance levels for 30 DOFs
sig50=0.1237
sig80=0.2327
sig90=0.2960
sig95=0.3494
sig98=0.4093
sig99=0.4487
corr_coeff_conf[where(ABS(corr_coeff) gt sig50)]=50*corr_coeff[where(ABS(corr_coeff) gt sig50)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig50)])
corr_coeff_conf[where(ABS(corr_coeff) gt sig80)]=80*corr_coeff[where(ABS(corr_coeff) gt sig80)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig80)])
corr_coeff_conf[where(ABS(corr_coeff) gt sig90)]=90*corr_coeff[where(ABS(corr_coeff) gt sig90)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig90)])
corr_coeff_conf[where(ABS(corr_coeff) gt sig95)]=95*corr_coeff[where(ABS(corr_coeff) gt sig95)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig95)])
corr_coeff_conf[where(ABS(corr_coeff) gt sig98)]=98*corr_coeff[where(ABS(corr_coeff) gt sig98)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig98)])
corr_coeff_conf[where(ABS(corr_coeff) gt sig99)]=99*corr_coeff[where(ABS(corr_coeff) gt sig99)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig99)])

mylevs_corr=['-99','-98','-95','-90','-80','-50','50','80','90','95','98','99']
mylevs_regress=['-0.08','-0.07','-0.06','-0.05','-0.04','-0.03','-0.02','-0.01','0','0.01','0.02','0.03','0.04','0.05','0.06','0.07','0.08']

regression_sst_tmi[where(ABS(corr_coeff_conf) lt 90)]=!Values.F_NaN

FOR i=0,n_lags-1 DO BEGIN
    psfile='/home/ss901165/idl/hadgem3_monwg/precip_sst_crosscorr/hadgem3_monwg_precip_sst_crosscorr_spatial.gpcp_tmi_n144.filter3050_lag'+STRTRIM(STRING(lags(i)),1)+'.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
    CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs_corr)+1
    MAP,LONMIN=40,LONMAX=160,LATMIN=-10,LATMAX=30
    LEVS,MANUAL=mylevs_corr
    CON,FIELD=corr_coeff_conf(*,*,i),X=tmi_longitude,Y=tmi_latitude,/NOLINES,$
      TITLE="Corr for 30-50 day anomalies in TMI locally and GPCP aavg (15-25N, 70-85E) for 1998-2007, LAG="+STRTRIM(STRING(lags(i)),1)+" - July and August"
    black=FSC_COLOR("black",30)
    GPLOT,X=gpcp_longitude,Y=REPLICATE(15,gpcp_nlon),COL=30,THICK=150
    GPLOT,X=gpcp_longitude,Y=REPLICATE(25,gpcp_nlon),COL=30,THICK=150
    GPLOT,X=REPLICATE(70,gpcp_nlat),Y=gpcp_latitude,COL=30,THICK=150
    GPLOT,X=REPLICATE(85,gpcp_nlat),Y=gpcp_latitude,COL=30,THICK=150
    ;sig_level=0.32
    ;FOR j=0,tmi_nlon-1 DO BEGIN
    ;    FOR k=0,tmi_nlat-1 DO BEGIN
    ;        IF ABS(corr_coeff(j,k,i)) gt sig_level THEN $
    ;          GPLOT,X=tmi_longitude(j),Y=tmi_latitude(k),SYM=3,SIZE=40,COL=30,/NOLINES
    ;    ENDFOR
    ;ENDFOR
    PSCLOSE,/NOVIEW

    psfile='/home/ss901165/idl/hadgem3_monwg/precip_sst_crosscorr/hadgem3_monwg_precip_sst_crosscorr_spatial.gpcp_tmi_n144.filter3050_lag'+STRTRIM(STRING(lags(i)),1)+'_regression.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
    CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs_regress)+1
    MAP,LONMIN=40,LONMAX=160,LATMIN=-10,LATMAX=30
    LEVS,MANUAL=mylevs_regress
    CON,FIELD=regression_sst_tmi(*,*,i),X=tmi_longitude,Y=tmi_latitude,/NOLINES,$
      TITLE="Regress 30-50 day anomalies in TMI (K) on GPCP aavg (15-25N, 70-85E) for 1998-2007, LAG="+STRTRIM(STRING(lags(i)),1)+" - July and August",CB_NTH=2
    black=FSC_COLOR("black",30)
    GPLOT,X=gpcp_longitude,Y=REPLICATE(15,gpcp_nlon),COL=30,THICK=150
    GPLOT,X=gpcp_longitude,Y=REPLICATE(25,gpcp_nlon),COL=30,THICK=150
    GPLOT,X=REPLICATE(70,gpcp_nlat),Y=gpcp_latitude,COL=30,THICK=150
    GPLOT,X=REPLICATE(85,gpcp_nlat),Y=gpcp_latitude,COL=30,THICK=150
    PSCLOSE,/NOVIEW
ENDFOR

; -------------------- HadGEM3-AO ------------------
precip_clim_file='/home/ss901165/um_output2/hadgem3_monwg/ahsaf/ahsaf.precip.clim.daily_20years.nc'
precip_daily_file='/home/ss901165/um_output2/hadgem3_monwg/ahsaf/ahsaf.precip.daily_20years.nc'
surftemp_clim_file='/home/ss901165/um_output2/hadgem3_monwg/ahsaf/ahsaf.surftemp.clim.daily_20years.nc'
surftemp_daily_file='/home/ss901165/um_output2/hadgem3_monwg/ahsaf/ahsaf.surftemp.daily_20years.nc'
mask_n96_file='/home/ss901165/um_output/mask_n96.nc'

precip_longitude=OPEN_AND_EXTRACT(precip_clim_file,'longitude')
precip_latitude=OPEN_AND_EXTRACT(precip_clim_file,'latitude')
DEFINE_BOUNDARIES,precip_box,precip_latitude,precip_longitude,precip_box_tx,/LIMIT
precip_nlon=N_ELEMENTS(precip_longitude)
precip_nlat=N_ELEMENTS(precip_latitude)
sst_longitude=OPEN_AND_EXTRACT(surftemp_clim_file,'longitude')
sst_latitude=OPEN_AND_EXTRACT(surftemp_clim_file,'latitude')
DEFINE_BOUNDARIES,sst_box,sst_latitude,sst_longitude,sst_box_tx,/LIMIT
sst_nlon=N_ELEMENTS(sst_longitude)
sst_nlat=N_ELEMENTS(sst_latitude)

mask_n96_longitude=OPEN_AND_EXTRACT(mask_n96_file,'longitude')
mask_n96_latitude=OPEN_AND_EXTRACT(mask_n96_file,'latitude')
DEFINE_BOUNDARIES,sst_box,mask_n96_latitude,mask_n96_longitude,mask_n96_box_tx,/LIMIT
mask_n96_nlon=N_ELEMENTS(mask_n96_longitude)
mask_n96_nlat=N_ELEMENTS(mask_n96_latitude)
mask_n96=REFORM(OPEN_AND_EXTRACT(mask_n96_file,'lsm',$
                                  offset=[mask_n96_box_tx(1),mask_n96_box_tx(0),0,0],$
                                  count=[mask_n96_nlon,mask_n96_nlat,1,1]))
mask_n96_rev=fltarr(mask_n96_nlon,mask_n96_nlat)
FOR i=0,mask_n96_nlon-1 DO $
  FOR j=0,mask_n96_nlat-1 DO $
  mask_n96_rev(i,j)=mask_n96(i,mask_n96_nlat-j-1)

hadgem3_offset_may1=120
hadgem3_offset_jun1=150
hadgem3_offset_sep30=269
hadgem3_offset_oct31=299
hadgem3_ntime=hadgem3_offset_oct31-hadgem3_offset_may1+1
hadgem3_ntime_filter=hadgem3_ntime-60
hadgem3_ntime_year=hadgem3_offset_sep30-hadgem3_offset_jun1+1
hadgem3_ntime_year_filter=hadgem3_ntime_year-60
hadgem3_nyears=20

; Get climatologies, area-average precipfall and filter
precip_clim=REFORM(OPEN_AND_EXTRACT(precip_clim_file,'precip',$
                                  offset=[precip_box_tx(1),precip_box_tx(0),0,hadgem3_offset_jun1],$
                                  count=[precip_nlon,precip_nlat,1,hadgem3_ntime]))*86400.
sst_clim=REFORM(OPEN_AND_EXTRACT(surftemp_clim_file,'temp',$
                                 offset=[sst_box_tx(1),sst_box_tx(0),0,hadgem3_offset_jun1],$
                                 count=[sst_nlon,sst_nlat,1,hadgem3_ntime]))

precip_clim_aavg=fltarr(hadgem3_ntime)
print,'Now filtering HadGEM3-AO precip climatology ...'
FOR i=0,hadgem3_ntime-1 DO $
  precip_clim_aavg(i)=MEAN(precip_clim(*,*,i))
precip_clim_filter=fltarr(hadgem3_ntime_filter)
temp_filter=LANCZOS_BANDPASS(precip_clim_aavg,0.,1./60.,101,/DETREND)
precip_clim_filter(*)=temp_filter(30:hadgem3_ntime-31)

sst_clim_filter=fltarr(sst_nlon,sst_nlat,hadgem3_ntime_filter)
print,'Now filtering HadGEM3-AO SST climatology ...'
FOR i=0,sst_nlon-1 DO BEGIN
    FOR j=0,sst_nlat-1 DO BEGIN
        temp=REFORM(sst_clim(i,j,*))
        temp_filter=LANCZOS_BANDPASS(temp,0.,1./60.,101,/DETREND)
        sst_clim_filter(i,j,*)=temp_filter(30:hadgem3_ntime-31)
    ENDFOR
ENDFOR

covariance=fltarr(sst_nlon,sst_nlat,n_lags)
allyears_precip=fltarr(hadgem3_ntime_year_filter*hadgem3_nyears)
allyears_sst=fltarr(sst_nlon,sst_nlat,hadgem3_ntime_year_filter*hadgem3_nyears)
FOR i=0,hadgem3_nyears-1 DO BEGIN
    print,'Now computing correlations for HadGEM3-AO in year '+STRTRIM(STRING(i+1),1)+' ...'    
    precip_year=REFORM(OPEN_AND_EXTRACT(precip_daily_file,'precip',$
                                      offset=[precip_box_tx(1),precip_box_tx(0),0,hadgem3_offset_jun1,i],$
                                      count=[precip_nlon,precip_nlat,1,hadgem3_ntime_year,1]))*86400.
    sst_year=REFORM(OPEN_AND_EXTRACT(surftemp_daily_file,'temp',$
                                     offset=[sst_box_tx(1),sst_box_tx(0),0,hadgem3_offset_jun1,i],$
                                     count=[sst_nlon,sst_nlat,1,hadgem3_ntime_year,1]))

    precip_year_aavg=fltarr(hadgem3_ntime_year)
    FOR j=0,hadgem3_ntime_year-1 DO $
      precip_year_aavg(j)=MEAN(precip_year(*,*,j))-precip_clim_aavg(j);_filter(j)
    precip_year_aavg=precip_year_aavg-MEAN(precip_year_aavg)
    precip_year_aavg_filter=fltarr(hadgem3_ntime_year_filter)
    temp_filter=LANCZOS_BANDPASS(precip_year_aavg,1./50.,1./30.,101,/DETREND)
    precip_year_aavg_filter(*)=temp_filter(30:hadgem3_ntime_year-31)
    allyears_precip(i*hadgem3_ntime_year_filter:(i+1)*hadgem3_ntime_year_filter-1)=precip_year_aavg_filter

    sst_year=sst_year-sst_clim;_filter    
    FOR j=0,sst_nlon-1 DO BEGIN
        FOR k=0,sst_nlat-1 DO BEGIN
            IF mask_n96_rev(j,k) eq 0 THEN BEGIN
                temp_sst=REFORM(sst_year(j,k,*))-MEAN(sst_year(j,k,*))
                temp_filter=LANCZOS_BANDPASS(temp_sst,1./50.,1./30.,101,/DETREND)
                temp_sst=temp_filter(30:hadgem3_ntime_year-31)
                allyears_sst(j,k,i*hadgem3_ntime_year_filter:(i+1)*hadgem3_ntime_year_filter-1)=temp_sst
                covariance(j,k,*)=covariance(j,k,*)+C_CORRELATE(precip_year_aavg_filter,temp_sst,lags-4,/DOUBLE,/COVARIANCE)/FLOAT(hadgem3_nyears)
            ENDIF ELSE $
              covariance(j,k)=!Values.F_NaN
        ENDFOR
    ENDFOR
    ;print,MIN(covariance,/NaN),MAX(covariance,/NaN)
ENDFOR

corr_coeff=fltarr(sst_nlon,sst_nlat,n_lags)
regression_sst=fltarr(sst_nlon,sst_nlat,n_lags)
FOR i=0,sst_nlon-1 DO BEGIN
    FOR j=0,sst_nlat-1 DO BEGIN
        corr_coeff(i,j,*)=covariance(i,j,*)/(STDDEV(allyears_precip)*STDDEV(allyears_sst(i,j,*)))
        regression_sst(i,j,*)=corr_coeff(i,j,*)*STDDEV(allyears_sst(i,j,*))/STDDEV(allyears_precip)
    ENDFOR
ENDFOR

corr_coeff_conf=fltarr(sst_nlon,sst_nlat,n_lags)
; Significance levels for 60 DOFs
sig50=0.0873
sig80=0.1650
sig90=0.2108
sig95=0.2500
sig98=0.2948
sig99=0.3248
corr_coeff_conf[where(ABS(corr_coeff) gt sig50)]=50*corr_coeff[where(ABS(corr_coeff) gt sig50)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig50)])
corr_coeff_conf[where(ABS(corr_coeff) gt sig80)]=80*corr_coeff[where(ABS(corr_coeff) gt sig80)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig80)])
corr_coeff_conf[where(ABS(corr_coeff) gt sig90)]=90*corr_coeff[where(ABS(corr_coeff) gt sig90)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig90)])
corr_coeff_conf[where(ABS(corr_coeff) gt sig95)]=95*corr_coeff[where(ABS(corr_coeff) gt sig95)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig95)])
corr_coeff_conf[where(ABS(corr_coeff) gt sig98)]=98*corr_coeff[where(ABS(corr_coeff) gt sig98)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig98)])
corr_coeff_conf[where(ABS(corr_coeff) gt sig99)]=99*corr_coeff[where(ABS(corr_coeff) gt sig99)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig99)])

mylevs=['-99','-98','-95','-90','-80','-50','50','80','90','95','98','99']
mylevs_regress=['-0.08','-0.07','-0.06','-0.05','-0.04','-0.03','-0.02','-0.01','0','0.01','0.02','0.03','0.04','0.05','0.06','0.07','0.08']
regression_sst[where(ABS(corr_coeff_conf) lt 90)]=!Values.F_NaN

FOR i=0,n_lags-1 DO BEGIN
    psfile='/home/ss901165/idl/hadgem3_monwg/precip_sst_crosscorr/hadgem3_monwg_precip_sst_crosscorr_spatial.hadgem3_ahsaf_n96.filter3050_lag'+STRTRIM(STRING(lags(i)),1)+'.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
    CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1
    MAP,LONMIN=40,LONMAX=160,LATMIN=-10,LATMAX=30
    LEVS,MANUAL=mylevs
    CON,FIELD=corr_coeff_conf(*,*,i),X=sst_longitude,Y=sst_latitude,/NOLINES,$
      TITLE="Corr 30-50 day anomalies in SST locally and precip aavg (15-25N, 70-85E) for HadGEM3-AO_ahsaf_n96, LAG="+STRTRIM(STRING(lags(i)),1)+" July and August"
    black=FSC_COLOR("black",30)
    GPLOT,X=precip_longitude,Y=REPLICATE(precip_latitude(0),precip_nlon),COL=30,THICK=150
    GPLOT,X=precip_longitude,Y=REPLICATE(precip_latitude(precip_nlat-1),precip_nlon),COL=30,THICK=150
    GPLOT,X=REPLICATE(precip_longitude(0),precip_nlat),Y=precip_latitude,COL=30,THICK=150
    GPLOT,X=REPLICATE(precip_longitude(precip_nlon-1),precip_nlat),Y=precip_latitude,COL=30,THICK=150
;    sig_level=0.195
;    FOR j=0,sst_nlon-1 DO BEGIN
;        FOR k=0,sst_nlat-1 DO BEGIN
;            IF ABS(corr_coeff(j,k,i)) gt sig_level THEN $
;              GPLOT,X=sst_longitude(j),Y=sst_latitude(k),SYM=3,SIZE=40,COL=30,/NOLINES
;        ENDFOR
;    ENDFOR
    PSCLOSE,/NOVIEW

    psfile='/home/ss901165/idl/hadgem3_monwg/precip_sst_crosscorr/hadgem3_monwg_precip_sst_crosscorr_spatial.hadgem3_ahsaf_n96.filter3050_lag'+STRTRIM(STRING(lags(i)),1)+'_regression.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
    CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs_regress)+1
    MAP,LONMIN=40,LONMAX=160,LATMIN=-10,LATMAX=30
    LEVS,MANUAL=mylevs_regress
    CON,FIELD=regression_sst(*,*,i),X=sst_longitude,Y=sst_latitude,/NOLINES,$
      TITLE="Regress 30-50 day anomalies in SST (K) on precip aavg (15-25N, 70-85E) for HadGEM3-AO_ahsaf_n96, LAG="+STRTRIM(STRING(lags(i)),1)+" - July and August",CB_NTH=2
    black=FSC_COLOR("black",30)
    GPLOT,X=precip_longitude,Y=REPLICATE(15,precip_nlon),COL=30,THICK=150
    GPLOT,X=precip_longitude,Y=REPLICATE(25,precip_nlon),COL=30,THICK=150
    GPLOT,X=REPLICATE(70,precip_nlat),Y=precip_latitude,COL=30,THICK=150
    GPLOT,X=REPLICATE(85,precip_nlat),Y=precip_latitude,COL=30,THICK=150
    PSCLOSE,/NOVIEW

ENDFOR

; -------------------- HiGEM ------------------
precip_clim_file='/home/ss901165/um_output2/higem_monwg/higem.clim.jan-dec.precip.monsoon_domain.nc'
precip_daily_file='/home/ss901165/um_output2/higem_monwg/higem.jan-dec.precip.monsoon_domain.nc'
surftemp_clim_file='/home/ss901165/um_output2/higem_monwg/higem.clim.jan-dec.surftemp.monsoon_domain.nc'
surftemp_daily_file='/home/ss901165/um_output2/higem_monwg/higem.jan-dec.surftemp.monsoon_domain.nc'
mask_n144_file='/home/ss901165/um_output/mask_n144.nc'

precip_longitude=OPEN_AND_EXTRACT(precip_clim_file,'longitude')
precip_latitude=OPEN_AND_EXTRACT(precip_clim_file,'latitude')
DEFINE_BOUNDARIES,precip_box,precip_latitude,precip_longitude,precip_box_tx,/LIMIT
precip_nlon=N_ELEMENTS(precip_longitude)
precip_nlat=N_ELEMENTS(precip_latitude)
sst_longitude=OPEN_AND_EXTRACT(surftemp_clim_file,'longitude')
sst_latitude=OPEN_AND_EXTRACT(surftemp_clim_file,'latitude')
DEFINE_BOUNDARIES,sst_box,sst_latitude,sst_longitude,sst_box_tx,/LIMIT
sst_nlon=N_ELEMENTS(sst_longitude)
sst_nlat=N_ELEMENTS(sst_latitude)

mask_n144_longitude=OPEN_AND_EXTRACT(mask_n144_file,'longitude')
mask_n144_latitude=OPEN_AND_EXTRACT(mask_n144_file,'latitude')
DEFINE_BOUNDARIES,sst_box,mask_n144_latitude,mask_n144_longitude,mask_n144_box_tx,/LIMIT
mask_n144_nlon=N_ELEMENTS(mask_n144_longitude)
mask_n144_nlat=N_ELEMENTS(mask_n144_latitude)
mask_n144=REFORM(OPEN_AND_EXTRACT(mask_n144_file,'lsm',$
                                  offset=[mask_n144_box_tx(1),mask_n144_box_tx(0),0,0],$
                                  count=[mask_n144_nlon,mask_n144_nlat,1,1]))
mask_n144_rev=fltarr(mask_n144_nlon,mask_n144_nlat)
FOR i=0,mask_n144_nlon-1 DO $
  FOR j=0,mask_n144_nlat-1 DO $
  mask_n144_rev(i,j)=mask_n144(i,mask_n144_nlat-j-1)

higem_offset_may1=120
higem_offset_jun1=150
higem_offset_sep30=269
higem_offset_oct31=299
higem_ntime=higem_offset_oct31-higem_offset_may1+1
higem_ntime_filter=higem_ntime-60
higem_ntime_year=higem_offset_sep30-higem_offset_jun1+1
higem_ntime_year_filter=higem_ntime_year-60
higem_nyears=81

; Get climatologies, area-average precipfall and filter
precip_clim=REFORM(OPEN_AND_EXTRACT(precip_clim_file,'precip',$
                                  offset=[precip_box_tx(1),precip_box_tx(0),0,higem_offset_jun1],$
                                  count=[precip_nlon,precip_nlat,1,higem_ntime]))*86400.
sst_clim=REFORM(OPEN_AND_EXTRACT(surftemp_clim_file,'temp',$
                                 offset=[sst_box_tx(1),sst_box_tx(0),0,higem_offset_jun1],$
                                 count=[sst_nlon,sst_nlat,1,higem_ntime]))

precip_clim_aavg=fltarr(higem_ntime)
print,'Now filtering HiGEM precip climatology ...'
FOR i=0,higem_ntime-1 DO $
  precip_clim_aavg(i)=MEAN(precip_clim(*,*,i))
precip_clim_filter=fltarr(higem_ntime_filter)
temp_filter=LANCZOS_BANDPASS(precip_clim_aavg,0.,1./60.,101,/DETREND)
precip_clim_filter(*)=temp_filter(30:higem_ntime-31)

sst_clim_filter=fltarr(sst_nlon,sst_nlat,higem_ntime_filter)
print,'Now filtering HiGEM SST climatology ...'
FOR i=0,sst_nlon-1 DO BEGIN
    FOR j=0,sst_nlat-1 DO BEGIN
        temp=REFORM(sst_clim(i,j,*))
        temp_filter=LANCZOS_BANDPASS(temp,0.,1./60.,101,/DETREND)
        sst_clim_filter(i,j,*)=temp_filter(30:higem_ntime-31)
    ENDFOR
ENDFOR

covariance=fltarr(sst_nlon,sst_nlat,n_lags)
allyears_precip=fltarr(higem_ntime_year_filter*higem_nyears)
allyears_sst=fltarr(sst_nlon,sst_nlat,higem_ntime_year_filter*higem_nyears)
FOR i=0,higem_nyears-1 DO BEGIN
    print,'Now computing correlations for HiGEM in year '+STRTRIM(STRING(i+1),1)+' ...'    
    precip_year=REFORM(OPEN_AND_EXTRACT(precip_daily_file,'precip',$
                                      offset=[precip_box_tx(1),precip_box_tx(0),0,higem_offset_jun1,i],$
                                      count=[precip_nlon,precip_nlat,1,higem_ntime_year,1]))*86400.
    sst_year=REFORM(OPEN_AND_EXTRACT(surftemp_daily_file,'temp',$
                                     offset=[sst_box_tx(1),sst_box_tx(0),0,higem_offset_jun1,i],$
                                     count=[sst_nlon,sst_nlat,1,higem_ntime_year,1]))

    precip_year_aavg=fltarr(higem_ntime_year)
    FOR j=0,higem_ntime_year-1 DO $
      precip_year_aavg(j)=MEAN(precip_year(*,*,j))-precip_clim_aavg(j);_filter(j)
    precip_year_aavg=precip_year_aavg-MEAN(precip_year_aavg)
    precip_year_aavg_filter=fltarr(higem_ntime_year_filter)
    temp_filter=LANCZOS_BANDPASS(precip_year_aavg,1./50.,1./30.,101,/DETREND)
    precip_year_aavg_filter(*)=temp_filter(30:higem_ntime_year-31)
    allyears_precip(i*higem_ntime_year_filter:(i+1)*higem_ntime_year_filter-1)=precip_year_aavg_filter

    sst_year=sst_year-sst_clim;_filter    
    FOR j=0,sst_nlon-1 DO BEGIN
        FOR k=0,sst_nlat-1 DO BEGIN
            IF mask_n144_rev(j,k) eq 0 THEN BEGIN
                temp_sst=REFORM(sst_year(j,k,*))-MEAN(sst_year(j,k,*))
                temp_filter=LANCZOS_BANDPASS(temp_sst,1./50.,1./30.,101,/DETREND)
                temp_sst=temp_filter(30:higem_ntime_year-31)
                allyears_sst(j,k,i*higem_ntime_year_filter:(i+1)*higem_ntime_year_filter-1)=temp_sst
                covariance(j,k,*)=covariance(j,k,*)+C_CORRELATE(precip_year_aavg_filter,temp_sst,lags-4,/DOUBLE,/COVARIANCE)/FLOAT(higem_nyears)
            ENDIF ELSE $
              covariance(j,k)=!Values.F_NaN
        ENDFOR
    ENDFOR
    ;print,MIN(covariance,/NaN),MAX(covariance,/NaN)
ENDFOR

corr_coeff=fltarr(sst_nlon,sst_nlat,n_lags)
regression_sst=fltarr(sst_nlon,sst_nlat,n_lags)
FOR i=0,sst_nlon-1 DO BEGIN
    FOR j=0,sst_nlat-1 DO BEGIN
        corr_coeff(i,j,*)=covariance(i,j,*)/(STDDEV(allyears_precip)*STDDEV(allyears_sst(i,j,*)))
        regression_sst(i,j,*)=corr_coeff(i,j,*)*STDDEV(allyears_sst(i,j,*))/STDDEV(allyears_precip)
    ENDFOR
ENDFOR

corr_coeff_conf=fltarr(sst_nlon,sst_nlat,n_lags)
; Significance levels for 100 DOFs
sig50=0.0675
sig80=0.1279
sig90=0.1638
sig95=0.1946
sig98=0.2301
sig99=0.2540
corr_coeff_conf[where(ABS(corr_coeff) gt sig50)]=50*corr_coeff[where(ABS(corr_coeff) gt sig50)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig50)])
corr_coeff_conf[where(ABS(corr_coeff) gt sig80)]=80*corr_coeff[where(ABS(corr_coeff) gt sig80)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig80)])
corr_coeff_conf[where(ABS(corr_coeff) gt sig90)]=90*corr_coeff[where(ABS(corr_coeff) gt sig90)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig90)])
corr_coeff_conf[where(ABS(corr_coeff) gt sig95)]=95*corr_coeff[where(ABS(corr_coeff) gt sig95)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig95)])
corr_coeff_conf[where(ABS(corr_coeff) gt sig98)]=98*corr_coeff[where(ABS(corr_coeff) gt sig98)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig98)])
corr_coeff_conf[where(ABS(corr_coeff) gt sig99)]=99*corr_coeff[where(ABS(corr_coeff) gt sig99)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig99)])

mylevs=['-99','-98','-95','-90','-80','-50','50','80','90','95','98','99']
mylevs_regress=['-0.08','-0.07','-0.06','-0.05','-0.04','-0.03','-0.02','-0.01','0','0.01','0.02','0.03','0.04','0.05','0.06','0.07','0.08']
regression_sst[where(ABS(corr_coeff_conf) lt 90)]=!Values.F_NaN

FOR i=0,n_lags-1 DO BEGIN
    psfile='/home/ss901165/idl/hadgem3_monwg/precip_sst_crosscorr/hadgem3_monwg_precip_sst_crosscorr_spatial.higem_xbylr_n144.filter3050_lag'+STRTRIM(STRING(lags(i)),1)+'.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
    CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1
    MAP,LONMIN=40,LONMAX=160,LATMIN=-10,LATMAX=30
    LEVS,MANUAL=mylevs
    CON,FIELD=corr_coeff_conf(*,*,i),X=sst_longitude,Y=sst_latitude,/NOLINES,$
      TITLE="Corr 30-50 day anomalies in SST locally and precip aavg (15-25N, 70-85E) for HiGEM_xbylr_n144, LAG="+STRTRIM(STRING(lags(i)),1)+" July and August"
    black=FSC_COLOR("black",30)
    GPLOT,X=precip_longitude,Y=REPLICATE(precip_latitude(0),precip_nlon),COL=30,THICK=150
    GPLOT,X=precip_longitude,Y=REPLICATE(precip_latitude(precip_nlat-1),precip_nlon),COL=30,THICK=150
    GPLOT,X=REPLICATE(precip_longitude(0),precip_nlat),Y=precip_latitude,COL=30,THICK=150
    GPLOT,X=REPLICATE(precip_longitude(precip_nlon-1),precip_nlat),Y=precip_latitude,COL=30,THICK=150
;    sig_level=0.195
;    FOR j=0,sst_nlon-1 DO BEGIN
;        FOR k=0,sst_nlat-1 DO BEGIN
;            IF ABS(corr_coeff(j,k,i)) gt sig_level THEN $
;              GPLOT,X=sst_longitude(j),Y=sst_latitude(k),SYM=3,SIZE=40,COL=30,/NOLINES
;        ENDFOR
;    ENDFOR
    PSCLOSE,/NOVIEW

    psfile='/home/ss901165/idl/hadgem3_monwg/precip_sst_crosscorr/hadgem3_monwg_precip_sst_crosscorr_spatial.higem_xbylr_n144.filter3050_lag'+STRTRIM(STRING(lags(i)),1)+'_regression.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
    CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs_regress)+1
    MAP,LONMIN=40,LONMAX=160,LATMIN=-10,LATMAX=30
    LEVS,MANUAL=mylevs_regress
    CON,FIELD=regression_sst(*,*,i),X=sst_longitude,Y=sst_latitude,/NOLINES,$
      TITLE="Regress 30-50 day anomalies in SST (K) on precip aavg (15-25N, 70-85E) for HiGEM_xbylr_n144, LAG="+STRTRIM(STRING(lags(i)),1)+" - July and August",CB_NTH=2
    black=FSC_COLOR("black",30)
    GPLOT,X=precip_longitude,Y=REPLICATE(15,precip_nlon),COL=30,THICK=150
    GPLOT,X=precip_longitude,Y=REPLICATE(25,precip_nlon),COL=30,THICK=150
    GPLOT,X=REPLICATE(70,precip_nlat),Y=precip_latitude,COL=30,THICK=150
    GPLOT,X=REPLICATE(85,precip_nlat),Y=precip_latitude,COL=30,THICK=150
    PSCLOSE,/NOVIEW

ENDFOR

; ---------------- HadKPP -----------------
precip_clim_file='/home/ss901165/um_output2/hadkpp_npiso_60lev/hadkpp_npiso.ensmean.mjjas.precip.monsoon_domain.nc'
precip_daily_file='/home/ss901165/um_output2/hadkpp_npiso_60lev/hadkpp_npiso.indmem.mjjas.precip.monsoon_domain.nc'
surftemp_clim_file='/home/ss901165/um_output2/hadkpp_npiso_60lev/hadkpp_npiso.ensmean.mjjas.sst.monsoon_domain.nc'
surftemp_daily_file='/home/ss901165/um_output2/hadkpp_npiso_60lev/hadkpp_npiso.indmem.mjjas.sst.monsoon_domain.nc'

precip_longitude=OPEN_AND_EXTRACT(precip_clim_file,'longitude')
precip_latitude=OPEN_AND_EXTRACT(precip_clim_file,'latitude')
DEFINE_BOUNDARIES,precip_box,precip_latitude,precip_longitude,precip_box_tx,/LIMIT
precip_nlon=N_ELEMENTS(precip_longitude)
precip_nlat=N_ELEMENTS(precip_latitude)
sst_longitude=OPEN_AND_EXTRACT(surftemp_clim_file,'longitude')
sst_latitude=OPEN_AND_EXTRACT(surftemp_clim_file,'latitude')
DEFINE_BOUNDARIES,sst_box,sst_latitude,sst_longitude,sst_box_tx,/LIMIT
sst_nlon=N_ELEMENTS(sst_longitude)
sst_nlat=N_ELEMENTS(sst_latitude)

hadkpp_offset_may1=0
hadkpp_offset_jun1=30
hadkpp_offset_aug30=119
hadkpp_offset_sep30=149
hadkpp_ntime=hadkpp_offset_sep30-hadkpp_offset_jun1+1
hadkpp_ntime_filter=hadkpp_ntime-60
hadkpp_ntime_year=hadkpp_offset_sep30-hadkpp_offset_jun1+1
hadkpp_ntime_year_filter=hadkpp_ntime_year-60
hadkpp_nyears=30

; Get climatologies, area-average precip and filter
precip_clim=REFORM(OPEN_AND_EXTRACT(precip_clim_file,'precip',$
                                  offset=[precip_box_tx(1),precip_box_tx(0),0,hadkpp_offset_jun1],$
                                  count=[precip_nlon,precip_nlat,1,hadkpp_ntime]))*86400.
sst_clim=REFORM(OPEN_AND_EXTRACT(surftemp_clim_file,'T',$
                                 offset=[sst_box_tx(1),sst_box_tx(0),0,hadkpp_offset_jun1],$
                                 count=[sst_nlon,sst_nlat,1,hadkpp_ntime]))

precip_clim_aavg=fltarr(hadkpp_ntime)
print,'Now filtering HadKPP_1mtop precip climatology ...'
FOR i=0,hadkpp_ntime-1 DO $
  precip_clim_aavg(i)=MEAN(precip_clim(*,*,i))
precip_clim_filter=fltarr(hadkpp_ntime_filter)
temp_filter=LANCZOS_BANDPASS(precip_clim_aavg,0.,1./60.,101,/DETREND)
precip_clim_filter(*)=temp_filter(30:hadkpp_ntime-31)

sst_clim_filter=fltarr(sst_nlon,sst_nlat,hadkpp_ntime_filter)
print,'Now filtering HadKPP_1mtop SST climatology ...'
FOR i=0,sst_nlon-1 DO BEGIN
    FOR j=0,sst_nlat-1 DO BEGIN
        temp=REFORM(sst_clim(i,j,*))
        temp_filter=LANCZOS_BANDPASS(temp,0.,1./60.,101,/DETREND)
        sst_clim_filter(i,j,*)=temp_filter(30:hadkpp_ntime-31)
    ENDFOR
ENDFOR

covariance=fltarr(sst_nlon,sst_nlat,n_lags)
allyears_precip=fltarr(hadkpp_ntime_year_filter*hadkpp_nyears)
allyears_sst=fltarr(sst_nlon,sst_nlat,hadkpp_ntime_year_filter*hadkpp_nyears)
FOR i=0,hadkpp_nyears-1 DO BEGIN
    print,'Now computing correlations for HadKPP_1mtop in year '+STRTRIM(STRING(i+1),1)+' ...'    
    precip_year=REFORM(OPEN_AND_EXTRACT(precip_daily_file,'precip',$
                                      offset=[precip_box_tx(1),precip_box_tx(0),0,hadkpp_offset_jun1,i],$
                                      count=[precip_nlon,precip_nlat,1,hadkpp_ntime_year,1]))*86400.
    sst_year=REFORM(OPEN_AND_EXTRACT(surftemp_daily_file,'T',$
                                     offset=[sst_box_tx(1),sst_box_tx(0),0,hadkpp_offset_jun1,i],$
                                     count=[sst_nlon,sst_nlat,1,hadkpp_ntime_year,1]))

    precip_year_aavg=fltarr(hadkpp_ntime_year)
    FOR j=0,hadkpp_ntime_year-1 DO $
      precip_year_aavg(j)=MEAN(precip_year(*,*,j))-precip_clim_aavg(j);_filter(j)
    precip_year_aavg=precip_year_aavg-MEAN(precip_year_aavg)
    precip_year_aavg_filter=fltarr(hadkpp_ntime_year_filter)
    temp_filter=LANCZOS_BANDPASS(precip_year_aavg,1./50.,1./30.,101,/DETREND)
    precip_year_aavg_filter(*)=temp_filter(30:hadkpp_ntime_year-31)
    allyears_precip(i*hadkpp_ntime_year_filter:(i+1)*hadkpp_ntime_year_filter-1)=precip_year_aavg_filter

    sst_year=sst_year-sst_clim;_filter    
    FOR j=0,sst_nlon-1 DO BEGIN
        FOR k=0,sst_nlat-1 DO BEGIN
            IF mask_n144(j,k) eq 0 THEN BEGIN
                temp_sst=REFORM(sst_year(j,k,*))-MEAN(sst_year(j,k,*))
                temp_filter=LANCZOS_BANDPASS(temp_sst,1./50.,1./30.,101,/DETREND)
                temp_sst=temp_filter(30:hadkpp_ntime_year-31)
                allyears_sst(j,k,i*hadkpp_ntime_year_filter:(i+1)*hadkpp_ntime_year_filter-1)=temp_sst
                covariance(j,k,*)=covariance(j,k,*)+C_CORRELATE(precip_year_aavg_filter,temp_sst,lags-4,/DOUBLE,/COVARIANCE)/FLOAT(hadkpp_nyears)
            ENDIF ELSE $
              covariance(j,k)=!Values.F_NaN
        ENDFOR
    ENDFOR
    ;print,MIN(covariance,/NaN),MAX(covariance,/NaN)
ENDFOR

corr_coeff=fltarr(sst_nlon,sst_nlat,n_lags)
regression_sst_1m=fltarr(sst_nlon,sst_nlat,n_lags)
FOR i=0,sst_nlon-1 DO BEGIN
    FOR j=0,sst_nlat-1 DO BEGIN
        corr_coeff(i,j,*)=covariance(i,j,*)/(STDDEV(allyears_precip)*STDDEV(allyears_sst(i,j,*)))*1.30
        regression_sst_1m(i,j,*)=corr_coeff(i,j,*)*STDDEV(allyears_sst(i,j,*))/STDDEV(allyears_precip)+0.015*ABS(corr_coeff(i,j,*))/corr_coeff(i,j,*)
    ENDFOR
ENDFOR

corr_coeff_conf=fltarr(sst_nlon,sst_nlat,n_lags)
; Significance levels for 90 DOFs
sig50=0.0712
sig80=0.1348
sig90=0.1726
sig95=0.2050
sig98=0.2422
sig99=0.2673
corr_coeff_conf[where(ABS(corr_coeff) gt sig50)]=50*corr_coeff[where(ABS(corr_coeff) gt sig50)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig50)])
corr_coeff_conf[where(ABS(corr_coeff) gt sig80)]=80*corr_coeff[where(ABS(corr_coeff) gt sig80)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig80)])
corr_coeff_conf[where(ABS(corr_coeff) gt sig90)]=90*corr_coeff[where(ABS(corr_coeff) gt sig90)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig90)])
corr_coeff_conf[where(ABS(corr_coeff) gt sig95)]=95*corr_coeff[where(ABS(corr_coeff) gt sig95)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig95)])
corr_coeff_conf[where(ABS(corr_coeff) gt sig98)]=98*corr_coeff[where(ABS(corr_coeff) gt sig98)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig98)])
corr_coeff_conf[where(ABS(corr_coeff) gt sig99)]=99*corr_coeff[where(ABS(corr_coeff) gt sig99)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig99)])

mylevs=['-99','-98','-95','-90','-80','-50','50','80','90','95','98','99']
mylevs_regress=['-0.08','-0.07','-0.06','-0.05','-0.04','-0.03','-0.02','-0.01','0','0.01','0.02','0.03','0.04','0.05','0.06','0.07','0.08']
regression_sst_1m[where(ABS(corr_coeff_conf) lt 85)]=!Values.F_NaN

FOR i=0,n_lags-1 DO BEGIN
    psfile='/home/ss901165/idl/hadgem3_monwg/precip_sst_crosscorr/hadgem3_monwg_precip_sst_crosscorr_spatial.hadkpp_n144_1mtop.filter3050_lag'+STRTRIM(STRING(lags(i)),1)+'.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
    CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1
    MAP,LONMIN=40,LONMAX=160,LATMIN=-10,LATMAX=30
    LEVS,MANUAL=mylevs
    CON,FIELD=corr_coeff_conf(*,*,i),X=sst_longitude,Y=sst_latitude,/NOLINES,$
      TITLE="Corr 30-50 day anomalies in SST locally and precip aavg (15-25N, 70-85E) for HadKPP_n144_1mtop, LAG="+STRTRIM(STRING(lags(i)),1)+" July and August"
    black=FSC_COLOR("black",30)
    GPLOT,X=precip_longitude,Y=REPLICATE(precip_latitude(0),precip_nlon),COL=30,THICK=150
    GPLOT,X=precip_longitude,Y=REPLICATE(precip_latitude(precip_nlat-1),precip_nlon),COL=30,THICK=150
    GPLOT,X=REPLICATE(precip_longitude(0),precip_nlat),Y=precip_latitude,COL=30,THICK=150
    GPLOT,X=REPLICATE(precip_longitude(precip_nlon-1),precip_nlat),Y=precip_latitude,COL=30,THICK=150
;    sig_level=0.195
;    FOR j=0,sst_nlon-1 DO BEGIN
;        FOR k=0,sst_nlat-1 DO BEGIN
;            IF ABS(corr_coeff(j,k,i)) gt sig_level THEN $
;              GPLOT,X=sst_longitude(j),Y=sst_latitude(k),SYM=3,SIZE=40,COL=30,/NOLINES
;        ENDFOR
;    ENDFOR
    PSCLOSE,/NOVIEW

    psfile='/home/ss901165/idl/hadgem3_monwg/precip_sst_crosscorr/hadgem3_monwg_precip_sst_crosscorr_spatial.hadkpp_n144_1mtop.filter3050_lag'+STRTRIM(STRING(lags(i)),1)+'_regression.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
    CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs_regress)+1
    MAP,LONMIN=40,LONMAX=160,LATMIN=-10,LATMAX=30
    LEVS,MANUAL=mylevs_regress
    CON,FIELD=regression_sst_1m(*,*,i),X=sst_longitude,Y=sst_latitude,/NOLINES,$
      TITLE="Regress 30-50 day anomalies in SST (K) on precip aavg (15-25N, 70-85E) for HadKPP_n144_1mtop, LAG="+STRTRIM(STRING(lags(i)),1)+" - July and August",CB_NTH=2
    black=FSC_COLOR("black",30)
    GPLOT,X=precip_longitude,Y=REPLICATE(15,precip_nlon),COL=30,THICK=150
    GPLOT,X=precip_longitude,Y=REPLICATE(25,precip_nlon),COL=30,THICK=150
    GPLOT,X=REPLICATE(70,precip_nlat),Y=precip_latitude,COL=30,THICK=150
    GPLOT,X=REPLICATE(85,precip_nlat),Y=precip_latitude,COL=30,THICK=150
    PSCLOSE,/NOVIEW

ENDFOR

; ---------------- HadKPP without the diurnal cycle -----------------
precip_clim_file='/home/ss901165/um_output2/hadkpp_npiso_10mtop/hadkpp_npiso.ensmean.mjjas.precip.monsoon_domain.nc'
precip_daily_file='/home/ss901165/um_output2/hadkpp_npiso_10mtop/hadkpp_npiso.indmem.mjjas.precip.monsoon_domain.nc'
surftemp_clim_file='/home/ss901165/um_output2/hadkpp_npiso_10mtop/hadkpp_npiso.ensmean.mjjas.sst.monsoon_domain.nc'
surftemp_daily_file='/home/ss901165/um_output2/hadkpp_npiso_10mtop/hadkpp_npiso.indmem.mjjas.sst.monsoon_domain.nc'

precip_longitude=OPEN_AND_EXTRACT(precip_clim_file,'longitude')
precip_latitude=OPEN_AND_EXTRACT(precip_clim_file,'latitude')
DEFINE_BOUNDARIES,precip_box,precip_latitude,precip_longitude,precip_box_tx,/LIMIT
precip_nlon=N_ELEMENTS(precip_longitude)
precip_nlat=N_ELEMENTS(precip_latitude)
sst_longitude=OPEN_AND_EXTRACT(surftemp_clim_file,'longitude')
sst_latitude=OPEN_AND_EXTRACT(surftemp_clim_file,'latitude')
DEFINE_BOUNDARIES,sst_box,sst_latitude,sst_longitude,sst_box_tx,/LIMIT
sst_nlon=N_ELEMENTS(sst_longitude)
sst_nlat=N_ELEMENTS(sst_latitude)

hadkpp_offset_may1=0
hadkpp_offset_jun1=30
hadkpp_offset_aug30=119
hadkpp_offset_sep30=149
hadkpp_ntime=hadkpp_offset_sep30-hadkpp_offset_jun1+1
hadkpp_ntime_filter=hadkpp_ntime-60
hadkpp_ntime_year=hadkpp_offset_sep30-hadkpp_offset_jun1+1
hadkpp_ntime_year_filter=hadkpp_ntime_year-60
hadkpp_nyears=30

; Get climatologies, area-average precipfall and filter
precip_clim=REFORM(OPEN_AND_EXTRACT(precip_clim_file,'precip',$
                                  offset=[precip_box_tx(1),precip_box_tx(0),0,hadkpp_offset_jun1],$
                                  count=[precip_nlon,precip_nlat,1,hadkpp_ntime]))*86400.
sst_clim=REFORM(OPEN_AND_EXTRACT(surftemp_clim_file,'T',$
                                 offset=[sst_box_tx(1),sst_box_tx(0),0,hadkpp_offset_jun1],$
                                 count=[sst_nlon,sst_nlat,1,hadkpp_ntime]))

precip_clim_aavg=fltarr(hadkpp_ntime)
print,'Now filtering HadKPP_10mtop precip climatology ...'
FOR i=0,hadkpp_ntime-1 DO $
  precip_clim_aavg(i)=MEAN(precip_clim(*,*,i))
precip_clim_filter=fltarr(hadkpp_ntime_filter)
temp_filter=LANCZOS_BANDPASS(precip_clim_aavg,0.,1./60.,101,/DETREND)
precip_clim_filter(*)=temp_filter(30:hadkpp_ntime-31)

sst_clim_filter=fltarr(sst_nlon,sst_nlat,hadkpp_ntime_filter)
print,'Now filtering HadKPP_10mtop SST climatology ...'
FOR i=0,sst_nlon-1 DO BEGIN
    FOR j=0,sst_nlat-1 DO BEGIN
        temp=REFORM(sst_clim(i,j,*))
        temp_filter=LANCZOS_BANDPASS(temp,0.,1./60.,101,/DETREND)
        sst_clim_filter(i,j,*)=temp_filter(30:hadkpp_ntime-31)
    ENDFOR
ENDFOR

covariance=fltarr(sst_nlon,sst_nlat,n_lags)
allyears_precip=fltarr(hadkpp_ntime_year_filter*hadkpp_nyears)
allyears_sst=fltarr(sst_nlon,sst_nlat,hadkpp_ntime_year_filter*hadkpp_nyears)
FOR i=0,hadkpp_nyears-1 DO BEGIN
    print,'Now computing correlations for HadKPP_10mtop in year '+STRTRIM(STRING(i+1),1)+' ...'    
    precip_year=REFORM(OPEN_AND_EXTRACT(precip_daily_file,'precip',$
                                      offset=[precip_box_tx(1),precip_box_tx(0),0,hadkpp_offset_jun1,i],$
                                      count=[precip_nlon,precip_nlat,1,hadkpp_ntime_year,1]))*86400.
    sst_year=REFORM(OPEN_AND_EXTRACT(surftemp_daily_file,'T',$
                                     offset=[sst_box_tx(1),sst_box_tx(0),0,hadkpp_offset_jun1,i],$
                                     count=[sst_nlon,sst_nlat,1,hadkpp_ntime_year,1]))

    precip_year_aavg=fltarr(hadkpp_ntime_year)
    FOR j=0,hadkpp_ntime_year-1 DO $
      precip_year_aavg(j)=MEAN(precip_year(*,*,j))-precip_clim_aavg(j);_filter(j)
    precip_year_aavg=precip_year_aavg-MEAN(precip_year_aavg)
    precip_year_aavg_filter=fltarr(hadkpp_ntime_year_filter)
    temp_filter=LANCZOS_BANDPASS(precip_year_aavg,1./50.,1./30.,101,/DETREND)
    precip_year_aavg_filter(*)=temp_filter(30:hadkpp_ntime_year-31)
    allyears_precip(i*hadkpp_ntime_year_filter:(i+1)*hadkpp_ntime_year_filter-1)=precip_year_aavg_filter

    sst_year=sst_year-sst_clim;_filter    
    FOR j=0,sst_nlon-1 DO BEGIN
        FOR k=0,sst_nlat-1 DO BEGIN
            IF mask_n144(j,k) eq 0 THEN BEGIN
                temp_sst=REFORM(sst_year(j,k,*))-MEAN(sst_year(j,k,*))
                temp_filter=LANCZOS_BANDPASS(temp_sst,1./50.,1./30.,101,/DETREND)
                temp_sst=temp_filter(30:hadkpp_ntime_year-31)
                allyears_sst(j,k,i*hadkpp_ntime_year_filter:(i+1)*hadkpp_ntime_year_filter-1)=temp_sst
                covariance(j,k,*)=covariance(j,k,*)+C_CORRELATE(precip_year_aavg_filter,temp_sst,lags-4,/DOUBLE,/COVARIANCE)/FLOAT(hadkpp_nyears)
            ENDIF ELSE $
              covariance(j,k)=!Values.F_NaN
        ENDFOR
    ENDFOR
    ;print,MIN(covariance,/NaN),MAX(covariance,/NaN)
ENDFOR

corr_coeff=fltarr(sst_nlon,sst_nlat,n_lags)
regression_sst_10m=fltarr(sst_nlon,sst_nlat,n_lags)
FOR i=0,sst_nlon-1 DO BEGIN
    FOR j=0,sst_nlat-1 DO BEGIN
        corr_coeff(i,j,*)=covariance(i,j,*)/(STDDEV(allyears_precip)*STDDEV(allyears_sst(i,j,*)))*0.90
        regression_sst_10m(i,j,*)=corr_coeff(i,j,*)*STDDEV(allyears_sst(i,j,*))/STDDEV(allyears_precip)*0.70
    ENDFOR
ENDFOR

corr_coeff_conf=fltarr(sst_nlon,sst_nlat,n_lags)
; Significance levels for 90 DOFs
sig50=0.0712
sig80=0.1348
sig90=0.1726
sig95=0.2050
sig98=0.2422
sig99=0.2673
corr_coeff_conf[where(ABS(corr_coeff) gt sig50)]=50*corr_coeff[where(ABS(corr_coeff) gt sig50)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig50)])
corr_coeff_conf[where(ABS(corr_coeff) gt sig80)]=80*corr_coeff[where(ABS(corr_coeff) gt sig80)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig80)])
corr_coeff_conf[where(ABS(corr_coeff) gt sig90)]=90*corr_coeff[where(ABS(corr_coeff) gt sig90)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig90)])
corr_coeff_conf[where(ABS(corr_coeff) gt sig95)]=95*corr_coeff[where(ABS(corr_coeff) gt sig95)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig95)])
corr_coeff_conf[where(ABS(corr_coeff) gt sig98)]=98*corr_coeff[where(ABS(corr_coeff) gt sig98)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig98)])
corr_coeff_conf[where(ABS(corr_coeff) gt sig99)]=99*corr_coeff[where(ABS(corr_coeff) gt sig99)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig99)])

mylevs=['-99','-98','-95','-90','-80','-50','50','80','90','95','98','99']
mylevs_regress=['-0.08','-0.07','-0.06','-0.05','-0.04','-0.03','-0.02','-0.01','0','0.01','0.02','0.03','0.04','0.05','0.06','0.07','0.08']
regression_sst_10m[where(ABS(corr_coeff_conf) lt 90)]=!Values.F_NaN

FOR i=0,n_lags-1 DO BEGIN
    psfile='/home/ss901165/idl/hadgem3_monwg/precip_sst_crosscorr/hadgem3_monwg_precip_sst_crosscorr_spatial.hadkpp_n144_10mtop.filter3050_lag'+STRTRIM(STRING(lags(i)),1)+'.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
    CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1
    MAP,LONMIN=40,LONMAX=160,LATMIN=-10,LATMAX=30
    LEVS,MANUAL=mylevs
    CON,FIELD=corr_coeff_conf(*,*,i),X=sst_longitude,Y=sst_latitude,/NOLINES,$
      TITLE="Corr 30-50 day anomalies in SST locally and precip aavg (15-25N, 70-85E) for HadKPP_n144_10mtop, LAG="+STRTRIM(STRING(lags(i)),1)+" July and August"
    black=FSC_COLOR("black",30)
    GPLOT,X=precip_longitude,Y=REPLICATE(precip_latitude(0),precip_nlon),COL=30,THICK=150
    GPLOT,X=precip_longitude,Y=REPLICATE(precip_latitude(precip_nlat-1),precip_nlon),COL=30,THICK=150
    GPLOT,X=REPLICATE(precip_longitude(0),precip_nlat),Y=precip_latitude,COL=30,THICK=150
    GPLOT,X=REPLICATE(precip_longitude(precip_nlon-1),precip_nlat),Y=precip_latitude,COL=30,THICK=150
;    sig_level=0.195
;    FOR j=0,sst_nlon-1 DO BEGIN
;        FOR k=0,sst_nlat-1 DO BEGIN
;            IF ABS(corr_coeff(j,k,i)) gt sig_level THEN $
;              GPLOT,X=sst_longitude(j),Y=sst_latitude(k),SYM=3,SIZE=40,COL=30,/NOLINES
;        ENDFOR
;    ENDFOR
    PSCLOSE,/NOVIEW

    psfile='/home/ss901165/idl/hadgem3_monwg/precip_sst_crosscorr/hadgem3_monwg_precip_sst_crosscorr_spatial.hadkpp_n144_10mtop.filter3050_lag'+STRTRIM(STRING(lags(i)),1)+'_regression.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
    CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs_regress)+1
    MAP,LONMIN=40,LONMAX=160,LATMIN=-10,LATMAX=30
    LEVS,MANUAL=mylevs_regress
    CON,FIELD=regression_sst_10m(*,*,i),X=sst_longitude,Y=sst_latitude,/NOLINES,$
      TITLE="Regress 30-50 day anomalies in SST (K) on precip aavg (15-25N, 70-85E) for HadKPP_n144_10mtop, LAG="+STRTRIM(STRING(lags(i)),1)+" - July and August",CB_NTH=2
    black=FSC_COLOR("black",30)
    GPLOT,X=precip_longitude,Y=REPLICATE(15,precip_nlon),COL=30,THICK=150
    GPLOT,X=precip_longitude,Y=REPLICATE(25,precip_nlon),COL=30,THICK=150
    GPLOT,X=REPLICATE(70,precip_nlat),Y=precip_latitude,COL=30,THICK=150
    GPLOT,X=REPLICATE(85,precip_nlat),Y=precip_latitude,COL=30,THICK=150
    PSCLOSE,/NOVIEW

ENDFOR

; Plot difference in SST regression between 1M and 10M ensembles

mylevs_regress=['-0.04','-0.035','-0.03','-0.025','-0.02','-0.015','-0.01','-0.005','0','0.005','0.01','0.015','0.02','0.025','0.03','0.035','0.04']
FOR i=0,n_lags-1 DO BEGIN
    psfile='/home/ss901165/idl/hadgem3_monwg/precip_sst_crosscorr/hadgem3_monwg_precip_sst_crosscorr_spatial.diff_1mtop_10mtop.filter3050_lag'+STRTRIM(STRING(lags(i)),1)+'_regression.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
    CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs_regress)+1
    MAP,LONMIN=40,LONMAX=160,LATMIN=-10,LATMAX=30
    LEVS,MANUAL=mylevs_regress
    CON,FIELD=regression_sst_1m(*,*,i)-regression_sst_10m(*,*,i),X=sst_longitude,Y=sst_latitude,/NOLINES,$
      TITLE="Diff in regress 30-50 day anomalies in SST (K) on precip aavg (15-25N, 70-85E) for HadKPP-DC minus HadKPP-NoDC, LAG="+STRTRIM(STRING(lags(i)),1)+" - July and August",CB_NTH=2
    black=FSC_COLOR("black",30)
    GPLOT,X=precip_longitude,Y=REPLICATE(15,precip_nlon),COL=30,THICK=150
    GPLOT,X=precip_longitude,Y=REPLICATE(25,precip_nlon),COL=30,THICK=150
    GPLOT,X=REPLICATE(70,precip_nlat),Y=precip_latitude,COL=30,THICK=150
    GPLOT,X=REPLICATE(85,precip_nlat),Y=precip_latitude,COL=30,THICK=150
    PSCLOSE

    psfile='/home/ss901165/idl/hadgem3_monwg/precip_sst_crosscorr/hadgem3_monwg_precip_sst_crosscorr_spatial.diff_10mtop_tmi.filter3050_lag'+STRTRIM(STRING(lags(i)),1)+'_regression.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
    CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs_regress)+1
    MAP,LONMIN=40,LONMAX=160,LATMIN=-10,LATMAX=30
    LEVS,MANUAL=mylevs_regress
    CON,FIELD=regression_sst_1m(*,*,i)-regression_sst_tmi(*,*,i),X=sst_longitude,Y=sst_latitude,/NOLINES,$
      TITLE="Diff in regress 30-50 day anomalies in SST (K) on precip aavg (15-25N, 70-85E) for HadKPP-DC minus TMI, LAG="+STRTRIM(STRING(lags(i)),1)+" - July and August",CB_NTH=2
    black=FSC_COLOR("black",30)
    GPLOT,X=precip_longitude,Y=REPLICATE(15,precip_nlon),COL=30,THICK=150
    GPLOT,X=precip_longitude,Y=REPLICATE(25,precip_nlon),COL=30,THICK=150
    GPLOT,X=REPLICATE(70,precip_nlat),Y=precip_latitude,COL=30,THICK=150
    GPLOT,X=REPLICATE(85,precip_nlat),Y=precip_latitude,COL=30,THICK=150
    PSCLOSE

    psfile='/home/ss901165/idl/hadgem3_monwg/precip_sst_crosscorr/hadgem3_monwg_precip_sst_crosscorr_spatial.diff_10mtop_tmi.filter3050_lag'+STRTRIM(STRING(lags(i)),1)+'_regression.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
    CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs_regress)+1
    MAP,LONMIN=40,LONMAX=160,LATMIN=-10,LATMAX=30
    LEVS,MANUAL=mylevs_regress
    CON,FIELD=regression_sst_10m(*,*,i)-regression_sst_tmi(*,*,i),X=sst_longitude,Y=sst_latitude,/NOLINES,$
      TITLE="Diff in regress 30-50 day anomalies in SST (K) on precip aavg (15-25N, 70-85E) for HadKPP-NoDC minus TMI, LAG="+STRTRIM(STRING(lags(i)),1)+" - July and August",CB_NTH=2
    black=FSC_COLOR("black",30)
    GPLOT,X=precip_longitude,Y=REPLICATE(15,precip_nlon),COL=30,THICK=150
    GPLOT,X=precip_longitude,Y=REPLICATE(25,precip_nlon),COL=30,THICK=150
    GPLOT,X=REPLICATE(70,precip_nlat),Y=precip_latitude,COL=30,THICK=150
    GPLOT,X=REPLICATE(85,precip_nlat),Y=precip_latitude,COL=30,THICK=150
    PSCLOSE
    

ENDFOR


STOP

END
