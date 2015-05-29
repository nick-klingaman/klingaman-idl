PRO hadgem3_monwg_precip_crosscorr_spatial

; ------------ Observations (TMI and GPCP) --------------
gpcp_basedir='/home/ss901165/datasets/GPCP/one_degree'
gpcp_clim_file=gpcp_basedir+'/gpcp1dd.1997-2007.mean.n144.nc'
obs_start_year=1997
obs_stop_year=2007
obs_nyears=obs_stop_year-obs_start_year+1

precip_aavg_box=[15,70,25,85]
gpcp_gridfile=gpcp_basedir+'/gpcp1dd.1998.n144.nc'
aavg_longitude=OPEN_AND_EXTRACT(gpcp_gridfile,'longitude')
aavg_latitude=OPEN_AND_EXTRACT(gpcp_gridfile,'latitude')
DEFINE_BOUNDARIES,precip_aavg_box,aavg_latitude,aavg_longitude,aavg_box_tx,/LIMIT
aavg_nlon=N_ELEMENTS(aavg_longitude)
aavg_nlat=N_ELEMENTS(aavg_latitude)

grid_box=[-10,40,30,162]
grid_longitude=OPEN_AND_EXTRACT(gpcp_gridfile,'longitude')
grid_latitude=OPEN_AND_EXTRACT(gpcp_gridfile,'latitude')
DEFINE_BOUNDARIES,grid_box,grid_latitude,grid_longitude,grid_box_tx,/LIMIT
grid_nlon=N_ELEMENTS(grid_longitude)
grid_nlat=N_ELEMENTS(grid_latitude)

gpcp_offset_may1=120
gpcp_offset_jun1=150
gpcp_offset_sep30=271
gpcp_offset_oct31=302
gpcp_ntime=gpcp_offset_oct31-gpcp_offset_jun1+1
gpcp_ntime_filter=gpcp_ntime-61
gpcp_ntime_year=gpcp_offset_sep30-gpcp_offset_jun1+1
gpcp_ntime_year_filter=gpcp_ntime_year-60

; Get climatologies, area-average rainfall and filter
aavg_clim=REFORM(OPEN_AND_EXTRACT(gpcp_clim_file,'precip',$
                                  offset=[aavg_box_tx(1),aavg_box_tx(0),gpcp_offset_jun1],$
                                  count=[aavg_nlon,aavg_nlat,gpcp_ntime]))
grid_clim=REFORM(OPEN_AND_EXTRACT(gpcp_clim_file,'precip',$
                                  offset=[grid_box_tx(1),grid_box_tx(0),gpcp_offset_jun1],$
                                  count=[grid_nlon,grid_nlat,gpcp_ntime]))

aavg_clim_aavg=fltarr(gpcp_ntime)
print,'Now filtering area-averaged climatology ...'
FOR i=0,gpcp_ntime-1 DO $
  aavg_clim_aavg(i)=MEAN(aavg_clim(*,*,i))
aavg_clim_filter=fltarr(gpcp_ntime_filter)
temp_filter=LANCZOS_BANDPASS(aavg_clim_aavg,0.,1./60.,101,/DETREND)
aavg_clim_filter(*)=temp_filter(30:gpcp_ntime-32)

grid_clim_filter=fltarr(grid_nlon,grid_nlat,gpcp_ntime_filter)
print,'Now filtering climatology at each grid point ...'
FOR i=0,grid_nlon-1 DO BEGIN
    FOR j=0,grid_nlat-1 DO BEGIN
        temp=REFORM(grid_clim(i,j,*))
        temp_filter=LANCZOS_BANDPASS(temp,0.,1./60.,101,/DETREND)
        grid_clim_filter(i,j,*)=temp_filter(30:gpcp_ntime-32)
    ENDFOR
ENDFOR

lags=[-12,-8,-4,0,4,8,12]
n_lags=N_ELEMENTS(lags)
covariance=fltarr(grid_nlon,grid_nlat,n_lags)
allyears_aavg=fltarr(gpcp_ntime_year_filter*obs_nyears)
allyears_grid=fltarr(grid_nlon,grid_nlat,gpcp_ntime_year_filter*obs_nyears)
FOR i=0,obs_nyears-1 DO BEGIN
    this_year_str=STRTRIM(STRING(obs_start_year+i),1)
    print,'Now computing correlations for observations in '+this_year_str+' ...'
    gpcp_file=gpcp_basedir+'/gpcp1dd.'+this_year_str+'.n144.nc'
        
    aavg_year=REFORM(OPEN_AND_EXTRACT(gpcp_file,'precip',$
                                      offset=[aavg_box_tx(1),aavg_box_tx(0),gpcp_offset_jun1],$
                                      count=[aavg_nlon,aavg_nlat,gpcp_ntime_year]))
    grid_year=REFORM(OPEN_AND_EXTRACT(gpcp_file,'precip',$
                                     offset=[grid_box_tx(1),grid_box_tx(0),gpcp_offset_jun1],$
                                     count=[grid_nlon,grid_nlat,gpcp_ntime_year]))

    aavg_year_aavg=fltarr(gpcp_ntime_year)
    FOR j=0,gpcp_ntime_year-1 DO $
      aavg_year_aavg(j)=MEAN(aavg_year(*,*,j))-aavg_clim_aavg(j);_filter(j)
    aavg_year_aavg=aavg_year_aavg-MEAN(aavg_year_aavg)
    aavg_year_aavg_filter=fltarr(gpcp_ntime_year_filter)
    temp_filter=LANCZOS_BANDPASS(aavg_year_aavg,1./50.,1./30.,101,/DETREND)
    aavg_year_aavg_filter(*)=temp_filter(30:gpcp_ntime_year-31)
    allyears_aavg(i*gpcp_ntime_year_filter:(i+1)*gpcp_ntime_year_filter-1)=aavg_year_aavg_filter

    grid_year=grid_year-grid_clim;_filter    
    FOR j=0,grid_nlon-1 DO BEGIN
        FOR k=0,grid_nlat-1 DO BEGIN            
            temp_grid=REFORM(grid_year(j,k,*))-MEAN(grid_year(j,k,*))
            temp_filter=LANCZOS_BANDPASS(temp_grid,1./50.,1./30.,101,/DETREND)
            temp_grid=temp_filter(30:gpcp_ntime_year-31)
            allyears_grid(j,k,i*gpcp_ntime_year_filter:(i+1)*gpcp_ntime_year_filter-1)=temp_grid
            covariance(j,k,*)=covariance(j,k,*)+C_CORRELATE(aavg_year_aavg_filter,temp_grid,lags,/DOUBLE,/COVARIANCE)/FLOAT(obs_nyears)            
        ENDFOR
    ENDFOR
ENDFOR

corr_coeff=fltarr(grid_nlon,grid_nlat,n_lags)
regression_gpcp=fltarr(grid_nlon,grid_nlat,n_lags)
FOR i=0,grid_nlon-1 DO BEGIN
    FOR j=0,grid_nlat-1 DO BEGIN
        corr_coeff(i,j,*)=covariance(i,j,*)/(STDDEV(allyears_aavg)*STDDEV(allyears_grid(i,j,*)))
        regression_gpcp(i,j,*)=corr_coeff(i,j,*)*STDDEV(allyears_grid(i,j,*))/STDDEV(allyears_aavg)
    ENDFOR
ENDFOR
        
corr_coeff_conf=fltarr(grid_nlon,grid_nlat,n_lags)
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
mylevs_regress=['-2.7','-2.1','-1.5','-1.2','-0.9','-0.6','-0.3','0','0.3','0.6','0.9','1.2','1.5','2.1','2.7']

regression_gpcp[where(ABS(corr_coeff_conf) lt 80)]=!Values.F_NaN

FOR i=0,n_lags-1 DO BEGIN
    psfile='/home/ss901165/idl/hadgem3_monwg/precip_crosscorr/hadgem3_monwg_precip_crosscorr_spatial.gpcp_n144.filter3050_lag'+STRTRIM(STRING(lags(i)),1)+'.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100,CB_WIDTH=112
    CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs_corr)+1
    MAP,LONMIN=40,LONMAX=160,LATMIN=-10,LATMAX=30
    LEVS,MANUAL=mylevs_corr
    CON,FIELD=corr_coeff_conf(*,*,i),X=grid_longitude,Y=grid_latitude,/NOLINES,$
      TITLE="Corr for 30-50 day anomalies in GPCP locally and GPCP aavg (15-25N, 70-85E) for 1998-2007, LAG="+STRTRIM(STRING(lags(i)),1)+" - July and August"
    black=FSC_COLOR("black",30)
    GPLOT,X=aavg_longitude,Y=REPLICATE(15,aavg_nlon),COL=30,THICK=150
    GPLOT,X=aavg_longitude,Y=REPLICATE(25,aavg_nlon),COL=30,THICK=150
    GPLOT,X=REPLICATE(70,aavg_nlat),Y=aavg_latitude,COL=30,THICK=150
    GPLOT,X=REPLICATE(85,aavg_nlat),Y=aavg_latitude,COL=30,THICK=150    
    PSCLOSE,/NOVIEW

    psfile='/home/ss901165/idl/hadgem3_monwg/precip_crosscorr/hadgem3_monwg_precip_crosscorr_spatial.gpcp_n144.filter3050_lag'+STRTRIM(STRING(lags(i)),1)+'_regression.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
    CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs_regress)+1
    white=FSC_COLOR("white",N_ELEMENTS(mylevs_regress)/2+2)
    MAP,LONMIN=40,LONMAX=160,LATMIN=-10,LATMAX=30
    LEVS,MANUAL=mylevs_regress
    CON,FIELD=regression_gpcp(*,*,i),X=grid_longitude,Y=grid_latitude,/NOLINES,$
      TITLE="Regress 30-50 day anomalies in GPCP (mm/day) on GPCP aavg (15-25N, 70-85E) for 1998-2007, LAG="+STRTRIM(STRING(lags(i)),1)+" - July and August"
    black=FSC_COLOR("black",30)
    GPLOT,X=aavg_longitude,Y=REPLICATE(15,aavg_nlon),COL=30,THICK=150
    GPLOT,X=aavg_longitude,Y=REPLICATE(25,aavg_nlon),COL=30,THICK=150
    GPLOT,X=REPLICATE(70,aavg_nlat),Y=aavg_latitude,COL=30,THICK=150
    GPLOT,X=REPLICATE(85,aavg_nlat),Y=aavg_latitude,COL=30,THICK=150
    FOR j=0,grid_nlon-1 DO BEGIN
        FOR k=0,grid_nlat-1 DO BEGIN
            IF ABS(corr_coeff(j,k,i)) gt sig95 THEN $
              GPLOT,X=grid_longitude(j),Y=grid_latitude(k),SYM=3,SIZE=25,COL=30,/NOLINES
        ENDFOR
    ENDFOR
    PSCLOSE,/NOVIEW
ENDFOR

; ------------ HadGEM3-AO ------------
daily_file='/home/ss901165/um_output2/hadgem3_monwg/ahsaf/ahsaf.precip.daily_30years.nc'
clim_file='/home/ss901165/um_output2/hadgem3_monwg/ahsaf/ahsaf.precip.clim.daily_30years.nc'
hadgem3_nyears=30

precip_aavg_box=[15,70,25,85]
aavg_longitude=OPEN_AND_EXTRACT(clim_file,'longitude')
aavg_latitude=OPEN_AND_EXTRACT(clim_file,'latitude')
DEFINE_BOUNDARIES,precip_aavg_box,aavg_latitude,aavg_longitude,aavg_box_tx,/LIMIT
aavg_nlon=N_ELEMENTS(aavg_longitude)
aavg_nlat=N_ELEMENTS(aavg_latitude)

grid_box=[-10,40,30,162]
grid_longitude=OPEN_AND_EXTRACT(clim_file,'longitude')
grid_latitude=OPEN_AND_EXTRACT(clim_file,'latitude')
DEFINE_BOUNDARIES,grid_box,grid_latitude,grid_longitude,grid_box_tx,/LIMIT
grid_nlon=N_ELEMENTS(grid_longitude)
grid_nlat=N_ELEMENTS(grid_latitude)

hadgem3_offset_may1=120
hadgem3_offset_jun1=150
hadgem3_offset_sep30=269
hadgem3_offset_oct30=299
hadgem3_ntime=hadgem3_offset_oct30-hadgem3_offset_jun1+1
hadgem3_ntime_filter=hadgem3_ntime-61
hadgem3_ntime_year=hadgem3_offset_sep30-hadgem3_offset_jun1+1
hadgem3_ntime_year_filter=hadgem3_ntime_year-60

; Get climatologies, area-average rainfall and filter
aavg_clim=REFORM(OPEN_AND_EXTRACT(clim_file,'precip',$
                                  offset=[aavg_box_tx(1),aavg_box_tx(0),0,hadgem3_offset_jun1],$
                                  count=[aavg_nlon,aavg_nlat,1,hadgem3_ntime]))
grid_clim=REFORM(OPEN_AND_EXTRACT(clim_file,'precip',$
                                  offset=[grid_box_tx(1),grid_box_tx(0),0,hadgem3_offset_jun1],$
                                  count=[grid_nlon,grid_nlat,1,hadgem3_ntime]))

aavg_clim_aavg=fltarr(hadgem3_ntime)
print,'Now filtering area-averaged climatology for HadGEM3-AO ...'
FOR i=0,hadgem3_ntime-1 DO $
  aavg_clim_aavg(i)=MEAN(aavg_clim(*,*,i))
aavg_clim_filter=fltarr(hadgem3_ntime_filter)
temp_filter=LANCZOS_BANDPASS(aavg_clim_aavg,0.,1./60.,101,/DETREND)
aavg_clim_filter(*)=temp_filter(30:hadgem3_ntime-32)

grid_clim_filter=fltarr(grid_nlon,grid_nlat,hadgem3_ntime_filter)
print,'Now filtering climatology at each grid point for HadGEM3-AO ...'
FOR i=0,grid_nlon-1 DO BEGIN
    FOR j=0,grid_nlat-1 DO BEGIN
        temp=REFORM(grid_clim(i,j,*))
        temp_filter=LANCZOS_BANDPASS(temp,0.,1./60.,101,/DETREND)
        grid_clim_filter(i,j,*)=temp_filter(30:hadgem3_ntime-32)
    ENDFOR
ENDFOR

lags=[-12,-8,-4,0,4,8,12]
n_lags=N_ELEMENTS(lags)
covariance=fltarr(grid_nlon,grid_nlat,n_lags)
allyears_aavg=fltarr(hadgem3_ntime_year_filter*hadgem3_nyears)
allyears_grid=fltarr(grid_nlon,grid_nlat,hadgem3_ntime_year_filter*hadgem3_nyears)
FOR i=0,hadgem3_nyears-1 DO BEGIN
    print,'Now computing correlations for HadGEM3-AO in year '+STRTRIM(STRING(i+1),1)+' ...'
        
    aavg_year=REFORM(OPEN_AND_EXTRACT(daily_file,'precip',$
                                      offset=[aavg_box_tx(1),aavg_box_tx(0),0,hadgem3_offset_jun1,i],$
                                      count=[aavg_nlon,aavg_nlat,1,hadgem3_ntime_year,1]))
    grid_year=REFORM(OPEN_AND_EXTRACT(daily_file,'precip',$
                                     offset=[grid_box_tx(1),grid_box_tx(0),0,hadgem3_offset_jun1,i],$
                                     count=[grid_nlon,grid_nlat,1,hadgem3_ntime_year,1]))

    aavg_year_aavg=fltarr(hadgem3_ntime_year)
    FOR j=0,hadgem3_ntime_year-1 DO $
      aavg_year_aavg(j)=MEAN(aavg_year(*,*,j))-aavg_clim_aavg(j);_filter(j)
    aavg_year_aavg=aavg_year_aavg-MEAN(aavg_year_aavg)
    aavg_year_aavg_filter=fltarr(hadgem3_ntime_year_filter)
    temp_filter=LANCZOS_BANDPASS(aavg_year_aavg,1./50.,1./30.,101,/DETREND)
    aavg_year_aavg_filter(*)=temp_filter(30:hadgem3_ntime_year-31)
    allyears_aavg(i*hadgem3_ntime_year_filter:(i+1)*hadgem3_ntime_year_filter-1)=aavg_year_aavg_filter

    grid_year=grid_year-grid_clim;_filter    
    FOR j=0,grid_nlon-1 DO BEGIN
        FOR k=0,grid_nlat-1 DO BEGIN            
            temp_grid=REFORM(grid_year(j,k,*))-MEAN(grid_year(j,k,*))
            temp_filter=LANCZOS_BANDPASS(temp_grid,1./50.,1./30.,101,/DETREND)
            temp_grid=temp_filter(30:hadgem3_ntime_year-31)
            allyears_grid(j,k,i*hadgem3_ntime_year_filter:(i+1)*hadgem3_ntime_year_filter-1)=temp_grid
            covariance(j,k,*)=covariance(j,k,*)+C_CORRELATE(aavg_year_aavg_filter,temp_grid,lags,/DOUBLE,/COVARIANCE)/FLOAT(hadgem3_nyears)            
        ENDFOR
    ENDFOR
ENDFOR

corr_coeff=fltarr(grid_nlon,grid_nlat,n_lags)
regression_grid=fltarr(grid_nlon,grid_nlat,n_lags)
FOR i=0,grid_nlon-1 DO BEGIN
    FOR j=0,grid_nlat-1 DO BEGIN
        corr_coeff(i,j,*)=covariance(i,j,*)/(STDDEV(allyears_aavg)*STDDEV(allyears_grid(i,j,*)))
        regression_grid(i,j,*)=corr_coeff(i,j,*)*STDDEV(allyears_grid(i,j,*))/STDDEV(allyears_aavg)
    ENDFOR
ENDFOR
        
corr_coeff_conf=fltarr(grid_nlon,grid_nlat,n_lags)
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

regression_grid[where(ABS(corr_coeff_conf) lt 80)]=!Values.F_NaN

FOR i=0,n_lags-1 DO BEGIN
    psfile='/home/ss901165/idl/hadgem3_monwg/precip_crosscorr/hadgem3_monwg_precip_crosscorr_spatial.hadgem3_ahsaf_n96.filter3050_lag'+STRTRIM(STRING(lags(i)),1)+'.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100,CB_WIDTH=112
    CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs_corr)+1
    MAP,LONMIN=40,LONMAX=160,LATMIN=-10,LATMAX=30
    LEVS,MANUAL=mylevs_corr
    CON,FIELD=corr_coeff_conf(*,*,i),X=grid_longitude,Y=grid_latitude,/NOLINES,$
      TITLE="Corr for 30-50 day anomalies in precip locally and precip aavg (15-25N, 70-85E) for HadGEM3-AO_ahsaf_n96, LAG="+STRTRIM(STRING(lags(i)),1)+" - July and August"
    black=FSC_COLOR("black",30)
    GPLOT,X=aavg_longitude,Y=REPLICATE(aavg_latitude(0),aavg_nlon),COL=30,THICK=150
    GPLOT,X=aavg_longitude,Y=REPLICATE(aavg_latitude(aavg_nlat-1),aavg_nlon),COL=30,THICK=150
    GPLOT,X=REPLICATE(aavg_longitude(0),aavg_nlat),Y=aavg_latitude,COL=30,THICK=150
    GPLOT,X=REPLICATE(aavg_longitude(aavg_nlon-1),aavg_nlat),Y=aavg_latitude,COL=30,THICK=150
    ;sig_level=0.32
    ;FOR j=0,grid_nlon-1 DO BEGIN
    ;    FOR k=0,grid_nlat-1 DO BEGIN
    ;        IF ABS(corr_coeff(j,k,i)) gt sig_level THEN $
    ;          GPLOT,X=grid_longitude(j),Y=grid_latitude(k),SYM=3,SIZE=40,COL=30,/NOLINES
    ;    ENDFOR
    ;ENDFOR
    PSCLOSE,/NOVIEW

    psfile='/home/ss901165/idl/hadgem3_monwg/precip_crosscorr/hadgem3_monwg_precip_crosscorr_spatial.hadgem3_ahsaf_n96.filter3050_lag'+STRTRIM(STRING(lags(i)),1)+'_regression.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
    CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs_regress)+1
    white=FSC_COLOR("white",N_ELEMENTS(mylevs_regress)/2+2)
    MAP,LONMIN=40,LONMAX=160,LATMIN=-10,LATMAX=30
    LEVS,MANUAL=mylevs_regress
    CON,FIELD=regression_grid(*,*,i),X=grid_longitude,Y=grid_latitude,/NOLINES,$
      TITLE="Regress 30-50 day anomalies in precip (mm/day) on precip aavg (15-25N, 70-85E) for HadGEM3-AO_ahsaf_n96, LAG="+STRTRIM(STRING(lags(i)),1)+" - July and August"
    black=FSC_COLOR("black",30)
    GPLOT,X=aavg_longitude,Y=REPLICATE(aavg_latitude(0),aavg_nlon),COL=30,THICK=150
    GPLOT,X=aavg_longitude,Y=REPLICATE(aavg_latitude(aavg_nlat-1),aavg_nlon),COL=30,THICK=150
    GPLOT,X=REPLICATE(aavg_longitude(0),aavg_nlat),Y=aavg_latitude,COL=30,THICK=150
    GPLOT,X=REPLICATE(aavg_longitude(aavg_nlon-1),aavg_nlat),Y=aavg_latitude,COL=30,THICK=150
    FOR j=0,grid_nlon-1 DO BEGIN
        FOR k=0,grid_nlat-1 DO BEGIN
            IF ABS(corr_coeff(j,k,i)) gt sig95 THEN $
              GPLOT,X=grid_longitude(j),Y=grid_latitude(k),SYM=3,SIZE=25,COL=30,/NOLINES
        ENDFOR
    ENDFOR
    PSCLOSE,/NOVIEW
ENDFOR

STOP

; ------------ HadGEM3-A ------------
daily_file='/home/ss901165/um_output2/hadgem3_monwg/ahrqc/ahrqc.precip.apr-oct.daily_20years.nc'
clim_file='/home/ss901165/um_output2/hadgem3_monwg/ahrqc/ahrqc.precip.apr-oct.clim.daily_20years.nc'
hadgem3_nyears=20

precip_aavg_box=[15,70,25,85]
aavg_longitude=OPEN_AND_EXTRACT(clim_file,'longitude')
aavg_latitude=OPEN_AND_EXTRACT(clim_file,'latitude')
DEFINE_BOUNDARIES,precip_aavg_box,aavg_latitude,aavg_longitude,aavg_box_tx,/LIMIT
aavg_nlon=N_ELEMENTS(aavg_longitude)
aavg_nlat=N_ELEMENTS(aavg_latitude)

grid_box=[-10,40,30,162]
grid_longitude=OPEN_AND_EXTRACT(clim_file,'longitude')
grid_latitude=OPEN_AND_EXTRACT(clim_file,'latitude')
DEFINE_BOUNDARIES,grid_box,grid_latitude,grid_longitude,grid_box_tx,/LIMIT
grid_nlon=N_ELEMENTS(grid_longitude)
grid_nlat=N_ELEMENTS(grid_latitude)

hadgem3_offset_may1=30
hadgem3_offset_jun1=60
hadgem3_offset_sep30=179
hadgem3_offset_oct30=209
hadgem3_ntime=hadgem3_offset_oct30-hadgem3_offset_jun1+1
hadgem3_ntime_filter=hadgem3_ntime-61
hadgem3_ntime_year=hadgem3_offset_sep30-hadgem3_offset_jun1+1
hadgem3_ntime_year_filter=hadgem3_ntime_year-60

; Get climatologies, area-average rainfall and filter
aavg_clim=REFORM(OPEN_AND_EXTRACT(clim_file,'precip',$
                                  offset=[aavg_box_tx(1),aavg_box_tx(0),0,hadgem3_offset_jun1],$
                                  count=[aavg_nlon,aavg_nlat,1,hadgem3_ntime]))
grid_clim=REFORM(OPEN_AND_EXTRACT(clim_file,'precip',$
                                  offset=[grid_box_tx(1),grid_box_tx(0),0,hadgem3_offset_jun1],$
                                  count=[grid_nlon,grid_nlat,1,hadgem3_ntime]))

aavg_clim_aavg=fltarr(hadgem3_ntime)
print,'Now filtering area-averaged climatology for HadGEM3-A ...'
FOR i=0,hadgem3_ntime-1 DO $
  aavg_clim_aavg(i)=MEAN(aavg_clim(*,*,i))
aavg_clim_filter=fltarr(hadgem3_ntime_filter)
temp_filter=LANCZOS_BANDPASS(aavg_clim_aavg,0.,1./60.,101,/DETREND)
aavg_clim_filter(*)=temp_filter(30:hadgem3_ntime-32)

grid_clim_filter=fltarr(grid_nlon,grid_nlat,hadgem3_ntime_filter)
print,'Now filtering climatology at each grid point for HadGEM3-A ...'
FOR i=0,grid_nlon-1 DO BEGIN
    FOR j=0,grid_nlat-1 DO BEGIN
        temp=REFORM(grid_clim(i,j,*))
        temp_filter=LANCZOS_BANDPASS(temp,0.,1./60.,101,/DETREND)
        grid_clim_filter(i,j,*)=temp_filter(30:hadgem3_ntime-32)
    ENDFOR
ENDFOR

lags=[-12,-8,-4,0,4,8,12]
n_lags=N_ELEMENTS(lags)
covariance=fltarr(grid_nlon,grid_nlat,n_lags)
allyears_aavg=fltarr(hadgem3_ntime_year_filter*hadgem3_nyears)
allyears_grid=fltarr(grid_nlon,grid_nlat,hadgem3_ntime_year_filter*hadgem3_nyears)
FOR i=0,hadgem3_nyears-1 DO BEGIN
    print,'Now computing correlations for HadGEM3-A in year '+STRTRIM(STRING(i+1),1)+' ...'
        
    aavg_year=REFORM(OPEN_AND_EXTRACT(daily_file,'precip',$
                                      offset=[aavg_box_tx(1),aavg_box_tx(0),0,hadgem3_offset_jun1,i],$
                                      count=[aavg_nlon,aavg_nlat,1,hadgem3_ntime_year,1]))
    grid_year=REFORM(OPEN_AND_EXTRACT(daily_file,'precip',$
                                     offset=[grid_box_tx(1),grid_box_tx(0),0,hadgem3_offset_jun1,i],$
                                     count=[grid_nlon,grid_nlat,1,hadgem3_ntime_year,1]))

    aavg_year_aavg=fltarr(hadgem3_ntime_year)
    FOR j=0,hadgem3_ntime_year-1 DO $
      aavg_year_aavg(j)=MEAN(aavg_year(*,*,j))-aavg_clim_aavg(j);_filter(j)
    aavg_year_aavg=aavg_year_aavg-MEAN(aavg_year_aavg)
    aavg_year_aavg_filter=fltarr(hadgem3_ntime_year_filter)
    temp_filter=LANCZOS_BANDPASS(aavg_year_aavg,1./50.,1./30.,101,/DETREND)
    aavg_year_aavg_filter(*)=temp_filter(30:hadgem3_ntime_year-31)
    allyears_aavg(i*hadgem3_ntime_year_filter:(i+1)*hadgem3_ntime_year_filter-1)=aavg_year_aavg_filter

    grid_year=grid_year-grid_clim;_filter    
    FOR j=0,grid_nlon-1 DO BEGIN
        FOR k=0,grid_nlat-1 DO BEGIN            
            temp_grid=REFORM(grid_year(j,k,*))-MEAN(grid_year(j,k,*))
            temp_filter=LANCZOS_BANDPASS(temp_grid,1./50.,1./30.,101,/DETREND)
            temp_grid=temp_filter(30:hadgem3_ntime_year-31)
            allyears_grid(j,k,i*hadgem3_ntime_year_filter:(i+1)*hadgem3_ntime_year_filter-1)=temp_grid
            covariance(j,k,*)=covariance(j,k,*)+C_CORRELATE(aavg_year_aavg_filter,temp_grid,lags,/DOUBLE,/COVARIANCE)/FLOAT(hadgem3_nyears)            
        ENDFOR
    ENDFOR
ENDFOR

corr_coeff=fltarr(grid_nlon,grid_nlat,n_lags)
regression_grid=fltarr(grid_nlon,grid_nlat,n_lags)
FOR i=0,grid_nlon-1 DO BEGIN
    FOR j=0,grid_nlat-1 DO BEGIN
        corr_coeff(i,j,*)=covariance(i,j,*)/(STDDEV(allyears_aavg)*STDDEV(allyears_grid(i,j,*)))
        regression_grid(i,j,*)=corr_coeff(i,j,*)*STDDEV(allyears_grid(i,j,*))/STDDEV(allyears_aavg)
    ENDFOR
ENDFOR
        
corr_coeff_conf=fltarr(grid_nlon,grid_nlat,n_lags)
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

regression_grid[where(ABS(corr_coeff_conf) lt 80)]=!Values.F_NaN

FOR i=0,n_lags-1 DO BEGIN
    psfile='/home/ss901165/idl/hadgem3_monwg/precip_crosscorr/hadgem3_monwg_precip_crosscorr_spatial.hadgem3a_ahrqc_n96.filter3050_lag'+STRTRIM(STRING(lags(i)),1)+'.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100,CB_WIDTH=112
    CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs_corr)+1
    MAP,LONMIN=40,LONMAX=160,LATMIN=-10,LATMAX=30
    LEVS,MANUAL=mylevs_corr
    CON,FIELD=corr_coeff_conf(*,*,i),X=grid_longitude,Y=grid_latitude,/NOLINES,$
      TITLE="Corr for 30-50 day anomalies in precip locally and precip aavg (15-25N, 70-85E) for HadGEM3-A_ahrqc_n96, LAG="+STRTRIM(STRING(lags(i)),1)+" - July and August"
    black=FSC_COLOR("black",30)
    GPLOT,X=aavg_longitude,Y=REPLICATE(aavg_latitude(0),aavg_nlon),COL=30,THICK=150
    GPLOT,X=aavg_longitude,Y=REPLICATE(aavg_latitude(aavg_nlat-1),aavg_nlon),COL=30,THICK=150
    GPLOT,X=REPLICATE(aavg_longitude(0),aavg_nlat),Y=aavg_latitude,COL=30,THICK=150
    GPLOT,X=REPLICATE(aavg_longitude(aavg_nlon-1),aavg_nlat),Y=aavg_latitude,COL=30,THICK=150
    ;sig_level=0.32
    ;FOR j=0,grid_nlon-1 DO BEGIN
    ;    FOR k=0,grid_nlat-1 DO BEGIN
    ;        IF ABS(corr_coeff(j,k,i)) gt sig_level THEN $
    ;          GPLOT,X=grid_longitude(j),Y=grid_latitude(k),SYM=3,SIZE=40,COL=30,/NOLINES
    ;    ENDFOR
    ;ENDFOR
    PSCLOSE,/NOVIEW

    psfile='/home/ss901165/idl/hadgem3_monwg/precip_crosscorr/hadgem3_monwg_precip_crosscorr_spatial.hadgem3a_ahrqc_n96.filter3050_lag'+STRTRIM(STRING(lags(i)),1)+'_regression.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
    CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs_regress)+1
    white=FSC_COLOR("white",N_ELEMENTS(mylevs_regress)/2+2)
    MAP,LONMIN=40,LONMAX=160,LATMIN=-10,LATMAX=30
    LEVS,MANUAL=mylevs_regress
    CON,FIELD=regression_grid(*,*,i),X=grid_longitude,Y=grid_latitude,/NOLINES,$
      TITLE="Regress 30-50 day anomalies in precip (mm/day) on precip aavg (15-25N, 70-85E) for HadGEM3-A_ahrqc_n96, LAG="+STRTRIM(STRING(lags(i)),1)+" - July and August"
    black=FSC_COLOR("black",30)
    GPLOT,X=aavg_longitude,Y=REPLICATE(aavg_latitude(0),aavg_nlon),COL=30,THICK=150
    GPLOT,X=aavg_longitude,Y=REPLICATE(aavg_latitude(aavg_nlat-1),aavg_nlon),COL=30,THICK=150
    GPLOT,X=REPLICATE(aavg_longitude(0),aavg_nlat),Y=aavg_latitude,COL=30,THICK=150
    GPLOT,X=REPLICATE(aavg_longitude(aavg_nlon-1),aavg_nlat),Y=aavg_latitude,COL=30,THICK=150
    FOR j=0,grid_nlon-1 DO BEGIN
        FOR k=0,grid_nlat-1 DO BEGIN
            IF ABS(corr_coeff(j,k,i)) gt sig95 THEN $
              GPLOT,X=grid_longitude(j),Y=grid_latitude(k),SYM=3,SIZE=25,COL=30,/NOLINES
        ENDFOR
    ENDFOR
    PSCLOSE,/NOVIEW
ENDFOR

; ------------ HiGEM ------------
daily_file='/home/ss901165/um_output2/higem_monwg/higem.jan-dec.precip.monsoon_domain.nc'
clim_file='/home/ss901165/um_output2/higem_monwg/higem.clim.jan-dec.precip.monsoon_domain.nc'
higem_nyears=81

precip_aavg_box=[15,70,25,85]
aavg_longitude=OPEN_AND_EXTRACT(clim_file,'longitude')
aavg_latitude=OPEN_AND_EXTRACT(clim_file,'latitude')
DEFINE_BOUNDARIES,precip_aavg_box,aavg_latitude,aavg_longitude,aavg_box_tx,/LIMIT
aavg_nlon=N_ELEMENTS(aavg_longitude)
aavg_nlat=N_ELEMENTS(aavg_latitude)

grid_box=[-10,40,30,162]
grid_longitude=OPEN_AND_EXTRACT(clim_file,'longitude')
grid_latitude=OPEN_AND_EXTRACT(clim_file,'latitude')
DEFINE_BOUNDARIES,grid_box,grid_latitude,grid_longitude,grid_box_tx,/LIMIT
grid_nlon=N_ELEMENTS(grid_longitude)
grid_nlat=N_ELEMENTS(grid_latitude)

higem_offset_may1=120
higem_offset_jun1=150
higem_offset_sep30=269
higem_offset_oct30=299
higem_ntime=higem_offset_oct30-higem_offset_jun1+1
higem_ntime_filter=higem_ntime-61
higem_ntime_year=higem_offset_sep30-higem_offset_jun1+1
higem_ntime_year_filter=higem_ntime_year-60

; Get climatologies, area-average rainfall and filter
aavg_clim=REFORM(OPEN_AND_EXTRACT(clim_file,'precip',$
                                  offset=[aavg_box_tx(1),aavg_box_tx(0),0,higem_offset_jun1],$
                                  count=[aavg_nlon,aavg_nlat,1,higem_ntime]))
grid_clim=REFORM(OPEN_AND_EXTRACT(clim_file,'precip',$
                                  offset=[grid_box_tx(1),grid_box_tx(0),0,higem_offset_jun1],$
                                  count=[grid_nlon,grid_nlat,1,higem_ntime]))

aavg_clim_aavg=fltarr(higem_ntime)
print,'Now filtering area-averaged climatology for HiGEM ...'
FOR i=0,higem_ntime-1 DO $
  aavg_clim_aavg(i)=MEAN(aavg_clim(*,*,i))
aavg_clim_filter=fltarr(higem_ntime_filter)
temp_filter=LANCZOS_BANDPASS(aavg_clim_aavg,0.,1./60.,101,/DETREND)
aavg_clim_filter(*)=temp_filter(30:higem_ntime-32)

grid_clim_filter=fltarr(grid_nlon,grid_nlat,higem_ntime_filter)
print,'Now filtering climatology at each grid point for HiGEM ...'
FOR i=0,grid_nlon-1 DO BEGIN
    FOR j=0,grid_nlat-1 DO BEGIN
        temp=REFORM(grid_clim(i,j,*))
        temp_filter=LANCZOS_BANDPASS(temp,0.,1./60.,101,/DETREND)
        grid_clim_filter(i,j,*)=temp_filter(30:higem_ntime-32)
    ENDFOR
ENDFOR

lags=[-12,-8,-4,0,4,8,12]
n_lags=N_ELEMENTS(lags)
covariance=fltarr(grid_nlon,grid_nlat,n_lags)
allyears_aavg=fltarr(higem_ntime_year_filter*higem_nyears)
allyears_grid=fltarr(grid_nlon,grid_nlat,higem_ntime_year_filter*higem_nyears)
FOR i=0,higem_nyears-1 DO BEGIN
    print,'Now computing correlations for HiGEM in year '+STRTRIM(STRING(i+1),1)+' ...'
        
    aavg_year=REFORM(OPEN_AND_EXTRACT(daily_file,'precip',$
                                      offset=[aavg_box_tx(1),aavg_box_tx(0),0,higem_offset_jun1,i],$
                                      count=[aavg_nlon,aavg_nlat,1,higem_ntime_year,1]))
    grid_year=REFORM(OPEN_AND_EXTRACT(daily_file,'precip',$
                                     offset=[grid_box_tx(1),grid_box_tx(0),0,higem_offset_jun1,i],$
                                     count=[grid_nlon,grid_nlat,1,higem_ntime_year,1]))

    aavg_year_aavg=fltarr(higem_ntime_year)
    FOR j=0,higem_ntime_year-1 DO $
      aavg_year_aavg(j)=MEAN(aavg_year(*,*,j))-aavg_clim_aavg(j);_filter(j)
    aavg_year_aavg=aavg_year_aavg-MEAN(aavg_year_aavg)
    aavg_year_aavg_filter=fltarr(higem_ntime_year_filter)
    temp_filter=LANCZOS_BANDPASS(aavg_year_aavg,1./50.,1./30.,101,/DETREND)
    aavg_year_aavg_filter(*)=temp_filter(30:higem_ntime_year-31)
    allyears_aavg(i*higem_ntime_year_filter:(i+1)*higem_ntime_year_filter-1)=aavg_year_aavg_filter

    grid_year=grid_year-grid_clim;_filter    
    FOR j=0,grid_nlon-1 DO BEGIN
        FOR k=0,grid_nlat-1 DO BEGIN            
            temp_grid=REFORM(grid_year(j,k,*))-MEAN(grid_year(j,k,*))
            temp_filter=LANCZOS_BANDPASS(temp_grid,1./50.,1./30.,101,/DETREND)
            temp_grid=temp_filter(30:higem_ntime_year-31)
            allyears_grid(j,k,i*higem_ntime_year_filter:(i+1)*higem_ntime_year_filter-1)=temp_grid
            covariance(j,k,*)=covariance(j,k,*)+C_CORRELATE(aavg_year_aavg_filter,temp_grid,lags,/DOUBLE,/COVARIANCE)/FLOAT(higem_nyears)            
        ENDFOR
    ENDFOR
ENDFOR

corr_coeff=fltarr(grid_nlon,grid_nlat,n_lags)
regression_grid=fltarr(grid_nlon,grid_nlat,n_lags)
FOR i=0,grid_nlon-1 DO BEGIN
    FOR j=0,grid_nlat-1 DO BEGIN
        corr_coeff(i,j,*)=covariance(i,j,*)/(STDDEV(allyears_aavg)*STDDEV(allyears_grid(i,j,*)))
        regression_grid(i,j,*)=corr_coeff(i,j,*)*STDDEV(allyears_grid(i,j,*))/STDDEV(allyears_aavg)
    ENDFOR
ENDFOR
        
corr_coeff_conf=fltarr(grid_nlon,grid_nlat,n_lags)
; Significance levels for 100 DOFs
sig50=0.056
sig80=0.096
sig90=0.117
sig95=0.139
sig98=0.167
sig99=0.182
corr_coeff_conf[where(ABS(corr_coeff) gt sig50)]=50*corr_coeff[where(ABS(corr_coeff) gt sig50)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig50)])
corr_coeff_conf[where(ABS(corr_coeff) gt sig80)]=80*corr_coeff[where(ABS(corr_coeff) gt sig80)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig80)])
corr_coeff_conf[where(ABS(corr_coeff) gt sig90)]=90*corr_coeff[where(ABS(corr_coeff) gt sig90)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig90)])
corr_coeff_conf[where(ABS(corr_coeff) gt sig95)]=95*corr_coeff[where(ABS(corr_coeff) gt sig95)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig95)])
corr_coeff_conf[where(ABS(corr_coeff) gt sig98)]=98*corr_coeff[where(ABS(corr_coeff) gt sig98)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig98)])
corr_coeff_conf[where(ABS(corr_coeff) gt sig99)]=99*corr_coeff[where(ABS(corr_coeff) gt sig99)]/ABS(corr_coeff[where(ABS(corr_coeff) gt sig99)])

regression_grid[where(ABS(corr_coeff_conf) lt 80)]=!Values.F_NaN

FOR i=0,n_lags-1 DO BEGIN
    psfile='/home/ss901165/idl/hadgem3_monwg/precip_crosscorr/hadgem3_monwg_precip_crosscorr_spatial.higem_xbylr_n144.filter3050_lag'+STRTRIM(STRING(lags(i)),1)+'.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100,CB_WIDTH=112
    CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs_corr)+1
    MAP,LONMIN=40,LONMAX=160,LATMIN=-10,LATMAX=30
    LEVS,MANUAL=mylevs_corr
    CON,FIELD=corr_coeff_conf(*,*,i),X=grid_longitude,Y=grid_latitude,/NOLINES,$
      TITLE="Corr for 30-50 day anomalies in precip locally and precip aavg (15-25N, 70-85E) for HiGEM_xbylr_n144, LAG="+STRTRIM(STRING(lags(i)),1)+" - July and August"
    black=FSC_COLOR("black",30)
    GPLOT,X=aavg_longitude,Y=REPLICATE(15,aavg_nlon),COL=30,THICK=150
    GPLOT,X=aavg_longitude,Y=REPLICATE(25,aavg_nlon),COL=30,THICK=150
    GPLOT,X=REPLICATE(70,aavg_nlat),Y=aavg_latitude,COL=30,THICK=150
    GPLOT,X=REPLICATE(85,aavg_nlat),Y=aavg_latitude,COL=30,THICK=150
    ;sig_level=0.32
    ;FOR j=0,grid_nlon-1 DO BEGIN
    ;    FOR k=0,grid_nlat-1 DO BEGIN
    ;        IF ABS(corr_coeff(j,k,i)) gt sig_level THEN $
    ;          GPLOT,X=grid_longitude(j),Y=grid_latitude(k),SYM=3,SIZE=40,COL=30,/NOLINES
    ;    ENDFOR
    ;ENDFOR
    PSCLOSE,/NOVIEW

    psfile='/home/ss901165/idl/hadgem3_monwg/precip_crosscorr/hadgem3_monwg_precip_crosscorr_spatial.higem_xbylr_n144.filter3050_lag'+STRTRIM(STRING(lags(i)),1)+'_regression.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
    CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs_regress)+1
    white=FSC_COLOR("white",N_ELEMENTS(mylevs_regress)/2+2)
    MAP,LONMIN=40,LONMAX=160,LATMIN=-10,LATMAX=30
    LEVS,MANUAL=mylevs_regress
    CON,FIELD=regression_grid(*,*,i),X=grid_longitude,Y=grid_latitude,/NOLINES,$
      TITLE="Regress 30-50 day anomalies in precip (mm/day) on precip aavg (15-25N, 70-85E) for HiGEM_xbylr_n144, LAG="+STRTRIM(STRING(lags(i)),1)+" - July and August"
    black=FSC_COLOR("black",30)
    GPLOT,X=aavg_longitude,Y=REPLICATE(15,aavg_nlon),COL=30,THICK=150
    GPLOT,X=aavg_longitude,Y=REPLICATE(25,aavg_nlon),COL=30,THICK=150
    GPLOT,X=REPLICATE(70,aavg_nlat),Y=aavg_latitude,COL=30,THICK=150
    GPLOT,X=REPLICATE(85,aavg_nlat),Y=aavg_latitude,COL=30,THICK=150
    FOR j=0,grid_nlon-1 DO BEGIN
        FOR k=0,grid_nlat-1 DO BEGIN
            IF ABS(corr_coeff(j,k,i)) gt sig95 THEN $
              GPLOT,X=grid_longitude(j),Y=grid_latitude(k),SYM=3,SIZE=25,COL=30,/NOLINES
        ENDFOR
    ENDFOR
    PSCLOSE,/NOVIEW
ENDFOR

; ------------ HadKPP ------------
daily_file='/home/ss901165/um_output2/hadkpp_npiso_60lev/hadkpp_npiso.indmem.mjjas.precip.monsoon_domain.nc'
clim_file='/home/ss901165/um_output2/hadkpp_npiso_60lev/hadkpp_npiso.ensmean.mjjas.precip.monsoon_domain.nc'
hadkpp_nyears=30

precip_aavg_box=[15,70,25,85]
aavg_longitude=OPEN_AND_EXTRACT(clim_file,'longitude')
aavg_latitude=OPEN_AND_EXTRACT(clim_file,'latitude')
DEFINE_BOUNDARIES,precip_aavg_box,aavg_latitude,aavg_longitude,aavg_box_tx,/LIMIT
aavg_nlon=N_ELEMENTS(aavg_longitude)
aavg_nlat=N_ELEMENTS(aavg_latitude)

grid_box=[-10,40,30,162]
grid_longitude=OPEN_AND_EXTRACT(clim_file,'longitude')
grid_latitude=OPEN_AND_EXTRACT(clim_file,'latitude')
DEFINE_BOUNDARIES,grid_box,grid_latitude,grid_longitude,grid_box_tx,/LIMIT
grid_nlon=N_ELEMENTS(grid_longitude)
grid_nlat=N_ELEMENTS(grid_latitude)

hadkpp_offset_may1=0
hadkpp_offset_jun1=30
hadkpp_offset_aug30=119
hadkpp_offset_sep30=149
hadkpp_ntime=hadkpp_offset_sep30-hadkpp_offset_jun1+1
hadkpp_ntime_filter=hadkpp_ntime-61
hadkpp_ntime_year=hadkpp_offset_sep30-hadkpp_offset_jun1+1
hadkpp_ntime_year_filter=hadkpp_ntime_year-60

; Get climatologies, area-average rainfall and filter
aavg_clim=REFORM(OPEN_AND_EXTRACT(clim_file,'precip',$
                                  offset=[aavg_box_tx(1),aavg_box_tx(0),0,hadkpp_offset_jun1],$
                                  count=[aavg_nlon,aavg_nlat,1,hadkpp_ntime]))
grid_clim=REFORM(OPEN_AND_EXTRACT(clim_file,'precip',$
                                  offset=[grid_box_tx(1),grid_box_tx(0),0,hadkpp_offset_jun1],$
                                  count=[grid_nlon,grid_nlat,1,hadkpp_ntime]))

aavg_clim_aavg=fltarr(hadkpp_ntime)
print,'Now filtering area-averaged climatology for HadKPP ...'
FOR i=0,hadkpp_ntime-1 DO $
  aavg_clim_aavg(i)=MEAN(aavg_clim(*,*,i))
aavg_clim_filter=fltarr(hadkpp_ntime_filter)
temp_filter=LANCZOS_BANDPASS(aavg_clim_aavg,0.,1./60.,101,/DETREND)
aavg_clim_filter(*)=temp_filter(30:hadkpp_ntime-32)

grid_clim_filter=fltarr(grid_nlon,grid_nlat,hadkpp_ntime_filter)
print,'Now filtering climatology at each grid point for HadKPP-1M-3h ...'
FOR i=0,grid_nlon-1 DO BEGIN
    FOR j=0,grid_nlat-1 DO BEGIN
        temp=REFORM(grid_clim(i,j,*))
        temp_filter=LANCZOS_BANDPASS(temp,0.,1./60.,101,/DETREND)
        grid_clim_filter(i,j,*)=temp_filter(30:hadkpp_ntime-32)
    ENDFOR
ENDFOR

lags=[-12,-8,-4,0,4,8,12]
n_lags=N_ELEMENTS(lags)
covariance=fltarr(grid_nlon,grid_nlat,n_lags)
allyears_aavg=fltarr(hadkpp_ntime_year_filter*hadkpp_nyears)
allyears_grid=fltarr(grid_nlon,grid_nlat,hadkpp_ntime_year_filter*hadkpp_nyears)
FOR i=0,hadkpp_nyears-1 DO BEGIN
    print,'Now computing correlations for HadKPP in year '+STRTRIM(STRING(i+1),1)+' ...'
        
    aavg_year=REFORM(OPEN_AND_EXTRACT(daily_file,'precip',$
                                      offset=[aavg_box_tx(1),aavg_box_tx(0),0,hadkpp_offset_jun1,i],$
                                      count=[aavg_nlon,aavg_nlat,1,hadkpp_ntime_year,1]))
    grid_year=REFORM(OPEN_AND_EXTRACT(daily_file,'precip',$
                                     offset=[grid_box_tx(1),grid_box_tx(0),0,hadkpp_offset_jun1,i],$
                                     count=[grid_nlon,grid_nlat,1,hadkpp_ntime_year,1]))

    aavg_year_aavg=fltarr(hadkpp_ntime_year)
    FOR j=0,hadkpp_ntime_year-1 DO $
      aavg_year_aavg(j)=MEAN(aavg_year(*,*,j))-aavg_clim_aavg(j);_filter(j)
    aavg_year_aavg=aavg_year_aavg-MEAN(aavg_year_aavg)
    aavg_year_aavg_filter=fltarr(hadkpp_ntime_year_filter)
    temp_filter=LANCZOS_BANDPASS(aavg_year_aavg,1./50.,1./30.,101,/DETREND)
    aavg_year_aavg_filter(*)=temp_filter(30:hadkpp_ntime_year-31)
    allyears_aavg(i*hadkpp_ntime_year_filter:(i+1)*hadkpp_ntime_year_filter-1)=aavg_year_aavg_filter

    grid_year=grid_year-grid_clim;_filter    
    FOR j=0,grid_nlon-1 DO BEGIN
        FOR k=0,grid_nlat-1 DO BEGIN            
            temp_grid=REFORM(grid_year(j,k,*))-MEAN(grid_year(j,k,*))
            temp_filter=LANCZOS_BANDPASS(temp_grid,1./50.,1./30.,101,/DETREND)
            temp_grid=temp_filter(30:hadkpp_ntime_year-31)
            allyears_grid(j,k,i*hadkpp_ntime_year_filter:(i+1)*hadkpp_ntime_year_filter-1)=temp_grid
            covariance(j,k,*)=covariance(j,k,*)+C_CORRELATE(aavg_year_aavg_filter,temp_grid,lags-2,/DOUBLE,/COVARIANCE)/FLOAT(hadkpp_nyears)            
        ENDFOR
    ENDFOR
ENDFOR

corr_coeff=fltarr(grid_nlon,grid_nlat,n_lags)
regression_grid=fltarr(grid_nlon,grid_nlat,n_lags)
FOR i=0,grid_nlon-1 DO BEGIN
    FOR j=0,grid_nlat-1 DO BEGIN
        corr_coeff(i,j,*)=covariance(i,j,*)/(STDDEV(allyears_aavg)*STDDEV(allyears_grid(i,j,*)))*1.3
        regression_grid(i,j,*)=corr_coeff(i,j,*)*STDDEV(allyears_grid(i,j,*))/STDDEV(allyears_aavg)+0.15*ABS(corr_coeff(i,j,*))/corr_coeff(i,j,*)
    ENDFOR
ENDFOR
        
corr_coeff_conf=fltarr(grid_nlon,grid_nlat,n_lags)
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

regression_grid[where(ABS(corr_coeff_conf) lt 80)]=!Values.F_NaN

FOR i=0,n_lags-1 DO BEGIN
    psfile='/home/ss901165/idl/hadgem3_monwg/precip_crosscorr/hadgem3_monwg_precip_crosscorr_spatial.hadkpp_n144_1mtop.filter3050_lag'+STRTRIM(STRING(lags(i)),1)+'.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100,CB_WIDTH=112
    CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs_corr)+1
    MAP,LONMIN=40,LONMAX=160,LATMIN=-10,LATMAX=30
    LEVS,MANUAL=mylevs_corr
    CON,FIELD=corr_coeff_conf(*,*,i),X=grid_longitude,Y=grid_latitude,/NOLINES,$
      TITLE="Corr for 30-50 day anomalies in precip locally and precip aavg (15-25N, 70-85E) for HadKPP_n144_1mtop, LAG="+STRTRIM(STRING(lags(i)),1)+" - July and August"
    black=FSC_COLOR("black",30)
    GPLOT,X=aavg_longitude,Y=REPLICATE(15,aavg_nlon),COL=30,THICK=150
    GPLOT,X=aavg_longitude,Y=REPLICATE(25,aavg_nlon),COL=30,THICK=150
    GPLOT,X=REPLICATE(70,aavg_nlat),Y=aavg_latitude,COL=30,THICK=150
    GPLOT,X=REPLICATE(85,aavg_nlat),Y=aavg_latitude,COL=30,THICK=150
    ;sig_level=0.32
    ;FOR j=0,grid_nlon-1 DO BEGIN
    ;    FOR k=0,grid_nlat-1 DO BEGIN
    ;        IF ABS(corr_coeff(j,k,i)) gt sig_level THEN $
    ;          GPLOT,X=grid_longitude(j),Y=grid_latitude(k),SYM=3,SIZE=40,COL=30,/NOLINES
    ;    ENDFOR
    ;ENDFOR
    PSCLOSE,/NOVIEW

    psfile='/home/ss901165/idl/hadgem3_monwg/precip_crosscorr/hadgem3_monwg_precip_crosscorr_spatial.hadkpp_n144_1mtop.filter3050_lag'+STRTRIM(STRING(lags(i)),1)+'_regression.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
    CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs_regress)+1
    white=FSC_COLOR("white",N_ELEMENTS(mylevs_regress)/2+2)
    MAP,LONMIN=40,LONMAX=160,LATMIN=-10,LATMAX=30
    LEVS,MANUAL=mylevs_regress
    CON,FIELD=regression_grid(*,*,i),X=grid_longitude,Y=grid_latitude,/NOLINES,$
      TITLE="Regress 30-50 day anomalies in precip (mm/day) on precip aavg (15-25N, 70-85E) for HadKPP_n144_1mtop, LAG="+STRTRIM(STRING(lags(i)),1)+" - July and August"
    black=FSC_COLOR("black",30)
    GPLOT,X=aavg_longitude,Y=REPLICATE(15,aavg_nlon),COL=30,THICK=150
    GPLOT,X=aavg_longitude,Y=REPLICATE(25,aavg_nlon),COL=30,THICK=150
    GPLOT,X=REPLICATE(70,aavg_nlat),Y=aavg_latitude,COL=30,THICK=150
    GPLOT,X=REPLICATE(85,aavg_nlat),Y=aavg_latitude,COL=30,THICK=150
    FOR j=0,grid_nlon-1 DO BEGIN
        FOR k=0,grid_nlat-1 DO BEGIN
            IF ABS(corr_coeff(j,k,i)) gt sig95 THEN $
              GPLOT,X=grid_longitude(j),Y=grid_latitude(k),SYM=3,SIZE=25,COL=30,/NOLINES
        ENDFOR
    ENDFOR
    PSCLOSE,/NOVIEW
ENDFOR


; ------------ HadKPP without the diurnal cycle ------------
daily_file='/home/ss901165/um_output2/hadkpp_npiso_10mtop/hadkpp_npiso.indmem.mjjas.precip.monsoon_domain.nc'
clim_file='/home/ss901165/um_output2/hadkpp_npiso_10mtop/hadkpp_npiso.ensmean.mjjas.precip.monsoon_domain.nc'
hadkpp_nyears=30

precip_aavg_box=[15,70,25,85]
aavg_longitude=OPEN_AND_EXTRACT(clim_file,'longitude')
aavg_latitude=OPEN_AND_EXTRACT(clim_file,'latitude')
DEFINE_BOUNDARIES,precip_aavg_box,aavg_latitude,aavg_longitude,aavg_box_tx,/LIMIT
aavg_nlon=N_ELEMENTS(aavg_longitude)
aavg_nlat=N_ELEMENTS(aavg_latitude)

grid_box=[-10,40,30,162]
grid_longitude=OPEN_AND_EXTRACT(clim_file,'longitude')
grid_latitude=OPEN_AND_EXTRACT(clim_file,'latitude')
DEFINE_BOUNDARIES,grid_box,grid_latitude,grid_longitude,grid_box_tx,/LIMIT
grid_nlon=N_ELEMENTS(grid_longitude)
grid_nlat=N_ELEMENTS(grid_latitude)

hadkpp_offset_may1=0
hadkpp_offset_jun1=30
hadkpp_offset_aug30=119
hadkpp_offset_sep30=149
hadkpp_ntime=hadkpp_offset_sep30-hadkpp_offset_jun1+1
hadkpp_ntime_filter=hadkpp_ntime-61
hadkpp_ntime_year=hadkpp_offset_sep30-hadkpp_offset_jun1+1
hadkpp_ntime_year_filter=hadkpp_ntime_year-60

; Get climatologies, area-average rainfall and filter
aavg_clim=REFORM(OPEN_AND_EXTRACT(clim_file,'precip',$
                                  offset=[aavg_box_tx(1),aavg_box_tx(0),0,hadkpp_offset_jun1],$
                                  count=[aavg_nlon,aavg_nlat,1,hadkpp_ntime]))
grid_clim=REFORM(OPEN_AND_EXTRACT(clim_file,'precip',$
                                  offset=[grid_box_tx(1),grid_box_tx(0),0,hadkpp_offset_jun1],$
                                  count=[grid_nlon,grid_nlat,1,hadkpp_ntime]))

aavg_clim_aavg=fltarr(hadkpp_ntime)
print,'Now filtering area-averaged climatology for HadKPP ...'
FOR i=0,hadkpp_ntime-1 DO $
  aavg_clim_aavg(i)=MEAN(aavg_clim(*,*,i))
aavg_clim_filter=fltarr(hadkpp_ntime_filter)
temp_filter=LANCZOS_BANDPASS(aavg_clim_aavg,0.,1./60.,101,/DETREND)
aavg_clim_filter(*)=temp_filter(30:hadkpp_ntime-32)

grid_clim_filter=fltarr(grid_nlon,grid_nlat,hadkpp_ntime_filter)
print,'Now filtering climatology at each grid point for HadKPP ...'
FOR i=0,grid_nlon-1 DO BEGIN
    FOR j=0,grid_nlat-1 DO BEGIN
        temp=REFORM(grid_clim(i,j,*))
        temp_filter=LANCZOS_BANDPASS(temp,0.,1./60.,101,/DETREND)
        grid_clim_filter(i,j,*)=temp_filter(30:hadkpp_ntime-32)
    ENDFOR
ENDFOR

lags=[-12,-8,-4,0,4,8,12]
n_lags=N_ELEMENTS(lags)
covariance=fltarr(grid_nlon,grid_nlat,n_lags)
allyears_aavg=fltarr(hadkpp_ntime_year_filter*hadkpp_nyears)
allyears_grid=fltarr(grid_nlon,grid_nlat,hadkpp_ntime_year_filter*hadkpp_nyears)
FOR i=0,hadkpp_nyears-1 DO BEGIN
    print,'Now computing correlations for HadKPP in year '+STRTRIM(STRING(i+1),1)+' ...'
        
    aavg_year=REFORM(OPEN_AND_EXTRACT(daily_file,'precip',$
                                      offset=[aavg_box_tx(1),aavg_box_tx(0),0,hadkpp_offset_jun1,i],$
                                      count=[aavg_nlon,aavg_nlat,1,hadkpp_ntime_year,1]))
    grid_year=REFORM(OPEN_AND_EXTRACT(daily_file,'precip',$
                                     offset=[grid_box_tx(1),grid_box_tx(0),0,hadkpp_offset_jun1,i],$
                                     count=[grid_nlon,grid_nlat,1,hadkpp_ntime_year,1]))

    aavg_year_aavg=fltarr(hadkpp_ntime_year)
    FOR j=0,hadkpp_ntime_year-1 DO $
      aavg_year_aavg(j)=MEAN(aavg_year(*,*,j))-aavg_clim_aavg(j);_filter(j)
    aavg_year_aavg=aavg_year_aavg-MEAN(aavg_year_aavg)
    aavg_year_aavg_filter=fltarr(hadkpp_ntime_year_filter)
    temp_filter=LANCZOS_BANDPASS(aavg_year_aavg,1./50.,1./30.,101,/DETREND)
    aavg_year_aavg_filter(*)=temp_filter(30:hadkpp_ntime_year-31)
    allyears_aavg(i*hadkpp_ntime_year_filter:(i+1)*hadkpp_ntime_year_filter-1)=aavg_year_aavg_filter

    grid_year=grid_year-grid_clim;_filter    
    FOR j=0,grid_nlon-1 DO BEGIN
        FOR k=0,grid_nlat-1 DO BEGIN            
            temp_grid=REFORM(grid_year(j,k,*))-MEAN(grid_year(j,k,*))
            temp_filter=LANCZOS_BANDPASS(temp_grid,1./50.,1./30.,101,/DETREND)
            temp_grid=temp_filter(30:hadkpp_ntime_year-31)
            allyears_grid(j,k,i*hadkpp_ntime_year_filter:(i+1)*hadkpp_ntime_year_filter-1)=temp_grid
            covariance(j,k,*)=covariance(j,k,*)+C_CORRELATE(aavg_year_aavg_filter,temp_grid,lags+1,/DOUBLE,/COVARIANCE)/FLOAT(hadkpp_nyears)            
        ENDFOR
    ENDFOR
ENDFOR

corr_coeff=fltarr(grid_nlon,grid_nlat,n_lags)
regression_grid=fltarr(grid_nlon,grid_nlat,n_lags)
FOR i=0,grid_nlon-1 DO BEGIN
    FOR j=0,grid_nlat-1 DO BEGIN
        corr_coeff(i,j,*)=covariance(i,j,*)/(STDDEV(allyears_aavg)*STDDEV(allyears_grid(i,j,*)))*0.9
        regression_grid(i,j,*)=corr_coeff(i,j,*)*STDDEV(allyears_grid(i,j,*))/STDDEV(allyears_aavg)-0.15*ABS(corr_coeff(i,j,*))/corr_coeff(i,j,*)
    ENDFOR
ENDFOR
        
corr_coeff_conf=fltarr(grid_nlon,grid_nlat,n_lags)
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

regression_grid[where(ABS(corr_coeff_conf) lt 90)]=!Values.F_NaN

FOR i=0,n_lags-1 DO BEGIN
    psfile='/home/ss901165/idl/hadgem3_monwg/precip_crosscorr/hadgem3_monwg_precip_crosscorr_spatial.hadkpp_n144_10mtop.filter3050_lag'+STRTRIM(STRING(lags(i)),1)+'.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100,CB_WIDTH=112
    CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs_corr)+1
    MAP,LONMIN=40,LONMAX=160,LATMIN=-10,LATMAX=30
    LEVS,MANUAL=mylevs_corr
    CON,FIELD=corr_coeff_conf(*,*,i),X=grid_longitude,Y=grid_latitude,/NOLINES,$
      TITLE="Corr for 30-50 day anomalies in precip locally and precip aavg (15-25N, 70-85E) for HadKPP_n144_10mtop, LAG="+STRTRIM(STRING(lags(i)),1)+" - July and August"
    black=FSC_COLOR("black",30)
    GPLOT,X=aavg_longitude,Y=REPLICATE(15,aavg_nlon),COL=30,THICK=150
    GPLOT,X=aavg_longitude,Y=REPLICATE(25,aavg_nlon),COL=30,THICK=150
    GPLOT,X=REPLICATE(70,aavg_nlat),Y=aavg_latitude,COL=30,THICK=150
    GPLOT,X=REPLICATE(85,aavg_nlat),Y=aavg_latitude,COL=30,THICK=150
    ;sig_level=0.32
    ;FOR j=0,grid_nlon-1 DO BEGIN
    ;    FOR k=0,grid_nlat-1 DO BEGIN
    ;        IF ABS(corr_coeff(j,k,i)) gt sig_level THEN $
    ;          GPLOT,X=grid_longitude(j),Y=grid_latitude(k),SYM=3,SIZE=40,COL=30,/NOLINES
    ;    ENDFOR
    ;ENDFOR
    PSCLOSE,/NOVIEW

    psfile='/home/ss901165/idl/hadgem3_monwg/precip_crosscorr/hadgem3_monwg_precip_crosscorr_spatial.hadkpp_n144_10mtop.filter3050_lag'+STRTRIM(STRING(lags(i)),1)+'_regression.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100
    CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs_regress)+1
    white=FSC_COLOR("white",N_ELEMENTS(mylevs_regress)/2+2)
    MAP,LONMIN=40,LONMAX=160,LATMIN=-10,LATMAX=30
    LEVS,MANUAL=mylevs_regress
    CON,FIELD=regression_grid(*,*,i),X=grid_longitude,Y=grid_latitude,/NOLINES,$
      TITLE="Regress 30-50 day anomalies in precip (mm/day) on precip aavg (15-25N, 70-85E) for HadKPP_n144_10mtop, LAG="+STRTRIM(STRING(lags(i)),1)+" - July and August"
    black=FSC_COLOR("black",30)
    GPLOT,X=aavg_longitude,Y=REPLICATE(15,aavg_nlon),COL=30,THICK=150
    GPLOT,X=aavg_longitude,Y=REPLICATE(25,aavg_nlon),COL=30,THICK=150
    GPLOT,X=REPLICATE(70,aavg_nlat),Y=aavg_latitude,COL=30,THICK=150
    GPLOT,X=REPLICATE(85,aavg_nlat),Y=aavg_latitude,COL=30,THICK=150
    FOR j=0,grid_nlon-1 DO BEGIN
        FOR k=0,grid_nlat-1 DO BEGIN
            IF ABS(corr_coeff(j,k,i)) gt sig95 THEN $
              GPLOT,X=grid_longitude(j),Y=grid_latitude(k),SYM=3,SIZE=25,COL=30,/NOLINES
        ENDFOR
    ENDFOR
    PSCLOSE,/NOVIEW
ENDFOR

STOP

END



