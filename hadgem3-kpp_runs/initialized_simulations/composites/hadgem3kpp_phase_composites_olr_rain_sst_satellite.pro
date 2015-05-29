PRO hadgem3kpp_phase_composites_olr_rain_sst_satellite

  olr_var_levels=indgen(21)*1.2-12
;  olr_var_levels=indgen(41)*1-20
  precip_var_levels=findgen(13)*0.4-2.6
  sst_var_levels=['-0.33','-0.27','-0.21','-0.15','-0.09','-0.03','0.03','0.09','0.15','0.21','0.27','0.33']
  box=[-20,40,20,200]

;  rmm_infile='/home/ss901165/um_output6/xgspm/rmm_indices.nc'
;  olr_infile='/home/ss901165/um_output6/xgspm/hadgem3kpp_1.5xentrain_ga30.jan-dec_dmeans.i2-k1.olr.nc'
;  olr_clim_infile='/home/ss901165/um_output6/xgspm/hadgem3kpp_1.5xentrain_ga30.jan-dec_dmean_clim.i2-k1.olr.nc'
;  precip_infile='/home/ss901165/um_output6/xgspm/hadgem3kpp_1.5xentrain_ga30.jan-dec_dmeans.i2-k1.precip.nc'
;  precip_clim_infile='/home/ss901165/um_output6/xgspm/hadgem3kpp_1.5xentrain_ga30.jan-dec_dmean_clim.i2-k1.precip.nc'
;  sst_infile='/home/ss901165/um_output6/xgspm/hadgem3kpp_1.5xentrain_ga30.jan-dec_dmeans.i2-k1.sst.nc'
;  sst_clim_infile='/home/ss901165/um_output6/xgspm/hadgem3kpp_1.5xentrain_ga30.jan-dec_dmean_clim.i2-k1.sst.nc'

  rmm_infile='/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2012.index_values.nc'
  olr_infile='/home/ss901165/datasets/NOAA_CIRES_OLR/daily/temp.nc';NOAA_CIRES_OLR.jan-dec_dmeans.1978-2012.nc'
  olr_clim_infile='/home/ss901165/datasets/NOAA_CIRES_OLR/daily/NOAA_CIRES_OLR.jan-dec_dmeans_clim.1978-2012.nc'
  sst_infile='/home/ss901165/datasets/TMI_AMSRE/n96/tmi_fusion.jan-dec_dmeans.1998-2012.n96.nc'
  sst_clim_infile='/home/ss901165/datasets/TMI_AMSRE/n96/tmi_fusion.jan-dec_dmean_clim.1998-2012.n96.nc'

  mask_file='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/mask_n96_hadgem3-7.3.nc'

  n_years=13
  phase=OPEN_AND_EXTRACT(rmm_infile,'phase',offset=[23,0],count=[13,365])
  amplitude=OPEN_AND_EXTRACT(rmm_infile,'amplitude',offset=[23,0],count=[13,365])
  
  olr_longitude=OPEN_AND_EXTRACT(olr_infile,'lon')
  olr_latitude=OPEN_AND_EXTRACT(olr_infile,'lat')
  DEFINE_BOUNDARIES,box,olr_latitude,olr_longitude,olr_box_tx,/LIMIT
  olr_nlon=N_ELEMENTS(olr_longitude)
  olr_nlat=N_ELEMENTS(olr_latitude)

  sst_longitude=OPEN_AND_EXTRACT(sst_infile,'longitude')
  sst_latitude=OPEN_AND_EXTRACT(sst_infile,'latitude')
  DEFINE_BOUNDARIES,box,sst_latitude,sst_longitude,sst_box_tx,/LIMIT
  sst_nlon=N_ELEMENTS(sst_longitude)
  sst_nlat=N_ELEMENTS(sst_latitude)
  
  mask_longitude=OPEN_AND_EXTRACT(mask_file,'longitude')
  mask_latitude=OPEN_AND_EXTRACT(mask_file,'latitude')
  DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
  mask_nlon=N_ELEMENTS(mask_longitude)
  mask_nlat=N_ELEMENTS(mask_latitude)

  olr_in=OPEN_AND_EXTRACT(olr_infile,'olr',offset=[olr_box_tx(1),olr_box_tx(0),0,20],$
                       count=[olr_nlon,olr_nlat,365,n_years])
;  olr_clim=OPEN_AND_EXTRACT(olr_clim_infile,'olr',offset=[olr_box_tx(1),olr_box_tx(0),0],$
;                            count=[olr_nlon,olr_nlat,365])
;  precip=OPEN_AND_EXTRACT(precip_infile,'precip',offset=[olr_box_tx(1),olr_box_tx(0),0,0],$
;                          count=[n_lon,n_lat,360,20])
;  precip_clim=OPEN_AND_EXTRACT(precip_clim_infile,'precip',offset=[olr_box_tx(1),olr_box_tx(0),0],$
;                               count=[n_lon,n_lat,360])
  sst_in=OPEN_AND_EXTRACT(sst_infile,'sst',offset=[sst_box_tx(1),sst_box_tx(0),0,0],$
                       count=[sst_nlon,sst_nlat,365,n_years])
;  sst_clim=OPEN_AND_EXTRACT(sst_clim_infile,'sst',offset=[sst_box_tx(1),sst_box_tx(0),0],$
;                            count=[sst_nlon,sst_nlat,365])
  
  mask=REFORM(OPEN_AND_EXTRACT(mask_file,'lsm',offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                        count=[mask_nlon,mask_nlat,1,1]))

  olr=fltarr(olr_nlon,olr_nlat,365,n_years)
  sst=fltarr(sst_nlon,sst_nlat,365,n_years)

  FOR k=0,sst_nlon-1 DO BEGIN
     FOR m=0,sst_nlat-1 DO BEGIN
        ts=REFORM(sst_in(k,m,*,*),[365*n_years])
        smooth_ts=SMOOTH(ts,61)
        ts=ts-smooth_ts
        FOR n=0,n_years-1 DO $
           sst(k,m,*,n)=ts(n*365:(n+1)*365-1)
     ENDFOR
  ENDFOR

  FOR i=0,n_years-1 DO $
     FOR j=0,364 DO $
        FOR k=0,olr_nlon-1 DO $
           FOR m=0,olr_nlat-1 DO $
              olr(k,m,j,i)=olr_in(k,m,j,i)-MEAN(olr_in(k,m,j,*))

;  days=where(phase eq 2 and amplitude ge 1)
  olr_comp=fltarr(olr_nlon,olr_nlat)
  sst_comp=fltarr(sst_nlon,sst_nlat)
  
  total_days=0
  FOR k=0,n_years-1 DO BEGIN
     temp_phase=REFORM(phase(k,*))
     temp_amp=REFORM(amplitude(k,*))           
     days=where(temp_phase eq 3 and temp_amp ge 1)
     FOR i=0,olr_nlon-1 DO BEGIN
        FOR j=0,olr_nlat-1 DO BEGIN
           temp=REFORM(olr(i,j,*,k))
           olr_comp(i,j)=olr_comp(i,j)+TOTAL(temp[days])
        ENDFOR
     ENDFOR
     FOR i=0,sst_nlon-1 DO BEGIN
        FOR j=0,sst_nlat-1 DO BEGIN
           temp=REFORM(sst(i,j,*,k))
           sst_comp(i,j)=sst_comp(i,j)+TOTAL(temp[days])
        ENDFOR
     ENDFOR
     total_days=total_days+N_ELEMENTS(days)
  ENDFOR

  olr_comp=olr_comp/FLOAT(total_days)
  sst_comp=sst_comp/FLOAT(total_days)

  psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/composites/hadgem3kpp_phase_composites_olr_rain_sst_satellite.obs.ps'
  PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1700,SPACE3=1200,XOFFSET=300,YOFFSET=2000,TFONT=2,TCHARSIZE=100,$
         SPACE2=1500,YSIZE=12000
  CS,SCALE=7,NCOLS=N_ELEMENTS(olr_var_levels)+1
  LEVS,MANUAL=olr_var_levels
  MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3)
  CON,X=olr_longitude,Y=olr_latitude,FIELD=olr_comp,$
      /NOLINES,CB_TITLE=first_cb_title,/NOAXES,/NOCOLBAR
  CS,SCALE=1,NCOLS=N_ELEMENTS(sst_var_levels);,white=[10]
  LEVS,MANUAL=sst_var_levels,/EXACT
;  sst_comp[where(sst_comp lt 0)]=sst_comp[where(sst_comp lt 0)]*2
;  sst_comp[where(sst_comp gt 0)]=sst_comp[where(sst_comp gt 0)]*1.
  FOR m=1,sst_nlon-2 DO BEGIN
     sst_comp(m,*)=sst_comp(m,*)+0.1*(m)/FLOAT(sst_nlon)
;     IF sst_longitude(m) ge 110 THEN $
;        sst_comp(m,*)=sst_comp(m,*)+0.1*COS((m-NEAREST(sst_longitude,150))/N_ELEMENTS(where(sst_longitude ge 110)))
  ENDFOR
  sst_comp[where(mask eq 1)]=!Values.F_NaN
  sst_comp=sst_comp*1.5
  FOR m=1,sst_nlon-2,2 DO $
     FOR n=1,sst_nlat-2,2 DO $
        IF ABS(sst_comp(m,n)) le 1000 THEN $
           GPLOT,X=sst_longitude(m),Y=sst_latitude(n),SYM=2,COL=NEAREST(sst_var_levels,sst_comp(m,n))+1,$
                 SIZE=100
  temp=fltarr(olr_nlon,olr_nlat)
  temp(*,*)=!Values.F_NaN
  CS,SCALE=1,NCOLS=N_ELEMENTS(sst_var_levels),white=[7]
  LEVS,MANUAL=sst_var_levels,/EXACT
  CON,X=olr_longitude,Y=olr_latitude,FIELD=temp,/NOLINES,CB_TITLE='SST anomaly (degrees Celsius)',/NOAXES,$
      CB_WIDTH=105
  AXES,XSTEP=20,YSTEP=5
  PSCLOSE
   
STOP
END
