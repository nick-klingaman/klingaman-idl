PRO hadgem3kpp_fcorr_compare_heat_contents_spatial

;kpp_infile='/home/ss901165/um_output6/xgspm/kpp_ocean/KPPocean.jan-dec_mmeans.i2-k1.nc'
;kpp_inst_infile='/home/ss901165/um_output6/xgspm/kpp_ocean/KPPocean.jan-dec_mmeans_frominst.i2-k1.nc'
;kpp_infile='/home/ss901165/um_output6/xgspt/KPPocean.jan-dec_mmeans.i2-i6.nc'
kpp_dir='/home/ss901165/um_output6/xihva'
kpp_infile=kpp_dir+'/KPPocean.jan-dec_mmeans.i2-j1.nc'
kpp_inst_infile=kpp_infile
lsm_ocndepth_file='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/bathymetry/lsm_ocndepth_for_kpp.n96.coastal_tiling.nearest_neighbor.um_73.nc'
atmos_file='/home/ss901165/um_output6/kpp_ocean/ancillaries/flux_data/temp.nc'
n_time=10

box=[-30,0,30,360]

longitude=OPEN_AND_EXTRACT(kpp_infile,'longitude')
latitude=OPEN_AND_EXTRACT(kpp_infile,'latitude')
DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)
z=OPEN_AND_EXTRACT(kpp_infile,'z')
nz=N_ELEMENTS(z)
h=OPEN_AND_EXTRACT(kpp_infile,'h')

inst_T=fltarr(n_lon,n_lat,nz)
inst_rho=fltarr(n_lon,n_lat,nz)
inst_cp=fltarr(n_lon,n_lat,nz)
vint_heat_content=fltarr(n_lon,n_lat,n_time)
vint_fcorr=fltarr(n_lon,n_lat,n_time*12)
dh_dt=fltarr(n_lon,n_lat,nz,n_time)
flux_in=fltarr(n_lon,n_lat,n_time*12)

oc_lon=OPEN_AND_EXTRACT(lsm_ocndepth_file,'longitude')
oc_lat=OPEN_AND_EXTRACT(lsm_ocndepth_file,'latitude')
DEFINE_BOUNDARIES,[MIN(latitude),MIN(longitude),MAX(latitude),MAX(longitude)],oc_lat,oc_lon,oc_box_tx,/LIMIT
ocdepth=OPEN_AND_EXTRACT(lsm_ocndepth_file,'max_depth',$
                         offset=[oc_box_tx(1),oc_box_tx(0)],$
                         count=[n_lon,n_lat])

atmos_lon=OPEN_AND_EXTRACT(atmos_file,'longitude')
atmos_lat=OPEN_AND_EXTRACT(atmos_file,'latitude')
DEFINE_BOUNDARIES,[MIN(latitude),MIN(longitude),MAX(latitude),MAX(longitude)],atmos_lat,atmos_lon,atmos_box_tx,/LIMIT
atmos_net=OPEN_AND_EXTRACT(atmos_file,'budget',$
                           offset=[atmos_box_tx(1),atmos_box_tx(0)],$
                           count=[n_lon,n_lat])
atmos_solar=OPEN_AND_EXTRACT(atmos_file,'swf',$
                             offset=[atmos_box_tx(1),atmos_box_tx(0)],$
                             count=[n_lon,n_lat])

mmean_fcorr=REFORM(OPEN_AND_EXTRACT(kpp_infile,'tinc_fcorr',$
                                    offset=[box_tx(1),box_tx(0),0,0],$
                                    count=[n_lon,n_lat,nz,n_time*12]))
mmean_rho=REFORM(OPEN_AND_EXTRACT(kpp_infile,'rho',$
                                  offset=[box_tx(1),box_tx(0),0,0],$
                                  count=[n_lon,n_lat,nz,n_time*12]))
mmean_cp=REFORM(OPEN_AND_EXTRACT(kpp_infile,'cp',$
                                 offset=[box_tx(1),box_tx(0),0,0],$
                                 count=[n_lon,n_lat,nz,n_time*12]))
mmean_solar=REFORM(OPEN_AND_EXTRACT(kpp_inst_infile,'solar_in',$
                                    offset=[box_tx(1),box_tx(0),0],$
                                    count=[n_lon,n_lat,n_time*12]))
mmean_nsolar=REFORM(OPEN_AND_EXTRACT(kpp_inst_infile,'nsolar_in',$
                                     offset=[box_tx(1),box_tx(0),0],$
                                     count=[n_lon,n_lat,n_time*12]))

FOR i=0,n_time-1 DO BEGIN
;   time=360*i+25
;   time=360*i+10
   time=360*i+30
;   IF (i eq n_time-1) THEN $
;      time=time-10
   IF time lt 100 THEN BEGIN
      time='00'+STRTRIM(STRING(time),1)
   ENDIF ELSE IF time lt 1000 THEN BEGIN
      time='0'+STRTRIM(STRING(time),1)
   ENDIF ELSE $
      time=STRTRIM(STRING(time),1)
   infile=kpp_dir+'/KPPocean_'+time+'_means.nc'
   inst_T(*,*,*)=REFORM(OPEN_AND_EXTRACT(infile,'T',$
                                           offset=[box_tx(1),box_tx(0),0,0],$
                                           count=[n_lon,n_lat,nz,1]))
   inst_rho(*,*,*)=REFORM(OPEN_AND_EXTRACT(kpp_infile,'rho',$
                                             offset=[box_tx(1),box_tx(0),0,0],$
                                             count=[n_lon,n_lat,nz,1]))
   inst_cp(*,*,*)=REFORM(OPEN_AND_EXTRACT(kpp_infile,'cp',$
                                          offset=[box_tx(1),box_tx(0),0,0],$
                                          count=[n_lon,n_lat,nz,1]))
   FOR j=0,n_lon-1 DO BEGIN
      FOR k=0,n_lat-1 DO BEGIN
         IF ocdepth(j,k) lt 2e10 THEN BEGIN
            FOR m=0,nz-1 DO BEGIN
               ;IF ABS(z(m)) le ABS(ocdepth(j,k)) THEN BEGIN
                  vint_heat_content(j,k,i)=vint_heat_content(j,k,i)+$
                                           inst_T(j,k,m)*inst_rho(j,k,m)*inst_cp(j,k,m)*h(m)
               ;ENDIF
            ENDFOR
            IF i gt 0 THEN $
               dh_dt(j,k,i-1)=vint_heat_content(j,k,i)-vint_heat_content(j,k,i-1)
         ENDIF
      ENDFOR
   ENDFOR
ENDFOR

FOR i=0,n_time*12-1 DO BEGIN
   FOR j=0,n_lon-1 DO BEGIN
      FOR k=0,n_lat-1 DO BEGIN
         IF ocdepth(j,k) lt 2e10 THEN BEGIN
            FOR m=0,nz-2 DO BEGIN
               ;IF ABS(z(m)) le ABS(ocdepth(j,k)) THEN BEGIN
                  vint_fcorr(j,k,i)=vint_fcorr(j,k,i)+mmean_fcorr(j,k,m,i)*$
                                    mmean_rho(j,k,m,i)*mmean_cp(j,k,m,i)*h(m)*720.*12.
               ;ENDIF
            ENDFOR            
            flux_in(j,k,i)=(mmean_solar(j,k,i)+mmean_nsolar(j,k,i))*86400.*360.
         ENDIF ELSE $
            atmos_net(j,k)=!Values.F_NaN
      ENDFOR
   ENDFOR
ENDFOR

dh_dt_mean=fltarr(n_lon,n_lat)
vint_fcorr_mean=fltarr(n_lon,n_lat)
flux_in_mean=fltarr(n_lon,n_lat)
solar_in_mean=fltarr(n_lon,n_lat)
FOR j=0,n_lon-1 DO BEGIN
   FOR k=0,n_lat-1 DO BEGIN
      dh_dt_mean(j,k)=MEAN(dh_dt(j,k,*))
      vint_fcorr_mean(j,k)=MEAN(vint_fcorr(j,k,*))
      flux_in_mean(j,k)=MEAN(flux_in(j,k,*))
      solar_in_mean(j,k)=MEAN(mmean_solar(j,k,*))
   ENDFOR
ENDFOR

dh_dt_levs=['-34','-30','-26','-22','-18','-14','-10','-6','-2','2','6','10','14','18','22','26','30','34']
psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_heat_contents_spatial.xihva_15day.dh_dt_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110
MAP,LONMIN=MIN(longitude),LATMIN=-26,LONMAX=MAX(longitude),LATMAX=26
CS,SCALE=1,NCOLS=N_ELEMENTS(dh_dt_levs)+1,white=[11]
LEVS,MANUAL=dh_dt_levs
CON,X=longitude,Y=latitude,FIELD=dh_dt_mean/1E6,CB_TITLE='Mean annual change in heat content (MJ)',$
    /NOLINES,/BLOCK,TITLE='Mean annual change in ocean heat content from xihva_15day (5 years)'
AXES
PSCLOSE,/NOVIEW

fcorr_levs=['-2250','-1950','-1650','-1350','-1050','-750','-450','-150','150','450','750','1050','1350','1650','1950','2250']
psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_heat_contents_spatial.xihva_15day.vint_fcorr_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110
MAP,LONMIN=MIN(longitude),LATMIN=-26,LONMAX=MAX(longitude),LATMAX=26
CS,SCALE=1,NCOLS=N_ELEMENTS(fcorr_levs)+1,white=[10]
LEVS,MANUAL=fcorr_levs
CON,X=longitude,Y=latitude,FIELD=vint_fcorr_mean/1E6,CB_TITLE='Mean annual change in heat content (MJ)',$
    /NOLINES,/BLOCK,TITLE='Mean annual change in ocean heat content due to flux correction for xihva_15day'
AXES
PSCLOSE,/NOVIEW

flux_in_levs=fcorr_levs
psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_heat_contents_spatial.xihva_15day.atmos_flux_in.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110
MAP,LONMIN=MIN(longitude),LATMIN=-26,LONMAX=MAX(longitude),LATMAX=26
CS,SCALE=1,NCOLS=N_ELEMENTS(flux_in_levs)+1,white=[10]
LEVS,MANUAL=flux_in_levs
CON,X=longitude,Y=latitude,FIELD=atmos_net*86400.*360./1E6,CB_TITLE='Mean annual change in heat content (MJ)',$
    /NOLINES,/BLOCK,TITLE='Mean annual change in ocean heat content from xgspj fluxes used for fcorr'
AXES
PSCLOSE,/NOVIEW

flux_diff_levs=['-340','-300','-260','-220','-180','-140','-100','-60','-20','20','60','100','140','180','220','260','300','340']
psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_heat_contents_spatial.xihva_15day.atmos_flux_in-minus-vint_fcorr.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110
MAP,LONMIN=MIN(longitude),LATMIN=-26,LONMAX=MAX(longitude),LATMAX=26
CS,SCALE=1,NCOLS=N_ELEMENTS(flux_diff_levs)+1,white=[11]
LEVS,MANUAL=flux_diff_levs
CON,X=longitude,Y=latitude,FIELD=(vint_fcorr_mean+atmos_net*86400.*360.)/1E6,CB_TITLE='Mean annual change in heat content (MJ)',$
    /NOLINES,/BLOCK,TITLE='Residual of flux correction minus net surface flux input (error in flux correction?)' 
AXES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_heat_contents_spatial.xihva_15day.flux_in_diff.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110
MAP,LONMIN=MIN(longitude),LATMIN=-26,LONMAX=MAX(longitude),LATMAX=26
CS,SCALE=1,NCOLS=N_ELEMENTS(flux_diff_levs)+1,white=[11]
LEVS,MANUAL=flux_diff_levs
CON,X=longitude,Y=latitude,FIELD=(flux_in_mean-atmos_net*86400.*360.)/1E6,CB_TITLE='Mean annual change in heat content (MJ)',$
    /NOLINES,/BLOCK,TITLE='Mean annual change in ocean heat content from difference in net heat input xihva_15day-xgspj'
AXES
PSCLOSE,/NOVIEW

flux_wmsq_diff_levs=['-34','-30','-26','-22','-18','-14','-10','-6','-2','2','6','10','14','18','22','26','30','34']
psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_heat_contents_spatial.xihva_15day.flux_in_diff_asflux.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110
MAP,LONMIN=MIN(longitude),LATMIN=-26,LONMAX=MAX(longitude),LATMAX=26
CS,SCALE=1,NCOLS=N_ELEMENTS(flux_diff_levs)+1,white=[11]
LEVS,MANUAL=flux_wmsq_diff_levs
CON,X=longitude,Y=latitude,FIELD=(flux_in_mean/86400./360.-atmos_net),CB_TITLE='Change in mean annual flux (W m!U-2!N)',$
    /NOLINES,/BLOCK,TITLE='Mean annual change in net surface heat input xihva_15day-xgspj'
AXES
PSCLOSE

psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_heat_contents_spatial.xihva_15day.solar_in_diff_asflux.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110
MAP,LONMIN=MIN(longitude),LATMIN=-26,LONMAX=MAX(longitude),LATMAX=26
CS,SCALE=1,NCOLS=N_ELEMENTS(flux_diff_levs)+1,white=[11]
LEVS,MANUAL=flux_wmsq_diff_levs
CON,X=longitude,Y=latitude,FIELD=(solar_in_mean-atmos_solar),CB_TITLE='Change in mean annual flux (W m!U-2!N)',$
    /NOLINES,/BLOCK,TITLE='Mean annual change in net shortwave flux xihva_15day-xgspj'
AXES
PSCLOSE

psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_heat_contents_spatial.xihva_15day.nsolar_in_diff_asflux.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110
MAP,LONMIN=MIN(longitude),LATMIN=-26,LONMAX=MAX(longitude),LATMAX=26
CS,SCALE=1,NCOLS=N_ELEMENTS(flux_diff_levs)+1,white=[11]
LEVS,MANUAL=flux_wmsq_diff_levs
CON,X=longitude,Y=latitude,FIELD=(flux_in_mean/86400./360.-solar_in_mean)-(atmos_net-atmos_solar),CB_TITLE='Change in mean annual flux (W m!U-2!N)',$
    /NOLINES,/BLOCK,TITLE='Mean annual change in net non-solar flux xihva_15day-xgspj'
AXES
PSCLOSE

psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_heat_contents_spatial.xihva_15day.dh_dt-minus-fcorr_error-minus-flux_diff.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110
MAP,LONMIN=MIN(longitude),LATMIN=-26,LONMAX=MAX(longitude),LATMAX=26
CS,SCALE=1,NCOLS=N_ELEMENTS(flux_diff_levs)+1,white=[11]
LEVS,MANUAL=flux_diff_levs
CON,X=longitude,Y=latitude,FIELD=(dh_dt_mean-(vint_fcorr_mean+atmos_net*86400.*360.)-(flux_in_mean-atmos_net*86400.*360.))/1E6,$
    CB_TITLE='Mean annual change in heat content (MJ)',$
    /NOLINES,/BLOCK,TITLE='Mean annual change'
AXES
PSCLOSE,/NOVIEW

STOP
END

