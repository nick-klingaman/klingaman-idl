PRO hadgem3kpp_cascade_criticalrh_mean_stddev
  
; Compare diagnosed values of critical RH from control and 1.5x entrainment runs

um3='/home/ss901165/um_output3'
control_runid='xfyfc'
entrain_runid='xfyfa'

box=[-5,40,5,200]
time_offset=0
n_times=24
n_z=85

rh_levels=['0.57','0.60','0.63','0.66','0.69','0.72','0.75','0.78','0.81','0.84','0.87','0.90','0.93','0.96','0.99']
rh_levels_diff=['-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13']
rh_levels_stddev=['0.03','0.06','0.09','0.12','0.15','0.18','0.21']
rh_levels_ratio=['0.35','0.45','0.55','0.65','0.75','0.85','0.95','1.05','1.18','1.33','1.54','1.82','2.22','2.86']

control_infile=um3+'/'+control_runid+'.new_stash/'+control_runid+'a.04nov09-13nov09.critical_rh_rholvl.nc'
entrain_infile=um3+'/'+entrain_runid+'.new_stash/'+entrain_runid+'a.04nov09-13nov09.critical_rh_rholvl.nc'

rholvl_pres_file=um3+'/xfrla.new_stash/xfrla.04nov09-06nov09_3hrmeans.p_rholvl_after_ts.nc'

longitude=OPEN_AND_EXTRACT(control_infile,'longitude_1')
latitude=OPEN_AND_EXTRACT(control_infile,'latitude')
DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)

control_critical_rh=REFORM(OPEN_AND_EXTRACT(control_infile,'C3H7OOH',$
                                            offset=[box_tx(1),box_tx(0),0,time_offset],$
                                            count=[n_lon,n_lat,n_z,n_times]))
entrain_critical_rh=REFORM(OPEN_AND_EXTRACT(entrain_infile,'C3H7OOH',$
                                               offset=[box_tx(1),box_tx(0),0,time_offset],$
                                               count=[n_lon,n_lat,n_z,n_times]))

rholvl_longitude=OPEN_AND_EXTRACT(rholvl_pres_file,'longitude')
rholvl_latitude=OPEN_AND_EXTRACT(rholvl_pres_file,'latitude_1')
DEFINE_BOUNDARIES,box,rholvl_latitude,rholvl_longitude,rholvl_box_tx,/LIMIT
rholvl_nlon=N_ELEMENTS(rholvl_longitude)
rholvl_nlat=N_ELEMENTS(rholvl_latitude)
rholvl_pres=REFORM(OPEN_AND_EXTRACT(rholvl_pres_file,'p',$
                                    offset=[rholvl_box_tx(1),rholvl_box_tx(0),0,0],$
                                    count=[rholvl_nlon,rholvl_nlat,n_z,n_times]))
rholvl_pres_mean=fltarr(n_z)
FOR m=0,n_z-1 DO $
   rholvl_pres_mean(m)=MEAN(rholvl_pres(*,*,m,*))/100.

control_mean_latavg=fltarr(n_lon,n_z)
control_latavg=fltarr(n_lon,n_z,n_times)
control_stddev_latavg=fltarr(n_lon,n_z)
entrain_mean_latavg=fltarr(n_lon,n_z)
entrain_latavg=fltarr(n_lon,n_z,n_times)
entrain_stddev_latavg=fltarr(n_lon,n_z)

FOR i=0,n_lon-1 DO BEGIN
   FOR j=0,n_z-1 DO BEGIN
      FOR k=0,n_times-1 DO BEGIN
         control_latavg(i,j,k)=MEAN(control_critical_rh(i,*,j,k))
         entrain_latavg(i,j,k)=MEAN(entrain_critical_rh(i,*,j,k))
      ENDFOR
      control_mean_latavg(i,j)=MEAN(control_latavg(i,j,*))
      entrain_mean_latavg(i,j)=MEAN(entrain_latavg(i,j,*))
      control_stddev_latavg(i,j)=STDDEV(control_latavg(i,j,*))
      entrain_stddev_latavg(i,j)=STDDEV(entrain_latavg(i,j,*))
   ENDFOR
ENDFOR

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/critical_rh/hadgem3kpp_cascade_criticalrh_mean_stddev.control_latavg5S-5N.hours0-71.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT,CB_WIDTH=112
GSET,XMIN=MIN(longitude),XMAX=MAX(longitude),YMIN=MAX(rholvl_pres_mean),YMAX=100.
CS,SCALE=26,NCOLS=N_ELEMENTS(rh_levels)+1,/REV
LEVS,MANUAL=REFORM(rh_levels)
CON,X=longitude,Y=rholvl_pres_mean,FIELD=control_mean_latavg,/BLOCK,/NOLINES
LEVS,MANUAL=REFORM(rh_levels_stddev)
pink=FSC_COLOR("pink",30)
CON,X=longitude,Y=rholvl_pres_mean,FIELD=control_stddev_latavg,/NOFILL,/NOLINELABELS,THICK=indgen(N_ELEMENTS(rh_levels_stddev)+2)*25+50,COL=30,STYLE=0
AXES,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)',XVALS=indgen(17)*10+40,YVALS=indgen(19)*50+100
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/critical_rh/hadgem3kpp_cascade_criticalrh_mean_stddev.15xentrain_latavg5S-5N.hours0-71.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT,CB_WIDTH=112
GSET,XMIN=MIN(longitude),XMAX=MAX(longitude),YMIN=MAX(rholvl_pres_mean),YMAX=100.
CS,SCALE=26,NCOLS=N_ELEMENTS(rh_levels)+1,/REV
LEVS,MANUAL=REFORM(rh_levels)
CON,X=longitude,Y=rholvl_pres_mean,FIELD=entrain_mean_latavg,/BLOCK,/NOLINES
LEVS,MANUAL=REFORM(rh_levels_stddev)
pink=FSC_COLOR("pink",30)
CON,X=longitude,Y=rholvl_pres_mean,FIELD=entrain_stddev_latavg,/NOFILL,/NOLINELABELS,THICK=indgen(N_ELEMENTS(rh_levels_stddev)+2)*25+50,COL=30,STYLE=0
AXES,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)',XVALS=indgen(17)*10+40,YVALS=indgen(19)*50+100
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/critical_rh/hadgem3kpp_cascade_criticalrh_mean_stddev.15xentrain-minus-control_latavg5S-5N.hours0-71.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT,CB_WIDTH=112
GSET,XMIN=MIN(longitude),XMAX=MAX(longitude),YMIN=MAX(rholvl_pres_mean),YMAX=100.
CS,SCALE=26,NCOLS=N_ELEMENTS(rh_levels_diff)+1,/REV,white=[9]
LEVS,MANUAL=REFORM(rh_levels_diff)
CON,X=longitude,Y=rholvl_pres_mean,FIELD=entrain_mean_latavg-control_mean_latavg,/BLOCK,/NOLINES
AXES,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)',XVALS=indgen(17)*10+40,YVALS=indgen(19)*50+100
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/critical_rh/hadgem3kpp_cascade_criticalrh_mean_stddev.15xentrain-minus-control_stddev_latavg5S-5N.hours0-71.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT,CB_WIDTH=112
GSET,XMIN=MIN(longitude),XMAX=MAX(longitude),YMIN=MAX(rholvl_pres_mean),YMAX=100.
CS,SCALE=26,NCOLS=N_ELEMENTS(rh_levels_ratio)+1,/REV,white=[9]
LEVS,MANUAL=REFORM(rh_levels_ratio)
CON,X=longitude,Y=rholvl_pres_mean,FIELD=entrain_stddev_latavg/control_stddev_latavg,/BLOCK,/NOLINES
AXES,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)',XVALS=indgen(17)*10+40,YVALS=indgen(19)*50+100
PSCLOSE

STOP
END

