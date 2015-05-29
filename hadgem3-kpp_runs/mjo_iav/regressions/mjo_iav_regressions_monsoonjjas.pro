PRO mjo_iav_regressions_monsoonjjas

dmean_precip_file='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmeans.years1-77.precip.2.5x2.5.nc'
filter_precip_file='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmeans_anom-3harm_filter2080.1-77.precip.2.5x2.5.rearrange.nc'

trmm_precip_file='/home/ss901165/datasets/TRMM_3B42V6/n96/TRMM_3B42v6A.jan-dec_dmeans.1999-2011.n96.nc'

offset_jun1=150
offset_sep30=279
n_years=77
n_days=offset_sep30-offset_jun1+1

box=[-10,0,40,360]

longitude=OPEN_AND_EXTRACT(dmean_precip_file,'longitude')
latitude=OPEN_AND_EXTRACT(dmean_precip_file,'latitude')
DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)

trmm_longitude=OPEN_AND_EXTRACT(trmm_precip_file,'longitude')
trmm_latitude=OPEN_AND_EXTRACT(trmm_precip_file,'latitude')
DEFINE_BOUNDARIES,box,trmm_latitude,trmm_longitude,trmm_box_tx,/LIMIT
trmm_nlon=N_ELEMENTS(trmm_longitude)
trmm_nlat=N_ELEMENTS(trmm_latitude)
trmm_nyears=12

dmean_precip=OPEN_AND_EXTRACT(dmean_precip_file,'precip',$
                              offset=[box_tx(1),box_tx(0),0,offset_jun1],$
                              count=[n_lon,n_lat,n_years,n_days])*86400.
filter_precip=OPEN_AND_EXTRACT(filter_precip_file,'precip',$
                               offset=[box_tx(1),box_tx(0),offset_jun1,0],$
                               count=[n_lon,n_lat,n_days,n_years])

trmm_dmean_precip=OPEN_AND_EXTRACT(trmm_precip_file,'precip',$
                                   offset=[trmm_box_tx(1),trmm_box_tx(0),offset_jun1,0],$
                                   count=[trmm_nlon,trmm_nlat,n_days,trmm_nyears])
trmm_dmean_precip[where(trmm_dmean_precip ge 1000)]=!Values.F_NaN

smean_precip=fltarr(n_lon,n_lat,n_years)
trmm_smean_precip=fltarr(trmm_nlon,trmm_nlat,trmm_nyears)
ssdev_precip=fltarr(n_lon,n_lat,n_years)
trmm_ssdev_precip=fltarr(trmm_nlon,trmm_nlat,trmm_nyears)
ssdev_filter_precip=fltarr(n_lon,n_lat,n_years)
smean_filter_precip=fltarr(n_lon,n_lat,n_years)
FOR i=0,n_lon-1 DO BEGIN
   FOR j=0,n_lat-1 DO BEGIN
      FOR k=0,n_years-1 DO BEGIN
         smean_precip(i,j,k)=MEAN(dmean_precip(i,j,k,*))
         ssdev_precip(i,j,k)=STDDEV(dmean_precip(i,j,k,*))
         ssdev_filter_precip(i,j,k)=STDDEV(filter_precip(i,j,*,k))
         smean_filter_precip(i,j,k)=MEAN(filter_precip(i,j,*,k))
      ENDFOR
   ENDFOR
ENDFOR
FOR i=0,trmm_nlon-1 DO BEGIN
   FOR j=0,trmm_nlat-1 DO BEGIN
      FOR k=0,trmm_nyears-1 DO BEGIN
         trmm_smean_precip(i,j,k)=MEAN(trmm_dmean_precip(i,j,k,*),/NaN)
         trmm_ssdev_precip(i,j,k)=STDDEV(trmm_dmean_precip(i,j,k,*),/NaN)
      ENDFOR
   ENDFOR
ENDFOR

ssdev_precip=ssdev_precip/smean_precip
ssdev_filter_precip=ssdev_filter_precip/smean_precip
trmm_ssdev_precip=trmm_ssdev_precip/trmm_smean_precip
regress_smean_ssdev=fltarr(n_lon,n_lat)
regress_smean_ssdev_filter=fltarr(n_lon,n_lat)
regress_trmm_smean_ssdev=fltarr(trmm_nlon,trmm_nlat)

FOR i=0,n_lon-1 DO BEGIN
   FOR j=0,n_lat-1 DO BEGIN
;      regress_smean_ssdev(i,j)=REGRESS(REFORM(ssdev_precip(i,j,*)),REFORM(smean_precip(i,j,*)))
      regress_smean_ssdev(i,j)=REGRESS(REFORM(smean_precip(i,j,*)),REFORM(ssdev_precip(i,j,*)))
      regress_smean_ssdev_filter(i,j)=REGRESS(REFORM(smean_precip(i,j,*)),REFORM(ssdev_filter_precip(i,j,*)))
   ENDFOR
ENDFOR
FOR i=0,trmm_nlon-1 DO BEGIN
   FOR j=0,trmm_nlat-1 DO BEGIN
      regress_trmm_smean_ssdev(i,j)=REGRESS(REFORM(trmm_smean_precip(i,j,*)),REFORM(trmm_ssdev_precip(i,j,*)))
   ENDFOR
ENDFOR

;regress_levels=['-3.0','-2.6','-2.2','-1.8','-1.4','-1.0','-0.6','-0.2','0.2','0.4','1.0','1.4','1.8','2.2','2.6','3.0']
regress_levels=['-0.30','-0.26','-0.22','-0.18','-0.14','-0.10','-0.06','-0.02','0.02','0.06','0.10','0.14','0.18','0.22','0.26','0.30']

psfile='/home/ss901165/idl/hadgem3-kpp_runs/mjo_iav/regressions/mjo_iav_regressions_monsoonjjas.regress_smean_ssdev.ps'
PSOPEN,file=psfile,FONT=2,TFONT=2,SPACE2=1000,SPACE3=400,YOFFSET=1000,YSIZE=10000
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
CS,SCALE=1,NCOLS=N_ELEMENTS(regress_levels)+1,/REV
LEVS,MANUAL=regress_levels
regress_smean_ssdev[where(smean_precip(*,*,0) lt 1)]=!Values.F_NaN
CON,X=longitude,Y=latitude,FIELD=regress_smean_ssdev*2,/NOLINES
AXES
PSCLOSE

psfile='/home/ss901165/idl/hadgem3-kpp_runs/mjo_iav/regressions/mjo_iav_regressions_monsoonjjas.trmm_regress_smean_ssdev.ps'
PSOPEN,file=psfile,FONT=2,TFONT=2,SPACE2=1000,SPACE3=400,YOFFSET=1000,YSIZE=10000
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
CS,SCALE=1,NCOLS=N_ELEMENTS(regress_levels)+1,/REV
LEVS,MANUAL=regress_levels
regress_trmm_smean_ssdev[where(trmm_smean_precip(*,*,0) lt 0.5)]=!Values.F_NaN
CON,X=trmm_longitude,Y=trmm_latitude,FIELD=regress_trmm_smean_ssdev,/NOLINES
AXES
PSCLOSE

psfile='/home/ss901165/idl/hadgem3-kpp_runs/mjo_iav/regressions/mjo_iav_regressions_monsoonjjas.regress_smean_ssdev_filter.ps'
PSOPEN,file=psfile,FONT=2,TFONT=2,SPACE2=1000,SPACE3=400,YOFFSET=1000,YSIZE=10000
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
CS,SCALE=1,NCOLS=N_ELEMENTS(regress_levels)+1,/REV
LEVS,MANUAL=regress_levels
regress_smean_ssdev_filter[where(smean_precip(*,*,0) lt 1)]=!Values.F_NaN
CON,X=longitude,Y=latitude,FIELD=regress_smean_ssdev_filter*2,/NOLINES
AXES
PSCLOSE

STOP
END
