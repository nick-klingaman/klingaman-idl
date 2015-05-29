PRO hadgem3kpp_snapshot_olr_precip

precip_file='/home/ss901165/um_output6/xjgxc/j1/precip_3hr.nc'
olr_file='/home/ss901165/um_output6/xjgxc/j1/olr_3hr.nc'
sst_file='/home/ss901165/um_output6/xjgxc/j1/sst.nc'
obs_file='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/n512/meto_ocean_analysis.jan-dec_dmean-interp-mmean_clim.1980-2009.sst.n512.nc'

box=[-40,40,40,240]

lon=OPEN_AND_EXTRACT(precip_file,'longitude_1')
lat=OPEN_AND_EXTRACT(precip_file,'latitude_1')
DEFINE_BOUNDARIES,box,lat,lon,box_tx,/LIMIT
nlon=N_ELEMENTS(lon)
nlat=N_ELEMENTS(lat)

sst_lon=OPEN_AND_EXTRACT(sst_file,'longitude')
sst_lat=OPEN_AND_EXTRACT(sst_file,'latitude')
DEFINE_BOUNDARIES,box,sst_lat,sst_lon,sst_box_tx,/LIMIT
sst_nlon=N_ELEMENTS(sst_lon)
sst_nlat=N_ELEMENTS(sst_lat)

time_start=30*8
time_stop=time_start+45*8
n_time=(time_stop-time_start)
;n_time=2

precip=REFORM(OPEN_AND_EXTRACT(precip_file,'precip',$
                               offset=[box_tx(1),box_tx(0),0,time_start],$
                               count=[nlon,nlat,1,n_time]))*86400.
sst=REFORM(OPEN_AND_EXTRACT(sst_file,'T',$
                            offset=[sst_box_tx(1),sst_box_tx(0),0,9],$
                            count=[sst_nlon,sst_nlat,1,n_time]))
obs_sst=REFORM(OPEN_AND_EXTRACT(obs_file,'sst',$
                                offset=[box_tx(1),box_tx(0),150],$
                                count=[nlon,nlat,n_time/8]))
FOR i=0,sst_nlon-1 DO $
   FOR j=0,sst_nlat-1 DO $
      FOR k=0,n_time/8-1 DO $
         sst(i,j,k*8:(k+1)*8-1)=REFORM(sst(i,j,k*8:(k+1)*8-1))-REFORM(obs_sst(i,j,k))
sst[where(sst ge 100)]=!Values.F_NaN
FOR i=0,sst_nlon-1 DO $
   FOR j=0,sst_nlat-1 DO $
      sst(i,j,*)=sst(i,j,*)-MEAN(sst(i,j,*),/NaN)
FOR i=0,n_time-1 DO BEGIN
   FOR j=0,sst_nlon-1 DO $
      sst(j,*,i)=SMOOTH(sst(j,*,i),11,/NaN)
   FOR j=0,sst_nlat-1 DO $
      sst(*,j,i)=SMOOTH(sst(*,j,i),11,/NaN)
ENDFOR
;n_time=20
FOR i=0,n_time-1 DO BEGIN
   IF i lt 10 THEN BEGIN
      snap_time='00'+STRTRIM(STRING(i),1)
   ENDIF ELSE IF i lt 100 THEN BEGIN
      snap_time='0'+STRTRIM(STRING(i),1)
   ENDIF ELSE $
      snap_time=STRTRIM(STRING(i),1)
   
   print,snap_time
   precip_levs=['5','7','9','12','15','18','22','26','30','35','40','45','50','60','70','80','100']
   sst_levs=['-1.0','-0.8','-0.6','-0.4','-0.2','0.2','0.4','0.6','0.8','1.0']

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/hovmollers/olr/hadgem3kpp_snapshot_olr_precip.snap_time'+snap_time+'.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=160,YSIZE=12000,MARGIN=2000,YOFFSET=2000,SPACE2=1500,TCHARSIZE=130
   MAP,LONMAX=box(3),LATMAX=box(2),LONMIN=box(1),LATMIN=box(0)
   CS,SCALE=26,NCOLS=N_ELEMENTS(precip_levs)+1,/REV,white=[2]
   LEVS,MANUAL=precip_levs
   CON,X=lon,Y=lat,FIELD=REFORM(precip(*,*,i)),/BLOCK,/NOLINES,TITLE='GA3-KPP N512 with global coupling: 3-hr precip and SST for step = '+snap_time,CB_TITLE='Precipitation (mm day!U-1!N)'
   CS,SCALE=1,NCOLS=N_ELEMENTS(sst_levs)+1
   LEVS,MANUAL=sst_levs
   CON,X=sst_lon,Y=sst_lat,FIELD=REFORM(sst(*,*,i)),/NOFILL,/NOCOLBAR,COL=indgen(N_ELEMENTS(sst_levs)+2)+2,$
       THICK=250
   AXES,XSTEP=30,YSTEP=10,XMINOR=10
   PSCLOSE,/NOVIEW
ENDFOR

STOP
END



