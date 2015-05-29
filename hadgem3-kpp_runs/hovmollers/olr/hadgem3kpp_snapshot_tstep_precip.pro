PRO hadgem3kpp_snapshot_olr_precip

precip_file='/home/ss901165/um_output3/hadgem3_monwg/anbba/anbbaa.jun-sep_tsmeans.1982.dcvrain.nc'

;olr_file='/home/ss901165/um_output6/xjgxc/j1/olr_3hr.nc'
;sst_file='/home/ss901165/um_output6/xjgxc/j1/sst.nc'
;obs_file='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/n512/meto_ocean_analysis.jan-dec_dmean-interp-mmean_clim.1980-2009.sst.n512.nc'

box=[-20,40,20,100]

lon=OPEN_AND_EXTRACT(precip_file,'longitude')
lat=OPEN_AND_EXTRACT(precip_file,'latitude')
DEFINE_BOUNDARIES,box,lat,lon,box_tx,/LIMIT
nlon=N_ELEMENTS(lon)
nlat=N_ELEMENTS(lat)

;sst_lon=OPEN_AND_EXTRACT(sst_file,'longitude')
;sst_lat=OPEN_AND_EXTRACT(sst_file,'latitude')
;DEFINE_BOUNDARIES,box,sst_lat,sst_lon,sst_box_tx,/LIMIT
;sst_nlon=N_ELEMENTS(sst_lon)
;sst_nlat=N_ELEMENTS(sst_lat)

time_start=1
time_stop=time_start+45*8
n_time=(time_stop-time_start)
;n_time=2

precip=REFORM(OPEN_AND_EXTRACT(precip_file,'precip_1',$
                               offset=[box_tx(1),box_tx(0),time_start],$
                               count=[nlon,nlat,n_time]))*86400.
;sst=REFORM(OPEN_AND_EXTRACT(sst_file,'T',$
;                            offset=[sst_box_tx(1),sst_box_tx(0),0,9],$
;                            count=[sst_nlon,sst_nlat,1,n_time]))
;obs_sst=REFORM(OPEN_AND_EXTRACT(obs_file,'sst',$
;                                offset=[box_tx(1),box_tx(0),150],$
;                                count=[nlon,nlat,n_time/8]))
;FOR i=0,sst_nlon-1 DO $
;   FOR j=0,sst_nlat-1 DO $
;      FOR k=0,n_time/8-1 DO $
;         sst(i,j,k*8:(k+1)*8-1)=REFORM(sst(i,j,k*8:(k+1)*8-1))-REFORM(obs_sst(i,j,k))
;sst[where(sst ge 100)]=!Values.F_NaN
;FOR i=0,sst_nlon-1 DO $
;   FOR j=0,sst_nlat-1 DO $
;      sst(i,j,*)=sst(i,j,*)-MEAN(sst(i,j,*),/NaN)
;FOR i=0,n_time-1 DO BEGIN
;   FOR j=0,sst_nlon-1 DO $
;      sst(j,*,i)=SMOOTH(sst(j,*,i),11,/NaN)
;   FOR j=0,sst_nlat-1 DO $
;      sst(*,j,i)=SMOOTH(sst(*,j,i),11,/NaN)
;ENDFOR
;n_time=20
FOR i=0,n_time-1 DO BEGIN
   IF i lt 10 THEN BEGIN
      snap_time='00'+STRTRIM(STRING(i),1)
   ENDIF ELSE IF i lt 100 THEN BEGIN
      snap_time='0'+STRTRIM(STRING(i),1)
   ENDIF ELSE $
      snap_time=STRTRIM(STRING(i),1)
   
   print,snap_time
   precip_levs=['1','2','3','5','8','13','21','34','55','89','144','233','377','610']
;   sst_levs=['-1.0','-0.8','-0.6','-0.4','-0.2','0.2','0.4','0.6','0.8','1.0']

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/hovmollers/olr/hadgem3kpp_snapshot_tstep_precip.anbba_dcvrain.snap_time'+snap_time+'.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=140,YSIZE=15000,MARGIN=2000,YOFFSET=2000,SPACE2=1500,TCHARSIZE=130,XSIZE=15000
   MAP,LONMAX=box(3),LATMAX=box(2),LONMIN=box(1),LATMIN=box(0)
   CS,SCALE=26,NCOLS=N_ELEMENTS(precip_levs)+1,/REV,white=[2]
   LEVS,MANUAL=precip_levs
   CON,X=lon,Y=lat,FIELD=REFORM(precip(*,*,i)),/BLOCK,/NOLINES,TITLE='GA5 N512 tstep dconv precip for step = '+snap_time,CB_TITLE='Precipitation from deep convection (mm day!U-1!N)',CB_WIDTH=120
;   CS,SCALE=1,NCOLS=N_ELEMENTS(sst_levs)+1
;   LEVS,MANUAL=sst_levs
;   CON,X=sst_lon,Y=sst_lat,FIELD=REFORM(sst(*,*,i)),/NOFILL,/NOCOLBAR,COL=indgen(N_ELEMENTS(sst_levs)+2)+2,$
;       THICK=250
   AXES,XSTEP=30,YSTEP=10,XMINOR=10
   PSCLOSE,/NOVIEW
ENDFOR

STOP
END



