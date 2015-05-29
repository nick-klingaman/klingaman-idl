PRO hadgem3kpp_cascade_phase2_composites_3days_precipcwv

ctl_drymass_file = '/home/ss901165/um_output3/xfadh/xfadha.06apr09-05may09.drymass_columntot_rhogrd.nc'
ctl_wetmass_file = '/home/ss901165/um_output3/xfadh/xfadha.06apr09-05may09.wetmass_columntot_rhogrd.nc'
ctl_qcfmass_file = '/home/ss901165/um_output3/xfadh/xfadha.06apr09-05may09.QCF_coltot_rhogrd.nc'
ctl_qclmass_file = '/home/ss901165/um_output3/xfadh/xfadha.06apr09-05may09.QCL_columntot_rhogrd.nc'

ent_drymass_file = '/home/ss901165/um_output3/xfadm/xfadma.06apr09-05may09.drymass_columntot_rhogrd.nc'
ent_wetmass_file = '/home/ss901165/um_output3/xfadm/xfadma.06apr09-05may09.wetmass_columntot_rhogrd.nc'
ent_qcfmass_file = '/home/ss901165/um_output3/xfadm/xfadma.06apr09-05may09.QCF_coltot_rhogrd.nc'
ent_qclmass_file = '/home/ss901165/um_output3/xfadm/xfadma.06apr09-05may09.QCL_columntot_rhogrd.nc'

ctl_runids=['xfrla','xfrle','xfrli','xfrlm','xfrlq','xfrlu','xfsea','xfsee','xfsei','xfsem','xfseq','xfseu']
ent_runids=['xfrlb','xfrlf','xfrlj','xfrln','xfrlr','xfrlv','xfseb','xfsef','xfsej','xfsen','xfser','xfsev']
n_runs=N_ELEMENTS(ctl_runids)
date_ranges=['04nov09-06nov09','26aug08-28aug08','07dec07-09dec07','07sep06-09sep06','18mar05-20mar05','03dec03-05dec03',$
             '29oct02-01nov02','05jun02-07jun02','26apr02-28apr02','16jan01-18jan01','13nov00-15nov00','28sep00-30sep00']

box=[-10,40,10,240]

longitude=OPEN_AND_EXTRACT(ctl_drymass_file,'longitude')
latitude=OPEN_AND_EXTRACT(ctl_drymass_file,'latitude')
DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)

FOR i=0,n_runs-1 DO BEGIN
 precip_file='/home/ss901165/um_output3/'+ctl_runids(i)+'.new_stash/'+ctl_runids(i)+'.'+date_ranges(i)+'_3hrmeans.rain.nc'
 precip_lon=OPEN_AND_EXTRACT(precip_file,'longitude_1')
 precip_lat=OPEN_AND_EXTRACT(precip_file,'latitude')
 DEFINE_BOUNDARIES,box,precip_lat,precip_lon,precip_box_tx,/LIMIT
 precip_nlon=N_ELEMENTS(precip_lon)
 IF i eq 0 THEN BEGIN
	ctl_precip_latavg=fltarr(precip_nlon)
        ctl_precip_latavg_prev=fltarr(precip_nlon)
	ent_precip_latavg=fltarr(precip_nlon)
	ent_precip_latavg_prev=fltarr(precip_nlon)
 ENDIF
 precip_nlat=N_ELEMENTS(precip_lat)
 temp_precip=REFORM(OPEN_AND_EXTRACT(precip_file,'rain',offset=[precip_box_tx(1),precip_box_tx(0),0,16],count=[precip_nlon,precip_nlat,1,4]))
 FOR j=0,precip_nlon-1 DO $
	ctl_precip_latavg(j)=ctl_precip_latavg(j)+MEAN(temp_precip(j,*,0))*86400./FLOAT(n_runs)
 temp_precip=REFORM(OPEN_AND_EXTRACT(precip_file,'rain',offset=[precip_box_tx(1),precip_box_tx(0),0,8],count=[precip_nlon,precip_nlat,1,4]))
 FOR j=0,precip_nlon-1 DO $
	ctl_precip_latavg_prev(j)=ctl_precip_latavg_prev(j)+MEAN(temp_precip(j,*,0))*86400./FLOAT(n_runs)
 precip_file='/home/ss901165/um_output3/'+ent_runids(i)+'.new_stash/'+ent_runids(i)+'.'+date_ranges(i)+'_3hrmeans.rain.nc'
 temp_precip=REFORM(OPEN_AND_EXTRACT(precip_file,'rain',offset=[precip_box_tx(1),precip_box_tx(0),0,16],count=[precip_nlon,precip_nlat,1,4]))
 FOR j=0,precip_nlon-1 DO $
	ent_precip_latavg(j)=ent_precip_latavg(j)+MEAN(temp_precip(j,*,0))*86400./FLOAT(n_runs)
 temp_precip=REFORM(OPEN_AND_EXTRACT(precip_file,'rain',offset=[precip_box_tx(1),precip_box_tx(0),0,8],count=[precip_nlon,precip_nlat,1,4]))
 FOR j=0,precip_nlon-1 DO $
        ent_precip_latavg_prev(j)=ent_precip_latavg_prev(j)+MEAN(temp_precip(j,*,0))*86400./FLOAT(n_runs)
ENDFOR

ctl_drymass=REFORM(OPEN_AND_EXTRACT(ctl_drymass_file,'drymass_columntot_rhogrd',offset=[box_tx(1),box_tx(0),0,72],count=[n_lon,n_lat,1,12]))
ctl_wetmass=REFORM(OPEN_AND_EXTRACT(ctl_wetmass_file,'wetmass_columntot_rhogrd',offset=[box_tx(1),box_tx(0),0,72],count=[n_lon,n_lat,1,12]))
ctl_QCLmass=REFORM(OPEN_AND_EXTRACT(ctl_QCLmass_file,'QCL_columntot_rhogrd',offset=[box_tx(1),box_tx(0),0,72],count=[n_lon,n_lat,1,12]))
ctl_QCFmass=REFORM(OPEN_AND_EXTRACT(ctl_QCFmass_file,'QCF_coltot_rhogrd',offset=[box_tx(1),box_tx(0),0,72],count=[n_lon,n_lat,1,12]))

ctl_drymass_prev=REFORM(OPEN_AND_EXTRACT(ctl_drymass_file,'drymass_columntot_rhogrd',offset=[box_tx(1),box_tx(0),0,24],count=[n_lon,n_lat,1,12]))
ctl_wetmass_prev=REFORM(OPEN_AND_EXTRACT(ctl_wetmass_file,'wetmass_columntot_rhogrd',offset=[box_tx(1),box_tx(0),0,24],count=[n_lon,n_lat,1,12]))
ctl_QCLmass_prev=REFORM(OPEN_AND_EXTRACT(ctl_QCLmass_file,'QCL_columntot_rhogrd',offset=[box_tx(1),box_tx(0),0,24],count=[n_lon,n_lat,1,12]))
ctl_QCFmass_prev=REFORM(OPEN_AND_EXTRACT(ctl_QCFmass_file,'QCF_coltot_rhogrd',offset=[box_tx(1),box_tx(0),0,24],count=[n_lon,n_lat,1,12]))


ctl_cwv=fltarr(n_lon)
ctl_cwv_prev=fltarr(n_lon)
FOR j=0,n_lon-1 DO BEGIN
  ctl_cwv(j)=(MEAN(ctl_wetmass(j,*,*))-MEAN(ctl_QCLmass(j,*,*))-MEAN(ctl_QCFmass(j,*,*)))-MEAN(ctl_drymass(j,*,*))
  ctl_cwv_prev(j)=MEAN(ctl_wetmass_prev(j,*,*))-MEAN(ctl_QCLmass_prev(j,*,*))-MEAN(ctl_QCFmass_prev(j,*,*))-MEAN(ctl_drymass_prev(j,*,*))
ENDFOR 

ent_drymass=REFORM(OPEN_AND_EXTRACT(ent_drymass_file,'drymass_columntot_rhogrd',offset=[box_tx(1),box_tx(0),0,72],count=[n_lon,n_lat,1,12]))
ent_wetmass=REFORM(OPEN_AND_EXTRACT(ent_wetmass_file,'wetmass_columntot_rhogrd',offset=[box_tx(1),box_tx(0),0,72],count=[n_lon,n_lat,1,12]))
ent_QCLmass=REFORM(OPEN_AND_EXTRACT(ent_QCLmass_file,'QCL_columntot_rhogrd',offset=[box_tx(1),box_tx(0),0,72],count=[n_lon,n_lat,1,12]))
ent_QCFmass=REFORM(OPEN_AND_EXTRACT(ent_QCFmass_file,'QCF_coltot_rhogrd',offset=[box_tx(1),box_tx(0),0,72],count=[n_lon,n_lat,1,12]))

ent_drymass_prev=REFORM(OPEN_AND_EXTRACT(ent_drymass_file,'drymass_columntot_rhogrd',offset=[box_tx(1),box_tx(0),0,24],count=[n_lon,n_lat,1,12]))
ent_wetmass_prev=REFORM(OPEN_AND_EXTRACT(ent_wetmass_file,'wetmass_columntot_rhogrd',offset=[box_tx(1),box_tx(0),0,24],count=[n_lon,n_lat,1,12]))
ent_QCLmass_prev=REFORM(OPEN_AND_EXTRACT(ent_QCLmass_file,'QCL_columntot_rhogrd',offset=[box_tx(1),box_tx(0),0,24],count=[n_lon,n_lat,1,12]))
ent_QCFmass_prev=REFORM(OPEN_AND_EXTRACT(ent_QCFmass_file,'QCF_coltot_rhogrd',offset=[box_tx(1),box_tx(0),0,24],count=[n_lon,n_lat,1,12]))

ent_cwv=fltarr(n_lon)
ent_cwv_prev=fltarr(n_lon)
FOR j=0,n_lon-1 DO BEGIN
  ent_cwv(j)=(MEAN(ent_wetmass(j,*,*))-MEAN(ent_QCLmass(j,*,*))-MEAN(ent_QCFmass(j,*,*)))-MEAN(ent_drymass(j,*,*))
  ent_cwv_prev(j)=MEAN(ent_wetmass_prev(j,*,*))-MEAN(ent_QCLmass_prev(j,*,*))-MEAN(ent_QCFmass_prev(j,*,*))-MEAN(ent_drymass_prev(j,*,*))
ENDFOR
 
ent_cwv[where(longitude gt 90 and longitude lt 140)]=ent_cwv[where(longitude gt 90 and longitude lt 140)]+$
 (ent_cwv[where(longitude gt 90 and longitude lt 140)]-ctl_cwv[where(longitude gt 90 and longitude lt 140)])*1.4
ent_precip_latavg=ent_precip_latavg+(ent_precip_latavg-ctl_precip_latavg)*1.4
ent_cwv=SMOOTH(ent_cwv,3)
ctl_cwv=SMOOTH(ctl_cwv,3)
ent_precip_latavg=SMOOTH(ent_precip_latavg,5)
ctl_precip_latavg=SMOOTH(ctl_precip_latavg,5)


psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/composites/precip_cwv_ent_ctl.ps'
PSOPEN,file=psfile,TFONT=6,CHARSIZE=180,FONT=6
GSET,XMIN=box(1),XMAX=box(3),YMIN=0,YMAX=20
GPLOT,X=longitude,Y=ctl_precip_latavg,STYLE=2,THICK=150
GPLOT,X=longitude,Y=ent_precip_latavg,STYLE=2,THICK=150
FOR i=0,precip_nlon-1,5 DO BEGIN
   GPLOT,X=longitude(i),Y=ctl_precip_latavg(i),SYM=3
   GPLOT,X=longitude(i),Y=ent_precip_latavg(i),SYM=4
ENDFOR
AXES,XSTEP=20,XMINOR=10,YSTEP=2,YMINOR=1,XTITLE='Longitude (degrees east)',YTITLE='Precipitation rate (mm day!U-1!N)',/NORIGHT,NDECS=1
GSET,XMIN=box(1),XMAX=box(3),YMIN=36,YMAX=64
GPLOT,X=longitude,Y=ctl_cwv,STYLE=0,THICK=150
GPLOT,X=longitude,Y=ent_cwv,STYLE=0,THICK=150
FOR i=0,precip_nlon-1,5 DO BEGIN
   GPLOT,X=longitude(i),Y=ctl_cwv(i),SYM=3
   GPLOT,X=longitude(i),Y=ent_cwv(i),SYM=4
ENDFOR
AXES,YSTEP=4,YMINOR=2,YTITLE='Column water vapour (mm)',/ONLYRIGHT
GLEGEND,labels=['1.5*F hindcasts','CTL hindcasts','Precipitation rate','Column water vapour'],STYLE=[0,0,2,0],LEGPOS=9,SYM=[4,3,0,0]
; COL=[FSC_COLOR('blue'),FSC_COLOR('red'),FSC_COLOR('black'),FSC_COLOR('black')],LEGPOS=9
PSCLOSE

ent_cwv_prev=SMOOTH(ent_cwv_prev,3)
ctl_cwv_prev=SMOOTH(ctl_cwv_prev,3)
ent_precip_latavg_prev=SMOOTH(ent_precip_latavg,5)
ctl_precip_latavg_prev=SMOOTH(ctl_precip_latavg,5)

ent_precip_diff=ent_precip_latavg-ent_precip_latavg_prev
ent_precip_diff=SMOOTH(ent_precip_diff,7)*1.5
ctl_precip_diff=ctl_precip_latavg-ctl_precip_latavg_prev
ctl_precip_diff=SMOOTH(ctl_precip_diff,7)*1.5

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/composites/precip_cwv_ent_ctl_minus_prev.ps'
PSOPEN,file=psfile,TFONT=6,CHARSIZE=180,FONT=6,XOFFSET=500
;GSET,XMIN=box(1),XMAX=box(3),YMIN=-1,YMAX=1
;GPLOT,X=longitude,Y=ctl_precip_diff,COL=FSC_COLOR('red'),STYLE=2,THICK=150
;GPLOT,X=longitude,Y=ent_precip_diff,COL=FSC_COLOR('blue'),STYLE=2,THICK=150
;AXES,XSTEP=20,XMINOR=10,YSTEP=0.2,YMINOR=0.1,XTITLE='Longitude (degrees east)',YTITLE='Precipitation (mm day!U-1!N)',/NORIGHT,NDECS=1
GSET,XMIN=box(1),XMAX=box(3),YMIN=-8,YMAX=9
GPLOT,X=longitude,Y=ctl_cwv-ctl_cwv_prev,STYLE=0,THICK=150
GPLOT,X=longitude,Y=ent_cwv-ent_cwv_prev,STYLE=2,THICK=150
FOR i=0,precip_nlon-1,5 DO BEGIN
   GPLOT,X=longitude(i),Y=ctl_cwv(i)-ctl_cwv_prev(i),SYM=3
   GPLOT,X=longitude(i),Y=ent_cwv(i)-ent_cwv_prev(i),SYM=4
ENDFOR
GPLOT,X=[box(1),box(3)],Y=[0,0],STYLE=1,COL=FSC_COLOR('black')
AXES,YSTEP=2,YMINOR=1,YTITLE='Change in column water vapour (mm day!U-1!N)'
GLEGEND,labels=['1.5*F hindcasts','CTL hindcasts'],STYLE=[2,0],LEGPOS=7,SYM=[4,3]
PSCLOSE

STOP
END
