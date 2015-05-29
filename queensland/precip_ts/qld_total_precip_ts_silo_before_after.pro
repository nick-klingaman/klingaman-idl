PRO qld_total_precip_ts_silo_before_after

; File containing timeseries of monthly mean SILO rainfall
silo_basedir='/home/ss901165/datasets_mango/SILO/one_quarter/'
silo_files=[silo_basedir+'SILO.may_mmeans.1900-2011.precip.0.25x0.25.nc',$
            silo_basedir+'SILO.jun_mmeans.1900-2011.precip.0.25x0.25.nc',$
            silo_basedir+'SILO.jul_mmeans.1900-2011.precip.0.25x0.25.nc',$
            silo_basedir+'SILO.aug_mmeans.1900-2011.precip.0.25x0.25.nc',$
            silo_basedir+'SILO.sep_mmeans.1900-2011.precip.0.25x0.25.nc',$
            silo_basedir+'SILO.oct_mmeans.1900-2011.precip.0.25x0.25.nc',$
            silo_basedir+'SILO.nov_mmeans.1900-2011.precip.0.25x0.25.nc',$
            silo_basedir+'SILO.dec_mmeans.1900-2011.precip.0.25x0.25.nc',$
            silo_basedir+'SILO.jan_mmeans.1900-2011.precip.0.25x0.25.nc',$
            silo_basedir+'SILO.feb_mmeans.1900-2011.precip.0.25x0.25.nc',$
            silo_basedir+'SILO.mar_mmeans.1900-2011.precip.0.25x0.25.nc',$
            silo_basedir+'SILO.apr_mmeans.1900-2011.precip.0.25x0.25.nc']
silo_nfiles=N_ELEMENTS(silo_files)
silo_nyears=111
silo_offset=[0,0,0,0,0,0,0,0,1,1,1,1]

; Box over which to area-average
box=[-10,140,-28,152]

; Read latitude and longitude, find box of interest
silo_longitude=OPEN_AND_EXTRACT(silo_files(0),'longitude')
silo_latitude=OPEN_AND_EXTRACT(silo_files(0),'latitude')
DEFINE_BOUNDARIES,box,silo_latitude,silo_longitude,silo_box_tx,/LIMIT
silo_nlon=N_ELEMENTS(silo_longitude)
silo_nlat=N_ELEMENTS(silo_latitude)

; Read rainfall for each month, area-average and accumulate
silo_aavg_precip_ts=fltarr(silo_nyears)
FOR i=0,silo_nfiles-1 DO BEGIN
   this_month_precip=REFORM(OPEN_AND_EXTRACT(silo_files(i),$
                                             'rain',offset=[silo_box_tx(1),silo_box_tx(0),silo_offset(i)],$
                                             count=[silo_nlon,silo_nlat,silo_nyears]))
   this_month_precip[where(this_month_precip ge 1E10)]=!Values.F_NaN
   FOR j=0,silo_nyears-1 DO $
      silo_aavg_precip_ts(j)=MEAN(this_month_precip(*,*,j),/NaN)+silo_aavg_precip_ts(j)     
ENDFOR

silo_aavg_precip_ts=silo_aavg_precip_ts/FLOAT(silo_nfiles)
;silo_aavg_precip_ts=silo_aavg_precip_ts/MEAN(silo_aavg_precip_ts,/NaN)*100-100
silo_aavg_precip_ts=silo_aavg_precip_ts*180.
;silo_aavg_precip_ts_smooth=SMOOTH(silo_aavg_precip_ts,11)

silo_aavg_diff_after_before=fltarr(silo_nyears)
FOR i=0,silo_nyears-2 DO $
  silo_aavg_diff_after_before(i)=MEAN(silo_aavg_precip_ts(i+1:silo_nyears-1))-$
	MEAN(silo_aavg_precip_ts(0:i))

silo_aavg_diff_after_before=silo_aavg_diff_after_before/MEAN(silo_aavg_precip_ts,/NaN)*100

psfile='/home/ss901165/idl/queensland/precip_ts/qld_total_precip_ts_silo_before_after.silo025_may-apr.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2500,SPACE2=300,XOFFSET=2000,YOFFSET=500,TFONT=2,$
       TCHARSIZE=100,CB_WIDTH=110
GSET,XMIN=1900,XMAX=1900+silo_nyears,YMIN=-20,YMAX=80
;GSET,XMIN=1900,XMAX=2007,YMIN=0,YMAX=1200
red=FSC_COLOR("red",10)
black=FSC_COLOR("black",11)
HIST,X=indgen(silo_nyears)+1900.5,Y=silo_aavg_diff_after_before,WIDTH=50,FILLCOL=10
;GPLOT,X=indgen(silo_nyears-11)+1905.5,Y=silo_aavg_precip_ts_smooth(5:silo_nyears-6),COL=11,THICK=225
AXES,XSTEP=10,YSTEP=10,XTITLE="Year at beginning of May-April year",$
	YTITLE="Difference in mean after year minus mean before year (percentage change from long-term mean)"
;items=['11-year running mean of percentage anomalies','Percentage anomaly in area-average rainfall']
;blue=FSC_COLOR("blue",12)
;GPLOT,X=indgen(50)+1900,Y=REPLICATE(MEAN(silo_aavg_precip_ts(0:49)),50),COL=12,STYLE=1,THICK=200
;GPLOT,X=indgen(silo_nyears-50)+1950,Y=REPLICATE(MEAN(silo_aavg_precip_ts(52:silo_nyears-1)),silo_nyears-50),COL=12,STYLE=1,THICK=200
;AXES,XSTEP=10,YSTEP=100,XTITLE="Year at beginning of November-April season",YTITLE="November-April total rainfall over Queensland"
;items=['11-year running mean','Area-average total rainfall during November-April']
;GLEGEND,labels=items,COL=[11,10],LEGPOS=1
PSCLOSE,/NOVIEW


STOP

END
