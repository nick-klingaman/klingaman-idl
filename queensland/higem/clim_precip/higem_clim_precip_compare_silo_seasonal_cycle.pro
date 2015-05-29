PRO higem_clim_precip_compare_silo_seasonal_cycle

; Daily climatologies
higem_xbylr_file='/home/ss901165/higem_qccce/hpcx_control_xbylr/higem_xbylr.jan-dec_dmeans_clim.h9-p9.precip.aus_domain.nc'
higem_eafeb_file='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.jan-dec_dmeans_clim.1979-2052.precip.aus_domain.nc'
silo_n144_file='/home/ss901165/datasets_mango/SILO/n144/SILO.jan-dec_dmeans_clim.1900-2008.n144.nc'
mask_n144_file='/home/ss901165/um_output/mask_n144.nc'

; Box to read
box=[-20,120,-12,147]
region_name='monsoon'

; Number of time points
higem_ntime=360
silo_ntime=365

; Contour levels
mylevs=['1','1.5','2','2.5','3','3.5','4','4.5','5','5.5','6','6.5','7','7.5','8','8.5','9','9.5','10']

; Grids
higem_latitude=OPEN_AND_EXTRACT(higem_xbylr_file,'latitude')
higem_longitude=OPEN_AND_EXTRACT(higem_xbylr_file,'longitude')
DEFINE_BOUNDARIES,box,higem_latitude,higem_longitude,higem_box_tx,/LIMIT
higem_nlon=N_ELEMENTS(higem_longitude)
higem_nlat=N_ELEMENTS(higem_latitude)

silo_latitude=OPEN_AND_EXTRACT(silo_n144_file,'latitude')
silo_longitude=OPEN_AND_EXTRACT(silo_n144_file,'longitude')
DEFINE_BOUNDARIES,box,silo_latitude,silo_longitude,silo_box_tx,/LIMIT
silo_nlon=N_ELEMENTS(silo_longitude)
silo_nlat=N_ELEMENTS(silo_latitude)

mask_latitude=OPEN_AND_EXTRACT(mask_n144_file,'latitude')
mask_longitude=OPEN_AND_EXTRACT(mask_n144_file,'longitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)

; Read precip
xbylr_precip=REFORM(OPEN_AND_EXTRACT(higem_xbylr_file,'precip',$
                                     offset=[higem_box_tx(1),higem_box_tx(0),0,0],$
                                     count=[higem_nlon,higem_nlat,1,higem_ntime]))*86400.
eafeb_precip=REFORM(OPEN_AND_EXTRACT(higem_eafeb_file,'precip',$
                                     offset=[higem_box_tx(1),higem_box_tx(0),0],$
                                     count=[higem_nlon,higem_nlat,higem_ntime]))*86400.
silo_precip=REFORM(OPEN_AND_EXTRACT(silo_n144_file,'rain',$
                                    offset=[silo_box_tx(1),silo_box_tx(0),0],$
                                    count=[silo_nlon,silo_nlat,silo_ntime]))

mask=REFORM(OPEN_AND_EXTRACT(mask_n144_file,'lsm',$
                             offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                             count=[mask_nlon,mask_nlat,1,1]))

mask_rev=fltarr(mask_nlon,mask_nlat)
FOR i=0,mask_nlat-1 DO $
   mask_rev(*,i)=mask(*,mask_nlat-i-1)

FOR i=0,higem_ntime-1 DO BEGIN
   temp=REFORM(xbylr_precip(*,*,i))
   temp[where(mask_rev eq 0)] = !Values.F_NaN
   xbylr_precip(*,*,i)=temp
   temp=REFORM(eafeb_precip(*,*,i))
   temp[where(mask_rev eq 0)] = !Values.F_NaN   
   eafeb_precip(*,*,i)=temp
ENDFOR
FOR i=0,silo_ntime-1 DO BEGIN
   temp=REFORM(silo_precip(*,*,i))
   temp[where(mask eq 0)]=!Values.F_NaN
   silo_precip(*,*,i)=temp
ENDFOR
;   silo_precip[where(silo_precip gt 1000)]=!Values.F_NaN

xbylr_precip_latavg=fltarr(higem_nlon,higem_ntime)
eafeb_precip_latavg=fltarr(higem_nlon,higem_ntime)
silo_precip_latavg=fltarr(silo_nlon,silo_ntime)
FOR i=0,higem_nlon-1 DO BEGIN
   FOR j=0,higem_ntime-1 DO BEGIN      
         xbylr_precip_latavg(i,j)=MEAN(xbylr_precip(i,*,j),/NaN)
         eafeb_precip_latavg(i,j)=MEAN(eafeb_precip(i,*,j),/NaN)
   ENDFOR
ENDFOR
FOR i=0,silo_nlon-1 DO $
   FOR j=0,silo_ntime-1 DO $
      silo_precip_latavg(i,j)=MEAN(silo_precip(i,*,j),/NaN)

higem_ticks=[0,30,60,90,120,150,180,210,240,270,300,330,360]
silo_ticks=[0,31,59,90,120,151,181,212,243,273,304,335,364]
ticknames=['1 Jan','1 Feb','1 Mar','1 Apr','1 May','1 Jun','1 Jul','1 Aug','1 Sep','1 Oct','1 Nov','1 Dec','30 Dec']

psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo_seasonal_cycle.higem_xbylr.'+region_name+'_region.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=1000,XOFFSET=2000,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,SPACE3=500
CS,SCALE=14,NCOLS=N_ELEMENTS(mylevs)+1,/REV
GSET,XMAX=MAX(higem_longitude),XMIN=MIN(higem_longitude),YMIN=higem_ntime,YMAX=0
LEVS,MANUAL=mylevs
CON,FIELD=xbylr_precip_latavg,X=higem_longitude,Y=indgen(higem_ntime),$
    TITLE='Daily climatology of rainfall in the '+region_name+' region from HiGEM xbylr (81 years), latitude-averaged 10-20S (land only)',$
    /NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
AXES,XSTEP=4,YTITLE="Day",XTITLE="Longitude (degrees east)",yvals=higem_ticks,ylabels=ticknames
PSCLOSE

psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo_seasonal_cycle.higem_eafeb.'+region_name+'_region.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=1000,XOFFSET=2000,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,SPACE3=500
CS,SCALE=14,NCOLS=N_ELEMENTS(mylevs)+1,/REV
GSET,XMAX=MAX(higem_longitude),XMIN=MIN(higem_longitude),YMIN=higem_ntime,YMAX=0
LEVS,MANUAL=mylevs
CON,FIELD=eafeb_precip_latavg,X=higem_longitude,Y=indgen(higem_ntime),$
    TITLE='Daily climatology of rainfall in the '+region_name+' region from HiGEM eafeb (74 years), latitude-averaged 10-20S (land only)',$
    /NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
AXES,XSTEP=4,YTITLE="Day",XTITLE="Longitude (degrees east)",yvals=higem_ticks,ylabels=ticknames
PSCLOSE

psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo_seasonal_cycle.silo_n144.'+region_name+'_region.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=1000,XOFFSET=2000,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,SPACE3=500
CS,SCALE=14,NCOLS=N_ELEMENTS(mylevs)+1,/REV
GSET,XMAX=MAX(silo_longitude),XMIN=MIN(silo_longitude),YMIN=silo_ntime,YMAX=0
LEVS,MANUAL=mylevs
CON,FIELD=silo_precip_latavg,X=silo_longitude,Y=indgen(silo_ntime),$
    TITLE='Daily climatology of rainfall in the '+region_name+' region from SILO (1900-2008), latitude-averaged 10-20S (land only)',$
    /NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
AXES,XSTEP=4,YTITLE="Day",XTITLE="Longitude (degrees east)",yvals=higem_ticks,ylabels=ticknames
PSCLOSE

   
STOP

END

