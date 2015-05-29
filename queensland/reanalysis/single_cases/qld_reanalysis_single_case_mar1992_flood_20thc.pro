PRO qld_reanalysis_single_case_mar1992_flood_20thc

; Make plots of sea-level pressure and precipitation to be
; turned into an animation of the March 1992 Queensland floods.

; Data from 20th Century Reanalysis
twentyc_mslp_file='/home/ss901165/datasets_mango/20THC_REANALYSIS/mslp/by_year/20thc_reanalysis.jan-dec_dmeans.1992.mslp.nc'
twentyc_precip_file='/home/ss901165/datasets_mango/20THC_REANALYSIS/precip/by_year/20thc_reanalysis.jan-dec_dmeans.1992.precip.nc'
twentyc_u850_file='/home/ss901165/datasets_mango/20THC_REANALYSIS/zonal_wind/by_year/20thc_reanalysis.jan-dec_dmeans.1992.uvel850.nc'
twentyc_v850_file='/home/ss901165/datasets_mango/20THC_REANALYSIS/meridional_wind/by_year/20thc_reanalysis.jan-dec_dmeans.1992.vvel850.nc'

; SILO at same resolution
silo_precip_file='/home/ss901165/datasets_mango/SILO/t62/by_year/SILO.jan-dec_dmeans.1992.precip.t62.nc'

; Plotting domain
box=[-10,90,-50,180]
offset_time=69 ; Offset corresponding to 10 March 1992
first_date=10
ndays=10

; Read latitude and longitude and find box of interest
mslp_longitude=OPEN_AND_EXTRACT(twentyc_mslp_file,'lon')
mslp_latitude=OPEN_AND_EXTRACT(twentyc_mslp_file,'lat')
DEFINE_BOUNDARIES,box,mslp_latitude,mslp_longitude,mslp_box_tx,/LIMIT
mslp_nlon=N_ELEMENTS(mslp_longitude)
mslp_nlat=N_ELEMENTS(mslp_latitude)

; Read for precip
precip_longitude=OPEN_AND_EXTRACT(twentyc_precip_file,'longitude')
precip_latitude=OPEN_AND_EXTRACT(twentyc_precip_file,'latitude')
DEFINE_BOUNDARIES,box,precip_latitude,precip_longitude,precip_box_tx,/LIMIT
precip_nlon=N_ELEMENTS(precip_longitude)
precip_nlat=N_ELEMENTS(precip_latitude)

; Read for SILO
silo_longitude=OPEN_AND_EXTRACT(silo_precip_file,'longitude')
silo_latitude=OPEN_AND_EXTRACT(silo_precip_file,'latitude')
DEFINE_BOUNDARIES,box,silo_latitude,silo_longitude,silo_box_tx,/LIMIT
silo_nlon=N_ELEMENTS(silo_longitude)
silo_nlat=N_ELEMENTS(silo_latitude)

; Do the same for the U grid
twentyc_longitude_u=OPEN_AND_EXTRACT(twentyc_u850_file,'longitude')
twentyc_latitude_u=OPEN_AND_EXTRACT(twentyc_u850_file,'latitude')
DEFINE_BOUNDARIES,box,twentyc_latitude_u,twentyc_longitude_u,twentyc_box_tx_u,/LIMIT
twentyc_nlon_u=N_ELEMENTS(twentyc_longitude_u)
twentyc_nlat_u=N_ELEMENTS(twentyc_latitude_u)

twentyc_accum_precip=fltarr(precip_nlon,precip_nlat)
silo_accum_precip=fltarr(silo_nlon,silo_nlat)

mylevs_precip=[1,2,4,8,16,32,64,128]
mylevs_mslp=[980,984,988,992,996,1000,1004,1008,1012,1016,1020,1024,1028]
; For all dates, read data and make a map
FOR i=0,ndays-1 DO BEGIN
   
   twentyc_mslp=REFORM(OPEN_AND_EXTRACT(twentyc_mslp_file,'prmsl',$
                                       offset=[mslp_box_tx(1),mslp_box_tx(0),offset_time+i],$
                                       count=[mslp_nlon,mslp_nlat,1]))/100.
   twentyc_precip=REFORM(OPEN_AND_EXTRACT(twentyc_precip_file,'PRATE',$
                                         offset=[precip_box_tx(1),precip_box_tx(0),0,offset_time+i],$
                                         count=[precip_nlon,precip_nlat,1,1]))*86400.
   silo_precip=REFORM(OPEN_AND_EXTRACT(silo_precip_file,'rain',$
                                       offset=[silo_box_tx(1),silo_box_tx(0),offset_time+i],$
                                       count=[silo_nlon,silo_nlat,1]))
   twentyc_u850=REFORM(OPEN_AND_EXTRACT(twentyc_u850_file,'U',$
                                       offset=[twentyc_box_tx_u(1),twentyc_box_tx_u(0),0,offset_time+i],$
                                       count=[twentyc_nlon_u,twentyc_nlat_u,1,1]))
   twentyc_v850=REFORM(OPEN_AND_EXTRACT(twentyc_v850_file,'V',$
                                       offset=[twentyc_box_tx_u(1),twentyc_box_tx_u(0),0,offset_time+i],$
                                       count=[twentyc_nlon_u,twentyc_nlat_u,1,1]))
   
   psfile='/home/ss901165/idl/queensland/reanalysis/single_cases/qld_reanalysis_single_case_mar1992_flood.mslp_precip.'+$
          STRTRIM(STRING(first_date+i),1)+'Mar1992_20thc.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_precip)+1,/REV
   white=FSC_COLOR("white",2)
   LEVS,MANUAL=mylevs_precip
   CON,FIELD=twentyc_precip,X=precip_longitude,Y=precip_latitude,/NOLINES
   LEVS,MANUAL=mylevs_mslp
   CON,FIELD=twentyc_mslp,X=mslp_longitude,Y=mslp_latitude,$
       TITLE="Mean sea-level pressure (daily mean, hPa) and rainfall (daily total, mm) - 20th Century V2- "+$
       STRTRIM(STRING(first_date+i),1)+' Mar 1992',/NOFILL,CB_TITLE="Rainfall (mm)"
   VECT,U=twentyc_u850,V=twentyc_v850,X=twentyc_longitude_u,Y=twentyc_latitude_u,MAG=15,STRIDE=2

   PSCLOSE,/NOVIEW

   twentyc_accum_precip=twentyc_accum_precip+twentyc_precip
   silo_accum_precip=silo_accum_precip+silo_precip
ENDFOR

mylevs_precip_accum=['20','40','60','80','100','125','150','175','200']
psfile='/home/ss901165/idl/queensland/reanalysis/single_cases/qld_reanalysis_single_case_mar1992_flood.accum_precip.10-19Mar1992_20thc.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_precip_accum)+1,/REV
white=FSC_COLOR("white",2)
LEVS,MANUAL=mylevs_precip_accum
CON,FIELD=twentyc_accum_precip,X=precip_longitude,Y=precip_latitude,/NOLINES,$
    TITLE='Accumulated precipitation (mm) from 20th Century V2 - 10-19 Mar 1992',CB_TITLE='Rainfall (mm)'
PSCLOSE,/NOVIEW

silo_accum_precip_filled=fltarr(precip_nlon,precip_nlat)
for i=0,precip_nlon-1 DO BEGIN
   IF precip_longitude(i) lt MIN(silo_longitude) or precip_longitude(i) gt MAX(silo_longitude) THEN BEGIN
      silo_accum_precip_filled(i,*)=!Values.F_NaN
   ENDIF ELSE BEGIN
      for j=0,precip_nlat-1 DO BEGIN
         IF precip_latitude(j) lt MIN(silo_latitude) or precip_latitude(j) gt MAX(silo_latitude) THEN BEGIN
            silo_accum_precip_filled(i,j)=!Values.F_NaN
         ENDIF ELSE $
            silo_accum_precip_filled(i,j)=

psfile='/home/ss901165/idl/queensland/reanalysis/single_cases/qld_reanalysis_single_case_mar1992_flood.accum_precip.10-19Mar1992_silo.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_precip_accum)+1,/REV
white=FSC_COLOR("white",2)
LEVS,MANUAL=mylevs_precip_accum
CON,FIELD=silo_accum_precip,X=silo_longitude,Y=silo_latitude,/NOLINES,$
    TITLE='Accumulated precipitation (mm) from SILO on T62 grid - 10-19 Mar 1992',CB_TITLE='Rainfall (mm)'
PSCLOSE,/NOVIEW

;psfile='/home/ss901165/idl/queensland/reanalysis/single_cases/qld_reanalysis_single_case_mar1992_flood.accum_precip.10-19Mar1992_20thc-minus-silo.ps'
;PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
;MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES

STOP

END

