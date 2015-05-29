PRO qld_reanalysis_single_case_mar1992_flood

; Make plots of sea-level pressure and precipitation to be
; turned into an animation of the March 1992 Queensland floods.

; Data from ERA-Interim
eraint_mslp_file='/home/ss901165/datasets_mango/ERA-INTERIM/MSL/ERA_interim.nov-apr_dmeans.1991.mslp.aus_domain.nc'
eraint_precip_file='/home/ss901165/datasets_mango/ERA-INTERIM/TP/ERA_interim.nov-apr_dmeans.1991.precip.aus_domain.nc'
eraint_u850_file='/home/ss901165/datasets_mango/ERA-INTERIM/U850/ERA_interim.nov-apr_dmeans.1991.u850.aus_domain.nc'
eraint_v850_file='/home/ss901165/datasets_mango/ERA-INTERIM/V850/ERA_interim.nov-apr_dmeans.1991.v850.aus_domain.nc'

; Plotting domain
box=[-10,90,-50,180]
offset_time=129 ; Offset corresponding to 10 March 1992
first_date=10
ndays=10

; Read latitude and longitude and find box of interest
eraint_longitude=OPEN_AND_EXTRACT(eraint_mslp_file,'longitude')
eraint_latitude=OPEN_AND_EXTRACT(eraint_mslp_file,'latitude')
DEFINE_BOUNDARIES,box,eraint_latitude,eraint_longitude,eraint_box_tx,/LIMIT
eraint_nlon=N_ELEMENTS(eraint_longitude)
eraint_nlat=N_ELEMENTS(eraint_latitude)

; Do the same for the U grid
eraint_longitude_u=OPEN_AND_EXTRACT(eraint_u850_file,'longitude')
eraint_latitude_u=OPEN_AND_EXTRACT(eraint_u850_file,'latitude')
DEFINE_BOUNDARIES,box,eraint_latitude_u,eraint_longitude_u,eraint_box_tx_u,/LIMIT
eraint_nlon_u=N_ELEMENTS(eraint_longitude_u)
eraint_nlat_u=N_ELEMENTS(eraint_latitude_u)

mylevs_precip=[1,2,4,8,16,32,64,128]
mylevs_mslp=[980,984,988,992,996,1000,1004,1008,1012,1016,1020,1024,1028]
; For all dates, read data and make a map
FOR i=0,ndays-1 DO BEGIN
   
   eraint_mslp=REFORM(OPEN_AND_EXTRACT(eraint_mslp_file,'MSL',$
                                       offset=[eraint_box_tx(1),eraint_box_tx(0),offset_time+i],$
                                       count=[eraint_nlon,eraint_nlat,1]))/100.
   eraint_precip=REFORM(OPEN_AND_EXTRACT(eraint_precip_file,'TP',$
                                         offset=[eraint_box_tx(1),eraint_box_tx(0),offset_time+i],$
                                         count=[eraint_nlon,eraint_nlat,1]))*1000.
   eraint_u850=REFORM(OPEN_AND_EXTRACT(eraint_u850_file,'U',$
                                       offset=[eraint_box_tx_u(1),eraint_box_tx_u(0),offset_time+i],$
                                       count=[eraint_nlon_u,eraint_nlat_u,1]))
   eraint_v850=REFORM(OPEN_AND_EXTRACT(eraint_v850_file,'V',$
                                       offset=[eraint_box_tx_u(1),eraint_box_tx_u(0),offset_time+i],$
                                       count=[eraint_nlon_u,eraint_nlat_u,1]))
   
   psfile='/home/ss901165/idl/queensland/reanalysis/single_cases/qld_reanalysis_single_case_mar1992_flood.mslp_precip.'+$
          STRTRIM(STRING(first_date+i),1)+'Mar1992.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_precip)+1,/REV
   white=FSC_COLOR("white",2)
   LEVS,MANUAL=mylevs_precip
   CON,FIELD=eraint_precip,X=eraint_longitude,Y=eraint_latitude,/NOLINES
   LEVS,MANUAL=mylevs_mslp
   CON,FIELD=eraint_mslp,X=eraint_longitude,Y=eraint_latitude,$
       TITLE="Mean sea-level pressure (daily mean, hPa) and rainfall (daily total, mm) - ERA-Interim - "+$
       STRTRIM(STRING(first_date+i),1)+' Mar 1992',/NOFILL,CB_TITLE="Rainfall (mm)"
   VECT,U=eraint_u850,V=eraint_v850,X=eraint_longitude_u,Y=eraint_latitude_u,MAG=15,STRIDE=2

   PSCLOSE
ENDFOR

STOP

END

