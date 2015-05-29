PRO queensland_clim_precip_trmm

infile='/home/ss901165/datasets/TRMM_3B42V6/TRMM_3B42v6A.1999-2008_clim.jan-dec_daily.nc'

box_plot=[-32,135,-10,165]
;box_plot=[-40,105,-10,165]
box_read=[-45,95,0,185]

mylevs=['200','250','300','400','500','600','700','800','900','1000','1200','1400','1600','1800','2000','2500']

latitude=OPEN_AND_EXTRACT(infile,'latitude')
longitude=OPEN_AND_EXTRACT(infile,'longitude')
DEFINE_BOUNDARIES,box_read,latitude,longitude,box_read_tx,/LIMIT
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)

precip=OPEN_AND_EXTRACT(infile,'precip',$
                        offset=[box_read_tx(1),box_read_tx(0),0],$
                        count=[n_lon,n_lat,365])

annual_mean_precip=fltarr(n_lon,n_lat)
FOR i=0,n_lon-1 DO $
  FOR j=0,n_lat-1 DO $
  annual_mean_precip(i,j)=MEAN(precip(i,j,*))

psfile='/home/ss901165/idl/queensland/clim_precip/queensland_clim_precip_trmm.annual_total_queensland.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs
CON,FIELD=annual_mean_precip*365,X=longitude,Y=latitude,$
  TITLE="Annual-total precipitation from TRMM (1999-2008; mm)",/NOLINES
PSCLOSE

summer_precip=fltarr(n_lon,n_lat)
spring_precip=fltarr(n_lon,n_lat)
autumn_precip=fltarr(n_lon,n_lat)
winter_precip=fltarr(n_lon,n_lat)
summer_precip(*,*)=0.
spring_precip(*,*)=0.
autumn_precip(*,*)=0.
winter_precip(*,*)=0.
FOR i=0,n_lon-1 DO BEGIN
  FOR j=0,n_lat-1 DO BEGIN
      FOR k=334,364 DO $
        summer_precip(i,j)=summer_precip(i,j)+precip(i,j,k)
      FOR k=0,58 DO $
        summer_precip(i,j)=summer_precip(i,j)+precip(i,j,k)
      FOR k=59,150 DO $
        autumn_precip(i,j)=autumn_precip(i,j)+precip(i,j,k)
      FOR k=151,242 DO $
        winter_precip(i,j)=winter_precip(i,j)+precip(i,j,k)
      FOR k=243,333 DO $
        spring_precip(i,j)=spring_precip(i,j)+precip(i,j,k)
  ENDFOR
ENDFOR

mylevs_seasonal=['50','100','150','200','250','300','350','400','450','500','550','600','700','800','900','1000','1200']

psfile='/home/ss901165/idl/queensland/clim_precip/queensland_clim_precip_trmm.spring_total_queensland.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_seasonal)+1,/REV
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_seasonal
CON,FIELD=spring_precip,X=longitude,Y=latitude,$
  TITLE="Spring-total precipitation from TRMM (SON 1999-2008; mm)",/NOLINES
PSCLOSE

psfile='/home/ss901165/idl/queensland/clim_precip/queensland_clim_precip_trmm.summer_total_queensland.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_seasonal)+1,/REV
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_seasonal
CON,FIELD=summer_precip,X=longitude,Y=latitude,$
  TITLE="Summer-total precipitation from TRMM (DJF 1999-2008; mm)",/NOLINES
PSCLOSE

psfile='/home/ss901165/idl/queensland/clim_precip/queensland_clim_precip_trmm.autumn_total_queensland.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_seasonal)+1,/REV
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_seasonal
CON,FIELD=autumn_precip,X=longitude,Y=latitude,$
  TITLE="Autumn-total precipitation from TRMM (MAM 1999-2008; mm)",/NOLINES
PSCLOSE

psfile='/home/ss901165/idl/queensland/clim_precip/queensland_clim_precip_trmm.winter_total_queensland.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_seasonal)+1,/REV
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_seasonal
CON,FIELD=winter_precip,X=longitude,Y=latitude,$
  TITLE="Winter-total precipitation from TRMM (JJA 1999-2008; mm)",/NOLINES
PSCLOSE

STOP
END
