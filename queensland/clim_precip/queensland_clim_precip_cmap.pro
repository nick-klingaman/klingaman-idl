PRO queensland_clim_precip_cmap

infile='/home/ss901165/datasets/CMAP_PRECIP/climatology/CMAP_enhanced.1979-2007_clim.pentad.nc'

;box_plot=[-35,130,-10,160]
;box_read=[-40,95,0,185]

;box_plot=[-32,135,-10,165]
box_plot=[-40,105,-10,165]
box_read=[-45,95,0,185]

mylevs=['200','250','300','400','500','600','700','800','900','1000','1200','1400','1600','1800','2000','2500']

latitude=OPEN_AND_EXTRACT(infile,'lat')
longitude=OPEN_AND_EXTRACT(infile,'lon')
DEFINE_BOUNDARIES,box_read,latitude,longitude,box_read_tx,/LIMIT
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)

precip=OPEN_AND_EXTRACT(infile,'precip',$
                        offset=[box_read_tx(1),box_read_tx(0),0],$
                        count=[n_lon,n_lat,73])

annual_mean_precip=fltarr(n_lon,n_lat)
FOR i=0,n_lon-1 DO $
  FOR j=0,n_lat-1 DO $
  annual_mean_precip(i,j)=MEAN(precip(i,j,*))

psfile='/home/ss901165/idl/queensland/clim_precip/queensland_clim_precip_cmap.annual_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs
CON,FIELD=annual_mean_precip*365,X=longitude,Y=latitude,$
  TITLE="Annual-mean precipitation from CMAP (1979-2007)",/NOLINES
PSCLOSE

STOP
END
