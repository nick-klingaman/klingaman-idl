PRO hadgem3kpp_mean_state_sst_bias

years=['i2','i3','i4','i5','i6','i7','i8','i9','j0','j1','j2','j3','j4','j5','j6','j7','j8','j9','k0']
nyears=N_ELEMENTS(years)
kpp_indir='/home/ss901165/um_output6/xihvd'
obs_file='/home/ss901165/datasets/METO_OCEAN_ANALYSIS/n96/meto_ocean_analysis.jan-dec_amean_clim.1980-2009.sst.n96.nc'

FOR i=0,nyears-1 DO BEGIN

   kpp_file=kpp_indir+'/kpp_'+years(i)+'.nc'
   kpp_lon=OPEN_AND_EXTRACT(kpp_file,'longitude')
   kpp_lat=OPEN_AND_EXTRACT(kpp_file,'latitude')
   nlon=N_ELEMENTS(kpp_lon)
   nlat=N_ELEMENTS(kpp_lat)
   IF i eq 0 THEN $
      kpp_sst=fltarr(nlon,nlat,nyears)
   
   box=[kpp_lat(0),kpp_lon(0),kpp_lat(nlat-1),kpp_lon(nlon-1)]
   obs_lon=OPEN_AND_EXTRACT(obs_file,'longitude')
   obs_lat=OPEN_AND_EXTRACT(obs_file,'latitude')
   DEFINE_BOUNDARIES,box,obs_lat,obs_lon,box_tx,/LIMIT
   
   kpp_sst(*,*,i)=REFORM(OPEN_AND_EXTRACT(kpp_file,'T',$
                                          offset=[0,0,0,0],count=[nlon,nlat,1,1]))
   obs_sst=OPEN_AND_EXTRACT(obs_file,'temp',$
                            offset=[box_tx(1),box_tx(0)],count=[nlon,nlat])-273.15
   
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/hadgem3kpp_mean_state_sst_bias.xihvd-minus-meto_analysis.amean_year'+years(i)+'.ps'
   PSOPEN,file=psfile,TFONT=2,FONT=2
   MAP,LONMIN=MIN(kpp_lon),LONMAX=MAX(kpp_lon),LATMIN=MIN(kpp_lat),LATMAX=MAX(kpp_lat)
   mylevs=['-2.2','-1.8','-1.4','-1.0','-0.6','-0.2','0.2','0.6','1.0','1.4','1.8','2.2']
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
   LEVS,MANUAL=mylevs
   sst_diff=REFORM(kpp_sst(*,*,i))-obs_sst
   CON,X=kpp_lon,Y=kpp_lat,FIELD=sst_diff,CB_TITLE='Difference in SST (K)',$
       TITLE='Difference in annual-mean SST between xihvd year '+years(i)+' (clim 15-day T,S) and Met Office analysis',/NOLINELABELS,POSITIVE_STYLE=2,NEGATIVE_STYLE=1
   AXES,XSTEP=30,YSTEP=10
   PSCLOSE,/NOVIEW
ENDFOR

psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/hadgem3kpp_mean_state_sst_bias.xihvd-minus-meto_analysis.amean_clim.ps'
PSOPEN,file=psfile,TFONT=2,FONT=2
MAP,LONMIN=MIN(kpp_lon),LONMAX=MAX(kpp_lon),LATMIN=MIN(kpp_lat),LATMAX=MAX(kpp_lat)
mylevs=['-2.2','-1.8','-1.4','-1.0','-0.6','-0.2','0.2','0.6','1.0','1.4','1.8','2.2']
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
LEVS,MANUAL=mylevs
sst_diff=fltarr(nlon,nlat)
FOR i=0,nlon-1 DO $
   FOR j=0,nlat-1 DO $
      sst_diff(i,j)=MEAN(kpp_sst(i,j,*))-obs_sst(i,j)
CON,X=kpp_lon,Y=kpp_lat,FIELD=sst_diff,CB_TITLE='Difference in SST (K)',$
    TITLE='Difference in annual-mean SST between xihvd (clim 15-day correction) and Met Office ocean analysis',/NOLINELABELS,POSITIVE_STYLE=2,NEGATIVE_STYLE=1
AXES,XSTEP=30,YSTEP=10
PSCLOSE

STOP
END

