PRO uk_cold_winters_regress_cet_swly

swwind_infile='/home/ss901165/datasets/20THC_REANALYSIS/wind_speed/20thc_reanalysis.dec_mmeans.1871-2008.swwind_index_850.nc.2'
cet_infile='/home/ss901165/datasets/20THC_REANALYSIS/temp_2m/20thc_reanalysis.jan-dec_mmeans.1870-2008.ctl_eng_temp.twod.nc'

swwind_longitude=OPEN_AND_EXTRACT(swwind_infile,'longitude')
swwind_latitude=OPEN_AND_EXTRACT(swwind_infile,'latitude')
swwind_nlon=N_ELEMENTS(swwind_longitude)
swwind_nlat=N_ELEMENTS(swwind_latitude)

cet=OPEN_AND_EXTRACT(cet_infile,'ctl_eng_temp',offset=[11,1],count=[1,138])
swwind=OPEN_AND_EXTRACT(swwind_infile,'swwind_index')

corr_cet_swwind=fltarr(swwind_nlon,swwind_nlat)
FOR i=0,swwind_nlon-1 DO BEGIN
   FOR j=0,swwind_nlat-1 DO BEGIN
      corr_cet_swwind(i,j)=CORRELATE(REFORM(swwind(i,j,*)),cet(*))
   ENDFOR
ENDFOR

mylevs=['-0.65','-0.55','-0.45','-0.35','-0.25','-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55','0.65']

psfile='/home/ss901165/idl/uk_cold_winters/uk_cold_winters_regress_cet_swly.dec_mmean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1000,SPACE2=900,XOFFSET=1000,YOFFSET=2000,$
       TFONT=2,TCHARSIZE=100,SPACE3=800
MAP,LONMIN=-90,LONMAX=30,LATMIN=MIN(swwind_latitude),LATMAX=MAX(swwind_latitude),/SECTOR
LEVS,MANUAL=mylevs
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
CON,X=swwind_longitude,Y=swwind_latitude,FIELD=corr_cet_swwind,$
    CB_TITLE='Correlation coefficient',/NOLINES
GPLOT,X=[-12,-12],Y=[50,55]
GPLOT,X=[-18,-18],Y=[50,55]
GPLOT,X=[-18,-12],Y=[50,50]
GPLOT,X=[-18,-12],Y=[55,55]
AXES
PSCLOSE

STOP
END


