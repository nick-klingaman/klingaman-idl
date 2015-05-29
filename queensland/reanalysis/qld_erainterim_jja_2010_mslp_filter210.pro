PRO qld_erainterim_jja_2010_mslp_filter210

; Input file
eraint_infile='/home/ss901165/datasets_mango/ERA-INTERIM/MSL/ERA_interim.jun-aug_dmeans.1989-2010.mslp_filter210.aus_domain.nc'
eraint_nyears=22
eraint_ntime=82

box=[-80,70,0,220]

eraint_longitude=OPEN_AND_EXTRACT(eraint_infile,'longitude')
eraint_latitude=OPEN_AND_EXTRACT(eraint_infile,'latitude')
DEFINE_BOUNDARIES,box,eraint_latitude,eraint_longitude,eraint_box_tx,/LIMIT
eraint_nlon=N_ELEMENTS(eraint_longitude)
eraint_nlat=N_ELEMENTS(eraint_latitude)

eraint_mslp_filter=OPEN_AND_EXTRACT(eraint_infile,'MSL',$
                                    offset=[eraint_box_tx(1),eraint_box_tx(0),0,0],$
                                    count=[eraint_nlon,eraint_nlat,eraint_nyears,eraint_ntime])/100.

eraint_mslp_stddev=fltarr(eraint_nlon,eraint_nlat,eraint_nyears)
eraint_mslp_stddev_clim=fltarr(eraint_nlon,eraint_nlat)
eraint_mslp_stddev_2010=fltarr(eraint_nlon,eraint_nlat)
FOR i=0,eraint_nlon-1 DO BEGIN
   FOR j=0,eraint_nlat-1 DO BEGIN
      FOR k=0,eraint_nyears-2 DO BEGIN
         eraint_mslp_stddev(i,j,k)=STDDEV(eraint_mslp_filter(i,j,k,*))
      ENDFOR
      eraint_mslp_stddev_clim(i,j)=MEAN(eraint_mslp_stddev(i,j,*))
      eraint_mslp_stddev_2010(i,j)=STDDEV(eraint_mslp_filter(i,j,eraint_nyears-1,*))
   ENDFOR
ENDFOR

mylevs_clim=['-9','-7','-5','-3','-1','1','3','5','7','9']
mylevs_ratio=['0.45','0.55','0.65','0.75','0.85','0.95','1.05','1.15','1.25','1.35','1.45','1.55','1.65']
mylevs_anom=['-2.25','-1.95','-1.65','-1.35','-1.05','-0.75','-0.45','-0.15',$
             '0.15','0.45','0.75','1.05','1.35','1.65','1.95','2.25']

psfile='/home/ss901165/idl/queensland/reanalysis/qld_erainterim_jja_2010_mslp_filter210.clim_stddev.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1500,SPACE2=800,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=90,CB_WIDTH=105,SPACE3=1000
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_clim)+1,white=[6]
MAP,/hires,LATMIN=box(0),LATMAX=box(2),/SH,LONMIN=box(1),LONMAX=box(3),/SECTOR
LEVS,MANUAL=REFORM(mylevs_clim)
CON,X=eraint_longitude,Y=eraint_latitude,FIELD=eraint_mslp_stddev_clim,/NOLINES,/BLOCK,$
    TITLE='Climatological 2-10 day filtered MSLP from ERA-Interim - JJA 1989-2010'
AXES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/reanalysis/qld_erainterim_jja_2010_mslp_filter210.2010_ratio_stddev.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1500,SPACE2=800,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=90,CB_WIDTH=105,SPACE3=1000
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_ratio)+1,white=[8]
MAP,/hires,LATMIN=box(0),LATMAX=box(2),/SH,LONMIN=box(1),LONMAX=box(3),/SECTOR
LEVS,MANUAL=REFORM(mylevs_ratio)
CON,X=eraint_longitude,Y=eraint_latitude,FIELD=eraint_mslp_stddev_2010/eraint_mslp_stddev_clim,/NOLINES,/BLOCK,$
    TITLE='Ratio of stddev in 2-10 day filtered MSLP from ERA-Interim for JJA 2010 against 1989-2009 climatology'
AXES
PSCLOSE

psfile='/home/ss901165/idl/queensland/reanalysis/qld_erainterim_jja_2010_mslp_filter210.2010_anom_stddev.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1500,SPACE2=800,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=90,CB_WIDTH=105,SPACE3=1000
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_anom)+1,white=[10]
MAP,/hires,LATMIN=box(0),LATMAX=box(2),/SH,LONMIN=box(1),LONMAX=box(3),/SECTOR
LEVS,MANUAL=REFORM(mylevs_anom)
CON,X=eraint_longitude,Y=eraint_latitude,FIELD=eraint_mslp_stddev_2010-eraint_mslp_stddev_clim,/NOLINES,/BLOCK,$
    TITLE='Anomalous stddev in 2-10 day filtered MSLP from ERA-Interim for JJA 2010 against 1989-2009 climatology'
AXES
PSCLOSE

STOP
END

