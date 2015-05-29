PRO ncep_ncar_clim_precip_compare_silo
  
; Input directories for reanalysis, SILO 0.25 degree and SILO on reanalysis grid
;
; Reanalysis directory names:
;   NCEP/NCAR - /home/ss901165/datasets_mango/NCEP_REANALYSIS/precipitation
;   ERA-40    - /home/ss901165/datasets/ERA40/PRECIP
;   20th Cent - /home/ss901165/datasets_mango/20THC_REANALYSIS/precip
; 
; SILO on 0.25 degree grid: /home/ss901165/datasets_mango/SILO/one_quarter
;
; SILO on reanalysis grids:
;   NCEP/NCAR - /home/ss901165/datasets_mango/SILO/t62
;   ERA-40    - /home/ss901165/datasets_mango/SILO/era40_resolution
;   20th Cent - /home/ss901165/datasets_mango/SILO/t62 (same grid as NCEP/NCAR)

reanalysis_indir='/home/ss901165/datasets_mango/NCEP_REANALYSIS/precipitation'
silo_025_indir='/home/ss901165/datasets_mango/SILO/one_quarter'
silo_t62_indir='/home/ss901165/datasets_mango/SILO/t62'

seasons=['dec-feb','mar-may','jun-aug','sep-nov']
seasons_long=['December-February','March-May','June-August','September-November']
n_seasons=N_ELEMENTS(seasons)

box_plot=[-44,110,-5,160]
box=[-45,110,-5,160]

; Read in the land/sea mask from the reanalysis
; File names:
;   NCEP/NCAR - /home/ss901165/datasets_mango/NCEP_REANALYSIS/lsmask.t62_gauss.nc
;   ERA-40    - /home/ss901165/datasets/ERA40/era40_lsm.nc
;   20th Cent - /home/ss901165/datasets_mango/20THC_REANALYSIS/mask_t62.nc
;
; Variable names:
;   NCEP/NCAR - lon, lat, lsmask
;   ERA-40    - longitude, latitude, LSM
;   20th Cent - longitude, latitude, lsm

mask_t62_infile='/home/ss901165/datasets_mango/NCEP_REANALYSIS/lsmask.t62_gauss.nc'
mask_longitude=OPEN_AND_EXTRACT(mask_t62_infile,'lon')
mask_latitude=OPEN_AND_EXTRACT(mask_t62_infile,'lat')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)
mask=REFORM(OPEN_AND_EXTRACT(mask_t62_infile,'lsmask',$
                             offset=[mask_box_tx(1),mask_box_tx(0)],$
                             count=[mask_nlon,mask_nlat]))

mylevs_annual=['1.0','1.25','1.5','1.75','2.0','2.25','2.5','2.75','3.0','3.5','4.0','4.5','5.0','5.5','6.0']
mylevs_diff_annual=['-2.5','-2.0','-1.5','-1.25','-1.0','-0.75','-0.5','-0.25','0.0','0.25','0.5','0.75','1.0','1.25','1.5','2.0','2.5']
mylevs_seasonal=['0.4','0.8','1.2','1.6','2.0','2.4','2.8','3.2','4.0','4.8','5.6','6.4','7.2','8.0','8.8','9.6','10.4']
mylevs_diff_seasonal=['-8.5','-6.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','6.5','8.5']

FOR i=0,n_seasons-1 DO BEGIN

                                ; Filename convention for reanalysis precipitation
                                ;   NCEP/NCAR - reanalysis_indir+'/ncep-ncar_reanalysis.'+seasons(i)+'_smean_clim.1948-2007.precip.t62_gauss.nc'
                                ;   ERA-40    - reanalysis_indir+'/era40.'+seasons(i)+'_smean_clim.1958-2001.precip.aus_domain.nc'
                                ;   20th Cent - reanalysis_indir+'/20thc_reanalysis.'+seasons(i)+'_smean_clim.1900-2007.precip.nc'
   reanalysis_infile=reanalysis_indir+'/ncep-ncar_reanalysis.'+seasons(i)+'_smean_clim.1948-2007.precip.t62_gauss.nc'
   
                                ; Filename convention for SILO precipitation on reanalysis grid
                                ;   NCEP/NCAR - silo_t62_indir+'/SILO.'+seasons(i)+'_smean_clim.1948-2007.precip.t62.nc'
                                ;   ERA-40    - silo_era40_indir+'/SILO.'+seasons(i)+'_smean_clim.1958-2001.precip.era40.nc'
                                ;   20th Cent - silo_t62_indir+'/SILO.'+seasons(i)+'_smean_clim.1900-2007.precip.t62.nc'
   silo_t62_infile=silo_t62_indir+'/SILO.'+seasons(i)+'_smean_clim.1948-2007.precip.t62.nc'
   
                                ; Filename convention for SILO precipitation on its own grid
                                ; No need to change this when switching reanalyses.
   silo_025_infile=silo_025_indir+'/SILO_precip.'+seasons(i)+'_smean_clim.1900-2007.0.25x0.25.nc'
   
   IF i eq 0 THEN BEGIN
      silo_t62_longitude=OPEN_AND_EXTRACT(silo_t62_infile,'longitude')
      silo_t62_latitude=OPEN_AND_EXTRACT(silo_t62_infile,'latitude')
      DEFINE_BOUNDARIES,box,silo_t62_latitude,silo_t62_longitude,silo_t62_box_tx,/LIMIT
      silo_t62_nlon=N_ELEMENTS(silo_t62_longitude)
      silo_t62_nlat=N_ELEMENTS(silo_t62_latitude)

      silo_025_longitude=OPEN_AND_EXTRACT(silo_025_infile,'longitude')
      silo_025_latitude=OPEN_AND_EXTRACT(silo_025_infile,'latitude')
      DEFINE_BOUNDARIES,box,silo_025_latitude,silo_025_longitude,silo_025_box_tx,/LIMIT
      silo_025_nlon=N_ELEMENTS(silo_025_longitude)
      silo_025_nlat=N_ELEMENTS(silo_025_latitude)

      ; Read in the latitude-longitude grid for the reanalysis
      ; Variable names:
      ;   NCEP/NCAR - lon, lat
      ;   ERA-40    - longitude, latitude
      ;   20th Cent - longitude, latitude
      reanalysis_longitude=OPEN_AND_EXTRACT(reanalysis_infile,'lon')
      reanalysis_latitude=OPEN_AND_EXTRACT(reanalysis_infile,'lat')
      DEFINE_BOUNDARIES,box,reanalysis_latitude,reanalysis_longitude,reanalysis_box_tx,/LIMIT 
      DEFINE_BOUNDARIES,[silo_t62_latitude(silo_t62_nlat-1),silo_t62_longitude(0),$
                         silo_t62_latitude(0),silo_t62_longitude(silo_t62_nlon-1)],$
                        reanalysis_latitude,reanalysis_longitude,reanalysis_silo_box_tx
      reanalysis_nlon=N_ELEMENTS(reanalysis_longitude)
      reanalysis_nlat=N_ELEMENTS(reanalysis_latitude)      
      
   ENDIF

   ; Read in reanalysis precipitation
   ; Variable names and units
   ;   NCEP/NCAR - 'prate' (mm/sec)
   ;   ERA-40    - 'precip' (m/day) <- Not a typo: metres per day
   ;   20th Cent - 'PRATE' (mm/sec)
   reanalysis_clim_precip=REFORM(OPEN_AND_EXTRACT(reanalysis_infile,'prate',$
                                                  offset=[reanalysis_box_tx(1),reanalysis_box_tx(0)],$
                                                  count=[reanalysis_nlon,reanalysis_nlat]))

   ; Convert to mm/day
   reanalysis_clim_precip=reanalysis_clim_precip*86400.

   ; Read in SILO precipitation on reanalysis grid
   ; Variable name: rain
   ; Units: mm/day
   silo_t62_clim_precip=REFORM(OPEN_AND_EXTRACT(silo_t62_infile,'rain',$
                                                offset=[silo_t62_box_tx(1),silo_t62_box_tx(0)],$
                                                count=[silo_t62_nlon,silo_t62_nlat]))

   ; Read in SILO precipitation on 25 km (0.25 degree) grid
   silo_025_clim_precip=REFORM(OPEN_AND_EXTRACT(silo_025_infile,'rain',$
                                                offset=[silo_025_box_tx(1),silo_025_box_tx(0)],$
                                                count=[silo_025_nlon,silo_025_nlat]))

   ; Take difference between reanalysis and SILO rainfall
   diff_reanalysis_silo=fltarr(reanalysis_nlon,reanalysis_nlat)
   FOR j=0,reanalysis_nlon-1 DO BEGIN
      FOR k=0,reanalysis_nlat-1 DO BEGIN
                                ; Mask out ocean points and those points in the reanalysis 
                                ; that lie outside the bounds of the observations.
         IF (mask(j,k) ne 0) and j gt reanalysis_silo_box_tx(1) and j lt reanalysis_silo_box_tx(3) and $
            k gt reanalysis_silo_box_tx(0) and k lt reanalysis_silo_box_tx(2) THEN BEGIN
                                ; Shift observations to agree with reanalysis grid conventions
            diff_reanalysis_silo(j,k)=reanalysis_clim_precip(j,k)-silo_t62_clim_precip(j-reanalysis_silo_box_tx(1),k-reanalysis_silo_box_tx(0))
         ENDIF ELSE $
                                ; Set all ocean points and points outside the observations' boundaries
                                ; to a missing value (Not a Number).
            diff_reanalysis_silo(j,k)=!Values.F_NaN
      ENDFOR
   ENDFOR

                                ; Plot reanalysis precipitation
                                ; You will need to change the path to refer to your own directory 
                                ; (e.g., /home/sw00....) rather than mine.
                                ; You will need to do this for all three plots below.
   psfile='/home/ss901165/idl/queensland/ncep_ncar_clim_precip_compare_silo.'+seasons(i)+'_mean.ncep_ncar.1948-2007.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1000,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,XSIZE=18000
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_seasonal)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_seasonal
   CON,FIELD=reanalysis_clim_precip,X=reanalysis_longitude,Y=reanalysis_latitude,$
       TITLE=seasons_long(i)+'-mean rainfall from NCEP/NCAR reanalysis (1948-2007)',/NOLINES,$
       CB_TITLE='Rainfall (mm day!U-1!N)',/BLOCK
   AXES,XSTEP=10,YSTEP=5
   PSCLOSE,/NOVIEW 

                                ; Plot SILO precipitation on reanalysis grid

   psfile='/home/ss901165/idl/queensland/ncep_ncar_clim_precip_compare_silo.'+seasons(i)+'_mean.silo_t62.1948-2007.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1000,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=100,XSIZE=18000
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_seasonal)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_seasonal
   CON,FIELD=silo_t62_clim_precip,X=silo_t62_longitude,Y=silo_t62_latitude,$
       TITLE=seasons_long(i)+'-mean rain for SILO on NCEP/NCAR grid (1948-2007)',/NOLINES,$
       CB_TITLE='Rainfall (mm day!U-1!N)',/BLOCK
   AXES,XSTEP=10,YSTEP=5
   PSCLOSE,/NOVIEW

   ; Plot SILO precipitation its own grid
   
   psfile='/home/ss901165/idl/queensland/ncep_ncar_clim_precip_compare_silo.'+seasons(i)+'_mean.silo_025.1900-2007.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1000,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=100,XSIZE=18000
   CS,SCALE=22,NCOLS=N_ELEMENTS(mylevs_seasonal)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_seasonal
   CON,FIELD=silo_025_clim_precip,X=silo_025_longitude,Y=silo_025_latitude,$
       TITLE=seasons_long(i)+'-mean rain for SILO on its own 25 km grid (1900-2007)',/NOLINES,$
       CB_TITLE='Rainfall (mm day!U-1!N)',/BLOCK
   AXES,XSTEP=10,YSTEP=5
   PSCLOSE,/NOVIEW

   ; Add code to plot difference between reanalysis and SILO here ...
   
   
;   difference=silo_t62_clim_precip-reanalysis_clim_precip

   psfile='/home/ss901165/idl/queensland/ncep_ncar_clim_precip_compare_silo.'+seasons(i)+'_difference.1900-2007.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1000,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=100,XSIZE=18000
   CS,SCALE=22,NCOLS=N_ELEMENTS(mylevs_seasonal)+1,/REV,WHITE=[17,18]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_diff_seasonal
   CON,FIELD=diff_reanalysis_silo,X=reanalysis_longitude,Y=reanalysis_latitude,$
       TITLE=seasons_long(i)+'- Difference between observations and NCEP/NCAR reanalysis (1948-2007)',/NOLINES,$
       CB_TITLE='Rainfall (mm day!U-1!N)',/BLOCK
   AXES,XSTEP=10,YSTEP=5
   PSCLOSE,/NOVIEW

ENDFOR

; Add code to read and plot annual means here ... 
; using 'jan-dec_amean_clim' instead of seasons(i)+'smean_clim' in
; filenames

 reanalysis_infile_amean=reanalysis_indir+'/ncep-ncar_reanalysis.jan-dec_amean_clim.1948-2007.precip.t62_gauss.nc'
 reanalysis_clim_precip_amean=REFORM(OPEN_AND_EXTRACT(reanalysis_infile_amean,'prate',$
                                                  offset=[reanalysis_box_tx(1),reanalysis_box_tx(0)],$
                                                  count=[reanalysis_nlon,reanalysis_nlat]))
 reanalysis_clim_precip_amean=reanalysis_clim_precip_amean*86400.

 psfile='/home/ss901165/idl/queensland/ncep_ncar_clim_precip_compare_silo_amean_reanalysis.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1000,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=100,XSIZE=18000
   CS,SCALE=22,NCOLS=N_ELEMENTS(mylevs_seasonal)+1,/REV,WHITE=[17,18]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_annual
   CON,FIELD=reanalysis_clim_precip_amean,X=reanalysis_longitude,Y=reanalysis_latitude,$
       TITLE='- Annual Mean Precipitation NCEP/NCAR reanalysis (1948-2007)',/NOLINES,$
       CB_TITLE='Rainfall (mm day!U-1!N)',/BLOCK
   AXES,XSTEP=10,YSTEP=5
  PSCLOSE

 silo_t62_infile_amean=silo_t62_indir+'/SILO.jan-dec_amean_clim.1948-2007.precip.t62.nc'
 silo_t62_clim_precip_amean=REFORM(OPEN_AND_EXTRACT(silo_t62_infile_amean,'rain',$
                                                offset=[silo_t62_box_tx(1),silo_t62_box_tx(0)],$
                                                count=[silo_t62_nlon,silo_t62_nlat]))
 psfile='/home/ss901165/idl/queensland/ncep_ncar_clim_precip_compare_silo_amean_obs.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1000,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=100,XSIZE=18000
   CS,SCALE=22,NCOLS=N_ELEMENTS(mylevs_seasonal)+1,/REV,WHITE=[17,18]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_annual
   CON,FIELD=reanalysis_clim_precip_amean,X=silo_t62_longitude,Y=silo_t62_latitude,$
       TITLE='- Annual Mean Precipitation from Observations (1948-2007)',/NOLINES,$
       CB_TITLE='Rainfall (mm day!U-1!N)',/BLOCK
   AXES,XSTEP=10,YSTEP=5
  PSCLOSE


STOP

END

