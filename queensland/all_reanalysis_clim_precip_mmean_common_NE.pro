PRO all_reanalysis_clim_precip_mmean_common_NE
  
; Input directories for reanalysis, SILO 0.25 degree and SILO on reanalysis grid
;
; Reanalysis directory names:
;   NCEP/NCAR - /home/ss901165/datasets_mango/NCEP_REANALYSIS/precipitation
;   ERA-40    - /home/ss901165/datasets/ERA40/PRECIP
;   20th Cent - /home/ss901165/datasets_mango/20THC_REANALYSIS/precip
; 
; SILO on reanalysis grids:
;   NCEP/NCAR - /home/ss901165/datasets_mango/SILO/t62
;   ERA-40    - /home/ss901165/datasets_mango/SILO/era40_resolution
;   20th Cent - /home/ss901165/datasets_mango/SILO/t62 (same grid as NCEP/NCAR)

reanalysis_indir='/home/ss901165/datasets_mango/NCEP_REANALYSIS/precipitation'
era40_reanalysis_indir='/home/ss901165/datasets/ERA40/PRECIP'
thcent_reanalysis_indir='/home/ss901165/datasets_mango/20THC_REANALYSIS/precip'
silo_t62_indir='/home/ss901165/datasets_mango/SILO/t62'
silo_era40_indir='/home/ss901165/datasets_mango/SILO/era40_resolution'

months=['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']
months_long=['January','February','March','April','May','June','July','August','September','October','November','December']
n_months=N_ELEMENTS(months)

box_plot=[-25,138,-11,154]  ;reading in precip for the NE region 
box=[-25,138,-11,154]

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

mask_era40_infile='/home/ss901165/datasets/ERA40/era40_lsm.nc'
mask_era40_longitude=OPEN_AND_EXTRACT(mask_era40_infile,'longitude')
mask_era40_latitude=OPEN_AND_EXTRACT(mask_era40_infile,'latitude')
DEFINE_BOUNDARIES,box,mask_era40_latitude,mask_era40_longitude,mask_era40_box_tx,/LIMIT
mask_era40_nlon=N_ELEMENTS(mask_era40_longitude)
mask_era40_nlat=N_ELEMENTS(mask_era40_latitude)
mask_era40=REFORM(OPEN_AND_EXTRACT(mask_era40_infile,'LSM',$
                             offset=[mask_era40_box_tx(1),mask_era40_box_tx(0)],$
                             count=[mask_era40_nlon,mask_era40_nlat]))

mask_twentyc_infile='/home/ss901165/datasets_mango/20THC_REANALYSIS/mask_t62.nc'
mask_twentyc_longitude=OPEN_AND_EXTRACT(mask_twentyc_infile,'longitude')
mask_twentyc_latitude=OPEN_AND_EXTRACT(mask_twentyc_infile,'latitude')
DEFINE_BOUNDARIES,box,mask_twentyc_latitude,mask_twentyc_longitude,mask_twentyc_box_tx,/LIMIT
mask_twentyc_nlon=N_ELEMENTS(mask_twentyc_longitude)
mask_twentyc_nlat=N_ELEMENTS(mask_twentyc_latitude)
mask_twentyc=REFORM(OPEN_AND_EXTRACT(mask_twentyc_infile,'lsm',$
                                     offset=[mask_twentyc_box_tx(1),mask_twentyc_box_tx(0)],$
                                     count=[mask_twentyc_nlon,mask_twentyc_nlat]))


mylevs_months=['0.4','0.8','1.2','1.6','2.0','2.4','2.8','3.2','4.0','4.8','5.6','6.4','7.2','8.0','8.8','9.6','10.4']
mylevs_diff_months=['-4.0','-3.5','-3.0','-2.5','-2.0','-1.5','-1.0','-0.5','0.0','0.5','1.0','1.5','2.0','2.5']

avg_precip_ncep_reanalysis_NE=findgen(12*1)
avg_precip_era40_reanalysis_NE=findgen(12*1)
avg_precip_thcent_reanalysis_NE=findgen(12*1)
avg_precip_silo_NE=findgen(12*1)
avg_precip_silo_era40_NE=findgen(12*1)

FOR i=0,n_months-1 DO BEGIN

                                ; Filename convention for reanalysis precipitation
                                ;   NCEP/NCAR - reanalysis_indir+'/ncep-ncar_reanalysis.'+seasons(i)+'_smean_clim.1948-2007.precip.t62_gauss.nc'
                                ;   ERA-40    - reanalysis_indir+'/era40.'+seasons(i)+'_smean_clim.1958-2001.precip.aus_domain.nc'
                                ;   20th Cent - reanalysis_indir+'/20thc_reanalysis.'+seasons(i)+'_smean_clim.1900-2007.precip.nc'
   reanalysis_infile=reanalysis_indir+'/ncep-ncar_reanalysis.'+months(i)+'_mmean_clim.1958-2001.precip.t62_gauss.nc'
   era40_reanalysis_infile=era40_reanalysis_indir+'/era40.'+months(i)+'_mmean_clim.1958-2001.precip.aus_domain.nc'
   thcent_reanalysis_infile=thcent_reanalysis_indir+'/20thc_reanalysis.'+months(i)+'_mmean_clim.1958-2001.precip.nc'
                                ; Filename convention for SILO precipitation on reanalysis grid
                                ;   NCEP/NCAR - silo_t62_indir+'/SILO.'+seasons(i)+'_smean_clim.1948-2007.precip.t62.nc'
                                ;   ERA-40    - silo_era40_indir+'/SILO.'+seasons(i)+'_smean_clim.1958-2001.precip.era40.nc'
                                ;   20th Cent - silo_t62_indir+'/SILO.'+seasons(i)+'_smean_clim.1900-2007.precip.t62.nc'
   silo_t62_infile=silo_t62_indir+'/SILO.'+months(i)+'_mmean_clim.1958-2001.precip.t62.nc'
   silo_era40_infile=silo_era40_indir+'/SILO.'+months(i)+'_mmean_clim.1958-2001.precip.era40.nc'
   
   IF i eq 0 THEN BEGIN
      silo_t62_longitude=OPEN_AND_EXTRACT(silo_t62_infile,'longitude')
      silo_t62_latitude=OPEN_AND_EXTRACT(silo_t62_infile,'latitude')
      DEFINE_BOUNDARIES,box,silo_t62_latitude,silo_t62_longitude,silo_t62_box_tx,/LIMIT
      silo_t62_nlon=N_ELEMENTS(silo_t62_longitude)
      silo_t62_nlat=N_ELEMENTS(silo_t62_latitude)

      silo_era40_longitude=OPEN_AND_EXTRACT(silo_era40_infile,'longitude')
      silo_era40_latitude=OPEN_AND_EXTRACT(silo_era40_infile,'latitude')
      DEFINE_BOUNDARIES,box,silo_era40_latitude,silo_era40_longitude,silo_era40_box_tx,/LIMIT
      silo_era40_nlon=N_ELEMENTS(silo_era40_longitude)
      silo_era40_nlat=N_ELEMENTS(silo_era40_latitude)

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

      era40_reanalysis_longitude=OPEN_AND_EXTRACT(era40_reanalysis_infile,'longitude')
      era40_reanalysis_latitude=OPEN_AND_EXTRACT(era40_reanalysis_infile,'latitude')
      DEFINE_BOUNDARIES,box,era40_reanalysis_latitude,era40_reanalysis_longitude,era40_reanalysis_box_tx,/LIMIT 
      DEFINE_BOUNDARIES,[silo_era40_latitude(silo_era40_nlat-1),silo_era40_longitude(0),$
                         silo_era40_latitude(0),silo_era40_longitude(silo_era40_nlon-1)],$
                        era40_reanalysis_latitude,era40_reanalysis_longitude,era40_reanalysis_silo_box_tx
      era40_reanalysis_nlon=N_ELEMENTS(era40_reanalysis_longitude)
      era40_reanalysis_nlat=N_ELEMENTS(era40_reanalysis_latitude)      
      
      thcent_reanalysis_longitude=OPEN_AND_EXTRACT(thcent_reanalysis_infile,'longitude')
      thcent_reanalysis_latitude=OPEN_AND_EXTRACT(thcent_reanalysis_infile,'latitude')
      DEFINE_BOUNDARIES,box,thcent_reanalysis_latitude,thcent_reanalysis_longitude,thcent_reanalysis_box_tx,/LIMIT 
      DEFINE_BOUNDARIES,[silo_t62_latitude(silo_t62_nlat-1),silo_t62_longitude(0),$
                         silo_t62_latitude(0),silo_t62_longitude(silo_t62_nlon-1)],$
                        thcent_reanalysis_latitude,thcent_reanalysis_longitude,thcent_reanalysis_silo_box_tx
      thcent_reanalysis_nlon=N_ELEMENTS(thcent_reanalysis_longitude)
      thcent_reanalysis_nlat=N_ELEMENTS(thcent_reanalysis_latitude)
      
   ENDIF

   ; Read in reanalysis precipitation
   ; Variable names and units
   ;   NCEP/NCAR - 'prate' (mm/sec)
   ;   ERA-40    - 'precip' (m/day) <- Not a typo: metres per day
   ;   20th Cent - 'PRATE' (mm/sec)
   reanalysis_clim_precip=REFORM(OPEN_AND_EXTRACT(reanalysis_infile,'prate',$
                                                  offset=[reanalysis_box_tx(1),reanalysis_box_tx(0)],$
                                                  count=[reanalysis_nlon,reanalysis_nlat]))
   era40_reanalysis_clim_precip=REFORM(OPEN_AND_EXTRACT(era40_reanalysis_infile,'precip',$
                                                  offset=[era40_reanalysis_box_tx(1),era40_reanalysis_box_tx(0)],$
                                                  count=[era40_reanalysis_nlon,era40_reanalysis_nlat]))
   thcent_reanalysis_clim_precip=REFORM(OPEN_AND_EXTRACT(thcent_reanalysis_infile,'PRATE',$
                                                  offset=[thcent_reanalysis_box_tx(1),thcent_reanalysis_box_tx(0)],$
                                                  count=[thcent_reanalysis_nlon,thcent_reanalysis_nlat]))


   ; Convert to mm/day
   reanalysis_clim_precip=reanalysis_clim_precip*86400.
   era40_reanalysis_clim_precip=era40_reanalysis_clim_precip*1000.
   thcent_reanalysis_clim_precip=thcent_reanalysis_clim_precip*86400.

   ; Read in SILO precipitation on reanalysis grid
   ; Variable name: rain
   ; Units: mm/day
   silo_t62_clim_precip=REFORM(OPEN_AND_EXTRACT(silo_t62_infile,'rain',$
                                                offset=[silo_t62_box_tx(1),silo_t62_box_tx(0)],$
                                                count=[silo_t62_nlon,silo_t62_nlat]))
   silo_era40_clim_precip=REFORM(OPEN_AND_EXTRACT(silo_era40_infile,'rain',$
                                                offset=[silo_era40_box_tx(1),silo_era40_box_tx(0)],$
                                                count=[silo_era40_nlon,silo_era40_nlat]))

   ; Mask ocean points
   reanalysis_clim_precip[where(mask eq 0)]=!Values.F_NaN
   era40_reanalysis_clim_precip[where(mask_era40 eq 0)]=!Values.F_NaN
   thcent_reanalysis_clim_precip[where(mask_twentyc eq 0)]=!Values.F_NaN

   silo_t62_clim_precip[where(mask eq 0)]=!Values.F_NaN
   silo_era40_clim_precip[where(mask_era40 eq 0)]=!Values.F_NaN

   ; Take difference between reanalysis and SILO rainfall
  ; diff_reanalysis_silo=fltarr(reanalysis_nlon,reanalysis_nlat)
  ; FOR j=0,reanalysis_nlon-1 DO BEGIN
  ;    FOR k=0,reanalysis_nlat-1 DO BEGIN
  ;                              ; Mask out ocean points and those points in the reanalysis 
  ;                              ; that lie outside the bounds of the observations.
  ;       IF (mask(j,k) ne 0) and j gt reanalysis_silo_box_tx(1) and j lt reanalysis_silo_box_tx(3) and $
  ;          k gt reanalysis_silo_box_tx(0) and k lt reanalysis_silo_box_tx(2) THEN BEGIN
  ;                              ; Shift observations to agree with reanalysis grid conventions
  ;          diff_reanalysis_silo(j,k)=reanalysis_clim_precip(j,k)-silo_t62_clim_precip(j-reanalysis_silo_box_tx(1),k-reanalysis_silo_box_tx(0))
  ;       ENDIF ELSE $
                                ; Set all ocean points and points outside the observations' boundaries
                                ; to a missing value (Not a Number).
  ;          diff_reanalysis_silo(j,k)=!Values.F_NaN
  ;    ENDFOR
  ; ENDFOR

                                ; Plot reanalysis precipitation
                                ; You will need to change the path to refer to your own directory 
                                ; (e.g., /home/sw00....) rather than mine.
                                ; You will need to do this for all three plots below.
   psfile='/home/ss901165/idl/queensland/ncep_ncar_clim_precip.'+months(i)+'_mmean.ncep_ncar_common_NE.1958-2001.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1000,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,XSIZE=18000
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_months)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_months
   CON,FIELD=reanalysis_clim_precip,X=reanalysis_longitude,Y=reanalysis_latitude,$
       TITLE=months_long(i)+'-mean rainfall from NCEP/NCAR reanalysis (1958-2001)',/NOLINES,$
       CB_TITLE='Rainfall (mm day!U-1!N)',/BLOCK
   AXES,XSTEP=10,YSTEP=5
   PSCLOSE,/NOVIEW 
   
   psfile='/home/ss901165/idl/queensland/era_40_clim_precip.'+months(i)+'_mmean.era_40_common_NE.1958-2001.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1000,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,XSIZE=18000
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_months)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_months
   CON,FIELD=era40_reanalysis_clim_precip,X=era40_reanalysis_longitude,Y=era40_reanalysis_latitude,$
       TITLE=months_long(i)+'-mean rainfall from ERA-40 reanalysis (1958-2001)',/NOLINES,$
       CB_TITLE='Rainfall (mm day!U-1!N)',/BLOCK
   AXES,XSTEP=10,YSTEP=5
   PSCLOSE,/NOVIEW 

   psfile='/home/ss901165/idl/queensland/20th_cent_clim_precip.'+months(i)+'_mmean.20th_cent_common_NE.1958-2001.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1000,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,XSIZE=18000
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_months)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_months
   CON,FIELD=thcent_reanalysis_clim_precip,X=thcent_reanalysis_longitude,Y=thcent_reanalysis_latitude,$
       TITLE=months_long(i)+'-mean rainfall from 20th Century reanalysis (1958-2001)',/NOLINES,$
       CB_TITLE='Rainfall (mm day!U-1!N)',/BLOCK
   AXES,XSTEP=10,YSTEP=5
   PSCLOSE,/NOVIEW 

                                  ; Plot SILO precipitation on reanalysis grid
   psfile='/home/ss901165/idl/queensland/ncep_ncar_clim_precip.'+months(i)+'_mmean.silo_t62_common_NE.1958-2001.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1000,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=100,XSIZE=18000
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_months)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_months
   CON,FIELD=silo_t62_clim_precip,X=silo_t62_longitude,Y=silo_t62_latitude,$
       TITLE=months_long(i)+'-mean rain for SILO on NCEP/NCAR grid (1958-2001)',/NOLINES,$
       CB_TITLE='Rainfall (mm day!U-1!N)',/BLOCK
   AXES,XSTEP=10,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/era_40_clim_precip.'+months(i)+'_mmean.silo_era40_common_NE.1958-2001.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1000,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=100,XSIZE=18000
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_months)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_months
   CON,FIELD=silo_era40_clim_precip,X=silo_era40_longitude,Y=silo_era40_latitude,$
       TITLE=months_long(i)+'-mean rain for SILO on ERA-40 grid (1958-2001)',/NOLINES,$
       CB_TITLE='Rainfall (mm day!U-1!N)',/BLOCK
   AXES,XSTEP=10,YSTEP=5
   PSCLOSE,/NOVIEW

   ; Add code to plot difference between reanalysis and SILO here ...
  ;   psfile='/home/ss901165/idl/queensland/ncep_ncar_clim_precip.'+months(i)+'_mmean_difference_SE.1948-2007.ps'
  ; PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1000,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=100,XSIZE=18000
  ; CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs_months)+1,/REV,WHITE=[17,18]
  ; MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
  ; LEVS,MANUAL=mylevs_diff_months
  ; CON,FIELD=diff_reanalysis_silo,X=reanalysis_longitude,Y=reanalysis_latitude,$
  ;     TITLE=Months_long(i)+'- Difference between observations and NCEP/NCAR reanalysis (1948-2007)',/NOLINES,$
  ;     CB_TITLE='Rainfall (mm day!U-1!N)',/BLOCK
  ; AXES,XSTEP=10,YSTEP=5
  ; PSCLOSE,/NOVIEW

   
   silo_t62_infile_amean=silo_t62_indir+'/SILO.jan-dec_amean_clim.1958-2001.precip.t62.nc'
   silo_t62_clim_precip_amean=REFORM(OPEN_AND_EXTRACT(silo_t62_infile_amean,'rain',$
                                                offset=[silo_t62_box_tx(1),silo_t62_box_tx(0)],$
                                                count=[silo_t62_nlon,silo_t62_nlat]))

   
  FOR j=0,reanalysis_nlon-1 DO BEGIN
     FOR k=0,reanalysis_nlat-1 DO BEGIN
 ;                              ; Mask out ocean points and those points in the reanalysis 
                               ; that lie outside the bounds of the observations.
     
        IF silo_t62_clim_precip(j,k) and silo_t62_clim_precip_amean(j,k) gt 100 THEN BEGIN

               silo_t62_clim_precip(j,k)=!Values.F_NaN
               silo_t62_clim_precip_amean(j,k)=!Values.F_NaN

            ENDIF
      
     ENDFOR

  ENDFOR

  avg_precip_silo_amean_NE=MEAN(silo_t62_clim_precip_amean,/NAN)
  PRINT,avg_precip_silo_amean_NE
 
  silo_era40_infile_amean=silo_era40_indir+'/SILO.jan-dec_amean_clim.1958-2001.precip.era40.nc'
  silo_era40_clim_precip_amean=REFORM(OPEN_AND_EXTRACT(silo_era40_infile_amean,'rain',$
                                                offset=[silo_era40_box_tx(1),silo_era40_box_tx(0)],$
                                                count=[silo_era40_nlon,silo_era40_nlat]))


    FOR l=0,era40_reanalysis_nlon-1 DO BEGIN
     FOR m=0,era40_reanalysis_nlat-1 DO BEGIN
 ;                              ; Mask out ocean points and those points in the reanalysis 
                               ; that lie outside the bounds of the observations.
    
           IF silo_era40_clim_precip(l,m) and silo_era40_clim_precip_amean(l,m) gt 100 THEN BEGIN

               silo_era40_clim_precip(l,m)=!Values.F_NaN
               silo_era40_clim_precip_amean(l,m)=!Values.F_NaN

            ENDIF
     ENDFOR
   
  ENDFOR
     
   avg_precip_ncep_reanalysis_NE(i)=MEAN(reanalysis_clim_precip,/NaN)
   avg_precip_era40_reanalysis_NE(i)=MEAN(era40_reanalysis_clim_precip,/NaN)
   avg_precip_thcent_reanalysis_NE(i)=MEAN(thcent_reanalysis_clim_precip,/NaN)
   avg_precip_silo_NE(i)=MEAN(silo_t62_clim_precip,/NAN)
   avg_precip_silo_era40_NE(i)=MEAN(silo_era40_clim_precip,/NAN)
   
 
ENDFOR

   avg_precip_silo_era40_amean_NE=MEAN(silo_era40_clim_precip_amean,/NAN)

   psfile='/home/ss901165/idl/queensland/ncep_ncar_clim_precip.avg_mmean_common_NE.1958-2001.ps'
   PSOPEN,file=psfile,FONT=2
   xvals=[1,2,3,4,5,6,7,8,9,10,11,12]
   CS,SCALE=26
   GSET,XMIN=1,XMAX=12,YMIN=0,YMAX=8,TITLE='Monthly mean rainfall rate across North-Eastern Australia (1958-2001)'
   GPLOT,X=xvals,Y=avg_precip_ncep_reanalysis_NE,COL=1
   GPLOT,X=xvals,Y=avg_precip_era40_reanalysis_NE,COL=2
   GPLOT,X=xvals,Y=avg_precip_thcent_reanalysis_NE,COL=8
   GPLOT,X=xvals,Y=avg_precip_silo_era40_NE,COL=14
   GPLOT,X=xvals,Y=avg_precip_silo_NE,COL=24
   AXES,XVALS=xvals,XLABELS=months_long,XTITLE='Month',YTITLE='Rainfall rate (mm/day)'
   GLEGEND,LEGPOS=9,COL=[1,2,8,14,24],LABELS=['NCEP/NCAR','ERA-40','20th Century','SILO Observations(ERA-40 grid)','SILO Observations (NCEP/NCAR grid)']
   PSCLOSE

   reanalysis_infile_amean=reanalysis_indir+'/ncep-ncar_reanalysis.jan-dec_amean_clim.1958-2001.precip.t62_gauss.nc'
   reanalysis_clim_precip_amean=REFORM(OPEN_AND_EXTRACT(reanalysis_infile_amean,'prate',$
                                                  offset=[reanalysis_box_tx(1),reanalysis_box_tx(0)],$
                                                  count=[reanalysis_nlon,reanalysis_nlat]))
   reanalysis_clim_precip_amean=reanalysis_clim_precip_amean*86400
   avg_precip_ncep_amean_NE=MEAN(reanalysis_clim_precip_amean)

   era40_reanalysis_infile_amean=era40_reanalysis_indir+'/era40.jan-dec_amean_clim.1958-2001.precip.aus_domain.nc'
   era40_reanalysis_clim_precip_amean=REFORM(OPEN_AND_EXTRACT(era40_reanalysis_infile_amean,'precip',$
                                                  offset=[era40_reanalysis_box_tx(1),era40_reanalysis_box_tx(0)],$
                                                  count=[era40_reanalysis_nlon,era40_reanalysis_nlat]))
   era40_reanalysis_clim_precip_amean=era40_reanalysis_clim_precip_amean*1000
   avg_precip_era40_amean_NE=MEAN(era40_reanalysis_clim_precip_amean)
   
   thcent_reanalysis_infile_amean=thcent_reanalysis_indir+'/20thc_reanalysis.jan-dec_amean_clim.1958-2001.precip.nc'
   thcent_reanalysis_clim_precip_amean=REFORM(OPEN_AND_EXTRACT(thcent_reanalysis_infile_amean,'PRATE',$
                                                  offset=[thcent_reanalysis_box_tx(1),thcent_reanalysis_box_tx(0)],$
                                                  count=[thcent_reanalysis_nlon,thcent_reanalysis_nlat]))
   thcent_reanalysis_clim_precip_amean=thcent_reanalysis_clim_precip_amean*86400
   avg_precip_thcent_amean_NE=MEAN(thcent_reanalysis_clim_precip_amean)
 

   contri_annual_mean_ncep_NE=findgen(1*12)
   contri_annual_mean_era40_NE=findgen(1*12)
   contri_annual_mean_thcent_NE=findgen(1*12)
   contri_annual_mean_silo_NE=findgen(1*12)
   contri_annual_mean_silo_era40_NE=findgen(1*12)


FOR i=0,n_months-1 DO BEGIN   
   contri_annual_mean_ncep_NE(i)=(avg_precip_ncep_reanalysis_NE(i)/(avg_precip_ncep_amean_NE*12))*100
   contri_annual_mean_era40_NE(i)=(avg_precip_era40_reanalysis_NE(i)/(avg_precip_era40_amean_NE*12))*100
   contri_annual_mean_thcent_NE(i)=(avg_precip_thcent_reanalysis_NE(i)/(avg_precip_thcent_amean_NE*12))*100
   contri_annual_mean_silo_NE(i)=(avg_precip_silo_NE(i)/(avg_precip_silo_amean_NE*12))*100
   contri_annual_mean_silo_era40_NE(i)=(avg_precip_silo_era40_NE(i)/(avg_precip_silo_era40_amean_NE*12))*100

ENDFOR
 

  psfile='/home/ss901165/idl/queensland/ncep_ncar_clim_precip.avg_mmean_contribution_common_NE.1958-2001.ps'
   PSOPEN,file=psfile,FONT=2
   xvals=[1,2,3,4,5,6,7,8,9,10,11,12]
   CS,SCALE=26
   GSET,XMIN=1,XMAX=12,YMIN=0,YMAX=25,TITLE='Percentage contribution of each months mean rainfall to the Annual total across North-Eastern Australia (1958-2001)'
   GPLOT,X=xvals,Y=contri_annual_mean_ncep_NE,COL=1
   GPLOT,X=xvals,Y=contri_annual_mean_era40_NE,COL=2
   GPLOT,X=xvals,Y=contri_annual_mean_thcent_NE,COL=8
   GPLOT,X=xvals,Y=contri_annual_mean_silo_era40_NE,COL=14
   GPLOT,X=xvals,Y=contri_annual_mean_silo_NE,COL=24
   AXES,XVALS=xvals,XLABELS=months_long,XTITLE='Month',YTITLE='Percentage (%)'
   GLEGEND,LEGPOS=9,COL=[1,2,8,14,24],LABELS=['NCEP/NCAR','ERA-40','20th Century','SILO Observations(ERA-40 grid)','SILO Observations (NCEP/NCAR grid)']
   PSCLOSE



STOP

END
