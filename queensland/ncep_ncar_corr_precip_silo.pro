PRO ncep_ncar_corr_precip_silo

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
silo_indir='/home/ss901165/datasets_mango/SILO/t62'

seasons=['dec-feb','mar-may','jun-aug','sep-nov']
seasons_long=['December-February','March-May','June-August','September-November']
n_seasons=N_ELEMENTS(seasons)

box_plot=[-44,110,-5,160]
box=[-45,110,-5,160]

; Define number of years for this reanalysis
; NCEP/NCAR - 60
; ERA-40    - 44
; 20th Cent - 108
reanalysis_nyears=44

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

mask_infile='/home/ss901165/datasets_mango/NCEP_REANALYSIS/lsmask.t62_gauss.nc'
mask_longitude=OPEN_AND_EXTRACT(mask_infile,'lon')
mask_latitude=OPEN_AND_EXTRACT(mask_infile,'lat')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)
mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsmask',$
                             offset=[mask_box_tx(1),mask_box_tx(0)],$
                             count=[mask_nlon,mask_nlat]))

mylevs_corr=['-0.35','-0.25','-0.15','-0.05',$
             '0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85']

FOR i=0,n_seasons-1 DO BEGIN
                                ; Filename convention for reanalysis precipitation
                                ;   NCEP/NCAR - reanalysis_indir+'/ncep-ncar_reanalysis.'+seasons(i)+'_smeans.1948-2007.precip.t62_gauss.nc'
                                ;   ERA-40    - reanalysis_indir+'/era40.'+seasons(i)+'_smeans.1958-2001.precip.aus_domain.nc'
                                ;   20th Cent - reanalysis_indir+'/20thc_reanalysis.'+seasons(i)+'_smeans.1900-2007.precip.nc'
   reanalysis_infile=reanalysis_indir+'/ncep-ncar_reanalysis.'+seasons(i)+'_smeans.1948-2007.precip.t62_gauss.nc'   

                                ; Filename convention for SILO precipitation on reanalysis grid
                                ;   NCEP/NCAR - silo_indir+'/SILO.'+seasons(i)+'_smeans.1948-2007.precip.t62.nc'
                                ;   ERA-40    - silo_indir+'/SILO.'+seasons(i)+'_smeans.1958-2001.precip.era40.nc'
                                ;   20th Cent - silo_indir+'/SILO.'+seasons(i)+'_smeans.1900-2007.precip.t62.nc'
   silo_infile=silo_indir+'/SILO.'+seasons(i)+'_smeans.1948-2007.precip.t62.nc'

   IF i eq 0 THEN BEGIN
      silo_longitude=OPEN_AND_EXTRACT(silo_infile,'longitude')
      silo_latitude=OPEN_AND_EXTRACT(silo_infile,'latitude')
      DEFINE_BOUNDARIES,box,silo_latitude,silo_longitude,silo_box_tx,/LIMIT
      silo_nlon=N_ELEMENTS(silo_longitude)
      silo_nlat=N_ELEMENTS(silo_latitude)
      
      ; Read in the latitude-longitude grid for the reanalysis
      ; Variable names:
      ;   NCEP/NCAR - lon, lat
      ;   ERA-40    - longitude, latitude
      ;   20th Cent - longitude, latitude
      reanalysis_longitude=OPEN_AND_EXTRACT(reanalysis_infile,'lon')
      reanalysis_latitude=OPEN_AND_EXTRACT(reanalysis_infile,'lat')
      DEFINE_BOUNDARIES,box,reanalysis_latitude,reanalysis_longitude,reanalysis_box_tx,/LIMIT 
      DEFINE_BOUNDARIES,[silo_latitude(silo_nlat-1),silo_longitude(0),$
                         silo_latitude(0),silo_longitude(silo_nlon-1)],$
                        reanalysis_latitude,reanalysis_longitude,reanalysis_silo_box_tx
      reanalysis_nlon=N_ELEMENTS(reanalysis_longitude)
      reanalysis_nlat=N_ELEMENTS(reanalysis_latitude)      
   ENDIF

   ; Read in reanalysis precipitation
   ; Variable names and units
   ;   NCEP/NCAR - 'prate' (mm/sec)
   ;   ERA-40    - 'precip' (m/day) <- Not a typo: metres per day
   ;   20th Cent - 'PRATE' (mm/sec)
   reanalysis_smean_precip=REFORM(OPEN_AND_EXTRACT(reanalysis_infile,'prate',$
                                                  offset=[reanalysis_box_tx(1),reanalysis_box_tx(0),0],$
                                                  count=[reanalysis_nlon,reanalysis_nlat,reanalysis_nyears]))
   ; Convert to mm/day
   reanalysis_smean_precip=reanalysis_smean_precip*86400.

   ; Read in SILO precipitation on reanalysis grid
   ; Variable name: rain
   ; Units: mm/day
   silo_smean_precip=REFORM(OPEN_AND_EXTRACT(silo_infile,'rain',$
                                             offset=[silo_box_tx(1),silo_box_tx(0),0],$
                                             count=[silo_nlon,silo_nlat,reanalysis_nyears]))
   
   ; Correlate reanalysis and SILO rainfall
   corr_reanalysis_silo=fltarr(reanalysis_nlon,reanalysis_nlat)
   FOR j=0,reanalysis_nlon-1 DO BEGIN
      FOR k=0,reanalysis_nlat-1 DO BEGIN
                                ; Mask out ocean points and those points in the reanalysis 
                                ; that lie outside the bounds of the observations.
         IF (mask(j,k) ne 0) and j gt reanalysis_silo_box_tx(1) and j lt reanalysis_silo_box_tx(3) and $
            k gt reanalysis_silo_box_tx(0) and k lt reanalysis_silo_box_tx(2) THEN BEGIN
            IF silo_smean_precip(j-reanalysis_silo_box_tx(1),k-reanalysis_silo_box_tx(0),0) le 1e10 THEN BEGIN
                                ; Shift observations to agree with reanalysis grid conventions
               corr_reanalysis_silo(j,k)=CORRELATE(reanalysis_smean_precip(j,k,*),$
                                                   silo_smean_precip(j-reanalysis_silo_box_tx(1),k-reanalysis_silo_box_tx(0),*))
            ENDIF ELSE $
               corr_reanalysis_silo(j,k)=!Values.F_NaN
         ENDIF ELSE $
                                ; Set all ocean points and points outside the observations' boundaries
                                ; to a missing value (Not a Number).
            corr_reanalysis_silo(j,k)=!Values.F_NaN
      ENDFOR
   ENDFOR
   
                                ; Plot correlation between reanalysis and SILO
                                ; You will need to change the path to refer to your own directory 
                                ; (e.g., /home/sw00....) rather than mine.                                
   psfile='/home/ss901165/idl/queensland/ncep_ncar_corr_precip_silo.'+seasons(i)+'_mean.ncep_ncar.1948-2007.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1000,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=90,CB_WIDTH=110,XSIZE=18000
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_corr)+1;,white=[6]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_corr
   CON,FIELD=corr_reanalysis_silo,X=reanalysis_longitude,Y=reanalysis_latitude,$
       TITLE='Correlation of '+seasons_long(i)+'-mean NCEP/NCAR reanalysis rainfall with SILO (1948-2007)',/NOLINES,$
       CB_TITLE='Correlation coefficient (unitless)',/BLOCK
   AXES,XSTEP=10,YSTEP=5
   PSCLOSE
ENDFOR

STOP
END


