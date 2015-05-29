PRO era_40_corr_precip_silo

; Input directories for reanalysis, SILO 0.25 degree and SILO on reanalysis grid
;
; Reanalysis directory names:
;   ERA-40    - /home/ss901165/datasets/ERA40/PRECIP
;
; SILO on reanalysis grids:
;   ERA-40    - /home/ss901165/datasets_mango/SILO/era40_resolution

reanalysis_indir='/home/ss901165/datasets/ERA40/PRECIP'
silo_indir='/home/ss901165/datasets_mango/SILO/era40_resolution'

seasons=['dec-feb','mar-may','jun-aug','sep-nov']
seasons_long=['December-February','March-May','June-August','September-November']
n_seasons=N_ELEMENTS(seasons)

box_plot=[-44,110,-5,160]
box=[-45,110,-5,160]

; Define number of years for this reanalysis
; ERA-40    - 44
reanalysis_nyears=44

; Read in the land/sea mask from the reanalysis
; File names:
;   ERA-40    - /home/ss901165/datasets/ERA40/era40_lsm.nc
;
; Variable names:
;   ERA-40    - longitude, latitude, LSM

mask_infile='/home/ss901165/datasets/ERA40/era40_lsm.nc'
mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)
mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'LSM',$
                             offset=[mask_box_tx(1),mask_box_tx(0)],$
                             count=[mask_nlon,mask_nlat]))

mylevs_corr=['-0.35','-0.25','-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95']

FOR i=0,n_seasons-1 DO BEGIN
                                ; Filename convention for reanalysis precipitation
                                ;   ERA-40    - reanalysis_indir+'/era40.'+seasons(i)+'_smeans.1958-2001.precip.aus_domain.nc'
   reanalysis_infile=reanalysis_indir+'/era40.'+seasons(i)+'_smeans.1958-2001.precip.aus_domain.nc'   

                                ; Filename convention for SILO precipitation on reanalysis grid
                                ;   ERA-40    - silo_indir+'/SILO.'+seasons(i)+'_smeans.1958-2001.precip.era40.nc'
   silo_infile=silo_indir+'/SILO.'+seasons(i)+'_smeans.1958-2001.precip.era40.nc'

   IF i eq 0 THEN BEGIN
      silo_longitude=OPEN_AND_EXTRACT(silo_infile,'longitude')
      silo_latitude=OPEN_AND_EXTRACT(silo_infile,'latitude')
      DEFINE_BOUNDARIES,box,silo_latitude,silo_longitude,silo_box_tx,/LIMIT
      silo_nlon=N_ELEMENTS(silo_longitude)
      silo_nlat=N_ELEMENTS(silo_latitude)
      
      ; Read in the latitude-longitude grid for the reanalysis
      ; Variable names:
      ;   ERA-40    - longitude, latitude
      reanalysis_longitude=OPEN_AND_EXTRACT(reanalysis_infile,'longitude')
      reanalysis_latitude=OPEN_AND_EXTRACT(reanalysis_infile,'latitude')
      DEFINE_BOUNDARIES,box,reanalysis_latitude,reanalysis_longitude,reanalysis_box_tx,/LIMIT 
      DEFINE_BOUNDARIES,[silo_latitude(silo_nlat-1),silo_longitude(0),$
                         silo_latitude(0),silo_longitude(silo_nlon-1)],$
                        reanalysis_latitude,reanalysis_longitude,reanalysis_silo_box_tx
      reanalysis_nlon=N_ELEMENTS(reanalysis_longitude)
      reanalysis_nlat=N_ELEMENTS(reanalysis_latitude)      
   ENDIF

   ; Read in reanalysis precipitation
   ; Variable names and units
   ;   ERA-40    - 'precip' (m/day) <- Not a typo: metres per day
   reanalysis_smean_precip=REFORM(OPEN_AND_EXTRACT(reanalysis_infile,'precip',$
                                                  offset=[reanalysis_box_tx(1),reanalysis_box_tx(0),0],$
                                                  count=[reanalysis_nlon,reanalysis_nlat,reanalysis_nyears]))
   ; Convert to mm/day
   reanalysis_smean_precip=reanalysis_smean_precip*1000.

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
   psfile='/home/ss901165/idl/queensland/era_40_corr_precip_silo.'+seasons(i)+'_mean.era40.1958-2001.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1000,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=90,CB_WIDTH=110,XSIZE=18000
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_corr)+1;,white=[6]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_corr
   CON,FIELD=corr_reanalysis_silo,X=reanalysis_longitude,Y=reanalysis_latitude,$
       TITLE='Correlation of '+seasons_long(i)+'-mean ERA-40 reanalysis rainfall with SILO (1958-2001)',/NOLINES,$
       CB_TITLE='Correlation coefficient (unitless)',/BLOCK
   AXES,XSTEP=10,YSTEP=5
   PSCLOSE,/NOVIEW

ENDFOR

; Add code for annual means

 reanalysis_infile_amean=reanalysis_indir+'/era40.jan-dec_ameans.1958-2001.precip.aus_domain.nc'
 reanalysis_amean_precip=REFORM(OPEN_AND_EXTRACT(reanalysis_infile_amean,'precip',$
                                                  offset=[reanalysis_box_tx(1),reanalysis_box_tx(0),0],$
                                                  count=[reanalysis_nlon,reanalysis_nlat,reanalysis_nyears]))
 reanalysis_amean_precip=reanalysis_amean_precip*1000.


 silo_infile_amean=silo_indir+'/SILO.jan-dec_ameans.1958-2001.precip.era40.nc'
 silo_amean_precip=REFORM(OPEN_AND_EXTRACT(silo_infile_amean,'rain',$
                                                offset=[silo_box_tx(1),silo_box_tx(0),0],$
                                                count=[silo_nlon,silo_nlat,reanalysis_nyears]))


   ; Correlation of the annual mean reanalysis and observations
 corr_reanalysis_silo_amean=fltarr(reanalysis_nlon,reanalysis_nlat)
 FOR j=0,reanalysis_nlon-1 DO BEGIN
    FOR k=0,reanalysis_nlat-1 DO BEGIN
                                ; Mask out ocean points and those points in the reanalysis 
                                ; that lie outside the bounds of the observations.
       IF (mask(j,k) ne 0) and j gt reanalysis_silo_box_tx(1) and j lt reanalysis_silo_box_tx(3) and $
          k gt reanalysis_silo_box_tx(0) and k lt reanalysis_silo_box_tx(2) THEN BEGIN
          IF silo_amean_precip(j-reanalysis_silo_box_tx(1),k-reanalysis_silo_box_tx(0),0) le 1e10 THEN BEGIN
                                ; Shift observations to agree with reanalysis grid conventions
             corr_reanalysis_silo_amean(j,k)=CORRELATE(reanalysis_amean_precip(j,k,*),$
                                                       silo_amean_precip(j-reanalysis_silo_box_tx(1),k-reanalysis_silo_box_tx(0),*))
          ENDIF ELSE $
             corr_reanalysis_silo_amean(j,k)=!Values.F_NaN
       ENDIF ELSE $
                                ; Set all ocean points and points outside the observations' boundaries
                                ; to a missing value (Not a Number).
          corr_reanalysis_silo_amean(j,k)=!Values.F_NaN
    ENDFOR
 ENDFOR

 psfile='/home/ss901165/idl/queensland/era_40_corr_precip_silo_amean.1958-2001.ps'
 PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1000,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=100,XSIZE=18000
 CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs_corr)+1,/REV,WHITE=[20,21]
 MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
 LEVS,MANUAL=mylevs_corr
 CON,FIELD=corr_reanalysis_silo_amean,X=reanalysis_longitude,Y=reanalysis_latitude,$
       TITLE='- Correlation of Annual Mean Precipitation ERA-40 reanalysis with SILO observations (1958-2001)',/NOLINES,$
       CB_TITLE='Correlation Coefficient (unitless)',/BLOCK
   AXES,XSTEP=10,YSTEP=5
  PSCLOSE

STOP
END
