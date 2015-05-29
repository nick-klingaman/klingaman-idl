PRO NE_regional_precip_mean_rmse_amean

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

ncep_reanalysis_indir='/home/ss901165/datasets_mango/NCEP_REANALYSIS/precipitation'
era40_reanalysis_indir='/home/ss901165/datasets/ERA40/PRECIP'
thcent_reanalysis_indir='/home/ss901165/datasets_mango/20THC_REANALYSIS/precip'
silo_indir='/home/ss901165/datasets_mango/SILO/t62'
silo_era40_indir='/home/ss901165/datasets_mango/SILO/era40_resolution'
silo_thcent_indir='/home/ss901165/datasets_mango/SILO/t62'

box_plot=[-25,138,-12,151]
box=[-25,138,-12,151]

; Define number of years for this reanalysis
; NCEP/NCAR - 60
; ERA-40    - 44
; 20th Cent - 108
ncep_reanalysis_nyears=60
era40_reanalysis_nyears=44
thcent_reanalysis_nyears=108

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



mylevs_corr=['-0.35','-0.25','-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95']

; Add code for annual means

 ncep_reanalysis_infile_amean=ncep_reanalysis_indir+'/ncep-ncar_reanalysis.jan-dec_ameans.1948-2007.precip.t62_gauss.nc'
 era40_reanalysis_infile_amean=era40_reanalysis_indir+'/era40.jan-dec_ameans.1958-2001.precip.aus_domain.nc'
 thcent_reanalysis_infile_amean=thcent_reanalysis_indir+'/20thc_reanalysis.jan-dec_ameans.1900-2007.precip.nc'
 silo_infile_amean_ncep=silo_indir+'/SILO.jan-dec_ameans.1948-2007.precip.t62.nc'
 silo_era40_infile_amean=silo_era40_indir+'/SILO.jan-dec_ameans.1958-2001.precip.era40.nc'
 silo_thcent_infile_amean=silo_thcent_indir+'/SILO.jan-dec_ameans.1900-2007.precip.t62.nc'

 silo_longitude_ncep=OPEN_AND_EXTRACT(silo_infile_amean_ncep,'longitude')
 silo_latitude_ncep=OPEN_AND_EXTRACT(silo_infile_amean_ncep,'latitude')
 DEFINE_BOUNDARIES,box,silo_latitude_ncep,silo_longitude_ncep,silo_ncep_box_tx,/LIMIT
 silo_nlon_ncep=N_ELEMENTS(silo_longitude_ncep)
 silo_nlat_ncep=N_ELEMENTS(silo_latitude_ncep)
      
 silo_era40_longitude=OPEN_AND_EXTRACT(silo_era40_infile_amean,'longitude')
 silo_era40_latitude=OPEN_AND_EXTRACT(silo_era40_infile_amean,'latitude')
 DEFINE_BOUNDARIES,box,silo_era40_latitude,silo_era40_longitude,silo_era40_box_tx,/LIMIT
 silo_era40_nlon=N_ELEMENTS(silo_era40_longitude)
 silo_era40_nlat=N_ELEMENTS(silo_era40_latitude)
 
      ; Read in the latitude-longitude grid for the reanalysis
      ; Variable names:
      ;   NCEP/NCAR - lon, lat
      ;   ERA-40    - longitude, latitude
      ;   20th Cent - longitude, latitude
 ncep_reanalysis_longitude=OPEN_AND_EXTRACT(ncep_reanalysis_infile_amean,'lon')
 ncep_reanalysis_latitude=OPEN_AND_EXTRACT(ncep_reanalysis_infile_amean,'lat')
 DEFINE_BOUNDARIES,box,ncep_reanalysis_latitude,ncep_reanalysis_longitude,ncep_reanalysis_box_tx,/LIMIT 
 DEFINE_BOUNDARIES,[silo_latitude_ncep(silo_nlat_ncep-1),silo_longitude_ncep(0),$
                   silo_latitude_ncep(0),silo_longitude_ncep(silo_nlon_ncep-1)],$
                   ncep_reanalysis_latitude,ncep_reanalysis_longitude,ncep_reanalysis_silo_box_tx
 ncep_reanalysis_nlon=N_ELEMENTS(ncep_reanalysis_longitude)
 ncep_reanalysis_nlat=N_ELEMENTS(ncep_reanalysis_latitude)   

 era40_reanalysis_longitude=OPEN_AND_EXTRACT(era40_reanalysis_infile_amean,'longitude')
 era40_reanalysis_latitude=OPEN_AND_EXTRACT(era40_reanalysis_infile_amean,'latitude')
 DEFINE_BOUNDARIES,box,era40_reanalysis_latitude,era40_reanalysis_longitude,era40_reanalysis_box_tx,/LIMIT 
 DEFINE_BOUNDARIES,[silo_era40_latitude(silo_era40_nlat-1),silo_era40_longitude(0),$
                         silo_era40_latitude(0),silo_era40_longitude(silo_era40_nlon-1)],$
                        era40_reanalysis_latitude,era40_reanalysis_longitude,era40_reanalysis_silo_box_tx
 era40_reanalysis_nlon=N_ELEMENTS(era40_reanalysis_longitude)
 era40_reanalysis_nlat=N_ELEMENTS(era40_reanalysis_latitude)      
      
 thcent_reanalysis_longitude=OPEN_AND_EXTRACT(thcent_reanalysis_infile_amean,'longitude')
 thcent_reanalysis_latitude=OPEN_AND_EXTRACT(thcent_reanalysis_infile_amean,'latitude')
 DEFINE_BOUNDARIES,box,thcent_reanalysis_latitude,thcent_reanalysis_longitude,thcent_reanalysis_box_tx,/LIMIT 
 DEFINE_BOUNDARIES,[silo_latitude_ncep(silo_nlat_ncep-1),silo_longitude_ncep(0),$
                         silo_latitude_ncep(0),silo_longitude_ncep(silo_nlon_ncep-1)],$
                        thcent_reanalysis_latitude,thcent_reanalysis_longitude,thcent_reanalysis_silo_box_tx
 thcent_reanalysis_nlon=N_ELEMENTS(thcent_reanalysis_longitude)
 thcent_reanalysis_nlat=N_ELEMENTS(thcent_reanalysis_latitude)
 
 ncep_reanalysis_amean_precip=REFORM(OPEN_AND_EXTRACT(ncep_reanalysis_infile_amean,'prate',$
                                       offset=[ncep_reanalysis_box_tx(1),ncep_reanalysis_box_tx(0),0],$
                                       count=[ncep_reanalysis_nlon,ncep_reanalysis_nlat,ncep_reanalysis_nyears]))
 ncep_reanalysis_amean_precip=ncep_reanalysis_amean_precip*86400.

 era40_reanalysis_amean_precip=REFORM(OPEN_AND_EXTRACT(era40_reanalysis_infile_amean,'precip',$
                                       offset=[era40_reanalysis_box_tx(1),era40_reanalysis_box_tx(0),0],$
                                       count=[era40_reanalysis_nlon,era40_reanalysis_nlat,era40_reanalysis_nyears]))
 era40_reanalysis_amean_precip=era40_reanalysis_amean_precip*1000.

 thcent_reanalysis_amean_precip=REFORM(OPEN_AND_EXTRACT(thcent_reanalysis_infile_amean,'PRATE',$
                                       offset=[thcent_reanalysis_box_tx(1),thcent_reanalysis_box_tx(0),0],$
                                       count=[thcent_reanalysis_nlon,thcent_reanalysis_nlat,thcent_reanalysis_nyears]))
 thcent_reanalysis_amean_precip=thcent_reanalysis_amean_precip*86400.

 silo_ncep_amean_precip=REFORM(OPEN_AND_EXTRACT(silo_infile_amean_ncep,'rain',$
                                                offset=[silo_ncep_box_tx(1),silo_ncep_box_tx(0),0],$
                                                count=[silo_nlon_ncep,silo_nlat_ncep,ncep_reanalysis_nyears]))

 silo_era40_amean_precip=REFORM(OPEN_AND_EXTRACT(silo_era40_infile_amean,'rain',$
                                                offset=[silo_era40_box_tx(1),silo_era40_box_tx(0),0],$
                                                count=[silo_era40_nlon,silo_era40_nlat,era40_reanalysis_nyears]))
 
 silo_thcent_amean_precip=REFORM(OPEN_AND_EXTRACT(silo_thcent_infile_amean,'rain',$
                                                offset=[silo_ncep_box_tx(1),silo_ncep_box_tx(0),0],$
                                                count=[silo_nlon_ncep,silo_nlat_ncep,thcent_reanalysis_nyears]))

 ; Apply land-sea mask here.

FOR i=0,ncep_reanalysis_nyears-1 DO BEGIN
   temp=REFORM(ncep_reanalysis_amean_precip(*,*,i))
   temp[where(mask eq 0)]=!Values.F_NaN
   ncep_reanalysis_amean_precip(*,*,i)=temp

   temp=REFORM(silo_ncep_amean_precip(*,*,i))
   temp[where(mask eq 0)]=!Values.F_NaN
   silo_ncep_amean_precip(*,*,i)=temp
ENDFOR

FOR i=0,era40_reanalysis_nyears-1 DO BEGIN
   temp=REFORM(era40_reanalysis_amean_precip(*,*,i))
   temp[where(mask_era40 eq 0)]=!Values.F_NaN
   era40_reanalysis_amean_precip(*,*,i)=temp

   temp=REFORM(silo_era40_amean_precip(*,*,i))
   temp[where(mask eq 0)]=!Values.F_NaN
   silo_era40_amean_precip(*,*,i)=temp
ENDFOR

FOR i=0,thcent_reanalysis_nyears-1 DO BEGIN
   temp=REFORM(thcent_reanalysis_amean_precip(*,*,i))
   temp[where(mask_twentyc eq 0)]=!Values.F_NaN
   thcent_reanalysis_amean_precip(*,*,i)=temp

   temp=REFORM(silo_thcent_amean_precip(*,*,i))
   temp[where(mask eq 0)]=!Values.F_NaN
   silo_thcent_amean_precip(*,*,i)=temp
ENDFOR

FOR i=0,ncep_reanalysis_nyears-1 DO BEGIN
  FOR j=0,ncep_reanalysis_nlon-1 DO BEGIN
     FOR k=0,ncep_reanalysis_nlat-1 DO BEGIN
 ;                              ; Mask out ocean points and those points in the reanalysis 
                               ; that lie outside the bounds of the observations.
             IF silo_ncep_amean_precip(j,k,i) gt 200 THEN BEGIN

               silo_ncep_amean_precip(j,k,i)=!Values.F_NaN      
         
            ENDIF
          ENDFOR
  ENDFOR
ENDFOR

FOR a=0,era40_reanalysis_nyears-1 DO BEGIN
  FOR b=0,era40_reanalysis_nlon-1 DO BEGIN
     FOR c=0,era40_reanalysis_nlat-1 DO BEGIN
 ;                              ; Mask out ocean points and those points in the reanalysis 
                               ; that lie outside the bounds of the observations.
             IF silo_era40_amean_precip(b,c,a) gt 200 THEN BEGIN

               silo_era40_amean_precip(b,c,a)=!Values.F_NaN      
         
            ENDIF
          ENDFOR
  ENDFOR
ENDFOR
FOR d=0,thcent_reanalysis_nyears-1 DO BEGIN
  FOR e=0,thcent_reanalysis_nlon-1 DO BEGIN
     FOR f=0,thcent_reanalysis_nlat-1 DO BEGIN
 ;                              ; Mask out ocean points and those points in the reanalysis 
                               ; that lie outside the bounds of the observations.
             IF silo_thcent_amean_precip(e,f,d) gt 200 THEN BEGIN

               silo_thcent_amean_precip(e,f,d)=!Values.F_NaN      
         
            ENDIF
          ENDFOR
  ENDFOR
ENDFOR

 ; Take area average for each year
 ncep_reanalysis_areaavg=fltarr(ncep_reanalysis_nyears)
 era40_reanalysis_areaavg=fltarr(era40_reanalysis_nyears)
 thcent_reanalysis_areaavg=fltarr(thcent_reanalysis_nyears)
 silo_ncep_areaavg=fltarr(ncep_reanalysis_nyears)
 silo_era40_areaavg=fltarr(era40_reanalysis_nyears)
 silo_thcent_areaavg=fltarr(thcent_reanalysis_nyears)
 variance_ncep_pos=fltarr(ncep_reanalysis_nyears)
 variance_era40_pos=fltarr(era40_reanalysis_nyears)
 variance_thcent_pos=fltarr(thcent_reanalysis_nyears)
 variance_silo_pos=fltarr(thcent_reanalysis_nyears)
 variance_ncep_neg=fltarr(ncep_reanalysis_nyears)
 variance_era40_neg=fltarr(era40_reanalysis_nyears)
 variance_thcent_neg=fltarr(thcent_reanalysis_nyears)
 variance_silo_neg=fltarr(thcent_reanalysis_nyears)


 FOR g=0,ncep_reanalysis_nyears-1 DO BEGIN
    ncep_reanalysis_areaavg(g)=MEAN(ncep_reanalysis_amean_precip(*,*,g),/NaN)
    silo_ncep_areaavg(g)=MEAN(silo_ncep_amean_precip(*,*,g),/NaN)

    variance_ncep_pos(g)=STDDEV(ncep_reanalysis_amean_precip(*,*,g),/NaN)
    variance_ncep_pos(g)=ncep_reanalysis_areaavg(g)+variance_ncep_pos(g)
    variance_ncep_neg(g)=STDDEV(ncep_reanalysis_amean_precip(*,*,g),/NaN)
    variance_ncep_neg(g)=ncep_reanalysis_areaavg(g)-variance_ncep_neg(g)
 ENDFOR

 FOR h=0,era40_reanalysis_nyears-1 DO BEGIN
    era40_reanalysis_areaavg(h)=MEAN(era40_reanalysis_amean_precip(*,*,h),/NaN)
    silo_era40_areaavg(h)=MEAN(silo_era40_amean_precip(*,*,h),/NAN)
    variance_era40_pos(h)=era40_reanalysis_areaavg(h)+(STDDEV(era40_reanalysis_amean_precip(*,*,h),/NaN))
    variance_era40_neg(h)=era40_reanalysis_areaavg(h)-(STDDEV(era40_reanalysis_amean_precip(*,*,h),/NaN))
 ENDFOR

 FOR i=0,thcent_reanalysis_nyears-1 DO BEGIN
    thcent_reanalysis_areaavg(i)=MEAN(thcent_reanalysis_amean_precip(*,*,i),/NAN)
    silo_thcent_areaavg(i)=MEAN(silo_thcent_amean_precip(*,*,i),/NAN)
    variance_thcent_pos(i)=thcent_reanalysis_areaavg(i)+(STDDEV(thcent_reanalysis_amean_precip(*,*,i),/NaN))
    variance_thcent_neg(i)=thcent_reanalysis_areaavg(i)-(STDDEV(thcent_reanalysis_amean_precip(*,*,i),/NaN))
    variance_silo_pos(i)=silo_thcent_areaavg(i)+(STDDEV(silo_thcent_amean_precip(*,*,i),/NAN))
    variance_silo_neg(i)=silo_thcent_areaavg(i)-(STDDEV(silo_thcent_amean_precip(*,*,i),/NAN))
 ENDFOR
 


   time=fltarr(thcent_reanalysis_nyears)
   ncep_time=fltarr(ncep_reanalysis_nyears)
   era40_time=fltarr(era40_reanalysis_nyears)
   FOR z=0,thcent_reanalysis_nyears-1 DO BEGIN
   
      IF z ge 48 THEN BEGIN
         FOR w=z-48,ncep_reanalysis_nyears-1 DO BEGIN
            ncep_time(w)=z+1900
         ENDFOR
      ENDIF 
      IF z ge 58 THEN BEGIN
            FOR y=z-58,era40_reanalysis_nyears-1 DO BEGIN
               era40_time(y)=z+1900
            ENDFOR
      ENDIF    
      time(z)=z+1900
   ENDFOR
   
 ;  FOR i=0,ncep_reanalysis_nyears-1 DO BEGIN
 ;     IF ncep_silo_correlations(i) eq 0 or variance_ncep(i) eq 0 THEN BEGIN
 ;        ncep_silo_correlations(i)=!Values.F_NaN
 ;        variance_ncep(i)=!Values.F_NaN
 ;     ENDIF
 ;  ENDFOR

 ;  FOR r=0,era40_reanalysis_nyears-1 DO BEGIN
 ;     IF era40_silo_correlations(r) eq 0 or variance_era40(r) eq 0 THEN BEGIN
 ;        era40_silo_correlations(r)=!Values.F_NaN
 ;        variance_era40(r)=!Values.F_NaN
 ;     ENDIF
 ;  ENDFOR

 ;  FOR s=0,thcent_reanalysis_nyears-1 DO BEGIN
 ;     IF thcent_silo_correlations(s) eq 0 or variance_thcent(s) eq 0 or variance_silo(s) eq 0 THEN BEGIN
 ;        thcent_silo_correlations(s)=!Values.F_NaN
 ;     variance_thcent(s)=!Values.F_NaN
 ;     variance_silo(s)=!Values.F_NaN
 ;     ENDIF
 ;  ENDFOR

 
  psfile='/home/ss901165/idl/queensland/NE_regional_running_mean_stdev_all_reanalysis.1900-2007.ps'
  PSOPEN,file=psfile,XPLOTS=2,YPLOTS=2,SPACING=3500
  CS,SCALE=26
  GSET,XMIN=1900,XMAX=2007,YMIN=0,YMAX=10.0

  POS,XPOS=1,YPOS=2
  GSET,XMIN=1900,XMAX=2007,YMIN=0,YMAX=7.0,TITLE='NCEP/NCAR'
  GPLOT,X=ncep_time,Y=ncep_reanalysis_areaavg,COL=1
  GPLOT,X=ncep_time,Y=variance_ncep_pos,COL=24
  GPLOT,X=ncep_time,Y=variance_ncep_neg,COL=24
  AXES,XSTEP=10,YSTEP=1,NDECS=2,XTITLE='Year',/NORIGHT,YTITLE='Rainfall rate (mm/day)'
 

  POS,XPOS=2,YPOS=2
  GSET,XMIN=1900,XMAX=2007,YMIN=0,YMAX=7.0,TITLE='ERA-40'
  GPLOT,X=era40_time,Y=era40_reanalysis_areaavg,COL=1
  GPLOT,X=era40_time,Y=variance_era40_pos,COL=24
  GPLOT,X=era40_time,Y=variance_era40_neg,COL=24
  AXES,XSTEP=10,YSTEP=1,NDECS=2,XTITLE='Year',/NORIGHT,YTITLE='Rainfall rate (mm/day)'

  POS,XPOS=1,YPOS=1
  GSET,XMIN=1900,XMAX=2007,YMIN=0,YMAX=7.0,TITLE='20TH CENTURY'
  GPLOT,X=time,Y=thcent_reanalysis_areaavg,COL=1
  GPLOT,X=time,Y=variance_thcent_pos,COL=24
  GPLOT,X=time,Y=variance_thcent_neg,COL=24
  AXES,XSTEP=10,YSTEP=1,NDECS=2,XTITLE='Year',/NORIGHT,YTITLE='Rainfall rate (mm/day)'

  POS,XPOS=2,YPOS=1
  GSET,XMIN=1900,XMAX=2007,YMIN=0,YMAX=7.0,TITLE='SILO'
  GPLOT,X=time,Y=silo_thcent_areaavg,COL=1
  GPLOT,X=time,Y=variance_silo_pos,COL=24
  GPLOT,X=time,Y=variance_silo_neg,COL=24
  AXES,XSTEP=10,YSTEP=1,NDECS=2,XTITLE='Year',/NORIGHT,YTITLE='Rainfall rate (mm/day)'
  PSCLOSE

  psfile='/home/ss901165/idl/queensland/NE_regional_annual_mean_all_reanalysis.1900-2007.ps'
  PSOPEN,file=psfile
  CS,SCALE=26
  GSET,XMIN=1900,XMAX=2007,YMIN=0,YMAX=4.0,TITLE='Timeseries of area average annual-mean daily rainfall rate (NE Australia)'
  GPLOT,X=ncep_time,Y=ncep_reanalysis_areaavg,COL=1
  GPLOT,X=era40_time,Y=era40_reanalysis_areaavg,COL=8
  GPLOT,X=time,Y=thcent_reanalysis_areaavg,COL=26
  GPLOT,X=time,Y=silo_thcent_areaavg,COL=14
  GPLOT,X=era40_time,Y=silo_era40_areaavg,COL=22
  AXES,XSTEP=10,YSTEP=0.5,NDECS=2,XTITLE='Year',/NORIGHT,YTITLE='Rainfall rate (mm/day)'
  GLEGEND,LEGPOS=3,COL=[1,8,26,14,22],LABELS=['NCEP/NCAR','ERA-40','20th Century','SILO on NCEP/NCAR grid','SILO on ERA-40 grid']
  PSCLOSE



 ; Calculating the 11-year runnning RMSE


  ncep_silo_11mean=fltarr(ncep_reanalysis_nyears)
  error_ncep=fltarr(ncep_reanalysis_nyears)
  era40_silo_11mean=fltarr(ncep_reanalysis_nyears)
  error_era40=fltarr(ncep_reanalysis_nyears)
  thcent_silo_11mean=fltarr(thcent_reanalysis_nyears)
  error_thcent=fltarr(thcent_reanalysis_nyears)
  silo_11mean=fltarr(ncep_reanalysis_nyears)
  silo_11mean_era40=fltarr(era40_reanalysis_nyears)
  silo_11mean_thcent=fltarr(thcent_reanalysis_nyears)
  std_ncep_11mean=fltarr(ncep_reanalysis_nyears)
  std_era40_11mean=fltarr(era40_reanalysis_nyears)
  std_thcent_11mean=fltarr(thcent_reanalysis_nyears)
  std_silo_ncep_11mean=fltarr(thcent_reanalysis_nyears)
  std_silo_era40_11mean=fltarr(era40_reanalysis_nyears)


FOR g=5,ncep_reanalysis_nyears-6 DO BEGIN
   ncep_silo_11mean(g)=MEAN(ncep_reanalysis_amean_precip(*,*,g-5:g+5),/NAN)
   silo_11mean(g)=MEAN(silo_ncep_amean_precip(*,*,g-5:g+5),/NAN)
   std_ncep_11mean(g)=STDDEV(ncep_reanalysis_amean_precip(*,*,g-5:g+5),/NAN)
ENDFOR
   
FOR g=5,era40_reanalysis_nyears-6 DO BEGIN
   era40_silo_11mean(g)=MEAN(era40_reanalysis_amean_precip(*,*,g-5:g+5),/NAN)
   silo_11mean_era40(g)=MEAN(silo_era40_amean_precip(*,*,g-5:g+5),/NAN)
   std_era40_11mean(g)=STDDEV(era40_reanalysis_amean_precip(*,*,g-5:g+5),/NAN)
   std_silo_era40_11mean(g)=STDDEV(silo_era40_amean_precip(*,*,g-5:g+5),/NAN)
ENDFOR

FOR g=5,thcent_reanalysis_nyears-6 DO BEGIN
   thcent_silo_11mean(g)=MEAN(thcent_reanalysis_amean_precip(*,*,g-5:g+5),/NAN)
   silo_11mean_thcent(g)=MEAN(silo_thcent_amean_precip(*,*,g-5:g+5),/NAN)
   std_thcent_11mean(g)=STDDEV(thcent_reanalysis_amean_precip(*,*,g-5:g+5),/NAN)
   std_silo_ncep_11mean(g)=STDDEV(silo_thcent_amean_precip(*,*,g-5:g+5),/NAN)
ENDFOR


FOR g=0,ncep_reanalysis_nyears-1 DO BEGIN
   IF silo_11mean(g) eq 0 or ncep_silo_11mean(g) eq 0 or std_ncep_11mean(g) eq 0 THEN BEGIN
      silo_11mean(g)=!Values.F_NaN
      ncep_silo_11mean(g)=!Values.F_Nan
      std_ncep_11mean(g)=!Values.F_Nan
   ENDIF
   error_ncep(g)= silo_11mean(g)-ncep_silo_11mean(g)
   error_ncep(g)=SQRT((error_ncep(g)^2))

ENDFOR

FOR g=0,era40_reanalysis_nyears-1 DO BEGIN
   IF silo_11mean_era40(g) eq 0 or era40_silo_11mean(g) eq 0 or std_era40_11mean(g) eq 0 or std_silo_era40_11mean(g) eq 0 THEN BEGIN
      silo_11mean_era40(g)=!Values.F_NaN
      era40_silo_11mean(g)=!Values.F_Nan
      std_era40_11mean(g)=!Values.F_Nan
      std_silo_era40_11mean(g)=!Values.F_Nan
   ENDIF
   error_era40(g)= silo_11mean_era40(g)-era40_silo_11mean(g)
   error_era40(g)=SQRT((error_era40(g)^2))
ENDFOR

FOR g=0,thcent_reanalysis_nyears-1 DO BEGIN
   IF silo_11mean_thcent(g) eq 0 or thcent_silo_11mean(g) eq 0 or std_thcent_11mean(g) eq 0 or std_silo_ncep_11mean(g) eq 0 THEN BEGIN
      silo_11mean_thcent(g)=!Values.F_NaN
      thcent_silo_11mean(g)=!Values.F_Nan
      std_thcent_11mean(g)=!Values.F_NaN
      std_silo_ncep_11mean(g)=!Values.F_NaN
   ENDIF
   error_thcent(g)= silo_11mean_thcent(g)-thcent_silo_11mean(g)
   error_thcent(g)=SQRT((error_thcent(g)^2))
ENDFOR

 psfile='/home/ss901165/idl/queensland/NE_regional_11running_mean_all_reanalysis.1900-2007.ps'
  PSOPEN,file=psfile
  CS,SCALE=26
  GSET,XMIN=1900,XMAX=2007,YMIN=0,YMAX=4.0,TITLE='11-year running rainfall rate means (NE Australia)'
  GPLOT,X=ncep_time,Y=ncep_silo_11mean,COL=1
  GPLOT,X=era40_time,Y=era40_silo_11mean,COL=8
  GPLOT,X=time,Y=thcent_silo_11mean,COL=26
  GPLOT,X=time,Y=silo_11mean_thcent,COL=14
  GPLOT,X=era40_time,Y=silo_11mean_era40,COL=22
  GPLOT,X=ncep_time,Y=std_ncep_11mean,STYLE=1,COL=1
  GPLOT,X=era40_time,Y=std_era40_11mean,STYLE=1,COL=8
  GPLOT,X=time,Y=std_thcent_11mean,STYLE=1,COL=26
  GPLOT,X=time,Y=std_silo_ncep_11mean,STYLE=1,COL=14
  GPLOT,X=era40_time,Y=std_silo_era40_11mean,STYLE=1,COL=22

 AXES,XSTEP=10,YSTEP=1.0,NDECS=2,XTITLE='Year',/NORIGHT,YTITLE='Rainfall rate (mm/day)'
 GLEGEND,LEGPOS=1,COL=[1,8,26,14,22],LABELS=['NCEP/NCAR','ERA-40','20th Century','SILO on NCEP/NCAR grid','SILO on ERA-40 grid']
PSCLOSE


 psfile='/home/ss901165/idl/queensland/NE_regional_running_rmse_all_reanalysis.1900-2007.ps'
  PSOPEN,file=psfile
  CS,SCALE=26
  GSET,XMIN=1900,XMAX=2007,YMIN=0,YMAX=1.0,TITLE='11-year running RMSE between reanalysis and observations annual-mean daily rainfall rate (NE Australia) '
  GPLOT,X=ncep_time,Y=error_ncep,COL=1
  GPLOT,X=era40_time,Y=error_era40,COL=8
  GPLOT,X=time,Y=error_thcent,COL=26
 AXES,XSTEP=10,YSTEP=0.2,NDECS=2,XTITLE='Year',/NORIGHT,YTITLE='Root Mean Square Error'
 GLEGEND,LEGPOS=1,COL=[1,8,26],LABELS=['NCEP/NCAR','ERA-40','20th Century']
PSCLOSE
 
STOP

END
