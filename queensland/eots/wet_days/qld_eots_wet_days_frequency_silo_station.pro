PRO qld_eots_wet_days_frequency_silo_station

seasons=['dec-feb','mar-may','jun-aug','sep-nov']
seasons_shift=[1,0,0,0]
n_seasons=N_ELEMENTS(seasons)
n_eots=3
silo_dir='/home/ss901165/datasets_mango/SILO/one_quarter'
eot_nyears=108
station_nyears=88

box=[-30,138,-10,154]
mylevs_regress=[['-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5'],$
                ['-1.2','-0.4','0.4','1.2','2.0','2.8','3.6','4.4','5.2','6.0'],$
                ['-0.6','-0.2','0.2','0.6','1.0','1.4','1.8','2.2','2.6','3.0']]

silo_wetdays_infile=silo_dir+'/SILO_precip.wet_days_threshold.1900-2008.0.25x0.25.nc'
n_thresholds=3
rainfall_thresholds=OPEN_AND_EXTRACT(silo_wetdays_infile,'threshold',$
                                     offset=[0],count=[n_thresholds])
mask_file='/home/ss901165/datasets_mango/SILO/one_quarter_lsm.nc'
mask_longitude=OPEN_AND_EXTRACT(mask_file,'longitude')
mask_latitude=OPEN_AND_EXTRACT(mask_file,'latitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)
mask=REFORM(OPEN_AND_EXTRACT(mask_file,'lsm',$
                             offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                             count=[mask_nlon,mask_nlat,1,1]))

FOR i=0,n_seasons-1 DO BEGIN
   silo_eots_infile=silo_dir+'/SILO.'+seasons(i)+'_smeans.1900-2007.eots.nc'
   silo_eots=REFORM(OPEN_AND_EXTRACT(silo_eots_infile,'loading',$
                                     offset=[0,0],count=[eot_nyears,n_eots]))

   silo_longitude=OPEN_AND_EXTRACT(silo_wetdays_infile,'longitude')
   silo_latitude=OPEN_AND_EXTRACT(silo_wetdays_infile,'latitude')
   DEFINE_BOUNDARIES,box,silo_latitude,silo_longitude,silo_box_tx,/LIMIT
   silo_nlon=N_ELEMENTS(silo_longitude)
   silo_nlat=N_ELEMENTS(silo_latitude)
   
   silo_wetdays=REFORM(OPEN_AND_EXTRACT(silo_wetdays_infile,'ndays_over_seas',$
                                        offset=[silo_box_tx(1),silo_box_tx(0),seasons_shift(i),i,0],$
                                        count=[silo_nlon,silo_nlat,eot_nyears,1,n_thresholds]))

   silo_frequency_regressions=fltarr(n_eots,n_thresholds,silo_nlon,silo_nlat)
   silo_frequency_correlations=fltarr(n_eots,n_thresholds,silo_nlon,silo_nlat)
   FOR j=0,silo_nlon-1 DO BEGIN
      FOR k=0,silo_nlat-1 DO BEGIN
         IF TOTAL(silo_wetdays(j,k,*,*)) ne 0 and mask(j,k) eq 1 THEN BEGIN
            FOR m=0,n_eots-1 DO BEGIN
               FOR n=0,n_thresholds-1 DO BEGIN
                  silo_frequency_regressions(m,n,j,k)=REGRESS(REFORM(silo_eots(*,m)),$
                                                              REFORM(silo_wetdays(j,k,*,n)),$
                                                              CORRELATION=temp)*STDDEV(silo_eots(*,m))
                  silo_frequency_correlations(m,n,j,k)=temp(0)
                  IF ABS(temp(0)) lt 0.198 THEN $
                     silo_frequency_regressions(m,n,j,k)=0.
               ENDFOR
            ENDFOR
         ENDIF
      ENDFOR
   ENDFOR
   
   FOR j=0,n_eots-1 DO BEGIN
      FOR k=0,n_thresholds-1 DO BEGIN
         psfile='/home/ss901165/idl/queensland/eots/wet_days/qld_eots_wet_days_frequency_silo_station.silo25km_'+seasons(i)+'_eot'+STRTRIM(STRING(j+1),1)+'_'+STRTRIM(STRING(FLOOR(rainfall_thresholds(k))),1)+'mm.ps'
         PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=100,CB_WIDTH=105,/PORTRAIT
         CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_regress(*,k))+1,MIN=8,white=[4]
         MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2),/hires,/SET
         LEVS,MANUAL=REFORM(mylevs_regress(*,k))
         CON,X=silo_longitude,Y=silo_latitude,FIELD=REFORM(silo_frequency_regressions(j,k,*,*)),/BLOCK,/NOLINES,$
             CB_TITLE='Days per '+STRTRIM(STRING(FLOOR(STDDEV(silo_eots(*,j)))),1)+$
             ' mm season!U-1!N (1 stddev) change in rainfall EOT timeseries',TITLE='Regression of frequency of >= '+$
             STRTRIM(STRING(FLOOR(rainfall_thresholds(k))),1)+' mm day!U-1!N onto '+seasons(i)+' EOT '+STRTRIM(STRING(j+1),1)+$
             ' timeseries - SILO 0.25!Uo!N for 1900-2007'
         DRAWSTATES,'/home/ss901165/idl/queensland/shapefiles/aust_cd66eaststates.shp',$
                    ATTRIBUTE_NAME='STE',COLOR="black",statenames='3',THICK=3
         AXES,XVALS=['138','140','142','144','146','148','150','152','154'],$
              YVALS=['-10','-12','-14','-16','-18','-20','-22','-24','-26','-28','-30']
         PSCLOSE,/NOVIEW
      ENDFOR
   ENDFOR
ENDFOR

STOP
END
