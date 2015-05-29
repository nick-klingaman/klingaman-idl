PRO uk_cold_winters_mslp_anomalies_20cr

; Input file of seasonal means MSLP from 20CR
twentyc_seasonal_mslp_file='/home/ss901165/datasets/20THC_REANALYSIS/mslp/20thc_reanalysis.dec-feb_smeans.1871-2007.mslp.nc'
twentyc_clim_mslp_file='/home/ss901165/datasets/20THC_REANALYSIS/mslp/20thc_reanalysis.dec-feb_smean_clim.1871-2007.mslp.nc'

; Give years (in December) for which to compute anomalies
years=[1878,1890,1894,1916,1939,1946,1962]
n_years=N_ELEMENTS(years)

; Give starting year (in December) of the 20CR file
twentyc_start_year=1871

; Contour levels for plot
;anom_levs=['-12.5','-11.5','-10.5','-9.5','-8.5','-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5','8.5','9.5','10.5','11.5','12.5']
anom_levs=['-13','-11','-9','-7','-5','-3','-1','1','3','5','7','9','11','13']

; Read latitude and longitude
twentyc_latitude=OPEN_AND_EXTRACT(twentyc_seasonal_mslp_file,'latitude')
twentyc_longitude=OPEN_AND_EXTRACT(twentyc_seasonal_mslp_file,'longitude')
twentyc_nlat=N_ELEMENTS(twentyc_latitude)
twentyc_nlon=N_ELEMENTS(twentyc_longitude)

; Read climatological MSLP
;twentyc_clim_mslp=OPEN_AND_EXTRACT(twentyc_clim_mslp_file,'PMSL',$
                                  ;offset=[0,0],count=[twentyc_nlon,twentyc_nlat])


; Loop over all years to plot
FOR i=0,n_years-1 DO BEGIN

   twentyc_seasonal_mslp=OPEN_AND_EXTRACT(twentyc_seasonal_mslp_file,'PMSL',$
                                         offset=[0,0,years(i)-twentyc_start_year],$
                                          count=[twentyc_nlon,twentyc_nlat,1])

                                ;Loop over all years using 30 year climate rather than the
                                ;climate of the whole period
   IF years(i)-15 lt twentyc_start_year THEN BEGIN
      year_offset=0
   ENDIF ELSE $
      year_offset=years(i)-15-twentyc_start_year
      
   twentyc_moving_clim_mslp=OPEN_AND_EXTRACT(twentyc_seasonal_mslp_file,'PMSL',$
                                             offset=[0,0,year_offset],$
                                             count=[twentyc_nlon,twentyc_nlat,31])
                                ;Create the array of the globe
   twentyc__mean_clim_mslp=fltarr(twentyc_nlon,twentyc_nlat)
                                ;loop for the mean 
   FOR j=0,twentyc_nlon-1 DO $
      FOR h=0,twentyc_nlat-1 DO $
         twentyc__mean_clim_mslp(j,h)=MEAN(twentyc_moving_clim_mslp(j,h,*))

   twentyc_anomalous_mslp=twentyc_seasonal_mslp-twentyc__mean_clim_mslp
   
                                ; Convert Pa to hPa
   twentyc_anomalous_mslp=twentyc_anomalous_mslp/100.
   
                                ; Plot here
   psfile='/home/ss901165/idl/uk_cold_winters_mslp_anomalies_20cr.djf_anom_30year_'+$
          STRTRIM(STRING(years(i)),1)+'.ps'
   PSOPEN,file=psfile,FONT=6,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=1000,YOFFSET=2000,$
          TFONT=6,TCHARSIZE=120,SPACE3=500
   MAP,LATMIN=20,LATMAX=90,LONMIN=-180,LONMAX=180,/NH
   CS,SCALE=1,NCOLS=N_ELEMENTS(anom_levs)+1,white=[9]
   LEVS,MANUAL=anom_levs
   CON,X=twentyc_longitude,Y=twentyc_latitude,FIELD=twentyc_anomalous_mslp,/NOLINES,/NOCOLBAR
;       TITLE='Anomalous MSLP (hPa) for DJF '+STRTRIM(STRING(years(i)),1),/NOLINES
   AXES
   PSCLOSE
ENDFOR

psfile='/home/ss901165/idl/uk_cold_winters_mslp_anomalies_20cr.djf_colorbar.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=150
MAP,LATMIN=20,LATMAX=90,LONMIN=-180,LONMAX=180,/NH
CS,SCALE=1,NCOLS=N_ELEMENTS(anom_levs)+1,white=[9]
LEVS,MANUAL=anom_levs
COLBAR,COORDS=[3000,3000,3700,18000],TITLE='hPa',/TEXTPOS
PSCLOSE


STOP
END

