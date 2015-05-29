PRO uk_cold_winters_wind_plots_850_DEC2010

; Input file of seasonal means MSLP from 20CR
twentyc_dec_uvel_file='/home/ss901165/datasets/20THC_REANALYSIS/zonal_wind/20thc_reanalysis.dec_mmeans.1871-2010.uvel850.nc'
twentyc_dec_vvel_file='/home/ss901165/datasets/20THC_REANALYSIS/meridional_wind/20thc_reanalysis.dec_mmeans.1871-2010.vvel850.nc'
twentyc_dec_windspeed_file='/home/ss901165/datasets/20THC_REANALYSIS/wind_speed/20thc_reanalysis.dec_mmeans.1871-2010.wind850.nc'

; Give years (in December) for which to compute anomalies
start_year=1871
end_year=2010
n_years=(end_year-start_year)+1
twentyc_start_year=1871

; Contour levels for plot
anom_levs=['-10','-9','-8','-7','-6','-5','-4','-3','-2','-1','0','1','2','3','4','5','6','7','8','9','10']

; Read latitude and longitude
twentyc_latitude=OPEN_AND_EXTRACT(twentyc_dec_uvel_file,'latitude')
twentyc_longitude=OPEN_AND_EXTRACT(twentyc_dec_uvel_file,'longitude')
twentyc_nlat=N_ELEMENTS(twentyc_latitude)
twentyc_nlon=N_ELEMENTS(twentyc_longitude)

twentyc_dec_uvel=fltarr(twentyc_nlon,twentyc_nlat,n_years)
twentyc_dec_vvel=fltarr(twentyc_nlon,twentyc_nlat,n_years)
twentyc_dec_windspeed=fltarr(twentyc_nlon,twentyc_nlat,n_years)

twentyc_dec_uvel=OPEN_AND_EXTRACT(twentyc_dec_uvel_file,'U',$
                                  offset=[0,0,(start_year-twentyc_start_year)],count=[twentyc_nlon,twentyc_nlat,n_years])

twentyc_dec_vvel=OPEN_AND_EXTRACT(twentyc_dec_vvel_file,'V',$
                                  offset=[0,0,(start_year-twentyc_start_year)],count=[twentyc_nlon,twentyc_nlat,n_years])

twentyc_dec_windspeed=OPEN_AND_EXTRACT(twentyc_dec_windspeed_file,'wind',$
                                  offset=[0,0,(start_year-twentyc_start_year)],count=[twentyc_nlon,twentyc_nlat,n_years])


dec_2010_uvel=(twentyc_dec_uvel(*,*,139))
dec_2010_vvel=(twentyc_dec_vvel(*,*,139))
dec_2010_windspeed=(twentyc_dec_windspeed(*,*,139))

mean_total_uvel=fltarr(twentyc_nlon,twentyc_nlat)
mean_total_vvel=fltarr(twentyc_nlon,twentyc_nlat)
mean_total_windspeed=fltarr(twentyc_nlon,twentyc_nlat)

mean_30_year_uvel=fltarr(twentyc_nlon,twentyc_nlat)
mean_30_year_vvel=fltarr(twentyc_nlon,twentyc_nlat)
mean_30_year_windspeed=fltarr(twentyc_nlon,twentyc_nlat)
FOR i=0,twentyc_nlon-1 DO BEGIN
   FOR j=0,twentyc_nlat-1 DO BEGIN
      mean_total_uvel(i,j)=(MEAN(twentyc_dec_uvel(i,j,*)))
      mean_total_vvel(i,j)=(MEAN(twentyc_dec_vvel(i,j,*)))
      mean_total_windspeed(i,j)=(MEAN(twentyc_dec_windspeed(i,j,*)))
      
      
      mean_30_year_uvel(i,j)=(MEAN(twentyc_dec_uvel(i,j,108:137)))
      mean_30_year_vvel(i,j)=(MEAN(twentyc_dec_vvel(i,j,108:137)))
      mean_30_year_windspeed(i,j)=(MEAN(twentyc_dec_windspeed(i,j,108:137)))
   ENDFOR
ENDFOR

dec_2010_anom_total_uvel=fltarr(twentyc_nlon,twentyc_nlat)
dec_2010_anom_total_vvel=fltarr(twentyc_nlon,twentyc_nlat)
dec_2010_anom_total_windspeed=fltarr(twentyc_nlon,twentyc_nlat)


dec_2010_anom_30_year_uvel=fltarr(twentyc_nlon,twentyc_nlat)
dec_2010_anom_30_year_vvel=fltarr(twentyc_nlon,twentyc_nlat)
dec_2010_anom_30_year_windspeed=fltarr(twentyc_nlon,twentyc_nlat)


dec_2010_anom_total_uvel=(dec_2010_uvel-mean_total_uvel)
dec_2010_anom_total_vvel=(dec_2010_vvel-mean_total_vvel)
dec_2010_anom_total_windspeed=(dec_2010_windspeed-mean_total_windspeed)


dec_2010_anom_30_year_uvel=(dec_2010_uvel-mean_30_year_uvel)
dec_2010_anom_30_year_vvel=(dec_2010_vvel-mean_30_year_vvel)
dec_2010_anom_30_year_windspeed=(dec_2010_windspeed-mean_30_year_windspeed)

   ; Plot here
   psfile='/home/ss901165/idl/uk_cold_winters/uk_cold_winters_winds_850_dec2010_total_anom.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=1000,YOFFSET=2000,$
          TFONT=2,TCHARSIZE=100
   MAP,LONMIN=-120,LONMAX=70,LATMAX=90,LATMIN=-10,/HIRES
   CS,SCALE=1,NCOLS=N_ELEMENTS(anom_levs)+1
   LEVS,MANUAL=anom_levs
   CON,X=twentyc_longitude,Y=twentyc_latitude,FIELD=dec_2010_anom_total_windspeed,$
       TITLE='Anomalous Winds at 850 hPa for December 2010 using the 140 year mean',/NOLINES
   VECT,X=twentyc_longitude,Y=twentyc_latitude,U=dec_2010_anom_total_uvel,V=dec_2010_anom_total_vvel,MAG=5,STRIDE=2
 AXES
   PSCLOSE

 ; Plot here
   psfile='/home/ss901165/idl/uk_cold_winters/uk_cold_winters_winds_850_dec2010_30_year_anom.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=1000,YOFFSET=2000,$
          TFONT=2,TCHARSIZE=100
   MAP,LONMIN=-120,LONMAX=70,LATMAX=90,LATMIN=-10,/HIRES
   CS,SCALE=1,NCOLS=N_ELEMENTS(anom_levs)+1
   LEVS,MANUAL=anom_levs
   CON,X=twentyc_longitude,Y=twentyc_latitude,FIELD=dec_2010_anom_30_year_windspeed,$
       TITLE='Anomalous Winds at 850 hPa for December 2010 using the 30 year mean',/NOLINES 
 VECT,X=twentyc_longitude,Y=twentyc_latitude,U=dec_2010_anom_30_year_uvel,V=dec_2010_anom_30_year_vvel,MAG=5,STRIDE=2
   AXES
   PSCLOSE


 

STOP
END

