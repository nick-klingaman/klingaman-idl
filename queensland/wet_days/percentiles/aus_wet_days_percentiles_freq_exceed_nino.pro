PRO aus_wet_days_percentiles_freq_exceed_nino

; Plot the frequency of exceeding a climatological percentile of daily
; rainfall during a specified period of years (here, separated into El Nino
; and La Nina years)
  
; Input file containing rainfall percentiles
  rainfall_input_file='/home/ss901165/datasets_mango/SILO/t62/SILO.may-apr_dmeans.1958-2000.precip_percentiles.t62.nc'
; Input file containing Nino 4 SST anomalies for the season of interest
  nino_input_file='/home/ss901165/datasets/NINO/nino4_hadisst.dec-feb_smeans.1958-2000.nc'

; Specify a season of interest, using 1 May as day zero
start_date=214 ; Equal to 1 December
stop_date=303 ; Equal to 28 February
n_days=stop_date-start_date+1
season_name='dec-feb'

; Give the percentile for which you want to compute the frequency of exceedance
percentile_to_plot=95

; Give the range of years to consider
start_year=0
stop_year=42
n_years=stop_year-start_year+1

; Read latitude and longitude from the file
longitude=OPEN_AND_EXTRACT(rainfall_input_file,'longitude')
latitude=OPEN_AND_EXTRACT(rainfall_input_file,'latitude')
; Get the number of latitude and longitude points
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)

; Read the daily percentiles of rainfall
daily_percentiles=OPEN_AND_EXTRACT(rainfall_input_file,'percentile_of_daily_rain',$
                                   offset=[0,0,start_year,start_date],$
                                   count=[n_lon,n_lat,n_years,n_days])
daily_percentiles[where(daily_percentiles gt 2e19)]=!Values.F_NaN

; Read the timeseries of Nino 4 SST values
nino4_sst=OPEN_AND_EXTRACT(nino_input_file,'NINO4',$
                           offset=[start_year],count=[n_years])

; Compute El Nino and La Nina years
nino4_sst=nino4_sst-MEAN(nino4_sst)
elnino_years=where(nino4_sst ge 0.5);MEAN(nino4_sst)+STDDEV(nino4_sst))
lanina_years=where(nino4_sst le -0.5);MEAN(nino4_sst)-STDDEV(nino4_sst))
neutral_years=where(ABS(nino4_sst) lt 0.5)

elnino_frequency_exceeding=fltarr(n_lon,n_lat)
lanina_frequency_exceeding=fltarr(n_lon,n_lat)
neutral_frequency_exceeding=fltarr(n_lon,n_lat)

FOR i=0,n_lon-1 DO BEGIN
   FOR j=0,n_lat-1 DO BEGIN
      IF TOTAL(where(FINITE(daily_percentiles(i,j,*,*)) eq 1)) gt 0 THEN BEGIN
         thispt_rainfall_percentiles=REFORM(daily_percentiles(i,j,*,*))
         daily_percentiles_elnino=thispt_rainfall_percentiles(elnino_years,*)
         elnino_frequency_exceeding(i,j)=N_ELEMENTS(where(daily_percentiles_elnino gt percentile_to_plot))
         daily_percentiles_lanina=thispt_rainfall_percentiles(lanina_years,*)
         lanina_frequency_exceeding(i,j)=N_ELEMENTS(where(daily_percentiles_lanina gt percentile_to_plot))
         daily_percentiles_neutral=thispt_rainfall_percentiles(neutral_years,*)
         neutral_frequency_exceeding(i,j)=N_ELEMENTS(where(daily_percentiles_neutral gt percentile_to_plot))
      ENDIF
   ENDFOR
ENDFOR

elnino_frequency_exceeding=elnino_frequency_exceeding/FLOAT(N_ELEMENTS(elnino_years))
lanina_frequency_exceeding=lanina_frequency_exceeding/FLOAT(N_ELEMENTS(lanina_years))
neutral_frequency_exceeding=neutral_frequency_exceeding/FLOAT(N_ELEMENTS(neutral_years))

STOP
END

