PRO qld_wet_days_percentiles_freqtrend_corr_nino4_ipo_positive
  
; Calculate and plot the linear trend in the frequency of days 
; above a given rainfall percentile

; Input file containing rainfall percentiles (note: file is already restricted to a box approximating Queensland)
input_file='/home/ss901165/datasets_mango/SILO/one_quarter/SILO_precip_percentiles.may-apr_dmeans.1900-2008.0.25x0.25.nc'
nino4='/home/ss901165/datasets/NINO/nino4_hadisst.sep-nov_smeans.1871-2007.nc'
ipo_file='/home/ss901165/datasets/HADISST/hadisst_ipo.sep-nov_smeans.1871-2008.nc'
; Percentile threshold (consider days >= this percentile)
percentile=95

; Read latitude and longitude from the file
longitude=OPEN_AND_EXTRACT(input_file,'longitude')
latitude=OPEN_AND_EXTRACT(input_file,'latitude')
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)

; Define period of the year to analyze (set to 0 and 365 for the whole year)
; The start and stop dates are of the form [date,month,year].  Keep the year set to 1901,
; no matter what set of years you are actually analyzing; just change the month and date.
start_date=[1,9,1901]          ; 1 March
stop_date=[30,11,1901]           ; 31 May
day_offset=GREGORIAN_TO_JULIAN(start_date(0),start_date(1),start_date(2))-$
           GREGORIAN_TO_JULIAN(1,5,1901)
IF day_offset lt 0 THEN $
   day_offset=day_offset+365
n_days=GREGORIAN_TO_JULIAN(stop_date(0),stop_date(1),stop_date(2))-$
       GREGORIAN_TO_JULIAN(start_date(0),start_date(1),start_date(2))
IF n_days lt 0 THEN $
   n_days=n_days+365
print,day_offset,n_days

; Define years to analyze (years available: 1900-2007)
start_year=1905
stop_year=2002
year_offset=start_year-1900
n_years=stop_year-start_year+1

start_year_ipo=1900
stop_year_ipo=2007
year_offset_ipo=start_year_ipo-1900
n_years_ipo=stop_year_ipo-start_year_ipo+1
; Define significance level for correlation
;sig_level=0.189

; Read rainfall percentiles
rainfall_percentiles=OPEN_AND_EXTRACT(input_file,'percentile_of_daily_rain',$
                                      offset=[0,0,day_offset,year_offset],$
                                      count=[n_lon,n_lat,n_days,n_years])
nino4_season=OPEN_AND_EXTRACT(nino4,'NINO4',$
                               offset=[34],$
                               count=[n_years])
ipo_index=OPEN_AND_EXTRACT(ipo_file,'ipo_index',$
                           offset=[29],$
                           count=[n_years_ipo])

; Smooth IPO index into 11-year moving average
ipo_index_smoothed=SMOOTH(ipo_index,11)
ipo_index_smoothed=ipo_index_smoothed(5:n_years_ipo-6)

; Set missing values to zero
rainfall_percentiles[where(rainfall_percentiles eq 2e20)]=0.

; Correlating with NINO4 SSTs
correlations=fltarr(n_lon,n_lat)

; For each year compute the number of days above the chosen percentile
exceedance_frequency=fltarr(n_lon,n_lat,n_years)
exceedance_frequency_trend=fltarr(n_lon,n_lat)
exceedance_frequency_corr=fltarr(n_lon,n_lat)
FOR i=0,n_lon-1 DO BEGIN
   FOR j=0,n_lat-1 DO BEGIN
      FOR k=0,n_years-1 DO BEGIN
         thispt_thisyear=REFORM(rainfall_percentiles(i,j,*,k))
         IF TOTAL(where(thispt_thisyear ge percentile) gt 0) THEN BEGIN
            exceedance_frequency(i,j,k)=N_ELEMENTS(where(thispt_thisyear ge percentile))
         ENDIF ELSE $
            exceedance_frequency(i,j,k)=0
      ENDFOR
      exceedance_frequency_trend(i,j)=REGRESS(indgen(n_years),REFORM(exceedance_frequency(i,j,*)),CORRELATION=temp)
      exceedance_frequency_corr(i,j)=temp
   ENDFOR
ENDFOR

n_years_ipo_positive=N_ELEMENTS(where(ipo_index_smoothed gt 0))
n_years_ipo_negative=N_ELEMENTS(where(ipo_index_smoothed lt 0))

rainfall_ipo_positive=fltarr(n_lon,n_lat,n_years_ipo_positive)
rainfall_ipo_negative=fltarr(n_lon,n_lat,n_years_ipo_negative)
nino4_ipo_positive=fltarr(n_years_ipo_positive-11)
nino4_ipo_negative=fltarr(n_years_ipo_negative-11)

FOR i=0,n_lon-1 DO BEGIN
FOR j=0,n_lat-1 DO BEGIN

;Make a temporary one-dimensional array to hold data for this gridpoint
   temp=REFORM(exceedance_frequency(i,j,*))

 ;Subscript the temporary array for positive and negative
   rainfall_ipo_positive(i,j,*)=temp[where(ipo_index_smoothed gt 0)]
   rainfall_ipo_negative(i,j,*)=temp[where(ipo_index_smoothed lt 0)]

ENDFOR
ENDFOR

;rainfall_ipo_positive=exceedance_frequency[where(ipo_index_smoothed gt 0)]
;rainfall_ipo_negative=exceedance_frequency[where(ipo_index_smoothed lt 0)]
nino4_ipo_positive=nino4_season[where(ipo_index_smoothed gt 0)]
nino4_ipo_negative=nino4_season[where(ipo_index_smoothed lt 0)]

FOR i=0,n_lon-1 DO BEGIN
   FOR j=0,n_lat-1 DO BEGIN
      correlations(i,j)=CORRELATE(REFORM(rainfall_ipo_positive(i,j,*)),nino4_ipo_positive)
   ENDFOR
ENDFOR

;mylevs=['-0.075','-0.065','-0.055','-0.045','-0.035','-0.025','-0.015','-0.005',$
       ; '0.005','0.015','0.025','0.035','0.045','0.055','0.065','0.075']*4.
mylevs=['-0.35','-0.25','-0.15','-0.05','0.05','0.15','0.25']
;psfile='/home/swu07adk/silo/qld_wet_days_percentiles_nion4.sep-nov_ipo_positive.ps'
psfile='/home/ss901165/idl/queensland/wet_days/percentiles/qld_wet_days_percentiles_freqtrend.'+$
       STRTRIM(STRING(percentile),1)+'.dec-mar_1900-2008.ps'

PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=1500,TFONT=2,$
       TCHARSIZE=80,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[6]
; To use the high-resolution map of Queensland, add the /SET option to MAP and 
; un-comment the line of code as indicated below.
MAP,LONMIN=MIN(longitude),LONMAX=MAX(longitude),LATMIN=MIN(latitude),LATMAX=MAX(latitude),/hires;,/SET
LEVS,MANUAL=mylevs
CON,FIELD=correlations,Y=latitude,X=longitude,$
    TITLE='Correlation between frequency of exceeding climatological '+STRTRIM(STRING(percentile),1)+$
    ' percentile of daily rainfall - Sep-Nov - SILO 1900-2007 and Sep-Nov NINO4 SSTs when IPO is positive',/NOLINES,/BLOCK,$
    CB_TITLE='Correlation coefficient'
;FOR i=0,n_lon-1 DO $
   ;FOR j=0,n_lat-1 DO $
      ;IF (ABS(correlations(i,j)) gt sig_level) THEN $
        ; GPLOT,X=longitude(i),Y=latitude(j),SYM=4,/NOLINES,SIZE=40
AXES
PSCLOSE


STOP
END

