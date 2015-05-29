PRO qld_wet_days_percentiles_freqtrend_scatter_region_99
  
; Produce a scatter plot of Nino4-SSTs with exceedance frequency for
; region 3

; Input file containing rainfall percentiles (note: file is already restricted to a box approximating Queensland)
input_file='/home/ss901165/datasets_mango/SILO/one_quarter/SILO_precip_percentiles.may-apr_dmeans.1900-2008.0.25x0.25.nc'
nino4='/home/ss901165/datasets/NINO/nino4_hadisst.sep-nov_smeans.1871-2007.nc'
mask_file='/home/ss901165/datasets_mango/SILO/one_quarter/SILO_qld_region_mask.0.25x0.25.may-apr.nc'

; Percentile threshold (consider days >= this percentile)
percentile=99

; Read latitude and longitude from the file
longitude=OPEN_AND_EXTRACT(input_file,'longitude')
latitude=OPEN_AND_EXTRACT(input_file,'latitude')
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)
mask_longitude=OPEN_AND_EXTRACT(mask_file,'longitude')
mask_latitude=OPEN_AND_EXTRACT(mask_file,'latitude')
DEFINE_BOUNDARIES,[latitude(0),longitude(0),latitude(n_lat-1),longitude(n_lon-1)],mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)

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
start_year=1900
stop_year=2007
year_offset=start_year-1900
n_years=stop_year-start_year+1

; Read rainfall percentiles
rainfall_percentiles=OPEN_AND_EXTRACT(input_file,'percentile_of_daily_rain',$
                                      offset=[0,0,day_offset,year_offset],$
                                      count=[n_lon,n_lat,n_days,n_years])
nino4_ssts=OPEN_AND_EXTRACT(nino4,'NINO4',$
                               offset=[29],$
                                      count=[n_years])
mask=OPEN_AND_EXTRACT(mask_file,'silo_regions',$
                      offset=[mask_box_tx(1),mask_box_tx(0)],$
                      count=[mask_nlon,mask_nlat])

nino4_anomalies=nino4_ssts-MEAN(nino4_ssts)
en_anomalies=nino4_anomalies gt 0.2
ln_anomalies=nino4_anomalies lt 0.2

; Set missing values to zero
rainfall_percentiles[where(rainfall_percentiles eq 2e20)]=0.
exceedance_frequency=fltarr(n_lon,n_lat,n_years)

; Compute exceedance frequency for all points
FOR i=0,n_lon-1 DO BEGIN
FOR j=0,n_lat-1 DO BEGIN
FOR k=0,n_years-1 DO BEGIN
thispt_thisyear=REFORM(rainfall_percentiles(i,j,*,k))
IF TOTAL(where(thispt_thisyear ge percentile) gt 0) THEN BEGIN
exceedance_frequency(i,j,k)=N_ELEMENTS(where(thispt_thisyear ge percentile))
ENDIF ELSE $
exceedance_frequency(i,j,k)=0
ENDFOR
ENDFOR
ENDFOR

; Compute area-averaged exceedance frequency for region 3
exceedance_frequency_area_avg=fltarr(n_years)
FOR k=0,n_years-1 DO BEGIN
temp=REFORM(exceedance_frequency(*,*,k))
exceedance_frequency_area_avg(k)=MEAN(temp[where(mask eq 3)])
ENDFOR

nino4_anomalies_filter=nino4_anomalies(where(exceedance_frequency_area_avg lt 0.2))

exceedance_frequency_area_avg_en=exceedance_frequency_area_avg(where(nino4_anomalies gt 0.2))
exceedance_frequency_area_avg_ln=exceedance_frequency_area_avg(where(nino4_anomalies_filter lt 0.2))

; Compute correlation between SSTs and exceedance frequency
correlation_en=CORRELATE(exceedance_frequency_area_avg_en,en_anomalies)
correlation_ln=CORRELATE(exceedance_frequency_area_avg_ln,ln_anomalies)

psfile='/home/swu07adk/silo/qld_wet_days_percentiles_nion4.sep-nov_scatter_reg3_99.ps'

PSOPEN,file=psfile,THICK=200,TCHARSIZE=100
xpts=nino4_anomalies
;xpts_en=en_anomalies
;xpts_ln=ln_anomalies
ypts=exceedance_frequency_area_avg
;ypts_en=exceedance_frequency_area_avg_en
;ypts_ln=exceedance_frequency_area_avg_ln

GSET,XMIN=-2,XMAX=2,YMIN=0,YMAX=0.8,TITLE='Scatter-plot of NINO4 SST anomalies and exceedance frequency at the 99th percentile in Region 3 (Sep-Nov)'
GPLOT,X=xpts,Y=ypts,SYM=2,/NOLINES
;coefficients=LINFIT(xpts_en,ypts_en,yfit=yfit_en)
;coefficients=LINFIT(xpts_ln,ypts_ln,yfit=yfit_ln)
;GPLOT,X=xpts_en,Y=yfit_en
;GPLOT,X=xpts_ln,Y=yfit_ln
AXES,XTITLE='NINO4 SST anomalies (K)',YTITLE='Exceedance frequency at the 99th percentile (Days)',NDECS=2
PSCLOSE


STOP
END

