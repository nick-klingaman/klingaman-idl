PRO mjo_coupling_plot_phase_relationship_filter

; All IDL programs begin with a "PRO [name]" statement.  The [name] should
; be the same as the name of this file, without the .pro extension.

; Plot the phase relationship (in time) between two variables.
; This example is for outgoing longwave radiation (OLR) and 
; sea-surface temperature (SST).
;
; This version of the program allows for bandpass-filtered data.  The bandpass filter
; affects the first n/2 and last n/2 days of data in the dataset, where n is the maximum
; period permitted by the filter (e.g., for a 20-100 day filter, the first and last 50 days
; are affected).  Specify the number of days affected using the filter_first and filter_last
; settings below.
; 
; If you are not analysing the first or last year in the dataset (i.e., year_offset is not
; equal to zero or n_years is not equal to the number of years in the dataset), then you can
; safely set filter_first and/or filter_last equal to zero.
;

; Give the locations of the input files for OLR and SST.
olr_file='/home/ss901165/datasets/NOAA_CIRES_OLR/daily/NOAA_CIRES_OLR.jan-dec_dmeans.1998-2009.filter20-100.2.5x2.5.nc'
sst_file='/home/ss901165/datasets/TMI_AMSRE/n96/tmi_fusion.jan-dec_dmeans.1998-2009.filter20-100.n96.nc'

; Define the region over which to area-average the variables before taking the lead-lag correlation.  
; The values are: [starting_latitude,starting_longitude,stopping_latitude,stopping_longitude]
box=[-10,70,10,90] ; Box in the Indian Ocean.

; Number of days to read per year
n_days_per_year=365
; Starting day to read
day_offset=0
; Number of years to read
n_years=12
; Starting year to read
year_offset=0

; Number of days affected by the filter in the first year of data analysed
filter_first=50
; Number of days affected by the filter in the last year of data analysed
filter_last=50

; Read longitude and latitude co-ordinate variables from the rainfall file.
olr_longitude=OPEN_AND_EXTRACT(olr_file,'longitude')
olr_latitude=OPEN_AND_EXTRACT(olr_file,'latitude')
; Restrict the dimensions to the box specified above.
DEFINE_BOUNDARIES,box,olr_latitude,olr_longitude,olr_box_tx,/LIMIT
; Get the number of longitude and latitude points in the "box" region.
olr_nlon=N_ELEMENTS(olr_longitude)
olr_nlat=N_ELEMENTS(olr_latitude)

; Read longitude and latitude co-ordinate variables from the land/sea mask file
sst_longitude=OPEN_AND_EXTRACT(sst_file,'longitude')
sst_latitude=OPEN_AND_EXTRACT(sst_file,'latitude')
DEFINE_BOUNDARIES,box,sst_latitude,sst_longitude,sst_box_tx,/LIMIT
sst_nlon=N_ELEMENTS(sst_longitude)
sst_nlat=N_ELEMENTS(sst_latitude)

; Read the daily OLR and SST values from the dataset.
daily_olr=OPEN_AND_EXTRACT(olr_file,'olr',$
                           offset=[olr_box_tx(1),olr_box_tx(0),day_offset,year_offset],$
                           count=[olr_nlon,olr_nlat,n_days_per_year,n_years])
daily_sst=OPEN_AND_EXTRACT(sst_file,'sst',$
                           offset=[sst_box_tx(1),sst_box_tx(0),day_offset,year_offset],$
                           count=[sst_nlon,sst_nlat,n_days_per_year,n_years])

; Create new variables to hold the box-average daily OLR and SST.
daily_olr_avg=fltarr(n_days_per_year,n_years)
daily_sst_avg=fltarr(n_days_per_year,n_years)

; Set the parts of the dataset affected by the bandpass filter to be missing values.
; In IDL, a missing value is specified by !Values.F_NaN.
IF filter_first gt 0 THEN BEGIN
   daily_olr(*,*,0:filter_first-1,0)=!Values.F_NaN
   daily_sst(*,*,0:filter_first-1,0)=!Values.F_NaN
ENDIF
IF filter_last gt 0 THEN BEGIN
   daily_olr(*,*,n_days_per_year-filter_last:n_days_per_year-1,n_years-1)=!Values.F_NaN
   daily_sst(*,*,n_days_per_year-filter_last:n_days_per_year-1,n_years-1)=!Values.F_NaN
ENDIF

; Take the box-average OLR and SST, after first removing the seasonal cycle (daily climatology).
FOR i=0,n_days_per_year-1 DO BEGIN
   FOR j=0,n_years-1 DO BEGIN
      daily_olr_avg(i,j)=MEAN(daily_olr(*,*,i,j),/NaN)-MEAN(daily_olr(*,*,i,*),/NaN)
      daily_sst_avg(i,j)=MEAN(daily_sst(*,*,i,j),/NaN)-MEAN(daily_sst(*,*,i,*),/NaN)
   ENDFOR
ENDFOR

; Compute the lead-lag correlation using the IDL C_CORRELATE routine
; Lags over which to compute the correlation.  Negative implies OLR leads SST, positive implies SST leads OLR.
lags=[-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10]
n_lags=N_ELEMENTS(lags)

; Do the correlation for each year, then take the average over all years.
cross_correlation=fltarr(n_lags)
; n_valid_days stores the number of days that are *not* affected by the filter.  This variable is then
; used to make a weighted average of the correlations over all years, with the weight equal to the number
; of unaffected days.
n_valid_days=fltarr(n_years)
FOR i=0,n_years-1 DO BEGIN
                                ; If some data has been affected by the filter, make sure not to use that data
                                ; for the correlation analysis.
   IF i eq 0 and filter_first gt 0 THEN BEGIN
      thisyear_correlation=C_CORRELATE(daily_sst_avg(filter_first:n_days_per_year-1,i),$
                                       daily_olr_avg(filter_first:n_days_per_year-1,i),lags)
      n_valid_days(i)=n_days_per_year-filter_first
   ENDIF ELSE IF i eq n_years-1 and filter_last gt 0 THEN BEGIN
      thisyear_correlation=C_CORRELATE(daily_sst_avg(0:filter_last-1,i),$
                                       daily_olr_avg(0:filter_last-1,i),lags)
      n_valid_days(i)=n_days_per_year-filter_last
   ENDIF ELSE BEGIN
      thisyear_correlation=C_CORRELATE(daily_sst_avg(*,i),daily_olr_avg(*,i),lags)
      n_valid_days(i)=n_days_per_year
   ENDELSE
   cross_correlation=cross_correlation+thisyear_correlation*n_valid_days(i)
ENDFOR
; Take the weighted average of the correlation coefficients using the number of days
; not affected by the bandpass filter.
cross_correlation=cross_correlation/TOTAL(FLOAT(n_valid_days))

; Plot the data.  See the IDL Guide for how these commands work.

; Give the location of the PostScript file that this program will make.
; This needs to be somewhere in your /home directory.

; Change this variable to point to somewhere in your Unix home space (e.g., /home/sv025016/idl ...)
psfile='/home/ss901165/idl/mjo_coupling/mjo_coupling_plot_phase_relationship_filter.obs_olr_sst_IndOcn.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,$
       TCHARSIZE=100,SPACE3=500
; Set the boundaries of the axes
GSET,XMIN=MIN(lags),XMAX=MAX(lags),YMIN=-0.5,YMAX=0.5,TITLE='Lead-lag correlation between 20-100 day bandpass-filtered SST and OLR in the Indian Ocean from observations (1998-2009)'
; Plot the lagged correlation
GPLOT,X=lags,Y=cross_correlation
; Set the axes tick marks and labels
AXES,XVALS=lags,YSTEP=0.05,YTITLE='Correlation coefficient',XTITLE='Lag (days, positive means SST leads OLR)',NDECS=2
; Put dotted lines at zero lag and zero correlation
GPLOT,X=[0,0],Y=[-0.5,0.5],STYLE=1
GPLOT,X=[MIN(lags),MAX(lags)],Y=[0,0],STYLE=1
; Close and display
PSCLOSE

STOP
END
