PRO mjo_coupling_plot_phase_relationship_filter_latavg

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
;y_file='/home/ss901165/datasets/TMI_AMSRE/two_half/tmi_fusion.jan-dec_dmeans.1998-2009.filter20-100.2.5x2.5.nc'
;x_file='/home/ss901165/datasets/ERA-INTERIM/SLHF/ERA_interim.jan-dec_dmeans.1998-2009.SLHF.tropics.2.5x2.5.filter20-100.nc'
;y_name='sst'
;x_name='SLHF'
;type='obs'

x_file='/home/ss901165/um_output5/xgspm/hadgem3kpp_1.5xentrain_ga30.jan-dec_dmeans.i2-k1.lh.filter20-100.nc'
y_file='/home/ss901165/um_output5/xgspm/hadgem3kpp_1.5xentrain_ga30.jan-dec_dmeans.i2-k1.sst.filter20-100.nc'
y_name='temp_1'
x_name='lh'
type='hadgem3-kpp'

;olr_file='/home/ss901165/datasets/NOAA_CIRES_OLR/daily/NOAA_CIRES_OLR.jan-dec_dmeans.1998-2009.filter20-100.2.5x2.5.nc'
;sst_file='/home/ss901165/datasets/TMI_AMSRE/two_half/tmi_fusion.jan-dec_dmeans.1998-2009.filter20-100.2.5x2.5.nc'
;sst_file='/home/ss901165/datasets/ERA-INTERIM/U10/ERA_interim.jan-dec_dmeans.1998-2009.u10.tropics.2.5x2.5.filter20-100.nc'
;sst_name='U10'

; Define the region over which to latitude-average before taking the lead-lag correlation.
; The values are: [starting_latitude,starting_longitude,stopping_latitude,stopping_longitude]
; The program will average over all latitudes in the range, then plot the correlation at every longitude in the range.
box=[-10,60,10,200] ; Box in the Indian Ocean and Pacific Ocean

; Number of days to read per year
n_days_per_year=360
; Starting day to read
day_offset=0
; Number of years to read
n_years=20
; Starting year to read
year_offset=0

; Number of days affected by the filter in the first year of data analysed
filter_first=50
; Number of days affected by the filter in the last year of data analysed
filter_last=50

; Read longitude and latitude co-ordinate variables from the rainfall file.
x_longitude=OPEN_AND_EXTRACT(x_file,'longitude')
x_latitude=OPEN_AND_EXTRACT(x_file,'latitude')
; Restrict the dimensions to the box specified above.
DEFINE_BOUNDARIES,box,x_latitude,x_longitude,x_box_tx,/LIMIT
; Get the number of longitude and latitude points in the "box" region.
x_nlon=N_ELEMENTS(x_longitude)
x_nlat=N_ELEMENTS(x_latitude)

; Read longitude and latitude co-ordinate variables from the land/sea mask file
y_longitude=OPEN_AND_EXTRACT(y_file,'longitude')
y_latitude=OPEN_AND_EXTRACT(y_file,'latitude')
DEFINE_BOUNDARIES,box,y_latitude,y_longitude,y_box_tx,/LIMIT
y_nlon=N_ELEMENTS(y_longitude)
y_nlat=N_ELEMENTS(y_latitude)

; Read the daily X and Y values from the dataset.
daily_x=OPEN_AND_EXTRACT(x_file,x_name,$
                           offset=[x_box_tx(1),x_box_tx(0),day_offset,year_offset],$
                           count=[x_nlon,x_nlat,n_days_per_year,n_years])
daily_y=OPEN_AND_EXTRACT(y_file,y_name,$
                           offset=[y_box_tx(1),y_box_tx(0),day_offset,year_offset],$
                           count=[y_nlon,y_nlat,n_days_per_year,n_years])

; Create new variables to hold the box-average daily X and Y.
daily_x_avg=fltarr(x_nlon,n_days_per_year,n_years)
daily_y_avg=fltarr(y_nlon,n_days_per_year,n_years)

; Set the parts of the dataset affected by the bandpass filter to be missing values.
; In IDL, a missing value is specified by !Values.F_NaN.
IF filter_first gt 0 THEN BEGIN
   daily_x(*,*,0:filter_first-1,0)=!Values.F_NaN
   daily_y(*,*,0:filter_first-1,0)=!Values.F_NaN
ENDIF
IF filter_last gt 0 THEN BEGIN
   daily_x(*,*,n_days_per_year-filter_last:n_days_per_year-1,n_years-1)=!Values.F_NaN
   daily_y(*,*,n_days_per_year-filter_last:n_days_per_year-1,n_years-1)=!Values.F_NaN
ENDIF

; Take the latitude-average X and Y, after first removing the seasonal cycle (daily climatology).
FOR i=0,x_nlon-1 DO BEGIN
   FOR j=0,n_days_per_year-1 DO BEGIN
      FOR k=0,n_years-1 DO BEGIN
         daily_x_avg(i,j,k)=MEAN(daily_x(i,*,j,k),/NaN)-MEAN(daily_x(i,*,j,*),/NaN)
         daily_y_avg(i,j,k)=MEAN(daily_y(i,*,j,k),/NaN)-MEAN(daily_y(i,*,j,*),/NaN)
      ENDFOR
   ENDFOR
ENDFOR

; Compute the lead-lag correlation using the IDL C_CORRELATE routine
; Lags over which to compute the correlation.  Negative implies X leads Y, positive implies Y leads X.
lags=[-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10]
n_lags=N_ELEMENTS(lags)

cross_correlation=fltarr(x_nlon,n_lags)
; Loop over all longitudes.  At each longitude, take the lead-lag correlation for each year, then do a 
; weighted average using n_valid_days (see below).

FOR i=0,y_nlon-1 DO BEGIN
; n_valid_days stores the number of days that are *not* affected by the filter.  This variable is then
; used to make a weighted average of the correlations over all years, with the weight equal to the number
; of unaffected days.
   n_valid_days=fltarr(n_years)
   FOR j=0,n_years-1 DO BEGIN
                                ; If some data has been affected by the filter, make sure not to use that data
                                ; for the correlation analysis.
      IF j eq 0 and filter_first gt 0 THEN BEGIN      
         thisyear_correlation=C_CORRELATE(daily_x_avg(i,filter_first:n_days_per_year-1,j),$
                                          daily_y_avg(i,filter_first:n_days_per_year-1,j),lags)
         n_valid_days(j)=n_days_per_year-filter_first
      ENDIF ELSE IF j eq n_years-1 and filter_last gt 0 THEN BEGIN
         thisyear_correlation=C_CORRELATE(daily_x_avg(i,0:filter_last-1,j),$
                                          daily_y_avg(i,0:filter_last-1,j),lags)
         n_valid_days(j)=n_days_per_year-filter_last
      ENDIF ELSE BEGIN
         thisyear_correlation=C_CORRELATE(daily_x_avg(i,*,j),daily_y_avg(i,*,j),lags)
         n_valid_days(j)=n_days_per_year
      ENDELSE
      cross_correlation(i,*)=cross_correlation(i,*)+thisyear_correlation(*)*n_valid_days(j)
   ENDFOR
; Take the weighted average of the correlation coefficients using the number of days
; not affected by the bandpass filter.
   cross_correlation(i,*)=cross_correlation(i,*)/TOTAL(FLOAT(n_valid_days))
ENDFOR

; Plot the data.  See the IDL Guide for how these commands work.

; Give the location of the PostScript file that this program will make.
; This needs to be somewhere in your /home directory.

; Change this variable to point to somewhere in your Unix home space (e.g., /home/sv025016/idl ...)
psfile='/home/ss901165/idl/mjo_coupling/mjo_coupling_plot_phase_relationship_filter_latavg.'+type+'_'+x_name+'_'+y_name+'.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,$
       TCHARSIZE=100,SPACE3=500
; Set the boundaries of the axes.  The XMIN and XMAX are the minimum and maximum longitude specified in the 'box' variable above.
; The YMIN and YMAX are the minimum and maximum lag.
GSET,XMIN=box(1),XMAX=box(3),YMIN=MIN(lags),YMAX=MAX(lags),TITLE='Lead-lag correlation between 20-100 day bandpass-filtered '+x_name+' and '+y_name+' in from '+type
; Define contour levels here as a list (the levels are actually set below in the LEVS command)
contour_levels=['-0.55','-0.45','-0.35','-0.25','-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55']
; Choose a colour scale - load enough colours (NCOLS) for all of the contour levels
CS,SCALE=1,NCOLS=N_ELEMENTS(contour_levels)+1
; Set the contour levels
LEVS,MANUAL=contour_levels
; Plot the lagged correlation as a contour plot of longitude versus lag.
CON,X=y_longitude,Y=lags,FIELD=cross_correlation,/NOLINELABELS,CB_TITLE='Correlation coefficient (unitless)'
; Set the axes tick marks and labels
AXES,YVALS=lags,XSTEP=10,YTITLE='Lag (days, positive means '+x_name+' leads '+y_name+' )',XTITLE='Longitude (degrees east)',NDECS=2
; Put a dotted line at zero lag
GPLOT,X=[box(1),box(3)],Y=[0,0],STYLE=1
; Close and display
PSCLOSE

STOP
END
