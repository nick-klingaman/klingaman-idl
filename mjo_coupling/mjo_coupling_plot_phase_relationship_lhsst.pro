PRO mjo_coupling_plot_phase_relationship_lhsst

; All IDL programs begin with a "PRO [name]" statement.  The [name] should
; be the same as the name of this file, without the .pro extension.

; Plot the phase relationship (in time) between two variables.
; This example is for outgoing longwave radiation (OLR) and 
; sea-surface temperature (SST).

; Give the locations of the input files for OLR and SST.
sst_file='/home/ss901165/datasets/ERA-INTERIM/U10/ERA_interim.jan-dec_dmeans.1998-2009.u10.tropics.nc'
;olr_file='/home/ss901165/datasets/TMI_AMSRE/n96/tmi_fusion.jan-dec_dmeans.1998-2009.n96.nc'
;olr_file='/home/ss901165/um_output4/hadgem3a_amip2_iav_1.5xentrain_vn74/hadgem3a_amip2_iav_1.5xentrain_vn74.jan-dec_dmeans.years1-21.sh.nc'
;sst_file='/home/ss901165/um_output4/hadgem3a_amip2_iav_1.5xentrain_vn74/hadgem3a_amip2_iav_1.5xentrain_vn74.jan-dec_dmeans.years1-21.surf_temp.nc'

; Define the region over which to area-average the variables before taking the lead-lag correlation.  
; The values are: [starting_latitude,starting_longitude,stopping_latitude,stopping_longitude]
box=[-5,70,5,75] ; Box in the Indian Ocean.

; Number of days to read per year
n_days_per_year=365
;n_days_per_year=360
; Starting day to read
day_offset=0
; Number of years to read
n_years=12
;n_years=21
; Starting year to read
year_offset=0

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
daily_sst=OPEN_AND_EXTRACT(sst_file,'U10',$
                           offset=[sst_box_tx(1),sst_box_tx(0),day_offset,year_offset],$
                           count=[sst_nlon,sst_nlat,n_days_per_year,n_years])

; Take box averages of the OLR and SST.  Also remove the seasona cycle (daily climatology).
daily_olr_avg=fltarr(n_days_per_year,n_years)
daily_sst_avg=fltarr(n_days_per_year,n_years)
FOR i=0,n_days_per_year-1 DO BEGIN
   FOR j=0,n_years-1 DO BEGIN
      daily_olr_avg(i,j)=MEAN(ABS(daily_olr(*,*,i,j)))-MEAN(ABS(daily_olr(*,*,i,*)))
      daily_sst_avg(i,j)=MEAN(daily_sst(*,*,i,j))-MEAN(daily_sst(*,*,i,*))
   ENDFOR
ENDFOR

; Compute the lead-lag correlation using the IDL C_CORRELATE routine
; Lags over which to compute the correlation.  Negative implies OLR leads SST, positive implies SST leads OLR.
lags=[-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10]
n_lags=N_ELEMENTS(lags)

; Do the correlation for each year, then take the average over all years.
cross_correlation=fltarr(n_lags)
FOR i=0,n_years-1 DO BEGIN
   thisyear_correlation=C_CORRELATE(daily_sst_avg(*,i),daily_olr_avg(*,i),lags)
   print,thisyear_correlation(0)
   cross_correlation=cross_correlation+thisyear_correlation/FLOAT(n_years)
ENDFOR

; Plot the data.  See the IDL Guide for how these commands work.

; Give the location of the PostScript file that this program will make.
; This needs to be somewhere in your /home directory.
psfile='/home/ss901165/idl/mjo_coupling/mjo_coupling_plot_phase_relationship_lhsst.hg3a_slhf_sst_IndOcn.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,$
       TCHARSIZE=100,SPACE3=500
; Set the boundaries of the axes
GSET,XMIN=MIN(lags),XMAX=MAX(lags),YMIN=-0.5,YMAX=0.5,TITLE='Lead-lag correlation between SST and LH flux in the Indian Ocean from observations and ERA-Interim (1998-2009)'
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
