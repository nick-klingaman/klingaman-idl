PRO mjo_coupling_plot_phase_relationship_comparehg3

; All IDL programs begin with a "PRO [name]" statement.  The [name] should
; be the same as the name of this file, without the .pro extension.

; Plot the phase relationship (in time) between two variables.
; This example is for outgoing longwave radiation (OLR) and 
; sea-surface temperature (SST).

; Give the locations of the input files for OLR and SST.
obs_olr_file='/home/ss901165/datasets/NOAA_CIRES_OLR/daily/NOAA_CIRES_OLR.jan-dec_dmeans.1998-2009.2.5x2.5.nc'
obs_sst_file='/home/ss901165/datasets/TMI_AMSRE/n96/tmi_fusion.jan-dec_dmeans.1998-2009.n96.nc'

hg3_olr_file='/home/ss901165/um_output4/hadgem3a_amip2_iav_1.5xentrain_vn74/hadgem3a_amip2_iav_1.5xentrain_vn74.jan-dec_dmeans.years1-21.olr.nc'
hg3_sst_file='/home/ss901165/um_output4/hadgem3a_amip2_iav_1.5xentrain_vn74/hadgem3a_amip2_iav_1.5xentrain_vn74.jan-dec_dmeans.years1-21.surf_temp.nc'

; Define the region over which to area-average the variables before taking the lead-lag correlation.  
; The values are: [starting_latitude,starting_longitude,stopping_latitude,stopping_longitude]
box=[-5,70,5,90] ; Box in the Indian Ocean.

; Number of days to read per year
obs_ndays_per_year=365
hg3_ndays_per_year=360
; Starting day to read
obs_day_offset=0
hg3_day_offset=0
; Number of years to read
obs_nyears=12
hg3_nyears=21
; Starting year to read
obs_year_offset=0
hg3_year_offset=0

; Read longitude and latitude co-ordinate variables from the rainfall file.
obs_olr_longitude=OPEN_AND_EXTRACT(obs_olr_file,'longitude')
obs_olr_latitude=OPEN_AND_EXTRACT(obs_olr_file,'latitude')
; Restrict the dimensions to the box specified above.
DEFINE_BOUNDARIES,box,obs_olr_latitude,obs_olr_longitude,obs_olr_box_tx,/LIMIT
; Get the number of longitude and latitude points in the "box" region.
obs_olr_nlon=N_ELEMENTS(obs_olr_longitude)
obs_olr_nlat=N_ELEMENTS(obs_olr_latitude)

; Read longitude and latitude co-ordinate variables from the land/sea mask file
obs_sst_longitude=OPEN_AND_EXTRACT(obs_sst_file,'longitude')
obs_sst_latitude=OPEN_AND_EXTRACT(obs_sst_file,'latitude')
DEFINE_BOUNDARIES,box,obs_sst_latitude,obs_sst_longitude,obs_sst_box_tx,/LIMIT
obs_sst_nlon=N_ELEMENTS(obs_sst_longitude)
obs_sst_nlat=N_ELEMENTS(obs_sst_latitude)

; Read longitude and latitude co-ordinate variables from the rainfall file.
hg3_olr_longitude=OPEN_AND_EXTRACT(hg3_olr_file,'longitude')
hg3_olr_latitude=OPEN_AND_EXTRACT(hg3_olr_file,'latitude')
; Restrict the dimensions to the box specified above.
DEFINE_BOUNDARIES,box,hg3_olr_latitude,hg3_olr_longitude,hg3_olr_box_tx,/LIMIT
; Get the number of longitude and latitude points in the "box" region.
hg3_olr_nlon=N_ELEMENTS(hg3_olr_longitude)
hg3_olr_nlat=N_ELEMENTS(hg3_olr_latitude)

; Read longitude and latitude co-ordinate variables from the land/sea mask file
hg3_sst_longitude=OPEN_AND_EXTRACT(hg3_sst_file,'longitude')
hg3_sst_latitude=OPEN_AND_EXTRACT(hg3_sst_file,'latitude')
DEFINE_BOUNDARIES,box,hg3_sst_latitude,hg3_sst_longitude,hg3_sst_box_tx,/LIMIT
hg3_sst_nlon=N_ELEMENTS(hg3_sst_longitude)
hg3_sst_nlat=N_ELEMENTS(hg3_sst_latitude)

; Read the daily OLR and SST values from the dataset.
daily_obs_olr=OPEN_AND_EXTRACT(obs_olr_file,'olr',$
                               offset=[obs_olr_box_tx(1),obs_olr_box_tx(0),obs_day_offset,obs_year_offset],$
                               count=[obs_olr_nlon,obs_olr_nlat,obs_ndays_per_year,obs_nyears])
daily_obs_sst=OPEN_AND_EXTRACT(obs_sst_file,'sst',$
                               offset=[obs_sst_box_tx(1),obs_sst_box_tx(0),obs_day_offset,obs_year_offset],$
                               count=[obs_sst_nlon,obs_sst_nlat,obs_ndays_per_year,obs_nyears])

daily_hg3_olr=OPEN_AND_EXTRACT(hg3_olr_file,'olr',$
                               offset=[hg3_olr_box_tx(1),hg3_olr_box_tx(0),hg3_day_offset,hg3_year_offset],$
                               count=[hg3_olr_nlon,hg3_olr_nlat,hg3_ndays_per_year,hg3_nyears])
daily_hg3_sst=OPEN_AND_EXTRACT(hg3_sst_file,'temp',$
                               offset=[hg3_sst_box_tx(1),hg3_sst_box_tx(0),hg3_day_offset,hg3_year_offset],$
                               count=[hg3_sst_nlon,hg3_sst_nlat,hg3_ndays_per_year,hg3_nyears])

; Take box averages of the OLR and SST.  Also remove the seasonal cycle (daily climatology).
daily_obs_olr_avg=fltarr(obs_ndays_per_year,obs_nyears)
daily_obs_sst_avg=fltarr(obs_ndays_per_year,obs_nyears)
FOR i=0,obs_ndays_per_year-1 DO BEGIN
   FOR j=0,obs_nyears-1 DO BEGIN
      daily_obs_olr_avg(i,j)=MEAN(daily_obs_olr(*,*,i,j))-MEAN(daily_obs_olr(*,*,i,*))
      daily_obs_sst_avg(i,j)=MEAN(daily_obs_sst(*,*,i,j))-MEAN(daily_obs_sst(*,*,i,*))
   ENDFOR
ENDFOR
FOR i=0,hg3_olr_nlon-1 DO BEGIN
   FOR j=0,hg3_olr_nlat-1 DO BEGIN
      FOR k=0,hg3_ndays_per_year-1 DO BEGIN
         daily_hg3_olr(i,j,k,*)=daily_hg3_olr(i,j,k,*)-MEAN(daily_hg3_olr(i,j,k,*))
         daily_hg3_sst(i,j,k,*)=daily_hg3_sst(i,j,k,*)-MEAN(daily_hg3_sst(i,j,k,*))
      ENDFOR
   ENDFOR
ENDFOR
daily_hg3_olr_avg=fltarr(hg3_ndays_per_year,hg3_nyears)
daily_hg3_sst_avg=fltarr(hg3_ndays_per_year,hg3_nyears)
FOR i=0,hg3_ndays_per_year-1 DO BEGIN
   FOR j=0,hg3_nyears-1 DO BEGIN
      daily_hg3_olr_avg(i,j)=MEAN(daily_hg3_olr(*,*,i,j));-MEAN(daily_hg3_olr(*,*,i,*))
      daily_hg3_sst_avg(i,j)=MEAN(daily_hg3_sst(*,*,i,j));-MEAN(daily_hg3_sst(*,*,i,*))
   ENDFOR
ENDFOR

; Compute the lead-lag correlation using the IDL C_CORRELATE routine
; Lags over which to compute the correlation.  Negative implies OLR leads SST, positive implies SST leads OLR.
lags=[-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12]
n_lags=N_ELEMENTS(lags)

; Do the correlation for each year, then take the average over all years.
obs_cross_correlation=fltarr(n_lags)
hg3_cross_correlation=fltarr(n_lags)
FOR i=0,obs_nyears-1 DO BEGIN
   thisyear_correlation=C_CORRELATE(daily_obs_sst_avg(*,i),daily_obs_olr_avg(*,i),lags)
   print,thisyear_correlation(0)
   obs_cross_correlation=obs_cross_correlation+thisyear_correlation/FLOAT(obs_nyears)   
ENDFOR
FOR i=0,hg3_nyears-1 DO BEGIN
   thisyear_correlation=C_CORRELATE(daily_hg3_sst_avg(*,i),daily_hg3_olr_avg(*,i),lags)
   print,thisyear_correlation(0)
   hg3_cross_correlation=hg3_cross_correlation+thisyear_correlation/FLOAT(hg3_nyears)
ENDFOR

; Plot the data.  See the IDL Guide for how these commands work.

; Give the location of the PostScript file that this program will make.
; This needs to be somewhere in your /home directory.
psfile='/home/ss901165/idl/mjo_coupling/mjo_coupling_plot_phase_relationship_comparehg3.olr_sst_IndOcn.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,$
       TCHARSIZE=100,SPACE3=500
; Set the boundaries of the axes
GSET,XMIN=MIN(lags),XMAX=MAX(lags),YMIN=-0.5,YMAX=0.5,TITLE='Lead-lag correlation between SST and OLR in the Indian Ocean from obs (1998-2009) and HG3-A (21 yrs)'
; Plot the lagged correlation
GPLOT,X=lags,Y=obs_cross_correlation,COL=FSC_COLOR('red')
GPLOT,X=lags,Y=hg3_cross_correlation,COL=FSC_COLOR('blue')
; Set the axes tick marks and labels
AXES,XVALS=lags,YSTEP=0.05,YTITLE='Correlation coefficient',XTITLE='Lag (days, positive means SST leads OLR)',NDECS=2
; Put dotted lines at zero lag and zero correlation
GPLOT,X=[0,0],Y=[-0.5,0.5],STYLE=1
GPLOT,X=[MIN(lags),MAX(lags)],Y=[0,0],STYLE=1
; Close and display
PSCLOSE

STOP
END
