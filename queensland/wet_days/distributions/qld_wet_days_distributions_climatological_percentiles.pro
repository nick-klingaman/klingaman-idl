PRO qld_wet_days_distributions_climatological_percentiles

; File containing timeseries of Nov-Apr mean rainfall
silo_seasmean_infile='/home/ss901165/datasets_mango/SILO/one_quarter/SILO_precip.nov-apr_smeans.1900-2008.0.25x0.25.nc'

; File containing daily timeseries of Nov-Apr rainfall
silo_dailymean_infile='/home/ss901165/datasets_mango/SILO/one_quarter/SILO_precip.nov-apr_dmeans.1900-2008.0.25x0.25.nc'

silo_nyears=107
silo_ndays_per_year=181

; Box to consider
;box=[-10,140,-30,154]
box=[-10,140,-15,145]

; Number of equally spaced intervals into which to separate climatological
; rainfall (for calculation of thresholds, e.g., a value of 10 gives
; the 10th, 20th, 30th, etc. percentiles)
n_intervals=10
interval_offset=0.90

; Read latitude and longitude, find box of interest
silo_longitude=OPEN_AND_EXTRACT(silo_seasmean_infile,'longitude')
silo_latitude=OPEN_AND_EXTRACT(silo_seasmean_infile,'latitude')
DEFINE_BOUNDARIES,box,silo_latitude,silo_longitude,silo_box_tx,/LIMIT
silo_nlon=N_ELEMENTS(silo_longitude)
silo_nlat=N_ELEMENTS(silo_latitude)

;latpt=NEAREST(silo_latitude,point(0))
;lonpt=NEAREST(silo_longitude,point(1))

silo_daily_upper_freq=fltarr(silo_nlon,silo_nlat,n_intervals)
silo_daily_upper_intensity=fltarr(silo_nlon,silo_nlat,n_intervals)
silo_daily_middle_freq=fltarr(silo_nlon,silo_nlat,n_intervals)
silo_daily_middle_intensity=fltarr(silo_nlon,silo_nlat,n_intervals)
silo_daily_lower_freq=fltarr(silo_nlon,silo_nlat,n_intervals)
silo_daily_lower_intensity=fltarr(silo_nlon,silo_nlat,n_intervals)
silo_daily_all_freq=fltarr(silo_nlon,silo_nlat,n_intervals)
silo_daily_all_intensity=fltarr(silo_nlon,silo_nlat,n_intervals)
silo_daily_scaled_upper_freq=fltarr(silo_nlon,silo_nlat,n_intervals)
FOR j=0,silo_nlon-1 DO BEGIN
   print,'Now running for j=',j,' of ',silo_nlon
   FOR k=0,silo_nlat-1 DO BEGIN
                                ; Read seasonal-mean rainfall, separate into terciles
      silo_seasmeans=REFORM(OPEN_AND_EXTRACT(silo_seasmean_infile,'rain',$
                                             offset=[silo_box_tx(1)+j,silo_box_tx(0)+k,0],count=[1,1,silo_nyears]))
      IF silo_seasmeans(0) lt 1E20 THEN BEGIN
         sorted_indices=SORT(silo_seasmeans)
         silo_seasmeans_sorted=silo_seasmeans[sorted_indices]
         upper_tercile_indices=sorted_indices(silo_nyears*2/3+1:silo_nyears-1)
         n_upper=N_ELEMENTS(upper_tercile_indices)
         lower_tercile_indices=sorted_indices(0:silo_nyears/3-1)
         n_lower=N_ELEMENTS(lower_tercile_indices)
         middle_tercile_indices=sorted_indices(silo_nyears/3:silo_nyears*2/3)
         n_middle=N_ELEMENTS(middle_tercile_indices)

                                ; Create arrays for daily rainfall at that point in upper, middle,
                                ; lower terciles
         silo_daily_upper=fltarr(n_upper*silo_ndays_per_year)
         silo_daily_middle=fltarr(n_middle*silo_ndays_per_year)
         silo_daily_lower=fltarr(n_lower*silo_ndays_per_year)
         
                                ; Read daily mean rainfall for each year in each tercile, organize
                                ; into a single array for each tercile
         
         FOR i=0,n_lower-1 DO BEGIN
            silo_daily_lower(i*silo_ndays_per_year:(i+1)*silo_ndays_per_year-1)=$
               REFORM(OPEN_AND_EXTRACT(silo_dailymean_infile,'rain',$
                                       offset=[silo_box_tx(1)+j,silo_box_tx(0)+k,0,lower_tercile_indices(i)],$
                                       count=[1,1,silo_ndays_per_year,1]))
         ENDFOR
         FOR i=0,n_middle-1 DO BEGIN
            silo_daily_middle(i*silo_ndays_per_year:(i+1)*silo_ndays_per_year-1)=$
               REFORM(OPEN_AND_EXTRACT(silo_dailymean_infile,'rain',$
                                       offset=[silo_box_tx(1)+j,silo_box_tx(0)+k,0,middle_tercile_indices(i)],$
                                       count=[1,1,silo_ndays_per_year,1]))
         ENDFOR
         FOR i=0,n_upper-1 DO BEGIN
            silo_daily_upper(i*silo_ndays_per_year:(i+1)*silo_ndays_per_year-1)=$
               REFORM(OPEN_AND_EXTRACT(silo_dailymean_infile,'rain',$
                                       offset=[silo_box_tx(1)+j,silo_box_tx(0)+k,0,upper_tercile_indices(i)],$
                                       count=[1,1,silo_ndays_per_year,1]))
         ENDFOR
         
         silo_daily_all=[silo_daily_lower,silo_daily_middle,silo_daily_upper]
         
                                ; Considering all days, limit to days > 1 mm/day and sort based on
                                ; daily rainfall amount
         silo_daily_all_sorted=silo_daily_all[where(silo_daily_all gt 1)]
         silo_daily_all_sorted=silo_daily_all_sorted[SORT(silo_daily_all_sorted)]
         n_silo_daily_all_sorted=N_ELEMENTS(silo_daily_all_sorted)
         
                                ; Find thresholds of interest
         silo_clim_percentiles=fltarr(n_intervals+1)
;         silo_clim_percentiles(0)=1.000
         FOR i=0,n_intervals-1 DO BEGIN
            this_interval=(1.-interval_offset)/FLOAT(n_intervals)*(i)+interval_offset
            this_index=FLOOR(n_silo_daily_all_sorted*FLOAT(this_interval))-1
            silo_clim_percentiles(i)=silo_daily_all_sorted(this_index)
         ENDFOR
                                ; Compute frequency of occurrence for each interval      
         n_silo_daily_upper=N_ELEMENTS(where(silo_daily_upper ge silo_clim_percentiles(0)))
         n_silo_daily_middle=N_ELEMENTS(where(silo_daily_middle ge silo_clim_percentiles(0)))
         n_silo_daily_lower=N_ELEMENTS(where(silo_daily_lower ge silo_clim_percentiles(0)))

         silo_daily_scaled_upper=silo_daily_lower[where(silo_daily_lower ge silo_clim_percentiles(0))]*$
                                 MEAN(silo_daily_upper[where(silo_daily_upper ge silo_clim_percentiles(0))])/$
                                 MEAN(silo_daily_lower[where(silo_daily_lower ge silo_clim_percentiles(0))])

         FOR i=0,n_intervals-1 DO BEGIN
            this_low_bound=silo_clim_percentiles(i)
            this_high_bound=silo_clim_percentiles(i+1)
            IF i ne n_intervals-1 THEN BEGIN
               silo_daily_upper_freq(j,k,i)=N_ELEMENTS(where(silo_daily_upper ge this_low_bound and silo_daily_upper lt this_high_bound))
               silo_daily_upper_intensity(j,k,i)=MEAN(silo_daily_upper[where(silo_daily_upper ge this_low_bound and silo_daily_upper lt this_high_bound)])

               silo_daily_middle_freq(j,k,i)=N_ELEMENTS(where(silo_daily_middle ge this_low_bound and silo_daily_middle lt this_high_bound))
               silo_daily_middle_intensity(j,k,i)=MEAN(silo_daily_middle[where(silo_daily_middle ge this_low_bound and silo_daily_middle lt this_high_bound)])

               silo_daily_lower_freq(j,k,i)=N_ELEMENTS(where(silo_daily_lower ge this_low_bound and silo_daily_lower lt this_high_bound))
               silo_daily_lower_intensity(j,k,i)=MEAN(silo_daily_lower[where(silo_daily_lower ge this_low_bound and silo_daily_lower lt this_high_bound)])

               silo_daily_all_freq(j,k,i)=N_ELEMENTS(where(silo_daily_all_sorted ge this_low_bound and silo_daily_all_sorted lt this_high_bound))
               silo_daily_all_intensity(j,k,i)=MEAN(silo_daily_all[where(silo_daily_all_sorted ge this_low_bound and silo_daily_all_sorted lt this_high_bound)])

               silo_daily_scaled_upper_freq(j,k,i)=N_ELEMENTS(where(silo_daily_scaled_upper ge this_low_bound and silo_daily_scaled_upper lt this_high_bound))
            ENDIF ELSE BEGIN
               IF TOTAL(where(silo_daily_upper ge this_low_bound and silo_daily_upper le this_high_bound)) gt -1 THEN BEGIN
                  silo_daily_upper_freq(j,k,i)=N_ELEMENTS(where(silo_daily_upper ge this_low_bound and silo_daily_upper le this_high_bound))
                  silo_daily_upper_intensity(j,k,i)=MEAN(silo_daily_upper[where(silo_daily_upper ge this_low_bound and silo_daily_upper le this_high_bound)])
               ENDIF ELSE BEGIN
                  silo_daily_upper_freq(j,k,i)=0
                  silo_daily_upper_intensity(j,k,i)=!Values.F_NaN
               ENDELSE

               silo_daily_middle_freq(j,k,i)=N_ELEMENTS(where(silo_daily_middle ge this_low_bound and silo_daily_middle le this_high_bound))
               silo_daily_middle_intensity(j,k,i)=MEAN(silo_daily_middle[where(silo_daily_middle ge this_low_bound and silo_daily_middle le this_high_bound)])

               IF TOTAL(where(silo_daily_lower ge this_low_bound and silo_daily_lower le this_high_bound)) gt -1 THEN BEGIN
                  silo_daily_lower_freq(j,k,i)=N_ELEMENTS(where(silo_daily_lower ge this_low_bound and silo_daily_lower le this_high_bound))
                  silo_daily_lower_intensity(j,k,i)=MEAN(silo_daily_lower[where(silo_daily_lower ge this_low_bound and silo_daily_lower le this_high_bound)])
               ENDIF ELSE BEGIN
                  silo_daily_lower_freq(j,k,i)=0
                  silo_daily_lower_intensity(j,k,i)=!Values.F_NaN
               ENDELSE

               silo_daily_all_freq(j,k,i)=N_ELEMENTS(where(silo_daily_all_sorted ge this_low_bound and silo_daily_all_sorted le this_high_bound))
               silo_daily_all_intensity(j,k,i)=MEAN(silo_daily_all[where(silo_daily_all_sorted ge this_low_bound and silo_daily_all_sorted le this_high_bound)])

               silo_daily_scaled_upper_freq(j,k,i)=N_ELEMENTS(where(silo_daily_scaled_upper ge this_low_bound and silo_daily_scaled_upper le this_high_bound))
            ENDELSE
         ENDFOR      
         silo_daily_upper_freq(j,k,*)=silo_daily_upper_freq(j,k,*)/FLOAT(n_silo_daily_upper)
         silo_daily_middle_freq(j,k,*)=silo_daily_middle_freq(j,k,*)/FLOAT(n_silo_daily_middle)
         silo_daily_lower_freq(j,k,*)=silo_daily_lower_freq(j,k,*)/FLOAT(n_silo_daily_lower)
         silo_daily_all_freq(j,k,*)=silo_daily_all_freq(j,k,*)/FLOAT(n_silo_daily_all_sorted)
         silo_daily_scaled_upper_freq(j,k,*)=silo_daily_scaled_upper_freq(j,k,*)/FLOAT(n_silo_daily_lower)
      ENDIF ELSE BEGIN
         silo_daily_upper_freq(j,k,*)=!Values.F_NaN
         silo_daily_upper_intensity(j,k,*)=!Values.F_NaN        
         silo_daily_middle_freq(j,k,*)=!Values.F_NaN
         silo_daily_middle_intensity(j,k,*)=!Values.F_NaN
         silo_daily_lower_freq(j,k,*)=!Values.F_NaN
         silo_daily_lower_intensity(j,k,*)=!Values.F_NaN
         silo_daily_all_freq(j,k,*)=!Values.F_NaN
         silo_daily_all_intensity(j,k,*)=!Values.F_NaN
         silo_daily_scaled_upper_freq(j,k,*)=!Values.F_NaN
      ENDELSE
   ENDFOR
ENDFOR

silo_daily_upper_freq_mean=fltarr(n_intervals)
silo_daily_upper_intensity_mean=fltarr(n_intervals)
silo_daily_middle_freq_mean=fltarr(n_intervals)
silo_daily_middle_intensity_mean=fltarr(n_intervals)
silo_daily_lower_freq_mean=fltarr(n_intervals)
silo_daily_lower_intensity_mean=fltarr(n_intervals)
silo_daily_all_intensity_mean=fltarr(n_intervals)
silo_daily_scaled_upper_freq_mean=fltarr(n_intervals)
FOR i=0,n_intervals-1 DO BEGIN
   silo_daily_upper_freq_mean(i)=MEAN(silo_daily_upper_freq(*,*,i),/NaN)
   silo_daily_upper_intensity_mean(i)=MEAN(silo_daily_upper_intensity(*,*,i),/NaN)
   silo_daily_middle_freq_mean(i)=MEAN(silo_daily_middle_freq(*,*,i),/NaN)
   silo_daily_middle_intensity_mean(i)=MEAN(silo_daily_middle_intensity(*,*,i),/NaN)
   silo_daily_lower_freq_mean(i)=MEAN(silo_daily_lower_freq(*,*,i),/NaN)
   silo_daily_lower_intensity_mean(i)=MEAN(silo_daily_lower_intensity(*,*,i),/NaN)
   silo_daily_all_intensity_mean(i)=MEAN(silo_daily_all_intensity(*,*,i),/NaN)
   silo_daily_scaled_upper_freq_mean(i)=MEAN(silo_daily_scaled_upper_freq(*,*,i),/NaN)
ENDFOR

psfile='/home/ss901165/idl/queensland/wet_days/distributions/qld_wet_days_distributions_climatological_percentiles.start_'+STRTRIM(STRING(interval_offset),1)+'.box'+$
       STRTRIM(STRING(ABS(box(0))),1)+'-'+STRTRIM(STRING(ABS(box(2))),1)+'S_'+STRTRIM(STRING(ABS(box(1))),1)+'-'+STRTRIM(STRING(ABS(box(3))),1)+'E.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=2000,SPACE2=300,XOFFSET=700,YOFFSET=500,TFONT=2,TCHARSIZE=100
GSET,XMIN=interval_offset,XMAX=100,YMIN=0.00,YMAX=0.16,TITLE="Freq of daily rain within clim %iles (all QLD land points, Nov-Apr 1900-2008, SILO 0.25)"
AXES,XSTEP=(100-interval_offset)/10,YSTEP=0.02,YTITLE="Fractional frequency (no units)",XTITLE="Climatological percentile of daily rain amounts (no units)",NDECS=2
blue=FSC_COLOR("blue",12)
red=FSC_COLOR("red",10)
black=FSC_COLOR("black",11)
purple=FSC_COLOR("purple",13)
cyan=FSC_COLOR("cyan",14)

HIST,X=indgen(n_intervals)*100/FLOAT(n_intervals)+100/FLOAT(2*n_intervals)-2.25,Y=silo_daily_lower_freq_mean,FILLCOL=10,width=70
HIST,X=indgen(n_intervals)*100/FLOAT(n_intervals)+100/FLOAT(2*n_intervals)-0.75,Y=silo_daily_middle_freq_mean,FILLCOL=11,width=70
HIST,X=indgen(n_intervals)*100/FLOAT(n_intervals)+100/FLOAT(2*n_intervals)+0.75,Y=silo_daily_upper_freq_mean,FILLCOL=12,width=70
HIST,X=indgen(n_intervals)*100/FLOAT(n_intervals)+100/FLOAT(2*n_intervals)+2.25,Y=silo_daily_scaled_upper_freq_mean,FILLCOL=14,width=70
;HIST,X=indgen(n_intervals)*100/FLOAT(n_intervals)+100/FLOAT(2*n_intervals)+1.5,Y=silo_daily_all_freq,FILLCOL=,width=70
GPLOT,X=indgen(101),Y=REPLICATE(0.10,101),COL=13

items=["Climatological frequency","Lower tercile scaled to mean intensity of upper tercile","All seasons in lower tercile (by seasonal total)","All seasons in middle tercile (by seasonal total)","All seasons in upper tercile (by seasonal total)"]
colors=[13,14,10,11,12]
sym=[1,1,1,1,1]
size=[0,100,100,100,100]
LEGEND,labels=items,col=colors,sym=sym,size=size,length=0,LEGPOS=5

PSCLOSE

STOP

END

