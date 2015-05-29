PRO mjo_enso_plot_20cr_boxavg_timeseries

input_directory='/export/mango/data-10/ss901165/20THC_REANALYSIS/every_member/u9950'
field_name='u9950'
netcdf_variable_name='u9950'
date_start=182  ; Julian date of the start of the period to plot
date_stop=212   ; Julian date of the end of the period to plot
n_days=date_stop-date_start+1

; Box over which to area-average: [start_lat,start_lon,stop_lat,stop_lon]
box=[0,160,10,180]

; Boundaries of bins to use for the probability distribution
bins=[-11,-9,-7,-5,-3,-1,1,3,5,7,9,11]
n_bins=N_ELEMENTS(bins)+1

; How many plots (different composites) do you want to make?
n_sets=1
FOR i=0,n_sets-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         ; Information for first composite
         years=[1918,1925,1930,1965,1994,1997,2002,2006]
         colors=['blue','red','orange','cyan','pink','brown','dodgerblue','violetred']
         remove_clim=1          ; Do you want to remove the daily climatology, to create anomalies for each year? (1=yes, 0=no)
         remove_years=0         ; Do you want to remove the mean of some other years?  (1=yes, 0=no; if yes, list years below)
         years_to_remove=[1905,1911,1913,1923,1940,1941,1951,1957,1963,1968,1969,1972,1976,1977,1982,1986,1987,1991,2004]
      END
      1 : BEGIN
         ; Information for second composite
         years=[1920,1923,1946,1955,1962,1974,1977,1982]
         colors=['blue','red','orange','cyan','pink','brown','dodgerblue','violetred']
         remove_clim=1          ; Do you want to remove the daily climatology, to create anomalies for each year? (1=yes, 0=no)
      END
   ENDCASE

   n_years=N_ELEMENTS(years)
   allyears_area_average=fltarr(n_years,n_days,56)
   allyears_ensmean_area_average=fltarr(n_years,n_days)
   IF remove_years eq 1 THEN $      
      clim_remove_area_average=fltarr(n_days,56)
   FOR j=0,n_years-1 DO BEGIN
      infile=input_directory+'/by_year/20thc_reanalysis_allmembers.jan-dec_dmeans.'+$
             STRTRIM(STRING(years(j)),1)+'.'+field_name+'.nc'
      IF j eq 0 THEN BEGIN
         ; Get grid information
         longitude=OPEN_AND_EXTRACT(infile,'lon')
         latitude=OPEN_AND_EXTRACT(infile,'lat')
         DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
         n_lon=N_ELEMENTS(longitude)
         n_lat=N_ELEMENTS(latitude)
      ENDIF
      thisyear_gridded=OPEN_AND_EXTRACT(infile,netcdf_variable_name,$
                                        offset=[box_tx(1),box_tx(0),0,date_start-1],$
                                        count=[n_lon,n_lat,56,n_days])/100.
      IF j eq 0 THEN BEGIN
         IF remove_clim eq 1 THEN BEGIN
            clim_infile=input_directory+'/20thc_reanalysis_allmembers.jan-dec_dmean_clim.1871-2008.'+field_name+'.nc'
            daily_clim=OPEN_AND_EXTRACT(clim_infile,netcdf_variable_name,$
                                        offset=[box_tx(1),box_tx(0),date_start-1,0],$
                                        count=[n_lon,n_lat,n_days,56])/100.
         ENDIF ELSE IF remove_years eq 1 THEN BEGIN
            IF j eq 0 THEN BEGIN
               FOR k=0,N_ELEMENTS(years_to_remove)-1 DO BEGIN
                  infile=input_directory+'/by_year/20thc_reanalysis_allmembers.jan-dec_dmeans.'+$
                         STRTRIM(STRING(years_to_remove(k)),1)+'.'+field_name+'.nc'
                  thisyear_gridded_remove=OPEN_AND_EXTRACT(infile,netcdf_variable_name,$
                                                           offset=[box_tx(1),box_tx(0),0,date_start-1],$
                                                           count=[n_lon,n_lat,56,n_days])/100.
                  print,MEAN(thisyear_gridded_remove)
                  FOR m=0,n_days-1 DO $
                     FOR n=0,55 DO $
                        clim_remove_area_average(m,n)=MEAN(thisyear_gridded_remove(*,*,m,k))+clim_remove_area_average(m,n)
               ENDFOR
               clim_remove_area_average=clim_remove_area_average/FLOAT(N_ELEMENTS(years_to_remove))
            ENDIF
         ENDIF
      ENDIF
      FOR k=0,n_days-1 DO BEGIN
         FOR m=0,55 DO BEGIN
            IF remove_clim eq 1 THEN BEGIN
               allyears_area_average(j,k,m)=MEAN(thisyear_gridded(*,*,m,k))-MEAN(daily_clim(*,*,k,m))
            ENDIF ELSE IF remove_years eq 1 THEN BEGIN
               allyears_area_average(j,k,m)=MEAN(thisyear_gridded(*,*,m,k))-clim_remove_area_average(k,m)
            ENDIF ELSE $
               allyears_area_average(j,k,m)=MEAN(thisyear_gridded(*,*,m,k))
            allyears_ensmean_area_average(j,k)=MEAN(allyears_area_average(j,k,*))
         ENDFOR
      ENDFOR
   ENDFOR
      
                                ; Create timeseries plot with box-and-whisker diagrams to show inter-quartile range
                                ; and total range of ensemble members
;   psfile='/home/mg001150/idl/mjo_enso_plot_20cr_boxavg_timeseries.'+netcdf_variable_name+$
;          '_composite'+STRTRIM(STRING(i+1),1)+'_dates'+STRTRIM(STRING(date_start),1)+'-'+$
;          STRTRIM(STRING(date_stop),1)+'.ps'
;   PSOPEN,file=psfile,CHARSIZE=120
;   GSET,XMIN=date_start,XMAX=date_stop+1,YMIN=-60,YMAX=10
;   FOR j=0,n_years-1 DO BEGIN
;      GPLOT,X=indgen(n_days)+date_start+0.5,Y=REFORM(allyears_ensmean_area_average(j,*)),$
;            THICK=150,COL=FSC_COLOR(colors(j))
;      FOR k=0,n_days-1 DO BEGIN
;         temp=REFORM(allyears_area_average(j,k,*))
;         sorted=SORT(temp)
;         EBAR,X=date_start+k+0.5,$
;              BOX=[temp(sorted(0)),temp(sorted(56/4)),temp(sorted(56/2)),temp(sorted(56*3/4)),temp(sorted(55))],$
;              COL=FSC_COLOR(colors(j))
;      ENDFOR
;   ENDFOR
;   AXES,XSTEP=3,YSTEP=3,YTITLE=netcdf_variable_name+' area averaged over lat '+STRTRIM(STRING(box(0)),1)+' to '+$
;        STRTRIM(STRING(box(2)),1)+' and lon '+STRTRIM(STRING(box(1)),1)+' to '+STRTRIM(STRING(box(3)),1),$
;        XTITLE='Julian date'
;   PSCLOSE

                                ; Create probability distribution using all days and all ensemble members.
;   psfile='/home/mg001150/idl/mjo_enso_plot_20cr_boxavg_timeseries.'+netcdf_variable_name+$
;          '_composite'+STRTRIM(STRING(i+1),1)+'_pdf_dates'+STRTRIM(STRING(date_start),1)+'-'+$
;          STRTRIM(STRING(date_stop),1)+'.ps'
;   PSOPEN,file=psfile,CHARSIZE=120
;   GSET,XMIN=0,XMAX=n_bins,YMIN=0,YMAX=1
   FOR j=0,n_years-1 DO BEGIN
      pdf=fltarr(n_bins)
      thisyear=REFORM(allyears_area_average(j,*,*))
      FOR k=0,n_bins-1 DO BEGIN
         IF k eq 0 THEN BEGIN
            IF TOTAL(where(thisyear le bins(k))) ge 0 THEN BEGIN
               pdf(k)=N_ELEMENTS(where(thisyear lt bins(k)))
            ENDIF ELSE $
               pdf(k)=0.
         ENDIF ELSE IF k eq n_bins-1 THEN BEGIN
            IF TOTAL(where(thisyear gt bins(k-1))) ge 0 THEN BEGIN
               pdf(k)=N_ELEMENTS(where(thisyear gt bins(k-1)))
            ENDIF ELSE $
               pdf(k)=0.
         ENDIF ELSE BEGIN
            IF TOTAL(where(thisyear gt bins(k-1) and thisyear le bins(k))) THEN BEGIN
               pdf(k)=N_ELEMENTS(where(thisyear gt bins(k-1) and thisyear lt bins(k)))
            ENDIF ELSE $
               pdf(k)=0.
         ENDELSE
      ENDFOR
      print,pdf
      pdf=pdf/FLOAT(56*n_days)
      print,'Skewness of year '+STRTRIM(STRING(years(j)),1)+' is: '+STRTRIM(STRING(SKEWNESS(thisyear)),1)
      GPLOT,X=indgen(n_bins)+0.5,Y=pdf,COL=FSC_COLOR(colors(j))
   ENDFOR
;   AXES,XVALS=indgen(n_bins+1),XLABELS=['< '+STRTRIM(STRING(bins(0)),1),STRTRIM(STRING(bins),1),$
;                                        '> '+STRTRIM(STRING(bins(n_bins-2)),1)],$
;        YSTEP=0.05,YTITLE='Probability',XTITLE='Area-averaged value of '+netcdf_variable_name,NDECS=2
;   PSCLOSE
ENDFOR

STOP
END

