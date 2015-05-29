PRO qld_wet_days_correlations_threshold_annrain

; Correlate the number of wet days with annual rainfall

silo_wetdays_infile='/home/ss901165/datasets_mango/SILO/one_quarter/SILO_precip.wet_days_threshold.1900-2008.0.25x0.25.nc'
silo_annrain_infile='/home/ss901165/datasets_mango/SILO/one_quarter/SILO_annual_precip.jan-dec_amean.1900-2008.0.25x0.25.nc'

start_year=1900
stop_year=2008
n_years=(stop_year-start_year)+1

silo_offset_year=start_year-1900 ; SILO dataset starts in 1900

; Get longitude and latitude from wet days file
wetdays_longitude=OPEN_AND_EXTRACT(silo_wetdays_infile,'longitude')
wetdays_latitude=OPEN_AND_EXTRACT(silo_wetdays_infile,'latitude')
wetdays_nlon=N_ELEMENTS(wetdays_longitude)
wetdays_nlat=N_ELEMENTS(wetdays_latitude)

box=[wetdays_latitude(0),wetdays_longitude(0),wetdays_latitude(wetdays_nlat-1),$
     wetdays_longitude(wetdays_nlon-1)]

; Get longitude and latitude from rainfall file, fit to the
; coorindates from the wet days file
annrain_longitude=OPEN_AND_EXTRACT(silo_annrain_infile,'longitude')
annrain_latitude=OPEN_AND_EXTRACT(silo_annrain_infile,'latitude')
DEFINE_BOUNDARIES,box,annrain_latitude,annrain_longitude,box_tx,/LIMIT

; Get number of thresholds and their values
wetdays_thresholds=OPEN_AND_EXTRACT(silo_wetdays_infile,'threshold')
wetdays_nthresholds=N_ELEMENTS(wetdays_thresholds)

; Read annual rainfall
silo_annrain=REFORM(OPEN_AND_EXTRACT(silo_annrain_infile,'rain',$
                                     offset=[box_tx(1),box_tx(0),0],$
                                     count=[wetdays_nlon,wetdays_nlat,n_years]))

correlation_wetdays_annrain=fltarr(wetdays_nlon,wetdays_nlat)
correlation_meanrain_annrain=fltarr(wetdays_nlon,wetdays_nlat)
correlation_fracrain_annrain=fltarr(wetdays_nlon,wetdays_nlat)
mylevs_correlation=['0.20','0.25','0.30',$
                    '0.35','0.40','0.45','0.50','0.55','0.60','0.65','0.70','0.75','0.80','0.85',$
                    '0.90','0.95']
FOR i=0,wetdays_nthresholds-1 DO BEGIN
   
   this_threshold=FLOOR(wetdays_thresholds(i))
   this_threshold_str=STRTRIM(STRING(this_threshold),1)
                                ; Read number of wet days for this
                                ; threshold.
   silo_wetdays=REFORM(OPEN_AND_EXTRACT(silo_wetdays_infile,'ndays_over',$
                                        offset=[0,0,0,i],$
                                        count=[wetdays_nlon,wetdays_nlat,n_years,1]))
   silo_meanrain=REFORM(OPEN_AND_EXTRACT(silo_wetdays_infile,'mean_over',$
                                         offset=[0,0,0,i],$
                                         count=[wetdays_nlon,wetdays_nlat,n_years,1]))
   silo_fracrain=REFORM(OPEN_AND_EXTRACT(silo_wetdays_infile,'frac_of_annual',$
                                         offset=[0,0,0,i],$
                                         count=[wetdays_nlon,wetdays_nlat,n_years,1]))

   FOR j=0,wetdays_nlon-1 DO BEGIN
      FOR k=0,wetdays_nlat-1 DO BEGIN
         temp_wetdays=silo_wetdays(j,k,*)
         IF TOTAL(temp_wetdays) gt 0 THEN BEGIN
            good=where(temp_wetdays ne 0)
            temp_wetdays=temp_wetdays[good]
            temp_meanrain=silo_meanrain(j,k,*)
            temp_meanrain=temp_meanrain[good]
            temp_fracrain=silo_fracrain(j,k,*)
            temp_fracrain=temp_fracrain[good]
            temp_annrain=silo_annrain(j,k,*)
            temp_annrain=temp_annrain[good]
            correlation_wetdays_annrain(j,k)=CORRELATE(temp_wetdays,temp_annrain)
            correlation_meanrain_annrain(j,k)=CORRELATE(temp_meanrain,temp_annrain)
            correlation_fracrain_annrain(j,k)=CORRELATE(temp_fracrain,temp_annrain)
         ENDIF ELSE BEGIN
;         IF TOTAL(where(FINITE(silo_meanrain(j,k,*)) eq 0)) ne -1 THEN BEGIN
;            temp=silo_meanrain(j,k,*)            
;            temp[where(FINITE(temp) eq 0)]=0.
;            silo_meanrain(j,k,*)=temp
;         ENDIF
;         IF MEAN(silo_wetdays(j,k,*)) le 1 THEN BEGIN
            correlation_wetdays_annrain(j,k)=!Values.F_NaN
            correlation_meanrain_annrain(j,k)=!Values.F_NaN
            correlation_fracrain_annrain(j,k)=!Values.F_NaN
         ENDELSE
;         ENDIF ELSE BEGIN
;            correlation_wetdays_annrain(j,k)=CORRELATE($
;                                             silo_wetdays(j,k,*),silo_annrain(j,k,*))
            

;            correlation_meanrain_annrain(j,k)=CORRELATE($
;                                              silo_meanrain(j,k,*),silo_annrain(j,k,*))
            
            
;            correlation_fracrain_annrain(j,k)=CORRELATE($
;                                              silo_fracrain(j,k,*),silo_annrain(j,k,*))            
;         ENDELSE
;         IF correlation_meanrain_annrain(j,k) eq 0 THEN $
;            correlation_meanrain_annrain(j,k)=!Values.F_NaN
                                ;IF TOTAL(where(silo_wetdays(j,k,*) eq 0)) ne -1 THEN $
                                ;   correlation_meanrain_annrain(j,k)=!Values.F_NaN
      ENDFOR
   ENDFOR
        
   psfile='/home/ss901165/idl/queensland/wet_days/correlations/'+$
          'qld_wet_days_correlations_threshold_annrain.annual_ndays_'+this_threshold_str+'mm_silo025.test.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_correlation)+1,/REV
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
   LEVS,MANUAL=mylevs_correlation
   CON,FIELD=correlation_wetdays_annrain,X=wetdays_longitude,Y=wetdays_latitude,$
       TITLE="Inst corr for number of days over "+this_threshold_str+" mm/day and annual rain - SILO (1900-2008)",$
       /BLOCK,/CB_RIGHT,/NOLINES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/wet_days/correlations/'+$
          'qld_wet_days_correlations_threshold_annrain.annual_amount_'+this_threshold_str+'mm_silo025.test.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_correlation)+1,/REV
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
   LEVS,MANUAL=mylevs_correlation
   CON,FIELD=correlation_meanrain_annrain,X=wetdays_longitude,Y=wetdays_latitude,$
       TITLE="Inst corr for mean rain on days over "+this_threshold_str+" mm/day and annual rain - SILO (1900-2008)",$
       /BLOCK,/CB_RIGHT,/NOLINES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/wet_days/correlations/'+$
          'qld_wet_days_correlations_threshold_annrain.annual_fraction_'+this_threshold_str+'mm_silo025.test.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_correlation)+1,/REV
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
   LEVS,MANUAL=mylevs_correlation
   CON,FIELD=correlation_fracrain_annrain,X=wetdays_longitude,Y=wetdays_latitude,$
       TITLE="Inst corr for frac of annual rain falling on days over "+this_threshold_str+" mm/day and annual rain - SILO (1900-2008)",$
       /BLOCK,/CB_RIGHT,/NOLINES
   PSCLOSE,/NOVIEW

ENDFOR

STOP
END

   
