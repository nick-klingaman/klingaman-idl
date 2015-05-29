PRO qld_wet_days_terciles_frac_contributions

; File containing timeseries of Nov-Apr mean rainfall
silo_seasmean_infile='/home/ss901165/datasets/SILO/one_quarter/SILO_precip.nov-apr_smeans.1900-2008.0.25x0.25.nc'
; File containing timeseries of Nov-Apr daily rainfall
silo_dailymean_infile='/home/ss901165/datasets/SILO/one_quarter/SILO_precip.nov-apr_dmeans.1900-2008.0.25x0.25.nc'
; File containing climatology of Nov-Apr rainfall
silo_seasmean_clim_infile='/home/ss901165/datasets/SILO/one_quarter/SILO_precip.nov-apr_smean_clim.1900-2008.0.25x0.25.nc'

silo_nyears=107
silo_ndays_per_year=180

; Threshold for rainfall
precip_threshold=50
precip_threshold_str=STRTRIM(STRING(precip_threshold),1)
precip_threshold_str_title=' >= '+precip_threshold_str

; Box to consider
box=[-10,140,-30,154]
;box=[-15,145,-20,150]

; Read latitude and longitude, get number of points
silo_longitude=OPEN_AND_EXTRACT(silo_seasmean_infile,'longitude')
silo_latitude=OPEN_AND_EXTRACT(silo_seasmean_infile,'latitude')
DEFINE_BOUNDARIES,box,silo_latitude,silo_longitude,silo_box_tx,/LIMIT
silo_nlon=N_ELEMENTS(silo_longitude)
silo_nlat=N_ELEMENTS(silo_latitude)

; Mask infile
mask_infile='/home/ss901165/datasets/SILO/one_quarter_lsm.nc'
mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)
mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                      offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                      count=[mask_nlon,mask_nlat,1,1]))

; Read climatological mean rainfall
silo_clim_precip=REFORM(OPEN_AND_EXTRACT(silo_seasmean_clim_infile,'rain',$
                                         offset=[silo_box_tx(1),silo_box_tx(0)],$
                                         count=[silo_nlon,silo_nlat]))
silo_clim_precip[where(silo_clim_precip ge 1E20)]=!Values.F_NaN

silo_upper_frac_from_ndays=fltarr(silo_nlon,silo_nlat)
silo_upper_frac_from_mean=fltarr(silo_nlon,silo_nlat)
silo_upper_frac_from_residual=fltarr(silo_nlon,silo_nlat)
silo_lower_frac_from_ndays=fltarr(silo_nlon,silo_nlat)
silo_lower_frac_from_mean=fltarr(silo_nlon,silo_nlat)
silo_lower_frac_from_residual=fltarr(silo_nlon,silo_nlat)

FOR j=0,silo_nlon-1 DO BEGIN
   print,'-> Calculating for j = '+STRTRIM(STRING(j),1)+' of '+STRTRIM(STRING(silo_nlon),1)
   FOR k=0,silo_nlat-1 DO BEGIN
;      print,'---> Calculating for k = '+STRTRIM(STRING(k),1)+' of '+STRTRIM(STRING(silo_nlat),1)
      excess_rain=0
      excess_rain_from_ndays=0
      excess_rain_from_mean=0
      excess_rain_from_residual=0
      deficient_rain=0
      deficient_rain_from_ndays=0
      deficient_rain_from_mean=0
      deficient_rain_from_residual=0
      silo_ndays_upper_total=0
      silo_ndays_lower_total=0
                                ; Read seasonal mean data
      silo_seasmeans=REFORM(OPEN_AND_EXTRACT(silo_seasmean_infile,'rain',$
                                             offset=[silo_box_tx(1)+j,silo_box_tx(0)+k,0],$
                                             count=[1,1,silo_nyears]))
                                ; Sort seasonal mean data
      sorted_indices=SORT(silo_seasmeans)
      silo_seasmeans_sorted=silo_seasmeans[sorted_indices]
      upper_tercile_indices=sorted_indices(silo_nyears*2/3+1:silo_nyears-1)
      n_upper=N_ELEMENTS(upper_tercile_indices)
      lower_tercile_indices=sorted_indices(0:silo_nyears/3-1)
      n_lower=N_ELEMENTS(lower_tercile_indices)

      silo_all_precip=fltarr(silo_nyears*silo_ndays_per_year)

      IF silo_seasmeans(0) le 1E20 and mask(j,k) ne 0 THEN BEGIN
         FOR i=0,silo_nyears-1 DO BEGIN
            lower_bound=i*silo_ndays_per_year
            upper_bound=(i+1)*silo_ndays_per_year-1
            silo_all_precip(lower_bound:upper_bound)=$
               REFORM(OPEN_AND_EXTRACT(silo_dailymean_infile,'rain',$
                                       offset=[silo_box_tx(1)+j,silo_box_tx(0)+k,0,i],$
                                       count=[1,1,silo_ndays_per_year,1]))         
         ENDFOR
         
         silo_ndays_all=N_ELEMENTS(where(silo_all_precip ge precip_threshold))/FLOAT(silo_nyears)
         silo_mean_all=MEAN(silo_all_precip[where(silo_all_precip ge precip_threshold)])
         
         FOR i=0,n_upper-1 DO BEGIN
                                ; Count up all excess rainfall            
            silo_thisyear_precip=REFORM(OPEN_AND_EXTRACT(silo_dailymean_infile,'rain',$
                                                         offset=[silo_box_tx(1)+j,silo_box_tx(0)+k,0,upper_tercile_indices(i)],$
                                                         count=[1,1,silo_ndays_per_year,1]))
            excess_rain=excess_rain+TOTAL(silo_thisyear_precip)-silo_clim_precip(j,k)*silo_ndays_per_year
            IF TOTAL(where(silo_thisyear_precip ge precip_threshold)) ge 0 THEN BEGIN
               silo_ndays_upper=N_ELEMENTS(where(silo_thisyear_precip ge precip_threshold))
               silo_ndays_upper_total=silo_ndays_upper_total+silo_ndays_upper
               silo_mean_upper=MEAN(silo_thisyear_precip[where(silo_thisyear_precip ge precip_threshold)])
                                ; Count up all excess rainfall from
                                ; change in number of wet days
               excess_rain_from_ndays=excess_rain_from_ndays+$
                                      (silo_ndays_upper-silo_ndays_all)*silo_mean_all
                                ; Count up all excess rainfall from
                                ; change in mean amount of rainfall on
                                ; wet days
               excess_rain_from_mean=excess_rain_from_mean+$
                                     (silo_mean_upper-silo_mean_all)*silo_ndays_all
                                ; Count up all excess rainfall from
                                ; interaction term (change in rainfall
                                ; on change in wet days)
               excess_rain_from_residual=excess_rain_from_residual+$
                                         (silo_mean_upper-silo_mean_all)*(silo_ndays_upper-silo_ndays_all)
            ENDIF
         ENDFOR

         FOR i=0,n_lower-1 DO BEGIN
                                ; Count up all deficient rainfall
            silo_thisyear_precip=REFORM(OPEN_AND_EXTRACT(silo_dailymean_infile,'rain',$
                                                         offset=[silo_box_tx(1)+j,silo_box_tx(0)+k,0,lower_tercile_indices(i)],$
                                                         count=[1,1,silo_ndays_per_year,1]))
            deficient_rain=deficient_rain+TOTAL(silo_thisyear_precip)-silo_clim_precip(j,k)*silo_ndays_per_year
            IF TOTAL(where(silo_thisyear_precip ge precip_threshold)) ge 0 THEN BEGIN
               silo_ndays_lower=N_ELEMENTS(where(silo_thisyear_precip ge precip_threshold))
               silo_ndays_lower_total=silo_ndays_lower_total+silo_ndays_lower
               silo_mean_lower=MEAN(silo_thisyear_precip[where(silo_thisyear_precip ge precip_threshold)])
            ENDIF ELSE BEGIN
               silo_ndays_lower=0
               silo_mean_lower=0
            ENDELSE
                                ; Count up all deficient rainfall from
                                ; change in number of wet days
            deficient_rain_from_ndays=deficient_rain_from_ndays+$
                                      (silo_ndays_lower-silo_ndays_all)*silo_mean_all
                                ; Count up all deficient rainfall from
                                ; change in mean amount of rainfall on
                                ; wet days
            deficient_rain_from_mean=deficient_rain_from_mean+$
                                     (silo_mean_lower-silo_mean_all)*silo_ndays_all
                                ; Count up all deficient rainfall from
                                ; interaction term (change in rainfall
                                ; on change in wet days)
            deficient_rain_from_residual=deficient_rain_from_residual+$
                                         (silo_mean_lower-silo_mean_all)*(silo_ndays_lower-silo_ndays_all)
         ENDFOR

         IF FLOAT(silo_ndays_upper_total)/FLOAT(n_upper) ge 1 THEN BEGIN
            silo_upper_frac_from_ndays(j,k)=excess_rain_from_ndays/excess_rain
            silo_upper_frac_from_mean(j,k)=excess_rain_from_mean/excess_rain
            silo_upper_frac_from_residual(j,k)=excess_rain_from_residual/excess_rain
         ENDIF ELSE BEGIN
            silo_upper_frac_from_ndays(j,k)=!Values.F_NaN
            silo_upper_frac_from_mean(j,k)=!Values.F_NaN
            silo_upper_frac_from_residual(j,k)=!Values.F_NaN
         ENDELSE

         IF FLOAT(silo_ndays_lower_total)/FLOAT(n_lower) ge 1 THEN BEGIN
            silo_lower_frac_from_ndays(j,k)=deficient_rain_from_ndays/deficient_rain
            silo_lower_frac_from_mean(j,k)=deficient_rain_from_mean/deficient_rain
            silo_lower_frac_from_residual(j,k)=deficient_rain_from_residual/deficient_rain
         ENDIF ELSE BEGIN
            silo_lower_frac_from_ndays(j,k)=!Values.F_NaN
            silo_lower_frac_from_mean(j,k)=!Values.F_NaN
            silo_lower_frac_from_residual(j,k)=!Values.F_NaN
         ENDELSE

      ENDIF ELSE BEGIN
         silo_upper_frac_from_ndays(j,k)=!Values.F_NaN
         silo_upper_frac_from_mean(j,k)=!Values.F_NaN
         silo_upper_frac_from_residual(j,k)=!Values.F_NaN
         silo_lower_frac_from_ndays(j,k)=!Values.F_NaN
         silo_lower_frac_from_mean(j,k)=!Values.F_NaN
         silo_lower_frac_from_residual(j,k)=!Values.F_NaN
      ENDELSE
      
      
   ENDFOR
ENDFOR

mylevs=['0.00','0.05','0.10','0.15','0.20','0.25','0.30','0.35','0.40','0.45','0.50',$
        '0.55','0.60','0.65','0.70','0.75','0.80','0.85','0.90','0.95','1.00']
mylevs_residual=['-0.26','-0.22','-0.18','-0.14','-0.10','-0.06','-0.02','0.02','0.06','0.10','0.14','0.18','0.22','0.26']
mylevs_total=['0.10','0.20','0.30','0.40','0.50','0.60','0.70','0.80','0.90','1.00','1.10','1.20']
mylevs_ratio=['0.20','0.25','0.33','0.40','0.50','0.57','0.67','0.75','0.83','0.91','1.00',$
              '1.10','1.20','1.33','1.50','1.75','2.00','2.50','3.00','4.00','5.00']

; Plots for upper tercile

psfile='/home/ss901165/idl/queensland/wet_days/terciles/qld_wet_days_terciles_frac_contributions.upper_from_ndays.'+$
       precip_threshold_str+'mm_thresh.silo025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV;,white=[2]
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
LEVS,MANUAL=mylevs
CON,FIELD=silo_upper_frac_from_ndays,X=silo_longitude,Y=silo_latitude,$
    TITLE="Frac of add'l rain in + tercile Nov-Apr from more days "+precip_threshold_str_title+" mm/day - SILO 0.25",$
    /BLOCK,/CB_RIGHT,/NOLINES
PSCLOSE,/NOVIEW
 
psfile='/home/ss901165/idl/queensland/wet_days/terciles/qld_wet_days_terciles_frac_contributions.upper_from_mean.'+$
       precip_threshold_str+'mm_thresh.silo025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV;,white=[2]
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
LEVS,MANUAL=mylevs
CON,FIELD=silo_upper_frac_from_mean,X=silo_longitude,Y=silo_latitude,$
    TITLE="Frac of add'l rain in + tercile Nov-Apr from mean rain on days "+precip_threshold_str_title+" mm/day - SILO 0.25",$
    /BLOCK,/CB_RIGHT,/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/wet_days/terciles/qld_wet_days_terciles_frac_contributions.upper_ratio-mean-ndays_.'+$
       precip_threshold_str+'mm_thresh.silo025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio)+1,/REV,white=[12,13]
gray=FSC_COLOR("gray",12)
gray=FSC_COLOR("gray",13)
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
LEVS,MANUAL=mylevs_ratio
CON,FIELD=silo_upper_frac_from_mean/silo_upper_frac_from_ndays,X=silo_longitude,Y=silo_latitude,$
    TITLE="Ratio of frac of add'l rain in + tercile Nov-Apr from mean/days for "+precip_threshold_str_title+" mm/day - SILO 0.25",$
    /BLOCK,/CB_RIGHT,/NOLINES
FOR i=1,silo_nlon-1,2 DO BEGIN
   FOR j=1,silo_nlat-1,2 DO BEGIN
      IF silo_upper_frac_from_residual(i,j) gt 0.14 THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=4,SIZE=120
      IF silo_upper_frac_from_residual(i,j) gt 0.07 and silo_upper_frac_from_residual(i,j) le 0.14 THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=4,SIZE=80
      IF silo_upper_frac_from_residual(i,j) gt 0.0 and silo_upper_frac_from_residual(i,j) le 0.07 THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=4,SIZE=40
      IF silo_upper_frac_from_residual(i,j) gt -0.07 and silo_upper_frac_from_residual(i,j) le 0 THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=8,SIZE=40
      IF silo_upper_frac_from_residual(i,j) gt -0.14 and silo_upper_frac_from_residual(i,j) le -0.07 THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=8,SIZE=80 
      IF silo_upper_frac_from_residual(i,j) le -0.14 THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=8,SIZE=120
   ENDFOR
ENDFOR
labels=[' -0.21 < int. term < -0.14 ',' -0.14 < int. term < -0.07 ',' -0.07 < int. term < 0.00 ',' 0.00 < int. term < 0.07',' 0.07 < int. term < 0.14 ',' 0.14 < int. term < 0.21 ']
sym=[8,8,8,4,4,4]
size=[120,80,40,40,80,120]
GLEGEND,labels=labels,SYM=sym,SIZE=size,LEGPOS=9,LENGTH=0
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/wet_days/terciles/qld_wet_days_terciles_frac_contributions.upper_from_resid.'+$
       precip_threshold_str+'mm_thresh.silo025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_residual)+1,/REV,white=[9]
gray=FSC_COLOR("gray",9)
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
LEVS,MANUAL=mylevs_residual
CON,FIELD=silo_upper_frac_from_residual,X=silo_longitude,Y=silo_latitude,$
    TITLE="Frac of add'l rain in + tercile Nov-Apr from days*mean on days "+precip_threshold_str_title+" mm/day - SILO 0.25",$
    /BLOCK,/CB_RIGHT,/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/wet_days/terciles/qld_wet_days_terciles_frac_contributions.upper_from_all.'+$
       precip_threshold_str+'mm_thresh.silo025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_total)+1,/REV;,white=[2]
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
LEVS,MANUAL=mylevs_total
CON,FIELD=silo_upper_frac_from_residual+silo_upper_frac_from_mean+silo_upper_frac_from_ndays,$
    X=silo_longitude,Y=silo_latitude,$
    TITLE="Frac of add'l rain in + tercile Nov-Apr from all sources on days "+precip_threshold_str_title+" mm/day - SILO 0.25",$
    /BLOCK,/CB_RIGHT,/NOLINES
PSCLOSE,/NOVIEW

; Plots for lower tercile

psfile='/home/ss901165/idl/queensland/wet_days/terciles/qld_wet_days_terciles_frac_contributions.lower_from_ndays.'+$
       precip_threshold_str+'mm_thresh.silo025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV;,white=[2]
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
LEVS,MANUAL=mylevs
CON,FIELD=silo_lower_frac_from_ndays,X=silo_longitude,Y=silo_latitude,$
    TITLE="Frac of deficit rain in - tercile Nov-Apr from fewer days "+precip_threshold_str_title+" mm/day - SILO 0.25",$
    /BLOCK,/CB_RIGHT,/NOLINES
PSCLOSE,/NOVIEW
 
psfile='/home/ss901165/idl/queensland/wet_days/terciles/qld_wet_days_terciles_frac_contributions.lower_from_mean.'+$
       precip_threshold_str+'mm_thresh.silo025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV;,white=[2]
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
LEVS,MANUAL=mylevs
CON,FIELD=silo_lower_frac_from_mean,X=silo_longitude,Y=silo_latitude,$
    TITLE="Frac of deficit rain in - tercile Nov-Apr from mean rain on days "+precip_threshold_str_title+" mm/day - SILO 0.25",$
    /BLOCK,/CB_RIGHT,/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/wet_days/terciles/qld_wet_days_terciles_frac_contributions.lower_ratio-mean-ndays_.'+$
       precip_threshold_str+'mm_thresh.silo025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio)+1,/REV,white=[12,13]
gray=FSC_COLOR("gray",12)
gray=FSC_COLOR("gray",13)
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
LEVS,MANUAL=mylevs_ratio
CON,FIELD=silo_lower_frac_from_mean/silo_lower_frac_from_ndays,X=silo_longitude,Y=silo_latitude,$
    TITLE="Ratio of frac of deficit rain in - tercile Nov-Apr from mean/days for "+precip_threshold_str_title+" mm/day - SILO 0.25",$
    /BLOCK,/CB_RIGHT,/NOLINES
FOR i=1,silo_nlon-1,2 DO BEGIN
   FOR j=1,silo_nlat-1,2 DO BEGIN
      IF silo_lower_frac_from_residual(i,j) gt 0.14 THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=4,SIZE=120
      IF silo_lower_frac_from_residual(i,j) gt 0.07 and silo_lower_frac_from_residual(i,j) le 0.14 THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=4,SIZE=80
      IF silo_lower_frac_from_residual(i,j) gt 0.0 and silo_lower_frac_from_residual(i,j) le 0.07 THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=4,SIZE=40
      IF silo_lower_frac_from_residual(i,j) gt -0.07 and silo_lower_frac_from_residual(i,j) le 0 THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=8,SIZE=40
      IF silo_lower_frac_from_residual(i,j) gt -0.14 and silo_lower_frac_from_residual(i,j) le -0.07 THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=8,SIZE=80 
      IF silo_lower_frac_from_residual(i,j) le -0.14 THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=8,SIZE=120
   ENDFOR
ENDFOR
GLEGEND,labels=labels,SYM=sym,SIZE=size,LEGPOS=9,LENGTH=0
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/wet_days/terciles/qld_wet_days_terciles_frac_contributions.lower_from_resid.'+$
       precip_threshold_str+'mm_thresh.silo025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_residual)+1,/REV,white=[9]
gray=FSC_COLOR("gray",9)
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
LEVS,MANUAL=mylevs_residual
CON,FIELD=silo_lower_frac_from_residual,X=silo_longitude,Y=silo_latitude,$
    TITLE="Frac of deficit rain in - tercile Nov-Apr from days*mean on days "+precip_threshold_str_title+" mm/day - SILO 0.25",$
    /BLOCK,/CB_RIGHT,/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/wet_days/terciles/qld_wet_days_terciles_frac_contributions.lower_from_all.'+$
       precip_threshold_str+'mm_thresh.silo025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_total)+1,/REV;,white=[2]
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
LEVS,MANUAL=mylevs_total
CON,FIELD=silo_lower_frac_from_residual+silo_lower_frac_from_mean+silo_lower_frac_from_ndays,$
    X=silo_longitude,Y=silo_latitude,$
    TITLE="Frac of deficit rain in - tercile Nov-Apr from all sources on days "+precip_threshold_str_title+" mm/day - SILO 0.25",$
    /BLOCK,/CB_RIGHT,/NOLINES
PSCLOSE,/NOVIEW

STOP

END
     
