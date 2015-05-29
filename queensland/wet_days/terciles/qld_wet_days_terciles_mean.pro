PRO qld_wet_days_terciles_mean

; File containing timeseries of Nov-Apr mean rainfall
silo_seasmean_infile='/home/ss901165/datasets_mango/SILO/one_quarter/SILO_precip.nov-apr_smeans.1900-2008.0.25x0.25.nc'
silo_mask_infile='/home/ss901165/datasets_mango/SILO/one_quarter_lsm.nc'

silo_nyears=108
silo_ndays_per_year=181

; Box to consider
box=[-10,140,-30,154]

; Read latitude and longitude, get number of points
silo_longitude=OPEN_AND_EXTRACT(silo_seasmean_infile,'longitude')
silo_latitude=OPEN_AND_EXTRACT(silo_seasmean_infile,'latitude')
DEFINE_BOUNDARIES,box,silo_latitude,silo_longitude,silo_box_tx,/LIMIT
silo_nlon=N_ELEMENTS(silo_longitude)
silo_nlat=N_ELEMENTS(silo_latitude)

; Read mask latitude and longitude, restrict to box of interest
mask_longitude=OPEN_AND_EXTRACT(silo_mask_infile,'longitude')
mask_latitude=OPEN_AND_EXTRACT(silo_mask_infile,'latitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)

mask=REFORM(OPEN_AND_EXTRACT(silo_mask_infile,'lsm',$
                             offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                             count=[mask_nlon,mask_nlat,1,1]))

silo_mean_upper=fltarr(silo_nlon,silo_nlat)
silo_mean_middle=fltarr(silo_nlon,silo_nlat)
silo_mean_lower=fltarr(silo_nlon,silo_nlat)

FOR j=0,silo_nlon-1 DO BEGIN
   print,'Calculating for j=',j,' of ',silo_nlon
   FOR k=0,silo_nlat-1 DO BEGIN
                                ; Read seasonal mean data
      silo_seasmeans=REFORM(OPEN_AND_EXTRACT(silo_seasmean_infile,'rain',$
                                             offset=[silo_box_tx(1)+j,silo_box_tx(0)+k,0],$
                                             count=[1,1,silo_nyears]))
      IF mask(j,k) ne 0 THEN BEGIN
                                ; Sort seasonal mean data
         sorted_indices=SORT(silo_seasmeans)
         silo_seasmeans_sorted=silo_seasmeans[sorted_indices]
         upper_tercile_indices=sorted_indices(silo_nyears*2/3+1:silo_nyears-1)
         n_upper=N_ELEMENTS(upper_tercile_indices)
         lower_tercile_indices=sorted_indices(0:silo_nyears/3-1)
         n_lower=N_ELEMENTS(lower_tercile_indices)
         middle_tercile_indices=sorted_indices(silo_nyears/3:silo_nyears*2/3)
         n_middle=N_ELEMENTS(middle_tercile_indices)
         
         silo_mean_upper(j,k)=MEAN(silo_seasmeans[upper_tercile_indices])
         silo_mean_middle(j,k)=MEAN(silo_seasmeans[middle_tercile_indices])
         silo_mean_lower(j,k)=MEAN(silo_seasmeans[lower_tercile_indices])
      ENDIF ELSE BEGIN
         silo_mean_upper(j,k)=!Values.F_NaN
         silo_mean_middle(j,k)=!Values.F_NaN
         silo_mean_lower(j,k)=!Values.F_NaN
      ENDELSE
   ENDFOR
ENDFOR

mylevs=[1,2,3,4,5,6,7,8,9,10,12,14,16,18,20]
mylevs_ratio=['1.00','1.25','1.50','1.75','2.00','2.25','2.50','3.00','3.50','4.00','5.00','6.00']

psfile='/home/ss901165/idl/queensland/wet_days/terciles/qld_wet_days_terciles_mean.upper_tercile.silo_025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
LEVS,MANUAL=mylevs
CON,FIELD=silo_mean_upper,X=silo_longitude,Y=silo_latitude,$
    TITLE="Mean rainfall (mm/day) in seasons (Nov-Apr) in upper tercile - SILO 0.25 (1900-2008)",$
    /CB_RIGHT,/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/wet_days/terciles/qld_wet_days_terciles_mean.middle_tercile.silo_025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
LEVS,MANUAL=mylevs
CON,FIELD=silo_mean_middle,X=silo_longitude,Y=silo_latitude,$
    TITLE="Mean rainfall (mm/day) in seasons (Nov-Apr) in middle tercile - SILO 0.25 (1900-2008)",$
    /CB_RIGHT,/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/wet_days/terciles/qld_wet_days_terciles_mean.lower_tercile.silo_025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
LEVS,MANUAL=mylevs
CON,FIELD=silo_mean_lower,X=silo_longitude,Y=silo_latitude,$
    TITLE="Mean rainfall (mm/day) in seasons (Nov-Apr) in lower tercile - SILO 0.25 (1900-2008)",$
    /CB_RIGHT,/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/wet_days/terciles/qld_wet_days_terciles_mean.upper-ratio-lower_tercile.silo_025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio)+1,/REV
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
LEVS,MANUAL=mylevs_ratio
CON,FIELD=silo_mean_upper/silo_mean_lower,X=silo_longitude,Y=silo_latitude,$
    TITLE="Mean rainfall (mm/day) in Nov-Apr - upper/lower tercile - SILO 0.25 (1900-2008)",$
    /CB_RIGHT,/NOLINES
PSCLOSE

STOP

END

      
