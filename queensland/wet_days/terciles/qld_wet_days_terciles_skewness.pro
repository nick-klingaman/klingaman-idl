PRO qld_wet_days_terciles_skewness

; File containing timeseries of Nov-Apr mean rainfall
silo_seasmean_infile='/home/ss901165/datasets_mango/SILO/one_quarter/SILO_precip.nov-apr_smeans.1900-2008.0.25x0.25.nc'
; File containing timeseries of Nov-Apr daily rainfall
silo_dailymean_infile='/home/ss901165/datasets_mango/SILO/one_quarter/SILO_precip.nov-apr_dmeans.1900-2008.0.25x0.25.nc'

silo_nyears=107
silo_ndays_per_year=181

; Threshold for 'rain' (mm/day)
precip_low_threshold=10
precip_threshold_str=STRTRIM(STRING(precip_low_threshold),1)
precip_threshold_str_title=' >= '+precip_threshold_str
precip_high_threshold=9999

; Levels for 1 mm/day
IF precip_low_threshold eq 1 THEN BEGIN
   mylevs_ndays=[5,10,15,20,30,40,50,60,70,80,90,100,110,120,130]
   mylevs_mean=[2,4,6,8,10,12,14,16,18,20,22,24,27,30,33]
   mylevs_ndays_chng=[-21,-14,-7,0,7,14,21]
   mylevs_mean_chng=[-9,-6,-3,0,3,6,9]
   mylevs_ndays_ratio=['0.50','0.57','0.67','0.75','0.83','0.91','1.00','1.10','1.20','1.33','1.50','1.75','2.00']
   white=[8,9]
ENDIF

; Levels for 5 mm/day
IF precip_low_threshold eq 5 THEN BEGIN
   mylevs_ndays=[5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95]
   mylevs_mean=[5,8,11,14,17,20,24,28,32,36,40,45,50]
   mylevs_ndays_chng=[-18,-12,-6,0,6,12,18]
   mylevs_mean_chng=[-9,-6,-3,0,3,6,9]
   mylevs_ndays_ratio=['0.20','0.25','0.33','0.40','0.50','0.57','0.67','0.75','0.83','0.91','1.00',$
                       '1.10','1.20','1.33','1.50','1.75','2.00','2.50','3.00','4.00','5.00']
   white=[12,13]
ENDIF

; Levels for 10 mm/day
IF precip_low_threshold eq 10 THEN BEGIN
   mylevs_ndays=[2,4,6,9,12,15,18,21,24,28,32,36,40,45,50,55]
   mylevs_mean=[10,14,18,22,26,30,35,40,45,50,55,60]
   mylevs_ndays_chng=[-15,-10,-5,0,5,10,15]
   mylevs_mean_chng=[-12,-8,-4,0,4,8,12]
   mylevs_ndays_ratio=['0.20','0.25','0.33','0.40','0.50','0.57','0.67','0.75','0.83','0.91','1.00',$
                       '1.10','1.20','1.33','1.50','1.75','2.00','2.50','3.00','4.00','5.00']
   white=[12,13]
ENDIF

labels_ndays=$
   [STRTRIM(STRING(mylevs_ndays_chng(0)),1)+' < wet days < '+STRTRIM(STRING(mylevs_ndays_chng(1)),1),$
    STRTRIM(STRING(mylevs_ndays_chng(1)),1)+' < wet days < '+STRTRIM(STRING(mylevs_ndays_chng(2)),1),$
    STRTRIM(STRING(mylevs_ndays_chng(2)),1)+' < wet days < '+STRTRIM(STRING(mylevs_ndays_chng(3)),1),$
    STRTRIM(STRING(mylevs_ndays_chng(3)),1)+' < wet days < '+STRTRIM(STRING(mylevs_ndays_chng(4)),1),$
    STRTRIM(STRING(mylevs_ndays_chng(4)),1)+' < wet days < '+STRTRIM(STRING(mylevs_ndays_chng(5)),1),$
    STRTRIM(STRING(mylevs_ndays_chng(5)),1)+' < wet days < '+STRTRIM(STRING(mylevs_ndays_chng(6)),1)]

labels_mean=$
   [STRTRIM(STRING(mylevs_mean_chng(0)),1)+' < mean rain (mm day!U-1!N) < '+STRTRIM(STRING(mylevs_mean_chng(1)),1),$
    STRTRIM(STRING(mylevs_mean_chng(1)),1)+' < mean rain (mm day!U-1!N) < '+STRTRIM(STRING(mylevs_mean_chng(2)),1),$
    STRTRIM(STRING(mylevs_mean_chng(2)),1)+' < mean rain (mm day!U-1!N) < '+STRTRIM(STRING(mylevs_mean_chng(3)),1),$
    STRTRIM(STRING(mylevs_mean_chng(3)),1)+' < mean rain (mm day!U-1!N) < '+STRTRIM(STRING(mylevs_mean_chng(4)),1),$
    STRTRIM(STRING(mylevs_mean_chng(4)),1)+' < mean rain (mm day!U-1!N) < '+STRTRIM(STRING(mylevs_mean_chng(5)),1),$
    STRTRIM(STRING(mylevs_mean_chng(5)),1)+' < mean rain (mm day!U-1!N) < '+STRTRIM(STRING(mylevs_mean_chng(6)),1)]
                                                                        
; For bands of precip
;precip_low_threshold=20
;precip_high_threshold=40
;precip_low_threshold_str=STRTRIM(STRING(precip_low_threshold),1)
;precip_high_threshold_str=STRTRIM(STRING(precip_high_threshold),1)
;precip_threshold_str=precip_low_threshold_str+'-'+precip_high_threshold_str
;precip_threshold_str_title=precip_threshold_str

; Box to consider
box=[-10,140,-30,154]
;box=[-15,145,-20,150]

; Read latitude and longitude, get number of points
silo_longitude=OPEN_AND_EXTRACT(silo_seasmean_infile,'longitude')
silo_latitude=OPEN_AND_EXTRACT(silo_seasmean_infile,'latitude')
DEFINE_BOUNDARIES,box,silo_latitude,silo_longitude,silo_box_tx,/LIMIT
silo_nlon=N_ELEMENTS(silo_longitude)
silo_nlat=N_ELEMENTS(silo_latitude)

silo_ndays_upper=fltarr(silo_nlon,silo_nlat)
silo_ndays_lower=fltarr(silo_nlon,silo_nlat)
silo_ndays_middle=fltarr(silo_nlon,silo_nlat)
;silo_ndays_all=fltarr(silo_nlon,silo_nlat)
silo_mean_upper=fltarr(silo_nlon,silo_nlat)
silo_mean_lower=fltarr(silo_nlon,silo_nlat)
silo_mean_middle=fltarr(silo_nlon,silo_nlat)
;silo_mean_all=fltarr(silo_nlon,silo_nlat)
silo_skew_upper=fltarr(silo_nlon,silo_nlat)
silo_skew_lower=fltarr(silo_nlon,silo_nlat)
silo_skew_middle=fltarr(silo_nlon,silo_nlat)
;silo_skew_all=fltarr(silo_nlon,silo_nlat)

FOR j=0,silo_nlon-1 DO BEGIN
   print,'Calculating for j=',j,' of ',silo_nlon
   FOR k=0,silo_nlat-1 DO BEGIN
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
      middle_tercile_indices=sorted_indices(silo_nyears/3:silo_nyears*2/3)
      n_middle=N_ELEMENTS(middle_tercile_indices)

      silo_upper_tercile_precip=fltarr(n_upper*silo_ndays_per_year)
      silo_lower_tercile_precip=fltarr(n_lower*silo_ndays_per_year)
      silo_middle_tercile_precip=fltarr(n_middle*silo_ndays_per_year)
;      silo_all_precip=fltarr(silo_nyears*silo_ndays_per_year)

      IF silo_seasmeans(0) le 1E20 THEN BEGIN
         
         FOR i=0,n_upper-1 DO BEGIN
            lower_bound=i*silo_ndays_per_year
            upper_bound=(i+1)*silo_ndays_per_year-1
            silo_upper_tercile_precip(lower_bound:upper_bound)=$
               REFORM(OPEN_AND_EXTRACT(silo_dailymean_infile,'rain',$
                                       offset=[silo_box_tx(1)+j,silo_box_tx(0)+k,0,upper_tercile_indices(i)],$
                                       count=[1,1,silo_ndays_per_year,1]))
         ENDFOR
         
         FOR i=0,n_lower-1 DO BEGIN
            lower_bound=i*silo_ndays_per_year
            upper_bound=(i+1)*silo_ndays_per_year-1
            silo_lower_tercile_precip(lower_bound:upper_bound)=$
               REFORM(OPEN_AND_EXTRACT(silo_dailymean_infile,'rain',$
                                       offset=[silo_box_tx(1)+j,silo_box_tx(0)+k,0,lower_tercile_indices(i)],$
                                       count=[1,1,silo_ndays_per_year,1]))
         ENDFOR
         
;         FOR i=0,silo_nyears-1 DO BEGIN
;            lower_bound=i*silo_ndays_per_year
;            upper_bound=(i+1)*silo_ndays_per_year-1
;            silo_all_precip(lower_bound:upper_bound)=$
;               REFORM(OPEN_AND_EXTRACT(silo_dailymean_infile,'rain',$
;                                       offset=[silo_box_tx(1)+j,silo_box_tx(0)+k,0,i],$
;                                       count=[1,1,silo_ndays_per_year,1]))         
;         ENDFOR
         
         FOR i=0,n_middle-1 DO BEGIN
            lower_bound=i*silo_ndays_per_year
            upper_bound=(i+1)*silo_ndays_per_year-1
            silo_middle_tercile_precip(lower_bound:upper_bound)=$
               REFORM(OPEN_AND_EXTRACT(silo_dailymean_infile,'rain',$
                                       offset=[silo_box_tx(1)+j,silo_box_tx(0)+k,0,middle_tercile_indices(i)],$
                                       count=[1,1,silo_ndays_per_year,1]))
         ENDFOR
               
;         silo_lower_tercile_precip_threshold=silo_lower_tercile_precip[where(silo_lower_tercile_precip ge precip_threshold)]
;         silo_upper_tercile_precip_threshold=silo_upper_tercile_precip[where(silo_upper_tercile_precip ge precip_threshold)]
;         silo_middle_tercile_precip_threshold=silo_middle_tercile_precip[where(silo_middle_tercile_precip ge precip_threshold)]
         silo_lower_tercile_precip_threshold=silo_lower_tercile_precip[$
                                             where(silo_lower_tercile_precip ge precip_low_threshold and $
                                                   silo_lower_tercile_precip le precip_high_threshold)]
         silo_middle_tercile_precip_threshold=silo_middle_tercile_precip[$
                                             where(silo_middle_tercile_precip ge precip_low_threshold and $
                                                   silo_middle_tercile_precip le precip_high_threshold)]
         silo_upper_tercile_precip_threshold=silo_upper_tercile_precip[$
                                             where(silo_upper_tercile_precip ge precip_low_threshold and $
                                                   silo_upper_tercile_precip le precip_high_threshold)]
         ;silo_all_precip_threshold=silo_all_precip[where(silo_all_precip ge precip_threshold)]      
         
         silo_ndays_upper(j,k)=N_ELEMENTS(silo_upper_tercile_precip_threshold)/FLOAT(n_upper)
         silo_ndays_lower(j,k)=N_ELEMENTS(silo_lower_tercile_precip_threshold)/FLOAT(n_lower)
         silo_ndays_middle(j,k)=N_ELEMENTS(silo_middle_tercile_precip_threshold)/FLOAT(n_middle)
;         silo_ndays_all(j,k)=N_ELEMENTS(silo_all_precip_threshold)/FLOAT(silo_nyears)
         
         silo_mean_upper(j,k)=MEAN(silo_upper_tercile_precip_threshold)
         silo_mean_lower(j,k)=MEAN(silo_lower_tercile_precip_threshold)
         silo_mean_middle(j,k)=MEAN(silo_middle_tercile_precip_threshold)
;         silo_mean_all(j,k)=MEAN(silo_all_precip_threshold)
         
         silo_skew_upper(j,k)=SKEWNESS(silo_upper_tercile_precip_threshold)
         silo_skew_lower(j,k)=SKEWNESS(silo_lower_tercile_precip_threshold)      
         silo_skew_middle(j,k)=SKEWNESS(silo_middle_tercile_precip_threshold)
;         silo_skew_all(j,k)=SKEWNESS(silo_all_precip_threshold)
         
      ENDIF ELSE BEGIN
         
         silo_ndays_upper(j,k)=!Values.F_NaN
         silo_ndays_lower(j,k)=!Values.F_NaN
         silo_ndays_middle(j,k)=!Values.F_NaN
;         silo_ndays_all(j,k)=!Values.F_NaN
         silo_mean_upper(j,k)=!Values.F_NaN
         silo_mean_lower(j,k)=!Values.F_NaN
         silo_mean_middle(j,k)=!Values.F_NaN
;         silo_mean_all(j,k)=!Values.F_NaN
         silo_skew_upper(j,k)=!Values.F_NaN
         silo_skew_lower(j,k)=!Values.F_NaN
         silo_skew_middle(j,k)=!Values.F_NaN
;         silo_skew_all(j,k)=!Values.F_NaN

      ENDELSE
   ENDFOR
ENDFOR
      
;mylevs_ndays_ratio=[1/3.,1/2.,1/1.75,1/1.5,1/1.40,1/1.30,1/1.20,1/1.15,1/1.1,1/1.05,1.,1.05,1.1,1.15,1.20,1.30,1.40,1.5,1.75,2.,3.]
;mylevs_ndays_ratio=['0.50','0.55','0.60','0.65','0.70','0.75','0.80','0.85','0.90','0.95','1.00',$
;                    '1.05','1.10','1.15','1.20','1.25','1.30','1.35','1.40','1.45','1.50']
;mylevs_ndays_ratio=['0.35','0.45','0.55','0.65','0.75','0.85','0.95','1.05','1.15','1.25','1.35','1.45','1.55','1.65']

mylevs_frac=['-0.08','-0.06','-0.04','-0.02','0.00','0.02','0.04','0.06','0.08']
mylevs_skew=['1.5','1.8','2.1','2.4','2.7','3.0','3.3','3.6','3.9','4.2','4.5']
mylevs_diff_skew=['-2.00','-1.75','-1.50','-1.25','-1.00','-0.75','-0.50','-0.25','0.00',$
                  '0.25','0.50','0.75','1.00','1.25','1.50','1.75','2.00']
;FOR i=0,N_ELEMENTS(mylevs_ndays_ratio)-1 DO BEGIN
;   print,mylevs_ndays_ratio(i)
;   IF mylevs_ndays_ratio(i) lt 1 THEN BEGIN
;      print,'<1'
;      mylevs_ndays_ratio(i)=STRTRIM(SIGFIG(mylevs_ndays_ratio(i),2),1)
;   ENDIF ELSE $
;      mylevs_ndays_ratio(i)=STRTRIM(SIGFIG(mylevs_ndays_ratio(i),3),1)
;   print,mylevs_ndays_ratio(i)
;ENDFOR
;print,mylevs_ndays_ratio
mylevs_mean_ratio=mylevs_ndays_ratio  

psfile='/home/ss901165/idl/queensland/wet_days/terciles/qld_wet_days_terciles_skewness.nov-apr_ndays_upper.'+$
       STRTRIM(STRING(precip_low_threshold),1)+'mm_thresh.silo025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=13,NCOLS=N_ELEMENTS(mylevs_ndays)+1,/REV;,white=[2]
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
LEVS,MANUAL=mylevs_ndays
CON,FIELD=silo_ndays_upper,X=silo_longitude,Y=silo_latitude,$
    TITLE="Days per season with rain "+precip_threshold_str_title+$
    " mm/day in seasons in upper tercile of total rain - SILO 0.25",$
    /BLOCK,/CB_RIGHT,/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/wet_days/terciles/qld_wet_days_terciles_skewness.nov-apr_ndays_upper-ratio-middle.'+$
       precip_threshold_str+'mm_thresh.silo025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ndays_ratio)+1,/REV,white=[white]
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
LEVS,MANUAL=mylevs_ndays_ratio
CON,FIELD=silo_ndays_upper/FLOAT(silo_ndays_middle),X=silo_longitude,Y=silo_latitude,$
    TITLE="Frac change in days per season with rain "+precip_threshold_str_title+$
    " mm/day for seasons in upper tercile of total rain - SILO 0.25",$
    /BLOCK,/CB_RIGHT,/NOLINES
;CON,FIELD=silo_ndays_upper/FLOAT(silo_ndays_all),X=silo_longitude,Y=silo_latitude,$
;    TITLE="Frac change in days per season with rain "+precip_threshold_str_title+$
;    " mm/day for seasons in upper tercile of total rain - SILO 0.25",$
;    /BLOCK,/CB_RIGHT,/NOLINES
;LEVS,MANUAL=mylevs_frac
;CON,FIELD=(silo_ndays_upper-silo_ndays_middle)/FLOAT(silo_ndays_per_year),X=silo_longitude,Y=silo_latitude,$
;    /NOFILL,THICK=150
season_change=(silo_ndays_upper-silo_ndays_middle)

sym=[8,8,8,4,4,4]
size=[120,80,40,40,80,120]
LEGEND,labels=labels_ndays,SYM=sym,SIZE=size,LEGPOS=9,LENGTH=0

FOR i=1,silo_nlon-1,2 DO BEGIN
   FOR j=1,silo_nlat-1,2 DO BEGIN
      IF season_change(i,j) gt mylevs_ndays_chng(5) THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=4,SIZE=120
      IF season_change(i,j) gt mylevs_ndays_chng(4) and $
         season_change(i,j) le mylevs_ndays_chng(5) THEN $
            GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=4,SIZE=80
      IF season_change(i,j) gt mylevs_ndays_chng(3) and $
         season_change(i,j) le mylevs_ndays_chng(4) THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=4,SIZE=40
      IF season_change(i,j) gt mylevs_ndays_chng(2) and $
         season_change(i,j) le mylevs_ndays_chng(3) THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=8,SIZE=40
      IF season_change(i,j) gt mylevs_ndays_chng(1) and $
         season_change(i,j) le mylevs_ndays_chng(2) THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=8,SIZE=80 
      IF season_change(i,j) le mylevs_ndays_chng(1) THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=8,SIZE=120
   ENDFOR
ENDFOR

;STIPPLE,FIELD=season_change(0:silo_nlon-1:2,0:silo_nlat-1:2),$
;        X=silo_longitude(0:silo_nlon-1:2),Y=silo_latitude(0:silo_nlat-1:2),MIN=-0.10,MAX=-0.05,SYM=8,SIZE=200
;STIPPLE,FIELD=season_change(0:silo_nlon-1:2,0:silo_nlat-1:2),$
;        X=silo_longitude(0:silo_nlon-1:2),Y=silo_latitude(0:silo_nlat-1:2),MIN=-0.05,MAX=0,SYM=8,SIZE=100
;STIPPLE,FIELD=season_change(0:silo_nlon-1:2,0:silo_nlat-1:2),$
;        X=silo_longitude(0:silo_nlon-1:2),Y=silo_latitude(0:silo_nlat-1:2),MIN=0,MAX=0.05,SYM=4,SIZE=100
;STIPPLE,FIELD=season_change(0:silo_nlon-1:2,0:silo_nlat-1:2),$
;        X=silo_longitude(0:silo_nlon-1:2),Y=silo_latitude(0:silo_nlat-1:2),MIN=0.05,MAX=0.10,SYM=4,SIZE=200
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/wet_days/terciles/qld_wet_days_terciles_skewness.nov-apr_ndays_lower.'+$
       precip_threshold_str+'mm_thresh.silo025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=13,NCOLS=N_ELEMENTS(mylevs_ndays)+1,/REV;,white=[2]
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
LEVS,MANUAL=mylevs_ndays
CON,FIELD=silo_ndays_lower,X=silo_longitude,Y=silo_latitude,$
    TITLE="Days per season with rain "+precip_threshold_str_title+$
    " mm/day in seasons in lower tercile of total rain - SILO 0.25",$
    /BLOCK,/CB_RIGHT,/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/wet_days/terciles/qld_wet_days_terciles_skewness.nov-apr_ndays_lower-ratio-middle.'+$
       precip_threshold_str+'mm_thresh.silo025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ndays_ratio)+1,/REV,white=[white]
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
LEVS,MANUAL=mylevs_ndays_ratio
CON,FIELD=silo_ndays_lower/FLOAT(silo_ndays_middle),X=silo_longitude,Y=silo_latitude,$
    TITLE="Frac change in days per season with rain "+precip_threshold_str_title+$
    " mm/day for seasons in lower tercile of total rain - SILO 0.25",$
    /BLOCK,/CB_RIGHT,/NOLINES
;LEVS,MANUAL=mylevs_frac
;CON,FIELD=(silo_ndays_lower-silo_ndays_middle)/FLOAT(silo_ndays_per_year),X=silo_longitude,Y=silo_latitude,$
;    /NOFILL,THICK=150
season_change=(silo_ndays_lower-silo_ndays_middle)
FOR i=1,silo_nlon-1,2 DO BEGIN
   FOR j=1,silo_nlat-1,2 DO BEGIN
      IF season_change(i,j) gt mylevs_ndays_chng(5) THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=4,SIZE=120
      IF season_change(i,j) gt mylevs_ndays_chng(4) and $
         season_change(i,j) le mylevs_ndays_chng(5) THEN $
            GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=4,SIZE=80
      IF season_change(i,j) gt mylevs_ndays_chng(3) and $
         season_change(i,j) le mylevs_ndays_chng(4) THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=4,SIZE=40
      IF season_change(i,j) gt mylevs_ndays_chng(2) and $
         season_change(i,j) le mylevs_ndays_chng(3) THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=8,SIZE=40
      IF season_change(i,j) gt mylevs_ndays_chng(1) and $
         season_change(i,j) le mylevs_ndays_chng(2) THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=8,SIZE=80 
      IF season_change(i,j) le mylevs_ndays_chng(1) THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=8,SIZE=120
   ENDFOR
ENDFOR
LEGEND,labels=labels_ndays,SYM=sym,SIZE=size,LEGPOS=9,LENGTH=0
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/wet_days/terciles/qld_wet_days_terciles_skewness.nov-apr_ndays_middle.'+$
       precip_threshold_str+'mm_thresh.silo025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=13,NCOLS=N_ELEMENTS(mylevs_ndays)+1,/REV;,white=[white]
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
LEVS,MANUAL=mylevs_ndays
CON,FIELD=silo_ndays_middle,X=silo_longitude,Y=silo_latitude,$
    TITLE="Days per season with rain "+precip_threshold_str_title+" mm/day for seasons in middle tercile - SILO 0.25",$
    /BLOCK,/CB_RIGHT,/NOLINES
PSCLOSE,/NOVIEW

; Plots for mean rainfall

psfile='/home/ss901165/idl/queensland/wet_days/terciles/qld_wet_days_terciles_skewness.nov-apr_amount_upper.'+$
       precip_threshold_str+'mm_thresh.silo025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_mean)+1,/REV;,white=[white]
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
LEVS,MANUAL=mylevs_mean
CON,FIELD=silo_mean_upper,X=silo_longitude,Y=silo_latitude,$
    TITLE="Mean rain on days with rain "+precip_threshold_str_title+$
    " mm/day in seasons in upper tercile of total rain - SILO 0.25",$
    /BLOCK,/CB_RIGHT,/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/wet_days/terciles/qld_wet_days_terciles_skewness.nov-apr_amount_upper-ratio-middle.'+$
       precip_threshold_str+'mm_thresh.silo025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_mean_ratio)+1,/REV,white=[white]
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
LEVS,MANUAL=mylevs_mean_ratio
CON,FIELD=silo_mean_upper/FLOAT(silo_mean_middle),X=silo_longitude,Y=silo_latitude,$
    TITLE="Frac change in mean rain on days with rain "+precip_threshold_str_title+$
    " mm/day for seasons in upper tercile of total rain - SILO 0.25",$
    /BLOCK,/CB_RIGHT,/NOLINES
rainfall_change=silo_mean_upper-silo_mean_middle
FOR i=1,silo_nlon-1,2 DO BEGIN
   FOR j=1,silo_nlat-1,2 DO BEGIN
      IF rainfall_change(i,j) gt mylevs_mean_chng(5) THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=4,SIZE=120
      IF rainfall_change(i,j) gt mylevs_mean_chng(4) and $
         rainfall_change(i,j) le mylevs_mean_chng(5) THEN $
            GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=4,SIZE=80
      IF rainfall_change(i,j) gt mylevs_mean_chng(3) and $
         rainfall_change(i,j) le mylevs_mean_chng(4) THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=4,SIZE=40
      IF rainfall_change(i,j) gt mylevs_mean_chng(2) and $
         rainfall_change(i,j) le mylevs_mean_chng(3) THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=8,SIZE=40
      IF rainfall_change(i,j) gt mylevs_mean_chng(1) and $
         rainfall_change(i,j) le mylevs_mean_chng(2) THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=8,SIZE=80 
      IF rainfall_change(i,j) le mylevs_mean_chng(1) THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=8,SIZE=120
   ENDFOR
ENDFOR
sym=[8,8,8,4,4,4]
size=[120,80,40,40,80,120]
LEGEND,labels=labels_mean,SYM=sym,SIZE=size,LEGPOS=9,LENGTH=0
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/wet_days/terciles/qld_wet_days_terciles_skewness.nov-apr_amount_lower.'+$
       precip_threshold_str+'mm_thresh.silo025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_mean)+1,/REV;,white=[2]
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
LEVS,MANUAL=mylevs_mean
CON,FIELD=silo_mean_lower,X=silo_longitude,Y=silo_latitude,$
    TITLE="Mean rain on days with rain "+precip_threshold_str_title+$
    " mm/day in seasons in lower tercile of total rain - SILO 0.25",$
    /BLOCK,/CB_RIGHT,/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/wet_days/terciles/qld_wet_days_terciles_skewness.nov-apr_amount_lower-ratio-middle.'+$
       precip_threshold_str+'mm_thresh.silo025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_mean_ratio)+1,/REV,white=[white]
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
LEVS,MANUAL=mylevs_mean_ratio
CON,FIELD=silo_mean_lower/FLOAT(silo_mean_middle),X=silo_longitude,Y=silo_latitude,$
    TITLE="Frac change in mean rain on days with rain "+precip_threshold_str_title+$
    " mm/day for seasons in lower tercile of total rain - SILO 0.25",$
    /BLOCK,/CB_RIGHT,/NOLINES
rainfall_change=silo_mean_lower-silo_mean_middle
FOR i=1,silo_nlon-1,2 DO BEGIN
   FOR j=1,silo_nlat-1,2 DO BEGIN
      IF season_change(i,j) gt mylevs_ndays_chng(5) THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=4,SIZE=120
      IF season_change(i,j) gt mylevs_ndays_chng(4) and $
         season_change(i,j) le mylevs_ndays_chng(5) THEN $
            GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=4,SIZE=80
      IF season_change(i,j) gt mylevs_ndays_chng(3) and $
         season_change(i,j) le mylevs_ndays_chng(4) THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=4,SIZE=40
      IF season_change(i,j) gt mylevs_ndays_chng(2) and $
         season_change(i,j) le mylevs_ndays_chng(3) THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=8,SIZE=40
      IF season_change(i,j) gt mylevs_ndays_chng(1) and $
         season_change(i,j) le mylevs_ndays_chng(2) THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=8,SIZE=80 
      IF season_change(i,j) le mylevs_ndays_chng(1) THEN $
         GPLOT,X=silo_longitude(i),Y=silo_latitude(j),SYM=8,SIZE=120
   ENDFOR
ENDFOR
LEGEND,labels=labels_mean,SYM=sym,SIZE=size,LEGPOS=9,LENGTH=0
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/wet_days/terciles/qld_wet_days_terciles_skewness.nov-apr_amount_middle.'+$
       precip_threshold_str+'mm_thresh.silo025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_mean)+1,/REV;,white=[2]
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
LEVS,MANUAL=mylevs_mean
CON,FIELD=silo_mean_middle,X=silo_longitude,Y=silo_latitude,$
    TITLE="Mean rain on days with rain "+precip_threshold_str_title+" mm/day for seasons in middle tercile - SILO 0.25",$
    /BLOCK,/CB_RIGHT,/NOLINES
PSCLOSE,/NOVIEW

; Plots for skewness

;; psfile='/home/ss901165/idl/queensland/wet_days/terciles/qld_wet_days_terciles_skewness.nov-apr_skew_upper.'+$
;;        precip_threshold_str+'mm_thresh.silo025.ps'
;; PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;; CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_skew)+1,/REV;,white=[2]
;; MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
;; LEVS,MANUAL=mylevs_skew
;; CON,FIELD=silo_skew_upper,X=silo_longitude,Y=silo_latitude,$
;;     TITLE="Skewness in dist of events with rain "+precip_threshold_str_title+$
;;     " mm/day in seasons in upper tercile of total rain - SILO 0.25",$
;;     /BLOCK,/CB_RIGHT,/NOLINES
;; PSCLOSE,/NOVIEW

;; psfile='/home/ss901165/idl/queensland/wet_days/terciles/qld_wet_days_terciles_skewness.nov-apr_skew_upper-diff-middle.'+$
;;        precip_threshold_str+'mm_thresh.silo025.ps'
;; PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;; CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_skew)+1,/REV;,white=[2]
;; MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
;; LEVS,MANUAL=mylevs_diff_skew
;; CON,FIELD=silo_skew_upper-silo_skew_middle,X=silo_longitude,Y=silo_latitude,$
;;     TITLE="Diff in skew of dist of events with rain "+precip_threshold_str_title+$
;;     " mm/day between seasons in upper and middle terciles - SILO 0.25",$
;;     /BLOCK,/CB_RIGHT,/NOLINES
;; PSCLOSE,/NOVIEW

;; psfile='/home/ss901165/idl/queensland/wet_days/terciles/qld_wet_days_terciles_skewness.nov-apr_skew_lower.'+$
;;        precip_threshold_str+'mm_thresh.silo025.ps'
;; PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;; CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_skew)+1,/REV;,white=[2]
;; MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
;; LEVS,MANUAL=mylevs_skew
;; CON,FIELD=silo_skew_lower,X=silo_longitude,Y=silo_latitude,$
;;     TITLE="Skewness in dist of events with rain "+precip_threshold_str_title+$
;;     " mm/day in seasons in lower tercile of total rain - SILO 0.25",$
;;     /BLOCK,/CB_RIGHT,/NOLINES
;; PSCLOSE,/NOVIEW

;; psfile='/home/ss901165/idl/queensland/wet_days/terciles/qld_wet_days_terciles_skewness.nov-apr_skew_lower-diff-middle.'+$
;;        precip_threshold_str+'mm_thresh.silo025.ps'
;; PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;; CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_skew)+1,/REV;,white=[2]
;; MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
;; LEVS,MANUAL=mylevs_diff_skew
;; CON,FIELD=silo_skew_lower-silo_skew_middle,X=silo_longitude,Y=silo_latitude,$
;;     TITLE="Diff in skew of dist of events with rain "+precip_threshold_str_title+$
;;     " mm/day between seasons in lower and middle terciles - SILO 0.25",$
;;     /BLOCK,/CB_RIGHT,/NOLINES
;; PSCLOSE,/NOVIEW

;; psfile='/home/ss901165/idl/queensland/wet_days/terciles/qld_wet_days_terciles_skewness.nov-apr_skew_middle.'+$
;;        precip_threshold_str+'mm_thresh.silo025.ps'
;; PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;; CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_skew)+1,/REV;,white=[2]
;; MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
;; LEVS,MANUAL=mylevs_skew
;; CON,FIELD=silo_skew_middle,X=silo_longitude,Y=silo_latitude,$
;;     TITLE="Skewness in dist of events with rain "+precip_threshold_str_title+$
;;     " mm/day in seasons in middle tercile - SILO 0.25",$
;;     /BLOCK,/CB_RIGHT,/NOLINES
;; PSCLOSE,/NOVIEW

STOP

END





