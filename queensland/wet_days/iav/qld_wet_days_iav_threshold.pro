PRO qld_wet_days_iav_threshold

silo_indir='/home/ss901165/datasets_mango/SILO/one_quarter'
silo_annmean_file=silo_indir+'/SILO_clim_precip.annual_mean.1900-2008.0.25x0.25.nc'
mask_infile='/home/ss901165/datasets/SILO/one_quarter_lsm.nc'

start_year=1900
stop_year=2008
n_years=(stop_year-start_year)+1

thresholds=[5,10,25,50,100]
n_thresholds=N_ELEMENTS(thresholds)

n_seasons=4
months_in_seasons=[['dec','jan','feb'],$
                   ['mar','apr','may'],$
                   ['jun','jul','aug'],$
                   ['sep','oct','nov']]
abbrev_for_seasons=['djf','mam','jja','son']
start_dates_of_months_in_seasons_noleap=[[334,0,31],$
                                         [59,90,120],$
                                         [151,181,212],$
                                         [243,273,304]]
start_dates_of_months_in_seasons_leap=[[335,0,31],$
                                       [60,91,121],$
                                       [152,182,213],$
                                       [244,274,305]]
ndays_of_months_in_seasons_noleap=[[31,31,28],$
                                   [31,30,31],$
                                   [30,31,31],$
                                   [30,31,30]]
ndays_of_months_in_seasons_leap=[[31,31,29],$
                                 [31,30,31],$
                                 [30,31,31],$
                                 [30,31,30]]

box=[-10,130,-40,154]
; Mask
mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)
mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                      offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                      count=[mask_nlon,mask_nlat,1,1]))

FOR i=0,n_years-1 DO BEGIN
   this_year=i+start_year
   this_year_str=STRTRIM(STRING(this_year),1)
   this_silo_file=silo_indir+'/by_year/SILO_daily_precip_'+this_year_str+'.0.25x0.25.nc'
   print,'Now computing number of days above thresholds for '+this_year_str

   IF i eq 0 THEN BEGIN
      silo_longitude=OPEN_AND_EXTRACT(this_silo_file,'longitude')
      silo_latitude=OPEN_AND_EXTRACT(this_silo_file,'latitude')
      DEFINE_BOUNDARIES,box,silo_latitude,silo_longitude,silo_box_tx,/LIMIT
      silo_nlon=N_ELEMENTS(silo_longitude)
      silo_nlat=N_ELEMENTS(silo_latitude)
      
      annual_total_silo=fltarr(silo_nlon,silo_nlat,n_years)
      seasonal_total_silo=fltarr(silo_nlon,silo_nlat,n_seasons,n_years)

      ndays_exceeding_threshold=fltarr(silo_nlon,silo_nlat,n_years,n_thresholds)
      precip_exceeding_threshold=fltarr(silo_nlon,silo_nlat,n_years,n_thresholds)
      frac_of_annual_total=fltarr(silo_nlon,silo_nlat,n_years,n_thresholds)
      
      ndays_exceeding_threshold_byseason=fltarr(silo_nlon,silo_nlat,n_years,n_seasons,n_thresholds)
      precip_exceeding_threshold_byseason=fltarr(silo_nlon,silo_nlat,n_years,n_seasons,n_thresholds)
      frac_of_seasonal_total=fltarr(silo_nlon,silo_nlat,n_years,n_seasons,n_thresholds)
   ENDIF

   this_year_ndays=DAYS_IN_YEAR(this_year)
   IF this_year_ndays eq 365 THEN BEGIN
      start_dates_of_months_in_seasons=start_dates_of_months_in_seasons_noleap
      ndays_of_months_in_seasons=ndays_of_months_in_seasons_noleap
   ENDIF ELSE BEGIN
      start_dates_of_months_in_seasons=start_dates_of_months_in_seasons_leap
      ndays_of_months_in_seasons=ndays_of_months_in_seasons_leap
   ENDELSE

   this_year_silo=REFORM(OPEN_AND_EXTRACT(this_silo_file,'rain',$
                                          offset=[silo_box_tx(1),silo_box_tx(0),0],$
                                          count=[silo_nlon,silo_nlat,this_year_ndays]))
   this_year_silo[where(this_year_silo ge 10000)]=!Values.F_NaN

   FOR m=0,n_thresholds-1 DO BEGIN
      FOR j=0,silo_nlon-1 DO BEGIN
         FOR k=0,silo_nlat-1 DO BEGIN
            IF TOTAL(where(this_year_silo(j,k,*) ge thresholds(m))) ne -1 THEN BEGIN
               temp=this_year_silo(j,k,*)
               ndays_exceeding_threshold(j,k,i,m)=N_ELEMENTS(where(temp ge thresholds(m)))
               precip_exceeding_threshold(j,k,i,m)=TOTAL(temp[where(temp ge thresholds(m))])
            ENDIF
            IF m eq 0 THEN $
               annual_total_silo(j,k,i)=TOTAL(this_year_silo(j,k,*))
            frac_of_annual_total(j,k,i,m)=precip_exceeding_threshold(j,k,i,m)/annual_total_silo(j,k,i)
            FOR n=0,n_seasons-1 DO BEGIN
               FOR p=0,N_ELEMENTS(months_in_seasons(*,n))-1 DO BEGIN
                  temp=this_year_silo(j,k,$
                                       start_dates_of_months_in_seasons(p,n):$
                                       start_dates_of_months_in_seasons(p,n)+$
                                       ndays_of_months_in_seasons(p,n)-1)
                  IF m eq 0 THEN $
                     seasonal_total_silo(j,k,n,i)=seasonal_total_silo(j,k,n,i)+TOTAL(temp)
                  IF TOTAL(where(temp ge thresholds(m))) ne -1 THEN BEGIN
                     ndays_exceeding_threshold_byseason(j,k,i,n,m)=$
                        ndays_exceeding_threshold_byseason(j,k,i,n,m)+N_ELEMENTS(where(temp ge thresholds(m)))
                     precip_exceeding_threshold_byseason(j,k,i,n,m)=$
                        precip_exceeding_threshold_byseason(j,k,i,n,m)+TOTAL(temp[where(temp ge thresholds(m))])
                  ENDIF
               ENDFOR
               frac_of_seasonal_total(j,k,i,n,m)=precip_exceeding_threshold_byseason(j,k,i,n,m)/$
                                                 seasonal_total_silo(j,k,n,i)
            ENDFOR
         ENDFOR
      ENDFOR
   ENDFOR 
ENDFOR

mean_precip_exceeding_threshold=precip_exceeding_threshold/FLOAT(ndays_exceeding_threshold)
mean_precip_exceeding_threshold_byseason=$
   precip_exceeding_threshold_byseason/FLOAT(ndays_exceeding_threshold_byseason)

iav_ndays_exceeding_threshold=fltarr(silo_nlon,silo_nlat,n_thresholds)
iav_ndays_exceeding_threshold_byseason=fltarr(silo_nlon,silo_nlat,n_seasons,n_thresholds)
iav_mean_precip_exceeding_threshold=fltarr(silo_nlon,silo_nlat,n_thresholds)
iav_mean_precip_exceeding_threshold_byseason=fltarr(silo_nlon,silo_nlat,n_seasons,n_thresholds)
iav_frac_of_annual_total=fltarr(silo_nlon,silo_nlat,n_thresholds)
iav_frac_of_seasonal_total=fltarr(silo_nlon,silo_nlat,n_seasons,n_thresholds)
FOR j=0,silo_nlon-1 DO BEGIN
   FOR k=0,silo_nlat-1 DO BEGIN
      FOR m=0,n_thresholds-1 DO BEGIN
         mean_ndays=MEAN(ndays_exceeding_threshold(j,k,*,m))
         IF mean_ndays le 1 or FINITE(mean_ndays) eq 0 THEN BEGIN
            iav_ndays_exceeding_threshold(j,k,m)=!Values.F_NaN
            iav_mean_precip_exceeding_threshold(j,k,m)=!Values.F_NaN
            iav_frac_of_annual_total(j,k,m)=!Values.F_NaN
         ENDIF ELSE BEGIN
            iav_ndays_exceeding_threshold(j,k,m)=STDDEV(ndays_exceeding_threshold(j,k,*,m),/NaN)
            iav_frac_of_annual_total(j,k,m)=STDDEV(frac_of_annual_total(j,k,*,m),/NaN)
            IF N_ELEMENTS(where(FINITE($
               mean_precip_exceeding_threshold(j,k,*,m)) eq 1)) ge 2 THEN BEGIN
               iav_mean_precip_exceeding_threshold(j,k,m)=STDDEV(mean_precip_exceeding_threshold(j,k,*,m),/NaN)               
            ENDIF ELSE $
               iav_mean_precip_exceeding_threshold(j,k,m)=!Values.F_NaN
         ENDELSE
         FOR n=0,n_seasons-1 DO BEGIN
            mean_ndays=MEAN(ndays_exceeding_threshold_byseason(j,k,*,n,m))
            IF mean_ndays le 1 or FINITE(mean_ndays) eq 0 THEN BEGIN
               iav_ndays_exceeding_threshold_byseason(j,k,n,m)=!Values.F_NaN
               iav_mean_precip_exceeding_threshold_byseason(j,k,n,m)=!Values.F_NaN
               iav_frac_of_seasonal_total(j,k,n,m)=!Values.F_NaN
            ENDIF ELSE BEGIN
               iav_ndays_exceeding_threshold_byseason(j,k,n,m)=$
                  STDDEV(ndays_exceeding_threshold_byseason(j,k,*,n,m),/NaN)
               iav_frac_of_seasonal_total(j,k,n,m)=STDDEV(frac_of_seasonal_total(j,k,*,n,m),/NaN)
               IF N_ELEMENTS(where(FINITE($
                  mean_precip_exceeding_threshold_byseason(j,k,*,n,m)) eq 1)) ge 2 THEN BEGIN
                  iav_mean_precip_exceeding_threshold_byseason(j,k,n,m)=$
                     STDDEV(mean_precip_exceeding_threshold_byseason(j,k,*,n,m),/NaN)
               ENDIF ELSE $
                  iav_mean_precip_exceeding_threshold_byseason(j,k,n,m)=!Values.F_NaN
            ENDELSE
         ENDFOR
      ENDFOR
   ENDFOR
ENDFOR

;iav_ndays_exceeding_threshold[where(iav_ndays_exceeding_threshold eq 0)]=!Values.F_NaN
;iav_mean_precip_exceeding_threshold[where(iav_mean_precip_exceeding_threshold eq 0)]=!Values.F_NaN

iav_ndays_exceeding_threshold_scaled=fltarr(silo_nlon,silo_nlat,n_thresholds)
iav_ndays_exceeding_threshold_byseason_scaled=fltarr(silo_nlon,silo_nlat,n_seasons,n_thresholds)
iav_mean_precip_exceeding_threshold_scaled=fltarr(silo_nlon,silo_nlat,n_thresholds)
iav_mean_precip_exceeding_threshold_byseason_scaled=fltarr(silo_nlon,silo_nlat,n_seasons,n_thresholds)
iav_frac_of_annual_total_scaled=fltarr(silo_nlon,silo_nlat,n_thresholds)
iav_frac_of_seasonal_total_scaled=fltarr(silo_nlon,silo_nlat,n_seasons,n_thresholds)

FOR j=0,silo_nlon-1 DO BEGIN
   FOR k=0,silo_nlat-1 DO BEGIN
      FOR m=0,n_thresholds-1 DO BEGIN         
         iav_ndays_exceeding_threshold_scaled(j,k,m)=iav_ndays_exceeding_threshold(j,k,m)/$
            FLOAT(MEAN(ndays_exceeding_threshold(j,k,*,m),/NaN))
         iav_mean_precip_exceeding_threshold_scaled(j,k,m)=iav_mean_precip_exceeding_threshold(j,k,m)/$
            FLOAT(MEAN(mean_precip_exceeding_threshold(j,k,*,m),/NaN))
         iav_frac_of_annual_total_scaled(j,k,m)=iav_frac_of_annual_total(j,k,m)/FLOAT(MEAN(frac_of_annual_total(j,k,*,m),/NaN))
         FOR n=0,n_seasons-1 DO BEGIN
            iav_ndays_exceeding_threshold_byseason_scaled(j,k,n,m)=iav_ndays_exceeding_threshold_byseason(j,k,n,m)/$
               FLOAT(MEAN(ndays_exceeding_threshold_byseason(j,k,*,n,m),/NaN))
            iav_mean_precip_exceeding_threshold_byseason_scaled(j,k,n,m)=$
               iav_mean_precip_exceeding_threshold_byseason(j,k,n,m)/FLOAT(MEAN(mean_precip_exceeding_threshold_byseason(j,k,*,n,m),/NaN))
            iav_frac_of_seasonal_total_scaled(j,k,n,m)=iav_frac_of_seasonal_total(j,k,n,m)/$
               FLOAT(MEAN(frac_of_seasonal_total(j,k,*,n,m),/NaN))
         ENDFOR
      ENDFOR
   ENDFOR
ENDFOR

mylevs=[1,2,3,4,6,8,10,12,14,16,20,24,28]
mylevs_scaled=['0.1','0.2','0.3','0.4','0.5','0.6','0.7','0.8','0.9','1.0','1.1','1.2']
mylevs_frac=['0.02','0.04','0.06','0.08','0.10','0.12','0.14','0.16','0.18','0.20','0.22','0.24']

FOR m=0,n_thresholds-1 DO BEGIN
   this_threshold=thresholds(m)
   this_threshold_str=STRTRIM(STRING(this_threshold),1)

   mylevs_amount=[this_threshold*0.1,this_threshold*0.2,this_threshold*0.3,this_threshold*0.4,$
                  this_threshold*0.5,this_threshold*0.6,this_threshold*0.7,this_threshold*0.8,$
                  this_threshold*1.0,this_threshold*1.2,this_threshold*1.4,this_threshold*1.6,$
                  this_threshold*1.8,this_threshold*2.0]
   mylevs_amount=STRMID(STRTRIM(STRING(mylevs_amount),1),0,4)

   psfile='/home/ss901165/idl/queensland/wet_days/iav/qld_wet_days_iav_threshold.annual_ndays_'+this_threshold_str+'mm_silo025.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV;,white=[2]
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
   LEVS,MANUAL=mylevs
   toplot=REFORM(iav_ndays_exceeding_threshold(*,*,m))
   toplot[where(mask eq 0)]=!Values.F_NaN
   CON,FIELD=toplot,X=silo_longitude,Y=silo_latitude,$
       TITLE="IAV in days per year over "+this_threshold_str+" mm/day - SILO (1900-2008)",/BLOCK,/CB_RIGHT,/NOLINES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/wet_days/iav/qld_wet_days_iav_threshold.annual_ndays_'+this_threshold_str+'mm_scaled_silo025.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_scaled)+1,/REV ;,white=[2]
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
   LEVS,MANUAL=mylevs_scaled
   toplot=REFORM(iav_ndays_exceeding_threshold_scaled(*,*,m))
   toplot[where(mask eq 0)]=!Values.F_NaN
   CON,FIELD=toplot,X=silo_longitude,Y=silo_latitude,$
       TITLE="IAV in days per year over "+this_threshold_str+$
       " mm/day - SILO (1900-2008) - scaled by mean",/BLOCK,/CB_RIGHT,/NOLINES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/wet_days/iav/qld_wet_days_iav_threshold.annual_amount_'+this_threshold_str+'mm_silo025.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_amount)+1,/REV;,white=[2]
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
   LEVS,MANUAL=mylevs_amount
   toplot=REFORM(iav_mean_precip_exceeding_threshold(*,*,m))
   toplot[where(mask eq 0)]=!Values.F_NaN
   CON,FIELD=toplot,X=silo_longitude,Y=silo_latitude,$
       TITLE="IAV in mean precip over "+this_threshold_str+$
       " mm/day - SILO (1900-2008)",/BLOCK,/CB_RIGHT,/NOLINES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/wet_days/iav/qld_wet_days_iav_threshold.annual_amount_'+this_threshold_str+'mm_scaled_silo025.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_scaled)+1,/REV;,white=[2]
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
   LEVS,MANUAL=mylevs_scaled
   toplot=REFORM(iav_mean_precip_exceeding_threshold_scaled(*,*,m))
   toplot[where(mask eq 0)]=!Values.F_NaN
   CON,FIELD=toplot,X=silo_longitude,Y=silo_latitude,$
       TITLE="IAV in mean precip over "+this_threshold_str+" mm/day - SILO (1900-2008) - scaled by mean",/BLOCK,/CB_RIGHT,/NOLINES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/wet_days/iav/qld_wet_days_iav_threshold.annual_fraction_'+this_threshold_str+'mm_silo025.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_frac)+1,/REV;,white=[2]
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
   LEVS,MANUAL=mylevs_frac
   toplot=REFORM(iav_frac_of_annual_total(*,*,m))
   toplot[where(mask eq 0)]=!Values.F_NaN
   CON,FIELD=toplot,X=silo_longitude,Y=silo_latitude,$
       TITLE="IAV in frac of annual total from days over "+this_threshold_str+" mm/day - SILO (1900-2008)",/BLOCK,/CB_RIGHT,/NOLINES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/wet_days/iav/qld_wet_days_iav_threshold.annual_fraction_'+this_threshold_str+'mm_scaled_silo025.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_scaled)+1,/REV;,white=[2]
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
   LEVS,MANUAL=mylevs_scaled
   toplot=REFORM(iav_frac_of_annual_total_scaled(*,*,m))
   toplot[where(mask eq 0)]=!Values.F_NaN
   CON,FIELD=toplot,X=silo_longitude,Y=silo_latitude,$
       TITLE="IAV in frac of annual total from days over "+this_threshold_str+" mm/day - SILO (1900-2008) - scaled by mean",/BLOCK,/CB_RIGHT,/NOLINES
   PSCLOSE,/NOVIEW

   FOR n=0,n_seasons-1 DO BEGIN
      psfile='/home/ss901165/idl/queensland/wet_days/iav/qld_wet_days_iav_threshold.'+abbrev_for_seasons(n)+$
             '_ndays_'+this_threshold_str+'mm_silo025.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV;,white=[2]
      MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
      LEVS,MANUAL=mylevs
      toplot=REFORM(iav_ndays_exceeding_threshold_byseason(*,*,n,m))
      toplot[where(mask eq 0)]=!Values.F_NaN
      CON,FIELD=toplot,X=silo_longitude,Y=silo_latitude,$
          TITLE="IAV in days per year over "+this_threshold_str+" mm/day - "+abbrev_for_seasons(n)+$
          " - SILO (1900-2008)",/BLOCK,/CB_RIGHT,/NOLINES
      PSCLOSE,/NOVIEW
      
      psfile='/home/ss901165/idl/queensland/wet_days/iav/qld_wet_days_iav_threshold.'+abbrev_for_seasons(n)+$
             '_ndays_'+this_threshold_str+'mm_scaled_silo025.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_scaled)+1,/REV;,white=[2]
      MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
      LEVS,MANUAL=mylevs_scaled
      toplot=REFORM(iav_ndays_exceeding_threshold_byseason_scaled(*,*,n,m))
      toplot[where(mask eq 0)]=!Values.F_NaN
      CON,FIELD=toplot,X=silo_longitude,Y=silo_latitude,$
          TITLE="IAV in days per year over "+this_threshold_str+" mm/day - "+abbrev_for_seasons(n)+$
          " - SILO (1900-2008) - scaled by mean",/BLOCK,/CB_RIGHT,/NOLINES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/wet_days/iav/qld_wet_days_iav_threshold.'+abbrev_for_seasons(n)+$
             '_amount_'+this_threshold_str+'mm_silo025.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_amount)+1,/REV;,white=[2]
      MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
      LEVS,MANUAL=mylevs_amount
      toplot=REFORM(iav_mean_precip_exceeding_threshold_byseason(*,*,n,m))
      toplot[where(mask eq 0)]=!Values.F_NaN
      CON,FIELD=toplot,X=silo_longitude,Y=silo_latitude,$
          TITLE="IAV in mean precip over "+this_threshold_str+" mm/day - "+abbrev_for_seasons(n)+$
          " - SILO (1900-2008)",/BLOCK,/CB_RIGHT,/NOLINES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/wet_days/iav/qld_wet_days_iav_threshold.'+abbrev_for_seasons(n)+$
             '_amount_'+this_threshold_str+'mm_scaled_silo025.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_scaled)+1,/REV;,white=[2]
      MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
      LEVS,MANUAL=mylevs_scaled
      toplot=REFORM(iav_mean_precip_exceeding_threshold_byseason_scaled(*,*,n,m))
      toplot[where(mask eq 0)]=!Values.F_NaN      
      CON,FIELD=toplot,X=silo_longitude,Y=silo_latitude,$
          TITLE="IAV in mean precip over "+this_threshold_str+" mm/day - "+abbrev_for_seasons(n)+$
          " - SILO (1900-2008) - scaled by mean",/BLOCK,/CB_RIGHT,/NOLINES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/wet_days/iav/qld_wet_days_iav_threshold.'+abbrev_for_seasons(n)+$
             '_fraction_'+this_threshold_str+'mm_silo025.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_frac)+1,/REV ;,white=[2]
      MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
      LEVS,MANUAL=mylevs_frac
      toplot=REFORM(iav_frac_of_seasonal_total(*,*,n,m))
      toplot[where(mask eq 0)]=!Values.F_NaN
      CON,FIELD=toplot,X=silo_longitude,Y=silo_latitude,$
          TITLE="IAV in frac of "+abbrev_for_seasons(n)+"total from days over "+$
          this_threshold_str+" mm/day - SILO (1900-2008)",/BLOCK,/CB_RIGHT,/NOLINES
      PSCLOSE,/NOVIEW
      
      psfile='/home/ss901165/idl/queensland/wet_days/iav/qld_wet_days_iav_threshold.'+abbrev_for_seasons(n)+$
             '_fraction_'+this_threshold_str+'mm_scaled_silo025.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_scaled)+1,/REV ;,white=[2]
      MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
      LEVS,MANUAL=mylevs_scaled
      toplot=REFORM(iav_frac_of_seasonal_total_scaled(*,*,n,m))
      toplot[where(mask eq 0)]=!Values.F_NaN
      CON,FIELD=toplot,X=silo_longitude,Y=silo_latitude,$
          TITLE="IAV in frac of "+abbrev_for_seasons(n)+"total from days over "+$
          this_threshold_str+" mm/day - SILO (1900-2008) - scaled by mean",/BLOCK,/CB_RIGHT,/NOLINES
      PSCLOSE,/NOVIEW
   ENDFOR      
ENDFOR

; Output useful data to a file

outfile='/home/ss901165/datasets_mango/SILO/one_quarter/SILO_precip.wet_days_threshold.1900-2008.0.25x0.25.nc'
id=NCDF_CREATE(outfile,/CLOBBER)
lonid=NCDF_DIMDEF(id,'longitude',silo_nlon)
latid=NCDF_DIMDEF(id,'latitude',silo_nlat)
yearid=NCDF_DIMDEF(id,'year',n_years)
seasid=NCDF_DIMDEF(id,'season',n_seasons)
threshid=NCDF_DIMDEF(id,'threshold',/UNLIMITED)

varids=intarr(30)
varids(0)=NCDF_VARDEF(id,'longitude',[lonid],/FLOAT)
varids(1)=NCDF_VARDEF(id,'latitude',[latid],/FLOAT)
varids(2)=NCDF_VARDEF(id,'year',[yearid],/FLOAT)
varids(3)=NCDF_VARDEF(id,'season',[seasid],/FLOAT)
varids(4)=NCDF_VARDEF(id,'threshold',[threshid],/FLOAT)

varids(5)=NCDF_VARDEF(id,'ndays_over',[lonid,latid,yearid,threshid],/SHORT)
varids(6)=NCDF_VARDEF(id,'ndays_over_seas',[lonid,latid,yearid,seasid,threshid],/SHORT)
varids(7)=NCDF_VARDEF(id,'mean_over',[lonid,latid,yearid,threshid],/FLOAT)
varids(8)=NCDF_VARDEF(id,'mean_over_seas',[lonid,latid,yearid,seasid,threshid],/FLOAT)
varids(9)=NCDF_VARDEF(id,'frac_of_annual',[lonid,latid,yearid,threshid],/FLOAT)
varids(10)=NCDF_VARDEF(id,'frac_of_seasonal',[lonid,latid,yearid,seasid,threshid],/FLOAT)
varids(11)=NCDF_VARDEF(id,'iav_ndays_over',[lonid,latid,threshid],/FLOAT)
varids(12)=NCDF_VARDEF(id,'iav_ndays_over_scaled',[lonid,latid,threshid],/FLOAT)
varids(13)=NCDF_VARDEF(id,'iav_ndays_over_seas',[lonid,latid,seasid,threshid],/FLOAT)
varids(14)=NCDF_VARDEF(id,'iav_ndays_over_seas_scaled',[lonid,latid,seasid,threshid],/FLOAT)
varids(15)=NCDF_VARDEF(id,'iav_mean_over',[lonid,latid,threshid],/FLOAT)
varids(16)=NCDF_VARDEF(id,'iav_mean_over_scaled',[lonid,latid,threshid],/FLOAT)
varids(17)=NCDF_VARDEF(id,'iav_mean_over_seas',[lonid,latid,seasid,threshid],/FLOAT)
varids(18)=NCDF_VARDEF(id,'iav_mean_over_seas_scaled',[lonid,latid,seasid,threshid],/FLOAT)
varids(19)=NCDF_VARDEF(id,'iav_frac_of_annual',[lonid,latid,threshid],/FLOAT)
varids(20)=NCDF_VARDEF(id,'iav_frac_of_annual_scaled',[lonid,latid,threshid],/FLOAT)
varids(21)=NCDF_VARDEF(id,'iav_frac_of_seasonal',[lonid,latid,seasid,threshid],/FLOAT)
varids(22)=NCDF_VARDEF(id,'iav_frac_of_seasonal_scaled',[lonid,latid,seasid,threshid],/FLOAT)
;varids(15)=NCDF_VARDEF(id,'iav_frac_of_annual',[lonid,latid,yearid,threshid],/FLOAT)
;varids(16)=NCDF_VARDEF(id,'iav_frac_of_seasonal',[lonid,latid,yearid,seasid,threshid],/FLOAT)

NCDF_CONTROL,id,/ENDEF

NCDF_VARPUT,id,varids(0),silo_longitude
NCDF_VARPUT,id,varids(1),silo_latitude
NCDF_VARPUT,id,varids(2),indgen(n_years)+start_year
NCDF_VARPUT,id,varids(3),indgen(n_seasons)
NCDF_VARPUT,id,varids(4),thresholds

NCDF_VARPUT,id,varids(5),ndays_exceeding_threshold
NCDF_VARPUT,id,varids(6),ndays_exceeding_threshold_byseason
NCDF_VARPUT,id,varids(7),mean_precip_exceeding_threshold
NCDF_VARPUT,id,varids(8),mean_precip_exceeding_threshold_byseason
NCDF_VARPUT,id,varids(9),frac_of_annual_total
NCDF_VARPUT,id,varids(10),frac_of_seasonal_total
NCDF_VARPUT,id,varids(11),iav_ndays_exceeding_threshold
NCDF_VARPUT,id,varids(12),iav_ndays_exceeding_threshold_scaled
NCDF_VARPUT,id,varids(13),iav_ndays_exceeding_threshold_byseason
NCDF_VARPUT,id,varids(14),iav_ndays_exceeding_threshold_byseason_scaled
NCDF_VARPUT,id,varids(15),iav_mean_precip_exceeding_threshold
NCDF_VARPUT,id,varids(16),iav_mean_precip_exceeding_threshold_scaled
NCDF_VARPUT,id,varids(17),iav_mean_precip_exceeding_threshold_byseason
NCDF_VARPUT,id,varids(18),iav_mean_precip_exceeding_threshold_byseason_scaled
NCDF_VARPUT,id,varids(19),iav_frac_of_annual_total
NCDF_VARPUT,id,varids(20),iav_frac_of_annual_total_scaled
NCDF_VARPUT,id,varids(21),iav_frac_of_seasonal_total
NCDF_VARPUT,id,varids(22),iav_frac_of_seasonal_total_scaled

NCDF_CLOSE,id

STOP

END
