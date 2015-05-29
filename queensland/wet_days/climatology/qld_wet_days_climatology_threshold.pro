PRO qld_wet_days_climatology_threshold

silo_indir='/home/ss901165/datasets_mango/SILO/one_quarter'
silo_annmean_file='/home/ss901165/datasets_mango/SILO/one_quarter/SILO_clim_precip.annual_mean.1900-2008.0.25x0.25.nc'
mask_infile='/home/ss901165/datasets/SILO/one_quarter_lsm.nc'

start_year=1900
stop_year=2008
n_years=(stop_year-start_year)+1

thresholds=[1,5,10,25,50,100]
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
   
   IF i eq 0  THEN BEGIN
      silo_longitude=OPEN_AND_EXTRACT(this_silo_file,'longitude')
      silo_latitude=OPEN_AND_EXTRACT(this_silo_file,'latitude')
      DEFINE_BOUNDARIES,box,silo_latitude,silo_longitude,silo_box_tx,/LIMIT
      silo_nlon=N_ELEMENTS(silo_longitude)
      silo_nlat=N_ELEMENTS(silo_latitude)
      
      ndays_exceeding_threshold=fltarr(silo_nlon,silo_nlat,n_thresholds)
      mean_precip_exceeding_threshold=fltarr(silo_nlon,silo_nlat,n_thresholds)
      
      ndays_exceeding_threshold_byseason=fltarr(silo_nlon,silo_nlat,n_seasons,n_thresholds)
      mean_precip_exceeding_threshold_byseason=fltarr(silo_nlon,silo_nlat,n_seasons,n_thresholds)

      annual_mean_silo=REFORM(OPEN_AND_EXTRACT(silo_annmean_file,'rain',$
                                               offset=[silo_box_tx(1),silo_box_tx(0)],$
                                               count=[silo_nlon,silo_nlat]))      
      monthly_mean_silo=fltarr(silo_nlon,silo_nlat,12)
      seasonal_total_silo=fltarr(silo_nlon,silo_nlat,n_seasons)
      m=0
      FOR j=0,n_seasons-1 DO BEGIN
         FOR k=0,N_ELEMENTS(months_in_seasons(*,j))-1 DO BEGIN
            silo_mmean_file=silo_indir+'/SILO_clim_precip.'+months_in_seasons(k,j)+'_mmean.1900-2008.0.25x0.25.nc'
            print,silo_mmean_file
            monthly_mean_silo(*,*,m)=REFORM(OPEN_AND_EXTRACT(silo_mmean_file,'rain',$
                                                             offset=[silo_box_tx(1),silo_box_tx(0),0],$
                                                             count=[silo_nlon,silo_nlat,1]))
            seasonal_total_silo(*,*,j)=seasonal_total_silo(*,*,j)+monthly_mean_silo(*,*,m)*$
                                       ndays_of_months_in_seasons_noleap(k,j)
            m=m+1
         ENDFOR
      ENDFOR
      
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
               ndays_exceeding_threshold(j,k,m)=$
                  ndays_exceeding_threshold(j,k,m)+N_ELEMENTS(where(temp ge thresholds(m)))
               mean_precip_exceeding_threshold(j,k,m)=$
                  mean_precip_exceeding_threshold(j,k,m)+TOTAL(temp[where(temp ge thresholds(m))])
            ENDIF
            FOR n=0,n_seasons-1 DO BEGIN
               FOR p=0,N_ELEMENTS(months_in_seasons(*,n))-1 DO BEGIN
                  temp=this_year_silo(j,k,$
                                       start_dates_of_months_in_seasons(p,n):$
                                       start_dates_of_months_in_seasons(p,n)+$
                                       ndays_of_months_in_seasons(p,n)-1)
                  IF TOTAL(where(temp ge thresholds(m))) ne -1 THEN BEGIN
                     ndays_exceeding_threshold_byseason(j,k,n,m)=$
                        ndays_exceeding_threshold_byseason(j,k,n,m)+N_ELEMENTS(where(temp ge thresholds(m)))
                     mean_precip_exceeding_threshold_byseason(j,k,n,m)=$
                        mean_precip_exceeding_threshold_byseason(j,k,n,m)+TOTAL(temp[where(temp ge thresholds(m))])
                  ENDIF
               ENDFOR
            ENDFOR
         ENDFOR
      ENDFOR
   ENDFOR 
ENDFOR

mean_precip_exceeding_threshold=mean_precip_exceeding_threshold/FLOAT(ndays_exceeding_threshold)
mean_precip_exceeding_threshold_byseason=mean_precip_exceeding_threshold_byseason/$
                                         FLOAT(ndays_exceeding_threshold_byseason)
ndays_exceeding_threshold[where(ndays_exceeding_threshold eq 0)]=!Values.F_NaN
ndays_exceeding_threshold_byseason[where(ndays_exceeding_threshold_byseason eq 0)]=!Values.F_NaN

fraction_of_annual_total=fltarr(silo_nlon,silo_nlat)
fraction_of_seasonal_total=fltarr(silo_nlon,silo_nlat,n_seasons)

mylevs=[1,2,4,8,12,16,24,32,40,56,72,90]
mylevs_season=[1,2,4,8,12,16,20,24,28,32,36,40,44,48,52,56]
mylevs_frac=['0.1','0.15','0.2','0.25','0.3','0.35','0.4','0.45','0.5',$
             '0.55','0.6','0.65','0.7','0.75','0.8','0.85','0.9','0.95']

FOR m=0,n_thresholds-1 DO BEGIN
   FOR j=0,silo_nlon-1 DO BEGIN
      FOR k=0,silo_nlat-1 DO BEGIN
         fraction_of_annual_total(j,k)=ndays_exceeding_threshold(j,k,m)/FLOAT(n_years)*$
                                         mean_precip_exceeding_threshold(j,k,m)/$
                                         (annual_mean_silo(j,k)*365)
         FOR n=0,n_seasons-1 DO BEGIN
            fraction_of_seasonal_total(j,k,n)=ndays_exceeding_threshold_byseason(j,k,n,m)/FLOAT(n_years)*$
                                              mean_precip_exceeding_threshold_byseason(j,k,n,m)/$
                                              seasonal_total_silo(j,k,n)
         ENDFOR
      ENDFOR
   ENDFOR

   this_threshold=thresholds(m)
   this_threshold_str=STRTRIM(STRING(thresholds(m)),1)
   psfile='/home/ss901165/idl/queensland/wet_days/climatology/qld_wet_days_climatology_threshold.annual_ndays_'+this_threshold_str+'mm_silo025.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[2]
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
   LEVS,MANUAL=mylevs
   toplot=REFORM(ndays_exceeding_threshold(*,*,m))
   toplot[where(mask eq 0)]=!Values.F_NaN
   CON,FIELD=toplot/FLOAT(n_years),X=silo_longitude,Y=silo_latitude,$
       TITLE="Number of days per year exceeding threshold of "+this_threshold_str+" mm/day - SILO (1900-2008)",/BLOCK,/CB_RIGHT,/NOLINES
   PSCLOSE,/NOVIEW

   mylevs_precip=[this_threshold,this_threshold*1.2,this_threshold*1.4,this_threshold*1.6,this_threshold*1.8,this_threshold*2,$
                  this_threshold*2.5,this_threshold*3,this_threshold*3.5,this_threshold*4,this_threshold*5]
   psfile='/home/ss901165/idl/queensland/wet_days/climatology/qld_wet_days_climatology_threshold.annual_amount_'+this_threshold_str+'mm_silo025.ps'
   
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_precip)+1,/REV,white=[2]
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
   mylevs_precip=STRMID(STRTRIM(STRING(mylevs_precip),1),0,4)
   LEVS,MANUAL=mylevs_precip
   toplot=REFORM(mean_precip_exceeding_threshold(*,*,m))
   toplot[where(mask eq 0)]=!Values.F_NaN
   CON,FIELD=toplot,X=silo_longitude,Y=silo_latitude,$
       TITLE="Mean rainfall (mm/day) on days exceeding "+this_threshold_str+" mm/day - SILO (1900-2008)",/BLOCK,/CB_RIGHT,/NOLINES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/wet_days/climatology/qld_wet_days_climatology_threshold.annual_fraction_'+this_threshold_str+'mm_silo025.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_frac)+1,/REV,white=[2]
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
   mylevs_precip=STRMID(STRTRIM(STRING(mylevs_precip),1),0,4)
   LEVS,MANUAL=mylevs_frac
   toplot=fraction_of_annual_total
   toplot[where(mask eq 0)]=!Values.F_NaN
   CON,FIELD=toplot,X=silo_longitude,Y=silo_latitude,$
       TITLE="Fraction of annual total rainfall falling on days exceeding "+this_threshold_str+" mm/day - SILO (1900-2008)",/BLOCK,/CB_RIGHT,/NOLINES
   PSCLOSE,/NOVIEW

   FOR n=0,n_seasons-1 DO BEGIN
      psfile='/home/ss901165/idl/queensland/wet_days/climatology/qld_wet_days_climatology_threshold.'+abbrev_for_seasons(n)+'_ndays_'+$
             this_threshold_str+'mm_silo025.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_season)+1,/REV,white=[2]
      MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
      LEVS,MANUAL=mylevs_season
      toplot=REFORM(ndays_exceeding_threshold_byseason(*,*,n,m))
      toplot[where(mask eq 0)]=!Values.F_NaN
      CON,FIELD=toplot/FLOAT(n_years),X=silo_longitude,Y=silo_latitude,$
          TITLE="Number of days per season exceeding threshold of "+this_threshold_str+" mm/day - "+abbrev_for_seasons(n)+$
          " - SILO (1900-2008)",/BLOCK,/CB_RIGHT,/NOLINES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/wet_days/climatology/qld_wet_days_climatology_threshold.'+abbrev_for_seasons(n)+'_amount_'+$
             this_threshold_str+'mm_silo025.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_precip)+1,/REV,white=[2]
      MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
      LEVS,MANUAL=mylevs_precip
      toplot=REFORM(mean_precip_exceeding_threshold_byseason(*,*,n,m))
      toplot[where(mask eq 0)]=!Values.F_NaN
      CON,FIELD=toplot,X=silo_longitude,Y=silo_latitude,$
          TITLE="Mean rainfall (mm/day) on days exceeding "+this_threshold_str+" mm/day - "+abbrev_for_seasons(n)+$
          " - SILO (1900-2008)",/BLOCK,/CB_RIGHT,/NOLINES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/wet_days/climatology/qld_wet_days_climatology_threshold.'+abbrev_for_seasons(n)+'_fraction_'+$
             this_threshold_str+'mm_silo025.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_frac)+1,/REV,white=[2]
      MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(2),LATMAX=box(0),/HIRES
      LEVS,MANUAL=mylevs_frac
      toplot=REFORM(fraction_of_seasonal_total(*,*,n))
      toplot[where(mask eq 0)]=!Values.F_NaN
      CON,FIELD=toplot,X=silo_longitude,Y=silo_latitude,$
          TITLE="Fraction of "+abbrev_for_seasons(n)+" total rainfall falling on days exceeding "+this_threshold_str+" mm/day"+$
          " - SILO (1900-2008)",/BLOCK,/CB_RIGHT,/NOLINES
      PSCLOSE,/NOVIEW

   ENDFOR
ENDFOR

STOP
END
