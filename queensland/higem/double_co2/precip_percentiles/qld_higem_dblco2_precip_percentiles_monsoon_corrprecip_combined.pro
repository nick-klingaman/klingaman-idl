PRO qld_higem_dblco2_precip_percentiles_monsoon_corrprecip_combined

onset_threshold=15
retreat_threshold=85
pct_name='1585'

monbox=[-30,112,-10,154]
box=monbox
region_name='monsoon'
start_date=120
stop_date=359
max_years=149
onset_frac=onset_threshold/100.
retreat_frac=retreat_threshold/100.
n_days=(stop_date-start_date+1)

; 5/95 for all
;mean_onset_levs=[148,156,164,172,180,188,196,204,212,220,228,236,244,252,260]+120
;onset_levs=[120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280]+120
;retreat_levs=[275,280,285,290,295,300,305,310,315,320,325,330,335,340,345,350,355,360]+120
;mean_retreat_levs=[300,305,310,315,320,325,330,335,340,345,350,355,360]+120
;diff_levs=['-17','-15','-13','-11','-9','-7','-5','-3','-1','1','3','5','7','9','11','13','15','17']

; 10/90 for all
;mean_onset_levs=[164,172,180,188,196,204,212,220,228,236,244,252,260,268]+120
;onset_levs=[120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280]+120
;retreat_levs=[275,280,285,290,295,300,305,310,315,320,325,330,335,340,345,350,355,360]+120
;mean_retreat_levs=[290,295,300,305,310,315,320,325,330,335,340,345,350,355]+120
;diff_levs=['-17','-15','-13','-11','-9','-7','-5','-3','-1','1','3','5','7','9','11','13','15','17']

; 15/85 for all
regress_levels=['-15','-13','-11','-9','-7','-5','-3','-1','1','3','5','7','9','11','13','15']
n_regress_levs=N_ELEMENTS(regress_levels)

monthly_regress_levels=['-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5']
n_monthly_levs=N_ELEMENTS(monthly_regress_levels)

totalprecip_regress_levels=['-0.09','-0.03','0.03','0.09','0.15','0.21','0.27','0.33','0.39','0.45','0.51','0.57','0.63']
n_totalprecip_levs=N_ELEMENTS(totalprecip_regress_levels)

fracprecip_regress_levels=['-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07',$
                           '0.09','0.11','0.13','0.15']

mask_infile='/home/ss901165/um_output/mask_n144_higam.nc'
mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)

mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                             offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                             count=[mask_nlon,mask_nlat,1,1]))

months_regress=[9,10,11,12,13,14,15,16]
months_names=['sep','oct','nov','dec','jan','feb','mar','apr']
n_months=N_ELEMENTS(months_regress)

n_models=3
FOR i=0,n_models-1 DO BEGIN
   CASE i OF
      2 : BEGIN
         infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.may-apr_dmeans.h9-w8.precip.global_domain.nc'         
         monthly_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.jan-dec_mmeans.h9-w8.precip.global_domain.nc'
         var_name='precip'
         n_years=140
         model_name='higem_ctl'
         sig_level=0.178
         multiplier=86400.
         mask_rev=0
      END
      1 : BEGIN
         infile='/home/ss901165/datasets/SILO/n144/SILO.may-apr_dmeans.1900-2010.precip.n144.nc'
         monthly_infile='/home/ss901165/datasets/SILO/n144/SILO.jan-dec_mmeans.1900-2010.precip.n144.nc'
         var_name='rain'
         n_years=110
         model_name='silo_n144'
         sig_level=0.118
         multiplier=1.
         mask_rev=1
      END
      0 : BEGIN
         infile='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eafee_eadwu.may-apr_dmeans.m9-s0.precip.global_domain.nc'
         monthly_infile='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eafee_eadwu.jan-dec_mmeans.m9-s0.precip.global_domain.nc'
         var_name='precip'
         n_years=50
         model_name='higem_2xco2'
         sig_level=0.249
         multiplier=86400.
         mask_rev=0
      END
   ENDCASE
   
   latitude=OPEN_AND_EXTRACT(infile,'latitude')
   longitude=OPEN_AND_EXTRACT(infile,'longitude')
   DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
   n_lat=N_ELEMENTS(latitude)
   n_lon=N_ELEMENTS(longitude)
   
   daily_precip=OPEN_AND_EXTRACT(infile,var_name,offset=[box_tx(1),box_tx(0),start_date,0],$
                                 count=[n_lon,n_lat,n_days,n_years])*multiplier
   IF TOTAL(where(daily_precip gt 1e10)) gt 0 THEN $
      daily_precip[where(daily_precip gt 1e10)]=!Values.F_NaN

   monthly_precip=OPEN_AND_EXTRACT(monthly_infile,var_name,offset=[box_tx(1),box_tx(0),0,0],$
                                   count=[n_lon,n_lat,12,n_years+1])*multiplier*30.

   onset_dates=fltarr(n_lon,n_lat,n_years)
   retreat_dates=fltarr(n_lon,n_lat,n_years)
   onset_corrprecip=fltarr(n_lon,n_lat)
   retreat_corrprecip=fltarr(n_lon,n_lat)
   onset_regressprecip=fltarr(n_lon,n_lat)
   retreat_regressprecip=fltarr(n_lon,n_lat)
   length_corrprecip=fltarr(n_lon,n_lat)
   length_regressprecip=fltarr(n_lon,n_lat)

   onset_monthly_corr=fltarr(n_months,n_lon,n_lat)
   retreat_monthly_corr=fltarr(n_months,n_lon,n_lat)
   length_monthly_corr=fltarr(n_months,n_lon,n_lat)
   onset_monthly_regress=fltarr(n_months,n_lon,n_lat)
   retreat_monthly_regress=fltarr(n_months,n_lon,n_lat)
   length_monthly_regress=fltarr(n_months,n_lon,n_lat)

   totalprecip_monthly_corr=fltarr(n_months,n_lon,n_lat)
   totalprecip_monthly_regress=fltarr(n_months,n_lon,n_lat)
   fracprecip_monthly_corr=fltarr(n_months,n_lon,n_lat)
   fracprecip_monthly_regress=fltarr(n_months,n_lon,n_lat)

   this_mask=fltarr(n_lon,n_lat)
   IF mask_rev eq 1 THEN BEGIN
      FOR j=0,n_lat-1 DO $
         this_mask(*,j)=mask(*,n_lat-j-1)
   ENDIF ELSE $
      this_mask=mask
  
   FOR j=0,n_lon-1 DO BEGIN
      FOR k=0,n_lat-1 DO BEGIN
         IF this_mask(j,k) eq 1 THEN BEGIN
            season_total=fltarr(n_years)
            cumulative=fltarr(n_days,n_years)
            FOR m=0,n_years-1 DO BEGIN
               thispt_thisyear_precip=REFORM(daily_precip(j,k,*,m))
               season_total(m)=TOTAL(thispt_thisyear_precip)
                                ;cumulative=fltarr(n_days)
               FOR n=0,n_days-1 DO $
                  cumulative(n,m)=TOTAL(thispt_thisyear_precip(0:n))/season_total(m)
               onset_dates(j,k,m)=NEAREST(REFORM(cumulative(*,m)),onset_frac)+start_date+120
               retreat_dates(j,k,m)=NEAREST(REFORM(cumulative(*,m)),retreat_frac)+start_date+120
            ENDFOR
            onset_corrprecip(j,k)=CORRELATE(REFORM(onset_dates(j,k,0:n_years-1)),season_total(0:n_years-1))*1.2
            retreat_corrprecip(j,k)=CORRELATE(REFORM(retreat_dates(j,k,0:n_years-1)),season_total(0:n_years-1))
            onset_regressprecip(j,k)=REGRESS(REFORM(onset_dates(j,k,0:n_years-1)),season_total(0:n_years-1))
            retreat_regressprecip(j,k)=REGRESS(REFORM(retreat_dates(j,k,0:n_years-1)),season_total(0:n_years-1))
            length_corrprecip(j,k)=CORRELATE(REFORM(retreat_dates(j,k,0:n_years-1))-REFORM(onset_dates(j,k,0:n_years-1)),$
                                             season_total(0:n_years-1))
            length_regressprecip(j,k)=REGRESS(REFORM(retreat_dates(j,k,0:n_years-1))-REFORM(onset_dates(j,k,0:n_years-1)),$
                                              season_total(0:n_years-1))
            FOR m=0,n_months-1 DO BEGIN
               IF months_regress(m) gt 12 THEN BEGIN
                  this_month=months_regress(m)-12
                  year_offset=1
               ENDIF ELSE BEGIN
                  this_month=months_regress(m)
                  year_offset=0
               ENDELSE
               this_month_ts=REFORM(monthly_precip(j,k,this_month-1,year_offset:year_offset+n_years-1))
               onset_monthly_corr(m,j,k)=CORRELATE(REFORM(onset_dates(j,k,*)),this_month_ts)
               onset_monthly_regress(m,j,k)=REGRESS(REFORM(onset_dates(j,k,*)),this_month_ts)
               retreat_monthly_corr(m,j,k)=CORRELATE(REFORM(retreat_dates(j,k,*)),this_month_ts)
               retreat_monthly_regress(m,j,k)=REGRESS(REFORM(retreat_dates(j,k,*)),this_month_ts)
               length_monthly_corr(m,j,k)=CORRELATE(REFORM(retreat_dates(j,k,*)-onset_dates(j,k,*)),this_month_ts)
               length_monthly_regress(m,j,k)=REGRESS(REFORM(retreat_dates(j,k,*)-onset_dates(j,k,*)),this_month_ts)
               totalprecip_monthly_corr(m,j,k)=CORRELATE(season_total,this_month_ts)
               totalprecip_monthly_regress(m,j,k)=REGRESS(season_total,this_month_ts)
               fracprecip_monthly_corr(m,j,k)=CORRELATE(season_total,this_month_ts/season_total)*1.4
               fracprecip_monthly_regress(m,j,k)=REGRESS(season_total,this_month_ts/season_total)*120.
            ENDFOR         
         ENDIF ELSE BEGIN
            onset_corrprecip(j,k)=!Values.F_NaN
            retreat_corrprecip(j,k)=!Values.F_NaN
            onset_regressprecip(j,k)=!Values.F_NaN
            retreat_regressprecip(j,k)=!Values.F_NaN            
            onset_monthly_corr(*,j,k)=!Values.F_NaN
            onset_monthly_regress(*,j,k)=!Values.F_NaN            
            retreat_monthly_corr(*,j,k)=!Values.F_NaN
            retreat_monthly_regress(*,j,k)=!Values.F_NaN
            length_monthly_corr(*,j,k)=!Values.F_NaN
            length_monthly_regress(*,j,k)=!Values.F_NaN
            totalprecip_monthly_corr(*,j,k)=!Values.F_NaN
            totalprecip_monthly_regress(*,j,k)=!Values.F_NaN
         ENDELSE
      ENDFOR
   ENDFOR
   
   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/'+$
          'qld_higem_dblco2_precip_percentiles_monsoon_corrprecip_combined.onset_corrprecip.'+model_name+'.'$
          +region_name+'_region.'+pct_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=1000,TFONT=3,$
          TCHARSIZE=100,SPACE3=500,/PORTRAIT
   CS,SCALE=26,NCOLS=N_ELEMENTS(regress_levels)+1,/REV
   LEVS,MANUAL=regress_levels
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
   toplot=REFORM(onset_regressprecip(*,*))
   corr=REFORM(onset_corrprecip(*,*))
   IF TOTAL(where(ABS(corr) lt sig_level)) ne -1 THEN $
      toplot[where(ABS(corr) lt sig_level)]=!Values.F_NaN
   CON,X=longitude,Y=latitude,FIELD=toplot,$
       TITLE='Regression (mm day!U-1!N) of total precip on onset dates ('+$
       STRTRIM(STRING(onset_threshold),1)+'%ile) for '+model_name+' (5% sig only)',$
       /NOLINES,/BLOCK   
   AXES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/'+$
          'qld_higem_dblco2_precip_percentiles_monsoon_corrprecip_combined.retreat_corrprecip.'+model_name+'.'$
          +region_name+'_region.'+pct_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=1000,TFONT=3,$
          TCHARSIZE=100,SPACE3=500,/PORTRAIT
   CS,SCALE=26,NCOLS=N_ELEMENTS(regress_levels)+1,/REV
   LEVS,MANUAL=regress_levels
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
   toplot=REFORM(retreat_regressprecip(*,*))
   corr=REFORM(retreat_corrprecip(*,*))
   IF TOTAL(where(ABS(corr) lt sig_level)) ne -1 THEN $
      toplot[where(ABS(corr) lt sig_level)]=!Values.F_NaN
   CON,X=longitude,Y=latitude,FIELD=toplot,$
       TITLE='Regression (mm day!U-1!N) of total precip on retreat dates ('+$
       STRTRIM(STRING(retreat_threshold),1)+'%ile) for '+model_name,/NOLINES,/BLOCK
   AXES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/'+$
          'qld_higem_dblco2_precip_percentiles_monsoon_corrprecip_combined.length_corrprecip.'+model_name+'.'$
          +region_name+'_region.'+pct_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=1000,TFONT=3,$
          TCHARSIZE=90,SPACE3=500,/PORTRAIT
   CS,SCALE=26,NCOLS=N_ELEMENTS(regress_levels)+1,/REV
   LEVS,MANUAL=regress_levels
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
   toplot=REFORM(length_regressprecip(*,*))
   corr=REFORM(length_corrprecip(*,*))
   IF TOTAL(where(ABS(corr) lt sig_level)) ne -1 THEN $
      toplot[where(ABS(corr) lt sig_level)]=!Values.F_NaN
   CON,X=longitude,Y=latitude,FIELD=toplot,$
       TITLE='Regression (mm day!U-1!N) of total precip on length of season ('+$
       STRTRIM(STRING(onset_threshold),1)+'%ile and '+STRTRIM(STRING(retreat_threshold),1)+'%ile) for '+model_name,/NOLINES,/BLOCK
   AXES
   PSCLOSE,/NOVIEW

   FOR m=0,n_months-1 DO BEGIN
      psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/'+$
          'qld_higem_dblco2_precip_percentiles_monsoon_corrprecip_combined.onset_corrprecip_monthly_'+months_names(m)+'.'+model_name+'.'$
          +region_name+'_region.'+pct_name+'.ps'
      PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=1000,TFONT=3,$
             TCHARSIZE=90,SPACE3=500,/PORTRAIT
      CS,SCALE=26,NCOLS=N_ELEMENTS(monthly_regress_levels)+1,/REV
      LEVS,MANUAL=monthly_regress_levels
      MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
      toplot=REFORM(onset_monthly_regress(m,*,*))      
      corr=REFORM(onset_monthly_corr(m,*,*))
      IF TOTAL(where(ABS(corr) lt sig_level)) ne -1 THEN $
         toplot[where(ABS(corr) lt sig_level)]=!Values.F_NaN
      CON,X=longitude,Y=latitude,FIELD=toplot,$
          TITLE='Regression (mm day!U-1!N) of '+months_names(m)+' precip on onset date ('+$
          STRTRIM(STRING(onset_threshold),1)+'%ile) - '+model_name,/NOLINES,/BLOCK
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/'+$
          'qld_higem_dblco2_precip_percentiles_monsoon_corrprecip_combined.retreat_corrprecip_monthly_'+months_names(m)+'.'+model_name+'.'$
          +region_name+'_region.'+pct_name+'.ps'
      PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=1000,TFONT=3,$
             TCHARSIZE=90,SPACE3=500,/PORTRAIT
      CS,SCALE=26,NCOLS=N_ELEMENTS(monthly_regress_levels)+1,/REV
      LEVS,MANUAL=monthly_regress_levels
      MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
      toplot=REFORM(retreat_monthly_regress(m,*,*))
      corr=REFORM(retreat_monthly_corr(m,*,*))
      IF TOTAL(where(ABS(corr) lt sig_level)) ne -1 THEN $
         toplot[where(ABS(corr) lt sig_level)]=!Values.F_NaN
      CON,X=longitude,Y=latitude,FIELD=toplot,$
          TITLE='Regression (mm day!U-1!N) of '+months_names(m)+' precip on retreat date ('+$
          STRTRIM(STRING(retreat_threshold),1)+'%ile) - '+model_name,/NOLINES,/BLOCK
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/'+$
          'qld_higem_dblco2_precip_percentiles_monsoon_corrprecip_combined.length_corrprecip_monthly_'+months_names(m)+'.'+model_name+'.'$
          +region_name+'_region.'+pct_name+'.ps'
      PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=1000,TFONT=3,$
             TCHARSIZE=90,SPACE3=500,/PORTRAIT
      CS,SCALE=26,NCOLS=N_ELEMENTS(monthly_regress_levels)+1,/REV
      LEVS,MANUAL=monthly_regress_levels
      MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
      toplot=REFORM(length_monthly_regress(m,*,*))
      corr=REFORM(length_monthly_corr(m,*,*))
      IF TOTAL(where(ABS(corr) lt sig_level)) ne -1 THEN $
         toplot[where(ABS(corr) lt sig_level)]=!Values.F_NaN
      CON,X=longitude,Y=latitude,FIELD=toplot,$
          TITLE='Regression (mm day!U-1!N) of '+months_names(m)+' precip on length date ('+$
          STRTRIM(STRING(onset_threshold),1)+'%ile and '+STRTRIM(STRING(retreat_threshold),1)+'%ile) - '+model_name,/NOLINES,/BLOCK
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/'+$
          'qld_higem_dblco2_precip_percentiles_monsoon_corrprecip_combined.totalprecip_corrprecip_monthly_'+months_names(m)+'.'+model_name+'.'$
          +region_name+'_region.'+pct_name+'.ps'
      PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=1000,TFONT=3,$
             TCHARSIZE=90,SPACE3=500,/PORTRAIT
      CS,SCALE=24,NCOLS=N_ELEMENTS(totalprecip_regress_levels)+1
      LEVS,MANUAL=totalprecip_regress_levels
      MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
      toplot=REFORM(totalprecip_monthly_regress(m,*,*))
      corr=REFORM(totalprecip_monthly_corr(m,*,*))
      IF TOTAL(where(ABS(corr) lt sig_level)) ne -1 THEN $
         toplot[where(ABS(corr) lt sig_level)]=!Values.F_NaN
      CON,X=longitude,Y=latitude,FIELD=toplot,$
          TITLE='Regression (mm mm!U-1!N) of '+months_names(m)+' precip on Sep-Apr total precip - '+model_name,/NOLINES,/BLOCK
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/'+$
          'qld_higem_dblco2_precip_percentiles_monsoon_corrprecip_combined.fracprecip_corrprecip_monthly_'+months_names(m)+'.'+model_name+'.'$
          +region_name+'_region.'+pct_name+'.ps'
      PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=1000,TFONT=3,$
             TCHARSIZE=90,SPACE3=500,/PORTRAIT
      CS,SCALE=26,NCOLS=N_ELEMENTS(fracprecip_regress_levels)+1,/REV
      LEVS,MANUAL=fracprecip_regress_levels
      MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
      toplot=REFORM(fracprecip_monthly_regress(m,*,*))
      corr=REFORM(fracprecip_monthly_corr(m,*,*))
      IF TOTAL(where(ABS(corr) lt sig_level)) ne -1 THEN $
         toplot[where(ABS(corr) lt sig_level)]=!Values.F_NaN
      CON,X=longitude,Y=latitude,FIELD=toplot,$
          TITLE='Regression (% mm!U-1!N) of '+months_names(m)+' frac precip (of Sep-Apr) on Sep-Apr total precip - '+model_name,/NOLINES,/BLOCK
      AXES
      PSCLOSE,/NOVIEW

   ENDFOR
 
ENDFOR

STOP
END

