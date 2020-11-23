PRO hadgem3_monwg_jjas_precip_dailyair

;nick_1xentrain_daily_file='/home/ss901165/um_output3/hadgem3a_amip2_control_vn74/hadgem3a_amip2_ctl_vn74.jan-dec_dmeans.years1-77.precip.nc'
;nick_1xentrain_clim_file='/home/ss901165/um_output3/hadgem3a_amip2_control_vn74/hadgem3a_amip2_ctl_vn74.jan-dec_dmean_clim.years1-77.precip.nc'
;
;nick_15xentrain_daily_file='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmeans.years1-77.precip.nc'
;nick_15xentrain_clim_file='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmean_clim.years1-77.precip.nc'

imd_daily_file='/home/users/npklingaman/datasets/IMD_GRIDDED/imd_1x1v2_1951-2004.mjjas.nc'
imd_clim_file='/home/users/npklingaman/datasets/IMD_GRIDDED/imd_1x1v2_1951-2004_mjjas_daily_clim.nc'



; Box to area average
box=[10,70,30,88]

max_years=77

n_sets=3
FOR k=0,n_sets-1 DO BEGIN
   CASE k OF
      0 : BEGIN
         hadgem3_daily_file='/home/users/npklingaman/klingaman/metum/gc2/anqjm/hadgem3_gc2_n96_orca025.jan-dec_dmeans.years1-41.precip.nc'
         hadgem3_clim_file='/home/users/npklingaman/klingaman/metum/gc2/anqjm/hadgem3_gc2_n96_orca025.jan-dec_dmean_clim.years1-41.precip.nc'
         varname='precip'
         multiplier=86400.
         offset_jun1=150
         offset_sep30=269
         hadgem3_nyears=41
         runid='gc2_n96'
         description='MetUM GC2 N96'
         color='blue'
         n96_mask_file='/home/users/npklingaman/datasets/HADGEM3-KPP_ANCIL/landfrac_n96e_hadgem3-8.5.nc'
      END
      1 : BEGIN
         hadgem3_daily_file='/home/users/npklingaman/klingaman/metum/gc3/u-ab673/mjo/ab673a.jan-dec_dmeans.2013-2112.precip.nc'
         hadgem3_clim_file='/home/users/npklingaman/klingaman/metum/gc3/u-ab673/mjo/ab673a.jan-dec_dmean_clim.2013-2112.precip.nc'         
         varname='precipitation_flux'
         offset_jun1=150
         offset_sep30=269
         hadgem3_nyears=100
         runid='gc3_n96'
         description='MetUM GC3 N96'
         color='purple'
         n96_mask_file='/home/users/npklingaman/datasets/HADGEM3-KPP_ANCIL/landfrac_n96e_hadgem3-8.5.nc'
      END
      2 : BEGIN
         hadgem3_daily_file=imd_daily_file
         hadgem3_clim_file=imd_clim_file
         varname='rf'
         multiplier=1.
         offset_jun1=31
         offset_sep30=152
         hadgem3_nyears=54
         runid='imd_gridded'
         description='IMD 1x1 gridded dataset (1951-2004)'
         color='black'         
      END
   ENDCASE
   
   n_time=offset_sep30-offset_jun1+1
                                ; Get grid information
   hadgem3_longitude=OPEN_AND_EXTRACT(hadgem3_daily_file,'longitude')
   hadgem3_latitude=OPEN_AND_EXTRACT(hadgem3_daily_file,'latitude')
   DEFINE_BOUNDARIES,box,hadgem3_latitude,hadgem3_longitude,hadgem3_box_tx,/LIMIT
   hadgem3_nlon=N_ELEMENTS(hadgem3_longitude)
   hadgem3_nlat=N_ELEMENTS(hadgem3_latitude)
                                ; Get mask grid information
   mask_longitude=OPEN_AND_EXTRACT(n96_mask_file,'longitude')
   mask_latitude=OPEN_AND_EXTRACT(n96_mask_file,'latitude')
   DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
   mask_nlon=N_ELEMENTS(mask_longitude)
   mask_nlat=N_ELEMENTS(mask_latitude)
   
   IF k ne 2 THEN BEGIN
                                ; Get mask
      n96_mask=REFORM(OPEN_AND_EXTRACT(n96_mask_file,'lsm',offset=[mask_box_tx(1),mask_box_tx(0)],$
                                       count=[mask_nlon,mask_nlat]))
      n96_mask_rev=fltarr(mask_nlon,mask_nlat)
      FOR i=0,hadgem3_nlon-1 DO $
         FOR j=0,hadgem3_nlat-1 DO $
            n96_mask_rev(i,hadgem3_nlat-j-1)=n96_mask(i,j)
   ENDIF

   hadgem3_daily_air=fltarr(n_time,hadgem3_nyears)
   FOR i=0,hadgem3_nyears-1 DO BEGIN
      hadgem3_year=REFORM(OPEN_AND_EXTRACT(hadgem3_daily_file,varname,$
                                           offset=[hadgem3_box_tx(1),hadgem3_box_tx(0),offset_jun1,i],$
                                           count=[hadgem3_nlon,hadgem3_nlat,n_time,1]))*multiplier
      hadgem3_year_aavg=fltarr(n_time)
      FOR j=0,n_time-1 DO BEGIN
         temp_precip=REFORM(hadgem3_year(*,*,j))
         IF k ne 2 THEN BEGIN
            temp_precip[where(n96_mask_rev eq 0)]=!Values.F_NaN
         ENDIF ELSE $
            temp_precip[where(temp_precip ge 1e10)]=!Values.F_NaN
         hadgem3_daily_air(j,i)=MEAN(temp_precip,/NaN)
      ENDFOR
   ENDFOR
   
   hadgem3_clim=REFORM(OPEN_AND_EXTRACT(hadgem3_clim_file,varname,$
                                        offset=[hadgem3_box_tx(1),hadgem3_box_tx(0),offset_jun1],$
                                        count=[hadgem3_nlon,hadgem3_nlat,n_time]))*multiplier
   hadgem3_clim_air=fltarr(n_time)
   FOR i=0,n_time-1 DO BEGIN
      temp_precip=REFORM(hadgem3_clim(*,*,i))
      IF k ne 2 THEN BEGIN
         temp_precip[where(n96_mask_rev eq 0)]=!Values.F_NaN
      ENDIF ELSE $
         temp_precip[where(temp_precip ge 1e10)]=!Values.F_NaN
      hadgem3_clim_air(i)=MEAN(temp_precip,/NaN)
   ENDFOR
   hadgem3_anom_daily_air=fltarr(n_time,hadgem3_nyears)
   FOR i=0,hadgem3_nyears-1 DO $
      hadgem3_anom_daily_air(*,i)=hadgem3_daily_air(*,i)-hadgem3_clim_air
   
                                ; Settings for bins for raw and anomalous AIR
   n_bins=21
   bin_size=1.0
   bin_offset=0   
   bins=findgen(n_bins)*bin_size+bin_offset
   bins_mid=findgen(n_bins-1)*bin_size+bin_offset+bin_size/2.

   n_anom_bins=21
   anom_bin_size=1.0
   anom_bin_offset=-7
   anom_bins=findgen(n_anom_bins)*anom_bin_size+anom_bin_offset
   anom_bins_mid=findgen(n_anom_bins-1)*anom_bin_size+anom_bin_offset+anom_bin_size/2.

   IF k eq 0 THEN BEGIN
      my_pdf=fltarr(n_sets,n_bins-1)
      my_anom_pdf=fltarr(n_sets,n_anom_bins-1)
      trans_prob=fltarr(n_sets,n_bins-1,n_bins-1)
      anom_trans_prob=fltarr(n_sets,n_anom_bins-1,n_anom_bins-1)
      bin_npts=fltarr(n_sets,n_bins-1)
      anom_bin_npts=fltarr(n_sets,n_anom_bins-1)
      all_colors=strarr(n_sets)
      all_descriptions=strarr(n_sets)
   ENDIF
   all_colors(k)=color
   all_descriptions(k)=description

   FOR j=0,n_bins-2 DO BEGIN
      pts=where(hadgem3_daily_air gt bins(j) and hadgem3_daily_air le bins(j+1))
      IF TOTAL(pts) gt -1 THEN BEGIN
         my_pdf(k,j)=N_ELEMENTS(pts)
         FOR m=0,my_pdf(k,j)-1 DO BEGIN
            IF pts(m) ne hadgem3_nyears*n_time-1 THEN BEGIN
               tomorrow_bin=NEAREST(bins_mid,hadgem3_daily_air(pts(m)+1))
               trans_prob(k,j,tomorrow_bin)=trans_prob(k,j,tomorrow_bin)+1
            ENDIF
         ENDFOR
      ENDIF
      bin_npts(k,j)=my_pdf(k,j)
      trans_prob(k,j,*)=trans_prob(k,j,*)/FLOAT(TOTAL(trans_prob(k,j,*)))
   ENDFOR   
   my_pdf(k,*)=my_pdf(k,*)/FLOAT(hadgem3_nyears*n_time)

   FOR j=0,n_anom_bins-2 DO BEGIN
      pts=where(hadgem3_anom_daily_air gt anom_bins(j) and hadgem3_anom_daily_air le anom_bins(j+1))
      IF TOTAL(pts) gt -1 THEN BEGIN
         my_anom_pdf(k,j)=N_ELEMENTS(pts)
         FOR m=0,my_anom_pdf(k,j)-1 DO BEGIN
            IF pts(m) ne hadgem3_nyears*n_time-1 THEN BEGIN
               tomorrow_bin=NEAREST(anom_bins_mid,hadgem3_anom_daily_air(pts(m)+1))
               anom_trans_prob(k,j,tomorrow_bin)=anom_trans_prob(k,j,tomorrow_bin)+1
            ENDIF
         ENDFOR
      ENDIF
      anom_bin_npts(k,j)=my_anom_pdf(k,j)
      anom_trans_prob(k,j,*)=anom_trans_prob(k,j,*)/FLOAT(TOTAL(anom_trans_prob(k,j,*)))
   ENDFOR   
   my_anom_pdf(k,*)=my_anom_pdf(k,*)/FLOAT(hadgem3_nyears*n_time)

   mylevs_prob=['0.01','0.05','0.09','0.13','0.17','0.21','0.25','0.29','0.33','0.37','0.41','0.45','0.49','0.53',$
                '0.57','0.61','0.65','0.69','0.73']
   psfile='/home/users/npklingaman/plots/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_dailyair.'+runid+'.day1_trans_prob.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=2000,SPACE2=2000,XOFFSET=800,YOFFSET=2000,TFONT=2,TCHARSIZE=100,SPACE3=300,$
          XSIZE=15000,YSIZE=15000,CB_WIDTH=125
   GSET,XMIN=MIN(bins),XMAX=MAX(bins),YMIN=MIN(bins),YMAX=MAX(bins),$
        TITLE='Day +1 transition prob of all-India rain (JJAS) for '+description
   LEVS,MANUAL=mylevs_prob
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_prob)+1,white=[2]
   CON,X=bins_mid,Y=bins_mid,FIELD=REFORM(trans_prob(k,*,*)),/BLOCK,CB_TITLE='Probability (unitless)',/NOLINES,CB_NTH=2
   GPLOT,X=[0,MAX(bins)],Y=[0,MAX(bins)],STYLE=1
   AXES,XVALS=bins(0:n_bins-1:2),XLABELS=STRMID(STRTRIM(STRING(bins(0:n_bins-1:2)),1),0,4),$
        YVALS=bins(0:n_bins-1:2),YLABELS=STRMID(STRTRIM(STRING(bins(0:n_bins-1:2)),1),0,4),$
        YTITLE='All-India rainfall on day n+1 (mm day!U-1!N)',$
        XTITLE='All-India rainfall on day n (mm day!U-1!N)',ORIENTATION=30,/NORIGHT
   GSET,XMIN=MIN(bins),XMAX=MAX(bins),YMIN=0,YMAX=0.5
   GPLOT,X=bins_mid,Y=REFORM(bin_npts(k,*))/TOTAL(bin_npts(k,*)),STYLE=2,SYM=3
   AXES,XVALS=bins(0:n_bins-1:2),YSTEP=0.05,YTITLE='Fraction of points in bin for day n rainfall',/ONLYRIGHT,NDECS=2
   PSCLOSE,/NOVIEW

   psfile='/home/users/npklingaman/plots/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_dailyair.'+runid+'.anom_day1_trans_prob.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=2000,SPACE2=2000,XOFFSET=800,YOFFSET=2000,TFONT=2,TCHARSIZE=100,SPACE3=300,$
          XSIZE=15000,YSIZE=15000,CB_WIDTH=125
   GSET,XMIN=MIN(anom_bins),XMAX=MAX(anom_bins),YMIN=MIN(anom_bins),YMAX=MAX(anom_bins),$
        TITLE='Day +1 prob of AIR anom from daily clim for '+description
   LEVS,MANUAL=mylevs_prob
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_prob)+1,white=[2]
   CON,X=anom_bins_mid,Y=anom_bins_mid,FIELD=REFORM(anom_trans_prob(k,*,*)),/BLOCK,CB_TITLE='Probability (unitless)',/NOLINES,CB_NTH=2
   GPLOT,X=[MIN(anom_bins),MAX(anom_bins)],Y=[MIN(anom_bins),MAX(anom_bins)],STYLE=1
   GPLOT,X=[0,0],Y=[MIN(anom_bins),MAX(anom_bins)],STYLE=1
   GPLOT,X=[MIN(anom_bins),MAX(anom_bins)],Y=[0,0],STYLE=1
   AXES,XVALS=anom_bins(0:n_anom_bins-1:2),XLABELS=STRMID(STRTRIM(STRING(anom_bins(0:n_anom_bins-1:2)),1),0,4),$
        YVALS=anom_bins(0:n_anom_bins-1:2),YLABELS=STRMID(STRTRIM(STRING(anom_bins(0:n_anom_bins-1:2)),1),0,4),$
        YTITLE='All-India rainfall anomaly on day n+1 (mm day!U-1!N)',$
        XTITLE='All-India rainfall anomaly on day n (mm day!U-1!N)',ORIENTATION=30,/NORIGHT
   GSET,XMIN=MIN(anom_bins),XMAX=MAX(anom_bins),YMIN=0,YMAX=0.5
   GPLOT,X=anom_bins_mid,Y=REFORM(anom_bin_npts(k,*))/TOTAL(anom_bin_npts(k,*)),STYLE=2,SYM=3
   AXES,XVALS=anom_bins(0:n_anom_bins-1:2),YSTEP=0.05,YTITLE='Fraction of points in bin for day n rainfall',/ONLYRIGHT,NDECS=2
   PSCLOSE,/NOVIEW

ENDFOR
psfile='/home/users/npklingaman/plots/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_dailyair.daily_pdf.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=300,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=300
GSET,XMIN=MIN(bins),XMAX=MAX(bins),YMIN=0,YMAX=0.2,TITLE='PDF of daily all-India rainfall during JJAS'
FOR k=0,n_sets-1 DO $
   GPLOT,X=bins_mid,Y=REFORM(my_pdf(k,*)),COL=FSC_COLOR(all_colors(k))
AXES,XVALS=bins,XLABELS=STRMID(STRTRIM(STRING(bins),1),0,4),YSTEP=0.01,YTITLE='Fraction of days',$
     XTITLE='Daily all-India rainfall in JJAS (mm day!U-1!N)',NDECS=2,ORIENTATION=30
GLEGEND,LEGPOS=1,labels=all_descriptions,COL=FSC_COLOR(all_colors)
PSCLOSE,/NOVIEW

psfile='/home/users/npklingaman/plots/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_dailyair.anom_daily_pdf.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=300,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=300
GSET,XMIN=MIN(anom_bins),XMAX=MAX(anom_bins),YMIN=0,YMAX=0.3,$
     TITLE='PDF of daily all-India rainfall anomalies (from seascycle) during JJAS'
FOR k=0,n_sets-1 DO $
   GPLOT,X=anom_bins_mid,Y=REFORM(my_anom_pdf(k,*)),COL=FSC_COLOR(all_colors(k))
GPLOT,X=[0,0],Y=[0,0.3],STYLE=1
AXES,XVALS=anom_bins,XLABELS=STRMID(STRTRIM(STRING(anom_bins),1),0,4),YSTEP=0.02,YTITLE='Fraction of days',$
     XTITLE='Daily all-India rainfall anomaly (mm day!U-1!N)',NDECS=2,ORIENTATION=30
GLEGEND,LEGPOS=1,labels=all_descriptions,COL=FSC_COLOR(all_colors)
PSCLOSE,/NOVIEW

STOP

END
