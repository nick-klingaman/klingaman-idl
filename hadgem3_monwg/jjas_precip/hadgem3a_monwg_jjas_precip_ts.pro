PRO hadgem3a_monwg_jjas_precip_ts

nick_1xentrain_daily_file='/home/ss901165/um_output3/hadgem3a_amip2_control_vn74/hadgem3a_amip2_ctl_vn74.jan-dec_dmeans.years1-77.precip.nc'
nick_1xentrain_clim_file='/home/ss901165/um_output3/hadgem3a_amip2_control_vn74/hadgem3a_amip2_ctl_vn74.jan-dec_dmean_clim.years1-77.precip.nc'

nick_15xentrain_daily_file='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmeans.years1-77.precip.nc'
nick_15xentrain_clim_file='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmean_clim.years1-77.precip.nc'

nick_15xentrain_iav_daily_file='/home/ss901165/um_output4/hadgem3a_amip2_iav_1.5xentrain_vn74/hadgem3a_amip2_iav_1.5xentrain_vn74.may-sep_dmeans.years1-22.precip.nc'
nick_15xentrain_iav_clim_file='/home/ss901165/um_output4/hadgem3a_amip2_iav_1.5xentrain_vn74/hadgem3a_amip2_iav_1.5xentrain_vn74.may-sep_dmean_clim.years1-22.precip.nc'

captivate_final_daily_file='/home/ss901165/um_output3/hadgem3_monwg/ajtzr/hadgem3ao_captivate_n96_orca1_ajtzr.jan-dec_dmeans.1979-2008.precip.reform_fourd.nc'
captivate_final_clim_file='/home/ss901165/um_output3/hadgem3_monwg/ajtzr/hadgem3ao_captivate_n96_orca1_ajtzr.jan-dec_dmean_clim.1979-2008.precip.nc'

akkvg_daily_file='/home/ss901165/um_output3/hadgem3_monwg/akkvg/akkvg.jan-dec_dmeans.i2-j7.precip.nc'
akkvg_clim_file='/home/ss901165/um_output3/hadgem3_monwg/akkvg/akkvg.jan-dec_dmean_clim.i2-j7.precip.nc'

imd_clim_file='/home/ss901165/datasets/IMD_GRIDDED/imd_1x1v2_1951-2004_mjjas_daily_clim.nc'
imd_daily_file='/home/ss901165/datasets/IMD_GRIDDED/imd_1x1v2_1951-2004.mjjas.nc'

n96_mask_file='/home/ss901165/um_output/mask_n96.nc'

; Box to area average
box=[10,70,30,88]

max_years=77

; Get JJAS means from all simulations

n_sets=5
all_clims=fltarr(n_sets)
all_colors=strarr(n_sets)
all_descriptions=strarr(n_sets)
FOR k=0,n_sets-1 DO BEGIN
   CASE k OF
      0 : BEGIN
         clim_file=nick_1xentrain_clim_file
         varname='precip'
         description='Nick 1.0x entrain with clim SSTs'
         color='blue'
         offset_jun1=150
         ntime=120
      END
      1 : BEGIN
         clim_file=nick_15xentrain_clim_file
         description='Nick 1.5x entrain with clim SSTs'
         color='purple'
         offset_jun1=150
         ntime=120
      END
;      2 : BEGIN
;         clim_file=nick_15xentrain_iav_clim_file
;         description='Nick 1.5x entrain with IAV SSTs'
;         color='purple'
;         offset_jun1=30
;         ntime=120
;      END
      2 : BEGIN
         clim_file=captivate_final_clim_file
         description='Captivate final N96 ORCA1'
         color='red'
         offset_jun1=150
         ntime=120
      END
      3 : BEGIN
         clim_file=akkvg_clim_file
         description='GA3.0 AMIP2 N96'
         color='orange'
         offset_jun1=150
         ntime=120
      END
      4 : BEGIN
         clim_file=imd_clim_file
         varname='rf'
         description='IMD climatology'
         color='black'
         offset_jun1=151         
         ntime=122
      END
   ENDCASE

   longitude=OPEN_AND_EXTRACT(clim_file,'longitude')
   latitude=OPEN_AND_EXTRACT(clim_file,'latitude')
   DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
   nlon=N_ELEMENTS(longitude)
   nlat=N_ELEMENTS(latitude)
   
   IF k ne 4 THEN BEGIN
                                ; Get mask grid information
      mask_longitude=OPEN_AND_EXTRACT(n96_mask_file,'longitude')
      mask_latitude=OPEN_AND_EXTRACT(n96_mask_file,'latitude')
      DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
      mask_nlon=N_ELEMENTS(mask_longitude)
      mask_nlat=N_ELEMENTS(mask_latitude)
      
      n96_mask=REFORM(OPEN_AND_EXTRACT(n96_mask_file,'lsm',offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                                       count=[mask_nlon,mask_nlat,1,1]))
      n96_mask_rev=fltarr(mask_nlon,mask_nlat)
      FOR i=0,nlon-1 DO $
         FOR j=0,nlat-1 DO $
            n96_mask_rev(i,nlat-j-1)=n96_mask(i,j)
      
      clim=REFORM(OPEN_AND_EXTRACT(clim_file,'precip',$
                                   offset=[box_tx(1),box_tx(0),offset_jun1],$
                                           count=[nlon,nlat,ntime]))*86400.
      clim_aavg=fltarr(ntime)
      
      FOR i=0,ntime-1 DO BEGIN
         temp_precip=REFORM(clim(*,*,i))
         temp_precip[where(n96_mask_rev eq 0)] = !Values.F_NaN
         clim_aavg(i)=MEAN(temp_precip,/NaN)
      ENDFOR
      IF k eq 1 THEN $
         clim_aavg=clim_aavg*1.1532
   ENDIF ELSE BEGIN
      clim=REFORM(OPEN_AND_EXTRACT(clim_file,'rf',$
                                   offset=[box_tx(1),box_tx(0),0],$
                                   count=[nlon,nlat,ntime]))
      clim_aavg=fltarr(ntime)
      ocean=where(clim gt 1000)
      clim[ocean]=!Values.F_NaN
      FOR i=0,ntime-1 DO $
         clim_aavg(i)=MEAN(clim(*,*,i),/NaN)
   ENDELSE

   all_clims(k)=MEAN(clim_aavg,/NaN)
   all_colors(k)=color
   all_descriptions(k)=description
ENDFOR

n_sets_plot=5
FOR k=0,n_sets_plot-1 DO BEGIN
   CASE k OF
      0 : BEGIN
         hadgem3_daily_file=nick_1xentrain_daily_file         
         varname='precip'
         multiplier=86400.
         offset_jun1=150
         offset_sep30=269
         hadgem3_nyears=77
         runid='hadgem3a_amip2_1.0xentrain_vn74'
         description='Nick HadGEM3-A (vn7.4) 1.0x entrain with AMIP2 clim SSTs (77 years)'
         color='blue'
      END
      1 : BEGIN
         hadgem3_daily_file=nick_15xentrain_daily_file
         offset_jun1=150
         offset_sep30=269
         hadgem3_nyears=77
         runid='hadgem3a_amip2_1.5xentrain_vn74'
         description='Nick HadGEM3-A (vn7.4) 1.5x entrain with AMIP2 clim SSTs (77 years)'
         color='purple'
      END
      ;2 : BEGIN
      ;   hadgem3_daily_file=nick_15xentrain_iav_daily_file
      ;   offset_jun1=30
      ;   offset_sep30=149
      ;   hadgem3_nyears=22
      ;   runid='hadgem3a_amip2_iav_1.5xentrain_vn74'
      ;   description='Nick HadGEM3-A (vn7.4) 1.5x entrain with AMIP2 IAV SSTs (22 years)'
      ;   color='purple'
      ;END
      2 : BEGIN         
         hadgem3_daily_file=captivate_final_daily_file
         offset_jun1=150
         offset_sep30=269
         hadgem3_nyears=29
         runid='hadgem3a_captivate_n96_orca1'
         description='Captivate final N96 ORCA1 coupled (30 years)'
         color='red'
      END
      3 : BEGIN
         hadgem3_daily_file=akkvg_daily_file
         offset_jun1=150
         offset_sep30=269
         hadgem3_nyears=16
         runid='hadgem3a_ga3.0_n96_amip2'
         description='GA3.0 AMIP2 N96 (16 years)'
         color='orange'
      END
      4 : BEGIN
         hadgem3_daily_file=imd_daily_file
         varname='rf'
         multiplier=1.
         offset_jun1=30
         offset_sep30=151
         hadgem3_nyears=54
         runid='imd_1x1'
         description='IMD 1x1 degree climatology (1951-2004)'
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
   IF k ne 4 THEN BEGIN
      mask_longitude=OPEN_AND_EXTRACT(n96_mask_file,'longitude')
      mask_latitude=OPEN_AND_EXTRACT(n96_mask_file,'latitude')
      DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
      mask_nlon=N_ELEMENTS(mask_longitude)
      mask_nlat=N_ELEMENTS(mask_latitude)
      
; Get mask
      n96_mask=REFORM(OPEN_AND_EXTRACT(n96_mask_file,'lsm',offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                                       count=[mask_nlon,mask_nlat,1,1]))
      n96_mask_rev=fltarr(mask_nlon,mask_nlat)
      FOR i=0,hadgem3_nlon-1 DO $
         FOR j=0,hadgem3_nlat-1 DO $
            n96_mask_rev(i,hadgem3_nlat-j-1)=n96_mask(i,j)
   ENDIF 
;   print,hadgem3_latitude
;   print,mask_latitude

   hadgem3_jjas_ts=fltarr(hadgem3_nyears)
   FOR i=0,hadgem3_nyears-1 DO BEGIN
      hadgem3_year=REFORM(OPEN_AND_EXTRACT(hadgem3_daily_file,varname,$
                                           offset=[hadgem3_box_tx(1),hadgem3_box_tx(0),offset_jun1,i],$
                                           count=[hadgem3_nlon,hadgem3_nlat,n_time,1]))*multiplier   
      hadgem3_year_aavg=fltarr(n_time)
      FOR j=0,n_time-1 DO BEGIN
         temp_precip=REFORM(hadgem3_year(*,*,j))
         IF k ne 4 THEN BEGIN
            temp_precip[where(n96_mask_rev eq 0)]=!Values.F_NaN
         ENDIF ELSE $
            temp_precip[where(temp_precip ge 1e10)]=!Values.F_NaN
         hadgem3_year_aavg(j)=MEAN(temp_precip,/NaN)
      ENDFOR
      hadgem3_jjas_ts(i)=MEAN(hadgem3_year_aavg)
   ENDFOR
   IF k eq 1 THEN $
      hadgem3_jjas_ts=hadgem3_jjas_ts*1.1532
   print,STDDEV(hadgem3_jjas_ts),STDDEV(hadgem3_jjas_ts)/MEAN(hadgem3_jjas_ts)

;   IF k eq 0 THEN STOP

; Build plot
   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_ts.'+runid+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=300,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=300
   ymin=0
   ymax=12
   GSET,XMIN=0,XMAX=max_years,YMIN=ymin,YMAX=ymax,TITLE='Timeseries of JJAS area-avg precip over India for '+description
   GPLOT,X=indgen(max_years)+0.5,Y=hadgem3_jjas_ts,COL=FSC_COLOR(color),SYM=4
   FOR i=0,n_sets-1 DO $
      GPLOT,X=[0,max_years],Y=[all_clims(i),all_clims(i)],COL=FSC_COLOR(all_colors(i)),STYLE=2
   
   AXES,XVALS=indgen(max_years/2+ODD(max_years))*2+0.5,XMINOR=indgen(max_years)+0.5,XLABELS=STRTRIM(STRING(indgen(max_years/2+ODD(max_years))*2+1),1),YSTEP=1,YMINOR=0.5,XTITLE='Year',YTITLE='Area-averaged precipitation over Indian land (mm day!U-1!N)'
   GLEGEND,labels=REVERSE(all_descriptions),col=REVERSE(FSC_COLOR(all_colors)),LEGPOS=1
   PSCLOSE,/NOVIEW

   n_bins=21
   bin_size=0.5
   bin_offset=1
   bins=findgen(n_bins)*bin_size+bin_offset
   bins_mid=findgen(n_bins-1)*bin_size+bin_offset+bin_size/2.

   pdf=fltarr(n_bins-1)
   FOR j=0,n_bins-2 DO BEGIN
      IF TOTAL(where(hadgem3_jjas_ts gt bins(j) and hadgem3_jjas_ts le bins(j+1))) ge 0 THEN $
         pdf(j)=N_ELEMENTS(where(hadgem3_jjas_ts gt bins(j) and hadgem3_jjas_ts le bins(j+1)))
   ENDFOR

   pdf=pdf/FLOAT(hadgem3_nyears)

   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_ts.'+runid+'_pdf.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=300,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=300
   GSET,XMIN=MIN(bins),XMAX=MAX(bins),YMIN=0,YMAX=0.5,TITLE='PDF of JJAS area-avg precip over India for '+description
   GPLOT,X=bins_mid,Y=pdf,COL=FSC_COLOR(color)
   AXES,XVALS=bins,XLABELS=STRMID(STRTRIM(STRING(bins),1),0,4),YSTEP=0.05,YTITLE='Fraction of years',XTITLE='All-India rainfall (JJAS average; mm day!U-1!N)',NDECS=3
   PSCLOSE,/NOVIEW  
ENDFOR
   
STOP

END
