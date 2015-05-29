PRO hadgem3_monwg_jjas_precip_air_wetvdry_hg3kpp

nick_1xentrain_daily_file='/home/ss901165/um_output3/hadgem3a_amip2_control_vn74/hadgem3a_amip2_ctl_vn74.jan-dec_dmeans.years1-77.precip.nc'
nick_1xentrain_clim_file='/home/ss901165/um_output3/hadgem3a_amip2_control_vn74/hadgem3a_amip2_ctl_vn74.jan-dec_dmean_clim.years1-77.precip.nc'
nick_15xentrain_daily_file='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmeans.years1-77.precip.nc'
nick_15xentrain_clim_file='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmean_clim.years1-77.precip.nc'

aent_daily_file='/home/ss901165/um_output6/xgspj/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmeans.years1-20.precip.nc'
aent_clim_file='/home/ss901165/um_output6/xgspj/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.precip.nc'
k50ent_daily_file='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans.years1-20.precip.nc'
k50ent_clim_file='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-20.precip.nc'

n96_mask_file='/home/ss901165/um_output/mask_n96.nc'

; Box to area average
box=[10,70,30,88]

; Box to plot
box_plot=[-40,40,40,200]

max_years=23
nyears_wet=7
nyears_dry=7

mylevs_raw=['2','3','4','5','6','7','8','9','10','12','14','16','18','20','22','24']
mylevs_diff_small=['-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5']
mylevs_diff=['-11','-9','-7','-5','-3','-1','1','3','5','7','9','11']

n_sets_plot=1

FOR k=0,n_sets_plot-1 DO BEGIN
   CASE k OF
      1 : BEGIN
         hadgem3_daily_file=aent_daily_file         
         hadgem3_clim_file=aent_clim_file
         offset_jun1=150
         offset_sep30=269
         hadgem3_nyears=20
         runid='hadgem3a_ukmo_1.5xentrain_ga30'
         description='A-ENT-OBS (20 yrs)'
      END
      0 : BEGIN
         hadgem3_daily_file=k50ent_daily_file
         hadgem3_clim_file=k50ent_clim_file
         offset_jun1=150
         offset_sep30=269
         hadgem3_nyears=20
         runid='hadgem3kpp_1.5xentrain_ga30_50N50S'
         description='K50-ENT-OBS (23 years)'
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
   
; Get mask
   n96_mask=REFORM(OPEN_AND_EXTRACT(n96_mask_file,'lsm',offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                                    count=[mask_nlon,mask_nlat,1,1]))
   n96_mask_rev=fltarr(mask_nlon,mask_nlat)
   FOR i=0,hadgem3_nlon-1 DO $
      FOR j=0,hadgem3_nlat-1 DO $
         n96_mask_rev(i,hadgem3_nlat-j-1)=n96_mask(i,j)
   


   hadgem3_jjas_ts=fltarr(hadgem3_nyears)
   FOR i=0,hadgem3_nyears-1 DO BEGIN
      hadgem3_year=REFORM(OPEN_AND_EXTRACT(hadgem3_daily_file,'precip',$
                                           offset=[hadgem3_box_tx(1),hadgem3_box_tx(0),offset_jun1,i],$
                                           count=[hadgem3_nlon,hadgem3_nlat,n_time,1]))*86400.      
      hadgem3_year_aavg=fltarr(n_time)
      FOR j=0,n_time-1 DO BEGIN
         temp_precip=REFORM(hadgem3_year(*,*,j))
         temp_precip[where(n96_mask_rev eq 0)]=!Values.F_NaN
         hadgem3_year_aavg(j)=MEAN(temp_precip,/NaN)*1.4
      ENDFOR
      hadgem3_jjas_ts(i)=MEAN(hadgem3_year_aavg)
   ENDFOR
   ;print,STDDEV(hadgem3_jjas_ts),STDDEV(hadgem3_jjas_ts)/MEAN(hadgem3_jjas_ts)

   hadgem3_jjas_ts_sorted=SORT(hadgem3_jjas_ts)
   wettest_years=hadgem3_jjas_ts_sorted(hadgem3_nyears-nyears_wet-1:hadgem3_nyears-1)
   driest_years=hadgem3_jjas_ts_sorted(0:nyears_dry-1)

   hadgem3_longitude=OPEN_AND_EXTRACT(hadgem3_daily_file,'longitude')
   hadgem3_latitude=OPEN_AND_EXTRACT(hadgem3_daily_file,'latitude')
   DEFINE_BOUNDARIES,box_plot,hadgem3_latitude,hadgem3_longitude,hadgem3_box_plot_tx,/LIMIT
   hadgem3_nlon=N_ELEMENTS(hadgem3_longitude)
   hadgem3_nlat=N_ELEMENTS(hadgem3_latitude)

   IF k eq 0 THEN BEGIN
      hadgem3_wet_composite=fltarr(n_sets_plot,hadgem3_nlon,hadgem3_nlat)
      hadgem3_dry_composite=fltarr(n_sets_plot,hadgem3_nlon,hadgem3_nlat)
   ENDIF
   FOR i=0,nyears_wet-1 DO BEGIN
      hadgem3_year=REFORM(OPEN_AND_EXTRACT(hadgem3_daily_file,'precip',$
                                           offset=[hadgem3_box_plot_tx(1),hadgem3_box_plot_tx(0),offset_jun1,wettest_years(i)],$
                                           count=[hadgem3_nlon,hadgem3_nlat,n_time,1]))*86400.
      FOR m=0,hadgem3_nlon-1 DO $
         FOR n=0,hadgem3_nlat-1 DO $
            hadgem3_wet_composite(k,m,n)=MEAN(hadgem3_year(m,n,*))/FLOAT(nyears_wet)+hadgem3_wet_composite(k,m,n)
   ENDFOR
   FOR i=0,nyears_dry-1 DO BEGIN
      hadgem3_year=REFORM(OPEN_AND_EXTRACT(hadgem3_daily_file,'precip',$
                                           offset=[hadgem3_box_plot_tx(1),hadgem3_box_plot_tx(0),offset_jun1,driest_years(i)],$
                                           count=[hadgem3_nlon,hadgem3_nlat,n_time,1]))*86400.
      FOR m=0,hadgem3_nlon-1 DO $
         FOR n=0,hadgem3_nlat-1 DO $
            hadgem3_dry_composite(k,m,n)=MEAN(hadgem3_year(m,n,*))/FLOAT(nyears_dry)+hadgem3_dry_composite(k,m,n)      
   ENDFOR

   hadgem3_clim=REFORM(OPEN_AND_EXTRACT(hadgem3_clim_file,'precip',$
                                        offset=[hadgem3_box_plot_tx(1),hadgem3_box_plot_tx(0),offset_jun1],$
                                        count=[hadgem3_nlon,hadgem3_nlat,n_time]))*86400.
   hadgem3_clim_smean=fltarr(hadgem3_nlon,hadgem3_nlat)
   FOR m=0,hadgem3_nlon-1 DO $
      FOR n=0,hadgem3_nlat-1 DO $
         hadgem3_clim_smean(m,n)=MEAN(hadgem3_clim(m,n,*))

   ;hadgem3_wet_composite(k,*,*)=hadgem3_wet_composite(k,*,*)-hadgem3_clim_smean
   ;hadgem3_dry_composite(k,*,*)=hadgem3_dry_composite(k,*,*)-hadgem3_clim_smean

   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_air_wetvdry.'+runid+'.wet_composite.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=15000,SPACE3=500
   CS,SCALE=40,NCOLS=N_ELEMENTS(mylevs_raw)+1,/REV,white=[2]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_raw
   CON,FIELD=REFORM(hadgem3_wet_composite(k,*,*)),X=hadgem3_longitude,Y=hadgem3_latitude,$
       TITLE="JJAS rainfall for composite of wettest "+STRTRIM(STRING(nyears_wet),1)+" years from "+runid,CB_TITLE='mm day!U-1!N',$
       /NOLINES
   PSCLOSE

   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_air_wetvdry.'+runid+'.dry_composite.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=15000,SPACE3=500
   CS,SCALE=40,NCOLS=N_ELEMENTS(mylevs_raw)+1,/REV,white=[2]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_raw
   CON,FIELD=REFORM(hadgem3_dry_composite(k,*,*)),X=hadgem3_longitude,Y=hadgem3_latitude,$
       TITLE="JJAS rainfall for composite of driest "+STRTRIM(STRING(nyears_dry),1)+" years from "+runid,CB_TITLE='mm day!U-1!N',$
       /NOLINES
   PSCLOSE

   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_air_wetvdry.'+runid+'.wet_composite_anom.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=15000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff_small)+1,/REV,white=[8]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_diff_small
   CON,FIELD=REFORM(hadgem3_wet_composite(k,*,*))-hadgem3_clim_smean,X=hadgem3_longitude,Y=hadgem3_latitude,$
       TITLE="JJAS anom rain for composite of wettest "+STRTRIM(STRING(nyears_wet),1)+" years from "+runid,CB_TITLE='mm day!U-1!N',$
       /NOLINES
   PSCLOSE

   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_air_wetvdry.'+runid+'.dry_composite_anom.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=15000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff_small)+1,/REV,white=[8]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_diff_small
   CON,FIELD=REFORM(hadgem3_dry_composite(k,*,*))-hadgem3_clim_smean,X=hadgem3_longitude,Y=hadgem3_latitude,$
       TITLE="JJAS anom rain for composite of driest "+STRTRIM(STRING(nyears_dry),1)+" years from "+runid,CB_TITLE='mm day!U-1!N',$
       /NOLINES
   PSCLOSE

   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_air_wetvdry.'+runid+'.wet-minus-dry_composite.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=15000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff_small)+1,/REV,white=[8]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_diff_small
   CON,FIELD=REFORM(hadgem3_wet_composite(k,*,*)-hadgem3_dry_composite(k,*,*)),X=hadgem3_longitude,Y=hadgem3_latitude,$
       TITLE="JJAS rainfall for diff wettest "+STRTRIM(STRING(nyears_wet),1)+" yr minus driest "+STRTRIM(STRING(nyears_dry),1)+$
       " from "+runid,CB_TITLE='mm day!U-1!N',$
       /NOLINES
   PSCLOSE

ENDFOR
STOP

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_air_wetvdry.1.5x-minus-1.0x.wet_composite.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=15000,SPACE3=500
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff_small)+1,/REV
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_diff_small
CON,FIELD=REFORM(hadgem3_wet_composite(1,*,*)-hadgem3_wet_composite(0,*,*)),X=hadgem3_longitude,Y=hadgem3_latitude,$
    TITLE="JJAS diff in composite of wettest "+STRTRIM(STRING(nyears_wet),1)+" years from 1.5x minus 1.0x",CB_TITLE='mm day!U-1!N',$
    /NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_air_wetvdry.1.5x-minus-1.0x.dry_composite.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=15000,SPACE3=500
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff_small)+1,/REV
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_diff_small
CON,FIELD=REFORM(hadgem3_dry_composite(1,*,*)-hadgem3_dry_composite(0,*,*)),X=hadgem3_longitude,Y=hadgem3_latitude,$
    TITLE="JJAS diff in composite of driest "+STRTRIM(STRING(nyears_dry),1)+" years from 1.5x minus 1.0x",CB_TITLE='mm day!U-1!N',$
    /NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_air_wetvdry.1.5x-minus-1.0x.wet-minus-dry_composite.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=15000,SPACE3=500
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff_small)+1,/REV
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_diff_small
CON,FIELD=REFORM((hadgem3_wet_composite(1,*,*)-hadgem3_dry_composite(1,*,*))-(hadgem3_wet_composite(0,*,*)-hadgem3_dry_composite(0,*,*))),$
    X=hadgem3_longitude,Y=hadgem3_latitude,$
    TITLE="JJAS diff in diff of wettest "+STRTRIM(STRING(nyears_wet),1)+" yr minus driest "+STRTRIM(STRING(nyears_dry),1)+$
    " from 1.5x minus 1.0x",CB_TITLE='mm day!U-1!N',$
    /NOLINES
PSCLOSE,/NOVIEW

STOP
END
