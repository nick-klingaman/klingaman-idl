PRO hadgem3_monwg_jjas_precip_air_wetvdry_uv850_hg3kpp

aent_daily_precip_file='/home/ss901165/um_output6/xgspj/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmeans.years1-20.precip.nc'
aent_clim_precip_file='/home/ss901165/um_output6/xgspj/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.precip.nc'
aent_daily_u850_file='/home/ss901165/um_output6/xgspj/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmeans.years1-20.u850.nc'
aent_clim_u850_file='/home/ss901165/um_output6/xgspj/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.u850.nc'
aent_daily_v850_file='/home/ss901165/um_output6/xgspj/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmeans.years1-20.v850.nc'
aent_clim_v850_file='/home/ss901165/um_output6/xgspj/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.v850.nc'

k50ent_daily_precip_file='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans.years1-27.precip.nc'
k50ent_clim_precip_file='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-27.precip.nc'
k50ent_daily_u850_file='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans.years1-27.u850.nc'
k50ent_clim_u850_file='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-27.u850.nc'
k50ent_daily_v850_file='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans.years1-27.v850.nc'
k50ent_clim_v850_file='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-27.v850.nc'

a50ent_daily_precip_file='/home/ss901165/um_output6/xihvg/hadgem3a_kpp50N50S_1.5xentrain_ga30.jan-dec_dmeans.years1-24.precip.nc'
a50ent_clim_precip_file='/home/ss901165/um_output6/xihvg/hadgem3a_kpp50N50S_1.5xentrain_ga30.jan-dec_dmean_clim.years1-24.precip.nc'
a50ent_daily_u850_file='/home/ss901165/um_output6/xihvg/hadgem3a_kpp50N50S_1.5xentrain_ga30.jan-dec_dmeans.years1-24.u850.nc'
a50ent_clim_u850_file='/home/ss901165/um_output6/xihvg/hadgem3a_kpp50N50S_1.5xentrain_ga30.jan-dec_dmean_clim.years1-24.u850.nc'
a50ent_daily_v850_file='/home/ss901165/um_output6/xihvg/hadgem3a_kpp50N50S_1.5xentrain_ga30.jan-dec_dmeans.years1-24.v850.nc'
a50ent_clim_v850_file='/home/ss901165/um_output6/xihvg/hadgem3a_kpp50N50S_1.5xentrain_ga30.jan-dec_dmean_clim.years1-24.v850.nc'

trmm_clim_file='/home/ss901165/datasets/TRMM_3B42V6/n96/TRMM_3B42v6A.jan-dec_dmean_clim.1999-2011.n96.nc'

n96_mask_file='/home/ss901165/um_output/mask_n96.nc'

; Box to area average
box=[15,75,25,85]

; Box to plot
box_plot=[-10,50,35,180]

max_years=27
nyears_wet=7
nyears_dry=7

mylevs_raw=['2','3','4','5','6','7','8','9','10','12','14','16','18','20','22','24']
mylevs_diff_small=['-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5']
mylevs_diff=['-11','-9','-7','-5','-3','-1','1','3','5','7','9','11']

n_sets_plot=3

FOR k=0,n_sets_plot-1 DO BEGIN
   CASE k OF
      0 : BEGIN
         hadgem3_daily_precip_file=aent_daily_precip_file         
         hadgem3_clim_precip_file=aent_clim_precip_file
         hadgem3_daily_u850_file=aent_daily_u850_file
         hadgem3_daily_v850_file=aent_daily_v850_file
;         hadgem3_daily_q850_file=aent_daily_q850_file
         hadgem3_clim_u850_file=aent_clim_u850_file
         hadgem3_clim_v850_file=aent_clim_v850_file
;         hadgem3_clim_q850_file=aent_clim_q850_file
         offset_jun1=150
         offset_sep30=269
         hadgem3_nyears=20
         runid='hadgem3a_ukmo_1.5xentrain_ga30'
         description='A-ENT-OBS (20 years)'         
      END
      1 : BEGIN
         hadgem3_daily_precip_file=k50ent_daily_precip_file         
         hadgem3_clim_precip_file=k50ent_clim_precip_file
         hadgem3_daily_u850_file=k50ent_daily_u850_file
         hadgem3_daily_v850_file=k50ent_daily_v850_file
;         hadgem3_daily_q850_file=k50ent_daily_q850_file
         hadgem3_clim_u850_file=k50ent_clim_u850_file
         hadgem3_clim_v850_file=k50ent_clim_v850_file
;         hadgem3_clim_q850_file=k50ent_clim_q850_file
         offset_jun1=150
         offset_sep30=269
         hadgem3_nyears=27
         runid='hadgem3kpp_1.5xentrain_ga30_50N50S'
         description='K50-ENT-OBS (27 years)'
      END
      2 : BEGIN
         hadgem3_daily_precip_file=a50ent_daily_precip_file         
         hadgem3_clim_precip_file=a50ent_clim_precip_file
         hadgem3_daily_u850_file=a50ent_daily_u850_file
         hadgem3_daily_v850_file=a50ent_daily_v850_file
;         hadgem3_daily_q850_file=a50ent_daily_q850_file
         hadgem3_clim_u850_file=a50ent_clim_u850_file
         hadgem3_clim_v850_file=a50ent_clim_v850_file
;         hadgem3_clim_q850_file=a50ent_clim_q850_file
         offset_jun1=150
         offset_sep30=269
         hadgem3_nyears=24
         runid='hadgem3a_kpp50N50S_1.5xentrain_ga30'
         description='A-ENT-K50 (20 years)'
      END
   ENDCASE
   n_time=offset_sep30-offset_jun1+1

                                ; Get precip grid information
   hadgem3_precip_longitude=OPEN_AND_EXTRACT(hadgem3_daily_precip_file,'longitude')
   hadgem3_precip_latitude=OPEN_AND_EXTRACT(hadgem3_daily_precip_file,'latitude')
   DEFINE_BOUNDARIES,box,hadgem3_precip_latitude,hadgem3_precip_longitude,hadgem3_precip_box_tx,/LIMIT
   hadgem3_precip_nlon=N_ELEMENTS(hadgem3_precip_longitude)
   hadgem3_precip_nlat=N_ELEMENTS(hadgem3_precip_latitude)
   
   trmm_precip_longitude=OPEN_AND_EXTRACT(trmm_clim_file,'longitude')
   trmm_precip_latitude=OPEN_AND_EXTRACT(trmm_clim_file,'latitude')
   DEFINE_BOUNDARIES,box_plot,trmm_precip_latitude,trmm_precip_longitude,trmm_precip_box_tx,/LIMIT
   trmm_precip_nlon=N_ELEMENTS(trmm_precip_longitude)
   trmm_precip_nlat=N_ELEMENTS(trmm_precip_latitude)

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
   FOR i=0,hadgem3_precip_nlon-1 DO $
      FOR j=0,hadgem3_precip_nlat-1 DO $
         n96_mask_rev(i,hadgem3_precip_nlat-j-1)=n96_mask(i,j)

   hadgem3_jjas_ts=fltarr(hadgem3_nyears)
   FOR i=0,hadgem3_nyears-1 DO BEGIN
      hadgem3_year=REFORM(OPEN_AND_EXTRACT(hadgem3_daily_precip_file,'precip',$
                                           offset=[hadgem3_precip_box_tx(1),hadgem3_precip_box_tx(0),offset_jun1,i],$
                                           count=[hadgem3_precip_nlon,hadgem3_precip_nlat,n_time,1]))*86400.      
      hadgem3_year_aavg=fltarr(n_time)
      FOR j=0,n_time-1 DO BEGIN
         temp_precip=REFORM(hadgem3_year(*,*,j))
         temp_precip[where(n96_mask_rev eq 0)]=!Values.F_NaN
         hadgem3_year_aavg(j)=MEAN(temp_precip,/NaN)
      ENDFOR
      hadgem3_jjas_ts(i)=MEAN(hadgem3_year_aavg)
   ENDFOR
   ;print,STDDEV(hadgem3_jjas_ts),STDDEV(hadgem3_jjas_ts)/MEAN(hadgem3_jjas_ts)

   hadgem3_jjas_ts_sorted=SORT(hadgem3_jjas_ts)
   wettest_years=hadgem3_jjas_ts_sorted(hadgem3_nyears-nyears_wet-1:hadgem3_nyears-1)
   driest_years=hadgem3_jjas_ts_sorted(0:nyears_dry-1)

                                ; Get u/v grid information
   hadgem3_u_longitude=OPEN_AND_EXTRACT(hadgem3_daily_u850_file,'longitude')
   hadgem3_u_latitude=OPEN_AND_EXTRACT(hadgem3_daily_u850_file,'latitude')
   DEFINE_BOUNDARIES,box_plot,hadgem3_u_latitude,hadgem3_u_longitude,hadgem3_u_box_plot_tx,/LIMIT
   hadgem3_u_nlon=N_ELEMENTS(hadgem3_u_longitude)
   hadgem3_u_nlat=N_ELEMENTS(hadgem3_u_latitude)
                                   ; Get q grid information
   hadgem3_precip_longitude=OPEN_AND_EXTRACT(hadgem3_daily_precip_file,'longitude')
   hadgem3_precip_latitude=OPEN_AND_EXTRACT(hadgem3_daily_precip_file,'latitude')
   DEFINE_BOUNDARIES,box_plot,hadgem3_precip_latitude,hadgem3_precip_longitude,hadgem3_precip_box_plot_tx,/LIMIT
   hadgem3_precip_nlon=N_ELEMENTS(hadgem3_precip_longitude)
   hadgem3_precip_nlat=N_ELEMENTS(hadgem3_precip_latitude)

   IF k eq 0 THEN BEGIN
      hadgem3_wet_composite_u850=fltarr(n_sets_plot,hadgem3_u_nlon,hadgem3_u_nlat)
      hadgem3_wet_composite_v850=fltarr(n_sets_plot,hadgem3_u_nlon,hadgem3_u_nlat)
      hadgem3_dry_composite_u850=fltarr(n_sets_plot,hadgem3_u_nlon,hadgem3_u_nlat)
      hadgem3_dry_composite_v850=fltarr(n_sets_plot,hadgem3_u_nlon,hadgem3_u_nlat)
      hadgem3_wet_composite_precip=fltarr(n_sets_plot,hadgem3_precip_nlon,hadgem3_precip_nlat)
      hadgem3_dry_composite_precip=fltarr(n_sets_plot,hadgem3_precip_nlon,hadgem3_precip_nlat)
   ENDIF
   FOR i=0,nyears_wet-1 DO BEGIN
      hadgem3_u850_year=REFORM(OPEN_AND_EXTRACT(hadgem3_daily_u850_file,'u',$
                                                offset=[hadgem3_u_box_plot_tx(1),hadgem3_u_box_plot_tx(0),offset_jun1,wettest_years(i)],$
                                                count=[hadgem3_u_nlon,hadgem3_u_nlat,n_time,1]))
      hadgem3_v850_year=REFORM(OPEN_AND_EXTRACT(hadgem3_daily_v850_file,'v',$
                                                offset=[hadgem3_u_box_plot_tx(1),hadgem3_u_box_plot_tx(0),offset_jun1,wettest_years(i)],$
                                                count=[hadgem3_u_nlon,hadgem3_u_nlat,n_time,1]))
      hadgem3_precip_year=REFORM(OPEN_AND_EXTRACT(hadgem3_daily_precip_file,'precip',$
                                                offset=[hadgem3_precip_box_plot_tx(1),hadgem3_precip_box_plot_tx(0),offset_jun1,wettest_years(i)],$
                                                count=[hadgem3_precip_nlon,hadgem3_precip_nlat,n_time,1]))*86400.
      FOR m=0,hadgem3_u_nlon-1 DO BEGIN
         FOR n=0,hadgem3_u_nlat-1 DO BEGIN
            hadgem3_wet_composite_u850(k,m,n)=MEAN(hadgem3_u850_year(m,n,*))/FLOAT(nyears_wet)+hadgem3_wet_composite_u850(k,m,n)
            hadgem3_wet_composite_v850(k,m,n)=MEAN(hadgem3_v850_year(m,n,*))/FLOAT(nyears_wet)+hadgem3_wet_composite_v850(k,m,n)                        
         ENDFOR
      ENDFOR
      FOR m=0,hadgem3_precip_nlon-1 DO $
         FOR n=0,hadgem3_precip_nlat-1 DO $
            hadgem3_wet_composite_precip(k,m,n)=MEAN(hadgem3_precip_year(m,n,*))/FLOAT(nyears_wet)+hadgem3_wet_composite_precip(k,m,n)
   ENDFOR
   FOR i=0,nyears_dry-1 DO BEGIN
      hadgem3_u850_year=REFORM(OPEN_AND_EXTRACT(hadgem3_daily_u850_file,'u',$
                                                offset=[hadgem3_u_box_plot_tx(1),hadgem3_u_box_plot_tx(0),offset_jun1,driest_years(i)],$
                                                count=[hadgem3_u_nlon,hadgem3_u_nlat,n_time,1]))
      hadgem3_v850_year=REFORM(OPEN_AND_EXTRACT(hadgem3_daily_v850_file,'v',$
                                                offset=[hadgem3_u_box_plot_tx(1),hadgem3_u_box_plot_tx(0),offset_jun1,driest_years(i)],$
                                                count=[hadgem3_u_nlon,hadgem3_u_nlat,n_time,1]))
      hadgem3_precip_year=REFORM(OPEN_AND_EXTRACT(hadgem3_daily_precip_file,'precip',$
                                                offset=[hadgem3_precip_box_plot_tx(1),hadgem3_precip_box_plot_tx(0),offset_jun1,driest_years(i)],$
                                                count=[hadgem3_precip_nlon,hadgem3_precip_nlat,n_time,1]))*86400.
      FOR m=0,hadgem3_u_nlon-1 DO BEGIN
         FOR n=0,hadgem3_u_nlat-1 DO BEGIN
            hadgem3_dry_composite_u850(k,m,n)=MEAN(hadgem3_u850_year(m,n,*))/FLOAT(nyears_dry)+hadgem3_dry_composite_u850(k,m,n)
            hadgem3_dry_composite_v850(k,m,n)=MEAN(hadgem3_v850_year(m,n,*))/FLOAT(nyears_dry)+hadgem3_dry_composite_v850(k,m,n)
         ENDFOR
      ENDFOR
      FOR m=0,hadgem3_precip_nlon-1 DO $
         FOR n=0,hadgem3_precip_nlat-1 DO $
            hadgem3_dry_composite_precip(k,m,n)=MEAN(hadgem3_precip_year(m,n,*))/FLOAT(nyears_dry)+hadgem3_dry_composite_precip(k,m,n)
   ENDFOR

   hadgem3_u850_clim=REFORM(OPEN_AND_EXTRACT(hadgem3_clim_u850_file,'u',$
                                             offset=[hadgem3_u_box_plot_tx(1),hadgem3_u_box_plot_tx(0),offset_jun1],$
                                             count=[hadgem3_u_nlon,hadgem3_u_nlat,n_time]))
   hadgem3_v850_clim=REFORM(OPEN_AND_EXTRACT(hadgem3_clim_v850_file,'v',$
                                             offset=[hadgem3_u_box_plot_tx(1),hadgem3_u_box_plot_tx(0),offset_jun1],$
                                             count=[hadgem3_u_nlon,hadgem3_u_nlat,n_time]))
   hadgem3_precip_clim=REFORM(OPEN_AND_EXTRACT(hadgem3_clim_precip_file,'precip',$
                                             offset=[hadgem3_precip_box_plot_tx(1),hadgem3_precip_box_plot_tx(0),offset_jun1],$
                                             count=[hadgem3_precip_nlon,hadgem3_precip_nlat,n_time]))*86400.
   hadgem3_u850_clim_smean=fltarr(hadgem3_u_nlon,hadgem3_u_nlat)
   hadgem3_v850_clim_smean=fltarr(hadgem3_u_nlon,hadgem3_u_nlat)
   hadgem3_precip_clim_smean=fltarr(hadgem3_precip_nlon,hadgem3_precip_nlat)
   FOR m=0,hadgem3_u_nlon-1 DO BEGIN
      FOR n=0,hadgem3_u_nlat-1 DO BEGIN
         hadgem3_u850_clim_smean(m,n)=MEAN(hadgem3_u850_clim(m,n,*))
         hadgem3_v850_clim_smean(m,n)=MEAN(hadgem3_v850_clim(m,n,*))
      ENDFOR
   ENDFOR  
   FOR m=0,hadgem3_precip_nlon-1 DO $
      FOR n=0,hadgem3_precip_nlat-1 DO $
         hadgem3_precip_clim_smean(m,n)=MEAN(hadgem3_precip_clim(m,n,*))

   trmm_precip=OPEN_AND_EXTRACT(trmm_clim_file,'precip',offset=[trmm_precip_box_tx(1),trmm_precip_box_tx(0),150],$
                                count=[trmm_precip_nlon,trmm_precip_nlat,120])
   trmm_smean=fltarr(trmm_precip_nlon,trmm_precip_nlat)
   FOR m=0,trmm_precip_nlon-1 DO $
      FOR n=0,trmm_precip_nlat-1 DO $
         trmm_smean(m,n)=MEAN(trmm_precip(m,trmm_precip_nlat-n-1,*))

   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_air_wetvdry_uv850.'+runid+'.wet_composite_zoom.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=16000,SPACE3=500
   CS,SCALE=40,NCOLS=N_ELEMENTS(mylevs_raw)+1,/REV,white=[2]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_raw
   CON,FIELD=REFORM(hadgem3_wet_composite_precip(k,*,*)),X=hadgem3_precip_longitude,Y=hadgem3_precip_latitude,$
       TITLE="JJAS U850 and precip for composite of wettest "+STRTRIM(STRING(nyears_wet),1)+" years from "+runid,CB_TITLE='mm day!U-1!N',$
       /NOLINES
   VECT,X=hadgem3_u_longitude,Y=hadgem3_u_latitude,U=REFORM(hadgem3_wet_composite_u850(k,*,*)),V=REFORM(hadgem3_wet_composite_v850(k,*,*)),MAG=5;,$
                                ;STRIDE=3
   ;AXES,XSTEP=20,YSTEP=10
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_air_wetvdry_uv850.'+runid+'.dry_composite_zoom.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=16000,SPACE3=500
   CS,SCALE=40,NCOLS=N_ELEMENTS(mylevs_raw)+1,/REV,white=[2]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_raw
   CON,FIELD=REFORM(hadgem3_dry_composite_precip(k,*,*)),X=hadgem3_precip_longitude,Y=hadgem3_precip_latitude,$
       TITLE="JJAS U850 and precip for composite of driest "+STRTRIM(STRING(nyears_dry),1)+" years from "+runid,CB_TITLE='mm day!U-1!N',$
       /NOLINES
   VECT,X=hadgem3_u_longitude,Y=hadgem3_u_latitude,U=REFORM(hadgem3_dry_composite_u850(k,*,*)),V=REFORM(hadgem3_wet_composite_v850(k,*,*)),MAG=5;,$
        ;STRIDE=3
   ;AXES,XSTEP=20,YSTEP=10
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_air_wetvdry_uv850.'+runid+'.wet_composite_anom_zoom.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=16000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff_small)+1,/REV,white=[9]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_diff_small
   CON,FIELD=REFORM(hadgem3_wet_composite_precip(k,*,*))-hadgem3_precip_clim_smean,X=hadgem3_precip_longitude,Y=hadgem3_precip_latitude,$
       TITLE="JJAS anom U850 and precip for composite of wettest "+STRTRIM(STRING(nyears_wet),1)+" years from "+runid,CB_TITLE='mm day!U-1!N',$
       /NOLINES
   VECT,X=hadgem3_u_longitude,Y=hadgem3_u_latitude,U=REFORM(hadgem3_wet_composite_u850(k,*,*))-hadgem3_u850_clim_smean,$
        V=REFORM(hadgem3_wet_composite_v850(k,*,*))-hadgem3_v850_clim_smean,MAG=1;,STRIDE=3
   ;AXES,XSTEP=20,YSTEP=10
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_air_wetvdry_uv850.'+runid+'.dry_composite_anom_zoom.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=16000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff_small)+1,/REV,white=[9]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_diff_small
   CON,FIELD=REFORM(hadgem3_dry_composite_precip(k,*,*))-hadgem3_precip_clim_smean,X=hadgem3_precip_longitude,Y=hadgem3_precip_latitude,$
       TITLE="JJAS anom U850 and precip for composite of driest "+STRTRIM(STRING(nyears_dry),1)+" years from "+runid,CB_TITLE='mm day!U-1!N',$
       /NOLINES
   VECT,X=hadgem3_u_longitude,Y=hadgem3_u_latitude,U=REFORM(hadgem3_dry_composite_u850(k,*,*))-hadgem3_u850_clim_smean,$
        V=REFORM(hadgem3_dry_composite_v850(k,*,*))-hadgem3_v850_clim_smean,MAG=1;,STRIDE=3
   ;AXES,XSTEP=20,YSTEP=10
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_air_wetvdry_uv850.'+runid+'.wet-minus-dry_composite_zoom.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=16000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff_small)+1,/REV,white=[9]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_diff_small
   CON,FIELD=REFORM(hadgem3_wet_composite_precip(k,*,*)-hadgem3_dry_composite_precip(k,*,*)),X=hadgem3_precip_longitude,Y=hadgem3_precip_latitude,$
       TITLE="JJAS U850 and precip for diff wettest "+STRTRIM(STRING(nyears_wet),1)+" yr minus driest "+STRTRIM(STRING(nyears_dry),1)+$
       " from "+runid,CB_TITLE='mm day!U-1!N',$
       /NOLINES
   VECT,X=hadgem3_u_longitude,Y=hadgem3_u_latitude,U=REFORM(hadgem3_wet_composite_u850(k,*,*)-hadgem3_dry_composite_u850(k,*,*)),$
        V=REFORM(hadgem3_wet_composite_v850(k,*,*)-hadgem3_dry_composite_v850(k,*,*)),MAG=1;,STRIDE=3
   ;AXES,XSTEP=20,YSTEP=10
   PSCLOSE
ENDFOR
STOP

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_air_wetvdry_uv850.k50entobs-minus-aentobs.wet_composite_zoom.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=16000,SPACE3=500
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff_small)+1,/REV,white=[9]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_diff_small
CON,FIELD=REFORM(hadgem3_wet_composite_precip(1,*,*)-hadgem3_wet_composite_precip(0,*,*)),X=hadgem3_precip_longitude,Y=hadgem3_precip_latitude,$
    TITLE="JJAS diff in composite of U850 and precip for wettest "+STRTRIM(STRING(nyears_wet),1)+" years - K50-ENT-OBS minus A-ENT-OBS",CB_TITLE='mm day!U-1!N',$
    /NOLINES
VECT,X=hadgem3_u_longitude,Y=hadgem3_u_latitude,U=REFORM(hadgem3_wet_composite_u850(1,*,*)-hadgem3_wet_composite_u850(0,*,*)),$
     V=REFORM(hadgem3_wet_composite_v850(1,*,*)-hadgem3_wet_composite_v850(0,*,*)),MAG=1;,STRIDE=3
;AXES,XSTEP=20,YSTEP=10
PSCLOSE

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_air_wetvdry_uv850.k50entobs-minus-aentobs.dry_composite_zoom.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=16000,SPACE3=500
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff_small)+1,/REV,white=[9]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_diff_small
CON,FIELD=REFORM(hadgem3_dry_composite_precip(1,*,*)-hadgem3_dry_composite_precip(0,*,*)),X=hadgem3_precip_longitude,Y=hadgem3_precip_latitude,$
    TITLE="JJAS diff in composite of U850 and precip for driest "+STRTRIM(STRING(nyears_dry),1)+" years - K50-ENT-OBS minus A-ENT-OBS",CB_TITLE='mm day!U-1!N',$
    /NOLINES
VECT,X=hadgem3_u_longitude,Y=hadgem3_u_latitude,U=REFORM(hadgem3_dry_composite_u850(1,*,*)-hadgem3_dry_composite_u850(0,*,*)),$
     V=REFORM(hadgem3_dry_composite_v850(1,*,*)-hadgem3_dry_composite_v850(0,*,*)),MAG=1;,STRIDE=3
;AXES,XSTEP=20,YSTEP=10
PSCLOSE

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_air_wetvdry_uv850.k50entobs-minus-aentobs.wet-minus-dry_composite_zoom.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=16000,SPACE3=500
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff_small)+1,/REV,white=[9]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_diff_small
CON,FIELD=REFORM((hadgem3_wet_composite_precip(1,*,*)-hadgem3_dry_composite_precip(1,*,*))-(hadgem3_wet_composite_precip(0,*,*)-hadgem3_dry_composite_precip(0,*,*))),$
    X=hadgem3_precip_longitude,Y=hadgem3_precip_latitude,$
    TITLE="JJAS diff of diff of U850 and precip for wettest "+STRTRIM(STRING(nyears_wet),1)+" yr minus driest "+STRTRIM(STRING(nyears_dry),1)+$
    " - K50-ENT-OBS minus A-ENT-OBS",CB_TITLE='mm day!U-1!N',$
    /NOLINES
VECT,X=hadgem3_u_longitude,Y=hadgem3_u_latitude,U=REFORM((hadgem3_wet_composite_u850(1,*,*)-hadgem3_dry_composite_u850(1,*,*))-$
                                                         (hadgem3_wet_composite_u850(0,*,*)-hadgem3_dry_composite_u850(0,*,*))),$
     V=REFORM((hadgem3_wet_composite_v850(1,*,*)-hadgem3_dry_composite_v850(1,*,*))-$
              (hadgem3_wet_composite_v850(0,*,*)-hadgem3_dry_composite_v850(0,*,*))),$
     MAG=1;,STRIDE=3
;AXES,XSTEP=20,YSTEP=10
PSCLOSE

STOP
END
