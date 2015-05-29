PRO hadgem3_monwg_jjas_precip_air_wetvdry_uv850

nick_1xentrain_daily_precip_file='/home/ss901165/um_output3/hadgem3a_amip2_control_vn74/hadgem3a_amip2_ctl_vn74.jan-dec_dmeans.years1-77.precip.nc'
nick_1xentrain_daily_u850_file='/home/ss901165/um_output3/hadgem3a_amip2_control_vn74/hadgem3a_amip2_ctl_vn74.jan-dec_dmeans.years1-77.u850.nc'
nick_1xentrain_daily_v850_file='/home/ss901165/um_output3/hadgem3a_amip2_control_vn74/hadgem3a_amip2_ctl_vn74.jan-dec_dmeans.years1-77.v850.nc'
nick_1xentrain_daily_q850_file='/home/ss901165/um_output3/hadgem3a_amip2_control_vn74/hadgem3a_amip2_ctl_vn74.jan-dec_dmeans.years1-77.q850.nc'
nick_1xentrain_clim_precip_file='/home/ss901165/um_output3/hadgem3a_amip2_control_vn74/hadgem3a_amip2_ctl_vn74.jan-dec_dmean_clim.years1-77.precip.nc'
nick_1xentrain_clim_u850_file='/home/ss901165/um_output3/hadgem3a_amip2_control_vn74/hadgem3a_amip2_ctl_vn74.jan-dec_dmean_clim.years1-77.u850.nc'
nick_1xentrain_clim_v850_file='/home/ss901165/um_output3/hadgem3a_amip2_control_vn74/hadgem3a_amip2_ctl_vn74.jan-dec_dmean_clim.years1-77.v850.nc'
nick_1xentrain_clim_q850_file='/home/ss901165/um_output3/hadgem3a_amip2_control_vn74/hadgem3a_amip2_ctl_vn74.jan-dec_dmean_clim.years1-77.q850.nc'

nick_15xentrain_daily_precip_file='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmeans.years1-77.precip.nc'
nick_15xentrain_daily_u850_file='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmeans.years1-77.u850.nc'
nick_15xentrain_daily_v850_file='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmeans.years1-77.v850.nc'
nick_15xentrain_daily_q850_file='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmeans.years1-77.q850.nc'
nick_15xentrain_clim_precip_file='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmean_clim.years1-77.precip.nc'
nick_15xentrain_clim_u850_file='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmean_clim.years1-77.u850.nc'
nick_15xentrain_clim_v850_file='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmean_clim.years1-77.v850.nc'
nick_15xentrain_clim_q850_file='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmean_clim.years1-77.q850.nc'

n96_mask_file='/home/ss901165/um_output/mask_n96.nc'

; Box to area average
box=[10,70,30,88]

; Box to plot
box_plot=[-40,40,40,200]

max_years=77
nyears_wet=10
nyears_dry=10

mylevs_raw=['6.5','7.0','7.5','8.0','8.5','9.0','9.5','10.0','10.5','11.0','11.5','12.0','12.5','13.0','13.5','14.0','14.5','15.0']
mylevs_diff_small=['-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5']
mylevs_diff=['-3.0','-2.6','-2.2','-1.8','-1.4','-1.0','-0.6','-0.2','0.2','0.6','1.0','1.4','1.8','2.2','2.6','3.0']

n_sets_plot=2

FOR k=0,n_sets_plot-1 DO BEGIN
   CASE k OF
      0 : BEGIN
         hadgem3_daily_precip_file=nick_1xentrain_daily_precip_file         
         hadgem3_clim_precip_file=nick_1xentrain_clim_precip_file
         hadgem3_daily_u850_file=nick_1xentrain_daily_u850_file
         hadgem3_daily_v850_file=nick_1xentrain_daily_v850_file
         hadgem3_daily_q850_file=nick_1xentrain_daily_q850_file
         hadgem3_clim_u850_file=nick_1xentrain_clim_u850_file
         hadgem3_clim_v850_file=nick_1xentrain_clim_v850_file
         hadgem3_clim_q850_file=nick_1xentrain_clim_q850_file
         offset_jun1=150
         offset_sep30=269
         hadgem3_nyears=77
         runid='hadgem3a_amip2_1.0xentrain_vn74'
         description='Nick HadGEM3-A (vn7.4) 1.0x entrain with AMIP2 clim SSTs (77 years)'        
      END
      1 : BEGIN
         hadgem3_daily_precip_file=nick_15xentrain_daily_precip_file         
         hadgem3_clim_precip_file=nick_15xentrain_clim_precip_file
         hadgem3_daily_u850_file=nick_15xentrain_daily_u850_file
         hadgem3_daily_v850_file=nick_15xentrain_daily_v850_file
         hadgem3_daily_q850_file=nick_15xentrain_daily_q850_file
         hadgem3_clim_u850_file=nick_15xentrain_clim_u850_file
         hadgem3_clim_v850_file=nick_15xentrain_clim_v850_file
         hadgem3_clim_q850_file=nick_15xentrain_clim_q850_file
         offset_jun1=150
         offset_sep30=269
         hadgem3_nyears=77
         runid='hadgem3a_amip2_1.5xentrain_vn74'
         description='Nick HadGEM3-A (vn7.4) 1.5x entrain with AMIP2 clim SSTs (77 years)'
      END
   ENDCASE
   n_time=offset_sep30-offset_jun1+1

                                ; Get precip grid information
   hadgem3_precip_longitude=OPEN_AND_EXTRACT(hadgem3_daily_precip_file,'longitude')
   hadgem3_precip_latitude=OPEN_AND_EXTRACT(hadgem3_daily_precip_file,'latitude')
   DEFINE_BOUNDARIES,box,hadgem3_precip_latitude,hadgem3_precip_longitude,hadgem3_precip_box_tx,/LIMIT
   hadgem3_precip_nlon=N_ELEMENTS(hadgem3_precip_longitude)
   hadgem3_precip_nlat=N_ELEMENTS(hadgem3_precip_latitude)

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
   hadgem3_q_longitude=OPEN_AND_EXTRACT(hadgem3_daily_u850_file,'longitude')
   hadgem3_q_latitude=OPEN_AND_EXTRACT(hadgem3_daily_u850_file,'latitude')
   DEFINE_BOUNDARIES,box_plot,hadgem3_q_latitude,hadgem3_q_longitude,hadgem3_q_box_plot_tx,/LIMIT
   hadgem3_q_nlon=N_ELEMENTS(hadgem3_q_longitude)
   hadgem3_q_nlat=N_ELEMENTS(hadgem3_q_latitude)

   IF k eq 0 THEN BEGIN
      hadgem3_wet_composite_u850=fltarr(n_sets_plot,hadgem3_u_nlon,hadgem3_u_nlat)
      hadgem3_wet_composite_v850=fltarr(n_sets_plot,hadgem3_u_nlon,hadgem3_u_nlat)
      hadgem3_dry_composite_u850=fltarr(n_sets_plot,hadgem3_u_nlon,hadgem3_u_nlat)
      hadgem3_dry_composite_v850=fltarr(n_sets_plot,hadgem3_u_nlon,hadgem3_u_nlat)
      hadgem3_wet_composite_q850=fltarr(n_sets_plot,hadgem3_q_nlon,hadgem3_q_nlat)
      hadgem3_dry_composite_q850=fltarr(n_sets_plot,hadgem3_q_nlon,hadgem3_q_nlat)
   ENDIF
   FOR i=0,nyears_wet-1 DO BEGIN
      hadgem3_u850_year=REFORM(OPEN_AND_EXTRACT(hadgem3_daily_u850_file,'u',$
                                                offset=[hadgem3_u_box_plot_tx(1),hadgem3_u_box_plot_tx(0),offset_jun1,wettest_years(i)],$
                                                count=[hadgem3_u_nlon,hadgem3_u_nlat,n_time,1]))
      hadgem3_v850_year=REFORM(OPEN_AND_EXTRACT(hadgem3_daily_v850_file,'v',$
                                                offset=[hadgem3_u_box_plot_tx(1),hadgem3_u_box_plot_tx(0),offset_jun1,wettest_years(i)],$
                                                count=[hadgem3_u_nlon,hadgem3_u_nlat,n_time,1]))
      hadgem3_q850_year=REFORM(OPEN_AND_EXTRACT(hadgem3_daily_q850_file,'q',$
                                                offset=[hadgem3_q_box_plot_tx(1),hadgem3_q_box_plot_tx(0),offset_jun1,wettest_years(i)],$
                                                count=[hadgem3_q_nlon,hadgem3_q_nlat,n_time,1]))*1000.
      FOR m=0,hadgem3_u_nlon-1 DO BEGIN
         FOR n=0,hadgem3_u_nlat-1 DO BEGIN
            hadgem3_wet_composite_u850(k,m,n)=MEAN(hadgem3_u850_year(m,n,*))/FLOAT(nyears_wet)+hadgem3_wet_composite_u850(k,m,n)
            hadgem3_wet_composite_v850(k,m,n)=MEAN(hadgem3_v850_year(m,n,*))/FLOAT(nyears_wet)+hadgem3_wet_composite_v850(k,m,n)                        
         ENDFOR
      ENDFOR
      FOR m=0,hadgem3_q_nlon-1 DO $
         FOR n=0,hadgem3_q_nlat-1 DO $
            hadgem3_wet_composite_q850(k,m,n)=MEAN(hadgem3_q850_year(m,n,*))/FLOAT(nyears_wet)+hadgem3_wet_composite_q850(k,m,n)
   ENDFOR
   FOR i=0,nyears_dry-1 DO BEGIN
      hadgem3_u850_year=REFORM(OPEN_AND_EXTRACT(hadgem3_daily_u850_file,'u',$
                                                offset=[hadgem3_u_box_plot_tx(1),hadgem3_u_box_plot_tx(0),offset_jun1,driest_years(i)],$
                                                count=[hadgem3_u_nlon,hadgem3_u_nlat,n_time,1]))
      hadgem3_v850_year=REFORM(OPEN_AND_EXTRACT(hadgem3_daily_v850_file,'v',$
                                                offset=[hadgem3_u_box_plot_tx(1),hadgem3_u_box_plot_tx(0),offset_jun1,driest_years(i)],$
                                                count=[hadgem3_u_nlon,hadgem3_u_nlat,n_time,1]))
      hadgem3_q850_year=REFORM(OPEN_AND_EXTRACT(hadgem3_daily_q850_file,'q',$
                                                offset=[hadgem3_q_box_plot_tx(1),hadgem3_q_box_plot_tx(0),offset_jun1,driest_years(i)],$
                                                count=[hadgem3_q_nlon,hadgem3_q_nlat,n_time,1]))*1000.
      FOR m=0,hadgem3_u_nlon-1 DO BEGIN
         FOR n=0,hadgem3_u_nlat-1 DO BEGIN
            hadgem3_dry_composite_u850(k,m,n)=MEAN(hadgem3_u850_year(m,n,*))/FLOAT(nyears_dry)+hadgem3_dry_composite_u850(k,m,n)
            hadgem3_dry_composite_v850(k,m,n)=MEAN(hadgem3_v850_year(m,n,*))/FLOAT(nyears_dry)+hadgem3_dry_composite_v850(k,m,n)
         ENDFOR
      ENDFOR
      FOR m=0,hadgem3_q_nlon-1 DO $
         FOR n=0,hadgem3_q_nlat-1 DO $
            hadgem3_dry_composite_q850(k,m,n)=MEAN(hadgem3_q850_year(m,n,*))/FLOAT(nyears_dry)+hadgem3_dry_composite_q850(k,m,n)
   ENDFOR

   hadgem3_u850_clim=REFORM(OPEN_AND_EXTRACT(hadgem3_clim_u850_file,'u',$
                                             offset=[hadgem3_u_box_plot_tx(1),hadgem3_u_box_plot_tx(0),offset_jun1],$
                                             count=[hadgem3_u_nlon,hadgem3_u_nlat,n_time]))
   hadgem3_v850_clim=REFORM(OPEN_AND_EXTRACT(hadgem3_clim_v850_file,'v',$
                                             offset=[hadgem3_u_box_plot_tx(1),hadgem3_u_box_plot_tx(0),offset_jun1],$
                                             count=[hadgem3_u_nlon,hadgem3_u_nlat,n_time]))
   hadgem3_q850_clim=REFORM(OPEN_AND_EXTRACT(hadgem3_clim_q850_file,'q',$
                                             offset=[hadgem3_q_box_plot_tx(1),hadgem3_q_box_plot_tx(0),offset_jun1],$
                                             count=[hadgem3_q_nlon,hadgem3_q_nlat,n_time]))*1000.
   hadgem3_u850_clim_smean=fltarr(hadgem3_u_nlon,hadgem3_u_nlat)
   hadgem3_v850_clim_smean=fltarr(hadgem3_u_nlon,hadgem3_u_nlat)
   hadgem3_q850_clim_smean=fltarr(hadgem3_q_nlon,hadgem3_q_nlat)
   FOR m=0,hadgem3_u_nlon-1 DO BEGIN
      FOR n=0,hadgem3_u_nlat-1 DO BEGIN
         hadgem3_u850_clim_smean(m,n)=MEAN(hadgem3_u850_clim(m,n,*))
         hadgem3_v850_clim_smean(m,n)=MEAN(hadgem3_v850_clim(m,n,*))
      ENDFOR
   ENDFOR  
   FOR m=0,hadgem3_q_nlon-1 DO $
      FOR n=0,hadgem3_q_nlat-1 DO $
         hadgem3_q850_clim_smean(m,n)=MEAN(hadgem3_q850_clim(m,n,*))

   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_air_wetvdry_uv850.'+runid+'.wet_composite.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=12000,SPACE3=500
   CS,SCALE=40,NCOLS=N_ELEMENTS(mylevs_raw)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_raw
   CON,FIELD=REFORM(hadgem3_wet_composite_q850(k,*,*)),X=hadgem3_q_longitude,Y=hadgem3_q_latitude,$
       TITLE="JJAS u/v/q 850hPa for composite of wettest "+STRTRIM(STRING(nyears_wet),1)+" years from "+runid,CB_TITLE='g kg!U-1!N',$
       /NOLINES
   VECT,X=hadgem3_u_longitude,Y=hadgem3_u_latitude,U=REFORM(hadgem3_wet_composite_u850(k,*,*)),V=REFORM(hadgem3_wet_composite_v850(k,*,*)),MAG=5,$
        STRIDE=2
   ;AXES,XSTEP=20,YSTEP=10
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_air_wetvdry_uv850.'+runid+'.dry_composite.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=12000,SPACE3=500
   CS,SCALE=40,NCOLS=N_ELEMENTS(mylevs_raw)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_raw
   CON,FIELD=REFORM(hadgem3_dry_composite_q850(k,*,*)),X=hadgem3_q_longitude,Y=hadgem3_q_latitude,$
       TITLE="JJAS u/v/q 850hPa for composite of driest "+STRTRIM(STRING(nyears_dry),1)+" years from "+runid,CB_TITLE='g kg!U-1!N',$
       /NOLINES
   VECT,X=hadgem3_u_longitude,Y=hadgem3_u_latitude,U=REFORM(hadgem3_dry_composite_u850(k,*,*)),V=REFORM(hadgem3_wet_composite_v850(k,*,*)),MAG=5,$
        STRIDE=2
   ;AXES,XSTEP=20,YSTEP=10
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_air_wetvdry_uv850.'+runid+'.wet_composite_anom.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=12000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff_small)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_diff_small
   CON,FIELD=REFORM(hadgem3_wet_composite_q850(k,*,*))-hadgem3_q850_clim_smean,X=hadgem3_q_longitude,Y=hadgem3_q_latitude,$
       TITLE="JJAS anom u/v/q 850hPa for composite of wettest "+STRTRIM(STRING(nyears_wet),1)+" years from "+runid,CB_TITLE='g kg!U-1!N',$
       /NOLINES
   VECT,X=hadgem3_u_longitude,Y=hadgem3_u_latitude,U=REFORM(hadgem3_wet_composite_u850(k,*,*))-hadgem3_u850_clim_smean,$
        V=REFORM(hadgem3_wet_composite_v850(k,*,*))-hadgem3_v850_clim_smean,MAG=1,$
        STRIDE=2
   ;AXES,XSTEP=20,YSTEP=10
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_air_wetvdry_uv850.'+runid+'.dry_composite_anom.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=12000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff_small)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_diff_small
   CON,FIELD=REFORM(hadgem3_dry_composite_q850(k,*,*))-hadgem3_q850_clim_smean,X=hadgem3_q_longitude,Y=hadgem3_q_latitude,$
       TITLE="JJAS anom u/v/q 850hPa for composite of driest "+STRTRIM(STRING(nyears_dry),1)+" years from "+runid,CB_TITLE='g kg!U-1!N',$
       /NOLINES
   VECT,X=hadgem3_u_longitude,Y=hadgem3_u_latitude,U=REFORM(hadgem3_dry_composite_u850(k,*,*))-hadgem3_u850_clim_smean,$
        V=REFORM(hadgem3_dry_composite_v850(k,*,*))-hadgem3_v850_clim_smean,MAG=1,$
        STRIDE=2
   ;AXES,XSTEP=20,YSTEP=10
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_air_wetvdry_uv850.'+runid+'.wet-minus-dry_composite.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=12000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_diff
   CON,FIELD=REFORM(hadgem3_wet_composite_q850(k,*,*)-hadgem3_dry_composite_q850(k,*,*)),X=hadgem3_q_longitude,Y=hadgem3_q_latitude,$
       TITLE="JJAS u/v/q 850hPa for diff wettest "+STRTRIM(STRING(nyears_wet),1)+" yr minus driest "+STRTRIM(STRING(nyears_dry),1)+$
       " from "+runid,CB_TITLE='g kg!U-1!N',$
       /NOLINES
   VECT,X=hadgem3_u_longitude,Y=hadgem3_u_latitude,U=REFORM(hadgem3_wet_composite_u850(k,*,*)-hadgem3_dry_composite_u850(k,*,*)),$
        V=REFORM(hadgem3_wet_composite_v850(k,*,*)-hadgem3_dry_composite_v850(k,*,*)),MAG=2,$
        STRIDE=2
   ;AXES,XSTEP=20,YSTEP=10
   PSCLOSE,/NOVIEW
ENDFOR

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_air_wetvdry_uv850.1.5x-minus-1.0x.wet_composite.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=12000,SPACE3=500
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_diff
CON,FIELD=REFORM(hadgem3_wet_composite_q850(1,*,*)-hadgem3_wet_composite_q850(0,*,*)),X=hadgem3_q_longitude,Y=hadgem3_q_latitude,$
    TITLE="JJAS diff in composite of u/v/q 850 hPa for wettest "+STRTRIM(STRING(nyears_wet),1)+" years - 1.5x minus 1.0x",CB_TITLE='g kg!U-1!N',$
    /NOLINES
VECT,X=hadgem3_u_longitude,Y=hadgem3_u_latitude,U=REFORM(hadgem3_wet_composite_u850(1,*,*)-hadgem3_wet_composite_u850(0,*,*)),$
     V=REFORM(hadgem3_wet_composite_v850(1,*,*)-hadgem3_wet_composite_v850(0,*,*)),MAG=2,$
     STRIDE=2
;AXES,XSTEP=20,YSTEP=10
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_air_wetvdry_uv850.1.5x-minus-1.0x.dry_composite.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=12000,SPACE3=500
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_diff
CON,FIELD=REFORM(hadgem3_dry_composite_q850(1,*,*)-hadgem3_dry_composite_q850(0,*,*)),X=hadgem3_q_longitude,Y=hadgem3_q_latitude,$
    TITLE="JJAS diff in composite of u/v/q 850 hPa for driest "+STRTRIM(STRING(nyears_dry),1)+" years - 1.5x minus 1.0x",CB_TITLE='g kg!U-1!N',$
    /NOLINES
VECT,X=hadgem3_u_longitude,Y=hadgem3_u_latitude,U=REFORM(hadgem3_dry_composite_u850(1,*,*)-hadgem3_dry_composite_u850(0,*,*)),$
     V=REFORM(hadgem3_dry_composite_v850(1,*,*)-hadgem3_dry_composite_v850(0,*,*)),MAG=2,$
     STRIDE=2
;AXES,XSTEP=20,YSTEP=10
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_air_wetvdry_uv850.1.5x-minus-1.0x.wet-minus-dry_composite.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=12000,SPACE3=500
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_diff
CON,FIELD=REFORM((hadgem3_wet_composite_q850(1,*,*)-hadgem3_dry_composite_q850(1,*,*))-(hadgem3_wet_composite_q850(0,*,*)-hadgem3_dry_composite_q850(0,*,*))),$
    X=hadgem3_q_longitude,Y=hadgem3_q_latitude,$
    TITLE="JJAS diff of diff of u/v/q 850 hPa for wettest "+STRTRIM(STRING(nyears_wet),1)+" yr minus driest "+STRTRIM(STRING(nyears_dry),1)+$
    " - 1.5x minus 1.0x",CB_TITLE='g kg!U-1!N',$
    /NOLINES
VECT,X=hadgem3_u_longitude,Y=hadgem3_u_latitude,U=REFORM((hadgem3_wet_composite_u850(1,*,*)-hadgem3_dry_composite_u850(1,*,*))-$
                                                         (hadgem3_wet_composite_u850(0,*,*)-hadgem3_dry_composite_u850(0,*,*))),$
     V=REFORM((hadgem3_wet_composite_v850(1,*,*)-hadgem3_dry_composite_v850(1,*,*))-$
              (hadgem3_wet_composite_v850(0,*,*)-hadgem3_dry_composite_v850(0,*,*))),$
     MAG=2,STRIDE=2
;AXES,XSTEP=20,YSTEP=10
PSCLOSE,/NOVIEW

STOP
END
