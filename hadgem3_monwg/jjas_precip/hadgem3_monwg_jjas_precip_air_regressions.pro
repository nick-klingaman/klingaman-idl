PRO hadgem3_monwg_jjas_precip_air_regressions

nick_1xentrain_daily_file='/home/ss901165/um_output3/hadgem3a_amip2_control_vn74/hadgem3a_amip2_ctl_vn74.jan-dec_dmeans.years1-77.precip.nc'
nick_1xentrain_clim_file='/home/ss901165/um_output3/hadgem3a_amip2_control_vn74/hadgem3a_amip2_ctl_vn74.jan-dec_dmean_clim.years1-77.precip.nc'
nick_15xentrain_daily_file='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmeans.years1-77.precip.nc'
nick_15xentrain_clim_file='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmean_clim.years1-77.precip.nc'

n96_mask_file='/home/ss901165/um_output/mask_n96.nc'

; Box to area average
box=[10,70,30,88]

; Box to plot
box_plot=[-40,40,40,200]

max_years=77
nyears_wet=10
nyears_dry=10

;mylevs_raw=['2','3','4','5','6','8','10','12','15','18','21','24','27','30']
;mylevs_diff_small=['-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5']
;mylevs_diff=['-11','-9','-7','-5','-3','-1','1','3','5','7','9','11']

mylevs_corr=['-0.75','-0.65','-0.55','-0.45','-0.35','-0.25','-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75']
mylevs_regress=['-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5']

n_sets_plot=2

FOR k=0,n_sets_plot-1 DO BEGIN
   CASE k OF
      0 : BEGIN
         hadgem3_daily_file=nick_1xentrain_daily_file         
         hadgem3_clim_file=nick_1xentrain_clim_file
         offset_jun1=150
         offset_sep30=269
         hadgem3_nyears=77
         runid='hadgem3a_amip2_1.0xentrain_vn74'
         description='Nick HadGEM3-A (vn7.4) 1.0x entrain with AMIP2 clim SSTs (77 years)'        
      END
      1 : BEGIN
         hadgem3_daily_file=nick_15xentrain_daily_file
         hadgem3_clim_file=nick_15xentrain_clim_file
         offset_jun1=150
         offset_sep30=269
         hadgem3_nyears=77
         runid='hadgem3a_amip2_1.5xentrain_vn74'
         description='Nick HadGEM3-A (vn7.4) 1.5x entrain with AMIP2 clim SSTs (77 years)'
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
         hadgem3_year_aavg(j)=MEAN(temp_precip,/NaN)
      ENDFOR
      hadgem3_jjas_ts(i)=MEAN(hadgem3_year_aavg)
   ENDFOR

   hadgem3_longitude=OPEN_AND_EXTRACT(hadgem3_daily_file,'longitude')
   hadgem3_latitude=OPEN_AND_EXTRACT(hadgem3_daily_file,'latitude')
   DEFINE_BOUNDARIES,box_plot,hadgem3_latitude,hadgem3_longitude,hadgem3_box_plot_tx,/LIMIT
   hadgem3_nlon=N_ELEMENTS(hadgem3_longitude)
   hadgem3_nlat=N_ELEMENTS(hadgem3_latitude)

   hadgem3_allyears=REFORM(OPEN_AND_EXTRACT(hadgem3_daily_file,'precip',$
                                            offset=[hadgem3_box_plot_tx(1),hadgem3_box_plot_tx(0),offset_jun1,0],$
                                            count=[hadgem3_nlon,hadgem3_nlat,n_time,hadgem3_nyears]))*86400.
   hadgem3_smeans=fltarr(hadgem3_nlon,hadgem3_nlat,hadgem3_nyears)
   FOR m=0,hadgem3_nlon-1 DO $
      FOR n=0,hadgem3_nlat-1 DO $
         FOR p=0,hadgem3_nyears-1 DO $
            hadgem3_smeans(m,n,p)=MEAN(hadgem3_allyears(m,n,*,p))

   hadgem3_correlations=fltarr(hadgem3_nlon,hadgem3_nlat)
   hadgem3_regressions=fltarr(hadgem3_nlon,hadgem3_nlat)
   FOR m=0,hadgem3_nlon-1 DO BEGIN
      FOR n=0,hadgem3_nlat-1 DO BEGIN
         hadgem3_correlations(m,n)=CORRELATE(hadgem3_jjas_ts,REFORM(hadgem3_smeans(m,n,*)))
         hadgem3_regressions(m,n)=REGRESS(hadgem3_jjas_ts,REFORM(hadgem3_smeans(m,n,*)))
      ENDFOR
   ENDFOR

   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_air_regressions.'+runid+'.correlations.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=12000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_corr)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_corr
   CON,FIELD=REFORM(hadgem3_correlations),X=hadgem3_longitude,Y=hadgem3_latitude,$
       TITLE="Corr JJAS AIR with JJAS gridpt precip for "+runid,CB_TITLE='mm day!U-1!N',$
       /NOLINES
   PSCLOSE

   hadgem3_regressions[where(ABS(hadgem3_correlations) lt 0.178)]=!Values.F_NaN

   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_air_regressions.'+runid+'.regressions.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=12000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_regress)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_regress
   CON,FIELD=REFORM(hadgem3_regressions),X=hadgem3_longitude,Y=hadgem3_latitude,$
       TITLE="Regress JJAS AIR with JJAS gridpt precip for "+runid,CB_TITLE='mm day!U-1!N',$
       /NOLINES
   PSCLOSE
ENDFOR

STOP
END
