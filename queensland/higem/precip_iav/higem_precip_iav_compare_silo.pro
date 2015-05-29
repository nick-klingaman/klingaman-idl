PRO higem_precip_iav_compare_silo

; Timeseries of annual means
higem_xbylr_infile='/home/ss901165/higem_qccce/hpcx_control_xbylr/higem_xbylr.jan-dec_ameans.h9-t5.precip.aus_domain.nc'
higem_eafeb_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.jan-dec_ameans.h9-w8.precip.aus_domain.nc'
silo_n144_infile='/home/ss901165/datasets_mango/SILO/n144/SILO.jan-dec_ameans.1900-2008.n144.nc'

; Number of years
higem_xbylr_nyears=117
higem_eafeb_nyears=150
silo_n144_nyears=109

; Box to read
box_read=[-45,110,-10,155]
; Box to plot
box_plot=[-44,112,-10,154]

higem_latitude=OPEN_AND_EXTRACT(higem_xbylr_infile,'latitude')
higem_longitude=OPEN_AND_EXTRACT(higem_xbylr_infile,'longitude')
DEFINE_BOUNDARIES,box_read,higem_latitude,higem_longitude,higem_box_tx,/LIMIT
higem_nlon=N_ELEMENTS(higem_longitude)
higem_nlat=N_ELEMENTS(higem_latitude)

silo_n144_latitude=OPEN_AND_EXTRACT(silo_n144_infile,'latitude')
silo_n144_longitude=OPEN_AND_EXTRACT(silo_n144_infile,'longitude')
DEFINE_BOUNDARIES,box_read,silo_n144_latitude,silo_n144_longitude,silo_n144_box_tx,/LIMIT
silo_n144_nlon=N_ELEMENTS(silo_n144_longitude)
silo_n144_nlat=N_ELEMENTS(silo_n144_latitude)

; Read data
higem_xbylr_ameans_precip=REFORM(OPEN_AND_EXTRACT(higem_xbylr_infile,'precip',$
                                                 offset=[higem_box_tx(1),higem_box_tx(0),0,0,0],$
                                                 count=[higem_nlon,higem_nlat,1,1,higem_xbylr_nyears]))*86400.
higem_eafeb_ameans_precip=REFORM(OPEN_AND_EXTRACT(higem_eafeb_infile,'precip',$
                                                 offset=[higem_box_tx(1),higem_box_tx(0),0,0,0],$
                                                 count=[higem_nlon,higem_nlat,1,1,higem_eafeb_nyears]))*86400.
silo_n144_ameans_precip=REFORM(OPEN_AND_EXTRACT(silo_n144_infile,'rain',$
                                                offset=[silo_n144_box_tx(1),silo_n144_box_tx(0),0],$
                                                count=[silo_n144_nlon,silo_n144_nlat,silo_n144_nyears]))

; Take standard deviations
higem_xbylr_iav=fltarr(higem_nlon,higem_nlat)
higem_xbylr_clim=fltarr(higem_nlon,higem_nlat)
higem_eafeb_iav=fltarr(higem_nlon,higem_nlat)
higem_eafeb_dlv=fltarr(higem_nlon,higem_nlat)
higem_eafeb_clim=fltarr(higem_nlon,higem_nlat)
silo_n144_iav=fltarr(silo_n144_nlon,silo_n144_nlat)
silo_n144_dlv=fltarr(silo_n144_nlon,silo_n144_nlat)
silo_n144_clim=fltarr(silo_n144_nlon,silo_n144_nlat)

FOR i=0,higem_nlon-1 DO BEGIN
   FOR j=0,higem_nlat-1 DO BEGIN
      higem_xbylr_iav(i,j)=STDDEV(higem_xbylr_ameans_precip(i,j,*))
      higem_eafeb_iav(i,j)=STDDEV(higem_eafeb_ameans_precip(i,j,*))            
      temp=SMOOTH(REFORM(higem_eafeb_ameans_precip(i,j,*)),13)
      higem_eafeb_dlv(i,j)=STDDEV(temp(5:higem_eafeb_nyears-6))
      higem_xbylr_clim(i,j)=MEAN(higem_xbylr_ameans_precip(i,j,*))
      higem_eafeb_clim(i,j)=MEAN(higem_eafeb_ameans_precip(i,j,*))
   ENDFOR
ENDFOR
FOR i=0,silo_n144_nlon-1 DO BEGIN
   FOR j=0,silo_n144_nlat-1 DO BEGIN
      silo_n144_iav(i,j)=STDDEV(silo_n144_ameans_precip(i,j,*))
      temp=SMOOTH(REFORM(silo_n144_ameans_precip(i,j,*)),11)
      silo_n144_dlv(i,j)=STDDEV(temp(5:silo_n144_nyears-6))
      silo_n144_clim(i,j)=MEAN(silo_n144_ameans_precip(i,j,*))
   ENDFOR
ENDFOR

; Masks out ocean points
silo_n144_iav[where(silo_n144_iav eq 0)]=!Values.F_NaN
silo_n144_dlv[where(silo_n144_dlv eq 0)]=!Values.F_NaN
;silo_n144_clim[where(silo_n144_clim eq 0)]=!Values.F_NaN

mylevs=['0','0.1','0.2','0.3','0.4','0.5','0.6','0.7','0.8','0.9','1.0','1.1','1.2']

psfile='/home/ss901165/idl/queensland/higem/precip_iav/higem_precip_iav_compare_silo.annual_means_silo_n144.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=24,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs
CON,FIELD=silo_n144_iav,X=silo_n144_longitude,Y=silo_n144_latitude,$
    TITLE='Standard deviation in annual-mean rainfall from SILO at N144 (1900-2008)',/NOLINES
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/precip_iav/higem_precip_iav_compare_silo.annual_means_xbylr_n144.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=24,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs
CON,FIELD=higem_xbylr_iav,X=higem_longitude,Y=higem_latitude,$
    TITLE='Standard deviation in annual-mean rainfall from HiGEM (xbylr, 81 years)',/NOLINES
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/precip_iav/higem_precip_iav_compare_silo.annual_means_eafeb_n144.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=24,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs
CON,FIELD=higem_eafeb_iav,X=higem_longitude,Y=higem_latitude,$
    TITLE='Standard deviation in annual-mean rainfall from HiGEM (eafeb, 150 years)',/NOLINES
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

ratio=fltarr(higem_nlon,higem_nlat)
ratio_against_mean=fltarr(higem_nlon,higem_nlat)
decadal_ratio_against_mean=fltarr(higem_nlon,higem_nlat)
FOR i=0,higem_nlat-1 DO BEGIN
   ratio(*,i)=higem_eafeb_iav(*,i)/silo_n144_iav(*,higem_nlat-i-1)
   FOR j=0,higem_nlon-1 DO BEGIN
      ratio_against_mean(j,i)=(higem_eafeb_iav(j,i)/higem_eafeb_clim(j,i))/(silo_n144_iav(j,higem_nlat-i-1)/silo_n144_clim(j,higem_nlat-i-1))
      decadal_ratio_against_mean(j,i)=(higem_eafeb_dlv(j,i)/higem_eafeb_clim(j,i))/(silo_n144_dlv(j,higem_nlat-i-1)/silo_n144_clim(j,higem_nlat-i-1))
   ENDFOR
ENDFOR

mylevs_ratio=['0.35','0.45','0.55','0.65','0.75','0.85','0.95','1.05','1.15','1.25','1.35','1.45','1.55','1.65']
psfile='/home/ss901165/idl/queensland/higem/precip_iav/higem_precip_iav_compare_silo.annual_means_eafeb-ratio-silo_n144.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_ratio)+1,/REV,white=[9]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_ratio
CON,FIELD=ratio,X=higem_longitude,Y=higem_latitude,$
    TITLE='Ratio in stddev in annual-mean rainfall for HiGEM eafeb divided by SILO',/NOLINES
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

mylevs_ratio=['0.35','0.45','0.55','0.65','0.75','0.85','0.95','1.05','1.18','1.33','1.54','1.82','2.22','2.86']
psfile='/home/ss901165/idl/queensland/higem/precip_iav/higem_precip_dlv_compare_silo.annual_means_eafeb-ratio_norm-silo_n144.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=90,CB_WIDTH=110,/PORTRAIT
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_ratio)+1,/REV,white=[9]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_ratio
CON,FIELD=decadal_ratio_against_mean,X=higem_longitude,Y=higem_latitude,$
    TITLE='Ratio in stddev in 11-year smoothed annual-mean rainfall for HiGEM eafeb divided by SILO',/NOLINES
AXES,XSTEP=5,YSTEP=5
PSCLOSE

mylevs_ratio=['0.35','0.45','0.55','0.65','0.75','0.85','0.95','1.05','1.18','1.33','1.54','1.82','2.22','2.86']
;mylevs_ratio=['0.35','0.45','0.55','0.65','0.75','0.85','0.95','1.05','1.15','1.25','1.35','1.45','1.55','1.65']
psfile='/home/ss901165/idl/queensland/higem/precip_iav/higem_precip_iav_compare_silo.annual_means_eafeb-ratio_norm-silo_n144.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_ratio)+1,/REV,white=[9]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_ratio
CON,FIELD=ratio_against_mean,X=higem_longitude,Y=higem_latitude,$
    TITLE='Ratio in stddev in annual-mean rainfall for HiGEM eafeb divided by SILO (norm by mean)',/NOLINES
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

FOR i=0,higem_nlat-1 DO BEGIN
   ratio(*,i)=higem_xbylr_iav(*,i)/silo_n144_iav(*,higem_nlat-i-1)
   FOR j=0,higem_nlon-1 DO $
      ratio_against_mean(j,i)=(higem_xbylr_iav(j,i)/higem_xbylr_clim(j,i))/(silo_n144_iav(j,higem_nlat-i-1)/silo_n144_clim(j,higem_nlat-i-1))
ENDFOR

mylevs_ratio=['0.35','0.45','0.55','0.65','0.75','0.85','0.95','1.05','1.15','1.25','1.35','1.45','1.55','1.65']
psfile='/home/ss901165/idl/queensland/higem/precip_iav/higem_precip_iav_compare_silo.annual_means_xbylr-ratio-silo_n144.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_ratio)+1,/REV,white=[9]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_ratio
CON,FIELD=ratio,X=higem_longitude,Y=higem_latitude,$
    TITLE='Ratio in standard deviation in annual-mean rainfall for HiGEM xbylr divided by SILO',/NOLINES
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

mylevs_ratio=['0.35','0.45','0.55','0.65','0.75','0.85','0.95','1.05','1.15','1.25','1.35','1.45','1.55','1.65']
psfile='/home/ss901165/idl/queensland/higem/precip_iav/higem_precip_iav_compare_silo.annual_means_xbylr-ratio_norm-silo_n144.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_ratio)+1,/REV,white=[9]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_ratio
CON,FIELD=ratio_against_mean,X=higem_longitude,Y=higem_latitude,$
    TITLE='Ratio in standard in annual-mean rainfall for HiGEM xbylr divided by SILO (norm by mean)',/NOLINES
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

; Now do the same for each season (DJF, MAM, JJA, SON)
seasons_months=[[11,0,1],[2,3,4],[5,6,7],[8,9,10]]
seasons_months_names=[['dec','jan','feb'],$
                      ['mar','apr','may'],$
                      ['jun','jul','aug'],$
                      ['sep','oct','nov']]
seasons_abbrev=['DJF','MAM','JJA','SON']
n_seasons=N_ELEMENTS(seasons_months(0,*))

higem_xbylr_indir='/home/ss901165/higem_qccce/hpcx_control_xbylr'
higem_eafeb_indir='/home/ss901165/higem_qccce/es_control_eafeb'
silo_n144_indir='/home/ss901165/datasets_mango/SILO/n144'

FOR i=0,n_seasons-1 DO BEGIN
   xbylr_seasmean_precip_ts=fltarr(higem_nlon,higem_nlat,higem_xbylr_nyears)
   eafeb_seasmean_precip_ts=fltarr(higem_nlon,higem_nlat,higem_eafeb_nyears)
   silo_seasmean_precip_ts=fltarr(silo_n144_nlon,silo_n144_nlat,silo_n144_nyears)

   FOR j=0,N_ELEMENTS(seasons_months(*,i))-1 DO BEGIN
      xbylr_thismonth_precip=REFORM(OPEN_AND_EXTRACT(higem_xbylr_indir+'/higem_xbylr.'+seasons_months_names(j,i)+'_mmeans.h9-t5.precip.aus_domain.nc',$
                                                     'precip',offset=[higem_box_tx(1),higem_box_tx(0),0,0,0],count=[higem_nlon,higem_nlat,1,higem_xbylr_nyears,1]))*86400.
      eafeb_thismonth_precip=REFORM(OPEN_AND_EXTRACT(higem_eafeb_indir+'/higem_eafeb.'+seasons_months_names(j,i)+'_mmeans.h9-w8.precip.aus_domain.nc',$
                                                     'precip',offset=[higem_box_tx(1),higem_box_tx(0),0,0,0],count=[higem_nlon,higem_nlat,1,higem_eafeb_nyears,1]))*86400.
      silo_thismonth_precip=REFORM(OPEN_AND_EXTRACT(silo_n144_indir+'/SILO.'+seasons_months_names(j,i)+'_mmeans.1900-2008.n144.nc',$
                                                    'rain',offset=[silo_n144_box_tx(1),silo_n144_box_tx(0),0,0],$
                                                    count=[silo_n144_nlon,silo_n144_nlat,silo_n144_nyears,1]))

      xbylr_seasmean_precip_ts=xbylr_seasmean_precip_ts+xbylr_thismonth_precip/FLOAT(N_ELEMENTS(seasons_months(*,i)))
      eafeb_seasmean_precip_ts=eafeb_seasmean_precip_ts+eafeb_thismonth_precip/FLOAT(N_ELEMENTS(seasons_months(*,i)))
      silo_seasmean_precip_ts=silo_seasmean_precip_ts+silo_thismonth_precip/FLOAT(N_ELEMENTS(seasons_months(*,i)))
   ENDFOR

   ; Take standard deviations
   higem_xbylr_iav=fltarr(higem_nlon,higem_nlat)
   higem_xbylr_clim=fltarr(higem_nlon,higem_nlat)
   higem_eafeb_iav=fltarr(higem_nlon,higem_nlat)
   higem_eafeb_dlv=fltarr(higem_nlon,higem_nlat)
   higem_eafeb_clim=fltarr(higem_nlon,higem_nlat)
   silo_n144_iav=fltarr(silo_n144_nlon,silo_n144_nlat)
   silo_n144_dlv=fltarr(silo_n144_nlon,silo_n144_nlat)
   silo_n144_clim=fltarr(silo_n144_nlon,silo_n144_nlat)
   
   FOR j=0,higem_nlon-1 DO BEGIN
      FOR k=0,higem_nlat-1 DO BEGIN
         higem_xbylr_iav(j,k)=STDDEV(xbylr_seasmean_precip_ts(j,k,*))
         higem_xbylr_clim(j,k)=MEAN(xbylr_seasmean_precip_ts(j,k,*))
         temp=SMOOTH(REFORM(eafeb_seasmean_precip_ts(j,k,*)),13)
         higem_eafeb_dlv(j,k)=STDDEV(temp(5:higem_eafeb_nyears-6))
         higem_eafeb_iav(j,k)=STDDEV(eafeb_seasmean_precip_ts(j,k,*))
         higem_eafeb_clim(j,k)=MEAN(eafeb_seasmean_precip_ts(j,k,*))
      ENDFOR
   ENDFOR
   FOR j=0,silo_n144_nlon-1 DO BEGIN
      FOR k=0,silo_n144_nlat-1 DO BEGIN
         silo_n144_iav(j,k)=STDDEV(silo_seasmean_precip_ts(j,k,*))
         temp=SMOOTH(REFORM(silo_seasmean_precip_ts(j,k,*)),11)
         silo_n144_dlv(j,k)=STDDEV(temp(5:silo_n144_nyears-6))
         silo_n144_clim(j,k)=MEAN(silo_seasmean_precip_ts(j,k,*))
      ENDFOR
   ENDFOR
; Masks out ocean points
   silo_n144_iav[where(silo_n144_iav eq 0)]=!Values.F_NaN
   silo_n144_dlv[where(silo_n144_dlv eq 0)]=!Values.F_NaN
   
   mylevs=['0','0.2','0.4','0.6','0.8','1.0','1.2','1.4','1.6','1.8','2.0','2.2','2.4','2.6']
   
   psfile='/home/ss901165/idl/queensland/higem/precip_iav/higem_precip_iav_compare_silo.'+seasons_abbrev(i)+'_means_silo_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=24,NCOLS=N_ELEMENTS(mylevs)+1
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=silo_n144_iav,X=silo_n144_longitude,Y=silo_n144_latitude,$
       TITLE='Standard deviation in '+seasons_abbrev(i)+'-mean rainfall from SILO at N144 (1900-2008)',/NOLINES
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/precip_iav/higem_precip_iav_compare_silo.'+seasons_abbrev(i)+'_means_xbylr_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=24,NCOLS=N_ELEMENTS(mylevs)+1
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=higem_xbylr_iav,X=higem_longitude,Y=higem_latitude,$
       TITLE='Standard deviation in '+seasons_abbrev(i)+'-mean rainfall from HiGEM (xbylr, 81 years)',/NOLINES
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/queensland/higem/precip_iav/higem_precip_iav_compare_silo.'+seasons_abbrev(i)+'_means_eafeb_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=24,NCOLS=N_ELEMENTS(mylevs)+1
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=higem_eafeb_iav,X=higem_longitude,Y=higem_latitude,$
       TITLE='Standard deviation in '+seasons_abbrev(i)+'-mean rainfall from HiGEM (eafeb, 74 years)',/NOLINES
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW
   
   ratio=fltarr(higem_nlon,higem_nlat)
   decadal_ratio_against_mean=fltarr(higem_nlon,higem_nlat)
   FOR j=0,higem_nlat-1 DO BEGIN
      ratio(*,j)=higem_eafeb_iav(*,j)/silo_n144_iav(*,higem_nlat-j-1)
      FOR k=0,higem_nlon-1 DO BEGIN
         ratio_against_mean(k,j)=(higem_eafeb_iav(k,j)/higem_eafeb_clim(k,j))/(silo_n144_iav(k,higem_nlat-j-1)/silo_n144_clim(k,higem_nlat-j-1))
         decadal_ratio_against_mean(k,j)=(higem_eafeb_dlv(k,j)/higem_eafeb_clim(k,j))/(silo_n144_dlv(k,higem_nlat-j-1)/silo_n144_clim(k,higem_nlat-j-1))
      ENDFOR
   ENDFOR

   mylevs_ratio=['0.35','0.45','0.55','0.65','0.75','0.85','0.95','1.05','1.15','1.25','1.35','1.45','1.55','1.65']
   psfile='/home/ss901165/idl/queensland/higem/precip_iav/higem_precip_iav_compare_silo.'+seasons_abbrev(i)+'_means_eafeb-ratio-silo_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_ratio)+1,/REV,white=[9]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_ratio
   CON,FIELD=ratio,X=higem_longitude,Y=higem_latitude,$
       TITLE='Ratio in standard deviation in '+seasons_abbrev(i)+'-mean rainfall for HiGEM eafeb divided by SILO',/NOLINES
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   mylevs_ratio=['0.35','0.45','0.55','0.65','0.75','0.85','0.95','1.05','1.18','1.33','1.54','1.82','2.22','2.86']
   psfile='/home/ss901165/idl/queensland/higem/precip_iav/higem_precip_dlv_compare_silo.'+seasons_abbrev(i)+'_means_eafeb-ratio_norm-silo_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=90,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_ratio)+1,/REV,white=[9]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_ratio
   CON,FIELD=decadal_ratio_against_mean,X=higem_longitude,Y=higem_latitude,$
       TITLE='Ratio in stddev in 11-year smoothed '+seasons_abbrev(i)+'-mean rainfall for HiGEM eafeb divided by SILO',/NOLINES
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW
   
   mylevs_ratio=['0.35','0.45','0.55','0.65','0.75','0.85','0.95','1.05','1.18','1.33','1.54','1.82','2.22','2.86']
;   mylevs_ratio=['0.35','0.45','0.55','0.65','0.75','0.85','0.95','1.05','1.15','1.25','1.35','1.45','1.55','1.65']
   psfile='/home/ss901165/idl/queensland/higem/precip_iav/higem_precip_iav_compare_silo.'+seasons_abbrev(i)+'_means_eafeb-ratio_norm-silo_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_ratio)+1,/REV,white=[9]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_ratio
   CON,FIELD=ratio_against_mean,X=higem_longitude,Y=higem_latitude,$
       TITLE='Ratio in standard in '+seasons_abbrev(i)+'-mean rainfall for HiGEM eafeb divided by SILO (norm by mean)',/NOLINES
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW
   
   FOR j=0,higem_nlat-1 DO BEGIN
      ratio(*,j)=higem_xbylr_iav(*,j)/silo_n144_iav(*,higem_nlat-j-1)
      FOR k=0,higem_nlon-1 DO $
         ratio_against_mean(k,j)=(higem_xbylr_iav(k,j)/higem_xbylr_clim(k,j))/(silo_n144_iav(k,higem_nlat-j-1)/silo_n144_clim(k,higem_nlat-j-1))
   ENDFOR
   
   mylevs_ratio=['0.35','0.45','0.55','0.65','0.75','0.85','0.95','1.05','1.15','1.25','1.35','1.45','1.55','1.65']
   psfile='/home/ss901165/idl/queensland/higem/precip_iav/higem_precip_iav_compare_silo.'+seasons_abbrev(i)+'_means_xbylr-ratio-silo_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_ratio)+1,/REV,white=[9]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_ratio
   CON,FIELD=ratio,X=higem_longitude,Y=higem_latitude,$
       TITLE='Ratio in standard deviation in '+seasons_abbrev(i)+'-mean rainfall for HiGEM xbylr divided by SILO',/NOLINES
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW
   
   mylevs_ratio=['0.35','0.45','0.55','0.65','0.75','0.85','0.95','1.05','1.15','1.25','1.35','1.45','1.55','1.65']
   psfile='/home/ss901165/idl/queensland/higem/precip_iav/higem_precip_iav_compare_silo.'+seasons_abbrev(i)+'_means_xbylr-ratio_norm-silo_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_ratio)+1,/REV,white=[9]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_ratio
   CON,FIELD=ratio_against_mean,X=higem_longitude,Y=higem_latitude,$
       TITLE='Ratio in standard in '+seasons_abbrev(i)+'-mean rainfall for HiGEM xbylr divided by SILO (norm by mean)',/NOLINES
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

ENDFOR
   

STOP

END
