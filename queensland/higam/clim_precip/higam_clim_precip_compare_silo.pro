PRO higam_clim_precip_compare_silo

higam_xcquc_indir='/home/ss901165/higam_qccce/hpcx_amip2_xcquc'
silo_025_indir='/home/ss901165/datasets_mango/SILO/one_quarter'
silo_n144_indir='/home/ss901165/datasets_mango/SILO/n144'
trmm_n144_indir='/home/ss901165/datasets_mango/TRMM_3B42V6/n144'

months=['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']
months_long=['January','February','March','April','May','June','July','August','September','October','November','December']
;n_months=N_ELEMENTS(months)
n_months=12

box_plot=[-44,112,-10,154]
box=[-45,110,-10,155]
mylevs=['0','0.5','1.0','1.5','2.0','2.5','3.0','3.5','4.0','4.5','5.0','6.0','7.0','8.0','9.0','10.0']
mylevs_annual=['1.0','1.25','1.5','1.75','2.0','2.25','2.5','2.75','3.0','3.5','4.0','4.5','5.0','5.5','6.0']
mylevs_diff=['-6','-5','-4','-3','-2.5','-2','-1.5','-1','-0.5','0','0.5','1','1.5','2','2.5','3','4','5']
mylevs_diff_annual=['-2.0','-1.75','-1.5','-1.25','-1.0','-0.75','-0.5','-0.25','0.0','0.25','0.5','0.75','1.0','1.25','1.5','1.75','2.0']

FOR i=0,n_months-1 DO BEGIN
   higam_xcquc_infile=higam_xcquc_indir+'/higam_eafkg.'+months(i)+'_mmean_clim.h9-k2.precip.aus_domain.nc'
   silo_025_infile=silo_025_indir+'/SILO_clim_precip.'+months(i)+'_mmean.1900-2008.0.25x0.25.nc'
   silo_n144_infile=silo_n144_indir+'/SILO.'+months(i)+'_mmean_clim.1900-2008.n144.nc'
   trmm_n144_infile=trmm_n144_indir+'/TRMM_3B42v6A.'+months(i)+'_mmean_clim.1999-2008.n144.nc'
   IF i eq 0 THEN BEGIN
      higam_latitude=OPEN_AND_EXTRACT(higam_xcquc_infile,'latitude_1')
      higam_longitude=OPEN_AND_EXTRACT(higam_xcquc_infile,'longitude_1')
      DEFINE_BOUNDARIES,box,higam_latitude,higam_longitude,higam_box_tx,/LIMIT
      higam_nlon=N_ELEMENTS(higam_longitude)
      higam_nlat=N_ELEMENTS(higam_latitude)
      
      silo_025_latitude=OPEN_AND_EXTRACT(silo_025_infile,'latitude')
      silo_025_longitude=OPEN_AND_EXTRACT(silo_025_infile,'longitude')
      DEFINE_BOUNDARIES,box,silo_025_latitude,silo_025_longitude,silo_025_box_tx,/LIMIT
      silo_025_nlon=N_ELEMENTS(silo_025_longitude)
      silo_025_nlat=N_ELEMENTS(silo_025_latitude)

      silo_n144_latitude=OPEN_AND_EXTRACT(silo_n144_infile,'latitude')
      silo_n144_longitude=OPEN_AND_EXTRACT(silo_n144_infile,'longitude')
      DEFINE_BOUNDARIES,box,silo_n144_latitude,silo_n144_longitude,silo_n144_box_tx,/LIMIT
      silo_n144_nlon=N_ELEMENTS(silo_n144_longitude)
      silo_n144_nlat=N_ELEMENTS(silo_n144_latitude)

      trmm_n144_latitude=OPEN_AND_EXTRACT(trmm_n144_infile,'latitude')
      trmm_n144_longitude=OPEN_AND_EXTRACT(trmm_n144_infile,'longitude')
      DEFINE_BOUNDARIES,box,trmm_n144_latitude,trmm_n144_longitude,trmm_n144_box_tx,/LIMIT
      trmm_n144_nlon=N_ELEMENTS(trmm_n144_longitude)
      trmm_n144_nlat=N_ELEMENTS(trmm_n144_latitude)
   ENDIF
   higam_xcquc_mmean_precip=REFORM(OPEN_AND_EXTRACT(higam_xcquc_infile,'precip',$
                                              offset=[higam_box_tx(1),higam_box_tx(0),0,0],$
                                              count=[higam_nlon,higam_nlat,1,1]))*86400.
   silo_025_mmean_precip=REFORM(OPEN_AND_EXTRACT(silo_025_infile,'rain',$
                                             offset=[silo_025_box_tx(1),silo_025_box_tx(0),0],$
                                             count=[silo_025_nlon,silo_025_nlat,1]))
   silo_n144_mmean_precip=REFORM(OPEN_AND_EXTRACT(silo_n144_infile,'rain',$
                                             offset=[silo_n144_box_tx(1),silo_n144_box_tx(0),0],$
                                             count=[silo_n144_nlon,silo_n144_nlat,1]))
   trmm_n144_mmean_precip=REFORM(OPEN_AND_EXTRACT(trmm_n144_infile,'precip',$
                                                  offset=[trmm_n144_box_tx(1),trmm_n144_box_tx(0),0],$
                                                  count=[trmm_n144_nlon,trmm_n144_nlat,1])) 
   silo_merge_trmm=fltarr(higam_nlon,higam_nlat)
   FOR j=0,higam_nlon-1 DO BEGIN
      FOR k=0,higam_nlat-1 DO BEGIN
         IF (silo_n144_mmean_precip(j,k) gt 1000) THEN BEGIN
            silo_merge_trmm(j,k)=trmm_n144_mmean_precip(j,k)
         ENDIF ELSE $
            silo_merge_trmm(j,k)=silo_n144_mmean_precip(j,k)
      ENDFOR
   ENDFOR
            
   psfile='/home/ss901165/idl/queensland/higam/clim_precip/higam_clim_precip_compare_silo.'+months(i)+'_mmean_silo_n144.ps'   
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=silo_n144_mmean_precip,X=silo_n144_longitude,Y=silo_n144_latitude,$
       TITLE=months_long(i)+'-mean rainfall from SILO at N144 (1900-2008)',/NOLINES
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higam/clim_precip/higam_clim_precip_compare_silo.'+months(i)+'_mmean_silo_025.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=silo_025_mmean_precip,X=silo_025_longitude,Y=silo_025_latitude,$
       TITLE=months_long(i)+'-mean rainfall from SILO at 0.25x0.25 (1900-2008)',/NOLINES
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higam/clim_precip/higam_clim_precip_compare_silo.'+months(i)+'_mmean_trmm_n144.ps'   
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=trmm_n144_mmean_precip,X=trmm_n144_longitude,Y=trmm_n144_latitude,$
       TITLE=months_long(i)+'-mean rainfall from TRMM 3B42v6A at N144 (1999-2008)',/NOLINES
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higam/clim_precip/higam_clim_precip_compare_silo.'+months(i)+'_mmean_silo-merge-trmm_n144.ps'   
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=silo_merge_trmm,X=silo_n144_longitude,Y=trmm_n144_latitude,$
       TITLE=months_long(i)+'-mean rainfall from SILO (land) and TRMM 3B42v6A (ocean) at N144 (1999-2008)',/NOLINES
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higam/clim_precip/higam_clim_precip_compare_silo.'+months(i)+'_mmean_higam_xcquc_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=higam_xcquc_mmean_precip,X=higam_longitude,Y=higam_latitude,$
       TITLE=months_long(i)+'-mean rainfall from HiGAM at N144 (xcquc, 23 years)',/NOLINES
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   diff_higam_xcquc_silo=fltarr(higam_nlon,higam_nlat)
   diff_higam_xcquc_silo_merge_trmm=fltarr(higam_nlon,higam_nlat)
   FOR j=0,higam_nlon-1 DO BEGIN
      FOR k=0,higam_nlat-1 DO BEGIN
         IF silo_n144_mmean_precip(j,higam_nlat-k-1) lt 1000 THEN BEGIN
            diff_higam_xcquc_silo(j,k)=higam_xcquc_mmean_precip(j,k)-silo_n144_mmean_precip(j,higam_nlat-k-1)
         ENDIF ELSE BEGIN
            diff_higam_xcquc_silo(j,k)=!Values.F_NaN
         ENDELSE
         diff_higam_xcquc_silo_merge_trmm(j,k)=higam_xcquc_mmean_precip(j,k)-silo_merge_trmm(j,higam_nlat-k-1)
      ENDFOR
   ENDFOR
                     
   psfile='/home/ss901165/idl/queensland/higam/clim_precip/higam_clim_precip_compare_silo.'+months(i)+'_mmean_higam_xcquc-minus-silo_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_diff
   CON,FIELD=diff_higam_xcquc_silo,X=higam_longitude,Y=higam_latitude,$
       TITLE=months_long(i)+'-mean rainfall for HiGAM (xcquc, 23 years) minus SILO (1900-2008) at N144',/NOLINES
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higam/clim_precip/higam_clim_precip_compare_silo.'+months(i)+'_mmean_higam_xcquc-minus-silo-merge-trmm_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_diff
   CON,FIELD=diff_higam_xcquc_silo_merge_trmm,X=higam_longitude,Y=higam_latitude,$
       TITLE=months_long(i)+'-mean rainfall for HiGAM (xcquc, 23 years) minus SILO (land) and TRMM (ocean)',/NOLINES
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

ENDFOR

higam_xcquc_infile=higam_xcquc_indir+'/higam_eafkg.jan-dec_amean_clim.h9-k2.precip.aus_domain.nc'
silo_025_infile=silo_025_indir+'/SILO_clim_precip.annual_mean.1900-2008.0.25x0.25.nc'
silo_n144_infile=silo_n144_indir+'/SILO.jan-dec_amean_clim.1900-2008.n144.nc'
trmm_n144_infile=trmm_n144_indir+'/TRMM_3B42v6A.jan-dec_amean_clim.1999-2008.n144.nc'

higam_xcquc_amean_precip=REFORM(OPEN_AND_EXTRACT(higam_xcquc_infile,'precip',$
                                                 offset=[higam_box_tx(1),higam_box_tx(0),0,0],$
                                                 count=[higam_nlon,higam_nlat,1,1]))*86400.
silo_025_amean_precip=REFORM(OPEN_AND_EXTRACT(silo_025_infile,'rain',$
                                              offset=[silo_025_box_tx(1),silo_025_box_tx(0),0],$
                                              count=[silo_025_nlon,silo_025_nlat,1]))
silo_n144_amean_precip=REFORM(OPEN_AND_EXTRACT(silo_n144_infile,'rain',$
                                               offset=[silo_n144_box_tx(1),silo_n144_box_tx(0),0],$
                                               count=[silo_n144_nlon,silo_n144_nlat,1]))
trmm_n144_amean_precip=REFORM(OPEN_AND_EXTRACT(trmm_n144_infile,'precip',$
                                               offset=[trmm_n144_box_tx(1),trmm_n144_box_tx(0),0],$
                                               count=[trmm_n144_nlon,trmm_n144_nlat,1]))

silo_merge_trmm=fltarr(higam_nlon,higam_nlat)
FOR j=0,higam_nlon-1 DO BEGIN
   FOR k=0,higam_nlat-1 DO BEGIN
      IF (silo_n144_amean_precip(j,k) gt 1000) THEN BEGIN
         silo_merge_trmm(j,k)=trmm_n144_amean_precip(j,k)
      ENDIF ELSE $
         silo_merge_trmm(j,k)=silo_n144_amean_precip(j,k)
   ENDFOR
ENDFOR

diff_higam_xcquc_silo=fltarr(higam_nlon,higam_nlat)
FOR j=0,higam_nlon-1 DO BEGIN
   FOR k=0,higam_nlat-1 DO BEGIN
      IF silo_n144_amean_precip(j,higam_nlat-k-1) lt 1000 THEN BEGIN
         diff_higam_xcquc_silo(j,k)=higam_xcquc_amean_precip(j,k)-silo_n144_amean_precip(j,higam_nlat-k-1)
      ENDIF ELSE BEGIN
         diff_higam_xcquc_silo(j,k)=!Values.F_NaN
      ENDELSE
      diff_higam_xcquc_silo_merge_trmm(j,k)=higam_xcquc_amean_precip(j,k)-silo_merge_trmm(j,higam_nlat-k-1)
   ENDFOR
ENDFOR

psfile='/home/ss901165/idl/queensland/higam/clim_precip/higam_clim_precip_compare_silo.annual_mean_silo_n144.ps'   
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=silo_n144_amean_precip,X=silo_n144_longitude,Y=silo_n144_latitude,$
    TITLE='Annual-mean rainfall from SILO at N144 (1900-2008)',/NOLINES
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higam/clim_precip/higam_clim_precip_compare_silo.annual_mean_trmm_n144.ps'   
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=trmm_n144_amean_precip,X=trmm_n144_longitude,Y=trmm_n144_latitude,$
    TITLE='Annual-mean rainfall from TRMM at N144 (1900-2008)',/NOLINES
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higam/clim_precip/higam_clim_precip_compare_silo.annual_mean_silo-merge-trmm_n144.ps'   
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=silo_merge_trmm,X=silo_n144_longitude,Y=silo_n144_latitude,$
    TITLE='Annual-mean rainfall from SILO+TRMM at N144 (1900-2008)',/NOLINES
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higam/clim_precip/higam_clim_precip_compare_silo.annual_mean_silo_025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=silo_025_amean_precip,X=silo_025_longitude,Y=silo_025_latitude,$
    TITLE='Annual-mean rainfall from SILO at 0.25x0.25 (1900-2008)',/NOLINES
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higam/clim_precip/higam_clim_precip_compare_silo.annual_mean_higam_xcquc_n144.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=higam_xcquc_amean_precip,X=higam_longitude,Y=higam_latitude,$
    TITLE='Annual-mean rainfall from HiGAM at N144 (eafwb, 74 years)',/NOLINES
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higam/clim_precip/higam_clim_precip_compare_silo.annual_mean_higam_xcquc-minus-silo_n144.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff_annual)+1,/REV
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_diff_annual
CON,FIELD=diff_higam_xcquc_silo,X=higam_longitude,Y=higam_latitude,$
    TITLE='Annual-mean rainfall for HiGAM (xcquc, 23 years) minus SILO (1900-2008) at N144',/NOLINES
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higam/clim_precip/higam_clim_precip_compare_silo.annual_mean_higam_xcquc-minus-silo-merge-trmm_n144.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=100,/PORTRAIT
;PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff_annual)+1,/REV
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_diff_annual
CON,FIELD=diff_higam_xcquc_silo_merge_trmm,X=higam_longitude,Y=higam_latitude,$
    TITLE='Annual-mean rain for HiGAM (xcquc) minus SILO (land) + TRMM (ocean)',/NOLINES,/CB_RIGHT
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

STOP

END

