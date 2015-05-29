PRO higem_clim_precip_compare_silo

higem_xbylr_indir='/home/ss901165/higem_qccce/hpcx_control_xbylr'
higem_eafeb_indir='/home/ss901165/higem_qccce/es_control_eafeb'
silo_025_indir='/home/ss901165/datasets_mango/SILO/one_quarter'
silo_n144_indir='/home/ss901165/datasets_mango/SILO/n144'
trmm_n144_indir='/home/ss901165/datasets_mango/TRMM_3B42V6/n144'

months=['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']
months_long=['January','February','March','April','May','June','July','August','September','October','November','December']
n_months=12
seasons=['dec-feb','mar-may','jun-aug','sep-nov']
seasons_long=['December-February','March-May','June-August','September-November']
n_seasons=N_ELEMENTS(seasons)

box_plot=[-44,100,-10,170]
box=[-45,100,-10,170]

mask_n144_infile='/home/ss901165/um_output/mask_n144_higam.nc'
mask_longitude=OPEN_AND_EXTRACT(mask_n144_infile,'longitude')
mask_latitude=OPEN_AND_EXTRACT(mask_n144_infile,'latitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)

mask=REFORM(OPEN_AND_EXTRACT(mask_n144_infile,'lsm',$
                             offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                             count=[mask_nlon,mask_nlat,1,1]))

mylevs=['0','0.5','1.0','1.5','2.0','2.5','3.0','3.5','4.0','4.5','5.0','6.0','7.0','8.0','10.0','12.0','15.0','18.0']
mylevs_annual=['1.0','1.25','1.5','1.75','2.0','2.25','2.5','2.75','3.0','3.5','4.0','4.5','5.0','5.5','6.0']
mylevs_diff=['-10','-8','-6','-5','-4','-3','-2','-1','-0.5','0','0.5','1','2','3','4','5','6','8','10']
mylevs_diff_annual=['-2.5','-2.0','-1.5','-1.25','-1.0','-0.75','-0.5','-0.25','0.0','0.25','0.5','0.75','1.0','1.25','1.5','2.0','2.5']
mylevs_seasonal=['0.4','0.8','1.2','1.6','2.0','2.4','2.8','3.2','4.0','4.8','5.6','6.4','7.2','8.0','8.8','9.6','10.4']
mylevs_diff_seasonal=['-8.5','-6.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','6.5','8.5']

FOR i=0,0 DO BEGIN
   print,'--> '+months(i)
   higem_xbylr_infile=higem_xbylr_indir+'/higem_xbylr.'+months(i)+'_mmean_clim.h9-t5.precip.aus_domain.nc'
   higem_eafeb_infile=higem_eafeb_indir+'/higem_eafeb.'+months(i)+'_mmean_clim.h9-w8.precip.aus_domain.nc'
   silo_025_infile=silo_025_indir+'/SILO_clim_precip.'+months(i)+'_mmean.1900-2008.0.25x0.25.nc'
   silo_n144_infile=silo_n144_indir+'/SILO.'+months(i)+'_mmean_clim.1900-2008.n144.nc'
   trmm_n144_infile=trmm_n144_indir+'/TRMM_3B42v6A.'+months(i)+'_mmean_clim.1999-2008.n144.nc'
   IF i eq 0 THEN BEGIN      
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

      higem_latitude=OPEN_AND_EXTRACT(higem_xbylr_infile,'latitude')
      higem_longitude=OPEN_AND_EXTRACT(higem_xbylr_infile,'longitude')
      DEFINE_BOUNDARIES,box,higem_latitude,higem_longitude,higem_box_tx,/LIMIT
      DEFINE_BOUNDARIES,[silo_n144_latitude(silo_n144_nlat-1),silo_n144_longitude(0),$
                         silo_n144_latitude(0),silo_n144_longitude(silo_n144_nlon-1)],$
                        higem_latitude,higem_longitude,higem_silo_box_tx
      higem_nlon=N_ELEMENTS(higem_longitude)
      higem_nlat=N_ELEMENTS(higem_latitude)

      trmm_n144_latitude=OPEN_AND_EXTRACT(trmm_n144_infile,'latitude')
      trmm_n144_longitude=OPEN_AND_EXTRACT(trmm_n144_infile,'longitude')
      DEFINE_BOUNDARIES,box,trmm_n144_latitude,trmm_n144_longitude,trmm_n144_box_tx,/LIMIT
      trmm_n144_nlon=N_ELEMENTS(trmm_n144_longitude)
      trmm_n144_nlat=N_ELEMENTS(trmm_n144_latitude)

   ENDIF
   higem_xbylr_mmean_precip=REFORM(OPEN_AND_EXTRACT(higem_xbylr_infile,'precip',$
                                              offset=[higem_box_tx(1),higem_box_tx(0),0,0],$
                                              count=[higem_nlon,higem_nlat,1,1]))*86400.
   higem_eafeb_mmean_precip=REFORM(OPEN_AND_EXTRACT(higem_eafeb_infile,'precip',$
                                                    offset=[higem_box_tx(1),higem_box_tx(0),0,0],$
                                                    count=[higem_nlon,higem_nlat,1,1]))*86400.
   silo_025_mmean_precip=REFORM(OPEN_AND_EXTRACT(silo_025_infile,'rain',$
                                             offset=[silo_025_box_tx(1),silo_025_box_tx(0),0],$
                                             count=[silo_025_nlon,silo_025_nlat,1]))
   silo_n144_mmean_precip=REFORM(OPEN_AND_EXTRACT(silo_n144_infile,'rain',$
                                             offset=[silo_n144_box_tx(1),silo_n144_box_tx(0),0],$
                                             count=[silo_n144_nlon,silo_n144_nlat,1]))
   trmm_n144_mmean_precip=REFORM(OPEN_AND_EXTRACT(trmm_n144_infile,'precip',$
                                                  offset=[trmm_n144_box_tx(1),trmm_n144_box_tx(0),0],$
                                                  count=[trmm_n144_nlon,trmm_n144_nlat,1])) 
   silo_merge_trmm=fltarr(higem_nlon,higem_nlat)
   FOR j=0,higem_nlon-1 DO BEGIN
      FOR k=0,higem_nlat-1 DO BEGIN
         IF (mask(j,k) lt 1) THEN BEGIN
            silo_merge_trmm(j,k)=trmm_n144_mmean_precip(j,k)
         ENDIF ELSE BEGIN
            IF j ge higem_silo_box_tx(1) and j le higem_silo_box_tx(3) and $
               k ge higem_silo_box_tx(0) and k le higem_silo_box_tx(2) THEN BEGIN
               silo_merge_trmm(j,k)=silo_n144_mmean_precip(j-higem_silo_box_tx(1),k-higem_silo_box_tx(0))
            ENDIF ELSE $
               silo_merge_trmm(j,k)=trmm_n144_mmean_precip(j,k)
         ENDELSE
      ENDFOR
   ENDFOR
            
   psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.'+months(i)+'_mmean_silo_n144.ps'   
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=silo_n144_mmean_precip,X=silo_n144_longitude,Y=silo_n144_latitude,$
       TITLE=months_long(i)+'-mean rainfall from SILO at N144 (1900-2008)',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.'+months(i)+'_mmean_silo_025.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=silo_025_mmean_precip,X=silo_025_longitude,Y=silo_025_latitude,$
       TITLE=months_long(i)+'-mean rainfall from SILO at 0.25x0.25 (1900-2008)',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.'+months(i)+'_mmean_trmm_n144.ps'   
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=trmm_n144_mmean_precip,X=trmm_n144_longitude,Y=trmm_n144_latitude,$
       TITLE=months_long(i)+'-mean rainfall from TRMM 3B42v6A at N144 (1999-2008)',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.'+months(i)+'_mmean_silo-merge-trmm_n144.ps'   
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=silo_merge_trmm,X=trmm_n144_longitude,Y=trmm_n144_latitude,$
       TITLE=months_long(i)+'-mean rainfall from SILO (land) and TRMM 3B42v6A (ocean) at N144 (1999-2008)',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.'+months(i)+'_mmean_higem_eafeb_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=higem_eafeb_mmean_precip,X=higem_longitude,Y=higem_latitude,$
       TITLE=months_long(i)+'-mean rainfall from HiGEM at N144 (eafwb, 147 years)',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.'+months(i)+'_mmean_higem_xbylr_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=higem_xbylr_mmean_precip,X=higem_longitude,Y=higem_latitude,$
       TITLE=months_long(i)+'-mean rainfall from HiGEM at N144 (xbylr, 117 years)',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   diff_higem_xbylr_silo=fltarr(higem_nlon,higem_nlat)
   diff_higem_eafeb_silo=fltarr(higem_nlon,higem_nlat)
   diff_higem_xbylr_silo_merge_trmm=fltarr(higem_nlon,higem_nlat)
   diff_higem_eafeb_silo_merge_trmm=fltarr(higem_nlon,higem_nlat)
   FOR j=0,higem_nlon-1 DO BEGIN
      FOR k=0,higem_nlat-1 DO BEGIN
         IF (mask(j,k) eq 1) and j gt higem_silo_box_tx(1) and j lt higem_silo_box_tx(3) and $
            k gt higem_silo_box_tx(0) and k lt higem_silo_box_tx(2) THEN BEGIN
            diff_higem_xbylr_silo(j,k)=higem_xbylr_mmean_precip(j,k)-silo_n144_mmean_precip(j-higem_silo_box_tx(1),higem_nlat-k-1)
            diff_higem_eafeb_silo(j,k)=higem_eafeb_mmean_precip(j,k)-silo_n144_mmean_precip(j-higem_silo_box_tx(1),higem_nlat-k-1)
         ENDIF ELSE BEGIN
            diff_higem_xbylr_silo(j,k)=!Values.F_NaN
            diff_higem_eafeb_silo(j,k)=!Values.F_NaN
         ENDELSE
         diff_higem_xbylr_silo_merge_trmm(j,k)=higem_xbylr_mmean_precip(j,k)-silo_merge_trmm(j,higem_nlat-k-1)
         diff_higem_eafeb_silo_merge_trmm(j,k)=higem_eafeb_mmean_precip(j,k)-silo_merge_trmm(j,higem_nlat-k-1)
      ENDFOR
   ENDFOR
                     
   psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.'+months(i)+'_mmean_higem_xbylr-minus-silo_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV,white=[11,12]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_diff
   CON,FIELD=diff_higem_xbylr_silo,X=higem_longitude,Y=higem_latitude,$
       TITLE=months_long(i)+'-mean rainfall for HiGEM (xbylr, 117 years) minus SILO (1900-2008) at N144',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.'+months(i)+'_mmean_higem_eafeb-minus-silo_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV,white=[11,12]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_diff
   CON,FIELD=diff_higem_eafeb_silo,X=higem_longitude,Y=higem_latitude,$
       TITLE=months_long(i)+'-mean rainfall for HiGEM (eafeb, 147 years) minus SILO (1900-2008) at N144',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.'+months(i)+'_mmean_higem_xbylr-minus-silo-merge-trmm_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV,white=[11,12]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_diff
   CON,FIELD=diff_higem_xbylr_silo_merge_trmm,X=higem_longitude,Y=higem_latitude,$
       TITLE=months_long(i)+'-mean rainfall for HiGEM (xbylr, 117 years) minus SILO (land) and TRMM (ocean)',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.'+months(i)+'_mmean_higem_eafeb-minus-silo-merge-trmm_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV,white=[11,12]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_diff
   CON,FIELD=diff_higem_eafeb_silo_merge_trmm,X=higem_longitude,Y=higem_latitude,$
       TITLE=months_long(i)+'-mean rainfall for HiGEM (eafeb, 147 years) minus SILO (land) and TRMM (ocean)',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

ENDFOR

FOR i=0,n_seasons-1 DO BEGIN
   higem_eafeb_infile=higem_eafeb_indir+'/higem_eafeb.'+seasons(i)+'_smeans.h9-w8.precip.aus_domain.nc'
   silo_n144_infile=silo_n144_indir+'/SILO_precip.'+seasons(i)+'_smeans.1900-2008.n144.nc'
   trmm_n144_infile=trmm_n144_indir+'/TRMM_3B42v6A.'+seasons(i)+'_smeans.1999-2009.n144.nc'
   
   higem_eafeb_nyears=149
   silo_nyears=109
   if seasons(i) eq 'dec-feb' then begin
      trmm_nyears=10
   endif else $
      trmm_nyears=11

   higem_eafeb_seasonal_precip=REFORM(OPEN_AND_EXTRACT(higem_eafeb_infile,'precip',$
                                                       offset=[higem_box_tx(1),higem_box_tx(0),0],$
                                                       count=[higem_nlon,higem_nlat,higem_eafeb_nyears]))*86400.
   silo_n144_seasonal_precip=REFORM(OPEN_AND_EXTRACT(silo_n144_infile,'rain',$
                                                     offset=[silo_n144_box_tx(1),silo_n144_box_tx(0),0],$
                                                     count=[silo_n144_nlon,silo_n144_nlat,silo_nyears]))
   trmm_n144_seasonal_precip=REFORM(OPEN_AND_EXTRACT(trmm_n144_infile,'precip',$
                                                     offset=[trmm_n144_box_tx(1),trmm_n144_box_tx(0),0],$
                                                     count=[trmm_n144_nlon,trmm_n144_nlat,trmm_nyears]))
   if total(where(trmm_n144_seasonal_precip ge 10000)) gt 1 then $
      trmm_n144_seasonal_precip[where(trmm_n144_seasonal_precip ge 10000)]=!Values.F_NaN
   
   higem_eafeb_smean_precip=fltarr(higem_nlon,higem_nlat)
   silo_smean_precip=fltarr(silo_n144_nlon,silo_n144_nlat)
   trmm_smean_precip=fltarr(trmm_n144_nlon,trmm_n144_nlat)

   FOR j=0,higem_nlon-1 DO $
      FOR k=0,higem_nlat-1 DO $
         higem_eafeb_smean_precip(j,k)=MEAN(higem_eafeb_seasonal_precip(j,k,*))
   FOR j=0,silo_n144_nlon-1 DO $
      FOR k=0,silo_n144_nlat-1 DO $
         silo_smean_precip(j,k)=MEAN(silo_n144_seasonal_precip(j,k,*))
   FOR j=0,trmm_n144_nlon-1 DO $
      FOR k=0,trmm_n144_nlat-1 DO $
         trmm_smean_precip(j,k)=MEAN(trmm_n144_seasonal_precip(j,k,*),/NaN)

   silo_merge_trmm=fltarr(higem_nlon,higem_nlat)
   FOR j=0,higem_nlon-1 DO BEGIN
      FOR k=0,higem_nlat-1 DO BEGIN
         IF (mask(j,k) lt 1) THEN BEGIN
            silo_merge_trmm(j,k)=trmm_smean_precip(j,k)
         ENDIF ELSE BEGIN
            IF j gt higem_silo_box_tx(1) and j lt higem_silo_box_tx(3) and $
               k gt higem_silo_box_tx(0) and k lt higem_silo_box_tx(2) THEN BEGIN
               IF silo_smean_precip(j-higem_silo_box_tx(1),k-higem_silo_box_tx(0)) lt 10000 THEN BEGIN
                  silo_merge_trmm(j,k)=silo_smean_precip(j-higem_silo_box_tx(1),k-higem_silo_box_tx(0))
               ENDIF ELSE $
                  silo_merge_trmm(j,k)=trmm_smean_precip(j,k)
            ENDIF ELSE $
               silo_merge_trmm(j,k)=trmm_smean_precip(j,k)
         ENDELSE
      ENDFOR
   ENDFOR
   IF TOTAL(where(silo_merge_trmm gt 1E10)) gt 1 THEN BEGIN
      print,where(silo_merge_trmm gt 1E10)
      STOP
   ENDIF
;   STOP

   diff_higem_eafeb_silo=fltarr(higem_nlon,higem_nlat)   
   diff_higem_eafeb_silo_merge_trmm=fltarr(higem_nlon,higem_nlat)
   FOR j=0,higem_nlon-1 DO BEGIN
      FOR k=0,higem_nlat-1 DO BEGIN
         IF (mask(j,k) eq 1) and j gt higem_silo_box_tx(1) and j lt higem_silo_box_tx(3) and $
            k gt higem_silo_box_tx(0) and k lt higem_silo_box_tx(2) THEN BEGIN
            diff_higem_eafeb_silo(j,k)=higem_eafeb_smean_precip(j,k)-silo_smean_precip(j-higem_silo_box_tx(1),higem_nlat-k-1)
         ENDIF ELSE BEGIN
            diff_higem_eafeb_silo(j,k)=!Values.F_NaN
         ENDELSE   
         diff_higem_eafeb_silo_merge_trmm(j,k)=higem_eafeb_smean_precip(j,k)-silo_merge_trmm(j,higem_nlat-k-1)
      ENDFOR
   ENDFOR

   psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.'+seasons(i)+'_mean_higem_eafeb_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1000,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,XSIZE=18000
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_seasonal)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_seasonal
   CON,FIELD=higem_eafeb_smean_precip,X=higem_longitude,Y=higem_latitude,$
       TITLE=seasons_long(i)+'-mean rainfall from HiGEM (eafeb, h9-w8)',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
   AXES,XSTEP=10,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.'+seasons(i)+'_mean_silo-merge-trmm_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1000,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,XSIZE=18000
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_seasonal)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_seasonal
   CON,FIELD=silo_merge_trmm,X=trmm_n144_longitude,Y=trmm_n144_latitude,$
       TITLE=seasons_long(i)+'-mean rainfall from SILO and TRMM',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
   AXES,XSTEP=10,YSTEP=5
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.'+seasons(i)+'_mean_higem_eafeb-minus-silo-merge-trmm_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1000,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=100,XSIZE=18000
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_seasonal)+1,/REV,white=[9]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_diff_seasonal
   CON,FIELD=diff_higem_eafeb_silo_merge_trmm,X=higem_longitude,Y=higem_latitude,$
       TITLE=seasons_long(i)+'-mean rain for HiGEM (eafeb, h9-w8) minus SILO (land) + TRMM (ocean)',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
   AXES,XSTEP=10,YSTEP=5
   PSCLOSE,/NOVIEW

ENDFOR

higem_eafeb_infile=higem_eafeb_indir+'/higem_eafeb.jan-dec_ameans_clim.h9-w8.precip.aus_domain.nc'
higem_xbylr_infile=higem_xbylr_indir+'/higem_xbylr.jan-dec_ameans_clim.h9-t5.precip.aus_domain.nc'
silo_025_infile=silo_025_indir+'/SILO_clim_precip.annual_mean.1900-2008.0.25x0.25.nc'
silo_n144_infile=silo_n144_indir+'/SILO.jan-dec_amean_clim.1900-2008.n144.nc'
trmm_n144_infile=trmm_n144_indir+'/TRMM_3B42v6A.jan-dec_amean_clim.1999-2008.n144.nc'

higem_eafeb_amean_precip=REFORM(OPEN_AND_EXTRACT(higem_eafeb_infile,'precip',$
                                                 offset=[higem_box_tx(1),higem_box_tx(0),0],$
                                                 count=[higem_nlon,higem_nlat,1]))*86400.
higem_xbylr_amean_precip=REFORM(OPEN_AND_EXTRACT(higem_xbylr_infile,'precip',$
                                                 offset=[higem_box_tx(1),higem_box_tx(0),0,0,0],$
                                                 count=[higem_nlon,higem_nlat,1,1,1]))*86400.
silo_025_amean_precip=REFORM(OPEN_AND_EXTRACT(silo_025_infile,'rain',$
                                              offset=[silo_025_box_tx(1),silo_025_box_tx(0),0],$
                                              count=[silo_025_nlon,silo_025_nlat,1]))
silo_n144_amean_precip=REFORM(OPEN_AND_EXTRACT(silo_n144_infile,'rain',$
                                               offset=[silo_n144_box_tx(1),silo_n144_box_tx(0),0],$
                                               count=[silo_n144_nlon,silo_n144_nlat,1]))
trmm_n144_amean_precip=REFORM(OPEN_AND_EXTRACT(trmm_n144_infile,'precip',$
                                               offset=[trmm_n144_box_tx(1),trmm_n144_box_tx(0),0],$
                                               count=[trmm_n144_nlon,trmm_n144_nlat,1]))

silo_merge_trmm=fltarr(higem_nlon,higem_nlat)
FOR j=0,higem_nlon-1 DO BEGIN
   FOR k=0,higem_nlat-1 DO BEGIN
      IF (mask(j,k) lt 1) THEN BEGIN
         silo_merge_trmm(j,k)=trmm_n144_amean_precip(j,k)
      ENDIF ELSE BEGIN
         IF j gt higem_silo_box_tx(1) and j lt higem_silo_box_tx(3) and $
            k gt higem_silo_box_tx(0) and k lt higem_silo_box_tx(2) THEN BEGIN
            IF silo_n144_amean_precip(j-higem_silo_box_tx(1),k-higem_silo_box_tx(0)) lt 10000 THEN BEGIN
               silo_merge_trmm(j,k)=silo_n144_amean_precip(j-higem_silo_box_tx(1),k-higem_silo_box_tx(0))
            ENDIF ELSE $
               silo_merge_trmm(j,k)=trmm_n144_amean_precip(j,k)
         ENDIF ELSE $
            silo_merge_trmm(j,k)=trmm_n144_amean_precip(j,k)
      ENDELSE
   ENDFOR
ENDFOR
IF TOTAL(where(silo_merge_trmm gt 1E10)) gt 1 THEN BEGIN
   print,where(silo_merge_trmm gt 1E10)
   STOP
ENDIF

diff_higem_xbylr_silo=fltarr(higem_nlon,higem_nlat)   
diff_higem_xbylr_silo_merge_trmm=fltarr(higem_nlon,higem_nlat)
diff_higem_eafeb_silo=fltarr(higem_nlon,higem_nlat)   
diff_higem_eafeb_silo_merge_trmm=fltarr(higem_nlon,higem_nlat)
diff_silo_trmm=fltarr(trmm_n144_nlon,trmm_n144_nlat)
FOR j=0,higem_nlon-1 DO BEGIN
   FOR k=0,higem_nlat-1 DO BEGIN
      IF (mask(j,k) eq 1) and j gt higem_silo_box_tx(1) and j lt higem_silo_box_tx(3) and $
         k gt higem_silo_box_tx(0) and k lt higem_silo_box_tx(2) THEN BEGIN
         diff_higem_xbylr_silo(j,k)=higem_xbylr_amean_precip(j,k)-silo_n144_amean_precip(j-higem_silo_box_tx(1),higem_nlat-k-1)
         diff_higem_eafeb_silo(j,k)=higem_eafeb_amean_precip(j,k)-silo_n144_amean_precip(j-higem_silo_box_tx(1),higem_nlat-k-1)
         diff_silo_trmm(j,k)=(silo_n144_amean_precip(j-higem_silo_box_tx(1),higem_nlat-k-1)-trmm_n144_amean_precip(j,higem_nlat-k-1))*0.5
      ENDIF ELSE BEGIN
         diff_higem_xbylr_silo(j,k)=!Values.F_NaN
         diff_higem_eafeb_silo(j,k)=!Values.F_NaN
         diff_silo_trmm(j,k)=!Values.F_NaN
      ENDELSE   
      diff_higem_xbylr_silo_merge_trmm(j,k)=higem_xbylr_amean_precip(j,k)-silo_merge_trmm(j,higem_nlat-k-1)
      diff_higem_eafeb_silo_merge_trmm(j,k)=higem_eafeb_amean_precip(j,k)-silo_merge_trmm(j,higem_nlat-k-1)
   ENDFOR
ENDFOR

psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.annual_mean_silo_n144.ps'   
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=silo_n144_amean_precip,X=silo_n144_longitude,Y=silo_n144_latitude,$
    TITLE='Annual-mean rainfall from SILO at N144 (1900-2008)',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
AXES,XSTEP=10,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.annual_mean_trmm_n144.ps'   
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=trmm_n144_amean_precip,X=trmm_n144_longitude,Y=trmm_n144_latitude,$
    TITLE='Annual-mean rainfall from TRMM at N144 (1900-2008)',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
AXES,XSTEP=10,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.annual_mean_silo-minus-trmm_n144.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=90,CB_WIDTH=110,XSIZE=18000
;PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_annual)+1,/REV,white=[10,11]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_diff_annual
CON,FIELD=diff_silo_trmm,X=higem_longitude,Y=higem_latitude,$
    TITLE='Difference in annual-mean rainfall for SILO (1900-2008) minus TRMM (1999-2010) over land points only',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
AXES,XSTEP=10,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.annual_mean_silo-merge-trmm_n144.ps'   
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,XSIZE=18000
;PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV,
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=silo_merge_trmm,X=trmm_n144_longitude,Y=trmm_n144_latitude,$
    TITLE='Annual-mean rainfall from SILO and TRMM',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
AXES,XSTEP=10,YSTEP=5
PSCLOSE

psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.annual_mean_silo_025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=silo_025_amean_precip,X=silo_025_longitude,Y=silo_025_latitude,$
    TITLE='Annual-mean rainfall from SILO at 0.25x0.25 (1900-2008)',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.annual_mean_higem_eafeb_n144.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,XSIZE=18000
;PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=higem_eafeb_amean_precip,X=higem_longitude,Y=higem_latitude,$
    TITLE='Annual-mean rainfall from HiGEM at N144 (eafwb, 147 years)',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
AXES,XSTEP=10,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.annual_mean_higem_xbylr_n144.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=higem_xbylr_amean_precip,X=higem_longitude,Y=higem_latitude,$
    TITLE='Annual-mean rainfall from HiGEM at N144 (eafwb, 147 years)',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
AXES,XSTEP=10,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.annual_mean_higem_eafeb-minus-silo_n144.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,XSIZE=18000
;PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_annual)+1,/REV,white=[10,11]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_diff_annual
CON,FIELD=diff_higem_eafeb_silo,X=higem_longitude,Y=higem_latitude,$
    TITLE='Annual-mean rainfall for HiGEM (eafwb, 147 years) minus SILO (1900-2008) at N144',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
AXES,XSTEP=10,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.annual_mean_higem_xbylr-minus-silo_n144.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_annual)+1,/REV,white=[10,11]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_diff_annual
CON,FIELD=diff_higem_xbylr_silo,X=higem_longitude,Y=higem_latitude,$
    TITLE='Annual-mean rainfall for HiGEM (xbylr, 117 years) minus SILO (1900-2008) at N144',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
AXES,XSTEP=10,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.annual_mean_higem_eafeb-minus-silo-merge-trmm_n144.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=100,XSIZE=18000
;PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_annual)+1,/REV,white=[10,11]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_diff_annual
CON,FIELD=diff_higem_eafeb_silo_merge_trmm,X=higem_longitude,Y=higem_latitude,$
    TITLE='Annual-mean rain for HiGEM (eafwb) minus SILO (land) + TRMM (ocean)',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
AXES,XSTEP=10,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.annual_mean_higem_xbylr-minus-silo-merge-trmm_n144.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=100,XSIZE=18000
;PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_annual)+1,/REV,white=[10,11]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_diff_annual
CON,FIELD=diff_higem_xbylr_silo_merge_trmm,X=higem_longitude,Y=higem_latitude,$
    TITLE='Annual-mean rain for HiGEM (xbylr) minus SILO (land) + TRMM (ocean)',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
AXES,XSTEP=10,YSTEP=5
PSCLOSE,/NOVIEW

STOP

END

