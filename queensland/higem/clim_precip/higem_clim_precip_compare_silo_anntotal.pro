PRO higem_clim_precip_compare_silo_anntotal

higem_xbylr_indir='/home/ss901165/higem_qccce/hpcx_control_xbylr'
higem_eafeb_indir='/home/ss901165/higem_qccce/es_control_eafeb'
silo_025_indir='/home/ss901165/datasets_mango/SILO/one_quarter'
silo_n144_indir='/home/ss901165/datasets_mango/SILO/n144'
trmm_n144_indir='/home/ss901165/datasets_mango/TRMM_3B42V6/n144'

mask_025_file='/home/ss901165/datasets/SILO/one_quarter_lsm.nc'

months=['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']
months_long=['January','February','March','April','May','June','July','August','September','October','November','December']
;n_months=N_ELEMENTS(months)
n_months=12

box_plot=[-44,112,-10,154]
box=[-45,110,-10,155]
mylevs=['0','0.5','1.0','1.5','2.0','2.5','3.0','3.5','4.0','4.5','5.0','6.0','7.0','8.0','9.0','10.0']
;mylevs_annual=['1.0','1.25','1.5','1.75','2.0','2.25','2.5','2.75','3.0','3.5','4.0','4.5','5.0','5.5','6.0']
mylevs_annual=['1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16']
mylevs_diff=['-6','-5','-4','-3','-2.5','-2','-1.5','-1','-0.5','0','0.5','1','1.5','2','2.5','3','4','5']
;mylevs_diff_annual=['-2.0','-1.75','-1.5','-1.25','-1.0','-0.75','-0.5','-0.25','0.0','0.25','0.5','0.75','1.0','1.25','1.5','1.75','2.0']
mylevs_diff_annual=['-500','-400','-300','-200','-150','-100','-50','0','50','100','150','200','300','400','500']

FOR i=0,n_months-1 DO BEGIN
   higem_xbylr_infile=higem_xbylr_indir+'/higem_xbylr.'+months(i)+'_mmean_clim.h9-t5.precip.aus_domain.nc'
   higem_eafeb_infile=higem_eafeb_indir+'/higem_eafeb.'+months(i)+'_mmean_clim.h9-w8.precip.aus_domain.nc'
   silo_025_infile=silo_025_indir+'/SILO_clim_precip.'+months(i)+'_mmean.1900-2008.0.25x0.25.nc'
   silo_n144_infile=silo_n144_indir+'/SILO.'+months(i)+'_mmean_clim.1900-2008.n144.nc'
   trmm_n144_infile=trmm_n144_indir+'/TRMM_3B42v6A.'+months(i)+'_mmean_clim.1999-2008.n144.nc'
   IF i eq 0 THEN BEGIN
      higem_latitude=OPEN_AND_EXTRACT(higem_xbylr_infile,'latitude')
      higem_longitude=OPEN_AND_EXTRACT(higem_xbylr_infile,'longitude')
      DEFINE_BOUNDARIES,box,higem_latitude,higem_longitude,higem_box_tx,/LIMIT
      higem_nlon=N_ELEMENTS(higem_longitude)
      higem_nlat=N_ELEMENTS(higem_latitude)
      
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

      mask_025_longitude=OPEN_AND_EXTRACT(mask_025_file,'longitude')
      mask_025_latitude=OPEN_AND_EXTRACT(mask_025_file,'latitude')
      DEFINE_BOUNDARIES,box,mask_025_latitude,mask_025_longitude,mask_025_box_tx,/LIMIT
      mask_025_nlon=N_ELEMENTS(mask_025_longitude)
      mask_025_nlat=N_ELEMENTS(mask_025_latitude)
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
   mask_025=REFORM(OPEN_AND_EXTRACT(mask_025_file,'lsm',$
                                    offset=[mask_025_box_tx(1),mask_025_box_tx(0),0,0],$
                                    count=[mask_025_nlon,mask_025_nlat,1,1]))

   silo_merge_trmm=fltarr(higem_nlon,higem_nlat)
   FOR j=0,higem_nlon-1 DO BEGIN
      FOR k=0,higem_nlat-1 DO BEGIN
         IF (silo_n144_mmean_precip(j,k) gt 1000) THEN BEGIN
            silo_merge_trmm(j,k)=trmm_n144_mmean_precip(j,k)
         ENDIF ELSE $
            silo_merge_trmm(j,k)=silo_n144_mmean_precip(j,k)
      ENDFOR
   ENDFOR

   silo_025_mmean_precip[where(mask_025 eq 0)]=!Values.F_NaN
   
   psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.'+months(i)+'_mmean_silo_n144.ps'   
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=silo_n144_mmean_precip,X=silo_n144_longitude,Y=silo_n144_latitude,$
       TITLE=months_long(i)+'-mean rainfall from SILO at N144 (1900-2008)',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.'+months(i)+'_mmean_silo_025.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=silo_025_mmean_precip,X=silo_025_longitude,Y=silo_025_latitude,$
       TITLE=months_long(i)+'-mean rainfall from SILO at 0.25x0.25 (1900-2008)',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.'+months(i)+'_mmean_trmm_n144.ps'   
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=trmm_n144_mmean_precip,X=trmm_n144_longitude,Y=trmm_n144_latitude,$
       TITLE=months_long(i)+'-mean rainfall from TRMM 3B42v6A at N144 (1999-2008)',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.'+months(i)+'_mmean_silo-merge-trmm_n144.ps'   
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=silo_merge_trmm,X=silo_n144_longitude,Y=trmm_n144_latitude,$
       TITLE=months_long(i)+'-mean rainfall from SILO (land) and TRMM 3B42v6A (ocean) at N144 (1999-2008)',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.'+months(i)+'_mmean_higem_eafeb_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=higem_eafeb_mmean_precip,X=higem_longitude,Y=higem_latitude,$
       TITLE=months_long(i)+'-mean rainfall from HiGEM at N144 (eafwb, 74 years)',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.'+months(i)+'_mmean_higem_xbylr_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=higem_xbylr_mmean_precip,X=higem_longitude,Y=higem_latitude,$
       TITLE=months_long(i)+'-mean rainfall from HiGEM at N144 (xbylr, 81 years)',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   diff_higem_xbylr_silo=fltarr(higem_nlon,higem_nlat)
   diff_higem_eafeb_silo=fltarr(higem_nlon,higem_nlat)
   diff_higem_xbylr_silo_merge_trmm=fltarr(higem_nlon,higem_nlat)
   diff_higem_eafeb_silo_merge_trmm=fltarr(higem_nlon,higem_nlat)
   FOR j=0,higem_nlon-1 DO BEGIN
      FOR k=0,higem_nlat-1 DO BEGIN
         IF silo_n144_mmean_precip(j,higem_nlat-k-1) lt 1000 THEN BEGIN
            diff_higem_xbylr_silo(j,k)=higem_xbylr_mmean_precip(j,k)-silo_n144_mmean_precip(j,higem_nlat-k-1)
            diff_higem_eafeb_silo(j,k)=higem_eafeb_mmean_precip(j,k)-silo_n144_mmean_precip(j,higem_nlat-k-1)
         ENDIF ELSE BEGIN
            diff_higem_xbylr_silo(j,k)=!Values.F_NaN
            diff_higem_eafeb_silo(j,k)=!Values.F_NaN
         ENDELSE
         diff_higem_xbylr_silo_merge_trmm(j,k)=higem_xbylr_mmean_precip(j,k)-silo_merge_trmm(j,higem_nlat-k-1)
         diff_higem_eafeb_silo_merge_trmm(j,k)=higem_eafeb_mmean_precip(j,k)-silo_merge_trmm(j,higem_nlat-k-1)
      ENDFOR
   ENDFOR
                     
   psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.'+months(i)+'_mmean_higem_xbylr-minus-silo_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV,white=[11,12]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_diff
   CON,FIELD=diff_higem_xbylr_silo,X=higem_longitude,Y=higem_latitude,$
       TITLE=months_long(i)+'-mean rainfall for HiGEM (xbylr, 81 years) minus SILO (1900-2008) at N144',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.'+months(i)+'_mmean_higem_eafeb-minus-silo_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV,white=[11,12]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_diff
   CON,FIELD=diff_higem_eafeb_silo,X=higem_longitude,Y=higem_latitude,$
       TITLE=months_long(i)+'-mean rainfall for HiGEM (eafeb, 74 years) minus SILO (1900-2008) at N144',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.'+months(i)+'_mmean_higem_xbylr-minus-silo-merge-trmm_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV,white=[11,12]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_diff
   CON,FIELD=diff_higem_xbylr_silo_merge_trmm,X=higem_longitude,Y=higem_latitude,$
       TITLE=months_long(i)+'-mean rainfall for HiGEM (xbylr, 81 years) minus SILO (land) and TRMM (ocean)',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.'+months(i)+'_mmean_higem_eafeb-minus-silo-merge-trmm_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV,white=[11,12]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_diff
   CON,FIELD=diff_higem_eafeb_silo_merge_trmm,X=higem_longitude,Y=higem_latitude,$
       TITLE=months_long(i)+'-mean rainfall for HiGEM (eafeb, 74 years) minus SILO (land) and TRMM (ocean)',/NOLINES,CB_TITLE='Rainfall (mm day!U-1!N)'
   AXES,XSTEP=5,YSTEP=5
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
                                              offset=[silo_025_box_tx(1),silo_025_box_tx(0)],$
                                              count=[silo_025_nlon,silo_025_nlat]))
silo_n144_amean_precip=REFORM(OPEN_AND_EXTRACT(silo_n144_infile,'rain',$
                                               offset=[silo_n144_box_tx(1),silo_n144_box_tx(0),0],$
                                               count=[silo_n144_nlon,silo_n144_nlat,1]))
trmm_n144_amean_precip=REFORM(OPEN_AND_EXTRACT(trmm_n144_infile,'precip',$
                                               offset=[trmm_n144_box_tx(1),trmm_n144_box_tx(0),0],$
                                               count=[trmm_n144_nlon,trmm_n144_nlat,1]))

silo_merge_trmm=fltarr(higem_nlon,higem_nlat)
FOR j=0,higem_nlon-1 DO BEGIN
   FOR k=0,higem_nlat-1 DO BEGIN
      IF (silo_n144_amean_precip(j,k) gt 1000) THEN BEGIN
         silo_merge_trmm(j,k)=trmm_n144_amean_precip(j,k)
      ENDIF ELSE $
         silo_merge_trmm(j,k)=silo_n144_amean_precip(j,k)
   ENDFOR
ENDFOR

diff_higem_eafeb_silo=fltarr(higem_nlon,higem_nlat)
diff_higem_xbylr_silo=fltarr(higem_nlon,higem_nlat)
FOR j=0,higem_nlon-1 DO BEGIN
   FOR k=0,higem_nlat-1 DO BEGIN
      IF silo_n144_amean_precip(j,higem_nlat-k-1) lt 1000 THEN BEGIN
         diff_higem_eafeb_silo(j,k)=higem_eafeb_amean_precip(j,k)-silo_n144_amean_precip(j,higem_nlat-k-1)
         diff_higem_xbylr_silo(j,k)=higem_xbylr_amean_precip(j,k)-silo_n144_amean_precip(j,higem_nlat-k-1)
      ENDIF ELSE BEGIN
         diff_higem_eafeb_silo(j,k)=!Values.F_NaN
         diff_higem_xbylr_silo(j,k)=!Values.F_NaN
      ENDELSE
      diff_higem_xbylr_silo_merge_trmm(j,k)=higem_xbylr_amean_precip(j,k)-silo_merge_trmm(j,higem_nlat-k-1)
      diff_higem_eafeb_silo_merge_trmm(j,k)=higem_eafeb_amean_precip(j,k)-silo_merge_trmm(j,higem_nlat-k-1)
   ENDFOR
 ENDFOR

silo_025_amean_precip[where(mask_025 eq 0)]=!Values.F_NaN

print,'Now plotting SILO annual total at N144'
psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.annual_total_silo_n144.ps'   
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=silo_n144_amean_precip*3.65,X=silo_n144_longitude,Y=silo_n144_latitude,$
    TITLE='Mean annual-total rainfall from SILO at N144 (1900-2008)',/NOLINES,CB_TITLE='Rainfall (mm x 100)'
AXES,XSTEP=5,YSTEP=5
PSCLOSE

psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.annual_total_trmm_n144.ps'   
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=trmm_n144_amean_precip*3.65,X=trmm_n144_longitude,Y=trmm_n144_latitude,$
    TITLE='Mean annual-total rainfall from TRMM at N144 (1900-2008)',/NOLINES,CB_TITLE='Rainfall (mm x 100)'
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.annual_total_silo-merge-trmm_n144.ps'   
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=silo_merge_trmm*3.65,X=silo_n144_longitude,Y=silo_n144_latitude,$
    TITLE='Mean annual-total rainfall from SILO+TRMM at N144 (1900-2008)',/NOLINES,CB_TITLE='Rainfall (mm x 100)'
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.annual_total_silo_025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=silo_025_amean_precip*3.65,X=silo_025_longitude,Y=silo_025_latitude,$
                                ;TITLE='Mean annual-total rainfall from SILO at 0.25x0.25 (1900-2008)',
    /NOLINES,CB_TITLE='Rainfall (mm x 100)'
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.annual_total_higem_eafeb_n144.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=higem_eafeb_amean_precip*3.65,X=higem_longitude,Y=higem_latitude,$
    TITLE='Mean annual-total rainfall from HiGEM at N144 (eafwb, 74 years)',/NOLINES,CB_TITLE='Rainfall (mm x 100)'
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.annual_total_higem_xbylr_n144.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=higem_xbylr_amean_precip*3.65,X=higem_longitude,Y=higem_latitude,$
    TITLE='Mean annual-total rainfall from HiGEM at N144 (eafwb, 74 years)',/NOLINES,CB_TITLE='Rainfall (mm x 100)'
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.annual_total_higem_eafeb-minus-silo_n144.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_annual)+1,/REV,white=[10,11]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_diff_annual
CON,FIELD=diff_higem_eafeb_silo*3.65,X=higem_longitude,Y=higem_latitude,$
    TITLE='Mean annual-total rainfall for HiGEM (eafwb, 74 years) minus SILO (1900-2008) at N144',/NOLINES,CB_TITLE='Rainfall (mm x 100)'
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.annual_total_higem_xbylr-minus-silo_n144.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
;PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_annual)+1,/REV,white=[10,11]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_diff_annual
CON,FIELD=diff_higem_xbylr_silo*3.65,X=higem_longitude,Y=higem_latitude,$
    TITLE='Mean annual-total rainfall for HiGEM (xbylr, 81 years) minus SILO (1900-2008) at N144',/NOLINES,CB_TITLE='Rainfall (mm x 100)'
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.annual_total_higem_eafeb-minus-silo-merge-trmm_n144.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=100,/PORTRAIT
;PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_annual)+1,/REV,white=[10,11]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_diff_annual
CON,FIELD=diff_higem_eafeb_silo_merge_trmm*3.65,X=higem_longitude,Y=higem_latitude,$
    TITLE='Mean annual-total rain for HiGEM (eafwb) minus SILO (land) + TRMM (ocean)',/NOLINES,/CB_RIGHT,CB_TITLE='Rainfall (mm x 100)'
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/clim_precip/higem_clim_precip_compare_silo.annual_total_higem_xbylr-minus-silo-merge-trmm_n144.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=100,/PORTRAIT
;PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=6500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_annual)+1,/REV,white=[10,11]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_diff_annual
CON,FIELD=diff_higem_xbylr_silo_merge_trmm*3.65,X=higem_longitude,Y=higem_latitude,$
    TITLE='Mean annual-total rain for HiGEM (xbylr) minus SILO (land) + TRMM (ocean)',/NOLINES,/CB_RIGHT,CB_TITLE='Rainfall (mm x 100)'
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

STOP

END

