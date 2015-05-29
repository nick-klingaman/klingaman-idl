PRO hadgem3kpp_fcorr_diff_salcorr
  
; Plot the difference in flux corrections applied between runs 
; with and without salinity corrections.

fcorr_nosal_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/heat_corrections/hadgem3_1.5xentrain_ensmean_1mtop_3hr_junstart_ukmo.jun-may_mmeans.flxcorr.n96.nc'
;fcorr_sal30_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/heat_corrections/hadgem3_1.5xentrain_ensmean_1mtop_3hr_junstart_sal30.jun-may_mmeans.flxcorr.n96.nc'
fcorr_sal60_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/heat_corrections/hadgem3_1.5xentrain_ensmean_1mtop_3hr_junstart_ukmo_sal60.jun-may_mmeans.flxcorr.n96.nc'

depth_infile='/home/ss901165/kpp_ocean2/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_junstart/xfctb_i0/KPPocean_dec_dmeans.nc'

box=[-30,40,30,200]
n_months=12
months=['jun','jul','aug','sep','oct','nov','dec','jan','feb','mar','apr','may']

mylevs_diff_intz=['-150','-130','-110','-90','-70','-50','-30','-10','10','30','50','70','90','110','130','150']
mylevs_diff_mag_intz=['-600','-520','-440','-360','-280','-200','-120','-40','40','120','200','280','360','440','520','600']

mylevs_mag_intz=['0','80','160','240','320','400','480','560','640','720','800','880','960','1040','1120','1200']
mylevs_intz=['-850','-750','-650','-550','-450','-350','-250','-150','-50','50','150','250','350','450','550','650','750','850']

n_sets=2
FOR i=1,n_sets-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         salcorr_infile=fcorr_sal30_infile
         nocorr_infile=fcorr_nosal_infile
         salcorr_name='sal30'
         nocorr_name='nosal'
         salcorr_title='30-day salinity relaxation'
         nocorr_title='no salinity relaxation'
      END
      1 : BEGIN
         salcorr_infile=fcorr_sal60_infile
         nocorr_infile=fcorr_nosal_infile
         salcorr_name='ukmo_sal60'
         nocorr_name='ukmo_nosal'
         salcorr_title='60-day salinity relaxation'
         nocorr_title='no salinity relaxation'
      END
   ENDCASE
   
   salcorr_longitude=OPEN_AND_EXTRACT(salcorr_infile,'longitude')
   salcorr_latitude=OPEN_AND_EXTRACT(salcorr_infile,'latitude')
   DEFINE_BOUNDARIES,box,salcorr_latitude,salcorr_longitude,salcorr_box_tx,/LIMIT
   salcorr_nlon=N_ELEMENTS(salcorr_longitude)
   salcorr_nlat=N_ELEMENTS(salcorr_latitude)
   
   nocorr_longitude=OPEN_AND_EXTRACT(nocorr_infile,'longitude')
   nocorr_latitude=OPEN_AND_EXTRACT(nocorr_infile,'latitude')
   DEFINE_BOUNDARIES,box,nocorr_latitude,nocorr_longitude,nocorr_box_tx,/LIMIT
   nocorr_nlon=N_ELEMENTS(nocorr_longitude)
   nocorr_nlat=N_ELEMENTS(nocorr_latitude)

   depth_longitude=OPEN_AND_EXTRACT(depth_infile,'longitude')
   depth_latitude=OPEN_AND_EXTRACT(depth_infile,'latitude')
   DEFINE_BOUNDARIES,box,depth_latitude,depth_longitude,depth_box_tx,/LIMIT
   depth_nlon=N_ELEMENTS(depth_longitude)
   depth_nlat=N_ELEMENTS(depth_latitude)

   z=OPEN_AND_EXTRACT(salcorr_infile,'z')
   nz=N_ELEMENTS(z)

   h=OPEN_AND_EXTRACT(depth_infile,'h')

   salcorr_fcorr=OPEN_AND_EXTRACT(salcorr_infile,'fcorr',$
                                  offset=[salcorr_box_tx(1),salcorr_box_tx(0),0,0],$
                                  count=[salcorr_nlon,salcorr_nlat,nz,n_months])
   
   nocorr_fcorr=OPEN_AND_EXTRACT(nocorr_infile,'fcorr',$
                                 offset=[nocorr_box_tx(1),nocorr_box_tx(0),0,0],$
                                 count=[nocorr_nlon,nocorr_nlat,nz,n_months])

   FOR j=0,salcorr_nlon-1 DO $
      FOR k=0,salcorr_nlat-1 DO $
         IF TOTAL(salcorr_fcorr(j,k,*,0)) eq 0 THEN $
            salcorr_fcorr(j,k,*,*)=!Values.F_NaN
   FOR j=0,nocorr_nlon-1 DO $
      FOR k=0,nocorr_nlat-1 DO $
         IF TOTAL(nocorr_fcorr(j,k,*,0)) eq 0 THEN $
            nocorr_fcorr(j,k,*,*)=!Values.F_NaN

   salcorr_fcorr_mag_intz=fltarr(salcorr_nlon,salcorr_nlat,n_months)
   salcorr_fcorr_intz=fltarr(salcorr_nlon,salcorr_nlat,n_months)
   nocorr_fcorr_mag_intz=fltarr(nocorr_nlon,nocorr_nlat,n_months)
   nocorr_fcorr_intz=fltarr(nocorr_nlon,nocorr_nlat,n_months)

   rmsd_salcorr_nocorr_intz=fltarr(salcorr_nlon,salcorr_nlat,n_months)

   FOR j=0,salcorr_nlon-1 DO BEGIN
      FOR k=0,salcorr_nlat-1 DO BEGIN
         FOR m=0,n_months-1 DO BEGIN
            FOR n=0,nz-1 DO BEGIN
               salcorr_fcorr_mag_intz(j,k,m)=salcorr_fcorr_mag_intz(j,k,m)+ABS(salcorr_fcorr(j,k,n,m))*ABS((h(n)))
               salcorr_fcorr_intz(j,k,m)=salcorr_fcorr_intz(j,k,m)+salcorr_fcorr(j,k,n,m)*ABS(h(n))
            ENDFOR
         ENDFOR
      ENDFOR
   ENDFOR

;   salcorr_fcorr_intz(*,*,*)=salcorr_fcorr_intz(*,*,*)
;   salcorr_fcorr_mag_intz(*,*,*)=salcorr_fcorr_intz(*,*,*)
                             
   FOR j=0,nocorr_nlon-1 DO BEGIN
      FOR k=0,nocorr_nlat-1 DO BEGIN
         FOR m=0,n_months-1 DO BEGIN
            FOR n=0,nz-1 DO BEGIN
               nocorr_fcorr_mag_intz(j,k,m)=nocorr_fcorr_mag_intz(j,k,m)+ABS(nocorr_fcorr(j,k,n,m))*ABS((h(n)))
               nocorr_fcorr_intz(j,k,m)=nocorr_fcorr_intz(j,k,m)+nocorr_fcorr(j,k,n,m)*ABS(h(n))
            ENDFOR
         ENDFOR
      ENDFOR
   ENDFOR

;   nocorr_fcorr_intz(*,*,*)=nocorr_fcorr_intz(*,*,*)/TOTAL(h)
   
   diff_salcorr_nocorr_intz=salcorr_fcorr_intz-nocorr_fcorr_intz
   diff_salcorr_nocorr_mag_intz=salcorr_fcorr_mag_intz-nocorr_fcorr_mag_intz

   FOR j=0,n_months-1 DO BEGIN
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_diff_salcorr.mag_fcorrintz_'+salcorr_name+'.'+months(j)+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=200,XOFFSET=1000,YOFFSET=1800,TFONT=2,TCHARSIZE=90,SPACE3=400           
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_mag_intz)+1
      LEVS,MANUAL=mylevs_mag_intz
      MAP,LATMIN=box(0),LONMIN=box(1),LATMAX=box(2),LONMAX=box(3)
      CON,X=salcorr_longitude,Y=salcorr_latitude,FIELD=REFORM(salcorr_fcorr_mag_intz(*,*,j)),TITLE='Depth-int mag of '+months(j)+'-mean fcorr for '+$
          salcorr_title,CB_TITLE='W m!U-2!N',/BLOCK,/NOLINES
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_diff_salcorr.mag_fcorrintz_'+nocorr_name+'.'+months(j)+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=200,XOFFSET=1000,YOFFSET=1800,TFONT=2,TCHARSIZE=90,SPACE3=400           
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_mag_intz)+1
      LEVS,MANUAL=mylevs_mag_intz
      MAP,LATMIN=box(0),LONMIN=box(1),LATMAX=box(2),LONMAX=box(3)
      CON,X=nocorr_longitude,Y=nocorr_latitude,FIELD=REFORM(nocorr_fcorr_mag_intz(*,*,j)),TITLE='Depth-int mag of '+months(j)+'-mean fcorr for '+$
          nocorr_title,CB_TITLE='W m!U-2!N',/BLOCK,/NOLINES
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_diff_salcorr.fcorrintz_'+salcorr_name+'.'+months(j)+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=200,XOFFSET=1000,YOFFSET=1800,TFONT=2,TCHARSIZE=90,SPACE3=400           
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_intz)+1
      LEVS,MANUAL=mylevs_intz
      MAP,LATMIN=box(0),LONMIN=box(1),LATMAX=box(2),LONMAX=box(3)
      CON,X=salcorr_longitude,Y=salcorr_latitude,FIELD=REFORM(salcorr_fcorr_intz(*,*,j)),TITLE='Depth-int '+months(j)+'-mean fcorr for '+$
          salcorr_title,CB_TITLE='W m!U-2!N',/BLOCK,/NOLINES
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_diff_salcorr.fcorrintz_'+nocorr_name+'.'+months(j)+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=200,XOFFSET=1000,YOFFSET=1800,TFONT=2,TCHARSIZE=90,SPACE3=400           
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_intz)+1
      LEVS,MANUAL=mylevs_intz
      MAP,LATMIN=box(0),LONMIN=box(1),LATMAX=box(2),LONMAX=box(3)
      CON,X=nocorr_longitude,Y=nocorr_latitude,FIELD=REFORM(nocorr_fcorr_intz(*,*,j)),TITLE='Depth-int '+months(j)+'-mean fcorr for '+$
          nocorr_title,CB_TITLE='W m!U-2!N',/BLOCK,/NOLINES
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_diff_salcorr.diff_mag_fcorrintz_'+salcorr_name+'-minus-'+nocorr_name+$
             '.'+months(j)+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=200,XOFFSET=1000,YOFFSET=1800,TFONT=2,TCHARSIZE=90,SPACE3=400           
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff_mag_intz)+1
      LEVS,MANUAL=mylevs_diff_mag_intz
      MAP,LATMIN=box(0),LONMIN=box(1),LATMAX=box(2),LONMAX=box(3)
      CON,X=salcorr_longitude,Y=salcorr_latitude,FIELD=REFORM(diff_salcorr_nocorr_mag_intz(*,*,j)),TITLE='Diff in depth-int mag of '+months(j)+'-mean fcorr for '+$
          salcorr_title+' minus '+nocorr_title,CB_TITLE='W m!U-2!N',/BLOCK,/NOLINES
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_diff_salcorr.diff_fcorrintz_'+salcorr_name+'-minus-'+nocorr_name+$
             '.'+months(j)+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=200,XOFFSET=1000,YOFFSET=1800,TFONT=2,TCHARSIZE=90,SPACE3=400           
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff_intz)+1
      LEVS,MANUAL=mylevs_diff_intz
      MAP,LATMIN=box(0),LONMIN=box(1),LATMAX=box(2),LONMAX=box(3)
      CON,X=salcorr_longitude,Y=salcorr_latitude,FIELD=REFORM(diff_salcorr_nocorr_intz(*,*,j)),TITLE='Diff in depth-int '+months(j)+'-mean fcorr for '+$
          salcorr_title+' minus '+nocorr_title,CB_TITLE='W m!U-2!N',/BLOCK,/NOLINES
      AXES
      PSCLOSE,/NOVIEW
   ENDFOR
ENDFOR

STOP
END



