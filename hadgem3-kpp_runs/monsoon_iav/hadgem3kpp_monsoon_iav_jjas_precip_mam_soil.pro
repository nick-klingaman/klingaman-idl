PRO hadgem3kpp_monsoon_iav_jjas_precip_mam_soil

; Plot composites of seasonal-mean JJAS rainfall and monthly-mean MAM
; soil moisture from the vn7.4 FOAM/AMIP2 ensemble.

; Seasonal-mean rainfall
jjas_precip_infile='/home/ss901165/um_output3/hadgem3a_foam_control_vn74_blendsst/hadgem3a_foam_ctl_vn74_blendsst.jun-sep_smeans.years1-35.precip.nc'
morph3_precip_infile='/home/ss901165/um_output3/hadgem3_monwg/airxv/hadgem3a_morph3_final_n96_amip2_airxv.jun-sep_smean_clim.1982-2008.precip.nc'
trmm_precip_infile='/home/ss901165/datasets_mango/TRMM_3B42V6/n96/TRMM_3B42v6A.jun-sep_smean_clim.1999-2008.n96.nc'

; Monthly-mean soil moisture
mam_soilmoisture_infile='/home/ss901165/um_output3/hadgem3a_foam_control_vn74_blendsst/hadgem3a_foam_ctl_vn74_blendsst.mar-may_mmeans.years1-35.soil_moisture.nc'
months=['march','april','may']
n_months=N_ELEMENTS(months)

; Box to plot
box=[-10,40,35,150]

foam_vn74_blendsst_nyears=[7,7,7,7,7]
foam_vn74_blendsst_nruns=N_ELEMENTS(foam_vn74_blendsst_nyears)
foam_vn74_blendsst_totalyears=TOTAL(foam_vn74_blendsst_nyears)
foam_vn74_blendsst_maxyears=MAX(foam_vn74_blendsst_nyears)

foam_vn74_blendsst_longitude=OPEN_AND_EXTRACT(jjas_precip_infile,'longitude')
foam_vn74_blendsst_latitude=OPEN_AND_EXTRACT(jjas_precip_infile,'latitude')
DEFINE_BOUNDARIES,box,foam_vn74_blendsst_latitude,foam_vn74_blendsst_longitude,box_tx,/LIMIT
foam_vn74_blendsst_nlon=N_ELEMENTS(foam_vn74_blendsst_longitude)
foam_vn74_blendsst_nlat=N_ELEMENTS(foam_vn74_blendsst_latitude)

trmm_longitude=OPEN_AND_EXTRACT(trmm_precip_infile,'longitude')
trmm_latitude=OPEN_AND_EXTRACT(trmm_precip_infile,'latitude')
DEFINE_BOUNDARIES,box,trmm_latitude,trmm_longitude,trmm_box_tx,/LIMIT
trmm_nlon=N_ELEMENTS(trmm_longitude)
trmm_nlat=N_ELEMENTS(trmm_latitude)

jjas_precip=fltarr(foam_vn74_blendsst_nruns,foam_vn74_blendsst_maxyears,foam_vn74_blendsst_nlon,foam_vn74_blendsst_nlat)
mam_soilmoisture=fltarr(foam_vn74_blendsst_nruns,foam_vn74_blendsst_maxyears,n_months,foam_vn74_blendsst_nlon,foam_vn74_blendsst_nlat)

offset_time=0
FOR i=0,foam_vn74_blendsst_nruns-1 DO BEGIN
   FOR j=0,foam_vn74_blendsst_nyears(i)-1 DO BEGIN
      thisyear_precip=REFORM(OPEN_AND_EXTRACT(jjas_precip_infile,'precip',$
                                              offset=[box_tx(1),box_tx(0),0,offset_time+j],$
                                              count=[foam_vn74_blendsst_nlon,foam_vn74_blendsst_nlat,1,1]))*86400.
      jjas_precip(i,j,*,*)=thisyear_precip
      FOR k=0,n_months-1 DO BEGIN
         thismonth_sm=REFORM(OPEN_AND_EXTRACT(mam_soilmoisture_infile,'sm',$
                                              offset=[box_tx(1),box_tx(0),0,k,offset_time+j],$
                                              count=[foam_vn74_blendsst_nlon,foam_vn74_blendsst_nlat,1,1,1]))
         mam_soilmoisture(i,j,k,*,*)=thismonth_sm
      ENDFOR
   ENDFOR
   IF j ne foam_vn74_blendsst_maxyears THEN BEGIN
      jjas_precip(i,j:foam_vn74_blendsst_maxyears-1,*,*)=!Values.F_NaN
      mam_soilmoisture(i,j:foam_vn74_blendsst_maxyears-1,*,*,*)=!Values.F_NaN
   ENDIF
   offset_time=offset_time+foam_vn74_blendsst_nyears(i)
ENDFOR

morph3_jjas_precip_clim=REFORM(OPEN_AND_EXTRACT(morph3_precip_infile,'precip',$
                                                offset=[box_tx(1),box_tx(0),0],$
                                                count=[foam_vn74_blendsst_nlon,foam_vn74_blendsst_nlat,1]))*86400.
trmm_jjas_precip_clim=REFORM(OPEN_AND_EXTRACT(trmm_precip_infile,'precip',$
                                              offset=[trmm_box_tx(1),trmm_box_tx(0)],$
                                              count=[trmm_nlon,trmm_nlat]))
trmm_jjas_precip_clim_revlat=fltarr(trmm_nlon,trmm_nlat)
FOR i=0,trmm_nlat-1 DO $
   trmm_jjas_precip_clim_revlat(*,i)=trmm_jjas_precip_clim(*,trmm_nlat-i-1)

mylevs=['1.0','2.0','3.0','4.0','5.0','7.0','9.0','11.0','14.0','17.0','20.0','25.0','30.0','40.0','50.0']
mylevs_sm=['-100','-50','0','50','100','150','200','250','300','350','400']
mylevs_diff=['-15','-13','-11','-9','-7','-5','-3','-1','1','3','5','7','9','11','13','15']
FOR i=0,foam_vn74_blendsst_maxyears-1 DO BEGIN
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/monsoon_iav/hadgem3kpp_monsoon_iav_jjas_precip_mam_soil.composite_jjas_precip_year'+STRTRIM(STRING(i+1),1)+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=200,XOFFSET=200,YOFFSET=1000,TFONT=2,TCHARSIZE=100,SPACE3=300
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[2]
   MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
   LEVS,MANUAL=mylevs
   precip_composite=fltarr(foam_vn74_blendsst_nlon,foam_vn74_blendsst_nlat)
   FOR j=0,foam_vn74_blendsst_nlon-1 DO $
      FOR k=0,foam_vn74_blendsst_nlat-1 DO $
         precip_composite(j,k)=MEAN(jjas_precip(*,i,j,k),/NaN)
   CON,FIELD=precip_composite,X=foam_vn74_blendsst_longitude,Y=foam_vn74_blendsst_latitude,/NOLINELABELS,CB_TITLE='Precipitation (mm day!U-1!N)',$
       TITLE='JJAS-mean precip for year '+STRTRIM(STRING(i+1),1)+' after applying AMIP2 SSTs remotely - '+$
       STRTRIM(STRING(N_ELEMENTS(where(foam_vn74_blendsst_nyears ge i+1))),1)+' members'
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/monsoon_iav/hadgem3kpp_monsoon_iav_jjas_precip_mam_soil.composite_jjas_precip_year'+STRTRIM(STRING(i+1),1)+'-minus-trmm.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=200,XOFFSET=200,YOFFSET=1000,TFONT=2,TCHARSIZE=100,SPACE3=300
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV,white=[10]
   MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
   LEVS,MANUAL=mylevs_diff
   CON,FIELD=precip_composite-trmm_jjas_precip_clim_revlat,X=foam_vn74_blendsst_longitude,Y=foam_vn74_blendsst_latitude,$
       /NOLINELABELS,CB_TITLE='Precipitation (mm day!U-1!N)',$
       TITLE='Diff in JJAS-mean precip for year '+STRTRIM(STRING(i+1),1)+' after applying AMIP2 SSTs remotely minus TRMM (1999-2008) - '+$
       STRTRIM(STRING(N_ELEMENTS(where(foam_vn74_blendsst_nyears ge i+1))),1)+' members in year '+STRTRIM(STRING(i+1),1)
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/monsoon_iav/hadgem3kpp_monsoon_iav_jjas_precip_mam_soil.composite_jjas_precip_year'+STRTRIM(STRING(i+1),1)+'-minus-airxv.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=200,XOFFSET=200,YOFFSET=1000,TFONT=2,TCHARSIZE=100,SPACE3=300
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV,white=[10]
   MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
   LEVS,MANUAL=mylevs_diff
   CON,FIELD=precip_composite-morph3_jjas_precip_clim,X=foam_vn74_blendsst_longitude,Y=foam_vn74_blendsst_latitude,$
       /NOLINELABELS,CB_TITLE='Precipitation (mm day!U-1!N)',$
       TITLE='Diff in JJAS-mean precip for year '+STRTRIM(STRING(i+1),1)+' after applying AMIP2 SSTs remotely minus MORPH3 airxv (1982-2008) - '+$
       STRTRIM(STRING(N_ELEMENTS(where(foam_vn74_blendsst_nyears ge i+1))),1)+' members in year '+STRTRIM(STRING(i+1),1)
   PSCLOSE,/NOVIEW

   IF i ge 1 THEN BEGIN
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/monsoon_iav/hadgem3kpp_monsoon_iav_jjas_precip_mam_soil.composite_jjas_precip_year'+STRTRIM(STRING(i+1),1)+'-minus-year1.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=200,XOFFSET=200,YOFFSET=1000,TFONT=2,TCHARSIZE=100,SPACE3=300
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV,white=[10]
      MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
      LEVS,MANUAL=mylevs_diff
      precip_composite=fltarr(foam_vn74_blendsst_nlon,foam_vn74_blendsst_nlat)
      FOR j=0,foam_vn74_blendsst_nlon-1 DO $
         FOR k=0,foam_vn74_blendsst_nlat-1 DO $
            precip_composite(j,k)=MEAN(jjas_precip(*,i,j,k),/NaN)
      CON,FIELD=precip_composite-precip_composite_year1,X=foam_vn74_blendsst_longitude,Y=foam_vn74_blendsst_latitude,/NOLINELABELS,$
          CB_TITLE='Precipitation (mm day!U-1!N)',$
          TITLE='Diff in JJAS-mean precipitation for year '+STRTRIM(STRING(i+1),1)+' minus year 1 - '+$
          STRTRIM(STRING(N_ELEMENTS(where(foam_vn74_blendsst_nyears ge i+1))),1)+' ensemble members in year '+STRTRIM(STRING(i+1),1)
      PSCLOSE,/NOVIEW
   ENDIF ELSE $
      precip_composite_year1=precip_composite

   FOR j=0,n_months-1 DO BEGIN 
      psfile='/home/ss901165/idl//hadgem3-kpp_runs/monsoon_iav/hadgem3kpp_monsoon_iav_jjas_precip_mam_soil.composite_'+months(j)+'_soilmoisture_year'+STRTRIM(STRING(i+1),1)+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=200,XOFFSET=200,YOFFSET=1000,TFONT=2,TCHARSIZE=100,SPACE3=300
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_sm)+1,/REV,white=[2]
      MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
      LEVS,MANUAL=mylevs_sm
      sm_composite=fltarr(foam_vn74_blendsst_nlon,foam_vn74_blendsst_nlat)
      FOR k=0,foam_vn74_blendsst_nlon-1 DO $
         FOR m=0,foam_vn74_blendsst_nlat-1 DO $
            sm_composite(k,m)=MEAN(mam_soilmoisture(*,i,j,k,m),/NaN)
      CON,FIELD=sm_composite,X=foam_vn74_blendsst_longitude,Y=foam_vn74_blendsst_latitude,/NOLINES,CB_TITLE='Soil moisture (kg m!U-2!N)',$
          TITLE=months(j)+'-mean soil moisture for year '+STRTRIM(STRING(i+1),1)+' after applying AMIP2 SSTs remotely - '+$
          STRTRIM(STRING(N_ELEMENTS(where(foam_vn74_blendsst_nyears ge i+1))),1)+' members',/BLOCK
      PSCLOSE,/NOVIEW    
   ENDFOR
ENDFOR

psfile='/home/ss901165/idl/hadgem3-kpp_runs/monsoon_iav/hadgem3kpp_monsoon_iav_jjas_precip_mam_soil.composite_jjas_precip_morph3_amip2_airxv.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=200,XOFFSET=200,YOFFSET=1000,TFONT=2,TCHARSIZE=100,SPACE3=300
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[2]
MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
LEVS,MANUAL=mylevs
CON,FIELD=morph3_jjas_precip_clim,X=foam_vn74_blendsst_longitude,Y=foam_vn74_blendsst_latitude,/NOLINELABELS,CB_TITLE='Precipitation (mm day!U-1!N)',$
    TITLE='JJAS-mean precipitation for MORPH3 AMIP2 integration (airxv, 1982-2008)'
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3-kpp_runs/monsoon_iav/hadgem3kpp_monsoon_iav_jjas_precip_mam_soil.composite_jjas_precip_trmm_n96.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=200,XOFFSET=200,YOFFSET=1000,TFONT=2,TCHARSIZE=100,SPACE3=300
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[2]
MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
LEVS,MANUAL=mylevs
CON,FIELD=trmm_jjas_precip_clim_revlat,X=foam_vn74_blendsst_longitude,Y=foam_vn74_blendsst_latitude,/NOLINELABELS,CB_TITLE='Precipitation (mm day!U-1!N)',$
    TITLE='JJAS-mean precipitation for TRMM at N96 resolution (1999-2008)'
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3-kpp_runs/monsoon_iav/hadgem3kpp_monsoon_iav_jjas_precip_mam_soil.composite_jjas_precip_morph3_amip2_airxv-minus-trmm_n96.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=200,XOFFSET=200,YOFFSET=1000,TFONT=2,TCHARSIZE=100,SPACE3=300
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV,white=[10]
MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
LEVS,MANUAL=mylevs_diff
CON,FIELD=morph3_jjas_precip_clim-trmm_jjas_precip_clim_revlat,X=foam_vn74_blendsst_longitude,Y=foam_vn74_blendsst_latitude,/NOLINELABELS,CB_TITLE='Precipitation (mm day!U-1!N)',$
    TITLE='JJAS-mean precipitation for MORPH3 AMIP2 integration (airxv, 1982-2008) minus TRMM (1999-2008)'
PSCLOSE,/NOVIEW

FOR j=0,foam_vn74_blendsst_nlon-1 DO $
      FOR k=0,foam_vn74_blendsst_nlat-1 DO $
         precip_composite(j,k)=MEAN(jjas_precip(*,*,j,k),/NaN)

psfile='/home/ss901165/idl/hadgem3-kpp_runs/monsoon_iav/hadgem3kpp_monsoon_iav_jjas_precip_mam_soil.composite_jjas_precip_allyears-minus-trmm_n96.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=200,XOFFSET=200,YOFFSET=1000,TFONT=2,TCHARSIZE=100,SPACE3=300
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV,white=[10]
MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
LEVS,MANUAL=mylevs_diff
CON,FIELD=precip_composite-trmm_jjas_precip_clim_revlat,X=foam_vn74_blendsst_longitude,Y=foam_vn74_blendsst_latitude,/NOLINELABELS,CB_TITLE='Precipitation (mm day!U-1!N)',$
    TITLE='JJAS-mean precipitation for FOAM/AMIP2 blended SST (35 years) minus TRMM (1999-2008)'
PSCLOSE,/NOVIEW



STOP

END

