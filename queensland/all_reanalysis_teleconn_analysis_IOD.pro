PRO all_reanalysis_teleconn_analysis_IOD

ncep_reanalysis_indir='/home/ss901165/datasets_mango/NCEP_REANALYSIS/precipitation'
era40_reanalysis_indir='/home/ss901165/datasets/ERA40/PRECIP'
thcent_reanalysis_indir='/home/ss901165/datasets_mango/20THC_REANALYSIS/precip'
silo_t62_indir='/home/ss901165/datasets_mango/SILO/t62'
silo_era40_indir='/home/ss901165/datasets_mango/SILO/era40_resolution'

; reading in the precip for the whole of australia
box_plot=[-44,110,-10,160]
box=[-45,110,-10,160]

ncep_reanalysis_nyears=49
era40_reanalysis_nyears=44
thcent_reanalysis_nyears=49

    mask_t62_infile='/home/ss901165/datasets_mango/NCEP_REANALYSIS/lsmask.t62_gauss.nc'
    mask_longitude=OPEN_AND_EXTRACT(mask_t62_infile,'lon')
    mask_latitude=OPEN_AND_EXTRACT(mask_t62_infile,'lat')
    DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
    mask_nlon=N_ELEMENTS(mask_longitude)
    mask_nlat=N_ELEMENTS(mask_latitude)
    mask=REFORM(OPEN_AND_EXTRACT(mask_t62_infile,'lsmask',$
                             offset=[mask_box_tx(1),mask_box_tx(0)],$
                             count=[mask_nlon,mask_nlat]))

    mask_era40_infile='/home/ss901165/datasets/ERA40/era40_lsm.nc'
    mask_era40_longitude=OPEN_AND_EXTRACT(mask_era40_infile,'longitude')
    mask_era40_latitude=OPEN_AND_EXTRACT(mask_era40_infile,'latitude')
    DEFINE_BOUNDARIES,box,mask_era40_latitude,mask_era40_longitude,mask_era40_box_tx,/LIMIT
    mask_era40_nlon=N_ELEMENTS(mask_era40_longitude)
    mask_era40_nlat=N_ELEMENTS(mask_era40_latitude)
    mask_era40=REFORM(OPEN_AND_EXTRACT(mask_era40_infile,'LSM',$
                             offset=[mask_era40_box_tx(1),mask_era40_box_tx(0)],$
                             count=[mask_era40_nlon,mask_era40_nlat]))

    mask_twentyc_infile='/home/ss901165/datasets_mango/20THC_REANALYSIS/mask_t62.nc'
    mask_twentyc_longitude=OPEN_AND_EXTRACT(mask_twentyc_infile,'longitude')
    mask_twentyc_latitude=OPEN_AND_EXTRACT(mask_twentyc_infile,'latitude')
    DEFINE_BOUNDARIES,box,mask_twentyc_latitude,mask_twentyc_longitude,mask_twentyc_box_tx,/LIMIT
    mask_twentyc_nlon=N_ELEMENTS(mask_twentyc_longitude)
    mask_twentyc_nlat=N_ELEMENTS(mask_twentyc_latitude)
    mask_twentyc=REFORM(OPEN_AND_EXTRACT(mask_twentyc_infile,'lsm',$
                                     offset=[mask_twentyc_box_tx(1),mask_twentyc_box_tx(0)],$
                                     count=[mask_twentyc_nlon,mask_twentyc_nlat]))

    seasons=['dec-feb','mar-may','jun-aug','sep-nov']
    seasons_long=['December-February','March-May','June-August','September-November']
    n_seasons=N_ELEMENTS(seasons)
    mylevs_corr=['-0.85','-0.75','-0.65','-0.55','-0.45','-0.35','-0.25','-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55','0.65']


    ncep_reanalysis_infile_amean=ncep_reanalysis_indir+'/ncep-ncar_reanalysis.may-apr_ameans.1948-2007.precip.t62_gauss.nc'
    era40_reanalysis_infile_amean=era40_reanalysis_indir+'/era40.may-apr_ameans.1958-2000.precip.aus_domain.nc'
    thcent_reanalysis_infile_amean=thcent_reanalysis_indir+'/20thc_reanalysis.may-apr_ameans.1891-2007.precip.nc'


   ; silo_t62_infile_amean=silo_t62_indir+'/SILO.jan-dec_amean_clim.1948-2007.precip.t62.nc'
   ;   silo_t62_longitude=OPEN_AND_EXTRACT(silo_t62_infile_amean,'longitude')
   ;   silo_t62_latitude=OPEN_AND_EXTRACT(silo_t62_infile_amean,'latitude')
   ;   DEFINE_BOUNDARIES,box,silo_t62_latitude,silo_t62_longitude,silo_t62_box_tx,/LIMIT
   ;   silo_t62_nlon=N_ELEMENTS(silo_t62_longitude)
   ;   silo_t62_nlat=N_ELEMENTS(silo_t62_latitude)
   ; silo_era40_infile_amean=silo_era40_indir+'/SILO.jan-dec_amean_clim.1958-2001.precip.era40.nc'
   ;   silo_era40_longitude=OPEN_AND_EXTRACT(silo_era40_infile_amean,'longitude')
    ;  silo_era40_latitude=OPEN_AND_EXTRACT(silo_era40_infile_amean,'latitude')
   ;   DEFINE_BOUNDARIES,box,silo_era40_latitude,silo_era40_longitude,silo_era40_box_tx,/LIMIT
   ;   silo_era40_nlon=N_ELEMENTS(silo_era40_longitude)
   ;   silo_era40_nlat=N_ELEMENTS(silo_era40_latitude)

    ncep_reanalysis_longitude=OPEN_AND_EXTRACT(ncep_reanalysis_infile_amean,'lon')
    ncep_reanalysis_latitude=OPEN_AND_EXTRACT(ncep_reanalysis_infile_amean,'lat')
    DEFINE_BOUNDARIES,box,ncep_reanalysis_latitude,ncep_reanalysis_longitude,ncep_reanalysis_box_tx,/LIMIT 
    ncep_reanalysis_nlon=N_ELEMENTS(ncep_reanalysis_longitude)
    ncep_reanalysis_nlat=N_ELEMENTS(ncep_reanalysis_latitude)  
 
    era40_reanalysis_longitude=OPEN_AND_EXTRACT(era40_reanalysis_infile_amean,'longitude')
    era40_reanalysis_latitude=OPEN_AND_EXTRACT(era40_reanalysis_infile_amean,'latitude')
    DEFINE_BOUNDARIES,box,era40_reanalysis_latitude,era40_reanalysis_longitude,era40_reanalysis_box_tx,/LIMIT 
    era40_reanalysis_nlon=N_ELEMENTS(era40_reanalysis_longitude)
    era40_reanalysis_nlat=N_ELEMENTS(era40_reanalysis_latitude)  

   
    thcent_reanalysis_longitude=OPEN_AND_EXTRACT(thcent_reanalysis_infile_amean,'longitude')
    thcent_reanalysis_latitude=OPEN_AND_EXTRACT(thcent_reanalysis_infile_amean,'latitude')
    DEFINE_BOUNDARIES,box,thcent_reanalysis_latitude,thcent_reanalysis_longitude,thcent_reanalysis_box_tx,/LIMIT 
    thcent_reanalysis_nlon=N_ELEMENTS(thcent_reanalysis_longitude)
    thcent_reanalysis_nlat=N_ELEMENTS(thcent_reanalysis_latitude)
  

   ncep_reanalysis_precip_amean=REFORM(OPEN_AND_EXTRACT(ncep_reanalysis_infile_amean,'prate',$
                                                  offset=[ncep_reanalysis_box_tx(1),ncep_reanalysis_box_tx(0),10],$
                                                  count=[ncep_reanalysis_nlon,ncep_reanalysis_nlat,ncep_reanalysis_nyears]))
   ncep_reanalysis_precip_amean=ncep_reanalysis_precip_amean*86400.
   ncep_reanalysis_precip_amean[where(mask eq 0)]=!Values.F_NaN
 
  
   era40_reanalysis_precip_amean=REFORM(OPEN_AND_EXTRACT(era40_reanalysis_infile_amean,'precip',$
                                                  offset=[era40_reanalysis_box_tx(1),era40_reanalysis_box_tx(0),0],$
                                                  count=[era40_reanalysis_nlon,era40_reanalysis_nlat,era40_reanalysis_nyears]))
   era40_reanalysis_precip_amean=era40_reanalysis_precip_amean*1000.
   era40_reanalysis_precip_amean[where(mask_era40 eq 0)]=!Values.F_NaN
   
   
   thcent_reanalysis_precip_amean=REFORM(OPEN_AND_EXTRACT(thcent_reanalysis_infile_amean,'PRATE',$
                                                          offset=[thcent_reanalysis_box_tx(1),thcent_reanalysis_box_tx(0),67],$
                                                          count=[thcent_reanalysis_nlon,thcent_reanalysis_nlat,thcent_reanalysis_nyears]))
  
   thcent_reanalysis_precip_amean=thcent_reanalysis_precip_amean*86400.
   thcent_reanalysis_precip_amean[where(mask_twentyc eq 0)]=!Values.F_NaN
 
   IOD_infile_ncep_amean='/home/ss901165/datasets/IOD/iod_index.may-apr_ameans.1958-2008.hadisst.nc'
   IOD_index_ncep_amean=REFORM(OPEN_AND_EXTRACT(IOD_infile_ncep_amean,'IOD',$
                                             offset=[0],$
                                             count=[49]))
   IOD_infile_era40_amean='/home/ss901165/datasets/IOD/iod_index.may-apr_ameans.1958-2008.hadisst.nc'
   IOD_index_era40_amean=REFORM(OPEN_AND_EXTRACT(IOD_infile_era40_amean,'IOD',$
                                             offset=[0],$
                                             count=[44]))
   IOD_infile_thcent_amean='/home/ss901165/datasets/IOD/iod_index.may-apr_ameans.1958-2008.hadisst.nc'
   IOD_index_thcent_amean=REFORM(OPEN_AND_EXTRACT(IOD_infile_thcent_amean,'IOD',$
                                             offset=[0],$
                                             count=[49]))

   ncep_regression_coeff=fltarr(ncep_reanalysis_nlon,ncep_reanalysis_nlat)
   era40_regression_coeff=fltarr(era40_reanalysis_nlon,era40_reanalysis_nlat)
   thcent_regression_coeff=fltarr(thcent_reanalysis_nlon,thcent_reanalysis_nlat)

   ncep_correlation_coeff=fltarr(ncep_reanalysis_nlon,ncep_reanalysis_nlat)
   era40_correlation_coeff=fltarr(era40_reanalysis_nlon,era40_reanalysis_nlat)
   thcent_correlation_coeff=fltarr(thcent_reanalysis_nlon,thcent_reanalysis_nlat)

  FOR i=0,ncep_reanalysis_nlon-1 DO BEGIN
      FOR j=0,ncep_reanalysis_nlat-1 DO BEGIN
            ncep_temp_rainfall=REFORM(ncep_reanalysis_precip_amean(i,j,*))
            ncep_regression_coeff(i,j)=REGRESS(IOD_index_ncep_amean,ncep_temp_rainfall,CORRELATION=ncep_temp)*STDDEV(IOD_index_ncep_amean)
            ncep_correlation_coeff(i,j)=ncep_temp
      ENDFOR
   ENDFOR

 
   psfile='/home/ss901165/idl/queensland/IOD_reanalysis_corr_precip_amean_ncep.1958-2007.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1000,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=100,XSIZE=18000
   CS,SCALE=21,NCOLS=N_ELEMENTS(mylevs_corr)+1,WHITE=[11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_corr,NDECS=2
   CON,FIELD=ncep_correlation_coeff,X=ncep_reanalysis_longitude,Y=ncep_reanalysis_latitude,$ 
       TITLE='- Correlation of annual-mean daily rainfall rate between NCEP/NCAR reanalysis and IOD Index (1958-2007)',/NOLINES,$
       CB_TITLE='Correlation Coefficient (unitless)',/BLOCK
   AXES,XSTEP=10,YSTEP=5
  PSCLOSE

  psfile='/home/ss901165/idl/queensland/IOD_reanalysis_regress_precip_amean_ncep.1958-2007.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1000,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=100,XSIZE=18000
   CS,SCALE=21,NCOLS=N_ELEMENTS(mylevs_corr)+1,WHITE=[11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_corr,NDECS=2
   CON,FIELD=ncep_regression_coeff,X=ncep_reanalysis_longitude,Y=ncep_reanalysis_latitude,$ 
       TITLE='- Linear Regression of annual-mean daily rainfall rate between NCEP/NCAR reanalysis and IOD Index (1958-2007)',/NOLINES,$
       CB_TITLE='Regression Coefficient (mm/day/!Eo!NC)',/BLOCK
   AXES,XSTEP=10,YSTEP=5
   PFILL,FIELD=ncep_correlation_coeff,X=ncep_reanalysis_longitude,Y=ncep_reanalysis_latitude,MIN=0.275,MAX=1.5
   PFILL,FIELD=ncep_correlation_coeff,X=ncep_reanalysis_longitude,Y=ncep_reanalysis_latitude,MIN=-1.5,MAX=-0.275
  PSCLOSE


  FOR i=0,era40_reanalysis_nlon-1 DO BEGIN
      FOR j=0,era40_reanalysis_nlat-1 DO BEGIN
            era40_temp_rainfall=REFORM(era40_reanalysis_precip_amean(i,j,*))
            era40_regression_coeff(i,j)=REGRESS(IOD_index_era40_amean,era40_temp_rainfall,CORRELATION=era40_temp)
            era40_correlation_coeff(i,j)=era40_temp
      ENDFOR
   ENDFOR

 
   psfile='/home/ss901165/idl/queensland/IOD_reanalysis_corr_precip_amean_era40.1958-2001.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1000,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=100,XSIZE=18000
   CS,SCALE=21,NCOLS=N_ELEMENTS(mylevs_corr)+1,WHITE=[11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_corr,NDECS=2
   CON,FIELD=era40_correlation_coeff,X=era40_reanalysis_longitude,Y=era40_reanalysis_latitude,$ 
       TITLE='- Correlation of annual-mean daily rainfall rate between ERA-40 reanalysis and IOD Index (1958-2001)',/NOLINES,$
       CB_TITLE='Correlation Coefficient (unitless)',/BLOCK
   AXES,XSTEP=10,YSTEP=5
  PSCLOSE

psfile='/home/ss901165/idl/queensland/IOD_reanalysis_regress_precip_amean_era40.1958-2001.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1000,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=100,XSIZE=18000
   CS,SCALE=21,NCOLS=N_ELEMENTS(mylevs_corr)+1,WHITE=[11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_corr,NDECS=2
   CON,FIELD=era40_regression_coeff,X=era40_reanalysis_longitude,Y=era40_reanalysis_latitude,$ 
       TITLE='- Linear Regression of annual-mean daily rainfall rate between ERA-40 reanalysis and IOD Index (1958-2001)',/NOLINES,$
       CB_TITLE='Regression Coefficient (mm/day/!Eo!NC)',/BLOCK
   AXES,XSTEP=10,YSTEP=5
   PFILL,FIELD=era40_regression_coeff,X=era40_reanalysis_longitude,Y=era40_reanalysis_latitude,MIN=0.29,MAX=1.5
   PFILL,FIELD=era40_regression_coeff,X=era40_reanalysis_longitude,Y=era40_reanalysis_latitude,MIN=-1.5,MAX=-0.29
  PSCLOSE

   FOR i=0,thcent_reanalysis_nlon-1 DO BEGIN
    FOR j=0,thcent_reanalysis_nlat-1 DO BEGIN
            thcent_temp_rainfall=REFORM(thcent_reanalysis_precip_amean(i,j,*))
            thcent_regression_coeff(i,j)=REGRESS(IOD_index_thcent_amean,thcent_temp_rainfall,CORRELATION=thcent_temp)
            thcent_correlation_coeff(i,j)=thcent_temp
      ENDFOR
   ENDFOR

  psfile='/home/ss901165/idl/queensland/IOD_reanalysis_corr_precip_amean_thcent.1900-2007.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1000,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=100,XSIZE=18000
   CS,SCALE=21,NCOLS=N_ELEMENTS(mylevs_corr)+1,WHITE=[11]
  MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
  LEVS,MANUAL=mylevs_corr,NDECS=2
  CON,FIELD=thcent_correlation_coeff,X=thcent_reanalysis_longitude,Y=thcent_reanalysis_latitude,$ 
       TITLE='- Correlation of annual-mean daily rainfall rate between 20th Century reanalysis and IOD Index (1958-2007)',/NOLINES,$
       CB_TITLE='Correlation Coefficient (unitless)',/BLOCK
   AXES,XSTEP=10,YSTEP=5
  PSCLOSE   

  psfile='/home/ss901165/idl/queensland/IOD_reanalysis_regress_precip_amean_thcent.1900-2007.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1000,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=100,XSIZE=18000
   CS,SCALE=21,NCOLS=N_ELEMENTS(mylevs_corr)+1,WHITE=[11]
  MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
  LEVS,MANUAL=mylevs_corr,NDECS=2
  CON,FIELD=thcent_regression_coeff,X=thcent_reanalysis_longitude,Y=thcent_reanalysis_latitude,$ 
       TITLE='- Linear Regression of annual-mean daily rainfall rate between 20th Century reanalysis and IOD Index (1958-2007)',/NOLINES,$
       CB_TITLE='Regression Coefficient (unitless)',/BLOCK
   AXES,XSTEP=10,YSTEP=5
   PFILL,FIELD=thcent_regression_coeff,X=thcent_reanalysis_longitude,Y=thcent_reanalysis_latitude,MIN=0.195,MAX=1.0
   PFILL,FIELD=thcent_regression_coeff,X=thcent_reanalysis_longitude,Y=thcent_reanalysis_latitude,MIN=-1.0,MAX=-0.195
  PSCLOSE

  
FOR k=0,n_seasons-1 DO BEGIN
   
    ncep_reanalysis_infile=ncep_reanalysis_indir+'/ncep-ncar_reanalysis.'+seasons(k)+'_smeans.1948-2007.precip.t62_gauss.nc'
    era40_reanalysis_infile=era40_reanalysis_indir+'/era40.'+seasons(k)+'_smeans.1958-2001.precip.aus_domain.nc'
    thcent_reanalysis_infile=thcent_reanalysis_indir+'/20thc_reanalysis.'+seasons(k)+'_smeans.1900-2007.precip.nc'


   ncep_reanalysis_precip=REFORM(OPEN_AND_EXTRACT(ncep_reanalysis_infile,'prate',$
                                                  offset=[ncep_reanalysis_box_tx(1),ncep_reanalysis_box_tx(0),10],$
                                                  count=[ncep_reanalysis_nlon,ncep_reanalysis_nlat,ncep_reanalysis_nyears]))
   ncep_reanalysis_precip=ncep_reanalysis_precip*86400.
   ncep_reanalysis_precip[where(mask eq 0)]=!Values.F_NaN

   era40_reanalysis_precip=REFORM(OPEN_AND_EXTRACT(era40_reanalysis_infile,'precip',$
                                                  offset=[era40_reanalysis_box_tx(1),era40_reanalysis_box_tx(0),0],$
                                                  count=[era40_reanalysis_nlon,era40_reanalysis_nlat,era40_reanalysis_nyears]))
   era40_reanalysis_precip=era40_reanalysis_precip*1000.
   era40_reanalysis_precip[where(mask_era40 eq 0)]=!Values.F_NaN

   thcent_reanalysis_precip=REFORM(OPEN_AND_EXTRACT(thcent_reanalysis_infile,'PRATE',$
                                                  offset=[thcent_reanalysis_box_tx(1),thcent_reanalysis_box_tx(0),58],$
                                                  count=[thcent_reanalysis_nlon,thcent_reanalysis_nlat,49]))
    thcent_reanalysis_precip=thcent_reanalysis_precip*86400.
    thcent_reanalysis_precip[where(mask_twentyc eq 0)]=!Values.F_NaN

   IOD_infile_ncep='/home/ss901165/datasets/IOD/iod_index.'+seasons(k)+'_smeans.1958-2009.hadisst.nc'
   IOD_index_ncep=REFORM(OPEN_AND_EXTRACT(IOD_infile_ncep,'IOD',$
                                             offset=[0],$
                                             count=[49]))
   IOD_infile_era40='/home/ss901165/datasets/IOD/iod_index.'+seasons(k)+'_smeans.1958-2009.hadisst.nc'
   IOD_index_era40=REFORM(OPEN_AND_EXTRACT(IOD_infile_ncep,'IOD',$
                                             offset=[0],$
                                             count=[44]))
   IOD_infile_thcent='/home/ss901165/datasets/IOD/iod_index.'+seasons(k)+'_smeans.1958-2009.hadisst.nc'
   IOD_index_thcent=REFORM(OPEN_AND_EXTRACT(IOD_infile_ncep,'IOD',$
                                             offset=[0],$
                                             count=[49]))

   ncep_regression_coeff_smeans=fltarr(ncep_reanalysis_nlon,ncep_reanalysis_nlat)
   era40_regression_coeff_smeans=fltarr(era40_reanalysis_nlon,era40_reanalysis_nlat)
   thcent_regression_coeff_smeans=fltarr(thcent_reanalysis_nlon,thcent_reanalysis_nlat)

   ncep_correlation_coeff_smeans=fltarr(ncep_reanalysis_nlon,ncep_reanalysis_nlat)
   era40_correlation_coeff_smeans=fltarr(era40_reanalysis_nlon,era40_reanalysis_nlat)
   thcent_correlation_coeff_smeans=fltarr(thcent_reanalysis_nlon,thcent_reanalysis_nlat)

  FOR i=0,ncep_reanalysis_nlon-1 DO BEGIN
      FOR j=0,ncep_reanalysis_nlat-1 DO BEGIN
            ncep_temp_rainfall_smeans=REFORM(ncep_reanalysis_precip(i,j,*))
            ncep_regression_coeff_smeans(i,j)=REGRESS(IOD_index_ncep,ncep_temp_rainfall_smeans,CORRELATION=ncep_temp_smeans)*STDDEV(IOD_index_ncep)
            ncep_correlation_coeff_smeans(i,j)=ncep_temp_smeans
      ENDFOR
   ENDFOR

 
   psfile='/home/ss901165/idl/queensland/IOD_reanalysis_corr_precip_'+seasons(k)+'_smean_ncep.1958-2007.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1000,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=100,XSIZE=18000
   CS,SCALE=21,NCOLS=N_ELEMENTS(mylevs_corr)+1,WHITE=[11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_corr,NDECS=2
   CON,FIELD=ncep_correlation_coeff_smeans,X=ncep_reanalysis_longitude,Y=ncep_reanalysis_latitude,$ 
       TITLE='- Correlation of '+seasons_long(k)+'-mean precipitation between NCEP/NCAR reanalysis with IOD Index (1958-2007)',/NOLINES,$
       CB_TITLE='Correlation Coefficient (unitless)',/BLOCK
   AXES,XSTEP=10,YSTEP=5
  PSCLOSE

  psfile='/home/ss901165/idl/queensland/IOD_reanalysis_regress_precip_'+seasons(k)+'_smean_ncep.1958-2007.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1000,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=100,XSIZE=18000
   CS,SCALE=21,NCOLS=N_ELEMENTS(mylevs_corr)+1,WHITE=[11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_corr,NDECS=2
   CON,FIELD=ncep_regression_coeff_smeans,X=ncep_reanalysis_longitude,Y=ncep_reanalysis_latitude,$ 
       TITLE='- Linear Regression of '+seasons_long(k)+'-mean precipitation between NCEP/NCAR reanalysis with IOD Index (1958-2007)',/NOLINES,$
       CB_TITLE='Regression Coefficient (mm/day/!Eo!NC)',/BLOCK
   AXES,XSTEP=10,YSTEP=5
   PFILL,FIELD=ncep_correlation_coeff_smeans,X=ncep_reanalysis_longitude,Y=ncep_reanalysis_latitude,MIN=0.273,MAX=1.0
   PFILL,FIELD=ncep_correlation_coeff_smeans,X=ncep_reanalysis_longitude,Y=ncep_reanalysis_latitude,MIN=-1.0,MAX=-0.273
  PSCLOSE


  FOR i=0,era40_reanalysis_nlon-1 DO BEGIN
      FOR j=0,era40_reanalysis_nlat-1 DO BEGIN
            era40_temp_rainfall_smeans=REFORM(era40_reanalysis_precip(i,j,*))
            era40_regression_coeff_smeans(i,j)=REGRESS(IOD_index_era40,era40_temp_rainfall_smeans,CORRELATION=era40_temp_smeans)
            era40_correlation_coeff_smeans(i,j)=era40_temp_smeans
      ENDFOR
   ENDFOR

 
   psfile='/home/ss901165/idl/queensland/IOD_reanalysis_corr_precip_'+seasons(k)+'_smean_era40.1958-2001.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1000,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=100,XSIZE=18000
   CS,SCALE=21,NCOLS=N_ELEMENTS(mylevs_corr)+1,WHITE=[11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_corr,NDECS=2
   CON,FIELD=era40_correlation_coeff_smeans,X=era40_reanalysis_longitude,Y=era40_reanalysis_latitude,$ 
       TITLE='- Correlation of '+seasons_long(k)+'-mean precipitation between ERA-40 reanalysis with IOD Index (1958-2001)',/NOLINES,$
       CB_TITLE='Correlation Coefficient (unitless)',/BLOCK
   AXES,XSTEP=10,YSTEP=5
  PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/IOD_reanalysis_regress_precip_'+seasons(k)+'_smean_era40.1958-2001.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1000,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=100,XSIZE=18000
   CS,SCALE=21,NCOLS=N_ELEMENTS(mylevs_corr)+1,WHITE=[11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_corr,NDECS=2
   CON,FIELD=era40_regression_coeff_smeans,X=era40_reanalysis_longitude,Y=era40_reanalysis_latitude,$ 
       TITLE='- Linear Regression of '+seasons_long(k)+'-mean precipitation between ERA-40 reanalysis with IOD Index (1958-2001)',/NOLINES,$
       CB_TITLE='Regression Coefficient (mm/day/!Eo!NC)',/BLOCK
   AXES,XSTEP=10,YSTEP=5
   PFILL,FIELD=era40_regression_coeff_smeans,X=era40_reanalysis_longitude,Y=era40_reanalysis_latitude,MIN=0.29,MAX=1.0
   PFILL,FIELD=era40_regression_coeff_smeans,X=era40_reanalysis_longitude,Y=era40_reanalysis_latitude,MIN=-1.0,MAX=-0.29
  PSCLOSE,/NOVIEW

  FOR i=0,thcent_reanalysis_nlon-1 DO BEGIN
      FOR j=0,thcent_reanalysis_nlat-1 DO BEGIN
            thcent_temp_rainfall_smeans=REFORM(thcent_reanalysis_precip(i,j,*))
            thcent_regression_coeff_smeans(i,j)=REGRESS(IOD_index_thcent,thcent_temp_rainfall_smeans,CORRELATION=thcent_temp_smeans)
            thcent_correlation_coeff_smeans(i,j)=thcent_temp_smeans
      ENDFOR
  ENDFOR

  psfile='/home/ss901165/idl/queensland/IOD_reanalysis_corr_precip_'+seasons(k)+'_smean_thcent.1958-2007.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1000,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=100,XSIZE=18000
   CS,SCALE=21,NCOLS=N_ELEMENTS(mylevs_corr)+1,WHITE=[11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_corr,NDECS=2
   CON,FIELD=thcent_correlation_coeff_smeans,X=thcent_reanalysis_longitude,Y=thcent_reanalysis_latitude,$ 
       TITLE='- Correlation of '+seasons_long(k)+'-mean precipitation between 20th Century reanalysis with IOD Index (1958-2007)',/NOLINES,$
       CB_TITLE='Correlation Coefficient (unitless)',/BLOCK
   AXES,XSTEP=10,YSTEP=5
  PSCLOSE,/NOVIEW

  psfile='/home/ss901165/idl/queensland/IOD_reanalysis_regress_precip_'+seasons(k)+'_smean_thcent.1958-2007.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1000,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=100,XSIZE=18000
   CS,SCALE=21,NCOLS=N_ELEMENTS(mylevs_corr)+1,WHITE=[11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_corr,NDECS=2
   CON,FIELD=thcent_regression_coeff_smeans,X=thcent_reanalysis_longitude,Y=thcent_reanalysis_latitude,$ 
       TITLE='- Linear Regression of '+seasons_long(k)+'-mean precipitation between 20th Century reanalysis with IOD Index (1958-2007)',/NOLINES,$
       CB_TITLE='Regression Coefficient (mm/day/!Eo!NC)',/BLOCK
   AXES,XSTEP=10,YSTEP=5
   PFILL,FIELD=thcent_regression_coeff_smeans,X=thcent_reanalysis_longitude,Y=thcent_reanalysis_latitude,MIN=0.273,MAX=1.0
   PFILL,FIELD=thcent_regression_coeff_smeans,X=thcent_reanalysis_longitude,Y=thcent_reanalysis_latitude,MIN=-1.0,MAX=-0.273
  PSCLOSE,/NOVIEW

ENDFOR



STOP
END
