PRO qld_20thc_reanalysis_nino_correlations_compare_silo

; Timeseries of observed NINO values (monthly)
obs_nino_infile='/home/ss901165/datasets/NINO/nino_hadisst.jan-dec_mmeans.1871-2008.nc'
obs_nino_start_year=1871
obs_nino_end_year=2008
obs_nino_nyears=(obs_nino_end_year-obs_nino_start_year)+1
obs_nino_nmonths=obs_nino_nyears*12

; Timeseries of Australian rainfall from SILO (monthly)
silo_t62_indir='/home/ss901165/datasets_mango/SILO/t62'
silo_start_year=1891
silo_end_year=2008
silo_nyears=(silo_end_year-silo_start_year)+1
silo_nmonths=silo_nyears*12

; Timeseries of Australian rainfall from 20th Century Reanalysis (monthly)
twentyc_v2_indir='/home/ss901165/datasets_mango/20THC_REANALYSIS/precip'
twentyc_start_year=1891
twentyc_end_year=2008
twentyc_nyears=(twentyc_end_year-twentyc_start_year)+1
twentyc_nmonths=twentyc_nyears*12

mask_infile='/home/ss901165/datasets_mango/20THC_REANALYSIS/mask_t62.nc'

; Box to plot
box_plot=[-44,112,-10,153]
box_read=[-45,110,-10,155]

months=['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']
months_long=['January','February','March','April','May','June','July','August','September','October','November','December']
n_months=12

mylevs=['-0.4','-0.35','-0.3','-0.25','-0.2','-0.15','-0.1','-0.05','0','0.05','0.1','0.15','0.2','0.25','0.3','0.35','0.4']
mylevs_annual=['-0.7','-0.6','-0.5','-0.4','-0.3','-0.2','-0.1','0','0.1','0.2','0.3','0.4','0.5','0.6','0.7']

; Get NINO timeseries to match SILO data period, use 13th position for
; annual-mean value
obs_nino_offset=(silo_start_year-obs_nino_start_year)*12 ; Number of months to skip
obs_nino_count=silo_nmonths

obs_nino3_ts=OPEN_AND_EXTRACT(obs_nino_infile,'NINO3',offset=[obs_nino_offset],count=[obs_nino_count])
obs_nino3_ts_bymonth=fltarr(13,silo_nyears)
FOR i=0,silo_nyears-1 DO $
   obs_nino3_ts_bymonth(0:11,i)=obs_nino3_ts(i*12:(i+1)*12-1)
;FOR i=0,silo_nyears-1 DO $
;   obs_nino3_ts_bymonth(12,i)=MEAN(obs_nino3_ts(i*12:(i+1)*12-1))
FOR i=0,silo_nyears-2 DO $
   obs_nino3_ts_bymonth(12,i)=MEAN(obs_nino3_ts(i*12+4:(i+1)*12+3))

obs_nino4_ts=OPEN_AND_EXTRACT(obs_nino_infile,'NINO4',offset=[obs_nino_offset],count=[obs_nino_count])
obs_nino4_ts_bymonth=fltarr(13,silo_nyears)
FOR i=0,silo_nyears-1 DO $
   obs_nino4_ts_bymonth(0:11,i)=obs_nino4_ts(i*12:(i+1)*12-1)
;FOR i=0,silo_nyears-1 DO $
;   obs_nino4_ts_bymonth(12,i)=MEAN(obs_nino4_ts(i*12:(i+1)*12-1))
FOR i=0,silo_nyears-2 DO $
   obs_nino4_ts_bymonth(12,i)=MEAN(obs_nino4_ts(i*12+4:(i+1)*12+3))

obs_nino34_ts=OPEN_AND_EXTRACT(obs_nino_infile,'NINO34',offset=[obs_nino_offset],count=[obs_nino_count])
obs_nino34_ts_bymonth=fltarr(13,silo_nyears)
FOR i=0,silo_nyears-1 DO $
   obs_nino34_ts_bymonth(0:11,i)=obs_nino34_ts(i*12:(i+1)*12-1)
;FOR i=0,silo_nyears-1 DO $
;   obs_nino34_ts_bymonth(12,i)=MEAN(obs_nino34_ts(i*12:(i+1)*12-1))
FOR i=0,silo_nyears-2 DO $
   obs_nino34_ts_bymonth(12,i)=MEAN(obs_nino34_ts(i*12+4:(i+1)*12+3))


FOR i=0,n_months-1 DO BEGIN
   twentyc_v2_infile=twentyc_v2_indir+'/20thc_reanalysis.'+months(i)+'_mmeans.1891-2008.precip.nc'
   silo_t62_infile=silo_t62_indir+'/SILO.'+months(i)+'_mmeans.1891-2008.t62.nc'
   IF i eq 0 THEN BEGIN
      twentyc_latitude=OPEN_AND_EXTRACT(twentyc_v2_infile,'latitude')
      twentyc_longitude=OPEN_AND_EXTRACT(twentyc_v2_infile,'longitude')
      DEFINE_BOUNDARIES,box_read,twentyc_latitude,twentyc_longitude,twentyc_box_tx,/LIMIT
      twentyc_nlon=N_ELEMENTS(twentyc_longitude)
      twentyc_nlat=N_ELEMENTS(twentyc_latitude)
      
      silo_latitude=OPEN_AND_EXTRACT(silo_t62_infile,'latitude')
      silo_longitude=OPEN_AND_EXTRACT(silo_t62_infile,'longitude')
      DEFINE_BOUNDARIES,box_read,silo_latitude,silo_longitude,silo_box_tx,/LIMIT
      silo_nlon=N_ELEMENTS(silo_longitude)
      silo_nlat=N_ELEMENTS(silo_latitude)

      mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
      mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
      DEFINE_BOUNDARIES,box_read,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
      mask_nlon=N_ELEMENTS(mask_longitude)
      mask_nlat=N_ELEMENTS(mask_latitude)
      mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                                   offset=[mask_box_tx(1),mask_box_tx(0)],$
                                   count=[mask_nlon,mask_nlat]))
   ENDIF

   twentyc_v2_mmean_precip=REFORM(OPEN_AND_EXTRACT(twentyc_v2_infile,'PRATE',$
                                              offset=[twentyc_box_tx(1),twentyc_box_tx(0),0,0],$
                                                    count=[twentyc_nlon,twentyc_nlat,1,twentyc_nyears]))*86400.
   silo_t62_mmean_precip=REFORM(OPEN_AND_EXTRACT(silo_t62_infile,'rain',$
                                                  offset=[silo_box_tx(1),silo_box_tx(0),0,0],$
                                                  count=[silo_nlon,silo_nlat,silo_nyears,1]))
   

   silo_t62_nino3_correlation=fltarr(silo_nlon,silo_nlat)
   silo_t62_nino4_correlation=fltarr(silo_nlon,silo_nlat)
   silo_t62_nino34_correlation=fltarr(silo_nlon,silo_nlat)
   FOR j=0,silo_nlon-1 DO BEGIN
      FOR k=0,silo_nlat-1 DO BEGIN
         IF mask(j,k) eq 1 and silo_t62_mmean_precip(j,k) lt 10000 THEN BEGIN
            silo_t62_nino3_correlation(j,k)=CORRELATE(silo_t62_mmean_precip(j,k,*),obs_nino3_ts_bymonth(i,*))   
            silo_t62_nino4_correlation(j,k)=CORRELATE(silo_t62_mmean_precip(j,k,*),obs_nino4_ts_bymonth(i,*))
            silo_t62_nino34_correlation(j,k)=CORRELATE(silo_t62_mmean_precip(j,k,*),obs_nino34_ts_bymonth(i,*))
         ENDIF ELSE BEGIN
            silo_t62_nino3_correlation(j,k)=!Values.F_NaN
            silo_t62_nino4_correlation(j,k)=!Values.F_NaN
            silo_t62_nino34_correlation(j,k)=!Values.F_NaN
         ENDELSE
      ENDFOR
   ENDFOR

   twentyc_v2_nino3_correlation=fltarr(twentyc_nlon,twentyc_nlat)
   twentyc_v2_nino4_correlation=fltarr(twentyc_nlon,twentyc_nlat)
   twentyc_v2_nino34_correlation=fltarr(twentyc_nlon,twentyc_nlat)
   FOR j=0,twentyc_nlon-1 DO BEGIN
      FOR k=0,twentyc_nlat-1 DO BEGIN
         IF mask(j,k) eq 1 THEN BEGIN
            twentyc_v2_nino3_correlation(j,k)=CORRELATE(twentyc_v2_mmean_precip(j,k,*),obs_nino3_ts_bymonth(i,*))
            twentyc_v2_nino4_correlation(j,k)=CORRELATE(twentyc_v2_mmean_precip(j,k,*),obs_nino4_ts_bymonth(i,*))
            twentyc_v2_nino34_correlation(j,k)=CORRELATE(twentyc_v2_mmean_precip(j,k,*),obs_nino34_ts_bymonth(i,*))
         ENDIF ELSE BEGIN
            twentyc_v2_nino3_correlation(j,k)=!Values.F_NaN
            twentyc_v2_nino4_correlation(j,k)=!Values.F_NaN
            twentyc_v2_nino34_correlation(j,k)=!Values.F_NaN
         ENDELSE
      ENDFOR
   ENDFOR

   psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/nino_correlations/qld_20thc_reanalysis_nino_correlations_compare_silo.'+months(i)+'_nino3.silo_t62.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[10,11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=silo_t62_nino3_correlation,X=silo_longitude,Y=silo_latitude,$
       TITLE=months_long(i)+' instant correlation between NINO3 and SILO rainfall (1891-2008)',/NOLINES,$
       CB_NTH=2,/BLOCK
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/nino_correlations/qld_20thc_reanalysis_nino_correlations_compare_silo.'+months(i)+'_nino4.silo_t62.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[10,11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=silo_t62_nino4_correlation,X=silo_longitude,Y=silo_latitude,$
       TITLE=months_long(i)+' instant correlation between NINO4 and SILO rainfall (1891-2008)',/NOLINES,$
       CB_NTH=2,/BLOCK
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/nino_correlations/qld_20thc_reanalysis_nino_correlations_compare_silo.'+months(i)+'_nino34.silo_t62.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[10,11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=silo_t62_nino34_correlation,X=silo_longitude,Y=silo_latitude,$
       TITLE=months_long(i)+' instant correlation between NINO3.4 and SILO rainfall (1891-2008)',/NOLINES,$
       CB_NTH=2,/BLOCK
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/nino_correlations/qld_20thc_reanalysis_nino_correlations_compare_silo.'+months(i)+'_nino3.twentyc_v2.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[10,11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=twentyc_v2_nino3_correlation,X=twentyc_longitude,Y=twentyc_latitude,$
       TITLE=months_long(i)+' instant correlation between NINO3 and 20th Century V2 ens-mean rain (1891-2008)',/NOLINES,$
       CB_NTH=2,/BLOCK
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/nino_correlations/qld_20thc_reanalysis_nino_correlations_compare_silo.'+months(i)+'_nino4.twentyc_v2.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[10,11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=twentyc_v2_nino4_correlation,X=twentyc_longitude,Y=twentyc_latitude,$
       TITLE=months_long(i)+' instant correlation between NINO4 and 20th Century V2 ens-mean rain (1891-2008)',/NOLINES,$
       CB_NTH=2,/BLOCK
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/nino_correlations/qld_20thc_reanalysis_nino_correlations_compare_silo.'+months(i)+'_nino34.twentyc_v2.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[10,11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=twentyc_v2_nino34_correlation,X=twentyc_longitude,Y=twentyc_latitude,$
       TITLE=months_long(i)+' instant correlation between NINO3.4 and 20th Century V2 ens-mean rain (1891-2008)',/NOLINES,$
       CB_NTH=2,/BLOCK
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

ENDFOR
      
; Read in annual-mean rainfall for annual mean correlations

twentyc_v2_infile=twentyc_v2_indir+'/20thc_reanalysis.may-apr_ameans.1891-2007.precip.nc'
silo_t62_infile=silo_t62_indir+'/SILO_precip.may-apr_ameans.1891-2007.t62.nc'

;twentyc_v2_infile=twentyc_v2_indir+'/twentyc_v2.may-apr_ameans.h9-t5.precip.aus_domain.nc'
;higem_eafeb_infile=higem_eafeb_indir+'/higem_eafeb.may-apr_ameans.h9-w8.precip.aus_domain.nc'
;silo_t62_infile=silo_t62_indir+'/SILO_precip.may-apr_ameans.1900-2007.t62.nc'


twentyc_v2_amean_precip=REFORM(OPEN_AND_EXTRACT(twentyc_v2_infile,'PRATE',$
                                           offset=[twentyc_box_tx(1),twentyc_box_tx(0),0,0],$
                                           count=[twentyc_nlon,twentyc_nlat,1,twentyc_nyears-1]))*86400.
silo_t62_amean_precip=REFORM(OPEN_AND_EXTRACT(silo_t62_infile,'rain',$
                                               offset=[silo_box_tx(1),silo_box_tx(0),0],$
                                               count=[silo_nlon,silo_nlat,silo_nyears-1]))

FOR j=0,silo_nlon-1 DO BEGIN
   FOR k=0,silo_nlat-1 DO BEGIN
      IF mask(j,k) eq 1 THEN BEGIN
         silo_t62_nino3_correlation(j,k)=CORRELATE(silo_t62_amean_precip(j,k,*),obs_nino3_ts_bymonth(12,0:silo_nyears-2))   
         silo_t62_nino4_correlation(j,k)=CORRELATE(silo_t62_amean_precip(j,k,*),obs_nino4_ts_bymonth(12,0:silo_nyears-2))
         silo_t62_nino34_correlation(j,k)=CORRELATE(silo_t62_amean_precip(j,k,*),obs_nino34_ts_bymonth(12,0:silo_nyears-2))
      ENDIF
   ENDFOR
ENDFOR
FOR j=0,twentyc_nlon-1 DO BEGIN
   FOR k=0,twentyc_nlat-1 DO BEGIN
      IF mask(j,k) eq 1 THEN BEGIN
         twentyc_v2_nino3_correlation(j,k)=CORRELATE(twentyc_v2_amean_precip(j,k,*),obs_nino3_ts_bymonth(12,0:twentyc_nyears-2))   
         twentyc_v2_nino4_correlation(j,k)=CORRELATE(twentyc_v2_amean_precip(j,k,*),obs_nino4_ts_bymonth(12,0:twentyc_nyears-2))
         twentyc_v2_nino34_correlation(j,k)=CORRELATE(twentyc_v2_amean_precip(j,k,*),obs_nino34_ts_bymonth(12,0:twentyc_nyears-2))
      ENDIF ELSE BEGIN
         twentyc_v2_nino3_correlation(j,k)=!Values.F_NaN
         twentyc_v2_nino4_correlation(j,k)=!Values.F_NaN
         twentyc_v2_nino34_correlation(j,k)=!Values.F_NaN
      ENDELSE
   ENDFOR
ENDFOR

psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/nino_correlations/qld_20thc_reanalysis_nino_correlations_compare_silo.annmean_may-apr_nino3.silo_t62.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_annual)+1,/REV,white=[9,10]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=silo_t62_nino3_correlation,X=silo_longitude,Y=silo_latitude,$
    TITLE='Annual-mean (May-Apr) instant correlation between NINO3 and SILO rainfall (1891-2008)',/NOLINES,$
    CB_NTH=2,/BLOCK
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/nino_correlations/qld_20thc_reanalysis_nino_correlations_compare_silo.annmean_may-apr_nino4.silo_t62.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_annual)+1,/REV,white=[9,10]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=silo_t62_nino4_correlation,X=silo_longitude,Y=silo_latitude,$
    TITLE='Annual-mean (May-Apr) instant correlation between NINO4 and SILO rainfall (1891-2008)',/NOLINES,$
    CB_NTH=2,/BLOCK
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/nino_correlations/qld_20thc_reanalysis_nino_correlations_compare_silo.annmean_may-apr_nino34.silo_t62.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_annual)+1,/REV,white=[9,10]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=silo_t62_nino34_correlation,X=silo_longitude,Y=silo_latitude,$
    TITLE='Annual-mean (May-Apr) instant correlation between NINO3.4 and SILO rainfall (1891-2008)',/NOLINES,$
    CB_NTH=2,/BLOCK
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/nino_correlations/qld_20thc_reanalysis_nino_correlations_compare_silo.annmean_may-apr_nino3.twentyc_v2.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_annual)+1,/REV,white=[9,10]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=twentyc_v2_nino3_correlation,X=twentyc_longitude,Y=twentyc_latitude,$
    TITLE='Annual-mean (May-Apr) instant correlation between NINO3 and 20th Century V2 ens-mean rain (1891-2008)',/NOLINES,$
    CB_NTH=2,/BLOCK
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/nino_correlations/qld_20thc_reanalysis_nino_correlations_compare_silo.annmean_may-apr_nino4.twentyc_v2.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_annual)+1,/REV,white=[9,10]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=twentyc_v2_nino4_correlation,X=twentyc_longitude,Y=twentyc_latitude,$
    TITLE='Annual-mean (May-Apr) instant correlation between NINO4 and 20th Century V2 ens-mean rain (1891-2008)',/NOLINES,$
    CB_NTH=2,/BLOCK
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/nino_correlations/qld_20thc_reanalysis_nino_correlations_compare_silo.annmean_may-apr_nino34.twentyc_v2.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_annual)+1,/REV,white=[9,10]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=twentyc_v2_nino34_correlation,X=twentyc_longitude,Y=twentyc_latitude,$
    TITLE='Annual-mean (May-Apr) instant correlation between NINO3.4 and 20th Century V2 ens-mean rain (1891-2008)',/NOLINES,$
    CB_NTH=2,/BLOCK
AXES,XSTEP=5,YSTEP=5
PSCLOSE

STOP

END

