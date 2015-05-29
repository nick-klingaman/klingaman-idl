PRO higem_nino_correlations_compare_silo

; Timeseries of observed NINO values (monthly)
obs_nino_infile='/home/ss901165/datasets/NINO/nino_hadisst.jan-dec_mmeans.1871-2008.nc'
obs_nino_start_year=1871
obs_nino_end_year=2008
obs_nino_nyears=(obs_nino_end_year-obs_nino_start_year)+1
obs_nino_nmonths=obs_nino_nyears*12

; Timeseries of Australian rainfall from SILO (monthly)
silo_n144_indir='/home/ss901165/datasets_mango/SILO/n144'
silo_start_year=1900
silo_end_year=2008
silo_nyears=(silo_end_year-silo_start_year)+1
silo_nmonths=silo_nyears*12

; Timeseries of NINO indices from HiGEM (monthly)
higem_xbylr_nino_infile='/home/ss901165/higem_qccce/hpcx_control_xbylr/higem_xbylr.jan-dec_mmeans.h9-t5.nino_indices.nc'
higem_eafeb_nino_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.jan-dec_mmeans.h9-w8.nino_indices.nc'

; Timeseries of Australian rainfall from HiGEM (monthly)
higem_xbylr_indir='/home/ss901165/higem_qccce/hpcx_control_xbylr'
higem_eafeb_indir='/home/ss901165/higem_qccce/es_control_eafeb'
higem_xbylr_nyears=117
higem_eafeb_nyears=150

mask_infile='/home/ss901165/um_output/mask_n144_higam.nc'

; Box to plot
box_plot=[-44,112,-10,154]
box_read=[-45,110,-10,155]

months=['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']
months_long=['January','February','March','April','May','June','July','August','September','October','November','December']
n_months=12

seasons=['mar-may','jun-aug','sep-nov','dec-feb']
seasons_long=['March-May','June-August','September-November','December-February']
n_seasons=4

mylevs=['-0.375','-0.325','-0.275','-0.225','-0.175','-0.125','-0.075','-0.025','0.025','0.075','0.125','0.175','0.225','0.275','0.325','0.375']
mylevs_annual=['-0.65','-0.55','-0.45','-0.35','-0.25','-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55','0.65']
mylevs_seasonal=['-0.55','-0.45','-0.35','-0.25','-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55']

; Get NINO timeseries to match SILO data period, use 13th position for
; annual-mean value
obs_nino_offset=(silo_start_year-obs_nino_start_year)*12 ; Number of months to skip
obs_nino_count=silo_nmonths

obs_nino3_ts=OPEN_AND_EXTRACT(obs_nino_infile,'NINO3',offset=[obs_nino_offset],count=[obs_nino_count])
obs_nino3_ts_bymonth=fltarr(13,silo_nyears)
obs_nino3_ts_byseason=fltarr(n_seasons,silo_nyears)
FOR i=0,silo_nyears-1 DO BEGIN
   obs_nino3_ts_bymonth(0:11,i)=obs_nino3_ts(i*12:(i+1)*12-1)
   IF i ne silo_nyears-1 THEN BEGIN
      FOR j=0,n_seasons-1 DO $
         obs_nino3_ts_byseason(j,i)=MEAN(obs_nino3_ts(i*12+j*3+2:i*12+(j+1)*3+1))
   ENDIF
ENDFOR
;FOR i=0,silo_nyears-1 DO $
;   obs_nino3_ts_bymonth(12,i)=MEAN(obs_nino3_ts(i*12:(i+1)*12-1))
FOR i=0,silo_nyears-2 DO $
   obs_nino3_ts_bymonth(12,i)=MEAN(obs_nino3_ts(i*12+4:(i+1)*12+3))

obs_nino4_ts=OPEN_AND_EXTRACT(obs_nino_infile,'NINO4',offset=[obs_nino_offset],count=[obs_nino_count])
obs_nino4_ts_bymonth=fltarr(13,silo_nyears)
obs_nino4_ts_byseason=fltarr(n_seasons,silo_nyears)
FOR i=0,silo_nyears-1 DO BEGIN
   obs_nino4_ts_bymonth(0:11,i)=obs_nino4_ts(i*12:(i+1)*12-1)
   IF i ne silo_nyears-1 THEN BEGIN
      FOR j=0,n_seasons-1 DO $
         obs_nino4_ts_byseason(j,i)=MEAN(obs_nino4_ts(i*12+j*3+2:i*12+(j+1)*3+1))
   ENDIF
ENDFOR
;FOR i=0,silo_nyears-1 DO $
;   obs_nino4_ts_bymonth(12,i)=MEAN(obs_nino4_ts(i*12:(i+1)*12-1))
FOR i=0,silo_nyears-2 DO $
   obs_nino4_ts_bymonth(12,i)=MEAN(obs_nino4_ts(i*12+4:(i+1)*12+3))

obs_nino34_ts=OPEN_AND_EXTRACT(obs_nino_infile,'NINO34',offset=[obs_nino_offset],count=[obs_nino_count])
obs_nino34_ts_bymonth=fltarr(13,silo_nyears)
obs_nino34_ts_byseason=fltarr(n_seasons,silo_nyears)
FOR i=0,silo_nyears-1 DO BEGIN
   obs_nino34_ts_bymonth(0:11,i)=obs_nino34_ts(i*12:(i+1)*12-1)
   IF I ne silo_nyears-1 THEN BEGIN
      FOR j=0,n_seasons-1 DO $
         obs_nino34_ts_byseason(j,i)=MEAN(obs_nino34_ts(i*12+j*3+2:i*12+(j+1)*3+1))
   ENDIF
ENDFOR
;FOR i=0,silo_nyears-1 DO $
;   obs_nino34_ts_bymonth(12,i)=MEAN(obs_nino34_ts(i*12:(i+1)*12-1))
FOR i=0,silo_nyears-2 DO $
   obs_nino34_ts_bymonth(12,i)=MEAN(obs_nino34_ts(i*12+4:(i+1)*12+3))

; Get NINO timeseries from HiGEM, use 13th position for annual-mean value
higem_xbylr_nino3_ts=OPEN_AND_EXTRACT(higem_xbylr_nino_infile,'nino3')
higem_xbylr_nino4_ts=OPEN_AND_EXTRACT(higem_xbylr_nino_infile,'nino4')
higem_xbylr_nino34_ts=OPEN_AND_EXTRACT(higem_xbylr_nino_infile,'nino34')
higem_eafeb_nino3_ts=OPEN_AND_EXTRACT(higem_eafeb_nino_infile,'nino3')
higem_eafeb_nino4_ts=OPEN_AND_EXTRACT(higem_eafeb_nino_infile,'nino4')
higem_eafeb_nino34_ts=OPEN_AND_EXTRACT(higem_eafeb_nino_infile,'nino34')
higem_xbylr_nino3_ts_bymonth=fltarr(13,higem_xbylr_nyears)
higem_xbylr_nino4_ts_bymonth=fltarr(13,higem_xbylr_nyears)
higem_xbylr_nino34_ts_bymonth=fltarr(13,higem_xbylr_nyears)
higem_eafeb_nino3_ts_bymonth=fltarr(13,higem_eafeb_nyears)
higem_eafeb_nino4_ts_bymonth=fltarr(13,higem_eafeb_nyears)
higem_eafeb_nino34_ts_bymonth=fltarr(13,higem_eafeb_nyears)
higem_eafeb_nino3_ts_byseason=fltarr(4,higem_eafeb_nyears)
higem_eafeb_nino4_ts_byseason=fltarr(4,higem_eafeb_nyears)
higem_eafeb_nino34_ts_byseason=fltarr(4,higem_eafeb_nyears)
FOR i=0,higem_xbylr_nyears-1 DO BEGIN
   higem_xbylr_nino3_ts_bymonth(0:11,i)=higem_xbylr_nino3_ts(i*12:(i+1)*12-1)   
   higem_xbylr_nino4_ts_bymonth(0:11,i)=higem_xbylr_nino4_ts(i*12:(i+1)*12-1)  
   higem_xbylr_nino34_ts_bymonth(0:11,i)=higem_xbylr_nino34_ts(i*12:(i+1)*12-1)
ENDFOR
FOR i=0,higem_xbylr_nyears-2 DO BEGIN
   higem_xbylr_nino3_ts_bymonth(12,i)=MEAN(higem_xbylr_nino3_ts(i*12+4:(i+1)*12+3))
   higem_xbylr_nino4_ts_bymonth(12,i)=MEAN(higem_xbylr_nino4_ts(i*12+4:(i+1)*12+3))
   higem_xbylr_nino34_ts_bymonth(12,i)=MEAN(higem_xbylr_nino34_ts(i*12+4:(i+1)*12+3)) 
ENDFOR
FOR i=0,higem_eafeb_nyears-1 DO BEGIN
   higem_eafeb_nino3_ts_bymonth(0:11,i)=higem_eafeb_nino3_ts(i*12:(i+1)*12-1)
   higem_eafeb_nino4_ts_bymonth(0:11,i)=higem_eafeb_nino4_ts(i*12:(i+1)*12-1)
   higem_eafeb_nino34_ts_bymonth(0:11,i)=higem_eafeb_nino34_ts(i*12:(i+1)*12-1)
   IF i ne higem_eafeb_nyears-1 THEN BEGIN
      FOR j=0,n_seasons-1 DO BEGIN
         higem_eafeb_nino3_ts_byseason(j,i)=MEAN(higem_eafeb_nino3_ts(i*12+j*3+2:i*12+(j+1)*3+1))
         higem_eafeb_nino4_ts_byseason(j,i)=MEAN(higem_eafeb_nino4_ts(i*12+j*3+2:i*12+(j+1)*3+1))
         higem_eafeb_nino34_ts_byseason(j,i)=MEAN(higem_eafeb_nino34_ts(i*12+j*3+2:i*12+(j+1)*3+1))
      ENDFOR
   ENDIF
ENDFOR
FOR i=0,higem_eafeb_nyears-2 DO BEGIN
   higem_eafeb_nino3_ts_bymonth(12,i)=MEAN(higem_eafeb_nino3_ts(i*12+4:(i+1)*12+3))
   higem_eafeb_nino4_ts_bymonth(12,i)=MEAN(higem_eafeb_nino4_ts(i*12+4:(i+1)*12+3))
   higem_eafeb_nino34_ts_bymonth(12,i)=MEAN(higem_eafeb_nino34_ts(i*12+4:(i+1)*12+3))   
ENDFOR
FOR i=0,n_months-1 DO BEGIN
   higem_xbylr_infile=higem_xbylr_indir+'/higem_xbylr.'+months(i)+'_mmeans.h9-t5.precip.aus_domain.nc'
   higem_eafeb_infile=higem_eafeb_indir+'/higem_eafeb.'+months(i)+'_mmeans.h9-w8.precip.aus_domain.nc'
   silo_n144_infile=silo_n144_indir+'/SILO.'+months(i)+'_mmeans.1900-2008.n144.nc'
   IF i eq 0 THEN BEGIN
      higem_latitude=OPEN_AND_EXTRACT(higem_xbylr_infile,'latitude')
      higem_longitude=OPEN_AND_EXTRACT(higem_xbylr_infile,'longitude')
      DEFINE_BOUNDARIES,box_read,higem_latitude,higem_longitude,higem_box_tx,/LIMIT
      higem_nlon=N_ELEMENTS(higem_longitude)
      higem_nlat=N_ELEMENTS(higem_latitude)
      
      silo_latitude=OPEN_AND_EXTRACT(silo_n144_infile,'latitude')
      silo_longitude=OPEN_AND_EXTRACT(silo_n144_infile,'longitude')
      DEFINE_BOUNDARIES,box_read,silo_latitude,silo_longitude,silo_box_tx,/LIMIT
      silo_nlon=N_ELEMENTS(silo_longitude)
      silo_nlat=N_ELEMENTS(silo_latitude)

      mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
      mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
      DEFINE_BOUNDARIES,box_read,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
      mask_nlon=N_ELEMENTS(mask_longitude)
      mask_nlat=N_ELEMENTS(mask_latitude)
      mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                                   offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                                   count=[mask_nlon,mask_nlat,1,1]))
   ENDIF

   higem_xbylr_mmean_precip=REFORM(OPEN_AND_EXTRACT(higem_xbylr_infile,'precip',$
                                              offset=[higem_box_tx(1),higem_box_tx(0),0,0,0],$
                                                    count=[higem_nlon,higem_nlat,1,higem_xbylr_nyears,1]))*86400.
   higem_eafeb_mmean_precip=REFORM(OPEN_AND_EXTRACT(higem_eafeb_infile,'precip',$
                                                    offset=[higem_box_tx(1),higem_box_tx(0),0,0,0],$
                                                    count=[higem_nlon,higem_nlat,1,higem_eafeb_nyears,1]))*86400.
   silo_n144_mmean_precip=REFORM(OPEN_AND_EXTRACT(silo_n144_infile,'rain',$
                                                  offset=[silo_box_tx(1),silo_box_tx(0),0,0],$
                                                  count=[silo_nlon,silo_nlat,silo_nyears,1]))
   

   silo_n144_nino3_correlation=fltarr(silo_nlon,silo_nlat)
   silo_n144_nino4_correlation=fltarr(silo_nlon,silo_nlat)
   silo_n144_nino34_correlation=fltarr(silo_nlon,silo_nlat)
   FOR j=0,silo_nlon-1 DO BEGIN
      FOR k=0,silo_nlat-1 DO BEGIN
         IF mask(j,silo_nlat-k-1) eq 1 and silo_n144_mmean_precip(j,k) lt 10000 THEN BEGIN
            silo_n144_nino3_correlation(j,k)=CORRELATE(silo_n144_mmean_precip(j,k,*),obs_nino3_ts_bymonth(i,*))   
            silo_n144_nino4_correlation(j,k)=CORRELATE(silo_n144_mmean_precip(j,k,*),obs_nino4_ts_bymonth(i,*))
            silo_n144_nino34_correlation(j,k)=CORRELATE(silo_n144_mmean_precip(j,k,*),obs_nino34_ts_bymonth(i,*))
         ENDIF ELSE BEGIN
            silo_n144_nino3_correlation(j,k)=!Values.F_NaN
            silo_n144_nino4_correlation(j,k)=!Values.F_NaN
            silo_n144_nino34_correlation(j,k)=!Values.F_NaN
         ENDELSE
      ENDFOR
   ENDFOR

   higem_xbylr_nino3_correlation=fltarr(higem_nlon,higem_nlat)
   higem_xbylr_nino4_correlation=fltarr(higem_nlon,higem_nlat)
   higem_xbylr_nino34_correlation=fltarr(higem_nlon,higem_nlat)
   higem_eafeb_nino3_correlation=fltarr(higem_nlon,higem_nlat)
   higem_eafeb_nino4_correlation=fltarr(higem_nlon,higem_nlat)
   higem_eafeb_nino34_correlation=fltarr(higem_nlon,higem_nlat)
   FOR j=0,higem_nlon-1 DO BEGIN
      FOR k=0,higem_nlat-1 DO BEGIN
         IF mask(j,k) eq 1 THEN BEGIN
            higem_xbylr_nino3_correlation(j,k)=CORRELATE(higem_xbylr_mmean_precip(j,k,*),higem_xbylr_nino3_ts_bymonth(i,*))
            higem_xbylr_nino4_correlation(j,k)=CORRELATE(higem_xbylr_mmean_precip(j,k,*),higem_xbylr_nino4_ts_bymonth(i,*))
            higem_xbylr_nino34_correlation(j,k)=CORRELATE(higem_xbylr_mmean_precip(j,k,*),higem_xbylr_nino34_ts_bymonth(i,*))

            higem_eafeb_nino3_correlation(j,k)=CORRELATE(higem_eafeb_mmean_precip(j,k,*),higem_eafeb_nino3_ts_bymonth(i,*))
            higem_eafeb_nino4_correlation(j,k)=CORRELATE(higem_eafeb_mmean_precip(j,k,*),higem_eafeb_nino4_ts_bymonth(i,*))
            higem_eafeb_nino34_correlation(j,k)=CORRELATE(higem_eafeb_mmean_precip(j,k,*),higem_eafeb_nino34_ts_bymonth(i,*))
         ENDIF ELSE BEGIN
            higem_xbylr_nino3_correlation(j,k)=!Values.F_NaN
            higem_xbylr_nino4_correlation(j,k)=!Values.F_NaN
            higem_xbylr_nino34_correlation(j,k)=!Values.F_NaN

            higem_eafeb_nino3_correlation(j,k)=!Values.F_NaN
            higem_eafeb_nino4_correlation(j,k)=!Values.F_NaN
            higem_eafeb_nino34_correlation(j,k)=!Values.F_NaN
         ENDELSE
      ENDFOR
   ENDFOR

   psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo.'+months(i)+'_nino3.silo_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[10,11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=silo_n144_nino3_correlation,X=silo_longitude,Y=silo_latitude,$
       TITLE=months_long(i)+' corr NINO3 and SILO rainfall (1900-2008)',/NOLINES,$
       CB_NTH=2
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo.'+months(i)+'_nino4.silo_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[10,11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=silo_n144_nino4_correlation,X=silo_longitude,Y=silo_latitude,$
       TITLE=months_long(i)+' corr NINO4 and SILO rainfall (1900-2008)',/NOLINES,$
       CB_NTH=2
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo.'+months(i)+'_nino34.silo_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[10,11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=silo_n144_nino34_correlation,X=silo_longitude,Y=silo_latitude,$
       TITLE=months_long(i)+' corr NINO3.4 and SILO rainfall (1900-2008)',/NOLINES,$
       CB_NTH=2
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo.'+months(i)+'_nino3.higem_xbylr.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[10,11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=higem_xbylr_nino3_correlation,X=higem_longitude,Y=higem_latitude,$
       TITLE=months_long(i)+' corr HiGEM NINO3 and rainfall (xbylr, 117 years)',/NOLINES,$
       CB_NTH=2
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo.'+months(i)+'_nino4.higem_xbylr.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[10,11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=higem_xbylr_nino4_correlation,X=higem_longitude,Y=higem_latitude,$
       TITLE=months_long(i)+' corr HiGEM NINO4 and rainfall (xbylr, 117 years)',/NOLINES,$
       CB_NTH=2
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo.'+months(i)+'_nino34.higem_xbylr.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[10,11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=higem_xbylr_nino34_correlation,X=higem_longitude,Y=higem_latitude,$
       TITLE=months_long(i)+' corr HiGEM NINO3.4 and rainfall (xbylr, 117 years)',/NOLINES,$
       CB_NTH=2
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo.'+months(i)+'_nino3.higem_eafeb.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[10,11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=higem_eafeb_nino3_correlation,X=higem_longitude,Y=higem_latitude,$
       TITLE=months_long(i)+' corr HiGEM NINO3 and rainfall (eafeb, 150 years)',/NOLINES,$
       CB_NTH=2
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo.'+months(i)+'_nino4.higem_eafeb.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[10,11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=higem_eafeb_nino4_correlation,X=higem_longitude,Y=higem_latitude,$
       TITLE=months_long(i)+' corr HiGEM NINO4 and rainfall (eafeb, 150 years)',/NOLINES,$
       CB_NTH=2
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo.'+months(i)+'_nino34.higem_eafeb.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[10,11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=higem_eafeb_nino34_correlation,X=higem_longitude,Y=higem_latitude,$
       TITLE=months_long(i)+' corr HiGEM NINO3.4 and rainfall (eafeb, 150 years)',/NOLINES,$
       CB_NTH=2
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

ENDFOR

FOR i=0,n_seasons-1 DO BEGIN
   ; Read in seasonal-mean rainfall for seasonal mean correlations
   higem_eafeb_infile=higem_eafeb_indir+'/higem_eafeb.'+seasons(i)+'_smeans.h9-w8.precip.aus_domain.nc'
   silo_n144_infile=silo_n144_indir+'/SILO_precip.'+seasons(i)+'_smeans.1900-2008.n144.nc'

   higem_eafeb_smean_precip=REFORM(OPEN_AND_EXTRACT(higem_eafeb_infile,'precip',$
                                                    offset=[higem_box_tx(1),higem_box_tx(0),0],$
                                                    count=[higem_nlon,higem_nlat,higem_eafeb_nyears-1]))*86400.
   silo_n144_smean_precip=REFORM(OPEN_AND_EXTRACT(silo_n144_infile,'rain',$
                                                  offset=[silo_box_tx(1),silo_box_tx(0),0],$
                                                  count=[silo_nlon,silo_nlat,silo_nyears-1]))
   FOR j=0,silo_nlon-1 DO BEGIN
      FOR k=0,silo_nlat-1 DO BEGIN
         IF mask(j,silo_nlat-k-1) eq 1 THEN BEGIN
            silo_n144_nino3_correlation(j,k)=CORRELATE(silo_n144_smean_precip(j,k,*),obs_nino3_ts_byseason(i,0:silo_nyears-2))
            silo_n144_nino4_correlation(j,k)=CORRELATE(silo_n144_smean_precip(j,k,*),obs_nino4_ts_byseason(i,0:silo_nyears-2))
            silo_n144_nino34_correlation(j,k)=CORRELATE(silo_n144_smean_precip(j,k,*),obs_nino34_ts_byseason(i,0:silo_nyears-2))
         ENDIF ELSE BEGIN
            silo_n144_nino3_correlation(j,k)=!Values.F_NaN
            silo_n144_nino4_correlation(j,k)=!Values.F_NaN
            silo_n144_nino34_correlation(j,k)=!Values.F_NaN
         ENDELSE
      ENDFOR
   ENDFOR
   FOR j=0,higem_nlon-1 DO BEGIN
      FOR k=0,higem_nlat-1 DO BEGIN
         IF mask(j,k) eq 1 THEN BEGIN
            higem_eafeb_nino3_correlation(j,k)=CORRELATE(higem_eafeb_smean_precip(j,k,*),higem_eafeb_nino3_ts_byseason(i,0:higem_eafeb_nyears-2))
            higem_eafeb_nino4_correlation(j,k)=CORRELATE(higem_eafeb_smean_precip(j,k,*),higem_eafeb_nino4_ts_byseason(i,0:higem_eafeb_nyears-2))
            higem_eafeb_nino34_correlation(j,k)=CORRELATE(higem_eafeb_smean_precip(j,k,*),higem_eafeb_nino34_ts_byseason(i,0:higem_eafeb_nyears-2))
         ENDIF ELSE BEGIN
            higem_eafeb_nino3_correlation(j,k)=!Values.F_NaN
            higem_eafeb_nino4_correlation(j,k)=!Values.F_NaN
            higem_eafeb_nino34_correlation(j,k)=!Values.F_NaN
         ENDELSE
      ENDFOR
   ENDFOR

   psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo.'+seasons(i)+'_smean.nino3_silo_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=115,XSIZE=15000
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_seasonal)+1,/REV,white=[8]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_seasonal
   CON,FIELD=silo_n144_nino3_correlation,X=silo_longitude,Y=silo_latitude,$
       TITLE=seasons_long(i)+' corr NINO3 and SILO rainfall (1900-2008)',/NOLINES,$
       CB_NTH=1
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo.'+seasons(i)+'_smean.nino4_silo_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=115,XSIZE=15000
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_seasonal)+1,/REV,white=[8]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_seasonal
   CON,FIELD=silo_n144_nino4_correlation,X=silo_longitude,Y=silo_latitude,$
       TITLE=seasons_long(i)+' corr NINO4 and SILO rainfall (1900-2008)',/NOLINES,$
       CB_NTH=1
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo.'+seasons(i)+'_smean.nino34_silo_n144.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=115,XSIZE=15000
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_seasonal)+1,/REV,white=[8]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_seasonal
   CON,FIELD=silo_n144_nino34_correlation,X=silo_longitude,Y=silo_latitude,$
       TITLE=seasons_long(i)+' corr NINO3.4 and SILO rainfall (1900-2008)',/NOLINES,$
       CB_NTH=1
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo.'+seasons(i)+'_smean.nino3_higem_eafeb.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=115,XSIZE=15000
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_seasonal)+1,/REV,white=[8]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_seasonal
   CON,FIELD=higem_eafeb_nino3_correlation,X=higem_longitude,Y=higem_latitude,$
       TITLE=seasons_long(i)+' corr HiGEM NINO3 and rainfall (eafeb, 150 years)',/NOLINES,$
       CB_NTH=1
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo.'+seasons(i)+'_smean.nino4_higem_eafeb.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=115,XSIZE=15000
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_seasonal)+1,/REV,white=[8]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_seasonal
   CON,FIELD=higem_eafeb_nino4_correlation,X=higem_longitude,Y=higem_latitude,$
       TITLE=seasons_long(i)+' corr HiGEM NINO4 and rainfall (eafeb, 150 years)',/NOLINES,$
       CB_NTH=1
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo.'+seasons(i)+'.nino34_higem_eafeb.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=115,XSIZE=15000
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_seasonal)+1,/REV,white=[8]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_seasonal
   CON,FIELD=higem_eafeb_nino34_correlation,X=higem_longitude,Y=higem_latitude,$
       TITLE=seasons_long(i)+' corr HiGEM NINO3.4 and rainfall (eafeb, 150 years)',/NOLINES,$
       CB_NTH=1
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

ENDFOR
      
; Read in annual-mean rainfall for annual mean correlations

higem_xbylr_infile=higem_xbylr_indir+'/higem_xbylr.may-apr_ameans.h9-t5.precip.aus_domain.nc'
higem_eafeb_infile=higem_eafeb_indir+'/higem_eafeb.may-apr_ameans.h9-w8.precip.aus_domain.nc'
silo_n144_infile=silo_n144_indir+'/SILO_precip.may-apr_ameans.1900-2007.n144.nc'

;higem_xbylr_infile=higem_xbylr_indir+'/higem_xbylr.may-apr_ameans.h9-t5.precip.aus_domain.nc'
;higem_eafeb_infile=higem_eafeb_indir+'/higem_eafeb.may-apr_ameans.h9-w8.precip.aus_domain.nc'
;silo_n144_infile=silo_n144_indir+'/SILO_precip.may-apr_ameans.1900-2007.n144.nc'


higem_xbylr_amean_precip=REFORM(OPEN_AND_EXTRACT(higem_xbylr_infile,'precip',$
                                           offset=[higem_box_tx(1),higem_box_tx(0),0,0,0],$
                                           count=[higem_nlon,higem_nlat,1,1,higem_xbylr_nyears-1]))*86400.
higem_eafeb_amean_precip=REFORM(OPEN_AND_EXTRACT(higem_eafeb_infile,'precip',$
                                           offset=[higem_box_tx(1),higem_box_tx(0),0,0,0],$
                                           count=[higem_nlon,higem_nlat,1,1,higem_eafeb_nyears-1]))*86400.
silo_n144_amean_precip=REFORM(OPEN_AND_EXTRACT(silo_n144_infile,'rain',$
                                               offset=[silo_box_tx(1),silo_box_tx(0),0],$
                                               count=[silo_nlon,silo_nlat,silo_nyears-1]))

FOR j=0,silo_nlon-1 DO BEGIN
   FOR k=0,silo_nlat-1 DO BEGIN
      IF mask(j,silo_nlat-k-1) eq 1 THEN BEGIN
         silo_n144_nino3_correlation(j,k)=CORRELATE(silo_n144_amean_precip(j,k,*),obs_nino3_ts_bymonth(12,0:silo_nyears-2))   
         silo_n144_nino4_correlation(j,k)=CORRELATE(silo_n144_amean_precip(j,k,*),obs_nino4_ts_bymonth(12,0:silo_nyears-2))
         silo_n144_nino34_correlation(j,k)=CORRELATE(silo_n144_amean_precip(j,k,*),obs_nino34_ts_bymonth(12,0:silo_nyears-2))
      ENDIF
   ENDFOR
ENDFOR
FOR j=0,higem_nlon-1 DO BEGIN
   FOR k=0,higem_nlat-1 DO BEGIN
      IF mask(j,k) eq 1 THEN BEGIN
         higem_xbylr_nino3_correlation(j,k)=CORRELATE(higem_xbylr_amean_precip(j,k,*),higem_xbylr_nino3_ts_bymonth(12,0:higem_xbylr_nyears-2))   
         higem_xbylr_nino4_correlation(j,k)=CORRELATE(higem_xbylr_amean_precip(j,k,*),higem_xbylr_nino4_ts_bymonth(12,0:higem_xbylr_nyears-2))
         higem_xbylr_nino34_correlation(j,k)=CORRELATE(higem_xbylr_amean_precip(j,k,*),higem_xbylr_nino34_ts_bymonth(12,0:higem_xbylr_nyears-2))

         higem_eafeb_nino3_correlation(j,k)=CORRELATE(higem_eafeb_amean_precip(j,k,*),higem_eafeb_nino3_ts_bymonth(12,0:higem_eafeb_nyears-2))   
         higem_eafeb_nino4_correlation(j,k)=CORRELATE(higem_eafeb_amean_precip(j,k,*),higem_eafeb_nino4_ts_bymonth(12,0:higem_eafeb_nyears-2))
         higem_eafeb_nino34_correlation(j,k)=CORRELATE(higem_eafeb_amean_precip(j,k,*),higem_eafeb_nino34_ts_bymonth(12,0:higem_eafeb_nyears-2))
      ENDIF ELSE BEGIN
         higem_xbylr_nino3_correlation(j,k)=!Values.F_NaN
         higem_xbylr_nino4_correlation(j,k)=!Values.F_NaN
         higem_xbylr_nino34_correlation(j,k)=!Values.F_NaN

         higem_eafeb_nino3_correlation(j,k)=!Values.F_NaN
         higem_eafeb_nino4_correlation(j,k)=!Values.F_NaN
         higem_eafeb_nino34_correlation(j,k)=!Values.F_NaN
      ENDELSE
   ENDFOR
ENDFOR

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo.annmean_may-apr_nino3.silo_n144.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=115,XSIZE=15000
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_annual)+1,/REV,white=[9]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=silo_n144_nino3_correlation,X=silo_longitude,Y=silo_latitude,$
    TITLE='Annual-mean (May-Apr) corr NINO3 and SILO rainfall (1900-2008)',/NOLINES,$
    CB_NTH=1
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo.annmean_may-apr_nino4.silo_n144.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=115,XSIZE=15000
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_annual)+1,/REV,white=[9]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=silo_n144_nino4_correlation,X=silo_longitude,Y=silo_latitude,$
    TITLE='Annual-mean (May-Apr) corr NINO4 and SILO rainfall (1900-2008)',/NOLINES,$
    CB_NTH=1
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo.annmean_may-apr_nino34.silo_n144.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=115,XSIZE=15000
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_annual)+1,/REV,white=[9]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=silo_n144_nino34_correlation,X=silo_longitude,Y=silo_latitude,$
    TITLE='Annual-mean (May-Apr) corr NINO3.4 and SILO rainfall (1900-2008)',/NOLINES,$
    CB_NTH=1
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo.annmean_may-apr_nino3.higem_xbylr.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=115,XSIZE=15000
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_annual)+1,/REV,white=[9]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=higem_xbylr_nino3_correlation,X=higem_longitude,Y=higem_latitude,$
    TITLE='Annual-mean (May-Apr) corr NINO3 and HiGEM rainfall',/NOLINES,$
    CB_NTH=1
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo.annmean_may-apr_nino4.higem_xbylr.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=115,XSIZE=15000
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_annual)+1,/REV,white=[9]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=higem_xbylr_nino4_correlation,X=higem_longitude,Y=higem_latitude,$
    TITLE='Annual-mean (May-Apr) corr NINO4 and HiGEM xbylr rainfall',/NOLINES,$
    CB_NTH=1
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo.annmean_may-apr_nino34.higem_xbylr.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=115,XSIZE=15000
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_annual)+1,/REV,white=[9]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=higem_xbylr_nino34_correlation,X=higem_longitude,Y=higem_latitude,$
    TITLE='Annual-mean (May-Apr) corr NINO3.4 and HiGEM xbylr rainfall',/NOLINES,$
    CB_NTH=1
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo.annmean_may-apr_nino3.higem_eafeb.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=115,XSIZE=15000
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_annual)+1,/REV,white=[9]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=higem_eafeb_nino3_correlation,X=higem_longitude,Y=higem_latitude,$
    TITLE='Annual-mean (May-Apr) corr NINO3 and HiGEM rainfall',/NOLINES,$
    CB_NTH=1
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo.annmean_may-apr_nino4.higem_eafeb.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=115,XSIZE=15000
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_annual)+1,/REV,white=[9]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=higem_eafeb_nino4_correlation,X=higem_longitude,Y=higem_latitude,$
    TITLE='Annual-mean (May-Apr) corr NINO4 and HiGEM eafeb rainfall',/NOLINES,$
    CB_NTH=1
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo.annmean_may-apr_nino34.higem_eafeb.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=115,XSIZE=15000
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_annual)+1,/REV,white=[9]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=higem_eafeb_nino34_correlation,X=higem_longitude,Y=higem_latitude,$
    TITLE='Annual-mean (May-Apr) corr NINO3.4 and HiGEM eafeb rainfall',/NOLINES,$
    CB_NTH=1
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

STOP

END

