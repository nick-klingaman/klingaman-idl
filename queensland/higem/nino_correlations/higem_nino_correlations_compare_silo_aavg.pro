PRO higem_nino_correlations_compare_silo_aavg

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
higem_eafeb_nyears=149

; N144 land/sea mask
mask_infile='/home/ss901165/um_output/mask_n144_higam.nc'

; Box over which to area-average
box_aavg=[-10,138,-30,155]
region_name='queensland'
;box_aavg=[-10,112,-20,155]
;region_name='monsoon'

months=['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']
months_long=['January','February','March','April','May','June','July','August','September','October','November','December']
n_months=12

mylevs=['-0.6','-0.5','-0.4','-0.3','-0.25','-0.2','-0.15','-0.1','-0.05','0','0.05','0.1','0.15','0.2','0.25','0.3','0.4','0.5','0.6']

; Get NINO timeseries to match SILO data period
obs_nino_offset=(silo_start_year-obs_nino_start_year)*12 ; Number of months to skip
obs_nino_count=silo_nmonths

; Make longer NINO timeseries for cross-correlation
obs_nino3_ts=OPEN_AND_EXTRACT(obs_nino_infile,'NINO3',offset=[obs_nino_offset],count=[obs_nino_count])
obs_nino3_ts_bymonth=fltarr(36,silo_nyears)
obs_nino4_ts=OPEN_AND_EXTRACT(obs_nino_infile,'NINO4',offset=[obs_nino_offset],count=[obs_nino_count])
obs_nino4_ts_bymonth=fltarr(36,silo_nyears)
obs_nino34_ts=OPEN_AND_EXTRACT(obs_nino_infile,'NINO34',offset=[obs_nino_offset],count=[obs_nino_count])
obs_nino34_ts_bymonth=fltarr(36,silo_nyears)
FOR i=1,silo_nyears-2 DO BEGIN
   obs_nino3_ts_bymonth(0:11,i)=obs_nino3_ts((i-1)*12:i*12-1)
   obs_nino3_ts_bymonth(12:23,i)=obs_nino3_ts(i*12:(i+1)*12-1)
   obs_nino3_ts_bymonth(24:35,i)=obs_nino3_ts((i+1)*12:(i+2)*12-1)
   obs_nino4_ts_bymonth(0:11,i)=obs_nino4_ts((i-1)*12:i*12-1)
   obs_nino4_ts_bymonth(12:23,i)=obs_nino4_ts(i*12:(i+1)*12-1)
   obs_nino4_ts_bymonth(24:35,i)=obs_nino4_ts((i+1)*12:(i+2)*12-1)
   obs_nino34_ts_bymonth(0:11,i)=obs_nino34_ts((i-1)*12:i*12-1)
   obs_nino34_ts_bymonth(12:23,i)=obs_nino34_ts(i*12:(i+1)*12-1)
   obs_nino34_ts_bymonth(24:35,i)=obs_nino34_ts((i+1)*12:(i+2)*12-1)
ENDFOR

STOP

; Get NINO timeseries from HiGEM xbylr
higem_xbylr_nino3_ts=OPEN_AND_EXTRACT(higem_xbylr_nino_infile,'nino3')
higem_xbylr_nino4_ts=OPEN_AND_EXTRACT(higem_xbylr_nino_infile,'nino4')
higem_xbylr_nino34_ts=OPEN_AND_EXTRACT(higem_xbylr_nino_infile,'nino34')
higem_xbylr_nino3_ts_bymonth=fltarr(36,higem_xbylr_nyears)
higem_xbylr_nino4_ts_bymonth=fltarr(36,higem_xbylr_nyears)
higem_xbylr_nino34_ts_bymonth=fltarr(36,higem_xbylr_nyears)
FOR i=1,higem_xbylr_nyears-2 DO BEGIN
   higem_xbylr_nino3_ts_bymonth(0:11,i)=higem_xbylr_nino3_ts((i-1)*12:i*12-1)
   higem_xbylr_nino3_ts_bymonth(12:23,i)=higem_xbylr_nino3_ts(i*12:(i+1)*12-1)
   higem_xbylr_nino3_ts_bymonth(24:35,i)=higem_xbylr_nino3_ts((i+1)*12:(i+2)*12-1)
   higem_xbylr_nino4_ts_bymonth(0:11,i)=higem_xbylr_nino4_ts((i-1)*12:i*12-1)
   higem_xbylr_nino4_ts_bymonth(12:23,i)=higem_xbylr_nino4_ts(i*12:(i+1)*12-1)
   higem_xbylr_nino4_ts_bymonth(24:35,i)=higem_xbylr_nino4_ts((i+1)*12:(i+2)*12-1)
   higem_xbylr_nino34_ts_bymonth(0:11,i)=higem_xbylr_nino34_ts((i-1)*12:i*12-1)
   higem_xbylr_nino34_ts_bymonth(12:23,i)=higem_xbylr_nino34_ts(i*12:(i+1)*12-1)
   higem_xbylr_nino34_ts_bymonth(24:35,i)=higem_xbylr_nino34_ts((i+1)*12:(i+2)*12-1)
ENDFOR

; Get NINO timeseries from HiGEM eafeb
higem_eafeb_nino3_ts=OPEN_AND_EXTRACT(higem_eafeb_nino_infile,'nino3')
higem_eafeb_nino4_ts=OPEN_AND_EXTRACT(higem_eafeb_nino_infile,'nino4')
higem_eafeb_nino34_ts=OPEN_AND_EXTRACT(higem_eafeb_nino_infile,'nino34')
higem_eafeb_nino3_ts_bymonth=fltarr(36,higem_eafeb_nyears)
higem_eafeb_nino4_ts_bymonth=fltarr(36,higem_eafeb_nyears)
higem_eafeb_nino34_ts_bymonth=fltarr(36,higem_eafeb_nyears)
FOR i=1,higem_eafeb_nyears-2 DO BEGIN
   higem_eafeb_nino3_ts_bymonth(0:11,i)=higem_eafeb_nino3_ts((i-1)*12:i*12-1)
   higem_eafeb_nino3_ts_bymonth(12:23,i)=higem_eafeb_nino3_ts(i*12:(i+1)*12-1)
   higem_eafeb_nino3_ts_bymonth(24:35,i)=higem_eafeb_nino3_ts((i+1)*12:(i+2)*12-1)
   higem_eafeb_nino4_ts_bymonth(0:11,i)=higem_eafeb_nino4_ts((i-1)*12:i*12-1)
   higem_eafeb_nino4_ts_bymonth(12:23,i)=higem_eafeb_nino4_ts(i*12:(i+1)*12-1)
   higem_eafeb_nino4_ts_bymonth(24:35,i)=higem_eafeb_nino4_ts((i+1)*12:(i+2)*12-1)
   higem_eafeb_nino34_ts_bymonth(0:11,i)=higem_eafeb_nino34_ts((i-1)*12:i*12-1)
   higem_eafeb_nino34_ts_bymonth(12:23,i)=higem_eafeb_nino34_ts(i*12:(i+1)*12-1)
   higem_eafeb_nino34_ts_bymonth(24:35,i)=higem_eafeb_nino34_ts((i+1)*12:(i+2)*12-1)
ENDFOR

; Correlation arrays - from month minus 1 year to month plus 1 year
; Extra month in first dimension to provide wrap-around for plot
higem_xbylr_corr_nino3=fltarr(13,25)
higem_eafeb_corr_nino3=fltarr(13,25)
silo_n144_corr_nino3=fltarr(13,25)
higem_xbylr_corr_nino4=fltarr(13,25)
higem_eafeb_corr_nino4=fltarr(13,25)
silo_n144_corr_nino4=fltarr(13,25)
higem_xbylr_corr_nino34=fltarr(13,25)
higem_eafeb_corr_nino34=fltarr(13,25)
silo_n144_corr_nino34=fltarr(13,25)

FOR i=0,n_months-1 DO BEGIN
   higem_xbylr_infile=higem_xbylr_indir+'/higem_xbylr.'+months(i)+'_mmeans.h9-t5.precip.aus_domain.nc'
   higem_eafeb_infile=higem_eafeb_indir+'/higem_eafeb.'+months(i)+'_mmeans.h9-w8.precip.aus_domain.nc'
   silo_n144_infile=silo_n144_indir+'/SILO.'+months(i)+'_mmeans.1900-2008.n144.nc'
   IF i eq 0 THEN BEGIN
      higem_latitude=OPEN_AND_EXTRACT(higem_xbylr_infile,'latitude')
      higem_longitude=OPEN_AND_EXTRACT(higem_xbylr_infile,'longitude')
      DEFINE_BOUNDARIES,box_aavg,higem_latitude,higem_longitude,higem_box_tx,/LIMIT
      higem_nlon=N_ELEMENTS(higem_longitude)
      higem_nlat=N_ELEMENTS(higem_latitude)
      higem_aavg_weights=fltarr(higem_nlat)
      FOR j=0,higem_nlat-1 DO $
         higem_aavg_weights(j)=COS(3.14159*higem_latitude(j)/180.)
      higem_aavg_weights=higem_aavg_weights/TOTAL(higem_aavg_weights)

      silo_latitude=OPEN_AND_EXTRACT(silo_n144_infile,'latitude')
      silo_longitude=OPEN_AND_EXTRACT(silo_n144_infile,'longitude')
      DEFINE_BOUNDARIES,box_aavg,silo_latitude,silo_longitude,silo_box_tx,/LIMIT
      silo_nlon=N_ELEMENTS(silo_longitude)
      silo_nlat=N_ELEMENTS(silo_latitude)
      silo_aavg_weights=fltarr(silo_nlat)
      FOR j=0,silo_nlat-1 DO $
         silo_aavg_weights(j)=COS(3.14159*silo_latitude(j)/180.)
      silo_aavg_weights=silo_aavg_weights/TOTAL(silo_aavg_weights)

      mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
      mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
      DEFINE_BOUNDARIES,box_aavg,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
      mask_nlon=N_ELEMENTS(mask_longitude)
      mask_nlat=N_ELEMENTS(mask_latitude)
      mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                                   offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                                   count=[mask_nlon,mask_nlat,1,1]))
      mask_latrev=fltarr(mask_nlon,mask_nlat)
      FOR j=0,mask_nlat-1 DO $
         mask_latrev(*,j)=mask(*,mask_nlat-j-1)
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
   

   ; Create timeseries of area-averaged rainfall
   higem_xbylr_aavg_ts=fltarr(higem_xbylr_nyears)
   higem_eafeb_aavg_ts=fltarr(higem_eafeb_nyears)
   silo_n144_aavg_ts=fltarr(silo_nyears)   
   FOR j=0,higem_xbylr_nyears-1 DO BEGIN
      temp=REFORM(higem_xbylr_mmean_precip(*,*,j))
      temp[where(mask eq 0)]=!Values.F_NaN
      FOR k=0,higem_nlon-1 DO BEGIN
         temp2=REFORM(temp(k,*))
         higem_xbylr_aavg_ts(j)=higem_xbylr_aavg_ts(j)+TOTAL(temp2*higem_aavg_weights,/NaN)*1./FLOAT(higem_nlon)
      ENDFOR
   ENDFOR
   FOR j=0,higem_eafeb_nyears-1 DO BEGIN
      temp=REFORM(higem_eafeb_mmean_precip(*,*,j))
      temp[where(mask eq 0)]=!Values.F_NaN
      FOR k=0,higem_nlon-1 DO BEGIN
         temp2=REFORM(temp(k,*))
         higem_eafeb_aavg_ts(j)=higem_eafeb_aavg_ts(j)+TOTAL(temp2*higem_aavg_weights,/NaN)*1./FLOAT(higem_nlon)
      ENDFOR
   ENDFOR
   FOR j=0,silo_nyears-1 DO BEGIN
      temp_silo=REFORM(silo_n144_mmean_precip(*,*,j))
      temp_silo[where(temp_silo gt 1000)]=!Values.F_NaN
      temp_silo[where(mask_latrev eq 0)]=!Values.F_NaN
      FOR k=0,silo_nlon-1 DO BEGIN
         temp2=REFORM(temp_silo(k,*))
         silo_n144_aavg_ts(j)=silo_n144_aavg_ts(j)+TOTAL(temp2*silo_aavg_weights,/NaN)*1./FLOAT(silo_nlon)
      ENDFOR
   ENDFOR
   
   FOR j=0,24 DO BEGIN
      higem_xbylr_corr_nino3(i+1,j)=CORRELATE(higem_xbylr_nino3_ts_bymonth(i+j,1:higem_xbylr_nyears-2),higem_xbylr_aavg_ts(1:higem_xbylr_nyears-2))
      higem_eafeb_corr_nino3(i+1,j)=CORRELATE(higem_eafeb_nino3_ts_bymonth(i+j,1:higem_eafeb_nyears-2),higem_eafeb_aavg_ts(1:higem_eafeb_nyears-2))
      silo_n144_corr_nino3(i+1,j)=CORRELATE(obs_nino3_ts_bymonth(i+j,1:silo_nyears-2),silo_n144_aavg_ts(1:silo_nyears-2))
      higem_xbylr_corr_nino4(i+1,j)=CORRELATE(higem_xbylr_nino4_ts_bymonth(i+j,1:higem_xbylr_nyears-2),higem_xbylr_aavg_ts(1:higem_xbylr_nyears-2))
      higem_eafeb_corr_nino4(i+1,j)=CORRELATE(higem_eafeb_nino4_ts_bymonth(i+j,1:higem_eafeb_nyears-2),higem_eafeb_aavg_ts(1:higem_eafeb_nyears-2))
      silo_n144_corr_nino4(i+1,j)=CORRELATE(obs_nino4_ts_bymonth(i+j,1:silo_nyears-2),silo_n144_aavg_ts(1:silo_nyears-2))
      higem_xbylr_corr_nino34(i+1,j)=CORRELATE(higem_xbylr_nino34_ts_bymonth(i+j,1:higem_xbylr_nyears-2),higem_xbylr_aavg_ts(1:higem_xbylr_nyears-2))
      higem_eafeb_corr_nino34(i+1,j)=CORRELATE(higem_eafeb_nino34_ts_bymonth(i+j,1:higem_eafeb_nyears-2),higem_eafeb_aavg_ts(1:higem_eafeb_nyears-2))
      silo_n144_corr_nino34(i+1,j)=CORRELATE(obs_nino34_ts_bymonth(i+j,1:silo_nyears-2),silo_n144_aavg_ts(1:silo_nyears-2))
   ENDFOR
ENDFOR

higem_xbylr_corr_nino3(0,*)=higem_xbylr_corr_nino3(12,*)
higem_eafeb_corr_nino3(0,*)=higem_eafeb_corr_nino3(12,*)
silo_n144_corr_nino3(0,*)=silo_n144_corr_nino3(12,*)
higem_xbylr_corr_nino4(0,*)=higem_xbylr_corr_nino4(12,*)
higem_eafeb_corr_nino4(0,*)=higem_eafeb_corr_nino4(12,*)
silo_n144_corr_nino4(0,*)=silo_n144_corr_nino4(12,*)
higem_xbylr_corr_nino34(0,*)=higem_xbylr_corr_nino34(12,*)
higem_eafeb_corr_nino34(0,*)=higem_eafeb_corr_nino34(12,*)
silo_n144_corr_nino34(0,*)=silo_n144_corr_nino34(12,*)

xlabels=['Dec','Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
ylabels=['-12 months','-10 months','-8 months','-6 months','-4 months','-2 months','Coincident',$
         '+2 months','+4 months','+6 months','+8 months','+10 months','+12 months']

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo_aavg.'+region_name+'_region_nino3.silo_n144.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1000,SPACE2=800,XOFFSET=3600,YOFFSET=600,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT,SPACE3=500
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[11,12]
GSET,XMIN=-1,XMAX=11,YMIN=-12,YMAX=12
LEVS,MANUAL=mylevs
CON,FIELD=silo_n144_corr_nino3,X=indgen(13)-1,Y=indgen(25)-12,$
    TITLE='Corr aavg SILO precip (10-30S, 138-155E) N144 vs NINO3 (1901-2007)',CB_NTH=2,/NOLINES,$
    CB_TITLE='Correlation coefficient'
AXES,XSTEP=1,YSTEP=2,XTITLE='Month for rainfall',YTITLE='Lead-lag for NINO3 (negative means NINO3 leads)',$
     yvals=indgen(13)*2-12,xvals=indgen(13)-1,ylabels=ylabels,xlabels=xlabels
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo_aavg.'+region_name+'_region_nino4.silo_n144.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1000,SPACE2=800,XOFFSET=3600,YOFFSET=600,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT,SPACE3=500
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[11,12]
GSET,XMIN=-1,XMAX=11,YMIN=-12,YMAX=12
LEVS,MANUAL=mylevs
CON,FIELD=silo_n144_corr_nino4,X=indgen(13)-1,Y=indgen(25)-12,$
    TITLE='Corr aavg SILO precip (10-30S, 138-155E) N144 vs NINO4 (1901-2007)',CB_NTH=2,/NOLINES,$
    CB_TITLE='Correlation coefficient'
AXES,XSTEP=1,YSTEP=2,XTITLE='Month for rainfall',YTITLE='Lead-lag for NINO4 (negative means NINO4 leads)',$
     yvals=indgen(13)*2-12,xvals=indgen(13)-1,ylabels=ylabels,xlabels=xlabels
PSCLOSE

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo_aavg.'+region_name+'_region_nino34.silo_n144.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1000,SPACE2=800,XOFFSET=3600,YOFFSET=600,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT,SPACE3=500
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[11,12]
GSET,XMIN=-1,XMAX=11,YMIN=-12,YMAX=12
LEVS,MANUAL=mylevs
CON,FIELD=silo_n144_corr_nino34,X=indgen(13)-1,Y=indgen(25)-12,$
    TITLE='Corr aavg SILO precip (10-30S, 138-155E) N144 vs NINO3.4 (1901-2007)',CB_NTH=2,/NOLINES,$
    CB_TITLE='Correlation coefficient'
AXES,XSTEP=1,YSTEP=2,XTITLE='Month for rainfall',YTITLE='Lead-lag for NINO3.4 (negative means NINO3.4 leads)',$
     yvals=indgen(13)*2-12,xvals=indgen(13)-1,ylabels=ylabels,xlabels=xlabels
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo_aavg.'+region_name+'_region_nino3.higem_xbylr.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1000,SPACE2=800,XOFFSET=3600,YOFFSET=600,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT,SPACE3=500
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[11,12]
GSET,XMIN=-1,XMAX=11,YMIN=-12,YMAX=12
LEVS,MANUAL=mylevs
CON,FIELD=higem_xbylr_corr_nino3,X=indgen(13)-1,Y=indgen(25)-12,$
    TITLE='Corr aavg HiGEM xbylr precip (10-30S, 138-155E) vs NINO3 (117 years)',CB_NTH=2,/NOLINES,$
    CB_TITLE='Correlation coefficient'
AXES,XSTEP=1,YSTEP=2,XTITLE='Month for rainfall',YTITLE='Lead-lag for NINO3 (negative means NINO3 leads)',$
     yvals=indgen(13)*2-12,xvals=indgen(13)-1,ylabels=ylabels,xlabels=xlabels
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo_aavg.'+region_name+'_region_nino4.higem_xbylr.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1000,SPACE2=800,XOFFSET=3600,YOFFSET=600,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT,SPACE3=500
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[11,12]
GSET,XMIN=-1,XMAX=11,YMIN=-12,YMAX=12
LEVS,MANUAL=mylevs
CON,FIELD=higem_xbylr_corr_nino4,X=indgen(13)-1,Y=indgen(25)-12,$
    TITLE='Corr aavg HiGEM xbylr precip (10-30S, 138-155E) vs NINO4 (117 years)',CB_NTH=2,/NOLINES,$
    CB_TITLE='Correlation coefficient'
AXES,XSTEP=1,YSTEP=2,XTITLE='Month for rainfall',YTITLE='Lead-lag for NINO4 (negative means NINO4 leads)',$
     yvals=indgen(13)*2-12,xvals=indgen(13)-1,ylabels=ylabels,xlabels=xlabels
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo_aavg.'+region_name+'_region_nino34.higem_xbylr.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1000,SPACE2=800,XOFFSET=3600,YOFFSET=600,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT,SPACE3=500
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[11,12]
GSET,XMIN=-1,XMAX=11,YMIN=-12,YMAX=12
LEVS,MANUAL=mylevs
CON,FIELD=higem_xbylr_corr_nino34,X=indgen(13)-1,Y=indgen(25)-12,$
    TITLE='Corr aavg HiGEM xbylr precip (10-30S, 138-155E) vs NINO3.4 (117 years)',CB_NTH=2,/NOLINES,$
    CB_TITLE='Correlation coefficient'
AXES,XSTEP=1,YSTEP=2,XTITLE='Month for rainfall',YTITLE='Lead-lag for NINO3.4 (negative means NINO3.4 leads)',$
     yvals=indgen(13)*2-12,xvals=indgen(13)-1,ylabels=ylabels,xlabels=xlabels
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo_aavg.'+region_name+'_region_nino3.higem_eafeb.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1000,SPACE2=800,XOFFSET=3600,YOFFSET=600,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT,SPACE3=500
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[11,12]
GSET,XMIN=-1,XMAX=11,YMIN=-12,YMAX=12
LEVS,MANUAL=mylevs
CON,FIELD=higem_eafeb_corr_nino3,X=indgen(13)-1,Y=indgen(25)-12,$
    TITLE='Corr aavg HiGEM eafeb precip (10-30S, 138-155E) vs NINO3 (147 years)',CB_NTH=2,/NOLINES,$
    CB_TITLE='Correlation coefficient'
AXES,XSTEP=1,YSTEP=2,XTITLE='Month for rainfall',YTITLE='Lead-lag for NINO3 (negative means NINO3 leads)',$
     yvals=indgen(13)*2-12,xvals=indgen(13)-1,ylabels=ylabels,xlabels=xlabels
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo_aavg.'+region_name+'_region_nino4.higem_eafeb.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1000,SPACE2=800,XOFFSET=3600,YOFFSET=600,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT,SPACE3=500
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[11,12]
GSET,XMIN=-1,XMAX=11,YMIN=-12,YMAX=12
LEVS,MANUAL=mylevs
CON,FIELD=higem_eafeb_corr_nino4,X=indgen(13)-1,Y=indgen(25)-12,$
    TITLE='Corr aavg HiGEM eafeb precip (10-30S, 138-155E) vs NINO4 (147 years)',CB_NTH=2,/NOLINES,$
    CB_TITLE='Correlation coefficient'
AXES,XSTEP=1,YSTEP=2,XTITLE='Month for rainfall',YTITLE='Lead-lag for NINO4 (negative means NINO4 leads)',$
     yvals=indgen(13)*2-12,xvals=indgen(13)-1,ylabels=ylabels,xlabels=xlabels
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo_aavg.'+region_name+'_region_nino34.higem_eafeb.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1000,SPACE2=800,XOFFSET=3600,YOFFSET=600,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT,SPACE3=500
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[11,12]
GSET,XMIN=-1,XMAX=11,YMIN=-12,YMAX=12
LEVS,MANUAL=mylevs
CON,FIELD=higem_eafeb_corr_nino34,X=indgen(13)-1,Y=indgen(25)-12,$
    TITLE='Corr aavg HiGEM eafeb precip (10-30S, 138-155E) vs NINO3.4 (147 years)',CB_NTH=2,/NOLINES,$
    CB_TITLE='Correlation coefficient'
AXES,XSTEP=1,YSTEP=2,XTITLE='Month for rainfall',YTITLE='Lead-lag for NINO3.4 (negative means NINO3.4 leads)',$
     yvals=indgen(13)*2-12,xvals=indgen(13)-1,ylabels=ylabels,xlabels=xlabels
PSCLOSE,/NOVIEW


STOP

END
