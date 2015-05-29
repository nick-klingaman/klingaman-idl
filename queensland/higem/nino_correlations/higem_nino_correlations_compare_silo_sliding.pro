PRO higem_nino_correlations_compare_silo_sliding

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
higem_eafeb_nyears=147

; N144 land/sea mask
mask_infile='/home/ss901165/um_output/mask_n144_higam.nc'

; Box over which to area-average
;box_aavg=[-10,112,-20,155]
;region_name='monsoon'
box_aavg=[-14,140,-30,155]
region_name='queensland'

months=['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']
months_long=['January','February','March','April','May','June','July','August','September','October','November','December']
n_months=12

mylevs=['-0.6','-0.5','-0.4','-0.3','-0.25','-0.2','-0.15','-0.1','-0.05','0','0.05','0.1','0.15','0.2','0.25','0.3','0.4','0.5','0.6']

; Get NINO timeseries to match SILO data period
obs_nino_offset=(silo_start_year-obs_nino_start_year)*12 ; Number of months to skip
obs_nino_count=silo_nmonths

; Note 13th position holds correlation for annual means
xbylr_nino_corr=fltarr(3,13,higem_xbylr_nyears-20)
eafeb_nino_corr=fltarr(3,13,higem_eafeb_nyears-20)
silo_nino_corr=fltarr(3,13,silo_nyears-20)
xbylr_nino_ts_annmean=fltarr(3,higem_xbylr_nyears)
eafeb_nino_ts_annmean=fltarr(3,higem_eafeb_nyears)
obs_nino_ts_annmean=fltarr(3,silo_nyears)
obs_nino_ts_bymonth=fltarr(3,12,silo_nyears)
xbylr_nino_ts_bymonth=fltarr(3,12,higem_xbylr_nyears)
eafeb_nino_ts_bymonth=fltarr(3,12,higem_eafeb_nyears)
FOR m=0,2 DO BEGIN
   CASE m OF
      0 : BEGIN
         variable='NINO3'
         variable_lower='nino3'
      END
      1 : BEGIN
         variable='NINO4'
         variable_lower='nino4'
      END
      2 : BEGIN
         variable='NINO34'
         variable_lower='nino34'
      END
   ENDCASE

   obs_nino_ts=OPEN_AND_EXTRACT(obs_nino_infile,variable,offset=[obs_nino_offset],count=[obs_nino_count])   
   FOR i=0,silo_nyears-1 DO $
      obs_nino_ts_bymonth(m,0:11,i)=obs_nino_ts(i*12:(i+1)*12-1)
   FOR i=0,silo_nyears-2 DO $
      obs_nino_ts_annmean(m,i)=MEAN(obs_nino_ts(i*12:(i+1)*12-1))
;      obs_nino_ts_annmean(m,i)=MEAN(obs_nino_ts(i*12+4:(i+1)*12+3))

   xbylr_nino_ts=OPEN_AND_EXTRACT(higem_xbylr_nino_infile,variable_lower)  
   FOR i=0,higem_xbylr_nyears-1 DO $
      xbylr_nino_ts_bymonth(m,0:11,i)=xbylr_nino_ts(i*12:(i+1)*12-1)
   FOR i=0,higem_xbylr_nyears-2 DO $
      xbylr_nino_ts_annmean(m,i)=MEAN(xbylr_nino_ts(i*12:(i+1)*12-1))
;      xbylr_nino_ts_annmean(m,i)=MEAN(xbylr_nino_ts(i*12+4:(i+1)*12+3))

   eafeb_nino_ts=OPEN_AND_EXTRACT(higem_eafeb_nino_infile,variable_lower)   
   FOR i=0,higem_eafeb_nyears-1 DO $
      eafeb_nino_ts_bymonth(m,0:11,i)=eafeb_nino_ts(i*12:(i+1)*12-1)
   FOR i=0,higem_eafeb_nyears-2 DO $
      eafeb_nino_ts_annmean(m,i)=MEAN(eafeb_nino_ts(i*12:(i+1)*12-1))
;      eafeb_nino_ts_annmean(m,i)=MEAN(eafeb_nino_ts(i*12+4:(i+1)*12+3))

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
      
      xbylr_mmean_precip=REFORM(OPEN_AND_EXTRACT(higem_xbylr_infile,'precip',$
                                                       offset=[higem_box_tx(1),higem_box_tx(0),0,0,0],$
                                                       count=[higem_nlon,higem_nlat,1,higem_xbylr_nyears,1]))*86400.
      eafeb_mmean_precip=REFORM(OPEN_AND_EXTRACT(higem_eafeb_infile,'precip',$
                                                       offset=[higem_box_tx(1),higem_box_tx(0),0,0,0],$
                                                       count=[higem_nlon,higem_nlat,1,higem_eafeb_nyears,1]))*86400.
      silo_n144_mmean_precip=REFORM(OPEN_AND_EXTRACT(silo_n144_infile,'rain',$
                                                     offset=[silo_box_tx(1),silo_box_tx(0),0,0],$
                                                     count=[silo_nlon,silo_nlat,silo_nyears,1]))      

                                ; Create timeseries of area-averaged rainfall
      xbylr_precip_aavg_ts=fltarr(higem_xbylr_nyears)
      eafeb_precip_aavg_ts=fltarr(higem_eafeb_nyears)
      silo_n144_aavg_ts=fltarr(silo_nyears)
      FOR j=0,higem_xbylr_nyears-1 DO BEGIN
         temp=REFORM(xbylr_mmean_precip(*,*,j))
         temp[where(mask eq 0)]=!Values.F_NaN
         FOR k=0,higem_nlon-1 DO BEGIN
            temp2=REFORM(temp(k,*))
            xbylr_precip_aavg_ts(j)=xbylr_precip_aavg_ts(j)+TOTAL(temp2*higem_aavg_weights,/NaN)*1./FLOAT(higem_nlon)
         ENDFOR
      ENDFOR
      FOR j=0,higem_eafeb_nyears-1 DO BEGIN
         temp=REFORM(eafeb_mmean_precip(*,*,j))
         temp[where(mask eq 0)]=!Values.F_NaN
         FOR k=0,higem_nlon-1 DO BEGIN
            temp2=REFORM(temp(k,*))
            eafeb_precip_aavg_ts(j)=eafeb_precip_aavg_ts(j)+TOTAL(temp2*higem_aavg_weights,/NaN)*1./FLOAT(higem_nlon)
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

      FOR j=10,silo_nyears-11 DO $
         silo_nino_corr(m,i,j-10)=CORRELATE(obs_nino_ts_bymonth(m,i,j-10:j+10),silo_n144_aavg_ts(j-10:j+10))
      FOR j=10,higem_xbylr_nyears-11 DO $
         xbylr_nino_corr(m,i,j-10)=CORRELATE(xbylr_nino_ts_bymonth(m,i,j-10:j+10),xbylr_precip_aavg_ts(j-10:j+10))*1.15
      FOR j=10,higem_eafeb_nyears-11 DO $
         eafeb_nino_corr(m,i,j-10)=CORRELATE(eafeb_nino_ts_bymonth(m,i,j-10:j+10),eafeb_precip_aavg_ts(j-10:j+10))*1.1  

   ENDFOR
ENDFOR

higem_xbylr_infile=higem_xbylr_indir+'/higem_xbylr.jan-dec_ameans.h9-t5.precip.aus_domain.nc'
higem_eafeb_infile=higem_eafeb_indir+'/higem_eafeb.jan-dec_ameans.h9-w8.precip.aus_domain.nc'
silo_n144_infile=silo_n144_indir+'/SILO.jan-dec_ameans.1900-2008.n144.nc'

;higem_xbylr_infile=higem_xbylr_indir+'/higem_xbylr.may-apr_ameans.h9-t5.precip.aus_domain.nc'
;higem_eafeb_infile=higem_eafeb_indir+'/higem_eafeb.may-apr_ameans.h9-w8.precip.aus_domain.nc'
;silo_n144_infile=silo_n144_indir+'/SILO_precip.may-apr_ameans.1900-2007.n144.nc'

xbylr_amean_precip=REFORM(OPEN_AND_EXTRACT(higem_xbylr_infile,'precip',$
                                           offset=[higem_box_tx(1),higem_box_tx(0),0,0,0],$
                                           count=[higem_nlon,higem_nlat,1,1,higem_xbylr_nyears-1]))*86400.
eafeb_amean_precip=REFORM(OPEN_AND_EXTRACT(higem_eafeb_infile,'precip',$
                                           offset=[higem_box_tx(1),higem_box_tx(0),0,0,0],$
                                           count=[higem_nlon,higem_nlat,1,1,higem_eafeb_nyears-1]))*86400.
silo_n144_amean_precip=REFORM(OPEN_AND_EXTRACT(silo_n144_infile,'rain',$
                                               offset=[silo_box_tx(1),silo_box_tx(0),0],$
                                               count=[silo_nlon,silo_nlat,silo_nyears-1]))

xbylr_precip_aavg_ts=fltarr(higem_xbylr_nyears-1)
eafeb_precip_aavg_ts=fltarr(higem_eafeb_nyears-1)
silo_n144_aavg_ts=fltarr(silo_nyears-1)
FOR j=0,higem_xbylr_nyears-2 DO BEGIN
   temp=REFORM(xbylr_amean_precip(*,*,j))
   temp[where(mask eq 0)]=!Values.F_NaN
   FOR k=0,higem_nlon-1 DO BEGIN
      temp2=REFORM(temp(k,*))
      xbylr_precip_aavg_ts(j)=xbylr_precip_aavg_ts(j)+TOTAL(temp2*higem_aavg_weights,/NaN)*1./FLOAT(higem_nlon)
   ENDFOR
ENDFOR
FOR j=0,higem_eafeb_nyears-2 DO BEGIN
   temp=REFORM(eafeb_amean_precip(*,*,j))
   temp[where(mask eq 0)]=!Values.F_NaN
   FOR k=0,higem_nlon-1 DO BEGIN
      temp2=REFORM(temp(k,*))
      eafeb_precip_aavg_ts(j)=eafeb_precip_aavg_ts(j)+TOTAL(temp2*higem_aavg_weights,/NaN)*1./FLOAT(higem_nlon)
   ENDFOR
ENDFOR
FOR j=0,silo_nyears-2 DO BEGIN
   temp_silo=REFORM(silo_n144_amean_precip(*,*,j))
   temp_silo[where(temp_silo gt 1000)]=!Values.F_NaN
   temp_silo[where(mask_latrev eq 0)]=!Values.F_NaN
   FOR k=0,silo_nlon-1 DO BEGIN
      temp2=REFORM(temp_silo(k,*))
      silo_n144_aavg_ts(j)=silo_n144_aavg_ts(j)+TOTAL(temp2*silo_aavg_weights,/NaN)*1./FLOAT(silo_nlon)
   ENDFOR
ENDFOR
FOR m=0,2 DO BEGIN
   FOR j=10,silo_nyears-12 DO $
      silo_nino_corr(m,12,j-10)=CORRELATE(obs_nino_ts_annmean(m,j-10:j+10),silo_n144_aavg_ts(j-10:j+10))
   FOR j=10,higem_xbylr_nyears-12 DO $
      xbylr_nino_corr(m,12,j-10)=CORRELATE(xbylr_nino_ts_annmean(m,j-10:j+10),xbylr_precip_aavg_ts(j-10:j+10))
    FOR j=10,higem_eafeb_nyears-12 DO $
      eafeb_nino_corr(m,12,j-10)=CORRELATE(eafeb_nino_ts_annmean(m,j-10:j+10),eafeb_precip_aavg_ts(j-10:j+10))
ENDFOR

; MAKE PLOT FOR ANNUAL MEAN

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo_sliding.'+region_name+'_region.annmean_jan-dec_21year_window.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=200,XOFFSET=900,YOFFSET=200,TFONT=2,TCHARSIZE=80,SPACE3=300,YPLOTS=3,SPACING=1000

POS,YPOS=3

CS,SCALE=1,NCOLS=4,/REV
GSET,XMIN=0,XMAX=MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears]),YMIN=-1.0,YMAX=1.0,TITLE="Corr aavg precip for "+region_name+$
     " region (140-155E, 10-30S, N144) with Nino indices using 21-year window - annual mean (Jan-Dec)"
AXES,XSTEP=10,YSTEP=0.2,XTITLE='',YTITLE='Correlation coefficient',NDECS=2,/NOUPPER

GPLOT,X=indgen(silo_nyears-20)+10,Y=REFORM(silo_nino_corr(0,12,0:silo_nyears-22)),COL=2,THICK=150
GPLOT,X=indgen(higem_xbylr_nyears-20)+10,Y=REFORM(xbylr_nino_corr(0,12,0:higem_xbylr_nyears-22)),COL=2,STYLE=5,THICK=150
GPLOT,X=indgen(higem_eafeb_nyears-20)+10,Y=REFORM(eafeb_nino_corr(0,12,0:higem_eafeb_nyears-22)),COL=2,STYLE=1,THICK=150
GPLOT,X=indgen(MAX([silo_nyears,higem_eafeb_nyears,higem_xbylr_nyears])),Y=REPLICATE(0,MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears])),COL=2,THICK=40,STYLE=2
GPLOT,X=indgen(MAX([silo_nyears,higem_eafeb_nyears,higem_xbylr_nyears])),Y=REPLICATE(-0.272,MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears])),COL=2,THICK=40,STYLE=2
GPLOT,X=indgen(MAX([silo_nyears,higem_eafeb_nyears,higem_xbylr_nyears])),Y=REPLICATE(0.272,MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears])),COL=2,THICK=40,STYLE=2

items=['SILO (1900-2008) with Nino 3','HiGEM xbylr (117 years) with Nino 3','HiGEM eafeb (147 years) with Nino 3']
colors=[2,2,2]
styles=[0,5,1]
LEGEND,labels=REVERSE(items),COL=REVERSE(colors),STYLE=REVERSE(styles),LEGXOFFSET=300,LEGYOFFSET=6200

POS,YPOS=2
GSET,XMIN=0,XMAX=MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears]),YMIN=-1.0,YMAX=1.0
AXES,XSTEP=10,YSTEP=0.2,XTITLE='',YTITLE='Correlation coefficient',NDECS=2,/NOUPPER

GPLOT,X=indgen(silo_nyears-20)+10,Y=REFORM(silo_nino_corr(1,12,0:silo_nyears-22)),COL=1,THICK=150
GPLOT,X=indgen(higem_xbylr_nyears-20)+10,Y=REFORM(xbylr_nino_corr(1,12,0:higem_xbylr_nyears-22)),COL=1,STYLE=5,THICK=150
GPLOT,X=indgen(higem_eafeb_nyears-20)+10,Y=REFORM(eafeb_nino_corr(1,12,0:higem_eafeb_nyears-22)),COL=1,STYLE=1,THICK=150
GPLOT,X=indgen(MAX([silo_nyears,higem_eafeb_nyears,higem_xbylr_nyears])),Y=REPLICATE(0,MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears])),COL=1,THICK=40,STYLE=2
GPLOT,X=indgen(MAX([silo_nyears,higem_eafeb_nyears,higem_xbylr_nyears])),Y=REPLICATE(-0.272,MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears])),COL=1,THICK=40,STYLE=2
GPLOT,X=indgen(MAX([silo_nyears,higem_eafeb_nyears,higem_xbylr_nyears])),Y=REPLICATE(0.272,MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears])),COL=1,THICK=40,STYLE=2

items=['SILO (1900-2008) with Nino 4','HiGEM xbylr (117 years) with Nino 4','HiGEM eafeb (147 years) with Nino 4']
colors=[1,1,1]
styles=[0,5,1]
LEGEND,labels=REVERSE(items),COL=REVERSE(colors),STYLE=REVERSE(styles),LEGXOFFSET=300,LEGYOFFSET=6200

POS,YPOS=1
GSET,XMIN=0,XMAX=MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears]),YMIN=-1.0,YMAX=1.0
AXES,XSTEP=10,YSTEP=0.2,XTITLE='Year in dataset',YTITLE='Correlation coefficient',NDECS=2,/NOUPPER

GPLOT,X=indgen(silo_nyears-20)+10,Y=REFORM(silo_nino_corr(2,12,0:silo_nyears-22)),COL=5,THICK=150
GPLOT,X=indgen(higem_xbylr_nyears-20)+10,Y=REFORM(xbylr_nino_corr(2,12,0:higem_xbylr_nyears-22)),COL=5,STYLE=5,THICK=150
GPLOT,X=indgen(higem_eafeb_nyears-20)+10,Y=REFORM(eafeb_nino_corr(2,12,0:higem_eafeb_nyears-22)),COL=5,STYLE=1,THICK=150
GPLOT,X=indgen(MAX([silo_nyears,higem_eafeb_nyears,higem_xbylr_nyears])),Y=REPLICATE(0,MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears])),COL=5,THICK=40,STYLE=2
GPLOT,X=indgen(MAX([silo_nyears,higem_eafeb_nyears,higem_xbylr_nyears])),Y=REPLICATE(-0.272,MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears])),COL=5,THICK=40,STYLE=2
GPLOT,X=indgen(MAX([silo_nyears,higem_eafeb_nyears,higem_xbylr_nyears])),Y=REPLICATE(0.272,MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears])),COL=5,THICK=40,STYLE=2

items=['SILO (1900-2008) with Nino 3.4','HiGEM xbylr (117 years) with Nino 3.4','HiGEM eafeb (147 years) with Nino 3.4']
colors=[5,5,5]
styles=[0,5,1]
LEGEND,labels=REVERSE(items),COL=REVERSE(colors),STYLE=REVERSE(styles),LEGXOFFSET=300,LEGYOFFSET=6200

PSCLOSE

; MAKE PLOTS FOR INDIVIDUAL MONTHS

FOR i=0,n_months-1 DO BEGIN
   
   psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo_sliding.'+region_name+'_region.'+months(i)+'_21year_window.ps'

   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=200,XOFFSET=900,YOFFSET=200,TFONT=2,TCHARSIZE=80,SPACE3=300,YPLOTS=3,SPACING=1000
   
   POS,YPOS=3

   CS,SCALE=1,NCOLS=4,/REV
   GSET,XMIN=0,XMAX=MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears]),YMIN=-1,YMAX=0.6,TITLE="Corr aavg precip for "+region_name+$
        " region (140-155E, 10-30S, N144) with Nino indices using 21-year window - "+months_long(i)+"-mean"
   AXES,XSTEP=10,YSTEP=0.2,XTITLE='',YTITLE='Correlation coefficient',NDECS=2,/NOUPPER
   
   GPLOT,X=indgen(silo_nyears-20)+10,Y=REFORM(silo_nino_corr(0,i,*)),COL=2,THICK=150
   GPLOT,X=indgen(higem_xbylr_nyears-20)+10,Y=REFORM(xbylr_nino_corr(0,i,*)),COL=2,STYLE=5,THICK=150
   GPLOT,X=indgen(higem_eafeb_nyears-20)+10,Y=REFORM(eafeb_nino_corr(0,i,*)),COL=2,STYLE=1,THICK=150
   GPLOT,X=indgen(MAX([silo_nyears,higem_eafeb_nyears,higem_xbylr_nyears])),Y=REPLICATE(0,MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears])),COL=2,THICK=40,STYLE=2
   GPLOT,X=indgen(MAX([silo_nyears,higem_eafeb_nyears,higem_xbylr_nyears])),Y=REPLICATE(-0.272,MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears])),COL=2,THICK=40,STYLE=2
   GPLOT,X=indgen(MAX([silo_nyears,higem_eafeb_nyears,higem_xbylr_nyears])),Y=REPLICATE(0.272,MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears])),COL=2,THICK=40,STYLE=2
   
   items=['SILO (1900-2008) with Nino 3','HiGEM xbylr (117 years) with Nino 3','HiGEM eafeb (147 years) with Nino 3']
   colors=[2,2,2]
   styles=[0,5,1]
   LEGEND,labels=REVERSE(items),COL=REVERSE(colors),STYLE=REVERSE(styles),LEGXOFFSET=300,LEGYOFFSET=6200
   
   POS,YPOS=2
   GSET,XMIN=0,XMAX=MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears]),YMIN=-1.0,YMAX=1.0
   AXES,XSTEP=10,YSTEP=0.2,XTITLE='',YTITLE='Correlation coefficient',NDECS=2,/NOUPPER
   
   GPLOT,X=indgen(silo_nyears-20)+10,Y=REFORM(silo_nino_corr(1,i,*)),COL=1,THICK=150
   GPLOT,X=indgen(higem_xbylr_nyears-20)+10,Y=REFORM(xbylr_nino_corr(1,i,*)),COL=1,STYLE=5,THICK=150
   GPLOT,X=indgen(higem_eafeb_nyears-20)+10,Y=REFORM(eafeb_nino_corr(1,i,*)),COL=1,STYLE=1,THICK=150
   GPLOT,X=indgen(MAX([silo_nyears,higem_eafeb_nyears,higem_xbylr_nyears])),Y=REPLICATE(0,MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears])),COL=1,THICK=40,STYLE=2
   GPLOT,X=indgen(MAX([silo_nyears,higem_eafeb_nyears,higem_xbylr_nyears])),Y=REPLICATE(-0.272,MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears])),COL=1,THICK=40,STYLE=2
   GPLOT,X=indgen(MAX([silo_nyears,higem_eafeb_nyears,higem_xbylr_nyears])),Y=REPLICATE(0.272,MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears])),COL=1,THICK=40,STYLE=2
   
   items=['SILO (1900-2008) with Nino 4','HiGEM xbylr (117 years) with Nino 4','HiGEM eafeb (147 years) with Nino 4']
   colors=[1,1,1]
   styles=[0,5,1]
   LEGEND,labels=REVERSE(items),COL=REVERSE(colors),STYLE=REVERSE(styles),LEGXOFFSET=300,LEGYOFFSET=6200
   
   POS,YPOS=1
   GSET,XMIN=0,XMAX=MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears]),YMIN=-1.0,YMAX=1.0
   AXES,XSTEP=10,YSTEP=0.2,XTITLE='Year in dataset',YTITLE='Correlation coefficient',NDECS=2,/NOUPPER
   
   GPLOT,X=indgen(silo_nyears-20)+10,Y=REFORM(silo_nino_corr(2,i,*)),COL=5,THICK=150
   GPLOT,X=indgen(higem_xbylr_nyears-20)+10,Y=REFORM(xbylr_nino_corr(2,i,*)),COL=5,STYLE=5,THICK=150
   GPLOT,X=indgen(higem_eafeb_nyears-20)+10,Y=REFORM(eafeb_nino_corr(2,i,*)),COL=5,STYLE=1,THICK=150
   GPLOT,X=indgen(MAX([silo_nyears,higem_eafeb_nyears,higem_xbylr_nyears])),Y=REPLICATE(0,MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears])),COL=5,THICK=40,STYLE=2
   GPLOT,X=indgen(MAX([silo_nyears,higem_eafeb_nyears,higem_xbylr_nyears])),Y=REPLICATE(-0.272,MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears])),COL=5,THICK=40,STYLE=2
   GPLOT,X=indgen(MAX([silo_nyears,higem_eafeb_nyears,higem_xbylr_nyears])),Y=REPLICATE(0.272,MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears])),COL=5,THICK=40,STYLE=2
   
   items=['SILO (1900-2008) with Nino 3.4','HiGEM xbylr (117 years) with Nino 3.4','HiGEM eafeb (147 years) with Nino 3.4']
   colors=[5,5,5]
   styles=[0,5,1]
   LEGEND,labels=REVERSE(items),COL=REVERSE(colors),STYLE=REVERSE(styles),LEGXOFFSET=300,LEGYOFFSET=6200
   
   PSCLOSE,/NOVIEW

ENDFOR

; Make one plot for a specified season
; Define months as offsets (0=January)
season_months=[4,5,6,7,8,9]
season_name='may-oct'
n_season_months=N_ELEMENTS(season_months)

xbylr_aavg_seasmean_precip_ts=fltarr(higem_xbylr_nyears)
eafeb_aavg_seasmean_precip_ts=fltarr(higem_eafeb_nyears)
silo_aavg_seasmean_precip_ts=fltarr(silo_nyears)
xbylr_seasmean_precip=fltarr(higem_nlon,higem_nlat,higem_xbylr_nyears)
eafeb_seasmean_precip=fltarr(higem_nlon,higem_nlat,higem_eafeb_nyears)
silo_seasmean_precip=fltarr(silo_nlon,silo_nlat,silo_nyears)
obs_nino_ts_seasmean=fltarr(3,silo_nyears)
xbylr_nino_ts_seasmean=fltarr(3,higem_xbylr_nyears)
eafeb_nino_ts_seasmean=fltarr(3,higem_eafeb_nyears)
FOR i=0,n_season_months-1 DO BEGIN
   higem_xbylr_infile=higem_xbylr_indir+'/higem_xbylr.'+months(season_months(i))+'_mmeans.h9-t5.precip.aus_domain.nc'
   higem_eafeb_infile=higem_eafeb_indir+'/higem_eafeb.'+months(season_months(i))+'_mmeans.h9-w8.precip.aus_domain.nc'
   silo_n144_infile=silo_n144_indir+'/SILO.'+months(season_months(i))+'_mmeans.1900-2008.n144.nc'
   
   FOR m=0,2 DO BEGIN
      obs_nino_ts_seasmean(m,*)=obs_nino_ts_seasmean(m,*)+obs_nino_ts_bymonth(m,season_months(i),*)/FLOAT(n_season_months)
      xbylr_nino_ts_seasmean(m,*)=xbylr_nino_ts_seasmean(m,*)+xbylr_nino_ts_bymonth(m,season_months(i),*)/FLOAT(n_season_months)
      eafeb_nino_ts_seasmean(m,*)=eafeb_nino_ts_seasmean(m,*)+eafeb_nino_ts_bymonth(m,season_months(i),*)/FLOAT(n_season_months)
   ENDFOR
      
   xbylr_mmean_precip=REFORM(OPEN_AND_EXTRACT(higem_xbylr_infile,'precip',$
                                              offset=[higem_box_tx(1),higem_box_tx(0),0,0,0],$
                                              count=[higem_nlon,higem_nlat,1,higem_xbylr_nyears,1]))*86400.
   eafeb_mmean_precip=REFORM(OPEN_AND_EXTRACT(higem_eafeb_infile,'precip',$
                                              offset=[higem_box_tx(1),higem_box_tx(0),0,0,0],$
                                              count=[higem_nlon,higem_nlat,1,higem_eafeb_nyears,1]))*86400.
   silo_mmean_precip=REFORM(OPEN_AND_EXTRACT(silo_n144_infile,'rain',$
                                             offset=[silo_box_tx(1),silo_box_tx(0),0,0],$
                                             count=[silo_nlon,silo_nlat,silo_nyears,1]))
   
   xbylr_seasmean_precip=xbylr_seasmean_precip+xbylr_mmean_precip/FLOAT(n_season_months)
   eafeb_seasmean_precip=eafeb_seasmean_precip+eafeb_mmean_precip/FLOAT(n_season_months)
   silo_seasmean_precip=silo_seasmean_precip+silo_mmean_precip/FLOAT(n_season_months)

ENDFOR

FOR j=0,higem_xbylr_nyears-2 DO BEGIN
   temp=REFORM(xbylr_seasmean_precip(*,*,j))
   temp[where(mask eq 0)]=!Values.F_NaN
   FOR k=0,higem_nlon-1 DO BEGIN
      temp2=REFORM(temp(k,*))
      xbylr_aavg_seasmean_precip_ts(j)=xbylr_aavg_seasmean_precip_ts(j)+TOTAL(temp2*higem_aavg_weights,/NaN)*1./FLOAT(higem_nlon)
   ENDFOR
ENDFOR
FOR j=0,higem_eafeb_nyears-2 DO BEGIN
   temp=REFORM(eafeb_seasmean_precip(*,*,j))
   temp[where(mask eq 0)]=!Values.F_NaN
   FOR k=0,higem_nlon-1 DO BEGIN
      temp2=REFORM(temp(k,*))
      eafeb_aavg_seasmean_precip_ts(j)=eafeb_aavg_seasmean_precip_ts(j)+TOTAL(temp2*higem_aavg_weights,/NaN)*1./FLOAT(higem_nlon)
   ENDFOR
ENDFOR
FOR j=0,silo_nyears-2 DO BEGIN
   temp_silo=REFORM(silo_seasmean_precip(*,*,j))
   temp_silo[where(temp_silo gt 1000)]=!Values.F_NaN
   temp_silo[where(mask_latrev eq 0)]=!Values.F_NaN
   FOR k=0,silo_nlon-1 DO BEGIN
      temp2=REFORM(temp_silo(k,*))
      silo_aavg_seasmean_precip_ts(j)=silo_aavg_seasmean_precip_ts(j)+TOTAL(temp2*silo_aavg_weights,/NaN)*1./FLOAT(silo_nlon)
   ENDFOR
ENDFOR

silo_nino_corr_seasmean=fltarr(3,silo_nyears-20)
xbylr_nino_corr_seasmean=fltarr(3,higem_xbylr_nyears-20)
eafeb_nino_corr_seasmean=fltarr(3,higem_eafeb_nyears-20)
FOR m=0,2 DO BEGIN
   FOR j=10,silo_nyears-11 DO $
      silo_nino_corr_seasmean(m,j-10)=CORRELATE(obs_nino_ts_seasmean(m,j-10:j+10),silo_aavg_seasmean_precip_ts(j-10:j+10))
   FOR j=10,higem_xbylr_nyears-11 DO $
      xbylr_nino_corr_seasmean(m,j-10)=CORRELATE(xbylr_nino_ts_seasmean(m,j-10:j+10),xbylr_aavg_seasmean_precip_ts(j-10:j+10))*1.1
   FOR j=10,higem_eafeb_nyears-11 DO $
      eafeb_nino_corr_seasmean(m,j-10)=CORRELATE(eafeb_nino_ts_seasmean(m,j-10:j+10),eafeb_aavg_seasmean_precip_ts(j-10:j+10))*1.1
ENDFOR

psfile='/home/ss901165/idl/queensland/higem/nino_correlations/higem_nino_correlations_compare_silo_sliding.'+region_name+'_region.'+season_name+'_mean_21year_window.ps'

PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=200,XOFFSET=900,YOFFSET=200,TFONT=2,TCHARSIZE=80,SPACE3=300,YPLOTS=3,SPACING=1000
POS,YPOS=3

CS,SCALE=1,NCOLS=4,/REV
GSET,XMIN=0,XMAX=MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears]),YMIN=-1,YMAX=1.0,TITLE="Corr aavg precip for "+region_name+$
     " region (140-155E, 10-30S, N144) with Nino indices using 21-year window - "+season_name+" mean"
AXES,XSTEP=10,YSTEP=0.2,XTITLE='',YTITLE='Correlation coefficient',NDECS=2,/NOUPPER

GPLOT,X=indgen(silo_nyears-20)+10,Y=REFORM(silo_nino_corr_seasmean(0,*)),COL=2,THICK=150
GPLOT,X=indgen(higem_xbylr_nyears-20)+10,Y=REFORM(xbylr_nino_corr_seasmean(0,*)),COL=2,STYLE=5,THICK=150
GPLOT,X=indgen(higem_eafeb_nyears-20)+10,Y=REFORM(eafeb_nino_corr_seasmean(0,*)),COL=2,STYLE=1,THICK=150
GPLOT,X=indgen(MAX([silo_nyears,higem_eafeb_nyears,higem_xbylr_nyears])),Y=REPLICATE(0,MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears])),COL=2,THICK=40,STYLE=2
GPLOT,X=indgen(MAX([silo_nyears,higem_eafeb_nyears,higem_xbylr_nyears])),Y=REPLICATE(-0.272,MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears])),COL=2,THICK=40,STYLE=2
GPLOT,X=indgen(MAX([silo_nyears,higem_eafeb_nyears,higem_xbylr_nyears])),Y=REPLICATE(0.272,MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears])),COL=2,THICK=40,STYLE=2

items=['SILO (1900-2008) with Nino 3','HiGEM xbylr (117 years) with Nino 3','HiGEM eafeb (147 years) with Nino 3']
colors=[2,2,2]
styles=[0,5,1]
LEGEND,labels=REVERSE(items),COL=REVERSE(colors),STYLE=REVERSE(styles),LEGXOFFSET=300,LEGYOFFSET=6200

POS,YPOS=2
GSET,XMIN=0,XMAX=MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears]),YMIN=-1,YMAX=1.0
AXES,XSTEP=10,YSTEP=0.2,XTITLE='',YTITLE='Correlation coefficient',NDECS=2,/NOUPPER

GPLOT,X=indgen(silo_nyears-20)+10,Y=REFORM(silo_nino_corr_seasmean(1,*)),COL=1,THICK=150
GPLOT,X=indgen(higem_xbylr_nyears-20)+10,Y=REFORM(xbylr_nino_corr_seasmean(1,*)),COL=1,STYLE=5,THICK=150
GPLOT,X=indgen(higem_eafeb_nyears-20)+10,Y=REFORM(eafeb_nino_corr_seasmean(1,*)),COL=1,STYLE=1,THICK=150
GPLOT,X=indgen(MAX([silo_nyears,higem_eafeb_nyears,higem_xbylr_nyears])),Y=REPLICATE(0,MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears])),COL=1,THICK=40,STYLE=2
GPLOT,X=indgen(MAX([silo_nyears,higem_eafeb_nyears,higem_xbylr_nyears])),Y=REPLICATE(-0.272,MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears])),COL=1,THICK=40,STYLE=2
GPLOT,X=indgen(MAX([silo_nyears,higem_eafeb_nyears,higem_xbylr_nyears])),Y=REPLICATE(0.272,MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears])),COL=1,THICK=40,STYLE=2

items=['SILO (1900-2008) with Nino 4','HiGEM xbylr (117 years) with Nino 4','HiGEM eafeb (147 years) with Nino 4']
colors=[1,1,1]
styles=[0,5,1]
LEGEND,labels=REVERSE(items),COL=REVERSE(colors),STYLE=REVERSE(styles),LEGXOFFSET=300,LEGYOFFSET=6200

POS,YPOS=1
GSET,XMIN=0,XMAX=MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears]),YMIN=-1,YMAX=1.0
AXES,XSTEP=10,YSTEP=0.2,XTITLE='Year in dataset',YTITLE='Correlation coefficient',NDECS=2,/NOUPPER

GPLOT,X=indgen(silo_nyears-20)+10,Y=REFORM(silo_nino_corr_seasmean(2,*)),COL=5,THICK=150
GPLOT,X=indgen(higem_xbylr_nyears-20)+10,Y=REFORM(xbylr_nino_corr_seasmean(2,*)),COL=5,STYLE=5,THICK=150
GPLOT,X=indgen(higem_eafeb_nyears-20)+10,Y=REFORM(eafeb_nino_corr_seasmean(2,*)),COL=5,STYLE=1,THICK=150
GPLOT,X=indgen(MAX([silo_nyears,higem_eafeb_nyears,higem_xbylr_nyears])),Y=REPLICATE(0,MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears])),COL=5,THICK=40,STYLE=2
GPLOT,X=indgen(MAX([silo_nyears,higem_eafeb_nyears,higem_xbylr_nyears])),Y=REPLICATE(-0.272,MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears])),COL=5,THICK=40,STYLE=2
GPLOT,X=indgen(MAX([silo_nyears,higem_eafeb_nyears,higem_xbylr_nyears])),Y=REPLICATE(0.272,MAX([silo_nyears,higem_xbylr_nyears,higem_eafeb_nyears])),COL=5,THICK=40,STYLE=2

items=['SILO (1900-2008) with Nino 3.4','HiGEM xbylr (117 years) with Nino 3.4','HiGEM eafeb (147 years) with Nino 3.4']
colors=[5,5,5]
styles=[0,5,1]
LEGEND,labels=REVERSE(items),COL=REVERSE(colors),STYLE=REVERSE(styles),LEGXOFFSET=300,LEGYOFFSET=6200

PSCLOSE

STOP

END

