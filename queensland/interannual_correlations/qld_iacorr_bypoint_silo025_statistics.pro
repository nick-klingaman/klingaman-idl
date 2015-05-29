PRO qld_iacorr_bypoint_silo025_statistics

; SILO 0.25x0.25 annual-mean rainfall timeseries
;silo_ameans_infile='/home/ss901165/datasets_mango/SILO/one_quarter/SILO_annual_precip.jan-dec_amean.1900-2008.0.25x0.25.nc'
silo_ameans_infile='/home/ss901165/datasets_mango/SILO/one_quarter/SILO_precip.may-apr_ameans.1900-2008.0.25x0.25.nc'
silo_nyears=108
; Region masks
silo_regions_infile='/home/ss901165/datasets_mango/SILO/one_quarter/SILO_qld_region_mask.0.25x0.25.may-apr.nc'
silo_nregions=N_ELEMENTS(OPEN_AND_EXTRACT(silo_regions_infile,'region'))
; NINO indices timeseries (annual means from May-Apr)
nino_ameans_files=['/home/ss901165/datasets/NINO/nino3_hadisst.may-apr_ameans.1871-2008.nc',$
                   '/home/ss901165/datasets/NINO/nino4_hadisst.may-apr_ameans.1871-2008.nc',$
                   '/home/ss901165/datasets/NINO/nino34_hadisst.may-apr_ameans.1871-2008.nc']

; Read regions latitude and longitude
silo_regions_latitude=OPEN_AND_EXTRACT(silo_regions_infile,'latitude')
silo_regions_longitude=OPEN_AND_EXTRACT(silo_regions_infile,'longitude')
silo_regions_nlon=N_ELEMENTS(silo_regions_longitude)
silo_regions_nlat=N_ELEMENTS(silo_regions_latitude)

; Read precip latitude and longitude, restrict to QLD box
silo_ameans_latitude=OPEN_AND_EXTRACT(silo_ameans_infile,'latitude')
silo_ameans_longitude=OPEN_AND_EXTRACT(silo_ameans_infile,'longitude')
DEFINE_BOUNDARIES,[silo_regions_latitude(0),silo_regions_longitude(0),$
                   silo_regions_latitude(silo_regions_nlat-1),silo_regions_longitude(silo_regions_nlon-1)],$
                  silo_ameans_latitude,silo_ameans_longitude,qld_box_tx,/LIMIT

; Read precipitation data
silo_ameans_precip=REFORM(OPEN_AND_EXTRACT(silo_ameans_infile,'rain',$
                                           offset=[qld_box_tx(1),qld_box_tx(0),0],$
                                           count=[silo_regions_nlon,silo_regions_nlat,silo_nyears]))

silo_ameans_precip_region_aavg=fltarr(silo_nregions,silo_nyears)
silo_intraregion_stddev=fltarr(silo_nregions)
silo_intraregion_stddev_acorr=fltarr(silo_nregions)
FOR i=0,silo_nregions-1 DO BEGIN
   ; Read region mask for this region
   this_region_mask=OPEN_AND_EXTRACT(silo_regions_infile,'mask_region'+STRTRIM(STRING(i+1),1))
   ; Mask annual-mean rainfall timeseries
   silo_ameans_precip_masked=fltarr(silo_regions_nlon,silo_regions_nlat,silo_nyears)
   silo_ameans_precip_masked_clim=fltarr(silo_regions_nlon,silo_regions_nlat)
   silo_ameans_precip_masked_acorr=fltarr(silo_regions_nlon,silo_regions_nlat)
   FOR j=0,silo_nyears-1 DO BEGIN
      temp=REFORM(silo_ameans_precip(*,*,j))
      temp[where(this_region_mask eq 0)]=!Values.F_NaN
      silo_ameans_precip_masked(*,*,j)=temp
      silo_ameans_precip_region_aavg(i,j)=MEAN(temp(*,*),/NaN)
   ENDFOR
   FOR j=0,silo_regions_nlon-1 DO BEGIN
      FOR k=0,silo_regions_nlat-1 DO BEGIN
         silo_ameans_precip_masked_clim(j,k)=MEAN(silo_ameans_precip_masked(j,k,*))*365.
         silo_ameans_precip_masked_acorr(j,k)=A_CORRELATE(silo_ameans_precip_masked(j,k,*),1)         
      ENDFOR
   ENDFOR
   print,MEAN(silo_ameans_precip_masked_acorr,/NaN),MAX([silo_ameans_precip_masked_acorr],/NaN),MIN([silo_ameans_precip_masked_acorr],/NaN)
   silo_intraregion_stddev(i)=STDDEV(silo_ameans_precip_masked_clim,/NaN)
   silo_intraregion_stddev_acorr(i)=STDDEV(silo_ameans_precip_masked_acorr,/NaN)
ENDFOR

openw,lun,'/home/ss901165/idl/queensland/interannual_correlations/qld_iacorr_silo025_statistics_may-apr.txt',/GET_LUN
correlation_matrix=fltarr(silo_nregions,silo_nregions)
FOR i=0,silo_nregions-1 DO $
   FOR j=0,silo_nregions-1 DO $
      correlation_matrix(i,j)=CORRELATE(silo_ameans_precip_region_aavg(i,*),silo_ameans_precip_region_aavg(j,*))
printf,lun,'   CORRELATIONS OF AREA-AVERAGE, ANNUAL TOTAL RAINFALL (1900-2008, FROM SILO 0.25 DEGREE)'
printf,lun,' '
printf,lun,'                               REGION 1 (N PENS)  REGION 2 (N CNTL)  REGION 3 (COAST)   REGION 4 (INTER)   REGION 5 (SE QLD)'
printf,lun,'   REGION 1 (N PENINSULA)',correlation_matrix(0,*),format='(A25,8x,5(F7.5,12x))'
printf,lun,'   REGION 2 (N CNTRL QLD)',correlation_matrix(1,*),format='(A25,8x,5(F7.5,12x))'
printf,lun,'   REGION 3 (E COAST QLD)',correlation_matrix(2,*),format='(A25,8x,5(F7.5,12x))'
printf,lun,'   REGION 4 (SW INTERIOR)',correlation_matrix(3,*),format='(A25,8x,5(F7.5,12x))'
printf,lun,'   REGION 5 (STHEAST QLD)',correlation_matrix(4,*),format='(A25,8x,5(F7.5,12x))'

printf,lun,' '
printf,lun,' '

printf,lun,'   SUMMARY STATISTICS FOR AREA-AVERAGE ANNUAL TOTAL RAINFALL IN EACH REGION (1900-2008, FROM SILO 0.25 DEGREE)'
printf,lun,' '
printf,lun,'                              MEAN ANN TOT    ITR-ANN STD.DEV.   ITR-ANN CO.VAR.   LAG1 ACORR  ITA-RGN STD.DEV.  '
printf,lun,'   REGION 1 (N PENINSULA)',MEAN(silo_ameans_precip_region_aavg(0,*))*365.,STDDEV(silo_ameans_precip_region_aavg(0,*))*365.,$
       STDDEV(silo_ameans_precip_region_aavg(0,*))/MEAN(silo_ameans_precip_region_aavg(0,*)),A_CORRELATE(silo_ameans_precip_region_aavg(0,*),1),$
       silo_intraregion_stddev(0),format='(A25,8x,5(F8.3,8x))'
printf,lun,'   REGION 2 (N CNTRL QLD)',MEAN(silo_ameans_precip_region_aavg(1,*))*365.,STDDEV(silo_ameans_precip_region_aavg(1,*))*365.,$
       STDDEV(silo_ameans_precip_region_aavg(1,*))/MEAN(silo_ameans_precip_region_aavg(1,*)),A_CORRELATE(silo_ameans_precip_region_aavg(1,*),1),$
       silo_intraregion_stddev(1),format='(A25,8x,5(F8.3,8x))'
printf,lun,'   REGION 3 (E COAST QLD)',MEAN(silo_ameans_precip_region_aavg(2,*))*365.,STDDEV(silo_ameans_precip_region_aavg(2,*))*365.,$
       STDDEV(silo_ameans_precip_region_aavg(2,*))/MEAN(silo_ameans_precip_region_aavg(2,*)),A_CORRELATE(silo_ameans_precip_region_aavg(2,*),1),$
       silo_intraregion_stddev(2),format='(A25,8x,5(F8.3,8x))'
printf,lun,'   REGION 4 (SW INTERIOR)',MEAN(silo_ameans_precip_region_aavg(3,*))*365.,STDDEV(silo_ameans_precip_region_aavg(3,*))*365.,$
       STDDEV(silo_ameans_precip_region_aavg(3,*))/MEAN(silo_ameans_precip_region_aavg(3,*)),A_CORRELATE(silo_ameans_precip_region_aavg(3,*),1),$
       silo_intraregion_stddev(3),format='(A25,8x,5(F8.3,8x))'
printf,lun,'   REGION 5 (STHEAST QLD)',MEAN(silo_ameans_precip_region_aavg(4,*))*365.,STDDEV(silo_ameans_precip_region_aavg(4,*))*365.,$
       STDDEV(silo_ameans_precip_region_aavg(4,*))/MEAN(silo_ameans_precip_region_aavg(4,*)),A_CORRELATE(silo_ameans_precip_region_aavg(4,*),1),$
       silo_intraregion_stddev(4),format='(A25,8x,5(F8.3,8x))'

lags=indgen(11)-5
n_lags=N_ELEMENTS(lags)
nino_correlations=fltarr(3,n_lags,silo_nregions)
FOR i=0,2 DO BEGIN
   CASE i OF 
      0: BEGIN
         index='Nino 3'
         variable='NINO3'        
      END
      1: BEGIN
         index='Nino 4'
         variable='NINO4'
      END
      2: BEGIN
         index='Nino 3.4'
         variable='NINO34'
      END
   ENDCASE
   nino_nyears=silo_nyears
   nino_offset=29
   nino_infile=nino_ameans_files(i)
   nino_ts=OPEN_AND_EXTRACT(nino_infile,variable,offset=[nino_offset],count=[nino_nyears])
   FOR j=0,silo_nregions-1 DO $
      nino_correlations(i,*,j)=C_CORRELATE(nino_ts,silo_ameans_precip_region_aavg(j,*),lags)
ENDFOR
            
free_lun,lun

mylevs=['0.02','0.04','0.08','0.16','0.32','0.64','1.28','2.56','5.12']

FOR i=0,silo_nregions-1 DO BEGIN
   wave = WAVELET(silo_ameans_precip_region_aavg(i,*),1,$
                  lag1=A_CORRELATE(silo_ameans_precip_region_aavg(i,*),1),siglvl=0.90,signif=sig90,$
                  period=period,scale=scale)
   wave = WAVELET(silo_ameans_precip_region_aavg(i,*),1,$
                  lag1=A_CORRELATE(silo_ameans_precip_region_aavg(i,*),1),siglvl=0.95,signif=sig95,$
                  period=period,scale=scale,coi=coi)
   power=ABS(wave)^2
   psfile='/home/ss901165/idl/queensland/interannual_correlations/qld_iacorr_bypoint_silo025_statistics.wavelet_region'+$
          STRTRIM(STRING(i+1),1)+'_aavg.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
   CS,IDL=3,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   GSET,XMIN=1900,XMAX=2008,YMIN=50,YMAX=MIN(period),/YLOG
   LEVS,MANUAL=mylevs
   CON,FIELD=power,X=indgen(silo_nyears)+1900,Y=period,$
       TITLE="Wavelet transform of a-avg ann-mean rainfall in QLD region "+STRTRIM(STRING(i+1),1)+" - SILO 0.25 (1900-2008)",$
       /NOLINELABELS
   LEVS,MANUAL=['0','90']
   white=FSC_COLOR("white",50)
   power_sig90=fltarr(silo_nyears,N_ELEMENTS(period))
   power_sig95=fltarr(silo_nyears,N_ELEMENTS(period))
   FOR j=0,N_ELEMENTS(period)-1 DO BEGIN
      power_sig90(*,j)=power(*,j)/sig90(j)
      power_sig95(*,j)=power(*,j)/sig95(j)
   ENDFOR
   CON,FIELD=power_sig90*90,X=indgen(silo_nyears)+1900,Y=period,/NOFILL,/NOCOLBAR,COL=[50,50,50,50]
   LEVS,MANUAL=['0','95']
   CON,FIELD=power_sig95*95,X=indgen(silo_nyears)+1900,Y=period,/NOFILL,/NOCOLBAR,COL=[50,50,50,50]
   AXES,XSTEP=10,yvals=[1,2,3,4,5,6,8,10,15,20,30,40,50],xtitle='Year',ytitle='Period (years)'
   GPLOT,X=indgen(silo_nyears)+1900,Y=coi,COL=50,STYLE=1
   
   PSCLOSE

ENDFOR
   

STOP
END
