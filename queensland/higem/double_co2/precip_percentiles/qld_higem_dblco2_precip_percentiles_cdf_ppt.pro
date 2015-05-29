PRO qld_higem_dblco2_precip_percentiles_cdf_ppt
  
; Bin daily rainfall from the 2x CO2 simulation by the percentiles 
; of the control simulation.

; Input files for grids and mask
higem_ctl_precip_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.may-apr_dmeans.h9-w8.precip_percentiles.aus_domain.nc'
mask_infile='/home/ss901165/um_output/mask_n144_higam.nc'

; Box approximating Queensland
box=[-10,138,-30,154]
box_name='queensland'

; Read land/sea mask
mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)
mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                             offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                             count=[mask_nlon,mask_nlat,1,1]))

; Read HiGEM latitude and longitude
ctl_longitude=OPEN_AND_EXTRACT(higem_ctl_precip_infile,'longitude')
ctl_latitude=OPEN_AND_EXTRACT(higem_ctl_precip_infile,'latitude')
DEFINE_BOUNDARIES,box,ctl_latitude,ctl_longitude,ctl_box_tx,/LIMIT
ctl_nlon=N_ELEMENTS(ctl_longitude)
ctl_nlat=N_ELEMENTS(ctl_latitude)

n_seasons=5
FOR i=0,0 DO BEGIN
   CASE i OF
      0 : BEGIN
         higem_ctl_precip_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.dec-feb_dmeans.h9-w8.precip.global_domain.nc'
         higem_ctl_ndays=90
         higem_ctl_nyears=149
         higem_2xco2_precip_infile='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.dec-feb_dmeans.o2-r3.precip.global_domain.nc'
         higem_2xco2_ndays=90
         higem_2xco2_nyears=31
         season_name='dec-feb'
         xmax=100
      END
      1 : BEGIN
         higem_ctl_precip_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.mar-may_dmeans.h9-w8.precip.global_domain.nc'
         higem_ctl_ndays=90
         higem_ctl_nyears=149
         higem_2xco2_precip_infile='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.mar-may_dmeans.o2-r3.precip.global_domain.nc'
         higem_2xco2_ndays=90
         higem_2xco2_nyears=31
         season_name='mar-may'
         xmax=100
      END
      2 : BEGIN
         higem_ctl_precip_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.jun-aug_dmeans.h9-w8.precip.global_domain.nc'
         higem_ctl_ndays=90
         higem_ctl_nyears=149
         higem_2xco2_precip_infile='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.jun-aug_dmeans.o2-r3.precip.global_domain.nc'
         higem_2xco2_ndays=90
         higem_2xco2_nyears=31
         season_name='jun-aug'
         xmax=50
      END
      3 : BEGIN
         higem_ctl_precip_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.sep-nov_dmeans.h9-w8.precip.global_domain.nc'
         higem_ctl_ndays=90
         higem_ctl_nyears=149
         higem_2xco2_precip_infile='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.sep-nov_dmeans.o2-r3.precip.global_domain.nc'
         higem_2xco2_ndays=90
         higem_2xco2_nyears=31
         season_name='sep-nov'
         extreme_frac_max=0.015
         extreme_count_max=0.1
         extreme_count_step=0.01
         extreme_ratio_max=1.4
         extreme_ratio_step=0.1
         xmax=50
      END
      4 : BEGIN         
         higem_ctl_precip_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.may-apr_dmeans.h9-w8.precip.global_domain.nc'
         higem_ctl_ndays=360
         higem_ctl_nyears=149
         higem_2xco2_precip_infile='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.may-apr_dmeans.o2-r3.precip.global_domain.nc'
         higem_2xco2_ndays=360
         higem_2xco2_nyears=31
         season_name='may-apr'
         extreme_frac_max=0.015
         extreme_count_max=1
         extreme_count_step=0.1
         extreme_ratio_max=1.4
         extreme_ratio_step=0.1
         xmax=100
      END
   ENDCASE
                                ; Read HiGEM latitude and longitude
   higem_ctl_longitude=OPEN_AND_EXTRACT(higem_ctl_precip_infile,'longitude')
   higem_ctl_latitude=OPEN_AND_EXTRACT(higem_ctl_precip_infile,'latitude')
   DEFINE_BOUNDARIES,box,higem_ctl_latitude,higem_ctl_longitude,higem_ctl_box_tx,/LIMIT
   higem_ctl_nlon=N_ELEMENTS(higem_ctl_longitude)
   higem_ctl_nlat=N_ELEMENTS(higem_ctl_latitude)

                                ; Read HiGEM latitude and longitude
   higem_2xco2_longitude=OPEN_AND_EXTRACT(higem_2xco2_precip_infile,'longitude')
   higem_2xco2_latitude=OPEN_AND_EXTRACT(higem_2xco2_precip_infile,'latitude')
   DEFINE_BOUNDARIES,box,higem_2xco2_latitude,higem_2xco2_longitude,higem_2xco2_box_tx,/LIMIT
   higem_2xco2_nlon=N_ELEMENTS(higem_2xco2_longitude)
   higem_2xco2_nlat=N_ELEMENTS(higem_2xco2_latitude)
   
                                ; Read the daily rainfall amounts for all days and all years
   print,'Reading daily rainfall ...'
   higem_2xco2_daily_rainfall=OPEN_AND_EXTRACT(higem_2xco2_precip_infile,'precip',$
                                               offset=[higem_2xco2_box_tx(1),higem_2xco2_box_tx(0),0,0],$
                                               count=[higem_2xco2_nlon,higem_2xco2_nlat,higem_2xco2_ndays,higem_2xco2_nyears])*86400.
   higem_ctl_daily_rainfall=OPEN_AND_EXTRACT(higem_ctl_precip_infile,'precip',$
                                             offset=[higem_ctl_box_tx(1),higem_ctl_box_tx(0),0,0],$
                                             count=[higem_ctl_nlon,higem_ctl_nlat,higem_ctl_ndays,higem_ctl_nyears])*86400.
   print,'... done reading'
   IF TOTAL(where(higem_ctl_daily_rainfall le -1000) gt 0) THEN $
      higem_ctl_daily_rainfall[where(higem_ctl_daily_rainfall le 0)]=!Values.F_NaN
   IF TOTAL(where(higem_ctl_daily_rainfall ge 1E6) gt 0) THEN $
      higem_ctl_daily_rainfall[where(higem_ctl_daily_rainfall ge 1E6)]=!Values.F_NaN

   IF TOTAL(where(higem_ctl_daily_rainfall le 0 and higem_ctl_daily_rainfall gt -1) gt 0) THEN $
      higem_ctl_daily_rainfall[where(higem_ctl_daily_rainfall le 0 and higem_ctl_daily_rainfall gt -1)]=0.
   IF TOTAL(where(higem_ctl_daily_rainfall le 0 and higem_ctl_daily_rainfall gt -1) gt 0) THEN $
      higem_ctl_daily_rainfall[where(higem_ctl_daily_rainfall le 0 and higem_ctl_daily_rainfall gt -1)]=0.
    

   FOR j=0,higem_2xco2_ndays-1 DO BEGIN
      FOR k=0,higem_2xco2_nyears-1 DO BEGIN
         temp=REFORM(higem_2xco2_daily_rainfall(*,*,j,k))
         temp[where(mask eq 0)]=!Values.F_NaN
         higem_2xco2_daily_rainfall(*,*,j,k)=temp
      ENDFOR
   ENDFOR
   FOR j=0,higem_ctl_ndays-1 DO BEGIN
      FOR k=0,higem_ctl_nyears-1 DO BEGIN
         temp=REFORM(higem_ctl_daily_rainfall(*,*,j,k))
         temp[where(mask eq 0)]=!Values.F_NaN
         higem_ctl_daily_rainfall(*,*,j,k)=temp
      ENDFOR
   ENDFOR

   n_chunks=5
   nyears_per_chunk=30

   npdf=100.
   maxpdf=100.
   pdfstep=maxpdf/npdf
   higem_ctl_xcdf=findgen(npdf)*pdfstep+pdfstep/2.
   higem_2xco2_xcdf=findgen(npdf)*pdfstep+pdfstep/2.
  
   input=higem_ctl_daily_rainfall[where(FINITE(higem_ctl_daily_rainfall) eq 1)]
   higem_ctl_cdf=fltarr(npdf)
   FOR j=1,npdf DO $
      higem_ctl_cdf(j-1)=N_ELEMENTS(where(input le j*pdfstep))/FLOAT(N_ELEMENTS(input))

   input=higem_2xco2_daily_rainfall[where(FINITE(higem_2xco2_daily_rainfall) eq 1)]
   higem_2xco2_cdf=fltarr(npdf)
   FOR j=1,npdf DO $
      higem_2xco2_cdf(j-1)=N_ELEMENTS(where(input le j*pdfstep))/FLOAT(N_ELEMENTS(input))

   higem_ctl_cdf_chunks=fltarr(n_chunks,npdf)
   FOR j=0,n_chunks-1 DO BEGIN
      IF j ne n_chunks-1 THEN BEGIN
         stop_year=(j+1)*nyears_per_chunk-1
      ENDIF ELSE $
         stop_year=(j+1)*nyears_per_chunk-2
      input=higem_ctl_daily_rainfall(*,*,*,j*nyears_per_chunk:stop_year)
      input=input[where(FINITE(input) eq 1)]
      FOR k=1,npdf DO $
         higem_ctl_cdf_chunks(j,k-1)=N_ELEMENTS(where(input lt k*pdfstep and input ge 0))/FLOAT(N_ELEMENTS(input))
   ENDFOR

   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/qld_higem_dblco2_precip_percentiles_cdf_ppt.'+box_name+'.'+season_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=120,MARGIN=2500,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=1000,TFONT=3,$
          TCHARSIZE=100,SPACE3=500
   GSET,XMIN=0,XMAX=xmax,YMIN=0.4,YMAX=0.001,/YLOG
   GPLOT,X=higem_ctl_xcdf[where(higem_ctl_cdf le 0.999)],Y=1-higem_ctl_cdf[where(higem_ctl_cdf le 0.999)],COL=FSC_COLOR('black')
   GPLOT,X=higem_2xco2_xcdf[where(higem_2xco2_cdf le 0.999)],Y=1-higem_2xco2_cdf[where(higem_2xco2_cdf le 0.999)],COL=FSC_COLOR('red')
;   FOR j=0,n_chunks-1 DO BEGIN
;      temp=REFORM(higem_ctl_cdf_chunks(j,*))
;      GPLOT,X=higem_ctl_xcdf[where(temp le 0.999)],Y=1-temp[where(temp le 0.999)],COL=FSC_COLOR('black'),STYLE=2
;   ENDFOR
   AXES,XVALS=['0','5','10','15','20','25','30','35','40','45','50','55','60','65','70','75','80','85','90','95','100'],$
        XMINOR=2.5,YVALS=[0.001,0.0015,0.002,0.003,0.004,0.005,0.007,0.01,0.015,0.02,0.03,0.04,0.06,0.10,0.15,0.20,0.30,0.40],$
        YLABELS=['1000','667','500','333','250','200','143','100','66.7','50','33.3','25','16.7','10.0','6.7','5.0','3.33','2.5'],NDECS=3,$
        XTITLE='Daily precipitation (mm)',YTITLE='Frequency of event (days)',/NORIGHT
   GSET,XMIN=0,XMAX=xmax,YMIN=-50,YMAX=100
   GPLOT,X=higem_2xco2_xcdf(where(higem_2xco2_xcdf le xmax)),Y=(1-higem_2xco2_cdf(where(higem_2xco2_xcdf le xmax))-(1-higem_ctl_cdf(where(higem_2xco2_xcdf le xmax))))/(1-higem_ctl_cdf(where(higem_2xco2_xcdf le xmax)))*100,COL=FSC_COLOR('blue')
   GPLOT,X=[0,maxpdf],Y=[0,0],STYLE=1,COL=FSC_COLOR('blue')
;   FOR j=0,n_chunks-1 DO BEGIN
;      chunk=REFORM(higem_ctl_cdf_chunks(j,*))
;      GPLOT,X=higem_2xco2_xcdf(where(higem_2xco2_xcdf le xmax)),Y=(1-higem_2xco2_cdf(where(higem_2xco2_xcdf le xmax)))/$
;            (1-chunk(where(higem_2xco2_xcdf le xmax))),COL=FSC_COLOR('blue'),STYLE=2
;   ENDFOR
   AXES,XSTEP=10.,XMINOR=5.,YSTEP=10,YTITLE='Percentage change in frequency of event due to 2xCO2',/ONLYRIGHT,NDECS=1
   GLEGEND,LEGPOS=1,LABELS=REVERSE(['HiGEM control','HiGEM 2xCO2','Ratio: 2xCO2 divided by control']),$
           COL=REVERSE(FSC_COLOR(['black','red','blue'])),STYLE=REVERSE([0,0,0])
;   GLEGEND,LEGPOS=1,LABELS=REVERSE(['HiGEM CTL (150 years)','HiGEM 2xCO2 (30 years)','CTL 30 year segments']),$
;           COL=REVERSE(FSC_COLOR(['black','red','black'])),STYLE=REVERSE([0,0,2])
;   GLEGEND,LEGPOS=11,LABELS=REVERSE(['Ratio: 2xCO2 to CTL (150 years)','Ratio: 2xCO2 to CTL (30 years)']),$
;           COL=REVERSE(FSC_COLOR(['blue','blue'])),STYLE=REVERSE([0,2])
   PSCLOSE
ENDFOR
   
STOP
END
