PRO hadgem3kpp_cascade_apr09_precip_pdf

n_models=3
um3='/home/ss901165/um_output3'
box=[-5,40,5,200]

npdf=61
xmin=0
xmax=2
precip_pdfs=fltarr(n_models,npdf)
precip_cdfs=fltarr(n_models,npdf)
descriptions=strarr(n_models)

FOR i=0,n_models-1 DO BEGIN
   CASE i OF
      2 : BEGIN
         runid='xfadm'
         dirname='xfadm'
         description='T-Vary OSTIA, 1.5x mix entrain and no CMT (xfadm)'
      END
      1 : BEGIN
         runid='xfadk'
         dirname='xfadk'
         description='T-vary OSTIA and 1.5x mix entrain (xfadk)'
      END
      0 : BEGIN
         runid='xfadh'
         dirname='xfadh'
         description='T-vary OSTIA (xfadh)'
      END
   ENDCASE

   precip_hrly_infile=um3+'/'+dirname+'/'+runid+'a.06apr09-05may09.precip.nc'
   n_time=720

   precip_longitude=OPEN_AND_EXTRACT(precip_hrly_infile,'longitude_1')
   precip_latitude=OPEN_AND_EXTRACT(precip_hrly_infile,'latitude_2')
   DEFINE_BOUNDARIES,box,precip_latitude,precip_longitude,precip_box_tx,/LIMIT
   precip_nlon=N_ELEMENTS(precip_longitude)
   precip_nlat=N_ELEMENTS(precip_latitude)

   precip=REFORM(OPEN_AND_EXTRACT(precip_hrly_infile,'precip',$
                                  offset=[precip_box_tx(1),precip_box_tx(0),0,0],$
                                  count=[precip_nlon,precip_nlat,1,n_time]))*3600.

   precip_oned=REFORM(precip,[precip_nlon*precip_nlat*n_time])
   print,'Computing PDF for run '+runid
   PDF,precip_oned,xrange=[xmin,xmax],npdf=npdf,pdf=precip_pdf,/bandwidth,xid=pdf_xaxis,/noplot
   precip_pdfs(i,*)=precip_pdf  

   precip_sum=TOTAL(precip)
   print,'For run '+runid+' domain-average precip rate: '+STRTRIM(STRING(MEAN(precip)),1)
   FOR j=0,npdf-1 DO $
      precip_cdfs(i,j)=(TOTAL(precip[where(precip le pdf_xaxis(j))])/precip_sum)*100.
   descriptions(i)=description
ENDFOR

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_pdf.1hr_means.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1800,SPACE2=700,XOFFSET=1200,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT
GSET,XMIN=xmin,XMAX=xmax,YMIN=0.00003,YMAX=0.015,/YLOG
colors=['blue','red','brown']
FOR i=0,n_models-1 DO $
   GPLOT,X=indgen(npdf)*(xmax-xmin)/FLOAT(npdf-1)+xmin,Y=REFORM(precip_pdfs(i,*)),COL=FSC_COLOR(colors(i))
AXES,XSTEP=0.25,XMINOR=0.125,NDECS=4,YVALS=['0.00003','0.00004','0.00005','0.00007','0.0001','0.00015','0.0002','0.0003','0.0004',$
                                            '0.0005','0.0007','0.001','0.0015','0.002','0.003','0.004','0.005','0.007','0.01','0.012','0.015'],$
     XTITLE='Precipitation rate (mm hr!U-1!N)',YTITLE='Probability density'
GPLOT,X=0,Y=0.017,TEXT='Probability density function of hourly precipitation - 5S-5N, 40-200E - April 2009 case',ALIGN=0.0
LEGEND,labels=REVERSE(descriptions),COL=REVERSE(FSC_COLOR(colors)),STYLE=REPLICATE(0,n_models),LEGPOS=9

;YVALS=['0.00001','0.00002','0.00004','0.00007','0.0001','0.0002','0.0004',$
;'0.0007','0.001','0.002','0.004','0.007','0.01','0.02']
PSCLOSE

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_cdf.1hr_means.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1800,SPACE2=700,XOFFSET=1200,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT
GSET,XMIN=xmin,XMAX=xmax,YMIN=0.5,YMAX=100,/YLOG
colors=['blue','red','brown']
FOR i=0,n_models-1 DO $
   GPLOT,X=indgen(npdf)*(xmax-xmin)/FLOAT(npdf-1)+xmin,Y=REFORM(precip_cdfs(i,*)),COL=FSC_COLOR(colors(i))
AXES,XSTEP=0.25,XMINOR=0.125,NDECS=2,YVALS=['0.7','1.0','1.5','2.0','2.5','3.0','4.0','5.0','7.0','10.0','14.0','17.0','20.0',$
                                            '25.0','30.0','40.0','50.0','60.0','70.0','80.0','100.0'],$
     XTITLE='Precipitation rate (mm hr!U-!N)',YTITLE='Cumulative fraction of domain-total precipitation'
GPLOT,X=0,Y=105,TEXT='Cumulative fraction of domain-total precipitation - 5S-5N, 40-200E - April 2009 case - hourly data',ALIGN=0.0
LEGEND,labels=REVERSE(descriptions),COL=REVERSE(FSC_COLOR(colors)),STYLE=REPLICATE(0,n_models),LEGPOS=10

PSCLOSE

STOP

END
