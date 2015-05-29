PRO qld_higem_dblco2_precip_clim_wetdays
  
; Plot the seasonal cycle of the average number of points with > threshold rain
; on each day

higem_ctl_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.may-apr_dmeans.h9-w8.precip.global_domain.nc'
higem_ctl_nyears=149
higem_stbl_infile='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.may-apr_dmeans.o2-r3.precip.global_domain.nc'
higem_stbl_nyears=31

higam_ctl_infile='/home/ss901165/higam_qccce/es_higemctl_eagsl/higam_eagsl.may-apr_dmeans.k9-n7.precip.global_domain.nc'
higam_ctl_nyears=28
higam_stbl_infile='/home/ss901165/higam_qccce/es_higem2xco2_eagsm/higam_eagsm.may-apr_dmeans.o3-r1.precip.global_domain.nc'
higam_stbl_nyears=28

higem_mask_infile='/home/ss901165/um_output/mask_n144_higam.nc'

n_days=360
box=[-30,138,-10,154]


higem_longitude=OPEN_AND_EXTRACT(higem_ctl_infile,'longitude')
higem_latitude=OPEN_AND_EXTRACT(higem_ctl_infile,'latitude')
DEFINE_BOUNDARIES,box,higem_latitude,higem_longitude,higem_box_tx,/LIMIT
higem_nlat=N_ELEMENTS(higem_latitude)
higem_nlon=N_ELEMENTS(higem_longitude)

higem_mask=REFORM(OPEN_AND_EXTRACT(higem_mask_infile,'lsm',$
                                   offset=[higem_box_tx(1),higem_box_tx(0),0,0],$
                                   count=[higem_nlon,higem_nlat,1,1]))
higem_ctl_precip=OPEN_AND_EXTRACT(higem_ctl_infile,'precip',$
                                  offset=[higem_box_tx(1),higem_box_tx(0),0,0],$
                                  count=[higem_nlon,higem_nlat,360,higem_ctl_nyears])*86400.
higem_ctl_precip[where(higem_ctl_precip ge 1e10)]=!Values.F_NaN
higem_stbl_precip=REFORM(OPEN_AND_EXTRACT(higem_stbl_infile,'precip',$
                                          offset=[higem_box_tx(1),higem_box_tx(0),0,0],$
                                          count=[higem_nlon,higem_nlat,360,higem_stbl_nyears]))*86400.
higam_ctl_precip=OPEN_AND_EXTRACT(higam_ctl_infile,'precip',$
                                  offset=[higem_box_tx(1),higem_box_tx(0),0,0],$
                                  count=[higem_nlon,higem_nlat,360,higam_ctl_nyears])*86400.
higam_stbl_precip=REFORM(OPEN_AND_EXTRACT(higam_stbl_infile,'precip',$
                                          offset=[higem_box_tx(1),higem_box_tx(0),0,0],$
                                          count=[higem_nlon,higem_nlat,360,higam_stbl_nyears]))*86400.

higem_ctl_wetfrac=fltarr(n_days)
higem_ctl_wetints=fltarr(n_days)
higem_stbl_wetfrac=fltarr(n_days)
higem_stbl_wetints=fltarr(n_days)
higam_ctl_wetfrac=fltarr(n_days)
higam_ctl_wetints=fltarr(n_days)
higam_stbl_wetfrac=fltarr(n_days)
higam_stbl_wetints=fltarr(n_days)

FOR i=0,n_days-1 DO BEGIN
   FOR j=0,higem_ctl_nyears-1 DO BEGIN
      temp=REFORM(higem_ctl_precip(*,*,i,j))
      temp[where(higem_mask eq 0)]=!Values.F_NaN
      higem_ctl_precip(*,*,i,j)=temp
   ENDFOR
   temp=REFORM(higem_ctl_precip(*,*,i,*))
   higem_ctl_wetfrac(i)=N_ELEMENTS(where(temp ge 1 and FINITE(temp) eq 1))/$
                        FLOAT(N_ELEMENTS(where(FINITE(temp) eq 1)))   
   goodpts=where(temp ge 1 and FINITE(temp) eq 1)
   higem_ctl_wetints(i)=MEAN(temp[goodpts])
   IF higem_ctl_wetints(i) lt 1 THEN STOP

   FOR j=0,higem_stbl_nyears-1 DO BEGIN
      temp=REFORM(higem_stbl_precip(*,*,i,j))
      temp[where(higem_mask eq 0)]=!Values.F_NaN
      higem_stbl_precip(*,*,i,j)=temp
   ENDFOR
   temp=REFORM(higem_stbl_precip(*,*,i,*))
   higem_stbl_wetfrac(i)=N_ELEMENTS(where(temp ge 1 and FINITE(temp) eq 1))/$
                        FLOAT(N_ELEMENTS(where(FINITE(temp) eq 1)))   
   goodpts=where(temp ge 1 and FINITE(temp) eq 1)
   higem_stbl_wetints(i)=MEAN(temp[goodpts])
   IF higem_stbl_wetints(i) lt 1 THEN STOP
  
   FOR j=0,higam_ctl_nyears-1 DO BEGIN
      temp=REFORM(higam_ctl_precip(*,*,i,j))
      temp[where(higem_mask eq 0)]=!Values.F_NaN
      higam_ctl_precip(*,*,i,j)=temp
   ENDFOR
   temp=REFORM(higam_ctl_precip(*,*,i,*))
   higam_ctl_wetfrac(i)=N_ELEMENTS(where(temp ge 1 and FINITE(temp) eq 1))/$
                        FLOAT(N_ELEMENTS(where(FINITE(temp) eq 1)))   
   goodpts=where(temp ge 1 and FINITE(temp) eq 1)
   higam_ctl_wetints(i)=MEAN(temp[goodpts])
   IF higam_ctl_wetints(i) lt 1 THEN STOP

   FOR j=0,higam_stbl_nyears-1 DO BEGIN
      temp=REFORM(higam_stbl_precip(*,*,i,j))
      temp[where(higem_mask eq 0)]=!Values.F_NaN
      higam_stbl_precip(*,*,i,j)=temp
   ENDFOR
   temp=REFORM(higam_stbl_precip(*,*,i,*))
   higam_stbl_wetfrac(i)=N_ELEMENTS(where(temp ge 1 and FINITE(temp) eq 1))/$
                        FLOAT(N_ELEMENTS(where(FINITE(temp) eq 1)))   
   goodpts=where(temp ge 1 and FINITE(temp) eq 1)
   higam_stbl_wetints(i)=MEAN(temp[goodpts])
   IF higam_stbl_wetints(i) lt 1 THEN STOP
ENDFOR

n_chunks=5
higem_ctl_chunk_nyears=29
higem_ctl_chunks_wetfrac=fltarr(n_chunks,n_days)
FOR m=0,n_chunks-1 DO BEGIN
   CASE m OF 
      0 : BEGIN
         infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.may-apr_dmeans.h9-k8.precip.aus_domain.nc'
      END
      1 : BEGIN
         infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.may-apr_dmeans.k9-n8.precip.aus_domain.nc'
      END
      2 : BEGIN
         infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.may-apr_dmeans.n9-q8.precip.aus_domain.nc'
      END
      3 : BEGIN
         infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.may-apr_dmeans.q9-t8.precip.aus_domain.nc'
      END
      4 : BEGIN
         infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.may-apr_dmeans.t9-w8.precip.aus_domain.nc'
      END
   ENDCASE

   chunk_longitude=OPEN_AND_EXTRACT(infile,'longitude')
   chunk_latitude=OPEN_AND_EXTRACT(infile,'latitude')
   DEFINE_BOUNDARIES,box,chunk_latitude,chunk_longitude,chunk_box_tx,/LIMIT
   chunk_nlat=N_ELEMENTS(chunk_latitude)
   chunk_nlon=N_ELEMENTS(chunk_longitude)

   higem_ctl_chunk_precip=OPEN_AND_EXTRACT(infile,'precip',$
                                           offset=[chunk_box_tx(1),chunk_box_tx(0),0,0],$
                                           count=[chunk_nlon,chunk_nlat,360,higem_ctl_chunk_nyears])*86400.
   FOR i=0,n_days-1 DO BEGIN
      FOR j=0,higem_ctl_chunk_nyears-1 DO BEGIN
         temp=REFORM(higem_ctl_chunk_precip(*,*,i,j))
         temp[where(higem_mask eq 0)]=!Values.F_NaN
         higem_ctl_chunk_precip(*,*,i,j)=temp
      ENDFOR
      higem_ctl_chunks_wetfrac(m,i)=N_ELEMENTS(where(REFORM(higem_ctl_chunk_precip(*,*,i,*)) ge 1 and FINITE(REFORM(higem_ctl_chunk_precip(*,*,i,*))) eq 1))/$
                                   FLOAT(N_ELEMENTS(where(FINITE(REFORM(higem_ctl_chunk_precip(*,*,i,*))) eq 1)))   
   ENDFOR
ENDFOR

psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_clim_wetdays.higem_higam.wetfrac.queensland.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1800,SPACE2=300,XOFFSET=800,YOFFSET=1500,TFONT=2,$
       TCHARSIZE=80,CB_WIDTH=110,SPACE3=300
GSET,XMIN=0,XMAX=n_days,YMIN=0,YMAX=0.5
GPLOT,X=indgen(n_days)+0.5,Y=SMOOTH(higem_ctl_wetfrac,7),COL=FSC_COLOR('blue'),THICK=200
FOR i=0,n_chunks-1 DO $
   GPLOT,X=indgen(n_days)+0.5,Y=SMOOTH(REFORM(higem_ctl_chunks_wetfrac(i,*)),7),COL=FSC_COLOR('black'),THICK=100
GPLOT,X=indgen(n_days)+0.5,Y=SMOOTH(higem_stbl_wetfrac,7),COL=FSC_COLOR('red'),THICK=200
GPLOT,X=indgen(n_days)+0.5,Y=SMOOTH(higam_ctl_wetfrac,7),COL=FSC_COLOR('cyan'),THICK=200
GPLOT,X=indgen(n_days)+0.5,Y=SMOOTH(higam_stbl_wetfrac,7),COL=FSC_COLOR('pink'),THICK=200
AXES,XVALS=[0,30,60,90,120,150,180,210,240,270,300,330,360],$
     XLABELS=['May','Jun','Jul','Aug','Sep','Oct','Nov','Dec','Jan','Feb','Mar','Apr','May'],$
     YSTEP=0.05,YMINOR=0.025,XMINOR=10,NDECS=3,$
     XTITLE='Time',YTITLE='Probability of >= 1 mm of rain, accumulated over all Queensland land points'
GLEGEND,labels=['HiGAM control (30 years)','HiGAM 2xCO2 (30 years)','HiGEM control (30 years)',$
                'HiGEM control (150 years)','HiGEM 2xCO2 (30 years)'],$
        COL=[FSC_COLOR('cyan'),FSC_COLOR('pink'),FSC_COLOR('black'),FSC_COLOR('blue'),$
             FSC_COLOR('red')],THICK=[200,200,100,200,200],LEGPOS=1
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_clim_wetdays.higem_higam.wetints.queensland.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1800,SPACE2=300,XOFFSET=800,YOFFSET=1500,TFONT=2,$
       TCHARSIZE=80,CB_WIDTH=110,SPACE3=300
GSET,XMIN=0,XMAX=n_days,YMIN=3,YMAX=15
GPLOT,X=indgen(n_days)+0.5,Y=SMOOTH(higem_ctl_wetints,7),COL=FSC_COLOR('blue'),THICK=200
;FOR i=0,n_chunks-1 DO $
;   GPLOT,X=indgen(n_days)+0.5,Y=SMOOTH(REFORM(higem_ctl_chunks_wetints(i,*)),7),COL=FSC_COLOR('black'),THICK=100
GPLOT,X=indgen(n_days)+0.5,Y=SMOOTH(higem_stbl_wetints,7),COL=FSC_COLOR('red'),THICK=200
GPLOT,X=indgen(n_days)+0.5,Y=SMOOTH(higam_ctl_wetints,7),COL=FSC_COLOR('cyan'),THICK=200
GPLOT,X=indgen(n_days)+0.5,Y=SMOOTH(higam_stbl_wetints,7),COL=FSC_COLOR('pink'),THICK=200
AXES,XVALS=[0,30,60,90,120,150,180,210,240,270,300,330,360],$
     XLABELS=['May','Jun','Jul','Aug','Sep','Oct','Nov','Dec','Jan','Feb','Mar','Apr','May'],$
     YSTEP=1,YMINOR=0.5,XMINOR=10,NDECS=3,$
     XTITLE='Time',YTITLE='Mean amount at points with >= 1 mm of rain, accumulated over all Queensland land points'
GLEGEND,labels=['HiGAM control (30 years)','HiGAM 2xCO2 (30 years)','HiGEM control (30 years)',$
                'HiGEM control (150 years)','HiGEM 2xCO2 (30 years)'],$
        COL=[FSC_COLOR('cyan'),FSC_COLOR('pink'),FSC_COLOR('black'),FSC_COLOR('blue'),$
             FSC_COLOR('red')],THICK=[200,200,100,200,200],LEGPOS=1
PSCLOSE


STOP
END
