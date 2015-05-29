PRO qld_higem_dblco2_precip_percentiles_aavg

n_seasons=5
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

FOR j=0,n_seasons-1 DO BEGIN
   CASE j OF 
      0 : BEGIN
         season_name='may-apr'
         ymax=91
         ystep=7
         ratio_max=1.45
         ratio_min=0.75
      END
      1 : BEGIN
         season_name='dec-feb'
         ymax=110
         ystep=10
      END
      2 : BEGIN
         season_name='mar-may'
         ymax=110
         ystep=10
      END
      3 : BEGIN
         season_name='jun-aug'
         ymax=80
         ystep=8
         ratio_max=2.00
         ratio_min=0.50
      END
      4 : BEGIN
         season_name='sep-nov'
         ymax=50
         ystep=5
         ratio_max=1.45
         ratio_min=0.75
      END
   ENDCASE
                                ; Input files
   ctl_percentiles_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.'+season_name+'_dmeans.h9-w8.precip_percentiles.aus_domain.nc'
   doubleco2_percentiles_infile='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.'+season_name+'_dmeans.o2-r3.precip_percentiles.aus_domain.nc'

                                ; Read HiGEM latitude and longitude
   ctl_longitude=OPEN_AND_EXTRACT(ctl_percentiles_infile,'longitude')
   ctl_latitude=OPEN_AND_EXTRACT(ctl_percentiles_infile,'latitude')
   DEFINE_BOUNDARIES,box,ctl_latitude,ctl_longitude,ctl_box_tx,/LIMIT
   ctl_nlon=N_ELEMENTS(ctl_longitude)
   ctl_nlat=N_ELEMENTS(ctl_latitude)
   
   doubleco2_longitude=OPEN_AND_EXTRACT(doubleco2_percentiles_infile,'longitude')
   doubleco2_latitude=OPEN_AND_EXTRACT(doubleco2_percentiles_infile,'latitude')
   DEFINE_BOUNDARIES,box,doubleco2_latitude,doubleco2_longitude,doubleco2_box_tx,/LIMIT
   doubleco2_nlon=N_ELEMENTS(doubleco2_longitude)
   doubleco2_nlat=N_ELEMENTS(doubleco2_latitude)
   
   percentiles=OPEN_AND_EXTRACT(ctl_percentiles_infile,'percentile')
   n_percentiles=N_ELEMENTS(percentiles)
                                ; Read the climatological percentiles of daily rainfall
   ctl_percentiles=REFORM(OPEN_AND_EXTRACT(ctl_percentiles_infile,'amount_at_percentile',$
                                           offset=[ctl_box_tx(1),ctl_box_tx(0),0],$
                                           count=[ctl_nlon,ctl_nlat,n_percentiles]))
   doubleco2_percentiles=REFORM(OPEN_AND_EXTRACT(doubleco2_percentiles_infile,'amount_at_percentile',$
                                                 offset=[doubleco2_box_tx(1),doubleco2_box_tx(0),0],$
                                                 count=[doubleco2_nlon,doubleco2_nlat,n_percentiles]))
   ctl_percentiles_aavg=fltarr(n_percentiles)
   ctl_percentiles_stddev=fltarr(n_percentiles)
   doubleco2_percentiles_aavg=fltarr(n_percentiles)
   doubleco2_percentiles_stddev=fltarr(n_percentiles)
   
   FOR i=0,n_percentiles-1 DO BEGIN
      temp=REFORM(ctl_percentiles(*,*,i))   
      temp[where(mask eq 0)]=!Values.F_NaN
      ctl_percentiles_aavg(i)=MEAN(temp,/NaN)
      ctl_percentiles_stddev(i)=STDDEV(temp,/NaN)
      temp=REFORM(doubleco2_percentiles(*,*,i))
      temp[where(mask eq 0)]=!Values.F_NaN
      doubleco2_percentiles_aavg(i)=MEAN(temp,/NaN)
      doubleco2_percentiles_stddev(i)=STDDEV(temp,/NaN)
   ENDFOR
   
   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/qld_higem_dblco2_precip_percentiles_aavg.'+season_name+'_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=200,XOFFSET=0,YOFFSET=1000,TFONT=3,$
          TCHARSIZE=100,SPACE3=100
   GSET,XMIN=0,XMAX=100,YMIN=0,YMAX=ymax,TITLE='Mean rain amounts at each percentile - '+season_name+' - '+box_name
   GPLOT,X=percentiles,Y=ctl_percentiles_aavg,COL=FSC_COLOR('black')
   GPLOT,X=percentiles,Y=ctl_percentiles_aavg+ctl_percentiles_stddev,COL=FSC_COLOR('black'),STYLE=2
   GPLOT,X=percentiles,Y=ctl_percentiles_aavg-ctl_percentiles_stddev,COL=FSC_COLOR('black'),STYLE=2
   GPLOT,X=percentiles,Y=doubleco2_percentiles_aavg,COL=FSC_COLOR('red')
   GPLOT,X=percentiles,Y=doubleco2_percentiles_aavg+doubleco2_percentiles_stddev,COL=FSC_COLOR('red'),STYLE=2
   GPLOT,X=percentiles,Y=doubleco2_percentiles_aavg-doubleco2_percentiles_stddev,COL=FSC_COLOR('red'),STYLE=2
   AXES,XSTEP=5,XMINOR=2.5,YSTEP=ystep,YMINOR=ystep/2.,XTITLE='Percentile of daily rainfall',YTITLE='Precipitation (mm)',NDECS=1,/NORIGHT
   GSET,XMIN=0,XMAX=100,YMIN=ratio_min,YMAX=ratio_max
   GPLOT,X=percentiles,Y=doubleco2_percentiles_aavg/ctl_percentiles_aavg,COL=FSC_COLOR('blue')
   GPLOT,X=[0,100],Y=[1,1],COL=FSC_COLOR('blue'),STYLE=2
   AXES,YSTEP=0.05,YMINOR=0.025,YTITLE='Ratio of amounts: 2xCO2 to control',/ONLYRIGHT,NDECS=2
   GLEGEND,LEGPOS=1,$
           labels=REVERSE(['HiGEM control (150 years)','HiGEM 2xCO2 (30 years)','Spatial standard deviation','Ratio']),$
           COL=REVERSE(FSC_COLOR(['black','red','black','blue'])),STYLE=REVERSE([0,0,2,0])
   PSCLOSE,/NOVIEW
   
                                ; 30 year chunks of control integration
   n_chunks=5
   chunk_percentiles_aavg=fltarr(n_chunks,n_percentiles)
   FOR k=0,n_chunks-1 DO BEGIN
      CASE k OF
         0 : BEGIN
            year_range='h9-k8'
         END
         1 : BEGIN
            year_range='k9-n8'
         END
         2 : BEGIN
            year_range='n9-q8'
         END
         3 : BEGIN
            year_range='q9-t8'
         END
         4 : BEGIN
            year_range='t9-w8'
         END
      ENDCASE
      
      chunk_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.'+season_name+'_dmeans.'+year_range+'.precip_percentiles.aus_domain.nc'
      chunk_longitude=OPEN_AND_EXTRACT(chunk_infile,'longitude')
      chunk_latitude=OPEN_AND_EXTRACT(chunk_infile,'latitude')
      DEFINE_BOUNDARIES,box,chunk_latitude,chunk_longitude,chunk_box_tx,/LIMIT
      chunk_nlon=N_ELEMENTS(chunk_longitude)
      chunk_nlat=N_ELEMENTS(chunk_latitude)
      
      chunk_percentiles=REFORM(OPEN_AND_EXTRACT(chunk_infile,'amount_at_percentile',$
                                                offset=[chunk_box_tx(1),chunk_box_tx(0),0],$
                                                count=[chunk_nlon,chunk_nlat,n_percentiles]))
      FOR i=0,n_percentiles-1 DO BEGIN
         temp=REFORM(chunk_percentiles(*,*,i))
         temp[where(mask eq 0)]=!Values.F_NaN
         chunk_percentiles_aavg(k,i)=MEAN(temp,/NaN)
      ENDFOR
      print,chunk_percentiles_aavg(k,*)
   ENDFOR
   
   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/qld_higem_dblco2_precip_percentiles_aavg.'+season_name+'_'+box_name+'_chunks.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=200,XOFFSET=0,YOFFSET=1000,TFONT=3,$
          TCHARSIZE=100,SPACE3=100
   GSET,XMIN=0,XMAX=100,YMIN=0,YMAX=ymax,TITLE='Mean rain amounts at each percentile - '+season_name+' - '+box_name
   GPLOT,X=percentiles,Y=ctl_percentiles_aavg,COL=FSC_COLOR('black')
;   GPLOT,X=percentiles,Y=ctl_percentiles_aavg+ctl_percentiles_stddev,COL=FSC_COLOR('black'),STYLE=2
;   GPLOT,X=percentiles,Y=ctl_percentiles_aavg-ctl_percentiles_stddev,COL=FSC_COLOR('black'),STYLE=2
   GPLOT,X=percentiles,Y=doubleco2_percentiles_aavg,COL=FSC_COLOR('red')
;   GPLOT,X=percentiles,Y=doubleco2_percentiles_aavg+doubleco2_percentiles_stddev,COL=FSC_COLOR('red'),STYLE=2
;   GPLOT,X=percentiles,Y=doubleco2_percentiles_aavg-doubleco2_percentiles_stddev,COL=FSC_COLOR('red'),STYLE=2
   FOR k=0,n_chunks-1 DO $
      GPLOT,X=percentiles,Y=REFORM(chunk_percentiles_aavg(k,*)),COL=FSC_COLOR('black'),STYLE=2
   AXES,XSTEP=5,XMINOR=2.5,YSTEP=ystep,YMINOR=ystep/2.,XTITLE='Percentile of daily rainfall',YTITLE='Precipitation (mm)',NDECS=1,/NORIGHT
   GSET,XMIN=0,XMAX=100,YMIN=ratio_min,YMAX=ratio_max
   GPLOT,X=percentiles,Y=doubleco2_percentiles_aavg/ctl_percentiles_aavg,COL=FSC_COLOR('blue')
   FOR k=0,n_chunks-1 DO $
      GPLOT,X=percentiles,Y=doubleco2_percentiles_aavg/REFORM(chunk_percentiles_aavg(k,*)),COL=FSC_COLOR('blue'),STYLE=2
   GPLOT,X=[0,100],Y=[1,1],COL=FSC_COLOR('blue'),STYLE=1
   AXES,YSTEP=0.05,YMINOR=0.025,YTITLE='Ratio of amounts: 2xCO2 to control',/ONLYRIGHT,NDECS=2
   GLEGEND,LEGPOS=1,$
           labels=REVERSE(['HiGEM control (150 years)','HiGEM control (30 year chunks)','HiGEM 2xCO2 (30 years)','Ratio: control (150 yrs) to 2xCO2','Ratio: control (30 yrs) to 2xCO2']),$
           COL=REVERSE(FSC_COLOR(['black','black','red','blue','blue'])),STYLE=REVERSE([0,2,0,0,2])
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/qld_higem_dblco2_precip_percentiles_aavg.'+season_name+'_'+box_name+'_chunks_90pct.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=200,XOFFSET=0,YOFFSET=1000,TFONT=3,$
          TCHARSIZE=100,SPACE3=100
   GSET,XMIN=90,XMAX=100,YMIN=0,YMAX=ymax,TITLE='Mean rain amounts at each percentile - '+season_name+' - '+box_name
   GPLOT,X=percentiles[where(percentiles ge 90)]+0.5,Y=ctl_percentiles_aavg[where(percentiles ge 90)],COL=FSC_COLOR('black')
;   GPLOT,X=percentiles,Y=ctl_percentiles_aavg+ctl_percentiles_stddev,COL=FSC_COLOR('black'),STYLE=2
;   GPLOT,X=percentiles,Y=ctl_percentiles_aavg-ctl_percentiles_stddev,COL=FSC_COLOR('black'),STYLE=2
   GPLOT,X=percentiles[where(percentiles ge 90)]+0.5,Y=doubleco2_percentiles_aavg[where(percentiles ge 90)],COL=FSC_COLOR('red')
;   GPLOT,X=percentiles,Y=doubleco2_percentiles_aavg+doubleco2_percentiles_stddev,COL=FSC_COLOR('red'),STYLE=2
;   GPLOT,X=percentiles,Y=doubleco2_percentiles_aavg-doubleco2_percentiles_stddev,COL=FSC_COLOR('red'),STYLE=2
   FOR k=0,n_chunks-1 DO BEGIN   
      toplot=REFORM(chunk_percentiles_aavg(k,*))
      GPLOT,X=percentiles[where(percentiles ge 90)]+0.5,Y=toplot[where(percentiles ge 90)],COL=FSC_COLOR('black'),STYLE=2
   ENDFOR
   AXES,XSTEP=1,XMINOR=0.5,YSTEP=ystep,YMINOR=ystep/2.,XTITLE='Percentile of daily rainfall',YTITLE='Precipitation (mm)',NDECS=1,/NORIGHT
   GSET,XMIN=90,XMAX=100,YMIN=ratio_min,YMAX=ratio_max
   ratio=doubleco2_percentiles_aavg/ctl_percentiles_aavg
   GPLOT,X=percentiles[where(percentiles ge 90)]+0.5,Y=ratio[where(percentiles ge 90)],COL=FSC_COLOR('blue')
   FOR k=0,n_chunks-1 DO BEGIN
      ratio=doubleco2_percentiles_aavg/REFORM(chunk_percentiles_aavg(k,*))
      GPLOT,X=percentiles[where(percentiles ge 90)]+0.5,Y=ratio[where(percentiles ge 90)],COL=FSC_COLOR('blue'),STYLE=2
   ENDFOR
   GPLOT,X=[90,100],Y=[1,1],COL=FSC_COLOR('blue'),STYLE=1
   AXES,YSTEP=0.05,YMINOR=0.025,YTITLE='Ratio of amounts: 2xCO2 to control',/ONLYRIGHT,NDECS=2
   GLEGEND,LEGPOS=1,$
           labels=REVERSE(['HiGEM control (150 years)','HiGEM control (30 year chunks)','HiGEM 2xCO2 (30 years)','Ratio: control (150 yrs) to 2xCO2','Ratio: control (30 yrs) to 2xCO2']),$
           COL=REVERSE(FSC_COLOR(['black','black','red','blue','blue'])),STYLE=REVERSE([0,2,0,0,2])
   PSCLOSE,/NOVIEW

ENDFOR

STOP
END

