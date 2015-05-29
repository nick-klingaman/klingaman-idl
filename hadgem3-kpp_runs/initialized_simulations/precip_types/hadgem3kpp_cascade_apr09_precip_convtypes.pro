PRO hadgem3kpp_cascade_apr09_precip_convtypes
  
; Create plots of the fraction of precipitation coming from shallow, mid and deep convection.

box=[-15,50,15,200]
mylevs_deep=['0.3','0.4','0.5','0.6','0.7','0.8','0.9','0.92','0.94','0.96','0.98','0.99']
mylevs_mid=['0.01','0.02','0.03','0.04','0.05','0.06','0.08','0.10','0.12','0.15','0.20','0.30']
mylevs_shallow=mylevs_mid
mylevs_diff=['-0.30','-0.20','-0.15','-0.10','-0.06','-0.03','-0.01','0.01','0.03','0.06','0.10','0.15','0.20','0.30']

n_models=2
FOR i=0,n_models-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         runid='xfadh'
         dirname='xfadh'
      END
      1 : BEGIN
         runid='xfadk'
         dirname='xfadk'
      END
   ENDCASE
   directory='/home/ss901165/um_output3/'+dirname
   shallowconv_infile=directory+'/'+runid+'a.06apr09-05may09.precip_shallowconv.nc'
   midconv_infile=directory+'/'+runid+'a.06apr09-05may09.precip_midconv.nc'
   deepconv_infile=directory+'/'+runid+'a.06apr09-05may09.precip_deepconv.nc'
   cvrain_infile=directory+'/'+runid+'a.06apr09-05may09.cvrain.nc'

   latitude=OPEN_AND_EXTRACT(shallowconv_infile,'latitude')
   longitude=OPEN_AND_EXTRACT(shallowconv_infile,'longitude')
   DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
   n_lon=N_ELEMENTS(longitude)
   n_lat=N_ELEMENTS(latitude)

   precip_shallowconv=REFORM(OPEN_AND_EXTRACT(shallowconv_infile,'precip_shallowconv',$
                                              offset=[box_tx(1),box_tx(0),0,0],$
                                              count=[n_lon,n_lat,1,720]))*1200.
   precip_midconv=REFORM(OPEN_AND_EXTRACT(midconv_infile,'precip_midconv',$
                                          offset=[box_tx(1),box_tx(0),0,0],$
                                          count=[n_lon,n_lat,1,720]))*1200.
   precip_deepconv=REFORM(OPEN_AND_EXTRACT(deepconv_infile,'precip_deepconv',$
                                           offset=[box_tx(1),box_tx(0),0,0],$
                                           count=[n_lon,n_lat,1,720]))*1200.
   precip_totalconv=REFORM(OPEN_AND_EXTRACT(cvrain_infile,'cvrain',$
                                            offset=[box_tx(1),box_tx(0),0,0],$
                                            count=[n_lon,n_lat,1,720]))

   precip_shallowconv_mean=fltarr(n_lon,n_lat)
   precip_midconv_mean=fltarr(n_lon,n_lat)
   precip_deepconv_mean=fltarr(n_lon,n_lat)   
   precip_totalconv_mean=fltarr(n_lon,n_lat)
  
   IF i eq 0 THEN BEGIN
      precip_shallowconv_frac=fltarr(n_models,n_lon,n_lat)
      precip_midconv_frac=fltarr(n_models,n_lon,n_lat)
      precip_deepconv_frac=fltarr(n_models,n_lon,n_lat)
   ENDIF

   FOR j=0,n_lon-1 DO $
      FOR k=0,n_lat-1 DO $
         precip_totalconv_mean(j,k)=MEAN(precip_totalconv(j,k,*))
   FOR j=0,n_lon-1 DO BEGIN
      FOR k=0,n_lat-1 DO BEGIN
         precip_shallowconv_mean(j,k)=MEAN(precip_shallowconv(j,k,*))
         precip_shallowconv_frac(i,j,k)=precip_shallowconv_mean(j,k)/precip_totalconv_mean(j,k)
         precip_midconv_mean(j,k)=MEAN(precip_midconv(j,k,*))
         precip_midconv_frac(i,j,k)=precip_midconv_mean(j,k)/precip_totalconv_mean(j,k)
         precip_deepconv_mean(j,k)=MEAN(precip_deepconv(j,k,*))
         precip_deepconv_frac(i,j,k)=precip_deepconv_mean(j,k)/precip_totalconv_mean(j,k)
      ENDFOR
   ENDFOR
   
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_types/hadgem3kpp_cascade_apr09_precip_convtypes.deepconv_frac.'+runid+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1000,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=8000
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_deep)+1
   LEVS,MANUAL=mylevs_deep
   MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3)
   CON,X=longitude,Y=latitude,FIELD=REFORM(precip_deepconv_frac(i,*,*)),$
       CB_TITLE='Fraction of convective precipitation from deep-convection routines',$
       /NOLINES,/BLOCK
   AXES,XSTEP=20,YSTEP=10,XMINOR=10,YMINOR=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_types/hadgem3kpp_cascade_apr09_precip_convtypes.midconv_frac.'+runid+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1000,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=8000
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_mid)+1
   LEVS,MANUAL=mylevs_mid
   MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3)
   CON,X=longitude,Y=latitude,FIELD=REFORM(precip_midconv_frac(i,*,*)),$
       CB_TITLE='Fraction of convective precipitation from mid-convection routines',$
       /NOLINES,/BLOCK
   AXES,XSTEP=20,YSTEP=10,XMINOR=10,YMINOR=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_types/hadgem3kpp_cascade_apr09_precip_convtypes.shallowconv_frac.'+runid+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1000,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=8000
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_shallow)+1
   LEVS,MANUAL=mylevs_shallow
   MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3)
   CON,X=longitude,Y=latitude,FIELD=REFORM(precip_shallowconv_frac(i,*,*)),$
       CB_TITLE='Fraction of convective precipitation from shallow-convection routines',$
       /NOLINES,/BLOCK
   AXES,XSTEP=20,YSTEP=10,XMINOR=10,YMINOR=5
   PSCLOSE,/NOVIEW
ENDFOR

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_types/hadgem3kpp_cascade_apr09_precip_convtypes.deepconv_frac.xfadk-minus-xfadh.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1000,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=8000
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1
LEVS,MANUAL=mylevs_diff
MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3)
CON,X=longitude,Y=latitude,FIELD=REFORM(precip_deepconv_frac(1,*,*))-REFORM(precip_deepconv_frac(0,*,*)),$
    CB_TITLE='Difference in fraction of convective precipitation from deep-convection routines',$
    /NOLINES,/BLOCK,TITLE='Diff in frac of conv precip from deep: 1.5*entrain - 1.0*entrain (xfadk-xfadh)'
AXES,XSTEP=20,YSTEP=10,XMINOR=10,YMINOR=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_types/hadgem3kpp_cascade_apr09_precip_convtypes.midconv_frac.xfadk-minus-xfadh.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1000,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=8000
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1
LEVS,MANUAL=mylevs_diff
MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3)
CON,X=longitude,Y=latitude,FIELD=REFORM(precip_midconv_frac(1,*,*))-REFORM(precip_midconv_frac(0,*,*)),$
    CB_TITLE='Difference in fraction of convective precipitation from mid-convection routines',$
    /NOLINES,/BLOCK,TITLE='Diff in frac of conv precip from mid: 1.5*entrain - 1.0*entrain (xfadk-xfadh)'
AXES,XSTEP=20,YSTEP=10,XMINOR=10,YMINOR=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_types/hadgem3kpp_cascade_apr09_precip_convtypes.shallowconv_frac.xfadk-minus-xfadh.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1000,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=8000
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1
LEVS,MANUAL=mylevs_diff
MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3)
CON,X=longitude,Y=latitude,FIELD=REFORM(precip_shallowconv_frac(1,*,*))-REFORM(precip_shallowconv_frac(0,*,*)),$
    CB_TITLE='Difference in fraction of convective precipitation from shallow-convection routines',$
    /NOLINES,/BLOCK,TITLE='Diff in frac of conv precip from shallow: 1.5*entrain - 1.0*entrain (xfadk-xfadh)'
AXES,XSTEP=20,YSTEP=10,XMINOR=10,YMINOR=5
PSCLOSE,/NOVIEW

latavg_range=[NEAREST(latitude,-5),NEAREST(latitude,5)]
precip_shallowconv_frac_latavg=fltarr(n_models,n_lon)
precip_midconv_frac_latavg=fltarr(n_models,n_lon)
precip_deepconv_frac_latavg=fltarr(n_models,n_lon)
FOR j=0,n_models-1 DO BEGIN
   FOR k=0,n_lon-1 DO BEGIN
      precip_shallowconv_frac_latavg(j,k)=MEAN(precip_shallowconv_frac(j,k,latavg_range(0):latavg_range(1)))
      precip_midconv_frac_latavg(j,k)=MEAN(precip_midconv_frac(j,k,latavg_range(0):latavg_range(1)))
      precip_deepconv_frac_latavg(j,k)=MEAN(precip_deepconv_frac(j,k,latavg_range(0):latavg_range(1)))
   ENDFOR
ENDFOR      

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_types/hadgem3kpp_cascade_apr09_precip_convtypes.latavg_5S-5N.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90
GSET,XMIN=box(1),XMAX=box(3),YMIN=0,YMAX=0.25,TITLE='Fraction of convective precipitation from diagnoses, latitude-averaged 5S-5N'
colors=['blue','red']
FOR j=0,n_models-1 DO BEGIN
   GPLOT,X=longitude,Y=REFORM(precip_shallowconv_frac_latavg(j,*)),COL=FSC_COLOR(colors(j)),STYLE=1
   GPLOT,X=longitude,Y=REFORM(precip_midconv_frac_latavg(j,*)),COL=FSC_COLOR(colors(j)),STYLE=2
ENDFOR
AXES,XSTEP=10,YVALS=['0.02','0.04','0.06','0.08','0.10','0.12','0.14'],XTITLE='Longitude (degrees east)',YTITLE='Fraction of convective precipitation from shallow and mid',/NORIGHT,NDECS=2
GSET,XMIN=MIN(longitude),XMAX=MAX(longitude),YMIN=0.75,YMAX=1.00
FOR j=0,n_models-1 DO $
   GPLOT,X=longitude,Y=REFORM(precip_deepconv_frac_latavg(j,*)),COL=FSC_COLOR(colors(j)),STYLE=0
AXES,XSTEP=10,YVALS=['0.84','0.86','0.88','0.90','0.92','0.94','0.96','0.98','1.00'],YTITLE='Fraction of convective precipitation from deep',NDECS=2,/ONLYRIGHT
model_labels=['Time-varying OSTIA SSTs (xfadh)','Time-varying OSTIA SSTs and 1.5*entrainment (xfadk)']
LEGEND,labels=REVERSE(model_labels),COL=REVERSE(FSC_COLOR(colors)),STYLE=REPLICATE(0,n_models),LEGPOS=2
conv_labels=['Shallow convection','Mid-level convection','Deep convection']
LEGEND,labels=REVERSE(conv_labels),COL=REPLICATE(FSC_COLOR("black"),3),STYLE=REVERSE([1,2,0]),LEGPOS=10

PSCLOSE

STOP

END
