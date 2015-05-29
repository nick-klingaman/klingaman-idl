PRO hadgem3kpp_cascade_jun02case_monsoon_precip_patterncorr

; Box to plot
box=[-10,60,31,100]
pcorr_box=[10,70,25,90]

; Observed Indian rainfall
trmm_infile='/home/ss901165/datasets_mango/TRMM_3B42V6/n96/TRMM_3B42v6A.jan-dec_dmeans.1999-2010.n96.nc'
trmm_latitude=OPEN_AND_EXTRACT(trmm_infile,'latitude')
trmm_longitude=OPEN_AND_EXTRACT(trmm_infile,'longitude')
DEFINE_BOUNDARIES,box,trmm_latitude,trmm_longitude,trmm_box_tx,/LIMIT
DEFINE_BOUNDARIES,pcorr_box,trmm_latitude,trmm_longitude,pcorr_box_tx
trmm_nlat=N_ELEMENTS(trmm_latitude)
trmm_nlon=N_ELEMENTS(trmm_longitude)
trmm_year_offset=3
trmm_offset=177
trmm_ndays=20

trmm_rainfall=OPEN_AND_EXTRACT(trmm_infile,'precip',$
                               offset=[trmm_box_tx(1),trmm_box_tx(0),trmm_offset,trmm_year_offset],$
                               count=[trmm_nlon,trmm_nlat,trmm_ndays,1])
;trmm_rainfall[where(trmm_rainfall eq 2e20)]=!Values.F_NaN

mask_infile='/home/ss901165/um_output/mask_n96_hadgem3-7.1.nc'
mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)
mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                             offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                             count=[mask_nlon,mask_nlat,1,1]))

trmm_rainfall_avg=fltarr(trmm_nlon,trmm_nlat)
FOR i=0,trmm_nlon-1 DO $
   FOR j=0,trmm_nlat-1 DO $
      trmm_rainfall_avg(i,j)=MEAN(trmm_rainfall(i,j,*),/NaN)

mylevs=['0.5','1.5','2.5','3.5','4.5','5.5','6.5','8.5','10.5','12.5','14.5','16.5','18.5','20.5']
mylevs_diff=['-8.5','-6.5','-4.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','4.5','6.5','8.5']

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/monsoon_precip/hadgem3kpp_cascade_jun02case_monsoon_precip_patterncorr.trmm_n96.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1500,SPACE3=500,XOFFSET=1500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
       XSIZE=18000,SPACE2=1200
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV
LEVS,MANUAL=mylevs
MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3),/hires
CON,X=trmm_longitude,Y=trmm_latitude,FIELD=trmm_rainfall_avg,/NOLINES,/BLOCK,$
    TITLE='Mean precipitation over 26 June - 15 July from TRMM at N96',CB_TITLE='Precipitation (mm day!U-1!N)'
AXES,XSTEP=10,XMINOR=5,YSTEP=5,YMINOR=2.5
PSCLOSE,/NOVIEW

;trmm_rainfall_avg[where(mask lt 1)]=!Values.F_NaN
;trmm_rainfall_avg(0:pcorr_box_tx(1)-1,*)=!Values.F_NaN
;trmm_rainfall_avg(pcorr_box_tx(3):trmm_nlon-1,*)=!Values.F_NaN
;trmm_rainfall_avg(*,0:pcorr_box_tx(0)-1)=!Values.F_NaN
;trmm_rainfall_avg(*,pcorr_box_tx(2):trmm_nlat-1)=!Values.F_NaN

n_models=4
model_basedir='/home/ss901165/um_output3'
FOR i=0,n_models-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         runid='xftge'
         date='15jun02'
         n_days=20
         offset=10
         color='brown'
         style=0   
      END
      1 : BEGIN
         runid='xftgf'
         date='15jun02'
         n_days=20
         offset=10
         color='red'
         style=0
      END
      2 : BEGIN
         runid='xftgg'
         date='15jun02'
         n_days=20
         offset=10
         color='blue'
         style=0
      END
      3 : BEGIN
         runid='xftgh'
         date='15jun02'
         n_days=20
         offset=10
         color='purple'
         style=0
      END
   ENDCASE
   model_infile=model_basedir+'/'+runid+'/'+runid+'a.'+date+'_precip.nc'
   model_latitude=OPEN_AND_EXTRACT(model_infile,'latitude')
   model_longitude=OPEN_AND_EXTRACT(model_infile,'longitude')
   DEFINE_BOUNDARIES,box,model_latitude,model_longitude,model_box_tx,/LIMIT
   DEFINE_BOUNDARIES,pcorr_box,model_latitude,model_longitude,pcorr_box_tx
   model_nlon=N_ELEMENTS(model_longitude)
   model_nlat=N_ELEMENTS(model_latitude)
   
   model_rainfall=REFORM(OPEN_AND_EXTRACT(model_infile,'precip',$
                                          offset=[model_box_tx(1),model_box_tx(0),offset],$
                                          count=[model_nlon,model_nlat,n_days]))*86400.
   model_rainfall_avg=fltarr(model_nlon,model_nlat)
   FOR j=0,model_nlon-1 DO $
      FOR k=0,model_nlat-1 DO $
         model_rainfall_avg(j,k)=MEAN(model_rainfall(j,k,*))

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/monsoon_precip/hadgem3kpp_cascade_jun02case_monsoon_precip_patterncorr.'+runid+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1500,SPACE3=500,XOFFSET=1500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          XSIZE=18000,SPACE2=1200
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   LEVS,MANUAL=mylevs
   MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3),/hires
   CON,X=model_longitude,Y=model_latitude,FIELD=model_rainfall_avg,/NOLINES,/BLOCK,$
       TITLE='Mean precipitation over 26 June - 15 July from '+runid,CB_TITLE='Precipitation (mm day!U-1!N)'
   GPLOT,Y=[pcorr_box(0),pcorr_box(2)],X=[pcorr_box(1),pcorr_box(1)],STYLE=2,THICK=150
   GPLOT,Y=[pcorr_box(0),pcorr_box(2)],X=[pcorr_box(3),pcorr_box(3)],STYLE=2,THICK=150
   GPLOT,Y=[pcorr_box(0),pcorr_box(0)],X=[pcorr_box(1),pcorr_box(3)],STYLE=2,THICK=150
   GPLOT,Y=[pcorr_box(2),pcorr_box(2)],X=[pcorr_box(1),pcorr_box(3)],STYLE=2,THICK=150
   
   ;model_rainfall_avg[where(mask lt 1)]=!Values.F_NaN
   ;model_rainfall_avg(0:pcorr_box_tx(1)-1,*)=!Values.F_NaN
   ;model_rainfall_avg(pcorr_box_tx(3):model_nlon-1,*)=!Values.F_NaN
   ;model_rainfall_avg(*,0:pcorr_box_tx(0)-1)=!Values.F_NaN
   ;model_rainfall_avg(*,pcorr_box_tx(2):model_nlat-1)=!Values.F_NaN

   ;print,(TOTAL(model_rainfall_avg*trmm_rainfall_avg,/NaN)-(N_ELEMENTS(where(FINITE(model_rainfall_avg) eq 1))*MEAN(model_rainfall_avg,/NaN)*$
   ;                                                         MEAN(trmm_rainfall_avg,/NaN)))/$
   ;       ((N_ELEMENTS(where(FINITE(model_rainfall_avg) eq 1))-1)*STDDEV(model_rainfall_avg,/NaN)*STDDEV(trmm_rainfall_avg,/NaN))

   AXES,XSTEP=10,XMINOR=5,YSTEP=5,YMINOR=2.5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/monsoon_precip/hadgem3kpp_cascade_jun02case_monsoon_precip_patterncorr.'+runid+'_diff_trmm.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1500,SPACE3=500,XOFFSET=1500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          XSIZE=18000,SPACE2=1200
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV
   LEVS,MANUAL=mylevs_diff
   MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3),/hires
   diff_precip=fltarr(model_nlon,model_nlat)
   FOR j=0,model_nlat-1 DO $
      diff_precip(*,j)=model_rainfall_avg(*,j)-trmm_rainfall_avg(*,trmm_nlat-j-1)
   CON,X=model_longitude,Y=model_latitude(0:model_nlat-1),FIELD=diff_precip,/NOLINES,/BLOCK,$
       TITLE='Difference in mean precipitation over 26 June - 15 July for '+runid+' minus TRMM',CB_TITLE='Precipitation (mm day!U-1!N)'
   GPLOT,Y=[pcorr_box(0),pcorr_box(2)],X=[pcorr_box(1),pcorr_box(1)],STYLE=2,THICK=150
   GPLOT,Y=[pcorr_box(0),pcorr_box(2)],X=[pcorr_box(3),pcorr_box(3)],STYLE=2,THICK=150
   GPLOT,Y=[pcorr_box(0),pcorr_box(0)],X=[pcorr_box(1),pcorr_box(3)],STYLE=2,THICK=150
   GPLOT,Y=[pcorr_box(2),pcorr_box(2)],X=[pcorr_box(1),pcorr_box(3)],STYLE=2,THICK=150
   AXES,XSTEP=10,XMINOR=5,YSTEP=5,YMINOR=2.5
   PSCLOSE,/NOVIEW

ENDFOR

STOP
END

