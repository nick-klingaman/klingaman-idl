PRO hadgem3kpp_cascade_sep00case_monsoon_precip

; Box
box=[10,70,25,90]

; Observed Indian rainfall
trmm_infile='/home/ss901165/datasets_mango/TRMM_3B42V6/n96/TRMM_3B42v6A.jan-dec_dmeans.1999-2010.n96.nc'
trmm_latitude=OPEN_AND_EXTRACT(trmm_infile,'latitude')
trmm_longitude=OPEN_AND_EXTRACT(trmm_infile,'longitude')
DEFINE_BOUNDARIES,box,trmm_latitude,trmm_longitude,trmm_box_tx,/LIMIT
trmm_nlat=N_ELEMENTS(trmm_latitude)
trmm_nlon=N_ELEMENTS(trmm_longitude)
trmm_year_offset=1
trmm_offset=262
trmm_ndays=63

trmm_rainfall=REFORM(OPEN_AND_EXTRACT(trmm_infile,'precip',$
                                      offset=[trmm_box_tx(1),trmm_box_tx(0),trmm_offset,trmm_year_offset],$
                                      count=[trmm_nlon,trmm_nlat,trmm_ndays,1]))
IF TOTAL(where(trmm_rainfall eq 2e20)) ne -1 THEN $
   trmm_rainfall[where(trmm_rainfall eq 2e20)]=!Values.F_NaN

mask_infile='/home/ss901165/um_output/mask_n96_hadgem3-7.1.nc'
mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)
mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                             offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                             count=[mask_nlon,mask_nlat,1,1]))

trmm_rainfall_aavg=fltarr(trmm_ndays)
FOR i=0,trmm_ndays-1 DO BEGIN
   temp=REFORM(trmm_rainfall(*,*,i))
   temp[where(mask lt 1)]=!Values.F_NaN
   trmm_rainfall_aavg(i)=MEAN(temp,/NaN)
ENDFOR


psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/monsoon_precip/hadgem3kpp_cascade_sep00case_monsoon_precip.all_india.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1500,SPACE3=1500,XOFFSET=1500,YOFFSET=1500,TFONT=2,TCHARSIZE=100
GSET,XMIN=0,XMAX=trmm_ndays,YMIN=0,YMAX=15
smooth_aavg=SMOOTH(trmm_rainfall_aavg,7)
GPLOT,X=indgen(trmm_ndays)+3.5,Y=smooth_aavg(3:trmm_ndays-4),COL=FSC_COLOR('black')

n_models=8
model_basedir='/home/ss901165/um_output3'
FOR i=0,n_models-1 DO BEGIN
   CASE i OF
      0: BEGIN
         runid='xfseu'
         date='28sep00'
         n_days=30
         offset=8
         color='red'
         style=2
      END
      1 : BEGIN
         runid='xfsev'
         date='28sep00'
         n_days=30
         offset=8
         color='blue'
         style=2
      END
      2 : BEGIN
         runid='xfsew'
         date='28sep00'
         n_days=30
         offset=8
         color='brown'
         style=2
      END
      3 : BEGIN
         runid='xfsex'
         date='28sep00'
         n_days=30
         offset=8
         color='purple'
         style=2
      END
      4 : BEGIN
         runid='xftgu'
         date='08oct00'
         n_days=30
         offset=18
         color='red'
         style=0   
      END
      5 : BEGIN
         runid='xftgv'
         date='08oct00'
         n_days=30
         offset=18
         color='brown'
         style=0
      END
      6 : BEGIN
         runid='xftgw'
         date='08oct00'
         n_days=30
         offset=18
         color='blue'
         style=0
      END
      7 : BEGIN
         runid='xftgx'
         date='08oct00'
         n_days=30
         offset=18
         color='purple'
         style=0
      END
   ENDCASE
   model_infile=model_basedir+'/'+runid+'/'+runid+'a.'+date+'_precip.nc'
   model_latitude=OPEN_AND_EXTRACT(model_infile,'latitude')
   model_longitude=OPEN_AND_EXTRACT(model_infile,'longitude')
   DEFINE_BOUNDARIES,box,model_latitude,model_longitude,model_box_tx,/LIMIT
   model_nlon=N_ELEMENTS(model_longitude)
   model_nlat=N_ELEMENTS(model_latitude)
   
   model_rainfall=REFORM(OPEN_AND_EXTRACT(model_infile,'precip',$
                                   offset=[model_box_tx(1),model_box_tx(0),0],$
                                   count=[model_nlon,model_nlat,n_days]))*86400.
   model_rainfall_aavg=fltarr(n_days)
   FOR j=0,n_days-1 DO BEGIN
      temp=REFORM(model_rainfall(*,*,j))
      temp[where(mask lt 1)]=!Values.F_NaN
      model_rainfall_aavg(j)=MEAN(temp,/NaN)
   ENDFOR
   smooth_aavg=SMOOTH(model_rainfall_aavg,7)
   GPLOT,X=indgen(n_days)+0.5+offset+3,Y=smooth_aavg(3:n_days-4),COL=FSC_COLOR(color),$
         STYLE=style
;   GPLOT,X=indgen(n_days)+0.5+offset+3,Y=model_rainfall_aavg,COL=FSC_COLOR(color),STYLE=style
ENDFOR

AXES,YSTEP=1,YMINOR=0.5,YTITLE='7-day smoothed area-averaged rainfall over (10!Uo!N-25!Uo!NN, 70!Uo!N-90!Uo!NE), land boxes only (mm day!U-1!N)',XVALS=[0,5,10,15,20,25,30,35,40,45,50,55,60],XLABELS=['20 Sep','25 Sep','30 Sep','5 Oct','10 Oct','15 Oct','20 Oct','25 Oct','30 Oct','5 Nov','10 Nov','15 Nov','20 Nov']

labels=['TRMM at N96','Control','1.5x entrainment','No CMT','1.5x entrainment and no CMT']
GLEGEND,labels=REVERSE(labels),COL=REVERSE(FSC_COLOR(['black','red','blue','brown','purple'])),$
        STYLE=[0,0,0,0,0],LEGPOS=9
labels=['Initialized 28 Sep','Initialized 8 Oct']
GLEGEND,labels=REVERSE(labels),COL=[FSC_COLOR('black'),FSC_COLOR('black')],STYLE=REVERSE([2,0]),$
        LEGPOS=10

PSCLOSE

STOP
END

