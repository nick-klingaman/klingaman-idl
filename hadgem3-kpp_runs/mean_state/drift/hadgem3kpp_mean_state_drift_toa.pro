PRO hadgem3kpp_mean_state_drift_toa

;olr_file='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_ameans.years1-60.olr.nc'
;swup_file='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_ameans.years1-60.swup_toa.nc'
;swup_file='/home/ss901165/um_output6/xihvd/swup_ameans.nc'
;swdown_file='/home/ss901165/um_output6/xihvd/swdown_amean.nc'

;xjhwc='/home/ss901165/um_output6/xjhwc'
;olr_file=xjhwc+'/hadgem3kpp_nrglobal2_n216.jan-dec_ameans.years1-20.olr.nc'
;swup_file=xjhwc+'/hadgem3kpp_nrglobal2_n216.jan-dec_ameans.years1-20.sw_toa.nc'

xihvu='/home/ss901165/um_output6/xihvu'
olr_file=xihvu+'/hadgem3kpp_fwgbln216_1.5xentrain_ga30.jan-dec_ameans.years1-15.olr.nc'
swup_file=xihvu+'/hadgem3kpp_fwgbln216_1.5xentrain_ga30.jan-dec_ameans.years1-15.swup.nc'

lon=OPEN_AND_EXTRACT(olr_file,'longitude')
lat=OPEN_AND_EXTRACT(olr_file,'latitude')
nlon=N_ELEMENTS(lon)
nlat=N_ELEMENTS(lat)

olr=REFORM(OPEN_AND_EXTRACT(olr_file,'olr')) ;'toa_outgoing_longwave_flux'))
nyears=N_ELEMENTS(olr(0,0,*))
swup=REFORM(OPEN_AND_EXTRACT(swup_file,'field201')) ;'PP_1_1208_vn708'))
;swdown=REFORM(OPEN_AND_EXTRACT(swdown_file,'field200'))

weights=fltarr(nlon,nlat)
FOR i=0,nlat-1 DO $
   weights(*,i)=COS(lat(i)*!Pi/180.)
weights=weights/TOTAL(weights)

olr_aavg=fltarr(nyears)
swup_aavg=fltarr(nyears)
FOR i=0,nyears-1 DO BEGIN
   olr_aavg(i)=TOTAL(REFORM(olr(*,*,i))*weights)
   swup_aavg(i)=TOTAL(REFORM(swup(*,*,i))*weights)
ENDFOR
swdown_aavg=341.41 
;swdown_aavg=TOTAL(REFORM(swdown*weights))

psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/drift/hadgem3kpp_mean_state_drift_toa.xihvu.ps'
PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,TCHARSIZE=110,MARGIN=4000,XOFFSET=1000,YOFFSET=1000
GSET,XMIN=0,XMAX=nyears,YMIN=241,YMAX=250,TITLE='TOA radiative balance in K!D50!N-ENT-OBS (xihvd)'
GPLOT,X=indgen(nyears)+0.5,Y=olr_aavg,COL=FSC_COLOR('blue')
GPLOT,X=indgen(nyears)+0.5,Y=swdown_aavg-swup_aavg,COL=FSC_COLOR('red')
AXES,XSTEP=6,XMINOR=3,YSTEP=0.5,YMINOR=0.25,NDECS=2,$
     XTITLE='Year of simulation',YTITLE='Area-weighted annual-mean radiation (W m!U-2!N)',/NORIGHT
GSET,XMIN=0,XMAX=nyears,YMIN=-1,YMAX=5
GPLOT,X=indgen(nyears)+0.5,Y=(swdown_aavg-swup_aavg)-olr_aavg,$
      COL=FSC_COLOR('black')
GPLOT,X=[0,nyears],Y=[0,0],STYLE=1
GLEGEND,labels=['OLR','SW!Dd!N - SW!Du!N','Balance'],COL=FSC_COLOR(['blue','red','black']),$
        LEGPOS=9
AXES,YSTEP=0.5,YMINOR=0.25,NDECS=2,YTITLE='Radiative balance (W m!U-2!N, positive down)',/ONLYRIGHT
PSCLOSE

STOP
END
