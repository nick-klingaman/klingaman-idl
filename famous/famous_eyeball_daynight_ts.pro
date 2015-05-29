PRO famous_eyeball_daynight_ts

infile='/home/ss901165/um_output6/xhhbg/apy.nc'
lon=OPEN_AND_EXTRACT(infile,'longitude')
lat=OPEN_AND_EXTRACT(infile,'latitude')
nlon=N_ELEMENTS(lon)
nlat=N_ELEMENTS(lat)

solar_in=REFORM(OPEN_AND_EXTRACT(infile,'field200'))
ntime=N_ELEMENTS(solar_in(0,0,*))

weights=fltarr(nlon,nlat)
FOR i=0,nlat-1 DO $
   weights(i,*)=COS(lat*!Pi/180.)

varnames=['precip','iceconc','temp','field30','field1560','field1562','field1563','field1564']
vardescs=['Precipitation (mm day!U-1!N)','Ice cover (fraction)','Surface temperature (K)',$
          'Cloud cover (fraction)','CO!D2!N flux to ocean (1E8 kg m!U-2!N s!U-1!N)',$
          'CO!D2!N flux to land (1E8 kg m!U-2!N s!U-1!N)',$
          'CO!D2!N flux to atmosphere (1E8 kg m!U-2!N s!U-1!N)','CO!D2!N concentration (ppmv)']


weights_day=fltarr(nlon,nlat)
weights_night=fltarr(nlon,nlat)
temp=REFORM(solar_in(*,*,0))
FOR j=0,nlon-1 DO BEGIN
   FOR k=0,nlat-1 DO BEGIN
      IF temp(j,k) gt 0 THEN BEGIN
         weights_day(j,k)=weights(j,k)
         weights_night(j,k)=0.
      ENDIF ELSE BEGIN
         weights_day(j,k)=0.
         weights_night(j,k)=weights(j,k)
      ENDELSE
   ENDFOR
ENDFOR
weights_day=weights_day/TOTAL(weights_day)
weights_night=weights_night/TOTAL(weights_night)

;FOR i=0,N_ELEMENTS(varnames)-1 DO BEGIN
FOR i=0,N_ELEMENTS(varnames)-1 DO BEGIN
   CASE varnames(i) OF 
      'temp' : BEGIN
         day_ymin=220
         day_ymax=300
         day_ystep=10
         night_ymin=220
         night_ymax=300
         night_ystep=10
         mult=1.
      END
      'precip' : BEGIN
         day_ymin=0
         day_ymax=5
         day_ystep=0.5
         night_ymin=0
         night_ymax=1
         night_ystep=0.1
         mult=86400.
      END
      'iceconc' : BEGIN
         day_ymin=0.
         day_ymax=1.
         night_ymin=0.
         night_ymax=1.
         day_ystep=0.1
         night_ystep=0.1
         mult=1
      END
      'field30' : BEGIN
         day_ymin=0.
         day_ymax=1.
         night_ymin=0.
         night_ymax=01
         day_ystep=0.1
         night_ystep=0.1
         mult=1
      END
      'field1560' : BEGIN
         day_ymin=-2
         day_ymax=2
         night_ymin=-2
         night_ymax=2
         day_ystep=0.4
         night_ystep=0.4
         mult=1E8
      END      
      'field1562' : BEGIN
         day_ymin=-2
         day_ymax=2
         night_ymin=-2
         night_ymax=2
         day_ystep=0.4
         night_ystep=0.4
         mult=1E8
      END
      'field1563' : BEGIN
         day_ymin=-2
         day_ymax=2
         night_ymin=-2
         night_ymax=2
         day_ystep=0.4
         night_ystep=0.4
         mult=1E8
      END      
      'field1564' : BEGIN
         day_ymin=200
         day_ymax=500
         night_ymin=200
         night_ymax=500
         day_ystep=30
         night_ystep=30
         mult=6.57E5
      END
   ENDCASE
   
   field=REFORM(OPEN_AND_EXTRACT(infile,varnames(i),offset=[0,0,0,0],$
                                 count=[nlon,nlat,1,ntime])*mult)

   IF TOTAL(where(ABS(field) ge 1E9)) ge 0 THEN $
      field[where(ABS(field) ge 1E9)]=!Values.F_NaN

   day_mean=fltarr(ntime)
   night_mean=fltarr(ntime)
   day_lower=fltarr(ntime)
   night_lower=fltarr(ntime)
   day_upper=fltarr(ntime)
   night_upper=fltarr(ntime)
   
   FOR j=0,ntime-1 DO BEGIN
      temp=REFORM(field(*,*,j))
      day=temp[where(solar_in(*,*,0) gt 0)]
      night=temp[where(solar_in(*,*,0) eq 0)]
      day_mean(j)=TOTAL(REFORM(field(*,*,j))*weights_day,/NaN)
      day_sort=SORT(day)
      night_sort=SORT(night)
      day_lower(j)=day[day_sort(N_ELEMENTS(day_sort)/4)]
      day_upper(j)=day[day_sort(N_ELEMENTS(day_sort)*3/4)]
      night_lower(j)=night[night_sort(N_ELEMENTS(night_sort)/4)]
      night_upper(j)=night[night_sort(N_ELEMENTS(night_sort)*3/4)]
      night_mean(j)=TOTAL(REFORM(field(*,*,j))*weights_night,/NaN)      
   ENDFOR

   psfile='/home/ss901165/idl/famous/famous_eyeball_daynight_ts.xhhbg_'+varnames(i)+'_annual.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,MARGIN=3500,XOFFSET=500,SPACE3=100
   GSET,XMIN=0,XMAX=ntime,YMIN=day_ymin,YMAX=day_ymax,$
        TITLE='Eyeball Earth day- and night-mean timeseries for xhhbg (C-cycle, 1-day year, 12GMT)'
   GPLOT,X=findgen(ntime)+0.5,Y=day_mean,COL=FSC_COLOR('orange'),THICK=200
   GPLOT,X=findgen(ntime)+0.5,Y=day_upper,COL=FSC_COLOR('orange'),THICK=150,STYLE=2
   GPLOT,X=findgen(ntime)+0.5,Y=day_lower,COL=FSC_COLOR('orange'),THICK=150,STYLE=2
                                ;smoothed=SMOOTH(day_mean,11)
                                ;GPLOT,X=findgen(ntime-5)+2.5,Y=smoothed(2:ntime-3),COL=FSC_COLOR('red'),THICK=250
   AXES,XSTEP=5,XMINOR=1,YSTEP=day_ystep,YMINOR=day_ystep/2.,$
        YTITLE='Day-side mean '+vardescs(i),/NORIGHT,NDECS=2,XTITLE='Year of simulation'
   GSET,XMIN=0,XMAX=ntime,YMIN=night_ymin,YMAX=night_ymax
   GPLOT,X=findgen(ntime)+0.5,Y=night_mean,COL=FSC_COLOR('blue'),THICK=200
   GPLOT,X=findgen(ntime)+0.5,Y=night_upper,COL=FSC_COLOR('blue'),THICK=150,STYLE=2
   GPLOT,X=findgen(ntime)+0.5,Y=night_lower,COL=FSC_COLOR('blue'),THICK=150,STYLE=2
   AXES,YSTEP=night_ystep,YMINOR=night_ystep/2.,YTITLE='Night-side mean '+vardescs(i),/ONLYRIGHT,$
        NDECS=2
   PSCLOSE,/NOVIEW

ENDFOR

STOP
END

