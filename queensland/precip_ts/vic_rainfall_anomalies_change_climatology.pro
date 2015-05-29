PRO vic_rainfall_anomalies_change_climatology

infile='/home/ss901165/datasets/AUS_RAIN_GAUGE/data/VIC2008/vic_hq_rain_gauges.jan-dec_dmeans.1920-2007.nc'

noleap_month_ndays=[31,28,31,30,31,30,31,31,30,31,30,31]
leap_month_ndays=[31,29,31,30,31,30,31,31,30,31,30,31]

station_numbers=OPEN_AND_EXTRACT(infile,'station_number')
mel_station=NEAREST(station_numbers,86071)

mel_rainfall=REFORM(OPEN_AND_EXTRACT(infile,'rainfall',offset=[0,mel_station],count=[32143,1]))

start_year=1920
n_years=88
n_months=n_years*12
mel_rainfall_monthly=fltarr(n_months)

thisyear_start=0
FOR i=0,n_years-1 DO BEGIN
   ;print,((i+start_year) mod 4)
   IF (i+start_year) mod 4 eq 0 THEN BEGIN
      ;print,'leap year ',i+start_year 
      month_ndays=leap_month_ndays
   ENDIF ELSE $
      month_ndays=noleap_month_ndays
   thismonth_start=thisyear_start
   FOR j=0,11 DO BEGIN
      thismonth_stop=thismonth_start+month_ndays(j)
      mel_rainfall_monthly(i*12+j)=TOTAL(mel_rainfall(thismonth_start:thismonth_stop-1))
      thismonth_start=thismonth_stop
   ENDFOR
   thisyear_start=thismonth_start
ENDFOR

climatology_start=[(1961-start_year)*12,(1951-start_year)*12,(1941-start_year)*12,(1931-start_year)*12,(1921-start_year)*12]
climatology_stop=[(1990-start_year)*12,(1980-start_year)*12,(1970-start_year)*12,(1960-start_year)*12,(1950-start_year)*12]-1
n_climatologies=N_ELEMENTS(climatology_start)

mel_rainfall_anomalies=fltarr(n_climatologies,n_months)
mel_rainfall_anomalies_sum=fltarr(n_climatologies,n_months)
mel_rainfall_anomalies_sumfrom1960=fltarr(n_climatologies,(2007-1960+1)*12)
offset_jan1960=(1960-start_year)*12
mel_rainfall_climatologies=fltarr(n_climatologies,12)
FOR i=0,n_climatologies-1 DO BEGIN
   FOR j=0,11 DO $
      mel_rainfall_climatologies(i,j)=MEAN(mel_rainfall_monthly(climatology_start(i)+j:climatology_stop(i):12))
   FOR j=0,n_months-1 DO BEGIN
      clim_month=(j mod 12)
      mel_rainfall_anomalies(i,j)=mel_rainfall_monthly(j)-mel_rainfall_climatologies(i,clim_month)
      mel_rainfall_anomalies_sum(i,j)=TOTAL(mel_rainfall_anomalies(i,0:j))
      IF j ge (1960-start_year)*12 THEN $
         mel_rainfall_anomalies_sumfrom1960(i,j-offset_jan1960)=$
         TOTAL(mel_rainfall_anomalies(i,offset_jan1960:j))
   ENDFOR
ENDFOR

psfile='/home/ss901165/idl/queensland/precip_ts/vic_rainfall_anomalies_change_climatology.1920-2007_sum_anomalies.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=500,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80,SPACE1=100
GSET,XMIN=0,XMAX=(2007-start_year+1)*12,YMIN=-3500,YMAX=2500
colors=['red','blue','purple','brown','cyan']
FOR i=0,n_climatologies-1 DO BEGIN
   GPLOT,X=indgen(n_months),Y=REFORM(mel_rainfall_anomalies_sum(i,*)),COL=FSC_COLOR(colors(i))
   GPLOT,X=12,Y=-1500-(i*300),TEXT='Magnitude of 1996-present drying: '+$
         STRMID(STRTRIM(STRING(mel_rainfall_anomalies_sum(i,920)-mel_rainfall_anomalies_sum(i,n_months-1)),1),0,7)+' mm',$
         COL=FSC_COLOR(colors(i)),ALIGN=0.0,CHARSIZE=120
ENDFOR
AXES,XVALS=indgen(n_years/2)*24,XLABELS=indgen(n_years/2)*2+start_year,YSTEP=500,YMINOR=250,ORIENTATION=40,$
     XTITLE='Year',YTITLE='Accumulated rainfall anomaly at Melbourne Regional Office (mm)'
LEGEND,labels=REVERSE(['1961-1990','1951-1980','1941-1970','1931-1960','1921-1950']),$
       COL=REVERSE(FSC_COLOR(colors)),LEGPOS=1
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/precip_ts/vic_rainfall_anomalies_change_climatology.1960-2007_sum_anomalies.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=500,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80,SPACE1=100
GSET,XMIN=0,XMAX=(2007-1960+1)*12,YMIN=-2750,YMAX=1000
colors=['red','blue','purple','brown','cyan']
FOR i=0,n_climatologies-1 DO $
   GPLOT,X=indgen(n_months-offset_jan1960),Y=REFORM(mel_rainfall_anomalies_sumfrom1960(i,*)),COL=FSC_COLOR(colors(i))
AXES,XVALS=indgen((2007-1960+1)/2)*24,XLABELS=indgen((2007-1960+1)/2)*2+1960,YSTEP=250,YMINOR=125,ORIENTATION=40,$
     XTITLE='Year',YTITLE='Accumulated rainfall anomaly at Melbourne Regional Office (mm)'
LEGEND,labels=REVERSE(['1961-1990','1951-1980','1941-1970','1931-1960','1921-1950']),$
       COL=REVERSE(FSC_COLOR(colors)),LEGPOS=1
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/precip_ts/vic_rainfall_anomalies_change_climatology.1960-2007_rainfall.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=500,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80,SPACE1=200
GSET,XMIN=0,XMAX=(2007-start_year+1)*12,YMIN=0,YMAX=240
GPLOT,X=indgen(n_months),Y=REFORM(mel_rainfall_monthly),COL=FSC_COLOR('black')
temp=SMOOTH(mel_rainfall_monthly,61)
GPLOT,X=indgen(n_months-60)+30,Y=temp(30:n_months-31),COL=FSC_COLOR('red')
AXES,XVALS=indgen(n_years/2)*24,XLABELS=indgen(n_years/2)*2+start_year,YSTEP=20,YMINOR=10,ORIENTATION=40,$
     XTITLE='Year',YTITLE='Rainfall at Melbourne Regional Office (mm month!U-1!N)'
PSCLOSE

STOP

END
