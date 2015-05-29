PRO rio_tmax_enso

; As offset from 1979
en_years=[3,7,8,12,15,18,23,25,27,30]
en_years_real=en_years+1979
n_enyears=N_ELEMENTS(en_years)

; Read tmax data, subset for June/July
tmax=OPEN_AND_EXTRACT('ncseries78.nc','tmax')
time=OPEN_AND_EXTRACT('ncseries78.nc','time')
n_years=N_ELEMENTS(time)/365

tmax_mean_en=fltarr(n_enyears)
tmax_count_en=fltarr(n_enyears)
tmax_mean_all=0
tmax_count_all=0
FOR i=0,n_enyears-1 DO BEGIN
   this_year=REFORM(tmax(en_years(i)*365+150:en_years(i)*365+179))
   tmax_mean_en(i)=MEAN(this_year)
   tmax_count_en(i)=N_ELEMENTS(where(this_year ge 25))
ENDFOR
FOR i=0,n_years-1 DO BEGIN
   this_year=REFORM(tmax(365*i+150:365*i+179))
   tmax_mean_all=MEAN(this_year)/FLOAT(n_years)+tmax_mean_all
   tmax_count_all=N_ELEMENTS(where(this_year ge 25))/FLOAT(n_years)+tmax_count_all
ENDFOR

file='HadGHCND_TX_1981-1990_acts_jul12.txt'
openr,lun,file,/GET_LUN
mystr=''
stop_flag=0
FOR i=0,3 DO $
   readf,lun,mystr
rio_pts=fltarr(30*n_years)
count=0
FOR j=0,3649 DO BEGIN
   readf,lun,mystr
   temp=STRSPLIT(mystr,' ',/EXTRACT)
   print,temp(0)
   IF temp(0) eq -999 THEN STOP
   IF temp(1) ge 150 and temp(1) le 179 THEN BEGIN
      print,temp
      FOR i=0,41 DO $
         readf,lun,mystr
      readf,lun,mystr
      temp=STRSPLIT(mystr,' ',/EXTRACT)
      rio_pts(count)=temp(36)
      count=count+1
      FOR i=43,72 DO $
         readf,lun,mystr
      STOP
   ENDIF ELSE $
      FOR i=0,72 DO $
         readf,lun,mystr
ENDFOR

         

   

STOP
END

