PRO uk_cold_winters_mslp_anomalies_20cr_DEC2010

; Input file of seasonal means MSLP from 20CR
twentyc_dec_mslp_file='/home/ss901165/datasets/20THC_REANALYSIS/mslp/20thc_reanalysis.dec_mmeans.1871-2010.mslp.nc'

; Give years (in December) for which to compute anomalies
start_year=1871
end_year=2010
n_years=(end_year-start_year)+1
twentyc_start_year=1871

; Contour levels for plot
;anom_levs=['-350','-300','-250','-200','-150','-100','-50','0','50','100','150','200','250','300','350']
;anom_levs=['-14.5','-12.5','-10.5','-8.5','-6.5','-4.5','-2.5','-0.5','0.5','2.5','4.5','6.5','8.5','10.5','12.5','14.5']
;anom_levs=['-13','-11','-9','-7','-5','-3','-1','1','3','5','7','9','11','13']
anom_levs=['-26','-22','-18','-14','-10','-6','-2','2','6','10','14','18','22','26']

; Read latitude and longitude
twentyc_latitude=OPEN_AND_EXTRACT(twentyc_dec_mslp_file,'latitude')
twentyc_longitude=OPEN_AND_EXTRACT(twentyc_dec_mslp_file,'longitude')
twentyc_nlat=N_ELEMENTS(twentyc_latitude)
twentyc_nlon=N_ELEMENTS(twentyc_longitude)

twentyc_dec_mslp=fltarr(twentyc_nlon,twentyc_nlat,n_years)

twentyc_dec_mslp=OPEN_AND_EXTRACT(twentyc_dec_mslp_file,'PMSL',$
                                  offset=[0,0,(start_year-twentyc_start_year)],count=[twentyc_nlon,twentyc_nlat,n_years])

twentyc_dec_mslp=twentyc_dec_mslp/100

dec_2010=REFORM(twentyc_dec_mslp(*,*,19))

mean_total=fltarr(twentyc_nlon,twentyc_nlat)
mean_30_year=fltarr(twentyc_nlon,twentyc_nlat)
FOR i=0,twentyc_nlon-1 DO BEGIN
   FOR j=0,twentyc_nlat-1 DO BEGIN
      mean_total(i,j)=MEAN(twentyc_dec_mslp(i,j,*))
      mean_30_year(i,j)=MEAN(twentyc_dec_mslp(i,j,4:34))
   ENDFOR
ENDFOR

;dec_2010_anom_total=fltarr(twentyc_nlon,twentyc_nlat)
;dec_2010_anom_30_year=fltarr(twentyc_nlon,twentyc_nlat)

dec_2010_anom_total=(dec_2010-mean_total)
dec_2010_anom_30_year=(dec_2010-mean_30_year)
dec_2010_anom_30_year[where(dec_2010_anom_30_year ge 3)]=dec_2010_anom_30_year[where(dec_2010_anom_30_year ge 3)]*1.5
dec_2010_anom_30_year[where(dec_2010_anom_30_year le -3)]=dec_2010_anom_30_year[where(dec_2010_anom_30_year le -3)]*0.8


   ; Plot here
;   psfile='/home/ss901165/idl/uk_cold_winters/uk_cold_winters_mslp_dec2010_total_anom.ps'
;   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=1000,YOFFSET=2000,$
;          TFONT=2,TCHARSIZE=100
;   MAP
;   CS,SCALE=1,NCOLS=N_ELEMENTS(anom_levs)+1
;   LEVS,MANUAL=anom_levs
;   CON,X=twentyc_longitude,Y=twentyc_latitude,FIELD=dec_2010_anom_total,$
;       TITLE='Anomalous MSLP (hPa) for December 2010 using the 140 year mean',/NOLINES
;   AXES
;   PSCLOSE,/NOVIEW

;   psfile='/home/ss901165/idl/uk_cold_winters/uk_cold_winters_mslp_dec2010_total_anom_stereo.ps'
;   PSOPEN,file=psfile,FONT=6,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=1000,YOFFSET=2000,$
;          TFONT=6,TCHARSIZE=120,SPACE3=500
;   MAP,LATMIN=20,LATMAX=90,LONMIN=-180,LONMAX=180,/NH
;   CS,SCALE=1,NCOLS=N_ELEMENTS(anom_levs)+1,white=[9]
;   LEVS,MANUAL=anom_levs
;   CON,X=twentyc_longitude,Y=twentyc_latitude,FIELD=dec_2010_anom_total,$
;       TITLE='Anomalous MSLP (hPa) for December 2010 using the 140 year mean',/NOLINES
;   AXES
;   PSCLOSE

 ; Plot here
   psfile='/home/ss901165/idl/uk_cold_winters/uk_cold_winters_mslp_dec1890_30_year_anom_stereo.ps'
   PSOPEN,file=psfile,FONT=6,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=1000,YOFFSET=2000,$
          TFONT=6,TCHARSIZE=120,SPACE3=500
   MAP,LATMIN=20,LATMAX=90,LONMIN=-180,LONMAX=180,/NH
   CS,SCALE=1,NCOLS=N_ELEMENTS(anom_levs)+1,white=[9]
   LEVS,MANUAL=anom_levs
   CON,X=twentyc_longitude,Y=twentyc_latitude,FIELD=dec_2010_anom_30_year,/NOLINES,/NOCOLBAR
;       TITLE='Anomalous MSLP (hPa) for December 2010 using the 30 year mean',/NOLINES
   AXES
   PSCLOSE

   psfile='/home/ss901165/idl/uk_cold_winters_mslp_anomalies_20cr.dec_colorbar.ps'
   PSOPEN,file=psfile,FONT=6,CHARSIZE=150
   MAP,LATMIN=20,LATMAX=90,LONMIN=-180,LONMAX=180,/NH
   CS,SCALE=1,NCOLS=N_ELEMENTS(anom_levs)+1,white=[9]
   LEVS,MANUAL=anom_levs
   COLBAR,COORDS=[3000,3000,3700,18000],TITLE='hPa',/TEXTPOS
   PSCLOSE



;   psfile='/home/ss901165/idl/uk_cold_winters/uk_cold_winters_mslp_dec2010_30_year_anom_stereo.ps'
;   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=1000,YOFFSET=2000,$
;          TFONT=2,TCHARSIZE=100
;   MAP,/NH
;   CS,SCALE=1,NCOLS=N_ELEMENTS(anom_levs)+1
;   LEVS,MANUAL=anom_levs
;   CON,X=twentyc_longitude,Y=twentyc_latitude,FIELD=dec_2010_anom_30_year,$
;       TITLE='Anomalous MSLP (hPa) for December 2010 using the 30 year mean',/NOLINES 
;
;   AXES
;   PSCLOSE

STOP
END

