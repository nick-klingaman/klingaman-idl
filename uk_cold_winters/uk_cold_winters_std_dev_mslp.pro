PRO uk_cold_winters_std_dev_mslp

; Input file of seasonal means MSLP from 20CR
twentyc_mslp_file='/home/ss901165/datasets/20THC_REANALYSIS/every_member/mslp/20thc_reanalysis_allmembers.dec-feb_smeans.1871-2007.mslp.nc'

; Give years (in December) for which to compute anomalies
years=[1878,1890,1894,1916,1939,1946,1962]

twentyc_nmembers=56

; Give starting year (in December) of the 20CR file
twentyc_start_year=1871


n_years=N_ELEMENTS(years)
; Contour levels for plot
anom_levs=['0','20','40','60','80','100','120','140','160','180','200','220','240','260']

; Read latitude and longitude
twentyc_latitude=OPEN_AND_EXTRACT(twentyc_mslp_file,'lat')
twentyc_longitude=OPEN_AND_EXTRACT(twentyc_mslp_file,'lon')
twentyc_nlat=N_ELEMENTS(twentyc_latitude)
twentyc_nlon=N_ELEMENTS(twentyc_longitude)



; Read climatological wind components


std_dev_mslp=fltarr(twentyc_nlon,twentyc_nlat)
 FOR i=0,n_years-1 DO BEGIN 
    FOR j=0,twentyc_nlon-1 DO BEGIN 
       FOR k=0,twentyc_nlat-1 DO BEGIN 
        

          twentyc_mslp=OPEN_AND_EXTRACT(twentyc_mslp_file,'prmsl',$
                                        offset=[0,0,0,years(i)-twentyc_start_year], $
                                        count=[twentyc_nlon,twentyc_nlat,twentyc_nmembers,1])
          
          std_dev_mslp(j,k)=STDDEV(twentyc_mslp(j,k,*,0))


       ENDFOR
    ENDFOR

 
   ; Plot here
   psfile='/home/ss901165/idl/uk_cold_winters/uk_cold_winters_std_dev_mslp'+$
          STRTRIM(STRING(years(i)),1)+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=1000,YOFFSET=2000,$
          TFONT=2,TCHARSIZE=100
   MAP,LONMIN=-120,LONMAX=70,LATMAX=90,LATMIN=-10,/HIRES
   CS,SCALE=14,NCOLS=N_ELEMENTS(anom_levs)+1
   LEVS,MANUAL=anom_levs
  CON,X=twentyc_longitude,Y=twentyc_latitude,FIELD=std_dev_mslp,$
       TITLE='Standard Deviation of Ensemble Members for MSLP over DJF '+STRTRIM(STRING(years(i)),1),/NOLINES
     AXES   
   PSCLOSE

ENDFOR


STOP
END

