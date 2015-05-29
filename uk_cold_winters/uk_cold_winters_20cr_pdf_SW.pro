PRO uk_cold_winters_20cr_pdf_SW
  
; Give the file containing the 20th Century Reanalysis every-member
; data
SW_index_infile='/home/ss901165/datasets/20THC_REANALYSIS/every_member/wind_speed/20thc_reanalysis_allmembers.dec-feb_smeans.1871-2007.swwind_box_index_850.nc'
;SW_index_ob_infile='/home/ss901165/datasets/20THC_REANALYSIS/wind_speed/20thc_reanalysis.jan-dec_mmeans.1871-2008.swwind_box_index_850.nc'
; Number of ensemble members in 20CR
twentyc_nmembers=56
; Start year and total number of years
my_start_year=1871
twentyc_start_year=1871
my_end_year=2007

years=[1878,1890,1894,1916,1939,1946,1962]
n_years=N_ELEMENTS(years)
colours=['blue','green','red','black','cyan','brown','orange']


; Bin the 20CR anomalies by the anomaly from their climatologies
max_bin=4; Value of the maximum bin in hPa (units of NAO index)
min_bin=-4  ; Value of the minimum bin in hPa (units of NAO index)
n_bins=21      ; Number of bins between the maximum and minimum
bins=findgen(n_bins)*(max_bin-min_bin)/FLOAT(n_bins-1)+min_bin
bins_mid=findgen(n_bins-1)*(max_bin-min_bin)/FLOAT(n_bins-1)+min_bin+(max_bin-min_bin)/FLOAT((n_bins-1)*2)
print,'Bins Max Value'
print,bins
print,'Bins Mid Value'
print,bins_mid

anom_max_bin=-2
anom_min_bin=-12
n_anom_bins=21
anom_bins=findgen(n_anom_bins)*(anom_max_bin-anom_min_bin)/FLOAT(n_anom_bins-1)+anom_min_bin
anom_bins_mid=findgen(n_anom_bins-1)*(anom_max_bin-anom_min_bin)/$
              FLOAT(n_anom_bins-1)+anom_min_bin+(anom_max_bin-anom_min_bin)/FLOAT((n_anom_bins-1)*2)

scaled_max_bin=0
scaled_min_bin=-4
n_scaled_bins=21
scaled_bins=findgen(n_scaled_bins)*(scaled_max_bin-scaled_min_bin)/FLOAT(n_scaled_bins-1)+min_bin
scaled_bins_mid=findgen(n_scaled_bins-1)*(scaled_max_bin-scaled_min_bin)/$
                FLOAT(n_scaled_bins-1)+scaled_min_bin+(scaled_max_bin-scaled_min_bin)/FLOAT((n_scaled_bins-1)*2)

twentyc_SW_binned=fltarr(n_years,n_bins)
twentyc_SW_anom_binned=fltarr(n_years,n_bins)
twentyc_SW_scaled_binned=fltarr(n_years,n_bins)
;anomaly=fltarr(twentyc_nmembers,n_years)
anomaly=fltarr(twentyc_nmembers)
;scaled=fltarr(twentyc_nmembers,n_years)
scaled=fltarr(twentyc_nmembers)

twentyc_SW_all=OPEN_AND_EXTRACT(SW_index_infile,'swwind_index',offset=[0,(my_start_year-twentyc_start_year)],$
                           count=[twentyc_nmembers,(my_end_year-my_start_year)])

FOR i=0,n_years-1 DO BEGIN

   twentyc_SW=OPEN_AND_EXTRACT(SW_index_infile,'swwind_index',offset=[0,(years(i)-twentyc_start_year)],$
                           count=[twentyc_nmembers,1])

   twentyc_SW_indices=VALUE_LOCATE(bins,twentyc_SW)
   FOR j=0,n_bins-1 DO BEGIN
      IF TOTAL(where(REFORM(twentyc_SW_indices(*,0)) eq j)) ge 0 THEN BEGIN
         twentyc_SW_binned(i,j)=N_ELEMENTS(where(REFORM(twentyc_SW_indices(*,0)) eq j))
      ENDIF ELSE $
         twentyc_SW_binned(i,j)=0
   ENDFOR 
   
                                ; The anomaly is the difference between this member for this year 
                                ; and this member for all years.   
   FOR j=0,twentyc_nmembers-1 DO $
      anomaly(j)=twentyc_SW(j)-MEAN(twentyc_SW_all(j,*))
   
;   anomaly(i)=twentyc_SW(i)-(MEAN(twentyc_SW_all(i,*)))
   twentyc_SW_anom_indices=VALUE_LOCATE(anom_bins,anomaly)
   FOR k=0,n_anom_bins-1 DO BEGIN
      IF TOTAL(where(REFORM(twentyc_SW_anom_indices(*,0)) eq k)) ge 0 THEN BEGIN
         twentyc_SW_anom_binned(i,k)=N_ELEMENTS(where(REFORM(twentyc_SW_anom_indices(*,0)) eq k))
      ENDIF ELSE $
         twentyc_SW_anom_binned(i,k)=0
   ENDFOR    
   
                                ; The scaled anomaly is the anomaly divided by the standard deviation
                                ; using all years for this member
   FOR j=0,twentyc_nmembers-1 DO $
      scaled(j)=anomaly(j)/STDDEV(twentyc_SW_all(k,*))

;  scaled(i)=anomaly(i)/(STDEV(twentyc_SW_all(i,*)))
   twentyc_SW_scaled_indices=VALUE_LOCATE(scaled_bins,scaled)
   FOR h=0,n_scaled_bins-1 DO BEGIN
      IF TOTAL(where(REFORM(twentyc_SW_scaled_indices(*,0)) eq h)) ge 0 THEN BEGIN
         twentyc_SW_scaled_binned(i,h)=N_ELEMENTS(where(REFORM(twentyc_SW_scaled_indices(*,0)) eq h))
      ENDIF ELSE $
         twentyc_SW_scaled_binned(i,h)=0
   ENDFOR  
ENDFOR 
mean=MEAN(twentyc_SW_all)
print,'mean'
print,mean 
  
print, twentyc_SW

print,'anomaly'   
print,anomaly 

print, 'scaled'
print,scaled


; Give name of PostScript file
 psfile='/home/ss901165/idl/uk_cold_winters/uk_cold_winters_20cr_pdf_SW.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2000,SPACE3=500,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100,$
      SPACE2=700
 GSET,XMIN=min_bin,YMIN=0,XMAX=max_bin,YMAX=twentyc_nmembers,TITLE='Probability Distribution Function for SWly Wind Index'
FOR i=0,n_years-1 DO $
GPLOT,X=bins_mid,Y=REFORM(twentyc_SW_binned(i,*)),COL=FSC_COLOR(colours(i))
leg_years=['1878','1890','1894','1916','1939','1946','1962']
GLEGEND,LABELS=leg_years,LEGPOS=9,SIZE=80,COL=FSC_COLOR(colours)
AXES,XSTEP=0.8,YSTEP=2,XMINOR=0.4,YMINOR=1.0,XTITLE='SWly Wind Index',YTITLE='Number of 20CR Ensemble Members', ORIENTATION=45,ndecs=1
PSCLOSE

psfile='/home/ss901165/idl/uk_cold_winters/uk_cold_winters_20cr_pdf_SW_anom.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2000,SPACE3=500,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100,$
      SPACE2=700
 GSET,XMIN=anom_min_bin,YMIN=0,XMAX=anom_max_bin,YMAX=twentyc_nmembers,TITLE='Probability Distribution Function for SWly Wind Index Anomalies'
FOR i=0,n_years-1 DO $
GPLOT,X=anom_bins_mid,Y=REFORM(twentyc_SW_anom_binned(i,*)),COL=FSC_COLOR(colours(i))
leg_years=['1878','1890','1894','1916','1939','1946','1962']
GLEGEND,LABELS=leg_years,LEGPOS=9,SIZE=80,COL=FSC_COLOR(colours)
AXES,XSTEP=0.4,YSTEP=2,XMINOR=0.4,YMINOR=1.0,XTITLE='SWly Wind Index',YTITLE='Number of 20CR Ensemble Members', ORIENTATION=45,ndecs=1
PSCLOSE

psfile='/home/ss901165/idl/uk_cold_winters/uk_cold_winters_20cr_pdf_SW_scaled.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2000,SPACE3=500,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100,$
      SPACE2=700
 GSET,XMIN=scaled_min_bin,YMIN=0,XMAX=scaled_max_bin,YMAX=twentyc_nmembers,TITLE='Probability Distribution Function for SWly Wind Index Scaled Anomlaies'
FOR i=0,n_years-1 DO $
GPLOT,X=scaled_bins_mid,Y=REFORM(twentyc_SW_scaled_binned(i,*)),COL=FSC_COLOR(colours(i))
leg_years=['1878','1890','1894','1916','1939','1946','1962']
GLEGEND,LABELS=leg_years,LEGPOS=9,SIZE=80,COL=FSC_COLOR(colours)
AXES,XSTEP=1.0,YSTEP=2,XMINOR=0.1,YMINOR=1.0,XTITLE='SWly Wind Index',YTITLE='Number of 20CR Ensemble Members', ORIENTATION=45,ndecs=1
PSCLOSE



STOP
END

