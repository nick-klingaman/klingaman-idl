PRO uk_cold_winters_20cr_pdf_cet
  
; Give the file containing the 20th Century Reanalysis every-member data
twentyc_cet_file='/home/ss901165/datasets/20THC_REANALYSIS/temp_2m/20thc_reanalysis_allmembers.dec-feb_smeans.1871-2007.ctl_eng_temp.twod.nc'
; Give the file containing the observed NAO index
obs_cet_file='/home/ss901165/datasets/CTL_ENG_TEMP/ctl_eng_temp.dec-feb_smeans.1659-2009.twod.nc'
; Number of ensemble members in 20CR
twentyc_nmembers=56
; Start year and total number of years
my_start_year=1871
twentyc_cet_start_year=1871
obs_cet_start_year=1659
years=[1878,1890,1894,1916,1939,1946,1962]
n_years=N_ELEMENTS(years)

; Construct anomalies from climatology
;obs_nao_anom=obs_nao-MEAN(obs_nao)
;twentyc_nao_anom=fltarr(twentyc_nmembers,n_years)
;FOR i=0,twentyc_nmembers-1 DO $
;   twentyc_nao_anom(i,*)=twentyc_nao(i,*)-MEAN(twentyc_nao(i,*))

; Bin the 20CR anomalies by the anomaly from their climatologies
max_bin=280    ; Value of the maximum bin in hPa (units of NAO index)
min_bin=270   ; Value of the minimum bin in hPa (units of NAO index)
n_bins=21      ; Number of bins between the maximum and minimum
bins=findgen(n_bins)*(max_bin-min_bin)/FLOAT(n_bins-1)+min_bin
bins_mid=findgen(n_bins-1)*(max_bin-min_bin)/FLOAT(n_bins-1)+min_bin+(max_bin-min_bin)/FLOAT((n_bins-1)*2)
print,'Bins Max Value'
print,bins
print,'Bins Mid Value'
print,bins_mid

twentyc_cet_binned=fltarr(n_years,n_bins)
observed_cet_value=fltarr(n_years)
FOR i=0,n_years-1 DO BEGIN
     
twentyc_cet=OPEN_AND_EXTRACT(twentyc_cet_file,'ctl_eng_temp',offset=[0,(years(i)-twentyc_cet_start_year)],$
                           count=[twentyc_nmembers,1])

; Read 20CR NAO index
obs_cet=OPEN_AND_EXTRACT(obs_cet_file,'ctl_eng_temp',offset=[years(i)-obs_cet_start_year],$
                           count=[1])
   ; Convert Celsius to Kelvin
obs_cet=obs_cet+273.15


   twentyc_cet_indices=VALUE_LOCATE(bins,twentyc_cet)
   FOR j=0,n_bins-1 DO BEGIN
      IF TOTAL(where(REFORM(twentyc_cet_indices(*,0)) eq j)) ge 0 THEN BEGIN
         twentyc_cet_binned(i,j)=N_ELEMENTS(where(REFORM(twentyc_cet_indices(*)) eq j))
      ENDIF ELSE $
         twentyc_cet_binned(i,j)=0
   ENDFOR 
 print,'Number of Twentyc NAO Binned'
 print,twentyc_cet_binned 
 observed_cet_value(i)=obs_cet
 print,'Observed CET'
 print,observed_cet_value
colours=['blue','green','red','black','cyan','brown','orange']
ENDFOR 


; Give name of PostScript file
 psfile='/home/ss901165/idl/uk_cold_winters/uk_cold_winters_20cr_pdf_cet.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2000,SPACE3=500,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100,$
      SPACE2=700
 GSET,XMIN=min_bin,YMIN=0,XMAX=max_bin,YMAX=twentyc_nmembers,TITLE='Probability Distribution Function for CET'
FOR i=0,n_years-1 DO $
   GPLOT,X=bins_mid,Y=REFORM(twentyc_cet_binned(i,*)),COL=FSC_COLOR(colours(i))
FOR i=0,n_years-1 DO $
   GPLOT,X=observed_cet_value(i),Y=0,SYM=3,SIZE=100,COL=FSC_COLOR(colours(i))
leg_years=['1878','1890','1894','1916','1939','1946','1962']
GLEGEND,LABELS=leg_years,LEGPOS=11,SIZE=80,COL=FSC_COLOR(colours)
AXES,XSTEP=1.0,YSTEP=2,XMINOR=0.5,YMINOR=1.0,XTITLE='CET',YTITLE='Number of 20CR Ensemble Members', ORIENTATION=45,ndecs=1
PSCLOSE

 

STOP
END

