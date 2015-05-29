PRO uk_cold_winters_20cr_pdf_swwind
  
; Give the file containing the 20th Century Reanalysis every-member data
twentyc_nao_file='/home/ss901165/datasets/20THC_REANALYSIS/every_member/wind_speed/20thc_reanalysis_allmembers.dec-feb_smeans.1871-2007.swwind_box_index_850.nc'
; Give the file containing the observed NAO index
;obs_nao_file='/home/ss901165/datasets/NAO_INDEX/nao_gibraltar_iceland.dec-feb_smeans.1821-2010.twod.nc'
; Number of ensemble members in 20CR
twentyc_nmembers=56
; Start year and total number of years
my_start_year=1871
twentyc_nao_start_year=1871
;obs_nao_start_year=1821
years=[1878,1890,1894,1916,1939,1946,1962]
n_years=N_ELEMENTS(years)

; Construct anomalies from climatology
;obs_nao_anom=obs_nao-MEAN(obs_nao)
;twentyc_nao_anom=fltarr(twentyc_nmembers,n_years)
;FOR i=0,twentyc_nmembers-1 DO $
;   twentyc_nao_anom(i,*)=twentyc_nao(i,*)-MEAN(twentyc_nao(i,*))

; Bin the 20CR anomalies by the anomaly from their climatologies
max_bin=10    ; Value of the maximum bin in hPa (units of NAO index)
min_bin=-10.0   ; Value of the minimum bin in hPa (units of NAO index)
n_bins=21      ; Number of bins between the maximum and minimum
bins=findgen(n_bins)*(max_bin-min_bin)/FLOAT(n_bins-1)+min_bin
bins_mid=findgen(n_bins-1)*(max_bin-min_bin)/FLOAT(n_bins-1)+min_bin+(max_bin-min_bin)/FLOAT((n_bins-1)*2)
print,'Bins Max Value'
print,bins
print,'Bins Mid Value'
print,bins_mid

twentyc_nao_binned=fltarr(n_years,n_bins)
observed_nao_values=fltarr(n_years)
FOR i=0,n_years-1 DO BEGIN
                                ; Read observed NAO index
   twentyc_nao=REFORM(OPEN_AND_EXTRACT(twentyc_nao_file,'swwind_index',offset=[0,(years(i)-twentyc_nao_start_year)],$
                                count=[twentyc_nmembers,1]))

                                ; Read 20CR NAO index
;   obs_nao=OPEN_AND_EXTRACT(obs_nao_file,'nao_index',offset=[years(i)-obs_nao_start_year],$
;                           count=[1])

   
   twentyc_nao_indices=VALUE_LOCATE(bins,twentyc_nao)
   FOR j=0,n_bins-1 DO BEGIN
      IF TOTAL(where(REFORM(twentyc_nao_indices(*,0)) eq j)) ge 0 THEN BEGIN
         twentyc_nao_binned(i,j)=N_ELEMENTS(where(REFORM(twentyc_nao_indices(*,0)) eq j))
      ENDIF ELSE $
         twentyc_nao_binned(i,j)=0
   ENDFOR 
 print,'Number of Twentyc NAO Binned'
 print,twentyc_nao_binned 
; observed_nao_values(i)=obs_nao
; print,'Observed NAO'
; print,observed_nao_values(i)
ENDFOR

; Express levels in terms of number of ensemble members fitting into each bin
;mylevs=['4','8','12','16','20','24','28','32','36','40','44','48','52']

; Give name of PostScript file
 psfile='/home/ss901165/idl/uk_cold_winters/uk_cold_winters_20cr_pdf_swwind.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2000,SPACE3=500,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100,$
      SPACE2=700
 GSET,XMIN=min_bin,YMIN=0,XMAX=max_bin,YMAX=twentyc_nmembers,TITLE='Probability Distribution Functions for NAO'
colors=['purple','blue','cyan','orange','red','brown','pink']
FOR i=0,n_years-1 DO BEGIN
   GPLOT,X=bins_mid,Y=REFORM(twentyc_nao_binned(i,*)),COL=FSC_COLOR(colors(i)),THICK=150
   ;GPLOT,X=observed_nao_value+0.5,Y=2,TEXT='Observed Value',ALIGN=1.0,VALIGN=0.5
;   GPLOT,X=REFORM(observed_nao_values(i)),Y=0,SYM=3,SIZE=100,COL=FSC_COLOR(colors(i)),THICK=150
ENDFOR
AXES,XSTEP=0.4,YSTEP=4,XMINOR=0.2,YMINOR=2.0,XTITLE='NAO',YTITLE='Number of 20CR Ensemble Members', ORIENTATION=45,ndecs=1
PSCLOSE

STOP
END

