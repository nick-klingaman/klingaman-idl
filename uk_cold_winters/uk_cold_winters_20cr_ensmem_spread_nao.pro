PRO uk_cold_winters_20cr_ensmem_spread_nao
  
; Give the file containing the 20th Century Reanalysis every-member data
twentyc_nao_file='/home/ss901165/datasets/20THC_REANALYSIS/mslp/20thc_reanalysis_allmembers.dec-feb_smeans.1871-2007.nao_index.twod.nc'
; Give the file containing the observed NAO index
obs_nao_file='/home/ss901165/datasets/NAO_INDEX/nao_gibraltar_iceland.dec-feb_smeans.1821-2010.twod.nc'
; Number of ensemble members in 20CR
twentyc_nmembers=56
; Start year and total number of years
my_start_year=1871
twentyc_nao_start_year=1871
obs_nao_start_year=1821
n_years=137

; Read observed NAO index
twentyc_nao=OPEN_AND_EXTRACT(twentyc_nao_file,'nao_index',offset=[0,(my_start_year-twentyc_nao_start_year)],$
                           count=[twentyc_nmembers,n_years])

; Read 20CR NAO index
obs_nao=OPEN_AND_EXTRACT(obs_nao_file,'nao_index',offset=[my_start_year-obs_nao_start_year],$
                           count=[n_years])


; Construct anomalies from climatology
obs_nao_anom=obs_nao-MEAN(obs_nao)
twentyc_nao_anom=fltarr(twentyc_nmembers,n_years)
FOR i=0,twentyc_nmembers-1 DO $
   twentyc_nao_anom(i,*)=twentyc_nao(i,*)-MEAN(twentyc_nao(i,*))

; Bin the 20CR anomalies by the anomaly from their climatologies
max_bin=4.0    ; Value of the maximum bin in hPa (units of NAO index)
min_bin=-4.0   ; Value of the minimum bin in hPa (units of NAO index)
n_bins=21      ; Number of bins between the maximum and minimum
bins=findgen(n_bins)*(max_bin-min_bin)/FLOAT(n_bins-1)+min_bin
print,bins

twentyc_nao_anom_indices=VALUE_LOCATE(bins,twentyc_nao_anom)
twentyc_nao_anom_binned=fltarr(n_years,n_bins)
obs_nao_anom_bin=VALUE_LOCATE(bins,obs_nao_anom)
twentyc_nmembers_match_silo=intarr(n_years)
; The number of bins within which a 20CR ensemble member must be in order to declare it a match to SILO
tolerance=1

FOR i=0,n_years-1 DO BEGIN
   FOR j=0,n_bins-1 DO BEGIN
      IF TOTAL(where(REFORM(twentyc_nao_anom_indices(*,i)) eq j)) ge 0 THEN BEGIN
         twentyc_nao_anom_binned(i,j)=N_ELEMENTS(where(REFORM(twentyc_nao_anom_indices(*,i)) eq j))
      ENDIF ELSE $
         twentyc_nao_anom_binned(i,j)=0
   ENDFOR   
                                ; Compute the number of ensemble members in 20CR that are in the same bin as SILO
   IF TOTAL(where(ABS(REFORM(twentyc_nao_anom_indices(*,i))-obs_nao_anom_bin(i)) le tolerance)) gt 0 THEN BEGIN
      twentyc_nmembers_match_silo(i)=N_ELEMENTS(where(ABS(REFORM(twentyc_nao_anom_indices(*,i))-obs_nao_anom_bin(i)) le tolerance))
   ENDIF ELSE $
      twentyc_nmembers_match_silo(i)=0
ENDFOR

; Express levels in terms of number of ensemble members fitting into each bin
mylevs=['4','8','12','16','20','24','28','32','36','40','44','48','52']

; Give name of PostScript file
 psfile='/home/ss901165/idl/uk_cold_winters/uk_cold_winters_20cr_ensmem_spread_nao.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2000,SPACE3=500,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100,$
       SPACE2=700
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,white=2
LEVS,MANUAL=mylevs
GSET,XMIN=my_start_year-1,XMAX=my_start_year+n_years+1,YMIN=min_bin,YMAX=max_bin
CON,X=indgen(n_years)+my_start_year,Y=bins,FIELD=twentyc_nao_anom_binned,/BLOCK,/NOLINES,$
    CB_TITLE='Number of ensemble members in bin (bin size='+STRMID(STRTRIM(STRING((max_bin-min_bin)/(n_bins-1)),1),0,4)+' mm/day)'
GPLOT,X=indgen(n_years)+my_start_year,Y=obs_nao_anom,THICK=150
xstep=6
AXES,XVALS=indgen(n_years/xstep+1)*xstep+my_start_year,XMINOR=indgen(n_years/xstep*2+1)*xstep/2+my_start_year,$
     YSTEP=0.5,YMINOR=0.25,NDECS=2,YTITLE='Anomaly in area-averaged precipitation (mm/day)',XTITLE='Year',/NORIGHT

GSET,XMIN=my_start_year-1,XMAX=my_start_year+n_years+1,YMIN=0,YMAX=twentyc_nmembers
GPLOT,X=indgen(n_years)+my_start_year,Y=twentyc_nmembers_match_silo,STYLE=2,/NOLINES,SYM=3,SIZE=70
AXES,YSTEP=4,YMINOR=2,/ONLYRIGHT,YTITLE='Number of ensemble members within '+$
     STRTRIM(STRING(tolerance),1)+' bin(s) of the SILO value'

PSCLOSE

STOP
END

