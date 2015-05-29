PRO uk_cold_winters_contingency_nao_cet
  
; Make a contingency table for the NAO against Central England Temperature

; Starting year for analysis
my_start_year=1871

; Ending year for analysis
my_stop_year=2010

; Total number of years
n_years=my_stop_year-my_start_year+1

; Number of divisions (rows or columns) in the contingency table
n_divisions=6
n_years_per_division=n_years/n_divisions

; Input file for NAO index
nao_infile='/home/ss901165/datasets/NAO_INDEX/nao_gibraltar_iceland.jan-dec_mmeans.1821-2011.twod.nc'
nao_start_year=1821

; Input file for CET index
cet_infile='/home/ss901165/datasets/CTL_ENG_TEMP/ctl_eng_temp.jan-dec_mmeans.1659-2010.twod.nc'
cet_start_year=1659

; Read NAO and CET indices
nao_index=OPEN_AND_EXTRACT(nao_infile,'nao_index',offset=[0,(my_start_year-nao_start_year)],$
                           count=[12,n_years])
cet_index=OPEN_AND_EXTRACT(cet_infile,'ctl_eng_temp',offset=[0,(my_start_year-cet_start_year)],$
                           count=[12,n_years])

; Make DJF means of NAO and CET
djf_nao_index=fltarr(n_years-1)
djf_cet_index=fltarr(n_years-1)
FOR i=0,n_years-2 DO BEGIN
   djf_nao_index(i)=(nao_index(11,i)+nao_index(0,i+1)+nao_index(1,i+1))/3.
   djf_cet_index(i)=(cet_index(11,i)+cet_index(0,i+1)+cet_index(1,i+1))/3.
ENDFOR

; Sort NAO and CET indices
djf_nao_index_sorted=SORT(djf_nao_index)
djf_cet_index_sorted=SORT(djf_cet_index)

; Compute contingency table
contingency_table=intarr(n_divisions,n_divisions)
FOR i=0,n_years-2 DO BEGIN
   nao_division=i/n_years_per_division
   IF nao_division eq n_divisions THEN $
      nao_division=nao_division-1
   cet_division=where(djf_cet_index_sorted eq djf_nao_index_sorted(i))/n_years_per_division
   contingency_table(nao_division,cet_division)=contingency_table(nao_division,cet_division)+1
ENDFOR

; Compute contingency table as a probability (number of years / number of possible years)
contingency_table_probabilities=contingency_table/FLOAT(n_years_per_division)

expected=fltarr(n_divisions,n_divisions)
FOR i=0,n_divisions-1 DO BEGIN
   FOR j=0,n_divisions-1 DO BEGIN
      expected(i,j)=(TOTAL(contingency_table(i,*))*TOTAL(contingency_table(*,j)))/FLOAT(TOTAL(contingency_table))
   ENDFOR
ENDFOR
chisq=TOTAL((expected-contingency_table)^2/expected)

STOP
END

