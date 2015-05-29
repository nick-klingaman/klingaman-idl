PRO uk_cold_winters_contingency_swwind_cet_ensmem
  
; Make a contingency table for the SWWIND against Central England Temperature

; Starting year for analysis
my_start_year=1871

; Ending year for analysis
my_stop_year=2007

; Total number of years
n_years=my_stop_year-my_start_year+1

; Number of divisions (rows or columns) in the contingency table
n_divisions=6
n_years_per_division=(n_years/n_divisions+1)*56

; Input file for SWWIND index
;swwind_infile='/home/ss901165/datasets/20THC_REANALYSIS/every_member/wind_speed/20thc_reanalysis_allmembers.dec-feb_smeans.1871-2007.swwind_box_index_850.nc'
swwind_infile='/home/ss901165/datasets/20THC_REANALYSIS/wind_speed/20thc_reanalysis.dec-feb_smeans.1871-2007.swwind_box_index_850.nc'
swwind_start_year=1871

; Input file for CET index
cet_infile='/home/ss901165/datasets/20THC_REANALYSIS/temp_2m/20thc_reanalysis_allmembers.dec-feb_smeans.1871-2007.ctl_eng_temp.twod.nc'
;cet_infile='/home/ss901165/datasets/20THC_REANALYSIS/temp_2m/20thc_reanalysis.dec-feb_smeans.1871-2007.ctl_eng_temp.twod.nc'
cet_start_year=1871

; Read SWWIND and CET indices
;swwind_index=OPEN_AND_EXTRACT(swwind_infile,'swwind_index',offset=[0,(my_start_year-swwind_start_year)],$
;                           count=[56,n_years])
swwind_index=OPEN_AND_EXTRACT(swwind_infile,'swwind_index',offset=[my_start_year-swwind_start_year],$
                              count=[n_years])
cet_index=OPEN_AND_EXTRACT(cet_infile,'ctl_eng_temp',offset=[0,(my_start_year-cet_start_year)],$
                           count=[56,n_years])
;cet_index=OPEN_AND_EXTRACT(cet_infile,'ctl_eng_temp',offset=[0,my_start_year-cet_start_year],count=[1,n_years])

swwind_index_oned=fltarr(56*n_years)
cet_index_oned=fltarr(56*n_years)
FOR i=0,55 DO BEGIN
   swwind_index_oned(n_years*i:n_years*(i+1)-1)=swwind_index(*)
   cet_index_oned(n_years*i:n_years*(i+1)-1)=cet_index(i,*)
ENDFOR

; Make DJF means of SWWIND and CET
;djf_swwind_index=fltarr(n_years-1)
;djf_cet_index=fltarr(n_years-1)
;FOR i=0,n_years-2 DO BEGIN
;   djf_swwind_index(i)=(swwind_index(11,i)+swwind_index(0,i+1)+swwind_index(1,i+1))/3.
;   djf_cet_index(i)=(cet_index(11,i)+cet_index(0,i+1)+cet_index(1,i+1))/3.
;ENDFOR

; Sort SWWIND and CET indices
djf_swwind_index_sorted=SORT(swwind_index_oned)
djf_cet_index_sorted=SORT(cet_index_oned)

; Compute contingency table
contingency_table=intarr(n_divisions,n_divisions)
FOR i=0,(n_years)*56-1 DO BEGIN
   swwind_division=i/n_years_per_division
   cet_division=where(djf_cet_index_sorted eq djf_swwind_index_sorted(i))/n_years_per_division
   contingency_table(swwind_division,cet_division)=contingency_table(swwind_division,cet_division)+1
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

