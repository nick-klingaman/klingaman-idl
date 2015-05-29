PRO uk_cold_winters_correlate_cet_20cr_obs_djf

obs_cet_file='/home/ss901165/datasets/CTL_ENG_TEMP/ctl_eng_temp.jan-dec_mmeans.1659-2010.twod.nc'
twentyc_cet_file='/home/ss901165/datasets/20THC_REANALYSIS/temp_2m/20thc_reanalysis.jan-dec_mmeans.1870-2008.ctl_eng_temp.twod.nc'

; Define start years of each file
obs_start_year=1659
twentyc_start_year=1870

; Define year to start analysis
my_start_year=1870
; Define year to end analysis
my_stop_year=2008
; Total number of years
n_years=my_stop_year-my_start_year+1

; Read Central England Temperature record for all months and specified years
obs_cet=OPEN_AND_EXTRACT(obs_cet_file,'ctl_eng_temp',offset=[0,(my_start_year-obs_start_year)],$
                         count=[12,n_years])
twentyc_cet=OPEN_AND_EXTRACT(twentyc_cet_file,'ctl_eng_temp',offset=[0,(my_start_year-twentyc_start_year)],$
                             count=[12,n_years])
; Convert Celsius to Kelvin
;obs_cet=obs_cet+273.15
twentyc_cet=twentyc_cet-273.15

; Compute overall correlation, using all months and all years
overall_correlation=CORRELATE(obs_cet,twentyc_cet)

; Compute correlation for each month separately
monthly_correlation=fltarr(12)
FOR i=0,11 DO $
   monthly_correlation(i)=CORRELATE(REFORM(obs_cet(i,*)),REFORM(twentyc_cet(i,*)))

; Compute running correlation using all months
; Define window length for running correlation (in years) - must be an odd number
window_length=11
running_correlation=fltarr(n_years-window_length+1)
FOR i=window_length/2,n_years-window_length/2-1 DO $
   running_correlation(i-window_length/2)=CORRELATE(obs_cet(*,i-window_length/2:i+window_length/2),$
                                                    twentyc_cet(*,i-window_length/2:i+window_length/2))

; Compute running correlation for each month separately

; Compute total RMSE
overall_rmse=SQRT(MEAN((obs_cet-twentyc_cet)^2))

; Compute RMSE for each month separately
monthly_rmse=fltarr(12)
FOR i=0,11 DO $
   monthly_rmse(i)=SQRT(MEAN((REFORM(obs_cet(i,*))-REFORM(twentyc_cet(i,*)))^2))

; Compute running RMSE using all months

; Compute running RMSE for each month separately


psfile='/home/ss901165/idl/uk_cold_winters/uk_cold_winters_scatter_cet_20cr_obs.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=150,MARGIN=2500
GSET,XMIN=-5,XMAX=15,YMIN=5,YMAX=15
GPLOT,X=obs_cet,Y=twentyc_cet,/NOLINES,SYM=3,SIZE=50
AXES,XSTEP=1,YSTEP=1
PSCLOSE


STOP
END
