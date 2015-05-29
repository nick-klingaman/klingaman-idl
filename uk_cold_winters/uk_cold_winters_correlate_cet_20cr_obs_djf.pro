PRO uk_cold_winters_correlate_cet_20cr_obs_djf

obs_cet_file='/home/ss901165/datasets/CTL_ENG_TEMP/ctl_eng_temp.dec-feb_smeans.1659-2009.twod.nc'
twentyc_cet_file='/home/ss901165/datasets/20THC_REANALYSIS/temp_2m/20thc_reanalysis.dec-feb_smeans.1871-2007.ctl_eng_temp.twod.nc'

; Define start years of each file
obs_start_year=1659
twentyc_start_year=1871

; Define year to start analysis
my_start_year=1871
; Define year to end analysis
my_stop_year=2007
; Total number of years
n_years=my_stop_year-my_start_year+1

; Read Central England Temperature record for all months and specified years
obs_cet=OPEN_AND_EXTRACT(obs_cet_file,'ctl_eng_temp',offset=[my_start_year-obs_start_year],$
                         count=[n_years])
twentyc_cet_in=OPEN_AND_EXTRACT(twentyc_cet_file,'ctl_eng_temp',offset=[0,my_start_year-twentyc_start_year],$
                             count=[56,n_years])
twentyc_cet=fltarr(n_years)
FOR i=0,n_years-1 DO $
   twentyc_cet(i)=MEAN(twentyc_cet_in(*,i))
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
   running_correlation(i-window_length/2)=CORRELATE(obs_cet(i-window_length/2:i+window_length/2),$
                                                    twentyc_cet(i-window_length/2:i+window_length/2))

; Compute running correlation for each month separately

; Compute total RMSE
overall_rmse=SQRT(MEAN((obs_cet-twentyc_cet)^2))

; Compute RMSE for each month separately
;monthly_rmse=fltarr(12)
;FOR i=0,11 DO $
;   monthly_rmse(i)=SQRT(MEAN((REFORM(obs_cet(i))-REFORM(twentyc_cet(i)))^2))

; Compute running RMSE using all months

; Compute running RMSE for each month separately


psfile='/home/ss901165/idl/uk_cold_winters/uk_cold_winters_scatter_cet_20cr_obs.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=150,MARGIN=2500,XSIZE=15000,YSIZE=15000,XOFFSET=1000,YOFFSET=1000
GSET,XMIN=-1,XMAX=8,YMIN=-1,YMAX=8
GPLOT,X=obs_cet,Y=twentyc_cet,/NOLINES,SYM=3,SIZE=40,STYLE=1
GPLOT,X=[-1,8],Y=[-1,8],STYLE=2
GPLOT,X=[-1,6.2],Y=[0.8,8],STYLE=1
highlight_years=[7,19,23,45,68,75,91]
n_highlight=N_ELEMENTS(highlight_years)
red=FSC_COLOR('red',30)
FOR i=0,n_highlight-1 DO BEGIN
   print,obs_cet(highlight_years(i)),twentyc_cet(highlight_years(i))
   GPLOT,X=obs_cet(highlight_years(i)),Y=twentyc_cet(highlight_years(i)),COL=30,SYM=3,/NOLINES,SIZE=80
ENDFOR
GPLOT,X=0.7,Y=2.9,TEXT='1878',CHARSIZE=80
GPLOT,X=1.46,Y=4.7,TEXT='1890',CHARSIZE=80
GPLOT,X=1.56,Y=3.48,TEXT='1894',CHARSIZE=80
GPLOT,X=0.96,Y=3.99,TEXT='1916',CHARSIZE=80
GPLOT,X=0.96,Y=4.45,TEXT='1939',CHARSIZE=80
GPLOT,X=1.13,Y=2.50,TEXT='1946',CHARSIZE=80
GPLOT,X=-0.33,Y=2.57,TEXT='1962',CHARSIZE=80
AXES,XSTEP=1,YSTEP=1,XMINOR=0.5,YMINOR=0.5,XTITLE='Observed Central England Temperature (!Uo!NC)',YTITLE='20CR Central England Temperature (!Uo!NC)'
PSCLOSE


STOP
END
