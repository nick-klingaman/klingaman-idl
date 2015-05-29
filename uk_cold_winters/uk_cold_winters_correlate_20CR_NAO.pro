PRO uk_cold_winters_correlate_20CR_NAO

nao_file='/home/ss901165/datasets/NAO_INDEX/nao_gibraltar_iceland.jan-dec_mmeans.1821-2011.twod.nc'
twentyc_nao_file='/home/ss901165/datasets/20THC_REANALYSIS/mslp/20thc_reanalysis.jan-dec_mmeans.1871-2008.nao_index.twod.nc'

; Define start years of each file
nao_start_year=1821
twentyc_start_year=1871

; Define year to start analysis
my_start_year=1871
; Define year to end analysis
my_stop_year=2008
; Total number of years
n_years=my_stop_year-my_start_year+1

; Read Central England Temperature record for all months and specified years
obs_nao=OPEN_AND_EXTRACT(nao_file,'nao_index',offset=[0,(my_start_year-nao_start_year)],$
                         count=[12,n_years])
twentyc_nao=OPEN_AND_EXTRACT(twentyc_nao_file,'nao_index',offset=[0,(my_start_year-twentyc_start_year)],$
                             count=[12,n_years])
years=findgen(n_years)+my_start_year
print,'year','nao obs','20CR nao'
FOR i=0,n_years-2 DO BEGIN
   djf_obs_nao=(obs_nao(11,i)+obs_nao(0,i+1)+obs_nao(1,i+1))/3.
   djf_twentyc_nao=(twentyc_nao(11,i)+twentyc_nao(0,i+1)+twentyc_nao(1,i+1))/3.
   print,years(i),djf_obs_nao,djf_twentyc_nao
ENDFOR
STOP

; Compute overall correlation, using all months and all years
overall_correlation=CORRELATE(obs_nao,twentyc_nao)

; Compute correlation for each month separately
monthly_correlation=fltarr(12)
FOR i=0,11 DO $
   monthly_correlation(i)=CORRELATE(REFORM(obs_nao(i,*)),REFORM(twentyc_nao(i,*)))

; Compute running correlation using all months
; Define window length for running correlation (in years) - must be an odd number
window_length=11
running_correlation=fltarr(n_years-window_length+1)
FOR i=window_length/2,n_years-window_length/2-1 DO $
   running_correlation(i-window_length/2)=CORRELATE(obs_nao(*,i-window_length/2:i+window_length/2),$
                                                   twentyc_nao(*,i-window_length/2:i+window_length/2))

 ;Give the name for the PostScript format output file
psfile='/home/ss901165/idl/uk_cold_winters_correlate_20CR_NAO.ps'

;This command opens the PostScript file and sets the font size and margins
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=3000,SPACE2=300,XOFFSET=1000,YOFFSET=2000,$
       TFONT=2,TCHARSIZE=100

GSET,XMIN=1870,YMIN=0.8,XMAX=2012,YMAX=1.0,TITLE='Running Correlation'

years=findgen(n_years-10)+1875

GPLOT,X=years,Y=running_correlation,COL=FSC_COLOR('RED')

; This command draws the axes
AXES,XSTEP=10,YSTEP=0.1,XMINOR=5,YMINOR=0.05,XTITLE='Year',YTITLE='Correlation', ORIENTATION=45,ndecs=1

PSCLOSE

; Compute running correlation for each month separately
window_length=11
running_monthly_correlation=fltarr(12,n_years-window_length+1)
FOR h=0,11 DO $
FOR i=window_length/2,n_years-window_length/2-1 DO $
   running_monthly_correlation(h,i-window_length/2)=CORRELATE(obs_nao(h,i-window_length/2:i+window_length/2),$
                                                   twentyc_nao(h,i-window_length/2:i+window_length/2))


;pull out monthly running correlations
jan_running_correlation=REFORM(running_monthly_correlation(0,0:n_years-window_length-1))
feb_running_correlation=REFORM(running_monthly_correlation(1,0:n_years-window_length-1))
mar_running_correlation=REFORM(running_monthly_correlation(2,0:n_years-window_length-1))
apr_running_correlation=REFORM(running_monthly_correlation(3,0:n_years-window_length-1))
may_running_correlation=REFORM(running_monthly_correlation(4,0:n_years-window_length-1))
jun_running_correlation=REFORM(running_monthly_correlation(5,0:n_years-window_length-1))
jul_running_correlation=REFORM(running_monthly_correlation(6,0:n_years-window_length-1))
aug_running_correlation=REFORM(running_monthly_correlation(7,0:n_years-window_length-1))
sep_running_correlation=REFORM(running_monthly_correlation(8,0:n_years-window_length-1))
oct_running_correlation=REFORM(running_monthly_correlation(9,0:n_years-window_length-1))
nov_running_correlation=REFORM(running_monthly_correlation(10,0:n_years-window_length-1))
dec_running_correlation=REFORM(running_monthly_correlation(11,0:n_years-window_length-1))

; Give the name for the PostScript format output file
psfile='/home/ss901165/idl/uk_cold_winters_correlate_cet_20CR_NAO_monthly.ps'

; This command opens the PostScript file and sets the font size and margins
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1000,SPACE2=300,XOFFSET=1000,YOFFSET=2000,$
       TFONT=2,TCHARSIZE=100


GSET,XMIN=1870,YMIN=0,XMAX=2012,YMAX=1,TITLE='Running Monthly Correlation'
years=findgen(n_years-10)+1875


; This command draws the line plot.  The COL= command makes the line red.
GPLOT,X=years,Y=jan_running_correlation,COL=2
GPLOT,X=years,Y=feb_running_correlation,COL=4
GPLOT,X=years,Y=mar_running_correlation,COL=6
GPLOT,X=years,Y=apr_running_correlation,COL=8
GPLOT,X=years,Y=may_running_correlation,COL=10
GPLOT,X=years,Y=jun_running_correlation,COL=12
GPLOT,X=years,Y=jul_running_correlation,COL=14
GPLOT,X=years,Y=aug_running_correlation,COL=16
GPLOT,X=years,Y=sep_running_correlation,COL=18
GPLOT,X=years,Y=oct_running_correlation,COL=1
GPLOT,X=years,Y=nov_running_correlation,COL=3
GPLOT,X=years,Y=dec_running_correlation,COL=5

months=['January','Feburary','March','April','May','June','July','August','September','October','November','December']
GLEGEND,LABELS=months,LEGPOS=11,SIZE=80,COL=[2,4,6,8,10,12,14,16,18,1,3,5]

; This command draws the axes
AXES,XSTEP=10,YSTEP=0.1,XMINOR=5,YMINOR=0.05,XTITLE='Year',YTITLE='Correlation', ORIENTATION=45


PSCLOSE


; Compute total RMSE
overall_rmse=SQRT(MEAN((obs_nao-twentyc_nao)^2))

; Compute RMSE for each month separately
monthly_rmse=fltarr(12)
FOR i=0,11 DO $
   monthly_rmse(i)=SQRT(MEAN((REFORM(obs_nao(i,*))-REFORM(twentyc_nao(i,*)))^2))

; Compute running RMSE using all months

window_length=11
running_rmse=fltarr(n_years-window_length+1)
FOR i=window_length/2,n_years-window_length/2-1 DO $
   running_rmse(i-window_length/2)=SQRT(MEAN((obs_nao(*,i-window_length/2:i+window_length/2)-twentyc_nao(*,i-window_length/2:i+window_length/2))^2))

 ;Give the name for the PostScript format output file
psfile='/home/ss901165/idl/uk_cold_winters_running_rmse_20CR_NAO_total.ps'

;This command opens the PostScript file and sets the font size and margins
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1000,SPACE2=300,XOFFSET=1000,YOFFSET=2000,$
       TFONT=2,TCHARSIZE=100

GSET,XMIN=1870,YMIN=0,XMAX=2012,YMAX=2,TITLE='Running RMSE'

years=findgen(n_years-10)+1875

GPLOT,X=years,Y=running_rmse,COL=FSC_COLOR('RED')

; This command draws the axes
AXES,XSTEP=10,YSTEP=1,XMINOR=5,YMINOR=0.5,XTITLE='Year',YTITLE='RMSE', ORIENTATION=45

PSCLOSE


; Compute running RMSE for each month separately
window_length=11
running_monthly_rmse=fltarr(12,n_years-window_length+1)
FOR h=0,11 DO $
FOR i=window_length/2,n_years-window_length/2-1 DO $
   running_monthly_rmse(h,i-window_length/2)=SQRT(MEAN((obs_nao(h,i-window_length/2:i+window_length/2)-twentyc_nao(h,i-window_length/2:i+window_length/2))^2))


;pull out monthly running correlations
jan_running_rmse=REFORM(running_monthly_rmse(0,0:n_years-window_length-1))
feb_running_rmse=REFORM(running_monthly_rmse(1,0:n_years-window_length-1))
mar_running_rmse=REFORM(running_monthly_rmse(2,0:n_years-window_length-1))
apr_running_rmse=REFORM(running_monthly_rmse(3,0:n_years-window_length-1))
may_running_rmse=REFORM(running_monthly_rmse(4,0:n_years-window_length-1))
jun_running_rmse=REFORM(running_monthly_rmse(5,0:n_years-window_length-1))
jul_running_rmse=REFORM(running_monthly_rmse(6,0:n_years-window_length-1))
aug_running_rmse=REFORM(running_monthly_rmse(7,0:n_years-window_length-1))
sep_running_rmse=REFORM(running_monthly_rmse(8,0:n_years-window_length-1))
oct_running_rmse=REFORM(running_monthly_rmse(9,0:n_years-window_length-1))
nov_running_rmse=REFORM(running_monthly_rmse(10,0:n_years-window_length-1))
dec_running_rmse=REFORM(running_monthly_rmse(11,0:n_years-window_length-1))

; Give the name for the PostScript format output file
psfile='/home/ss901165/idl/uk_cold_winters_running_rmse_20CR_NAO_monthly.ps'

; This command opens the PostScript file and sets the font size and margins
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1000,SPACE2=300,XOFFSET=1000,YOFFSET=2000,$
       TFONT=2,TCHARSIZE=100


GSET,XMIN=1870,YMIN=0,XMAX=2012,YMAX=4,TITLE='Running Monthly RMSE'
years=findgen(n_years-10)+1875


; This command draws the line plot.  The COL= command makes the line red.
GPLOT,X=years,Y=jan_running_rmse,COL=2
GPLOT,X=years,Y=feb_running_rmse,COL=4
GPLOT,X=years,Y=mar_running_rmse,COL=6
GPLOT,X=years,Y=apr_running_rmse,COL=8
GPLOT,X=years,Y=may_running_rmse,COL=10 
GPLOT,X=years,Y=jun_running_rmse,COL=12 
GPLOT,X=years,Y=jul_running_rmse,COL=14
GPLOT,X=years,Y=aug_running_rmse,COL=16
GPLOT,X=years,Y=sep_running_rmse,COL=18
GPLOT,X=years,Y=oct_running_rmse,COL=1 
GPLOT,X=years,Y=nov_running_rmse,COL=3 
GPLOT,X=years,Y=dec_running_rmse,COL=5

months=['January','Feburary','March','April','May','June','July','August','September','October','November','December']
GLEGEND,LABELS=months,LEGPOS=9,SIZE=60,COL=[2,4,6,8,10,12,14,16,18,1,3,5]

; This command draws the axes
AXES,XSTEP=10,YSTEP=1,XMINOR=5,YMINOR=0.5,XTITLE='Year',YTITLE='RMSE', ORIENTATION=45


PSCLOSE



STOP
END
