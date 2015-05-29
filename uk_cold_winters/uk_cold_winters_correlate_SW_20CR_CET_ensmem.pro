PRO uk_cold_winters_correlate_SW_20CR_CET_ensmem

SW_index_file='/home/ss901165/datasets/20THC_REANALYSIS/every_member/wind_speed/20thc_reanalysis_allmembers.jan-dec_mmeans.1871-2008.swwind_box_index_850.nc'
twentyc_cet_file='/home/ss901165/datasets/20THC_REANALYSIS/temp_2m/20thc_reanalysis_allmembers.jan-dec_mmeans.1871-2008.ctl_eng_temp.twod.nc'

; Define start years of each file
SW_index_start_year=1871
twentyc_start_year=1871

; Define year to start analysis
my_start_year=1871
; Define year to end analysis
my_stop_year=2008
; Total number of years
n_years=my_stop_year-my_start_year+1

; Read Central England Temperature record for all months and specified years
SW_index=OPEN_AND_EXTRACT(SW_index_file,'swwind_index',offset=[0,(my_start_year-SW_index_start_year),0],$
                         count=[56,n_years,12])
twentyc_cet=OPEN_AND_EXTRACT(twentyc_cet_file,'ctl_eng_temp',offset=[0,(my_start_year-twentyc_start_year),0],$
                             count=[56,n_years,12])

years=findgen(n_years)+my_start_year
;print,'year','cet obs','20CR temp'
;FOR i=0,n_years-2 DO BEGIN
  ; djf_obs_cet=(obs_cet(11,i)+obs_cet(0,i+1)+obs_cet(1,i+1))/3.
  ; djf_twentyc_temp=(twentyc_cet(11,i)+twentyc_cet(0,i+1)+twentyc_cet(1,i+1))/3.
  ; print,years(i),djf_obs_cet,djf_twentyc_temp
;ENDFOR
;STOP

; Compute overall correlation, using all months and all years
;overall_correlation=CORRELATE(SW_index,twentyc_cet)

; Compute correlation for each month separately
monthly_correlation=fltarr(56,12)

FOR j=0,55 DO $
   FOR i=0,11 DO $
      monthly_correlation(j,i)=CORRELATE(REFORM(SW_index(j,*,i)),REFORM(twentyc_cet(j,*,i)))

; Compute running correlation using all months
; Define window length for running correlation (in years) - must be an odd number
;window_length=11
;running_correlation=fltarr(56,n_years-window_length+1)
;FOR j=0,55 DO $
;   FOR i=window_length/2,n_years-window_length/2-1 DO $
;      running_correlation(j,i-window_length/2)=CORRELATE(SW_index(j,i-window_length/2:i+window_length/2,*),$
;                                                         twentyc_cet(j*,i-window_length/2:i+window_length/2))

 ;Give the name for the PostScript format output file
;psfile='/home/pz002212/idl/uk_cold_winters_correlate_SW_20CR_CET_total.ps'

;This command opens the PostScript file and sets the font size and margins
;PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=3000,SPACE2=300,XOFFSET=1000,YOFFSET=2000,$
;       TFONT=2,TCHARSIZE=100

;GSET,XMIN=1870,YMIN=-0.5,XMAX=2012,YMAX=0.5,TITLE='Running Correlation between SW index and 20CR temperatures'

;years=findgen(n_years-10)+1875

;GPLOT,X=years,Y=running_correlation,COL=FSC_COLOR('RED')


; This command draws the axes
;AXES,XSTEP=10,YSTEP=0.1,XMINOR=5,YMINOR=0.05,XTITLE='Year',YTITLE='Correlation', ORIENTATION=45,ndecs=1

;PSCLOSE

; Compute running correlation for each month separately
window_length=21
running_monthly_correlation=fltarr(56,n_years-window_length+1,12)
FOR j=0,55 DO $
   FOR h=0,11 DO $
      FOR i=window_length/2,n_years-window_length/2-1 DO $
         running_monthly_correlation(j,i-window_length/2,h)=CORRELATE(SW_index(j,i-window_length/2:i+window_length/2,h),$
                                                                    twentyc_cet(j,i-window_length/2:i+window_length/2,h))

mean_running_correlation=fltarr(12,n_years-window_length+1)
FOR i=0,11 DO $
   FOR j=0,n_years-window_length DO $
      mean_running_correlation(i,j)=MEAN(running_monthly_correlation(*,j,i))

;pull out monthly running correlations
;jan_running_correlation=REFORM(running_monthly_correlation(0,0:n_years-window_length-1))
;feb_running_correlation=REFORM(running_monthly_correlation(1,0:n_years-window_length-1))
;mar_running_correlation=REFORM(running_monthly_correlation(2,0:n_years-window_length-1))
;apr_running_correlation=REFORM(running_monthly_correlation(3,0:n_years-window_length-1))
;may_running_correlation=REFORM(running_monthly_correlation(4,0:n_years-window_length-1))
;jun_running_correlation=REFORM(running_monthly_correlation(5,0:n_years-window_length-1))
;jul_running_correlation=REFORM(running_monthly_correlation(6,0:n_years-window_length-1))
;aug_running_correlation=REFORM(running_monthly_correlation(7,0:n_years-window_length-1))
;sep_running_correlation=REFORM(running_monthly_correlation(8,0:n_years-window_length-1))
;oct_running_correlation=REFORM(running_monthly_correlation(9,0:n_years-window_length-1))
;nov_running_correlation=REFORM(running_monthly_correlation(10,0:n_years-window_length-1))
;dec_running_correlation=REFORM(running_monthly_correlation(11,0:n_years-window_length-1))

; Give the name for the PostScript format output file
psfile='/home/ss901165/idl/uk_cold_winters/uk_cold_winters_correlate_SW_20CR_CET_ensmem_monthly.ps'

; This command opens the PostScript file and sets the font size and margins
PSOPEN,file=psfile,FONT=6,CHARSIZE=160,MARGIN=2000,SPACE2=300,XOFFSET=1000,YOFFSET=2000,$
       TFONT=6,TCHARSIZE=100

GSET,XMIN=1870,YMIN=-1,XMAX=2012,YMAX=1,TITLE='Running Monthly Correlation between SW index and 20CR temperatures'
years=findgen(n_years-window_length)+1871+window_length/2
CS,SCALE=28,NCOLS=13

; This command draws the line plot.  
FOR i=0,11 DO $
   GPLOT,X=years,Y=REFORM(mean_running_correlation(i,0:n_years-window_length-1)),COL=2+i

months=['January','Feburary','March','April','May','June','July','August','September','October','November','December']
GLEGEND,LABELS=REVERSE(months(0:3)),LEGPOS=4,SIZE=80,COL=REVERSE([2,3,4,5])
GLEGEND,LABELS=REVERSE(months(4:7)),LEGPOS=8,SIZE=80,COL=REVERSE([6,7,8,9])
GLEGEND,LABELS=REVERSE(months(8:11)),LEGPOS=12,SIZE=80,COL=REVERSE([10,11,12,13])


; This command draws the axes
AXES,XSTEP=10,YSTEP=0.1,XMINOR=5,YMINOR=0.05,XTITLE='Year',YTITLE='Correlation', ORIENTATION=30,ndecs=1
PSCLOSE,/NOVIEW

; Give the name for the PostScript format output file
psfile='/home/ss901165/idl/uk_cold_winters/uk_cold_winters_correlate_SW_20CR_CET_ensmem_djf.ps'

; This command opens the PostScript file and sets the font size and margins
PSOPEN,file=psfile,FONT=6,CHARSIZE=160,MARGIN=2000,SPACE2=100,XOFFSET=1000,YOFFSET=1000,$
       TFONT=6,TCHARSIZE=120,SPACE3=100

GSET,XMIN=1870,YMIN=0.2,XMAX=2012,YMAX=1;,TITLE='21-year windowed correlation between 20CR ensemble-member SWI and CET'
CS,SCALE=28,NCOLS=5

; This command draws the line plot.  
GPLOT,X=years,Y=REFORM(mean_running_correlation(11,0:n_years-window_length-1)),COL=FSC_COLOR('blue')
GPLOT,X=years,Y=REFORM(mean_running_correlation(0,0:n_years-window_length-1)),COL=FSC_COLOR('red')
GPLOT,X=years,Y=REFORM(mean_running_correlation(1,0:n_years-window_length-1)),COL=FSC_COLOR('purple')
GPLOT,X=[1870,2012],Y=[0.433,0.433],STYLE=2

months=['December','January','Feburary','5% significance level']
GLEGEND,LABELS=REVERSE(months),LEGPOS=11,SIZE=80,COL=REVERSE([FSC_COLOR(['blue','red','purple','black'])]),STYLE=REVERSE([0,0,0,2])

; This command draws the axes
AXES,XSTEP=10,YSTEP=0.05,XMINOR=5,YMINOR=0.025,XTITLE='Year',YTITLE='Correlation', ORIENTATION=25,ndecs=1
PSCLOSE

STOP


; Compute total RMSE
overall_rmse=SQRT(MEAN((SW_index-twentyc_cet)^2))

; Compute RMSE for each month separately
monthly_rmse=fltarr(12)
FOR i=0,11 DO $
   monthly_rmse(i)=SQRT(MEAN((REFORM(SW_index(i,*))-REFORM(twentyc_cet(i,*)))^2))

; Compute running RMSE using all months

window_length=11
running_rmse=fltarr(n_years-window_length+1)
FOR i=window_length/2,n_years-window_length/2-1 DO $
   running_rmse(i-window_length/2)=SQRT(MEAN((SW_index(*,i-window_length/2:i+window_length/2)-twentyc_cet(*,i-window_length/2:i+window_length/2))^2))

 ;Give the name for the PostScript format output file
psfile='/home/pz002212/idl/uk_cold_winters_running_rmse_SW_20CR_CET_total.ps'

;This command opens the PostScript file and sets the font size and margins
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=3000,SPACE2=300,XOFFSET=1000,YOFFSET=2000,$
       TFONT=2,TCHARSIZE=100



GSET,XMIN=1870,YMIN=0,XMAX=2012,YMAX=50,TITLE='Running RMSE for SW index and 20CR temperatures'

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
   running_monthly_rmse(h,i-window_length/2)=SQRT(MEAN((SW_index(h,i-window_length/2:i+window_length/2)-twentyc_cet(h,i-window_length/2:i+window_length/2))^2))


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
psfile='/home/pz002212/idl/uk_cold_winters_running_rmse_SW_20CR_CET_monthly.ps'

; This command opens the PostScript file and sets the font size and margins
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=3000,SPACE2=300,XOFFSET=1000,YOFFSET=2000,$
       TFONT=2,TCHARSIZE=100


GSET,XMIN=1870,YMIN=0,XMAX=2012,YMAX=6,TITLE='Running Monthly RMSE for SW index and 20CR temperatures'
years=findgen(n_years-10)+1875
CS,SCALE=28,NCOLS=13


; This command draws the line plot. 
GPLOT,X=years,Y=jan_running_rmse,COL=2
GPLOT,X=years,Y=feb_running_rmse,COL=3
GPLOT,X=years,Y=mar_running_rmse,COL=4
GPLOT,X=years,Y=apr_running_rmse,COL=5
GPLOT,X=years,Y=may_running_rmse,COL=6 
GPLOT,X=years,Y=jun_running_rmse,COL=7 
GPLOT,X=years,Y=jul_running_rmse,COL=8
GPLOT,X=years,Y=aug_running_rmse,COL=9
GPLOT,X=years,Y=sep_running_rmse,COL=10
GPLOT,X=years,Y=oct_running_rmse,COL=11 
GPLOT,X=years,Y=nov_running_rmse,COL=12
GPLOT,X=years,Y=dec_running_rmse,COL=13


months=['January','Feburary','March','April','May','June','July','August','September','October','November','December']
GLEGEND,LABELS=months,LEGPOS=9,SIZE=60,COL=[2,3,4,5,6,7,8,9,10,11,12,13]

; This command draws the axes
AXES,XSTEP=10,YSTEP=1,XMINOR=5,YMINOR=0.5,XTITLE='Year',YTITLE='RMSE', ORIENTATION=45


PSCLOSE



STOP
END
