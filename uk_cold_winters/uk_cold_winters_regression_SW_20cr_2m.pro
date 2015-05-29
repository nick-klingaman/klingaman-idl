PRO uk_cold_winters_regression_SW_20CR_2m
  
; Make a contingency table for the NAO against 20CR

; Starting year for analysis
my_start_year=1871

; Ending year for analysis
my_stop_year=2007

; Total number of years
n_years=my_stop_year-my_start_year+1


; Input file
SW_index_infile='/home/ss901165/datasets/20THC_REANALYSIS/wind_speed/20thc_reanalysis.dec-feb_smeans.1871-2007.swwind_box_index_850.nc'
SW_index_start_year=1871

; Input file 
twentyc_2m_infile='/home/ss901165/datasets/20THC_REANALYSIS/temp_2m/20thc_reanalysis.dec-feb_smeans.1871-2007.temp2m.nc'
twentyc_2m_start_year=1871

twentyc_latitude=OPEN_AND_EXTRACT(twentyc_2m_infile,'latitude')
twentyc_longitude=OPEN_AND_EXTRACT(twentyc_2m_infile,'longitude')
twentyc_nlat=N_ELEMENTS(twentyc_latitude)
twentyc_nlon=N_ELEMENTS(twentyc_longitude)




; Read indices
SW_index=OPEN_AND_EXTRACT(SW_index_infile,'swwind_index',offset=[my_start_year-SW_index_start_year],$
                           count=[n_years])
twentyc_2m=OPEN_AND_EXTRACT(twentyc_2m_infile,'TMP',offset=[0,0,(my_start_year-twentyc_2m_start_year)],$
                           count=[twentyc_nlon,twentyc_nlat,n_years])



overall_regression=fltarr(twentyc_nlon,twentyc_nlat)

FOR j=0,twentyc_nlon-1 DO $
      FOR k=0,twentyc_nlat-1 DO $
         
            
   overall_regression(j,k)=REGRESS(SW_index(*),REFORM(twentyc_2m(j,k,*)))

















;overall_regression[where(ABS(overall_correlation) lt threshold_value)]=!Values.F_NaN

;overall_regression=REGRESS(SW_index,twentyc_2m)
; Compute correlation for each month separately
;overall_regression=fltarr(
;FOR i=0,11 DO $
 ;  monthly_regression(i)=REGRESS(REFORM(SW_index(i,*)),REFORM(twentyc_cet(i,*)))

; Compute running correlation using all months
; Define window length for running correlation (in years) - must be an odd number
;window_length=11
;running_regression=fltarr(n_years-window_length+1)
;FOR i=window_length/2,n_years-window_length/2-1 DO $
 ;  running_regression(i-window_length/2)=REGRESS(SW_index(*,i-window_length/2:i+window_length/2),$
;                                                    twentyc_cet(*,i-window_length/2:i+window_length/2))

 ;Give the name for the PostScript format output file
;psfile='/home/pz002212/idl/uk_cold_winters_regression_SW_20CR_CET.ps'

;This command opens the PostScript file and sets the font size and margins
;PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=3000,SPACE2=300,XOFFSET=1000,YOFFSET=2000,$
 ;      TFONT=2,TCHARSIZE=100

;GSET,XMIN=1870,YMIN=-0.5,XMAX=2012,YMAX=0.5,TITLE='Regression between SW index and 20CR temperatures'

;years=findgen(n_years-10)+1875

;GPLOT,X=twentyc_cet,Y=overall_regression,COL=FSC_COLOR('RED')


; This command draws the axes
;AXES,XSTEP=10,YSTEP=0.1,XMINOR=5,YMINOR=0.05,XTITLE='20CR CET',YTITLE='SW INDEX', ORIENTATION=45,ndecs=1




STOP
END

