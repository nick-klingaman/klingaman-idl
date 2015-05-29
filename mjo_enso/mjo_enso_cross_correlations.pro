PRO mjo_enso_cross_correlations
  
; All IDL programs begin with PRO, then the name of the program
; (no spaces, must begin with a letter).  It helps to have the program
; name be the same as the name of the file, minus the .pro extension.

; In IDL, comments start with a semi-colon, like this line.

; This program plots the amplitude of the MJO (from the Wheeler and 
; Hendon 2004 indices) and a Nino SST index.

; Specify the file from which to read the MJO index.
;mjo_input_file='/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_mmeans_ts.1979-2008.index_values.nc'
  mjo_input_file='/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_mmeans_ts.1979-2012.with_enso.nc'
; Specify the file from which to read the ENSO index.
enso_input_file='/home/ss901165/datasets/NINO/allnino_hadisst.jan-dec_mmeans.1979-2008.nc'

; Read the 'amplitude' variable from the MJO input file,
; to extract the MJO amplitude from 1979-2008.
mjo_amplitude=OPEN_AND_EXTRACT(mjo_input_file,'rmm2_ts',offset=[0],count=[360])
; Read the NINO SST index from the ENSO input file 
; to extract the ENSO amplitude from 1979-2008 (available variables: nino12, nino3, nino34, nino4)
enso_amplitude=OPEN_AND_EXTRACT(enso_input_file,'nino3')

; Set up the lags (in months) for ENSO with respect to the MJO
lags=[0,1,2,3,4,5,6,7,8,9,10,11]
n_lags=N_ELEMENTS(lags) ; Computed automatically

; Set up the starting months (months of MJO activity) for which to compute the correlation
months=[1,2,3,4,5,6,7,8,9,10,11,12]
n_months=N_ELEMENTS(months) ; Computed automatically
; Names of the months for the plot
month_names=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']

; Set up a matrix to hold the correlation coefficient values
cross_correlation_coefficients=fltarr(n_lags,n_months)

; Compute the lag corrlelations
FOR i=0,n_months-1 DO BEGIN
   ; Pull MJO amplitudes for this month from all years
   thismonth_mjo_amplitude=mjo_amplitude(i:N_ELEMENTS(mjo_amplitude)-1:12)
   FOR j=0,n_lags-1 DO BEGIN
      ; Pull ENSO amplitudes for this month (i) + lag months (j)
      shifted_enso_amplitudes=enso_amplitude(i+j:N_ELEMENTS(enso_amplitude)-1:12)      
      ; Compute the correlation between the two timeseries
      cross_correlation_coefficients(j,i)=CORRELATE(thismonth_mjo_amplitude,shifted_enso_amplitudes)
   ENDFOR
ENDFOR

; Plot the cross-correlation coeffients as a contour plot

; Set the location of the output PostScript file on the Unix system.
postscript_file='/home/ss901165/idl/mjo_enso/mjo_enso_cross_correlations.wheeler_hendon.1979-2008.ps'
; Create the PostScript output file.  See the PSOPEN documentation for what these options mean.
PSOPEN,file=postscript_file,TFONT=2,CHARSIZE=120,SPACE2=1000
; Set up the boundaries of the plot and the title
GSET,XMIN=0,XMAX=MAX(lags),YMIN=1,YMAX=n_months,TITLE='Lag correlations of monthly-mean MJO and ENSO'
; Levels at which to draw contour lines
levels=['-0.5','-0.4','-0.3','-0.2','-0.1','0','0.1','0.2','0.3','0.4','0.5']
LEVS,MANUAL=levels
; Select a color scale and give the number of colors to use from that scale (number of levels + 1)
CS,SCALE=1,NCOLS=N_ELEMENTS(levels)+1
; Draw the contour plot
CON,X=lags,Y=months,FIELD=cross_correlation_coefficients
; Draw horizontal and vertical axes, with appropriate labels.  Do not draw the right-hand vertical axis (/NORIGHT), because
; we will use this for the ENSO timeseries.
AXES,XSTEP=1,YVALS=months,YLABELS=month_names,$
     YMINOR=0.5,XMINOR=0.5,XTITLE='Lag time (months) for ENSO',YTITLE='Starting month (month of MJO activity)'
; Close the plot and display it on the screen.  To stop displaying it, add ,/NOVIEW to the PSCLOSE command
PSCLOSE

; End the IDL program
STOP
END

