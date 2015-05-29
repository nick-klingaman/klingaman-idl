PRO mjo_enso_plot_amplitude_smoothed
  
; All IDL programs begin with PRO, then the name of the program
; (no spaces, must begin with a letter).  It helps to have the program
; name be the same as the name of the file, minus the .pro extension.

; In IDL, comments start with a semi-colon, like this line.

; This program plots the amplitude of the MJO (from the Wheeler and 
; Hendon 2004 indices) and a Nino SST index.

; Specify the file from which to read the MJO index.
mjo_input_file='/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1979-2011.index_values.nc'
; Specify the file from which to read the ENSO index.
enso_input_file='/home/ss901165/datasets/NINO/allnino_hadisst.jan-dec_mmeans.1979-2011.nc'

; Read the 'amplitude' variable from the MJO input file,
; to extract the MJO amplitude from 1979-2011.
mjo_amplitude=OPEN_AND_EXTRACT(mjo_input_file,'amplitude')
; Read the NINO SST index from the ENSO input file 
; to extract the ENSO amplitude from 1979-2011 (available variables: nino12, nino3, nino34, nino4)
enso_amplitude=OPEN_AND_EXTRACT(enso_input_file,'nino3')

; The amplitude variable comes as a two-dimensional matrix of day_in_year x year (366 x 33)
; We want to make a single timeseries of length 366x33.
n_days=33*366
n_months=33*12
mjo_amplitude=REFORM(mjo_amplitude,[n_days])
; Mask missing values in the amplitude timeseries.
mjo_amplitude[where(mjo_amplitude le -9999)]=!Values.F_NaN

; Now smooth the MJO amplitude timeseries using a 91-day running mean.  You can change
; this to a different running-mean length, but it must be an odd number to centre the window.
running_mean_length=181
mjo_amplitude_smoothed=SMOOTH(mjo_amplitude,running_mean_length,/NaN)
; Smooth the monthly-mean ENSO amplitude timeseries, using the length of the MJO running mean
; divided by 30 (to convert from days to months)
enso_amplitude_smoothed=SMOOTH(enso_amplitude,running_mean_length/30,/NaN)

; We have to throw away the first running_mean_length/2 days and the last running_mean_length/2
; days, because these cannot be computed (although IDL still tries). 
; !Values.F_NaN is the IDL missing value.
mjo_amplitude_smoothed(0:running_mean_length/2)=!Values.F_NaN
mjo_amplitude_smoothed(n_days-running_mean_length/2-1:n_days-1)=!Values.F_NaN
; Throw away the first running_mean_length/60 and the last running_mean_length/60 months
; of the ENSO timeseries
enso_amplitude_smoothed(0:running_mean_length/60)=!Values.F_NaN
enso_amplitude_smoothed(n_months-running_mean_length/60-1:n_months-1)=!Values.F_NaN

; Plot the MJO timeseries as a line graph, using the IDL GUIDE routines.

; Set the location of the output PostScript file on the Unix system.
postscript_file='/home/ss901165/idl/mjo_enso/mjo_enso_plot_amplitude_smoothed.'+STRTRIM(STRING(running_mean_length),1)+'day_mean.ps'
; Create the PostScript output file.  See the PSOPEN documentation for what these options mean.
PSOPEN,file=postscript_file,TFONT=2,CHARSIZE=120
; Set up the boundaries of the plot and the title
GSET,XMIN=0,XMAX=n_days,YMIN=0,YMAX=2.4,TITLE='MJO and ENSO amplitude with a '+STRTRIM(STRING(running_mean_length),1)+'-day running mean'
; Plot the smoothed MJO amplitude timeseries
GPLOT,X=indgen(n_days),Y=mjo_amplitude_smoothed,COL=FSC_COLOR('red')
; Draw horizontal and vertical axes, with appropriate labels.  Do not draw the right-hand vertical axis (/NORIGHT), because
; we will use this for the ENSO timeseries.
AXES,XVALS=indgen(n_days/366)*366,XLABELS=indgen(n_days/366)+1979,YSTEP=0.2,YMINOR=0.1,NDECS=2,ORIENTATION=35,$
     XTITLE='Year (January is labelled)',YTITLE='MJO amplitude',/NORIGHT

; Start a new set of axes for the ENSO amplitude timeseries
GSET,XMIN=0,XMAX=n_months,YMIN=-2.8,YMAX=4
; Plot the smoothed ENSO amplitude timeseries
GPLOT,X=indgen(n_months),Y=enso_amplitude_smoothed,COL=FSC_COLOR('blue')
; Draw only a new right-hand vertical axis
AXES,YSTEP=0.4,YMINOR=0.2,NDECS=2,/ONLYRIGHT,YTITLE='ENSO amplitude (SST anomalies from NINO3 region)'

; Close the plot and display it on the screen.  To stop displaying it, add ,/NOVIEW to the PSCLOSE command
PSCLOSE

; End the IDL program
STOP
END

