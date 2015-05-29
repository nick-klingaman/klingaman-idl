PRO mjo_enso_running_correlations
  
; Plot running correlations between MJO in one month and ENSO in another month 
; (or average of months), using a window of specified length.

mjo_input_file='/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_mmeans_ts.1979-2008.index_values.nc'
enso_input_file='/home/ss901165/datasets/NINO/allnino_hadisst.jan-dec_mmeans.1979-2008.nc'

; Set start year and number of years
start_year=1979
n_years=30

; Set window length for running correlation (should be an odd number)
window_length=11

; Open MJO input file and read amplitude values
mjo_amplitude=OPEN_AND_EXTRACT(mjo_input_file,'amplitude')

; Open ENSO input file and read Nino values
enso_amplitude=OPEN_AND_EXTRACT(enso_input_file,'nino4')

; Determine number of months in MJO and Nino timeseries
mjo_nmonths=N_ELEMENTS(mjo_amplitude)
enso_nmonths=N_ELEMENTS(enso_amplitude)

; Select month of MJO forcing (note: 0 = January)
mjo_month=4
; Select month or months for ENSO response.  
; Use one value for a single month and two values in square brackets, 
; separated by a comma, for an average.  To wrap around the end of a
; year, use the month+12 (e.g., [11,13] for December-February).
; example of one value: enso_month=11
; example of two values: enso_month=[11,13]
enso_month=[11,13]

mjo_timeseries=mjo_amplitude(mjo_month:mjo_nmonths-1:12)
IF N_ELEMENTS(enso_month) eq 1 THEN BEGIN
   enso_timeseries=enso_amplitude(enso_month:enso_nmonths-1:12)
ENDIF ELSE IF N_ELEMENTS(enso_month) eq 2 THEN BEGIN
   IF enso_month(1) gt 11 THEN BEGIN
      enso_timeseries=fltarr(n_years-1)
   ENDIF ELSE $
      enso_timeseries=fltarr(n_years)
   FOR i=0,N_ELEMENTS(enso_timeseries)-1 DO BEGIN
      enso_timeseries(i)=MEAN(enso_amplitude(12*i+enso_month(0):12*i+enso_month(1)))
      print,12*i+enso_month(0),12*i+enso_month(1)
   ENDFOR
ENDIF

running_correlation=fltarr(n_years)
FOR i=window_length/2,N_ELEMENTS(enso_timeseries)-window_length/2-1 DO BEGIN
   this_window_mjo=mjo_timeseries(i-window_length/2:i+window_length/2)
   this_window_enso=enso_timeseries(i-window_length/2:i+window_length/2)
   running_correlation(i)=CORRELATE(this_window_mjo,this_window_enso)
ENDFOR
running_correlation(0:window_length/2)=!Values.F_NaN
running_correlation(n_years-window_length/2:N_ELEMENTS(enso_timeseries)-1)=!Values.F_NaN

psfile='/home/ss901165/idl/mjo_enso/mjo_enso_running_correlations.window_length_'+STRTRIM(STRING(window_length),1)+'years.ps'
PSOPEN,file=psfile,TFONT=2,CHARSIZE=120
GSET,XMIN=start_year,XMAX=start_year+n_years,YMIN=-1,YMAX=1
GPLOT,X=indgen(n_years)+0.5+start_year,Y=running_correlation
AXES,XSTEP=2,YSTEP=0.2,YMINOR=0.1,YTITLE='Running correlation of MJO and ENSO',XTITLE='Year in centre of window'
PSCLOSE

STOP
END
