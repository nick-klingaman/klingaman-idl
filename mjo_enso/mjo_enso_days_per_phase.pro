PRO mjo_enso_days_per_phase
  
; Plot days of strong MJO activity in each WH04/OT12 phase, within a given period

rmm_infile='/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1979-2008.index_values.nc'
infile_start_year=1979

; What periods do you want to plot?  All periods will be shown on one plot, in different colors.
years=[1994,1997]
start_days=[183,183] ; Julian dates
stop_days=[212,212]  ; Julian dates

; Colours to use (for a list of colors available, see http://solarmuri.ssl.berkeley.edu/~welsch/public/software/ssw_stuff/fsc_color.pro)
colors=['blue','red','turquoise','violetred','orange','brown']

; Get RMM1 and RMM2 from observations
n_periods=N_ELEMENTS(years)
counts=stop_days-start_days+1
max_count=MAX(counts)
n_days_per_phase=fltarr(n_periods,8)

FOR i=0,n_periods-1 DO BEGIN   
   this_start=DAYS_SINCE([1,infile_start_year],[start_days(i),years(i)])
   print,this_start   
                                ; Change amplitude_ts and phase_ts to IHR_amp and IHR_phase for the Oliver and Thompson index
   obs_amp=REFORM(OPEN_AND_EXTRACT(rmm_infile,'amplitude_ts',$
                                   offset=[this_start],count=[counts(i)]))
   obs_phase=REFORM(OPEN_AND_EXTRACT(rmm_infile,'phase_ts',$
                                     offset=[this_start],count=[counts(i)]))
   FOR j=0,7 DO BEGIN
      strong_days=where(obs_amp ge 1 and obs_phase eq j+1)
      IF TOTAL(strong_days) ge 0 THEN BEGIN
         n_days_per_phase(i,j)=N_ELEMENTS(strong_days)
      ENDIF ELSE $
         n_days_per_phase(i,j)=0
   ENDFOR
ENDFOR

; Read all years between these Julian dates to work out climatological number of days in each phase
; This will work only if you are using the same range of Julian dates (start_days and stop_days) for all years you are reading.
n_years=30 ; Number of years in file
clim_ndays_per_phase=fltarr(8)
FOR i=0,n_years-1 DO BEGIN
   this_start=DAYS_SINCE([1,infile_start_year],[start_days(0),infile_start_year+i])
   obs_amp=REFORM(OPEN_AND_EXTRACT(rmm_infile,'amplitude_ts',$
                                   offset=[this_start],count=[max_count]))
   obs_phase=REFORM(OPEN_AND_EXTRACT(rmm_infile,'phase_ts',$
                                     offset=[this_start],count=[max_count]))
   FOR j=0,7 DO BEGIN
      strong_days=where(obs_amp ge 1 and obs_phase eq j+1)
      IF TOTAL(strong_days) ge 0 THEN $
         clim_ndays_per_phase(j)=clim_ndays_per_phase(j)+N_ELEMENTS(strong_days)
   ENDFOR
ENDFOR
;print,clim_ndays_per_phase,TOTAL(clim_ndays_per_phase)/FLOAT(n_years*max_count)
clim_ndays_per_phase=clim_ndays_per_phase/FLOAT(n_years)

; Setup PostScript file
psfile='/home/ss901165/idl/mjo_enso/mjo_enso_days_per_phase.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=115,MARGIN=1500,SPACE3=1500,XOFFSET=1500,YOFFSET=500,TFONT=2,TCHARSIZE=100
GSET,XMIN=0,XMAX=8,YMIN=0,YMAX=30

; Plot the lines for each period
FOR i=0,n_periods-1 DO $
   GPLOT,X=indgen(8)+0.5,Y=REFORM(n_days_per_phase(i,*)),STYLE=0,THICK=150,COL=FSC_COLOR(colors(i)),SYM=i+2
; Plot climatological number of days in each phase
GPLOT,X=indgen(8)+0.5,Y=clim_ndays_per_phase,STYLE=2,THICK=100,COL=FSC_COLOR('black'),SYM=1,SIZE=80

AXES,XVALS=indgen(8)+0.5,XLABELS=['1','2','3','4','5','6','7','8'],YSTEP=2,YMINOR=1,XTITLE='Phase',YTITLE='Number of days of strong MJO activity (amplitude > 1)'

; Draw a legend to the right of the plot, using the years as labels
GLEGEND,labels=REVERSE([STRTRIM(STRING(years)),'Climatology']),COL=REVERSE(FSC_COLOR([colors(0:n_periods-1),'black'])),$
        SYM=REVERSE([indgen(n_periods)+2,1]),LEGPOS=1,STYLE=REVERSE([REPLICATE(0,n_periods),2])
PSCLOSE

STOP

END
