PRO mjo_enso_integrate_rmm2_amplitude
  
; Plot time-integrated RMM2 amplitude in each phase for specified periods

rmm_infile='/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1979-2008.index_values.nc'
infile_start_year=1979

; What periods do you want to plot?  All periods will be shown on one plot, in different colors.
years=[1994,1997,2002,2006]
start_days=[122,122,122,122] ; Julian dates
stop_days=[212,212,212,212]  ; Julian dates

; Colours to use (for a list of colors available, see http://solarmuri.ssl.berkeley.edu/~welsch/public/software/ssw_stuff/fsc_color.pro)
colors=['blue','red','turquoise','violetred','orange','brown']

; Get RMM1 and RMM2 from observations
n_periods=N_ELEMENTS(years)
counts=stop_days-start_days+1
max_count=MAX(counts)
int_rmm2_phase=fltarr(n_periods,8)

FOR i=0,n_periods-1 DO BEGIN   
   this_start=DAYS_SINCE([1,infile_start_year],[start_days(i),years(i)])
   print,this_start   
                                ; Change amplitude_ts and phase_ts to IHR_amp and IHR_phase for the Oliver and Thompson index
   obs_amp=REFORM(OPEN_AND_EXTRACT(rmm_infile,'amplitude_ts',$
                                   offset=[this_start],count=[counts(i)]))
   obs_rmm2=REFORM(OPEN_AND_EXTRACT(rmm_infile,'rmm2_ts',$
                                    offset=[this_start],count=[counts(i)]))
   obs_phase=REFORM(OPEN_AND_EXTRACT(rmm_infile,'phase_ts',$
                                     offset=[this_start],count=[counts(i)]))
   FOR j=0,7 DO BEGIN
      strong_days=where(obs_amp ge 1 and obs_phase eq j+1)
      IF TOTAL(strong_days) ge 0 THEN BEGIN
         int_rmm2_phase(i,j)=TOTAL(obs_rmm2[strong_days])
      ENDIF ELSE $
         int_rmm2_phase(i,j)=0
   ENDFOR
ENDFOR

; Read all years between these Julian dates to work out climatological number of days in each phase
; This will work only if you are using the same range of Julian dates (start_days and stop_days) for all years you are reading.
n_years=30 ; Number of years in file
clim_int_rmm2_phase=fltarr(8)
FOR i=0,n_years-1 DO BEGIN
   this_start=DAYS_SINCE([1,infile_start_year],[start_days(0),infile_start_year+i])
   obs_amp=REFORM(OPEN_AND_EXTRACT(rmm_infile,'amplitude_ts',$
                                   offset=[this_start],count=[max_count]))
   obs_rmm2=REFORM(OPEN_AND_EXTRACT(rmm_infile,'rmm2_ts',$
                                   offset=[this_start],count=[max_count]))
   obs_phase=REFORM(OPEN_AND_EXTRACT(rmm_infile,'phase_ts',$
                                     offset=[this_start],count=[max_count]))
   FOR j=0,7 DO BEGIN
      strong_days=where(obs_amp ge 1 and obs_phase eq j+1)
      IF TOTAL(strong_days) ge 0 THEN $
         clim_int_rmm2_phase(j)=clim_int_rmm2_phase(j)+TOTAL(obs_rmm2[strong_days])     
   ENDFOR
ENDFOR
clim_int_rmm2_phase=clim_int_rmm2_phase/FLOAT(n_years)

; Setup PostScript file
psfile='/home/ss901165/idl/mjo_enso/mjo_enso_integrate_rmm2_amplitude.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=115,MARGIN=1500,SPACE3=1500,XOFFSET=1500,YOFFSET=500,TFONT=2,TCHARSIZE=100
ymin=-50
ymax=50
ystep=(ymax-ymin)/20.
GSET,XMIN=0,XMAX=8,YMIN=ymin,YMAX=ymax

; Plot the lines for each period
GPLOT,X=0.5,Y=ymax-ystep,TEXT='Integral of phases 1-4:',CHARSIZE=120,ALIGN=0.0
GPLOT,X=5.5,Y=(n_periods+1)*ystep+(ymin+ystep),TEXT='Integral of phases 5-8:',CHARSIZE=120,ALIGN=0.0
GPLOT,X=0.5,Y=ymax-ystep*2,TEXT=STRMID(STRTRIM(STRING(TOTAL(clim_int_rmm2_phase(0:3))),1),0,4),CHARSIZE=120,ALIGN=0.0
GPLOT,X=5.5,Y=(n_periods)*ystep+(ymin+ystep),TEXT=STRMID(STRTRIM(STRING(TOTAL(clim_int_rmm2_phase(4:7))),1),0,4),CHARSIZE=120,ALIGN=0.0
FOR i=0,n_periods-1 DO BEGIN
   GPLOT,X=indgen(8)+0.5,Y=REFORM(int_rmm2_phase(i,*)),STYLE=0,THICK=150,COL=FSC_COLOR(colors(i)),SYM=i+2
   GPLOT,X=0.5+ODD(i),Y=(ymax-ystep*3)-i/2*ystep,TEXT=STRMID(STRTRIM(STRING(TOTAL(int_rmm2_phase(i,0:3))),1),0,4),$
         COL=FSC_COLOR(colors(i)),CHARSIZE=120,ALIGN=0.0
   GPLOT,X=5.5+ODD(i),Y=(n_periods-i/2-1)*ystep+(ymin+ystep),TEXT=STRMID(STRTRIM(STRING(TOTAL(int_rmm2_phase(i,4:7))),1),0,4),$
         COL=FSC_COLOR(colors(i)),CHARSIZE=120,ALIGN=0.0
ENDFOR

; Plot climatological amplitude of RMM2
GPLOT,X=indgen(8)+0.5,Y=clim_int_rmm2_phase,STYLE=2,THICK=100,COL=FSC_COLOR('black'),SYM=1,SIZE=80
; Plot dotted line at zero
GPLOT,X=[0,8],Y=[0,0],THICK=100,STYLE=1

AXES,XVALS=indgen(8)+0.5,XLABELS=['1','2','3','4','5','6','7','8'],YSTEP=ystep,YMINOR=1,XTITLE='Phase',$
     YTITLE='Time-integrated RMM2 on days when MJO is strong (amplitude > 1)',NDECS=2

; Draw a legend to the right of the plot, using the years as labels
GLEGEND,labels=REVERSE([STRTRIM(STRING(years)),'Climatology']),COL=REVERSE(FSC_COLOR([colors(0:n_periods-1),'black'])),$
        SYM=REVERSE([indgen(n_periods)+2,1]),LEGPOS=7,STYLE=REVERSE([REPLICATE(0,n_periods),2])

PSCLOSE

STOP

END
