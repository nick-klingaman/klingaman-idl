PRO plot_tropical_cyclone_tracks

; Specify season to plot (will be added to end of filename)
year='1979-1980'
track_infile='/home/ss901165/higam_qccce/hpcx_amip2_xcquc/tropical_cyclones/ff_trs.vor_fullgrid_wind_mslp_mask.'+year
;track_infile='/home/ss901165/datasets/IBTRACS/ff_trs.mslp.'+year
; Specify months included in season
months=['Oct','Nov','Dec','Jan','Feb','Mar','Apr','May']
n_months=N_ELEMENTS(months)
; Specify number of month corresponding to "months" above
month_numbers=['10','11','12','01','02','03','04','05']
month_colors=['purple','blue','cyan','green','orange','brown','red','pink']

; Open file for reading
openr,lun,track_infile,/GET_LUN
mystr=''
; Read past header information
FOR i=0,1 DO $
   readf,lun,mystr
; Read number of storms
readf,lun,mystr
temp=STRSPLIT(mystr,' ',/EXTRACT)
n_storms=temp(1)

; Start plot
psfile='/home/ss901165/idl/queensland/tropical_cyclones/plot_tropical_cyclone_tracks.higam_'+year+'.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=2000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
       YSIZE=10000,SPACE3=500
CS,SCALE=26,NCOLS=N_ELEMENTS(month_colors)
; Draw map
MAP,LATMIN=-45,LATMAX=0,LONMIN=90,LONMAX=270,/DRAW

; Read tropical-cyclone information
start_month=intarr(n_storms)
track_number=intarr(n_storms)
n_points=intarr(n_storms)
FOR i=0,n_storms-1 DO BEGIN
                                ; Get number of the track and the month of genesis
   readf,lun,mystr
   temp=STRSPLIT(mystr,' ',/EXTRACT)
   track_number(i)=temp(1)
   start_month(i)=STRMID(temp(3),4,2)
   readf,lun,mystr
                                ; Get number of points along track
   temp=STRSPLIT(mystr,' ',/EXTRACT)
   n_points(i)=temp(1)
   track_longitude=fltarr(n_points(i))
   track_latitude=fltarr(n_points(i))   
                                ; For each point along track, read latitude and longitude
   FOR j=0,n_points(i)-1 DO BEGIN
      readf,lun,date,templon,templat
      track_longitude(j)=templon
      track_latitude(j)=templat
   ENDFOR
                                ; Decide which color to make the track (by month)
   FOR j=0,n_months-1 DO $
      IF start_month(i) eq month_numbers(j) THEN $
         color=month_colors(j)
                                ; Plot the track if storm tracked within region of interest
   IF TOTAL(where(track_longitude lt 240 and track_longitude gt 100) gt 0) THEN BEGIN
      GPLOT,X=track_longitude,Y=track_latitude,COL=FSC_COLOR(color),THICK=50
      GPLOT,X=track_longitude(0),Y=track_latitude(0),COL=FSC_COLOR(color),SYM=3,SIZE=50
                                ; Only plot the lysis point if the lysis point is within the mapped region
      IF track_latitude(n_points(i)-1) gt -45 and track_longitude(n_points(i)-1) lt 240 THEN $
         GPLOT,X=track_longitude(n_points(i)-1),Y=track_latitude(n_points(i)-1),COL=FSC_COLOR(color),SYM=4,SIZE=50
   ENDIF
   AXES
ENDFOR
; Plot title
GPLOT,X=180,Y=5,ALIGN=0.5,TEXT='Tropical cyclone tracks for higam for season '+year+' (October-May) - all tracks'
; Legends
GLEGEND,labels=REVERSE(months),COL=REVERSE(FSC_COLOR(month_colors)),LEGPOS=9
GLEGEND,labels=['Lysis location','Genesis location'],SYM=[4,3],LEGPOS=1

PSCLOSE

; Release the text file from IDL
FREE_LUN,lun

END

