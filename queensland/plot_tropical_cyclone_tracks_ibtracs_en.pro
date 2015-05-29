PRO plot_tropical_cyclone_tracks_ibtracs_en

; Specify season to plot (will be added to end of filename)
;years=['1983-1984','1984-1985','1988-1989','1998-1999','1999-2000','2000-2001','2007-2008']
years=['1981-1982','1985-1986','1986-1987','1990-1991','1993-1994','1996-1997','2001-2002'];['1981-1982','1985-1986','1986-1987','1990-1991','1993-1994','1996-1997','2002-2003','2004-2005','2006-2007'];['1982-1983','1983-1984','1987-1988','1997-1998','1998-1999','1999-2000','2006-2007']
n_years=N_ELEMENTS(years)

;track_infile='/home/ss901165/datasets/IBTRACS/ff_trs.mslp.'+year
; Specify months included in season
;months=['Oct','Nov','Dec','Jan','Feb','Mar','Apr','May']
months=['Dec','Jan','Feb']
n_months=N_ELEMENTS(months)
; Specify number of month corresponding to "months" above
month_numbers=['12','01','02']
month_colors=['red','red','red']

; Start plot
psfile='/home/ss901165/idl/queensland/ibtracs_tracks_en.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=2000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
       YSIZE=10000,SPACE3=500,XSIZE=15000
CS,SCALE=26,NCOLS=N_ELEMENTS(month_colors)
; Draw map
MAP,LATMIN=-45,LATMAX=0,LONMIN=110,LONMAX=210,/DRAW

; Loop to read in data from array of all years.
FOR k=0,n_years-1 DO BEGIN
   track_infile='/home/ss901165/datasets/IBTRACS/ff_trs.mslp.'+years(k)
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
   IF track_longitude(0) gt 160 or track_longitude(0) lt 145 THEN BEGIN
      IF TOTAL(where(track_longitude lt 240 and track_longitude gt 100) gt 0) THEN BEGIN
         GPLOT,X=track_longitude,Y=track_latitude,COL=FSC_COLOR(color),THICK=50
         GPLOT,X=track_longitude(0),Y=track_latitude(0),COL=FSC_COLOR(color),SYM=3,SIZE=50
                                ; Only plot the lysis point if the lysis point is within the mapped region
         IF track_latitude(n_points(i)-1) gt -45 and track_longitude(n_points(i)-1) lt 240 THEN $
            GPLOT,X=track_longitude(n_points(i)-1),Y=track_latitude(n_points(i)-1),COL=FSC_COLOR(color) ;,SYM=4,SIZE=50
      ENDIF
   ENDIF
ENDFOR
   ;AXES
ENDFOR

; Plot title
;GPLOT,X=180,Y=5,ALIGN=0.5,TEXT='Tropical cyclones during La Nina'
; Legends
;LEGEND,labels=REVERSE(months),COL=REVERSE(FSC_COLOR(month_colors)),LEGPOS=9
;LEGEND,labels=['Lysis location','Genesis location'],SYM=[4,3],LEGPOS=1
GPLOT,Y=-16-57./60.,X=145+45./60.,SYM=6,COL=FSC_COLOR('black')
GPLOT,Y=-19.2,X=142+45./60.,TEXT='Cairns',COL=FSC_COLOR('black'),CHARSIZE=90

FREE_LUN,lun

PSCLOSE

; Release the text file from IDL

END

