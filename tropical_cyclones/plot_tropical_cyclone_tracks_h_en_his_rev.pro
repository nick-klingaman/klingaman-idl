PRO plot_tropical_cyclone_tracks_h_en_his_rev

; Specify season to plot (will be added to end of filename)
years=['1982-1983','1986-1987','1987-1988','1991-1992','1994-1995','1997-1998','2002-2003','2004-2005','2006-2007']
;years=['1983-1984','1984-1985','1988-1989','1998-1999','1999-2000','2000-2001']
n_years=N_ELEMENTS(years)

;track_infile='/home/ss901165/datasets/IBTRACS/ff_trs.mslp.'+year
; Specify months included in season
months=['Oct','Nov','Dec','Jan','Feb','Mar','Apr','May']
n_months=N_ELEMENTS(months)
; Specify number of month corresponding to "months" above
month_numbers=['10','11','12','01','02','03','04','05']
month_colors=['purple','blue','cyan','green','orange','brown','red','pink']

; Start plot
psfile='/home/swu07adk/tropicalcyclones/higam_tracks_en_hist_rev.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=2000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
       YSIZE=10000,SPACE3=500
CS,SCALE=26,NCOLS=N_ELEMENTS(month_colors)

; Loop to read in data from array of all years.
total_storms=0
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
   total_storms=total_storms+n_storms
   FREE_LUN,lun
ENDFOR

start_month=intarr(total_storms)
track_number=intarr(total_storms)
n_points=intarr(total_storms)
cumulative_storms=0
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
   
   FOR i=0,n_storms-1 DO BEGIN
                                ; Get number of the track and the month of genesis
      readf,lun,mystr
      temp=STRSPLIT(mystr,' ',/EXTRACT)

      track_number(cumulative_storms+i)=temp(1)
      start_month(cumulative_storms+i)=STRMID(temp(3),4,2)
      readf,lun,mystr
                                ; Get number of points along track
      temp=STRSPLIT(mystr,' ',/EXTRACT)
      n_points(cumulative_storms+i)=temp(1)
      track_longitude=fltarr(n_points(cumulative_storms+i))
      track_latitude=fltarr(n_points(cumulative_storms+i))   
  
                                ; For each point along track, read latitude and longitude
      FOR j=0,n_points(cumulative_storms+i)-1 DO BEGIN
         readf,lun,date,templon,templat
         track_longitude(j)=templon
         track_latitude(j)=templat
         IF j eq 0 THEN BEGIN
            IF track_longitude(j) ge 200 or track_longitude(j) le 120 $
               or track_latitude(j) le -25 THEN $
                  start_month(cumulative_storms+i)=-9999
         ENDIF
      ENDFOR
                                ; Decide which color to make the track (by month)
      FOR j=0,n_months-1 DO $
         IF start_month(cumulative_storms+i) eq month_numbers(j) THEN $
            color=month_colors(j)
   ENDFOR
   cumulative_storms=cumulative_storms+i
ENDFOR

; Release the text file from IDL
FREE_LUN,lun
lanina_tcs=[19,18,19,19,23,32,23,15]
elnino_tcs=[16,16,18,19,23,19,12,8]

; Draw histogram
GSET,XMIN=0,XMAX=8,YMIN=0,YMAX=6,TITLE='Months when tropical cyclones formed in the south-west Pacific region in average of El Nino and La Nina years from HiGAM'
histogram_output=HISTOGRAM(start_month,binsize=1,min=1,max=8)
HIST,X=indgen(8)+0.35,Y=elnino_tcs/FLOAT(6),WIDTH=200,FILLCOL=8
HIST,X=indgen(8)+0.65,Y=lanina_tcs/FLOAT(6),WIDTH=200,FILLCOL=2
xlabels=['Oct','Nov','Dec','Jan','Feb','Mar','Apr','May']
xvals=INDGEN(8)+0.5
AXES,XVALS=xvals,XLABELS=xlabels,YSTEP=1,NDECS=0,XTITLE='Months',YTITLE='Number of Tropical Cyclones/Year/Month'
GLEGEND,LEGPOS=9,LABELS=['El Nino','La Nina'],COL=[8,2],TYPE=1

; Plot title
GPLOT,X=180,Y=5,ALIGN=0.5,TEXT='Histogram of months tropical cyclones formed in in HiGAM El Nino and La Nina years'

PSCLOSE

STOP
END

