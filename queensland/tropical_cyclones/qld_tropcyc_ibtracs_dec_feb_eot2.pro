PRO qld_tropcyc_ibtracs_dec_feb_eot2

eot_infile='/home/ss901165/datasets_mango/SILO/one_quarter/SILO.dec-feb_smeans.1900-2007.eots.nc'
eot=2
ibtracs_offset=78
n_years_ibtracs=30
n_years_total=108

eot_ts=REFORM(OPEN_AND_EXTRACT(eot_infile,'loading',$
                               offset=[0,eot-1],count=[n_years_total,1]))

years_plus_stddev=where(eot_ts(ibtracs_offset:n_years_total-1) ge MEAN(eot_ts)+STDDEV(eot_ts))+ibtracs_offset+1900
years_minus_stddev=where(eot_ts(ibtracs_offset:n_years_total-1) le MEAN(eot_ts)-STDDEV(eot_ts))+ibtracs_offset+1900

nyears_plus=N_ELEMENTS(years_plus_stddev)
nyears_minus=N_ELEMENTS(years_minus_stddev)

FOR i=0,1 DO BEGIN
   CASE i OF
      0 : BEGIN
         type='plus'
         years=years_plus_stddev
         nyears=nyears_plus
      END
      1 : BEGIN
         type='minus'
         years=years_minus_stddev
         nyears=nyears_minus
      END
   ENDCASE
                                ; Specify months included in season
   months=['December','January','February']
   n_months=N_ELEMENTS(months)
                                ; Specify number of month corresponding to "months" above
   month_numbers=['12','01','02']
   ;month_colors=['blue','purple','red']
   month_colors=['black','black','black']
                                ; Start plot
   psfile='/home/ss901165/idl/queensland/tropical_cyclones/qld_tropcyc_ibtracs_dec_feb_eot2.tracks_'+type+'_stddev.bw.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=2000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
          YSIZE=15000,SPACE3=500
   CS,SCALE=26,NCOLS=N_ELEMENTS(month_colors)
                                ; Draw map
   MAP,LATMIN=-30,LATMAX=0,LONMIN=110,LONMAX=220,/DRAW
   
   FOR j=0,nyears-1 DO BEGIN   
                                ; Specify season to plot (will be added to end of filename)
      year=STRTRIM(STRING(years(j)),1)+'-'+STRTRIM(STRING(years(j)+1),1)
      track_infile='/home/ss901165/datasets/IBTRACS/ff_trs.mslp.'+year                              
                                ; Open file for reading
      openr,lun,track_infile,/GET_LUN
      mystr=''
                                ; Read past header information
      FOR k=0,1 DO $
         readf,lun,mystr
      print,mystr
                                ; Read number of storms
      readf,lun,mystr
      temp=STRSPLIT(mystr,' ',/EXTRACT)
      n_storms=temp(1)
      print,temp
                                ; Read tropical-cyclone information
      start_month=intarr(n_storms)
      track_number=intarr(n_storms)
      n_points=intarr(n_storms)
      FOR k=0,n_storms-1 DO BEGIN
                                ; Get number of the track and the month of genesis
         print,'k=',k
         readf,lun,mystr
         print,mystr
         temp=STRSPLIT(mystr,' ',/EXTRACT)
         track_number(k)=temp(1)
         start_month(k)=STRMID(temp(3),4,2)
                                ; Get number of points along track
         readf,lun,mystr
         print,mystr
         temp=STRSPLIT(mystr,' ',/EXTRACT)
         n_points(k)=temp(1)
         track_longitude=fltarr(n_points(k))
         track_latitude=fltarr(n_points(k))   
                                ; For each point along track, read latitude and longitude
         FOR m=0,n_points(k)-1 DO BEGIN
            readf,lun,date,templon,templat
            track_longitude(m)=templon
            track_latitude(m)=templat
         ENDFOR
                                ; Plot tracks if during months of interest
         IF where(month_numbers eq start_month(k)) ne -1 THEN BEGIN
                                ; Decide which color to make the track (by month)
            FOR m=0,n_months-1 DO $
               IF start_month(k) eq month_numbers(m) THEN $
                  color=month_colors(m)
                                ; Plot the track if storm tracked within region of interest
            IF TOTAL(where(track_longitude lt 240 and track_longitude gt 100) gt 0) THEN BEGIN
               GPLOT,X=track_longitude,Y=track_latitude,COL=FSC_COLOR(color),THICK=90,STYLE=2
               GPLOT,X=track_longitude(0),Y=track_latitude(0),COL=FSC_COLOR(color),SYM=3,SIZE=70
                                ; Only plot the lysis point if the lysis point is within the mapped region
               IF track_latitude(n_points(k)-1) gt -30 and track_longitude(n_points(k)-1) lt 240 and track_longitude(n_points(k)-1) gt 110 THEN $
                  GPLOT,X=track_longitude(n_points(k)-1),Y=track_latitude(n_points(k)-1),COL=FSC_COLOR(color),SYM=4,SIZE=70
            ENDIF
         ENDIF
      ENDFOR
      ; Release the text file from IDL
      FREE_LUN,lun
   ENDFOR
   AXES,XSTEP=10,YSTEP=5
                                ; Plot title
   ;IF type eq 'plus' THEN $
   ;   GPLOT,X=160,Y=2,ALIGN=0.5,TEXT='Tropical cyclone tracks for seasons when DJF EOT 2 is >= one standard deviation (1979-2008)'
   ;IF type eq 'minus' THEN $
   ;   GPLOT,X=160,Y=2,ALIGN=0.5,TEXT='Tropical cyclone tracks for seasons when DJF EOT 2 is <= one standard deviation (1979-2008)'
                                ; Legends 
   ;GLEGEND,labels=REVERSE(months),COL=REVERSE(FSC_COLOR(month_colors)),LEGPOS=9
   GLEGEND,labels=['Lysis location','Genesis location'],SYM=[4,3],LEGPOS=9,LENGTH=0
   
   PSCLOSE


ENDFOR

END

