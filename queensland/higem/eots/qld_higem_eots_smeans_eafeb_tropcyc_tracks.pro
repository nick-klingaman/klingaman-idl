PRO qld_higem_eots_smeans_eafeb_tropcyc_tracks

eot_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.mar-may_smeans.h9-w8.eots.nc'
eot=4
n_years=149

eot_ts=REFORM(OPEN_AND_EXTRACT(eot_infile,'loading',$
                               offset=[0,eot-1],count=[n_years,1]))

higem_years=['i0i1','i1i2','i2i3','i3i4','i4i5','i5i6','i6i7','i7i8','i8i9','i9j0',$
             'j0j1','j1j2','j2j3','j3j4','j4j5','j5j6','j6j7','j7j8','j8j9','j9k0',$
             'k0k1','k1k2','k2k3','k3k4','k4k5','k5k6','k6k7','k7k8','k8k9','k9l0',$
             'l0l1','l1l2','l2l3','l3l4','l4l5','l5l6','l6l7','l7l8','l8l9','l9m0',$
             'm0m1','m1m2','m2m3','m3m4','m4m5','m5m6','m6m7','m7m8','m8m9','m9n0',$
             'n0n1','n1n2','n2n3','n3n4','n4n5','n5n6','n6n7','n7n8','n8n9','n9o0',$
             'o0o1','o1o2','o2o3','o3o4','o4o5','o5o6','o6o7','o7o8','o8o9','o9p0',$
             'p0p1','p1p2','p2p3','p3p4','p4p5','p5p6','p6p7','p7p8','p8p9','p9q0',$
             'q0q1','q1q2','q2q3','q3q4','q4q5','q5q6','q6q7','q7q8','q8q9','q9r0',$
             'r0r1','r1r2','r2r3','r3r4','r4r5','r5r6','r6r7','r7r8','r8r9','r9s0',$
             's0s1','s1s2','s2s3','s3s4','s4s5','s5s6','s6s7','s7s8','s8s9','s9t0',$
             't0t1','t1t2','t2t3','t3t4','t4t5','t5t6','t6t7','t7t8','t8t9','t9u0',$
             'u0u1','u1u2','u2u3','u3u4','u4u5','u5u6','u6u7','u7u8','u8u9','u9v0',$
             'v0v1','v1v2','v2v3','v3v4','v4v5','v5v6','v6v7','v7v8','v8v9','v9w0',$
             'w0w1','w1w2','w2w3','w3w4','w4w5','w5w6','w6w7','w7w8','w8w9','w9x0']

years_plus_stddev=where(eot_ts(0:n_years-1) ge MEAN(eot_ts)+STDDEV(eot_ts))-1
years_minus_stddev=where(eot_ts(0:n_years-1) le MEAN(eot_ts)-STDDEV(eot_ts))-1

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
   month_colors=['blue','purple','red']
                                ; Start plot
   psfile='/home/ss901165/idl/queensland/higem/eots/qld_higem_eots_smeans_eafeb_tropcyc_tracks.mar-may_eot'+STRTRIM(STRING(eot),1)+'.'+type+'_stddev.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=2000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
          YSIZE=15000,SPACE3=500
   CS,SCALE=26,NCOLS=N_ELEMENTS(month_colors)
                                ; Draw map
   MAP,LATMIN=-45,LATMAX=0,LONMIN=110,LONMAX=220,/DRAW
   
   FOR j=0,nyears-1 DO BEGIN
                                ; Specify season to plot (will be added to end of filename)
      year=higem_years(years(j))
      track_infile='/home/ss901165/higem_qccce/es_control_eafeb/tropical_cyclones/tr_trs.HiGEM_eadwh_'+year                              
                                ; Open file for reading
      openr,lun,track_infile,/GET_LUN
      mystr=''
                                ; Read past header information
      FOR k=0,2 DO $
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
      min_mslp=fltarr(n_storms)
      FOR k=0,n_storms-1 DO BEGIN
                                ; Get number of the track 
         print,'k=',k
         readf,lun,mystr
         print,mystr
         temp=STRSPLIT(mystr,' ',/EXTRACT)
         track_number(k)=temp(1)
                                ; Get number of points along track
         readf,lun,mystr
         print,mystr
         temp=STRSPLIT(mystr,' ',/EXTRACT)
         n_points(k)=temp(1)
         track_longitude=fltarr(n_points(k))
         track_latitude=fltarr(n_points(k))
         track_mslp=fltarr(n_points(k))
                                ; For each point along track, read latitude and longitude
         FOR m=0,n_points(k)-1 DO BEGIN
            readf,lun,mystr
            temp=STRSPLIT(mystr,' ',/EXTRACT)
            IF m eq 0 THEN BEGIN
               start_month(k)=(temp(0)/120)+10
               IF start_month(k) gt 12 THEN $
                  start_month(k)=start_month(k)-12
            ENDIF
            track_longitude(m)=temp(1)
            track_latitude(m)=temp(2)
            track_mslp(m)=temp(39)/100.
         ENDFOR
         min_mslp(k)=MIN(track_mslp)
                                ; Plot tracks if during months of interest
         IF where(month_numbers eq start_month(k)) ne -1 THEN BEGIN
                                ; Decide which color to make the track (by month)
            FOR m=0,n_months-1 DO $
               IF start_month(k) eq month_numbers(m) THEN $
                  color=month_colors(m)            
                                ; Plot the track if storm tracked within region of interest
            IF TOTAL(where(track_longitude lt 240 and track_longitude gt 100) gt 0) and min_mslp(k) lt 990 THEN BEGIN
               GPLOT,X=track_longitude,Y=track_latitude,COL=FSC_COLOR(color),THICK=50
                                ; Only plot the genesis point if the genesis point is within the mapped region
               IF track_latitude(0) gt -45 and track_longitude(0) lt 220 and track_longitude(0) gt 110 THEN $
                  GPLOT,X=track_longitude(0),Y=track_latitude(0),COL=FSC_COLOR(color),SYM=3,SIZE=50
                                ; Only plot the lysis point if the lysis point is within the mapped region
               IF track_latitude(n_points(k)-1) gt -45 and track_latitude(n_points(k)-1) lt 0 and $
                  track_longitude(n_points(k)-1) lt 220 and track_longitude(n_points(k)-1) gt 110 THEN $
                  GPLOT,X=track_longitude(n_points(k)-1),Y=track_latitude(n_points(k)-1),COL=FSC_COLOR(color),SYM=4,SIZE=50
            ENDIF
         ENDIF
      ENDFOR
      ; Release the text file from IDL
      FREE_LUN,lun
   ENDFOR
   AXES
                                ; Plot title
   IF type eq 'plus' THEN $
      GPLOT,X=160,Y=2,ALIGN=0.5,TEXT='Tropical cyclone tracks for seasons when MAM EOT '+STRTRIM(STRING(eot),1)+' is >= one standard deviation (h9-w8)'
   IF type eq 'minus' THEN $
      GPLOT,X=160,Y=2,ALIGN=0.5,TEXT='Tropical cyclone tracks for seasons when MAM EOT '+STRTRIM(STRING(eot),1)+' is <= one standard deviation (h9-w8)'
                                ; Legends 
   GLEGEND,labels=REVERSE(months),COL=REVERSE(FSC_COLOR(month_colors)),LEGPOS=9
   GLEGEND,labels=['Lysis location','Genesis location'],SYM=[4,3],LEGPOS=1
   
   PSCLOSE,/NOVIEW
ENDFOR

STOP
END

