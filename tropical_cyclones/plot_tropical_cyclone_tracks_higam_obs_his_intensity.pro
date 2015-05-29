PRO plot_tropical_cyclone_tracks_higam_obs_his_intensity

; Specify season to plot (will be added to end of filename)
;years=['1982-1983','1986-1987','1987-1988','1991-1992','1994-1995','1997-1998']
;years=['1983-1984','1984-1985','1988-1989','1998-1999','1999-2000','2000-2001']

;track_infile='/home/ss901165/datasets/IBTRACS/ff_trs.mslp.'+year
; Specify source of input data (higam or ibtracs)
;data_source='higam'

; Specify months included in season
months=['Oct','Nov','Dec','Jan','Feb','Mar','Apr','May']
n_months=N_ELEMENTS(months)
; Specify number of month corresponding to "months" above
month_numbers=['10','11','12','01','02','03','04','05']
month_colors=['purple','blue','cyan','green','orange','brown','red','pink']
; Specify minimum MSLP that tropical cyclone must attain to be plotted
min_mslp_threshold=1050

n_sets=3

; Start plot
psfile='/home/ss901165/idl/tropical_cyclones/higam_ibtracs_eraint_his_intensity.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=2000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
       SPACE3=500
CS,SCALE=26,NCOLS=N_ELEMENTS(month_colors)
xmax=1020
xmin=900
GSET,XMIN=xmin,XMAX=xmax,YMIN=0,YMAX=30,TITLE='Distribution of minimum MSLP for all storms'

labels=strarr(n_sets)
colors=strarr(n_sets)
FOR m=0,n_sets-1 DO BEGIN
   CASE m OF
      0 : BEGIN
         track_basename='/home/ss901165/higam_qccce/hpcx_amip2_xcquc/tropical_cyclones/ff_trs.vor_fullgrid_wind_mslp_mask.'
         labels(m)='HiGAM'
         colors(m)='blue'
         data_source='higam'
         years=STRTRIM(STRING(indgen(23)+1979),1)+'-'+STRTRIM(STRING(indgen(23)+1980),1)
      END
      1 : BEGIN
         track_basename='/home/ss901165/datasets/IBTRACS/ff_trs.mslp.'
         labels(m)='IBTrACS'
         colors(m)='red'
         data_source='ibtracs'
         years=STRTRIM(STRING(indgen(23)+1979),1)+'-'+STRTRIM(STRING(indgen(23)+1980),1)
      END
      2 : BEGIN
         track_basename='/home/ss901165/datasets/ERA-INTERIM/tropical_storms/tr_trs.ERAI_'
         labels(m)='ERA-Interim'
         colors(m)='orange'
         years=STRTRIM(STRING(indgen(23)+1979),1)+STRTRIM(STRING(indgen(23)+1980),1)+'_att20.SPAC'
         data_source='era'
      END
   ENDCASE
   n_years=N_ELEMENTS(years)
                                ; Loop to read in data from array of all years.
   total_storms=0
   FOR k=0,n_years-1 DO BEGIN
      track_infile=track_basename+years(k)
                                ; Open file for reading
      openr,lun,track_infile,/GET_LUN
      mystr=''
; Read past header information
      FOR i=0,1 DO $
         readf,lun,mystr
; Read number of storms
      readf,lun,mystr
      temp=STRSPLIT(mystr,' ',/EXTRACT)
      total_storms=temp(1)+total_storms
                                ; Release the text file from IDL
      FREE_LUN,lun
   ENDFOR
   start_month=intarr(total_storms)
   track_number=intarr(total_storms)
   n_points=intarr(total_storms)
   min_mslp=fltarr(total_storms)
   cumulative_storms=0
   
   FOR k=0,n_years-1 DO BEGIN
      track_infile=track_basename+years(k)
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
      FOR i=0,n_storms-1 DO BEGIN
                                ; Get number of the track and the month of genesis
         readf,lun,mystr
         temp=STRSPLIT(mystr,' ',/EXTRACT)
         
         track_number(cumulative_storms+i)=temp(1)
         ;start_month(cumulative_storms+i)=STRMID(temp(3),4,2)
         readf,lun,mystr
                                ; Get number of points along track
         temp=STRSPLIT(mystr,' ',/EXTRACT)
         n_points(cumulative_storms+i)=temp(1)
         track_longitude=fltarr(n_points(cumulative_storms+i))
         track_latitude=fltarr(n_points(cumulative_storms+i))
         track_mslp=fltarr(n_points(cumulative_storms+i)) 
         
                                ; For each point along track, read latitude and longitude
         FOR j=0,n_points(cumulative_storms+i)-1 DO BEGIN
            readf,lun,mystr
            temp=STRSPLIT(mystr,' ',/EXTRACT)
            track_longitude(j)=temp(1)
            track_latitude(j)=temp(2)
            IF data_source eq 'higam' THEN BEGIN
               temp=STRSPLIT(mystr,' & ',/EXTRACT)
               track_mslp(j)=temp(21)/100.
            ENDIF ELSE IF data_source eq 'ibtracs' THEN BEGIN
               track_mslp(j)=temp(5)
            ENDIF ELSE IF data_source eq 'era' THEN BEGIN
               temp=STRSPLIT(mystr,' & ',/EXTRACT)
               track_mslp(j)=temp(33)/100.
            ENDIF
         ENDFOR
         IF TOTAL(where(track_mslp gt 1E10)) ge 0 THEN $
            track_mslp[where(track_mslp gt 1E10)]=!Values.F_NaN
         min_mslp(cumulative_storms+i)=MIN(track_mslp,/NaN)
         IF min_mslp(cumulative_storms+i) eq 0 THEN BEGIN
            print,track_mslp
            min_mslp(cumulative_storms+i)=!Values.F_NaN
            print,min_mslp(cumulative_storms+i)
         ENDIF
         IF j eq 0 THEN BEGIN
            IF track_longitude(j) ge 200 or track_longitude(j) le 120 $
               or track_latitude(j) le -25 THEN $
                  min_mslp(cumulative_storms+i)=-9999
         ENDIF
      ENDFOR
      cumulative_storms=cumulative_storms+i
                                ; Release the text file from IDL
      FREE_LUN,lun
   ENDFOR
   
; Draw histogram
;xmin=880
;xmax=1010
;en_diff=[-0.1111,-0.1111,-0.3333,-0.5556,-1.1111,-0.1667,0.05556,0.6111,3,1.7778,5.6667,15.4444,26.3889,0.5]
;ln_diff=[0,0,0,-0.14286,-0.40476,-0.02381,2.047619,1.809524,1.33333,2.380952,3.47619,17.38095,34.52381,0.66667]  
   histogram_output=HISTOGRAM(min_mslp,binsize=10,min=xmin,max=xmax,locations=locations)
;xlabels=['950','960','970','980','990','1000','1010','1020','1030','1040','1050']
;HIST,X=indgen((xmax-xmin)/10+1)*10+xmin+7.2,Y=ln_diff,WIDTH=200,FILLCOL=2
;HIST,X=indgen((xmax-xmin)/10+1)*10+xmin+2.8,Y=en_diff,WIDTH=200,FILLCOL=8
   HIST,X=locations+3+m*2,Y=histogram_output/FLOAT(n_years),WIDTH=100,FILLCOL=FSC_COLOR(colors(m))
ENDFOR

   AXES,XVALS=locations,YSTEP=5,NDECS=1,XTITLE='Minimum mean sea level pressure (hPa)',YTITLE='Number of Tropical Cyclones/Year'
   GLEGEND,labels=REVERSE(labels),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=1
   
   PSCLOSE



STOP

END

