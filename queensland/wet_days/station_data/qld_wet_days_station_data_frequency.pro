PRO qld_wet_days_station_data_frequency

; Make a map of selected rain gauge stations in Queensland
input_file='/home/ss901165/datasets/AUS_RAIN_GAUGE/data/QLD2008/qld_hq_rain_gauges.jan-dec_dmeans.1920-2007.nc'

; Read timeseries of rainfall for each station
rainfall_all_stations=OPEN_AND_EXTRACT(input_file,'rainfall')
station_longitude=OPEN_AND_EXTRACT(input_file,'station_lon')
station_latitude=OPEN_AND_EXTRACT(input_file,'station_lat')
station_number=OPEN_AND_EXTRACT(input_file,'station_number')
n_stations=N_ELEMENTS(station_longitude)
years=OPEN_AND_EXTRACT(input_file,'year')
n_years=N_ELEMENTS(years)
n_missing_days=OPEN_AND_EXTRACT(input_file,'number_missing_days')

; Give an example SILO file for latitude and longitude grid
;silo_dir='/home/ss901165/datasets_mango/SILO/native_resolution'
;silo_example_file=silo_dir+'/SILO_daily_precip_1920.0.25x0.25.nc'
silo_input_file='/home/ss901165/datasets_mango/SILO/one_quarter/SILO_precip.jan-dec_dmeans.1900-2008.0.25x0.25.rearrange.nc'
silo_offset=20
silo_longitude=OPEN_AND_EXTRACT(silo_input_file,'longitude')
silo_latitude=OPEN_AND_EXTRACT(silo_input_file,'latitude')

rainfall_thresholds=[5,10,25]
n_thresholds=N_ELEMENTS(rainfall_thresholds)
station_rainfall_frequency=fltarr(n_stations,n_thresholds)
silo_rainfall_frequency=fltarr(n_stations,n_thresholds)
station_rainfall_frequency(*,*)=0.
silo_rainfall_frequency(*,*)=0.

rainfall_all_stations[where(rainfall_all_stations ge 2E20)]=!Values.F_NaN

n_stations_analyzed=0
FOR i=0,n_stations-1 DO BEGIN
   print,'-> Analyzing station '+STRTRIM(STRING(i+1),1)
   thisstation_missing_days=REFORM(n_missing_days(*,i))
   IF N_ELEMENTS(where(thisstation_missing_days ge 10)) le 8 THEN BEGIN
      thisstation_rainfall=REFORM(rainfall_all_stations(*,i))
      ts_start=0
      ts_stop=DAYS_IN_YEAR(years(0))-1
      silo_pt_lon=NEAREST(silo_longitude,station_longitude(i))
      print,'Matched longitude: station = ',station_longitude(i),' SILO = ',silo_longitude[silo_pt_lon]
      silo_pt_lat=NEAREST(silo_latitude,station_latitude(i))
      print,'Matched latitude: station = ',station_latitude(i),' SILO = ',silo_latitude[silo_pt_lat] 
      n_years_analyzed=0
      FOR j=0,n_years-1 DO BEGIN
         IF thisstation_missing_days(j) lt 10 THEN BEGIN            
            thisyear_thisstation_rainfall=thisstation_rainfall(ts_start:ts_stop)
            thisyear_silo_rainfall_grid=REFORM(OPEN_AND_EXTRACT(silo_input_file,'rain',$
                                                                offset=[j+silo_offset,0,silo_pt_lat-1,silo_pt_lon-1],count=[1,365,3,3]))
            FOR k=0,n_thresholds-1 DO $
               station_rainfall_frequency(i,k)=station_rainfall_frequency(i,k)+N_ELEMENTS(where(thisyear_thisstation_rainfall ge rainfall_thresholds(k)))
            silo_rainfall_frequency_minerror=fltarr(n_thresholds)
            silo_rainfall_frequency_save=fltarr(n_thresholds)
            silo_rainfall_frequency_minerror(*)=9999
            FOR k=0,2 DO BEGIN
               FOR m=0,2 DO BEGIN
                  thisyear_silo_rainfall=REFORM(thisyear_silo_rainfall_grid(*,k,m))
                  IF TOTAL(where(FINITE(thisyear_thisstation_rainfall) eq 0)) gt 0 THEN $
                     thisyear_silo_rainfall[where(FINITE(thisyear_thisstation_rainfall) eq 0)]=!Values.F_NaN
                  FOR n=0,n_thresholds-1 DO BEGIN               
                     temp_silo_frequency=N_ELEMENTS(where(thisyear_silo_rainfall ge rainfall_thresholds(n)))+silo_rainfall_frequency(i,n)
                     ;print,temp_silo_frequency
                     IF ABS(temp_silo_frequency-station_rainfall_frequency(i,n)) lt silo_rainfall_frequency_minerror(n) THEN BEGIN
                        silo_rainfall_frequency_minerror(n)=ABS(temp_silo_frequency-station_rainfall_frequency(i,n))
                        silo_rainfall_frequency_save(n)=temp_silo_frequency
                     ENDIF
                  ENDFOR
               ENDFOR
            ENDFOR
            silo_rainfall_frequency(i,*)=silo_rainfall_frequency_save(*)
            n_years_analyzed=n_years_analyzed+1
         ENDIF
         ts_start=ts_stop+1
         ts_stop=ts_start+DAYS_IN_YEAR(years(j))-1
      ENDFOR
      station_rainfall_frequency(i,*)=station_rainfall_frequency(i,*)/FLOAT(n_years_analyzed)
      silo_rainfall_frequency(i,*)=silo_rainfall_frequency(i,*)/FLOAT(n_years_analyzed)
      n_stations_analyzed=n_stations_analyzed+1
   ENDIF ELSE BEGIN
      print,'Station '+STRTRIM(STRING(station_number(i)),1)+' not analyzed because of missing data'
      station_rainfall_frequency(i,*)=0
   ENDELSE
ENDFOR
                                ; Make map
FOR j=0,n_thresholds-1 DO BEGIN
   CASE j OF
      0 : BEGIN
         mylevs=[0,8,16,24,32,40,48,56,64,72,80]         
      END
      1 : BEGIN
         mylevs=[0,4,8,12,16,20,24,28,32,40]
      END
      2 : BEGIN
         mylevs=[0,3,6,9,12,15,18,21,24,27,30]
      END
   ENDCASE
   n_levels=N_ELEMENTS(mylevs)
   legend_labels=strarr(n_levels)
   FOR i=0,n_levels-2 DO $
      legend_labels(i)=STRTRIM(STRING(mylevs(i)),1)+' <= frequency <= '+STRTRIM(STRING(mylevs(i+1)),1)
   legend_labels(n_levels-1)=STRTRIM(STRING(mylevs(n_levels-1)),1)+' <= frequency '

   mylevs_diff=[-100,-0.9,-0.6,-0.3,-0.1,-0.05,-0.01,0.01,0.05,0.1,0.3,0.6,0.9]
   n_levels_diff=N_ELEMENTS(mylevs_diff)
   diff_legend_labels=strarr(n_levels_diff)
   FOR i=1,n_levels_diff-2 DO $
      diff_legend_labels(i)=STRMID(STRTRIM(STRING(mylevs_diff(i)),1),0,5)+' <= difference <= '+STRMID(STRTRIM(STRING(mylevs_diff(i+1)),1),0,5)
   diff_legend_labels(0)='difference <= '+STRMID(STRTRIM(STRING(mylevs_diff(1)),1),0,5)
   diff_legend_labels(n_levels_diff-1)=STRMID(STRTRIM(STRING(mylevs_diff(n_levels_diff-1)),1),0,5)+' <= difference '
   
   psfile='/home/ss901165/idl/queensland/wet_days/station_data/qld_wet_days_station_data_frequency.annual_'+STRTRIM(STRING(rainfall_thresholds(j)),1)+'mm_station.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   white=FSC_COLOR("white",2)
   this_station_rainfall_frequency=REFORM(station_rainfall_frequency(*,j))
   indices=VALUE_LOCATE(mylevs,this_station_rainfall_frequency)
   MAP,LONMIN=137,LONMAX=154,LATMIN=-30,LATMAX=-9,/HIRES,/SET
   FOR i=0,n_stations-1 DO BEGIN
      IF station_rainfall_frequency(i,j) ne 0 THEN $
         GPLOT,X=station_longitude(i),Y=station_latitude(i),SYM=4,SIZE=100,/NOLINES,COL=indices(i)+2
   ENDFOR
   AXES,XSTEP=3,YSTEP=2,XMINOR=1,YMINOR=1
   DRAWSTATES,'/home/ss901165/idl/queensland/shapefiles/aust_cd66eaststates.shp',ATTRIBUTE_NAME='STE',COLOR="black",statenames='3',THICK=3
   GPLOT,X=137,Y=-8.5,TEXT='Days year!U-1!N with rainfall above '+STRTRIM(STRING(rainfall_thresholds(j)),1)+' mm day!U-1!N for '+STRTRIM(STRING(years(0)),1)+' - '+STRTRIM(STRING(years(n_years-1)),1)+' from station data',ALIGN=0.0,CHARSIZE=100
   GPLOT,X=149,Y=-16,TEXT='Number of stations analyzed = '+STRTRIM(STRING(n_stations_analyzed),1)
   LEGEND,labels=REVERSE(legend_labels),COL=REVERSE(indgen(n_levels)+2),LEGPOS=9,SYM=REPLICATE(4,n_levels),LENGTH=0
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/wet_days/station_data/qld_wet_days_station_data_frequency.annual_'+STRTRIM(STRING(rainfall_thresholds(j)),1)+'mm_silo25km.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   white=FSC_COLOR("white",2)
   this_silo_rainfall_frequency=REFORM(silo_rainfall_frequency(*,j))
   indices=VALUE_LOCATE(mylevs,this_silo_rainfall_frequency)
   MAP,LONMIN=137,LONMAX=154,LATMIN=-30,LATMAX=-9,/HIRES,/SET
   FOR i=0,n_stations-1 DO BEGIN
      IF silo_rainfall_frequency(i,j) ne 0 THEN $
         GPLOT,X=station_longitude(i),Y=station_latitude(i),SYM=4,SIZE=100,/NOLINES,COL=indices(i)+2
   ENDFOR
   AXES,XSTEP=3,YSTEP=2,XMINOR=1,YMINOR=1
   DRAWSTATES,'/home/ss901165/idl/queensland/shapefiles/aust_cd66eaststates.shp',ATTRIBUTE_NAME='STE',COLOR="black",statenames='3',THICK=3
   GPLOT,X=138,Y=-8.5,TEXT='Days year!U-1!N with rainfall above '+STRTRIM(STRING(rainfall_thresholds(j)),1)+' mm day!U-1!N for '+STRTRIM(STRING(years(0)),1)+' - '+STRTRIM(STRING(years(n_years-1)),1)+' from SILO 5 km data',ALIGN=0.0,CHARSIZE=100
   LEGEND,labels=REVERSE(legend_labels),COL=REVERSE(indgen(n_levels)+2),LEGPOS=9,SYM=REPLICATE(4,n_levels),LENGTH=0
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/wet_days/station_data/qld_wet_days_station_data_frequency.annual_'+STRTRIM(STRING(rainfall_thresholds(j)),1)+'mm_station-minus-silo25km.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV
   black=FSC_COLOR("black",8)
   indices=VALUE_LOCATE(mylevs_diff,this_station_rainfall_frequency-this_silo_rainfall_frequency)
   MAP,LONMIN=137,LONMAX=154,LATMIN=-30,LATMAX=-9,/HIRES,/SET
   FOR i=0,n_stations-1 DO BEGIN
      GPLOT,X=station_longitude(i),Y=station_latitude(i),SYM=4,SIZE=100,/NOLINES,COL=indices(i)+2
   ENDFOR
   AXES,XSTEP=3,YSTEP=2,XMINOR=1,YMINOR=1
   DRAWSTATES,'/home/ss901165/idl/queensland/shapefiles/aust_cd66eaststates.shp',ATTRIBUTE_NAME='STE',COLOR="black",statenames='3',THICK=3
   GPLOT,X=136,Y=-8.5,TEXT='Diff in days year!U-1!N with rain above '+STRTRIM(STRING(rainfall_thresholds(j)),1)+' mm day!U-1!N for '+STRTRIM(STRING(years(0)),1)+' - '+STRTRIM(STRING(years(n_years-1)),1)+': station data minus SILO 25km',ALIGN=0.0,CHARSIZE=100
   LEGEND,labels=REVERSE(diff_legend_labels),COL=REVERSE(indgen(n_levels_diff)+2),LEGPOS=9,SYM=REPLICATE(4,n_levels_diff),LENGTH=0
   PSCLOSE,/NOVIEW


ENDFOR

STOP

END

