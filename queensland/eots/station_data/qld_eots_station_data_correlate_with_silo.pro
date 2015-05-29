PRO qld_eots_station_data_correlate_with_silo

; Correlate station observations of rainfall with the timeseries
; of EOTs determined from the SILO 0.25 degree data

; Input directory for SILO EOTs
eot_indir='/home/ss901165/datasets_mango/SILO/one_quarter'

; Input directory for station rainfall observations
obs_indir='/home/ss901165/datasets/AUS_RAIN_GAUGE/data/QLD2008'

; Number of seasons and EOTs to consider
n_seasons=4
n_eots=3

mylevs=[-0.30,-0.18,-0.06,0.06,0.18,0.30,0.42,0.54,0.66,0.78,0.90]
nlevs=N_ELEMENTS(mylevs)
legend_labels=strarr(nlevs+1)
FOR i=1,nlevs-1 DO $
   legend_labels(i)=STRMID(STRTRIM(STRING(mylevs(i-1)),1),0,5)+' <= Correlation < '+STRMID(STRTRIM(STRING(mylevs(i)),1),0,5)
legend_labels(0)='Correlation <= '+STRMID(STRTRIM(STRING(mylevs(0)),1),0,5)
legend_labels(nlevs)=STRMID(STRTRIM(STRING(mylevs(nlevs-1)),1),0,5)+' <= Correlation'

FOR i=0,n_seasons-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         season='mar-may'
         eot_offset=20 ; Offset of station rainfall with respect to EOT timeseries
         n_years=88
         obs_end_year=2007
      END
      1 : BEGIN
         season='jun-aug'
         eot_offset=20
         n_years=88
         obs_end_year=2007
      END
      2 : BEGIN
         season='sep-nov'
         eot_offset=20
         n_years=88
         obs_end_year=2007
      END
      3 : BEGIN
         season='dec-feb'
         eot_offset=20
         n_years=87
         obs_end_year=2006
      END
   ENDCASE
   eot_infile=eot_indir+'/SILO.'+season+'_smeans.1900-2007.eots.nc'
   obs_infile=obs_indir+'/qld_hq_rain_gauges.'+season+'_dmeans.1920-'+$
              STRTRIM(STRING(obs_end_year),1)+'.nc'

   ; Get number of stations and number of days
   n_stations=N_ELEMENTS(OPEN_AND_EXTRACT(obs_infile,'station'))
   n_days=N_ELEMENTS(OPEN_AND_EXTRACT(obs_infile,'time'))

   ; Read EOTs for this season
   eots_ts=REFORM(OPEN_AND_EXTRACT(eot_infile,'loading',offset=[eot_offset,0],$
                                   count=[n_years,n_eots]))
   ; Read station rainfall for this season
   all_stations_rainfall=REFORM(OPEN_AND_EXTRACT(obs_infile,'rainfall',offset=[0,0,0],$
                                                 count=[n_days,n_stations,n_years]))
   all_stations_rainfall[where(all_stations_rainfall eq 2E20)]=!Values.F_NaN

   ; Read station longitude and latitude
   station_lon=REFORM(OPEN_AND_EXTRACT(obs_infile,'station_lon'))
   station_lat=REFORM(OPEN_AND_EXTRACT(obs_infile,'station_lat'))

   ; Correlate station rainfall with EOTs from gridded product
   n_stations_analyzed=0
   station_eot_correlations=fltarr(n_eots,n_stations)
   FOR j=0,n_stations-1 DO BEGIN
      this_station_rainfall=REFORM(all_stations_rainfall(*,j,*))
      n_missing_days=intarr(n_years)
      n_missing_days(*)=0
      this_station_rainfall_totals=fltarr(n_years)
      this_station_eots=eots_ts
      FOR k=0,n_years-1 DO BEGIN
         IF TOTAL(where(FINITE(this_station_rainfall(*,k)) eq 0)) gt 0 THEN BEGIN
            n_missing_days(k)=N_ELEMENTS(where(FINITE(this_station_rainfall(*,k)) eq 0))
         ENDIF ELSE $
            n_missing_days(k)=0
      ENDFOR
      ;print,N_ELEMENTS(where(n_missing_days ge 3))
      IF N_ELEMENTS(where(n_missing_days ge 3)) le 8 THEN BEGIN
         FOR k=0,n_years-1 DO $
            this_station_rainfall_totals(k)=TOTAL(this_station_rainfall(*,k),/NaN)
         IF TOTAL(where(n_missing_days ge 3)) ge 0 THEN BEGIN
            this_station_rainfall_totals[where(n_missing_days ge 3)]=!Values.F_NaN
            good_rainfall_totals=this_station_rainfall_totals[where(FINITE(this_station_rainfall_totals) eq 1)]
            FOR k=0,n_eots-1 DO BEGIN
               temp=REFORM(this_station_eots(*,k))
               temp[where(n_missing_days ge 3)]=!Values.F_NaN
               good_eot_ts=temp[where(FINITE(temp) eq 1)]
               station_eot_correlations(k,j)=CORRELATE(good_eot_ts,good_rainfall_totals)
            ENDFOR            
            n_stations_analyzed=n_stations_analyzed+1
         ENDIF ELSE BEGIN
            FOR k=0,n_eots-1 DO $
               station_eot_correlations(k,j)=CORRELATE(this_station_rainfall_totals,REFORM(this_station_eots(*,k)))
         ENDELSE
      ENDIF ELSE BEGIN
         ;print,'Station ',STRTRIM(STRING(j+1),1)+' not analyzed because of missing data'      
         station_eot_correlations(*,j)=!Values.F_NaN
      ENDELSE      
   ENDFOR
   print,'Number of stations analyzed: '+STRTRIM(STRING(n_stations_analyzed),1)
   
   FOR k=0,n_eots-1 DO BEGIN
      psfile='/home/ss901165/idl/queensland/eots/station_data/qld_eots_station_data_correlate_with_silo.'+season+'_eot'+STRTRIM(STRING(k+1),1)+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=112,/PORTRAIT
      CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs)+3,MIN=4,white=[5]
      MAP,LONMIN=137,LONMAX=154,LATMIN=-30,LATMAX=-8,/SET
      LEVS,MANUAL=mylevs
      this_eot_correlation=REFORM(station_eot_correlations(k,*))
      indices=VALUE_LOCATE(mylevs,this_eot_correlation)
      FOR j=0,n_stations-1 DO BEGIN
         IF FINITE(this_eot_correlation(j)) eq 1 THEN $
            GPLOT,X=station_lon(j),Y=station_lat(j),COL=indices(j)+3,SYM=4,SIZE=80
      ENDFOR
      AXES,XSTEP=3,XMINOR=1,YSTEP=2,YMINOR=1
      DRAWSTATES,'/home/ss901165/idl/queensland/shapefiles/aust_cd66eaststates.shp',ATTRIBUTE_NAME='STE',COLOR="black",statenames='3',THICK=3
      GPLOT,X=138,Y=-7.5,TEXT='Correlation of station rainfall with EOT'+STRTRIM(STRING(k+1),1)+' for '+season+' from SILO 0.25 (1920-2007)',ALIGN=0.0
      GPLOT,X=138,Y=-29.5,TEXT='Number of stations analyzed: '+STRTRIM(STRING(n_stations_analyzed),1),ALIGN=0.0
      LEGEND,labels=REVERSE(legend_labels),COL=REVERSE(indgen(nlevs+1)+2),LEGPOS=9,SYM=REPLICATE(4,nlevs+1),LENGTH=0
      PSCLOSE,/NOVIEW
   ENDFOR
ENDFOR

STOP
END
