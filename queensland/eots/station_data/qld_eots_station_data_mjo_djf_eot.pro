PRO qld_eots_station_data_mjo_djf_eot

  ; Consider only stations north of this latitude
  min_station_lat=-17

  ; Timeseries of station rainfall data
  rainfall_infile='/home/ss901165/datasets/AUS_RAIN_GAUGE/data/QLD2008/qld_hq_rain_gauges.dec-feb_dmeans.1975-2006.nc'
  ; Timeseries of MJO indices
  mjo_infile='/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.dec-feb_dmeans.1975-2009.index_values.nc'
  ; Timeseries of EOT
  eot_infile='/home/ss901165/datasets_mango/SILO/one_quarter/SILO.dec-feb_smeans.1900-2007.eots.nc'
  eot_offset=75

  n_years=32
  n_days=90

  ; Read station location information
  station_latitude=OPEN_AND_EXTRACT(rainfall_infile,'station_lat')
  station_longitude=OPEN_AND_EXTRACT(rainfall_infile,'station_lon')
  good_stations=where(station_latitude ge min_station_lat)
  n_good_stations=N_ELEMENTS(good_stations)
  station_rainfall=fltarr(n_good_stations,n_days,n_years)
  ; Read station rainfall
  FOR i=0,n_good_stations-1 DO $
     station_rainfall(i,*,*)=REFORM(OPEN_AND_EXTRACT(rainfall_infile,'rainfall',$
                                                     offset=[0,good_stations(i),0],$
                                                     count=[n_days,1,n_years]))

  ; Read MJO phase and amplitude information
  mjo_phase=fltarr(n_days,n_years)
  mjo_amplitude=fltarr(n_days,n_years)
  mjo_phase(*,*)=OPEN_AND_EXTRACT(mjo_infile,'phase',$
                                  offset=[0,0],count=[n_days,n_years])
  mjo_amplitude(*,*)=OPEN_AND_EXTRACT(mjo_infile,'amplitude',$
                                      offset=[0,0],count=[n_days,n_years])

  ; Read first EOT of rainfall
  eot1_ts=REFORM(OPEN_AND_EXTRACT(eot_infile,'loading',offset=[eot_offset,0],count=[n_years,1]))  
  ; Read second EOT of rainfall
  eot2_ts=REFORM(OPEN_AND_EXTRACT(eot_infile,'loading',offset=[eot_offset,1],count=[n_years,1]))

  total_rainfall=fltarr(n_good_stations,n_years)
  freq_rainfall=fltarr(n_good_stations,n_years)
  intensity_rainfall=fltarr(n_good_stations,n_years)
  eot2_correlations=fltarr(n_good_stations)
  eot2_explained=fltarr(n_good_stations,n_years)
  FOR i=0,n_good_stations-1 DO BEGIN
     n_good_years=0
     FOR j=0,n_years-1 DO $
        IF N_ELEMENTS(where(station_rainfall(i,*,j) eq 2E20)) le 3 THEN $
           n_good_years=n_good_years+1
     IF n_good_years lt 29 THEN BEGIN
        station_rainfall(i,*,*)=!Values.F_NaN
        print,'Excluding station '+STRTRIM(STRING(i+1)),' because of missing data'
     ENDIF ELSE BEGIN
        FOR j=0,n_years-1 DO BEGIN
           IF N_ELEMENTS(where(station_rainfall(i,*,j) eq 2E20)) le 3 THEN BEGIN
              thisyear_thisstation_rainfall=REFORM(station_rainfall(i,*,j))
              total_rainfall(i,j)=TOTAL(thisyear_thisstation_rainfall[where(thisyear_thisstation_rainfall ne 2E20)])
              freq_rainfall(i,j)=N_ELEMENTS(where(thisyear_thisstation_rainfall gt 5 and $
                                                  thisyear_thisstation_rainfall ne 2E20))
              intensity_rainfall(i,j)=MEAN(where(thisyear_thisstation_rainfall gt 5 and $
                                                 thisyear_thisstation_rainfall ne 2E20))
           ENDIF ELSE BEGIN
              total_rainfall(i,j)=!Values.F_NaN
           ENDELSE
        ENDFOR
        thisstation_total_rainfall=REFORM(total_rainfall(i,*))-MEAN(total_rainfall(i,*),/NaN)
        good_years=where(FINITE(thisstation_total_rainfall) eq 1)
        thispt_eot1_corr=(1./FLOAT(n_good_years)*TOTAL(eot1_ts[good_years]*thisstation_total_rainfall[good_years]))/$
                         (STDDEV(eot1_ts[good_years])*STDDEV(thisstation_total_rainfall[good_years]))
        thispt_eot1_regress=thispt_eot1_corr*STDDEV(thisstation_total_rainfall[good_years])/STDDEV(eot1_ts[good_years])
        eot1_explained=fltarr(n_good_years)
        eot1_residual=fltarr(n_good_years)
        FOR j=0,n_good_years-1 DO BEGIN
           eot1_explained(j)=eot1_ts[good_years(j)]*thispt_eot1_regress
           eot1_residual(j)=thisstation_total_rainfall[good_years(j)]-eot1_explained(j)
        ENDFOR
;        print,eot1_residual
;        eot2_correlations(i)=(1./FLOAT(n_good_years))*TOTAL(eot2_ts[good_years]*thisstation_total_rainfall[good_years])/$
;                             (STDDEV(eot2_ts[good_years])*STDDEV(thisstation_total_rainfall[good_years]))
;        thispt_eot2_regress=eot2_correlations(i)*STDDEV(eot2_ts[good_years])/STDDEV(thisstation_total_rainfall[good_years])
;        print,thispt_eot2_regress
        eot2_correlations(i)=(1./FLOAT(n_good_years))*TOTAL(eot2_ts[good_years]*eot1_residual)/$
                             (STDDEV(eot2_ts[good_years])*STDDEV(eot1_residual))
        thispt_eot2_regress=eot2_correlations(i)*STDDEV(eot2_ts[good_years])/STDDEV(eot1_residual)
        
        eot2_explained(i,*)=!Values.F_NaN
        FOR j=0,n_good_years-1 DO $
;           eot2_explained(i,good_years(j))=thisstation_total_rainfall[good_years(j)]*thispt_eot2_regress
;           eot2_explained(i,good_years(j))=thisstation_total_rainfall[good_years(j)]*eot2_ts[good_years(j)]
           eot2_explained(i,good_years(j))=eot1_residual(j)*thispt_eot2_regress
;           eot2_explained(i,good_years(j))=(eot2_ts[good_years(j)]*thisstation_total_rainfall[good_years(j)])*(1./FLOAT(n_good_years))
;           eot2_explained(i,good_years(j))=(eot2_ts[good_years(j)]*eot1_residual(j))*(1./FLOAT(n_good_years))0
     ENDELSE
  ENDFOR

  mjo_frequency_phase=fltarr(8,n_years)
  mjo_amplitude_phase=fltarr(8,n_years)
  corr_totalrain_mjofreq=fltarr(n_good_stations,8)
  corr_totalrain_mjoamp=fltarr(n_good_stations,8)
  corr_eot2_mjofreq=fltarr(n_good_stations,8)
  corr_eot2_mjoamp=fltarr(n_good_stations,8)
  corr_freqrain_mjoamp=fltarr(n_good_stations,8)
  corr_freqrain_mjofreq=fltarr(n_good_stations,8)
  corr_intsrain_mjoamp=fltarr(n_good_stations,8)
  corr_intsrain_mjofreq=fltarr(n_good_stations,8)
  ;corr_totalrain_mjofreq45=fltarr(n_good_stations)
  ;corr_totalrain_mjoamp45=fltarr(n_good_stations)
  FOR i=0,n_years-1 DO BEGIN
     thisyear_mjo_amplitude=REFORM(mjo_amplitude(*,i))
     thisyear_mjo_phase=REFORM(mjo_phase(*,i))     
     FOR j=0,7 DO BEGIN
        mjo_frequency_phase(j,i)=N_ELEMENTS(where(thisyear_mjo_amplitude ge 1 and thisyear_mjo_phase eq j+1))
        IF TOTAL(where(thisyear_mjo_phase eq j+1)) gt 0 THEN BEGIN
           mjo_amplitude_phase(j,i)=MEAN(thisyear_mjo_amplitude[where(thisyear_mjo_phase eq j+1)])
        ENDIF ELSE $
           mjo_amplitude_phase(j,i)=!Values.F_NaN
     ENDFOR
  ENDFOR
  FOR i=0,n_good_stations-1 DO BEGIN
     thisstation_total_rainfall=REFORM(total_rainfall(i,*))
     thisstation_freq_rainfall=REFORM(freq_rainfall(i,*))
     thisstation_intensity_rainfall=REFORM(intensity_rainfall(i,*))
     thisstation_eot2_explained=REFORM(eot2_explained(i,*))
     thissstation_good_years=where(FINITE(thisstation_total_rainfall) eq 1)     
     FOR j=0,7 DO BEGIN
        thisphase_frequency=REFORM(mjo_frequency_phase(j,*))
        thisphase_amplitude=REFORM(mjo_amplitude_phase(j,*))
        good_years=where(FINITE(thisstation_total_rainfall) eq 1 and FINITE(thisphase_amplitude) eq 1)
        corr_totalrain_mjofreq(i,j)=CORRELATE(thisstation_total_rainfall[good_years],thisphase_frequency[good_years])
        corr_totalrain_mjoamp(i,j)=CORRELATE(thisstation_total_rainfall[good_years],thisphase_amplitude[good_years])
        corr_freqrain_mjoamp(i,j)=CORRELATE(thisstation_freq_rainfall[good_years],thisphase_amplitude[good_years])
        corr_freqrain_mjofreq(i,j)=CORRELATE(thisstation_freq_rainfall[good_years],thisphase_frequency[good_years])
        corr_intsrain_mjoamp(i,j)=CORRELATE(thisstation_intensity_rainfall[good_years],thisphase_amplitude[good_years])
        corr_intsrain_mjofreq(i,j)=CORRELATE(thisstation_intensity_rainfall[good_years],thisphase_frequency[good_years])
        corr_eot2_mjofreq(i,j)=CORRELATE(thisstation_eot2_explained[good_years],thisphase_frequency[good_years])
        corr_eot2_mjoamp(i,j)=CORRELATE(thisstation_eot2_explained[good_years],thisphase_amplitude[good_years])
     ENDFOR
     ;corr_totalrain_mjofreq45(i)=CORRELATE(thisstation_total_rainfall[good_years],mjo_frequency_phase(3,*)+mjo_frequency_phase(4,*))
  ENDFOR

  sig_level=0.30
  psfile='/home/ss901165/idl/queensland/eots/station_data/qld_eots_station_data_mjo_djf_eot.correlations_total_freq_ints.ps'
  PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1800,SPACE2=300,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=112
  GSET,XMIN=0,XMAX=18,YMAX=0.55,YMIN=-0.55
  colors=['blue','red','purple']
  count=1
  FOR i=0,n_good_stations-1 DO BEGIN     
     IF total_rainfall(i,0) ne 0 THEN BEGIN
        FOR j=3,5 DO $ ; phases 4-6
           GPLOT,X=j-2.5,Y=corr_totalrain_mjofreq(i,j),SYM=count,COL=FSC_COLOR(colors(j-3))
        FOR j=3,5 DO $
           GPLOT,X=j+0.5,Y=corr_totalrain_mjoamp(i,j),SYM=count,COL=FSC_COLOR(colors(j-3))
        FOR j=3,5 DO $
           GPLOT,X=j+3.5,Y=corr_freqrain_mjofreq(i,j),SYM=count,COL=FSC_COLOR(colors(j-3))
        FOR j=3,5 DO $
           GPLOT,X=j+6.5,Y=corr_freqrain_mjoamp(i,j),SYM=count,COL=FSC_COLOR(colors(j-3))
        FOR j=3,5 DO $
           GPLOT,X=j+9.5,Y=corr_intsrain_mjofreq(i,j),SYM=count,COL=FSC_COLOR(colors(j-3))
        FOR j=3,5 DO $
           GPLOT,X=j+12.5,Y=corr_intsrain_mjoamp(i,j),SYM=count,COL=FSC_COLOR(colors(j-3))
        count=count+1
     ENDIF
  ENDFOR
  GPLOT,X=[0,18],Y=[sig_level,sig_level],STYLE=1
  GPLOT,X=[0,18],Y=[-sig_level,-sig_level],STYLE=1
  GPLOT,X=[0,18],Y=[0,0],STYLE=1
  AXES,YSTEP=0.1,XVALS=indgen(6)*3+1.5,XLABELS=['Corr(Total_rain,MJO_freq)','Corr(Total_rain,MJO_amp)','Corr(Freq_rain,MJO_freq)',$
                                                'Corr(Freq_rain,MJO_amp)','Corr(Ints_rain,MJO_freq)','Corr(Ints_rain,MJO_amp)'],$
       NDECS=2,ORIENTATION=10,YTITLE='Correlation coefficient'
  LEGEND,labels=REVERSE(['Phase 4','Phase 5','Phase 6']),SYM=[1,1,1],COL=REVERSE(FSC_COLOR(colors)),LEGPOS=1,LENGTH=0  
  PSCLOSE,/NOVIEW

  psfile='/home/ss901165/idl/queensland/eots/station_data/qld_eots_station_data_mjo_djf_eot.station_map.ps'
  PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1800,SPACE2=300,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,/PORTRAIT
  MAP,LONMIN=140,LONMAX=147,LATMIN=min_station_lat-1,LATMAX=-8,/SET
  count=1
  FOR i=0,n_good_stations-1 DO BEGIN
     IF total_rainfall(i,0) ne 0 THEN BEGIN
        GPLOT,X=station_longitude(good_stations(i)),Y=station_latitude(good_stations(i)),SYM=count,COL=FSC_COLOR("red")
        count=count+1
     ENDIF
  ENDFOR
  AXES,XSTEP=3,YSTEP=1
  DRAWSTATES,'/home/ss901165/idl/queensland/shapefiles/aust_cd66eaststates.shp',ATTRIBUTE_NAME='STE',COLOR="black",statenames='3',THICK=3
  PSCLOSE,/NOVIEW

  FOR i=0,n_good_stations-1 DO BEGIN
     IF ABS(corr_freqrain_mjoamp(i,3)) ge sig_level THEN BEGIN
        phase4_days=[0]
        phase5_days=[0]
        phase6_days=[0]
        FOR j=0,n_years-1 DO BEGIN
           IF N_ELEMENTS(where(station_rainfall(i,*,j) eq 2E20)) le 3 THEN BEGIN
              print,j
              thisyear_thisstation_rainfall=REFORM(station_rainfall(i,*,j))
              thisyear_mjo_amplitude=REFORM(mjo_amplitude(*,j))
              thisyear_mjo_phase=REFORM(mjo_phase(*,j))
              IF TOTAL(where(thisyear_mjo_phase eq 4 and thisyear_mjo_amplitude ge 1)) gt 0 THEN $
                 phase4_days=[phase4_days,thisyear_thisstation_rainfall[where(thisyear_mjo_phase eq 4 and thisyear_mjo_amplitude ge 1)]]
              IF TOTAL(where(thisyear_mjo_phase eq 5 and thisyear_mjo_amplitude ge 1)) gt 0 THEN $
                 phase5_days=[phase5_days,thisyear_thisstation_rainfall[where(thisyear_mjo_phase eq 5 and thisyear_mjo_amplitude ge 1)]]
              IF TOTAL(where(thisyear_mjo_phase eq 6 and thisyear_mjo_amplitude ge 1)) gt 0 THEN $
                 phase6_days=[phase6_days,thisyear_thisstation_rainfall[where(thisyear_mjo_phase eq 6 and thisyear_mjo_amplitude ge 1)]]
           ENDIF
        ENDFOR
        STOP
     ENDIF
  ENDFOR
  STOP

END

