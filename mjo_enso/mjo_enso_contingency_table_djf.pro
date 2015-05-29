PRO mjo_enso_contingency_table

mjo_input_file='/home/ss901165/datasets/MJO_INDICES/MJO_ihr1_ihr2.jan-dec_mmeans_ts.1905-2008.index_values.nc'
enso_input_file='/home/ss901165/datasets/NINO/allnino_hadisst.jan-dec_mmeans.1905-2008.nc'
start_year=1905

; Open MJO input file and read amplitude values
mjo_amplitude=OPEN_AND_EXTRACT(mjo_input_file,'IHR2')

; Open ENSO input file and read Nino values
enso_amplitude=OPEN_AND_EXTRACT(enso_input_file,'nino34')

; Determine number of months in MJO and Nino timeseries
mjo_nmonths=N_ELEMENTS(mjo_amplitude)
enso_nmonths=N_ELEMENTS(enso_amplitude)

; Remove monthly mean climatology from ENSO to compute anomalies
FOR i=0,11 DO $
   enso_amplitude(i:enso_nmonths-1:12)=enso_amplitude(i:enso_nmonths-1:12)-MEAN(enso_amplitude(i:enso_nmonths-1:12))

; Take three-month running means of ENSO anomalies to conform to NOAA definition of El Nino and La Nina
enso_amplitude=SMOOTH(enso_amplitude,3)

; Set threshold values for El Nino and La Nina (in terms of SST anomaly)
elnino_threshold=0.5
lanina_threshold=-0.5
; Number of consecutive three-month periods required for determining El Nino or La Nina
enso_nperiods=5

; Set threshold values for MJO low/normal/high activity (low: 0 < amp < threshold ; normal: low < amp < high ; high: amp > high)
mjo_low_threshold = -0.4
mjo_high_threshold = 0.4
;mjo_low_threshold=1.1
;mjo_high_threshold=1.6

; Select month of MJO activity (note: 0 = January)
; in looking for the feedbacks I'm trying to use this as starting month for ENSO
mjo_start=11
;starting month of MJO activity after for feeedbacks
mjo_window=1
; Select "window" to consider for ENSO response, in terms of months after MJO forcing (must be at least equal to enso_nperiods)
enso_start=7
enso_window=6

;mjo_forcing_points=indgen(mjo_nmonths/12)*12+mjo_month
;mjo_forcing=indgen(mjo_nmonths/12)*12;+mjo_aft
;mjo_forcing_values=mjo_amplitude[mjo_forcing]

;enso_forcing_points=indgen(enso_nmonths/12)*12+enso_window
;enso_forcing_values=enso_amplitude[enso_forcing_points]

mjo_count=intarr(3)  ; low/normal/high
enso_count=intarr(3) ; lanina/neutral/elnino

contingency=intarr(3,3) ; MJO x ENSO

print,''
print,'El Nino years followed by MJO activity: '

FOR i=0,mjo_nmonths/12-2 DO BEGIN
;   IF mjo_forcing(i)+enso_window lt enso_nmonths THEN BEGIN
;      IF mjo_forcing_values(i) lt mjo_low_threshold THEN BEGIN
;         mjo=0
;      ENDIF ELSE IF mjo_forcing_values(i) gt mjo_high_threshold THEN BEGIN
;         mjo=2
;      ENDIF ELSE $
;         mjo=1     
;      mjo_count(mjo)=mjo_count(mjo)+1
      
      enso_values=enso_amplitude((i*12)+enso_start:(i*12)+enso_start+enso_window-1)
      IF N_ELEMENTS(where(enso_values ge elnino_threshold)) lt enso_nperiods and $
         N_ELEMENTS(where(enso_values le lanina_threshold)) lt enso_nperiods THEN BEGIN
         enso=1
      ENDIF ELSE BEGIN
         elnino_count=0
         lanina_count=0
         enso=1
         FOR j=0,enso_window-1 DO BEGIN
            IF enso_values(j) ge elnino_threshold THEN BEGIN
               elnino_count=elnino_count+1
               IF elnino_count ge enso_nperiods THEN $
                  enso=2
            ENDIF ELSE $
               elnino_count=0
            IF enso_values(j) le lanina_threshold THEN BEGIN
               lanina_count=lanina_count+1
               IF lanina_count ge enso_nperiods THEN $
                  enso=0
            ENDIF ELSE $
               lanina_count=0
         ENDFOR
      ENDELSE      
      enso_count(enso)=enso_count(enso)+1      
      
      mjo_value=MEAN(mjo_amplitude(i*12+mjo_start:(i*12)+mjo_start+mjo_window-1))
      print,mjo_value
      IF mjo_value lt mjo_low_threshold THEN BEGIN
         mjo=0
      ENDIF ELSE IF mjo_value gt mjo_high_threshold THEN BEGIN
         mjo=2
      ENDIF ELSE $
         mjo=1
      mjo_count(mjo)=mjo_count(mjo)+1

      contingency(mjo,enso)=contingency(mjo,enso)+1         
;      IF mjo eq 2 and enso eq 2 THEN $
;         print,STRTRIM(STRING(i+start_year),1)
   ;ENDIF
ENDFOR


print,''
print,'MJO counts  (low / normal / high):         '+STRTRIM(STRING(mjo_count(0)),1)+'  /  '+STRTRIM(STRING(mjo_count(1)),1)+'  /  '+STRTRIM(STRING(mjo_count(2)),1)
print,'ENSO counts (La Nina / neutral / El Nino): '+STRTRIM(STRING(enso_count(0)),1)+'  /  '+STRTRIM(STRING(enso_count(1)),1)+'  /  '+STRTRIM(STRING(enso_count(2)),1)
print,''
print,'Contingency table: Values are printed for La Nina / Normal / El Nino'
print,'MJO low    : '+STRTRIM(STRING(contingency(0,0)),1)+'  /  '+STRTRIM(STRING(contingency(0,1)),1)+'  /  '+STRTRIM(STRING(contingency(0,2)),1)
print,'MJO normal : '+STRTRIM(STRING(contingency(1,0)),1)+'  /  '+STRTRIM(STRING(contingency(1,1)),1)+'  /  '+STRTRIM(STRING(contingency(1,2)),1)
print,'MJO high   : '+STRTRIM(STRING(contingency(2,0)),1)+'  /  '+STRTRIM(STRING(contingency(2,1)),1)+'  /  '+STRTRIM(STRING(contingency(2,2)),1)
print,''

STOP
END
