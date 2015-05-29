FUNCTION nc_pressure_levs,data,pressure,pressure_levs,CONSERVE=conserve,missing_value=missing_value

IF KEYWORD_SET(missing_value) THEN BEGIN
   our_missing_value=missing_value
ENDIF ELSE $
   our_missing_value=-9999
n_pressure_levs=N_ELEMENTS(pressure_levs)

IF SIZE(data,/N_DIMENSIONS) eq 4 THEN BEGIN
   nx=N_ELEMENTS(data(*,0,0,0))
   ny=N_ELEMENTS(data(0,*,0,0))
   nz=N_ELEMENTS(data(0,0,*,0))
   nt=N_ELEMENTS(data(0,0,0,*))
ENDIF ELSE BEGIN
   print,'FUNCTION nc_pressure_levs currently accepts only four-dimensional input data'
   STOP
ENDELSE

IF SIZE(data,/N_DIMENSIONS) ne SIZE(pressure,/N_DIMENSIONS) THEN BEGIN
   print,'nc_pressure_levs : Number of dimensions in input data must match that in input pressure data'
   STOP
ENDIF
IF N_ELEMENTS(data) ne N_ELEMENTS(pressure) THEN BEGIN
   print,'nc_pressure_levs : Number of elements in input data must match that in input pressure data'
   STOP
ENDIF

output=fltarr(nx,ny,n_pressure_levs,nt)

FOR j=0,nx-1 DO BEGIN
   FOR k=0,ny-1 DO BEGIN
      FOR m=0,nt-1 DO BEGIN
         FOR i=0,n_pressure_levs-1 DO BEGIN
            pressure_below=pressure(j,k,0,m)
            pressure_above=pressure(j,k,1,m)
            data_below=data(j,k,0,m)
            data_above=data(j,k,1,m)
            FOR n=1,nz-2 DO BEGIN
               indices=where(pressure(j,k,n,m) gt pressure_levs(i))
               IF indices(0) ne -1 THEN BEGIN
                  pressure_below=pressure(j,k,n,m)
                  pressure_above=pressure(j,k,n+1,m)
                  data_below=data(j,k,n,m)
                  data_above=data(j,k,n+1,m)
               ENDIF               
            ENDFOR
            
            IF pressure(j,k,0,m) lt pressure_levs(i) THEN BEGIN
               output(j,k,i,m)=our_missing_value
            ENDIF ELSE IF pressure(j,k,nz-1,m) gt pressure_levs(i) THEN BEGIN
               output(j,k,i,m)=our_missing_value
            ENDIF ELSE BEGIN
               weight_upper=ALOG(pressure_levs(i)/pressure_below)/ALOG(pressure_above/pressure_below)
               IF weight_upper lt 0 or weight_upper gt 1 THEN BEGIN
                  print,'weight_upper out of range ... STOP'
                  STOP
               ENDIF
               weight_lower=1.0-weight_upper
               output(j,k,i,m)=weight_upper*data_above+weight_lower*data_below
            ENDELSE
         ENDFOR
         IF KEYWORD_SET(conserve) THEN BEGIN
            original_integral=0
            new_integral=0
            original_dp_total=0
            thispt_output=REFORM(output(j,k,*,m))
            valid_pressure=pressure_levs[where(thispt_output ne our_missing_value)]
;            print,valid_pressure
            FOR n=0,nz-2 DO BEGIN
               IF pressure(j,k,n+1,m) ge MIN(valid_pressure) and pressure(j,k,n,m) le MAX(valid_pressure) THEN BEGIN
                  original_integral=(data(j,k,n,m)+data(j,k,n+1,m))/2.*(pressure(j,k,n,m)-pressure(j,k,n+1,m))+original_integral
                  original_dp_total=original_dp_total+(pressure(j,k,n,m)-pressure(j,k,n+1,m))
               ENDIF ELSE IF pressure(j,k,n+1,m) le MIN(valid_pressure) and pressure(j,k,n,m) ge MIN(valid_pressure) THEN BEGIN
                  weight_upper=ALOG(MIN(valid_pressure)/pressure(j,k,n,m))/ALOG(pressure(j,k,n+1,m)/pressure(j,k,n,m))
                  weight_lower=1.0-weight_upper
                  top_value=data(j,k,n,m)*weight_lower+data(j,k,n+1,m)*weight_upper
                  original_integral=(data(j,k,n,m)+top_value)/2.*(pressure(j,k,n,m)-MIN(valid_pressure))+original_integral
                  original_dp_total=original_dp_total+(pressure(j,k,n,m)-MIN(valid_pressure))
               ENDIF ELSE IF pressure(j,k,n,m) ge MAX(valid_pressure) and pressure(j,k,n+1,m) le MAX(valid_pressure) THEN BEGIN
                  weight_upper=ALOG(MAX(valid_pressure)/pressure(j,k,n,m))/ALOG(pressure(j,k,n+1,m)/pressure(j,k,n,m))
                  weight_lower=1.0-weight_upper
                  bottom_value=data(j,k,n,m)*weight_lower+data(j,k,n+1,m)*weight_upper
                  original_integral=(bottom_value+data(j,k,n+1,m))/2.*(MAX(valid_pressure)-pressure(j,k,n+1,m))+original_integral
                  original_dp_total=original_dp_total+(MAX(valid_pressure)-pressure(j,k,n+1,m))
               ENDIF
            ENDFOR
            original_integral=original_integral/original_dp_total            
            new_dp_total=0            
            contribution=fltarr(n_pressure_levs)
            FOR i=0,n_pressure_levs-2 DO BEGIN
               IF thispt_output(i) ne our_missing_value and thispt_output(i+1) ne our_missing_value THEN BEGIN
                  contribution(i)=(thispt_output(i)+thispt_output(i+1))/2.*(pressure_levs(i)-pressure_levs(i+1))
                  new_integral=contribution(i)+new_integral 
                  new_dp_total=new_dp_total+(pressure_levs(i)-pressure_levs(i+1))
               ENDIF
            ENDFOR
            new_integral=new_integral/new_dp_total
;            print,original_integral,new_integral
            stop_flag=0
            pass_count=0
            IF new_integral ne original_integral and original_integral ne 0 and new_integral ne 0 THEN BEGIN
               WHILE stop_flag eq 0 DO BEGIN
                                
                  adjustment=fltarr(n_pressure_levs)
;               adjustment=(original_integral-new_integral)*contribution/TOTAL(ABS(contribution))
;               thispt_output[where(thispt_output ne our_missing_value)]=$
;                  thispt_output[where(thispt_output ne our_missing_value)]+adjustment
                  new_new_integral=0
                  new_new_dp_total=0
                  FOR i=0,n_pressure_levs-2 DO BEGIN
                     IF thispt_output(i) ne our_missing_value THEN BEGIN
                        adjustment(i)=(original_integral-new_integral)*ABS(contribution(i))/TOTAL(ABS(contribution))*new_dp_total/(pressure_levs(i)-pressure_levs(i+1))
                        thispt_output(i)=thispt_output(i)+adjustment(i)
                     ENDIF
                  ENDFOR
                  FOR i=0,n_pressure_levs-2 DO BEGIN
                     IF thispt_output(i) ne our_missing_value and thispt_output(i+1) ne our_missing_value THEN BEGIN
                        new_new_integral=(thispt_output(i)+thispt_output(i+1))/2.*(pressure_levs(i)-pressure_levs(i+1))+new_new_integral
                        new_new_dp_total=new_new_dp_total+(pressure_levs(i)-pressure_levs(i+1))
                     ENDIF
                  ENDFOR
                  new_new_integral=new_new_integral/new_new_dp_total
                  IF ABS(new_new_integral-original_integral)/original_integral gt 0.01 THEN BEGIN
                     pass_count=pass_count+1
                     new_integral=new_new_integral
                     new_new_integral=0
                     new_new_dp_total=0
                     IF pass_count eq 20 THEN BEGIN
                        print,'Conservation failed after 20 passes ... '
                        print,'Original integral: '+STRTRIM(STRING(original_integral),1)
                        print,'Final integral: '+STRTRIM(STRING(new_integral),1)
                        stop_flag=1
                     ENDIF ELSE $
                        stop_flag=0
                  ENDIF ELSE $
                     stop_flag=1               
               ENDWHILE               
            ENDIF ELSE IF original_integral eq 0 and new_integral eq 0 THEN $
               output(j,k,*,m)=0.            
            output(j,k,*,m)=thispt_output
            ENDIF
      ENDFOR
   ENDFOR
ENDFOR

RETURN,output

END

