PRO hadgem3_monwg_detect_missing_data

infile='/home/ss901165/um_output3/hadgem3_monwg/airxv/airxv_surftemp_daily_1982-2008.nc'

t=OPEN_AND_EXTRACT(infile,'t')
nt=N_ELEMENTS(t)

FOR i=0,nt-2 DO BEGIN
   IF t(i+1) ne t(i)+1 THEN $
      print,'Suspect missing data at position ',i,' with t(i) = ',t(i),' t(i+1) = ',t(i+1)
ENDFOR

STOP

END

