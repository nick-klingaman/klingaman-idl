FUNCTION open_and_extract,file_name,variable_name,offset=offset,count=count,wrap=wrap

; Open a netCDF file and extract a variable therefrom.

id = NCDF_OPEN(file_name)
varid = NCDF_VARID(id,variable_name)
IF KEYWORD_SET(count) THEN BEGIN
   IF KEYWORD_SET(wrap) THEN BEGIN
      FOR j=0,5 DO BEGIN
         NCDF_DIMINQ,id,j,dimname,n_dim
         IF dimname eq 'longitude' or dimname eq 'longitude_1' or dimname eq 'longitude_2' or $
            dimname eq 'lon' or dimname eq 'x' or dimname eq 'long' THEN BEGIN
            n_lon=n_dim
            BREAK
         ENDIF
      ENDFOR
      IF offset(0)+count(0) gt n_lon THEN BEGIN
         offset_one=offset
         count_one=[n_lon-offset(0),count(1:N_ELEMENTS(count)-1)]
         NCDF_VARGET,id,varid,x1,offset=offset_one,count=count_one
         offset_two=[0,offset(1:N_ELEMENTS(offset)-1)]
         count_two=[count(0)-count_one(0),count(1:N_ELEMENTS(count)-1)]
         NCDF_VARGET,id,varid,x2,offset=offset_two,count=count_two
         x=fltarr(count)
         x=[x1,x2]
      ENDIF
   ENDIF ELSE $
      NCDF_VARGET,id,varid,x,offset=offset,count=count
ENDIF ELSE $
  NCDF_VARGET,id,varid,x
NCDF_CLOSE,id

RETURN,x

END
