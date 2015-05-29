FUNCTION levs2string, levels, interval, num_chars, offset=offset

nx = N_ELEMENTS(levels)
string_levels = strarr(nx)

IF KEYWORD_SET(offset) THEN BEGIN
    our_offset = offset
ENDIF ELSE $
  our_offset = 0

num_transform = (nx+1-our_offset)/interval+1
transform = indgen(num_transform)*interval+our_offset

FOR i=0,nx-1 DO BEGIN
    IF where(transform eq i) ne -1 THEN BEGIN
        IF levels(i) gt 0 THEN BEGIN
            this_num_chars = num_chars-1
        ENDIF ELSE $
          this_num_chars = num_chars
        string_levels(i)= STRMID(STRTRIM(STRING(levels(i)),1),0,this_num_chars)
        IF ABS(levels(i)) lt 0.0000001 THEN string_levels(i) = '0.00'
    ENDIF ELSE $
      string_levels(i)= ' '
ENDFOR

RETURN, string_levels

END



