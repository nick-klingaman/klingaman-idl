PRO lpf_climatology, infile, outfile, variable, width, nyears, start, ndays

;+
; NAME:
;       LPF Climatology
;
; PURPOSE:
;       This procedure constructs a climatology from a given input
;       netCDF file.  It uses a low-pass filter with a given width
;       and outputs its results to a specified netCDF file.
;
; CALLING SEQUENCE:
;       LPF_CLIMATOLOGY, infile, outfile, variable, width, nyears,
;       ndays
;
; INPUTS:
;       infile   - the full path to the netCDF input file to be 
;                  processed (string)
;       outfile  - the full path to the netCDF output file in which to
;                  store the climatology (string)
;       variable - the name of the variable in the netCDF input file.
;                  The output variable will have the same name. (string)
;       width    - the width of the low-pass filter to use, in steps
;                  in the input file (integer)
;       nyears   - the number of years of data to process and average
;                  to create the climatology (integer)
;       start    - the number of the year (1-based) in which to start
;                  calculating the climatology (relative to the first
;                  year in the dataset, which is taken to be year 1)
;       ndays    - the number of days (or steps) in the dataset for
;                  each year (integer)
;
; OUTPUTS:
;       This procedure does not return any variables.  It creates a
;       netCDF file at location [outfile].
;
; PROCEDURE:
;       The data are read in from the input file, and the mean is
;       calculated for each step (i.e., across all years).  The
;       resulting mean is put through a low-pass filter of the
;       specified width to remove high-frequency variability and
;       obtain the final climatology.  The climatology is written to
;       the output file.
; 
; RESTRICTIONS:
;       This procedure requires that the FILTER function be installed
;       as well.
;       This procedure assumes that the input netCDF file has
;       dimensions for longitude, latitude, and time, and that the
;       input variable will be read in as [longitude,latitude,time].
;       This means that the netCDF variable itself must be stored as
;       [time,latitude,longitude].
;       Note that since version 0.3, the dimension names can be any of
;       the following:
;           longitude - 'longitude', 'Longitude', 'lon', 'Lon',
;                       'long', 'Long', 'x', 'X'
;           latitude  - 'latitude', 'Latitude', 'lat', 'Lat', 'lati',
;                       'Lati', 'y', 'Y'
;           time      - 'time','z','Time','Z'
;
; SIDE EFFECTS:
;       This procedure CLOBBERs the netCDF output file, so any file
;       that already exists at location [outfile] will be overwritten.
;
; COMMON BLOCKS:
;       None
;
; MODIFICATION HISTORY:
;       0.4 - Added "start" argument so that the climatology does not
;             have to start with the first year of the dataset (NPK)
;             (20/04/06 Euro).
;       0.3 - Added ability to scan netCDF file for possible
;             longitude, latitude, and time variable names, rather
;             than relying strictly on 'longitude', 'latitude', and
;             'time' (NPK) (18/04/06 Euro).
;       0.2 - Converted for use with the FILTER procedure 
;             (NPK) (30/03/06 Euro)
;       0.1 - Written by Nicholas Klingaman (14/03/06 Euro)
;
;-

input_varids = intarr(4)
input_id = NCDF_OPEN(infile)
possible_lon_names = ['longitude','Longitude','lon','Lon','long','Long','x',$
                     'X']
possible_lat_names = ['latitude','Latitude','lat','Lat','lati','Lati','y', $
                     'Y']
possible_time_names = ['time','z','Time','Z']
variable_names = strarr(3)

FOR i=0,2 DO BEGIN
    CASE i OF
        0 : BEGIN
            possible_names = possible_lon_names
            type='longitude'
        END
        1: BEGIN
            possible_names = possible_lat_names
            type='latitude'
        END
        2 : BEGIN
            possible_names = possible_time_names
            type='time'
        END
    ENDCASE
    j = 0
    success = -1
    WHILE success eq -1 DO BEGIN
        IF j ge N_ELEMENTS(possible_names) THEN $
          MESSAGE,'LPF Climatology : Could not find '+type+' variable in the netCDF file.'
        success = NCDF_VARID(input_id,possible_names(j))
        IF success ne -1 THEN BEGIN
            input_varids(i) = success
            variable_names(i) = possible_names(j)
        ENDIF
        j = j+1
    ENDWHILE
ENDFOR

input_varids(3) = NCDF_VARID(input_id,variable)
NCDF_VARGET,input_id,input_varids(0),longitude
NCDF_VARGET,input_id,input_varids(1),latitude
NCDF_VARGET,input_id,input_varids(2),time
NCDF_VARGET,input_id,input_varids(3),x

n_lon = N_ELEMENTS(longitude)
n_lat = N_ELEMENTS(latitude)
n_time = N_ELEMENTS(time)
nx = N_ELEMENTS(x)

n_time_out = ndays
output_dimids = intarr(3)
output_varids = intarr(4)
output_id = NCDF_CREATE(outfile,/CLOBBER)
output_dimids(0) = NCDF_DIMDEF(output_id,variable_names(0),n_lon)
output_dimids(1) = NCDF_DIMDEF(output_id,variable_names(1),n_lat)
output_dimids(2) = NCDF_DIMDEF(output_id,variable_names(2),n_time_out)
output_varids(0) = NCDF_VARDEF(output_id,variable_names(0),[output_dimids(0)])
output_varids(1) = NCDF_VARDEF(output_id,variable_names(1),[output_dimids(1)])
output_varids(2) = NCDF_VARDEF(output_id,variable_names(2),[output_dimids(2)])
output_varids(3) = NCDF_VARDEF(output_id,variable,[output_dimids(0), $
                                                   output_dimids(1), $
                                                   output_dimids(2)])
FOR i=0,3 DO BEGIN
    temp_struct = NCDF_VARINQ(input_id,input_varids(i))
    FOR j=0,temp_struct.Natts-1 DO BEGIN
        temp_name = NCDF_ATTNAME(input_id,variable,j)
        temp_number = NCDF_ATTCOPY(input_id,variable,temp_name,output_id,$
                                   output_varids(i))
    ENDFOR
ENDFOR
temp_struct = NCDF_INQUIRE(input_id)
FOR i=0,temp_struct.Ngatts-1 DO BEGIN
    temp_name = NCDF_ATTNAME(input_id,/GLOBAL,i)
    temp_number = NCDF_ATTCOPY(input_id,/IN_GLOBAL,temp_name,output_id,/OUT_GLOBAL)
ENDFOR
NCDF_CONTROL,output_id,/ENDEF

mean_x = fltarr(n_lon,n_lat,ndays)
total_x = fltarr(n_lon,n_lat,ndays)
total_x(*,*,*) = 0.

FOR i=0,nyears-1 DO BEGIN
    year = start+i
    PRINT, 'Now reading data for year '+strtrim(string(year),1)+' . . .'
    FOR j=0, ndays-1 DO BEGIN
                                ; Read in current day's data
        NCDF_VARGET,input_id,input_varids(3),oneday,count=[n_lon,n_lat,1], $
          offset=[0,0,j+(year-1)*ndays]
        total_x(*,*,j) = total_x(*,*,j)+oneday
    ENDFOR
ENDFOR

temp = fltarr(ndays)
filter_x = fltarr(n_lon,n_lat,ndays)

PRINT, 'Now calculating daily means . . .'
PRINT, 'Now low-pass filtering data . . .'
FOR i=0, n_lon-1 DO BEGIN
    FOR j=0, n_lat-1 DO BEGIN
        FOR k=0, ndays-1 DO BEGIN
            mean_x(i,j,k) = total_x(i,j,k)/FLOAT(nyears)
        ENDFOR
        temp = fltarr(ndays)
        temp(*) = mean_x(i,j,*)
        temp(*) = FILTER(temp,width,/TIME,/LOWPASS)
        filter_x(i,j,*) = temp(*)
    ENDFOR
ENDFOR

NCDF_VARPUT,output_id,output_varids(0), longitude
NCDF_VARPUT,output_id,output_varids(1), latitude
NCDF_VARPUT,output_id,output_varids(2), intarr(ndays)
NCDF_VARPUT,output_id,output_varids(3), filter_x

NCDF_CLOSE,input_id
NCDF_CLOSE,output_id

STOP

END
