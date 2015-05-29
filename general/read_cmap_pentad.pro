PRO read_cmap_pentad, filename, dates, precip, time, lat, lon, missing

;+
; NAME: 
;       Read CMAP Pentad
;
; PURPOSE:
;       Read pentad data from NOAA's "enhanced" CMAP analysis, which
;       is the CMAP data blended with NCEP/NCAR reanalysis estimates.
;
; CALLING SEQUENCE:
;       READ_CMAP_PENTAD, filename, dates, precip, lat, lon, time
;
; INPUTS:
;       filename - the full path to the CMAP netCDF file
;       dates    - a four-element array containing the following (in
;                  order): starting date (Julian), starting year,
;                  ending date (Julian), ending year.
;
; OUTPUTS:
;       precip   - the precipitation data, dimensioned as [time, lat,
;                  lon]      
;       lon      - the longitude points for the precip data, as read
;                  from the CMAP file.
;       lat      - the latitude points for the precip data, as read
;                  from the CMAP file.
;       time     - the time points for the precip data, as read from
;                  the CMAP file.
;       missing  - the missing value, as read from the CMAP file.
;
; RESTRICTIONS:
;       In order to run this procedure, the JULIAN_TO_HOURS function
;       must also be installed.
;
; SIDE EFFECTS:
;       This procedure does not handle missing values; it merely
;       returns the missing_value from the netCDF dataset.  The user
;       may decide what to do with missing values separately.
;
; COMMON BLOCKS:
;       None
;
; MODIFICATION HISTORY:
;       0.2 - Procedure appears to read correct data (NPK) (18/04/06 Euro)
;       0.1 - Written by Nicholas Klingaman (18/04/06 Euro)
;
;-

ON_ERROR,2

; Do some rudimentary error-checking
IF N_ELEMENTS(dates) ne 4 THEN $
  MESSAGE, 'Read CMAP Pentad : Input variable "dates" must have four elements.'
IF (dates(1) eq dates(3) and dates(0) gt dates(2)) $
  or (dates(1) gt dates(3)) THEN $
  MESSAGE, 'Read CMAP Pentad : Starting date must not be after ending date'

id = NCDF_OPEN(filename)
lon_id = NCDF_VARID(id,'lon')
lat_id = NCDF_VARID(id,'lat')
time_id = NCDF_VARID(id,'time')
precip_id = NCDF_VARID(id,'precip')

NCDF_VARGET,id,lon_id,lon
NCDF_VARGET,id,lat_id,lat
NCDF_VARGET,id,time_id,time

n_time = N_ELEMENTS(time)
n_lat = N_ELEMENTS(lat)
print,'n_lat from read_cmap_pentad',n_lat
n_lon = N_ELEMENTS(lon)

; Unpack the date array into starting and ending dates.  Add an hour
; of 0 (midnight) as the first element to appease the JULIAN_TO_HOURS
; function.
start_julian = [0,dates(0),dates(1)]
stop_julian = [0,dates(2),dates(3)]

; Define the starting time for the CMAP dataset: 1 January 1 AD [sic].
start_cmap = [0,1,1]

; Convert the starting and ending Julian dates to hours for easy
; comparison against the CMAP dataset.
start_hours = JULIAN_TO_HOURS(start_julian,start_cmap)
stop_hours = JULIAN_TO_HOURS(stop_julian,start_cmap)

; Figure out the starting and ending time points for reading the CMAP data.
start_time = where(ABS(time-start_hours) eq (MIN(ABS(time-start_hours))))
stop_time = where(ABS(time-stop_hours) eq (MIN(ABS(time-stop_hours))))

time = time(start_time:stop_time)

; print,start_time,stop_time,start_hours,stop_hours

; Read the CMAP data.
NCDF_VARGET,id,precip_id,precip,OFFSET=[0,0,start_time-1],COUNT=[n_lon,n_lat,stop_time-start_time+1]

NCDF_ATTGET,id,precip_id,'missing_value',missing

NCDF_CLOSE,id

END
