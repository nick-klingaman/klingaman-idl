PRO read_ostia_netcdf,filename,varname,data_start_date,start_date,stop_date,box,sst,lon,lat,time
;+
; NAME:
;       READ_OSTIA_NETCDF
;
; PURPOSE:
;       Read OSTIA netCDF file of GHRSST project SSTs
;
; CALLING SEQUENCE:
;       READ_OSTIA_NETCDF,filename,varname,data_start_date,start_date,end_date
;
; INPUTS:
;       filename        - The full path to the OSTIA netCDF file
;       varname         - The name of the variable to read in the OSTIA
;                         netCDF file
;       data_start_date - A three-element array containing
;                         [day,month,year] of the starting date of the
;                         OSTIA file.
;       start_date      - A three-element array containing
;                         [day,month,year] of the starting point for
;                         reading.
;       stop_date       - A three-element array containing
;                         [day,month,year] of the ending point for
;                         reading.
;       box             - A four-element array containing
;                         [start_lat,start_lon,stop_lat,stop_lon] to
;                         define a box for GHRSST data to read.
;
; OUTPUTS:
;       sst  - The sea-surface temperatures from the OSTIA file,
;              dimensioned as [lon,lat,time]
;       lon  - A one-dimensional array of longitudes from the OSTIA
;              file.
;       lat  - A one-dimensional array of latitudes from the OSTIA
;              file.
;       time - A one-dimensional array of time points from the OSTIA
;              file.
;
; RESTRICTIONS:
;       None.
;
; SIDE EFFECTS:
;       None.
;
; COMMON BLOCKS:
;       None.
;
; MODIFICATION HISTORY:
;       0.1 - Written by Nicholas Klingaman (4/8/06 Euro)
;-

ON_ERROR,2
IF N_PARAMS(0) ne 10 THEN $
  MESSAGE,'READ_OSTIA_NETCDF : Need 10 input parameters, got '+STRING(strtrim(N_PARAMS(0)),1)

; Decompose data_start_date, start_date, stop_date arrays
data_start_day = data_start_date(0)
data_start_month = data_start_date(1)
data_start_year = data_start_date(2)
start_date_day = start_date(0)
start_date_month = start_date(1)
start_date_year = start_date(2)
stop_date_day = stop_date(0)
stop_date_month = stop_date(1)
stop_date_year = stop_date(2)

; Decompose box array
start_lat = box(0)
start_lon = box(1)
stop_lat = box(2)
stop_lon = box(3)

; Compute offset time and length (in days) for reading OSTIA file
julian_data_start = YMD2JD(data_start_year,data_start_month,data_start_day)
julian_read_start = YMD2JD(start_date_year,start_date_month,start_date_day)
julian_read_stop = YMD2JD(stop_date_year,stop_date_month,stop_date_day)
time_offset = julian_read_start-julian_data_start
time_length = julian_read_stop-julian_read_start+1

; Open netCDF file and get variable IDs
id = NCDF_OPEN(filename)
lonid = NCDF_VARID(id,'longitude')
latid = NCDF_VARID(id,'latitude')
timeid = NCDF_VARID(id,'time')
sstid = NCDF_VARID(id,varname)

; Read latitudes, longitudes, and times
NCDF_VARGET,id,latid,latitudes
NCDF_VARGET,id,lonid,longitudes
NCDF_VARGET,id,timeid,time

; Compute offset lat and lon and length (in grid boxes) for reading
; OSTIA file.
grid_lat_start = MIN([NEAREST(latitudes,start_lat),NEAREST(latitudes,stop_lat)])
grid_lat_stop = MAX([NEAREST(latitudes,start_lat),NEAREST(latitudes,stop_lat)])
grid_lon_start = MIN([NEAREST(longitudes,start_lon),NEAREST(longitudes,stop_lon)])
grid_lon_stop = MAX([NEAREST(longitudes,start_lon),NEAREST(longitudes,stop_lon)])
lon_offset = grid_lon_start
lon_length = grid_lon_stop-grid_lon_start+1
lat_offset = grid_lat_start
lat_length = grid_lat_stop-grid_lat_start+1

; Select proper latitude, longitude, and time values to return
lat = latitudes(grid_lat_start:grid_lat_stop)
lon = longitudes(grid_lon_start:grid_lon_stop)
time = time(time_offset+1:time_offset+time_length)

; Read SST data (finally)
NCDF_VARGET,id,sstid,sst,offset=[lon_offset,lat_offset,time_offset],count=[lon_length,lat_length,time_length]

; Cleanup
CLOSE,id

END














 














