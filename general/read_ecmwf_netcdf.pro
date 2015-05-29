PRO read_ecmwf_netcdf, filename, variable, dates, x

; Procedure to read full (1998-2005) ECMWF analysis netCDF file.
; Required arguments are:

; filename (string) - full path to file to be read
; variable (string) - the name of the variable to be read
; dates (integer)   - a four-element array of the following:
;                     start_date: the date to start the read
;                     start_year: the year to start the read
;                     end_date  : the date to stop the read
;                     end_year  : the year to stop the read
; x                 - an array containing the resulting data

; Unpack the dates array
start_date = dates(0)
start_year = dates(1)
stop_date = dates(2)
stop_year = dates(3)

; Open the netCDF file to be read and get variable ID
id = NCDF_OPEN(filename)
varid = NCDF_VARID(id,variable)

; Start date and year of the data file
file_start_date = 1
file_start_year = 1998

; Calculate offseta and count
offset = DIFF_DATES([file_start_date,file_start_year],[start_date,start_year])-1
count = DIFF_DATES([start_date,start_year],[stop_date,stop_year])+1

; Read in netCDF files
latid = NCDF_VARID(id,'latitude')
lonid = NCDF_VARID(id,'longitude')
NCDF_VARGET,id,latid,latitudes
NCDF_VARGET,id,lonid,longitudes
n_lat = N_ELEMENTS(latitudes)
n_lon = N_ELEMENTS(longitudes)
print,offset,count
NCDF_VARGET,id,varid,x,offset=[0,0,offset],count=[n_lon,n_lat,count]

END
