PRO make_u_climatology

; This procedure makes a climatology of zonal winds using a Butterworth
; low-pass filter for a specified latitude, longitude, and date range.

start_date_clim = 1
stop_date_clim = 365
start_year_clim = 1998
stop_year_clim = 2004

ndates = stop_date_clim - start_date_clim + 1
nyears = stop_year_clim - start_year_clim + 1
n_obs = 4

netcdf_varids_in = intarr(3)
daily_dimids = intarr(3)
netcdf_dimids = intarr(3)
daily_varids = intarr(4)
netcdf_varids = intarr(4)

; Read in latitude and longitude data
input_file = '/net/hermes/export/hermes/data-03/ss901165/usurf.nc'
daily_output = '/net/hermes/export/hermes/data-03/ss901165/usurf_daily.nc'
netcdf_id_in = NCDF_OPEN(input_file)
netcdf_varids_in(0) = NCDF_VARID(netcdf_id_in,'longitude')
netcdf_varids_in(1) = NCDF_VARID(netcdf_id_in,'latitude')
netcdf_varids_in(2) = NCDF_VARID(netcdf_id_in,'usurf')
NCDF_VARGET,netcdf_id_in,netcdf_varids_in(0),u_lon
NCDF_VARGET,netcdf_id_in,netcdf_varids_in(1),u_lat

n_lon = n_elements(u_lon)
n_lat = n_elements(u_lat)

dates = indgen(ndates)+1

mean_u = fltarr(n_lon,n_lat,ndates)
u_totals = fltarr(n_lon,n_lat,ndates)
u_average = fltarr(n_lon,n_lat,ndates)
u_filter_low = fltarr(n_lon,n_lat,ndates)

u_totals(*,*,*) = 0.

daily_id = NCDF_CREATE(daily_output,/CLOBBER)
daily_dimids(0) = NCDF_DIMDEF(daily_id,'longitude',n_lon)
daily_dimids(1) = NCDF_DIMDEF(daily_id,'latitude',n_lat)
daily_dimids(2) = NCDF_DIMDEF(daily_id,'time',ndates*nyears)
daily_varids(0) = NCDF_VARDEF(daily_id,'longitude',[daily_dimids(0)])
daily_varids(1) = NCDF_VARDEF(daily_id,'latitude',[daily_dimids(1)])
daily_varids(2) = NCDF_VARDEF(daily_id,'time',[daily_dimids(2)])
daily_varids(3) = NCDF_VARDEF(daily_id,'usurf',[daily_dimids(0), $
                                                    daily_dimids(1), $
                                                    daily_dimids(2)])
NCDF_CONTROL,daily_id,/endef
NCDF_VARPUT,daily_id,daily_varids(0),u_lon
NCDF_VARPUT,daily_id,daily_varids(1),u_lat
NCDF_VARPUT,daily_id,daily_varids(2),indgen(ndates*nyears)

leap_years = 0
FOR i = 0, nyears-1 DO BEGIN
    PRINT,'Now reading data for '+strtrim(string(i+start_year_clim),1)+' . . . '
                                ; If the previous year was a leap year
                                ; then we need to skip a day; this
                                ; effectively skips the last day of
                                ; every leap year, creating a
                                ; climatology for the first 365 of any year.
    IF (i-1+start_year_clim) MOD 4 eq 0 THEN leap_years = leap_years+1
    FOR j=0, ndates-1 DO BEGIN
        
                                ; Read in current day's zonal wind data and add to total for date.
        NCDF_VARGET,netcdf_id_in,netcdf_varids_in(2),u,count=[n_lon,n_lat,n_obs],$
          offset=[0,0,(j+leap_years)*n_obs+i*n_obs*ndates]
                                ; Take a time average over all points
                                ; and assign to u_totals
        FOR k=0,n_lon-1 DO BEGIN
            FOR m=0,n_lat-1 DO BEGIN
                good = where(u(k,m,*) lt 1000)
                mean_u(k,m,j) = MEAN(u(k,m,[good]))
                u_totals(k,m,j) = mean_u(k,m,j) + u_totals(k,m,j)
            ENDFOR
        ENDFOR
    ENDFOR
    NCDF_VARPUT,daily_id,daily_varids(3),mean_u,offset=[0,0,365*i]
ENDFOR

NCDF_CLOSE,daily_id

; Calculate average zonal wind for each date

PRINT,'Now calculating daily means . . .'

for i = 0, n_lon-1 do begin
    for j = 0, n_lat-1 do begin
        for k=0, ndates-1 do begin
            u_average(i,j,k) = u_totals(i,j,k)/float(nyears)
        endfor
    endfor
endfor

; Use a 15-day Butterworth low-pass filter on each grid point

PRINT,'Now calculating Butterworth low-pass filter . . .'
filter = 1.0 / (1.0d + (DIST(ndates)/15.0)^2)

for i = 0, n_lon-1 do begin
    for j = 0, n_lat-1 do begin
        u_filter_low(i,j,*) = fft(fft(u_average(i,j,*),-1)*filter,1)
    endfor
endfor

netcdf_dimids = intarr(3)
netcdf_varids = intarr(4)

netcdf_filename = '/home/ss901165/datasets/ECMWF_CLIMATOLOGY/usurf_15day.nc'
netcdf_id = NCDF_CREATE(netcdf_filename,/CLOBBER)
netcdf_dimids(0) = NCDF_DIMDEF(netcdf_id,'longitude',n_lon)
netcdf_dimids(1) = NCDF_DIMDEF(netcdf_id,'latitude',n_lat)
netcdf_dimids(2) = NCDF_DIMDEF(netcdf_id,'time',ndates)
netcdf_varids(0) = NCDF_VARDEF(netcdf_id,'longitude',netcdf_dimids(0))
netcdf_varids(1) = NCDF_VARDEF(netcdf_id,'latitude',netcdf_dimids(1))
netcdf_varids(2) = NCDF_VARDEF(netcdf_id,'time',netcdf_dimids(2))
netcdf_varids(3) = NCDF_VARDEF(netcdf_id,'usurf', $
                               [netcdf_dimids(0),netcdf_dimids(1), $
                                netcdf_dimids(2)])
NCDF_CONTROL,netcdf_id,/endef
NCDF_VARPUT,netcdf_id,netcdf_varids(0),u_lon
NCDF_VARPUT,netcdf_id,netcdf_varids(1),u_lat
NCDF_VARPUT,netcdf_id,netcdf_varids(2),dates
NCDF_VARPUT,netcdf_id,netcdf_varids(3),u_filter_low

NCDF_CLOSE, netcdf_id

STOP

END
