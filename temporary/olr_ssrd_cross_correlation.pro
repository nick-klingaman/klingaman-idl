PRO olr_ssrd_cross_correlation

box = [65,10,85,25] ; start_lon,start_lat,stop_lon,stop_lat

olr_filename = '/home/ss901165/datasets/ACTIVE_BREAK_COMPOSITES/olr_active.nc'
ssrd_filename = '/home/ss901165/datasets/ACTIVE_BREAK_COMPOSITES/ssrd_active.nc'
olrid = NCDF_OPEN(olr_filename)
ssrdid = NCDF_OPEN(ssrd_filename)

olr_varid = NCDF_VARID(olrid,'olr_anomalies_avg')
ssrd_varid = NCDF_VARID(ssrdid,'ssrd_anomalies_avg')
olr_lonid = NCDF_VARID(olrid,'longitude')
ssrd_lonid = NCDF_VARID(ssrdid,'longitude')
olr_latid = NCDF_VARID(olrid,'latitude')
ssrd_latid = NCDF_VARID(ssrdid,'latitude')

NCDF_VARGET,olrid,olr_varid,olr
NCDF_VARGET,ssrdid,ssrd_varid,ssrd
NCDF_VARGET,olrid,olr_lonid,olr_lon
NCDF_VARGET,ssrdid,ssrd_lonid,ssrd_lon
NCDF_VARGET,olrid,olr_latid,olr_lat
NCDF_VARGET,ssrdid,ssrd_latid,ssrd_lat

temp_olr = fltarr(N_ELEMENTS(olr(0,*,0)),N_ELEMENTS(olr(0,0,*)))
temp_ssrd = fltarr(N_ELEMENTS(ssrd(0,*,0)),N_ELEMENTS(ssrd(0,0,*)))

n_time = N_ELEMENTS(olr(*,0,0))
olr_avg = fltarr(n_time)
ssrd_avg = fltarr(n_time)

lags = indgen(15)-7

FOR i=0,n_time-1 DO BEGIN
    temp_olr(*,*) = olr(i,*,*)
    temp_ssrd(*,*) = ssrd(i,*,*)
    olr_avg(i) = AREA_AVERAGE_BOX(temp_olr,olr_lon,olr_lat,box,-99.99)
    ssrd_avg(i) = AREA_AVERAGE_BOX(temp_ssrd,ssrd_lon,ssrd_lat,box,-99.99)
ENDFOR

result = C_CORRELATE(olr_avg,ssrd_avg,lags)

FOR i=0,N_ELEMENTS(lags)-1 DO $
    print, 'Cross-correlation for olr (day 0) and ssrd (time ',strtrim(string(lags(i)),1),') = ', result(i)

END

