PRO bcfilemake

; takes the 5-day filtered sfc temperature data from Cristina's SPCAM5d run
; and converts is to monthly mean data, centered on the middle day of each month.

ncfile = '/disk2/Data/sp-cam-spec/sst_bc_spccsm_64x128_09.0001-12.0023.nc'
netid = NCDF_OPEN(ncfile)
NCDF_VARGET, netid, 'lon', lon
NCDF_VARGET, netid, 'lat', lat
NCDF_VARGET, netid, 'time', time
NCDF_VARGET, netid, 'date', date
NCDF_VARGET, netid, 'datesec', datesec
NCDF_VARGET, netid, 'SST_cpl', sst
NCDF_CLOSE, netid

ncfile = '/temp/sst_HadOIBl_bc_64x128_clim_c020411.nc'
netid = NCDF_OPEN(ncfile)
NCDF_VARGET, netid, 'ice_cov', ice_clim
NCDF_VARGET, netid, 'date', date_clim
NCDF_VARGET, netid, 'datesec', datesec_clim
NCDF_CLOSE, netid



dim = SIZE(sst)
ndays = dim(3)
jday0 = JULDAY(9,1,1997)+findgen(122)
jday1 = JULDAY(1,1,1998)+findgen(365)
CALDAT, jday0, m0,d0,y0
y0(0:121) = 1
CALDAT, jday1, m1,d1,y1
y1 = intarr(8030)
FOR y=0,21 DO y1(y*365:y*365+364) = y+2
yr = [y0,y1]
mn = [m0,m1,m1,m1,m1,m1,m1,m1,m1,m1,m1,m1,m1,m1,m1,m1,m1,m1,m1,m1,m1,m1,m1]
day = [d0,d1,d1,d1,d1,d1,d1,d1,d1,d1,d1,d1,d1,d1,d1,d1,d1,d1,d1,d1,d1,d1,d1]
nmonths = 269
sstm = fltarr(N_ELEMENTS(lon),N_ELEMENTS(lat),nmonths)
icem = fltarr(N_ELEMENTS(lon),N_ELEMENTS(lat),nmonths)
timem = fltarr(nmonths)
datem = lonarr(nmonths)
datesec = lonarr(nmonths)

m2 = indgen(12)+1
mon = [9,10,11,12,m2,m2,m2,m2,m2,m2,m2,m2,m2,m2,m2,m2,m2,m2,m2,m2,m2,m2,m2,m2,m2,m2,1]

FOR m=0,11 DO BEGIN
	x = WHERE(mon EQ m+1,nmatch)
	icem(*,*,x) = REBIN(ice_clim(*,*,m),N_ELEMENTS(lon),N_ELEMENTS(lat),nmatch)
	datesec(x) = datesec_clim(m)
ENDFOR
	

FOR y=0,22 DO BEGIN
	IF y EQ 0 THEN BEGIN
		FOR m=9,12 DO BEGIN
			sstm(*,*,y*12+m-9) = MEAN(sst(*,*,WHERE(yr EQ y+1 AND mn EQ m)),dimension=3)
			timem(y*12+m-9) = time(WHERE(yr EQ y+1 AND mn EQ m AND day EQ 16))
			datem(y*12+m-9) = date(WHERE(yr EQ y+1 AND mn EQ m AND day EQ 16))
		ENDFOR
	ENDIF ELSE BEGIN	
		FOR m=1,12 DO BEGIN
			sstm(*,*,y*12+m-9) = MEAN(sst(*,*,WHERE(yr EQ y+1 AND mn EQ m)),dimension=3)
			IF (m NE 2) THEN BEGIN
				timem(y*12+m-9) = time(WHERE(yr EQ y+1 AND mn EQ m AND day EQ 16))
				datem(y*12+m-9) = date(WHERE(yr EQ y+1 AND mn EQ m AND day EQ 16))
			ENDIF ELSE BEGIN
				timem(y*12+m-9) = time(WHERE(yr EQ y+1 AND mn EQ m AND day EQ 15))
				datem(y*12+m-9) = date(WHERE(yr EQ y+1 AND mn EQ m AND day EQ 15))
			ENDELSE
		ENDFOR
	ENDELSE
ENDFOR
; generate a January SST so that model can get all the way to Dec 31
sstm(*,*,268) = MEAN(sst(*,*,WHERE(mn EQ 1)),dimension=3)
timem(268) = timem(267)+31
datem(268) = 240116
icem(*,*,268) = ice_clim(*,*,0)

ncfileout = '/disk2/Data/sp-cam-spec/sst_bc_spccsm_64x128_09.0001-12.0023_monthly_test.nc'
netidout = NCDF_CREATE(ncfileout, /CLOBBER)

; define dimensions
nlonid = NCDF_DIMDEF(netidout, 'lon', N_ELEMENTS(lon))
nlatid = NCDF_DIMDEF(netidout, 'lat', N_ELEMENTS(lat))
ntid = NCDF_DIMDEF(netidout, 'time',/UNLIMITED)

; define 1D variables and assign attributes
vid0 = NCDF_VARDEF(netidout, 'lat', [nlatid], /FLOAT)
NCDF_ATTPUT, netidout, vid0, 'units', 'degrees north'
NCDF_ATTPUT, netidout, vid0, 'long_name', 'latitude'

vid1 = NCDF_VARDEF(netidout, 'lon', [nlonid], /FLOAT)
NCDF_ATTPUT, netidout, vid1, 'units', 'degrees east'
NCDF_ATTPUT, netidout, vid1, 'long_name', 'longitude'

vid3 = NCDF_VARDEF(netidout, 'time', [ntid], /FLOAT)
NCDF_ATTPUT, netidout, vid3, 'units', 'days since 0001-09-01 00:00:00'
NCDF_ATTPUT, netidout, vid3, 'long_name', 'days'

vid4 = NCDF_VARDEF(netidout, 'date', [ntid], /LONG)
NCDF_ATTPUT, netidout, vid4, 'long_name', 'current date as 8 digit integer (YYYYMMDD)'

vid5 = NCDF_VARDEF(netidout, 'datesec', [ntid], /LONG)
NCDF_ATTPUT, netidout, vid5, 'long_name', 'seconds to complete current date'

vid = NCDF_VARDEF(netidout, 'SST_cpl', [nlonid,nlatid,ntid], /FLOAT)
NCDF_ATTPUT, netidout, vid, 'long_name', 'sea surface temperature'
NCDF_ATTPUT, netidout, vid, 'units', 'degrees C'
NCDF_ATTPUT, netidout, vid, 'missing', -999.

vid6 = NCDF_VARDEF(netidout, 'ice_cov', [nlonid,nlatid,ntid], /FLOAT)
NCDF_ATTPUT, netidout, vid6, 'long_name', 'BCS Pseudo Sea-ice concentration'
NCDF_ATTPUT, netidout, vid6, 'units', 'fraction'
NCDF_ATTPUT, netidout, vid6, 'missing', -999.


; make some global variable attributes
NCDF_ATTPUT, netidout, /GLOBAL, 'description','Monthly mean Sfc temp from SPCCSM3.0 run by Cristiana Stan'

NCDF_CONTROL, netidout, /ENDEF
NCDF_VARPUT, netidout, vid0, lat
NCDF_VARPUT, netidout, vid1, lon
NCDF_VARPUT, netidout, vid3, timem
NCDF_VARPUT, netidout, vid4, datem
NCDF_VARPUT, netidout, vid5, datesec
NCDF_VARPUT, netidout, vid, sstm
NCDF_VARPUT, netidout, vid6, icem

NCDF_CONTROL, netidout, /REDEF
print, '/REDEF control successful...'

;help, netidout,varname,nlonid,nlatid,ntid

vid = NCDF_VARDEF(netidout, 'SST_cpl', [nlonid,nlatid,ntid], /FLOAT)
print, 'vid successful...'

NCDF_ATTPUT, netidout, vid, 'long_name', 'sea surface temperature'
print, 'long_name attput successful...'

NCDF_ATTPUT, netidout, vid, 'units', 'degrees C'
print, 'units attput successful...'

NCDF_ATTPUT, netidout, vid, 'missing', -999.
print, 'units attput successful...'

NCDF_CONTROL, netidout, /ENDEF
print, '/ENDEF control successful...'

NCDF_VARPUT, netidout, vid, sstm
print, 'varput successful...'

NCDF_CLOSE, netidout


ncfile = '/disk2/Data/sp-cam-spec/sst_HadOIBl_bc_64x128_clim_c020411_daily.nc'
netid = NCDF_OPEN(ncfile)
NCDF_VARGET, netid, 'ice_cov', ice
NCDF_VARGET, netid, 'day', ice_day
NCDF_CLOSE, netid
jdayice = JULDAY(1,1,1989) + findgen(365)
CALDAT, jdayice, mice, dice, yice
ice_daily = fltarr(128,64,8152)
FOR i=0,8151 DO ice_daily(*,*,i) = ice(*,*,WHERE(mice EQ mn(i) AND dice EQ day(i)))

ncfileout = '/disk2/Data/sp-cam-spec/ice_bc_spccsm_64x128_09.0001-12.0023_daily.nc'
netidout = NCDF_CREATE(ncfileout, /CLOBBER)

; define dimensions
nlonid = NCDF_DIMDEF(netidout, 'lon', N_ELEMENTS(lon))
nlatid = NCDF_DIMDEF(netidout, 'lat', N_ELEMENTS(lat))
;ntid = NCDF_DIMDEF(netidout, 'time', N_ELEMENTS(time),/UNLIMITED)
ntid = NCDF_DIMDEF(netidout, 'time',/UNLIMITED)

; define 1D variables and assign attributes
vid0 = NCDF_VARDEF(netidout, 'lat', [nlatid], /FLOAT)
NCDF_ATTPUT, netidout, vid0, 'units', 'degrees north'
NCDF_ATTPUT, netidout, vid0, 'long_name', 'latitude'

vid1 = NCDF_VARDEF(netidout, 'lon', [nlonid], /FLOAT)
NCDF_ATTPUT, netidout, vid1, 'units', 'degrees east'
NCDF_ATTPUT, netidout, vid1, 'long_name', 'longitude'

vid3 = NCDF_VARDEF(netidout, 'time', [ntid], /FLOAT)
NCDF_ATTPUT, netidout, vid3, 'units', 'days since 0001-09-01 00:00:00'
NCDF_ATTPUT, netidout, vid3, 'long_name', 'days'

vid4 = NCDF_VARDEF(netidout, 'date', [ntid], /LONG)
NCDF_ATTPUT, netidout, vid4, 'long_name', 'current date as 8 digit integer (YYYYMMDD)'

vid5 = NCDF_VARDEF(netidout, 'datesec', [ntid], /LONG)
NCDF_ATTPUT, netidout, vid5, 'long_name', 'seconds to complete current date'

vid6 = NCDF_VARDEF(netidout, 'ice_cov', [nlonid,nlatid,ntid], /FLOAT)
NCDF_ATTPUT, netidout, vid6, 'long_name', 'BCS Pseudo Sea-ice concentration'
NCDF_ATTPUT, netidout, vid6, 'units', 'fraction'
NCDF_ATTPUT, netidout, vid6, 'missing', -999.


; make some global variable attributes
NCDF_ATTPUT, netidout, /GLOBAL, 'description','Daily climatological ice coverage based on climatological monthly data'

NCDF_CONTROL, netidout, /ENDEF
NCDF_VARPUT, netidout, vid0, lat
NCDF_VARPUT, netidout, vid1, lon
NCDF_VARPUT, netidout, vid3, time
NCDF_VARPUT, netidout, vid4, date
NCDF_VARPUT, netidout, vid5, datesec
NCDF_VARPUT, netidout, vid6, ice_daily
NCDF_CLOSE, netidout

ncfile = '/disk2/Data/sp-cam-spec/sst_ice_bc_spccsm_64x128_09.0001-12.0023.nc'
netid = NCDF_OPEN(ncfile)
NCDF_VARGET, netid, 'ice_cov', ice
NCDF_VARGET, netid, 'SST_cpl', sst

dum(i,j) = 0

;;;;;;; now add Jan 1 of year 24 so the daily SST run can go all the way to Dec 31 yr 23.
ncfile = '/disk2/Data/sp-cam-spec/sst_ice_bc_spccsm_64x128_09.0001-12.0023.nc'
netid = NCDF_OPEN(ncfile)
NCDF_VARGET, netid, 'lon', lon
NCDF_VARGET, netid, 'lat', lat
NCDF_VARGET, netid, 'time', time
NCDF_VARGET, netid, 'date', date
NCDF_VARGET, netid, 'datesec', datesec
NCDF_VARGET, netid, 'SST_cpl', sst
NCDF_VARGET, netid, 'ice_cov', ice
NCDF_CLOSE, netid
sst = [[[sst]],[[sst(*,*,8151)]]]
ice = [[[ice]],[[ice(*,*,8151)]]]
time = [time,8152.]
date = [date,240101]
datesec = [datesec,0]

ncfileout = '/disk2/Data/sp-cam-spec/ice_bc_spccsm_64x128_09.0001-12.0023_daily_test.nc'
netidout = NCDF_CREATE(ncfileout, /CLOBBER)

; define dimensions
nlonid = NCDF_DIMDEF(netidout, 'lon', N_ELEMENTS(lon))
nlatid = NCDF_DIMDEF(netidout, 'lat', N_ELEMENTS(lat))
;ntid = NCDF_DIMDEF(netidout, 'time', N_ELEMENTS(time),/UNLIMITED)
ntid = NCDF_DIMDEF(netidout, 'time',/UNLIMITED)

; define 1D variables and assign attributes
vid0 = NCDF_VARDEF(netidout, 'lat', [nlatid], /FLOAT)
NCDF_ATTPUT, netidout, vid0, 'units', 'degrees north'
NCDF_ATTPUT, netidout, vid0, 'long_name', 'latitude'

vid1 = NCDF_VARDEF(netidout, 'lon', [nlonid], /FLOAT)
NCDF_ATTPUT, netidout, vid1, 'units', 'degrees east'
NCDF_ATTPUT, netidout, vid1, 'long_name', 'longitude'

vid3 = NCDF_VARDEF(netidout, 'time', [ntid], /FLOAT)
NCDF_ATTPUT, netidout, vid3, 'units', 'days since 0001-09-01 00:00:00'
NCDF_ATTPUT, netidout, vid3, 'long_name', 'days'

vid4 = NCDF_VARDEF(netidout, 'date', [ntid], /LONG)
NCDF_ATTPUT, netidout, vid4, 'long_name', 'current date as 8 digit integer (YYYYMMDD)'

vid5 = NCDF_VARDEF(netidout, 'datesec', [ntid], /LONG)
NCDF_ATTPUT, netidout, vid5, 'long_name', 'seconds to complete current date'

vid6 = NCDF_VARDEF(netidout, 'ice_cov', [nlonid,nlatid,ntid], /FLOAT)
NCDF_ATTPUT, netidout, vid6, 'long_name', 'BCS Pseudo Sea-ice concentration'
NCDF_ATTPUT, netidout, vid6, 'units', 'fraction'
NCDF_ATTPUT, netidout, vid6, 'missing_value', -999.

vid7 = NCDF_VARDEF(netidout, 'SST_cpl', [nlonid,nlatid,ntid], /FLOAT)
NCDF_ATTPUT, netidout, vid7, 'long_name', 'sea surface temperature'
NCDF_ATTPUT, netidout, vid7, 'units', 'degrees C'
NCDF_ATTPUT, netidout, vid7, 'missing_value', -999.


; make some global variable attributes
NCDF_ATTPUT, netidout, /GLOBAL, 'description','Daily climatological ice coverage based on climatological monthly data'

NCDF_CONTROL, netidout, /ENDEF
NCDF_VARPUT, netidout, vid0, lat
NCDF_VARPUT, netidout, vid1, lon
NCDF_VARPUT, netidout, vid3, time
NCDF_VARPUT, netidout, vid4, date
NCDF_VARPUT, netidout, vid5, datesec
NCDF_VARPUT, netidout, vid6, ice
NCDF_VARPUT, netidout, vid7, sst
NCDF_CLOSE, netidout

END

;; sanity check plots
;ncfile = '/disk2/Data/sp-cam-spec/sst_bc_spccsm_64x128_09.0001-12.0023_monthly.nc'
;netid = NCDF_OPEN(ncfile)
;NCDF_VARGET, netid, 'time', time_inputmon
;NCDF_VARGET, netid, 'SST_cpl', sst_inputmon
;NCDF_CLOSE, netid
;jday_inputmon = JULDAY(9,1,1) + time_inputmon

;ncfile = '/disk2/Data/sp-cam-spec/sst_bc_spccsm_64x128_09.0001-12.0023.nc'
;netid = NCDF_OPEN(ncfile)
;NCDF_VARGET, netid, 'time', time_input5dStan
;NCDF_VARGET, netid, 'SST_cpl', sst_input5dStan
;NCDF_CLOSE, netid
;jday_input5dStan = JULDAY(9,1,1) + time_input5dStan

;ncfile = '/disk2/Data/sp-cam-spec/SPCAMspec_TSPREC_0004-0023.nc'
;netid = NCDF_OPEN(ncfile)
;NCDF_VARGET, netid, 'time', time_output5dStan
;NCDF_VARGET, netid, 'TS', sst_output5dStan
;NCDF_CLOSE, netid
;jday_output5dStan = JULDAY(9,1,1) + time_output5dStan

;;ncfile = '/volumes/Rocstor/Model_output/spcam_monthlySSTs_spec/spcam_monthlySST_spec.00010917-00120112.nc'
;ncfile = '/Users/demott/Downloads/SPCAM_monthlySSTs_spec/TS.daily.SPCAM_monthlySSTs.nc'
;netid = NCDF_OPEN(ncfile)
;NCDF_VARGET, netid, 'time', time_outputMon
;NCDF_VARGET, netid, 'TS', sst_outputMon
;NCDF_CLOSE, netid
;jday_outputMon = JULDAY(9,17,1) + time_outputMon

;ncfile = '/volumes/Rocstor/Model_output/cam3_monthlySSTs_spec/rawdata/TS.daily.CAM3_monthlySSTs.nc'
;netid = NCDF_OPEN(ncfile)
;NCDF_VARGET, netid, 'time', time_outputMonCAM3
;NCDF_VARGET, netid, 'TS', sst_outputMonCAM3
;NCDF_CLOSE, netid
;jday_outputMonCAM3 = JULDAY(9,17,1) + time_outputMonCAM3

;ncfile = '/volumes/Rocstor/Model_output/cam3_5dayRMSSTs_spec/rawdata/TS.daily.CAM3_5dSSTs.nc'
;netid = NCDF_OPEN(ncfile)
;NCDF_VARGET, netid, 'time', time_output5dCAM3
;NCDF_VARGET, netid, 'TS', sst_output5dCAM3
;NCDF_CLOSE, netid
;jday_output5dCAM3 = JULDAY(9,16,1) + time_output5dCAM3

;plot, jday_inputmon, sst_inputmon(32,34,*),charsize=2,xrange=[jday_inputmon(50),jday_inputmon(100)], yrange=[18,30],xtickunits='time'
;oplot, jday_input5dStan, sst_input5dStan(32,34,*),linestyle=3
;oplot, jday_output5dStan, sst_output5dStan(32,34,*)-278,linestyle=3
;oplot, jday_outputMon, sst_outputMon(32,34,*)-278,linestyle=0
;oplot, jday_outputMonCAM3, sst_outputMonCAM3(32,34,*)-280,linestyle=0
;oplot, jday_output5dCAM3, sst_output5dCAM3(32,34,*)-280,linestyle=3
;xyouts, 1100, 28, 'Input SSTs',charsize=2
;xyouts, 1100, 23, 'SPCAM TS',charsize=2
;xyouts, 1100, 20, 'CAM3 TS',charsize=2






