PRO spcam_ssts_make_smooth_ts

; Make a smoothed timeseries of SST, combining KPP and obs SST 
; using the coupling-weight file.

kpp_sst_file='/group_workspaces/jasmin2/klingaman/cam/spcam_iceedge_free_rx360/kpp/sst.nc'
obs_sst_file='/home/users/npklingaman/datasets/METO_OCEAN_ANALYSIS/t42/spcam-kpp_sst.jan-dec_dmean_clim.1980-2009.64x128.nc'
cplwght_file='/home/users/npklingaman/datasets/SPCAM-KPP_ANCIL/spcam-kpp_cplwght.iceedge_64x128.nc'
mask_file='/home/users/npklingaman/datasets/SPCAM-KPP_ANCIL/spcam-kpp_lsm_ocndepth.64x128.nc'
obs_ice_file='/home/users/npklingaman/datasets/SPCAM-KPP_ANCIL/spcam-kpp_iceconc.jan-dec_dmeans_clim.64x128.nc'

smooth_length=31
out_sst_file='/home/users/npklingaman/datasets/SPCAM-KPP_ANCIL/spcam_iceedge_free_rx360.jan-dec_dmeans_smooth31.years1-29.sst.nc'

; Get global grid from cplwght file
gbl_longitude=OPEN_AND_EXTRACT(cplwght_file,'longitude')
gbl_latitude=OPEN_AND_EXTRACT(cplwght_file,'latitude')
n_lon=N_ELEMENTS(gbl_longitude)
n_lat=N_ELEMENTS(gbl_latitude)

; Get KPP grid from KPP file
kpp_longitude=OPEN_AND_EXTRACT(kpp_sst_file,'longitude')
kpp_latitude=OPEN_AND_EXTRACT(kpp_sst_file,'latitude')
kpp_nlon=N_ELEMENTS(kpp_longitude)
kpp_nlat=N_ELEMENTS(kpp_latitude)

start=0   ; For reading netCDF file of KPP SSTs
count=10740
start_date=244 ; Start date of KPP SSTs (Julian)
; Get SSTs
kpp_sst=OPEN_AND_EXTRACT(kpp_sst_file,'T',offset=[0,0,start],count=[kpp_nlon,kpp_nlat,count])
n_time=N_ELEMENTS(kpp_sst(0,0,*))
obs_sst=OPEN_AND_EXTRACT(obs_sst_file,'sst')
obs_ice=OPEN_AND_EXTRACT(obs_ice_file,'iceconc')

kpp_latoff=NEAREST(gbl_latitude,kpp_latitude(0))

; Get cplwght
cplwght=OPEN_AND_EXTRACT(cplwght_file,'alpha')
; Get land fraction
landfrac=REFORM(OPEN_AND_EXTRACT(mask_file,'LANDFRAC'))

sst_out=fltarr(n_lon,n_lat,n_time+1)

; Note this code assumes the KPP grid is global in longitude
FOR i=0,n_lon-1 DO BEGIN
   FOR j=0,n_lat-1 DO BEGIN
      IF landfrac(i,j) ne 1 THEN BEGIN
         FOR k=0,n_time-1 DO BEGIN
            date=((start_date+k) MOD 365)
            IF gbl_latitude(j) lt kpp_latitude(0) or gbl_latitude(j) gt kpp_latitude(kpp_nlat-1) THEN BEGIN
               sst_out(i,j,k+1)=obs_sst(i,j,date)
                                ;IF n_time MOD 365 ne 0 THEN $
                                ;   sst_out(i,j,n_time/365*365:n_time-1)=obs_sst(i,j,0:(n_time MOD 365)-1)
            ENDIF ELSE IF kpp_sst(i,j-kpp_latoff,0) eq 1e20 THEN BEGIN
               sst_out(i,j,k+1)=obs_sst(i,j,date)
                                ;IF n_time MOD 365 ne 0 THEN $
                                ;   sst_out(i,j,n_time/365*365:n_time-1)=obs_sst(i,j,0:(n_time MOD 365)-1)
            ENDIF ELSE BEGIN
               sst_out(i,j,k+1)=kpp_sst(i,j-kpp_latoff,k)*cplwght(i,j)+$
                                obs_sst(i,j,date)*(1.-cplwght(i,j))
                                ;IF n_time MOD 365 ne 0 THEN $
                                ;sst_out(i,j,n_time/365*365:n_time-1)=kpp_sst(i,j-kpp_latoff,n_time/365*365:n_time-1)$
                                ;*cplwght(i,j)+obs_sst(i,j,0:(n_time MOD 365)-1)*$
                                ;(1.-cplwght(i,j))
            ENDELSE            
         ENDFOR
         sst_out(i,j,0)=sst_out(i,j,1)
      ENDIF ELSE $
         sst_out(i,j,*)=2e20
      sst_out(i,j,*)=SMOOTH(sst_out(i,j,*),smooth_length)
      FOR k=0,n_time DO BEGIN
         date=((start_date+k-1) MOD 365)
         IF date lt 360 THEN BEGIN
            IF obs_ice(i,j,date) gt 0.01 THEN $
               sst_out(i,j,k)=-1.80
         ENDIF ELSE $
            IF obs_ice(i,j,359) gt 0.01 THEN $
               sst_out(i,j,1)=-1.80
      ENDFOR
;      IF TOTAL(where(obs_ice(i,j,*) gt 0.)) gt 0 and landfrac(i,j) ne 1 THEN BEGIN
;            temp_sst=REFORM(sst_out(i,j,k*365:(k+1)*365-1))
;            temp_ice=REFORM(obs_ice(i,j,*))
            ;temp_sst[where(temp_ice gt 0.0)]=-1.80*temp_ice[where(temp_ice gt 0.0)]+$
                                ;temp_sst[where(temp_ice gt 0.0)]*(1.0-temp_ice[where(temp_ice gt 0.0)])
;            temp_sst[where(temp_ice gt 0.01)]=-1.80
;            sst_out(i,j,k*365:(k+1)*365-1)=temp_sst
;         ENDFOR
;      ENDIF
      temp_sst=REFORM(sst_out(i,j,*))
      IF TOTAL(where(temp_sst lt -1.80)) ge 0 THEN BEGIN
         temp_sst[where(temp_sst lt -1.80)]=-1.80
         sst_out(i,j,*)=temp_sst
      ENDIF
   ENDFOR
ENDFOR

; Repeat ice climatology
ice_out=fltarr(n_lon,n_lat,n_time+1)
FOR i=0,n_time DO BEGIN
   IF ((i+start_date-1) MOD 365) lt 360 THEN BEGIN
      ice_out(*,*,i)=obs_ice(*,*,((i+start_date-1) MOD 365))
   ENDIF ELSE $
      ice_out(*,*,i)=obs_ice(*,*,359)
ENDFOR

; Construct YYYYMMDD
date_out=lonarr(n_time+1)
num_days=[31,28,31,30,31,30,31,31,30,31,30,31]
month=8
date=31
year=0
FOR i=0,n_time DO BEGIN
   date_out(i)=LONG(year)*LONG(10000)+LONG(month)*LONG(100)+LONG(date)
   ;print,date_out(i)
   date=date+1
   IF (date gt num_days(month-1)) THEN BEGIN
      month=month+1
      date=1
      IF month gt 12 THEN BEGIN
         month=1
         year=year+1
      ENDIF
   ENDIF
ENDFOR

sst_out[where(sst_out ge 1e10)]=-999.

id=NCDF_CREATE(out_sst_file,/CLOBBER)

dimids=intarr(3)
dimids(0)=NCDF_DIMDEF(id,'lon',n_lon)
dimids(1)=NCDF_DIMDEF(id,'lat',n_lat)
dimids(2)=NCDF_DIMDEF(id,'time',/UNLIMITED)

varids=intarr(7)
varids(0)=NCDF_VARDEF(id,'lon',[dimids(0)],/FLOAT)
NCDF_ATTPUT,id,varids(0),'units','degrees east'
NCDF_ATTPUT,id,varids(0),'long_name','longitude'
varids(1)=NCDF_VARDEF(id,'lat',[dimids(1)],/FLOAT)
NCDF_ATTPUT,id,varids(1),'units','degrees north'
NCDF_ATTPUT,id,varids(1),'long_name','latitude'
varids(2)=NCDF_VARDEF(id,'time',[dimids(2)],/FLOAT)
NCDF_ATTPUT,id,varids(2),'units','days since 0000-08-31 00:00:00'
NCDF_ATTPUT,id,varids(2),'long_name','days'
varids(3)=NCDF_VARDEF(id,'date',[dimids(2)],/LONG)
NCDF_ATTPUT,id,varids(3),'long_name','current date as 8 digit integer (YYYYMMDD)'
varids(4)=NCDF_VARDEF(id,'datesec',[dimids(2)],/LONG)
NCDF_ATTPUT,id,varids(4),'long_name','current seconds of current date'
varids(5)=NCDF_VARDEF(id,'SST_cpl',[dimids(0),dimids(1),dimids(2)],/FLOAT)
NCDF_ATTPUT,id,varids(5),'long_name','sea surface temperature'
NCDF_ATTPUT,id,varids(5),'units','degrees C'
NCDF_ATTPUT,id,varids(5),'missing',-999.
varids(6)=NCDF_VARDEF(id,'ice_cov',[dimids(0),dimids(1),dimids(2)],/FLOAT)
NCDF_ATTPUT,id,varids(6),'long_name','BCS Pseudo Sea-ice concentration'
NCDF_ATTPUT,id,varids(6),'units','fraction'
NCDF_ATTPUT,id,varids(6),'missing',-999.

NCDF_CONTROL,id,/ENDEF

NCDF_VARPUT,id,varids(0),gbl_longitude
NCDF_VARPUT,id,varids(1),gbl_latitude
NCDF_VARPUT,id,varids(2),findgen(n_time+1)
NCDF_VARPUT,id,varids(3),date_out
NCDF_VARPUT,id,varids(4),REPLICATE(0,n_time+1)
NCDF_VARPUT,id,varids(5),sst_out
NCDF_VARPUT,id,varids(6),ice_out

NCDF_CLOSE,id

STOP
END
