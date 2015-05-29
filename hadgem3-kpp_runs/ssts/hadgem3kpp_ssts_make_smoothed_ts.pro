PRO hadgem3kpp_ssts_make_smoothed_ts

smooth_length=15

dir='/export/niagara/data-02/ss901165/xihvd'
infile=dir+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans.years1-34.sst.nc'
outfile=dir+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans_ts_smooth'+STRTRIM(STRING(smooth_length),1)+'.years1-34.sst.nc'

longitude=OPEN_AND_EXTRACT(infile,'longitude')
latitude=OPEN_AND_EXTRACT(infile,'latitude')
day=OPEN_AND_EXTRACT(infile,'t_1')
year=OPEN_AND_EXTRACT(infile,'year')
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)
n_day=N_ELEMENTS(day)
n_year=N_ELEMENTS(year)

sst_out=fltarr(n_lon,n_lat,n_day*n_year)
sst_in=OPEN_AND_EXTRACT(infile,'temp_2')

FOR i=0,n_lon-1 DO BEGIN
   FOR j=0,n_lat-1 DO BEGIN
      temp_ts=fltarr(2*n_day*n_year)
      FOR k=0,n_year-1 DO $
         temp_ts(k*n_day:(k+1)*n_day-1)=sst_in(i,j,*,k)
      temp_ts(n_day*n_year:2*n_day*n_year-1)=temp_ts(0:n_day*n_year-1)
      temp_ts=SMOOTH(temp_ts,smooth_length)
      sst_out(i,j,*)=[temp_ts(n_day*n_year:n_day*n_year+smooth_length-1),temp_ts(smooth_length:n_day*n_year-1)]
   ENDFOR
ENDFOR

id=NCDF_CREATE(outfile,/CLOBBER)
dimids=intarr(3)
varids=intarr(4)
dimids(0)=NCDF_DIMDEF(id,'longitude',n_lon)
dimids(1)=NCDF_DIMDEF(id,'latitude',n_lat)
dimids(2)=NCDF_DIMDEF(id,'t',n_day*n_year)
varids(0)=NCDF_VARDEF(id,'longitude',dimids(0))
varids(1)=NCDF_VARDEF(id,'latitude',dimids(1))
varids(2)=NCDF_VARDEF(id,'t',dimids(2))
varids(3)=NCDF_VARDEF(id,'sst',[dimids(0),dimids(1),dimids(2)])
NCDF_ATTPUT,id,varids(2),'units','days since 1982-01-01 00:00:00'
NCDF_ATTPUT,id,varids(2),'calendar','360_day'
NCDF_ATTPUT,id,varids(2),'time_origin','01-JAN-1982:00:00:00'
NCDF_CONTROL,id,/ENDEF
NCDF_VARPUT,id,varids(0),longitude
NCDF_VARPUT,id,varids(1),latitude
NCDF_VARPUT,id,varids(2),findgen(n_day*n_year)+0.5
NCDF_VARPUT,id,varids(3),sst_out
NCDF_CLOSE,id

STOP
END

