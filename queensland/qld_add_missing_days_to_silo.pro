PRO qld_add_missing_days_to_silo

infile='/home/ss901165/datasets_mango/SILO/update/SILO.jan-dec_dmeans.2011.precip.nc'

input_var=OPEN_AND_EXTRACT(infile,'rain')
n_lon=N_ELEMENTS(input_var(*,0,0))
n_lat=N_ELEMENTS(input_var(0,*,0))
n_time_in=N_ELEMENTS(input_var(0,0,*))
n_time_out=365
output_var=fltarr(n_lon,n_lat,n_time_out)

output_var(*,*,0:n_time_in-1)=input_var
output_var(*,*,n_time_in:n_time_out-1)=-99.

id=NCDF_OPEN(infile,/WRITE)
varid=NCDF_VARID(id,'rain')
NCDF_VARPUT,id,varid,output_var
NCDF_CLOSE,id

STOP
END
