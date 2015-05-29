PRO two_netcdfs_to_one

; Merge two netCDF files into one file

infile1 = '/net/hermes/export/hermes/data-03/ss901165/usurf_era40.nc'
file1_varname='usurf'
infile2 = '/net/hermes/export/hermes/data-03/ss901165/usurf_ecmwfop.nc'
file2_varname='usurf'

outfile = '/net/hermes/export/hermes/data-03/ss901165/usurf.nc'
outfile_varname='usurf'

file1_varids = intarr(4)
file2_varids = intarr(2)
outfile_dimids = intarr(3)
outfile_varids = intarr(4)

file1_id = NCDF_OPEN(infile1)
file1_varids(0) = NCDF_VARID(file1_id,'longitude')
file1_varids(1) = NCDF_VARID(file1_id,'latitude')
file1_varids(2) = NCDF_VARID(file1_id,'time')
file1_varids(3) = NCDF_VARID(file1_id,file1_varname)
NCDF_VARGET,file1_id,file1_varids(0),longitude
NCDF_VARGET,file1_id,file1_varids(1),latitude
NCDF_VARGET,file1_id,file1_varids(2),file1_time

file1_nlon = n_elements(longitude)
file1_nlat = n_elements(latitude)
file1_ntime = n_elements(file1_time)

file2_id = NCDF_OPEN(infile2)
file2_varids(0) = NCDF_VARID(file2_id,'time')
file2_varids(1) = NCDF_VARID(file2_id,file2_varname)
NCDF_VARGET,file2_id,file2_varids(0),file2_time

file2_ntime = n_elements(file2_time)
ntime = file1_ntime + file2_ntime

outfile_id = NCDF_CREATE(outfile,/CLOBBER)
outfile_dimids(0) = NCDF_DIMDEF(outfile_id,'longitude',file1_nlon)
outfile_dimids(1) = NCDF_DIMDEF(outfile_id,'latitude',file1_nlat)
outfile_dimids(2) = NCDF_DIMDEF(outfile_id,'time',ntime)
outfile_varids(0) = NCDF_VARDEF(outfile_id,'longitude',[outfile_dimids(0)])
outfile_varids(1) = NCDF_VARDEF(outfile_id,'latitude',[outfile_dimids(1)])
outfile_varids(2) = NCDF_VARDEF(outfile_id,'time',[outfile_dimids(2)])
outfile_varids(3) = NCDF_VARDEF(outfile_id,outfile_varname,$
                                [outfile_dimids(0),outfile_dimids(1),$
                                 outfile_dimids(2)])
NCDF_CONTROL,outfile_id,/endef
NCDF_VARPUT,outfile_id,outfile_varids(0), longitude
NCDF_VARPUT,outfile_id,outfile_varids(1), latitude
NCDF_VARPUT,outfile_id,outfile_varids(2), indgen(ntime)

output_var = fltarr(1,1,ntime)

FOR i=0,file1_nlon-1 DO BEGIN
    FOR j=0,file1_nlat-1 DO BEGIN
        print,'Now running for i= '+strtrim(string(i),1)+' and j= '+strtrim(string(j),1)
        NCDF_VARGET,file1_id,file1_varids(3),file1_var,offset=[i,j,0],$
          count=[1,1,file1_ntime]
        output_var(0,0,0:file1_ntime-1) = file1_var
        NCDF_VARGET,file2_id,file2_varids(1),file2_var,offset=[i,j,0],$
          count=[1,1,file2_ntime]
        output_var(0,0,file1_ntime:ntime-1) = file2_var
        NCDF_VARPUT,outfile_id,outfile_varids(3),output_var,offset=[i,j,0]
    ENDFOR
ENDFOR

NCDF_CLOSE,file1_id
NCDF_CLOSE,file2_id
NCDF_CLOSE,outfile_id

END

        
