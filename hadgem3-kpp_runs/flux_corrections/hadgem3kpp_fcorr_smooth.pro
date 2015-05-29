PRO hadgem3kpp_fcorr_smooth

runs=['xgspr_i3']
n_runs=N_ELEMENTS(runs)
type='flxcorr_1mtop_3hr_ga30cpl_sal60_amip_vn78_smooth_12421_ind'
date_range='jan-dec_mmeans'

FOR i=0,n_runs-1 DO BEGIN
   infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/heat_corrections/'+runs(i)+'_'+type+'.'+date_range+'.flxcorr.n96.nc'
   outfile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/heat_corrections/'+runs(i)+'_'+type+'.'+date_range+'.flxcorr_smooth.n96.nc'
   
   fcorr_in=OPEN_AND_EXTRACT(infile,'fcorr')
   time_in=OPEN_AND_EXTRACT(infile,'t')
   n_time=N_ELEMENTS(time_in)
   longitude_in=OPEN_AND_EXTRACT(infile,'longitude')
   latitude_in=OPEN_AND_EXTRACT(infile,'latitude')
   z_in=OPEN_AND_EXTRACT(infile,'z')
   n_lon=N_ELEMENTS(longitude_in)
   n_lat=N_ELEMENTS(latitude_in)
   n_z=N_ELEMENTS(z_in)

   temp=fltarr(n_time+2)
   fcorr_out=fltarr(n_lon,n_lat,n_z,n_time)
   FOR j=0,n_lon-1 DO BEGIN
      FOR k=0,n_lat-1 DO BEGIN
         FOR m=0,n_z-1 DO BEGIN
            temp=[REFORM(fcorr_in(j,k,m,n_time-1)),REFORM(fcorr_in(j,k,m,*)),$
                  REFORM(fcorr_in(j,k,m,0))]
            temp2=SMOOTH(temp,3)
            fcorr_out(j,k,m,*)=REFORM(temp2(1:n_time))
         ENDFOR
      ENDFOR
   ENDFOR

   id=NCDF_CREATE(outfile,/CLOBBER)
   dimids=intarr(4)
   varids=intarr(5)
   dimids(0)=NCDF_DIMDEF(id,'longitude',n_lon)
   dimids(1)=NCDF_DIMDEF(id,'latitude',n_lat)
   dimids(2)=NCDF_DIMDEF(id,'z',n_z)
   dimids(3)=NCDF_DIMDEF(id,'t',n_time)
   varids(0)=NCDF_VARDEF(id,'longitude',dimids(0))
   varids(1)=NCDF_VARDEF(id,'latitude',dimids(1))
   varids(2)=NCDF_VARDEF(id,'z',dimids(2))
   varids(3)=NCDF_VARDEF(id,'t',dimids(3))
   varids(4)=NCDF_VARDEF(id,'fcorr',[dimids(0),dimids(1),dimids(2),dimids(3)])

   NCDF_ATTPUT,id,varids(4),'missing_value',2e20

   NCDF_CONTROL,id,/ENDEF

   NCDF_VARPUT,id,varids(0),longitude_in
   NCDF_VARPUT,id,varids(1),latitude_in
   NCDF_VARPUT,id,varids(2),z_in
   NCDF_VARPUT,id,varids(3),time_in
   NCDF_VARPUT,id,varids(4),fcorr_out
   
   NCDF_CLOSE,id

ENDFOR

STOP
END
