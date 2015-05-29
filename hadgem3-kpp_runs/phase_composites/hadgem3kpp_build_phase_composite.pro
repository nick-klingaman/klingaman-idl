PRO hadgem3kpp_build_phase_composite

run_name='1.0xentrain'
year_range='1-14'
basedir='/home/ss901165/um_output4/hadgem3a_amip2_'+run_name+'_rerun_vn74'
rmm_index_file=basedir+'/hadgem3a_amip2_'+run_name+'_rerun_vn74.mar-feb_6hrmeans.'+year_range+'.rmm_indices.nc'

input_vars=['Tinc_lwrad','Qinc_conv','Qinc_lsrain','Qinc_bdylr']
ncvarnames=['unspecified_7','unspecified_9','unspecified_1','unspecified_5']
input_vars=['precip']
ncvarnames=['precip']
dims=3

box=[-30,0,30,360]

n_variables=N_ELEMENTS(input_vars)
n_years=14
n_times_per_year=1440

; Set to one the first time phase-composite files are created, or to overwrite existing files
create_files=0
create_variables=[REPLICATE(1,9)]

n_phases=9

mjo_phase=REFORM(OPEN_AND_EXTRACT(rmm_index_file,'phase',$
                                  offset=[0,0],count=[n_years,n_times_per_year]))
mjo_phase_allyears=fltarr(n_times_per_year*n_years)

mjo_amp=REFORM(OPEN_AND_EXTRACT(rmm_index_file,'amplitude',$
                                offset=[0,0],count=[n_years,n_times_per_year]))
mjo_amp_allyears=fltarr(n_times_per_year*n_years)
FOR i=0,n_years-1 DO BEGIN
   mjo_amp_allyears(i*n_times_per_year:(i+1)*n_times_per_year-1)=mjo_amp(i,*)
   mjo_phase_allyears(i*n_times_per_year:(i+1)*n_times_per_year-1)=mjo_phase(i,*)
ENDFOR

FOR i=0,n_variables-1 DO BEGIN
   input_file=basedir+'/hadgem3a_amip2_'+run_name+'_rerun_vn74.mar-feb_6hrmeans.years'+year_range+'.'+input_vars(i)+'.nc'
   print,'Now reading from file '+input_file+' ...'
 
   longitude=OPEN_AND_EXTRACT(input_file,'longitude')
   latitude=OPEN_AND_EXTRACT(input_file,'latitude')
   DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
   n_lon=N_ELEMENTS(longitude)
   n_lat=N_ELEMENTS(latitude)
   IF dims eq 4 THEN BEGIN
      z=OPEN_AND_EXTRACT(input_file,'hybrid_ht')
      n_z=N_ELEMENTS(z)
      phase_composite=fltarr(n_phases,n_lon,n_lat,n_z)
      phase_composite_scaleamp=fltarr(n_phases,n_lon,n_lat,n_z)
      phase_composite_stddev=fltarr(n_phases,n_lon,n_lat,n_z)
      phase_ntimes=fltarr(n_phases,n_lon,n_lat,n_z)
      variable=REFORM(OPEN_AND_EXTRACT(input_file,ncvarnames(i),$
                                    offset=[box_tx(1),box_tx(0),0,0,0],$
                                    count=[n_lon,n_lat,n_z,n_times_per_year,n_years]),$
                      [n_lon,n_lat,n_z,n_years*n_times_per_year])
   ENDIF ELSE BEGIN
      phase_composite=fltarr(n_phases,n_lon,n_lat)
      phase_composite_scaleamp=fltarr(n_phases,n_lon,n_lat)
      phase_composite_stddev=fltarr(n_phases,n_lon,n_lat)
      phase_ntimes=fltarr(n_phases,n_lon,n_lat)
      variable=REFORM(OPEN_AND_EXTRACT(input_file,ncvarnames(i),$
                                       offset=[box_tx(1),box_tx(0),0,0],$
                                       count=[n_lon,n_lat,n_times_per_year,n_years]),$
                      [n_lon,n_lat,n_years*n_times_per_year])
   ENDELSE
   phase_sumamp=fltarr(n_phases)
   print,'... done'
   
   FOR j=1,8 DO BEGIN
      print,'Now computing composite for phase '+STRTRIM(STRING(j),1)+' ...'
      print,'    ---> Finding days ...'
      thisphase_days=where(mjo_phase_allyears eq j and mjo_amp_allyears ge 1)
      IF N_ELEMENTS(thisphase_days) gt 1 THEN BEGIN
         phase_ntimes(j)=N_ELEMENTS(thisphase_days)
         phase_sumamp(j)=TOTAL(mjo_amp[thisphase_days])
         IF dims eq 4 THEN BEGIN
            FOR k=0,n_z-1 DO BEGIN
               print,'    ---> Computing for vertical level '+STRTRIM(STRING(k+1),1)+' ...'
               FOR m=0,n_lon-1 DO BEGIN
                  FOR n=0,n_lat-1 DO BEGIN
                     temp_variable=REFORM(variable(m,n,k,*))
                     phase_composite(j,m,n,k)=MEAN(temp_variable[thisphase_days])
                     phase_composite_stddev(j,m,n,k)=STDDEV(temp_variable[thisphase_days])
                     phase_composite_scaleamp(j,m,n,k)=TOTAL(temp_variable[thisphase_days])/phase_sumamp(j)
                  ENDFOR
               ENDFOR               
            ENDFOR
         ENDIF ELSE BEGIN
            FOR m=0,n_lon-1 DO BEGIN
               FOR n=0,n_lat-1 DO BEGIN
                  temp_variable=REFORM(variable(m,n,*))
                  phase_composite(j,m,n)=MEAN(temp_variable[thisphase_days])
                  phase_composite_stddev(j,m,n)=STDDEV(temp_variable[thisphase_days])
                  phase_composite_scaleamp(j,m,n)=TOTAL(temp_variable[thisphase_days])/phase_sumamp(j)
               ENDFOR
            ENDFOR     
         ENDELSE
      ENDIF
   ENDFOR
   
   print,'Now computing composite for phase 0'
   print,'    ---> Finding days ...'
   zerophase_days=where(mjo_amp_allyears lt 1)
   phase_ntimes(0)=N_ELEMENTS(zerophase_days)
   phase_sumamp(0)=TOTAL(mjo_amp[zerophase_days])
   IF dims eq 4 THEN BEGIN
      FOR k=0,n_z-1 DO BEGIN
         print,'    ---> Computing for vertical level '+STRTRIM(STRING(k+1),1)+' ...'
         FOR m=0,n_lon-1 DO BEGIN
            FOR n=0,n_lat-1 DO BEGIN
               temp_variable=REFORM(variable(m,n,k,*))
               phase_composite(0,m,n,k)=MEAN(temp_variable[zerophase_days])
               phase_composite_stddev(0,m,n,k)=STDDEV(temp_variable[zerophase_days])
               phase_composite_scaleamp(0,m,n,k)=TOTAL(temp_variable[zerophase_days])/phase_sumamp(0)
            ENDFOR
         ENDFOR
      ENDFOR
   ENDIF ELSE BEGIN
      FOR m=0,n_lon-1 DO BEGIN
         FOR n=0,n_lat-1 DO BEGIN
            temp_variable=REFORM(variable(m,n,*))
            phase_composite(0,m,n)=MEAN(temp_variable[zerophase_days])
            phase_composite_stddev(0,m,n)=STDDEV(temp_variable[zerophase_days])
            phase_composite_scaleamp(0,m,n)=TOTAL(temp_variable[zerophase_days])/phase_sumamp(0)
         ENDFOR
      ENDFOR   
   ENDELSE

   print,'Writing to output files ...'
   FOR j=0,8 DO BEGIN
      output_netcdf_file=basedir+'/hadgem3a_amip2_'+run_name+'_rerun_vn74.years'+year_range+'.composite_mjo_phase'+STRTRIM(STRING(j),1)+'.nc'
      IF create_files eq 1 THEN BEGIN
         id=NCDF_CREATE(output_netcdf_file,/CLOBBER)
         dimids=intarr(3)
         varids=intarr(6)
         dimids(0)=NCDF_DIMDEF(id,'longitude',n_lon)
         dimids(1)=NCDF_DIMDEF(id,'latitude',n_lat)
         dimids(2)=NCDF_DIMDEF(id,'z',n_z)
         varids(0)=NCDF_VARDEF(id,'longitude',[dimids(0)])
         varids(1)=NCDF_VARDEF(id,'latitude',[dimids(1)])
         varids(2)=NCDF_VARDEF(id,'z',[dimids(2)])
         varids(3)=NCDF_VARDEF(id,input_vars(i)+'_composite',[dimids(0),dimids(1),dimids(2)])
         varids(4)=NCDF_VARDEF(id,input_vars(i)+'_stddev',[dimids(0),dimids(1),dimids(2)])
         varids(5)=NCDF_VARDEF(id,input_vars(i)+'_scaleamp',[dimids(0),dimids(1),dimids(2)])
         NCDF_CONTROL,id,/ENDEF
         NCDF_VARPUT,id,varids(0),longitude
         NCDF_VARPUT,id,varids(1),latitude
         NCDF_VARPUT,id,varids(2),z
         NCDF_VARPUT,id,varids(3),REFORM(phase_composite(j,*,*,*))
         NCDF_VARPUT,id,varids(4),REFORM(phase_composite_stddev(j,*,*,*))
         NCDF_VARPUT,id,varids(5),REFORM(phase_composite_scaleamp(j,*,*,*))
         NCDF_CLOSE,id
      ENDIF ELSE BEGIN
         id=NCDF_OPEN(output_netcdf_file,/WRITE)
         dimids=intarr(3)
         dimids(0)=NCDF_DIMID(id,'longitude')
         dimids(1)=NCDF_DIMID(id,'latitude')
         IF dims eq 4 THEN BEGIN
            dimids(2)=NCDF_DIMID(id,'z')
            varids=intarr(3)
            IF create_variables(j) eq 1 THEN BEGIN
               NCDF_CONTROL,id,/REDEF
               varids(0)=NCDF_VARDEF(id,input_vars(i)+'_composite',[dimids(0),dimids(1),dimids(2)])
               varids(1)=NCDF_VARDEF(id,input_vars(i)+'_stddev',[dimids(0),dimids(1),dimids(2)])
               varids(2)=NCDF_VARDEF(id,input_vars(i)+'_scaleamp',[dimids(0),dimids(1),dimids(2)])
               NCDF_CONTROL,id,/ENDEF
            ENDIF ELSE BEGIN
               varids(0)=NCDF_VARID(id,input_vars(i)+'_composite')
               varids(1)=NCDF_VARID(id,input_vars(i)+'_stddev')
               varids(2)=NCDF_VARID(id,input_vars(i)+'_scaleamp')
            ENDELSE
            NCDF_VARPUT,id,varids(0),REFORM(phase_composite(j,*,*,*))
            NCDF_VARPUT,id,varids(1),REFORM(phase_composite_stddev(j,*,*,*))
            NCDF_VARPUT,id,varids(2),REFORM(phase_composite_scaleamp(j,*,*,*))
         ENDIF ELSE BEGIN
            varids=intarr(3)
            IF create_variables(j) eq 1 THEN BEGIN
               NCDF_CONTROL,id,/REDEF
               varids(0)=NCDF_VARDEF(id,input_vars(i)+'_composite',[dimids(0),dimids(1)])
               varids(1)=NCDF_VARDEF(id,input_vars(i)+'_stddev',[dimids(0),dimids(1)])
               varids(2)=NCDF_VARDEF(id,input_vars(i)+'_scaleamp',[dimids(0),dimids(1)])
               NCDF_CONTROL,id,/ENDEF
            ENDIF ELSE BEGIN
               varids(0)=NCDF_VARID(id,input_vars(i)+'_composite')
               varids(1)=NCDF_VARID(id,input_vars(i)+'_stddev')
               varids(2)=NCDF_VARID(id,input_vars(i)+'_scaleamp')
            ENDELSE
            NCDF_VARPUT,id,varids(0),REFORM(phase_composite(j,*,*))
            NCDF_VARPUT,id,varids(1),REFORM(phase_composite_stddev(j,*,*))
            NCDF_VARPUT,id,varids(2),REFORM(phase_composite_scaleamp(j,*,*))
            NCDF_CLOSE,id
         ENDELSE
      ENDELSE
   ENDFOR   
ENDFOR

STOP
END

