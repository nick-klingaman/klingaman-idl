PRO project_onto_eof,infile_project,varname_project,timename_project,box,infile_eof,varname_eof,outfile_project,varname_out,multiplier_project=multiplier_project,infile_norm=infile_norm,varname_norm=varname_norm,infile_clim=infile_clim,varname_clim=varname_clim,ndays_per_year=ndays_per_year,start_date=start_date

; Read coordinate dimensions from EOF file
eof_longitude=OPEN_AND_EXTRACT(infile_eof,'longitude')
eof_latitude=OPEN_AND_EXTRACT(infile_eof,'latitude')
DEFINE_BOUNDARIES,box,eof_latitude,eof_longitude,eof_box_tx,/LIMIT
eof_nlon=N_ELEMENTS(eof_longitude)
eof_nlat=N_ELEMENTS(eof_latitude)

; Read coordinate dimensions from file of data to project
project_longitude=OPEN_AND_EXTRACT(infile_project,'longitude')
project_latitude=OPEN_AND_EXTRACT(infile_project,'latitude')
DEFINE_BOUNDARIES,box,project_latitude,project_longitude,project_box_tx,/LIMIT
project_nlon=N_ELEMENTS(project_longitude)
project_nlat=N_ELEMENTS(project_latitude)

IF (project_nlon ne eof_nlon) or (project_nlat ne eof_nlat) THEN BEGIN
   print,'Resolutions of input files are not compatiable.'
   STOP
ENDIF

IF KEYWORD_SET(multiplier_project) THEN BEGIN
   our_multiplier_project=multiplier_project
ENDIF ELSE $
   our_multiplier_project=1

IF KEYWORD_SET(start_date) THEN BEGIN
   our_start_date=start_date
ENDIF ELSE $
   our_start_date=0

IF KEYWORD_SET(infile_clim) THEN BEGIN
   clim_longitude=OPEN_AND_EXTRACT(infile_clim,'longitude')
   clim_latitude=OPEN_AND_EXTRACT(infile_clim,'latitude')
   DEFINE_BOUNDARIES,box,clim_latitude,clim_longitude,clim_box_tx,/LIMIT
   clim_nlon=N_ELEMENTS(clim_longitude)
   clim_nlat=N_ELEMENTS(clim_latitude)

   clim_data=OPEN_AND_EXTRACT(infile_clim,varname_clim,$
                              offset=[clim_box_tx(1),clim_box_tx(0),0],$
                              count=[clim_nlon,clim_nlat,ndays_per_year],/WRAP)*our_multiplier_project
   FOR i=0,clim_nlon-1 DO BEGIN
      FOR j=0,clim_nlat-1 DO BEGIN
         temp=[REFORM(clim_data(i,j,*)),REFORM(clim_data(i,j,*)),REFORM(clim_data(i,j,*))]
         temp=SMOOTH(temp,31)
         clim_data(i,j,*)=temp(ndays_per_year:2*ndays_per_year-1)
      ENDFOR
   ENDFOR

ENDIF
   
; Read time coordinate
time=OPEN_AND_EXTRACT(infile_project,timename_project)
ntime=N_ELEMENTS(time)

; Read data to project
project_data=OPEN_AND_EXTRACT(infile_project,varname_project,$
                              offset=[project_box_tx(1),project_box_tx(0),0],$
                              count=[project_nlon,project_nlat,ntime],/WRAP)*our_multiplier_project

; Read EOF pattern
eof_data=OPEN_AND_EXTRACT(infile_eof,varname_eof,$
                          offset=[eof_box_tx(1),eof_box_tx(0)],count=[eof_nlon,eof_nlat],/WRAP)
eof_data_oned=REFORM(eof_data,[eof_nlon*eof_nlat])

out_data=fltarr(ntime)
FOR i=0,ntime-1 DO BEGIN
   today=REFORM(project_data(*,*,i),[project_nlon*project_nlat])
   IF KEYWORD_SET(infile_clim) THEN BEGIN
      today_clim=REFORM(clim_data(*,*,((i+our_start_date) MOD ndays_per_year)),[project_nlon*project_nlat])
      today=today-today_clim
   ENDIF
   ;out_data(i)=TOTAL(today/eof_data_oned)/N_ELEMENTS(eof_data_oned) 
   out_data(i)=REGRESS(today,eof_data_oned)
ENDFOR

IF KEYWORD_SET(infile_norm) THEN BEGIN
   norm_data=OPEN_AND_EXTRACT(infile_norm,varname_norm)
   out_data=(out_data-MEAN(norm_data))/STDDEV(norm_data)
ENDIF

;out_data=out_data-MEAN(out_data)
;out_data=out_data/STDDEV(out_data)

id=NCDF_CREATE(outfile_project,/CLOBBER)
dimid=NCDF_DIMDEF(id,'time',ntime)
varids=intarr(2)
varids(0)=NCDF_VARDEF(id,'time',[dimid])
varids(1)=NCDF_VARDEF(id,varname_out,[dimid])
NCDF_CONTROL,id,/ENDEF
NCDF_VARPUT,id,varids(0),time
NCDF_VARPUT,id,varids(1),out_data
NCDF_CLOSE,id

END

