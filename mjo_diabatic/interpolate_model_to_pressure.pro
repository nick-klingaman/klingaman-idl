PRO interpolate_model_to_pressure,data_infile,data_invarname,$
                                  pressure_infile,$
                                  pressure_varname,pressure_levels,data_outfile,data_outvarname


  id=NCDF_OPEN(data_infile)
  varid=NCDF_VARID(id,data_invarname)
  varstruct=NCDF_VARINQ(id,varid)
  
  NCDF_DIMINQ,id,varstruct.dim(0),lon_varname,data_nlon
  NCDF_DIMINQ,id,varstruct.dim(1),lat_varname,data_nlat
  NCDF_DIMINQ,id,varstruct.dim(2),z_varname,data_nz
  NCDF_DIMINQ,id,varstruct.dim(3),t_varname,data_nt

  NCDF_CLOSE,id

  id=NCDF_OPEN(pressure_infile)
  varid=NCDF_VARID(id,pressure_varname)
  varstruct=NCDF_VARINQ(id,varid)

  NCDF_DIMINQ,id,varstruct.dim(0),pressure_lon_varname,pressure_nlon
  NCDF_DIMINQ,id,varstruct.dim(1),pressure_lat_varname,pressure_nlat
  NCDF_DIMINQ,id,varstruct.dim(2),pressure_z_varname,pressure_nz  

  NCDF_CLOSE,id

  ; Open input file, extract variable and determine lat/lon ranges
  longitude=OPEN_AND_EXTRACT(data_infile,lon_varname)
  latitude=OPEN_AND_EXTRACT(data_infile,lat_varname)
  height=OPEN_AND_EXTRACT(data_infile,z_varname)
  time=OPEN_AND_EXTRACT(data_infile,t_varname)

  pressure_longitude=OPEN_AND_EXTRACT(pressure_infile,pressure_lon_varname)
  pressure_latitude=OPEN_AND_EXTRACT(pressure_infile,pressure_lat_varname)
  DEFINE_BOUNDARIES,[MIN(latitude),MIN(longitude),MAX(latitude),MAX(longitude)],$
                    pressure_latitude,pressure_longitude,pressure_box_tx,/LIMIT
  pressure_height=OPEN_AND_EXTRACT(pressure_infile,pressure_z_varname)

  IF TOTAL(ABS(pressure_height - height)) ge 0.01 THEN BEGIN
     print,'Pressure height coordinate does not match data height coordinate'
     STOP
  ENDIF

  outid = NCDF_CREATE(data_outfile,/CLOBBER)
  outdimids = intarr(4)
  outdimids(0) = NCDF_DIMDEF(outid,'lon',data_nlon)
  outdimids(1) = NCDF_DIMDEF(outid,'lat',data_nlat)
  outdimids(2) = NCDF_DIMDEF(outid,'plev',N_ELEMENTS(pressure_levels))
  outdimids(3) = NCDF_DIMDEF(outid,'time',data_nt)

  outvarids = intarr(5)
  outvarids(0) = NCDF_VARDEF(outid,'lon',[outdimids(0)])
  outvarids(1) = NCDF_VARDEF(outid,'lat',[outdimids(1)])
  outvarids(2) = NCDF_VARDEF(outid,'plev',[outdimids(2)])
  outvarids(3) = NCDF_VARDEF(outid,'time',[outdimids(3)])
  outvarids(4) = NCDF_VARDEF(outid,data_outvarname,[outdimids(0),outdimids(1),outdimids(2),outdimids(3)])

  NCDF_ATTPUT,outid,outvarids(4),'missing_value',-9999.

  NCDF_CONTROL,outid,/ENDEF

  NCDF_VARPUT,outid,outvarids(0),longitude
  NCDF_VARPUT,outid,outvarids(1),latitude
  NCDF_VARPUT,outid,outvarids(2),pressure_levels
  NCDF_VARPUT,outid,outvarids(3),time

  IF ODD(data_nt) THEN BEGIN
     print,'ERROR: code does not work for time dimensions of ODD() length.'
     STOP
  ENDIF ELSE BEGIN
     FOR i=0,data_nt-1,2 DO BEGIN
	print,'Processing for chunk '+STRTRIM(STRING(i+1),1)+' ...'
        data = OPEN_AND_EXTRACT(data_infile,data_invarname,$
                                offset=[0,0,0,i],count=[data_nlon,data_nlat,data_nz,2])
        pressure = OPEN_AND_EXTRACT(pressure_infile,pressure_varname,$
                                    offset=[pressure_box_tx(1),pressure_box_tx(0),0,i],$
                                    count=[data_nlon,data_nlat,pressure_nz,2])/100.	
        output = NC_PRESSURE_LEVS(data,pressure,pressure_levels,/CONSERVE)
        NCDF_VARPUT,outid,outvarids(4),output,$
		offset=[0,0,0,i],count=[data_nlon,data_nlat,N_ELEMENTS(pressure_levels),2]
     ENDFOR
  ENDELSE

  NCDF_CLOSE,id
END
