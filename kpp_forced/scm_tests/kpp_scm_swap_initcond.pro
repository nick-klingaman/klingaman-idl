PRO kpp_scm_swap_initcond

  ; Swap initial conditions between two gridpoints

  basedir='/export/mango/data-10/ss901165/kpp_ocean/ancillaries/initial_conditions'
  runid='xgspj'

  pt1=[0,60]
  pt1_str='0N60E'
  pt2=[-2.5,60]
  pt2_str='2.5S60E'

  fields=['sal','u','v','temp']
  n_fields=N_ELEMENTS(fields)
     
  infile=basedir+'/kpp_jan15_initcond_n96_ukmotemp_ukmosal_amip.nc'
  outfile=basedir+'/kpp_jan15_initcond_n96_ukmotemp_ukmosal_amip.swap_'+pt1_str+'_'+pt2_str+'.nc'

  longitude=OPEN_AND_EXTRACT(infile,'longitude')
  pt1_lon=NEAREST(longitude,pt1(1))
  pt2_lon=NEAREST(longitude,pt2(1))
  latitude=OPEN_AND_EXTRACT(infile,'latitude')
  pt1_lat=NEAREST(latitude,pt1(0))
  pt2_lat=NEAREST(latitude,pt2(0))
  zsal=OPEN_AND_EXTRACT(infile,'zsal')
  ztemp=OPEN_AND_EXTRACT(infile,'ztemp')
  zvel=OPEN_AND_EXTRACT(infile,'zvel')
  
  n_lon=N_ELEMENTS(longitude)
  n_lat=N_ELEMENTS(latitude)
  n_zsal=N_ELEMENTS(zsal)
  n_ztemp=N_ELEMENTS(ztemp)
  n_zvel=N_ELEMENTS(zvel)

  id=NCDF_CREATE(outfile,/CLOBBER)
  dimids=intarr(5)
  varids=intarr(5+n_fields)
  dimids(0)=NCDF_DIMDEF(id,'longitude',n_lon)
  dimids(1)=NCDF_DIMDEF(id,'latitude',n_lat)
  dimids(2)=NCDF_DIMDEF(id,'zsal',n_zsal)
  dimids(3)=NCDF_DIMDEF(id,'zvel',n_zvel)
  dimids(4)=NCDF_DIMDEF(id,'ztemp',n_ztemp)
  varids(0)=NCDF_VARDEF(id,'longitude',[dimids(0)])
  varids(1)=NCDF_VARDEF(id,'latitude',[dimids(1)])
  varids(2)=NCDF_VARDEF(id,'zsal',[dimids(2)])
  varids(3)=NCDF_VARDEF(id,'zvel',[dimids(3)])
  varids(4)=NCDF_VARDEF(id,'ztemp',[dimids(4)])
  varids(5)=NCDF_VARDEF(id,'sal',[dimids(0),dimids(1),dimids(2)])
  varids(6)=NCDF_VARDEF(id,'u',[dimids(0),dimids(1),dimids(3)])
  varids(7)=NCDF_VARDEF(id,'v',[dimids(0),dimids(1),dimids(3)])
  varids(8)=NCDF_VARDEF(id,'temp',[dimids(0),dimids(1),dimids(4)])
  
  NCDF_CONTROL,id,/ENDEF
  NCDF_VARPUT,id,varids(0),longitude
  NCDF_VARPUT,id,varids(1),latitude
  NCDF_VARPUT,id,varids(2),zsal
  NCDF_VARPUT,id,varids(3),zvel
  NCDF_VARPUT,id,varids(4),ztemp
     
  FOR j=0,n_fields-1 DO BEGIN
     print,'Now processing '+fields(j)+' ...'
     flux_in=OPEN_AND_EXTRACT(infile,fields(j))
     nz=N_ELEMENTS(flux_in(0,0,*))

     flux_out=fltarr(n_lon,n_lat,nz)
     flux_out=flux_in
     temp=flux_out(pt1_lon,pt1_lat,*)
     flux_out(pt1_lon,pt1_lat,*)=flux_out(pt2_lon,pt2_lat,*)
     flux_out(pt2_lon,pt2_lat,*)=temp
     
     NCDF_VARPUT,id,varids(5+j),flux_out
  ENDFOR

  STOP
END



