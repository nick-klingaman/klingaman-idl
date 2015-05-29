PRO kpp_scm_swap_fluxes

  ; Swap fluxes between two gridpoints

  basedir='/export/mango/data-10/ss901165/kpp_ocean/ancillaries/flux_data'
  runid='xgspj'
  years=['i2','i3','i4','i5']
  n_years=N_ELEMENTS(years)

  pt1=[-2.5,60]
  pt1_str='2.5S60E'
  pt2=[-5.0,60]
  pt2_str='5.0S60E'

  fluxes=['swf','lwf','lhf','shf','taux','tauy','precip']
  n_fluxes=N_ELEMENTS(fluxes)

  FOR i=0,n_years-1 DO BEGIN
     
     infile=basedir+'/'+runid+'.fluxes_for_kpp.jan-dec_dmeans.'+years(i)+'.3hr_n96_tgrid.nc'
     outfile=basedir+'/'+runid+'.fluxes_for_kpp.jan-dec_dmeans.'+years(i)+'.3hr_n96_tgrid.swap_'+pt1_str+'_'+pt2_str+'.nc'

     longitude=OPEN_AND_EXTRACT(infile,'longitude')
     pt1_lon=NEAREST(longitude,pt1(1))
     pt2_lon=NEAREST(longitude,pt2(1))
     latitude=OPEN_AND_EXTRACT(infile,'latitude')
     pt1_lat=NEAREST(latitude,pt1(0))
     pt2_lat=NEAREST(latitude,pt2(0))
     time=OPEN_AND_EXTRACT(infile,'time')
     n_lon=N_ELEMENTS(longitude)
     n_lat=N_ELEMENTS(latitude)
     n_time=N_ELEMENTS(time)

     id=NCDF_CREATE(outfile,/CLOBBER)
     dimids=intarr(3)
     varids=intarr(3+n_fluxes)
     dimids(0)=NCDF_DIMDEF(id,'longitude',n_lon)
     dimids(1)=NCDF_DIMDEF(id,'latitude',n_lat)
     dimids(2)=NCDF_DIMDEF(id,'time',n_time)
     varids(0)=NCDF_VARDEF(id,'longitude',[dimids(0)])
     varids(1)=NCDF_VARDEF(id,'latitude',[dimids(1)])
     varids(2)=NCDF_VARDEF(id,'time',[dimids(2)])
     FOR j=0,n_fluxes-1 DO $
        varids(3+j)=NCDF_VARDEF(id,fluxes(j),[dimids(0),dimids(1),dimids(2)])
     NCDF_CONTROL,id,/ENDEF
     NCDF_VARPUT,id,varids(0),longitude
     NCDF_VARPUT,id,varids(1),latitude
     NCDF_VARPUT,id,varids(2),time
     
     FOR j=0,n_fluxes-1 DO BEGIN
        print,'Now processing '+fluxes(j)+' ...'
        flux_in=OPEN_AND_EXTRACT(infile,fluxes(j))
        
        flux_out=fltarr(n_lon,n_lat,n_time)
        flux_out=flux_in
        temp=flux_out(pt1_lon,pt1_lat,*)
        flux_out(pt1_lon,pt1_lat,*)=flux_out(pt2_lon,pt2_lat,*)
        flux_out(pt2_lon,pt2_lat,*)=temp

        NCDF_VARPUT,id,varids(3+j),flux_out
     ENDFOR
  ENDFOR

  STOP
END



