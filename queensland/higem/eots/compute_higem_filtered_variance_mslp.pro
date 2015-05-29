PRO compute_higem_filtered_variance_mslp
  
; Compute the seasonal variance in HiGEM MSLP from a single file of January-December values.

infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.mar-feb_dmeans.h9-w8.mslp_filter210.nc'
n_seasons=4

FOR i=0,n_seasons-1 DO BEGIN
   CASE i OF
      3 : BEGIN
         season='mar-may'
         offset=0
         ndays=90
      END
      1 : BEGIN
         season='jun-aug'
         offset=90
         ndays=90
      END
      2 : BEGIN
         season='sep-nov'
         offset=180
         ndays=90
      END
      0 : BEGIN
         season='dec-feb'
         offset=270
         ndays=90
      END
   ENDCASE
   latitude=OPEN_AND_EXTRACT(infile,'latitude')
   longitude=OPEN_AND_EXTRACT(infile,'longitude')
   time=OPEN_AND_EXTRACT(infile,'time')
  ; year=OPEN_AND_EXTRACT(infile,'record')
   nlat=N_ELEMENTS(latitude)
   nlon=N_ELEMENTS(longitude)
   ntime=N_ELEMENTS(time)
  ; nyear=N_ELEMENTS(year)
   nyear=149
 
   IF offset+ndays gt 365 THEN BEGIN
      nyear=nyear-1
      this_season_filtered_mslp=fltarr(nlon,nlat,ndays,nyear)
      FOR j=0,nyear-1 DO BEGIN
         this_season_filtered_mslp(*,*,0:(365-offset-1),j)=REFORM(OPEN_AND_EXTRACT(infile,'p',$
                                                                                 offset=[0,0,0,offset,j],$
                                                                                 count=[nlon,nlat,1,365-offset,1]))
         this_season_filtered_mslp(*,*,(365-offset):ndays-1,j)=$
            REFORM(OPEN_AND_EXTRACT(infile,'p',offset=[0,0,0,j+1],count=[nlon,nlat,ndays-(365-offset),1]))
      ENDFOR
   ENDIF ELSE BEGIN       
      this_season_filtered_mslp=REFORM(OPEN_AND_EXTRACT(infile,'p',$
                                                        offset=[0,0,0,offset,0],$
                                                        count=[nlon,nlat,1,ndays,nyear]))
   ENDELSE
   this_season_filtered_variance=fltarr(nlon,nlat,nyear)
   FOR j=0,nlon-1 DO $
      FOR k=0,nlat-1 DO $
         FOR m=0,nyear-1 DO $
            this_season_filtered_variance(j,k,m)=STDDEV(this_season_filtered_mslp(j,k,*,m))
 
   output_netcdf_file='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.'+season+'_smeans.h9-w8.msl_filter210_stddev.nc'
   dimids=intarr(3)
   varids=intarr(4)
   outid=NCDF_CREATE(output_netcdf_file,/CLOBBER)
   dimids(0)=NCDF_DIMDEF(outid,'longitude',nlon)
   dimids(1)=NCDF_DIMDEF(outid,'latitude',nlat)
   dimids(2)=NCDF_DIMDEF(outid,'year',nyear)
   varids(0)=NCDF_VARDEF(outid,'longitude',[dimids(0)])
   varids(1)=NCDF_VARDEF(outid,'latitude',[dimids(1)])
   varids(2)=NCDF_VARDEF(outid,'year',[dimids(2)])
   varids(3)=NCDF_VARDEF(outid,'MSL',[dimids(0),dimids(1),dimids(2)])
   NCDF_CONTROL,outid,/ENDEF
   NCDF_VARPUT,outid,varids(0),longitude
   NCDF_VARPUT,outid,varids(1),latitude
;   NCDF_VARPUT,outid,varids(2),year(0:nyear-1)
   NCDF_VARPUT,outid,varids(2),indgen(nyear)
   NCDF_VARPUT,outid,varids(3),this_season_filtered_variance
   NCDF_CLOSE,outid

ENDFOR

STOP
END

