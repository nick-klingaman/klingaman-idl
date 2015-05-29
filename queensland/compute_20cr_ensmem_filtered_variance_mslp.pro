PRO compute_20cr_ensmem_filtered_variance_mslp
  
; Compute the seasonal variance in HiGEM MSLP from a single file of January-December values.

indir='/home/ss901165/datasets/20THC_REANALYSIS/every_member/mslp_6hr'
n_seasons=12
n_members=56

FOR p=0,n_members-1 DO BEGIN
infile=indir+'/20thc_reanalysis.jan-dec_6hrmeans.1871-2008.member'+STRTRIM(STRING(p+1),1)+'.mslp_filter26.sh_domain.nc'
print,'Member '+STRTRIM(STRING(p+1),1)+' ...'
FOR i=0,n_seasons-1 DO BEGIN
   CASE i OF
      3 : BEGIN
         season='mar'
         offset=59
         ndays=31
      END
      1 : BEGIN
         season='jan'
         offset=0
         ndays=31
      END
      2 : BEGIN
         season='feb'
         offset=31
         ndays=28
      END
      0 : BEGIN
         season='dec'
         offset=334
         ndays=31
      END
      4 : BEGIN
         season='apr'
         offset=90
         ndays=30
      END
      5 : BEGIN
         season='may'
         offset=120
         ndays=31
      END
      6 : BEGIN
         season='jun'
         offset=151
         ndays=30
      END
      7 : BEGIN
         season='jul'
         offset=181
         ndays=31
      END
      8 : BEGIN
         season='aug'
         offset=212
         ndays=31
      END
      9 : BEGIN
         season='sep'
         offset=243
         ndays=30
      END
      10 : BEGIN
         season='oct'
         offset=273
         ndays=31
      END
      11 : BEGIN
         season='nov'
         offset=304
         ndays=30
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
   nyear=138
 
   IF offset+ndays gt 365 THEN BEGIN
      nyear=nyear-1
      this_season_filtered_mslp=fltarr(ndays,nyear,nlat,nlon)
      FOR j=0,nyear-1 DO BEGIN
         this_season_filtered_mslp(0:(365-offset-1),j,*,*)=REFORM(OPEN_AND_EXTRACT(infile,'prmsl',$
                                                                                 offset=[offset,j,0,0],$
                                                                                 count=[365-offset,1,nlat,nlon]))
         this_season_filtered_mslp((365-offset):ndays-1,j,*,*)=$
            REFORM(OPEN_AND_EXTRACT(infile,'prmsl',offset=[0,j+1,0,0],count=[ndays-(365-offset),1,nlat,nlon]))
      ENDFOR
   ENDIF ELSE BEGIN       
      this_season_filtered_mslp=REFORM(OPEN_AND_EXTRACT(infile,'prmsl',$
                                                        offset=[offset,0,0,0],$
                                                        count=[ndays,nyear,nlat,nlon]))
   ENDELSE
   this_season_filtered_variance=fltarr(nlon,nlat,nyear)
   FOR j=0,nlon-1 DO $
      FOR k=0,nlat-1 DO $
         FOR m=0,nyear-1 DO $
            this_season_filtered_variance(j,k,m)=STDDEV(this_season_filtered_mslp(*,m,k,j))
 
   
output_netcdf_file=indir+'/20thc_reanalysis.'+season+'_mmeans.1871-2008.member'+STRTRIM(STRING(p+1),1)+$
'.mslp_filter26_stddev.sh_domain.nc'
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
ENDFOR

STOP
END

