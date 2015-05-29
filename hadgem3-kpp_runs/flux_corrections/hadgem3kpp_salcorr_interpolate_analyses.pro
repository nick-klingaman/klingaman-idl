PRO hadgem3kpp_salcorr_interpolate_analyses

; Interpolate the Met Office ocean analysis salinity data onto the KPP
; vertical grid, so that the KPP salinity field can be interactively
; relaxed to the analysis.

n_times=12
times=[15,45,75,105,135,165,195,225,255,285,315,345]
period='jan-dec'

; Salinity infile on native (FOAM) grid
sal_infile='/home/ss901165/datasets_mango/METO_OCEAN_ANALYSIS/n96/meto_ocean_analysis.'+period+'_mmean_clim.1980-2009.pot_temp.n96.nc'
;sal_infile='/home/ss901165/um_output6/kpp_ocean/ancillaries/sal_clim/hadgem3ao_ajtzr_ga30.jan-dec_mmean_clim.sal.nc'

; Salinity outfile on KPP vertical grid
sal_outfile='/home/ss901165/datasets_mango/METO_OCEAN_ANALYSIS/n96/meto_ocean_analysis.'+period+'_mmean_clim.1980-2009.pot_temp.n96.kpp_1000m_100lev.nc'
;sal_outfile='/home/ss901165/um_output6/kpp_ocean/ancillaries/sal_clim/hadgem3ao_ajtzr_ga30.jan-dec_mmean_clim.kpp_1m_vert.nc'

; KPP file from which to read ocean depths
landfrac_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/landfrac_n96_hadgem3-7.3.nc'
ocndepth_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/bathymetry/lsm_ocndepth_for_kpp.n96.ETOP02v2c.nc'
;landfrac_infile='/home/ss901165/datasets/NCOF_OCEAN/lsm_ocndepth_for_kpp.n144.nc'
;ocndepth_infile='/home/ss901165/datasets/NCOF_OCEAN/lsm_ocndepth_for_kpp.n144.nc'

; Example KPP input file for horizontal and vertical domain
kpp_infile='/home/ss901165/um_output6/kpp_ocean/3D_kpp_r4b1/test_run_1000m/KPPocean_0001_means.nc'
;kpp_infile='/home/ss901165/um_output/xcwyh_60lev/KPPocean_sep_mmeans.nc'

; Read in global latitude, longitude, depth
global_latitude=OPEN_AND_EXTRACT(sal_infile,'latitude')
global_longitude=OPEN_AND_EXTRACT(sal_infile,'longitude')
global_depth=OPEN_AND_EXTRACT(sal_infile,'depth')
global_nz=N_ELEMENTS(global_depth)
global_nlon=N_ELEMENTS(global_longitude)
global_nlat=N_ELEMENTS(global_latitude)

; Reverse UKMO depth to have same convection as KPP
global_depth=(-1.)*global_depth

; Read in KPP latitude, longitude and depth
kpp_latitude=OPEN_AND_EXTRACT(ocndepth_infile,'latitude')
kpp_longitude=OPEN_AND_EXTRACT(ocndepth_infile,'longitude')
kpp_depth=OPEN_AND_EXTRACT(kpp_infile,'z')
kpp_nlon=N_ELEMENTS(kpp_longitude)
kpp_nlat=N_ELEMENTS(kpp_latitude)
kpp_nz=N_ELEMENTS(kpp_depth)
DEFINE_BOUNDARIES,[kpp_latitude(0),kpp_longitude(0),$
                   kpp_latitude(global_nlat-1),kpp_longitude(kpp_nlon-1)],$
                  global_latitude,global_longitude,kpp_box_tx,/LIMIT

landfrac = REFORM(OPEN_AND_EXTRACT(landfrac_infile,'lsm',$
                                   offset=[0,0,0,0],$
                                   count=[global_nlon,global_nlat,1,1]))

ocndepth = REFORM(OPEN_AND_EXTRACT(ocndepth_infile,'max_depth',$
                                   offset=[0,0],$
                                   count=[global_nlon,global_nlat]))

ukmo_sal_mmean = REFORM(OPEN_AND_EXTRACT(sal_infile,'temp',$
                                         offset=[0,0,0,0],$
                                         count=[global_nlon,global_nlat,global_nz,n_times]))

ukmo_sal_mmean_kppgrid=fltarr(global_nlon,global_nlat,kpp_nz,n_times)

FOR i=0,global_nlon-1 DO BEGIN
   FOR j=0,global_nlat-1 DO BEGIN
                                ; Only compute the salinity if this is an ocean point
      IF landfrac(i,j) lt 1 THEN BEGIN
                                ; Use a cubic spline to interpolate the UKMO salinity
                                ; onto the KPP vertical grid.
         thispt_ukmo_sal_mmean=REFORM(ukmo_sal_mmean(i,j,*,*))
         ; Initialize the spline
         FOR k=0,n_times-1 DO BEGIN
            ukmo_to_kpp_spline=SPL_INIT(ABS(global_depth),REFORM(thispt_ukmo_sal_mmean(*,k)))
            ukmo_sal_mmean_kppgrid(i,j,*,k)=SPL_INTERP(ABS(global_depth),REFORM(thispt_ukmo_sal_mmean(*,k)),$
                                                     ukmo_to_kpp_spline,ABS(kpp_depth))
         ENDFOR
      ENDIF ELSE $
         ukmo_sal_mmean_kppgrid(i,j,*,*)=2e20
   ENDFOR
ENDFOR

id=NCDF_CREATE(sal_outfile,/CLOBBER)
lon_dimid=NCDF_DIMDEF(id,'longitude',global_nlon)
lon_varid=NCDF_VARDEF(id,'longitude',lon_dimid)
lat_dimid=NCDF_DIMDEF(id,'latitude',global_nlat)
lat_varid=NCDF_VARDEF(id,'latitude',lat_dimid)
z_dimid=NCDF_DIMDEF(id,'z',kpp_nz)
z_varid=NCDF_VARDEF(id,'z',z_dimid)
t_dimid=NCDF_DIMDEF(id,'t',/UNLIMITED)
t_varid=NCDF_VARDEF(id,'t',t_dimid)
sal_varid=NCDF_VARDEF(id,'temperature',[lon_dimid,lat_dimid,z_dimid,t_dimid])

NCDF_ATTPUT,id,sal_varid,'missing_value',2e20

NCDF_CONTROL,id,/ENDEF

NCDF_VARPUT,id,lon_varid,global_longitude
NCDF_VARPUT,id,lat_varid,global_latitude
NCDF_VARPUT,id,z_varid,[kpp_depth]
NCDF_VARPUT,id,t_varid,times
NCDF_VARPUT,id,sal_varid,ukmo_sal_mmean_kppgrid

NCDF_CLOSE,id

STOP
END

