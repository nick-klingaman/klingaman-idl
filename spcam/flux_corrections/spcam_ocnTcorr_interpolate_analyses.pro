PRO spcam_ocnTcorr_interpolate_analyses

; Interpolate the Met Office ocean analysis temperature data onto the KPP
; vertical grid, so that the KPP temperature field can be interactively
; relaxed to the analysis.

;n_times=12
n_times=1095
times=findgen(1095)+0.5
;times=[15,45,75,105,135,165,195,225,255,285,315,345]
period='jan-dec'

; Temperature infile on native (FOAM) grid
;ocnT_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/n512/test_sal.nc';meto_ocean_analysis.'+period+'_mmean-amip_clim.1980-2009.sal.n512_extrap.nc'
;ocnT_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/ga30_coupled_TandS/hadgem3ao_ajtzr_ga30.jan-dec_mmean_clim.sal.n96.nc'
;ocnT_infile='/home/ss901165/um_output6/kpp_ocean/ancillaries/temp_clim/hadgem3ao_ajtzr_ga30.jan-dec_mmean_clim.sal.nc'
ocnT_infile='/home/users/npklingaman/klingaman/cam/spccsm_mjodiab/spccsm_enso_cycle.jan-dec_dmean-interp-mmean_amip_3yrclim.TEMP_flagged.64x128.nc'
varname='TEMP'
depth_varname='z_t'

; Temperature outfile on KPP vertical grid
;ocnT_outfile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/n512/meto_ocean_analysis.'+period+'_mmean-amip_clim.1980-2009.sal.n512.kpp_1000m_100lev-2.nc'
;ocnT_outfile='/home/ss901165/um_output6/kpp_ocean/ancillaries/sal_clim/hadgem3ao_ajtzr_ga30.jan-dec_mmean_clim.sal.n96.kpp_1000m_100lev.nc'
ocnT_outfile='/home/users/npklingaman/datasets/SPCAM-KPP_ANCIL/spcam-kpp_ocnT_spccsm_enso.jan-dec_dmean-interp-mmean_amip_3yrclim.1000m_100lev.64x128.nc'
out_varname='temperature'

; KPP file from which to read ocean depths
;landfrac_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/landfrac_n96_hadgem3-7.3.nc'
;ocndepth_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/bathymetry/lsm_ocndepth_for_kpp.n512.ETOPO2v2c.nc'
ocndepth_infile='/home/users/npklingaman/datasets/SPCAM-KPP_ANCIL/spcam-kpp_lsm_ocndepth.64x128.nc'
;landfrac_infile='/home/ss901165/datasets/NCOF_OCEAN/lsm_ocndepth_for_kpp.n144.nc'
;ocndepth_infile='/home/ss901165/datasets/NCOF_OCEAN/lsm_ocndepth_for_kpp.n144.nc'

; Example KPP input file for horizontal and vertical domain
;kpp_infile='/home/ss901165/um_output6/xihvd/kpp_ocean/KPPocean_0030_means.nc'
;kpp_infile='/home/ss901165/um_output/xcwyh_60lev/KPPocean_sep_mmeans.nc'
kpp_infile='/home/users/npklingaman/datasets/SPCAM-KPP_ANCIL/spcam-kpp_ocnT_spccsm_neutralnino.jan-dec_mmean_clim.1000m_100lev.64x128.nc'

; Read in global latitude, longitude, depth
global_latitude=OPEN_AND_EXTRACT(ocnT_infile,'latitude')
global_longitude=OPEN_AND_EXTRACT(ocnT_infile,'longitude')
global_nlat=N_ELEMENTS(global_latitude)
global_nlon=N_ELEMENTS(global_longitude)
global_depth=OPEN_AND_EXTRACT(ocnT_infile,depth_varname)
global_nz=N_ELEMENTS(global_depth)

; Reverse SPCCSM depth to have same convection as KPP and convert cm
; to m
global_depth=(-1.)*global_depth/100.

; Read in KPP latitude, longitude and depth
kpp_latitude=OPEN_AND_EXTRACT(ocndepth_infile,'latitude')
kpp_longitude=OPEN_AND_EXTRACT(ocndepth_infile,'longitude')
kpp_depth=OPEN_AND_EXTRACT(kpp_infile,'z')
kpp_nlon=N_ELEMENTS(kpp_longitude)
kpp_nlat=N_ELEMENTS(kpp_latitude)
kpp_nz=N_ELEMENTS(kpp_depth)
DEFINE_BOUNDARIES,[kpp_latitude(0),kpp_longitude(0),$
                   kpp_latitude(kpp_nlat-1),kpp_longitude(kpp_nlon-1)],$
                  global_latitude,global_longitude,kpp_box_tx,/LIMIT

landfrac = REFORM(OPEN_AND_EXTRACT(ocndepth_infile,'lsm',$
                                   offset=[0,0],$
                                   count=[global_nlon,global_nlat]))

ocndepth = REFORM(OPEN_AND_EXTRACT(ocndepth_infile,'max_depth',$
                                   offset=[0,0],$
                                   count=[global_nlon,global_nlat]))

spccsm_ocnT_mmean = REFORM(OPEN_AND_EXTRACT(ocnT_infile,varname,$
                                         offset=[0,0,0,0],$
                                         count=[global_nlon,global_nlat,global_nz,n_times]))

spccsm_ocnT_mmean_kppgrid=fltarr(global_nlon,global_nlat,kpp_nz,n_times)

FOR i=0,global_nlon-1 DO BEGIN
   FOR j=0,global_nlat-1 DO BEGIN
                                ; Only compute the temperature if this is an ocean point
     ; IF landfrac(i,j) lt 0.7 THEN BEGIN
                                ; Use a cubic spline to interpolate the SPCCSM temperature
                                ; onto the KPP vertical grid.
         thispt_spccsm_ocnT_mmean=REFORM(spccsm_ocnT_mmean(i,j,*,*))
         IF TOTAL(where(thispt_spccsm_ocnT_mmean(*,0) gt 1e10)) ge 0 THEN BEGIN
            max_depth=MIN(where(thispt_spccsm_ocnT_mmean(*,0) gt 1e10))
         ENDIF ELSE $
            max_depth=global_nz-1
                                ; Initialize the spline
    ;     IF max_depth lt 2 THEN max_depth=2
         FOR k=0,n_times-1 DO BEGIN
            ;print,REFORM(thispt_spccsm_ocnT_mmean(0:max_depth-1,k))
            spccsm_to_kpp_spline=SPL_INIT(ABS(global_depth(0:max_depth-1)),REFORM(thispt_spccsm_ocnT_mmean(0:max_depth-1,k)))
            spccsm_ocnT_mmean_kppgrid(i,j,*,k)=SPL_INTERP(ABS(global_depth(0:max_depth-1)),REFORM(thispt_spccsm_ocnT_mmean(0:max_depth-1,k)),$
                                                        spccsm_to_kpp_spline,ABS(kpp_depth))
            max_depth_kpp=NEAREST(kpp_depth,global_depth(max_depth))	    
         ENDFOR
         IF (max_depth_kpp lt kpp_nz-1) THEN $
            FOR k=max_depth_kpp-1,kpp_nz-1 DO $
               spccsm_ocnT_mmean_kppgrid(i,j,k,*)=spccsm_ocnT_mmean_kppgrid(i,j,max_depth_kpp-1,*)
         ;IF (global_depth(0) lt kpp_depth(0)) THEN BEGIN
         ;   FOR k=0,n_times-1 DO $
         ;      spccsm_ocnT_mmean_kppgrid(i,j,0:NEAREST(kpp_depth,global_depth(0)),k) = spccsm_ocnT_mmean_kppgrid(i,j,NEAREST(kpp_depth,global_depth(0)),k)
         ;ENDIF
     ; ENDIF ELSE $
     ;    spccsm_ocnT_mmean_kppgrid(i,j,*,*)=2e20
   ENDFOR
ENDFOR

id=NCDF_CREATE(ocnT_outfile,/CLOBBER)
lon_dimid=NCDF_DIMDEF(id,'longitude',global_nlon)
lon_varid=NCDF_VARDEF(id,'longitude',lon_dimid)
lat_dimid=NCDF_DIMDEF(id,'latitude',global_nlat)
lat_varid=NCDF_VARDEF(id,'latitude',lat_dimid)
z_dimid=NCDF_DIMDEF(id,'z',kpp_nz)
z_varid=NCDF_VARDEF(id,'z',z_dimid)
t_dimid=NCDF_DIMDEF(id,'t',/UNLIMITED)
t_varid=NCDF_VARDEF(id,'t',t_dimid)
ocnT_varid=NCDF_VARDEF(id,out_varname,[lon_dimid,lat_dimid,z_dimid,t_dimid])
NCDF_ATTPUT,id,ocnT_varid,'missing_value',2e20

NCDF_CONTROL,id,/ENDEF

NCDF_VARPUT,id,lon_varid,global_longitude
NCDF_VARPUT,id,lat_varid,global_latitude
NCDF_VARPUT,id,z_varid,[kpp_depth]
NCDF_VARPUT,id,t_varid,times
NCDF_VARPUT,id,ocnT_varid,spccsm_ocnT_mmean_kppgrid

NCDF_CLOSE,id

STOP
END

