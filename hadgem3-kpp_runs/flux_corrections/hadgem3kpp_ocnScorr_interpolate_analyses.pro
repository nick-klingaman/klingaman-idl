PRO hadgem3kpp_ocnScorr_interpolate_analyses

; Interpolate the Met Office ocean analysis temperature data onto the KPP
; vertical grid, so that the KPP temperature field can be interactively
; relaxed to the analysis.

n_times=360
times=findgen(n_times)+0.5
;times=[15,45,75,105,135,165,195,225,255,285,315,345]
time_range='jan-dec'
year_range=''
resolution='n216e'
;time_range='.1925-1996'
;time_range=''
;condition='_blend_amoneg2xamean_atlocn'
condition=''
time_range='1980-2009'

; Temperature infile on native (FOAM) grid
;ocnT_infile='/home/users/npklingaman/klingaman/cam/spccsm_mjodiab/SPCCSM_mjodiab.jan-dec_mmean_clim.0004-0023.nc'
;ocnT_infile='/home/users/npklingaman/datasets/METO_OCEAN_ANALYSIS/t42/meto_ocean_analysis.jan-dec_mmean_clim.1980-2009.pot_temp.t42_test.nc'
ocnT_infile='/home/users/npklingaman/datasets/METO_OCEAN_ANALYSIS/'+resolution+'/meto_ocean_analysis.jan-dec_dmean-interp-mmean_amip_clim.1980-2009.sal.BoB_zmean.'+resolution+'.nc'
;ocnT_infile='/home/users/npklingaman/klingaman/metum/gc2/anqjn/anqjn.jan-dec_dmean-interp-mmean_amip_clim.1979-2077.pot_temp.'+resolution+'.nc'
;ocnT_infile='/home/users/npklingaman/datasets/EN4/'+resolution+'/EN4'+condition+'.'+time_range+'_dmeans-interp-mmeans_amip_clim'+year_range+'.ocnT.'+resolution+'.nc'

;ocnT_infile='/home/users/npklingaman/klingaman/metum/gc3/u-ab673/ab673.jan-dec_dmeans-interp-mmeans_amip_clim.'+time_range+'.ocnS_'+resolution+'.nc'
varname='salinity'
depth_varname='depth'

; Temperature outfile on KPP vertical grid
;ocnT_outfile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/n512/meto_ocean_analysis.'+period+'_mmean-amip_clim.1980-2009.pot_temp.n512.kpp_1000m_100lev-2.nc'
;ocnT_outfile='/home/ss901165/um_output6/kpp_ocean/ancillaries/sal_clim/hadgem3ao_ajtzr_ga30.jan-dec_mmean_clim.pot_temp.n96.kpp_1000m_100lev.nc'
;ocnT_outfile='/home/users/npklingaman/datasets/SPCAM-KPP_ANCIL/spcam-kpp_ocnT.jan-dec_mmean_clim.1980-2009.64x128.nc'
ocnT_outfile='/home/users/npklingaman/datasets/METO_OCEAN_ANALYSIS/'+resolution+'/meto_ocean_analysis.jan-dec_dmean-interp-mmean_amip_clim.1980-2009.sal.BoB_zmean.'+resolution+'.kpp_1000m_100lev.nc'
;ocnT_outfile='/home/users/npklingaman/datasets/HADGEM3-KPP_ANCIL/gc2_anjqn.jan-dec_dmean-interp-mmean_amip_clim.1979-2077.pot_temp.'+resolution+'.kpp_1000m_100lev.nc'
;ocnT_outfile='/home/users/npklingaman/datasets/EN4/n96e/EN4'+condition+'.jan-dec_dmeans-interp-mmeans_amip_clim'+time_range+'.ocnT.'+resolution+'.kpp_1000m_100lev.nc'
;ocnT_outfile='/home/users/npklingaman/datasets/EN4/'+resolution+'/EN4'+condition+'.'+time_range+'_dmeans-interp-mmeans_amip_clim'+year_range+'.ocnT.'+resolution+'.kpp_1000m_100lev.nc'
;ocnT_outfile='/home/users/npklingaman/klingaman/metum/gc3/u-ab673/ab673.jan-dec_dmeans-interp-mmeans_amip_clim.'+time_range+'.ocnS_'+resolution+'.kpp_1000m_100lev.nc'


; KPP file from which to read ocean depths
;landfrac_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/landfrac_n96_hadgem3-7.3.nc'
;ocndepth_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/bathymetry/lsm_ocndepth_for_kpp.n512.ETOPO2v2c.nc'
;ocndepth_infile='/home/users/npklingaman/datasets/SPCAM-KPP_ANCIL/spcam-kpp_lsm_ocndepth.64x128.nc'
;landfrac_infile='/home/ss901165/datasets/NCOF_OCEAN/lsm_ocndepth_for_kpp.n144.nc'
;ocndepth_infile='/home/ss901165/datasets/NCOF_OCEAN/lsm_ocndepth_for_kpp.n144.nc'
ocndepth_infile='/home/users/npklingaman/datasets/HADGEM3-KPP_ANCIL/bathymetry/lsm_ocndepth_for_kpp.'+resolution+'_go61.ETOPO2v2c.nc'

; Example KPP input file for vertical domain
;kpp_infile='/home/ss901165/um_output6/xihvd/kpp_ocean/KPPocean_0030_means.nc'
;kpp_infile='/home/ss901165/um_output/xcwyh_60lev/KPPocean_sep_mmeans.nc'
;kpp_infile='/home/users/npklingaman/klingaman/cam/spcam_iceedge_free/kpp/KPPocean_00274_means.nc'
kpp_infile='/home/users/npklingaman/datasets/HADGEM3-KPP_ANCIL/kpp_vgrid_1000m_100lev.nc'

; Read in global latitude, longitude, depth
global_latitude=OPEN_AND_EXTRACT(ocnT_infile,'latitude')
global_longitude=OPEN_AND_EXTRACT(ocnT_infile,'longitude')
global_nlat=N_ELEMENTS(global_latitude)
global_nlon=N_ELEMENTS(global_longitude)
global_depth=OPEN_AND_EXTRACT(ocnT_infile,depth_varname)
global_nz=N_ELEMENTS(global_depth)

; Reverse UKMO depth to have same convention as KPP
global_depth=(-1.)*global_depth

; Read in KPP latitude, longitude and depth
kpp_latitude=OPEN_AND_EXTRACT(ocndepth_infile,'latitude')
kpp_longitude=OPEN_AND_EXTRACT(ocndepth_infile,'longitude')
kpp_depth=OPEN_AND_EXTRACT(kpp_infile,'z')
kpp_depth=[kpp_depth,-1000.0]
kpp_nlon=N_ELEMENTS(kpp_longitude)
kpp_nlat=N_ELEMENTS(kpp_latitude)
kpp_nz=N_ELEMENTS(kpp_depth)
DEFINE_BOUNDARIES,[kpp_latitude(0),kpp_longitude(0),$
                   kpp_latitude(kpp_nlat-1),kpp_longitude(kpp_nlon-1)],$
                  global_latitude,global_longitude,kpp_box_tx,/LIMIT
print,kpp_box_tx

landfrac = REFORM(OPEN_AND_EXTRACT(ocndepth_infile,'lsm',$
                                   offset=[0,0],$
                                   count=[global_nlon,global_nlat]))

ocndepth = REFORM(OPEN_AND_EXTRACT(ocndepth_infile,'max_depth',$
                                   offset=[0,0],$
                                   count=[global_nlon,global_nlat]))

ukmo_ocnT_mmean = REFORM(OPEN_AND_EXTRACT(ocnT_infile,varname,$
                                         offset=[0,0,0,0],$
                                         count=[global_nlon,global_nlat,global_nz,n_times]))

ukmo_ocnT_mmean_kppgrid=fltarr(global_nlon,global_nlat,kpp_nz,n_times)

FOR i=0,global_nlon-1 DO BEGIN
   print,'Running for longitude = '+STRTRIM(STRING(i+1),1)+' of '+STRTRIM(STRING(global_nlon),1)
   FOR j=0,global_nlat-1 DO BEGIN
                                     ; Only compute the temperature if this is an ocean point
      IF landfrac(i,j) eq 0 THEN BEGIN
                                ; Use a cubic spline to interpolate the UKMO temperature
                                ; onto the KPP vertical grid.
         thispt_ukmo_ocnT_mmean=REFORM(ukmo_ocnT_mmean(i,j,*,*))
         IF TOTAL(where(thispt_ukmo_ocnT_mmean(*,0) gt 1e10 or FINITE(thispt_ukmo_ocnT_mmean(*,0)) eq 0)) ge 0 THEN BEGIN
            max_depth=MIN(where(thispt_ukmo_ocnT_mmean(*,0) gt 1e10 or FINITE(thispt_ukmo_ocnT_mmean(*,0)) eq 0))         
         ENDIF ELSE $
            max_depth=global_nz-1
         ;IF i eq 429 and j eq 728 THEN $
         ;   print,max_depth,thispt_ukmo_ocnT_mmean(0:max_depth-1,0)
                                ; Initialize the spline
    ;     IF max_depth lt 2 THEN max_depth=2
         FOR k=0,n_times-1 DO BEGIN
            ;print,REFORM(thispt_ukmo_ocnT_mmean(0:max_depth-1,k))
            ukmo_to_kpp_spline=SPL_INIT(ABS(global_depth(0:max_depth-1)),REFORM(thispt_ukmo_ocnT_mmean(0:max_depth-1,k)))
            ukmo_ocnT_mmean_kppgrid(i,j,*,k)=SPL_INTERP(ABS(global_depth(0:max_depth-1)),REFORM(thispt_ukmo_ocnT_mmean(0:max_depth-1,k)),$
                                                        ukmo_to_kpp_spline,ABS(kpp_depth))
            max_depth_kpp=NEAREST(kpp_depth,global_depth(max_depth))
          ENDFOR
            IF (max_depth_kpp lt kpp_nz-1) THEN $
		FOR k=max_depth_kpp-1,kpp_nz-1 DO $
		ukmo_ocnT_mmean_kppgrid(i,j,k,*)=ukmo_ocnT_mmean_kppgrid(i,j,max_depth_kpp-1,*)
      ENDIF ELSE $
         ukmo_ocnT_mmean_kppgrid(i,j,*,*)=2e20
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
ocnT_varid=NCDF_VARDEF(id,varname,[lon_dimid,lat_dimid,z_dimid,t_dimid])
NCDF_ATTPUT,id,ocnT_varid,'missing_value',2e20

NCDF_CONTROL,id,/ENDEF

NCDF_VARPUT,id,lon_varid,global_longitude
NCDF_VARPUT,id,lat_varid,global_latitude
NCDF_VARPUT,id,z_varid,[kpp_depth]
NCDF_VARPUT,id,t_varid,times
NCDF_VARPUT,id,ocnT_varid,ukmo_ocnT_mmean_kppgrid

NCDF_CLOSE,id

STOP
END

