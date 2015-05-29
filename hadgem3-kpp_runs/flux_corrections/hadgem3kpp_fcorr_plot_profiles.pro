PRO hadgem3kpp_fcorr_plot_profiles

uncorr_infile='/home/ss901165/kpp_ocean3/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_junstart_ukmo_sal60_ind/xfzbj_i0/KPPocean_0090_means.nc'
corr_infile='/home/ss901165/kpp_ocean3/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_junstart_ukmo_sal60_ind/xfzbj_i0/corrected/KPPocean_0090_means.nc'
ukmo_infile='/home/ss901165/datasets/METO_OCEAN_ANALYSIS/n96/meto_ocean_analysis.aug_mmean_clim.1980-2009.pot_temp.n96.nc'
fcorr_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/heat_corrections/hadgem3_flxcorr_1mtop_3hr_junstart_ukmo_sal60_xfzbj_i0.aug_mmean.n96.nc'

kpp_longitude=OPEN_AND_EXTRACT(uncorr_infile,'longitude')
kpp_latitude=OPEN_AND_EXTRACT(uncorr_infile,'latitude')
kpp_z=OPEN_AND_EXTRACT(uncorr_infile,'z')
kpp_nlon=N_ELEMENTS(kpp_longitude)
kpp_nlat=N_ELEMENTS(kpp_latitude)
kpp_nz=N_ELEMENTS(kpp_z)

ukmo_longitude=OPEN_AND_EXTRACT(ukmo_infile,'longitude')
ukmo_latitude=OPEN_AND_EXTRACT(ukmo_infile,'latitude')
ukmo_z=OPEN_AND_EXTRACT(ukmo_infile,'depth')*(-1.)
DEFINE_BOUNDARIES,[kpp_latitude(0),kpp_longitude(0),kpp_latitude(kpp_nlat-1),$
                   kpp_longitude(kpp_nlon-1)],ukmo_latitude,ukmo_longitude,$
                  ukmo_box_tx,/LIMIT
ukmo_nlon=N_ELEMENTS(ukmo_longitude)
ukmo_nlat=N_ELEMENTS(ukmo_latitude)
ukmo_z=ukmo_z[where(ukmo_z ge -205)]
ukmo_nz=N_ELEMENTS(ukmo_z)

uncorr_temps=OPEN_AND_EXTRACT(uncorr_infile,'T')
corr_temps=OPEN_AND_EXTRACT(corr_infile,'T')
ukmo_temps=OPEN_AND_EXTRACT(ukmo_infile,'temp',$
                            offset=[ukmo_box_tx(1),ukmo_box_tx(0),0],$
                            count=[ukmo_nlon,ukmo_nlat,ukmo_nz])
fcorr=OPEN_AND_EXTRACT(fcorr_infile,'fcorr')

uncorr_mmean_temps=fltarr(kpp_nlon,kpp_nlat,kpp_nz)
corr_mmean_temps=fltarr(kpp_nlon,kpp_nlat,kpp_nz)
FOR i=0,kpp_nlon-1 DO BEGIN
   FOR j=0,kpp_nlat-1 DO BEGIN
      FOR k=0,kpp_nz-1 DO BEGIN
         uncorr_mmean_temps(i,j,k)=MEAN(uncorr_temps(i,j,k,*))
         corr_mmean_temps(i,j,k)=MEAN(corr_temps(i,j,k,*))
      ENDFOR
   ENDFOR
ENDFOR

uncorr_sst_bias=REFORM(uncorr_mmean_temps(*,*,0))-REFORM(ukmo_temps(*,*,0))
corr_sst_bias=REFORM(corr_mmean_temps(*,*,0))-REFORM(ukmo_temps(*,*,0))

STOP
END
