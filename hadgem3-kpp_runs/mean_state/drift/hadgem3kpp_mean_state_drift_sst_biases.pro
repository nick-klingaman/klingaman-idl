PRO hadgem3kpp_mean_state_drift_sst_biases

;kpp_indir='/home/ss901165/um_output5/xgspm/kpp_ocean'
kpp_indir='/home/ss901165/um_output5/xgspm_ind3'
obs_indir='/home/ss901165/datasets/METO_OCEAN_ANALYSIS/n96'

box=[-30,20,30,200]

;years=['i2','i3','i4','i5','i6','i7','i8','i9','j0']
years=['i9','j0','j1']
;years=['i2']
n_years=N_ELEMENTS(years)
;months=['jan15-feb15','feb15-mar15','mar15-apr15']
months=['jan15-feb15','feb15-mar15','mar15-apr15','apr15-may15','may15-jun15','jun15-jul15',$
        'jul15-aug15','aug15-sep15','sep15-oct15','oct15-nov15','nov15-dec15','dec15-jan15']
n_months=N_ELEMENTS(months)

levels_diff=['-1.7','-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5','1.7']
levels_flux=['-300','-260','-220','-180','-140','-100','-60','-20','20','60','100','140','180','220','260','300']

;kpp_ensmean_fluxes_infile='/export/mango/data-10/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421/ensmean.jan-dec_mmeans.fluxes.nc'
;kpp_ensmean_temp_infile='/export/mango/data-10/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421/ensmean.jan-dec_mmeans.corrected.nc'
kpp_ensmean_fluxes_infile='/export/mango/data-10/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421_ind/ensmean.jan-dec_mmeans.fluxes.nc'
kpp_ensmean_temp_infile='/export/mango/data-10/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421_ind/ensmean.jan-dec_mmeans.corrected.nc'

kpp_longitude=OPEN_AND_EXTRACT(kpp_ensmean_fluxes_infile,'longitude')
kpp_latitude=OPEN_AND_EXTRACT(kpp_ensmean_fluxes_infile,'latitude')
DEFINE_BOUNDARIES,box,kpp_latitude,kpp_longitude,kpp_box_tx,/LIMIT
kpp_nlon=N_ELEMENTS(kpp_longitude)
kpp_nlat=N_ELEMENTS(kpp_latitude)

forced_solar=REFORM(OPEN_AND_EXTRACT(kpp_ensmean_fluxes_infile,'swf',$
                                     offset=[kpp_box_tx(1),kpp_box_tx(0),0],$
                                     count=[kpp_nlon,kpp_nlat,n_months]))
forced_longwave=REFORM(OPEN_AND_EXTRACT(kpp_ensmean_fluxes_infile,'lwf',$
                                        offset=[kpp_box_tx(1),kpp_box_tx(0),0],$
                                        count=[kpp_nlon,kpp_nlat,n_months]))
forced_latent=REFORM(OPEN_AND_EXTRACT(kpp_ensmean_fluxes_infile,'lhf',$
                                      offset=[kpp_box_tx(1),kpp_box_tx(0),0],$
                                      count=[kpp_nlon,kpp_nlat,n_months]))
forced_sensible=REFORM(OPEN_AND_EXTRACT(kpp_ensmean_fluxes_infile,'shf',$
                                        offset=[kpp_box_tx(1),kpp_box_tx(0),0],$
                                        count=[kpp_nlon,kpp_nlat,n_months]))

forced_net_flux=forced_solar+forced_longwave+forced_latent+forced_sensible

kpp_longitude=OPEN_AND_EXTRACT(kpp_ensmean_temp_infile,'longitude')
kpp_latitude=OPEN_AND_EXTRACT(kpp_ensmean_temp_infile,'latitude')
DEFINE_BOUNDARIES,box,kpp_latitude,kpp_longitude,kpp_box_tx,/LIMIT
kpp_nlon=N_ELEMENTS(kpp_longitude)
kpp_nlat=N_ELEMENTS(kpp_latitude)

forced_sst=REFORM(OPEN_AND_EXTRACT(kpp_ensmean_temp_infile,'T',$
                                   offset=[kpp_box_tx(1),kpp_box_tx(0),0,0],$
                                   count=[kpp_nlon,kpp_nlat,1,n_months]))

FOR i=0,n_years-1 DO BEGIN
   FOR j=0,n_months-1 DO BEGIN

      kpp_infile=kpp_indir+'/KPPocean.'+months(j)+'_mmean.'+years(i)+'.nc'
      obs_infile=obs_indir+'/meto_ocean_analysis.'+months(j)+'_mmean_clim.1980-2009.pot_temp.amip_type.n96.nc'      

      kpp_longitude=OPEN_AND_EXTRACT(kpp_infile,'longitude')
      kpp_latitude=OPEN_AND_EXTRACT(kpp_infile,'latitude')
      DEFINE_BOUNDARIES,box,kpp_latitude,kpp_longitude,kpp_box_tx,/LIMIT
      kpp_nlon=N_ELEMENTS(kpp_longitude)
      kpp_nlat=N_ELEMENTS(kpp_latitude)

      obs_longitude=OPEN_AND_EXTRACT(obs_infile,'longitude')
      obs_latitude=OPEN_AND_EXTRACT(obs_infile,'latitude')
      DEFINE_BOUNDARIES,box,obs_latitude,obs_longitude,obs_box_tx,/LIMIT
      obs_nlon=N_ELEMENTS(obs_longitude)
      obs_nlat=N_ELEMENTS(obs_latitude)

      kppT=REFORM(OPEN_AND_EXTRACT(kpp_infile,'T',$
                                   offset=[kpp_box_tx(1),kpp_box_tx(0),0,0],$
                                   count=[kpp_nlon,kpp_nlat,1,1]))
      kppsolar=REFORM(OPEN_AND_EXTRACT(kpp_infile,'solar_in',$
                                       offset=[kpp_box_tx(1),kpp_box_tx(0),0],$
                                       count=[kpp_nlon,kpp_nlat,1]))
      kppnsolar=REFORM(OPEN_AND_EXTRACT(kpp_infile,'nsolar_in',$
                                        offset=[kpp_box_tx(1),kpp_box_tx(0),0],$
                                        count=[kpp_nlon,kpp_nlat,1]))

      obsT=REFORM(OPEN_AND_EXTRACT(obs_infile,'temp',$
                                   offset=[obs_box_tx(1),obs_box_tx(0),0],$
                                   count=[obs_nlon,obs_nlat,1]))

      diffT=kppT-obsT
      net_flux=kppsolar+kppnsolar
      net_flux[where(net_flux eq 2e20)]=!Values.F_NaN

      diffT_forced=kppT-forced_sst(*,*,j)
      diff_net_flux=net_flux-forced_net_flux(*,*,j)

      psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/drift/hadgem3kpp_mean_state_drift_sst_biases.'+years(i)+months(j)+'.xgspm_ind3.kpp-minus-obs.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=400
      MAP,/hires,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
      LEVS,MANUAL=levels_diff
      CS,SCALE=1,NCOLS=N_ELEMENTS(levels_diff)+1
      CON,X=kpp_longitude,Y=kpp_latitude,FIELD=diffT,/NOLINES,CB_TITLE='SST bias (K)',$
          TITLE='SST bias for '+months(j)+' in year '+years(i)
      LEVS,MANUAL=levels_flux
      CON,X=kpp_longitude,Y=kpp_latitude,FIELD=net_flux,NEGATIVE_STYLE=1,POSITIVE_STYLE=2,/NOFILL
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/drift/hadgem3kpp_mean_state_drift_sst_biases.'+years(i)+months(j)+'.xgspm_ind3.kpp-minus-ensmean_forced.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=400
      MAP,/hires,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
      LEVS,MANUAL=levels_diff
      CS,SCALE=1,NCOLS=N_ELEMENTS(levels_diff)+1
      CON,X=kpp_longitude,Y=kpp_latitude,FIELD=diffT_forced,/NOLINES,CB_TITLE='SST bias (K)',$
          TITLE='SST bias against ensmean of forced, corrected runs for '+months(j)+' in year '+years(i)
      LEVS,MANUAL=levels_flux
      CON,X=kpp_longitude,Y=kpp_latitude,FIELD=diff_net_flux,NEGATIVE_STYLE=1,POSITIVE_STYLE=2,/NOFILL
      AXES
      PSCLOSE,/NOVIEW

   ENDFOR
ENDFOR

STOP
END

