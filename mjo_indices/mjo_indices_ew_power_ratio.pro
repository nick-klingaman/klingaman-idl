PRO mjo_indices_ew_power_ratio

n_models=6
um6='/home/ss901165/um_output6'
all_names=strarr(n_models)
all_ewprecip=fltarr(n_models)
all_e2wprecip=fltarr(n_models)
all_ewu850=fltarr(n_models)
all_e2wu850=fltarr(n_models)
all_syms=intarr(n_models)
FOR i=0,n_models-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         u850_infile='/home/ss901165/datasets/MJO_INDICES/noaa_eraint_trmm_obs.jan-dec_dmeans_anom-3harm_space-time_powerspec.1999-2012.u850.2.5x2.5.nc'
         precip_infile='/home/ss901165/datasets/MJO_INDICES/noaa_eraint_trmm_obs.jan-dec_dmeans_anom-3harm_space-time_powerspec.1999-2008.precip.2.5x2.5.nc'
         all_names(i)='TRMM-ERA'
         a=1
         all_syms(i)=3
      END
      1 : BEGIN
         u850_infile=um6+'/xgspm/spacetime_spec_u850.nc'
         precip_infile=um6+'/xgspm/spacetime_spec_precip.nc'
         all_names(i)='MetUM-GOML1 1.5F'
         a=1.0
         all_syms(i)=1
      END      
      2 : BEGIN
         u850_infile=um6+'/spcam/cam_iceedge_free/spacetime_spec_u850.nc'
         precip_infile=um6+'/spcam/cam_iceedge_free/spacetime_spec_precip.nc'
         all_names(i)='CAM3-KPP'
         a=1.0
         all_syms(i)=2
      END
      3 : BEGIN
         u850_infile=um6+'/spcam/spcam_iceedge_free/spacetime_spec_u850.nc'
         precip_infile=um6+'/spcam/spcam_iceedge_free/spacetime_spec_precip.nc'
         all_names(i)='SPCAM3-KPP'
         a=1.0
         all_syms(i)=4
      END      
      4 : BEGIN
         u850_infile=um6+'/mjodiab_20year/spcam/spacetime_spec_u850.nc'
         precip_infile=um6+'/mjodiab_20year/spcam/spacetime_spec_precip.nc'
         all_names(i)='SPCAM3 AMIP'
         a=1.0
         all_syms(i)=5
      END      
      5 : BEGIN
         u850_infile=um6+'/mjodiab_20year/spccsm/spacetime_spec_u850.nc'
         precip_infile=um6+'/mjodiab_20year/spccsm/spacetime_spec_precip.nc'
         all_names(i)='SPCCSM3'
         a=1.0
         all_syms(i)=6
      END
   ENDCASE
   
   frequency=OPEN_AND_EXTRACT(precip_infile,'frequency')
   wavenumber=OPEN_AND_EXTRACT(precip_infile,'wavenumber')
   precip_powerspec=OPEN_AND_EXTRACT(precip_infile,'powerspec')
   u850_powerspec=OPEN_AND_EXTRACT(u850_infile,'powerspec')

   east_wavestart=NEAREST(wavenumber,1)
   east_wavestop=NEAREST(wavenumber,3)
   west_wavestart=NEAREST(wavenumber,-3)
   west_wavestop=NEAREST(wavenumber,-1)
   freqstart=NEAREST(frequency,1/80.)
   freqstop=NEAREST(frequency,1/30.)

   all_ewprecip(i)=TOTAL(precip_powerspec(east_wavestart:east_wavestop,freqstart:freqstop))/$
                   TOTAL(precip_powerspec(west_wavestart:west_wavestop,freqstart:freqstop))*a
   all_e2wprecip(i)=TOTAL(precip_powerspec(east_wavestart:east_wavestop,freqstart:freqstop))^2/$
                   TOTAL(precip_powerspec(west_wavestart:west_wavestop,freqstart:freqstop))*a
   all_ewu850(i)=TOTAL(u850_powerspec(east_wavestart:east_wavestop,freqstart:freqstop))/$
                 TOTAL(u850_powerspec(west_wavestart:west_wavestop,freqstart:freqstop))*a
   all_e2wu850(i)=TOTAL(u850_powerspec(east_wavestart:east_wavestop,freqstart:freqstop))^2/$
                  TOTAL(u850_powerspec(west_wavestart:west_wavestop,freqstart:freqstop))*a
   
ENDFOR

psfile='/home/ss901165/idl/mjo_indices/plots/mjo_indices_ew_power_ratio.spcam.precip_u850_ewratio.pub.ps'
PSOPEN,file=psfile,TFONT=2,FONT=6,CHARSIZE=180,MARGIN=2500,YOFFSET=500,XOFFSET=1000
GSET,XMIN=0,XMAX=5.5,YMIN=0,YMAX=7
FOR i=0,n_models-1 DO $
   GPLOT,X=all_ewprecip(i),Y=all_ewu850(i),SYM=all_syms(i),SIZE=150
AXES,XSTEP=0.5,YSTEP=0.5,YMINOR=0.25,XMINOR=0.25,XTITLE='East/west ratio in precipitation',$
     YTITLE='East/west ratio in 850 hPa zonal wind',NDECS=1
GLEGEND,labels=REVERSE(all_names),LEGPOS=11,SYM=REVERSE(all_syms),LENGTH=0,SIZE=REPLICATE(150,n_models)
PSCLOSE

psfile='/home/ss901165/idl/mjo_indices/plots/mjo_indices_ew_power_ratio.spcam.precip_u850_e2wratio.pub.ps'
PSOPEN,file=psfile,TFONT=2,FONT=6,CHARSIZE=180,MARGIN=2500,YOFFSET=500,XOFFSET=1000
GSET,XMIN=0,XMAX=1.2,YMIN=0,YMAX=2.6
FOR i=0,n_models-1 DO $
   GPLOT,X=all_e2wprecip(i),Y=all_e2wu850(i),SYM=all_syms(i),SIZE=150
AXES,XSTEP=0.1,YSTEP=0.2,YMINOR=0.1,XMINOR=0.05,XTITLE='East!U2!N/west ratio in precipitation',$
     YTITLE='East!U2!N/west ratio in 850 hPa zonal wind',NDECS=1
GLEGEND,labels=REVERSE(all_names),LEGPOS=11,SYM=REVERSE(all_syms),LENGTH=0,SIZE=REPLICATE(150,n_models)
PSCLOSE

STOP
END
