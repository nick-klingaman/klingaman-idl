PRO mjo_indices_ew_power_ratio

n_models=9
um6='/home/ss901165/um_output6'
gc2='/group_workspaces/jasmin2/klingaman/metum/gc2'
gc3='/group_workspaces/jasmin2/klingaman/metum/gc3'
all_names=strarr(n_models)
all_ewprecip=fltarr(n_models)
all_e2wprecip=fltarr(n_models)
all_ewu850=fltarr(n_models)
all_e2wu850=fltarr(n_models)
all_syms=intarr(n_models)
all_colors=strarr(n_models)
FOR i=0,n_models-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         u850_infile='/home/users/npklingaman/datasets/MJO_INDICES/noaa_eraint_trmm_obs.jan-dec_dmeans_anom-3harm_space-time_powerspec.1999-2012.u850.2.5x2.5.nc'
         precip_infile='/home/users/npklingaman/datasets/MJO_INDICES/noaa_eraint_trmm_obs.jan-dec_dmeans_anom-3harm_space-time_powerspec.1999-2008.precip.2.5x2.5.nc'
         all_names(i)='TRMM-ERA'
         a=1
         all_syms(i)=3
         all_colors(i)='black'
      END
      2 : BEGIN
         u850_infile=gc2+'/anqjm/hadgem3_gc2_n96_orca025.jan-dec_dmeans_anom-3harm_space-time_powerspec.years1-41.u850.2.5x2.5.nc'
         precip_infile=gc2+'/anqjm/hadgem3_gc2_n96_orca025.jan-dec_dmeans_anom-3harm_space-time_powerspec.years1-41.precip.2.5x2.5.nc'
         all_names(i)='GC2 N96'
         a=1
         all_syms(i)=2
         all_colors(i)='blue'
      END
      1 : BEGIN
         u850_infile=gc2+'/antia/hadgem3_ga6_n96.jan-dec_dmeans_anom-3harm_space-time_powerspec.years1-27.u850.2.5x2.5.nc'
         precip_infile=gc2+'/antia/hadgem3_ga6_n96.jan-dec_dmeans_anom-3harm_space-time_powerspec.years1-27.precip.2.5x2.5.nc'
         all_names(i)='GA6 N96'
         a=1
         all_syms(i)=1
         all_colors(i)='blue'
      END
      3 : BEGIN
         u850_infile=gc2+'/antib/hadgem3_ga6_n216.jan-dec_dmeans_anom-3harm_space-time_powerspec.years1-27.u850.2.5x2.5.nc'
         precip_infile=gc2+'/antib/hadgem3_ga6_n216.jan-dec_dmeans_anom-3harm_space-time_powerspec.years1-27.precip.2.5x2.5.nc'
         all_names(i)='GA6 N216'
         a=1
         all_syms(i)=4
         all_colors(i)='blue'
      END
      4 : BEGIN
         u850_infile=gc2+'/anqjn/hadgem3_gc2_n216.jan-dec_dmeans_anom-3harm_space-time_powerspec.years1-29.u850.2.5x2.5.nc'
         precip_infile=gc2+'/anqjn/hadgem3_gc2_n216.jan-dec_dmeans_anom-3harm_space-time_powerspec.years1-29.precip.2.5x2.5.nc'
         all_names(i)='GC2 N216'
         a=1
         all_syms(i)=5
         all_colors(i)='blue'
      END
      5 : BEGIN
         u850_infile=gc3+'/u-ab642/mjo/ab642a.jan-dec_dmeans_anom-3harm_space-time_powerspec.1982-2008.u850.2.5x2.5.nc'
         precip_infile=gc3+'/u-ab642/mjo/ab642a.jan-dec_dmeans_anom-3harm_space-time_powerspec.1982-2008.precip.2.5x2.5.nc'
         all_names(i)='GA7 N96'
         a=1
         all_syms(i)=1
         all_colors(i)='red'
      END 
      6 : BEGIN
         u850_infile=gc3+'/u-ab673/mjo/ab673a.jan-dec_dmeans_anom-3harm_space-time_powerspec.2013-2112.u850.2.5x2.5.nc'
         precip_infile=gc3+'/u-ab673/mjo/ab673a.jan-dec_dmeans_anom-3harm_space-time_powerspec.2013-2112.precip.2.5x2.5.nc'
         all_names(i)='GC3 N96'
         a=1
         all_syms(i)=2
         all_colors(i)='red'
      END
      7 : BEGIN
         u850_infile=gc3+'/u-ab680/mjo/ab680a.jan-dec_dmeans_anom-3harm_space-time_powerspec.1982-2008.u850.2.5x2.5.nc'
         precip_infile=gc3+'/u-ab680/mjo/ab680a.jan-dec_dmeans_anom-3harm_space-time_powerspec.1982-2008.precip.2.5x2.5.nc'
         all_names(i)='GA7 N216'
         a=1
         all_syms(i)=4
         all_colors(i)='red'
      END
      8 : BEGIN
         u850_infile=gc3+'/u-ab674/mjo/ab674a.jan-dec_dmeans_anom-3harm_space-time_powerspec.2013-2042.u850.2.5x2.5.nc'
         precip_infile=gc3+'/u-ab674/mjo/ab674a.jan-dec_dmeans_anom-3harm_space-time_powerspec.2013-2042.precip.2.5x2.5.nc'
         all_names(i)='GC3 N216'
         a=1
         all_syms(i)=5
         all_colors(i)='red'
      END 
     ; 1 : BEGIN
     ;    u850_infile=um6+'/xgspm/spacetime_spec_u850.nc'
     ;    precip_infile=um6+'/xgspm/spacetime_spec_precip.nc'
     ;    all_names(i)='MetUM-GOML1 1.5F'
     ;    a=1.0
     ;    all_syms(i)=1
     ; END      
     ; 2 : BEGIN
     ;    u850_infile=um6+'/spcam/cam_iceedge_free/spacetime_spec_u850.nc'
     ;    precip_infile=um6+'/spcam/cam_iceedge_free/spacetime_spec_precip.nc'
     ;    all_names(i)='CAM3-KPP'
     ;    a=1.0
     ;    all_syms(i)=2
     ; END
     ; 3 : BEGIN
     ;    u850_infile=um6+'/spcam/spcam_iceedge_free/spacetime_spec_u850.nc'
     ;    precip_infile=um6+'/spcam/spcam_iceedge_free/spacetime_spec_precip.nc'
     ;    all_names(i)='SPCAM3-KPP'
     ;    a=1.0
     ;    all_syms(i)=4
     ; END      
     ; 4 : BEGIN
     ;    u850_infile=um6+'/mjodiab_20year/spcam/spacetime_spec_u850.nc'
     ;    precip_infile=um6+'/mjodiab_20year/spcam/spacetime_spec_precip.nc'
     ;    all_names(i)='SPCAM3 AMIP'
     ;    a=1.0
     ;    all_syms(i)=5
     ; END      
     ; 5 : BEGIN
     ;    u850_infile=um6+'/mjodiab_20year/spccsm/spacetime_spec_u850.nc'
     ;    precip_infile=um6+'/mjodiab_20year/spccsm/spacetime_spec_precip.nc'
     ;    all_names(i)='SPCCSM3'
     ;    a=1.0
     ;    all_syms(i)=6
     ; END
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

psfile='/home/users/npklingaman/plots/mjo_indices/mjo_indices_ew_power_ratio.gc2_gc3.precip_u850_ewratio.pub.ps'
PSOPEN,file=psfile,TFONT=2,FONT=6,CHARSIZE=180,MARGIN=2500,YOFFSET=500,XOFFSET=1000
GSET,XMIN=0,XMAX=5.5,YMIN=0,YMAX=7
FOR i=0,n_models-1 DO $
   GPLOT,X=all_ewprecip(i),Y=all_ewu850(i),SYM=all_syms(i),SIZE=150,COL=FSC_COLOR(all_colors(i))
AXES,XSTEP=0.5,YSTEP=0.5,YMINOR=0.25,XMINOR=0.25,XTITLE='East/west ratio in precipitation',$
     YTITLE='East/west ratio in 850 hPa zonal wind',NDECS=1
GLEGEND,labels=REVERSE(all_names),LEGPOS=11,SYM=REVERSE(all_syms),LENGTH=0,SIZE=REPLICATE(150,n_models),COL=REVERSE(FSC_COLOR(all_colors))
PSCLOSE

psfile='/home/users/npklingaman/plots/mjo_indices/mjo_indices_ew_power_ratio.gc2_gc3.precip_u850_e2wratio.pub.ps'
PSOPEN,file=psfile,TFONT=2,FONT=6,CHARSIZE=180,MARGIN=2500,YOFFSET=500,XOFFSET=1000
GSET,XMIN=0,XMAX=1.2,YMIN=0,YMAX=2.6
FOR i=0,n_models-1 DO $
   GPLOT,X=all_e2wprecip(i),Y=all_e2wu850(i),SYM=all_syms(i),SIZE=150,COL=FSC_COLOR(all_colors(i))
AXES,XSTEP=0.1,YSTEP=0.2,YMINOR=0.1,XMINOR=0.05,XTITLE='East!U2!N/west ratio in precipitation',$
     YTITLE='East!U2!N/west ratio in 850 hPa zonal wind',NDECS=1
GLEGEND,labels=REVERSE(all_names),LEGPOS=11,SYM=REVERSE(all_syms),LENGTH=0,SIZE=REPLICATE(150,n_models),COL=REVERSE(FSC_COLOR(all_colors))
PSCLOSE

STOP
END
