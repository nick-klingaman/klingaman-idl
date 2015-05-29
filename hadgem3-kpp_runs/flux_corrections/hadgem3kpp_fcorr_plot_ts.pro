PRO hadgem3kpp_fcorr_plot_ts

uncorr_dir='/home/ss901165/kpp_ocean3/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_junstart_ukmo_sal60_amip/xfzbj_i0'
uncorr_files=['0045']
;uncorr_files=['0045','0075','0105','0135','0165','0195','0225','0255']
n_uncorr_files=N_ELEMENTS(uncorr_files)

corr_dir='/export/mango/data-10/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_junstart_ukmo_sal60_amip/xfzbj_i0'
corr_files=['0045']
;corr_files=['0045','0075','0105','0135','0165','0195','0225']
n_corr_files=N_ELEMENTS(corr_files)

obs_file='/home/ss901165/datasets/METO_OCEAN_ANALYSIS/n96/meto_ocean_analysis.jun-may_dmean_clim.1980-2009.pot_temp.amip_type.n96.nc'

flxcorr_file='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/heat_corrections/hadgem3_1.5xentrain_ensmean_1mtop_3hr_junstart_ukmo_sal60_amip.jun-may_mmeans.flxcorr.n96.nc'

n_times_per_file=30
n_uncorr_times=uncorr_files(n_uncorr_files-1)-15
n_corr_times=corr_files(n_corr_files-1)-15

pt=[18.75,127.5,0]

kpp_grid_file=uncorr_dir+'/KPPocean_'+uncorr_files(0)+'_means.nc'
kpp_latitude=OPEN_AND_EXTRACT(kpp_grid_file,'latitude')
kpp_longitude=OPEN_AND_EXTRACT(kpp_grid_file,'longitude')
kpp_z=OPEN_AND_EXTRACT(kpp_grid_file,'z')
kpp_latpt=NEAREST(kpp_latitude,pt(0))
kpp_lonpt=NEAREST(kpp_longitude,pt(1))
kpp_zpt=NEAREST(kpp_z,pt(2))

obs_latitude=OPEN_AND_EXTRACT(obs_file,'latitude')
obs_longitude=OPEN_AND_EXTRACT(obs_file,'longitude')
obs_z=OPEN_AND_EXTRACT(obs_file,'depth')
obs_latpt=NEAREST(obs_latitude,pt(0))
obs_lonpt=NEAREST(obs_longitude,pt(1))
obs_zpt=NEAREST(obs_z,pt(2)*(-1.))

uncorr_temps=fltarr(n_uncorr_times)
corr_temps=fltarr(n_corr_times)
obs_temps=fltarr(n_uncorr_times)
expcorr=fltarr(n_uncorr_files)

FOR i=0,n_uncorr_files-1 DO BEGIN
   uncorr_file=uncorr_dir+'/KPPocean_'+uncorr_files(i)+'_means.nc'
   temp=REFORM(OPEN_AND_EXTRACT(uncorr_file,'T',$
                         offset=[kpp_lonpt,kpp_latpt,kpp_zpt,0],$
                         count=[1,1,1,n_times_per_file]))
   uncorr_temps(i*n_times_per_file:(i+1)*n_times_per_file-1)=temp
ENDFOR
FOR i=0,n_corr_files-1 DO BEGIN
   corr_file=corr_dir+'/KPPocean_'+corr_files(i)+'_means.nc'
   temp=REFORM(OPEN_AND_EXTRACT(corr_file,'T',$
                         offset=[kpp_lonpt,kpp_latpt,kpp_zpt,0],$
                         count=[1,1,1,n_times_per_file]))
   corr_temps(i*n_times_per_file:(i+1)*n_times_per_file-1)=temp
ENDFOR
obs_temps=REFORM(OPEN_AND_EXTRACT(obs_file,'temp',$
                           offset=[obs_lonpt,obs_latpt,obs_zpt,15],$
                           count=[1,1,1,n_uncorr_times]))
expcorr=REFORM(OPEN_AND_EXTRACT(flxcorr_file,'expcorr',$
                                offset=[kpp_lonpt,kpp_latpt,kpp_zpt,0],$
                                count=[1,1,1,n_uncorr_files]))

expcorr_temps=fltarr(n_uncorr_times)
FOR i=0,n_corr_times-1 DO BEGIN
   print,i,corr_temps(i/30*30),expcorr(i/30)*(i MOD 30)/15.
   expcorr_temps(i)=corr_temps(i/30*30)+expcorr(i/30)*(i MOD 30)/15.
ENDFOR

psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_plot_ts_ensmean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=200,XOFFSET=1000,YOFFSET=1800,TFONT=2,TCHARSIZE=90,SPACE3=400
GSET,XMIN=0,XMAX=n_uncorr_times,YMIN=25,YMAX=33

GPLOT,X=findgen(n_uncorr_times)+0.5,Y=obs_temps,COL=FSC_COLOR('black')
FOR i=15,n_uncorr_times,30 DO BEGIN
   GPLOT,X=findgen(30)+i-14.5,Y=uncorr_temps(i-15:i+14),COL=FSC_COLOR('red')
   GPLOT,X=i,Y=MEAN(uncorr_temps(i-15:i+14)),SYM=4,/NOLINES,COL=FSC_COLOR('red')
   GPLOT,X=i,Y=MEAN(obs_temps(i-15:i+14)),SYM=4,/NOLINES,COL=FSC_COLOR('black')
   GPLOT,X=findgen(30)+i-14.5,Y=expcorr_temps(i-15:i+14),COL=FSC_COLOR('purple')
ENDFOR
FOR i=15,n_corr_times,30 DO BEGIN
   GPLOT,X=i,Y=MEAN(corr_temps(i-15:i+14)),SYM=4,/NOLINES,COL=FSC_COLOR('blue')
   GPLOT,X=findgen(30)+i-14.5,Y=corr_temps(i-15:i+14),COL=FSC_COLOR('blue')
ENDFOR

FOR i=30,n_uncorr_times,30 DO $
   GPLOT,X=[i,i],Y=[25,33],STYLE=2
AXES,XSTEP=10,YSTEP=1,YMINOR=0.5,XMINOR=5,XTITLE='Time minus 15 days'

PSCLOSE

STOP
END

