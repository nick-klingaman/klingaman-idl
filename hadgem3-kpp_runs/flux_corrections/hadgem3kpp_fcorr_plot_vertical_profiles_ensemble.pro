PRO hadgem3kpp_fcorr_plot_vertical_profiles_ensemble
  
; Plot vertical profiles of flux corrections from ensembles of integrations
; Compare fcorr from individual runs to the ensemble-mean correction.

pt=[5,90]  ; [lat,lon]
pt_str='0N70E'
months_str=['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']
kpp_times=['0045','0075','0105','0135','0165','0195','0225','0255','0285','0315','0345','0375']
obs_months=['jan15-feb15','feb15-mar15','mar15-apr15','apr15-may15','may15-jun15','jun15-jul15','jul15-aug15',$
            'aug15-sep15','sep15-oct15','oct15-nov15','nov15-dec15','dec15-jan15']
n_months=N_ELEMENTS(obs_months)

runs=['xgspj_i2','xgspj_i3','xgspj_i4','xgspj_i5','xgspj_i6','xgspj_i7',$
      'xgspj_i8'];,'xgspj_i9','xgspj_j0','xgspj_j1']
n_runs=N_ELEMENTS(runs)

ensmean_fcorr_file='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/heat_corrections/hadgem3_1.5xentrain_ensmean_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421_ind.jan-dec_mmeans.flxcorr.n96.nc'
ind_indir='/export/mango/data-10/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421_ind'
ensmean_indir='/export/mango/data-10/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421_ind_rerun'
obs_indir='/home/ss901165/datasets/METO_OCEAN_ANALYSIS/n96'

longitude=OPEN_AND_EXTRACT(ensmean_fcorr_file,'longitude')
latitude=OPEN_AND_EXTRACT(ensmean_fcorr_file,'latitude')
z=OPEN_AND_EXTRACT(ensmean_fcorr_file,'z')
nz=N_ELEMENTS(z)
pt_lat=NEAREST(latitude,pt(0))
pt_lon=NEAREST(longitude,pt(1))

ensmean_fcorr=fltarr(n_months,nz)
ind_fcorr=fltarr(n_runs,n_months,nz)
ind_sst_bias=fltarr(n_runs,n_months)
ensmean_sst_bias=fltarr(n_runs,n_months)
ind_hmix=fltarr(n_runs,n_months)
ensmean_hmix=fltarr(n_runs,n_months)

FOR i=0,n_months-1 DO BEGIN
   print,'---> '+months_str(i)
   obs_infile=obs_indir+'/meto_ocean_analysis.'+obs_months(i)+'_mmean_clim.1980-2009.pot_temp.amip_type.n96.nc'
   obs_longitude=OPEN_AND_EXTRACT(obs_infile,'longitude')
   obs_latitude=OPEN_AND_EXTRACT(obs_infile,'latitude')
   obs_pt_lat=NEAREST(obs_latitude,pt(0))
   obs_pt_lon=NEAREST(obs_longitude,pt(1))

   ensmean_fcorr(i,*)=REFORM(OPEN_AND_EXTRACT(ensmean_fcorr_file,'fcorr',$
                                         offset=[pt_lon,pt_lat,0,i],$
                                         count=[1,1,nz,1]))
   obs_sst=REFORM(OPEN_AND_EXTRACT(obs_infile,'temp',$
                                   offset=[obs_pt_lon,obs_pt_lat,0],$
                                   count=[1,1,1]))
   FOR j=0,n_runs-1 DO BEGIN
      ind_fcorr_file='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/heat_corrections/'+runs(j)+$
                     '_flxcorr_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421_ind.jan-dec_mmeans.flxcorr.n96.nc'
      
      this_ind_fcorr=REFORM(OPEN_AND_EXTRACT(ind_fcorr_file,'fcorr',$
                                             offset=[pt_lon,pt_lat,0,i],$
                                             count=[1,1,nz,1]))
      ind_fcorr(j,i,*)=this_ind_fcorr

      ind_infile=ind_indir+'/'+runs(j)+'/KPPocean_'+kpp_times(i)+'_means.nc'
      ensmean_infile=ensmean_indir+'/'+runs(j)+'/KPPocean_'+kpp_times(i)+'_means.nc'
      ind_sst=MEAN(REFORM(OPEN_AND_EXTRACT(ind_infile,'T',$
                                           offset=[pt_lon,pt_lat,0,0],$
                                           count=[1,1,1,30])))
      ensmean_sst=MEAN(REFORM(OPEN_AND_EXTRACT(ensmean_infile,'T',$
                                               offset=[pt_lon,pt_lat,0,0],$
                                               count=[1,1,1,30])))
      ind_sst_bias(j,i)=ind_sst-obs_sst
      ensmean_sst_bias(j,i)=ensmean_sst-obs_sst

      ind_hmix(j,i)=MEAN(REFORM(OPEN_AND_EXTRACT(ind_infile,'hmix',$
                                               offset=[pt_lon,pt_lat,0,0],$
                                               count=[1,1,1,30])))
      ensmean_hmix(j,i)=MEAN(REFORM(OPEN_AND_EXTRACT(ensmean_infile,'hmix',$
                                                   offset=[pt_lon,pt_lat,0,0],$
                                                   count=[1,1,1,30])))
   ENDFOR

;   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_plot_vertical_profiles_ensemble.pt_'+pt_str+'.'+months_str(i)+'.ps'
;   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2500,SPACE2=500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=200
;   GSET,XMIN=-5,XMAX=5,YMIN=MAX(ABS(z)),YMAX=MIN(ABS(z)),TITLE='Flux corrections at pt '+pt_str+' for month '+months_str(i)
;   colors=['purple','blue','cyan','orange','red','pink','brown','dark gray','green','maroon','violet red']
;   FOR j=0,n_runs-1 DO $
;      GPLOT,X=REFORM(ind_fcorr(j,*)),Y=ABS(z),COL=FSC_COLOR(colors(j))
;   GPLOT,X=REFORM(ensmean_fcorr(*)),Y=ABS(z),COL=FSC_COLOR('black')
;   AXES,XSTEP=1,XMINOR=0.5,YSTEP=-10,YMINOR=-5,XTITLE='Flux correction (W m!U-3!N)',YTITLE='Depth below surface (m)'
;   PSCLOSE
ENDFOR


ind_fcorr_timemean=fltarr(n_runs,nz)
ensmean_fcorr_timemean=fltarr(nz)
FOR i=0,nz-1 DO BEGIN
   ensmean_fcorr_timemean(i)=MEAN(ensmean_fcorr(*,i))
   FOR j=0,n_runs-1 DO $
      ind_fcorr_timemean(j,i)=MEAN(ind_fcorr(j,*,i))
ENDFOR

psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_plot_vertical_profiles_ensemble.pt_'+pt_str+'.timemean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2500,SPACE2=500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=200
GSET,XMIN=-1.5,XMAX=1.5,YMIN=MAX(ABS(z)),YMAX=MIN(ABS(z)),TITLE='Flux corrections at pt '+pt_str+' for jan-dec mean'
colors=['purple','blue','cyan','orange','red','pink','brown','dark gray','green','maroon','violet red']
FOR j=0,n_runs-1 DO $
   GPLOT,X=REFORM(ind_fcorr_timemean(j,*)),Y=ABS(z),COL=FSC_COLOR(colors(j))
GPLOT,X=REFORM(ensmean_fcorr_timemean(*)),Y=ABS(z),COL=FSC_COLOR('black')
AXES,XSTEP=1,XMINOR=0.5,YSTEP=-10,YMINOR=-5,XTITLE='Flux correction (W m!U-3!N)',YTITLE='Depth below surface (m)'
PSCLOSE

STOP
END


