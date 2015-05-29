PRO hadgem3kpp_mean_state_boxavg_ts_fluximbalance_xgspp
  
; Plot monthly-mean, domain-average SSTs from corrected (forced) and coupled
; simulations.  Also plot imbalance in surface fluxes over domain.

  months=['jan15-feb15','feb15-mar15','mar15-apr15','apr15-may15','may15-jun15','jun15-jul15',$
          'jul15-aug15','aug15-sep15','sep15-oct15','oct15-nov15','nov15-dec15','dec15-jan15']
  forced_runs=['xgspo_i2','xgspo_i3','xgspo_i4','xgspo_i5','xgspo_i6','xgspo_i7',$
               'xgspo_i8','xgspo_i9','xgspo_j0','xgspo_j1']
  coupled_years=['i2','i3','i4','i5','i6'];,'i7','i8','i9','j0','j1',$
                 ;'j2','j3','j4','j5','j6','j7','j8','j9','k0','k1']
  
  forced_indir='/net/niagara/export/niagara/data-02/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.0xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421_ind'
  coupled_indir='/home/ss901165/um_output5/xgspp/kpp_ocean'
  obs_indir='/home/ss901165/datasets/METO_OCEAN_ANALYSIS/n96'

  n_months=N_ELEMENTS(months)
  n_forced_runs=N_ELEMENTS(forced_runs)
  n_coupled_years=N_ELEMENTS(coupled_years)
  
  box=[-30,40,30,200]
  box_name='cpl'
  sst_max=301
  sst_min=299
  flux_max=50
  flux_min=-50

  mask_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/landfrac_n96_hadgem3-7.8.nc'
  mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
  mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
  DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
  mask_nlon=N_ELEMENTS(mask_longitude)
  mask_nlat=N_ELEMENTS(mask_latitude)
  mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                               offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                               count=[mask_nlon,mask_nlat,1,1]))

                                ; Read monthly means of observations
  obs_ssts_aavg=fltarr(n_months)
  obs_ssts=fltarr(mask_nlon,mask_nlat,n_months)
  FOR i=0,n_months-1 DO BEGIN
     obs_infile=obs_indir+'/meto_ocean_analysis.'+months(i)+'_mmean_clim.1980-2009.pot_temp.amip_type.n96.nc'
     obs_longitude=OPEN_AND_EXTRACT(obs_infile,'longitude')
     obs_latitude=OPEN_AND_EXTRACT(obs_infile,'latitude')
     DEFINE_BOUNDARIES,box,obs_latitude,obs_longitude,obs_box_tx,/LIMIT
     obs_nlon=N_ELEMENTS(obs_longitude)
     obs_nlat=N_ELEMENTS(obs_latitude)
     
     this_obs_sst=REFORM(OPEN_AND_EXTRACT(obs_infile,'temp',$
                                          offset=[obs_box_tx(1),obs_box_tx(0),0],$
                                          count=[obs_nlon,obs_nlat,1]))+273.15
     this_obs_sst[where(mask eq 1)]=!Values.F_NaN
     obs_ssts_aavg(i)=MEAN(this_obs_sst,/NaN)
     obs_ssts(*,*,i)=this_obs_sst
  ENDFOR

                                ; Read monthly means from each forced run and from ensemble-mean of forced runs
  forced_ssts_rmse=fltarr(n_months,n_forced_runs)
  forced_ssts_aavg=fltarr(n_months,n_forced_runs)
  forced_ssts_aavg_ensmean=fltarr(n_months)
  forced_imbalance=fltarr(n_months,n_forced_runs)
  forced_hmix=fltarr(n_months,n_forced_runs)
  FOR i=0,n_forced_runs-1 DO BEGIN
     forced_infile=forced_indir+'/'+forced_runs(i)+'.jan-dec_mmeans.corrected.nc'
     forced_longitude=OPEN_AND_EXTRACT(forced_infile,'longitude')
     forced_latitude=OPEN_AND_EXTRACT(forced_infile,'latitude')
     DEFINE_BOUNDARIES,box,forced_latitude,forced_longitude,forced_box_tx,/LIMIT
     forced_nlon=N_ELEMENTS(forced_longitude)
     forced_nlat=N_ELEMENTS(forced_latitude)

     forced_flux_infile=forced_indir+'/'+forced_runs(i)+'.jan-dec_mmeans.fluxes.nc'
     forced_flux_longitude=OPEN_AND_EXTRACT(forced_flux_infile,'longitude')
     forced_flux_latitude=OPEN_AND_EXTRACT(forced_flux_infile,'latitude')
     DEFINE_BOUNDARIES,box,forced_flux_latitude,forced_flux_longitude,forced_flux_box_tx,/LIMIT
     forced_flux_nlon=N_ELEMENTS(forced_flux_longitude)
     forced_flux_nlat=N_ELEMENTS(forced_flux_latitude)
     
     this_forced_sst=REFORM(OPEN_AND_EXTRACT(forced_infile,'T',$
                                             offset=[forced_box_tx(1),forced_box_tx(0),0,0],$
                                             count=[forced_nlon,forced_nlat,1,n_months]))+273.15
     this_forced_hmix=REFORM(OPEN_AND_EXTRACT(forced_infile,'hmix',$
                                              offset=[forced_box_tx(1),forced_box_tx(0),0],$
                                              count=[forced_nlon,forced_nlat,n_months]))
     this_forced_solar=REFORM(OPEN_AND_EXTRACT(forced_flux_infile,'swf',$
                                               offset=[forced_flux_box_tx(1),forced_flux_box_tx(0),0],$
                                               count=[forced_flux_nlon,forced_flux_nlat,n_months]))
     this_forced_longwave=REFORM(OPEN_AND_EXTRACT(forced_flux_infile,'lwf',$
                                                offset=[forced_flux_box_tx(1),forced_flux_box_tx(0),0],$
                                                count=[forced_flux_nlon,forced_flux_nlat,n_months]))
     this_forced_latent=REFORM(OPEN_AND_EXTRACT(forced_flux_infile,'lhf',$
                                                offset=[forced_flux_box_tx(1),forced_flux_box_tx(0),0],$
                                                count=[forced_flux_nlon,forced_flux_nlat,n_months]))
     this_forced_sensible=REFORM(OPEN_AND_EXTRACT(forced_flux_infile,'shf',$
                                                  offset=[forced_flux_box_tx(1),forced_flux_box_tx(0),0],$
                                                  count=[forced_flux_nlon,forced_flux_nlat,n_months]))

     this_forced_diff=this_forced_solar+this_forced_longwave+this_forced_latent+this_forced_sensible

     FOR j=0,n_months-1 DO BEGIN
        temp=REFORM(this_forced_sst(*,*,j))
        temp[where(mask eq 1)]=!Values.F_NaN
        forced_ssts_rmse(j,i)=SQRT(MEAN((temp-obs_ssts(*,*,j))^2,/NaN))
        forced_ssts_aavg(j,i)=MEAN(temp,/NaN)
        temp=REFORM(this_forced_diff(*,*,j))
        temp[where(mask eq 1)]=!Values.F_NaN
        forced_imbalance(j,i)=MEAN(temp,/NaN)
        temp=REFORM(this_forced_hmix(*,*,j))
        temp[where(mask ne 0)]=!Values.F_NaN
        forced_hmix(j,i)=MEAN(temp,/NaN)   
     ENDFOR     
  ENDFOR
  forced_infile=forced_indir+'/ensmean.jan-dec_mmeans.corrected.nc'
  this_forced_sst_ensmean=REFORM(OPEN_AND_EXTRACT(forced_infile,'T',$
                                                  offset=[forced_box_tx(1),forced_box_tx(0),0,0],$
                                                  count=[forced_nlon,forced_nlat,1,n_months]))+273.15
  FOR j=0,n_months-1 DO BEGIN
     temp=REFORM(this_forced_sst_ensmean(*,*,j))
     temp[where(mask eq 1)]=!Values.F_NaN
     forced_ssts_aavg_ensmean(j)=MEAN(temp,/NaN)
  ENDFOR

  coupled_ssts_rmse=fltarr(n_months,n_coupled_years)
  coupled_ssts_aavg=fltarr(n_months,n_coupled_years)
  coupled_diff=fltarr(n_months,n_coupled_years)
  coupled_hmix=fltarr(n_months,n_coupled_years)
  FOR i=0,n_coupled_years-1 DO BEGIN
     FOR j=0,n_months-1 DO BEGIN
        coupled_infile=coupled_indir+'/KPPocean.'+months(j)+'_mmean.'+coupled_years(i)+'.nc'
        coupled_longitude=OPEN_AND_EXTRACT(coupled_infile,'longitude')
        coupled_latitude=OPEN_AND_EXTRACT(coupled_infile,'latitude')
        DEFINE_BOUNDARIES,box,coupled_latitude,coupled_longitude,coupled_box_tx,/LIMIT
        coupled_nlon=N_ELEMENTS(coupled_longitude)
        coupled_nlat=N_ELEMENTS(coupled_latitude)

        this_coupled_sst=REFORM(OPEN_AND_EXTRACT(coupled_infile,'T',$
                                                 offset=[coupled_box_tx(1),coupled_box_tx(0),0,0],$
                                                 count=[coupled_nlon,coupled_nlat,1,1]))+273.15
        this_coupled_hmix=REFORM(OPEN_AND_EXTRACT(coupled_infile,'hmix',$
                                                  offset=[coupled_box_tx(1),coupled_box_tx(0),0],$
                                                  count=[coupled_nlon,coupled_nlat,1]))
        this_coupled_solar=REFORM(OPEN_AND_EXTRACT(coupled_infile,'solar_in',$
                                                  offset=[coupled_box_tx(1),coupled_box_tx(0),0],$
                                                  count=[coupled_nlon,coupled_nlat,1]))
        this_coupled_nsolar=REFORM(OPEN_AND_EXTRACT(coupled_infile,'nsolar_in',$
                                                   offset=[coupled_box_tx(1),coupled_box_tx(0),0],$
                                                   count=[coupled_nlon,coupled_nlat,1]))
        this_coupled_diff=this_coupled_solar+this_coupled_nsolar

        this_coupled_sst[where(mask eq 1)]=!Values.F_NaN
        coupled_ssts_rmse(j,i)=SQRT(MEAN((this_coupled_sst-obs_ssts(*,*,j))^2,/NaN))
        coupled_ssts_aavg(j,i)=MEAN(this_coupled_sst,/NaN)
        this_coupled_diff[where(mask eq 1)]=!Values.F_NaN
        coupled_diff(j,i)=MEAN(this_coupled_diff,/NaN)
        this_coupled_hmix[where(mask eq 1)]=!Values.F_NaN       
        coupled_hmix(j,i)=MEAN(this_coupled_hmix,/NaN)
     ENDFOR
  ENDFOR

  psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/boxavg_ts/hadgem3kpp_mean_state_boxavg_ts_fluximbalance.'+box_name+'_sst.xgspp.ps'
  PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2500,SPACE2=500,XOFFSET=1500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=200
  GSET,XMIN=0,XMAX=n_months,YMIN=sst_min,YMAX=sst_max,TITLE='Monthly mean SSTs over '+box_name+''
  FOR i=0,n_months-1 DO BEGIN
     GPLOT,X=i+0.3,Y=forced_ssts_aavg_ensmean(i),SYM=6,COL=FSC_COLOR('red')
     GPLOT,X=[i+0.3,i+0.3],Y=[forced_ssts_aavg_ensmean(i),MAX(forced_ssts_aavg(i,*))],COL=FSC_COLOR('red')
     GPLOT,X=[i+0.3,i+0.3],Y=[forced_ssts_aavg_ensmean(i),MIN(forced_ssts_aavg(i,*))],COL=FSC_COLOR('red')
     GPLOT,X=i+0.5,Y=obs_ssts_aavg(i),SYM=6,COL=FSC_COLOR('black')
     GPLOT,X=i+0.7,Y=MEAN(coupled_ssts_aavg(i,*)),COL=FSC_COLOR('blue'),SYM=6
     GPLOT,X=[i+0.7,i+0.7],Y=[MAX(coupled_ssts_aavg(i,*)),MIN(coupled_ssts_aavg(i,*))],COL=FSC_COLOR('blue')
  ENDFOR
  AXES,XVALS=indgen(n_months)+0.5,XLABELS=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'],$
       XTITLE='Month',YTITLE='Area-averaged SST (K)',YSTEP=(sst_max-sst_min)/10.,YMINOR=(sst_max-sst_min)/20.,$
       NDECS=1
  PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/boxavg_ts/hadgem3kpp_mean_state_boxavg_ts_fluximbalance.'+box_name+'_hmix.xgspp.ps'
  PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2500,SPACE2=500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=200
  GSET,XMIN=0,XMAX=n_months,YMIN=50,YMAX=20,TITLE='Monthly mean hmix over '+box_name+''
  FOR i=0,n_months-1 DO BEGIN
     GPLOT,X=i+0.4,Y=ABS(MEAN(forced_hmix(i,*))),SYM=6,COL=FSC_COLOR('red')
     GPLOT,X=[i+0.4,i+0.4],Y=[ABS(MEAN(forced_hmix(i,*))),ABS(MAX(forced_hmix(i,*)))],COL=FSC_COLOR('red')
     GPLOT,X=[i+0.4,i+0.4],Y=[ABS(MEAN(forced_hmix(i,*))),ABS(MIN(forced_hmix(i,*)))],COL=FSC_COLOR('red')
;     GPLOT,X=i+0.5,Y=obs_ssts(i),SYM=6,COL=FSC_COLOR('black')
     GPLOT,X=i+0.6,Y=MEAN(coupled_hmix(i,*)),COL=FSC_COLOR('blue'),SYM=6
     GPLOT,X=[i+0.6,i+0.6],Y=[MAX(coupled_hmix(i,*)),MIN(coupled_hmix(i,*))],COL=FSC_COLOR('blue')
  ENDFOR
  AXES,XVALS=indgen(n_months)+0.5,XLABELS=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'],$
       XTITLE='Month',YTITLE='Area-averaged hmix (m)',YSTEP=-2,YMINOR=-1,NDECS=1
  PSCLOSE,/NOVIEW

  psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/boxavg_ts/hadgem3kpp_mean_state_boxavg_ts_fluximbalance.'+box_name+'_fluximbalance.xgspp.ps'
  PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2500,SPACE2=500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=200
  GSET,XMIN=0,XMAX=n_months,YMIN=flux_min,YMAX=flux_max,TITLE='Monthly mean net surface flux over '+box_name+' (ocean only)'
  FOR i=0,n_months-1 DO BEGIN
     GPLOT,X=i+0.4,Y=MEAN(forced_imbalance(i,*)),SYM=6,COL=FSC_COLOR('red')
     GPLOT,X=[i+0.4,i+0.4],Y=[MEAN(forced_imbalance(i,*)),MAX(forced_imbalance(i,*))],COL=FSC_COLOR('red')
     GPLOT,X=[i+0.4,i+0.4],Y=[MEAN(forced_imbalance(i,*)),MIN(forced_imbalance(i,*))],COL=FSC_COLOR('red')
;     GPLOT,X=i+0.5,Y=obs_ssts(i),SYM=6,COL=FSC_COLOR('black')
     GPLOT,X=i+0.6,Y=MEAN(coupled_diff(i,*)),COL=FSC_COLOR('blue'),SYM=6
     GPLOT,X=[i+0.6,i+0.6],Y=[MAX(coupled_diff(i,*)),MIN(coupled_diff(i,*))],COL=FSC_COLOR('blue')
  ENDFOR
  AXES,XVALS=indgen(n_months)+0.5,XLABELS=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'],$
       XTITLE='Month',YTITLE='Area-averaged net surface flux (positive into ocean)',YSTEP=(flux_max-flux_min)/10.,YMINOR=(flux_max-flux_min)/20.,$
       NDECS=1
  PSCLOSE,/NOVIEW

  psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/boxavg_ts/hadgem3kpp_mean_state_boxavg_ts_fluximbalance.'+box_name+'_fluximbalance_ts.xgspo.ps'
  PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2500,SPACE2=500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=200
  GSET,XMIN=0,XMAX=n_months*n_coupled_years,YMIN=sst_min,YMAX=sst_max,TITLE='Monthly mean SST (blue) and net surface flux (red) over '+box_name+' (ocean only)'
  GPLOT,X=indgen(n_months*n_coupled_years),Y=REFORM(coupled_ssts_aavg,[n_months*n_coupled_years]),COL=FSC_COLOR('blue')
  smoothed=SMOOTH(REFORM(coupled_ssts_aavg,[n_months*n_coupled_years]),3)
  GPLOT,X=indgen(n_months*n_coupled_years-2)+1,Y=smoothed(1:n_months*n_coupled_years-2),COL=FSC_COLOR('blue'),STYLE=2
  AXES,XSTEP=12,XMINOR=3,XTITLE='Month since start of simulation',YSTEP=(sst_max-sst_min)/10.,YMINOR=(sst_max-sst_min)/20.,/NORIGHT,$
       YTITLE='Area-averaged SST'
  GSET,XMIN=0,XMAX=n_months*n_coupled_years,YMIN=flux_min,YMAX=flux_max
  GPLOT,X=indgen(n_months*n_coupled_years),Y=REFORM(coupled_diff,[n_months*n_coupled_years]),COL=FSC_COLOR('red')
  smoothed=SMOOTH(REFORM(coupled_diff,[n_months*n_coupled_years]),3)
  GPLOT,X=indgen(n_months*n_coupled_years-2)+1,Y=smoothed(1:n_months*n_coupled_years-2),COL=FSC_COLOR('red'),STYLE=2  
  AXES,YSTEP=(flux_max-flux_min)/10.,YMINOR=(flux_max-flux_min)/20.,YTITLE='Area-averaged net surface flux (positive into ocean)',/ONLYRIGHT
  PSCLOSE,/NOVIEW

  psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/boxavg_ts/hadgem3kpp_mean_state_boxavg_ts_fluximbalance.'+box_name+'_sst_bias_ts.xgspp.ps'
  PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2500,SPACE2=500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=200
  GSET,XMIN=0,XMAX=n_months*n_coupled_years,YMIN=-0.5,YMAX=0.5,TITLE='Monthly mean SST bias over '+box_name+' (ocean only)'
  coupled_sst_bias=fltarr(n_months,n_coupled_years)
  FOR i=0,n_coupled_years-1 DO $
     coupled_sst_bias(*,i)=coupled_ssts_aavg(*,i)-obs_ssts_aavg(*)
  GPLOT,X=indgen(n_months*n_coupled_years),Y=REFORM(coupled_sst_bias,[n_months*n_coupled_years]),COL=FSC_COLOR('blue')
  smoothed=SMOOTH(REFORM(coupled_sst_bias,[n_months*n_coupled_years]),3)
  GPLOT,X=indgen(n_months*n_coupled_years-2)+1,Y=smoothed(1:n_months*n_coupled_years-2),COL=FSC_COLOR('blue'),STYLE=2
  AXES,XSTEP=12,XMINOR=3,XTITLE='Month since start of simulation',YSTEP=0.1,YMINOR=0.05,$
       YTITLE='Area-averaged SST bias (K)',NDECS=2
  PSCLOSE,/NOVIEW

  psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/boxavg_ts/hadgem3kpp_mean_state_boxavg_ts_fluximbalance.'+box_name+'_sst_rmse.xgspp.ps'
  PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2500,SPACE2=500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=200
  GSET,XMIN=0,XMAX=n_months,YMIN=0,YMAX=2.5,TITLE='RMSE in SSTs against obs climatology over '+box_name+' (ocean only)'
  FOR i=0,n_months-1 DO BEGIN
     GPLOT,X=i+0.4,Y=MEAN(forced_ssts_rmse(i,*)),SYM=6,COL=FSC_COLOR('red')
     GPLOT,X=[i+0.4,i+0.4],Y=[MEAN(forced_ssts_rmse(i,*)),MAX(forced_ssts_rmse(i,*))],COL=FSC_COLOR('red')
     GPLOT,X=[i+0.4,i+0.4],Y=[MEAN(forced_ssts_rmse(i,*)),MIN(forced_ssts_rmse(i,*))],COL=FSC_COLOR('red')
     GPLOT,X=i+0.6,Y=MEAN(coupled_ssts_rmse(i,*)),COL=FSC_COLOR('blue'),SYM=6
     GPLOT,X=[i+0.6,i+0.6],Y=[MAX(coupled_ssts_rmse(i,*)),MIN(coupled_ssts_rmse(i,*))],COL=FSC_COLOR('blue')
  ENDFOR
  AXES,XVALS=indgen(n_months)+0.5,XLABELS=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'],$
       XTITLE='Month',YTITLE='RMSE in SSTs (K)',YSTEP=0.1,YMINOR=0.05,$
       NDECS=2
  PSCLOSE,/NOVIEW

  psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/boxavg_ts/hadgem3kpp_mean_state_boxavg_ts_fluximbalance.'+box_name+'_sst_rmse_ts.xgspp.ps'
  PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2500,SPACE2=500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=200
  GSET,XMIN=0,XMAX=n_months*n_coupled_years,YMIN=0,YMAX=2.5,TITLE='RMSE in SSTs against obs climatology over '+box_name+' (ocean only)'
  GPLOT,X=indgen(n_months*n_coupled_years),Y=REFORM(coupled_ssts_rmse,[n_months*n_coupled_years]),COL=FSC_COLOR('blue')
  smoothed=SMOOTH(REFORM(coupled_ssts_rmse,[n_months*n_coupled_years]),3)
  GPLOT,X=indgen(n_months*n_coupled_years-2)+1,Y=smoothed(1:n_months*n_coupled_years-2),COL=FSC_COLOR('blue'),STYLE=2
  AXES,XSTEP=12,XMINOR=3,XTITLE='Month since start of simulation',YSTEP=0.1,YMINOR=0.05,$
       YTITLE='RMSE in SSTs (K)',NDECS=2
  PSCLOSE,/NOVIEW

STOP
END
