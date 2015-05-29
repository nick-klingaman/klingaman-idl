PRO hadgem3kpp_fcorr_sst_biases_12421_ind
  
; Compare SST biases and flux corrections from smoothed and unsmoothed 
; KPP integrations

runids=['xgspj_i2','xgspj_i3','xgspj_i4','xgspj_i5','xgspj_i6','xgspj_i7','xgspj_i8','xgspj_i9','xgspj_j0','xgspj_j1']
n_runs=N_ELEMENTS(runids)

basedir_uncorr='/home/ss901165/kpp_ocean3/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421_ind'
basedir_corr='/export/mango/data-10/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421_ind'

basedir_fcorr='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/heat_corrections'
file_fcorr_ensmean=basedir_fcorr+'/hadgem3_1.5xentrain_ensmean_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421_ind.jan-dec_mmeans.flxcorr.n96.nc'
;file_nosmooth_fcorr=basedir_fcorr+'/hadgem3_1.5xentrain_ensmean_1mtop_3hr_ukmo_sal60_amip_vn78.jan-dec_mmeans.flxcorr.n96.nc'

basedir_obs='/home/ss901165/datasets/METO_OCEAN_ANALYSIS/n96'

kpp_times=['0045','0075','0105','0135','0165','0195','0225','0255','0285','0315','0345','0375']
obs_times=['jan15-feb15','feb15-mar15','mar15-apr15','apr15-may15','may15-jun15','jun15-jul15','jul15-aug15','aug15-sep15','sep15-oct15','oct15-nov15','nov15-dec15','dec15-jan15']
fcorr_offset=[0,1,2,3,4,5,6,7,8,9,10,11]
n_times=N_ELEMENTS(kpp_times)

box=[-30,40,30,200]

mylevs_sst_diff=['-1.9','-1.7','-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1',$
                 '0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5','1.7','1.9']
mylevs_fcorr_surface=['-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5']
mylevs_fcorr_surface_diff=['-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5']

FOR i=0,n_times-1 DO BEGIN
   FOR j=0,n_runs-1 DO BEGIN   
      file_uncorr=basedir_uncorr+'/'+runids(j)+'/KPPocean_'+kpp_times(i)+'_means.nc'
      file_corr=basedir_corr+'/'+runids(j)+'/KPPocean_'+kpp_times(i)+'_means.nc'    
      
      file_obs=basedir_obs+'/meto_ocean_analysis.'+obs_times(i)+'_mmean_clim.1980-2009.pot_temp.amip_type.n96.nc'
      file_fcorr=basedir_fcorr+'/'+runids(j)+'_flxcorr_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421_ind.jan-dec_mmeans.flxcorr.n96.nc'
   
      kpp_longitude=OPEN_AND_EXTRACT(file_uncorr,'longitude')
      kpp_latitude=OPEN_AND_EXTRACT(file_uncorr,'latitude')
      DEFINE_BOUNDARIES,box,kpp_latitude,kpp_longitude,kpp_box_tx,/LIMIT
      kpp_nlon=N_ELEMENTS(kpp_longitude)
      kpp_nlat=N_ELEMENTS(kpp_latitude)
      
      obs_longitude=OPEN_AND_EXTRACT(file_obs,'longitude')
      obs_latitude=OPEN_AND_EXTRACT(file_obs,'latitude')
      DEFINE_BOUNDARIES,box,obs_latitude,obs_longitude,obs_box_tx,/LIMIT
      obs_nlon=N_ELEMENTS(obs_longitude)
      obs_nlat=N_ELEMENTS(obs_latitude)

      sst_uncorr=REFORM(OPEN_AND_EXTRACT(file_uncorr,'T',$
                                         offset=[kpp_box_tx(1),kpp_box_tx(0),0,0],$
                                         count=[kpp_nlon,kpp_nlat,1,30]))
      sst_corr=REFORM(OPEN_AND_EXTRACT(file_corr,'T',$
                                       offset=[kpp_box_tx(1),kpp_box_tx(0),0,0],$
                                       count=[kpp_nlon,kpp_nlat,1,30]))

      IF j eq 0 THEN BEGIN
         sst_uncorr_mmean=fltarr(kpp_nlon,kpp_nlat)
         sst_corr_mmean=fltarr(kpp_nlon,kpp_nlat)
         sst_uncorr_mmean_ensmean=fltarr(kpp_nlon,kpp_nlat)
         sst_corr_mmean_ensmean=fltarr(kpp_nlon,kpp_nlat)
      ENDIF
      FOR m=0,kpp_nlon-1 DO BEGIN
         FOR n=0,kpp_nlat-1 DO BEGIN
            sst_uncorr_mmean(m,n)=MEAN(sst_uncorr(m,n,*))
            sst_corr_mmean(m,n)=MEAN(sst_corr(m,n,*))           
         ENDFOR
      ENDFOR
      sst_corr_mmean_ensmean=sst_corr_mmean_ensmean+sst_corr_mmean/FLOAT(n_runs)      
      sst_uncorr_mmean_ensmean=sst_uncorr_mmean_ensmean+sst_uncorr_mmean/FLOAT(n_runs)

      sst_obs_mmean=REFORM(OPEN_AND_EXTRACT(file_obs,'temp',$
                                            offset=[obs_box_tx(1),obs_box_tx(0),0],$
                                            count=[obs_nlon,obs_nlat,1]))

      fcorr_sfc=REFORM(OPEN_AND_EXTRACT(file_fcorr,'fcorr',$
                                        offset=[kpp_box_tx(1),kpp_box_tx(0),0,fcorr_offset(i)],$
                                        count=[kpp_nlon,kpp_nlat,1,1]))

                                ; Plot SST bias for uncorrected, unsmoothed run
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_sst_biases_12421_ind.uncorr-minus-obs.'+$
             runids(j)+'_'+obs_times(i)+'.sst.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
      LEVS,MANUAL=mylevs_sst_diff
      MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
      CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_uncorr_mmean-sst_obs_mmean,$
          TITLE='Diff in '+obs_times(i)+' mean SST for uncorrected '+runids(j)+' minus UKMO analysis',CB_TITLE='K',$
          /BLOCK,/NOLINES
      PSCLOSE,/NOVIEW

                                ; Plot SST bias for corrected, unsmoothed run
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_sst_biases_12421_ind.corr-minus-obs.'+$
             runids(j)+'_'+obs_times(i)+'.sst.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
      LEVS,MANUAL=mylevs_sst_diff
      MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
      CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_corr_mmean-sst_obs_mmean,$
          TITLE='Diff in '+obs_times(i)+' mean SST for corrected '+runids(j)+' minus UKMO analysis',CB_TITLE='K',$
          /BLOCK,/NOLINES
      PSCLOSE,/NOVIEW
      
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_sst_biases_12421_ind.corr-minus-uncorr.'+$
             runids(j)+'_'+obs_times(i)+'.sst.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
      LEVS,MANUAL=mylevs_sst_diff
      MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
      CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_corr_mmean-sst_uncorr_mmean,$
          TITLE='Diff in '+obs_times(i)+' mean SST for smoothed, corrected '+runids(j)+' minus smoothed, uncorrected',CB_TITLE='K',$
          /BLOCK,/NOLINES
      PSCLOSE,/NOVIEW
      
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_sst_biases_12421_ind.'+runids(j)+'_'+obs_times(i)+'.fcorr.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_fcorr_surface)+1,white=[10]
      LEVS,MANUAL=mylevs_fcorr_surface
      MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
      CON,X=kpp_longitude,Y=kpp_latitude,FIELD=fcorr_sfc,$
          TITLE='Surface flux correction for '+obs_times(i)+' from run '+runids(j),CB_TITLE='W m!U-3!N',$
          /BLOCK,/NOLINES
      PSCLOSE,/NOVIEW
   ENDFOR
                                ; 
                                ; Plot differences between ensemble-mean KPP runs and observations in SST
                                ;

                                ; Plot SST bias for uncorrected, unsmoothed run
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_sst_biases_12421_ind.uncorr_ensmean-minus-obs.'+$
          obs_times(i)+'.sst.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
   LEVS,MANUAL=mylevs_sst_diff
   MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
   CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_uncorr_mmean_ensmean-sst_obs_mmean,$
       TITLE='Diff in '+obs_times(i)+' mean SST for uncorrected ensemble-mean minus UKMO analysis',CB_TITLE='K',$
       /BLOCK,/NOLINES
   PSCLOSE,/NOVIEW
   
                                ; Plot SST bias for corrected, unsmoothed run
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_sst_biases_12421_ind.corr_ensmean-minus-obs.'+$
          obs_times(i)+'.sst.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
   LEVS,MANUAL=mylevs_sst_diff
   MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
   CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_corr_mmean_ensmean-sst_obs_mmean,$
       TITLE='Diff in '+obs_times(i)+' mean SST for corrected ensemble-mean minus UKMO analysis',CB_TITLE='K',$
       /BLOCK,/NOLINES
   PSCLOSE,/NOVIEW
   
                                ; Plot SST difference between corrected and uncorrected unsmoothed runs
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_sst_biases_12421_ind.corr_ensmean-minus-uncorr_ensmean.'+$
          obs_times(i)+'.sst.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
   LEVS,MANUAL=mylevs_sst_diff
   MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
   CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_corr_mmean_ensmean-sst_uncorr_mmean_ensmean,$
       TITLE='Diff in '+obs_times(i)+' mean SST for corrected ensemble-mean minus uncorrected',CB_TITLE='K',$
       /BLOCK,/NOLINES
   PSCLOSE,/NOVIEW
   
                                ; Read and plot differences in flux corrections at surface and through mixed-layer depth
   fcorr_longitude=OPEN_AND_EXTRACT(file_fcorr_ensmean,'longitude')
   fcorr_latitude=OPEN_AND_EXTRACT(file_fcorr_ensmean,'latitude')
   fcorr_z=OPEN_AND_EXTRACT(file_fcorr_ensmean,'z')
   DEFINE_BOUNDARIES,box,fcorr_latitude,fcorr_longitude,fcorr_box_tx,/LIMIT
   fcorr_nlon=N_ELEMENTS(fcorr_longitude)
   fcorr_nlat=N_ELEMENTS(fcorr_latitude)
   fcorr_nz=N_ELEMENTS(fcorr_z)

   fcorr_ind=REFORM(OPEN_AND_EXTRACT(file_fcorr_ensmean,'fcorr',$
                                        offset=[fcorr_box_tx(1),fcorr_box_tx(0),0,fcorr_offset(i)],$
                                        count=[fcorr_nlon,fcorr_nlat,fcorr_nz,1]))
   
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_sst_biases_12421_ind.nosmooth.'+$
          obs_times(i)+'.fcorr_surface.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_fcorr_surface)+1,white=[10]
   LEVS,MANUAL=mylevs_fcorr_surface
   MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
   CON,X=kpp_longitude,Y=kpp_latitude,FIELD=REFORM(fcorr_ind(*,*,0)),$
       TITLE='Ensemble-mean surface flux correction from runs for '+obs_times(i),CB_TITLE='W m!U-3!N',$
       /BLOCK,/NOLINES
   PSCLOSE,/NOVIEW
ENDFOR

STOP
END

