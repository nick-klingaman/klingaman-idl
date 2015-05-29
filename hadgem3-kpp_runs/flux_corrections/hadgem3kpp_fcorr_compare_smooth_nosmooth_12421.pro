PRO hadgem3kpp_fcorr_compare_smooth_nosmooth_12421
  
; Compare SST biases and flux corrections from smoothed and unsmoothed 
; KPP integrations

runids=['xgspj_i2','xgspj_i3','xgspj_i4','xgspj_i5']
;runids=['xgspj_i2']
n_runs=N_ELEMENTS(runids)

basedir_smooth_uncorr='/home/ss901165/kpp_ocean3/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421'
basedir_smooth_corr='/export/mango/data-10/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421'
basedir_nosmooth_uncorr='/home/ss901165/kpp_ocean3/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78'
basedir_nosmooth_corr='/export/mango/data-10/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78'

basedir_fcorr='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/heat_corrections'
file_smooth_fcorr=basedir_fcorr+'/hadgem3_1.5xentrain_ensmean_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421.jan-dec_mmeans.flxcorr.n96.nc'
file_nosmooth_fcorr=basedir_fcorr+'/hadgem3_1.5xentrain_ensmean_1mtop_3hr_ukmo_sal60_amip_vn78.jan-dec_mmeans.flxcorr.n96.nc'

basedir_obs='/home/ss901165/datasets/METO_OCEAN_ANALYSIS/n96'

kpp_times=['0045','0075','0105','0135']
obs_times=['jan15-feb15','feb15-mar15','mar15-apr15','apr15-may15']
n_times=N_ELEMENTS(kpp_times)

box=[-30,40,30,200]

mylevs_sst_diff=['-1.9','-1.7','-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1',$
                 '0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5','1.7','1.9']
mylevs_fcorr_surface=['-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5']
mylevs_fcorr_surface_diff=['-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5']

FOR i=0,n_times-1 DO BEGIN
   FOR j=0,n_runs-1 DO BEGIN   
      file_smooth_uncorr=basedir_smooth_uncorr+'/'+runids(j)+'/KPPocean_'+kpp_times(i)+'_means.nc'
      file_smooth_corr=basedir_smooth_corr+'/'+runids(j)+'/KPPocean_'+kpp_times(i)+'_means.nc'
      file_nosmooth_uncorr=basedir_nosmooth_uncorr+'/'+runids(j)+'/KPPocean_'+kpp_times(i)+'_means.nc'
      file_nosmooth_corr=basedir_nosmooth_corr+'/'+runids(j)+'/KPPocean_'+kpp_times(i)+'_means.nc'
      
      file_obs=basedir_obs+'/meto_ocean_analysis.'+obs_times(i)+'_mmean_clim.1980-2009.pot_temp.amip_type.n96.nc'
   
      kpp_longitude=OPEN_AND_EXTRACT(file_smooth_uncorr,'longitude')
      kpp_latitude=OPEN_AND_EXTRACT(file_smooth_uncorr,'latitude')
      DEFINE_BOUNDARIES,box,kpp_latitude,kpp_longitude,kpp_box_tx,/LIMIT
      kpp_nlon=N_ELEMENTS(kpp_longitude)
      kpp_nlat=N_ELEMENTS(kpp_latitude)
      
      obs_longitude=OPEN_AND_EXTRACT(file_obs,'longitude')
      obs_latitude=OPEN_AND_EXTRACT(file_obs,'latitude')
      DEFINE_BOUNDARIES,box,obs_latitude,obs_longitude,obs_box_tx,/LIMIT
      obs_nlon=N_ELEMENTS(obs_longitude)
      obs_nlat=N_ELEMENTS(obs_latitude)

      sst_smooth_uncorr=REFORM(OPEN_AND_EXTRACT(file_smooth_uncorr,'T',$
                                         offset=[kpp_box_tx(1),kpp_box_tx(0),0,0],$
                                         count=[kpp_nlon,kpp_nlat,1,30]))
      sst_smooth_corr=REFORM(OPEN_AND_EXTRACT(file_smooth_corr,'T',$
                                       offset=[kpp_box_tx(1),kpp_box_tx(0),0,0],$
                                       count=[kpp_nlon,kpp_nlat,1,30]))
      sst_nosmooth_uncorr=REFORM(OPEN_AND_EXTRACT(file_nosmooth_uncorr,'T',$
                                                  offset=[kpp_box_tx(1),kpp_box_tx(0),0,0],$
                                                  count=[kpp_nlon,kpp_nlat,1,30]))
      sst_nosmooth_corr=REFORM(OPEN_AND_EXTRACT(file_nosmooth_corr,'T',$
                                                offset=[kpp_box_tx(1),kpp_box_tx(0),0,0],$
                                                count=[kpp_nlon,kpp_nlat,1,30]))

      IF j eq 0 THEN BEGIN
         sst_smooth_uncorr_mmean=fltarr(kpp_nlon,kpp_nlat)
         sst_smooth_corr_mmean=fltarr(kpp_nlon,kpp_nlat)
         sst_nosmooth_uncorr_mmean=fltarr(kpp_nlon,kpp_nlat)
         sst_nosmooth_corr_mmean=fltarr(kpp_nlon,kpp_nlat)
         sst_smooth_uncorr_mmean_ensmean=fltarr(kpp_nlon,kpp_nlat)
         sst_smooth_corr_mmean_ensmean=fltarr(kpp_nlon,kpp_nlat)
         sst_nosmooth_uncorr_mmean_ensmean=fltarr(kpp_nlon,kpp_nlat)
         sst_nosmooth_corr_mmean_ensmean=fltarr(kpp_nlon,kpp_nlat)
      ENDIF
      FOR m=0,kpp_nlon-1 DO BEGIN
         FOR n=0,kpp_nlat-1 DO BEGIN
            sst_smooth_uncorr_mmean(m,n)=MEAN(sst_smooth_uncorr(m,n,*))
            sst_smooth_corr_mmean(m,n)=MEAN(sst_smooth_corr(m,n,*))
            sst_nosmooth_uncorr_mmean(m,n)=MEAN(sst_nosmooth_uncorr(m,n,*))
            sst_nosmooth_corr_mmean(m,n)=MEAN(sst_nosmooth_corr(m,n,*))
         ENDFOR
      ENDFOR
      sst_smooth_uncorr_mmean_ensmean=sst_smooth_uncorr_mmean_ensmean+sst_smooth_uncorr_mmean/FLOAT(n_runs)
      sst_smooth_corr_mmean_ensmean=sst_smooth_corr_mmean_ensmean+sst_smooth_corr_mmean/FLOAT(n_runs)      
      sst_nosmooth_uncorr_mmean_ensmean=sst_nosmooth_uncorr_mmean_ensmean+sst_nosmooth_uncorr_mmean/FLOAT(n_runs)
      sst_nosmooth_corr_mmean_ensmean=sst_nosmooth_corr_mmean_ensmean+sst_nosmooth_corr_mmean/FLOAT(n_runs)

      sst_obs_mmean=REFORM(OPEN_AND_EXTRACT(file_obs,'temp',$
                                            offset=[obs_box_tx(1),obs_box_tx(0),0],$
                                            count=[obs_nlon,obs_nlat,1]))

                                ; Plot SST bias for uncorrected, unsmoothed run
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_smooth_nosmooth_12421.nosmooth_uncorr-minus-obs.'+$
             runids(j)+'_'+obs_times(i)+'.sst.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
      LEVS,MANUAL=mylevs_sst_diff
      MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
      CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_nosmooth_uncorr_mmean-sst_obs_mmean,$
          TITLE='Diff in '+obs_times(i)+' mean SST for unsmoothed, uncorrected '+runids(j)+' minus UKMO analysis',CB_TITLE='K',$
          /BLOCK,/NOLINES
      PSCLOSE,/NOVIEW

                                ; Plot SST bias for corrected, unsmoothed run
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_smooth_nosmooth_12421.nosmooth_corr-minus-obs.'+$
             runids(j)+'_'+obs_times(i)+'.sst.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
      LEVS,MANUAL=mylevs_sst_diff
      MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
      CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_nosmooth_corr_mmean-sst_obs_mmean,$
          TITLE='Diff in '+obs_times(i)+' mean SST for unsmoothed, corrected '+runids(j)+' minus UKMO analysis',CB_TITLE='K',$
          /BLOCK,/NOLINES
      PSCLOSE,/NOVIEW
      
                                ; Plot SST difference between corrected and uncorrected unsmoothed runs
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_smooth_nosmooth_12421.nosmooth_corr-minus-nosmooth_uncorr.'+$
             runids(j)+'_'+obs_times(i)+'.sst.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
      LEVS,MANUAL=mylevs_sst_diff
      MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
      CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_nosmooth_corr_mmean-sst_nosmooth_uncorr_mmean,$
          TITLE='Diff in '+obs_times(i)+' mean SST for unsmoothed, corrected '+runids(j)+' minus unsmoothed, uncorrected',CB_TITLE='K',$
          /BLOCK,/NOLINES
      PSCLOSE,/NOVIEW

                                ; Plot SST bias for uncorrected, smoothed run
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_smooth_nosmooth_12421.smooth_uncorr-minus-obs.'+$
             runids(j)+'_'+obs_times(i)+'.sst.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
      LEVS,MANUAL=mylevs_sst_diff
      MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
      CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_smooth_uncorr_mmean-sst_obs_mmean,$
          TITLE='Diff in '+obs_times(i)+' mean SST for smoothed, uncorrected '+runids(j)+' minus UKMO analysis',CB_TITLE='K',$
          /BLOCK,/NOLINES
      PSCLOSE,/NOVIEW

                                ; Plot SST bias for corrected, smoothed run
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_smooth_nosmooth_12421.smooth_corr-minus-obs.'+$
             runids(j)+'_'+obs_times(i)+'.sst.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
      LEVS,MANUAL=mylevs_sst_diff
      MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
      CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_smooth_corr_mmean-sst_obs_mmean,$
          TITLE='Diff in '+obs_times(i)+' mean SST for smoothed, corrected '+runids(j)+' minus UKMO analysis',CB_TITLE='K',$
          /BLOCK,/NOLINES
      PSCLOSE,/NOVIEW
      
                                ; Plot SST difference between corrected and uncorrected smoothed runs
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_smooth_nosmooth_12421.nosmooth_corr-minus-smooth_uncorr.'+$
             runids(j)+'_'+obs_times(i)+'.sst.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
      LEVS,MANUAL=mylevs_sst_diff
      MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
      CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_smooth_corr_mmean-sst_smooth_uncorr_mmean,$
          TITLE='Diff in '+obs_times(i)+' mean SST for smoothed, corrected '+runids(j)+' minus smoothed, uncorrected',CB_TITLE='K',$
          /BLOCK,/NOLINES
      PSCLOSE,/NOVIEW
      
                                ; Plot SST difference between smoothed and unsmoothed uncorrected runs
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_smooth_nosmooth_12421.smooth_uncorr-minus-nosmooth_uncorr.'+$
             runids(j)+'_'+obs_times(i)+'.sst.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
      LEVS,MANUAL=mylevs_sst_diff
      MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
      CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_smooth_uncorr_mmean-sst_nosmooth_uncorr_mmean,$
          TITLE='Diff in '+obs_times(i)+' mean SST for smoothed, uncorrected '+runids(j)+' minus unsmoothed, uncorrected',CB_TITLE='K',$
          /BLOCK,/NOLINES
      PSCLOSE,/NOVIEW

                                ; Plot SST difference between smoothed and unsmoothed corrected runs
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_smooth_nosmooth_12421.smooth_corr-minus-nosmooth_corr.'+$
             runids(j)+'_'+obs_times(i)+'.sst.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
      LEVS,MANUAL=mylevs_sst_diff
      MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
      CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_smooth_corr_mmean-sst_nosmooth_corr_mmean,$
          TITLE='Diff in '+obs_times(i)+' mean SST for smoothed, corrected '+runids(j)+' minus unsmoothed, corrected',CB_TITLE='K',$
          /BLOCK,/NOLINES
      PSCLOSE,/NOVIEW
   ENDFOR
                                ; 
                                ; Plot differences between ensemble-mean KPP runs and observations in SST
                                ;

                                ; Plot SST bias for uncorrected, unsmoothed run
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_smooth_nosmooth_12421.nosmooth_uncorr_ensmean-minus-obs.'+$
          obs_times(i)+'.sst.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
   LEVS,MANUAL=mylevs_sst_diff
   MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
   CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_nosmooth_uncorr_mmean_ensmean-sst_obs_mmean,$
       TITLE='Diff in '+obs_times(i)+' mean SST for unsmoothed, uncorrected ensemble-mean minus UKMO analysis',CB_TITLE='K',$
       /BLOCK,/NOLINES
   PSCLOSE,/NOVIEW
   
                                ; Plot SST bias for corrected, unsmoothed run
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_smooth_nosmooth_12421.nosmooth_corr_ensmean-minus-obs.'+$
          obs_times(i)+'.sst.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
   LEVS,MANUAL=mylevs_sst_diff
   MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
   CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_nosmooth_corr_mmean_ensmean-sst_obs_mmean,$
       TITLE='Diff in '+obs_times(i)+' mean SST for unsmoothed, corrected ensemble-mean minus UKMO analysis',CB_TITLE='K',$
       /BLOCK,/NOLINES
   PSCLOSE,/NOVIEW
   
                                ; Plot SST difference between corrected and uncorrected unsmoothed runs
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_smooth_nosmooth_12421.nosmooth_corr_ensmean-minus-nosmooth_uncorr_ensmean.'+$
          obs_times(i)+'.sst.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
   LEVS,MANUAL=mylevs_sst_diff
   MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
   CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_nosmooth_corr_mmean_ensmean-sst_nosmooth_uncorr_mmean_ensmean,$
       TITLE='Diff in '+obs_times(i)+' mean SST for unsmoothed, corrected ensemble-mean minus unsmoothed, uncorrected',CB_TITLE='K',$
       /BLOCK,/NOLINES
   PSCLOSE,/NOVIEW
   
                                ; Plot SST bias for uncorrected, smoothed run
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_smooth_nosmooth_12421.smooth_uncorr_ensmean-minus-obs.'+$
          obs_times(i)+'.sst.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
   LEVS,MANUAL=mylevs_sst_diff
   MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
   CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_smooth_uncorr_mmean_ensmean-sst_obs_mmean,$
       TITLE='Diff in '+obs_times(i)+' mean SST for smoothed, uncorrected ensemble-mean minus UKMO analysis',CB_TITLE='K',$
       /BLOCK,/NOLINES
   PSCLOSE,/NOVIEW
   
                                ; Plot SST bias for corrected, smoothed run
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_smooth_nosmooth_12421.smooth_corr_ensmean-minus-obs.'+$
          obs_times(i)+'.sst.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
   LEVS,MANUAL=mylevs_sst_diff
   MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
   CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_smooth_corr_mmean_ensmean-sst_obs_mmean,$
       TITLE='Diff in '+obs_times(i)+' mean SST for smoothed, corrected ensemble-mean minus UKMO analysis',CB_TITLE='K',$
       /BLOCK,/NOLINES
   PSCLOSE,/NOVIEW
   
                                ; Plot SST difference between corrected and uncorrected smoothed runs
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_smooth_nosmooth_12421.nosmooth_corr_ensmean-minus-smooth_uncorr_ensmean.'+$
          obs_times(i)+'.sst.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
   LEVS,MANUAL=mylevs_sst_diff
   MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
   CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_smooth_corr_mmean_ensmean-sst_smooth_uncorr_mmean_ensmean,$
       TITLE='Diff in '+obs_times(i)+' mean SST for smoothed, corrected ensemble-mean minus smoothed, uncorrected',CB_TITLE='K',$
          /BLOCK,/NOLINES
   PSCLOSE,/NOVIEW
   
                                ; Plot SST difference between smoothed and unsmoothed uncorrected runs
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_smooth_nosmooth_12421.smooth_uncorr_ensmean-minus-nosmooth_uncorr_ensmean.'+$
          obs_times(i)+'.sst.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
   LEVS,MANUAL=mylevs_sst_diff
   MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
   CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_smooth_uncorr_mmean_ensmean-sst_nosmooth_uncorr_mmean_ensmean,$
       TITLE='Diff in '+obs_times(i)+' mean SST for smoothed, uncorrected ensemble-mean minus unsmoothed, uncorrected',CB_TITLE='K',$
       /BLOCK,/NOLINES
   PSCLOSE,/NOVIEW
   
                                ; Plot SST difference between smoothed and unsmoothed corrected runs
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_smooth_nosmooth_12421.smooth_corr_ensmean-minus-nosmooth_corr_ensmean.'+$
          obs_times(i)+'.sst.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
   LEVS,MANUAL=mylevs_sst_diff
   MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
   CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_smooth_corr_mmean_ensmean-sst_nosmooth_corr_mmean_ensmean,$
       TITLE='Diff in '+obs_times(i)+' mean SST for smoothed, corrected ensemble-mean minus unsmoothed, corrected',CB_TITLE='K',$
       /BLOCK,/NOLINES
   PSCLOSE,/NOVIEW
   
   
                                ; Read and plot differences in flux corrections at surface and through mixed-layer depth
   fcorr_longitude=OPEN_AND_EXTRACT(file_smooth_fcorr,'longitude')
   fcorr_latitude=OPEN_AND_EXTRACT(file_smooth_fcorr,'latitude')
   fcorr_z=OPEN_AND_EXTRACT(file_smooth_fcorr,'z')
   DEFINE_BOUNDARIES,box,fcorr_latitude,fcorr_longitude,fcorr_box_tx,/LIMIT
   fcorr_nlon=N_ELEMENTS(fcorr_longitude)
   fcorr_nlat=N_ELEMENTS(fcorr_latitude)
   fcorr_nz=N_ELEMENTS(fcorr_z)

   fcorr_smooth=REFORM(OPEN_AND_EXTRACT(file_smooth_fcorr,'fcorr',$
                                        offset=[fcorr_box_tx(1),fcorr_box_tx(0),0,i],$
                                        count=[fcorr_nlon,fcorr_nlat,fcorr_nz,1]))
   fcorr_nosmooth=REFORM(OPEN_AND_EXTRACT(file_nosmooth_fcorr,'fcorr',$
                                          offset=[fcorr_box_tx(1),fcorr_box_tx(0),0,i],$
                                          count=[fcorr_nlon,fcorr_nlat,fcorr_nz,1]))

   
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_smooth_nosmooth_12421.smooth.'+$
          obs_times(i)+'.fcorr_surface.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_fcorr_surface)+1,white=[10]
   LEVS,MANUAL=mylevs_fcorr_surface
   MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
   CON,X=kpp_longitude,Y=kpp_latitude,FIELD=REFORM(fcorr_smooth(*,*,0)),$
       TITLE='Ensemble-mean surface flux correction from smoothed runs for '+obs_times(i),CB_TITLE='W m!U-2!N',$
       /BLOCK,/NOLINES
   PSCLOSE

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_smooth_nosmooth_12421.nosmooth.'+$
          obs_times(i)+'.fcorr_surface.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_fcorr_surface)+1,white=[10]
   LEVS,MANUAL=mylevs_fcorr_surface
   MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
   CON,X=kpp_longitude,Y=kpp_latitude,FIELD=REFORM(fcorr_nosmooth(*,*,0)),$
       TITLE='Ensemble-mean surface flux correction from unsmoothed runs for '+obs_times(i),CB_TITLE='W m!U-2!N',$
       /BLOCK,/NOLINES
       PSCLOSE

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_smooth_nosmooth_12421.smooth-minus-nosmooth.'+$
          obs_times(i)+'.fcorr_surface.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_fcorr_surface_diff)+1,white=[10]
   LEVS,MANUAL=mylevs_fcorr_surface_diff
   MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
   CON,X=kpp_longitude,Y=kpp_latitude,FIELD=REFORM(fcorr_smooth(*,*,0))-REFORM(fcorr_nosmooth(*,*,0)),$
       TITLE='Diff in '+obs_times(i)+' mean SST for smoothed, corrected ensemble-mean minus unsmoothed, corrected',CB_TITLE='K',$
       /BLOCK,/NOLINES
   PSCLOSE

ENDFOR

STOP
END

