PRO hadgem3kpp_fcorr_compare_sal60_nosal
  
; Compare SST biases and flux corrections from smoothed and norelax 
; KPP integrations

runids=['xgspj_i2','xgspj_i3','xgspj_i4','xgspj_i5']
;runids=['xgspj_i2']
n_runs=N_ELEMENTS(runids)

basedir_sal60_uncorr='/home/ss901165/kpp_ocean3/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78'
basedir_sal60_corr='/export/mango/data-10/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78'
basedir_nosal_uncorr='/home/ss901165/kpp_ocean3/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_nosal_amip_vn78'
basedir_nosal_corr='/export/mango/data-10/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_nosal_amip_vn78'

basedir_fcorr='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/heat_corrections'
file_sal60_fcorr=basedir_fcorr+'/hadgem3_1.5xentrain_ensmean_1mtop_3hr_ukmo_sal60_amip_vn78.jan-dec_mmeans.flxcorr.n96.nc'
file_nosal_fcorr=basedir_fcorr+'/hadgem3_1.5xentrain_ensmean_1mtop_3hr_ukmo_nosal_amip_vn78.jan-dec_mmeans.flxcorr.n96.nc'

basedir_obs='/home/ss901165/datasets/METO_OCEAN_ANALYSIS/n96'

;kpp_times=['0045','0075','0105','0135']
;obs_times=['jan15-feb15','feb15-mar15','mar15-apr15','apr15-may15']
kpp_times=['0045']
obs_times=['jan15-feb15']
n_times=N_ELEMENTS(kpp_times)

box=[-30,40,30,200]

mylevs_sst_diff=['-1.9','-1.7','-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1',$
                 '0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5','1.7','1.9']
mylevs_fcorr_surface=['-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5']
mylevs_fcorr_surface_diff=['-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5']

FOR i=0,n_times-1 DO BEGIN
   FOR j=0,n_runs-1 DO BEGIN   
      file_sal60_uncorr=basedir_sal60_uncorr+'/'+runids(j)+'/KPPocean_'+kpp_times(i)+'_means.nc'
      file_sal60_corr=basedir_sal60_corr+'/'+runids(j)+'/KPPocean_'+kpp_times(i)+'_means.nc'
      file_nosal_uncorr=basedir_nosal_uncorr+'/'+runids(j)+'/KPPocean_'+kpp_times(i)+'_means.nc'
      file_nosal_corr=basedir_nosal_corr+'/'+runids(j)+'/KPPocean_'+kpp_times(i)+'_means.nc'
      
      file_obs=basedir_obs+'/meto_ocean_analysis.'+obs_times(i)+'_mmean_clim.1980-2009.pot_temp.amip_type.n96.nc'
   
      kpp_longitude=OPEN_AND_EXTRACT(file_sal60_uncorr,'longitude')
      kpp_latitude=OPEN_AND_EXTRACT(file_sal60_uncorr,'latitude')
      DEFINE_BOUNDARIES,box,kpp_latitude,kpp_longitude,kpp_box_tx,/LIMIT
      kpp_nlon=N_ELEMENTS(kpp_longitude)
      kpp_nlat=N_ELEMENTS(kpp_latitude)
      
      obs_longitude=OPEN_AND_EXTRACT(file_obs,'longitude')
      obs_latitude=OPEN_AND_EXTRACT(file_obs,'latitude')
      DEFINE_BOUNDARIES,box,obs_latitude,obs_longitude,obs_box_tx,/LIMIT
      obs_nlon=N_ELEMENTS(obs_longitude)
      obs_nlat=N_ELEMENTS(obs_latitude)

      sst_sal60_uncorr=REFORM(OPEN_AND_EXTRACT(file_sal60_uncorr,'T',$
                                         offset=[kpp_box_tx(1),kpp_box_tx(0),0,0],$
                                         count=[kpp_nlon,kpp_nlat,1,30]))
      sst_sal60_corr=REFORM(OPEN_AND_EXTRACT(file_sal60_corr,'T',$
                                       offset=[kpp_box_tx(1),kpp_box_tx(0),0,0],$
                                       count=[kpp_nlon,kpp_nlat,1,30]))
      sst_nosal_uncorr=REFORM(OPEN_AND_EXTRACT(file_nosal_uncorr,'T',$
                                                  offset=[kpp_box_tx(1),kpp_box_tx(0),0,0],$
                                                  count=[kpp_nlon,kpp_nlat,1,30]))
      sst_nosal_corr=REFORM(OPEN_AND_EXTRACT(file_nosal_corr,'T',$
                                                offset=[kpp_box_tx(1),kpp_box_tx(0),0,0],$
                                                count=[kpp_nlon,kpp_nlat,1,30]))

      IF j eq 0 THEN BEGIN
         sst_sal60_uncorr_mmean=fltarr(kpp_nlon,kpp_nlat)
         sst_sal60_corr_mmean=fltarr(kpp_nlon,kpp_nlat)
         sst_nosal_uncorr_mmean=fltarr(kpp_nlon,kpp_nlat)
         sst_nosal_corr_mmean=fltarr(kpp_nlon,kpp_nlat)
         sst_sal60_uncorr_mmean_ensmean=fltarr(kpp_nlon,kpp_nlat)
         sst_sal60_corr_mmean_ensmean=fltarr(kpp_nlon,kpp_nlat)
         sst_nosal_uncorr_mmean_ensmean=fltarr(kpp_nlon,kpp_nlat)
         sst_nosal_corr_mmean_ensmean=fltarr(kpp_nlon,kpp_nlat)
      ENDIF
      FOR m=0,kpp_nlon-1 DO BEGIN
         FOR n=0,kpp_nlat-1 DO BEGIN
            sst_sal60_uncorr_mmean(m,n)=MEAN(sst_sal60_uncorr(m,n,*))
            sst_sal60_corr_mmean(m,n)=MEAN(sst_sal60_corr(m,n,*))
            sst_nosal_uncorr_mmean(m,n)=MEAN(sst_nosal_uncorr(m,n,*))
            sst_nosal_corr_mmean(m,n)=MEAN(sst_nosal_corr(m,n,*))
         ENDFOR
      ENDFOR
      sst_sal60_uncorr_mmean_ensmean=sst_sal60_uncorr_mmean_ensmean+sst_sal60_uncorr_mmean/FLOAT(n_runs)
      sst_sal60_corr_mmean_ensmean=sst_sal60_corr_mmean_ensmean+sst_sal60_corr_mmean/FLOAT(n_runs)      
      sst_nosal_uncorr_mmean_ensmean=sst_nosal_uncorr_mmean_ensmean+sst_nosal_uncorr_mmean/FLOAT(n_runs)
      sst_nosal_corr_mmean_ensmean=sst_nosal_corr_mmean_ensmean+sst_nosal_corr_mmean/FLOAT(n_runs)

      sst_obs_mmean=REFORM(OPEN_AND_EXTRACT(file_obs,'temp',$
                                            offset=[obs_box_tx(1),obs_box_tx(0),0],$
                                            count=[obs_nlon,obs_nlat,1]))

                                ; Plot SST bias for uncorrected, norelax run
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_sal60_nosal.nosal_uncorr-minus-obs.'+$
             runids(j)+'_'+obs_times(i)+'.sst.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
      LEVS,MANUAL=mylevs_sst_diff
      MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
      CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_nosal_uncorr_mmean-sst_obs_mmean,$
          TITLE='Diff in '+obs_times(i)+' mean SST for norelax, uncorrected '+runids(j)+' minus UKMO analysis',CB_TITLE='K',$
          /BLOCK,/NOLINES
      PSCLOSE,/NOVIEW

                                ; Plot SST bias for corrected, norelax run
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_sal60_nosal.nosal_corr-minus-obs.'+$
             runids(j)+'_'+obs_times(i)+'.sst.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
      LEVS,MANUAL=mylevs_sst_diff
      MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
      CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_nosal_corr_mmean-sst_obs_mmean,$
          TITLE='Diff in '+obs_times(i)+' mean SST for norelax, corrected '+runids(j)+' minus UKMO analysis',CB_TITLE='K',$
          /BLOCK,/NOLINES
      PSCLOSE,/NOVIEW
      
                                ; Plot SST difference between corrected and uncorrected norelax runs
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_sal60_nosal.nosal_corr-minus-nosal_uncorr.'+$
             runids(j)+'_'+obs_times(i)+'.sst.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
      LEVS,MANUAL=mylevs_sst_diff
      MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
      CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_nosal_corr_mmean-sst_nosal_uncorr_mmean,$
          TITLE='Diff in '+obs_times(i)+' mean SST for norelax, corrected '+runids(j)+' minus norelax, uncorrected',CB_TITLE='K',$
          /BLOCK,/NOLINES
      PSCLOSE,/NOVIEW

                                ; Plot SST bias for uncorrected, 60-day relax run
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_sal60_nosal.sal60_uncorr-minus-obs.'+$
             runids(j)+'_'+obs_times(i)+'.sst.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
      LEVS,MANUAL=mylevs_sst_diff
      MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
      CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_sal60_uncorr_mmean-sst_obs_mmean,$
          TITLE='Diff in '+obs_times(i)+' mean SST for 60-day relax, uncorrected '+runids(j)+' minus UKMO analysis',CB_TITLE='K',$
          /BLOCK,/NOLINES
      PSCLOSE,/NOVIEW

                                ; Plot SST bias for corrected, 60-day relax run
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_sal60_nosal.sal60_corr-minus-obs.'+$
             runids(j)+'_'+obs_times(i)+'.sst.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
      LEVS,MANUAL=mylevs_sst_diff
      MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
      CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_sal60_corr_mmean-sst_obs_mmean,$
          TITLE='Diff in '+obs_times(i)+' mean SST for 60-day relax, corrected '+runids(j)+' minus UKMO analysis',CB_TITLE='K',$
          /BLOCK,/NOLINES
      PSCLOSE,/NOVIEW
      
                                ; Plot SST difference between corrected and uncorrected 60-day relax runs
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_sal60_nosal.nosal_corr-minus-sal60_uncorr.'+$
             runids(j)+'_'+obs_times(i)+'.sst.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
      LEVS,MANUAL=mylevs_sst_diff
      MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
      CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_sal60_corr_mmean-sst_sal60_uncorr_mmean,$
          TITLE='Diff in '+obs_times(i)+' mean SST for 60-day relax, corrected '+runids(j)+' minus 60-day relax, uncorrected',CB_TITLE='K',$
          /BLOCK,/NOLINES
      PSCLOSE,/NOVIEW
      
                                ; Plot SST difference between 60-day relax and norelax uncorrected runs
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_sal60_nosal.sal60_uncorr-minus-nosal_uncorr.'+$
             runids(j)+'_'+obs_times(i)+'.sst.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
      LEVS,MANUAL=mylevs_sst_diff
      MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
      CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_sal60_uncorr_mmean-sst_nosal_uncorr_mmean,$
          TITLE='Diff in '+obs_times(i)+' mean SST for 60-day relax, uncorrected '+runids(j)+' minus norelax, uncorrected',CB_TITLE='K',$
          /BLOCK,/NOLINES
      PSCLOSE,/NOVIEW

                                ; Plot SST difference between 60-day relax and norelax corrected runs
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_sal60_nosal.sal60_corr-minus-nosal_corr.'+$
             runids(j)+'_'+obs_times(i)+'.sst.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
      LEVS,MANUAL=mylevs_sst_diff
      MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
      CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_sal60_corr_mmean-sst_nosal_corr_mmean,$
          TITLE='Diff in '+obs_times(i)+' mean SST for 60-day relax, corrected '+runids(j)+' minus norelax, corrected',CB_TITLE='K',$
          /BLOCK,/NOLINES
      PSCLOSE,/NOVIEW
   ENDFOR
                                ; 
                                ; Plot differences between ensemble-mean KPP runs and observations in SST
                                ;

                                ; Plot SST bias for uncorrected, norelax run
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_sal60_nosal.nosal_uncorr_ensmean-minus-obs.'+$
          obs_times(i)+'.sst.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
   LEVS,MANUAL=mylevs_sst_diff
   MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
   CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_nosal_uncorr_mmean_ensmean-sst_obs_mmean,$
       TITLE='Diff in '+obs_times(i)+' mean SST for norelax, uncorrected ensemble-mean minus UKMO analysis',CB_TITLE='K',$
       /BLOCK,/NOLINES
   PSCLOSE,/NOVIEW
   
                                ; Plot SST bias for corrected, norelax run
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_sal60_nosal.nosal_corr_ensmean-minus-obs.'+$
          obs_times(i)+'.sst.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
   LEVS,MANUAL=mylevs_sst_diff
   MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
   CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_nosal_corr_mmean_ensmean-sst_obs_mmean,$
       TITLE='Diff in '+obs_times(i)+' mean SST for norelax, corrected ensemble-mean minus UKMO analysis',CB_TITLE='K',$
       /BLOCK,/NOLINES
   PSCLOSE,/NOVIEW
   
                                ; Plot SST difference between corrected and uncorrected norelax runs
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_sal60_nosal.nosal_corr_ensmean-minus-nosal_uncorr_ensmean.'+$
          obs_times(i)+'.sst.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
   LEVS,MANUAL=mylevs_sst_diff
   MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
   CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_nosal_corr_mmean_ensmean-sst_nosal_uncorr_mmean_ensmean,$
       TITLE='Diff in '+obs_times(i)+' mean SST for norelax, corrected ensemble-mean minus norelax, uncorrected',CB_TITLE='K',$
       /BLOCK,/NOLINES
   PSCLOSE,/NOVIEW
   
                                ; Plot SST bias for uncorrected, 60-day relax run
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_sal60_nosal.sal60_uncorr_ensmean-minus-obs.'+$
          obs_times(i)+'.sst.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
   LEVS,MANUAL=mylevs_sst_diff
   MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
   CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_sal60_uncorr_mmean_ensmean-sst_obs_mmean,$
       TITLE='Diff in '+obs_times(i)+' mean SST for 60-day relax, uncorrected ensemble-mean minus UKMO analysis',CB_TITLE='K',$
       /BLOCK,/NOLINES
   PSCLOSE,/NOVIEW
   
                                ; Plot SST bias for corrected, 60-day relax run
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_sal60_nosal.sal60_corr_ensmean-minus-obs.'+$
          obs_times(i)+'.sst.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
   LEVS,MANUAL=mylevs_sst_diff
   MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
   CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_sal60_corr_mmean_ensmean-sst_obs_mmean,$
       TITLE='Diff in '+obs_times(i)+' mean SST for 60-day relax, corrected ensemble-mean minus UKMO analysis',CB_TITLE='K',$
       /BLOCK,/NOLINES
   PSCLOSE,/NOVIEW
   
                                ; Plot SST difference between corrected and uncorrected 60-day relax runs
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_sal60_nosal.nosal_corr_ensmean-minus-sal60_uncorr_ensmean.'+$
          obs_times(i)+'.sst.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
   LEVS,MANUAL=mylevs_sst_diff
   MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
   CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_sal60_corr_mmean_ensmean-sst_sal60_uncorr_mmean_ensmean,$
       TITLE='Diff in '+obs_times(i)+' mean SST for 60-day relax, corrected ensemble-mean minus 60-day relax, uncorrected',CB_TITLE='K',$
          /BLOCK,/NOLINES
   PSCLOSE,/NOVIEW
   
                                ; Plot SST difference between 60-day relax and norelax uncorrected runs
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_sal60_nosal.sal60_uncorr_ensmean-minus-nosal_uncorr_ensmean.'+$
          obs_times(i)+'.sst.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
   LEVS,MANUAL=mylevs_sst_diff
   MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
   CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_sal60_uncorr_mmean_ensmean-sst_nosal_uncorr_mmean_ensmean,$
       TITLE='Diff in '+obs_times(i)+' mean SST for 60-day relax, uncorrected ensemble-mean minus norelax, uncorrected',CB_TITLE='K',$
       /BLOCK,/NOLINES
   PSCLOSE,/NOVIEW
   
                                ; Plot SST difference between 60-day relax and norelax corrected runs
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_sal60_nosal.sal60_corr_ensmean-minus-nosal_corr_ensmean.'+$
          obs_times(i)+'.sst.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_sst_diff)+1,white=[12]
   LEVS,MANUAL=mylevs_sst_diff
   MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
   CON,X=kpp_longitude,Y=kpp_latitude,FIELD=sst_sal60_corr_mmean_ensmean-sst_nosal_corr_mmean_ensmean,$
       TITLE='Diff in '+obs_times(i)+' mean SST for 60-day relax, corrected ensemble-mean minus norelax, corrected',CB_TITLE='K',$
       /BLOCK,/NOLINES
   PSCLOSE,/NOVIEW
   
   
                                ; Read and plot differences in flux corrections at surface and through mixed-layer depth
   fcorr_longitude=OPEN_AND_EXTRACT(file_sal60_fcorr,'longitude')
   fcorr_latitude=OPEN_AND_EXTRACT(file_sal60_fcorr,'latitude')
   fcorr_z=OPEN_AND_EXTRACT(file_sal60_fcorr,'z')
   DEFINE_BOUNDARIES,box,fcorr_latitude,fcorr_longitude,fcorr_box_tx,/LIMIT
   fcorr_nlon=N_ELEMENTS(fcorr_longitude)
   fcorr_nlat=N_ELEMENTS(fcorr_latitude)
   fcorr_nz=N_ELEMENTS(fcorr_z)

   fcorr_sal60=REFORM(OPEN_AND_EXTRACT(file_sal60_fcorr,'fcorr',$
                                        offset=[fcorr_box_tx(1),fcorr_box_tx(0),0,i],$
                                        count=[fcorr_nlon,fcorr_nlat,fcorr_nz,1]))
   fcorr_nosal=REFORM(OPEN_AND_EXTRACT(file_nosal_fcorr,'fcorr',$
                                          offset=[fcorr_box_tx(1),fcorr_box_tx(0),0,i],$
                                          count=[fcorr_nlon,fcorr_nlat,fcorr_nz,1]))

   
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_sal60_nosal.sal60.'+$
          obs_times(i)+'.fcorr_surface.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_fcorr_surface)+1,white=[10]
   LEVS,MANUAL=mylevs_fcorr_surface
   MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
   CON,X=kpp_longitude,Y=kpp_latitude,FIELD=REFORM(fcorr_sal60(*,*,0)),$
       TITLE='Ensemble-mean surface flux correction from 60-day relax runs for '+obs_times(i),CB_TITLE='W m!U-2!N',$
       /BLOCK,/NOLINES
   PSCLOSE

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_sal60_nosal.nosal.'+$
          obs_times(i)+'.fcorr_surface.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_fcorr_surface)+1,white=[10]
   LEVS,MANUAL=mylevs_fcorr_surface
   MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
   CON,X=kpp_longitude,Y=kpp_latitude,FIELD=REFORM(fcorr_nosal(*,*,0)),$
       TITLE='Ensemble-mean surface flux correction from norelax runs for '+obs_times(i),CB_TITLE='W m!U-2!N',$
       /BLOCK,/NOLINES
       PSCLOSE

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_sal60_nosal.sal60-minus-nosal.'+$
          obs_times(i)+'.fcorr_surface.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_fcorr_surface_diff)+1,white=[10]
   LEVS,MANUAL=mylevs_fcorr_surface_diff
   MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
   CON,X=kpp_longitude,Y=kpp_latitude,FIELD=REFORM(fcorr_sal60(*,*,0))-REFORM(fcorr_nosal(*,*,0)),$
       TITLE='Diff in '+obs_times(i)+' mean SST for 60-day relax, corrected ensemble-mean minus norelax, corrected',CB_TITLE='K',$
       /BLOCK,/NOLINES
   PSCLOSE

ENDFOR

STOP
END

