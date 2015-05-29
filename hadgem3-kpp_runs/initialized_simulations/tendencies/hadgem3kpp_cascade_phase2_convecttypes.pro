PRO hadgem3kpp_cascade_phase2_convecttypes

n_sets=4
n_variables=3
um3='/home/ss901165/um_output3'
date_ranges=['04nov09-06nov09','26aug08-28aug08','07dec07-09dec07','07sep06-09sep06','18mar05-20mar05','03dec03-05dec03',$
             '29oct02-01nov02','05jun02-07jun02','26apr02-28apr02','16jan01-18jan01','13nov00-15nov00','28sep00-30sep00']

box=[-15,40,15,200]
n_bins=20
n_times=8
n_z=85

shallowfrac_levels=['0.005','0.010','0.015','0.020','0.025','0.030','0.035','0.040','0.045','0.050','0.055']
shallowfrac_diff_levels=['-0.045','-0.039','-0.033','-0.027','-0.021','-0.015','-0.009','-0.003','0.003','0.009','0.015','0.021','0.027','0.033','0.039','0.045']
midfrac_levels=['0.03','0.06','0.09','0.12','0.15','0.18','0.21','0.24','0.27','0.30','0.33','0.36','0.39']
midfrac_diff_levels=['-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15']
deepfrac_levels=['0.80','0.82','0.84','0.86','0.88','0.90','0.92','0.94','0.95','0.96','0.97','0.98','0.99']
deepfrac_diff_levels=['-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15']
lsfrac_levels=['0.04','0.08','0.12','0.16','0.20','0.24','0.28','0.32','0.36','0.40','0.44','0.48','0.52']
lsfrac_diff_levels=['-0.30','-0.26','-0.22','-0.18','-0.14','-0.10','-0.06','-0.02','0.02','0.06','0.10','0.14','0.18','0.22','0.26','0.30']

deepconv_term_levels=['100','150','200','250','300','350','400','450','500','550','600','650','700','750','800','850','900']
deepconv_term_diff_levels=['-260','-220','-180','-140','-100','-60','-20','20','60','100','140','180','220','260']
deepconv_term_bins=[REVERSE(indgen(31)*25+100)]
n_deepconv_term_bins=N_ELEMENTS(deepconv_term_bins)

joint_pdf_levels=['0.02','0.04','0.06','0.09','0.12','0.15','0.18','0.22','0.26','0.30','0.35','0.40','0.45','0.50','0.60','0.70']

FOR i=0,n_sets-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         runids=['xfrla','xfrle','xfrli','xfrlm','xfrlq','xfrlu','xfsea','xfsee','xfsei','xfsem','xfseq','xfseu']
         set_name='control'
      END
      1 : BEGIN
         runids=['xfrlb','xfrlf','xfrlj','xfrln','xfrlr','xfrlv','xfseb','xfsef','xfsej','xfsen','xfser','xfsev']
         set_name='15xentrain'
      END
      2 : BEGIN
         runids=['xfrlc','xfrlg','xfrlk','xfrlo','xfrls','xfrlw','xfsec','xfseg','xfsek','xfseo','xfses','xfsew']
         set_name='nocmt'
      END
      3 : BEGIN
         runids=['xfrld','xfrlh','xfrll','xfrlp','xfrlt','xfrlx','xfsed','xfseh','xfsel','xfsem','xfset','xfsex']
         set_name='15xentrain_nocmt'
      END
   ENDCASE
   n_cases=N_ELEMENTS(runids)
;   n_cases=2
   print,'---> '+set_name
      
   FOR m=0,n_cases-1 DO BEGIN
      print,'-------> '+runids(m)
      cvrain_infile=um3+'/'+runids(m)+'.new_stash/'+runids(m)+'.'+date_ranges(m)+'_3hrmeans.cvrain.nc'
      cvrain_longitude=OPEN_AND_EXTRACT(cvrain_infile,'longitude_1')
      cvrain_latitude=OPEN_AND_EXTRACT(cvrain_infile,'latitude')
      DEFINE_BOUNDARIES,box,cvrain_latitude,cvrain_longitude,cvrain_box_tx,/LIMIT
      cvrain_nlon=N_ELEMENTS(cvrain_longitude)
      cvrain_nlat=N_ELEMENTS(cvrain_latitude)
      
      IF m eq 0 THEN BEGIN
         cumulative_cvrain=fltarr(cvrain_nlon,cvrain_nlat)
         cumulative_shallowrain=fltarr(cvrain_nlon,cvrain_nlat)
         cumulative_midrain=fltarr(cvrain_nlon,cvrain_nlat)
         cumulative_deeprain=fltarr(cvrain_nlon,cvrain_nlat)
         cumulative_lsrain=fltarr(cvrain_nlon,cvrain_nlat)
         allruns_rain=fltarr(n_cases,cvrain_nlon*cvrain_nlat*n_times)         
         allruns_cvrain=fltarr(n_cases,cvrain_nlon*cvrain_nlat*n_times)
         allruns_shallowrain=fltarr(n_cases,cvrain_nlon*cvrain_nlat*n_times)
         allruns_midrain=fltarr(n_cases,cvrain_nlon*cvrain_nlat*n_times)
         allruns_deeprain=fltarr(n_cases,cvrain_nlon*cvrain_nlat*n_times)
         allruns_lsrain=fltarr(n_cases,cvrain_nlon*cvrain_nlat*n_times)
         allruns_deepconv_termlvl=fltarr(n_cases,cvrain_nlon*cvrain_nlat*n_times)
         mean_deepconv_termlvl=fltarr(cvrain_nlon,cvrain_nlat)
         mean_rholvl_pres=fltarr(n_z)
         count_deepconv=fltarr(cvrain_nlon,cvrain_nlat)
      ENDIF

      cvrain=REFORM(OPEN_AND_EXTRACT(cvrain_infile,'cvrain',$
                                     offset=[cvrain_box_tx(1),cvrain_box_tx(0),0,0],$
                                     count=[cvrain_nlon,cvrain_nlat,1,n_times]))*3600.

      rain_infile=um3+'/'+runids(m)+'.new_stash/'+runids(m)+'.'+date_ranges(m)+'_3hrmeans.rain.nc'
      rain=REFORM(OPEN_AND_EXTRACT(rain_infile,'rain',$
                                   offset=[cvrain_box_tx(1),cvrain_box_tx(0),0,0],$
                                   count=[cvrain_nlon,cvrain_nlat,1,n_times]))*3600.
      allruns_rain(m,*)=REFORM(rain,[cvrain_nlon*cvrain_nlat*n_times])

      shallowrain_infile=um3+'/'+runids(m)+'.new_stash/'+runids(m)+'.'+date_ranges(m)+'_3hrmeans.precip_shallowconv.nc'
      shallowrain=REFORM(OPEN_AND_EXTRACT(shallowrain_infile,'precip_1',$
                                          offset=[cvrain_box_tx(1),cvrain_box_tx(0),0,0],$
                                          count=[cvrain_nlon,cvrain_nlat,1,n_times]))*3600.

      midrain_infile=um3+'/'+runids(m)+'.new_stash/'+runids(m)+'.'+date_ranges(m)+'_3hrmeans.precip_midconv.nc'
      midrain=REFORM(OPEN_AND_EXTRACT(midrain_infile,'precip_2',$
                                      offset=[cvrain_box_tx(1),cvrain_box_tx(0),0,0],$
                                      count=[cvrain_nlon,cvrain_nlat,1,n_times]))*3600.

      deeprain_infile=um3+'/'+runids(m)+'.new_stash/'+runids(m)+'.'+date_ranges(m)+'_3hrmeans.precip_deepconv.nc'
      deeprain=REFORM(OPEN_AND_EXTRACT(deeprain_infile,'precip',$
                                      offset=[cvrain_box_tx(1),cvrain_box_tx(0),0,0],$
                                      count=[cvrain_nlon,cvrain_nlat,1,n_times]))*3600.

      lsrain_infile=um3+'/'+runids(m)+'.new_stash/'+runids(m)+'.'+date_ranges(m)+'_3hrmeans.lsrain.nc'
      lsrain=REFORM(OPEN_AND_EXTRACT(lsrain_infile,'lsrain',$
                                     offset=[cvrain_box_tx(1),cvrain_box_tx(0),0,0],$
                                     count=[cvrain_nlon,cvrain_nlat,1,n_times]))*3600.

      deepconv_infile=um3+'/'+runids(m)+'.new_stash/'+runids(m)+'.'+date_ranges(m)+'_3hrmeans.deepconv_termlvl.nc'
      deepconv_termlvl=REFORM(OPEN_AND_EXTRACT(deepconv_infile,'field1937',$
                                               offset=[cvrain_box_tx(1),cvrain_box_tx(0),0,0],$
                                               count=[cvrain_nlon,cvrain_nlat,1,n_times]))

      cape_tscale_infile=um3+'/'+runids(m)+'.new_stash/'+runids(m)+'.'+date_ranges(m)+'_3hrmeans.cape_tscale_deep.nc'
      cape_tscale=REFORM(OPEN_AND_EXTRACT(cape_tscale_infile,'unspecified_10',$
                                          offset=[cvrain_box_tx(1),cvrain_box_tx(0),0,0],$
                                          count=[cvrain_nlon,cvrain_nlat,1,n_times]))

      rholvl_pres_infile=um3+'/'+runids(m)+'.new_stash/'+runids(m)+'.'+date_ranges(m)+'_3hrmeans.p_rholvl_after_ts.nc'
      rholvl_pres=REFORM(OPEN_AND_EXTRACT(rholvl_pres_infile,'p',$
                                          offset=[cvrain_box_tx(1),cvrain_box_tx(0),0,0],$
                                          count=[cvrain_nlon,cvrain_nlat,n_z,n_times]))

      cape_tscale_save=cape_tscale
      cape_tscale_zero=where(cape_tscale lt 600)
      cape_tscale[cape_tscale_zero]=600.

      deepconv_nonzero=where(deepconv_termlvl ne 0)
      deepconv_zero=where(deepconv_termlvl eq 0)
      ;IF i eq 2 THEN $
      ;   STOP
      deepconv_termlvl_save=deepconv_termlvl
      deepconv_termlvl[deepconv_nonzero]=deepconv_termlvl[deepconv_nonzero]*(5400./(cape_tscale[deepconv_nonzero]))
      deepconv_termlvl[deepconv_zero]=!Values.F_NaN
      IF TOTAL(where(deepconv_termlvl gt n_z) gt 0) THEN $
         deepconv_termlvl[where(deepconv_termlvl gt n_z)]=!Values.F_NaN

      FOR n=0,cvrain_nlon-1 DO BEGIN
         FOR p=0,cvrain_nlat-1 DO BEGIN
            cumulative_cvrain(n,p)=cumulative_cvrain(n,p)+TOTAL(cvrain(n,p,*))
            cumulative_shallowrain(n,p)=cumulative_shallowrain(n,p)+TOTAL(shallowrain(n,p,*))
            cumulative_midrain(n,p)=cumulative_midrain(n,p)+TOTAL(midrain(n,p,*))
            cumulative_deeprain(n,p)=cumulative_deeprain(n,p)+TOTAL(deeprain(n,p,*))
            cumulative_lsrain(n,p)=cumulative_lsrain(n,p)+TOTAL(lsrain(n,p,*))
            FOR r=0,n_z-1 DO $
               mean_rholvl_pres(r)=MEAN(rholvl_pres(n,p,r,*))
            FOR r=0,n_times-1 DO BEGIN
               IF FINITE(deepconv_termlvl(n,p,r)) eq 1 THEN $
                  deepconv_termlvl(n,p,r)=mean_rholvl_pres(FLOOR(deepconv_termlvl(n,p,r)))
            ENDFOR
            IF TOTAL(where(FINITE(deepconv_termlvl(n,p,*)) eq 1)) gt 0 THEN BEGIN
               mean_deepconv_termlvl(n,p)=mean_deepconv_termlvl(n,p)+TOTAL(deepconv_termlvl(n,p,*),/NaN)
               count_deepconv(n,p)=count_deepconv(n,p)+N_ELEMENTS(where(FINITE(deepconv_termlvl(n,p,*)) eq 1))
            ENDIF
         ENDFOR
      ENDFOR      
      allruns_cvrain(m,*)=REFORM(cvrain,[cvrain_nlon*cvrain_nlat*n_times])
      allruns_shallowrain(m,*)=REFORM(shallowrain,[cvrain_nlon*cvrain_nlat*n_times])
      allruns_midrain(m,*)=REFORM(midrain,[cvrain_nlon*cvrain_nlat*n_times])
      allruns_deeprain(m,*)=REFORM(deeprain,[cvrain_nlon*cvrain_nlat*n_times])
      allruns_lsrain(m,*)=REFORM(lsrain,[cvrain_nlon*cvrain_nlat*n_times])
      allruns_deepconv_termlvl(m,*)=REFORM(deepconv_termlvl,[cvrain_nlon*cvrain_nlat*n_times])
   ENDFOR
   mean_deepconv_termlvl=mean_deepconv_termlvl/count_deepconv

   shallowfrac=cumulative_shallowrain/cumulative_cvrain
   midfrac=cumulative_midrain/cumulative_cvrain
   deepfrac=cumulative_deeprain/cumulative_cvrain
   lsfrac=cumulative_lsrain/(cumulative_cvrain+cumulative_lsrain)   

   IF i eq 0 THEN BEGIN
      cvrain_bins=fltarr(n_bins)
      rain_bins=fltarr(n_bins)
      allruns_nonzero_rain=allruns_rain[where(allruns_rain gt 0.001)]
      allruns_nonzero_rain_sorted=SORT(allruns_nonzero_rain)
      allruns_nonzero_cvrain=allruns_cvrain[where(allruns_cvrain gt 0.001)]
      allruns_nonzero_cvrain_sorted=SORT(allruns_nonzero_cvrain)
      FOR n=0,n_bins-1 DO BEGIN
         cvrain_bins(n)=allruns_nonzero_cvrain(allruns_nonzero_cvrain_sorted(FLOOR(N_ELEMENTS(allruns_nonzero_cvrain)*n/FLOAT(n_bins))))
         rain_bins(n)=allruns_nonzero_rain(allruns_nonzero_rain_sorted(FLOOR(N_ELEMENTS(allruns_nonzero_rain)*n/FLOAT(n_bins))))
      ENDFOR
      print,cvrain_bins
      print,rain_bins
      control_shallowfrac=shallowfrac
      control_midfrac=midfrac
      control_deepfrac=deepfrac
      control_lsfrac=lsfrac
      control_deepconv_termlvl=mean_deepconv_termlvl
      binned_shallow=fltarr(n_sets,n_bins)
      binned_mid=fltarr(n_sets,n_bins)
      binned_deep=fltarr(n_sets,n_bins)
      binned_cvrain=fltarr(n_sets,n_bins)
      binned_lsrain=fltarr(n_sets,n_bins)
      binned_deepconv_termlvl=fltarr(n_sets,n_bins)
      joint_pdf_cvrain_deepconv_termlvl=fltarr(n_sets,n_bins,n_deepconv_term_bins)
   ENDIF

   ;FOR m=0,n_cases-1 DO BEGIN
   ;thisrun_indices=VALUE_LOCATE(cvrain_bins(*),REFORM(allruns_cvrain(*,*)))
   allruns_cvrain_indices=VALUE_LOCATE(cvrain_bins(*),REFORM(allruns_cvrain(*,*)))
   allruns_rain_indices=VALUE_LOCATE(rain_bins(*),REFORM(allruns_rain(*,*)))
   allruns_deepconv_termlvl_indices=fltarr(n_cases,cvrain_nlon*cvrain_nlat*n_times)
   allruns_deepconv_termlvl_indices[where(FINITE(allruns_deepconv_termlvl) eq 1)]=$
      VALUE_LOCATE(deepconv_term_bins(*),REFORM(allruns_deepconv_termlvl[where(FINITE(allruns_deepconv_termlvl) eq 1)]/100.))
   allruns_deepconv_termlvl_indices[where(FINITE(allruns_deepconv_termlvl) eq 0)]=-2
                                ;thisrun_shallow=REFORM(allruns_shallowrain(m,*))
                                ;thisrun_mid=REFORM(allruns_midrain(m,*))
                                ;thisrun_deep=REFORM(allruns_deeprain(m,*))
                                ;thisrun_cvrain=REFORM(allruns_cvrain(m,*))
                                ;thisrun_lsrain=REFORM(allruns_lsrain(m,*))
                                ;thisrun_deepconv_termlvl=REFORM(allruns_deepconv_termlvl(m,*))
   FOR n=0,n_bins-1 DO BEGIN
      IF TOTAL(where(allruns_cvrain_indices eq n)) gt 0 THEN BEGIN
         binned_shallow(i,n)=MEAN(allruns_shallowrain[where(allruns_cvrain_indices eq n)],/NaN);*1./FLOAT(n_cases)+binned_shallow(i,n)
         binned_mid(i,n)=MEAN(allruns_midrain[where(allruns_cvrain_indices eq n)],/NaN);*1./FLOAT(n_cases)+binned_mid(i,n)
         binned_deep(i,n)=MEAN(allruns_deeprain[where(allruns_cvrain_indices eq n)],/NaN) ;*1./FLOAT(n_cases)+binned_deep(i,n)
         binned_cvrain(i,n)=MEAN(allruns_cvrain[where(allruns_cvrain_indices eq n)],/NaN);*1./FLOAT(n_cases)+binned_cvrain(i,n)
         binned_deepconv_termlvl(i,n)=MEAN(allruns_deepconv_termlvl[where(allruns_cvrain_indices eq n)],/NaN) ;*1./FLOAT(n_cases)+binned_deepconv_termlvl(i,n)
         FOR p=0,n_deepconv_term_bins-1 DO BEGIN
            IF TOTAL(where(allruns_deepconv_termlvl_indices eq p)) gt 0 THEN $
               joint_pdf_cvrain_deepconv_termlvl(i,n,p)=N_ELEMENTS(where(allruns_cvrain_indices eq n and $
                                                                         allruns_deepconv_termlvl_indices eq p))
         ENDFOR
      ENDIF
      IF TOTAL(where(allruns_rain_indices eq n)) gt 0 THEN $
         binned_lsrain(i,n)=MEAN(allruns_lsrain[where(allruns_rain_indices eq n)],/NaN) ;*1./FLOAT(n_cases)+binned_lsrain(i,n)     
   ENDFOR
   ;ENDFOR


            
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/tendencies/hadgem3kpp_cascade_phase2_convecttypes.shallowrain_frac.hours0-23.'+$
          set_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2200,SPACE3=500,XOFFSET=1000,YOFFSET=200,TFONT=2,TCHARSIZE=100,$
          SPACE2=700   
   CS,SCALE=2,NCOLS=N_ELEMENTS(shallowfrac_levels)+1,white=[2],/REV
   LEVS,MANUAL=shallowfrac_levels
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2),/hires
   CON,X=cvrain_longitude,Y=cvrain_latitude,FIELD=shallowfrac,/NOLINES,/BLOCK,$
       TITLE='Fraction of convective rain from shallow convection - hours 0-23 of '+set_name+' integrations',CB_TITLE='Fraction'
   AXES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/tendencies/hadgem3kpp_cascade_phase2_convecttypes.midrain_frac.hours0-23.'+$
          set_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2200,SPACE3=500,XOFFSET=1000,YOFFSET=200,TFONT=2,TCHARSIZE=100,$
          SPACE2=700   
   CS,SCALE=2,NCOLS=N_ELEMENTS(midfrac_levels)+1,white=[2],/REV
   LEVS,MANUAL=midfrac_levels
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2),/hires
   CON,X=cvrain_longitude,Y=cvrain_latitude,FIELD=midfrac,/NOLINES,/BLOCK,$
       TITLE='Fraction of convective rain from mid-level convection - hours 0-23 of '+set_name+' integrations',CB_TITLE='Fraction'
   AXES
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/tendencies/hadgem3kpp_cascade_phase2_convecttypes.deeprain_frac.hours0-23.'+$
          set_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2200,SPACE3=500,XOFFSET=1000,YOFFSET=200,TFONT=2,TCHARSIZE=100,$
          SPACE2=700   
   CS,SCALE=2,NCOLS=N_ELEMENTS(deepfrac_levels)+1,white=[2],/REV
   LEVS,MANUAL=deepfrac_levels
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2),/hires
   CON,X=cvrain_longitude,Y=cvrain_latitude,FIELD=deepfrac,/NOLINES,/BLOCK,$
       TITLE='Fraction of convective rain from deep convection - hours 0-23 of '+set_name+' integrations',CB_TITLE='Fraction'
   AXES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/tendencies/hadgem3kpp_cascade_phase2_convecttypes.lsrain_frac.hours0-23.'+$
          set_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2200,SPACE3=500,XOFFSET=1000,YOFFSET=200,TFONT=2,TCHARSIZE=100,$
          SPACE2=700   
   CS,SCALE=26,NCOLS=N_ELEMENTS(lsfrac_levels)+1,/REV
   LEVS,MANUAL=lsfrac_levels
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2),/hires
   CON,X=cvrain_longitude,Y=cvrain_latitude,FIELD=lsfrac,/NOLINES,/BLOCK,$
       TITLE='Fraction of total rain from large-scale rainfall - hours 0-23 of '+set_name+' integrations',CB_TITLE='Fraction'
   AXES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/tendencies/hadgem3kpp_cascade_phase2_convecttypes.deepconv_termlvl.hours0-23.'+$
          set_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1700,SPACE3=500,XOFFSET=1000,YOFFSET=200,TFONT=2,TCHARSIZE=100,$
          SPACE2=700
   CS,SCALE=26,NCOLS=N_ELEMENTS(deepconv_term_levels)+1,/REV
   LEVS,MANUAL=deepconv_term_levels
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2),/hires
   CON,X=cvrain_longitude,Y=cvrain_latitude,FIELD=mean_deepconv_termlvl/100.,/NOLINES,/BLOCK,$
       TITLE='Termination level for deep convection - hours 0-23 of '+set_name+' integrations',CB_TITLE='Pressure (hPa)'
   AXES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/tendencies/hadgem3kpp_cascade_phase2_convecttypes.deepconv_termlvl_cvrain_joint_pdf.'+$
          'hours0-23.'+set_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1700,SPACE3=500,XOFFSET=700,YOFFSET=800,TFONT=2,TCHARSIZE=80,$
          SPACE2=700,/PORTRAIT
   GSET,XMIN=0,XMAX=n_bins,YMIN=0,YMAX=n_deepconv_term_bins
   CS,SCALE=2,NCOLS=N_ELEMENTS(joint_pdf_levels)+1,white=[2]
   LEVS,MANUAL=joint_pdf_levels
   FOR j=0,n_bins-1 DO $
      joint_pdf_cvrain_deepconv_termlvl(i,j,*)=joint_pdf_cvrain_deepconv_termlvl(i,j,*)/TOTAL(joint_pdf_cvrain_deepconv_termlvl(i,j,*))
   CON,X=indgen(n_bins)+0.5,Y=indgen(n_deepconv_term_bins)+0.5,FIELD=REFORM(joint_pdf_cvrain_deepconv_termlvl(i,*,*)),$
       TITLE='Joint PDF of convective rain and termination level for deep convection - hours 0-23 of '+set_name+' integrations',CB_TITLE='Probability (unitless)',$
       /NOLINES,/BLOCK
   AXES,XVALS=indgen(n_bins+1),YVALS=indgen(n_deepconv_term_bins+1),$
        YLABELS=[STRMID(STRTRIM(STRING(deepconv_term_bins),1),0,5),'> '+STRMID(STRTRIM(STRING(deepconv_term_bins(n_deepconv_term_bins-1)),1),0,5)],$
        XLABELS=[STRMID(STRTRIM(STRING(cvrain_bins),1),0,5),'> '+STRMID(STRTRIM(STRING(cvrain_bins(n_bins-1)),1),0,5)],$
        XTITLE='Convective precipitation rate (mm hr!U-1!N)',YTITLE='Pressure (hPa) at level at which deep convection terminates',ORIENTATION=30
   PSCLOSE,/NOVIEW

   IF i gt 0 THEN BEGIN
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/tendencies/hadgem3kpp_cascade_phase2_convecttypes.shallowrain_frac.hours0-23.'+$
          set_name+'-minus-control.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1700,SPACE3=500,XOFFSET=1000,YOFFSET=200,TFONT=2,TCHARSIZE=100,$
             SPACE2=700   
      CS,SCALE=1,NCOLS=N_ELEMENTS(shallowfrac_diff_levels)+1,/REV;,white=[2]
      LEVS,MANUAL=shallowfrac_diff_levels
      MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2),/hires
      CON,X=cvrain_longitude,Y=cvrain_latitude,FIELD=shallowfrac-control_shallowfrac,/NOLINES,/BLOCK,$
          TITLE='Difference in fraction of convective rain from shallow convection - hours 0-23 of '+set_name+' integrations minus control integrations',CB_TITLE='Fraction'
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/tendencies/hadgem3kpp_cascade_phase2_convecttypes.midrain_frac.hours0-23.'+$
          set_name+'-minus-control.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1700,SPACE3=500,XOFFSET=1000,YOFFSET=200,TFONT=2,TCHARSIZE=100,$
             SPACE2=700   
      CS,SCALE=1,NCOLS=N_ELEMENTS(midfrac_diff_levels)+1,/REV ;,white=[2]
      LEVS,MANUAL=midfrac_diff_levels
      MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2),/hires
      CON,X=cvrain_longitude,Y=cvrain_latitude,FIELD=midfrac-control_midfrac,/NOLINES,/BLOCK,$
          TITLE='Difference in fraction of convective rain from mid-level convection - hours 0-23 of '+set_name+' integrations minus control integrations',CB_TITLE='Fraction'
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/tendencies/hadgem3kpp_cascade_phase2_convecttypes.deeprain_frac.hours0-23.'+$
          set_name+'-minus-control.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1700,SPACE3=500,XOFFSET=1000,YOFFSET=200,TFONT=2,TCHARSIZE=100,$
             SPACE2=700   
      CS,SCALE=1,NCOLS=N_ELEMENTS(deepfrac_diff_levels)+1,/REV;,white=[2]
      LEVS,MANUAL=deepfrac_diff_levels
      MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2),/hires
      CON,X=cvrain_longitude,Y=cvrain_latitude,FIELD=deepfrac-control_deepfrac,/NOLINES,/BLOCK,$
          TITLE='Difference in fraction of convective rain from deep convection - hours 0-23 of '+set_name+' integrations minus control integrations',CB_TITLE='Fraction'
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/tendencies/hadgem3kpp_cascade_phase2_convecttypes.lsrain_frac.hours0-23.'+$
          set_name+'-minus-control.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1700,SPACE3=500,XOFFSET=1000,YOFFSET=200,TFONT=2,TCHARSIZE=100,$
             SPACE2=700   
      CS,SCALE=1,NCOLS=N_ELEMENTS(lsfrac_diff_levels)+1,/REV;,white=[2]
      LEVS,MANUAL=lsfrac_diff_levels
      MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2),/hires
      CON,X=cvrain_longitude,Y=cvrain_latitude,FIELD=lsfrac-control_lsfrac,/NOLINES,/BLOCK,$
          TITLE='Difference in fraction of total rain from large-scale rain - hours 0-23 of '+set_name+' integrations minus control integrations',CB_TITLE='Fraction'
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/tendencies/hadgem3kpp_cascade_phase2_convecttypes.deepconv_termlvl.hours0-23.'+$
             set_name+'-minus-control.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1700,SPACE3=500,XOFFSET=1000,YOFFSET=200,TFONT=2,TCHARSIZE=100,$
             SPACE2=700
      CS,SCALE=26,NCOLS=N_ELEMENTS(deepconv_term_diff_levels)+1,/REV,white=[9]
      LEVS,MANUAL=deepconv_term_diff_levels
      MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2),/hires
      CON,X=cvrain_longitude,Y=cvrain_latitude,FIELD=(mean_deepconv_termlvl-control_deepconv_termlvl)/100.,/NOLINES,/BLOCK,$
          TITLE='Difference in termination level for deep convection - hours 0-23 of '+set_name+' integration minus control integrations',$
          CB_TITLE='Pressure (hPa) - note negative values indicate deeper convection'
      AXES
      PSCLOSE,/NOVIEW
   ENDIF
ENDFOR

experiment_labels=['Control','1.5x entrainment','No CMT','1.5x entrainment and no CMT']
diagnosis_labels=['Shallow (left axis)','Mid-level (left axis)','Deep (right axis)']

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/tendencies/hadgem3kpp_cascade_phase2_convecttypes.fraction_binned_by_precip.hours0-23.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2200,SPACE3=500,XOFFSET=1000,YOFFSET=200,TFONT=2,TCHARSIZE=100,$
       SPACE2=700 
GSET,XMIN=0,XMAX=n_bins,YMIN=0,YMAX=0.4
red=FSC_COLOR('red',30)
blue=FSC_COLOR('blue',31)
brown=FSC_COLOR('brown',32)
purple=FSC_COLOR('purple',33)
FOR i=0,n_sets-1 DO BEGIN
   GPLOT,X=indgen(n_bins)+0.5,Y=REFORM(binned_shallow(i,*))/REFORM(binned_cvrain(i,*)),COL=30+i,STYLE=2
   GPLOT,X=indgen(n_bins)+0.5,Y=REFORM(binned_mid(i,*))/REFORM(binned_cvrain(i,*)),COL=30+i,STYLE=1
ENDFOR
AXES,XVALS=indgen(n_bins+1),XLABELS=[STRMID(STRTRIM(STRING(cvrain_bins),1),0,5),'> '+STRMID(STRTRIM(STRING(cvrain_bins(n_bins-1)),1),0,4)],$
     YSTEP=0.05,YMINOR=0.025,NDECS=3,/NORIGHT,XTITLE='Convective precipitation rate (mm hr!U-1!N)',YTITLE='Fraction of convective precipitation from each diagnosis'

GSET,XMIN=0,XMAX=n_bins,YMIN=0.5,YMAX=1.0
FOR i=0,n_sets-1 DO $
   GPLOT,X=indgen(n_bins)+0.5,Y=REFORM(binned_deep(i,*))/REFORM(binned_cvrain(i,*)),COL=30+i,STYLE=0
AXES,YSTEP=0.05,YMINOR=0.025,NDECS=3,/ONLYRIGHT,YTITLE='Fraction of convective precipitation from each diagnosis'
GLEGEND,labels=REVERSE(experiment_labels),COL=REVERSE(indgen(n_sets)+30),LEGPOS=10
GLEGEND,labels=REVERSE(diagnosis_labels),COL=REPLICATE(FSC_COLOR("black"),3),STYLE=REVERSE([2,1,0]),LEGPOS=6
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/tendencies/hadgem3kpp_cascade_phase2_convecttypes.lsfraction_binned_by_precip.hours0-23.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2200,SPACE3=500,XOFFSET=1000,YOFFSET=200,TFONT=2,TCHARSIZE=100,$
       SPACE2=700 
GSET,XMIN=0,XMAX=n_bins,YMIN=0,YMAX=0.36
red=FSC_COLOR('red',30)
blue=FSC_COLOR('blue',31)
brown=FSC_COLOR('brown',32)
purple=FSC_COLOR('purple',33)
FOR i=0,n_sets-1 DO $
   GPLOT,X=indgen(n_bins)+0.5,Y=REFORM(binned_lsrain(i,*))/REFORM(binned_cvrain(i,*)+binned_lsrain(i,*)),COL=30+i,STYLE=2
AXES,XVALS=indgen(n_bins+1),XLABELS=[STRMID(STRTRIM(STRING(rain_bins),1),0,5),'> '+STRMID(STRTRIM(STRING(rain_bins),1),0,4)],$
     YSTEP=0.03,YMINOR=0.01,NDECS=3,XTITLE='Total precipitation rate (mm hr!U-1!N)',YTITLE='Fraction of total precipitation from large-scale precipitation'
GLEGEND,labels=REVERSE(experiment_labels),COL=REVERSE(indgen(n_sets)+30),LEGPOS=9
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/tendencies/hadgem3kpp_cascade_phase2_convecttypes.deepconv_termlvl_binned_by_precip.hours0-23.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2200,SPACE3=500,XOFFSET=1000,YOFFSET=200,TFONT=2,TCHARSIZE=100,$
       SPACE2=700 
GSET,XMIN=0,XMAX=n_bins,YMIN=825,YMAX=125
red=FSC_COLOR('red',30)
blue=FSC_COLOR('blue',31)
brown=FSC_COLOR('brown',32)
purple=FSC_COLOR('purple',33)
FOR i=0,n_sets-1 DO $
   GPLOT,X=indgen(n_bins)+0.5,Y=REFORM(binned_deepconv_termlvl(i,*)/100.),COL=30+i,STYLE=2
AXES,XVALS=indgen(n_bins+1),XLABELS=[STRMID(STRTRIM(STRING(cvrain_bins),1),0,5),$
                                     '> '+STRMID(STRTRIM(STRING(cvrain_bins(n_bins-1)),1),0,4)],$
     YSTEP=-50,YMINOR=-25,NDECS=3,XTITLE='Convective precipitation rate (mm hr!U-1!N)',$
     YTITLE='Pressure (hPa) at level at which deep convection terminates (hPa)'
GLEGEND,labels=REVERSE(experiment_labels),COL=REVERSE(indgen(n_sets)+30),LEGPOS=1
PSCLOSE

STOP
END

