PRO hadgem3kpp_cascade_phase2_tendencies_sum
  
; Plot line graphs of the vertical structure of individual physical tendencies for a given bin
; of precipitation, then plot the sum of all physical tendencies for each variable.

n_sets=4
n_variables=1
um3='/home/ss901165/um_output3'
date_ranges=['04nov09-06nov09','26aug08-28aug08','07dec07-09dec07','07sep06-09sep06','18mar05-20mar05','03dec03-05dec03',$
             '29oct02-01nov02','05jun02-07jun02','26apr02-28apr02','16jan01-18jan01','13nov00-15nov00','28sep00-30sep00']

box=[-5,40,5,200]
n_z=85

n_bins=10
precipitation_deciles=[3,6,9]
n_deciles=N_ELEMENTS(precipitation_deciles)
decile_styles=[1,2,0]

experiment_labels=['Control','1.5x entrainment','No CMT','1.5x entrain and no CMT']
decile_labels=strarr(n_deciles)
FOR i=0,n_deciles-1 DO $
   decile_labels(i)='Decile '+STRTRIM(STRING(precipitation_deciles(i)+1),1)+' of precipitation'

FOR j=0,n_variables-1 DO BEGIN
   CASE j OF
      2 : BEGIN
         longitude_var='longitude'
         latitude_var='latitude'
         z_var='hybrid_ht_1'
         varname='Uinc'
         tendencies=['bdylr.pgrd','conv.pgrd']
         nc_varnames=['unspecified_10','unspecified_3']
         meaning_period=4
         meaning_units='hours'
         n_times=24
         n_frames=n_times/meaning_period
         reverse_color_scale=0
         cb_units=['Zonal-wind increment from boundary-layer (m s!U-1!N hr!U-1!N)',$
                  'Zonal-wind increment from convection (m s!U-1!N hr!U-1!N)',$
                  'Sum of zonal-wind increments from physics (m s!U-1!N hr!U-1!N)']
         cb_units_accel=['Zonal-wind increment from boundary-layer (m s!U-1!N hr!U-1!N) relative to total zonal wind (positive for acceleration)',$
                         'Zonal-wind increment from convection (m s!U-1!N hr!U-1!N) relative to total zonal wind (positive for acceleration)',$
                         'Sum of zonal-wind increments from physics (m s!U-1!N hr!U-1!N) relative to total zonal wind (positive for acceleration)']
         multiplier=3.
         ymax=[700.,100.,100.]
         ymax_accel=[800.,100.,100.]
         ystep=[-20.,-100.,-100.]
         ystep_accel=[-20.,-100.,-100.]
         xmin=[-0.12,-0.12,-0.12]
         xmin_accel=[-0.36,-0.18,-0.20]
         xmax=[0.12,0.18,0.18]
         xmax_accel=[0.04,0.30,0.10]
         xstep=[0.02,0.02,0.03]
         xstep_accel=[0.04,0.03,0.02]
         legpos=[[9,10],$
                 [10,11],$
                 [10,11]]
         legpos_accel=[[1,2],$
                       [9,10],$                      
                       [2,11]]
      END
      1 : BEGIN
         longitude_var='longitude_1'
         latitude_var='latitude_2'
         z_var='hybrid_ht_1'
         varname='Qinc'
         tendencies=['bdylrlscld','conv','lsrain']           
         nc_varnames=['unspecified_14','unspecified_13','unspecified_1']
         meaning_period=4
         meaning_units='hours'
         n_times=24
         n_frames=n_times/meaning_period
         reverse_color_scale=1
         cb_units=['Specific humidity increment from boundary-layer and large-scale cloud (g kg!U-1!N hr!U-1!N)',$
                   'Specific humidity increment from convection (g kg!U-1!N hr!U-1!N)',$
                   'Specific humidity increment from large-scale rain (g kg!U-1!N hr!U-1!N)',$
                   'Sum of specific humidity increments from physics (g kg!U-1!N hr!U-1!N)']                  
         multiplier=3000.         
         ymax=[700.,100.,100.,100.]
         ystep=[-25.,-100.,-100.,-100.]
         xmin=[-0.04,-0.64,-0.05,-0.28]
         xmax=[0.60,0.08,0.06,0.20]
         xstep=[0.04,0.04,0.01,0.04]
         legpos=[[9,10],$
                 [2,1],$
                 [2,9],$
                 [9,1]]
      END
      3 : BEGIN
         longitude_var='longitude_1'
         latitude_var='latitude_2'
         z_var='hybrid_ht_1'
         varname='Tinc'
         tendencies=['bdylrlscld','conv','lsrain']           
         nc_varnames=['unspecified_13','unspecified_12','unspecified']
         meaning_period=4
         meaning_units='hours'
         n_times=24
         n_frames=n_times/meaning_period
         reverse_color_scale=1
         cb_units=['Temperature increment from boundary-layer and large-scale cloud (K hr!U-1!N)',$
                   'Temperature increment from convection (K hr!U-1!N)',$
                   'Temperature increment from large-scale rain (K hr!U-1!N)',$
                   'Sum of temperature increments from physics (K hr!U-1!N)']
         multiplier=3.
         ymax=[700.,100.,100.,100.]
         ystep=[-10.,-100.,-100.,-100.]
         xmin=[-0.15,-0.40,-0.16,-0.40]
         xmax=[0.33,0.90,0.04,0.90]
         xstep=[0.03,0.10,0.02,0.10]
         legpos=[[9,10],$
                 [9,11],$
                 [2,1],$
                 [9,11]]
      END
      0 : BEGIN
         longitude_var='longitude_1'
         latitude_var='latitude_2'
         z_var='hybrid_ht_1'
         varname='QCLinc'
         tendencies=['bdylrlscld','conv','lsrain']           
         nc_varnames=['unspecified_10','unspecified_14','unspecified_2']
         meaning_period=4
         meaning_units='hours'
         n_times=24
         n_frames=n_times/meaning_period
         reverse_color_scale=1
         cb_units=['QCL increment from boundary-layer and large-scale cloud (g kg!U-1!N hr!U-1!N) x 100',$
                   'QCL increment from convection (g kg!U-1!N hr!U-1!N) x 10',$
                   'QCL increment from large-scale rain (g kg !U-1!N hr!U-1!N) x 10',$
                   'Sum of QCL increments from physics (g kg !U-1!N hr!U-1!N) x 10']
         multiplier=[300000.,30000.,30000.]
         ymax=[100.,100.,100.,100.]
         ystep=[-100.,-100.,-100.,-100.]
         xmin=[-0.15,-0.10,-1.50,-0.40]
         xmax=[0.09,1.20,0.10,0.20]
         xstep=[0.03,0.10,0.10,0.04]
         legpos=[[1,2],$
                 [9,11],$
                 [1,9],$
                 [1,3]]
      END
   ENDCASE
   n_tendencies=N_ELEMENTS(tendencies)
   binned_tendencies=fltarr(n_sets,n_tendencies,n_deciles,n_z)
   IF varname eq 'Uinc' THEN $
      binned_tendencies_accel=fltarr(n_sets,n_tendencies,n_deciles,n_z)
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
            runids=['xfrld','xfrlh','xfrll','xfrlp','xfrlt','xfrlx','xfsed','xfseh','xfsel','xfsep','xfset','xfsex']
            set_name='15xentrain_nocmt'
         END
      ENDCASE
      n_cases=N_ELEMENTS(runids)
;      n_cases=1
      rholvl_pres_file=um3+'/'+runids(0)+'.new_stash/'+runids(0)+'.'+date_ranges(0)+'_3hrmeans.p_rholvl_after_ts.nc'
      
      grid_file=um3+'/'+runids(0)+'.new_stash/'+runids(0)+'.'+date_ranges(0)+'_3hrmeans.'+varname+'_'+tendencies(0)+'.nc'
      longitude=OPEN_AND_EXTRACT(grid_file,longitude_var)
      latitude=OPEN_AND_EXTRACT(grid_file,latitude_var)
      z=OPEN_AND_EXTRACT(grid_file,z_var)
      DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
      n_lon=N_ELEMENTS(longitude)
      n_lat=N_ELEMENTS(latitude)

      rholvl_longitude=OPEN_AND_EXTRACT(rholvl_pres_file,'longitude')
      rholvl_latitude=OPEN_AND_EXTRACT(rholvl_pres_file,'latitude_1')
      DEFINE_BOUNDARIES,box,rholvl_latitude,rholvl_longitude,rholvl_box_tx,/LIMIT
      rholvl_nlon=N_ELEMENTS(rholvl_longitude)
      rholvl_nlat=N_ELEMENTS(rholvl_latitude)
      
      rholvl_pres=REFORM(OPEN_AND_EXTRACT(rholvl_pres_file,'p',$
                                          offset=[rholvl_box_tx(1),rholvl_box_tx(0),0,0],$
                                          count=[rholvl_nlon,rholvl_nlat,n_z,n_times]))
      rholvl_pres_mean=fltarr(n_z)
      FOR m=0,n_z-1 DO $
         rholvl_pres_mean(m)=MEAN(rholvl_pres(*,*,m,*))/100.
      
;      STOP

      FOR m=0,n_cases-1 DO BEGIN
         precip_infile=um3+'/'+runids(m)+'.new_stash/'+runids(m)+'.'+date_ranges(m)+'_3hrmeans.rain.nc'
         precip_longitude=OPEN_AND_EXTRACT(precip_infile,'longitude_1')
         precip_latitude=OPEN_AND_EXTRACT(precip_infile,'latitude')
         DEFINE_BOUNDARIES,box,precip_latitude,precip_longitude,precip_box_tx,/LIMIT
         precip_nlon=N_ELEMENTS(precip_longitude)
         precip_nlat=N_ELEMENTS(precip_latitude)
         thisrun_latavg_precip=fltarr(precip_nlon,n_frames)
         IF m eq 0 THEN BEGIN
            allruns_latavg_precip=fltarr(n_cases,precip_nlon,n_frames)
                                ;allruns_precip_bins=fltarr(n_cases,n_bins)
            allruns_precip=fltarr(n_cases,precip_nlon*precip_nlat*n_times) 
            allruns_indices=fltarr(n_cases,precip_nlon*precip_nlat*n_times)
            composite_latavg_precip=fltarr(precip_nlon,n_frames)
         ENDIF
         precip=REFORM(OPEN_AND_EXTRACT(precip_infile,'rain',offset=[precip_box_tx(1),precip_box_tx(0),0,0],$
                                        count=[precip_nlon,precip_nlat,1,n_times]))*3600.
         allruns_precip(m,*)=REFORM(precip,[precip_nlon*precip_nlat*n_times])
         
      ENDFOR
                                ; Compute bins based on control simulation precipitation
      IF i eq 0 THEN BEGIN
         precip_bins=fltarr(n_bins)
         allruns_nonzero_precip=allruns_precip[where(allruns_precip gt 0.001)]
         allruns_nonzero_precip_sorted=SORT(allruns_nonzero_precip)
         FOR n=0,n_bins-1 DO $
            precip_bins(n)=allruns_nonzero_precip(allruns_nonzero_precip_sorted(FLOOR(N_ELEMENTS(allruns_nonzero_precip)*n/FLOAT(n_bins))))
         print,precip_bins
      ENDIF     
            
      FOR k=0,n_tendencies-1 DO BEGIN
         count_points=fltarr(n_deciles,n_z)
         thisrun_tendency=fltarr(n_lon,n_lat,n_z,n_frames)
         FOR m=0,n_cases-1 DO BEGIN
            infile=um3+'/'+runids(m)+'.new_stash/'+runids(m)+'.'+date_ranges(m)+'_3hrmeans.'+varname+'_'+tendencies(k)+'.nc'
            print,infile
            grid_file=um3+'/'+runids(m)+'.new_stash/'+runids(m)+'.'+date_ranges(m)+'_3hrmeans.'+varname+'_'+tendencies(0)+'.nc'
            longitude=OPEN_AND_EXTRACT(grid_file,longitude_var)
            latitude=OPEN_AND_EXTRACT(grid_file,latitude_var)
            z=OPEN_AND_EXTRACT(grid_file,z_var)
            DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT

            IF varname eq 'Uinc' THEN BEGIN
               total_u_infile=um3+'/'+runids(m)+'.new_stash/'+runids(m)+'.'+date_ranges(m)+'_3hrmeans.u_after_ts.pgrd.nc'
               u_longitude=OPEN_AND_EXTRACT(total_u_infile,'longitude')
               u_latitude=OPEN_AND_EXTRACT(total_u_infile,'latitude')
               DEFINE_BOUNDARIES,box,u_latitude,u_longitude,u_box_tx,/LIMIT
               u_nlon=N_ELEMENTS(u_longitude)
               u_nlat=N_ELEMENTS(u_latitude)
               total_u=OPEN_AND_EXTRACT(total_u_infile,'u',$
                                        offset=[u_box_tx(1),u_box_tx(0),0,0],$
                                        count=[u_nlon,u_nlat,n_z,n_times])
            ENDIF
            tendency=OPEN_AND_EXTRACT(infile,nc_varnames(k),$
                                      offset=[box_tx(1),box_tx(0),0,0],$                               
                                      count=[n_lon,n_lat,n_z,n_times])*multiplier(k)
            
            thisrun_indices=VALUE_LOCATE(precip_bins(*),REFORM(allruns_precip(m,*)))
            FOR p=0,n_z-1 DO BEGIN
               thislev_tendency=REFORM(tendency(*,*,p,*))
               FOR r=0,n_deciles-1 DO BEGIN
                  IF TOTAL(where(thisrun_indices eq precipitation_deciles(r))) gt 0 THEN BEGIN
                     binned_tendencies(i,k,r,p)=TOTAL(thislev_tendency[where(thisrun_indices eq precipitation_deciles(r))],/NaN)$
                                                +binned_tendencies(i,k,r,p)
                     IF varname eq 'Uinc' THEN BEGIN
                        thislev_totalu=REFORM(total_u(*,*,p,*))
                        thisdecile_tendencies=thislev_tendency[where(thisrun_indices eq precipitation_deciles(r))]
                        thisdecile_totalu=thislev_totalu[where(thisrun_indices eq precipitation_deciles(r))]                        
                        binned_tendencies_accel(i,k,r,p)=TOTAL(ABS(thisdecile_tendencies)*SIGN(thisdecile_totalu)*SIGN(thisdecile_tendencies),/NaN)$
                                                         +binned_tendencies_accel(i,k,r,p)
                     ENDIF
                     count_points(r,p)=count_points(r,p)+N_ELEMENTS(thislev_tendency[where(thisrun_indices eq precipitation_deciles(r))])
                  ENDIF
               ENDFOR
            ENDFOR
         ENDFOR
         binned_tendencies(i,k,*,*)=binned_tendencies(i,k,*,*)/FLOAT(count_points(*,*))
         IF varname eq 'Uinc' THEN $
            binned_tendencies_accel(i,k,*,*)=binned_tendencies_accel(i,k,*,*)/FLOAT(count_points(*,*))
      ENDFOR
   ENDFOR

   FOR k=0,n_tendencies-1 DO BEGIN
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/tendencies/hadgem3kpp_cascade_phase2_tendencies_sum.'+$
             varname+'_'+tendencies(k)+'.precip_deciles';+STRTRIM(STRING(precipitation_deciles+1),1)+'.hours0-71.allexpts.ps'
      FOR m=0,n_deciles-1 DO $
         psfile=psfile+STRTRIM(STRING(precipitation_deciles(m)+1),1)
      IF varname eq 'Uinc' THEN BEGIN
         psfile=psfile+'.with_acceleration.hours0-71.allexpts.ps'
      ENDIF ELSE $
         psfile=psfile+'.hours0-71.allexpts.ps'
      print,psfile
      PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1400,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT
      GSET,XMIN=xmin(k),XMAX=xmax(k),YMIN=MAX(rholvl_pres_mean),YMAX=ymax(k)
      red=FSC_COLOR('red',30)
      blue=FSC_COLOR('blue',31)
      brown=FSC_COLOR('brown',32)
      purple=FSC_COLOR('purple',33)
      FOR i=0,n_sets-1 DO BEGIN
         FOR m=0,n_deciles-1 DO BEGIN
            toplot=REFORM(binned_tendencies(i,k,m,*))
            GPLOT,X=toplot[where(rholvl_pres_mean ge ymax(k))],Y=rholvl_pres_mean[where(rholvl_pres_mean ge ymax(k))],COL=30+i,$
                  STYLE=decile_styles(m)
         ENDFOR
      ENDFOR
      AXES,XSTEP=xstep(k),xminor=xstep(k)/2.,XTITLE=cb_units(k),$
           YTITLE='Pressure (hPa)',NDECS=2,YVALS=indgen((1000.-ymax(k))/ABS(ystep(k))+1)*ABS(ystep(k))+ymax(k)
      GPLOT,X=[0,0],Y=[MAX(rholvl_pres_mean),ymax(k)],STYLE=2
      GLEGEND,labels=REVERSE(experiment_labels),COL=REVERSE(indgen(n_sets)+30),LEGPOS=legpos(0,k)
      GLEGEND,labels=REVERSE(decile_labels),STYLE=REVERSE(decile_styles),LEGPOS=legpos(1,k)
      PSCLOSE,/NOVIEW
      
      IF varname eq 'Uinc' THEN BEGIN
         psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/tendencies/hadgem3kpp_cascade_phase2_tendencies_sum.'+$
             varname+'_'+tendencies(k)+'.with_acceleration.precip_deciles';+STRTRIM(STRING(precipitation_deciles+1),1)+'.hours0-71.allexpts.ps'
         FOR m=0,n_deciles-1 DO $
            psfile=psfile+STRTRIM(STRING(precipitation_deciles(m)+1),1)         
         psfile=psfile+'.hours0-71.allexpts.ps'
         print,psfile
         PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1400,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT
         GSET,XMIN=xmin_accel(k),XMAX=xmax_accel(k),YMIN=MAX(rholvl_pres_mean),YMAX=ymax_accel(k)
         red=FSC_COLOR('red',30)
         blue=FSC_COLOR('blue',31)
         brown=FSC_COLOR('brown',32)
         purple=FSC_COLOR('purple',33)
         FOR i=0,n_sets-1 DO BEGIN
            FOR m=0,n_deciles-1 DO BEGIN
               toplot=REFORM(binned_tendencies_accel(i,k,m,*))
               GPLOT,X=toplot[where(rholvl_pres_mean ge ymax_accel(k))],Y=rholvl_pres_mean[where(rholvl_pres_mean ge ymax_accel(k))],COL=30+i,$
                     STYLE=decile_styles(m)
            ENDFOR
         ENDFOR
         AXES,XSTEP=xstep_accel(k),xminor=xstep_accel(k)/2.,XTITLE=cb_units_accel(k),$
              YTITLE='Pressure (hPa)',NDECS=2,YVALS=indgen((1000.-ymax_accel(k))/ABS(ystep_accel(k))+1)*ABS(ystep_accel(k))+ymax_accel(k)
         GPLOT,X=[0,0],Y=[MAX(rholvl_pres_mean),ymax_accel(k)],STYLE=2
         GLEGEND,labels=REVERSE(experiment_labels),COL=REVERSE(indgen(n_sets)+30),LEGPOS=legpos_accel(0,k)
         GLEGEND,labels=REVERSE(decile_labels),STYLE=REVERSE(decile_styles),LEGPOS=legpos_accel(1,k)
         PSCLOSE,/NOVIEW
      ENDIF
   ENDFOR
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/tendencies/hadgem3kpp_cascade_phase2_tendencies_sum.'+$
          varname+'_sumphys.precip_deciles'
   FOR m=0,n_deciles-1 DO $
         psfile=psfile+STRTRIM(STRING(precipitation_deciles(m)+1),1)
   psfile=psfile+'.hours0-71.allexpts.ps'
   print,psfile
   PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1400,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT
   GSET,XMIN=xmin(n_tendencies),XMAX=xmax(n_tendencies),YMIN=MAX(rholvl_pres_mean),YMAX=ymax(n_tendencies)
   red=FSC_COLOR('red',30)
   blue=FSC_COLOR('blue',31)
   brown=FSC_COLOR('brown',32)
   purple=FSC_COLOR('purple',33)
   FOR i=0,n_sets-1 DO BEGIN
      FOR m=0,n_deciles-1 DO BEGIN
         binned_tendencies_sum=fltarr(n_z)
         FOR k=0,n_z-1 DO $
            binned_tendencies_sum(k)=TOTAL(binned_tendencies(i,*,m,k))         
         GPLOT,X=binned_tendencies_sum[where(rholvl_pres_mean ge ymax(n_tendencies))],$
               Y=rholvl_pres_mean[where(rholvl_pres_mean ge ymax(n_tendencies))],COL=30+i,$
               STYLE=decile_styles(m)
      ENDFOR
   ENDFOR
   AXES,XSTEP=xstep(n_tendencies),xminor=xstep(n_tendencies)/2.,XTITLE=cb_units(n_tendencies),$
        YTITLE='Pressure (hPa)',NDECS=2,YVALS=indgen((1000.-ymax(n_tendencies))/ABS(ystep(n_tendencies))+1)$
        *ABS(ystep(n_tendencies))+ymax(n_tendencies)
   GPLOT,X=[0,0],Y=[MAX(rholvl_pres_mean),ymax(n_tendencies)],STYLE=2
   GLEGEND,labels=REVERSE(experiment_labels),COL=REVERSE(indgen(n_sets)+30),LEGPOS=legpos(0,n_tendencies)
   GLEGEND,labels=REVERSE(decile_labels),STYLE=REVERSE(decile_styles),LEGPOS=legpos(1,n_tendencies)
   PSCLOSE,/NOVIEW

   IF varname eq 'Uinc' THEN BEGIN
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/tendencies/hadgem3kpp_cascade_phase2_tendencies_sum.'+$
             varname+'_sumphys.with_acceleration.precip_deciles' ;+STRTRIM(STRING(precipitation_deciles+1),1)+'.hours0-71.allexpts.ps'
      FOR m=0,n_deciles-1 DO $
         psfile=psfile+STRTRIM(STRING(precipitation_deciles(m)+1),1)         
      psfile=psfile+'.hours0-71.allexpts.ps'
      print,psfile
      PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1400,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT
      GSET,XMIN=xmin_accel(n_tendencies),XMAX=xmax_accel(n_tendencies),YMIN=MAX(rholvl_pres_mean),YMAX=ymax_accel(n_tendencies)
      red=FSC_COLOR('red',30)
      blue=FSC_COLOR('blue',31)
      brown=FSC_COLOR('brown',32)
      purple=FSC_COLOR('purple',33)
      FOR i=0,n_sets-1 DO BEGIN
         FOR m=0,n_deciles-1 DO BEGIN
            FOR k=0,n_z-1 DO $
               binned_tendencies_sum(k)=TOTAL(binned_tendencies_accel(i,*,m,k))
            GPLOT,X=binned_tendencies_sum[where(rholvl_pres_mean ge ymax_accel(n_tendencies))],$
                  Y=rholvl_pres_mean[where(rholvl_pres_mean ge ymax_accel(n_tendencies))],COL=30+i,$
                  STYLE=decile_styles(m)
         ENDFOR
      ENDFOR
      AXES,XSTEP=xstep_accel(n_tendencies),xminor=xstep_accel(n_tendencies)/2.,XTITLE=cb_units_accel(n_tendencies),$
           YTITLE='Pressure (hPa)',NDECS=2,YVALS=indgen((1000.-ymax_accel(n_tendencies))/ABS(ystep_accel(n_tendencies))+1)$
           *ABS(ystep_accel(n_tendencies))+ymax_accel(n_tendencies)
      GPLOT,X=[0,0],Y=[MAX(rholvl_pres_mean),ymax_accel(n_tendencies)],STYLE=2
      GLEGEND,labels=REVERSE(experiment_labels),COL=REVERSE(indgen(n_sets)+30),LEGPOS=legpos_accel(0,n_tendencies)
      GLEGEND,labels=REVERSE(decile_labels),STYLE=REVERSE(decile_styles),LEGPOS=legpos_accel(1,n_tendencies)
      PSCLOSE,/NOVIEW
   ENDIF
ENDFOR

STOP

END
