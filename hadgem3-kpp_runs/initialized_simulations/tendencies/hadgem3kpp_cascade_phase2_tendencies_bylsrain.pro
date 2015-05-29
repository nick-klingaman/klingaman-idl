PRO hadgem3kpp_cascade_phase2_tendencies_bylsrain

; Plots PDFs of tendencies binned by large-scale rain rate

n_sets=4
n_variables=1
um3='/home/ss901165/um_output3'
date_ranges=['04nov09-06nov09','26aug08-28aug08','07dec07-09dec07','07sep06-09sep06','18mar05-20mar05','03dec03-05dec03',$
             '29oct02-01nov02','05jun02-07jun02','26apr02-28apr02','16jan01-18jan01','13nov00-15nov00','28sep00-30sep00']

box=[-5,40,5,200]
n_z=85
n_bins=20
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
;   n_cases=2

   FOR j=0,n_variables-1 DO BEGIN
      CASE j OF
         2 : BEGIN
            grid_file=um3+'/'+runids(0)+'.new_stash/'+runids(0)+'.'+date_ranges(0)+'_3hrmeans.Uinc_advect.nc'
            rholvl_pres_file=um3+'/'+runids(0)+'.new_stash/'+runids(0)+'.'+date_ranges(0)+'_3hrmeans.p_rholvl_after_ts.nc'
            longitude_var='longitude'
            latitude_var='latitude'
            z_var='hybrid_ht'
            varname='Uinc'
            tendencies=['advect.pgrd','bdylr.pgrd','conv.pgrd']           
            nc_varnames=['unspecified_4','unspecified_10','unspecified_3']
            pdf_levels=[['-0.075','-0.065','-0.055','-0.045','-0.035','-0.025','-0.015','-0.005','0.005','0.015','0.025','0.035','0.045','0.055','0.065','0.075'],$
                        ['-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15'],$
                        ['-0.075','-0.065','-0.055','-0.045','-0.035','-0.025','-0.015','-0.005','0.005','0.015','0.025','0.035','0.045','0.055','0.065','0.075']]
            meaning_period=4
            meaning_units='hours'
            n_times=24
            n_frames=n_times/meaning_period
            reverse_color_scale=0
            cb_units=['Zonal wind increment from advection (m s!U-1!N hr!U-1!N)',$
                      'Zonal wind increment from boundary layer (m s!U-1!N hr!U-1!N)',$
                      'Zonal wind increment from convection (m s!U-1!N hr!U-1!N)']
            multiplier=3.
            ymax=[100.,600.,100.]
            scale=1
         END
         1 : BEGIN
            grid_file=um3+'/'+runids(0)+'.new_stash/'+runids(0)+'.'+date_ranges(0)+'_3hrmeans.Qinc_advect.nc'
            rholvl_pres_file=um3+'/'+runids(0)+'.new_stash/'+runids(0)+'.'+date_ranges(0)+'_3hrmeans.p_rholvl_after_ts.nc'
            longitude_var='longitude_1'
            latitude_var='latitude_2'
            z_var='hybrid_ht_1'
            varname='Qinc'
            tendencies=['advect','bdylrlscld','conv','lsrain','lwrad','swrad']           
            nc_varnames=['unspecified_1','unspecified_14','unspecified_13','unspecified_1','unspecified_2','unspecified_2']
            pdf_levels=[['-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15'],$
                        ['-0.45','-0.39','-0.33','-0.27','-0.21','-0.15','-0.09','-0.03','0.03','0.09','0.15','0.21','0.27','0.33','0.39','0.45'],$
                        ['-0.45','-0.39','-0.33','-0.27','-0.21','-0.15','-0.09','-0.03','0.03','0.09','0.15','0.21','0.27','0.33','0.39','0.45'],$
                        ['-0.022','-0.019','-0.016','-0.013','-0.010','-0.007','-0.004','-0.001','0.001','0.004','0.007','0.010','0.013','0.016','0.019','0.022'],$
                        ['-0.0375','-0.0325','-0.0275','-0.0225','-0.0175','-0.0125','-0.0075','-0.0025','0.0025','0.0075','0.0125','0.0175','0.0225','0.0275','0.0325','0.0375'],$
                        ['-0.075','-0.065','-0.055','-0.045','-0.035','-0.025','-0.015','-0.005','0.005','0.015','0.025','0.035','0.045','0.055','0.065','0.075']]
            
            meaning_period=4
            meaning_units='hours'
            n_times=24
            n_frames=n_times/meaning_period
            reverse_color_scale=1
            cb_units=['Specific humidity increment from advection (g kg!U-1!N hr!U-1!N)',$
                      'Specific humidity increment from boundary-layer and large-scale cloud (g kg!U-1!N hr!U-1!N)',$
                      'Specific humidity increment from convection (g kg!U-1!N hr!U-1!N)',$
                      'Specific humidity increment from large-scale rain (g kg!U-1!N hr!U-1!N)',$
                      'Specific humidity increment from longwave radiation (g kg!U-1!N hr!U-1!N)',$
                      'Specific humidity increment from shortwave radiation (g kg!U-1!N hr!U-1!N)']
            multiplier=3000.
            ymax=[100.,700.,100.,100.,100.,100.]
            scale=1
         END
         5 : BEGIN
            grid_file=um3+'/'+runids(0)+'.new_stash/'+runids(0)+'.'+date_ranges(0)+'_3hrmeans.Tinc_advect.nc'
            rholvl_pres_file=um3+'/'+runids(0)+'.new_stash/'+runids(0)+'.'+date_ranges(0)+'_3hrmeans.p_rholvl_after_ts.nc'
            longitude_var='longitude_1'
            latitude_var='latitude_2'
            z_var='hybrid_ht_1'
            varname='Tinc'
            tendencies=['advect','bdylrlscld','conv','lsrain','lwrad','swrad']
            nc_varnames=['unspecified','unspecified_13','unspecified_12','unspecified','unspecified_1','unspecified_1']
            pdf_levels=[['-0.30','-0.26','-0.22','-0.18','-0.14','-0.10','-0.06','-0.02','0.02','0.06','0.10','0.14','0.18','0.22','0.26','0.30'],$
                        ['-0.30','-0.26','-0.22','-0.18','-0.14','-0.10','-0.06','-0.02','0.02','0.06','0.10','0.14','0.18','0.22','0.26','0.30'],$
                        ['-0.45','-0.39','-0.33','-0.27','-0.21','-0.15','-0.09','-0.03','0.03','0.09','0.15','0.21','0.27','0.33','0.39','0.45'],$
                        ['-0.075','-0.065','-0.055','-0.045','-0.035','-0.025','-0.015','-0.005','0.005','0.015','0.025','0.035','0.045','0.055','0.065','0.075'],$
                        ['-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15'],$
                        ['-0.075','-0.065','-0.055','-0.045','-0.035','-0.025','-0.015','-0.005','0.005','0.015','0.025','0.035','0.045','0.055','0.065','0.075']]
            stddev_spacing=['0.02','0.02','0.02','0.02','0.02','0.02']
            meaning_period=4
            meaning_units='hours'
            n_times=24
            n_frames=n_times/meaning_period
            reverse_color_scale=0
            cb_units=['Temperature increment from advection (g kg!U-1!N hr!U-1!N)',$
                      'Temperature increment from boundary-layer and large-scale cloud (g kg!U-1!N hr!U-1!N)',$
                      'Temperature increment from convection (g kg!U-1!N hr!U-1!N)',$
                      'Temperature increment from large-scale rain (g kg!U-1!N hr!U-1!N)',$
                      'Temperature increment from longwave radiation (g kg!U-1!N hr!U-1!N)',$
                      'Temperature increment from shortwave radiation (g kg!U-1!N hr!U-1!N)']
            multiplier=3.
            ymax=[100.,700.,100.,100.,100.,100.]
            scale=1
         END
         3 : BEGIN
            grid_file=um3+'/'+runids(0)+'.new_stash/'+runids(0)+'.'+date_ranges(0)+'_3hrmeans.3d_conv_rain.nc'
            rholvl_pres_file=um3+'/'+runids(0)+'.new_stash/'+runids(0)+'.'+date_ranges(0)+'_3hrmeans.p_rholvl_after_ts.nc'
            longitude_var='longitude_1'
            latitude_var='latitude_2'
            z_var='hybrid_ht_1'
            varname='3d_conv_rain'
            tendencies=['']
            nc_varnames=['field1697']
            pdf_levels=['0.06','0.12','0.18','0.24','0.30','0.36','0.42','0.48','0.54','0.60','0.66','0.72','0.78','0.84','0.90']
            meaning_period=4
            meaning_units='hours'
            n_times=24
            n_frames=n_times/meaning_period
            reverse_color_scale=1
            cb_units='Convective rain rate (mm hr!U-1!N) on model levels'
            multiplier=3600.
            ymax=[100.]
            scale=26
         END
         4 : BEGIN
            grid_file=um3+'/'+runids(0)+'.new_stash/'+runids(0)+'.'+date_ranges(0)+'_3hrmeans.lsrain_rholvl.nc'
            rholvl_pres_file=um3+'/'+runids(0)+'.new_stash/'+runids(0)+'.'+date_ranges(0)+'_3hrmeans.p_rholvl_after_ts.nc'
            longitude_var='longitude'
            latitude_var='latitude'
            z_var='hybrid_ht'
            varname='lsrain_rholvl'
            tendencies=['']
            nc_varnames=['lsrain_1']
            pdf_levels=['0.01','0.02','0.03','0.04','0.05','0.06','0.07','0.08','0.09','0.10','0.12','0.14','0.16','0.18','0.20','0.22']
            meaning_period=4
            meaning_units='hours'
            n_times=24
            n_frames=n_times/meaning_period
            reverse_color_scale=1
            cb_units='Large-scale rain rate (mm hr!U-1!N) on model levels'
            multiplier=3600.
            ymax=[100.]
            scale=26
         END
         0 : BEGIN
            grid_file=um3+'/'+runids(0)+'.new_stash/'+runids(0)+'.'+date_ranges(0)+'_3hrmeans.lsrain_rholvl.nc'
            rholvl_pres_file=um3+'/'+runids(0)+'.new_stash/'+runids(0)+'.'+date_ranges(0)+'_3hrmeans.p_rholvl_after_ts.nc'
            longitude_var='longitude'
            latitude_var='latitude'
            z_var='hybrid_ht'
            varname='lsrain_lssnow'
            tendencies=['']
            nc_varnames=['lsrain_1','lssnow']
            pdf_levels=['0.01','0.02','0.03','0.04','0.05','0.06','0.07','0.08','0.09','0.10','0.12','0.14','0.16','0.18','0.20','0.22']
            meaning_period=4
            meaning_units='hours'
            n_times=24
            n_frames=n_times/meaning_period
            reverse_color_scale=1
            cb_units='Large-scale rain rate plus large-scale snow rate (mm hr!U-1!N) on model levels'
            multiplier=3600.
            ymax=[100.]
            scale=26
         END
         5 : BEGIN
            grid_file=um3+'/'+runids(0)+'.new_stash/'+runids(0)+'.'+date_ranges(0)+'_3hrmeans.QCLinc_advect.nc'
            rholvl_pres_file=um3+'/'+runids(0)+'.new_stash/'+runids(0)+'.'+date_ranges(0)+'_3hrmeans.p_rholvl_after_ts.nc'
            longitude_var='longitude_1'
            latitude_var='latitude_2'
            z_var='hybrid_ht_1'
            varname='QCLinc'
            tendencies=['advect','bdylr','bdylrlscld','conv','lsrain','lwrad','swrad']
            nc_varnames=['unspecified_2','unspecified_8','unspecified_10','unspecified_14','unspecified_2','unspecified_3','unspecified_3']            
            pdf_levels=[['-0.45','-0.39','-0.33','-0.27','-0.21','-0.15','-0.09','-0.03','0.03','0.09','0.15','0.21','0.27','0.33','0.39','0.45'],$
                        ['-0.075','-0.065','-0.055','-0.045','-0.035','-0.025','-0.015','-0.005','0.005','0.015','0.025','0.035','0.045','0.055','0.065','0.075'],$
                        ['-0.075','-0.065','-0.055','-0.045','-0.035','-0.025','-0.015','-0.005','0.005','0.015','0.025','0.035','0.045','0.055','0.065','0.075'],$
                        ['-1.05','-0.91','-0.77','-0.63','-0.49','-0.35','-0.21','-0.07','0.07','0.21','0.35','0.49','0.63','0.77','0.91','1.05'],$
                        ['-1.05','-0.91','-0.77','-0.63','-0.49','-0.35','-0.21','-0.07','0.07','0.21','0.35','0.49','0.63','0.77','0.91','1.05'],$
                        ['-0.30','-0.26','-0.22','-0.18','-0.14','-0.10','-0.06','-0.02','0.02','0.06','0.10','0.14','0.18','0.22','0.26','0.30'],$
                        ['-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15']]
            stddev_spacing=['0.10','0.05','0.05','0.04','0.04','0.04','0.04']
            meaning_period=4
            meaning_units='hours'
            n_times=24
            n_frames=n_times/meaning_period
            reverse_color_scale=1
            cb_units=['QCL increment from advection (g kg!U-1!N hr!U-1!N) x 100',$
                      'QCL increment from boundary layer (g kg!U-1!N hr!U-1!N) x 100',$
                      'QCL increment from boundary layer and large-scale cloud (g kg!U-1!N hr!U-1!N) x 100',$
                      'QCL increment from convection (g kg!U-1!N hr!U-1!N) x 10',$
                      'QCL increment from large-scale rain (g kg!U-1!N hr!U-1!N) x 10',$
                      'QCL increment from longwave radiation (g kg!U-1!N hr!U-1!N) x 100',$
                      'QCL increment from shortwave radiation (g kg!U-1!N hr!U-1!N) x 100']
            multiplier=[300000.,300000.,300000.,30000.,30000.,300000.,300000.]
            ymax=[100.,100.,100.,100.,100.,100.,100.]
            scale=1
         END
      ENDCASE
      n_tendencies=N_ELEMENTS(tendencies)
     
      IF tendencies(0) ne '' THEN BEGIN
         grid_file=um3+'/'+runids(0)+'.new_stash/'+runids(0)+'.'+date_ranges(0)+'_3hrmeans.'+varname+'_'+tendencies(0)+'.nc'
      ENDIF ELSE BEGIN
         IF varname eq 'lsrain_lssnow' THEN BEGIN
            grid_file=um3+'/'+runids(0)+'.new_stash/'+runids(0)+'.'+date_ranges(0)+'_3hrmeans.lsrain_rholvl.nc'
         ENDIF ELSE $
            grid_file=um3+'/'+runids(0)+'.new_stash/'+runids(0)+'.'+date_ranges(0)+'_3hrmeans.'+varname+'.nc'
      ENDELSE
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

      FOR m=0,n_cases-1 DO BEGIN
         precip_infile=um3+'/'+runids(m)+'.new_stash/'+runids(m)+'.'+date_ranges(m)+'_3hrmeans.lsrain.nc'
         precip_longitude=OPEN_AND_EXTRACT(precip_infile,'longitude')
         precip_latitude=OPEN_AND_EXTRACT(precip_infile,'latitude_1')
         DEFINE_BOUNDARIES,box,precip_latitude,precip_longitude,precip_box_tx,/LIMIT
         precip_nlon=N_ELEMENTS(precip_longitude)
         precip_nlat=N_ELEMENTS(precip_latitude)
         thisrun_latavg_precip=fltarr(precip_nlon,n_frames)
         IF m eq 0 THEN BEGIN
            allruns_latavg_precip=fltarr(n_cases,precip_nlon,n_frames)
            allruns_precip=fltarr(n_cases,precip_nlon*precip_nlat*n_times) 
            allruns_indices=fltarr(n_cases,precip_nlon*precip_nlat*n_times)
            composite_latavg_precip=fltarr(precip_nlon,n_frames)
         ENDIF
         precip=REFORM(OPEN_AND_EXTRACT(precip_infile,'lsrain',offset=[precip_box_tx(1),precip_box_tx(0),0,0],$
                                 count=[precip_nlon,precip_nlat,1,n_times]))*3600.
         allruns_precip(m,*)=REFORM(precip,[precip_nlon*precip_nlat*n_times])
;         FOR n=0,n_frames-1 DO BEGIN
;            FOR p=0,precip_nlon-1 DO BEGIN
;               thisrun_latavg_precip(p,n)=MEAN(precip(p,*,n*meaning_period:(n+1)*meaning_period-1))
;               composite_latavg_precip(p,n)=composite_latavg_precip(p,n)+thisrun_latavg_precip(p,n)*1./FLOAT(n_cases)
;            ENDFOR
;         ENDFOR
;         allruns_latavg_precip(m,*,*)=thisrun_latavg_precip(*,*)
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
;         thisrun_tendency=fltarr(n_lon,n_lat,n_z,n_frames)
;         thisrun_latavg_tendency=fltarr(n_lon,n_z,n_frames)
;         allruns_latavg_tendency=fltarr(n_cases,n_lon,n_z,n_frames)
;         composite_tendency=fltarr(n_lon,n_lat,n_z,n_frames)
;         composite_latavg_tendency=fltarr(n_lon,n_z,n_frames)
         binned_tendency=fltarr(n_bins,n_z)
         n_points=fltarr(n_bins)
         mean_precip_in_bin=fltarr(n_bins)
         FOR m=0,n_cases-1 DO BEGIN
            IF tendencies(k) ne '' THEN BEGIN              
               infile=um3+'/'+runids(m)+'.new_stash/'+runids(m)+'.'+date_ranges(m)+'_3hrmeans.'+varname+'_'+tendencies(k)+'.nc'
               print,infile
            ENDIF ELSE BEGIN
               IF varname eq 'lsrain_lssnow' THEN BEGIN
                  infile_rain=um3+'/'+runids(m)+'.new_stash/'+runids(m)+'.'+date_ranges(m)+'_3hrmeans.lsrain_rholvl.nc'
                  infile_snow=um3+'/'+runids(m)+'.new_stash/'+runids(m)+'.'+date_ranges(m)+'_3hrmeans.lssnow_rholvl.nc'
                  print,infile_rain
               ENDIF ELSE BEGIN
                  infile=um3+'/'+runids(m)+'.new_stash/'+runids(m)+'.'+date_ranges(m)+'_3hrmeans.'+varname+'.nc'            
                  print,infile
               ENDELSE
            ENDELSE
            IF tendencies(k) ne '' THEN BEGIN
               grid_file=um3+'/'+runids(m)+'.new_stash/'+runids(m)+'.'+date_ranges(m)+'_3hrmeans.'+varname+'_'+tendencies(0)+'.nc'
            ENDIF ELSE BEGIN
               IF varname eq 'lsrain_lssnow' THEN BEGIN
                  grid_file=um3+'/'+runids(m)+'.new_stash/'+runids(m)+'.'+date_ranges(m)+'_3hrmeans.lsrain_rholvl.nc'
               ENDIF ELSE $
                  grid_file=um3+'/'+runids(m)+'.new_stash/'+runids(m)+'.'+date_ranges(m)+'_3hrmeans.'+varname+'.nc' 
            ENDELSE
            longitude=OPEN_AND_EXTRACT(grid_file,longitude_var)
            latitude=OPEN_AND_EXTRACT(grid_file,latitude_var)
            z=OPEN_AND_EXTRACT(grid_file,z_var)
            DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
            IF varname ne 'lsrain_lssnow' THEN BEGIN
               tendency=OPEN_AND_EXTRACT(infile,nc_varnames(k),$
                                         offset=[box_tx(1),box_tx(0),0,0],$
                                         count=[n_lon,n_lat,n_z,n_times])*multiplier(k)
            ENDIF ELSE BEGIN
               tendency_rain=OPEN_AND_EXTRACT(infile_rain,nc_varnames(0),$
                                              offset=[box_tx(1),box_tx(0),0,0],$
                                              count=[n_lon,n_lat,n_z,n_times])*multiplier(k)
               tendency_snow=OPEN_AND_EXTRACT(infile_snow,nc_varnames(1),$
                                              offset=[box_tx(1),box_tx(0),0,0],$
                                              count=[n_lon,n_lat,n_z,n_times])*multiplier(k)
               tendency=tendency_rain+tendency_snow
            ENDELSE
;            FOR n=0,n_frames-1 DO BEGIN
;               FOR p=0,n_lon-1 DO $
;                  FOR r=0,n_lat-1 DO $
;                     FOR s=0,n_z-1 DO $
;                        thisrun_tendency(p,r,s,n)=MEAN(tendency(p,r,s,n*meaning_period:(n+1)*meaning_period-1))
;               composite_tendency(*,*,*,n)=thisrun_tendency(*,*,*,n)*1./FLOAT(n_cases)+composite_tendency(*,*,*,n)
;               FOR p=0,n_lon-1 DO $
;                  FOR r=0,n_z-1 DO $
;                     thisrun_latavg_tendency(p,r,n)=MEAN(thisrun_tendency(p,*,r,n))
;               composite_latavg_tendency(*,*,n)=thisrun_latavg_tendency(*,*,n)*1./FLOAT(n_cases)+composite_latavg_tendency(*,*,n)
;            ENDFOR            
;            allruns_latavg_tendency(m,*,*,*)=thisrun_latavg_tendency(*,*,*)
            thisrun_indices=VALUE_LOCATE(precip_bins(*),REFORM(allruns_precip(m,*)))
            thisrun_precip=REFORM(allruns_precip(m,*))
            FOR n=0,n_bins-1 DO BEGIN
               FOR p=0,n_z-1 DO BEGIN
                  thislev_tendency=REFORM(tendency(*,*,p,*))
                  IF TOTAL(where(thisrun_indices eq n)) gt 0 THEN $
                     binned_tendency(n,p)=MEAN(thislev_tendency[where(thisrun_indices eq n)],/NaN)*1./FLOAT(n_cases)+binned_tendency(n,p)
               ENDFOR
               IF TOTAL(where(thisrun_indices eq n)) gt 0 THEN BEGIN
                  n_points(n)=n_points(n)+N_ELEMENTS(where(thisrun_indices eq n))
                  mean_precip_in_bin(n)=MEAN(thisrun_precip[where(thisrun_indices eq n)],/NaN)*1./FLOAT(n_cases)+mean_precip_in_bin(n)
               ENDIF
            ENDFOR
         ENDFOR
;         stddev_latavg_tendency=fltarr(n_lon,n_z)
         
;         FOR n=0,n_frames-1 DO BEGIN
;            FOR p=0,n_lon-1 DO $
;               FOR r=0,n_z-1 DO $
;                  stddev_latavg_tendency(p,r)=STDDEV(allruns_latavg_tendency(*,p,r,n))            
;            psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/tendencies/hadgem3kpp_cascade_phase2_tendencies.'+varname+$
;                   '_'+tendencies(k)+'.'+meaning_units+STRTRIM(STRING(n*meaning_period*3),1)+'-'+STRTRIM(STRING((n+1)*meaning_period*3),1)+'.'+$
;                   set_name+'.ps'
;            PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2200,SPACE3=500,XOFFSET=1000,YOFFSET=200,TFONT=2,TCHARSIZE=100,$
;                   SPACE2=700
;            IF reverse_color_scale eq 0 THEN $
;               CS,SCALE=1,NCOLS=N_ELEMENTS(contour_levels(*,k))+1
;            IF reverse_color_scale eq 1 THEN $
;               CS,SCALE=1,NCOLS=N_ELEMENTS(contour_levels(*,k))+1,/REV
;            LEVS,MANUAL=contour_levels(*,k)
;            GSET,XMIN=box(1),XMAX=box(3),YMIN=1000.,YMAX=ymax(k)
;            CON,X=longitude,Y=rholvl_pres_mean,FIELD=REFORM(composite_latavg_tendency(*,*,n)),/NOLINES,$
;                TITLE='Composite lat-avg (5S-5N) '+varname+'_'+tendencies(k)+' for '+meaning_units+' '+STRTRIM(STRING(n*meaning_period*3),1)+'-'+$
;                STRTRIM(STRING((n+1)*meaning_period*3),1)+' of '+set_name+' integrations',$
;                CB_TITLE=cb_units
;            LEVS,MANUAL=stddev_levels(*,k)            
;            purple=FSC_COLOR('purple',30)
;            CON,X=longitude,Y=rholvl_pres_mean,FIELD=stddev_latavg_tendency(*,*),/NOFILL,POSITIVE_STYLE=2,$
;                COL=REPLICATE(30,N_ELEMENTS(stddev_levels(*,k))+2),THICK=indgen(N_ELEMENTS(stddev_levels(*,k))+2)*25+25,/NOLINELABELS
;            GPLOT,X=(MAX(longitude)-MIN(longitude))*2./3.+MIN(longitude),Y=(1000-ymax(k))*1.1+ymax(k),$
;                  TEXT='Standard deviation (purple) contours are spaced '+stddev_spacing(k)+' from '+stddev_levels(0,k),ALIGN=0.0
;            AXES,XSTEP=20,XMINOR=5,NDECS=1,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)',YSTEP=-100,YMINOR=-25,/NORIGHT
;            GSET,XMIN=box(1),XMAX=box(3),YMIN=0,YMAX=0.9
;            GPLOT,X=longitude,Y=REFORM(composite_latavg_precip(*,n))        
;            AXES,YSTEP=0.06,YTITLE='Precipitation rate (mm hr!U-1!N)',/ONLYRIGHT,YMINOR=0.02,NDECS=2            
;            PSCLOSE,/NOVIEW
;         ENDFOR
         psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/tendencies/hadgem3kpp_cascade_phase2_tendencies_bylsrain.'+varname+$
                '_'+tendencies(k)+'.twod_pdf.'+meaning_units+STRTRIM(STRING(n*meaning_period*3),1)+'-'+STRTRIM(STRING((n+1)*meaning_period*3),1)+'.'+$
                set_name+'.ps'
         PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=2200,SPACE2=700,XOFFSET=400,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT,CB_WIDTH=112
         GSET,XMIN=0,XMAX=n_bins,YMIN=1000.,YMAX=ymax(k)
         IF reverse_color_scale eq 0 THEN $
            CS,SCALE=scale,NCOLS=N_ELEMENTS(pdf_levels(*,k))+1
         IF reverse_color_scale eq 1 THEN $
            CS,SCALE=scale,NCOLS=N_ELEMENTS(pdf_levels(*,k))+1,/REV
         LEVS,MANUAL=REFORM(pdf_levels(*,k))
         CON,X=indgen(n_bins)+0.5,Y=rholvl_pres_mean,FIELD=REFORM(binned_tendency),/BLOCK,$
             TITLE=varname+' '+tendencies(k)+' increment binned by lsrain - 5S-5N - '+meaning_units+' '+STRTRIM(STRING(0*meaning_period*3),1)+'-'+$
             STRTRIM(STRING(n_frames*meaning_period*3),1)+' of '+set_name+' integrations',/NOLINES,CB_title=cb_units(k)
         AXES,XVALS=indgen(n_bins+1),YSTEP=-100,YMINOR=-25,XTITLE='Large-scale precipitation bins from control simulation (mm hr!U-1!N), zero values excluded',$
              YTITLE='Pressure (hPa)',/NORIGHT,$
              XLABELS=[STRMID(STRTRIM(STRING(precip_bins),1),0,5),'> '+STRMID(STRTRIM(STRING(precip_bins(n_bins-1)),1),0,5)],ORIENTATION=30
         GSET,XMIN=0,XMAX=n_bins,YMIN=0.,YMAX=0.20
         GPLOT,X=indgen(n_bins)+0.5,Y=n_points/TOTAL(n_points)
         GPLOT,X=indgen(n_bins)+0.5,Y=mean_precip_in_bin,STYLE=5
         AXES,XVALS=indgen(n_bins+1),YSTEP=0.02,YMINOR=0.01,$
              YTITLE='Fraction of points in bin and large-scale rain rate (mm hr!U-1!N)',/ONLYRIGHT,NDECS=2         
         PSCLOSE
      ENDFOR      
   ENDFOR
ENDFOR


STOP
END

