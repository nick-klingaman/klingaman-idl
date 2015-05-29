PRO hadgem3kpp_cascade_phase2_criticalrh

n_sets=2
n_variables=1
um3='/home/ss901165/um_output3'
date_ranges=['04nov09-13nov09']
precip_date_ranges=['04nov09-06nov09']

box=[-5,40,5,200]
n_z=85
n_bins=20
FOR i=0,n_sets-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         runids=['xfyfc']
         precip_runids=['xfrla']
         set_name='control'
      END
      1 : BEGIN
         runids=['xfyfa']
         precip_runids=['xfrlb']
         set_name='15xentrain'
      END
   ENDCASE
   n_cases=N_ELEMENTS(runids)   

   FOR j=0,n_variables-1 DO BEGIN
      CASE j OF
         0 : BEGIN
            grid_file=um3+'/'+runids(0)+'.new_stash/'+runids(0)+'a.'+date_ranges(0)+'.critical_rh_rholvl.nc'
            rholvl_pres_file=um3+'/xfrla.new_stash/xfrla.04nov09-06nov09_3hrmeans.p_rholvl_after_ts.nc'
            longitude_var='longitude_1'
            latitude_var='latitude'
            z_var='hybrid_ht_1'
            varname='critical_rh_rholvl'
            tendencies=['']
            nc_varnames=['C3H7OOH']
            contour_levels=['0.57','0.60','0.63','0.66','0.69','0.72','0.75','0.78','0.81','0.84','0.87','0.90','0.93','0.96','0.99']
            stddev_levels=['0.03','0.06','0.09','0.12','0.15','0.18','0.21']
            pdf_levels=['0.57','0.60','0.63','0.66','0.69','0.72','0.75','0.78','0.81','0.84','0.87','0.90','0.93','0.96','0.99']
            meaning_period=4
            meaning_units='hours'
            n_times=24
            n_frames=n_times/meaning_period
            reverse_color_scale=1
            cb_units='Critical relative humidity from large-scale cloud scheme (decimal)'
            multiplier=1.
            ymax=[100.]
         END
      ENDCASE
      n_tendencies=N_ELEMENTS(tendencies)
     
      grid_file=um3+'/'+runids(0)+'.new_stash/'+runids(0)+'a.'+date_ranges(0)+'.'+varname+'.nc'
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
         precip_infile=um3+'/'+precip_runids(m)+'.new_stash/'+precip_runids(m)+'.'+precip_date_ranges(m)+'_3hrmeans.rain.nc'
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
;         nonzero_precip=precip[where(precip gt 0.001)]
;         nonzero_precip_sorted=SORT(nonzero_precip)
;         FOR n=0,n_bins-1 DO $
;            allruns_precip_bins(m,n)=nonzero_precip(nonzero_precip_sorted(FLOOR(N_ELEMENTS(nonzero_precip)*n/FLOAT(n_bins))))         
;         allruns_indices(m,*)=VALUE_LOCATE(REFORM(allruns_precip_bins(m,*)),precip)         
         FOR n=0,n_frames-1 DO BEGIN
            FOR p=0,precip_nlon-1 DO BEGIN
               thisrun_latavg_precip(p,n)=MEAN(precip(p,*,n*meaning_period:(n+1)*meaning_period-1))
               composite_latavg_precip(p,n)=composite_latavg_precip(p,n)+thisrun_latavg_precip(p,n)*1./FLOAT(n_cases)
            ENDFOR
         ENDFOR
         allruns_latavg_precip(m,*,*)=thisrun_latavg_precip(*,*)
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
         thisrun_tendency=fltarr(n_lon,n_lat,n_z,n_frames)
         thisrun_latavg_tendency=fltarr(n_lon,n_z,n_frames)
         allruns_latavg_tendency=fltarr(n_cases,n_lon,n_z,n_frames)
         composite_tendency=fltarr(n_lon,n_lat,n_z,n_frames)
         composite_latavg_tendency=fltarr(n_lon,n_z,n_frames)
         binned_tendency=fltarr(n_bins,n_z)
         FOR m=0,n_cases-1 DO BEGIN
            infile=um3+'/'+runids(m)+'.new_stash/'+runids(m)+'a.'+date_ranges(m)+'.'+varname+'.nc'
            print,infile
            grid_file=um3+'/'+runids(m)+'.new_stash/'+runids(m)+'a.'+date_ranges(m)+'.'+varname+'.nc'
            longitude=OPEN_AND_EXTRACT(grid_file,longitude_var)
            latitude=OPEN_AND_EXTRACT(grid_file,latitude_var)
            z=OPEN_AND_EXTRACT(grid_file,z_var)
            DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
            tendency=OPEN_AND_EXTRACT(infile,nc_varnames(k),$
                                      offset=[box_tx(1),box_tx(0),0,0],$
                                      count=[n_lon,n_lat,n_z,n_times])*multiplier(k)
            FOR n=0,n_frames-1 DO BEGIN
               FOR p=0,n_lon-1 DO $
                  FOR r=0,n_lat-1 DO $
                     FOR s=0,n_z-1 DO $
                        thisrun_tendency(p,r,s,n)=MEAN(tendency(p,r,s,n*meaning_period:(n+1)*meaning_period-1))
               composite_tendency(*,*,*,n)=thisrun_tendency(*,*,*,n)*1./FLOAT(n_cases)+composite_tendency(*,*,*,n)
               FOR p=0,n_lon-1 DO $
                  FOR r=0,n_z-1 DO $
                     thisrun_latavg_tendency(p,r,n)=MEAN(thisrun_tendency(p,*,r,n))
               composite_latavg_tendency(*,*,n)=thisrun_latavg_tendency(*,*,n)*1./FLOAT(n_cases)+composite_latavg_tendency(*,*,n)
            ENDFOR            
            allruns_latavg_tendency(m,*,*,*)=thisrun_latavg_tendency(*,*,*)
            thisrun_indices=VALUE_LOCATE(precip_bins(*),REFORM(allruns_precip(m,*)))
            FOR n=0,n_bins-1 DO BEGIN
               FOR p=0,n_z-1 DO BEGIN
                  thislev_tendency=REFORM(tendency(*,*,p,*))
                  IF TOTAL(where(thisrun_indices eq n)) gt 0 THEN BEGIN
                     binned_tendency(n,p)=MEAN(thislev_tendency[where(thisrun_indices eq n)],/NaN)*1./FLOAT(n_cases)+binned_tendency(n,p)
                  ENDIF
               ENDFOR
            ENDFOR
         ENDFOR
         stddev_latavg_tendency=fltarr(n_lon,n_z)
         
         FOR n=0,n_frames-1 DO BEGIN
            ;FOR p=0,n_lon-1 DO $
            ;   FOR r=0,n_z-1 DO $
            ;      stddev_latavg_tendency(p,r)=STDDEV(allruns_latavg_tendency(*,p,r,n))            
            psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/critical_rh/hadgem3kpp_cascade_phase2_criticalrh.'+varname+$
                   '.'+meaning_units+STRTRIM(STRING(n*meaning_period*3),1)+'-'+STRTRIM(STRING((n+1)*meaning_period*3),1)+'.'+$
                   set_name+'.ps'
            PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2200,SPACE3=500,XOFFSET=1000,YOFFSET=200,TFONT=2,TCHARSIZE=100,$
                   SPACE2=700
            IF reverse_color_scale eq 0 THEN $
               CS,SCALE=1,NCOLS=N_ELEMENTS(contour_levels(*,k))+1
            IF reverse_color_scale eq 1 THEN $
               CS,SCALE=1,NCOLS=N_ELEMENTS(contour_levels(*,k))+1,/REV
            LEVS,MANUAL=contour_levels(*,k)
            GSET,XMIN=box(1),XMAX=box(3),YMIN=1000.,YMAX=ymax(k)
            CON,X=longitude,Y=rholvl_pres_mean,FIELD=REFORM(composite_latavg_tendency(*,*,n)),/NOLINES,$
                TITLE='Composite lat-avg (5S-5N) '+varname+' for '+meaning_units+' '+STRTRIM(STRING(n*meaning_period*3),1)+'-'+$
                STRTRIM(STRING((n+1)*meaning_period*3),1)+' of '+set_name+' integrations',$
                CB_TITLE=cb_units(k)
            ;LEVS,MANUAL=stddev_levels(*,k)            
            ;purple=FSC_COLOR('purple',30)
            ;CON,X=longitude,Y=rholvl_pres_mean,FIELD=stddev_latavg_tendency(*,*),/NOFILL,POSITIVE_STYLE=2,$
            ;    COL=REPLICATE(30,N_ELEMENTS(stddev_levels(*,k))+2),THICK=indgen(N_ELEMENTS(stddev_levels(*,k))+2)*25+25,/NOLINELABELS
            ;GPLOT,X=(MAX(longitude)-MIN(longitude))*3./5.+MIN(longitude),Y=(1000-ymax(k))*1.1+ymax(k),$
            ;      TEXT='Standard deviation (purple) contours are spaced '+stddev_spacing(k)+' from '+stddev_levels(0,k),ALIGN=0.0
            AXES,XSTEP=20,XMINOR=5,NDECS=1,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)',YSTEP=-100,YMINOR=-25,/NORIGHT
            GSET,XMIN=box(1),XMAX=box(3),YMIN=0,YMAX=2.1
            GPLOT,X=longitude,Y=REFORM(composite_latavg_precip(*,n))        
            AXES,YSTEP=0.15,YTITLE='Precipitation rate (mm hr!U-1!N)',/ONLYRIGHT,YMINOR=0.075,NDECS=3     
            PSCLOSE,/NOVIEW           
         ENDFOR
         psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/critical_rh/hadgem3kpp_cascade_phase2_criticalrh.'+varname+$
                '.twod_pdf.'+meaning_units+STRTRIM(STRING(n*meaning_period*3),1)+'-'+STRTRIM(STRING((n+1)*meaning_period*3),1)+'.'+$
                set_name+'.ps'
         PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT
         GSET,XMIN=0,XMAX=n_bins,YMIN=1000.,YMAX=ymax(k)
         IF reverse_color_scale eq 0 THEN $
            CS,SCALE=1,NCOLS=N_ELEMENTS(pdf_levels(*,k))+1
         IF reverse_color_scale eq 1 THEN $
            CS,SCALE=1,NCOLS=N_ELEMENTS(pdf_levels(*,k))+1,/REV
         LEVS,MANUAL=REFORM(pdf_levels(*,k))
         CON,X=indgen(n_bins)+0.5,Y=rholvl_pres_mean,FIELD=REFORM(binned_tendency),/BLOCK,$
             TITLE=varname+' binned by precip - 5S-5N - '+meaning_units+' '+STRTRIM(STRING(0*meaning_period*3),1)+'-'+$
             STRTRIM(STRING(n_frames*meaning_period*3),1)+' of '+set_name+' integrations',/NOLINES,CB_TITLE=cb_units(k)
         AXES,XVALS=indgen(n_bins+1),YSTEP=-100,YMINOR=-25,XTITLE='Decile of precipitation (mm hr!U-1!N) from control simulation, zero values excluded',YTITLE='Pressure (hPa)',$
              XLABELS=[STRMID(STRTRIM(STRING(precip_bins),1),0,5),'> '+STRMID(STRTRIM(STRING(precip_bins(n_bins-1)),1),0,5)],$
              ORIENTATION=30
         PSCLOSE,/NOVIEW
      ENDFOR      
   ENDFOR
ENDFOR


STOP
END

