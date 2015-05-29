PRO hadgem3kpp_cascade_phase2_wbin_withheight_byregion
  
; Plot frequency of occurrence of w against height from initialized 
; HadGEM3 simulations with phase 2 MJOs.

n_sets=2
um3='/home/ss901165/um_output3'
date_ranges=['04nov09-06nov09','26aug08-28aug08','07dec07-09dec07','07sep06-09sep06']
n_z=85
n_times=24
time_offset=0

box=[-10,60,10,90]
box_name='eqIndOcn'
w_bins=indgen(21)*0.02-0.2
n_bins=N_ELEMENTS(w_bins)

FOR i=0,n_sets-1 DO BEGIN
   CASE i OF 
      0 : BEGIN
         runids=['xfrla','xfrle','xfrli','xfrlm']
         set_name='control'
      END
      1 : BEGIN
         runids=['xfrlb','xfrlf','xfrlj','xfrln']
         set_name='1.5x_entrain'
      END
   ENDCASE

   n_runs=N_ELEMENTS(runids)
   FOR j=0,n_runs-1 DO BEGIN
      w_file=um3+'/'+runids(j)+'.new_stash/'+runids(j)+'.'+date_ranges(j)+'_3hrmeans.w_after_ts.nc'
      w_latitude=OPEN_AND_EXTRACT(w_file,'latitude')
      w_longitude=OPEN_AND_EXTRACT(w_file,'longitude')
      DEFINE_BOUNDARIES,box,w_latitude,w_longitude,w_box_tx,/LIMIT
      w_nlon=N_ELEMENTS(w_longitude)
      w_nlat=N_ELEMENTS(w_latitude)

      w=REFORM(OPEN_AND_EXTRACT(w_file,'dz_dt',$
                                offset=[w_box_tx(1),w_box_tx(0),0,time_offset],$
                                count=[w_nlon,w_nlat,n_z,n_times]))

      IF j eq 0 THEN $
         w_allruns=fltarr(n_runs,n_z,w_nlon*w_nlat*n_times)
      FOR k=0,n_z-1 DO $
         w_allruns(j,k,*)=REFORM(w(*,*,k,*),[w_nlon*w_nlat*n_times])   
   ENDFOR
   w_npts=fltarr(n_bins,n_z)
   w_mean=fltarr(n_bins,n_z)
   FOR j=0,n_z-1 DO BEGIN
      thislev_w=REFORM(w_allruns(*,j,*))
      thislev_indices=VALUE_LOCATE(w_bins,thislev_w)
      FOR k=0,n_bins-1 DO BEGIN
         IF TOTAL(where(thislev_indices eq k)) gt 0 THEN BEGIN
            w_npts(k,j)=N_ELEMENTS(where(thislev_indices eq k))
            w_mean(k,j)=MEAN(thislev_w[where(thislev_indices eq k)])
         ENDIF
      ENDFOR
   ENDFOR

   thlvl_pres_file=um3+'/'+runids(0)+'.new_stash/'+runids(0)+'.'+date_ranges(0)+'_3hrmeans.p_thlvl_after_ts.nc'  
   thlvl_longitude=OPEN_AND_EXTRACT(thlvl_pres_file,'longitude')
   thlvl_latitude=OPEN_AND_EXTRACT(thlvl_pres_file,'latitude')
   DEFINE_BOUNDARIES,box,thlvl_latitude,thlvl_longitude,thlvl_box_tx,/LIMIT
   thlvl_nlon=N_ELEMENTS(thlvl_longitude)
   thlvl_nlat=N_ELEMENTS(thlvl_latitude)
   thlvl_pres=REFORM(OPEN_AND_EXTRACT(thlvl_pres_file,'p_1',$
                                       offset=[thlvl_box_tx(1),thlvl_box_tx(0),0,0],$
                                       count=[thlvl_nlon,thlvl_nlat,n_z,n_times]))
   thlvl_pres_mean=fltarr(n_z)
   FOR m=0,n_z-1 DO $
      thlvl_pres_mean(m)=MEAN(thlvl_pres(*,*,m,*))/100.
   
   freq_levels=['0.001','0.002','0.004','0.007','0.01','0.02','0.04','0.07','0.1','0.2','0.3','0.4','0.6','0.8']
   w_levels=['-0.30','-0.26','-0.22','-0.18','-0.14','-0.10','-0.06','-0.02','0.02','0.06','0.10','0.14','0.18','0.22','0.26','0.30']

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/w_bin/hadgem3kpp_cascade_phase2_wbin_withheight_byregion.'+set_name+'_3hrdata_wfreq.'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2200,SPACE3=500,XOFFSET=1000,YOFFSET=200,TFONT=2,TCHARSIZE=80,$
          SPACE2=700,/PORTRAIT
   CS,SCALE=26,NCOLS=N_ELEMENTS(freq_levels)+1,white=[2]
   LEVS,MANUAL=freq_levels
   GSET,XMIN=0,XMAX=n_bins,YMIN=MAX(thlvl_pres_mean),YMAX=100,TITLE='Frequency of vert vels from '+set_name+$
        ' simulation using 3hr data for '+box_name+' (lat:'+STRTRIM(STRING(box(0)),1)+'-'+STRTRIM(STRING(box(2)),1)+$
        ', lon:'+STRTRIM(STRING(box(1)),1)+'-'+STRTRIM(STRING(box(3)),1)+') - 5 days'
   FOR j=0,n_z-1 DO $
      w_npts(*,j)=w_npts(*,j)/TOTAL(w_npts(*,j))
   CON,X=indgen(n_bins)+0.5,Y=thlvl_pres_mean,FIELD=REFORM(w_npts),/BLOCK,/NOLINES,CB_TITLE='Frequency of occurrence'
   AXES,XVALS=indgen(n_bins+1),XLABELS=[STRMID(STRTRIM(STRING(w_bins),1),0,5),'>'+STRMID(STRTRIM(STRING(w_bins(n_bins-1)),1),0,5)],$
        YVALS=indgen(19)*50+100,ORIENTATION=30,XTITLE='Bins of vertical velocity (m s!U-1!N)',YTITLE='Pressure (hPa)'
   PSCLOSE

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/w_bin/hadgem3kpp_cascade_phase2_wbin_withheight_byregion.'+set_name+'_3hrdata_wmean.'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2200,SPACE3=500,XOFFSET=1000,YOFFSET=200,TFONT=2,TCHARSIZE=80,$
          SPACE2=700,/PORTRAIT,CB_WIDTH=112
   CS,SCALE=26,NCOLS=N_ELEMENTS(w_levels)+1,white=[10]
   LEVS,MANUAL=w_levels
   GSET,XMIN=0,XMAX=n_bins,YMIN=MAX(thlvl_pres_mean),YMAX=100,TITLE='Mean vert vels in each bin from '+set_name+$
        ' simulation using 3hr data for '+box_name+' (lat:'+STRTRIM(STRING(box(0)),1)+'-'+STRTRIM(STRING(box(2)),1)+$
        ', lon:'+STRTRIM(STRING(box(1)),1)+'-'+STRTRIM(STRING(box(3)),1)+') - 5 days'
   FOR j=0,n_z-1 DO $
      w_npts(*,j)=w_npts(*,j)/TOTAL(w_npts(*,j))
   CON,X=indgen(n_bins)+0.5,Y=thlvl_pres_mean,FIELD=REFORM(w_mean),/BLOCK,/NOLINES,CB_TITLE='Mean vertical velocity (m s!U-1!N) in bin'
   AXES,XVALS=indgen(n_bins+1),XLABELS=[STRMID(STRTRIM(STRING(w_bins),1),0,5),'>'+STRMID(STRTRIM(STRING(w_bins(n_bins-1)),1),0,5)],$
        YVALS=indgen(19)*50+100,ORIENTATION=30,XTITLE='Bins of vertical velocity (m s!U-1!N)',YTITLE='Pressure (hPa)'
   PSCLOSE

ENDFOR


STOP
END
