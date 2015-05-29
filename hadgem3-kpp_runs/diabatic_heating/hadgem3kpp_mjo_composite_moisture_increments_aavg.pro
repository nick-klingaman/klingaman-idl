PRO hadgem3kpp_mjo_composite_moisture_increments_aavg
  
; Make MJO phase composites of moisture increments, area-averaged within a box.

run_name='1.0xentrain'
basedir='/home/ss901165/um_output4/hadgem3a_amip2_'+run_name+'_rerun_vn74'
rmm_index_file=basedir+'/hadgem3a_amip2_'+run_name+'_rerun_vn74.mar-feb_6hrmeans.1-10.rmm_indices.nc'
heating_files=[basedir+'/hadgem3a_amip2_'+run_name+'_rerun_vn74.mar-feb_6hrmeans.years1-10.Qinc_lwrad.nc',$
               basedir+'/hadgem3a_amip2_'+run_name+'_rerun_vn74.mar-feb_6hrmeans.years1-10.Qinc_swrad.nc',$
               basedir+'/hadgem3a_amip2_'+run_name+'_rerun_vn74.mar-feb_6hrmeans.years1-10.Qinc_bdylr.nc',$
               basedir+'/hadgem3a_amip2_'+run_name+'_rerun_vn74.mar-feb_6hrmeans.years1-10.Qinc_conv.nc',$
               basedir+'/hadgem3a_amip2_'+run_name+'_rerun_vn74.mar-feb_6hrmeans.years1-10.Qinc_lsrain.nc']
heating_names=['unspecified_8','unspecified_2','unspecified_5','unspecified_9','unspecified_1']
;heating_files=[basedir+'/hadgem3a_amip2_'+run_name+'_rerun_vn74.mar-feb_6hrmeans.years1-10.Tinc_bdylr.nc',$
;               basedir+'/hadgem3a_amip2_'+run_name+'_rerun_vn74.mar-feb_6hrmeans.years1-10.Tinc_conv.nc',$
;               basedir+'/hadgem3a_amip2_'+run_name+'_rerun_vn74.mar-feb_6hrmeans.years1-10.Tinc_lsrain.nc']
;heating_names=['unspecified_4','unspecified_8','unspecified']
heating_title='total'
n_heatings=N_ELEMENTS(heating_files)

rholvl_file='/home/ss901165/um_output4/xfzbo/h9/xfzboa.pjh9cr0.nc'

n_years=10
n_times_per_year=1440

box_aavg=[-5,70,5,80]
box_name='IndOcn'

longitude=OPEN_AND_EXTRACT(heating_files(0),'longitude')
latitude=OPEN_AND_EXTRACT(heating_files(0),'latitude')
DEFINE_BOUNDARIES,box_aavg,latitude,longitude,box_aavg_tx,/LIMIT
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)

height_rholvl=OPEN_AND_EXTRACT(heating_files(0),'hybrid_ht')
n_z=N_ELEMENTS(height_rholvl)
rholvl_longitude=OPEN_AND_EXTRACT(rholvl_file,'longitude')
rholvl_latitude=OPEN_AND_EXTRACT(rholvl_file,'latitude')
DEFINE_BOUNDARIES,box_aavg,latitude,longitude,box_rholvl_tx,/LIMIT
rholvl_nlon=N_ELEMENTS(rholvl_longitude)
rholvl_nlat=N_ELEMENTS(rholvl_latitude)

p_rholvl=OPEN_AND_EXTRACT(rholvl_file,'p',offset=[box_rholvl_tx(1),box_rholvl_tx(0),0,0],$
                          count=[rholvl_nlon,rholvl_nlat,n_z,1])
p_rholvl_aavg=fltarr(n_z)
FOR i=0,n_z-1 DO $
   p_rholvl_aavg(i)=MEAN(p_rholvl(*,*,i))/100.

mjo_phase=REFORM(OPEN_AND_EXTRACT(rmm_index_file,'phase',$
                                  offset=[0,0],count=[n_years,n_times_per_year]))
mjo_amp=REFORM(OPEN_AND_EXTRACT(rmm_index_file,'amplitude',$
                                offset=[0,0],count=[n_years,n_times_per_year]))
               
phase_composite=fltarr(10,n_z)
phase_ntimes=fltarr(9)
FOR i=0,n_years-1 DO BEGIN
   thisyear_phase=REFORM(mjo_phase(i,*))
   thisyear_amp=REFORM(mjo_amp(i,*))
   print,'Now computing composites for year '+STRTRIM(STRING(i+1),1)+' ...'

   FOR m=0,n_heatings-1 DO BEGIN
      print,heating_files(m)+' ...'
      thisyear_heating=REFORM(OPEN_AND_EXTRACT(heating_files(m),heating_names(m),$
                                               offset=[box_aavg_tx(1),box_aavg_tx(0),0,0,i],$
                                               count=[n_lon,n_lat,n_z,n_times_per_year,1]))*72000.
      print,' ... done.'
      thisyear_heating_aavg=fltarr(n_z,n_times_per_year)
      FOR j=0,n_z-1 DO $
         FOR k=0,n_times_per_year-1 DO $
            thisyear_heating_aavg(j,k)=MEAN(thisyear_heating(*,*,j,k))
      
      FOR j=1,8 DO BEGIN
         thisphase_days=where(thisyear_phase eq j and thisyear_amp gt 1)
         IF N_ELEMENTS(thisphase_days) gt 1 THEN BEGIN
            IF m eq 0 THEN $
               phase_ntimes(j)=phase_ntimes(j)+N_ELEMENTS(thisphase_days)
            FOR k=0,n_z-1 DO BEGIN
               thislev_heating_aavg=REFORM(thisyear_heating_aavg(k,*))
               phase_composite(j,k)=TOTAL(thislev_heating_aavg[thisphase_days])+phase_composite(j,k)              
            ENDFOR
         ENDIF
      ENDFOR
      
      phasezero_days=where(thisyear_amp le 1)
      IF m eq 0 THEN $
         phase_ntimes(0)=phase_ntimes(0)+N_ELEMENTS(phasezero_days)
      FOR k=0,n_z-1 DO BEGIN
         thislev_heating_aavg=REFORM(thisyear_heating_aavg(k,*))
         phase_composite(0,k)=TOTAL(thislev_heating_aavg[phasezero_days])+phase_composite(0,k)
      ENDFOR
   ENDFOR
ENDFOR
phase_composite(0,*)=phase_composite(0,*)/FLOAT(phase_ntimes(0))
FOR j=1,8 DO $
   phase_composite(j,*)=phase_composite(j,*)/FLOAT(phase_ntimes(j))
mean_mjo_phases=fltarr(n_z)
FOR j=0,n_z-1 DO $
   mean_mjo_phases(j)=MEAN(phase_composite(1:8,j))
FOR j=1,8 DO $
   FOR k=0,n_z-1 DO $
      phase_composite(j,k)=phase_composite(j,k)-mean_mjo_phases(k)

mylevs=['-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5']
psfile='/home/ss901165/idl/hadgem3-kpp_runs/diabatic_heating/hadgem3kpp_mjo_composite_moisture_increments_aavg.'+box_name+'.all_phases.'+run_name+'.'+heating_title+'.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=700,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
GSET,XMIN=0,XMAX=16,YMIN=1000.,YMAX=MIN(p_rholvl_aavg)
LEVS,MANUAL=mylevs
phase_composite(9,*)=phase_composite(1,*)
FOR i=0,1 DO BEGIN
   IF i eq 0 THEN BEGIN
      CON,X=indgen(9),Y=p_rholvl_aavg,FIELD=phase_composite(1:9,*),/NOLINES,$
          TITLE='Phase composite of moisture increments ('+heating_title+') for '+run_name+' ('+$
          STRTRIM(STRING(n_years),1)+' years) in the '+box_name+' box - ('+$
          STRTRIM(STRING(box_aavg(0)),1)+'N to '+STRTRIM(STRING(box_aavg(2)),1)+'N, '+$
          STRTRIM(STRING(box_aavg(1)),1)+'E to '+STRTRIM(STRING(box_aavg(3)),1)+'E)',$
          CB_TITLE='Moisture increment (g kg!U-1!N day!U-1!N)'
   ENDIF ELSE $
      CON,X=indgen(9)+i*8,Y=p_rholvl_aavg,FIELD=phase_composite(1:9,*),/NOLINES,/NOCOLBAR
ENDFOR
AXES,XVALS=indgen(17),XLABELS=['1','2','3','4','5','6','7','8',$
                               '1','2','3','4','5','6','7','8','1'],$
     XTITLE='Wheeler and Hendon RMM phase',YTITLE='Pressure (hPa)',YSTEP=-50
PSCLOSE,/NOVIEW

STOP
END
