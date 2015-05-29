PRO hadgem3kpp_mjo_composite_moisture_increments_latavg_fromnc

; Plot MJO phase composites of latitude-averaged diabatic heating 
; from a netCDF file.

; Experiment name
expt_name='hadgem3a_amip2_1.0xentrain_rerun_vn74'
run_name='1.0x_entrain'

; Basename for netCDF files
netcdf_basename='/home/ss901165/um_output4/'+expt_name+'/'+expt_name+'.years1-13.composite_mjo_phase'

; Heating components to plot
;components=['Tinc_lwrad','Tinc_swrad']
components=['Qinc_lwrad','Qinc_swrad','Qinc_bdylr','Qinc_conv','Qinc_lsrain']
;components=['Tinc_bdylr','Tinc_conv','Tinc_lsrain']
n_components=N_ELEMENTS(components)
;heating_title='non-radiative'
;heating_title='radiative'
heating_title='total'

box_aavg=[-5,40,5,240]

;mylevs=['-2.55','-2.25','-1.95','-1.65','-1.35','-1.05','-0.75','-0.45','-0.15','0.15','0.45',$
;        '0.75','1.05','1.35','1.65','1.95','2.25','2.55']
;mylevs=['-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5']
mylevs=['-0.75','-0.65','-0.55','-0.45','-0.35','-0.25','-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75']
mylevs_phasezero=['-3.0','-2.6','-2.2','-1.8','-1.4','-1.0','-0.6','-0.2','0.2','0.6','1.0','1.4','1.8','2.2','2.6','3.0']

;mylevs=['-0.75','-0.65','-0.55','-0.45','-0.35','-0.25','-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75']
;mylevs_phasezero=['-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5']

; Transfer hybrid_ht coordinates to pressure coordinates
rholvl_file='/home/ss901165/um_output4/xfzbo/h9/xfzboa.pjh9cr0.nc'
rholvl_longitude=OPEN_AND_EXTRACT(rholvl_file,'longitude')
rholvl_latitude=OPEN_AND_EXTRACT(rholvl_file,'latitude')
DEFINE_BOUNDARIES,box_aavg,rholvl_latitude,rholvl_longitude,box_rholvl_tx,/LIMIT
rholvl_nlon=N_ELEMENTS(rholvl_longitude)
rholvl_nlat=N_ELEMENTS(rholvl_latitude)

n_phases=9

FOR i=0,n_phases-1 DO BEGIN
   input_file=netcdf_basename+STRTRIM(STRING(i),1)+'.nc'
   IF i eq 0 THEN BEGIN
      longitude=OPEN_AND_EXTRACT(input_file,'longitude')
      latitude=OPEN_AND_EXTRACT(input_file,'latitude')
      z=OPEN_AND_EXTRACT(input_file,'z')
      DEFINE_BOUNDARIES,box_aavg,latitude,longitude,box_aavg_tx,/LIMIT
      n_lon=N_ELEMENTS(longitude)
      n_lat=N_ELEMENTS(latitude)
      n_z=N_ELEMENTS(z)

      p_rholvl=OPEN_AND_EXTRACT(rholvl_file,'p',offset=[box_rholvl_tx(1),box_rholvl_tx(0),0,0],$
                                count=[rholvl_nlon,rholvl_nlat,n_z,1])
      p_rholvl_aavg=fltarr(n_z)
      FOR j=0,n_z-1 DO $
         p_rholvl_aavg(j)=MEAN(p_rholvl(*,*,j))/100.
  
      phase_composite=fltarr(n_phases,n_lon,n_z)
      phase_composite_scaleamp=fltarr(n_phases,n_lon,n_z)
   ENDIF

   FOR j=0,n_components-1 DO BEGIN
      this_phase_composite=REFORM(OPEN_AND_EXTRACT(input_file,components(j)+'_composite',$
                                                   offset=[box_aavg_tx(1),box_aavg_tx(0),0],$
                                                   count=[n_lon,n_lat,n_z]))*72000.

      this_phase_composite_scaleamp=REFORM(OPEN_AND_EXTRACT(input_file,components(j)+'_scaleamp',$
                                                            offset=[box_aavg_tx(1),box_aavg_tx(0),0],$
                                                            count=[n_lon,n_lat,n_z]))*72000.
      FOR k=0,n_lon-1 DO BEGIN
         FOR m=0,n_z-1 DO BEGIN
            phase_composite(i,k,m)=phase_composite(i,k,m)+MEAN(this_phase_composite(k,*,m))
            phase_composite_scaleamp(i,k,m)=phase_composite_scaleamp(i,k,m)+MEAN(this_phase_composite_scaleamp(k,*,m))
         ENDFOR
      ENDFOR
   ENDFOR
ENDFOR

phase_composite_anom_fromall=fltarr(n_phases+1,n_lon,n_z)
phase_composite_anom_fromzero=fltarr(n_phases+1,n_lon,n_z)
phase_composite_scaleamp_anom_fromall=fltarr(n_phases+1,n_lon,n_z)
phase_composite_scaleamp_anom_fromzero=fltarr(n_phases+1,n_lon,n_z)
FOR i=1,n_phases-1 DO BEGIN
   FOR j=0,n_lon-1 DO BEGIN
      FOR k=0,n_z-1 DO BEGIN
         phase_composite_anom_fromall(i,j,k)=phase_composite(i,j,k)-MEAN(phase_composite(1:8,j,k))
         phase_composite_anom_fromzero(i,j,k)=phase_composite(i,j,k)-phase_composite(0,j,k)
         phase_composite_scaleamp_anom_fromall(i,j,k)=phase_composite_scaleamp(i,j,k)-MEAN(phase_composite_scaleamp(1:8,j,k))
         phase_composite_scaleamp_anom_fromzero(i,j,k)=phase_composite_scaleamp(i,j,k)-phase_composite_scaleamp(0,j,k)
      ENDFOR
   ENDFOR

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/moisture_increments/hadgem3kpp_mjo_composite_moisture_increments_latavg_fromnc.phase'+STRTRIM(STRING(i),1)+'.'+run_name+'.'+$
          heating_title+'_anom_phases1-8.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=700,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
   GSET,XMIN=box_aavg(1),XMAX=box_aavg(3),YMIN=1000.,YMAX=MIN(p_rholvl_aavg)
   LEVS,MANUAL=mylevs
   CON,X=longitude,Y=p_rholvl_aavg,FIELD=REFORM(phase_composite_anom_fromall(i,*,*)),/NOLINES,CB_TITLE=heating_title+' moisture increment (g kg!U-1!N day!U-1!N)',$
       TITLE=heating_title+' moisture increment for MJO phase '+STRTRIM(STRING(i),1)+' latavg 5S-5N for '+run_name+' - anom from mean phases 1-8'
   AXES,XSTEP=10,YSTEP=-100,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)',YMINOR=-50,XMINOR=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/moisture_increments/hadgem3kpp_mjo_composite_moisture_increments_latavg_fromnc.phase'+STRTRIM(STRING(i),1)+'.'+run_name+'.'+$
          heating_title+'_scaleamp_anom_phases1-8.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=700,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
   GSET,XMIN=box_aavg(1),XMAX=box_aavg(3),YMIN=1000.,YMAX=MIN(p_rholvl_aavg)
   LEVS,MANUAL=mylevs
   CON,X=longitude,Y=p_rholvl_aavg,FIELD=REFORM(phase_composite_scaleamp_anom_fromall(i,*,*)),/NOLINES,CB_TITLE=heating_title+' moisture increment (g kg!U-1!N day!U-1!N)',$
       TITLE=heating_title+' moisture increment for MJO phase '+STRTRIM(STRING(i),1)+' latavg 5S-5N for '+run_name+' - anom from mean phases 1-8 - scale by MJO amp'
   AXES,XSTEP=10,YSTEP=-100,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)',YMINOR=-50,XMINOR=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/moisture_increments/hadgem3kpp_mjo_composite_moisture_increments_latavg_fromnc.phase'+STRTRIM(STRING(i),1)+'.'+run_name+'.'+$
          heating_title+'_anom_phase0.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=700,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
   GSET,XMIN=box_aavg(1),XMAX=box_aavg(3),YMIN=1000.,YMAX=MIN(p_rholvl_aavg)
   LEVS,MANUAL=mylevs
   CON,X=longitude,Y=p_rholvl_aavg,FIELD=REFORM(phase_composite_anom_fromzero(i,*,*)),/NOLINES,CB_TITLE=heating_title+' moisture increment (g kg!U-1!N day!U-1!N)',$
       TITLE=heating_title+' moisture increment for MJO phase '+STRTRIM(STRING(i),1)+' latavg 5S-5N for '+run_name+' - anom from phase 0'
   AXES,XSTEP=10,YSTEP=-100,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)',YMINOR=-50,XMINOR=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/moisture_increments/hadgem3kpp_mjo_composite_moisture_increments_latavg_fromnc.phase'+STRTRIM(STRING(i),1)+'.'+run_name+'.'+$
          heating_title+'_scaleamp_anom_phase0.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=700,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
   GSET,XMIN=box_aavg(1),XMAX=box_aavg(3),YMIN=1000.,YMAX=MIN(p_rholvl_aavg)
   LEVS,MANUAL=mylevs
   CON,X=longitude,Y=p_rholvl_aavg,FIELD=REFORM(phase_composite_scaleamp_anom_fromzero(i,*,*)),/NOLINES,CB_TITLE=heating_title+' moisture increment (g kg!U-1!N day!U-1!N)',$
       TITLE=heating_title+' moisture increment for MJO phase '+STRTRIM(STRING(i),1)+' latavg 5S-5N for '+run_name+' - anom from mean phase 0 - scale by MJO amp'
   AXES,XSTEP=10,YSTEP=-100,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)',YMINOR=-50,XMINOR=5
   PSCLOSE,/NOVIEW

ENDFOR

psfile='/home/ss901165/idl/hadgem3-kpp_runs/moisture_increments/hadgem3kpp_mjo_composite_moisture_increments_latavg_fromnc.phase0.'+run_name+'.'+heating_title+'_raw.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=700,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_phasezero)+1
GSET,XMIN=box_aavg(1),XMAX=box_aavg(3),YMIN=1000.,YMAX=MIN(p_rholvl_aavg)
LEVS,MANUAL=mylevs_phasezero
CON,X=longitude,Y=p_rholvl_aavg,FIELD=REFORM(phase_composite(0,*,*)),/NOLINES,CB_TITLE=heating_title+' moisture increment (g kg!U-1!N day!U-1!N)',$
    TITLE=heating_title+' moisture increment for MJO phase 0 (no activity), latavg 5S-5N for '+run_name+' (raw values)'
AXES,XSTEP=10,YSTEP=-100,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)',YMINOR=-50,XMINOR=5
PSCLOSE


STOP
END
