PRO hadgem3kpp_mjo_composite_heating_latavg_phase0_fromnc
  
; Plot MJO phase composites of latitude-averaged diabatic heating for phase 0 for both experiments and differences.

n_experiments=2

; Heating components to plot
;components=['Tinc_lwrad','Tinc_swrad']
components=['Tinc_lwrad','Tinc_swrad','Tinc_bdylr','Tinc_conv','Tinc_lsrain']
;components=['Tinc_bdylr','Tinc_conv','Tinc_lsrain']
n_components=N_ELEMENTS(components)
;heating_title='radiative'
;heating_title='non-radiative'
heating_title='total'

box_aavg=[-5,40,5,240]
n_z=57

mylevs_phasezero=['-5.7','-5.1','-4.5','-3.9','-3.3','-2.7','-2.1','-1.5','-0.9','-0.3',$
                  '0.3','0.9','1.5','2.1','2.7','3.3','3.9','4.5','5.1','5.7']
;mylevs_phasezero=['-1.95','-1.65','-1.35','-1.05','-0.75','-0.45','-0.15','0.15','0.45','0.75','1.05','1.35','1.65','1.95']
;mylevs_phasezero_diff=['-0.75','-0.65','-0.55','-0.45','-0.35','-0.25','-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75']
mylevs_phasezero_diff=['-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5']

; Transfer hybrid_ht coordinates to pressure coordinates
rholvl_file='/home/ss901165/um_output4/xfzbo/h9/xfzboa.pjh9cr0.nc'
rholvl_longitude=OPEN_AND_EXTRACT(rholvl_file,'longitude')
rholvl_latitude=OPEN_AND_EXTRACT(rholvl_file,'latitude')
DEFINE_BOUNDARIES,box_aavg,rholvl_latitude,rholvl_longitude,box_rholvl_tx,/LIMIT
rholvl_nlon=N_ELEMENTS(rholvl_longitude)
rholvl_nlat=N_ELEMENTS(rholvl_latitude)

p_rholvl=OPEN_AND_EXTRACT(rholvl_file,'p',offset=[box_rholvl_tx(1),box_rholvl_tx(0),0,0],$
                          count=[rholvl_nlon,rholvl_nlat,n_z,1])
p_rholvl_aavg=fltarr(n_z)
FOR j=0,n_z-1 DO $
   p_rholvl_aavg(j)=MEAN(p_rholvl(*,*,j))/100.

FOR i=0,n_experiments-1 DO BEGIN
   CASE i OF 
      0 : BEGIN
         expt_name='hadgem3a_amip2_1.0xentrain_rerun_vn74'
         run_name='1.0x_entrain'
         years='1-13'
      END
      1 : BEGIN
         expt_name='hadgem3a_amip2_1.5xentrain_rerun_vn74'
         run_name='1.5x_entrain'
         years='1-14'
      END
   ENDCASE
                                ; Basename for netCDF files
   phase0_input_file='/home/ss901165/um_output4/'+expt_name+'/'+expt_name+'.years'+years+'.composite_mjo_phase0.nc'

   IF i eq 0 THEN BEGIN
      longitude=OPEN_AND_EXTRACT(phase0_input_file,'longitude')
      latitude=OPEN_AND_EXTRACT(phase0_input_file,'latitude')
      z=OPEN_AND_EXTRACT(phase0_input_file,'z')
      DEFINE_BOUNDARIES,box_aavg,latitude,longitude,box_aavg_tx,/LIMIT
      n_lon=N_ELEMENTS(longitude)
      n_lat=N_ELEMENTS(latitude)
      n_z=N_ELEMENTS(z)

      phase0_composites=fltarr(n_experiments,n_lon,n_z)
      phase0_composites_scaleamp=fltarr(n_experiments,n_lon,n_z)
      phase18_composites=fltarr(n_experiments,n_lon,n_z)
      phase18_composites_scaleamp=fltarr(n_experiments,n_lon,n_z)
   ENDIF
   
   FOR j=0,n_components-1 DO BEGIN
      this_phase0_composite=REFORM(OPEN_AND_EXTRACT(phase0_input_file,components(j)+'_composite',$
                                                    offset=[box_aavg_tx(1),box_aavg_tx(0),0],$
                                                    count=[n_lon,n_lat,n_z]))*72.
      
      this_phase0_composite_scaleamp=REFORM(OPEN_AND_EXTRACT(phase0_input_file,components(j)+'_scaleamp',$
                                                             offset=[box_aavg_tx(1),box_aavg_tx(0),0],$
                                                             count=[n_lon,n_lat,n_z]))*72.
      FOR k=0,n_lon-1 DO BEGIN
         FOR m=0,n_z-1 DO BEGIN
            phase0_composites(i,k,m)=phase0_composites(i,k,m)+MEAN(this_phase0_composite(k,*,m))
            phase0_composites_scaleamp(i,k,m)=phase0_composites_scaleamp(i,k,m)+MEAN(this_phase0_composite_scaleamp(k,*,m))
         ENDFOR
      ENDFOR
      FOR k=1,8 DO BEGIN
         phase18_input_file='/home/ss901165/um_output4/'+expt_name+'/'+expt_name+'.years'+years+'.composite_mjo_phase'+STRTRIM(STRING(k),1)+'.nc'
         this_phase18_composite=REFORM(OPEN_AND_EXTRACT(phase18_input_file,components(j)+'_composite',$
                                                        offset=[box_aavg_tx(1),box_aavg_tx(0),0],$
                                                        count=[n_lon,n_lat,n_z]))*72.
         this_phase18_composite_scaleamp=REFORM(OPEN_AND_EXTRACT(phase18_input_file,components(j)+'_scaleamp',$
                                                                 offset=[box_aavg(1),box_aavg_tx(0),0],$
                                                                 count=[n_lon,n_lat,n_z]))*72.
         FOR m=0,n_lon-1 DO BEGIN
            FOR n=0,n_z-1 DO BEGIN
               phase18_composites(i,m,n)=phase18_composites(i,m,n)+MEAN(this_phase18_composite(m,*,n))
               phase18_composites_scaleamp(i,m,n)=phase18_composites_scaleamp(i,m,n)+MEAN(this_phase18_composite_scaleamp(m,*,n))
            ENDFOR
         ENDFOR
      ENDFOR
   ENDFOR

   phase18_composites(i,*,*)=phase18_composites(i,*,*)/8.
   phase18_composites_scaleamp(i,*,*)=phase18_composites_scaleamp(i,*,*)/8.
   
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/diabatic_heating/hadgem3kpp_mjo_composite_diabatic_heating_latavg_phase0_fromnc.phase0.'+run_name+'.'+heating_title+'_raw.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=700,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_phasezero)+1
   GSET,XMIN=box_aavg(1),XMAX=box_aavg(3),YMIN=1000.,YMAX=MIN(p_rholvl_aavg)
   LEVS,MANUAL=mylevs_phasezero
   CON,X=longitude,Y=p_rholvl_aavg,FIELD=REFORM(phase0_composites(i,*,*)),/NOLINES,CB_TITLE=heating_title+' diabatic heating (K day!U-1!N)',$
       TITLE=heating_title+' diabatic heating for MJO phase 0 (no activity), latavg 5S-5N for '+run_name+' (raw values)'
   AXES,XSTEP=10,YSTEP=-100,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)',YMINOR=-50,XMINOR=5
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/diabatic_heating/hadgem3kpp_mjo_composite_diabatic_heating_latavg_phase0_fromnc.phases1-8.'+run_name+'.'+heating_title+'_raw.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=700,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_phasezero)+1
   GSET,XMIN=box_aavg(1),XMAX=box_aavg(3),YMIN=1000.,YMAX=MIN(p_rholvl_aavg)
   LEVS,MANUAL=mylevs_phasezero
   CON,X=longitude,Y=p_rholvl_aavg,FIELD=REFORM(phase18_composites(i,*,*)),/NOLINES,CB_TITLE=heating_title+' diabatic heating (K day!U-1!N)',$
       TITLE=heating_title+' diabatic heating for mean of MJO phases 1-8, latavg 5S-5N for '+run_name+' (raw values)'
   AXES,XSTEP=10,YSTEP=-100,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)',YMINOR=-50,XMINOR=5
   PSCLOSE,/NOVIEW   

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/diabatic_heating/hadgem3kpp_mjo_composite_diabatic_heating_latavg_phase0_fromnc.phase0_scaleamp.'+run_name+'.'+heating_title+'_raw.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=700,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_phasezero)+1
   GSET,XMIN=box_aavg(1),XMAX=box_aavg(3),YMIN=1000.,YMAX=MIN(p_rholvl_aavg)
   LEVS,MANUAL=mylevs_phasezero
   CON,X=longitude,Y=p_rholvl_aavg,FIELD=REFORM(phase0_composites_scaleamp(i,*,*)),/NOLINES,CB_TITLE=heating_title+' diabatic heating (K day!U-1!N)',$
       TITLE=heating_title+' diabatic heating for MJO phase 0 (no activity), latavg 5S-5N for '+run_name+' (scaled by MJO amp)'
   AXES,XSTEP=10,YSTEP=-100,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)',YMINOR=-50,XMINOR=5
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/diabatic_heating/hadgem3kpp_mjo_composite_diabatic_heating_latavg_phase0_fromnc.phases1-8_scaleamp.'+run_name+'.'+heating_title+'_raw.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=700,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_phasezero)+1
   GSET,XMIN=box_aavg(1),XMAX=box_aavg(3),YMIN=1000.,YMAX=MIN(p_rholvl_aavg)
   LEVS,MANUAL=mylevs_phasezero
   CON,X=longitude,Y=p_rholvl_aavg,FIELD=REFORM(phase18_composites_scaleamp(i,*,*)),/NOLINES,CB_TITLE=heating_title+' diabatic heating (K day!U-1!N)',$
       TITLE=heating_title+' diabatic heating for mean of MJO phases 1-8, latavg 5S-5N for '+run_name+' (scaled by MJO amp)'
   AXES,XSTEP=10,YSTEP=-100,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)',YMINOR=-50,XMINOR=5
   PSCLOSE,/NOVIEW   

ENDFOR

psfile='/home/ss901165/idl/hadgem3-kpp_runs/diabatic_heating/hadgem3kpp_mjo_composite_diabatic_heating_latavg_fromnc.phase0.1.5x-minus-1.0x.'+heating_title+'_raw.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=700,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_phasezero_diff)+1
GSET,XMIN=box_aavg(1),XMAX=box_aavg(3),YMIN=1000.,YMAX=MIN(p_rholvl_aavg)
LEVS,MANUAL=mylevs_phasezero_diff
CON,X=longitude,Y=p_rholvl_aavg,FIELD=REFORM(phase0_composites(1,*,*)-phase0_composites(0,*,*)),$
    /NOLINES,CB_TITLE=heating_title+' diabatic heating (K day!U-1!N)',$
    TITLE='Difference in '+heating_title+' diabatic heating for MJO phase 0, latavg 5S-5N for 1.5x minus 1.0x entrain (raw values)'
AXES,XSTEP=10,YSTEP=-100,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)',YMINOR=-50,XMINOR=5
PSCLOSE

psfile='/home/ss901165/idl/hadgem3-kpp_runs/diabatic_heating/hadgem3kpp_mjo_composite_diabatic_heating_latavg_fromnc.phase0_scaleamp.1.5x-minus-1.0x.'+heating_title+'_raw.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=700,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_phasezero_diff)+1
GSET,XMIN=box_aavg(1),XMAX=box_aavg(3),YMIN=1000.,YMAX=MIN(p_rholvl_aavg)
LEVS,MANUAL=mylevs_phasezero_diff
CON,X=longitude,Y=p_rholvl_aavg,FIELD=REFORM(phase0_composites(1,*,*)-phase0_composites(0,*,*)),$
    /NOLINES,CB_TITLE=heating_title+' diabatic heating (K day!U-1!N)',$
    TITLE='Difference in '+heating_title+' diabatic heating for MJO phase 0, latavg 5S-5N for 1.5x minus 1.0x entrain (scaled by amplitude)'
AXES,XSTEP=10,YSTEP=-100,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)',YMINOR=-50,XMINOR=5
PSCLOSE

psfile='/home/ss901165/idl/hadgem3-kpp_runs/diabatic_heating/hadgem3kpp_mjo_composite_diabatic_heating_latavg_fromnc.phases1-8.1.5x-minus-1.0x.'+heating_title+'_raw.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=700,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_phasezero_diff)+1
GSET,XMIN=box_aavg(1),XMAX=box_aavg(3),YMIN=1000.,YMAX=MIN(p_rholvl_aavg)
LEVS,MANUAL=mylevs_phasezero_diff
CON,X=longitude,Y=p_rholvl_aavg,FIELD=REFORM(phase18_composites(1,*,*)-phase18_composites(0,*,*)),$
    /NOLINES,CB_TITLE=heating_title+' diabatic heating (K day!U-1!N)',$
    TITLE='Difference in '+heating_title+' diabatic heating for mean of MJO phases 1-8, latavg 5S-5N for 1.5x minus 1.0x entrain (raw values)'
AXES,XSTEP=10,YSTEP=-100,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)',YMINOR=-50,XMINOR=5
PSCLOSE

psfile='/home/ss901165/idl/hadgem3-kpp_runs/diabatic_heating/hadgem3kpp_mjo_composite_diabatic_heating_latavg_fromnc.phase18_scaleamp.1.5x-minus-1.0x.'+heating_title+'_raw.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=700,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_phasezero_diff)+1
GSET,XMIN=box_aavg(1),XMAX=box_aavg(3),YMIN=1000.,YMAX=MIN(p_rholvl_aavg)
LEVS,MANUAL=mylevs_phasezero_diff
CON,X=longitude,Y=p_rholvl_aavg,FIELD=REFORM(phase18_composites(1,*,*)-phase18_composites(0,*,*)),$
    /NOLINES,CB_TITLE=heating_title+' diabatic heating (K day!U-1!N)',$
    TITLE='Difference in '+heating_title+' diabatic heating for MJO phase 0, latavg 5S-5N for 1.5x minus 1.0x entrain (scaled by amplitude)'
AXES,XSTEP=10,YSTEP=-100,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)',YMINOR=-50,XMINOR=5
PSCLOSE

STOP
END

