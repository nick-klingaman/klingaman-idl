PRO hadgem3kpp_mjo_composite_diabatic_heating_aavg_fromnc

; Plot MJO phase composites of diabatic heating from a netCDF file.

; Experiment name
expt_name='hadgem3a_amip2_1.0xentrain_rerun_vn74'
run_name='1.0x_entrain'

; Basename for netCDF files
netcdf_basename='/home/ss901165/um_output4/'+expt_name+'/'+expt_name+'.years1-13.composite_mjo_phase'

; Heating components to plot
components=['Tinc_lwrad','Tinc_swrad']
;components=['Tinc_lwrad','Tinc_swrad','Tinc_bdylr','Tinc_conv','Tinc_lsrain']
;components=['Tinc_bdylr','Tinc_conv','Tinc_lsrain']
n_components=N_ELEMENTS(components)
heating_title='radiative'

box_aavg=[-5,150,5,160]
box_name='WPac'

;mylevs=['-1.9','-1.7','-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5','1.7','1.9']
mylevs=['-0.45','-0.39','-0.33','-0.27','-0.21','-0.15','-0.09','-0.03','0.03','0.09','0.15','0.21','0.27','0.33','0.39','0.45']

; Transfer hybrid_ht coordinates to pressure coordinates
rholvl_file='/home/ss901165/um_output4/xfzbv/h9/xfzbva.pjh9cr0.nc'
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
  
      phase_composite=fltarr(n_phases,n_z)
      phase_composite_scaleamp=fltarr(n_phases,n_z)
   ENDIF

   FOR j=0,n_components-1 DO BEGIN
      this_phase_composite=REFORM(OPEN_AND_EXTRACT(input_file,components(j)+'_composite',$
                                                   offset=[box_aavg_tx(1),box_aavg_tx(0),0],$
                                                   count=[n_lon,n_lat,n_z]))*72.

      this_phase_composite_scaleamp=REFORM(OPEN_AND_EXTRACT(input_file,components(j)+'_scaleamp',$
                                                            offset=[box_aavg_tx(1),box_aavg_tx(0),0],$
                                                            count=[n_lon,n_lat,n_z]))*72.
      FOR k=0,n_z-1 DO BEGIN
         phase_composite(i,k)=phase_composite(i,k)+MEAN(this_phase_composite(*,*,k))
         phase_composite_scaleamp(i,k)=phase_composite_scaleamp(i,k)+MEAN(this_phase_composite_scaleamp(*,*,k))
      ENDFOR
   ENDFOR
ENDFOR

phase_composite_anom_fromall=fltarr(n_phases+1,n_z)
phase_composite_anom_fromzero=fltarr(n_phases+1,n_z)
phase_composite_scaleamp_anom_fromall=fltarr(n_phases+1,n_z)
phase_composite_scaleamp_anom_fromzero=fltarr(n_phases+1,n_z)
FOR i=1,n_phases-1 DO BEGIN
   FOR j=0,n_z-1 DO BEGIN
      phase_composite_anom_fromall(i,j)=phase_composite(i,j)-MEAN(phase_composite(1:8,j))
      phase_composite_anom_fromzero(i,j)=phase_composite(i,j)-phase_composite(0,j)
      phase_composite_scaleamp_anom_fromall(i,j)=phase_composite_scaleamp(i,j)-MEAN(phase_composite_scaleamp(1:8,j))
      phase_composite_scaleamp_anom_fromzero(i,j)=phase_composite_scaleamp(i,j)-phase_composite_scaleamp(0,j)
   ENDFOR
ENDFOR

psfile='/home/ss901165/idl/hadgem3-kpp_runs/diabatic_heating/hadgem3kpp_mjo_composite_diabatic_heating_aavg_fromnc.'+box_name+'.all_phases.'+run_name+'.'+heating_title+'_anom_phases1-8.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=700,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
GSET,XMIN=0,XMAX=16,YMIN=1000.,YMAX=MIN(p_rholvl_aavg)
LEVS,MANUAL=mylevs
phase_composite_anom_fromall(n_phases,*)=phase_composite_anom_fromall(1,*)
FOR i=0,1 DO BEGIN
   IF i eq 0 THEN BEGIN
      CON,X=indgen(9),Y=p_rholvl_aavg,FIELD=phase_composite_anom_fromall(1:9,*),/NOLINES,$
          TITLE='Phase composite of diabatic heating ('+heating_title+') for '+run_name+' (14 years) in the '+$
          box_name+' box - ('+STRTRIM(STRING(box_aavg(0)),1)+'N to '+STRTRIM(STRING(box_aavg(2)),1)+'N, '+$
          STRTRIM(STRING(box_aavg(1)),1)+'E to '+STRTRIM(STRING(box_aavg(3)),1)+'E) - anom from phases 1-8',$
          CB_TITLE='Heating (K day!U-1!N)'
   ENDIF ELSE $
      CON,X=indgen(9)+i*8,Y=p_rholvl_aavg,FIELD=phase_composite_anom_fromall(1:9,*),/NOLINES,/NOCOLBAR
ENDFOR
AXES,XVALS=indgen(17),XLABELS=['1','2','3','4','5','6','7','8',$
                               '1','2','3','4','5','6','7','8','1'],$
     XTITLE='Wheeler and Hendon RMM phase',YTITLE='Pressure (hPa)',YSTEP=-50
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3-kpp_runs/diabatic_heating/hadgem3kpp_mjo_composite_diabatic_heating_aavg_fromnc.'+box_name+'.all_phases.'+run_name+'.'+heating_title+'_scaleamp_anom_phases1-8.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=700,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
GSET,XMIN=0,XMAX=16,YMIN=1000.,YMAX=MIN(p_rholvl_aavg)
LEVS,MANUAL=mylevs
phase_composite_scaleamp_anom_fromall(n_phases,*)=phase_composite_scaleamp_anom_fromall(1,*)
FOR i=0,1 DO BEGIN
   IF i eq 0 THEN BEGIN
      CON,X=indgen(9),Y=p_rholvl_aavg,FIELD=phase_composite_scaleamp_anom_fromall(1:9,*),/NOLINES,$
          TITLE='Comp diabatic heating ('+heating_title+') for '+run_name+' (14 years) in the '+$
          box_name+' box - ('+STRTRIM(STRING(box_aavg(0)),1)+'N to '+STRTRIM(STRING(box_aavg(2)),1)+'N, '+$
          STRTRIM(STRING(box_aavg(1)),1)+'E to '+STRTRIM(STRING(box_aavg(3)),1)+'E) - anom from phases 1-8 - scaled by MJO amp',$
          CB_TITLE='Heating (K day!U-1!N) per unit MJO amplitude'
   ENDIF ELSE $
      CON,X=indgen(9)+i*8,Y=p_rholvl_aavg,FIELD=phase_composite_scaleamp_anom_fromall(1:9,*),/NOLINES,/NOCOLBAR
ENDFOR
AXES,XVALS=indgen(17),XLABELS=['1','2','3','4','5','6','7','8',$
                               '1','2','3','4','5','6','7','8','1'],$
     XTITLE='Wheeler and Hendon RMM phase',YTITLE='Pressure (hPa)',YSTEP=-50
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3-kpp_runs/diabatic_heating/hadgem3kpp_mjo_composite_diabatic_heating_aavg_fromnc.'+box_name+'.all_phases.'+run_name+'.'+heating_title+'_anom_phase0.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=700,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
GSET,XMIN=0,XMAX=16,YMIN=1000.,YMAX=MIN(p_rholvl_aavg)
LEVS,MANUAL=mylevs
phase_composite_anom_fromzero(n_phases,*)=phase_composite_anom_fromzero(1,*)
FOR i=0,1 DO BEGIN
   IF i eq 0 THEN BEGIN
      CON,X=indgen(9),Y=p_rholvl_aavg,FIELD=phase_composite_anom_fromzero(1:9,*),/NOLINES,$
          TITLE='Phase composite of diabatic heating ('+heating_title+') for '+run_name+' (14 years) in the '+$
          box_name+' box - ('+STRTRIM(STRING(box_aavg(0)),1)+'N to '+STRTRIM(STRING(box_aavg(2)),1)+'N, '+$
          STRTRIM(STRING(box_aavg(1)),1)+'E to '+STRTRIM(STRING(box_aavg(3)),1)+'E) - anom from phase 0',$
          CB_TITLE='Heating (K day!U-1!N)'
   ENDIF ELSE $
      CON,X=indgen(9)+i*8,Y=p_rholvl_aavg,FIELD=phase_composite_anom_fromzero(1:9,*),/NOLINES,/NOCOLBAR
ENDFOR
AXES,XVALS=indgen(17),XLABELS=['1','2','3','4','5','6','7','8',$
                               '1','2','3','4','5','6','7','8','1'],$
     XTITLE='Wheeler and Hendon RMM phase',YTITLE='Pressure (hPa)',YSTEP=-50
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3-kpp_runs/diabatic_heating/hadgem3kpp_mjo_composite_diabatic_heating_aavg_fromnc.'+box_name+'.all_phases.'+run_name+'.'+heating_title+'_scaleamp_anom_phase0.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=700,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
GSET,XMIN=0,XMAX=16,YMIN=1000.,YMAX=MIN(p_rholvl_aavg)
LEVS,MANUAL=mylevs
phase_composite_scaleamp_anom_fromzero(n_phases,*)=phase_composite_scaleamp_anom_fromzero(1,*)
FOR i=0,1 DO BEGIN
   IF i eq 0 THEN BEGIN
      CON,X=indgen(9),Y=p_rholvl_aavg,FIELD=phase_composite_scaleamp_anom_fromzero(1:9,*),/NOLINES,$
          TITLE='Comp diabatic heating ('+heating_title+') for '+run_name+' (14 years) in the '+$
          box_name+' box - ('+STRTRIM(STRING(box_aavg(0)),1)+'N to '+STRTRIM(STRING(box_aavg(2)),1)+'N, '+$
          STRTRIM(STRING(box_aavg(1)),1)+'E to '+STRTRIM(STRING(box_aavg(3)),1)+'E) - anom from phase 0 - scaled by MJO amp',$
          CB_TITLE='Heating (K day!U-1!N) per unit MJO amplitude'
   ENDIF ELSE $
      CON,X=indgen(9)+i*8,Y=p_rholvl_aavg,FIELD=phase_composite_scaleamp_anom_fromzero(1:9,*),/NOLINES,/NOCOLBAR
ENDFOR
AXES,XVALS=indgen(17),XLABELS=['1','2','3','4','5','6','7','8',$
                               '1','2','3','4','5','6','7','8','1'],$
     XTITLE='Wheeler and Hendon RMM phase',YTITLE='Pressure (hPa)',YSTEP=-50
PSCLOSE,/NOVIEW

STOP
END
