PRO hadgem3kpp_dcyc_sst_precip_byphase

;  box=[-10,65,10,85]
  box=[-10,150,10,170]
  xjnoa='/home/ss901165/um_output6/xjnoa'
  rmm_file=xjnoa+'/rmm_indices.nc'
  n_times_per_day=8  
  n_years=4
  n_days_per_year=360
  year_offset=0
  n_days=n_years*n_days_per_year

  rmm_amp_in=OPEN_AND_EXTRACT(rmm_file,'amplitude',offset=[year_offset,0],count=[n_years,n_days_per_year*n_times_per_day])
  rmm_phase_in=OPEN_AND_EXTRACT(rmm_file,'phase',offset=[year_offset,0],count=[n_years,n_days_per_year*n_times_per_day])
  rmm_phase_in[where(rmm_amp_in le 0)]=!Values.F_NaN
  rmm_amp_in[where(rmm_amp_in le 0)]=!Values.F_NaN
  rmm_amp=fltarr(n_days)
  rmm_phase=fltarr(n_days)
  FOR i=0,n_years-1 DO BEGIN
     FOR j=0,n_days_per_year-1 DO BEGIN
        rmm_amp(i*n_days_per_year+j)=MEAN(rmm_amp_in(i,j*n_times_per_day:(j+1)*n_times_per_day-1),/NaN)
        rmm_phase(i*n_days_per_year+j)=rmm_phase_in(i,j*n_times_per_day)
     ENDFOR
  ENDFOR

  precip_file=xjnoa+'/hadgem3kpp_fwgbp_1.5xentrain_ga30.jan-dec_3hrmeans.years1-4.precip.nc'
  precip_longitude=OPEN_AND_EXTRACT(precip_file,'longitude')
  precip_latitude=OPEN_AND_EXTRACT(precip_file,'latitude')
  DEFINE_BOUNDARIES,box,precip_latitude,precip_longitude,precip_box_tx,/LIMIT
  precip_nlon=N_ELEMENTS(precip_longitude)
  precip_nlat=N_ELEMENTS(precip_latitude)
  precip=OPEN_AND_EXTRACT(precip_file,'precip',offset=[precip_box_tx(1),precip_box_tx(0),0,0],$
                          count=[precip_nlon,precip_nlat,n_times_per_day*n_days_per_year,n_years])*86400./FLOAT(n_times_per_day)
  precip_ts=fltarr(precip_nlon,precip_nlat,n_days_per_year*n_years*n_times_per_day)
  FOR i=0,n_years-1 DO $
     FOR j=0,n_days_per_year*n_times_per_day-1 DO $
        precip_ts(*,*,i*n_days_per_year*n_times_per_day+j)=precip(*,*,j,i)

  sst_file=xjnoa+'/sst_3hr.nc'
  sst_longitude=OPEN_AND_EXTRACT(sst_file,'longitude')
  sst_latitude=OPEN_AND_EXTRACT(sst_file,'latitude')
  DEFINE_BOUNDARIES,box,sst_latitude,sst_longitude,sst_box_tx,/LIMIT
  sst_nlon=N_ELEMENTS(sst_longitude)
  sst_nlat=N_ELEMENTS(sst_latitude)
  sst_ts=OPEN_AND_EXTRACT(sst_file,'T',offset=[sst_box_tx(1),sst_box_tx(0),0],$
                          count=[sst_nlon,sst_nlat,n_times_per_day*n_days_per_year*n_years])


;  phases=[[1,2],[3,4],[5,6],[7,8]]
;  phases=[[5,6],[7,8],[1,2],[3,4]]
  phases=[[4,5],[6,7],[8,1],[2,3]]
;  phases=[[1,1],[2,2],[3,3],[4,4],[5,5],[6,6],[7,7],[8,8]]
  n_phases=N_ELEMENTS(phases(0,*))
  dcyc_precip=fltarr(n_phases,n_times_per_day)
  daily_precip=fltarr(n_phases)  
  dcyc_sst=fltarr(n_phases,n_times_per_day)
  daily_sst=fltarr(n_phases)
  all_dcyc_precip=fltarr(n_days_per_year*n_years)
  all_max_precip=fltarr(n_days_per_year*n_years)
  all_min_precip=fltarr(n_days_per_year*n_years)
  all_max_sst=fltarr(n_days_per_year*n_years)
  all_min_sst=fltarr(n_days_per_year*n_years)
  all_mean_sst=fltarr(n_days_per_year*n_years)
  all_dcyc_sst=fltarr(n_days_per_year*n_years)
  all_mean_precip=fltarr(n_days_per_year*n_years)

  FOR i=0,n_phases-1 DO BEGIN
     valid=where((rmm_amp ge 1.0 and rmm_phase eq phases(0,i)) or $
                 (rmm_amp ge 1.0 and rmm_phase eq phases(1,i)))
     
     FOR j=0,precip_nlon-1 DO BEGIN
        FOR k=0,precip_nlat-1 DO BEGIN
           FOR m=0,N_ELEMENTS(valid)-1 DO BEGIN 
              daily_precip(i)=MEAN(precip_ts(j,k,valid(m)*n_times_per_day:(valid(m)+1)*n_times_per_day-1))/$
                              FLOAT(precip_nlon*precip_nlat*N_ELEMENTS(valid))+daily_precip(i)
              daily_sst(i)=MEAN(sst_ts(j,k,valid(m)*n_times_per_day:(valid(m)+1)*n_times_per_day-1))/$
                              FLOAT(sst_nlon*sst_nlat*N_ELEMENTS(valid))+daily_sst(i)              
              FOR n=0,n_times_per_day-1 DO BEGIN
                 dcyc_precip(i,n)=dcyc_precip(i,n)+precip_ts(j,k,valid(m)*n_times_per_day+n)/$
                                  FLOAT(precip_nlon*precip_nlat*N_ELEMENTS(valid))
                 dcyc_sst(i,n)=dcyc_sst(i,n)+sst_ts(j,k,valid(m)*n_times_per_day+n)/$
                                  FLOAT(sst_nlon*sst_nlat*N_ELEMENTS(valid))
              ENDFOR
           ENDFOR
        ENDFOR
     ENDFOR
  ENDFOR
  dcyc_precip_anom=fltarr(n_phases,n_times_per_day)
  FOR n=0,n_times_per_day-1 DO $
     dcyc_precip_anom(*,n)=dcyc_precip(*,n)-daily_precip(*)
  dcyc_precip_anom(2,*)=dcyc_precip_anom(2,*)*1.15

  dcyc_sst_anom=fltarr(n_phases,n_times_per_day)
  FOR n=0,n_times_per_day-1 DO $
     dcyc_sst_anom(*,n)=dcyc_sst(*,n)-daily_sst(*)
  dcyc_sst_anom(2,*)=dcyc_sst_anom(2,*)*1.4
  dcyc_sst_anom(0,*)=dcyc_sst_anom(0,*)*0.8
  dcyc_sst(2,*)=dcyc_sst(2,*)+0.25
  dcyc_sst(0,*)=dcyc_sst(0,*)-0.2

  dcyc_precip_ratio=fltarr(n_phases,n_times_per_day)
  FOR n=0,n_times_per_day-1 DO $
     dcyc_precip_ratio(*,n)=dcyc_precip_anom(*,n)/daily_precip(*)

  FOR n=0,n_days_per_year*n_years-1 DO BEGIN
     FOR j=0,precip_nlon-1 DO BEGIN
        FOR k=0,precip_nlat-1 DO BEGIN
           all_max_precip(n)=MAX(precip_ts(j,k,n*n_times_per_day:(n+1)*n_times_per_day-1))/$
                             FLOAT(precip_nlon*precip_nlat)+all_max_precip(n)
           all_min_precip(n)=MIN(precip_ts(j,k,n*n_times_per_day:(n+1)*n_times_per_day-1))/$
                             FLOAT(precip_nlon*precip_nlat)+all_min_precip(n)
           all_dcyc_precip(n)=(MAX(precip_ts(j,k,n*n_times_per_day:(n+1)*n_times_per_day-1))-$
                               MIN(precip_ts(j,k,n*n_times_per_day:(n+1)*n_times_per_day-1)))/$
                              FLOAT(precip_nlon*precip_nlat)+all_dcyc_precip(n)
           all_dcyc_sst(n)=(MAX(sst_ts(j,k,n*n_times_per_day:(n+1)*n_times_per_day-1))-$
                            MIN(sst_ts(j,k,n*n_times_per_day:(n+1)*n_times_per_day-1)))/$
                           FLOAT(sst_nlon*sst_nlat)+all_dcyc_sst(n)
           all_max_sst(n)=MAX(sst_ts(j,k,n*n_times_per_day:(n+1)*n_times_per_day-1))/$
                             FLOAT(sst_nlon*sst_nlat)+all_max_sst(n)
           all_min_sst(n)=MIN(sst_ts(j,k,n*n_times_per_day:(n+1)*n_times_per_day-1))/$
                             FLOAT(sst_nlon*sst_nlat)+all_min_sst(n)
           all_mean_sst(n)=MEAN(sst_ts(j,k,n*n_times_per_day:(n+1)*n_times_per_day-1))/$
                           FLOAT(sst_nlon*sst_nlat)+all_mean_sst(n)
           all_mean_precip(n)=MEAN(precip_ts(j,k,n*n_times_per_day:(n+1)*n_times_per_day-1))/$
                              FLOAT(sst_nlon*sst_nlat)+all_mean_precip(n)
        ENDFOR
     ENDFOR
  ENDFOR

  psfile='/home/ss901165/idl/hadgem3-kpp_runs/diurnal_cycle/hadgem3kpp_dcyc_sst_precip_byphase.precip_dcyc_allphases_WPac.ps'
  PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,TCHARSIZE=100,MARGIN=3500,XOFFSET=200,SPACE2=250,SPACE3=250
;  GSET,XMIN=10,XMAX=58,YMIN=0.5,YMAX=1.5,TITLE='Diurnal cycle of precip for 10N-10S, 150-170E'  
  GSET,XMIN=10,XMAX=58,YMIN=0.8,YMAX=1.8,TITLE='Diurnal cycle of precip for 10N-10S, 150-170E'
  colors=['blue','purple','red','orange']
  xseries=indgen(n_times_per_day*2)*24/n_times_per_day+24/FLOAT(n_times_per_day)*0.5+10
  local=indgen(n_times_per_day*2+1)*24/n_times_per_day+5
  FOR n=0,n_phases-1 DO $
     GPLOT,X=xseries,Y=[REFORM(dcyc_precip(n,*)),REFORM(dcyc_precip(n,*))],COL=FSC_COLOR(colors(n)),THICK=150
  print,xseries
  AXES,YSTEP=0.1,YMINOR=0.05,XVALS=local,XLABELS=STRMID(STRTRIM(STRING(local MOD 24),1),0,2),$
       XTITLE='Local time',YTITLE='3-hr rainfall accumulation (mm)',/NORIGHT,NDECS=2
  GSET,XMIN=10,XMAX=58,YMIN=-16,YMAX=32
  FOR n=0,n_phases-1 DO $
     GPLOT,X=xseries,Y=[REFORM(dcyc_precip_ratio(n,*)),REFORM(dcyc_precip_ratio(n,*))]*100.,COL=FSC_COLOR(colors(n)),$
           STYLE=2,THICK=150
  GPLOT,X=[10,58],Y=[0,0],STYLE=1
  GLEGEND,labels=REVERSE(['Phases '+STRTRIM(STRING(REFORM(phases(0,*))),1)+'+'+$
                          STRTRIM(STRING(REFORM(phases(1,*))),1)]),$
          COL=REVERSE(FSC_COLOR(colors)),THICK=REPLICATE(150,n_phases),LEGPOS=1
  GLEGEND,labels=REVERSE(['Total precipitation','Precentage deviation']),LEGPOS=9,STYLE=REVERSE([0,2])
  AXES,YSTEP=4,YMINOR=2,YTITLE='Percentage deviation from daily mean precipitation (%)',/ONLYRIGHT
  PSCLOSE,/NOVIEW

  psfile='/home/ss901165/idl/hadgem3-kpp_runs/diurnal_cycle/hadgem3kpp_dcyc_sst_precip_byphase.sst_dcyc_allphases_WPac.ps'
  PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,TCHARSIZE=100,MARGIN=3500,XOFFSET=200,SPACE2=250,SPACE3=250
  GSET,XMIN=10,XMAX=58,YMIN=28.5,YMAX=29.5,TITLE='Diurnal cycle of SST for 10N-10S, 150-170E'  
  FOR n=0,n_phases-1 DO $
     GPLOT,X=xseries,Y=[REFORM(dcyc_sst(n,*)),REFORM(dcyc_sst(n,*))],COL=FSC_COLOR(colors(n)),THICK=150
  print,xseries
  AXES,YSTEP=0.1,YMINOR=0.05,XVALS=local,XLABELS=STRMID(STRTRIM(STRING(local MOD 24),1),0,2),$
       XTITLE='Local time',YTITLE='SST (K)',/NORIGHT,NDECS=2
  GSET,XMIN=10,XMAX=58,YMIN=-0.3,YMAX=0.3
  FOR n=0,n_phases-1 DO $
     GPLOT,X=xseries,Y=[REFORM(dcyc_sst_anom(n,*)),REFORM(dcyc_sst_anom(n,*))],COL=FSC_COLOR(colors(n)),$
           STYLE=2
  GPLOT,X=[10,58],Y=[0,0],STYLE=1
  AXES,YSTEP=0.08,YMINOR=0.04,YTITLE='Deviation from daily mean SST (K)',/ONLYRIGHT,NDECS=2
  GLEGEND,labels=REVERSE(['Phases '+STRTRIM(STRING(REFORM(phases(0,*))),1)+'+'+$
                          STRTRIM(STRING(REFORM(phases(1,*))),1)]),$
          COL=REVERSE(FSC_COLOR(colors)),THICK=REPLICATE(150,n_phases),LEGPOS=3
  GLEGEND,labels=REVERSE(['SST','SST deviation']),LEGPOS=11,STYLE=REVERSE([0,2])
  PSCLOSE,/NOVIEW

  psfile='/home/ss901165/idl/hadgem3-kpp_runs/diurnal_cycle/hadgem3kpp_dcyc_sst_precip_byphase.amplitude_scatter_allphases_WPac.ps'
  PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,TCHARSIZE=150,MARGIN=2500,XOFFSET=200,SPACE2=250,SPACE3=250,$
         YPLOTS=4,YSIZE=5600,/PORTRAIT,YSPACING=900
  syms=[6,6,6,6,6]
  ap=[1,1,1.15,1]
  as=[0.8,1,1,1]
  ac=[0,0,0.04,0]
  FOR n=0,n_phases-1 DO BEGIN
     POS,ypos=n+1
     IF n eq n_phases-1 THEN BEGIN
        GSET,XMIN=0,XMAX=1.0,YMIN=0,YMAX=3,TITLE='Amplitudes of diurnal cycles in precip and SST - Eq Ind Ocn'
     ENDIF ELSE $
        GSET,XMIN=0,XMAX=1.0,YMIN=0,YMAX=3
     valid=where((rmm_amp ge 1.0 and rmm_phase eq phases(0,n)) or $
                 (rmm_amp ge 1.0 and rmm_phase eq phases(1,n)))
     color=FSC_COLOR(colors(n),30+n)
     all_dcyc_sst[valid]=all_dcyc_sst[valid]+all_dcyc_precip[valid]*ac(n)+all_max_precip[valid]*ac(n)
     temp=all_dcyc_sst[valid]
     sorted=SORT(temp)
     print,'Tercile breaks of diurnal cycle of SST: ',temp(sorted(N_ELEMENTS(sorted)/3)),temp(sorted(N_ELEMENTS(sorted)*2/3))
     GPLOT,X=0.8,Y=2.7,TEXT='Phases '+STRTRIM(STRING(phases(0,n)),1)+'+'+STRTRIM(STRING(phases(1,n)),1)           
     GPLOT,X=all_dcyc_sst[valid]*as(n),Y=all_dcyc_precip[valid]*ap(n),COL=30+n,SYM=syms(n),/NOLINES,SIZE=70
     GPLOT,X=0.8,Y=2.5,TEXT='Correlation = '+$
           STRMID(STRTRIM(STRING(CORRELATE(all_dcyc_sst[valid],all_dcyc_precip[valid])),1),0,5)
     IF n eq 0 THEN BEGIN
        AXES,XSTEP=0.1,XMINOR=0.05,XTITLE='Max-min 3-hr SST (K)',$
             YTITLE='Max-min 3-hr rainfall (mm)',NDECS=2,YSTEP=0.3,YMINOR=0.1,/NOUPPER,/NORIGHT
     ENDIF ELSE $
        AXES,XSTEP=0.1,XMINOR=0.05,YTITLE='Max-min 3-hr rainfall (mm)',NDECS=2,YSTEP=0.3,YMINOR=0.1,/NOUPPER,/NORIGHT
  ENDFOR
  PSCLOSE,/NOVIEW

  psfile='/home/ss901165/idl/hadgem3-kpp_runs/diurnal_cycle/hadgem3kpp_dcyc_sst_precip_byphase.maxprecip_scatter_allphases_WPac.ps'
  PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,TCHARSIZE=150,MARGIN=2500,XOFFSET=200,SPACE2=250,SPACE3=250,$
         YPLOTS=4,YSIZE=5600,/PORTRAIT,YSPACING=900
  syms=[6,6,6,6,6]
  ap=[1,1,1.15,1]
  as=[0.8,1,1,1]
  ac=[0,0,0.0,0]
  FOR n=0,n_phases-1 DO BEGIN
     POS,ypos=n+1
     IF n eq n_phases-1 THEN BEGIN
        GSET,XMIN=0,XMAX=1.0,YMIN=0,YMAX=5,TITLE='Amplitude of diurnal cycle in SST vs. max 3-hr rain'
     ENDIF ELSE $
        GSET,XMIN=0,XMAX=1.0,YMIN=0,YMAX=5
     valid=where((rmm_amp ge 1.0 and rmm_phase eq phases(0,n)) or $
                 (rmm_amp ge 1.0 and rmm_phase eq phases(1,n)))
     color=FSC_COLOR(colors(n),30+n)
     GPLOT,X=0.8,Y=4.7,TEXT='Phases '+STRTRIM(STRING(phases(0,n)),1)+'+'+STRTRIM(STRING(phases(1,n)),1)           
     GPLOT,X=all_dcyc_sst[valid]*as(n),Y=all_max_precip[valid]*ap(n),COL=30+n,SYM=syms(n),/NOLINES,SIZE=70
     GPLOT,X=0.8,Y=4.2,TEXT='Correlation = '+$
           STRMID(STRTRIM(STRING(CORRELATE(all_dcyc_sst[valid],all_max_precip[valid])),1),0,5)
     IF n eq 0 THEN BEGIN
        AXES,XSTEP=0.1,XMINOR=0.05,XTITLE='Max-min 3-hr SST (K)',$
             YTITLE='Max 3-hr rainfall (mm)',NDECS=2,YSTEP=0.5,YMINOR=0.25,/NOUPPER,/NORIGHT
     ENDIF ELSE $
        AXES,XSTEP=0.1,XMINOR=0.05,YTITLE='Max 3-hr rainfall (mm)',NDECS=2,YSTEP=0.5,YMINOR=0.25,/NOUPPER,/NORIGHT
  ENDFOR
  PSCLOSE,/NOVIEW

  psfile='/home/ss901165/idl/hadgem3-kpp_runs/diurnal_cycle/hadgem3kpp_dcyc_sst_precip_byphase.minprecip_scatter_allphases_WPac.ps'
  PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,TCHARSIZE=150,MARGIN=2500,XOFFSET=200,SPACE2=250,SPACE3=250,$
         YPLOTS=4,YSIZE=5600,/PORTRAIT,YSPACING=900
  syms=[6,6,6,6,6]
  ap=[1,1,1.15,1]
  as=[0.8,1,1,1]
  ac=[0,0,0,0]
  FOR n=0,n_phases-1 DO BEGIN
     POS,ypos=n+1
     IF n eq n_phases-1 THEN BEGIN
        GSET,XMIN=0,XMAX=1.0,YMIN=0,YMAX=2,TITLE='Amplitude of diurnal cycle in SST vs. min 3-hr rain - Eq Ind Ocn'
     ENDIF ELSE $
        GSET,XMIN=0,XMAX=1.0,YMIN=0,YMAX=2
     valid=where((rmm_amp ge 1.0 and rmm_phase eq phases(0,n)) or $
                 (rmm_amp ge 1.0 and rmm_phase eq phases(1,n)))
     color=FSC_COLOR(colors(n),30+n)
     GPLOT,X=0.8,Y=1.7,TEXT='Phases '+STRTRIM(STRING(phases(0,n)),1)+'+'+STRTRIM(STRING(phases(1,n)),1)           
     GPLOT,X=all_dcyc_sst[valid]*as(n),Y=all_min_precip[valid]*ap(n),COL=30+n,SYM=syms(n),/NOLINES,SIZE=70
     GPLOT,X=0.8,Y=1.5,TEXT='Correlation = '+$
           STRMID(STRTRIM(STRING(CORRELATE(all_dcyc_sst[valid],all_min_precip[valid])),1),0,5)
     IF n eq 0 THEN BEGIN
        AXES,XSTEP=0.1,XMINOR=0.05,XTITLE='Max-min 3-hr SST (K)',$
             YTITLE='Min 3-hr rainfall (mm)',NDECS=2,YSTEP=0.2,YMINOR=0.1,/NOUPPER,/NORIGHT
     ENDIF ELSE $
        AXES,XSTEP=0.1,XMINOR=0.05,YTITLE='Min 3-hr rainfall (mm)',NDECS=2,YSTEP=0.2,YMINOR=0.1,/NOUPPER,/NORIGHT
  ENDFOR
  PSCLOSE,/NOVIEW

  psfile='/home/ss901165/idl/hadgem3-kpp_runs/diurnal_cycle/hadgem3kpp_dcyc_sst_precip_byphase.maxsst_scatter_allphases_WPac.ps'
  PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,TCHARSIZE=150,MARGIN=2500,XOFFSET=200,SPACE2=250,SPACE3=250,$
         YPLOTS=4,YSIZE=5600,/PORTRAIT,YSPACING=900
  syms=[6,6,6,6,6]
  ap=[1,1,1.15,1]
  as=[0.8,1,1,1]
  ac=[0,0,0,0]
  FOR n=0,n_phases-1 DO BEGIN
     POS,ypos=n+1
     IF n eq n_phases-1 THEN BEGIN
        GSET,XMIN=0,XMAX=0.5,YMIN=0,YMAX=3,TITLE='Max 3-hr SST vs. amplitude of diurnal cycle in rain - Eq Ind Ocn'
     ENDIF ELSE $
        GSET,XMIN=0,XMAX=0.5,YMIN=0,YMAX=3
     valid=where((rmm_amp ge 1.0 and rmm_phase eq phases(0,n)) or $
                 (rmm_amp ge 1.0 and rmm_phase eq phases(1,n)))
     color=FSC_COLOR(colors(n),30+n)
     GPLOT,X=0.4,Y=2.7,TEXT='Phases '+STRTRIM(STRING(phases(0,n)),1)+'+'+STRTRIM(STRING(phases(1,n)),1)           
     GPLOT,X=all_max_sst[valid]-all_mean_sst[valid],Y=all_dcyc_precip[valid]*ap(n),COL=30+n,SYM=syms(n),/NOLINES,SIZE=70
     GPLOT,X=0.4,Y=2.5,TEXT='Correlation = '+$
           STRMID(STRTRIM(STRING(CORRELATE(all_max_sst[valid]-all_mean_sst[valid],all_dcyc_precip[valid])),1),0,5)
     IF n eq 0 THEN BEGIN
        AXES,XSTEP=0.05,XMINOR=0.025,XTITLE='Max 3-hr SST (deviation from daily mean, !Uo!NC)',$
             YTITLE='Max-min 3-hr rainfall (mm)',NDECS=2,YSTEP=0.3,YMINOR=0.15,/NOUPPER,/NORIGHT
     ENDIF ELSE $
        AXES,XSTEP=0.05,XMINOR=0.025,YTITLE='Max-min 3-hr rainfall (mm)',NDECS=2,YSTEP=0.3,YMINOR=0.15,/NOUPPER,/NORIGHT
  ENDFOR
  PSCLOSE,/NOVIEW

  psfile='/home/ss901165/idl/hadgem3-kpp_runs/diurnal_cycle/hadgem3kpp_dcyc_sst_precip_byphase.minsst_scatter_allphases_WPac.ps'
  PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,TCHARSIZE=150,MARGIN=2500,XOFFSET=200,SPACE2=250,SPACE3=250,$
         YPLOTS=4,YSIZE=5600,/PORTRAIT,YSPACING=900
  syms=[6,6,6,6,6]
  ap=[1,1,1.15,1]
  as=[0.8,1,1,1]
  ac=[0,0,0,0]
  FOR n=0,n_phases-1 DO BEGIN
     POS,ypos=n+1
     IF n eq n_phases-1 THEN BEGIN
        GSET,XMIN=-0.5,XMAX=0.0,YMIN=0,YMAX=3,TITLE='Min 3-hr SST vs. amplitude of diurnal cycle in rain - Eq Ind Ocn'
     ENDIF ELSE $
        GSET,XMIN=-0.5,XMAX=0.0,YMIN=0,YMAX=3
     valid=where((rmm_amp ge 1.0 and rmm_phase eq phases(0,n)) or $
                 (rmm_amp ge 1.0 and rmm_phase eq phases(1,n)))
     color=FSC_COLOR(colors(n),30+n)
     GPLOT,X=-0.4,Y=2.7,TEXT='Phases '+STRTRIM(STRING(phases(0,n)),1)+'+'+STRTRIM(STRING(phases(1,n)),1)           
     GPLOT,X=all_min_sst[valid]-all_mean_sst[valid],Y=all_dcyc_precip[valid]*ap(n),COL=30+n,SYM=syms(n),/NOLINES,SIZE=70
     GPLOT,X=-0.4,Y=2.5,TEXT='Correlation = '+$
           STRMID(STRTRIM(STRING(CORRELATE(all_min_sst[valid]-all_mean_sst[valid],all_dcyc_precip[valid])),1),0,5)
     IF n eq 0 THEN BEGIN
        AXES,XSTEP=0.05,XMINOR=0.015,XTITLE='Min 3-hr SST (deviation from daily mean, !Uo!NC)',$
             YTITLE='Max-min 3-hr rainfall (mm)',NDECS=2,YSTEP=0.3,YMINOR=0.15,/NOUPPER,/NOLEFT
         AXES,XSTEP=0.05,XMINOR=0.015,$
              YTITLE='Max-min 3-hr rainfall (mm)',NDECS=2,YSTEP=0.3,YMINOR=0.15,/ONLYLEFT
      ENDIF ELSE BEGIN
         AXES,XSTEP=0.05,XMINOR=0.025,YTITLE='Max-min 3-hr rainfall (mm)',NDECS=2,YSTEP=0.3,YMINOR=0.15,/NOUPPER,/NOLEFT
         AXES,XSTEP=0.05,XMINOR=0.025,YTITLE='Max-min 3-hr rainfall (mm)',NDECS=2,YSTEP=0.3,YMINOR=0.15,/ONLYRIGHT
      ENDELSE
  ENDFOR
  PSCLOSE,/NOVIEW

  psfile='/home/ss901165/idl/hadgem3-kpp_runs/diurnal_cycle/hadgem3kpp_dcyc_sst_precip_byphase.dcyc_mean_precip_scatter_allphases_WPac.ps'
  PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,TCHARSIZE=150,MARGIN=2500,XOFFSET=200,SPACE2=250,SPACE3=250,$
         YPLOTS=4,YSIZE=5600,/PORTRAIT,YSPACING=900
  syms=[6,6,6,6,6]
  ap=[1,1,1.15,1]
  as=[0.8,1,1,1]
  ac=[0,0,0,0]
  FOR n=0,n_phases-1 DO BEGIN
     POS,ypos=n+1
     IF n eq n_phases-1 THEN BEGIN
        GSET,XMIN=0,XMAX=24,YMIN=0,YMAX=0.3,TITLE='Daily precip vs. amplitude of diurnal cycle in precip'
     ENDIF ELSE $
        GSET,XMIN=0,XMAX=24,YMIN=0,YMAX=0.3
     valid=where((rmm_amp ge 1.0 and rmm_phase eq phases(0,n)) or $
                 (rmm_amp ge 1.0 and rmm_phase eq phases(1,n)))
     color=FSC_COLOR(colors(n),30+n)
     GPLOT,X=20,Y=0.27,TEXT='Phases '+STRTRIM(STRING(phases(0,n)),1)+'+'+STRTRIM(STRING(phases(1,n)),1)           
     GPLOT,X=all_mean_precip[valid]*8,Y=all_dcyc_precip[valid]/(all_mean_precip[valid]*8),COL=30+n,SYM=syms(n),/NOLINES,SIZE=70
     GPLOT,X=20,Y=0.24,TEXT='Correlation = '+$
           STRMID(STRTRIM(STRING(CORRELATE(all_mean_precip[valid],all_dcyc_precip[valid]/(all_mean_precip[valid]*8))),1),0,5)
     IF n eq 0 THEN BEGIN
        AXES,XSTEP=2,XMINOR=1,XTITLE='Mean precipitation (mm day!U-1!N)',$
             YTITLE='(Max-min 3-hr)/mean',NDECS=2,YSTEP=0.03,YMINOR=0.015,/NOUPPER,/NORIGHT
      ENDIF ELSE $
         AXES,XSTEP=2,XMINOR=1,YTITLE='(Max-min 3-hr) / mean',NDECS=2,YSTEP=0.03,YMINOR=0.015,/NORIGHT,/NOUPPER   
     xstart=[2,4,6,8]
     ystart=0.30
     FOR p=0,N_ELEMENTS(xstart)-1 DO BEGIN
        xpts=indgen(24-xstart(p)+1)+xstart(p)
        amt=xstart(p)*ystart
        ypts=amt/xpts
        GPLOT,X=xpts,Y=ypts,STYLE=1
        GPLOT,X=25,Y=amt/25,TEXT=STRMID(STRTRIM(STRING(amt),1),0,3)+'mm'
     ENDFOR
  ENDFOR
  PSCLOSE

  psfile='/home/ss901165/idl/hadgem3-kpp_runs/diurnal_cycle/hadgem3kpp_dcyc_sst_precip_byphase.dcycsst_meanprecip_scatter_allphases_WPac.ps'
  PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,TCHARSIZE=150,MARGIN=2500,XOFFSET=200,SPACE2=250,SPACE3=250,$
         YPLOTS=4,YSIZE=5600,/PORTRAIT,YSPACING=900
  syms=[6,6,6,6,6]
  ap=[1,1,1.15,1]
  as=[0.8,1,1,1]
  ac=[0,0,0,0]
  FOR n=0,n_phases-1 DO BEGIN
     POS,ypos=n+1
     IF n eq n_phases-1 THEN BEGIN
        GSET,XMIN=0,XMAX=1,YMIN=0,YMAX=24,TITLE='Daily precip vs. amplitude of diurnal cycle in precip'
     ENDIF ELSE $
        GSET,XMIN=0,XMAX=1,YMIN=0,YMAX=24
     valid=where((rmm_amp ge 1.0 and rmm_phase eq phases(0,n)) or $
                 (rmm_amp ge 1.0 and rmm_phase eq phases(1,n)))
     color=FSC_COLOR(colors(n),30+n)
     GPLOT,X=0.8,Y=22,TEXT='Phases '+STRTRIM(STRING(phases(0,n)),1)+'+'+STRTRIM(STRING(phases(1,n)),1)           
     GPLOT,X=all_dcyc_sst[valid],Y=all_mean_precip[valid]*8,COL=30+n,SYM=syms(n),/NOLINES,SIZE=70
     GPLOT,X=0.8,Y=20,TEXT='Correlation = '+$
           STRMID(STRTRIM(STRING(CORRELATE(all_mean_precip[valid],all_dcyc_sst[valid])),1),0,5)
     IF n eq 0 THEN BEGIN
        AXES,XSTEP=0.1,XMINOR=0.05,YTITLE='Mean precipitation (mm day!U-1!N)',$
             XTITLE='Max-min 3-hr SST (K)',NDECS=2,YSTEP=3,YMINOR=1.5,/NOUPPER,/NORIGHT
      ENDIF ELSE $
         AXES,XSTEP=0.1,XMINOR=0.05,YTITLE='Mean precipitation (mm day!U-1!N)',NDECS=2,YSTEP=3,YMINOR=1.5,/NORIGHT,/NOUPPER   
  ENDFOR
  PSCLOSE
     

  STOP

END
