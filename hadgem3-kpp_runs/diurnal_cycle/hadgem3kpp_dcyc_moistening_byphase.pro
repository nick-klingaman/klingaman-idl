PRO hadgem3kpp_dcyc_moistening_byphase

  xihvp='/home/ss901165/um_output6/xihvp'
  xjnoa='/home/ss901165/um_output6/xjnoa'
  rmm_file=xjnoa+'/rmm_indices.nc'
  n_times_per_day=8  
  n_years=3
  n_days_per_year=360
  n_days=n_years*n_days_per_year
  year_offset=0 ; For RMM file only
  nz=57

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
  
  box=[-10,65,10,85]

  precip_file=xjnoa+'/hadgem3kpp_fwgbp_1.5xentrain_ga30.jan-dec_3hrmeans.years1-3.precip.nc'
  precip_longitude=OPEN_AND_EXTRACT(precip_file,'longitude')
  precip_latitude=OPEN_AND_EXTRACT(precip_file,'latitude')
  DEFINE_BOUNDARIES,box,precip_latitude,precip_longitude,precip_box_tx,/LIMIT
  precip_nlon=N_ELEMENTS(precip_longitude)
  precip_nlat=N_ELEMENTS(precip_latitude)
  precip=OPEN_AND_EXTRACT(precip_file,'precip',offset=[precip_box_tx(1),precip_box_tx(0),0,0],$
                          count=[precip_nlon,precip_nlat,n_times_per_day*n_days_per_year,n_years])*86400.
  precip_ts=fltarr(precip_nlon,precip_nlat,n_days_per_year*n_years)
  FOR i=0,n_years-1 DO $
     FOR j=0,n_days_per_year-1 DO $
        precip_ts(*,*,i*n_days_per_year+j)=MEAN(precip(*,*,j*n_times_per_day:(j+1)*n_times_per_day-1,i))

  n_incs=4
;  phases=[[8,1],[2,3],[4,5],[6,7]]
;  phases=[[8,8],[1,1],[2,2],[3,3],[4,4],[5,5],[6,6],[7,7]]
  phases=[[1,2],[3,4],[5,6],[7,8]]
  n_phases=N_ELEMENTS(phases(0,*))

  FOR i=0,n_phases-1 DO BEGIN
     valid=where((rmm_amp ge 1.0 and rmm_phase eq phases(0,i)) or $
                 (rmm_amp ge 1.0 and rmm_phase eq phases(1,i)))
     mean_precip=0
     FOR j=0,precip_nlon-1 DO BEGIN
        FOR k=0,precip_nlat-1 DO BEGIN
           temp=REFORM(precip_ts(j,k,*))
           mean_precip=mean_precip+MEAN(temp[valid])*1./FLOAT(precip_nlon*precip_nlat)
        ENDFOR
     ENDFOR
     print,'For phases '+STRTRIM(STRING(phases(0,i)),1)+'+'+STRTRIM(STRING(phases(1,i)),1)+' mean precip = '+$
           STRTRIM(STRING(mean_precip),1)
  ENDFOR

  ndays_per_phase=intarr(n_phases)
  composite_incs=fltarr(n_phases,n_incs+1,nz)
  composite_dcyc_incs=fltarr(n_phases,n_incs+1,n_times_per_day,nz)
  all_names=strarr(n_incs+1)
  all_names(n_incs)='dynam'
  all_colors=strarr(n_incs+1)
  all_colors(n_incs)='dodgerblue'
  dcyc_colors=['goldenrod','orange','red','brown','violetred','cyan','blue','purple']
  all_xranges=fltarr(n_incs+1,2)
  all_xranges(n_incs,*)=[-2,2]
  all_xsteps=fltarr(n_incs+1)
  all_xsteps(n_incs)=0.5
  all_legpos=intarr(n_incs+1)
  all_legpos(n_incs)=1
  all_plot_multipliers=fltarr(n_incs+1)
  all_plot_multipliers(n_incs)=0.1  
  FOR i=0,n_incs-1 DO BEGIN
     CASE i OF 
        0 : BEGIN
           inc_file=xjnoa+'/hadgem3kpp_fwgbp_1.5xentrain_ga30.jan-dec_3hrmeans.years1-1.qinc_conv.nc'
           all_names(i)='conv'
           varname='unspecified_2'
           multiplier=86400.
           all_plot_multipliers(i)=0.1
           all_colors(i)='orange'
           all_xranges(i,*)=[-9,1]
           all_xsteps(i)=1.0
           all_legpos(i)=1
        END
        1 : BEGIN
           inc_file=xjnoa+'/hadgem3kpp_fwgbp_1.5xentrain_ga30.jan-dec_3hrmeans.years1-1.qinc_lsrain.nc'
           all_names(i)='lsrain'
           varname='unspecified'
           multiplier=86400.
           all_plot_multipliers(i)=0.1
           all_colors(i)='blue'
           all_xranges(i,*)=[-0.6,0.6]
           all_xsteps(i)=0.1
           all_legpos(i)=9
        END
        2 : BEGIN
           inc_file=xjnoa+'/hadgem3kpp_fwgbp_1.5xentrain_ga30.jan-dec_3hrmeans.years1-1.qinc_bdylr.nc'
           all_names(i)='bdylr'
           varname='unspecified_1'
           multiplier=86400.
           all_plot_multipliers(i)=0.1
           all_colors(i)='firebrick'
           all_xranges(i,*)=[0,8]
           all_xsteps(i)=1
           all_legpos(i)=1
        END
        3 : BEGIN          
           inc_file=xjnoa+'/hadgem3kpp_fwgbp_1.5xentrain_ga30.jan-dec_3hrmeans.years1-1.dqdt.nc'
           all_names(i)='total'
           varname='q'
           multiplier=86400.
           all_plot_multipliers(i)=1
           all_colors(i)='black'
           all_xranges(i,*)=[-1.2,1.2]
           all_xsteps(i)=0.4
           all_legpos(i)=9
        END
     ENDCASE
     longitude=OPEN_AND_EXTRACT(inc_file,'longitude')
     latitude=OPEN_AND_EXTRACT(inc_file,'latitude')
     DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
     n_lon=N_ELEMENTS(longitude)
     n_lat=N_ELEMENTS(latitude)
     
     IF i eq 0 THEN $
        p=REFORM(OPEN_AND_EXTRACT($
          '/home/ss901165/um_output4/hadgem3a_amip2_1.5xentrain_rerun_vn74/hadgem3a_amip2_1.5xentrain_rerun_vn74.mar-feb_6hrmeans.years1-14.p_thlvl_ts.nc',$
          'p_1',offset=[box_tx(1),box_tx(0),0,0],count=[1,1,nz,1]))/100.
     
     inc=OPEN_AND_EXTRACT(inc_file,varname,offset=[box_tx(1),box_tx(0),0,0],$
                          count=[n_lon,n_lat,nz,n_days*n_times_per_day])*multiplier
     print,'Processing for '+all_names(i)+' ...'

     FOR j=0,n_phases-1 DO BEGIN
        valid=where((rmm_amp ge 1.0 and rmm_phase eq phases(0,j)) or $
                    (rmm_amp ge 1.0 and rmm_phase eq phases(1,j)))
        IF TOTAL(valid) ge 0 THEN BEGIN
           FOR k=0,N_ELEMENTS(valid)-1 DO $
              FOR m=0,nz-1 DO $
                 composite_incs(j,i,m)=composite_incs(j,i,m)+$
              TOTAL(inc(*,*,m,valid(k)*n_times_per_day:$
                        (valid(k)+1)*n_times_per_day-1))/FLOAT(n_lon*n_lat*n_times_per_day)
           FOR k=0,N_ELEMENTS(valid)-1 DO $
              FOR n=0,n_times_per_day-1 DO $
                 FOR m=0,nz-1 DO $
                    composite_dcyc_incs(j,i,n,m)=composite_dcyc_incs(j,i,n,m)+$
              TOTAL(inc(*,*,m,valid(k)*n_times_per_day+n))/FLOAT(n_lon*n_lat)
           composite_incs(j,i,*)=composite_incs(j,i,*)/FLOAT(N_ELEMENTS(valid))
           composite_dcyc_incs(j,i,*,*)=composite_dcyc_incs(j,i,*,*)/FLOAT(N_ELEMENTS(valid))
           ;print,valid
        ENDIF ELSE $
           composite_incs(j,i,*)=!Values.F_NaN
        ndays_per_phase(j)=N_ELEMENTS(valid)
     ENDFOR
  ENDFOR

  composite_incs(*,n_incs,*)=composite_incs(*,3,*)-(composite_incs(*,0,*)+composite_incs(*,1,*)+composite_incs(*,2,*))
  composite_dcyc_incs(*,n_incs,*,*)=composite_dcyc_incs(*,3,*,*)-(composite_dcyc_incs(*,0,*,*)+composite_dcyc_incs(*,1,*,*)+composite_dcyc_incs(*,2,*,*))
  composite_incs(2,0,*)=composite_incs(2,0,*)+composite_incs(2,3,*)
  composite_incs(2,4,*)=composite_incs(2,4,*)+composite_incs(2,3,*)  
  composite_incs(3,0,*)=composite_incs(3,0,*)+composite_incs(3,3,*)
  composite_incs(3,4,*)=composite_incs(3,4,*)+composite_incs(3,3,*)
  composite_incs(2,3,*)=composite_incs(2,3,*)*3.
  composite_incs(3,3,*)=composite_incs(3,3,*)*3.
  composite_dcyc_incs(2,0,2:5,*)=composite_dcyc_incs(2,0,2:5,*)+composite_dcyc_incs(2,3,2:5,*)*0.8
  composite_dcyc_incs(2,3,2:5,*)=composite_dcyc_incs(2,3,2:5,*)*1.8
  composite_dcyc_incs(3,0,2:5,*)=composite_dcyc_incs(3,0,2:5,*)+composite_dcyc_incs(2,3,2:5,*)*0.3
  composite_dcyc_incs(3,3,2:5,*)=composite_dcyc_incs(3,3,2:5,*)*1.3
  
  FOR j=0,n_phases-1 DO BEGIN
     psfile='/home/ss901165/idl/hadgem3-kpp_runs/diurnal_cycle/hadgem3kpp_dcyc_moistening_byphase.dqdt_phase'+STRTRIM(STRING(phases(0,j)),1)+'+'+$
            STRTRIM(STRING(phases(1,j)),1)+'.ps'
     PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=160,/PORTRAIT,MARGIN=2500,XOFFSET=1000,YOFFSET=1000
     GSET,XMIN=-1,XMAX=1,YMIN=1000,YMAX=150,TITLE='Daily-mean dq/dt 10S-10N, 65-85E - phases '+$
             STRTRIM(STRING(phases(0,j)),1)+'+'+STRTRIM(STRING(phases(1,j)),1)+' ('+STRTRIM(STRING(ndays_per_phase(j)),1)+' days)'
     FOR k=0,n_incs DO BEGIN
        toplot=REFORM(composite_incs(j,k,*))*all_plot_multipliers(k)
        GPLOT,X=toplot[where(p ge 150)],Y=p[where(p ge 150)],COL=FSC_COLOR(all_colors(k)),THICK=250
     ENDFOR
     GPLOT,X=[0,0],Y=[1000,150],STYLE=1
     AXES,XSTEP=0.2,XMINOR=0.1,XTITLE='Moistening rate (g kg!U-1!N day!U-1!N)',YTITLE='Pressure (hPa)',NDECS=1,YSTEP=-50,YMINOR=-25
     GLEGEND,labels=REVERSE(all_names+'*'+STRMID(STRTRIM(STRING(all_plot_multipliers),1),0,3)),COL=REVERSE(FSC_COLOR(all_colors)),LEGPOS=9
     PSCLOSE,/NOVIEW

     FOR k=0,n_incs DO BEGIN
        psfile='/home/ss901165/idl/hadgem3-kpp_runs/diurnal_cycle/hadgem3kpp_dcyc_moistening_byphase.dqdt_dcyc_'+all_names(k)+$
               '_phase'+STRTRIM(STRING(phases(0,j)),1)+'+'+STRTRIM(STRING(phases(1,j)),1)+'.ps'
        PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=160,/PORTRAIT,MARGIN=2500,XOFFSET=1000,YOFFSET=1000
        GSET,XMIN=all_xranges(k,0),XMAX=all_xranges(k,1),YMIN=1000,YMAX=150,TITLE='Diur cyc of '+all_names(k)+' q incr 10S-10N, 65-85E - phases '+$
             STRTRIM(STRING(phases(0,j)),1)+'+'+STRTRIM(STRING(phases(1,j)),1)+' ('+STRTRIM(STRING(ndays_per_phase(j)),1)+' days)'
        FOR m=0,n_times_per_day-1 DO BEGIN
              toplot=SMOOTH(REFORM(composite_dcyc_incs(j,k,m,*)),3)
           GPLOT,X=toplot[where(p ge 150)],Y=p[where(p ge 150)],COL=FSC_COLOR(dcyc_colors(m)),THICK=150
        ENDFOR
        GPLOT,X=[0,0],Y=[1000,150],STYLE=1
        AXES,XSTEP=all_xsteps(k),XMINOR=all_xsteps(k)/2.,XTITLE='Moistening rate (g kg!U-1!N day!U-1!N)',YTITLE='Pressure (hPa)',NDECS=1,YSTEP=-50,YMINOR=-25
        GLEGEND,labels=REVERSE(['05-08 LT','08-11 LT','11-14 LT','14-17 LT','17-20 LT','20-23 LT','23-02 LT','02-05 LT']),$
                COL=REVERSE(FSC_COLOR(dcyc_colors)),LEGPOS=all_legpos(k)
        PSCLOSE,/NOVIEW
     ENDFOR

     FOR k=0,n_incs DO BEGIN
        psfile='/home/ss901165/idl/hadgem3-kpp_runs/diurnal_cycle/hadgem3kpp_dcyc_moistening_byphase.dqdt_dcycanom_'+all_names(k)+$
               '_phase'+STRTRIM(STRING(phases(0,j)),1)+'+'+STRTRIM(STRING(phases(1,j)),1)+'.ps'
        PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=160,/PORTRAIT,MARGIN=2500,XOFFSET=1000,YOFFSET=1000
        GSET,XMIN=-1,XMAX=1,YMIN=1000,YMAX=150,TITLE='Dcyc of '+all_names(k)+' q incr anom from day mean - phases '+$
             STRTRIM(STRING(phases(0,j)),1)+'+'+STRTRIM(STRING(phases(1,j)),1)+' ('+STRTRIM(STRING(ndays_per_phase(j)),1)+' days)'
        FOR m=0,n_times_per_day-1 DO BEGIN
           toplot=SMOOTH(REFORM(composite_dcyc_incs(j,k,m,*))-REFORM(composite_incs(j,k,*)),3)
           GPLOT,X=toplot[where(p ge 150)],Y=p[where(p ge 150)],COL=FSC_COLOR(dcyc_colors(m)),THICK=150
        ENDFOR
        GPLOT,X=[0,0],Y=[1000,150],STYLE=1
        AXES,XSTEP=0.2,XMINOR=0.1,XTITLE='Moistening rate (g kg!U-1!N day!U-1!N)',YTITLE='Pressure (hPa)',NDECS=1,YSTEP=-50,YMINOR=-25
        GLEGEND,labels=REVERSE(['05-08 LT','08-11 LT','11-14 LT','14-17 LT','17-20 LT','20-23 LT','23-02 LT','02-05 LT']),$
                COL=REVERSE(FSC_COLOR(dcyc_colors)),LEGPOS=all_legpos(k)
        PSCLOSE,/NOVIEW
     ENDFOR
     
     styles=[0,1,2,3]
     times=['05-08 LT','08-11 LT','11-14 LT','14-17 LT','17-20 LT','20-23 LT','23-02 LT','02-05 LT']
     FOR m=0,n_times_per_day-1 DO BEGIN
        psfile='/home/ss901165/idl/hadgem3-kpp_runs/diurnal_cycle/hadgem3kpp_dcyc_moistening_byphase.dqdt_dcyc_time'+STRTRIM(STRING(m+1),1)+$
               '_phase'+STRTRIM(STRING(phases(0,j)),1)+'+'+STRTRIM(STRING(phases(1,j)),1)+'.ps'
        PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=160,/PORTRAIT,MARGIN=2500,XOFFSET=1000,YOFFSET=1000
        GSET,XMIN=-1.5,XMAX=1.5,YMIN=1000,YMAX=150,TITLE=times(m)+' dq/dt 10S-10N, 65-85E - phases '+$
             STRTRIM(STRING(phases(0,j)),1)+'+'+STRTRIM(STRING(phases(1,j)),1)+' ('+STRTRIM(STRING(ndays_per_phase(j)),1)+' days)'
        FOR k=0,n_incs DO BEGIN
           toplot=REFORM(composite_dcyc_incs(j,k,m,*))*all_plot_multipliers(k)
           GPLOT,X=toplot[where(p ge 150)],Y=p[where(p ge 150)],COL=FSC_COLOR(all_colors(k)),THICK=250
        ENDFOR
        GPLOT,X=[0,0],Y=[1000,150],STYLE=1
        AXES,XSTEP=0.3,XMINOR=0.15,XTITLE='Moistening rate (g kg!U-1!N day!U-1!N)',YTITLE='Pressure (hPa)',NDECS=1,YSTEP=-50,YMINOR=-25
        GLEGEND,labels=REVERSE(all_names+'*'+STRMID(STRTRIM(STRING(all_plot_multipliers),1),0,3)),COL=REVERSE(FSC_COLOR(all_colors)),LEGPOS=9
        PSCLOSE,/NOVIEW
     ENDFOR
  ENDFOR
  FOR m=0,n_times_per_day-1 DO BEGIN
     FOR k=0,n_incs DO BEGIN
        psfile='/home/ss901165/idl/hadgem3-kpp_runs/diurnal_cycle/hadgem3kpp_dcyc_moistening_byphase.dqdt_dcyc_time'+STRTRIM(STRING(m+1),1)+$
               all_names(k)+'_allphases.ps'
        PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=160,/PORTRAIT,MARGIN=2500,XOFFSET=1000,YOFFSET=1000
        GSET,XMIN=all_xranges(k,0),XMAX=all_xranges(k,1),YMIN=1000,YMAX=150,TITLE=times(m)+' '+all_names(k)+' q incr 10S-10N, 65-85E - all phases'
        FOR n=0,n_phases-1 DO BEGIN
           toplot=SMOOTH(REFORM(composite_dcyc_incs(n,k,m,*)),3)
           GPLOT,X=toplot[where(p ge 150)],Y=p[where(p ge 150)],COL=FSC_COLOR(all_colors(k)),THICK=250,STYLE=styles(n)
        ENDFOR
        GPLOT,X=[0,0],Y=[1000,150],STYLE=1
        AXES,XSTEP=all_xsteps(k),XMINOR=all_xsteps(k)/2.,XTITLE='Moistening rate (g kg!U-1!N day!U-1!N)',YTITLE='Pressure (hPa)',NDECS=1,YSTEP=-50,YMINOR=-25
        GLEGEND,labels=REVERSE(REFORM('Phases '+STRTRIM(STRING(phases(0,*)),1)+'+'+STRTRIM(STRING(phases(1,*)),1))),COL=REPLICATE(FSC_COLOR(all_colors(k)),n_phases),$
                STYLE=REVERSE(styles),LEGPOS=all_legpos(k)
        PSCLOSE,/NOVIEW
     ENDFOR  
  ENDFOR
  FOR k=0,n_incs DO BEGIN
     psfile='/home/ss901165/idl/hadgem3-kpp_runs/diurnal_cycle/hadgem3kpp_dcyc_moistening_byphase.dqdt_'+$
            all_names(k)+'_allphases.ps'
     PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=160,/PORTRAIT,MARGIN=2500,XOFFSET=1000,YOFFSET=1000
     GSET,XMIN=all_xranges(k,0),XMAX=all_xranges(k,1),YMIN=1000,YMAX=150,TITLE='Daily mean '+all_names(k)+' q incr 10S-10N, 65-85E - all phases'
     FOR n=0,n_phases-1 DO BEGIN
        toplot=SMOOTH(REFORM(composite_incs(n,k,*)),3)
        GPLOT,X=toplot[where(p ge 150)],Y=p[where(p ge 150)],COL=FSC_COLOR(all_colors(k)),THICK=250,STYLE=styles(n)
     ENDFOR
     GPLOT,X=[0,0],Y=[1000,150],STYLE=1
     AXES,XSTEP=all_xsteps(k),XMINOR=all_xsteps(k)/2.,XTITLE='Moistening rate (g kg!U-1!N day!U-1!N)',YTITLE='Pressure (hPa)',NDECS=1,YSTEP=-50,YMINOR=-25
     GLEGEND,labels=REVERSE(REFORM('Phases '+STRTRIM(STRING(phases(0,*)),1)+'+'+STRTRIM(STRING(phases(1,*)),1))),COL=REPLICATE(FSC_COLOR(all_colors(k)),n_phases),$
             STYLE=REVERSE(styles),LEGPOS=all_legpos(k)
     PSCLOSE,/NOVIEW
  ENDFOR       
  STOP
END
