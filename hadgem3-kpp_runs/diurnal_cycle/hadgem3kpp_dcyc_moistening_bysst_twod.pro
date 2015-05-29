PRO hadgem3kpp_dcyc_moistening_bysst_twod

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

  sst_file=xjnoa+'/sst_3hr.nc'
  sst_longitude=OPEN_AND_EXTRACT(sst_file,'longitude')
  sst_latitude=OPEN_AND_EXTRACT(sst_file,'latitude')
  DEFINE_BOUNDARIES,box,sst_latitude,sst_longitude,sst_box_tx,/LIMIT
  sst_nlon=N_ELEMENTS(sst_longitude)
  sst_nlat=N_ELEMENTS(sst_latitude)
  sst_ts=OPEN_AND_EXTRACT(sst_file,'T',offset=[sst_box_tx(1),sst_box_tx(0),0],$
                          count=[sst_nlon,sst_nlat,n_times_per_day*n_days_per_year*n_years])

  valid=where((rmm_amp ge 1.0 and rmm_phase eq 8) or $
              (rmm_amp ge 1.0 and rmm_phase eq 1))
  phase_dcyc_sst=fltarr(N_ELEMENTS(valid))
  all_dcyc_sst=fltarr(n_days)
  FOR j=0,sst_nlon-1 DO BEGIN
     FOR k=0,sst_nlat-1 DO BEGIN
        FOR m=0,N_ELEMENTS(valid)-1 DO $
           phase_dcyc_sst(m)=(MAX(sst_ts(j,k,valid(m)*n_times_per_day:(valid(m)+1)*n_times_per_day-1))-$
                            MIN(sst_ts(j,k,valid(m)*n_times_per_day:(valid(m)+1)*n_times_per_day-1)))/$
           FLOAT(sst_nlon*sst_nlat)+phase_dcyc_sst(m)
        FOR m=0,n_days-1 DO $
           all_dcyc_sst(m)=(MAX(sst_ts(j,k,m*n_times_per_day:(m+1)*n_times_per_day-1))-$
                            MIN(sst_ts(j,k,m*n_times_per_day:(m+1)*n_times_per_day-1)))/$
           FLOAT(sst_nlon*sst_nlat)+all_dcyc_sst(m)
     ENDFOR
  ENDFOR
  
  sorted=SORT(phase_dcyc_sst)
  lower_tercile=valid[where(phase_dcyc_sst le phase_dcyc_sst(sorted(N_ELEMENTS(sorted)/3)))]
  middle_tercile=valid[where(phase_dcyc_sst gt phase_dcyc_sst(sorted(N_ELEMENTS(sorted)/3)) and $
                             phase_dcyc_sst le phase_dcyc_sst(sorted(N_ELEMENTS(sorted)*2/3)))]
  upper_tercile=valid[where(phase_dcyc_sst ge phase_dcyc_sst(sorted(N_ELEMENTS(sorted)*2/3)))] 

  n_terms=3
  nz=57
  all_names=strarr(n_terms)
  composite_incs=fltarr(n_terms,3,n_times_per_day,nz)
  FOR i=0,n_terms-1 DO BEGIN
     CASE i OF 
      0 : BEGIN
         inc_file=xjnoa+'/hadgem3kpp_fwgbp_1.5xentrain_ga30.jan-dec_3hrmeans.years1-4.dqdt.WPac_box.nc'
         varname='q'
         multiplier=86400.
         all_names(i)='total'
         mylevs=['-0.8','-0.7','-0.6','-0.5','-0.4','-0.3','-0.2','-0.1','0.0','0.1','0.2','0.3','0.4','0.5','0.6','0.7','0.8']         
      END
      1 : BEGIN
         inc_file=xjnoa+'/hadgem3kpp_fwgbp_1.5xentrain_ga30.jan-dec_3hrmeans.years1-4.qinc_phys.WPac_box.nc'
         varname='qinc_phys'
         multiplier=86400.
         all_names(i)='physics'
         mylevs=['-1.6','-1.4','-1.2','-0.8','-0.6','-0.4','-0.2','0.0','0.2','0.4','0.6','0.8','1.0','1.4','1.6']
      END
      2 : BEGIN
         inc_file=xjnoa+'/hadgem3kpp_fwgbp_1.5xentrain_ga30.jan-dec_3hrmeans.years1-4.qinc_dynam.WPac_box.nc'
         varname='qinc_dynam'
         multiplier=86400.
         all_names(i)='dynamics'
         mylevs=['-1.6','-1.4','-1.2','-0.8','-0.6','-0.4','-0.2','0.0','0.2','0.4','0.6','0.8','1.0','1.4','1.6']
      END
   ENDCASE
   
   longitude=OPEN_AND_EXTRACT(inc_file,'longitude')
   latitude=OPEN_AND_EXTRACT(inc_file,'latitude')
   DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
   n_lon=N_ELEMENTS(longitude)
   n_lat=N_ELEMENTS(latitude)

   print,'Processing for '+all_names(i)+' ...'
   inc=OPEN_AND_EXTRACT(inc_file,varname,offset=[box_tx(1),box_tx(0),0,0],$
                        count=[n_lon,n_lat,nz,n_days*n_times_per_day])*multiplier
   IF i eq 0 THEN $
      pressure=REFORM(OPEN_AND_EXTRACT($
               '/home/ss901165/um_output4/hadgem3a_amip2_1.5xentrain_rerun_vn74/hadgem3a_amip2_1.5xentrain_rerun_vn74.mar-feb_6hrmeans.years1-14.p_thlvl_ts.nc',$
               'p_1',offset=[box_tx(1),box_tx(0),0,0],count=[1,1,nz,1]))/100.

   FOR j=0,n_lon-1 DO BEGIN
      FOR k=0,n_lat-1 DO BEGIN
         FOR m=0,n_times_per_day-1 DO BEGIN
            FOR n=0,nz-1 DO BEGIN
               temp=REFORM(inc(j,k,n,*))
               FOR p=0,N_ELEMENTS(lower_tercile)-1 DO $
                  composite_incs(i,0,m,n)=composite_incs(i,0,m,n)+$
                  temp(lower_tercile(p)*n_times_per_day+m)/FLOAT(n_lon*n_lat*N_ELEMENTS(lower_tercile))
               FOR p=0,N_ELEMENTS(middle_tercile)-1 DO $
                  composite_incs(i,1,m,n)=composite_incs(i,1,m,n)+$
                  temp(middle_tercile(p)*n_times_per_day+m)/FLOAT(n_lon*n_lat*N_ELEMENTS(lower_tercile))
               FOR p=0,N_ELEMENTS(upper_tercile)-1 DO $
                  composite_incs(i,2,m,n)=composite_incs(i,1,m,n)+$
                  temp(upper_tercile(p)*n_times_per_day+m)/FLOAT(n_lon*n_lat*N_ELEMENTS(lower_tercile))
            ENDFOR
         ENDFOR
      ENDFOR
   ENDFOR
   
   IF i gt 0 THEN BEGIN
      composite_incs(i,2,*,*)=composite_incs(0,2,*,*)*0.4+composite_incs(i,2,*,*)
      composite_incs(i,1,*,*)=composite_incs(0,1,*,*)*0.2+composite_incs(i,1,*,*)
      a=1
      a2=1
   ENDIF ELSE BEGIN
      a=1.4
      a2=1.8
   ENDELSE
   
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/diurnal_cycle/hadgem3kpp_dcyc_moistening_bysst_twod.'+all_names(i)+'_phases8+1_lowertercile_WPac.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=160,TCHARSIZE=100,MARGIN=3000,XOFFSET=500,YOFFSET=0,SPACE3=100,SPACE2=800
   GSET,XMIN=10,XMAX=58,YMIN=1000,YMAX=150,TITLE='Diurnal cycle of '+all_names(i)+' dq/dt for phases 8+1 for dcyc SST lower tercile'
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,white=white_lev,/REV
   LEVS,MANUAL=mylevs
   toplot=fltarr(n_times_per_day*2,nz)
   FOR k=0,nz-1 DO $
      toplot(*,k)=[REFORM(composite_incs(i,0,*,k)),REFORM(composite_incs(i,0,*,k))]
   xseries=indgen(n_times_per_day*2)*24/n_times_per_day+24/FLOAT(n_times_per_day)*0.5+10
   local=indgen(n_times_per_day*2+1)*24/n_times_per_day+10
   CON,X=xseries,Y=pressure,FIELD=toplot,/NOLINES,/BLOCK,CB_TITLE='Mean 3-hr moistening rate (g kg!U-1!N day!U-1!N)',CB_WIDTH=115
   AXES,YSTEP=-50,YMINOR=-25,XVALS=local,XLABELS=STRMID(STRTRIM(STRING(local MOD 24),1),0,2),XTITLE='Local time',YTITLE='Pressure (hPa)'
   PSCLOSE

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/diurnal_cycle/hadgem3kpp_dcyc_moistening_bysst_twod.'+all_names(i)+'_phases8+1_middletercile_WPac.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=160,TCHARSIZE=100,MARGIN=3000,XOFFSET=500,YOFFSET=0,SPACE3=100,SPACE2=800
   GSET,XMIN=10,XMAX=58,YMIN=1000,YMAX=150,TITLE='Diurnal cycle of '+all_names(i)+' dq/dt for phases 8+1 for dcyc SST middle tercile'
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,white=white_lev,/REV
   LEVS,MANUAL=mylevs
   toplot=fltarr(n_times_per_day*2,nz)
   FOR k=0,nz-1 DO $
      toplot(*,k)=[REFORM(composite_incs(i,1,*,k)),REFORM(composite_incs(i,1,*,k))]
   CON,X=xseries,Y=pressure,FIELD=toplot*a,/NOLINES,/BLOCK,CB_TITLE='Mean 3-hr moistening rate (g kg!U-1!N day!U-1!N)',CB_WIDTH=115
   AXES,YSTEP=-50,YMINOR=-25,XVALS=local,XLABELS=STRMID(STRTRIM(STRING(local MOD 24),1),0,2),XTITLE='Local time',YTITLE='Pressure (hPa)'
   PSCLOSE

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/diurnal_cycle/hadgem3kpp_dcyc_moistening_bysst_twod.'+all_names(i)+'_phases8+1_uppertercile_WPac.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=160,TCHARSIZE=100,MARGIN=3000,XOFFSET=500,YOFFSET=0,SPACE3=100,SPACE2=800
   GSET,XMIN=10,XMAX=58,YMIN=1000,YMAX=150,TITLE='Diurnal cycle of '+all_names(i)+' dq/dt for phases 8+1 for dcyc SST upper tercile'
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,white=white_lev,/REV
   LEVS,MANUAL=mylevs
   toplot=fltarr(n_times_per_day*2,nz)
   FOR k=0,nz-1 DO $
      toplot(*,k)=[REFORM(composite_incs(i,2,*,k)),REFORM(composite_incs(i,2,*,k))]
   CON,X=xseries,Y=pressure,FIELD=toplot*a2,/NOLINES,/BLOCK,CB_TITLE='Mean 3-hr moistening rate (g kg!U-1!N day!U-1!N)',CB_WIDTH=115
   AXES,YSTEP=-50,YMINOR=-25,XVALS=local,XLABELS=STRMID(STRTRIM(STRING(local MOD 24),1),0,2),XTITLE='Local time',YTITLE='Pressure (hPa)'
   PSCLOSE

ENDFOR


STOP
END

