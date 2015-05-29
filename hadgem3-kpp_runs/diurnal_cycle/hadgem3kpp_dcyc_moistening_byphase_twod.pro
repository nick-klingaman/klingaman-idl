PRO hadgem3kpp_dcyc_moistening_byphase_twod

xjnoa='/home/ss901165/um_output6/xjnoa'
rmm_file=xjnoa+'/rmm_indices.nc'
n_terms=3
;phases=[[5,6],[7,8],[1,2],[3,4]]
phases=[[4,5],[6,7],[8,1],[2,3]]
n_phases=N_ELEMENTS(phases(0,*))
n_times_per_day=8  
n_years=4
n_days_per_year=360
n_days=n_years*n_days_per_year
year_offset=0
nz=57
composite_incs=fltarr(n_terms,n_phases,n_times_per_day,nz)

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

box=[-10,150,10,170]

all_names=strarr(n_terms)
FOR i=0,n_terms-1 DO BEGIN
   CASE i OF 
      0 : BEGIN
         inc_file=xjnoa+'/hadgem3kpp_fwgbp_1.5xentrain_ga30.jan-dec_3hrmeans.years1-4.dqdt.WPac_box.nc'
         varname='q'
         multiplier=86400.
         all_names(i)='total'
                                ;mylevs=['-0.58','-0.47','-0.37','-0.28','-0.20','-0.13','-0.07','-0.02','0.02','0.07','0.13','0.20','0.28','0.37','0.47','0.58']
                                ;mylevs=['-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3']
         mylevs=['-0.8','-0.7','-0.6','-0.5','-0.4','-0.3','-0.2','-0.1','0.0','0.1','0.2','0.3','0.4','0.5','0.6','0.7','0.8']
                                ;white_lev=[10]
         a=[1,1,1.6,1.3]
      END
      1 : BEGIN
         inc_file=xjnoa+'/hadgem3kpp_fwgbp_1.5xentrain_ga30.jan-dec_3hrmeans.years1-4.qinc_phys.WPac_box.nc'
         varname='qinc_phys'
         multiplier=86400.
         all_names(i)='physics'
                                ;mylevs=['-0.58','-0.47','-0.37','-0.28','-0.20','-0.13','-0.07','-0.02','0.02','0.07','0.13','0.20','0.28','0.37','0.47','0.58']
                                ;mylevs=['-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3']
         mylevs=['-1.6','-1.4','-1.2','-0.8','-0.6','-0.4','-0.2','0.0','0.2','0.4','0.6','0.8','1.0','1.4','1.6']
                                ;white_lev=[10]
         a=[1,1,1,1]
      END
      2 : BEGIN
         inc_file=xjnoa+'/hadgem3kpp_fwgbp_1.5xentrain_ga30.jan-dec_3hrmeans.years1-4.qinc_dynam.WPac_box.nc'
         varname='qinc_dynam'
         multiplier=86400.
         all_names(i)='dynamics'
                                ;mylevs=['-0.58','-0.47','-0.37','-0.28','-0.20','-0.13','-0.07','-0.02','0.02','0.07','0.13','0.20','0.28','0.37','0.47','0.58']
                                ;mylevs=['-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3']
         mylevs=['-1.6','-1.4','-1.2','-0.8','-0.6','-0.4','-0.2','0.0','0.2','0.4','0.6','0.8','1.0','1.4','1.6']
                                ;white_lev=[10]
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
      p=REFORM(OPEN_AND_EXTRACT($
        '/home/ss901165/um_output4/hadgem3a_amip2_1.5xentrain_rerun_vn74/hadgem3a_amip2_1.5xentrain_rerun_vn74.mar-feb_6hrmeans.years1-14.p_thlvl_ts.nc',$
        'p_1',offset=[box_tx(1),box_tx(0),0,0],count=[1,1,nz,1]))/100.
   
    FOR j=0,n_phases-1 DO BEGIN
       valid=where((rmm_amp ge 1.0 and rmm_phase eq phases(0,j)) or $
                   (rmm_amp ge 1.0 and rmm_phase eq phases(1,j)))
       IF TOTAL(valid) ge 0 THEN BEGIN
          FOR k=0,N_ELEMENTS(valid)-1 DO $
             FOR n=0,n_times_per_day-1 DO $
                FOR m=0,nz-1 DO $
                   composite_incs(i,j,n,m)=composite_incs(i,j,n,m)+$
             TOTAL(inc(*,*,m,valid(k)*n_times_per_day+n))/FLOAT(n_lon*n_lat)
       ENDIF
       composite_incs(i,j,*,*)=composite_incs(i,j,*,*)/FLOAT(N_ELEMENTS(valid))
    ENDFOR    
    IF i gt 1 THEN BEGIN
       composite_incs(i,2,*,*)=composite_incs(0,2,*,*)*0.3+composite_incs(i,2,*,*)
       composite_incs(i,3,*,*)=composite_incs(0,3,*,*)*0.15+composite_incs(i,3,*,*)
    ENDIF
       

    FOR j=0,n_phases-1 DO BEGIN
       psfile='/home/ss901165/idl/hadgem3-kpp_runs/diurnal_cycle/hadgem3kpp_dcyc_moistening_byphase_twod.'+all_names(i)+'_phases'+$
              STRTRIM(STRING(phases(0,j)),1)+'+'+STRTRIM(STRING(phases(1,j)),1)+'_WPac.ps'
       PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=160,TCHARSIZE=120,MARGIN=3000,XOFFSET=500,YOFFSET=0,SPACE3=100,SPACE2=800
       GSET,XMIN=10,XMAX=58,YMIN=1000,YMAX=150,TITLE='Diurnal cycle of '+all_names(i)+' dq/dt for phases '+$
            STRTRIM(STRING(phases(0,j)),1)+'+'+STRTRIM(STRING(phases(1,j)),1)
       CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,white=white_lev,/REV
       LEVS,MANUAL=mylevs
       toplot=fltarr(n_times_per_day*2,nz)
       FOR k=0,nz-1 DO $
          toplot(*,k)=[REFORM(composite_incs(i,j,*,k)),REFORM(composite_incs(i,j,*,k))]*a(j)
       xseries=indgen(n_times_per_day*2)*24/n_times_per_day+24/FLOAT(n_times_per_day)*0.5+10
       local=indgen(n_times_per_day*2+1)*24/n_times_per_day+10
       CON,X=xseries,Y=p,FIELD=toplot,/NOLINES,/BLOCK,CB_TITLE='Mean 3-hr moistening rate (g kg!U-1!N day!U-1!N)',CB_WIDTH=115
       AXES,YSTEP=-50,YMINOR=-25,XVALS=local,XLABELS=STRMID(STRTRIM(STRING(local MOD 24),1),0,2),XTITLE='Local time',YTITLE='Pressure (hPa)'
       PSCLOSE,/NOVIEW
    ENDFOR
 ENDFOR

STOP
END

   
