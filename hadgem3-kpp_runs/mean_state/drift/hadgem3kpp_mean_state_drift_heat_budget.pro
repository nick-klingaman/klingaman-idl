PRO hadgem3kpp_mean_state_drift_heat_budget

coupled_infile='/home/ss901165/um_output5/xgspm_ind2/KPPocean_0015-0595_dmeans.budget_vars.nc'
coupled_fcorr_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/heat_corrections/hadgem3_1.5xentrain_ensmean_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421_ind.jan-dec_mmeans.flxcorr.n96.nc'

npts=1

FOR i=0,npts-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         pt=[-10,90]
         pt_name='10S90E'
      END
   ENDCASE

   coupled_longitude=OPEN_AND_EXTRACT(coupled_infile,'longitude')
   coupled_latitude=OPEN_AND_EXTRACT(coupled_infile,'latitude')
   coupled_lon_pt=NEAREST(coupled_longitude,pt(1))
   coupled_lat_pt=NEAREST(coupled_latitude,pt(0))

   time=OPEN_AND_EXTRACT(coupled_infile,'time')
   n_time=N_ELEMENTS(time)
   
   IF i eq 0 THEN BEGIN
      dhdt_runsum=fltarr(npts,n_time)
      qflx_runsum=fltarr(npts,n_time)
      fcorr_runsum=fltarr(npts,n_time)
   ENDIF

   z_full=OPEN_AND_EXTRACT(coupled_infile,'z')
   nz=N_ELEMENTS(z_full)
   z_half=OPEN_AND_EXTRACT(coupled_infile,'d')
   dz=OPEN_AND_EXTRACT(coupled_infile,'h')
      
   ;hmix=REFORM(OPEN_AND_EXTRACT(coupled_infile,'hmix'))
      
   temperature=REFORM(OPEN_AND_EXTRACT(coupled_infile,'T',$
                                       offset=[coupled_lon_pt,coupled_lat_pt,0,0],$
                                       count=[1,1,nz,n_time]))
   wT=REFORM(OPEN_AND_EXTRACT(coupled_infile,'wT',$
                              offset=[coupled_lon_pt,coupled_lat_pt,0,0],$
                              count=[1,1,nz,n_time]))
   flux_nsolar=REFORM(OPEN_AND_EXTRACT(coupled_infile,'nsolar_in',$
                                       offset=[coupled_lon_pt,coupled_lat_pt,0],$
                                       count=[1,1,n_time]))
   flux_solar=REFORM(OPEN_AND_EXTRACT(coupled_infile,'solar_in',$
                                      offset=[coupled_lon_pt,coupled_lat_pt,0],$
                                      count=[1,1,n_time]))
   rho=REFORM(OPEN_AND_EXTRACT(coupled_infile,'rho',$
                               offset=[coupled_lon_pt,coupled_lat_pt,0,0],$
                               count=[1,1,nz,n_time]))
   cp=REFORM(OPEN_AND_EXTRACT(coupled_infile,'cp',$
                              offset=[coupled_lon_pt,coupled_lat_pt,0,0],$
                              count=[1,1,nz,n_time]))
   
   flux_net=flux_solar+flux_nsolar
   
   fcorr_longitude=OPEN_AND_EXTRACT(coupled_fcorr_infile,'longitude')
   fcorr_latitude=OPEN_AND_EXTRACT(coupled_fcorr_infile,'latitude')
   fcorr_lonpt=NEAREST(fcorr_longitude,pt(1))
   fcorr_latpt=NEAREST(fcorr_latitude,pt(0))
   
   fcorr_mmeans=REFORM(OPEN_AND_EXTRACT(coupled_fcorr_infile,'fcorr',$
                                        offset=[fcorr_lonpt,fcorr_latpt,0,0],$
                                        count=[1,1,nz,12]))
   vint_t1=fltarr(n_time)
   vint_h1=fltarr(n_time)
   vint_t2=fltarr(n_time)
   vint_h2=fltarr(n_time)
   kpp_dhdt=fltarr(n_time)
   my_dhdt=fltarr(n_time)
   qbot=fltarr(n_time)
   qtop=fltarr(n_time)
   delt=fltarr(nz,n_time)
   rhs=fltarr(nz,n_time)
   diff=fltarr(nz,n_time)
   dhdt_fcorr=fltarr(n_time)
   
   FOR k=0,n_time-2 DO BEGIN      
      fcorr_dt=fltarr(nz)
      ;IF fcorr_flag eq 1 THEN BEGIN
      FOR m=0,nz-1 DO BEGIN
         fcorr_month=k/30
         WHILE fcorr_month ge 12 DO $
            fcorr_month=fcorr_month-12
         fcorr_dt(m)=fcorr_mmeans(m,fcorr_month)*86400./(rho(m,k)*cp(m,k))
      ENDFOR
      ;ENDIF      
      FOR m=0,nz-1 DO BEGIN
         vint_t1(k)=temperature(m,k)*dz(m)+vint_t1(k)
         vint_t2(k)=temperature(m,k+1)*dz(m)+vint_t2(k)
         vint_h1(k)=temperature(m,k)*dz(m)*rho(m,k)*cp(m,k)/86400.+vint_h1(k)
         vint_h2(k)=temperature(m,k+1)*dz(m)*rho(m,k+1)*cp(m,k+1)/86400.+vint_h2(k)
         
         ;IF fcorr_flag eq 1 THEN $
         dhdt_fcorr(k)=fcorr_dt(m)*dz(m)*rho(m,k+1)*cp(m,k+1)/86400.+dhdt_fcorr(k)
      ENDFOR
      
;         kpp_dhdt(k)=(vint_t2(k)-vint_t1(k))*rho(0,k)*cp(0,k)/3600.
      my_dhdt(k)=(vint_h2(k)-vint_h1(k))
      
      qtop(k)=flux_net(k+1)
      qbot(k)=(-1.)*rho(0,k)*cp(0,k)*(wT(nz-1,k))
      
;      IF fcorr_flag eq 1 THEN BEGIN
      qflx_runsum(i,k)=TOTAL(qtop(0:k))-TOTAL(qbot(0:k))+TOTAL(dhdt_fcorr(0:k))
      fcorr_runsum(i,k)=TOTAL(dhdt_fcorr(0:k))
;      ENDIF ELSE $
;         qflx_runsum(i,k)=TOTAL(qtop(0:k))-TOTAL(qbot(0:k))
      
      dhdt_runsum(i,k)=TOTAL(my_dhdt(0:k))
      
      FOR m=1,nz-1 DO BEGIN
         delt(m,k)=temperature(m,k+1)-temperature(m,k)
         fact=86400./dz(m)
         rhs(m,k)=fact*(wT(m,k)-wT(m-1,k))
         diff(m,k)=delt(m,k)-rhs(m,k)
      ENDFOR
   ENDFOR
   
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/drift/hadgem3kpp_mean_state_drift_heat_budget.'+pt_name+'.cumulative_ts.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=3000,SPACE2=1300,XOFFSET=1500,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
   GSET,XMIN=0,XMAX=n_time,YMIN=FLOAT(-6E4),YMAX=FLOAT(6E4)
   colors=['red','blue','cyan','brown','purple','pink','violet red','dark grey','steel blue','orange']
 ;  FOR j=0,n_runs-1 DO BEGIN
   smoothed=SMOOTH(qflx_runsum(i,0:n_time-2),25)      
   GPLOT,X=indgen(n_time-26),Y=REFORM(smoothed(12:n_time-14)),COL=FSC_COLOR(colors(i)),STYLE=0
   smoothed=SMOOTH(dhdt_runsum(i,0:n_time-2),25)
   GPLOT,X=indgen(n_time-26),Y=REFORM(smoothed(12:n_time-14)),COL=FSC_COLOR(colors(i)),STYLE=1
;   IF fcorr_flag eq 1 THEN BEGIN
   smoothed=SMOOTH(fcorr_runsum(i,0:n_time-2),25)
   GPLOT,X=indgen(n_time-26),Y=REFORM(smoothed(12:n_time-14)),COL=FSC_COLOR(colors(i)),STYLE=3
;   ENDIF
;ENDFOR

;   ensmean_qflx_runsum=fltarr(n_time)
;   ensmean_dhdt_runsum=fltarr(n_time)
;   IF fcorr_flag eq 1 THEN $
;      ensmean_fcorr_runsum=fltarr(n_time)
;   FOR j=0,n_time-1 DO BEGIN
;      ensmean_qflx_runsum(j)=MEAN(qflx_runsum(*,j))
;      ensmean_dhdt_runsum(j)=MEAN(dhdt_runsum(*,j))
;      IF fcorr_flag eq 1 THEN $
;         ensmean_fcorr_runsum(j)=MEAN(fcorr_runsum(*,j))
;   ENDFOR
  
;   smoothed=SMOOTH(ensmean_qflx_runsum(0:n_time-2),25)   
;   GPLOT,X=indgen(n_time-26),Y=REFORM(smoothed(12:n_time-14)),COL=FSC_COLOR('black'),STYLE=0,THICK=200
;   smoothed=SMOOTH(ensmean_dhdt_runsum(0:n_time-2),25)   
;   GPLOT,X=indgen(n_time-26),Y=REFORM(smoothed(12:n_time-14)),COL=FSC_COLOR('black'),STYLE=1,THICK=200   
;   IF fcorr_flag eq 1 THEN BEGIN
;      smoothed=SMOOTH(ensmean_fcorr_runsum(0:n_time-2),25)   
;      GPLOT,X=indgen(n_time-26),Y=REFORM(smoothed(12:n_time-14)),COL=FSC_COLOR('black'),STYLE=3,THICK=200
;   ENDIF

   AXES,YSTEP=6000.,XSTEP=30,/NORIGHT,YTITLE='Accumulated heat input and accumulated heat storage (W m!U-2!N)',$
        XTITLE='Days since beginning of simulation (ticks every 20 days)',NDECS=1
   
;YVALS=[1,2,3,5,7,10,20,30,50,70,100,200,300,500,700,1000,2000,3000,5000,7000,10000,20000,30000,50000],$

   GSET,XMIN=0,XMAX=n_time,YMIN=-15000,YMAX=15000
;   FOR j=0,n_runs-1 DO BEGIN
   smoothed=SMOOTH(dhdt_runsum(i,0:n_time-2)-qflx_runsum(i,0:n_time-2),25)
   GPLOT,X=indgen(n_time-26),Y=smoothed(12:n_time-14),COL=FSC_COLOR(colors(i)),STYLE=2
;   ENDFOR
   AXES,YSTEP=1500,XSTEP=72,YTITLE='Accumulated heat storage minus accumulated heat input (W m!U-2!N)',$
        /ONLYRIGHT
   PSCLOSE

ENDFOR

STOP
END

