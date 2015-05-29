PRO kpp_scm_heat_budget

; Plot the heat budget from a KPP single-column integration

runs=['xgspj_i2','xgspj_i3','xgspj_i4','xgspj_i5','xgspj_i6','xgspj_i7','xgspj_i8','xgspj_i9','xgspj_j0','xgspj_j1']
n_runs=N_ELEMENTS(runs)

n_sets=3
pt=[10,60]

FOR i=0,n_sets-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         indir='/export/niagara/data-02/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_scm'
         kpp_time='0431'
         set_name='no_fcorr'
      END
      1 : BEGIN
         indir='/export/niagara/data-02/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_scm'
         kpp_time='0431'
         set_name='fcorr_ind'
         fcorr_indir='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/heat_corrections'
      END
      2 : BEGIN
         indir='/export/niagara/data-02/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_scm'
         kpp_time='0431'
         set_name='fcorr_ensmean'
         fcorr_indir='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/heat_corrections'
      END
   ENDCASE

   FOR j=0,n_runs-1 DO BEGIN
      infile=indir+'/'+runs(j)+'/KPPocean_'+kpp_time+'.'+set_name+'.nc'

      time=OPEN_AND_EXTRACT(infile,'time')
      n_time=N_ELEMENTS(time)

      IF j eq 0 THEN BEGIN
         dhdt_runsum=fltarr(n_runs,n_time)
         qflx_runsum=fltarr(n_runs,n_time)
         fcorr_runsum=fltarr(n_runs,n_time)
      ENDIF
         
      z_full=OPEN_AND_EXTRACT(infile,'z')
      nz=N_ELEMENTS(z_full)
      z_half=OPEN_AND_EXTRACT(infile,'d')
      dz=OPEN_AND_EXTRACT(infile,'h')

      hmix=REFORM(OPEN_AND_EXTRACT(infile,'hmix'))
      
      temperature=REFORM(OPEN_AND_EXTRACT(infile,'T'))
      wT=REFORM(OPEN_AND_EXTRACT(infile,'wT'))
      difT=REFORM(OPEN_AND_EXTRACT(infile,'dift'))
      flux_nsolar=REFORM(OPEN_AND_EXTRACT(infile,'nsolar_in'))
      flux_solar=REFORM(OPEN_AND_EXTRACT(infile,'solar_in'))
      rho=REFORM(OPEN_AND_EXTRACT(infile,'rho'))
      cp=REFORM(OPEN_AND_EXTRACT(infile,'cp'))

      flux_net=flux_solar+flux_nsolar

      CASE i OF 
         0 : BEGIN
            fcorr_flag=0
         END
         1 : BEGIN
            fcorr_flag=1
            fcorr_infile=fcorr_indir+'/'+runs(j)+'_flxcorr_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421_ind.jan-dec_mmeans.flxcorr.n96.nc'
         END
         2 : BEGIN
            fcorr_flag=1
            fcorr_infile=fcorr_indir+'/hadgem3_1.5xentrain_ensmean_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421_ind.jan-dec_mmeans.flxcorr.n96.nc'
         END
      ENDCASE

      IF fcorr_flag eq 1 THEN BEGIN
         fcorr_longitude=OPEN_AND_EXTRACT(fcorr_infile,'longitude')
         fcorr_latitude=OPEN_AND_EXTRACT(fcorr_infile,'latitude')
         fcorr_lonpt=NEAREST(fcorr_longitude,pt(1))
         fcorr_latpt=NEAREST(fcorr_latitude,pt(0))
         
         fcorr_mmeans=REFORM(OPEN_AND_EXTRACT(fcorr_infile,'fcorr',$
                                              offset=[fcorr_lonpt,fcorr_latpt,0,0],$
                                              count=[1,1,nz,n_time/720]))
      ENDIF

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
      IF fcorr_flag eq 1 THEN $
         dhdt_fcorr=fltarr(n_time)

      FOR k=0,n_time-2 DO BEGIN

         fcorr_dt=fltarr(nz)
         IF fcorr_flag eq 1 THEN BEGIN
            FOR m=0,nz-1 DO $
               fcorr_dt(m)=fcorr_mmeans(m,k/720)*3600/(rho(m,k)*cp(m,k))
         ENDIF

         FOR m=0,nz-1 DO BEGIN
            vint_t1(k)=temperature(m,k)*dz(m)+vint_t1(k)
            vint_t2(k)=temperature(m,k+1)*dz(m)+vint_t2(k)
            vint_h1(k)=temperature(m,k)*dz(m)*rho(m,k)*cp(m,k)/3600.+vint_h1(k)
            vint_h2(k)=temperature(m,k+1)*dz(m)*rho(m,k+1)*cp(m,k+1)/3600.+vint_h2(k)
            
            IF fcorr_flag eq 1 THEN $
               dhdt_fcorr(k)=fcorr_dt(m)*dz(m)*rho(m,k+1)*cp(m,k+1)/3600.+dhdt_fcorr(k)
         ENDFOR

;         kpp_dhdt(k)=(vint_t2(k)-vint_t1(k))*rho(0,k)*cp(0,k)/3600.
         my_dhdt(k)=(vint_h2(k)-vint_h1(k))

         qtop(k)=flux_net(k+1)
         qbot(k)=(-1.)*rho(0,k)*cp(0,k)*(wT(nz-1,k))
        
         IF fcorr_flag eq 1 THEN BEGIN
            qflx_runsum(j,k)=TOTAL(qtop(0:k))-TOTAL(qbot(0:k))+TOTAL(dhdt_fcorr(0:k))
            fcorr_runsum(j,k)=TOTAL(dhdt_fcorr(0:k))
         ENDIF ELSE $
            qflx_runsum(j,k)=TOTAL(qtop(0:k))-TOTAL(qbot(0:k))
            
         dhdt_runsum(j,k)=TOTAL(my_dhdt(0:k))

         FOR m=1,nz-1 DO BEGIN
            delt(m,k)=temperature(m,k+1)-temperature(m,k)
            fact=3600./dz(m)
            rhs(m,k)=fact*(wT(m,k)-wT(m-1,k))
            diff(m,k)=delt(m,k)-rhs(m,k)
         ENDFOR
      ENDFOR

      print,'----------------------------------------------------------------------------'
      print,runs(j)+' in set '+set_name
      print,'heat input at top = ',TOTAL(qtop)
      print,'heat output at bottom = ',TOTAL(qbot)
      IF fcorr_flag eq 1 THEN BEGIN
         print,'heat input by flux correction = ',fcorr_runsum(j,n_time-2)
         print,'net flux into ocean = ',TOTAL(qtop)-TOTAL(qbot)+fcorr_runsum(j,n_time-2)
      ENDIF ELSE $
         print,'net flux into ocean = ',TOTAL(qtop)-TOTAL(qbot)

      print,'heat content = ',dhdt_runsum(j,n_time-2)   
      print,'----------------------------------------------------------------------------'
   ENDFOR

   psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_heat_budget.'+set_name+'.cumulative_ts.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=3000,SPACE2=1300,XOFFSET=1500,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
   GSET,XMIN=0,XMAX=n_time,YMIN=FLOAT(-6E5),YMAX=FLOAT(6E5)
   colors=['red','blue','cyan','brown','purple','pink','violet red','dark grey','steel blue','orange']
   FOR j=0,n_runs-1 DO BEGIN
      smoothed=SMOOTH(qflx_runsum(j,0:n_time-2),25)      
      GPLOT,X=indgen(n_time-26),Y=REFORM(smoothed(12:n_time-14)),COL=FSC_COLOR(colors(j)),STYLE=0
      smoothed=SMOOTH(dhdt_runsum(j,0:n_time-2),25)
      GPLOT,X=indgen(n_time-26),Y=REFORM(smoothed(12:n_time-14)),COL=FSC_COLOR(colors(j)),STYLE=1
      IF fcorr_flag eq 1 THEN BEGIN
         smoothed=SMOOTH(fcorr_runsum(j,0:n_time-2),25)
         GPLOT,X=indgen(n_time-26),Y=REFORM(smoothed(12:n_time-14)),COL=FSC_COLOR(colors(j)),STYLE=3
      ENDIF
   ENDFOR

   ensmean_qflx_runsum=fltarr(n_time)
   ensmean_dhdt_runsum=fltarr(n_time)
   IF fcorr_flag eq 1 THEN $
      ensmean_fcorr_runsum=fltarr(n_time)
   FOR j=0,n_time-1 DO BEGIN
      ensmean_qflx_runsum(j)=MEAN(qflx_runsum(*,j))
      ensmean_dhdt_runsum(j)=MEAN(dhdt_runsum(*,j))
      IF fcorr_flag eq 1 THEN $
         ensmean_fcorr_runsum(j)=MEAN(fcorr_runsum(*,j))
   ENDFOR
  
   smoothed=SMOOTH(ensmean_qflx_runsum(0:n_time-2),25)   
   GPLOT,X=indgen(n_time-26),Y=REFORM(smoothed(12:n_time-14)),COL=FSC_COLOR('black'),STYLE=0,THICK=200
   smoothed=SMOOTH(ensmean_dhdt_runsum(0:n_time-2),25)   
   GPLOT,X=indgen(n_time-26),Y=REFORM(smoothed(12:n_time-14)),COL=FSC_COLOR('black'),STYLE=1,THICK=200   
   IF fcorr_flag eq 1 THEN BEGIN
      smoothed=SMOOTH(ensmean_fcorr_runsum(0:n_time-2),25)   
      GPLOT,X=indgen(n_time-26),Y=REFORM(smoothed(12:n_time-14)),COL=FSC_COLOR('black'),STYLE=3,THICK=200
   ENDIF

   AXES,YSTEP=60000.,XSTEP=480,/NORIGHT,YTITLE='Accumulated heat input and accumulated heat storage (W m!U-2!N)',$
        XTITLE='Hours since beginning of simulation (ticks every 20 days)',NDECS=1
   
;YVALS=[1,2,3,5,7,10,20,30,50,70,100,200,300,500,700,1000,2000,3000,5000,7000,10000,20000,30000,50000],$

   GSET,XMIN=0,XMAX=n_time,YMIN=-1500,YMAX=1500
   FOR j=0,n_runs-1 DO BEGIN
      smoothed=SMOOTH(dhdt_runsum(j,0:n_time-2)-qflx_runsum(j,0:n_time-2),25)
      GPLOT,X=indgen(n_time-26),Y=smoothed(12:n_time-14),COL=FSC_COLOR(colors(j)),STYLE=2
   ENDFOR
   AXES,YSTEP=150,XSTEP=72,YTITLE='Accumulated heat storage minus accumulated heat input (W m!U-2!N)',$
        /ONLYRIGHT
   PSCLOSE

   print,'----------------------------------------------------------------------------'
   print,'ensmean in set '+set_name
;   print,'heat input at top = ',TOTAL(qtop)
;   print,'heat output at bottom = ',TOTAL(qbot)
   IF fcorr_flag eq 1 THEN $
      print,'heat input by flux correction = ',ensmean_fcorr_runsum(n_time-2)
   print,'net flux into ocean = ',ensmean_qflx_runsum(n_time-2)
   
   print,'heat content = ',ensmean_dhdt_runsum(n_time-2)   
   print,'----------------------------------------------------------------------------'
   
ENDFOR

STOP
END
