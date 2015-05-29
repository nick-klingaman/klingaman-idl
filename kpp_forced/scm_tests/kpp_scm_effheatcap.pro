PRO kpp_scm_effheatcap

  kpp_noneq_infile='/export/mango/data-10/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_scm_swapflux/xgspj_i2/KPPocean_0431.2.5S60E_flux0N60E.nc'

  kpp_eq_infile='/export/mango/data-10/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_scm/xgspj_i2/KPPocean_0431.nc'

  time=OPEN_AND_EXTRACT(kpp_noneq_infile,'time')
  n_time=N_ELEMENTS(time)
  
  taux=REFORM(OPEN_AND_EXTRACT(kpp_noneq_infile,'taux_in'))
  tauy=REFORM(OPEN_AND_EXTRACT(kpp_noneq_infile,'tauy_in'))
  solar=REFORM(OPEN_AND_EXTRACT(kpp_noneq_infile,'solar_in'))
  non_solar=REFORM(OPEN_AND_EXTRACT(kpp_noneq_infile,'nsolar_in'))

  eq_sst=REFORM(OPEN_AND_EXTRACT(kpp_eq_infile,'T',offset=[0,0,0,0],$
                        count=[1,1,1,n_time]))
  eq_rho=REFORM(OPEN_AND_EXTRACT(kpp_eq_infile,'rho',offset=[0,0,0,0],$
                        count=[1,1,1,n_time]))
  noneq_sst=REFORM(OPEN_AND_EXTRACT(kpp_noneq_infile,'T',offset=[0,0,0,0],$
                        count=[1,1,1,n_time]))
  noneq_rho=REFORM(OPEN_AND_EXTRACT(kpp_noneq_infile,'rho',offset=[0,0,0,0],$
                        count=[1,1,1,n_time]))

  eq_dsst_dt=fltarr(n_time-1)
  noneq_dsst_dt=fltarr(n_time-1)
  FOR i=0,n_time-2 DO BEGIN
     eq_dsst_dt(i)=(eq_sst(i+1)-eq_sst(i))/(3600.)
     noneq_dsst_dt(i)=(noneq_sst(i+1)-noneq_sst(i))/(3600.)
  ENDFOR

  eq_ustar=((taux^2+tauy^2)^(0.5))/eq_rho
  noneq_ustar=((taux^2+tauy^2)^(0.5))/noneq_rho

  net_flux=solar+non_solar

  eq_eff_heat_cap=net_flux/eq_dsst_dt
  noneq_eff_heat_cap=net_flux/noneq_dsst_dt

  neg_pts=where(eq_eff_heat_cap le 0)
  eq_eff_heat_cap=ALOG10(ABS(eq_eff_heat_cap))
  eq_eff_heat_cap[neg_pts]=(-1.)*(eq_eff_heat_cap[neg_pts])

  neg_pts=where(noneq_eff_heat_cap le 0)
  noneq_eff_heat_cap=ALOG10(ABS(noneq_eff_heat_cap))
  noneq_eff_heat_cap[neg_pts]=(-1.)*(noneq_eff_heat_cap[neg_pts])

  psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_effheatcap.scatter_noneq-vs-eq.ps'
  PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500,$
         XPLOTS=2

  POS,XPOS=2,XSIZE=12000
  GSET,XMIN=0,XMAX=14,YMIN=5.,YMAX=10.
  eq_valid_pts=where(eq_eff_heat_cap ge 0 and eq_ustar ne 0)
  noneq_valid_pts=where(noneq_eff_heat_cap ge 0 and noneq_ustar ne 0)
  GPLOT,X=eq_ustar[eq_valid_pts]*10.^5,Y=eq_eff_heat_cap[eq_valid_pts],COL=FSC_COLOR('blue'),SYM=6,SIZE=60,/NOLINES
  GPLOT,X=noneq_ustar[noneq_valid_pts]*10.^5,Y=noneq_eff_heat_cap[noneq_valid_pts],COL=FSC_COLOR('red'),SYM=5,SIZE=60,/NOLINES

;  flux_divisions=[-300,-200,-100,0,100,200,300,400,500,600,700]
;  CS,SCALE=3,NCOLS=N_ELEMENTS(flux_divisions)+2
;  valid_pts=where(net_flux le flux_divisions(0))
;  GPLOT,X=eq_ustar[valid_pts]*10.^5,Y=eq_eff_heat_cap[valid_pts],COL=2,SYM=6,/NOLINES,SIZE=60
;  GPLOT,X=noneq_ustar[valid_pts]*10.^5,Y=eq_eff_heat_cap[valid_pts],COL=2,SYM=5,/NOLINES,SIZE=60
;  FOR i=1,N_ELEMENTS(flux_divisions)-2 DO BEGIN
;     valid_pts=where(net_flux ge flux_divisions(i) and net_flux lt flux_divisions(i+1))
;     GPLOT,X=eq_ustar[valid_pts]*10.^5,Y=eq_eff_heat_cap[valid_pts],COL=3+i,SYM=6,/NOLINES,SIZE=60
;     GPLOT,X=noneq_ustar[valid_pts]*10.^5,Y=noneq_eff_heat_cap[valid_pts],COL=3+i,SYM=5,/NOLINES,SIZE=60
;  ENDFOR
;  valid_pts=where(net_flux ge flux_divisions(N_ELEMENTS(flux_divisions)-1))
;  GPLOT,X=eq_ustar[valid_pts]*10.^5,Y=eq_eff_heat_cap[valid_pts],COL=N_ELEMENTS(flux_divisions)+3,SYM=6,/NOLINES,SIZE=60
;  GPLOT,X=noneq_ustar[valid_pts]*10.^5,Y=eq_eff_heat_cap[valid_pts],COL=N_ELEMENTS(flux_divisions)+3,SYM=5,/NOLINES,SIZE=60

;  GLEGEND,labels=['2.5S,60E with 0N,60E fluxes','0N,60E with 0N,60E fluxes'],$
;          SYM=[5,6],LEGPOS=11,LENGTH=0
  AXES,YTITLE="Logarithm of effective heat capacity [F_net/(d_sst/d_t); J m!U-2!N K!U-1!N]",$
       XTITLE="Surface friction velocity [(tau_x^2+tau_y^2)^(0.5)/rho; N m kg!U-1!N] * (10^5)",$
       YSTEP=0.5,YMINOR=0.25,XSTEP=1,XMINOR=0.5,NDECS=2

  POS,XPOS=1,XSIZE=12000
  GSET,XMIN=0,XMAX=14,YMIN=-10.,YMAX=-5.
  eq_valid_pts=where(eq_eff_heat_cap le 0 and eq_ustar ne 0)
  noneq_valid_pts=where(noneq_eff_heat_cap le 0 and noneq_ustar ne 0)
  GPLOT,X=eq_ustar[eq_valid_pts]*10.^5,Y=eq_eff_heat_cap[eq_valid_pts],COL=FSC_COLOR('blue'),SYM=6,SIZE=60,/NOLINES
  GPLOT,X=noneq_ustar[noneq_valid_pts]*10.^5,Y=noneq_eff_heat_cap[noneq_valid_pts],COL=FSC_COLOR('red'),SYM=5,SIZE=60,/NOLINES

;  flux_divisions=[-300,-200,-100,0,100,200,300,400,500,600,700]
;  CS,SCALE=3,NCOLS=N_ELEMENTS(flux_divisions)+2
;  valid_pts=where(net_flux le flux_divisions(0))
;  GPLOT,X=eq_ustar[valid_pts]*10.^5,Y=eq_eff_heat_cap[valid_pts],COL=2,SYM=6,/NOLINES,SIZE=60
;  GPLOT,X=noneq_ustar[valid_pts]*10.^5,Y=eq_eff_heat_cap[valid_pts],COL=2,SYM=5,/NOLINES,SIZE=60
;  FOR i=1,N_ELEMENTS(flux_divisions)-2 DO BEGIN
;     valid_pts=where(net_flux ge flux_divisions(i) and net_flux lt flux_divisions(i+1))
;     GPLOT,X=eq_ustar[valid_pts]*10.^5,Y=eq_eff_heat_cap[valid_pts],COL=3+i,SYM=6,/NOLINES,SIZE=60
;     GPLOT,X=noneq_ustar[valid_pts]*10.^5,Y=noneq_eff_heat_cap[valid_pts],COL=3+i,SYM=5,/NOLINES,SIZE=60
;  ENDFOR
;  valid_pts=where(net_flux ge flux_divisions(N_ELEMENTS(flux_divisions)-1))
;  GPLOT,X=eq_ustar[valid_pts]*10.^5,Y=eq_eff_heat_cap[valid_pts],COL=N_ELEMENTS(flux_divisions)+3,SYM=6,/NOLINES,SIZE=60
;  GPLOT,X=noneq_ustar[valid_pts]*10.^5,Y=eq_eff_heat_cap[valid_pts],COL=N_ELEMENTS(flux_divisions)+3,SYM=5,/NOLINES,SIZE=60

  GLEGEND,labels=['2.5S,60E with 0N,60E fluxes','0N,60E with 0N,60E fluxes'],$
          SYM=[5,6],LEGPOS=9,LENGTH=0,COL=[FSC_COLOR('red'),FSC_COLOR('blue')]
  AXES,YTITLE="Logarithm of effective heat capacity [F_net/(d_sst/d_t); J m!U-2!N K!U-1!N]",$
       XTITLE="Surface friction velocity [(tau_x^2+tau_y^2)^(0.5)/rho; N m kg!U-1!N] * (10^5)",$
       YSTEP=0.5,YMINOR=0.25,XSTEP=1,XMINOR=0.5,NDECS=2 
  PSCLOSE

  FOR j=0,1 DO BEGIN
     CASE j OF
        0 : BEGIN
           eq_eff_heat_cap_input=eq_eff_heat_cap[where(eq_eff_heat_cap le 0)]
           eq_ustar_input=eq_ustar[where(eq_eff_heat_cap le 0)]
           noneq_eff_heat_cap_input=noneq_eff_heat_cap[where(noneq_eff_heat_cap le 0)]
           noneq_ustar_input=noneq_ustar[where(noneq_eff_heat_cap le 0)]
           plot_type='negative_effheatcap'
           yrange=[-11,-4]
        END 
        1 : BEGIN
           eq_eff_heat_cap_input=eq_eff_heat_cap[where(eq_eff_heat_cap ge 0)]
           eq_ustar_input=eq_ustar[where(eq_eff_heat_cap ge 0)]
           noneq_eff_heat_cap_input=noneq_eff_heat_cap[where(noneq_eff_heat_cap ge 0)]
           noneq_ustar_input=noneq_ustar[where(noneq_eff_heat_cap ge 0)]
           plot_type='positive_effheatcap'
           yrange=[4,11]
        END
     ENDCASE
  
     bin_size=0.5
     n_bins=21
     min_bin=0.0001
     bins=findgen(n_bins)*bin_size+min_bin
     bins_mid=fltarr(n_bins)
     FOR i=0,n_bins-2 DO $
        bins_mid(i)=(bins(i)+bins(i+1))/2.
     bins_mid(n_bins-1)=MAX(bins)+bins_mid(0)
     
     eq_bin_mean=fltarr(n_bins)
     eq_bin_iqr=fltarr(2,n_bins)
     eq_bin_range=fltarr(2,n_bins)
     eq_bin_pts=fltarr(n_bins)
     noneq_bin_mean=fltarr(n_bins)
     noneq_bin_iqr=fltarr(2,n_bins)
     noneq_bin_range=fltarr(2,n_bins)
     noneq_bin_pts=fltarr(n_bins)
     FOR i=0,n_bins-1 DO BEGIN
        IF i eq n_bins-1 THEN BEGIN
           eq_pts=where(eq_ustar_input*10.^5 ge bins(n_bins-1))
           noneq_pts=where(noneq_ustar_input*10.^5 ge bins(n_bins-1))
        ENDIF ELSE BEGIN
           eq_pts=where(eq_ustar_input*10.^5 ge bins(i) and eq_ustar_input*10.^5 lt bins(i+1))
           noneq_pts=where(noneq_ustar_input*10.^5 ge bins(i) and noneq_ustar_input*10.^5 lt bins(i+1))
        ENDELSE
        
        eq_valid=eq_eff_heat_cap_input[eq_pts]
        noneq_valid=noneq_eff_heat_cap_input[noneq_pts]
        
        eq_bin_mean(i)=MEAN(eq_valid)
        noneq_bin_mean(i)=MEAN(noneq_valid)
        eq_bin_range(0,i)=MIN(eq_valid)
        noneq_bin_range(0,i)=MIN(noneq_valid)
        eq_bin_range(1,i)=MAX(eq_valid)
        noneq_bin_range(1,i)=MAX(noneq_valid)
        
        eq_bin_pts(i)=N_ELEMENTS(eq_pts)
        noneq_bin_pts(i)=N_ELEMENTS(noneq_pts)
        
        eq_sort=SORT(eq_valid)
        noneq_sort=SORT(noneq_valid)     
        
        eq_bin_iqr(0,i)=eq_valid(eq_sort(eq_bin_pts(i)/4))
        noneq_bin_iqr(0,i)=noneq_valid(noneq_sort(noneq_bin_pts(i)/4))
        eq_bin_iqr(1,i)=eq_valid(eq_sort(eq_bin_pts(i)*3/4))
        noneq_bin_iqr(1,i)=noneq_valid(noneq_sort(noneq_bin_pts(i)*3/4))
     ENDFOR
     
     psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_effheatcap.bin_effheatcap_by_ustar.'+plot_type+'.ps'
     PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
     GSET,XMIN=0,XMAX=MAX(bins_mid)+0.25,YMIN=yrange(0),YMAX=yrange(1)
     FOR i=0,n_bins-1 DO BEGIN
        GPLOT,X=bins_mid(i)-0.075,Y=eq_bin_mean(i),SYM=6,COL=FSC_COLOR('blue'),/NOLINES
        GPLOT,X=[bins_mid(i)-0.15,bins_mid(i)-0.15],Y=[eq_bin_iqr(0,i),eq_bin_iqr(1,i)],COL=FSC_COLOR('blue')
        GPLOT,X=[bins_mid(i),bins_mid(i)],Y=[eq_bin_iqr(0,i),eq_bin_iqr(1,i)],COL=FSC_COLOR('blue')
        GPLOT,X=[bins_mid(i)-0.15,bins_mid(i)],Y=[eq_bin_iqr(0,i),eq_bin_iqr(0,i)],COL=FSC_COLOR('blue')
        GPLOT,X=[bins_mid(i)-0.15,bins_mid(i)],Y=[eq_bin_iqr(1,i),eq_bin_iqr(1,i)],COL=FSC_COLOR('blue')
        GPLOT,X=[bins_mid(i)-0.075,bins_mid(i)-0.075],Y=[eq_bin_iqr(0,i),eq_bin_range(0,i)],COL=FSC_COLOR('blue')
        GPLOT,X=[bins_mid(i)-0.075,bins_mid(i)-0.075],Y=[eq_bin_iqr(1,i),eq_bin_range(1,i)],COL=FSC_COLOR('blue')
        
        GPLOT,X=bins_mid(i)+0.075,Y=noneq_bin_mean(i),SYM=6,COL=FSC_COLOR('red'),/NOLINES
        GPLOT,X=[bins_mid(i)+0.15,bins_mid(i)+0.15],Y=[noneq_bin_iqr(0,i),noneq_bin_iqr(1,i)],COL=FSC_COLOR('red')
        GPLOT,X=[bins_mid(i),bins_mid(i)],Y=[noneq_bin_iqr(0,i),noneq_bin_iqr(1,i)],COL=FSC_COLOR('red')
        GPLOT,X=[bins_mid(i)+0.15,bins_mid(i)],Y=[noneq_bin_iqr(0,i),noneq_bin_iqr(0,i)],COL=FSC_COLOR('red')
        GPLOT,X=[bins_mid(i)+0.15,bins_mid(i)],Y=[noneq_bin_iqr(1,i),noneq_bin_iqr(1,i)],COL=FSC_COLOR('red')
        GPLOT,X=[bins_mid(i)+0.075,bins_mid(i)+0.075],Y=[noneq_bin_iqr(0,i),noneq_bin_range(0,i)],COL=FSC_COLOR('red')
        GPLOT,X=[bins_mid(i)+0.075,bins_mid(i)+0.075],Y=[noneq_bin_iqr(1,i),noneq_bin_range(1,i)],COL=FSC_COLOR('red')
     ENDFOR   
     GLEGEND,labels=['2.5S,60E with 0N,60E fluxes','0N,60E with 0N,60E fluxes'],$
             COL=[FSC_COLOR('red'),FSC_COLOR('blue')],SYM=[6,6],LEGPOS=11
     AXES,XSTEP=1,XMINOR=0.5,YSTEP=1,YMINOR=0.5,$
          YTITLE="Logarithm of effective heat capacity [F_net/(d_sst/d_t); J m!U-2!N K!U-1!N]",$
          XTITLE="Surface friction velocity [(tau_x^2+tau_y^2)^(0.5)/rho; N m kg!U-1!N] * (10^5)",$
          NDECS=2
     PSCLOSE
  ENDFOR
STOP
END
