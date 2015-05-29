PRO kpp_scm_plot_fluxes_T_hmix

basedir='/export/mango/data-10/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_scm'
runs=['xgspj_i2','xgspj_i3','xgspj_i4','xgspj_i5']
years=['i2','i3','i4','i5']
n_runs=N_ELEMENTS(runs)
kpp_time='0045'

mylevs_T=['20','20.5','21','21.5','22','22.5','23','23.5','24','24.5','25','25.5','26','26.5','27','27.5','28','28.5','29','29.5']
mylevs_T_diffinit=['-1.9','-1.7','-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5','1.7','1.9']
mylevs_Tinc=['-0.095','-0.085','-0.075','-0.065','-0.055','-0.045','-0.035','-0.025','-0.015','-0.005','0.005','0.015','0.025','0.035','0.045','0.055','0.065','0.075','0.085','0.095']

flux_offset_start=15*8
flux_time_res=3 ; as multiple of KPP timestep
flux_count=30*8

n_pts=2
FOR i=0,n_runs-1 DO BEGIN
   flux_infile='/export/mango/data-10/ss901165/kpp_ocean/ancillaries/flux_data/xgspj.fluxes_for_kpp.jan-dec_dmeans.'+years(i)+'.3hr_n96_tgrid.nc'
   FOR j=0,n_pts-1 DO BEGIN
      CASE j OF 
         0 : BEGIN
            pt=[0,60]
            kpp_pt='0N60E'            
         END
         1 : BEGIN
            pt=[-2.5,60]
            kpp_pt='2.5S60E'
         END
      ENDCASE
      kpp_infile=basedir+'/'+runs(i)+'/KPPocean_'+kpp_time+'.'+kpp_pt+'.nc'

      kpp_z=OPEN_AND_EXTRACT(kpp_infile,'z')
      kpp_nz=N_ELEMENTS(kpp_z)
      kpp_t=OPEN_AND_EXTRACT(kpp_infile,'time')
      kpp_nt=N_ELEMENTS(kpp_t)

      flux_longitude=OPEN_AND_EXTRACT(flux_infile,'longitude')
      flux_latitude=OPEN_AND_EXTRACT(flux_infile,'latitude')
      flux_lonpt=NEAREST(flux_longitude,pt(1))
      flux_latpt=NEAREST(flux_latitude,pt(0))

      lhf = REFORM(OPEN_AND_EXTRACT(flux_infile,'lhf',offset=[flux_lonpt,flux_latpt,flux_offset_start],count=[1,1,flux_count]))
      shf = REFORM(OPEN_AND_EXTRACT(flux_infile,'shf',offset=[flux_lonpt,flux_latpt,flux_offset_start],count=[1,1,flux_count]))
      lwf = REFORM(OPEN_AND_EXTRACT(flux_infile,'lwf',offset=[flux_lonpt,flux_latpt,flux_offset_start],count=[1,1,flux_count]))
      swf = REFORM(OPEN_AND_EXTRACT(flux_infile,'swf',offset=[flux_lonpt,flux_latpt,flux_offset_start],count=[1,1,flux_count]))
      taux = REFORM(OPEN_AND_EXTRACT(flux_infile,'taux',offset=[flux_lonpt,flux_latpt,flux_offset_start],count=[1,1,flux_count]))
      tauy = REFORM(OPEN_AND_EXTRACT(flux_infile,'tauy',offset=[flux_lonpt,flux_latpt,flux_offset_start],count=[1,1,flux_count]))
      precip = REFORM(OPEN_AND_EXTRACT(flux_infile,'precip',offset=[flux_lonpt,flux_latpt,flux_offset_start],count=[1,1,flux_count]))

      kpp_temp = REFORM(OPEN_AND_EXTRACT(kpp_infile,'T',offset=[0,0,0,0],count=[1,1,kpp_nz,kpp_nt]))
      kpp_temp_tz = fltarr(kpp_nt,kpp_nz)
      FOR k=0,kpp_nz-1 DO $
         kpp_temp_tz(*,k)=kpp_temp(k,*)
      kpp_hmix = REFORM(OPEN_AND_EXTRACT(kpp_infile,'hmix',offset=[0,0,0],count=[1,1,kpp_nt]))

      psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_plot_fluxes_T_hmix.T_hmix.'+kpp_pt+'.'+runs(i)+'.'+kpp_time+'.nc'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_T)+1
      LEVS,MANUAL=mylevs_T
      GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMAX=MAX(kpp_z),YMIN=-100.
      CON,X=kpp_t,Y=kpp_z,FIELD=kpp_temp_tz,$
          TITLE='Fluxes, temperatures and hmix for '+runs(i)+' at pt '+kpp_pt,/NOLINES
      GPLOT,X=kpp_t,Y=-1.*(kpp_hmix)
      AXES,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='Depth',XSTEP=1,XMINOR=0.125,YSTEP=10,YMINOR=5
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_plot_fluxes_T_hmix.T_diff_init.'+kpp_pt+'.'+runs(i)+'.'+kpp_time+'.nc'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_T_diffinit)+1
      LEVS,MANUAL=mylevs_T_diffinit
      GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMAX=MAX(kpp_z),YMIN=-100.
      kpp_temp_tz_diffinit=fltarr(kpp_nt,kpp_nz)
      FOR k=0,kpp_nz-1 DO $
         kpp_temp_tz_diffinit(*,k)=kpp_temp_tz(*,k)-kpp_temp_tz(0,k)
      CON,X=kpp_t,Y=kpp_z,FIELD=kpp_temp_tz_diffinit,$
          TITLE='Difference in temperature from initial conditions for '+runs(i)+' at pt '+kpp_pt,/NOLINES
      GPLOT,X=kpp_t,Y=-1.*(kpp_hmix)
      AXES,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='Depth',XSTEP=1,XMINOR=0.125,YSTEP=10,YMINOR=5
      PSCLOSE

      psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_plot_fluxes_T_hmix.Tinc.'+kpp_pt+'.'+runs(i)+'.'+kpp_time+'.nc'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_Tinc)+1
      LEVS,MANUAL=mylevs_Tinc
      GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMAX=MAX(kpp_z),YMIN=-100.
      kpp_temp_tz_inc=fltarr(kpp_nt,kpp_nz)
      FOR k=0,kpp_nz-1 DO BEGIN
         kpp_temp_tz_inc(0,k)=0.
         FOR m=1,kpp_nt-1 DO BEGIN
            kpp_temp_tz_inc(m,k)=kpp_temp_tz(m,k)-kpp_temp_tz(m-1,k)
         ENDFOR
      ENDFOR
      CON,X=kpp_t,Y=kpp_z,FIELD=kpp_temp_tz_inc,$
          TITLE='Temperature increment for '+runs(i)+' at pt '+kpp_pt,/NOLINES,CB_TITLE='degrees C per timestep (1 hour)',$
          CB_WIDTH=115
      GPLOT,X=kpp_t,Y=-1.*(kpp_hmix)
      AXES,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='Depth',XSTEP=1,XMINOR=0.125,YSTEP=10,YMINOR=5
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_plot_fluxes_T_hmix.fluxes.'+kpp_pt+'.'+runs(i)+'.'+kpp_time+'.nc'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2500,SPACE2=800,XOFFSET=500,YOFFSET=2500,TFONT=2,TCHARSIZE=100,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_T)+1
      LEVS,MANUAL=mylevs_T
      GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMAX=500,YMIN=0
      GPLOT,X=findgen(flux_count)*(flux_time_res/24.)+MIN(kpp_t),Y=swf*0.5,COL=FSC_COLOR('orange')
      GPLOT,X=findgen(flux_count)*(flux_time_res/24.)+MIN(kpp_t),Y=lwf*(-1.),COL=FSC_COLOR('red')
      GPLOT,X=findgen(flux_count)*(flux_time_res/24.)+MIN(kpp_t),Y=lhf*(-1.),COL=FSC_COLOR('blue')      
      AXES,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='Fluxes (W m!U-2!N)',XSTEP=1,XMINOR=0.125,YSTEP=50,YMINOR=25,/NORIGHT
      
      GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMAX=30,YMIN=-15
      GPLOT,X=findgen(flux_count)*(flux_time_res/24.)+MIN(kpp_t),Y=shf*(-1.),COL=FSC_COLOR('purple')
      GPLOT,X=findgen(flux_count)*(flux_time_res/24.)+MIN(kpp_t),Y=taux*(100.),COL=FSC_COLOR('green')
      GPLOT,X=findgen(flux_count)*(flux_time_res/24.)+MIN(kpp_t),Y=tauy*(100.),COL=FSC_COLOR('green'),STYLE=2
      GPLOT,X=findgen(flux_count)*(flux_time_res/24.)+MIN(kpp_t),Y=(precip+(lhf/(2.5*10.^6.)))*43200.,COL=FSC_COLOR('cyan')
      GPLOT,X=findgen(flux_count)*(flux_time_res/24.)+MIN(kpp_t),Y=(swf+lwf+lhf+shf)*0.02,COL=FSC_COLOR('black')
      AXES,YSTEP=3.,YMINOR=1.5,YTITLE='Fluxes (W m!U-2!N) or PminusE (mm day!U-1!N) or wind stress (N m!U-2!N)',/ONLYRIGHT,NDECS=1

      GLEGEND,labels=['Precipitation minus evaporation (right axis, mm 12hr!U-1!N)',$
                      'Meridional wind stress * 100. (right axis, N m!U-2!N)',$
                      'Zonal wind stress * 100. (right axis, N m!U-2!N)',$
                      'Sensible heat flux * -1. (right axis, W m!U-2!N)'],COL=[FSC_COLOR('cyan'),FSC_COLOR('green'),FSC_COLOR('green'),$
                                                                               FSC_COLOR('purple')],STYLE=[0,2,0,0],LEGYOFFSET=-1000,LEGXOFFSET=0
      GLEGEND,labels=['Latent heat flux * -1. (left axis, W m!U-2!N)',$
                      'Longwave flux (left axis, W m!U-2!N)',$
                      'Shortwave flux * 0.5 (left axis, W m!U-2!N)',$
                      'Net flux at surface * 0.02 (right axis, W m!U-2!N)'],COL=[FSC_COLOR('blue'),FSC_COLOR('red'),$
                                                                                 FSC_COLOR('orange'),FSC_COLOR('black')],$
              STYLE=[0,0,0,0],LEGYOFFSET=-1000,LEGXOFFSET=-15000     
      PSCLOSE,/NOVIEW  
   ENDFOR

ENDFOR

STOP
END
