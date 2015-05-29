PRO kpp_scm_compare_swapped_fluxes

  ; Compare runs with swapped gridpoint fluxes

  ctl_basedir='/export/mango/data-10/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_scm'
;  swap_basedir='/export/mango/data-10/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_scm_swapflux'
  swap_basedir='/export/mango/data-10/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_scm_coriol1.25'

  runs=['xgspj_i2','xgspj_i3','xgspj_i4','xgspj_i5']
  n_runs=N_ELEMENTS(runs)
  kpp_time='0045'

  mylevs_T_diffinit=['-1.9','-1.7','-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1',$
                     '0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5','1.7','1.9']

  n_pts=2
;  pt1_str='0N60E'
;  pt2_str='2.5S60E'
  pt1_str='0N60E'
  pt2_str='1.25S60E'

  FOR i=0,n_runs-1 DO BEGIN
     ctl_pt1_infile=ctl_basedir+'/'+runs(i)+'/KPPocean_'+kpp_time+'.'+pt1_str+'.nc'
     swap_pt1_infile=swap_basedir+'/'+runs(i)+'/KPPocean_'+kpp_time+'.'+pt1_str+'_flux'+pt2_str+'.nc'
     ctl_pt2_infile=ctl_basedir+'/'+runs(i)+'/KPPocean_'+kpp_time+'.'+pt2_str+'.nc'
     swap_pt2_infile=swap_basedir+'/'+runs(i)+'/KPPocean_'+kpp_time+'.'+pt2_str+'_flux'+pt1_str+'.nc'
     
     kpp_z=OPEN_AND_EXTRACT(ctl_pt1_infile,'z')
     kpp_t=OPEN_AND_EXTRACT(ctl_pt1_infile,'time')
     kpp_nz=N_ELEMENTS(kpp_z)
     kpp_nt=N_ELEMENTS(kpp_t)

     IF i eq 0 THEN BEGIN
        ctl_pt1_sst=fltarr(n_runs,kpp_nt)        
        swap_pt1_sst=fltarr(n_runs,kpp_nt)
        ctl_pt2_sst=fltarr(n_runs,kpp_nt)
        swap_pt2_sst=fltarr(n_runs,kpp_nt)
     ENDIF

     ctl_pt1_T=REFORM(OPEN_AND_EXTRACT(ctl_pt1_infile,'T',$
                                       offset=[0,0,0,0],count=[1,1,kpp_nz,kpp_nt]))
     swap_pt1_T=REFORM(OPEN_AND_EXTRACT(swap_pt1_infile,'T',$
                                        offset=[0,0,0,0],count=[1,1,kpp_nz,kpp_nt]))
     ctl_pt2_T=REFORM(OPEN_AND_EXTRACT(ctl_pt2_infile,'T',$
                                       offset=[0,0,0,0],count=[1,1,kpp_nz,kpp_nt]))
     swap_pt2_T=REFORM(OPEN_AND_EXTRACT(swap_pt2_infile,'T',$
                                        offset=[0,0,0,0],count=[1,1,kpp_nz,kpp_nt]))

     ctl_pt1_hmix=REFORM(OPEN_AND_EXTRACT(ctl_pt1_infile,'hmix',$
                                       offset=[0,0,0],count=[1,1,kpp_nt]))
     swap_pt1_hmix=REFORM(OPEN_AND_EXTRACT(swap_pt1_infile,'hmix',$
                                        offset=[0,0,0],count=[1,1,kpp_nt]))
     ctl_pt2_hmix=REFORM(OPEN_AND_EXTRACT(ctl_pt2_infile,'hmix',$
                                       offset=[0,0,0],count=[1,1,kpp_nt]))
     swap_pt2_hmix=REFORM(OPEN_AND_EXTRACT(swap_pt2_infile,'hmix',$
                                        offset=[0,0,0],count=[1,1,kpp_nt]))

     ctl_pt1_sst(i,*)=ctl_pt1_T(0,*)
     ctl_pt2_sst(i,*)=ctl_pt2_T(0,*)
     swap_pt1_sst(i,*)=swap_pt1_T(0,*)
     swap_pt2_sst(i,*)=swap_pt2_T(0,*)

     psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes.T_diff_init.'+$
            pt1_str+'_ctlflux.'+runs(i)+'.'+kpp_time+'.ps'
     PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
     CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_T_diffinit)+1
     LEVS,MANUAL=mylevs_T_diffinit
     GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMAX=MAX(kpp_z),YMIN=-100.
     diffinit=fltarr(kpp_nt,kpp_nz)
     FOR k=0,kpp_nz-1 DO $
        diffinit(*,k)=REFORM(ctl_pt1_T(k,*)-ctl_pt1_T(k,0))
     CON,X=kpp_t,Y=kpp_z,FIELD=diffinit,$
         TITLE='Difference in temperature from initial conditions for '+runs(i)+' at pt '+pt1_str+' with control fluxes',/NOLINES
     GPLOT,X=kpp_t,Y=-1.*(ctl_pt1_hmix)
     AXES,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='Depth',XSTEP=1,XMINOR=0.125,YSTEP=10,YMINOR=5
     PSCLOSE,/NOVIEW

     psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes.T_diff_init.'+$
            pt2_str+'_ctlflux.'+runs(i)+'.'+kpp_time+'.ps'
     PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
     CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_T_diffinit)+1
     LEVS,MANUAL=mylevs_T_diffinit
     GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMAX=MAX(kpp_z),YMIN=-100.
     diffinit=fltarr(kpp_nt,kpp_nz)
     FOR k=0,kpp_nz-1 DO $
        diffinit(*,k)=REFORM(ctl_pt2_T(k,*)-ctl_pt2_T(k,0))
     CON,X=kpp_t,Y=kpp_z,FIELD=diffinit,$
         TITLE='Difference in temperature from initial conditions for '+runs(i)+' at pt '+pt2_str+' with control fluxes',/NOLINES
     GPLOT,X=kpp_t,Y=-1.*(ctl_pt2_hmix)
     AXES,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='Depth',XSTEP=1,XMINOR=0.125,YSTEP=10,YMINOR=5
     PSCLOSE,/NOVIEW

     psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes.T_diff_init.'+$
            pt1_str+'_swapflux.'+runs(i)+'.'+kpp_time+'.ps'
     PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
     CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_T_diffinit)+1
     LEVS,MANUAL=mylevs_T_diffinit
     GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMAX=MAX(kpp_z),YMIN=-100.
     diffinit=fltarr(kpp_nt,kpp_nz)
     FOR k=0,kpp_nz-1 DO $
        diffinit(*,k)=REFORM(swap_pt1_T(k,*)-swap_pt1_T(k,0))
     CON,X=kpp_t,Y=kpp_z,FIELD=diffinit,$
         TITLE='Difference in temperature from initial conditions for '+runs(i)+' at pt '+pt1_str+' with swapped fluxes',/NOLINES
     GPLOT,X=kpp_t,Y=-1.*(swap_pt1_hmix)
     AXES,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='Depth',XSTEP=1,XMINOR=0.125,YSTEP=10,YMINOR=5
     PSCLOSE,/NOVIEW

     psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes.T_diff_init.'+$
            pt2_str+'_swapflux.'+runs(i)+'.'+kpp_time+'.ps'
     PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
     CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_T_diffinit)+1
     LEVS,MANUAL=mylevs_T_diffinit
     GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMAX=MAX(kpp_z),YMIN=-100.
     diffinit=fltarr(kpp_nt,kpp_nz)
     FOR k=0,kpp_nz-1 DO $
        diffinit(*,k)=REFORM(swap_pt2_T(k,*)-swap_pt2_T(k,0))
     CON,X=kpp_t,Y=kpp_z,FIELD=diffinit,$
         TITLE='Difference in temperature from initial conditions for '+runs(i)+' at pt '+pt2_str+' with swapped fluxes',/NOLINES
     GPLOT,X=kpp_t,Y=-1.*(swap_pt2_hmix)
     AXES,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='Depth',XSTEP=1,XMINOR=0.125,YSTEP=10,YMINOR=5
     PSCLOSE,/NOVIEW

     psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes.T_effloc.'+$
            'fluxes_from_'+pt1_str+'.'+runs(i)+'.'+kpp_time+'.ps'
     PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
     CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_T_diffinit)+1
     LEVS,MANUAL=mylevs_T_diffinit
     GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMAX=MAX(kpp_z),YMIN=-100.
     diffinit=fltarr(kpp_nt,kpp_nz)
     FOR k=0,kpp_nz-1 DO $
        diffinit(*,k)=REFORM(swap_pt2_T(k,*)-ctl_pt1_T(k,*))
     CON,X=kpp_t,Y=kpp_z,FIELD=diffinit,$
         TITLE='Effect of location on temperature '+runs(i)+' when using fluxes from '+pt1_str+' ('+pt2_str+' minus '+pt1_str+')',/NOLINES
     AXES,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='Depth',XSTEP=1,XMINOR=0.125,YSTEP=10,YMINOR=5,/NORIGHT
     GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMIN=-20,YMAX=20
     GPLOT,X=kpp_t,Y=swap_pt2_hmix-ctl_pt1_hmix     
     AXES,YTITLE='Change in hmix depth (m)',YSTEP=2,YMINOR=1,/ONLYRIGHT
     PSCLOSE,/NOVIEW

     psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes.T_effloc.'+$
            'fluxes_from_'+pt2_str+'.'+runs(i)+'.'+kpp_time+'.ps'
     PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
     CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_T_diffinit)+1
     LEVS,MANUAL=mylevs_T_diffinit
     GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMAX=MAX(kpp_z),YMIN=-100.
     diffinit=fltarr(kpp_nt,kpp_nz)
     FOR k=0,kpp_nz-1 DO $
        diffinit(*,k)=REFORM(swap_pt1_T(k,*)-ctl_pt2_T(k,*))
     CON,X=kpp_t,Y=kpp_z,FIELD=diffinit,$
         TITLE='Effect of location on temperature '+runs(i)+' when using fluxes from '+pt1_str+' ('+pt1_str+' minus '+pt2_str+')',/NOLINES
     AXES,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='Depth',XSTEP=1,XMINOR=0.125,YSTEP=10,YMINOR=5,/NORIGHT
     GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMIN=-20,YMAX=20
     GPLOT,X=kpp_t,Y=swap_pt1_hmix-ctl_pt2_hmix
     AXES,YTITLE='Change in hmix depth (m)',YSTEP=2,YMINOR=1,/ONLYRIGHT
     PSCLOSE,/NOVIEW

     psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes.T_effflux.'+$
            'gridpt_'+pt1_str+'.'+runs(i)+'.'+kpp_time+'.ps'
     PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
     CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_T_diffinit)+1
     LEVS,MANUAL=mylevs_T_diffinit
     GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMAX=MAX(kpp_z),YMIN=-100.
     diffinit=fltarr(kpp_nt,kpp_nz)
     FOR k=0,kpp_nz-1 DO $
        diffinit(*,k)=REFORM(swap_pt1_T(k,*)-ctl_pt1_T(k,*))
     CON,X=kpp_t,Y=kpp_z,FIELD=diffinit,$
         TITLE='Effect of fluxes on temperature '+runs(i)+' at fixed location '+pt1_str+' (flux_'+pt2_str+' minus flux_'+pt1_str+')',/NOLINES
     AXES,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='Depth',XSTEP=1,XMINOR=0.125,YSTEP=10,YMINOR=5,/NORIGHT
     GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMIN=-20,YMAX=20
     GPLOT,X=kpp_t,Y=swap_pt1_hmix-ctl_pt1_hmix     
     AXES,YTITLE='Change in hmix depth (m)',YSTEP=2,YMINOR=1,/ONLYRIGHT
     PSCLOSE,/NOVIEW

     psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes.T_effflux.'+$
            'gridpt_'+pt2_str+'.'+runs(i)+'.'+kpp_time+'.ps'
     PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
     CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_T_diffinit)+1
     LEVS,MANUAL=mylevs_T_diffinit
     GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMAX=MAX(kpp_z),YMIN=-100.
     diffinit=fltarr(kpp_nt,kpp_nz)
     FOR k=0,kpp_nz-1 DO $
        diffinit(*,k)=REFORM(swap_pt2_T(k,*)-ctl_pt2_T(k,*))
     CON,X=kpp_t,Y=kpp_z,FIELD=diffinit,$
         TITLE='Effect of fluxes on temperature '+runs(i)+' at fixed location '+pt2_str+' (flux_'+pt1_str+' minus flux_'+pt2_str+')',/NOLINES
     AXES,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='Depth',XSTEP=1,XMINOR=0.125,YSTEP=10,YMINOR=5,/NORIGHT
     GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMIN=-20,YMAX=20
     GPLOT,X=kpp_t,Y=swap_pt2_hmix-ctl_pt2_hmix
     AXES,YTITLE='Change in hmix depth (m)',YSTEP=2,YMINOR=1,/ONLYRIGHT
     PSCLOSE,/NOVIEW
     
  ENDFOR
  
  psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes.sst_ts.'+pt1_str+'_swapfluxes.'+kpp_time+'.ps'
  PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
  GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMIN=25,YMAX=31,$
       TITLE='Effect of changing fluxes at '+pt1_str+' - solid = '+pt1_str+' fluxes ; dashed = '+pt2_str+' fluxes'
  colors=['purple','blue','orange','red']
  FOR i=0,n_runs-1 DO BEGIN
     GPLOT,X=kpp_t,Y=REFORM(ctl_pt1_sst(i,*)),COL=FSC_COLOR(colors(i))
     GPLOT,X=kpp_t,Y=REFORM(swap_pt1_sst(i,*)),COL=FSC_COLOR(colors(i)),STYLE=2
  ENDFOR
  GLEGEND,labels=REVERSE(runs),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=3
  GLEGEND,labels=['Fluxes from '+pt2_str,'Fluxes from '+pt1_str],STYLE=[2,0],LEGPOS=9
  AXES,XSTEP=1,XMINOR=0.125,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='SST (!Uo!NC)',YSTEP=0.3,YMINOR=0.15,NDECS=1
  PSCLOSE,/NOVIEW

  psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes.sst_ts_diff.'+pt1_str+'_swapfluxes.'+kpp_time+'.ps'
  PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
  GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMIN=-4,YMAX=4,$
       TITLE='Diff in SST from changing fluxes at '+pt1_str+' - '+pt2_str+' fluxes minus '+pt1_str+' fluxes'
  colors=['purple','blue','orange','red']
  FOR i=0,n_runs-1 DO $
     GPLOT,X=kpp_t,Y=REFORM(swap_pt1_sst(i,*))-REFORM(ctl_pt1_sst(i,*)),COL=FSC_COLOR(colors(i))
  GLEGEND,labels=REVERSE(runs),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=3
  AXES,XSTEP=1,XMINOR=0.125,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='SST (!Uo!NC)',YSTEP=0.4,YMINOR=0.2,NDECS=1
  PSCLOSE

  psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes.sst_ts.'+pt2_str+'_swapfluxes.'+kpp_time+'.ps'
  PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
  GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMIN=25,YMAX=31,$
       TITLE='Effect of changing fluxes at '+pt2_str+' - solid = '+pt1_str+' fluxes ; dashed = '+pt2_str+' fluxes'
  colors=['purple','blue','orange','red']
  FOR i=0,n_runs-1 DO BEGIN
     GPLOT,X=kpp_t,Y=REFORM(ctl_pt2_sst(i,*)),COL=FSC_COLOR(colors(i))
     GPLOT,X=kpp_t,Y=REFORM(swap_pt2_sst(i,*)),COL=FSC_COLOR(colors(i)),STYLE=2
  ENDFOR
  GLEGEND,labels=REVERSE(runs),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=3
  GLEGEND,labels=['Fluxes from '+pt1_str,'Fluxes from '+pt2_str],STYLE=[2,0],LEGPOS=9
  AXES,XSTEP=1,XMINOR=0.125,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='SST (!Uo!NC)',YSTEP=0.3,YMINOR=0.15,NDECS=1
  PSCLOSE,/NOVIEW

  psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes.sst_ts_diff.'+pt2_str+'_swapfluxes.'+kpp_time+'.ps'
  PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
  GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMIN=-4,YMAX=4,$
       TITLE='Diff in SST from changing fluxes at '+pt2_str+' - '+pt1_str+' fluxes minus '+pt2_str+' fluxes'
  colors=['purple','blue','orange','red']
  FOR i=0,n_runs-1 DO $
     GPLOT,X=kpp_t,Y=REFORM(swap_pt2_sst(i,*))-REFORM(ctl_pt2_sst(i,*)),COL=FSC_COLOR(colors(i))
  GLEGEND,labels=REVERSE(runs),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=3
  AXES,XSTEP=1,XMINOR=0.125,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='SST (!Uo!NC)',YSTEP=0.4,YMINOR=0.2,NDECS=1
  PSCLOSE

  psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes.sst_ts.'+pt1_str+'_swaploc.'+kpp_time+'.ps'
  PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
  GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMIN=25,YMAX=31,$
       TITLE='Effect of changing location using fluxes from '+pt1_str+' - solid = run at '+pt1_str+' ; dashed = run at '+pt2_str
  colors=['purple','blue','orange','red']
  FOR i=0,n_runs-1 DO BEGIN
     GPLOT,X=kpp_t,Y=REFORM(ctl_pt1_sst(i,*)),COL=FSC_COLOR(colors(i))
     GPLOT,X=kpp_t,Y=REFORM(swap_pt2_sst(i,*)),COL=FSC_COLOR(colors(i)),STYLE=2
  ENDFOR
  GLEGEND,labels=REVERSE(runs),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=3
  GLEGEND,labels=['Integration at '+pt2_str,'Integration at '+pt1_str],STYLE=[2,0],LEGPOS=9
  AXES,XSTEP=1,XMINOR=0.125,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='SST (!Uo!NC)',YSTEP=0.3,YMINOR=0.15,NDECS=1
  PSCLOSE,/NOVIEW

  psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes.sst_ts_diff.'+pt1_str+'_swaploc.'+kpp_time+'.ps'
  PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
  GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMIN=-4,YMAX=4,$
       TITLE='Diff in SST from changing location using fluxes from '+pt1_str+' - integration at '+pt2_str+' minus integration at '+pt1_str
  colors=['purple','blue','orange','red']
  FOR i=0,n_runs-1 DO $
     GPLOT,X=kpp_t,Y=REFORM(swap_pt2_sst(i,*))-REFORM(ctl_pt1_sst(i,*)),COL=FSC_COLOR(colors(i))
  GLEGEND,labels=REVERSE(runs),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=3
  AXES,XSTEP=1,XMINOR=0.125,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='Difference in SST (!Uo!NC)',YSTEP=0.4,YMINOR=0.2,NDECS=1
  PSCLOSE

  psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes.sst_ts.'+pt2_str+'_swaploc.'+kpp_time+'.ps'
  PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
  GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMIN=25,YMAX=31,$
       TITLE='Effect of changing location using fluxes from '+pt2_str+' - solid = run at '+pt2_str+' ; dashed = run at '+pt1_str
  colors=['purple','blue','orange','red']
  FOR i=0,n_runs-1 DO BEGIN
     GPLOT,X=kpp_t,Y=REFORM(ctl_pt2_sst(i,*)),COL=FSC_COLOR(colors(i))
     GPLOT,X=kpp_t,Y=REFORM(swap_pt1_sst(i,*)),COL=FSC_COLOR(colors(i)),STYLE=2
  ENDFOR
  GLEGEND,labels=REVERSE(runs),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=3
  GLEGEND,labels=['Integration at '+pt1_str,'Integration at '+pt2_str],STYLE=[2,0],LEGPOS=9
  AXES,XSTEP=1,XMINOR=0.125,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='SST (!Uo!NC)',YSTEP=0.3,YMINOR=0.15,NDECS=1
  PSCLOSE,/NOVIEW

  psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes.sst_ts_diff.'+pt2_str+'_swaploc.'+kpp_time+'.ps'
  PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
  GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMIN=-4,YMAX=4,$
       TITLE='Diff in SST from changing location using fluxes from '+pt2_str+' - integration at '+pt1_str+' minus integration at '+pt2_str
  colors=['purple','blue','orange','red']
  FOR i=0,n_runs-1 DO $
     GPLOT,X=kpp_t,Y=REFORM(swap_pt1_sst(i,*))-REFORM(ctl_pt2_sst(i,*)),COL=FSC_COLOR(colors(i))
  GLEGEND,labels=REVERSE(runs),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=3
  AXES,XSTEP=1,XMINOR=0.125,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='Difference in SST (!Uo!NC)',YSTEP=0.4,YMINOR=0.2,NDECS=1
  PSCLOSE

  STOP
END

