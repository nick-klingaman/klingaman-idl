PRO kpp_scm_compare_swapped_fluxes_difm

  ; Compare runs with swapped gridpoint fluxes

  ctl_basedir='/export/mango/data-10/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_scm'
;  ctl_basedir='/export/mango/data-10/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_scm_coriol25'
  swap_basedir='/export/mango/data-10/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_scm_swapflux'
  
;  runs=['xgspj_i2','xgspj_i3','xgspj_i4','xgspj_i5']
  runs=['xgspj_i2']
  n_runs=N_ELEMENTS(runs)
  kpp_time='0045'

  mylevs_difm=['0.1','0.4','0.7','1.0','1.3','1.6','1.9','2.2','2.5','2.8','3.1','3.4','3.7','4.0','4.3','4.6']
  mylevs_difm_diff=['-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5']
  mylevs_dift=['0.1','0.5','0.9','1.3','1.7','2.1','2.5','2.9','3.3','3.7','3.9','4.3','4.7','5.1','5.5','5.9']
;  mylevs_shear=['0.00','0.01','0.02','0.03','0.04','0.05','0.06','0.07','0.08','0.09','0.10','0.11','0.12','0.13','0.14','0.15','0.16']
;  mylevs_shear_diff=['-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15']
  mylevs_rig=['-10','-7','-4','-2','-1','1','2','4','7','10','20','40','70','100','200','400','700','1000','2000']
  mylevs_dbloc=['-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5','1.7','1.9','2.1','2.3','2.5']
  mylevs_dbloc_diff=['-3.1','-2.5','-1.9','-1.5','-1.1','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','1.1','1.5','1.9','2.5','3.1']
  mylevs_shsq=['0','1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16']
  mylevs_shsq_diff=['-6.2','-5.0','-3.8','-3.0','-2.2','-1.4','-1.0','-0.6','-0.2','0.2','0.6','1.0','1.4','2.2','3.0','3.8','5.0','6.2']

  n_pts=2
  pt2_str='2.5S60E'
  pt1_str='0N60E'

  FOR i=0,n_runs-1 DO BEGIN
     ctl_pt1_infile=ctl_basedir+'/'+runs(i)+'/KPPocean_'+kpp_time+'.Rig_diags.'+pt1_str+'.nc'
;     ctl_pt1_infile=ctl_basedir+'/'+runs(i)+'/KPPocean_'+kpp_time+'.nc'
     swap_pt2_infile=swap_basedir+'/'+runs(i)+'/KPPocean_'+kpp_time+'.Rig_diags.'+pt2_str+'_flux'+pt1_str+'.nc'
     
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

     ctl_pt1_difm=REFORM(OPEN_AND_EXTRACT(ctl_pt1_infile,'difm',$
                                       offset=[0,0,0,0],count=[1,1,kpp_nz,kpp_nt]))*100.
     ctl_pt1_dift=REFORM(OPEN_AND_EXTRACT(ctl_pt1_infile,'dift',$
                                       offset=[0,0,0,0],count=[1,1,kpp_nz,kpp_nt]))*100.
     swap_pt2_difm=REFORM(OPEN_AND_EXTRACT(swap_pt2_infile,'difm',$
                                        offset=[0,0,0,0],count=[1,1,kpp_nz,kpp_nt]))*100.
     swap_pt2_dift=REFORM(OPEN_AND_EXTRACT(swap_pt2_infile,'dift',$
                                        offset=[0,0,0,0],count=[1,1,kpp_nz,kpp_nt]))*100.
     
     ctl_pt1_rig=REFORM(OPEN_AND_EXTRACT(ctl_pt1_infile,'Rig',$
                                       offset=[0,0,0,0],count=[1,1,kpp_nz,kpp_nt]))
     ctl_pt1_dbloc=REFORM(OPEN_AND_EXTRACT(ctl_pt1_infile,'dbloc',$
                                       offset=[0,0,0,0],count=[1,1,kpp_nz,kpp_nt]))*1000.
     ctl_pt1_shsq=REFORM(OPEN_AND_EXTRACT(ctl_pt1_infile,'Shsq',$
                                           offset=[0,0,0,0],count=[1,1,kpp_nz,kpp_nt]))*1000.
     swap_pt2_rig=REFORM(OPEN_AND_EXTRACT(swap_pt2_infile,'Rig',$
                                        offset=[0,0,0,0],count=[1,1,kpp_nz,kpp_nt]))
     swap_pt2_dbloc=REFORM(OPEN_AND_EXTRACT(swap_pt2_infile,'dbloc',$
                                        offset=[0,0,0,0],count=[1,1,kpp_nz,kpp_nt]))*1000.
     swap_pt2_shsq=REFORM(OPEN_AND_EXTRACT(swap_pt2_infile,'Shsq',$
                                            offset=[0,0,0,0],count=[1,1,kpp_nz,kpp_nt]))*1000.

     ctl_pt1_hmix=REFORM(OPEN_AND_EXTRACT(ctl_pt1_infile,'hmix',$
                                       offset=[0,0,0],count=[1,1,kpp_nt]))
     swap_pt2_hmix=REFORM(OPEN_AND_EXTRACT(swap_pt2_infile,'hmix',$
                                        offset=[0,0,0],count=[1,1,kpp_nt]))

     ctl_pt1_difm_tz=fltarr(kpp_nt,kpp_nz)
     ctl_pt1_dift_tz=fltarr(kpp_nt,kpp_nz)
     ctl_pt1_rig_tz=fltarr(kpp_nt,kpp_nz)
     ctl_pt1_dbloc_tz=fltarr(kpp_nt,kpp_nz)
     ctl_pt1_shsq_tz=fltarr(kpp_nt,kpp_nz)
     swap_pt2_difm_tz=fltarr(kpp_nt,kpp_nz)
     swap_pt2_dift_tz=fltarr(kpp_nt,kpp_nz)     
     swap_pt2_rig_tz=fltarr(kpp_nt,kpp_nz)
     swap_pt2_dbloc_tz=fltarr(kpp_nt,kpp_nz)
     swap_pt2_shsq_tz=fltarr(kpp_nt,kpp_nz)
     FOR j=0,kpp_nt-1 DO BEGIN
        ctl_pt1_difm_tz(j,*)=ctl_pt1_difm(*,j)
        ctl_pt1_dift_tz(j,*)=ctl_pt1_dift(*,j)
        ctl_pt1_rig_tz(j,*)=ctl_pt1_rig(*,j)
        ctl_pt1_dbloc_tz(j,*)=ctl_pt1_dbloc(*,j)
        ctl_pt1_shsq_tz(j,*)=ctl_pt1_shsq(*,j)
        swap_pt2_difm_tz(j,*)=swap_pt2_difm(*,j)
        swap_pt2_dift_tz(j,*)=swap_pt2_dift(*,j)        
        swap_pt2_rig_tz(j,*)=swap_pt2_rig(*,j)
        swap_pt2_dbloc_tz(j,*)=swap_pt2_dbloc(*,j)
        swap_pt2_shsq_tz(j,*)=swap_pt2_shsq(*,j)
     ENDFOR

     psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes_difm.difm_dift.'+$
            pt1_str+'_ctlflux.'+runs(i)+'.'+kpp_time+'.ps'
     PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
     CS,SCALE=34,NCOLS=N_ELEMENTS(mylevs_difm)+1
     LEVS,MANUAL=mylevs_difm
     GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMAX=MAX(kpp_z),YMIN=-100.
     CON,X=kpp_t,Y=kpp_z,FIELD=ctl_pt1_difm_tz,$
         TITLE='difm (colors) and dift (lines) fields from '+runs(i)+' at pt '+pt1_str+' with '+pt1_str+' fluxes',/NOLINES
     LEVS,MANUAL=mylevs_dift
     CON,X=kpp_t,Y=kpp_z,FIELD=ctl_pt1_dift_tz,/NOFILL,NEGATIVE_STYLE=2
     GPLOT,X=kpp_t,Y=-1.*(ctl_pt1_hmix),STYLE=1,COL=FSC_COLOR('pink')
     AXES,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='Depth',XSTEP=1,XMINOR=0.125,YSTEP=10,YMINOR=5
     PSCLOSE,/NOVIEW

     psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes_difm.difm_dift.'+$
            pt1_str+'_swapflux.'+runs(i)+'.'+kpp_time+'.ps'
     PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
     CS,SCALE=34,NCOLS=N_ELEMENTS(mylevs_difm)+1
     LEVS,MANUAL=mylevs_difm
     GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMAX=MAX(kpp_z),YMIN=-100.     
     CON,X=kpp_t,Y=kpp_z,FIELD=swap_pt2_difm_tz,$
         TITLE='difm (colors) and dift (lines) fields from '+runs(i)+' at pt '+pt2_str+' with '+pt1_str+' fluxes',/NOLINES
     LEVS,MANUAL=mylevs_dift
     CON,X=kpp_t,Y=kpp_z,FIELD=swap_pt2_dift_tz,/NOFILL,NEGATIVE_STYLE=2
     GPLOT,X=kpp_t,Y=-1.*(swap_pt2_hmix),STYLE=1,COL=FSC_COLOR('pink')
     AXES,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='Depth',XSTEP=1,XMINOR=0.125,YSTEP=10,YMINOR=5
     PSCLOSE,/NOVIEW

     psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes_difm.Rig_dbloc.'+$
            pt1_str+'_ctlflux.'+runs(i)+'.'+kpp_time+'.ps'
     PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
     CS,SCALE=34,NCOLS=N_ELEMENTS(mylevs_rig)+1
     LEVS,MANUAL=mylevs_rig
     GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMAX=MAX(kpp_z),YMIN=-100.     
     CON,X=kpp_t,Y=kpp_z,FIELD=ctl_pt1_rig_tz,$
         TITLE='Local Richardson number (colors) and local delta buoyancy (lines) from '+runs(i)+' at pt '+pt1_str+' with '+pt1_str+' fluxes',/NOLINES
     LEVS,MANUAL=mylevs_dbloc
     CON,X=kpp_t,Y=kpp_z,FIELD=ctl_pt1_dbloc_tz,/NOFILL,NEGATIVE_STYLE=2
     GPLOT,X=kpp_t,Y=-1.*(ctl_pt1_hmix),STYLE=1,COL=FSC_COLOR('pink')
     AXES,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='Depth',XSTEP=1,XMINOR=0.125,YSTEP=10,YMINOR=5
     PSCLOSE,/NOVIEW

     psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes_difm.Rig_dbloc.'+$
            pt1_str+'_swapflux.'+runs(i)+'.'+kpp_time+'.ps'
     PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
     CS,SCALE=34,NCOLS=N_ELEMENTS(mylevs_rig)+1
     LEVS,MANUAL=mylevs_rig
     GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMAX=MAX(kpp_z),YMIN=-100.     
     CON,X=kpp_t,Y=kpp_z,FIELD=swap_pt2_rig_tz,$
         TITLE='Local Richardson number (colors) and local delta buoyancy (lines) from '+runs(i)+' at pt '+pt2_str+' with '+pt1_str+' fluxes',/NOLINES
     LEVS,MANUAL=mylevs_dbloc
     CON,X=kpp_t,Y=kpp_z,FIELD=swap_pt2_dbloc_tz,/NOFILL,NEGATIVE_STYLE=2
     GPLOT,X=kpp_t,Y=-1.*(swap_pt2_hmix),STYLE=1,COL=FSC_COLOR('pink')
     AXES,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='Depth',XSTEP=1,XMINOR=0.125,YSTEP=10,YMINOR=5
     PSCLOSE,/NOVIEW

     psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes_difm.Rig_shsq.'+$
            pt1_str+'_ctlflux.'+runs(i)+'.'+kpp_time+'.ps'
     PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
     CS,SCALE=34,NCOLS=N_ELEMENTS(mylevs_rig)+1
     LEVS,MANUAL=mylevs_rig
     GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMAX=MAX(kpp_z),YMIN=-100.     
     CON,X=kpp_t,Y=kpp_z,FIELD=ctl_pt1_rig_tz,$
         TITLE='Local Richardson number (colors) and local shear-squared term (lines) from '+runs(i)+' at pt '+pt1_str+' with '+pt1_str+' fluxes',/NOLINES
     LEVS,MANUAL=mylevs_shsq
     CON,X=kpp_t,Y=kpp_z,FIELD=ctl_pt1_shsq_tz,/NOFILL,NEGATIVE_STYLE=2
     GPLOT,X=kpp_t,Y=-1.*(ctl_pt1_hmix),STYLE=1,COL=FSC_COLOR('pink')
     AXES,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='Depth',XSTEP=1,XMINOR=0.125,YSTEP=10,YMINOR=5
     PSCLOSE,/NOVIEW

     psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes_difm.Rig_shsq.'+$
            pt1_str+'_swapflux.'+runs(i)+'.'+kpp_time+'.ps'
     PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
     CS,SCALE=34,NCOLS=N_ELEMENTS(mylevs_rig)+1
     LEVS,MANUAL=mylevs_rig
     GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMAX=MAX(kpp_z),YMIN=-100.     
     CON,X=kpp_t,Y=kpp_z,FIELD=swap_pt2_rig_tz,$
         TITLE='Local Richardson number (colors) and local shear-squared term (lines) from '+runs(i)+' at pt '+pt2_str+' with '+pt1_str+' fluxes',/NOLINES
     LEVS,MANUAL=mylevs_shsq
     CON,X=kpp_t,Y=kpp_z,FIELD=swap_pt2_shsq_tz,/NOFILL,NEGATIVE_STYLE=2
     GPLOT,X=kpp_t,Y=-1.*(swap_pt2_hmix),STYLE=1,COL=FSC_COLOR('pink')
     AXES,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='Depth',XSTEP=1,XMINOR=0.125,YSTEP=10,YMINOR=5
     PSCLOSE,/NOVIEW
     
     psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes_difm.dbloc_diff.'+$
            pt1_str+'_ctlflux.'+runs(i)+'.'+kpp_time+'.ps'
     PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
     CS,SCALE=34,NCOLS=N_ELEMENTS(mylevs_dbloc_diff)+1
     LEVS,MANUAL=mylevs_dbloc_diff
     GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMAX=MAX(kpp_z),YMIN=-100.     
     CON,X=kpp_t,Y=kpp_z,FIELD=ctl_pt1_dbloc_tz-swap_pt2_dbloc_tz,$
         TITLE='Difference in local delta buoyancy from '+runs(i)+' at pt '+pt1_str+' minus '+pt2_str+' - both with '+pt1_str+' fluxes',/NOLINES
;     LEVS,MANUAL=mylevs_shsq
;     CON,X=kpp_t,Y=kpp_z,FIELD=ctl_pt1_dbloc_tz-swap_pt2_dbloc_tz,/NOFILL,NEGATIVE_STYLE=2
     GPLOT,X=kpp_t,Y=-1.*(swap_pt2_hmix),COL=FSC_COLOR('blue')
     GPLOT,X=kpp_t,Y=-1.*(ctl_pt1_hmix),COL=FSC_COLOR('black')
     AXES,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='Depth',XSTEP=1,XMINOR=0.125,YSTEP=10,YMINOR=5
     PSCLOSE

     psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes_difm.shsq_diff.'+$
            pt1_str+'_ctlflux.'+runs(i)+'.'+kpp_time+'.ps'
     PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
     CS,SCALE=34,NCOLS=N_ELEMENTS(mylevs_shsq_diff)+1
     LEVS,MANUAL=mylevs_shsq_diff
     GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMAX=MAX(kpp_z),YMIN=-100.     
     CON,X=kpp_t,Y=kpp_z,FIELD=ctl_pt1_shsq_tz-swap_pt2_shsq_tz,$
         TITLE='Difference in local shear-squared term from '+runs(i)+' at pt '+pt1_str+' minus '+pt2_str+' - both with '+pt1_str+' fluxes',/NOLINES
;     LEVS,MANUAL=mylevs_shsq
;     CON,X=kpp_t,Y=kpp_z,FIELD=ctl_pt1_shsq_tz-swap_pt2_shsq_tz,/NOFILL,NEGATIVE_STYLE=2
     GPLOT,X=kpp_t,Y=-1.*(swap_pt2_hmix),COL=FSC_COLOR('blue')
     GPLOT,X=kpp_t,Y=-1.*(ctl_pt1_hmix),COL=FSC_COLOR('black')
     AXES,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='Depth',XSTEP=1,XMINOR=0.125,YSTEP=10,YMINOR=5
     PSCLOSE

     psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes_difm.difm_diff.'+$
            pt1_str+'_swapflux.'+runs(i)+'.'+kpp_time+'.ps'
     PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
     CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_difm_diff)+1
     LEVS,MANUAL=mylevs_difm_diff
     GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMAX=MAX(kpp_z),YMIN=-100.
     CON,X=kpp_t,Y=kpp_z,FIELD=ctl_pt1_difm_tz-swap_pt2_difm_tz,$
         TITLE='Diff in difm from '+runs(i)+' at pt '+pt1_str+' minus '+pt2_str+' - both with '+pt1_str+' fluxes',/NOLINES
     ;CON,X=kpp_t,Y=kpp_z,FIELD=swap_pt2_dift_tz,/NOFILL,NEGATIVE_STYLE=2
     GPLOT,X=kpp_t,Y=-1.*(swap_pt2_hmix),COL=FSC_COLOR('blue')
     GPLOT,X=kpp_t,Y=-1.*(ctl_pt1_hmix),COL=FSC_COLOR('black')
     AXES,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='Depth',XSTEP=1,XMINOR=0.125,YSTEP=10,YMINOR=5
     PSCLOSE

     psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes_difm.dift_diff.'+$
            pt1_str+'_swapflux.'+runs(i)+'.'+kpp_time+'.ps'
     PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
     CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_difm_diff)+1
     LEVS,MANUAL=mylevs_difm_diff
     GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMAX=MAX(kpp_z),YMIN=-100.
     CON,X=kpp_t,Y=kpp_z,FIELD=ctl_pt1_dift_tz-swap_pt2_dift_tz,$
         TITLE='Diff in dift from '+runs(i)+' at pt '+pt1_str+' minus '+pt2_str+' - both with '+pt1_str+' fluxes',/NOLINES
     ;CON,X=kpp_t,Y=kpp_z,FIELD=swap_pt2_dift_tz,/NOFILL,NEGATIVE_STYLE=2
     GPLOT,X=kpp_t,Y=-1.*(swap_pt2_hmix),COL=FSC_COLOR('blue')
     GPLOT,X=kpp_t,Y=-1.*(ctl_pt1_hmix),COL=FSC_COLOR('black')
     AXES,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='Depth',XSTEP=1,XMINOR=0.125,YSTEP=10,YMINOR=5
     PSCLOSE

  ENDFOR
  STOP

END

