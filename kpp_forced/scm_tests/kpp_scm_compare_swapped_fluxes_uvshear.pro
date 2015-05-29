PRO kpp_scm_compare_swapped_fluxes_uvshear

  ; Compare runs with swapped gridpoint fluxes

;  ctl_basedir='/export/mango/data-10/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_scm'
  ctl_basedir='/export/mango/data-10/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_scm_coriol25'
  swap_basedir='/export/mango/data-10/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_scm_swapflux'
  
;  runs=['xgspj_i2','xgspj_i3','xgspj_i4','xgspj_i5']
  runs=['xgspj_i2']
  n_runs=N_ELEMENTS(runs)
  kpp_time='0045'

  mylevs_UV=['-1.9','-1.7','-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1',$
             '0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5','1.7','1.9']
  mylevs_shear=['0.00','0.01','0.02','0.03','0.04','0.05','0.06','0.07','0.08','0.09','0.10','0.11','0.12','0.13','0.14','0.15','0.16']
  mylevs_shear_diff=['-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15']

  n_pts=2
  pt1_str='0N60E'
  pt2_str='2.5S60E'

  FOR i=0,n_runs-1 DO BEGIN
;     ctl_pt1_infile=ctl_basedir+'/'+runs(i)+'/KPPocean_'+kpp_time+'.'+pt1_str+'.nc'
     ctl_pt1_infile=ctl_basedir+'/'+runs(i)+'/KPPocean_'+kpp_time+'.nc'
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

     ctl_pt1_u=REFORM(OPEN_AND_EXTRACT(ctl_pt1_infile,'u',$
                                       offset=[0,0,0,0],count=[1,1,kpp_nz,kpp_nt]))
     ctl_pt1_v=REFORM(OPEN_AND_EXTRACT(ctl_pt1_infile,'v',$
                                       offset=[0,0,0,0],count=[1,1,kpp_nz,kpp_nt]))
     swap_pt2_u=REFORM(OPEN_AND_EXTRACT(swap_pt2_infile,'u',$
                                        offset=[0,0,0,0],count=[1,1,kpp_nz,kpp_nt]))
     swap_pt2_v=REFORM(OPEN_AND_EXTRACT(swap_pt2_infile,'v',$
                                        offset=[0,0,0,0],count=[1,1,kpp_nz,kpp_nt]))

     ctl_pt1_hmix=REFORM(OPEN_AND_EXTRACT(ctl_pt1_infile,'hmix',$
                                       offset=[0,0,0],count=[1,1,kpp_nt]))
     swap_pt2_hmix=REFORM(OPEN_AND_EXTRACT(swap_pt2_infile,'hmix',$
                                        offset=[0,0,0],count=[1,1,kpp_nt]))

     ctl_pt1_u_tz=fltarr(kpp_nt,kpp_nz)
     ctl_pt1_v_tz=fltarr(kpp_nt,kpp_nz)
     swap_pt2_u_tz=fltarr(kpp_nt,kpp_nz)
     swap_pt2_v_tz=fltarr(kpp_nt,kpp_nz)
     FOR j=0,kpp_nt-1 DO BEGIN
        ctl_pt1_u_tz(j,*)=ctl_pt1_u(*,j)
        ctl_pt1_v_tz(j,*)=ctl_pt1_v(*,j)
        swap_pt2_u_tz(j,*)=swap_pt2_u(*,j)
        swap_pt2_v_tz(j,*)=swap_pt2_v(*,j)
     ENDFOR

     ctl_pt1_shear_tz=fltarr(kpp_nt,kpp_nz-1)
     swap_pt2_shear_tz=fltarr(kpp_nt,kpp_nz-1)
     FOR j=0,kpp_nt-1 DO BEGIN
        FOR k=0,kpp_nz-2 DO BEGIN
           ctl_pt1_shear_tz(j,k)=SQRT((ctl_pt1_u_tz(j,k)-(ctl_pt1_u_tz(j,k+1)))^2+$
                                      (ctl_pt1_v_tz(j,k)-(ctl_pt1_v_tz(j,k+1)))^2)
           swap_pt2_shear_tz(j,k)=SQRT((swap_pt2_u_tz(j,k)-(swap_pt2_u_tz(j,k+1)))^2+$
                                       (swap_pt2_v_tz(j,k)-(swap_pt2_v_tz(j,k+1)))^2)
        ENDFOR
     ENDFOR
     
     psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes_uvshear.UV.'+$
            pt1_str+'_ctlflux.'+runs(i)+'.'+kpp_time+'.ps'
     PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
     CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_UV)+1
     LEVS,MANUAL=mylevs_UV
     GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMAX=MAX(kpp_z),YMIN=-100.
     CON,X=kpp_t,Y=kpp_z,FIELD=ctl_pt1_u_tz,$
         TITLE='U (colors) and V (lines) fields from '+runs(i)+' at pt '+pt1_str+' with '+pt1_str+' fluxes',/NOLINES
     CON,X=kpp_t,Y=kpp_z,FIELD=ctl_pt1_v_tz,/NOFILL,NEGATIVE_STYLE=2
     GPLOT,X=kpp_t,Y=-1.*(ctl_pt1_hmix),STYLE=1,COL=FSC_COLOR('pink')
     AXES,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='Depth',XSTEP=1,XMINOR=0.125,YSTEP=10,YMINOR=5
     PSCLOSE,/NOVIEW

     psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes_uvshear.UV.'+$
            pt1_str+'_swapflux.'+runs(i)+'.'+kpp_time+'.ps'
     PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
     CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_UV)+1
     LEVS,MANUAL=mylevs_UV
     GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMAX=MAX(kpp_z),YMIN=-100.
     CON,X=kpp_t,Y=kpp_z,FIELD=swap_pt2_u_tz,$
         TITLE='U (colors) and V (lines) fields from '+runs(i)+' at pt '+pt2_str+' with '+pt1_str+' fluxes',/NOLINES
     CON,X=kpp_t,Y=kpp_z,FIELD=swap_pt2_v_tz,/NOFILL,NEGATIVE_STYLE=2
     GPLOT,X=kpp_t,Y=-1.*(swap_pt2_hmix),STYLE=1,COL=FSC_COLOR('pink')
     AXES,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='Depth',XSTEP=1,XMINOR=0.125,YSTEP=10,YMINOR=5
     PSCLOSE,/NOVIEW
     
     psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes_uvshear.UV_shear.'+$
            pt1_str+'_ctlflux.'+runs(i)+'.'+kpp_time+'.ps'
     PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
     CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_shear)+1
     LEVS,MANUAL=mylevs_shear
     GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMAX=MAX(kpp_z),YMIN=-100.
     CON,X=kpp_t,Y=kpp_z(0:kpp_nz-2),FIELD=ctl_pt1_shear_tz,$
         TITLE='Local vertical shear from '+runs(i)+' at pt '+pt1_str+' with '+pt1_str+' fluxes',/NOLINES
     ;CON,X=kpp_t,Y=kpp_z,FIELD=ctl_pt1_v_tz,/NOFILL,NEGATIVE_STYLE=2
     GPLOT,X=kpp_t,Y=-1.*(ctl_pt1_hmix),STYLE=1,COL=FSC_COLOR('pink')
     AXES,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='Depth',XSTEP=1,XMINOR=0.125,YSTEP=10,YMINOR=5
     PSCLOSE,/NOVIEW

     psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes_uvshear.UV_shear.'+$
            pt1_str+'_swapflux.'+runs(i)+'.'+kpp_time+'.ps'
     PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
     CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_shear)+1
     LEVS,MANUAL=mylevs_shear
     GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMAX=MAX(kpp_z),YMIN=-100.
     CON,X=kpp_t,Y=kpp_z(0:kpp_nz-2),FIELD=swap_pt2_shear_tz,$
         TITLE='Local vertical shear from '+runs(i)+' at pt '+pt2_str+' with '+pt1_str+' fluxes',/NOLINES
     ;CON,X=kpp_t,Y=kpp_z,FIELD=swap_pt2_v_tz,/NOFILL,NEGATIVE_STYLE=2
     GPLOT,X=kpp_t,Y=-1.*(swap_pt2_hmix),STYLE=1,COL=FSC_COLOR('pink')
     AXES,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='Depth',XSTEP=1,XMINOR=0.125,YSTEP=10,YMINOR=5
     PSCLOSE,/NOVIEW

     psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_compare_swapped_fluxes_uvshear.UV_shear_diff.'+$
            pt1_str+'_swapflux.'+runs(i)+'.'+kpp_time+'.ps'
     PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=800,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=500
     CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_shear_diff)+1
     LEVS,MANUAL=mylevs_shear_diff
     GSET,XMIN=MIN(kpp_t),XMAX=MAX(kpp_t),YMAX=MAX(kpp_z),YMIN=-100.
     CON,X=kpp_t,Y=kpp_z(0:kpp_nz-2),FIELD=ctl_pt1_shear_tz-swap_pt2_shear_tz,$
         TITLE='Diff in local vertical shear from '+runs(i)+' at pt '+pt1_str+' minus '+pt2_str+' - both with '+pt1_str+' fluxes',/NOLINES
     ;CON,X=kpp_t,Y=kpp_z,FIELD=swap_pt2_v_tz,/NOFILL,NEGATIVE_STYLE=2
     GPLOT,X=kpp_t,Y=-1.*(swap_pt2_hmix),COL=FSC_COLOR('blue')
     GPLOT,X=kpp_t,Y=-1.*(ctl_pt1_hmix),COL=FSC_COLOR('black')
     AXES,XTITLE='Time in KPP integration (days, 15 = start)',YTITLE='Depth',XSTEP=1,XMINOR=0.125,YSTEP=10,YMINOR=5
     PSCLOSE

  ENDFOR
  STOP

END

