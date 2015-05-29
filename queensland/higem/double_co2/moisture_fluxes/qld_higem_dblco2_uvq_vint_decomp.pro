PRO qld_higem_dblco2_uvq_vint_decomp

; Input directories
higem_ctl_basedir='/home/ss901165/higem_qccce/es_control_eafeb'
higem_stbl_basedir='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu'

n_seasons=5

box=[-40,90,10,180]
mag_levs=['5','15','25','35','45','55','65','75','85','95','105','115','125','135','145','155','165']
diff_u_mag_levs=['-15','-13','-11','-9','-7','-5','-3','-1','1','3','5','7','9','11','13','15']
diff_v_mag_levs=['-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5']

FOR i=0,n_seasons-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         ctl_uq_infile=higem_ctl_basedir+'/higem_eafeb.dec-feb_smean_clim.h9-w8.uq.pac_domain.nc'
         ctl_q_infile=higem_ctl_basedir+'/higem_eafeb.dec-feb_smean_clim.h9-w8.q.pac_domain.nc'
         ctl_u_infile=higem_ctl_basedir+'/higem_eafeb.dec-feb_smean_clim.h9-w8.u.pac_domain.nc'
         ctl_vq_infile=higem_ctl_basedir+'/higem_eafeb.dec-feb_smean_clim.h9-w8.vq.pac_domain.nc'
         ctl_v_infile=higem_ctl_basedir+'/higem_eafeb.dec-feb_smean_clim.h9-w8.v.pac_domain.nc'
         stbl_uq_infile=higem_stbl_basedir+'/higem_eadwu.dec-feb_smean_clim.o2-r3.uq.pac_domain.nc'
         stbl_q_infile=higem_stbl_basedir+'/higem_eadwu.dec-feb_smean_clim.o2-r3.q.pac_domain.nc'
         stbl_u_infile=higem_stbl_basedir+'/higem_eadwu.dec-feb_smean_clim.o2-r3.u.pac_domain.nc'
         stbl_vq_infile=higem_stbl_basedir+'/higem_eadwu.dec-feb_smean_clim.o2-r3.vq.pac_domain.nc'
         stbl_v_infile=higem_stbl_basedir+'/higem_eadwu.dec-feb_smean_clim.o2-r3.v.pac_domain.nc'
         season_name='dec-feb'
      END
      1 : BEGIN
         ctl_uq_infile=higem_ctl_basedir+'/higem_eafeb.mar-may_smean_clim.h9-w8.uq.pac_domain.nc'
         ctl_q_infile=higem_ctl_basedir+'/higem_eafeb.mar-may_smean_clim.h9-w8.q.pac_domain.nc'
         ctl_u_infile=higem_ctl_basedir+'/higem_eafeb.mar-may_smean_clim.h9-w8.u.pac_domain.nc'
         ctl_vq_infile=higem_ctl_basedir+'/higem_eafeb.mar-may_smean_clim.h9-w8.vq.pac_domain.nc'
         ctl_v_infile=higem_ctl_basedir+'/higem_eafeb.mar-may_smean_clim.h9-w8.v.pac_domain.nc'
         stbl_uq_infile=higem_stbl_basedir+'/higem_eadwu.mar-may_smean_clim.o2-r3.uq.pac_domain.nc'
         stbl_q_infile=higem_stbl_basedir+'/higem_eadwu.mar-may_smean_clim.o2-r3.q.pac_domain.nc'
         stbl_u_infile=higem_stbl_basedir+'/higem_eadwu.mar-may_smean_clim.o2-r3.u.pac_domain.nc'
         stbl_vq_infile=higem_stbl_basedir+'/higem_eadwu.mar-may_smean_clim.o2-r3.vq.pac_domain.nc'
         stbl_v_infile=higem_stbl_basedir+'/higem_eadwu.mar-may_smean_clim.o2-r3.v.pac_domain.nc'
         season_name='mar-may'
      END
      2 : BEGIN
         ctl_uq_infile=higem_ctl_basedir+'/higem_eafeb.jun-aug_smean_clim.h9-w8.uq.pac_domain.nc'
         ctl_q_infile=higem_ctl_basedir+'/higem_eafeb.jun-aug_smean_clim.h9-w8.q.pac_domain.nc'
         ctl_u_infile=higem_ctl_basedir+'/higem_eafeb.jun-aug_smean_clim.h9-w8.u.pac_domain.nc'
         ctl_vq_infile=higem_ctl_basedir+'/higem_eafeb.jun-aug_smean_clim.h9-w8.vq.pac_domain.nc'
         ctl_v_infile=higem_ctl_basedir+'/higem_eafeb.jun-aug_smean_clim.h9-w8.v.pac_domain.nc'
         stbl_uq_infile=higem_stbl_basedir+'/higem_eadwu.jun-aug_smean_clim.o2-r3.uq.pac_domain.nc'
         stbl_q_infile=higem_stbl_basedir+'/higem_eadwu.jun-aug_smean_clim.o2-r3.q.pac_domain.nc'
         stbl_u_infile=higem_stbl_basedir+'/higem_eadwu.jun-aug_smean_clim.o2-r3.u.pac_domain.nc'
         stbl_vq_infile=higem_stbl_basedir+'/higem_eadwu.jun-aug_smean_clim.o2-r3.vq.pac_domain.nc'
         stbl_v_infile=higem_stbl_basedir+'/higem_eadwu.jun-aug_smean_clim.o2-r3.v.pac_domain.nc'
         season_name='jun-aug'
      END
      3 : BEGIN
         ctl_uq_infile=higem_ctl_basedir+'/higem_eafeb.sep-nov_smean_clim.h9-w8.uq.pac_domain.nc'
         ctl_q_infile=higem_ctl_basedir+'/higem_eafeb.sep-nov_smean_clim.h9-w8.q.pac_domain.nc'
         ctl_u_infile=higem_ctl_basedir+'/higem_eafeb.sep-nov_smean_clim.h9-w8.u.pac_domain.nc'
         ctl_vq_infile=higem_ctl_basedir+'/higem_eafeb.sep-nov_smean_clim.h9-w8.vq.pac_domain.nc'
         ctl_v_infile=higem_ctl_basedir+'/higem_eafeb.sep-nov_smean_clim.h9-w8.v.pac_domain.nc'
         stbl_uq_infile=higem_stbl_basedir+'/higem_eadwu.sep-nov_smean_clim.o2-r3.uq.pac_domain.nc'
         stbl_q_infile=higem_stbl_basedir+'/higem_eadwu.sep-nov_smean_clim.o2-r3.q.pac_domain.nc'
         stbl_u_infile=higem_stbl_basedir+'/higem_eadwu.sep-nov_smean_clim.o2-r3.u.pac_domain.nc'
         stbl_vq_infile=higem_stbl_basedir+'/higem_eadwu.sep-nov_smean_clim.o2-r3.vq.pac_domain.nc'
         stbl_v_infile=higem_stbl_basedir+'/higem_eadwu.sep-nov_smean_clim.o2-r3.v.pac_domain.nc'
         season_name='sep-nov'
      END
      4 : BEGIN
         ctl_uq_infile=higem_ctl_basedir+'/higem_eafeb.may-apr_smean_clim.h9-w8.uq.pac_domain.nc'
         ctl_q_infile=higem_ctl_basedir+'/higem_eafeb.may-apr_smean_clim.h9-w8.q.pac_domain.nc'
         ctl_u_infile=higem_ctl_basedir+'/higem_eafeb.may-apr_smean_clim.h9-w8.u.pac_domain.nc'
         ctl_vq_infile=higem_ctl_basedir+'/higem_eafeb.may-apr_smean_clim.h9-w8.vq.pac_domain.nc'
         ctl_v_infile=higem_ctl_basedir+'/higem_eafeb.may-apr_smean_clim.h9-w8.v.pac_domain.nc'
         stbl_uq_infile=higem_stbl_basedir+'/higem_eadwu.may-apr_smean_clim.o2-r3.uq.pac_domain.nc'
         stbl_q_infile=higem_stbl_basedir+'/higem_eadwu.may-apr_smean_clim.o2-r3.q.pac_domain.nc'
         stbl_u_infile=higem_stbl_basedir+'/higem_eadwu.may-apr_smean_clim.o2-r3.u.pac_domain.nc'
         stbl_vq_infile=higem_stbl_basedir+'/higem_eadwu.may-apr_smean_clim.o2-r3.vq.pac_domain.nc'
         stbl_v_infile=higem_stbl_basedir+'/higem_eadwu.may-apr_smean_clim.o2-r3.v.pac_domain.nc'
         season_name='may-apr'
      END
   ENDCASE

   ctl_uq_longitude=OPEN_AND_EXTRACT(ctl_uq_infile,'longitude')
   ctl_uq_latitude=OPEN_AND_EXTRACT(ctl_uq_infile,'latitude_1')
   DEFINE_BOUNDARIES,box,ctl_uq_latitude,ctl_uq_longitude,ctl_uq_box_tx,/LIMIT
   ctl_uq_nlon=N_ELEMENTS(ctl_uq_longitude)
   ctl_uq_nlat=N_ELEMENTS(ctl_uq_latitude)

   ctl_u_longitude=OPEN_AND_EXTRACT(ctl_u_infile,'longitude')
   ctl_u_latitude=OPEN_AND_EXTRACT(ctl_u_infile,'latitude_1')
   DEFINE_BOUNDARIES,box,ctl_u_latitude,ctl_u_longitude,ctl_u_box_tx,/LIMIT
   ctl_u_nlon=N_ELEMENTS(ctl_u_longitude)
   ctl_u_nlat=N_ELEMENTS(ctl_u_latitude)

   ctl_v_longitude=OPEN_AND_EXTRACT(ctl_v_infile,'longitude')
   ctl_v_latitude=OPEN_AND_EXTRACT(ctl_v_infile,'latitude_1')
   DEFINE_BOUNDARIES,box,ctl_v_latitude,ctl_v_longitude,ctl_v_box_tx,/LIMIT
   ctl_v_nlon=N_ELEMENTS(ctl_v_longitude)
   ctl_v_nlat=N_ELEMENTS(ctl_v_latitude)

   ctl_q_longitude=OPEN_AND_EXTRACT(ctl_q_infile,'longitude')
   ctl_q_latitude=OPEN_AND_EXTRACT(ctl_q_infile,'latitude_1')
   DEFINE_BOUNDARIES,box,ctl_q_latitude,ctl_q_longitude,ctl_q_box_tx,/LIMIT
   ctl_q_nlon=N_ELEMENTS(ctl_q_longitude)
   ctl_q_nlat=N_ELEMENTS(ctl_q_latitude)

   ctl_vq_longitude=OPEN_AND_EXTRACT(ctl_vq_infile,'longitude')
   ctl_vq_latitude=OPEN_AND_EXTRACT(ctl_vq_infile,'latitude_1')
   DEFINE_BOUNDARIES,box,ctl_vq_latitude,ctl_vq_longitude,ctl_vq_box_tx,/LIMIT
   ctl_vq_nlon=N_ELEMENTS(ctl_vq_longitude)
   ctl_vq_nlat=N_ELEMENTS(ctl_vq_latitude)

   stbl_uq_longitude=OPEN_AND_EXTRACT(stbl_uq_infile,'longitude')
   stbl_uq_latitude=OPEN_AND_EXTRACT(stbl_uq_infile,'latitude_1')
   DEFINE_BOUNDARIES,box,stbl_uq_latitude,stbl_uq_longitude,stbl_uq_box_tx,/LIMIT
   stbl_uq_nlon=N_ELEMENTS(stbl_uq_longitude)
   stbl_uq_nlat=N_ELEMENTS(stbl_uq_latitude)

   stbl_vq_longitude=OPEN_AND_EXTRACT(stbl_vq_infile,'longitude')
   stbl_vq_latitude=OPEN_AND_EXTRACT(stbl_vq_infile,'latitude_1')
   DEFINE_BOUNDARIES,box,stbl_vq_latitude,stbl_vq_longitude,stbl_vq_box_tx,/LIMIT
   stbl_vq_nlon=N_ELEMENTS(stbl_vq_longitude)
   stbl_vq_nlat=N_ELEMENTS(stbl_vq_latitude)

   stbl_u_longitude=OPEN_AND_EXTRACT(stbl_u_infile,'longitude')
   stbl_u_latitude=OPEN_AND_EXTRACT(stbl_u_infile,'latitude_1')
   DEFINE_BOUNDARIES,box,stbl_u_latitude,stbl_u_longitude,stbl_u_box_tx,/LIMIT
   stbl_u_nlon=N_ELEMENTS(stbl_u_longitude)
   stbl_u_nlat=N_ELEMENTS(stbl_u_latitude)

   stbl_q_longitude=OPEN_AND_EXTRACT(stbl_q_infile,'longitude')
   stbl_q_latitude=OPEN_AND_EXTRACT(stbl_q_infile,'latitude_1')
   DEFINE_BOUNDARIES,box,stbl_q_latitude,stbl_q_longitude,stbl_q_box_tx,/LIMIT
   stbl_q_nlon=N_ELEMENTS(stbl_q_longitude)
   stbl_q_nlat=N_ELEMENTS(stbl_q_latitude)

   stbl_v_longitude=OPEN_AND_EXTRACT(stbl_v_infile,'longitude')
   stbl_v_latitude=OPEN_AND_EXTRACT(stbl_v_infile,'latitude_1')
   DEFINE_BOUNDARIES,box,stbl_v_latitude,stbl_v_longitude,stbl_v_box_tx,/LIMIT
   stbl_v_nlon=N_ELEMENTS(stbl_v_longitude)
   stbl_v_nlat=N_ELEMENTS(stbl_v_latitude)

   plevs=OPEN_AND_EXTRACT(stbl_uq_infile,'p_5')
   plev_bottom=NEAREST(plevs,1000)
   plev_top=NEAREST(plevs,700)
   n_plevs=plev_top-plev_bottom+1

   ctl_uq=OPEN_AND_EXTRACT(ctl_uq_infile,'unspecified_8',$
                           offset=[ctl_uq_box_tx(1),ctl_uq_box_tx(0),plev_bottom],$
                           count=[ctl_uq_nlon,ctl_uq_nlat,n_plevs])*1000.
   ctl_vq=OPEN_AND_EXTRACT(ctl_vq_infile,'unspecified_14',$
                           offset=[ctl_vq_box_tx(1),ctl_vq_box_tx(0),plev_bottom],$
                           count=[ctl_vq_nlon,ctl_vq_nlat,n_plevs])*1000.
   ctl_u=OPEN_AND_EXTRACT(ctl_u_infile,'u_2',$
                          offset=[ctl_u_box_tx(1),ctl_u_box_tx(0),plev_bottom],$
                          count=[ctl_u_nlon,ctl_u_nlat,n_plevs])
   ctl_v=OPEN_AND_EXTRACT(ctl_v_infile,'v_2',$
                          offset=[ctl_v_box_tx(1),ctl_v_box_tx(0),plev_bottom],$
                          count=[ctl_v_nlon,ctl_v_nlat,n_plevs])
   ctl_q=OPEN_AND_EXTRACT(ctl_q_infile,'q_1',$
                          offset=[ctl_q_box_tx(1),ctl_q_box_tx(0),plev_bottom],$
                          count=[ctl_q_nlon,ctl_q_nlat,n_plevs])*1000.

   stbl_uq=OPEN_AND_EXTRACT(stbl_uq_infile,'unspecified_8',$
                            offset=[stbl_uq_box_tx(1),stbl_uq_box_tx(0),plev_bottom],$
                            count=[stbl_uq_nlon,stbl_uq_nlat,n_plevs])*1000.
   stbl_vq=OPEN_AND_EXTRACT(stbl_vq_infile,'unspecified_14',$
                            offset=[stbl_vq_box_tx(1),stbl_vq_box_tx(0),plev_bottom],$
                            count=[stbl_vq_nlon,stbl_vq_nlat,n_plevs])*1000.
   stbl_u=OPEN_AND_EXTRACT(stbl_u_infile,'u_2',$
                           offset=[stbl_u_box_tx(1),stbl_u_box_tx(0),plev_bottom],$
                           count=[stbl_u_nlon,stbl_u_nlat,n_plevs])
   stbl_v=OPEN_AND_EXTRACT(stbl_v_infile,'v_2',$
                           offset=[stbl_v_box_tx(1),stbl_v_box_tx(0),plev_bottom],$
                           count=[stbl_v_nlon,stbl_v_nlat,n_plevs])
   stbl_q=OPEN_AND_EXTRACT(stbl_q_infile,'q_1',$
                           offset=[stbl_q_box_tx(1),stbl_q_box_tx(0),plev_bottom],$
                           count=[stbl_q_nlon,stbl_q_nlat,n_plevs])*1000.

   ctl_u_vint=fltarr(ctl_u_nlon,ctl_u_nlat)
   ctl_q_vint=fltarr(ctl_q_nlon,ctl_q_nlat)
   ctl_v_vint=fltarr(ctl_v_nlon,ctl_v_nlat)
   ctl_uq_vint=fltarr(ctl_uq_nlon,ctl_uq_nlat)
   ctl_vq_vint=fltarr(ctl_vq_nlon,ctl_vq_nlat)
   stbl_u_vint=fltarr(stbl_u_nlon,stbl_u_nlat)
   stbl_q_vint=fltarr(stbl_q_nlon,stbl_q_nlat)
   stbl_v_vint=fltarr(stbl_v_nlon,stbl_v_nlat)
   stbl_uq_vint=fltarr(stbl_uq_nlon,stbl_uq_nlat)
   stbl_vq_vint=fltarr(stbl_vq_nlon,stbl_vq_nlat)

   FOR j=0,ctl_uq_nlon-1 DO BEGIN
      FOR k=0,ctl_vq_nlat-1 DO BEGIN
         FOR m=0,n_plevs-1 DO BEGIN
            ctl_uq_vint(j,k)=ctl_uq_vint(j,k)+ctl_uq(j,k,m)*plevs(m+plev_bottom)
            ctl_u_vint(j,k)=ctl_u_vint(j,k)+ctl_u(j,k,m)*plevs(m+plev_bottom) 
            ctl_v_vint(j,k)=ctl_v_vint(j,k)+ctl_v(j,k,m)*plevs(m+plev_bottom)
            ctl_q_vint(j,k)=ctl_q_vint(j,k)+ctl_q(j,k,m)*plevs(m+plev_bottom)
            ctl_vq_vint(j,k)=ctl_vq_vint(j,k)+ctl_vq(j,k,m)*plevs(m+plev_bottom)
            stbl_uq_vint(j,k)=stbl_uq_vint(j,k)+stbl_uq(j,k,m)*plevs(m+plev_bottom)
            stbl_vq_vint(j,k)=stbl_vq_vint(j,k)+stbl_vq(j,k,m)*plevs(m+plev_bottom)            
            stbl_u_vint(j,k)=stbl_u_vint(j,k)+stbl_u(j,k,m)*plevs(m+plev_bottom) 
            stbl_q_vint(j,k)=stbl_q_vint(j,k)+stbl_q(j,k,m)*plevs(m+plev_bottom)
            stbl_v_vint(j,k)=stbl_v_vint(j,k)+stbl_v(j,k,m)*plevs(m+plev_bottom)
         ENDFOR
      ENDFOR
   ENDFOR
   ctl_uq_vint=ctl_uq_vint/FLOAT(TOTAL(plevs(plev_bottom:plev_top)))
   ctl_u_vint=ctl_u_vint/FLOAT(TOTAL(plevs(plev_bottom:plev_top)))
   ctl_q_vint=ctl_q_vint/FLOAT(TOTAL(plevs(plev_bottom:plev_top)))
   ctl_vq_vint=ctl_vq_vint/FLOAT(TOTAL(plevs(plev_bottom:plev_top)))
   ctl_v_vint=ctl_v_vint/FLOAT(TOTAL(plevs(plev_bottom:plev_top)))
   stbl_uq_vint=stbl_uq_vint/FLOAT(TOTAL(plevs(plev_bottom:plev_top)))
   stbl_u_vint=stbl_u_vint/FLOAT(TOTAL(plevs(plev_bottom:plev_top)))
   stbl_q_vint=stbl_q_vint/FLOAT(TOTAL(plevs(plev_bottom:plev_top)))
   stbl_vq_vint=stbl_vq_vint/FLOAT(TOTAL(plevs(plev_bottom:plev_top)))
   stbl_v_vint=stbl_v_vint/FLOAT(TOTAL(plevs(plev_bottom:plev_top)))

   diffu_diffq=(stbl_u_vint-ctl_u_vint)*(stbl_q_vint-ctl_q_vint)
   diffabsu_diffq=(ABS(stbl_u_vint)-ABS(ctl_u_vint))*(stbl_q_vint-ctl_q_vint)
   diffu_q=(stbl_u_vint-ctl_u_vint)*ctl_q_vint
   diffabsu_q=(ABS(stbl_u_vint)-ABS(ctl_u_vint))*ctl_q_vint
   diffq_u=(stbl_q_vint-ctl_q_vint)*ctl_u_vint
   diffq_absu=(stbl_q_vint-ctl_q_vint)*ABS(ctl_u_vint)

   diffv_diffq=(stbl_v_vint-ctl_v_vint)*(stbl_q_vint-ctl_q_vint)
   diffabsv_diffq=(ABS(stbl_v_vint)-ABS(ctl_v_vint))*(stbl_q_vint-ctl_q_vint)
   diffv_q=(stbl_v_vint-ctl_v_vint)*ctl_q_vint
   diffabsv_q=(ABS(stbl_v_vint)-ABS(ctl_v_vint))*ctl_q_vint
   diffq_v=(stbl_q_vint-ctl_q_vint)*ctl_v_vint
   diffq_absv=(stbl_q_vint-ctl_q_vint)*ABS(ctl_v_vint)
   
   psfile='/home/ss901165/idl/queensland/higem/double_co2/moisture_fluxes/qld_higem_dblco2_uvq_vint_decomp.'+season_name+'_smean.diffuq_higem_stbl-minus-higem_ctl.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,SPACE3=500,XOFFSET=0,YOFFSET=100,TFONT=3,$
          TCHARSIZE=100
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
   CS,SCALE=12,NCOLS=N_ELEMENTS(diff_u_mag_levs)+1,/REV
   LEVS,MANUAL=diff_u_mag_levs
   CON,X=stbl_uq_longitude,Y=stbl_uq_latitude,FIELD=stbl_uq_vint-ctl_uq_vint,/NOLINES,$
       /BLOCK
   AXES,XSTEP=10,YSTEP=10
   PSCLOSE,/NOVIEW
  
;   psfile='/home/ss901165/idl/queensland/higem/double_co2/moisture_fluxes/qld_higem_dblco2_uvq_vint_decomp.'+season_name+'_smean.diffabsuq_higem_stbl-minus-higem_ctl.ps'
;   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,SPACE3=500,XOFFSET=0,YOFFSET=100,TFONT=3,$
;          TCHARSIZE=100
;   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
;   CS,SCALE=12,NCOLS=N_ELEMENTS(diff_u_mag_levs)+1,/REV
;   LEVS,MANUAL=diff_u_mag_levs
;   CON,X=stbl_uq_longitude,Y=stbl_uq_latitude,FIELD=ABS(stbl_uq_vint)-ABS(ctl_uq_vint),/NOLINES,$
;       /BLOCK
;   AXES,XSTEP=10,YSTEP=10
;   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/moisture_fluxes/qld_higem_dblco2_uvq_vint_decomp.'+season_name+'_smean.diffu_diffq_higem_stbl-minus-higem_ctl.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,SPACE3=500,XOFFSET=0,YOFFSET=100,TFONT=3,$
          TCHARSIZE=100
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
   CS,SCALE=12,NCOLS=N_ELEMENTS(diff_u_mag_levs)+1,/REV
   LEVS,MANUAL=diff_u_mag_levs
   CON,X=stbl_uq_longitude,Y=stbl_uq_latitude,FIELD=diffu_diffq,/NOLINES,$
       /BLOCK
   AXES,XSTEP=10,YSTEP=10
   PSCLOSE,/NOVIEW

;    psfile='/home/ss901165/idl/queensland/higem/double_co2/moisture_fluxes/qld_higem_dblco2_uvq_vint_decomp.'+season_name+'_smean.diffabsu_diffq_higem_stbl-minus-higem_ctl.ps'
;   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,SPACE3=500,XOFFSET=0,YOFFSET=100,TFONT=3,$
;          TCHARSIZE=100
;   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
;   CS,SCALE=12,NCOLS=N_ELEMENTS(diff_u_mag_levs)+1,/REV
;   LEVS,MANUAL=diff_u_mag_levs
;   CON,X=stbl_uq_longitude,Y=stbl_uq_latitude,FIELD=diffabsu_diffq,/NOLINES,$
;       /BLOCK
;   AXES,XSTEP=10,YSTEP=10
;   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/moisture_fluxes/qld_higem_dblco2_uvq_vint_decomp.'+season_name+'_smean.diffu_q_higem_stbl-minus-higem_ctl.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,SPACE3=500,XOFFSET=0,YOFFSET=100,TFONT=3,$
          TCHARSIZE=100
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
   CS,SCALE=12,NCOLS=N_ELEMENTS(diff_u_mag_levs)+1,/REV
   LEVS,MANUAL=diff_u_mag_levs
   CON,X=stbl_uq_longitude,Y=stbl_uq_latitude,FIELD=diffu_q,/NOLINES,$
       /BLOCK
   AXES,XSTEP=10,YSTEP=10
   PSCLOSE,/NOVIEW

;   psfile='/home/ss901165/idl/queensland/higem/double_co2/moisture_fluxes/qld_higem_dblco2_uvq_vint_decomp.'+season_name+'_smean.diffabsu_q_higem_stbl-minus-higem_ctl.ps'
;   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,SPACE3=500,XOFFSET=0,YOFFSET=100,TFONT=3,$
;          TCHARSIZE=100
;   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
;   CS,SCALE=12,NCOLS=N_ELEMENTS(diff_u_mag_levs)+1,/REV
;   LEVS,MANUAL=diff_u_mag_levs
;   CON,X=stbl_uq_longitude,Y=stbl_uq_latitude,FIELD=diffabsu_q,/NOLINES,$
;       /BLOCK
;   AXES,XSTEP=10,YSTEP=10
;   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/moisture_fluxes/qld_higem_dblco2_uvq_vint_decomp.'+season_name+'_smean.diffq_u_higem_stbl-minus-higem_ctl.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,SPACE3=500,XOFFSET=0,YOFFSET=100,TFONT=3,$
          TCHARSIZE=100
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
   CS,SCALE=12,NCOLS=N_ELEMENTS(diff_u_mag_levs)+1,/REV
   LEVS,MANUAL=diff_u_mag_levs
   CON,X=stbl_uq_longitude,Y=stbl_uq_latitude,FIELD=diffq_u,/NOLINES,$
       /BLOCK
   AXES,XSTEP=10,YSTEP=10
   PSCLOSE,/NOVIEW

;   psfile='/home/ss901165/idl/queensland/higem/double_co2/moisture_fluxes/qld_higem_dblco2_uvq_vint_decomp.'+season_name+'_smean.diffq_absu_higem_stbl-minus-higem_ctl.ps'
;   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,SPACE3=500,XOFFSET=0,YOFFSET=100,TFONT=3,$
;          TCHARSIZE=100
;   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
;   CS,SCALE=12,NCOLS=N_ELEMENTS(diff_u_mag_levs)+1,/REV
;   LEVS,MANUAL=diff_u_mag_levs
;   CON,X=stbl_uq_longitude,Y=stbl_uq_latitude,FIELD=diffq_absu,/NOLINES,$
;       /BLOCK
;   AXES,XSTEP=10,YSTEP=10
;   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/moisture_fluxes/qld_higem_dblco2_uvq_vint_decomp.'+season_name+'_smean.diffuq_resid_higem_stbl-minus-higem_ctl.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,SPACE3=500,XOFFSET=0,YOFFSET=100,TFONT=3,$
          TCHARSIZE=100
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
   CS,SCALE=12,NCOLS=N_ELEMENTS(diff_u_mag_levs)+1,/REV
   LEVS,MANUAL=diff_u_mag_levs
   CON,X=stbl_uq_longitude,Y=stbl_uq_latitude,FIELD=((stbl_uq_vint-ctl_uq_vint)-(diffq_u+diffu_diffq+diffu_q))*0.5,/NOLINES,$
       /BLOCK
   AXES,XSTEP=10,YSTEP=10
   PSCLOSE,/NOVIEW

;;;;;;;;;;;;;;; V CHANGES

   psfile='/home/ss901165/idl/queensland/higem/double_co2/moisture_fluxes/qld_higem_dblco2_uvq_vint_decomp.'+season_name+'_smean.diffvq_higem_stbl-minus-higem_ctl.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,SPACE3=500,XOFFSET=0,YOFFSET=100,TFONT=3,$
          TCHARSIZE=100
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
   CS,SCALE=12,NCOLS=N_ELEMENTS(diff_v_mag_levs)+1,/REV
   LEVS,MANUAL=diff_v_mag_levs
   CON,X=stbl_vq_longitude,Y=stbl_vq_latitude,FIELD=stbl_vq_vint-ctl_vq_vint,/NOLINES,$
       /BLOCK
   AXES,XSTEP=10,YSTEP=10
   PSCLOSE,/NOVIEW

;   psfile='/home/ss901165/idl/queensland/higem/double_co2/moisture_fluxes/qld_higem_dblco2_uvq_vint_decomp.'+season_name+'_smean.diffabsvq_higem_stbl-minus-higem_ctl.ps'
;   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,SPACE3=500,XOFFSET=0,YOFFSET=100,TFONT=3,$
;          TCHARSIZE=100
;   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
;   CS,SCALE=12,NCOLS=N_ELEMENTS(diff_v_mag_levs)+1,/REV
;   LEVS,MANUAL=diff_v_mag_levs
;   CON,X=stbl_vq_longitude,Y=stbl_vq_latitude,FIELD=ABS(stbl_vq_vint)-ABS(ctl_vq_vint),/NOLINES,$
;       /BLOCK
;   AXES,XSTEP=10,YSTEP=10
;   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/moisture_fluxes/qld_higem_dblco2_uvq_vint_decomp.'+season_name+'_smean.diffv_diffq_higem_stbl-minus-higem_ctl.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,SPACE3=500,XOFFSET=0,YOFFSET=100,TFONT=3,$
          TCHARSIZE=100
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
   CS,SCALE=12,NCOLS=N_ELEMENTS(diff_v_mag_levs)+1,/REV
   LEVS,MANUAL=diff_v_mag_levs
   CON,X=stbl_vq_longitude,Y=stbl_vq_latitude,FIELD=diffv_diffq,/NOLINES,$
       /BLOCK
   AXES,XSTEP=10,YSTEP=10
   PSCLOSE,/NOVIEW

;   psfile='/home/ss901165/idl/queensland/higem/double_co2/moisture_fluxes/qld_higem_dblco2_uvq_vint_decomp.'+season_name+'_smean.diffabsv_diffq_higem_stbl-minus-higem_ctl.ps'
;   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,SPACE3=500,XOFFSET=0,YOFFSET=100,TFONT=3,$
;          TCHARSIZE=100
;   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
;   CS,SCALE=12,NCOLS=N_ELEMENTS(diff_v_mag_levs)+1,/REV
;   LEVS,MANUAL=diff_v_mag_levs
;   CON,X=stbl_vq_longitude,Y=stbl_vq_latitude,FIELD=diffabsv_diffq,/NOLINES,$
;       /BLOCK
;   AXES,XSTEP=10,YSTEP=10
;   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/moisture_fluxes/qld_higem_dblco2_uvq_vint_decomp.'+season_name+'_smean.diffv_q_higem_stbl-minus-higem_ctl.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,SPACE3=500,XOFFSET=0,YOFFSET=100,TFONT=3,$
          TCHARSIZE=100
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
   CS,SCALE=12,NCOLS=N_ELEMENTS(diff_v_mag_levs)+1,/REV
   LEVS,MANUAL=diff_v_mag_levs
   CON,X=stbl_vq_longitude,Y=stbl_vq_latitude,FIELD=diffv_q,/NOLINES,$
       /BLOCK
   AXES,XSTEP=10,YSTEP=10
   PSCLOSE,/NOVIEW
   
;   psfile='/home/ss901165/idl/queensland/higem/double_co2/moisture_fluxes/qld_higem_dblco2_uvq_vint_decomp.'+season_name+'_smean.diffabsv_q_higem_stbl-minus-higem_ctl.ps'
;   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,SPACE3=500,XOFFSET=0,YOFFSET=100,TFONT=3,$
;          TCHARSIZE=100
;   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
;   CS,SCALE=12,NCOLS=N_ELEMENTS(diff_v_mag_levs)+1,/REV
;   LEVS,MANUAL=diff_v_mag_levs
;   CON,X=stbl_vq_longitude,Y=stbl_vq_latitude,FIELD=diffabsv_q,/NOLINES,$
;       /BLOCK
;   AXES,XSTEP=10,YSTEP=10
;   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/moisture_fluxes/qld_higem_dblco2_uvq_vint_decomp.'+season_name+'_smean.diffq_v_higem_stbl-minus-higem_ctl.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,SPACE3=500,XOFFSET=0,YOFFSET=100,TFONT=3,$
          TCHARSIZE=100
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
   CS,SCALE=12,NCOLS=N_ELEMENTS(diff_v_mag_levs)+1,/REV
   LEVS,MANUAL=diff_v_mag_levs
   CON,X=stbl_vq_longitude,Y=stbl_vq_latitude,FIELD=diffq_v,/NOLINES,$
       /BLOCK
   AXES,XSTEP=10,YSTEP=10
   PSCLOSE,/NOVIEW

;    psfile='/home/ss901165/idl/queensland/higem/double_co2/moisture_fluxes/qld_higem_dblco2_uvq_vint_decomp.'+season_name+'_smean.diffq_absv_higem_stbl-minus-higem_ctl.ps'
;   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,SPACE3=500,XOFFSET=0,YOFFSET=100,TFONT=3,$
;          TCHARSIZE=100
;   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
;   CS,SCALE=12,NCOLS=N_ELEMENTS(diff_v_mag_levs)+1,/REV
;   LEVS,MANUAL=diff_v_mag_levs
;   CON,X=stbl_vq_longitude,Y=stbl_vq_latitude,FIELD=diffq_absv,/NOLINES,$
;       /BLOCK
;   AXES,XSTEP=10,YSTEP=10
;   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/moisture_fluxes/qld_higem_dblco2_uvq_vint_decomp.'+season_name+'_smean.diffvq_resid_higem_stbl-minus-higem_ctl.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,SPACE3=500,XOFFSET=0,YOFFSET=100,TFONT=3,$
          TCHARSIZE=100
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
   CS,SCALE=12,NCOLS=N_ELEMENTS(diff_u_mag_levs)+1,/REV
   LEVS,MANUAL=diff_u_mag_levs
   CON,X=stbl_vq_longitude,Y=stbl_vq_latitude,FIELD=((stbl_vq_vint-ctl_vq_vint)-(diffq_v+diffv_diffq+diffv_q))*0.5,/NOLINES,$
       /BLOCK
   AXES,XSTEP=10,YSTEP=10
   PSCLOSE,/NOVIEW

ENDFOR

STOP
END
