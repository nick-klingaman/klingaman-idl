PRO qld_higem_dblco2_uvq_vint_means

; Input directories
higem_ctl_basedir='/home/ss901165/higem_qccce/es_control_eafeb'
higem_stbl_basedir='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu'

n_seasons=5

box=[-40,90,10,180]
mag_levs=['5','15','25','35','45','55','65','75','85','95','105','115','125','135','145','155','165']
diff_mag_levs=['-22','-18','-14','-10','-6','-2','2','6','10','14','18','22']

FOR i=0,n_seasons-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         ctl_uq_infile=higem_ctl_basedir+'/higem_eafeb.dec-feb_smean_clim.h9-w8.uq.pac_domain.nc'
         ctl_vq_infile=higem_ctl_basedir+'/higem_eafeb.dec-feb_smean_clim.h9-w8.vq.pac_domain.nc'
         stbl_uq_infile=higem_stbl_basedir+'/higem_eadwu.dec-feb_smean_clim.o2-r3.uq.pac_domain.nc'
         stbl_vq_infile=higem_stbl_basedir+'/higem_eadwu.dec-feb_smean_clim.o2-r3.vq.pac_domain.nc'
         season_name='dec-feb'
      END
      1 : BEGIN
         ctl_uq_infile=higem_ctl_basedir+'/higem_eafeb.mar-may_smean_clim.h9-w8.uq.pac_domain.nc'
         ctl_vq_infile=higem_ctl_basedir+'/higem_eafeb.mar-may_smean_clim.h9-w8.vq.pac_domain.nc'
         stbl_uq_infile=higem_stbl_basedir+'/higem_eadwu.mar-may_smean_clim.o2-r3.uq.pac_domain.nc'
         stbl_vq_infile=higem_stbl_basedir+'/higem_eadwu.mar-may_smean_clim.o2-r3.vq.pac_domain.nc'
         season_name='mar-may'
      END
      2 : BEGIN
         ctl_uq_infile=higem_ctl_basedir+'/higem_eafeb.jun-aug_smean_clim.h9-w8.uq.pac_domain.nc'
         ctl_vq_infile=higem_ctl_basedir+'/higem_eafeb.jun-aug_smean_clim.h9-w8.vq.pac_domain.nc'
         stbl_uq_infile=higem_stbl_basedir+'/higem_eadwu.jun-aug_smean_clim.o2-r3.uq.pac_domain.nc'
         stbl_vq_infile=higem_stbl_basedir+'/higem_eadwu.jun-aug_smean_clim.o2-r3.vq.pac_domain.nc'
         season_name='jun-aug'
      END
      3 : BEGIN
         ctl_uq_infile=higem_ctl_basedir+'/higem_eafeb.sep-nov_smean_clim.h9-w8.uq.pac_domain.nc'
         ctl_vq_infile=higem_ctl_basedir+'/higem_eafeb.sep-nov_smean_clim.h9-w8.vq.pac_domain.nc'
         stbl_uq_infile=higem_stbl_basedir+'/higem_eadwu.sep-nov_smean_clim.o2-r3.uq.pac_domain.nc'
         stbl_vq_infile=higem_stbl_basedir+'/higem_eadwu.sep-nov_smean_clim.o2-r3.vq.pac_domain.nc'
         season_name='sep-nov'
      END
      4 : BEGIN
         ctl_uq_infile=higem_ctl_basedir+'/higem_eafeb.may-apr_smean_clim.h9-w8.uq.pac_domain.nc'
         ctl_vq_infile=higem_ctl_basedir+'/higem_eafeb.may-apr_smean_clim.h9-w8.vq.pac_domain.nc'
         stbl_uq_infile=higem_stbl_basedir+'/higem_eadwu.may-apr_smean_clim.o2-r3.uq.pac_domain.nc'
         stbl_vq_infile=higem_stbl_basedir+'/higem_eadwu.may-apr_smean_clim.o2-r3.vq.pac_domain.nc'
         season_name='may-apr'
      END
   ENDCASE

   ctl_uq_longitude=OPEN_AND_EXTRACT(ctl_uq_infile,'longitude')
   ctl_uq_latitude=OPEN_AND_EXTRACT(ctl_uq_infile,'latitude_1')
   DEFINE_BOUNDARIES,box,ctl_uq_latitude,ctl_uq_longitude,ctl_uq_box_tx,/LIMIT
   ctl_uq_nlon=N_ELEMENTS(ctl_uq_longitude)
   ctl_uq_nlat=N_ELEMENTS(ctl_uq_latitude)

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
   
   stbl_uq=OPEN_AND_EXTRACT(stbl_uq_infile,'unspecified_8',$
                            offset=[stbl_uq_box_tx(1),stbl_uq_box_tx(0),plev_bottom],$
                            count=[stbl_uq_nlon,stbl_uq_nlat,n_plevs])*1000.
   stbl_vq=OPEN_AND_EXTRACT(stbl_vq_infile,'unspecified_14',$
                            offset=[stbl_vq_box_tx(1),stbl_vq_box_tx(0),plev_bottom],$
                            count=[stbl_vq_nlon,stbl_vq_nlat,n_plevs])*1000.
   
   ctl_uq_vint=fltarr(ctl_uq_nlon,ctl_uq_nlat)
   ctl_vq_vint=fltarr(ctl_vq_nlon,ctl_vq_nlat)
   stbl_uq_vint=fltarr(stbl_uq_nlon,stbl_uq_nlat)
   stbl_vq_vint=fltarr(stbl_vq_nlon,stbl_vq_nlat)

   FOR j=0,ctl_uq_nlon-1 DO BEGIN
      FOR k=0,ctl_vq_nlat-1 DO BEGIN
         FOR m=0,n_plevs-1 DO BEGIN
            ctl_uq_vint(j,k)=ctl_uq_vint(j,k)+ctl_uq(j,k,m)*plevs(m+plev_bottom)
            ctl_vq_vint(j,k)=ctl_vq_vint(j,k)+ctl_vq(j,k,m)*plevs(m+plev_bottom)
            stbl_uq_vint(j,k)=stbl_uq_vint(j,k)+stbl_uq(j,k,m)*plevs(m+plev_bottom)
            stbl_vq_vint(j,k)=stbl_vq_vint(j,k)+stbl_vq(j,k,m)*plevs(m+plev_bottom)
         ENDFOR
      ENDFOR
   ENDFOR
   ctl_uq_vint=ctl_uq_vint/FLOAT(TOTAL(plevs(plev_bottom:plev_top)))
   ctl_vq_vint=ctl_vq_vint/FLOAT(TOTAL(plevs(plev_bottom:plev_top)))
   stbl_uq_vint=stbl_uq_vint/FLOAT(TOTAL(plevs(plev_bottom:plev_top)))
   stbl_vq_vint=stbl_vq_vint/FLOAT(TOTAL(plevs(plev_bottom:plev_top)))

   
   psfile='/home/ss901165/idl/queensland/higem/double_co2/moisture_fluxes/qld_higem_dblco2_uvq_vint_means.'+season_name+'_smean.higem_ctl.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,SPACE3=500,XOFFSET=0,YOFFSET=100,TFONT=3,$
          TCHARSIZE=100
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
   CS,SCALE=12,NCOLS=N_ELEMENTS(mag_levs)+1,/REV
   LEVS,MANUAL=mag_levs
   CON,X=ctl_uq_longitude,Y=ctl_uq_latitude,FIELD=SQRT(ctl_uq_vint^2+ctl_vq_vint^2),/NOLINES,$
       /BLOCK
   VECT,X=ctl_uq_longitude,Y=ctl_uq_latitude,U=ctl_uq_vint,V=ctl_vq_vint,MAG=50,$
        TITLE='1000-700 hPa integrated moisture flux from HiGEM control for - '+season_name+' mean',/MAP,$
        STRIDE=2   
   AXES,XSTEP=10,YSTEP=10
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/moisture_fluxes/qld_higem_dblco2_uvq_vint_means.'+season_name+'_smean.higem_stbl.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,SPACE3=500,XOFFSET=0,YOFFSET=100,TFONT=3,$
          TCHARSIZE=100
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
   CS,SCALE=12,NCOLS=N_ELEMENTS(mag_levs)+1,/REV
   LEVS,MANUAL=mag_levs
   CON,X=stbl_uq_longitude,Y=stbl_uq_latitude,FIELD=SQRT(stbl_uq_vint^2+stbl_vq_vint^2),/NOLINES,$
       /BLOCK
   VECT,X=stbl_uq_longitude,Y=stbl_uq_latitude,U=stbl_uq_vint,V=stbl_vq_vint,MAG=50,$
        TITLE='1000-700 hPa integrated moisture flux from HiGEM 2xCO2 fixed for - '+season_name+' mean',/MAP,$
        STRIDE=2   
   AXES,XSTEP=10,YSTEP=10
   PSCLOSE,/NOVIEW


   psfile='/home/ss901165/idl/queensland/higem/double_co2/moisture_fluxes/qld_higem_dblco2_uvq_vint_means.'+season_name+'_smean.higem_stbl-minus-higem_ctl.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,SPACE3=500,XOFFSET=0,YOFFSET=100,TFONT=3,$
          TCHARSIZE=100
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
   CS,SCALE=12,NCOLS=N_ELEMENTS(diff_mag_levs)+1,/REV
   LEVS,MANUAL=diff_mag_levs
   CON,X=stbl_uq_longitude,Y=stbl_uq_latitude,FIELD=SQRT(stbl_uq_vint^2+stbl_vq_vint^2)-SQRT(ctl_uq_vint^2+ctl_vq_vint^2),/NOLINES,$
       /BLOCK
   VECT,X=stbl_uq_longitude,Y=stbl_uq_latitude,U=stbl_uq_vint-ctl_uq_vint,V=stbl_vq_vint-ctl_vq_vint,MAG=10,$
        TITLE='1000-700 hPa integrated moisture flux from HiGEM 2xCO2 fixed for - '+season_name+' mean',/MAP,$
        STRIDE=2   
   AXES,XSTEP=10,YSTEP=10
   PSCLOSE

ENDFOR

STOP
END
