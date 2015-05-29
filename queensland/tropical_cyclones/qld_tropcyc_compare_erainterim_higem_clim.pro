PRO qld_tropcyc_compare_erainterim_higem_clim

; Compare climatological tropical cyclone densities from HiGEM
; and ERA-Interim.

higem_infile='/home/ss901165/higem_qccce/es_control_eafeb/stat_trs_scl.HiGEM150_vort_wind_mslp.attain20_1.nc'
eraint_infile='/home/ss901165/datasets_mango/ERA-INTERIM/tropical_storms/stat_trs_scl.erainterimSH_19yrs_fullvor_1.nc'

box=[-45,20,0,250]

; Get latitude and longitude from HiGEM
higem_longitude=OPEN_AND_EXTRACT(higem_infile,'long')
higem_latitude=OPEN_AND_EXTRACT(higem_infile,'lat')
DEFINE_BOUNDARIES,box,higem_latitude,higem_longitude,higem_box_tx,/LIMIT
higem_nlon=N_ELEMENTS(higem_longitude)
higem_nlat=N_ELEMENTS(higem_latitude)

; Get latitude and longitude from ERA-Interim
eraint_longitude=OPEN_AND_EXTRACT(eraint_infile,'long')
eraint_latitude=OPEN_AND_EXTRACT(eraint_infile,'lat')
DEFINE_BOUNDARIES,box,eraint_latitude,eraint_longitude,eraint_box_tx,/LIMIT
eraint_nlon=N_ELEMENTS(eraint_longitude)
eraint_nlat=N_ELEMENTS(eraint_latitude)

; Get climatological densities from HiGEM and ERA-Interim
higem_clim_track_density=OPEN_AND_EXTRACT(higem_infile,'tden',$
                                          offset=[higem_box_tx(1),higem_box_tx(0)],$
                                          count=[higem_nlon,higem_nlat])*8.
higem_clim_genesis_density=OPEN_AND_EXTRACT(higem_infile,'gden',$
                                            offset=[higem_box_tx(1),higem_box_tx(0)],$
                                            count=[higem_nlon,higem_nlat])*8.
higem_clim_lysis_density=OPEN_AND_EXTRACT(higem_infile,'lden',$
                                          offset=[higem_box_tx(1),higem_box_tx(0)],$
                                          count=[higem_nlon,higem_nlat])*8.

eraint_clim_track_density=OPEN_AND_EXTRACT(eraint_infile,'tden',$
                                             offset=[eraint_box_tx(1),eraint_box_tx(0)],$
                                             count=[eraint_nlon,eraint_nlat])*8.
eraint_clim_genesis_density=OPEN_AND_EXTRACT(eraint_infile,'gden',$
                                              offset=[eraint_box_tx(1),eraint_box_tx(0)],$
                                              count=[eraint_nlon,eraint_nlat])*8.
eraint_clim_lysis_density=OPEN_AND_EXTRACT(eraint_infile,'lden',$
                                            offset=[eraint_box_tx(1),eraint_box_tx(0)],$
                                            count=[eraint_nlon,eraint_nlat])*8.

mylevs_track=['2','4','6','8','10','12','14','16','18','20','22','24']
mylevs_genesis=['0.30','0.60','0.90','1.20','1.50','1.80','2.10','2.40','2.70','3.00','3.30']
mylevs_lysis=['0.10','0.20','0.30','0.40','0.50','0.60','0.70','0.80','0.90','1.00','1.10','1.20']

psfile='/home/ss901165/idl/queensland/tropical_cyclones/qld_tropcyc_compare_erainterim_higem_clim.amean_track.higem_eafeb.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=1200,YOFFSET=2000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
       YSIZE=10000,SPACE3=500
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_track)+1,white=[2]
LEVS,MANUAL=mylevs_track
MAP,LATMIN=-45,LATMAX=0,LONMIN=20,LONMAX=250
CON,X=higem_longitude,Y=higem_latitude,FIELD=higem_clim_track_density,/NOLINELABELS,$
    TITLE='Tropical-cyclone track density [storms year!U-1!N 10!U6!N km!U-1!N] for HiGEM eafeb - 150 years'
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/tropical_cyclones/qld_tropcyc_compare_erainterim_higem_clim.amean_track.era_interim.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=1200,YOFFSET=2000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
       YSIZE=10000,SPACE3=500
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_track)+1,white=[2]
LEVS,MANUAL=mylevs_track
MAP,LATMIN=-45,LATMAX=0,LONMIN=20,LONMAX=250
CON,X=eraint_longitude,Y=eraint_latitude,FIELD=eraint_clim_track_density,/NOLINELABELS,$
    TITLE='Tropical-cyclone track density [storms year!U-1!N 10!U6!N km!U-1!N] for ERA-Interim - 19 years'
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/tropical_cyclones/qld_tropcyc_compare_erainterim_higem_clim.amean_genesis.higem_eafeb.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=1200,YOFFSET=2000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
       YSIZE=10000,SPACE3=500
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_genesis)+1,white=[2]
LEVS,MANUAL=mylevs_genesis
MAP,LATMIN=-45,LATMAX=0,LONMIN=20,LONMAX=250
CON,X=higem_longitude,Y=higem_latitude,FIELD=higem_clim_genesis_density,/NOLINELABELS,$
    TITLE='Tropical-cyclone genesis density [storms year!U-1!N 10!U6!N km!U-1!N] for HiGEM eafeb - 150 years'
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/tropical_cyclones/qld_tropcyc_compare_erainterim_higem_clim.amean_genesis.era_interim.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=1200,YOFFSET=2000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
       YSIZE=10000,SPACE3=500
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_genesis)+1,white=[2]
LEVS,MANUAL=mylevs_genesis
MAP,LATMIN=-45,LATMAX=0,LONMIN=20,LONMAX=250
CON,X=eraint_longitude,Y=eraint_latitude,FIELD=eraint_clim_genesis_density,/NOLINELABELS,$
    TITLE='Tropical-cyclone genesis density [storms year!U-1!N 10!U6!N km!U-1!N] for ERA-Interim - 19 years'
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/tropical_cyclones/qld_tropcyc_compare_erainterim_higem_clim.amean_lysis.higem_eafeb.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=1200,YOFFSET=2000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
       YSIZE=10000,SPACE3=500
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_lysis)+1,white=[2]
LEVS,MANUAL=mylevs_lysis
MAP,LATMIN=-45,LATMAX=0,LONMIN=20,LONMAX=250
CON,X=higem_longitude,Y=higem_latitude,FIELD=higem_clim_lysis_density,/NOLINELABELS,$
    TITLE='Tropical-cyclone lysis density [storms year!U-1!N 10!U6!N km!U-1!N] for HiGEM eafeb - 150 years'
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/tropical_cyclones/qld_tropcyc_compare_erainterim_higem_clim.amean_lysis.era_interim.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=1200,YOFFSET=2000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
       YSIZE=10000,SPACE3=500
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_lysis)+1,white=[2]
LEVS,MANUAL=mylevs_lysis
MAP,LATMIN=-45,LATMAX=0,LONMIN=20,LONMAX=250
CON,X=eraint_longitude,Y=eraint_latitude,FIELD=eraint_clim_lysis_density,/NOLINELABELS,$
    TITLE='Tropical-cyclone lysis density [storms year!U-1!N 10!U6!N km!U-1!N] for ERA-Interim - 19 years'
PSCLOSE,/NOVIEW


STOP
END
