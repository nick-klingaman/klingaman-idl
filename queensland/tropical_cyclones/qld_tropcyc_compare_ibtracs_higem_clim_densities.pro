PRO qld_tropcyc_compare_ibtracs_higem_clim_densities

; Compare climatological tropical cyclone densities from HiGEM and
; IBTRACS.  IBTRACS data must first be processed similarly to the
; TRACK program (see process_ibtracs_as_track.pro)

higem_infile='/home/ss901165/higem_qccce/es_control_eafeb/stat_trs_scl.HiGEM150_vort_wind_mslp.attain20_1.nc'
ibtracs_infile='/home/ss901165/datasets/IBTRACS/stat_trs_scl.SIBT_50_08.nc'

box=[-45,90,0,250]

; Get latitude and longitude
longitude=OPEN_AND_EXTRACT(higem_infile,'long')
latitude=OPEN_AND_EXTRACT(higem_infile,'lat')
DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)

; Get climatological densities from HiGEM and IBTRACS
higem_clim_track_density=OPEN_AND_EXTRACT(higem_infile,'tden',$
                                          offset=[box_tx(1),box_tx(0)],$
                                          count=[n_lon,n_lat])*8.
higem_clim_genesis_density=OPEN_AND_EXTRACT(higem_infile,'gden',$
                                            offset=[box_tx(1),box_tx(0)],$
                                            count=[n_lon,n_lat])*8.
higem_clim_lysis_density=OPEN_AND_EXTRACT(higem_infile,'lden',$
                                          offset=[box_tx(1),box_tx(0)],$
                                          count=[n_lon,n_lat])*8.

ibtracs_clim_track_density=OPEN_AND_EXTRACT(ibtracs_infile,'tden',$
                                             offset=[box_tx(1),box_tx(0)],$
                                             count=[n_lon,n_lat])*12
ibtracs_clim_genesis_density=OPEN_AND_EXTRACT(ibtracs_infile,'gden',$
                                              offset=[box_tx(1),box_tx(0)],$
                                              count=[n_lon,n_lat])*8
ibtracs_clim_lysis_density=OPEN_AND_EXTRACT(ibtracs_infile,'lden',$
                                            offset=[box_tx(1),box_tx(0)],$
                                            count=[n_lon,n_lat])*8

; Plot densities from HiGEM and IBTRACS

mylevs_track=['2','4','6','8','10','12','14','16','18','20','22','24']
mylevs_genesis=['0.40','0.80','1.20','1.60','2.00','2.40','2.80','3.20','3.60','4.00','4.40']
mylevs_lysis=['0.20','0.40','0.60','0.80','1.00','1.20','1.40','1.60','1.80','2.00','2.20','2.40']
mylevs_diff_track=['-13','-11','-9','-7','-5','-3','-1','1','3','5','7','9','11','13']
mylevs_diff_genesis=['-2.2','-1.8','-1.4','-1.0','-0.6','-0.2','0.2','0.6','1.0','1.4','1.8','2.2']
mylevs_diff_lysis=['-1.95','-1.65','-1.35','-1.05','-0.75','-0.45','-0.15','0.15','0.45','0.75','1.05','1.35','1.65','1.95']

psfile='/home/ss901165/idl/queensland/tropical_cyclones/qld_tropcyc_compare_ibtracs_higem_clim_densities.amean_track.higem_eafeb.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=1200,YOFFSET=2000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
       YSIZE=10000,SPACE3=500
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_track)+1,white=[2]
LEVS,MANUAL=mylevs_track
MAP,LATMIN=-45,LATMAX=0,LONMIN=90,LONMAX=250
CON,X=longitude,Y=latitude,FIELD=higem_clim_track_density,/NOLINELABELS,$
    TITLE='Tropical-cyclone track density [storms year!U-1!N (5!Uo!N radius circle)] for HiGEM eafeb (h9-w8)'
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/tropical_cyclones/qld_tropcyc_compare_ibtracs_higem_clim_densities.amean_track.ibtracs_1950-2008.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=1200,YOFFSET=2000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
       YSIZE=10000,SPACE3=500
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_track)+1,white=[2]
LEVS,MANUAL=mylevs_track
MAP,LATMIN=-45,LATMAX=0,LONMIN=90,LONMAX=250
CON,X=longitude,Y=latitude,FIELD=ibtracs_clim_track_density,/NOLINELABELS,$
    TITLE='Tropical-cyclone track density [storms year!U-1!N (5!Uo!N radius circle)] for IBTrACS - 1950-2008'
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/tropical_cyclones/qld_tropcyc_compare_ibtracs_higem_clim_densities.amean_track.higem_eafeb-minus-ibtracs_50-08.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=1200,YOFFSET=2000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
       YSIZE=10000,SPACE3=500
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_track)+1,white=[9]
LEVS,MANUAL=mylevs_diff_track
MAP,LATMIN=-45,LATMAX=0,LONMIN=90,LONMAX=250
CON,X=longitude,Y=latitude,FIELD=higem_clim_track_density-ibtracs_clim_track_density,/NOLINELABELS,$
    TITLE='Difference in tropical-cyclone track density [storms year!U-1!N (5!Uo!N radius circle)] for HiGEM (h9-w8) minus IBTrACS (1950-2008)'
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/tropical_cyclones/qld_tropcyc_compare_ibtracs_higem_clim_densities.amean_genesis.higem_eafeb.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=1200,YOFFSET=2000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
       YSIZE=10000,SPACE3=500
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_genesis)+1,white=[2]
LEVS,MANUAL=mylevs_genesis
MAP,LATMIN=-45,LATMAX=0,LONMIN=90,LONMAX=250
CON,X=longitude,Y=latitude,FIELD=higem_clim_genesis_density,/NOLINELABELS,$
    TITLE='Tropical-cyclone genesis density [storms year!U-1!N (5!Uo!N radius circle)] for HiGEM eafeb - 150 years'
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/tropical_cyclones/qld_tropcyc_compare_ibtracs_higem_clim_densities.amean_genesis.ibtracs_1950-2008.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=1200,YOFFSET=2000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
       YSIZE=10000,SPACE3=500
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_genesis)+1,white=[2]
LEVS,MANUAL=mylevs_genesis
MAP,LATMIN=-45,LATMAX=0,LONMIN=90,LONMAX=250
CON,X=longitude,Y=latitude,FIELD=ibtracs_clim_genesis_density,/NOLINELABELS,$
    TITLE='Tropical-cyclone genesis density [storms year!U-1!N (5!Uo!N radius circle)] for IBTRACS - 1950-2008'
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/tropical_cyclones/qld_tropcyc_compare_ibtracs_higem_clim_densities.amean_genesis.higem_eafeb-minus-ibtracs_50-08.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=1200,YOFFSET=2000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
       YSIZE=10000,SPACE3=500
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_genesis)+1,white=[8]
LEVS,MANUAL=mylevs_diff_genesis
MAP,LATMIN=-45,LATMAX=0,LONMIN=90,LONMAX=250
CON,X=longitude,Y=latitude,FIELD=higem_clim_genesis_density-ibtracs_clim_genesis_density,/NOLINELABELS,$
    TITLE='Difference in tropical-cyclone genesis density [storms year!U-1!N (5!Uo!N radius circle)] for HiGEM (h9-w8) minus IBTrACS (1950-2008)'
PSCLOSE

psfile='/home/ss901165/idl/queensland/tropical_cyclones/qld_tropcyc_compare_ibtracs_higem_clim_densities.amean_lysis.higem_eafeb.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=1200,YOFFSET=2000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
       YSIZE=10000,SPACE3=500
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_lysis)+1,white=[2]
LEVS,MANUAL=mylevs_lysis
MAP,LATMIN=-45,LATMAX=0,LONMIN=90,LONMAX=250
CON,X=longitude,Y=latitude,FIELD=higem_clim_lysis_density,/NOLINELABELS,$
    TITLE='Tropical-cyclone lysis density [storms year!U-1!N (5!Uo!N radius circle)] for HiGEM eafeb - 150 years'
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/tropical_cyclones/qld_tropcyc_compare_ibtracs_higem_clim_densities.amean_lysis.ibtracs_1950-2008.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=1200,YOFFSET=2000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
       YSIZE=10000,SPACE3=500
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_lysis)+1,white=[2]
LEVS,MANUAL=mylevs_lysis
MAP,LATMIN=-45,LATMAX=0,LONMIN=90,LONMAX=250
CON,X=longitude,Y=latitude,FIELD=ibtracs_clim_lysis_density,/NOLINELABELS,$
    TITLE='Tropical-cyclone lysis density [storms year!U-1!N (5!Uo!N radius circle)] for IBTRACS - 1950-2008'
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/tropical_cyclones/qld_tropcyc_compare_ibtracs_higem_clim_densities.amean_lysis.higem_eafeb-minus-ibtracs_50-08.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=1200,YOFFSET=2000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
       YSIZE=10000,SPACE3=500
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_lysis)+1,white=[9]
LEVS,MANUAL=mylevs_diff_lysis
MAP,LATMIN=-45,LATMAX=0,LONMIN=90,LONMAX=250
CON,X=longitude,Y=latitude,FIELD=higem_clim_lysis_density-ibtracs_clim_lysis_density,/NOLINELABELS,$
    TITLE='Difference in tropical-cyclone lysis density [storms year!U-1!N (5!Uo!N radius circle)] for HiGEM (h9-w8) minus IBTrACS (1950-2008)'
PSCLOSE

STOP

END

