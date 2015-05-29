PRO plot_IOD_precip_corr_coeff_djf

; Give the IOD input files.

IOD_obs_file='/home/ss901165/datasets/IOD/iod_index.dec-feb_smeans.1958-2009.hadisst.nc'

IOD_depresys1_file='/home/swr06jir/depresys_data/transient_index/PPE_trans/iod/qump1_iod_sst_index_djf.nc'
IOD_depresys2_file='/home/swr06jir/depresys_data/transient_index/PPE_trans/iod/qump2_iod_sst_index_djf.nc'
IOD_depresys3_file='/home/swr06jir/depresys_data/transient_index/PPE_trans/iod/qump3_iod_sst_index_djf.nc'
IOD_depresys4_file='/home/swr06jir/depresys_data/transient_index/PPE_trans/iod/qump4_iod_sst_index_djf.nc'
IOD_depresys5_file='/home/swr06jir/depresys_data/transient_index/PPE_trans/iod/qump5_iod_sst_index_djf.nc'
IOD_depresys6_file='/home/swr06jir/depresys_data/transient_index/PPE_trans/iod/qump6_iod_sst_index_djf.nc'
IOD_depresys7_file='/home/swr06jir/depresys_data/transient_index/PPE_trans/iod/qump7_iod_sst_index_djf.nc'
IOD_depresys8_file='/home/swr06jir/depresys_data/transient_index/PPE_trans/iod/qump8_iod_sst_index_djf.nc'
IOD_depresys0_file='/home/swr06jir/depresys_data/transient_index/PPE_trans/iod/qump0_iod_sst_index_djf.nc'

; Give the annual mean rainfall input files.

precip_obs_file='/home/ss901165/datasets/SILO/n48/SILO.dec-feb_smeans.1958-2001.precip.n48.nc'
precip_depresys1_file='/home/swr06jir/links/elder/PPE/trans/atmos/qump1/qump1_PRECIP_djf_DATA.nc'
precip_depresys2_file='/home/swr06jir/links/elder/PPE/trans/atmos/qump2/qump2_PRECIP_djf_DATA.nc'
precip_depresys3_file='/home/swr06jir/links/elder/PPE/trans/atmos/qump3/qump3_PRECIP_djf_DATA.nc'
precip_depresys4_file='/home/swr06jir/links/elder/PPE/trans/atmos/qump4/qump4_PRECIP_djf_DATA.nc'
precip_depresys5_file='/home/swr06jir/links/elder/PPE/trans/atmos/qump5/qump5_PRECIP_djf_DATA.nc'
precip_depresys6_file='/home/swr06jir/links/elder/PPE/trans/atmos/qump6/qump6_PRECIP_djf_DATA.nc'
precip_depresys7_file='/home/swr06jir/links/elder/PPE/trans/atmos/qump7/qump7_PRECIP_djf_DATA.nc'
precip_depresys8_file='/home/swr06jir/links/elder/PPE/trans/atmos/qump8/qump8_PRECIP_djf_DATA.nc'
precip_depresys0_file='/home/swr06jir/links/elder/PPE/trans/atmos/qump0/qump0_PRECIP_djf_DATA.nc'

; Give the input file containing the land/sea mask data.  We need this because the rainfall
; observations are valid only over land.  This mask is the same as that used in the climate model.
mask_file='/home/ss901165/um_output/mask_n48.nc'

; Define the region to plot as a box.  The values are:
; [starting_latitude,starting_longitude,stopping_latitude,stopping_longitude]
box=[-60,80,10,240]

; Number of years to read
n_years=44

obs_longitude=OPEN_AND_EXTRACT(precip_obs_file,'longitude')
obs_latitude=OPEN_AND_EXTRACT(precip_obs_file,'latitude')
; Restrict the dimensions to the box specified above.
DEFINE_BOUNDARIES,box,obs_latitude,obs_longitude,obs_box_tx,/LIMIT
; Get the number of longitude and latitude points in the "box" region.
obs_nlon=N_ELEMENTS(obs_longitude)
obs_nlat=N_ELEMENTS(obs_latitude)

depresys1_longitude=OPEN_AND_EXTRACT(precip_depresys1_file,'longitude')
depresys1_latitude=OPEN_AND_EXTRACT(precip_depresys1_file,'latitude')
; Restrict the dimensions to the box specified above.
DEFINE_BOUNDARIES,box,depresys1_latitude,depresys1_longitude,depresys1_box_tx,/LIMIT
; Get the number of longitude and latitude points in the "box" region.
depresys1_nlon=N_ELEMENTS(depresys1_longitude)
depresys1_nlat=N_ELEMENTS(depresys1_latitude)

; Read longitude and latitude co-ordinate variables from the land/sea mask file
mask_longitude=OPEN_AND_EXTRACT(mask_file,'longitude')
mask_latitude=OPEN_AND_EXTRACT(mask_file,'latitude')
DEFINE_BOUNDARIES,[obs_latitude(0),obs_longitude(0),obs_latitude(obs_nlat-1),obs_longitude(obs_nlon-1)],mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)

; Read the land/sea mask in the same way.  The variable is called "lsm"
mask=OPEN_AND_EXTRACT(mask_file,'lsm',$
                      offset=[mask_box_tx(1),mask_box_tx(0)],$
                      count=[mask_nlon,mask_nlat])


obs_IOD=OPEN_AND_EXTRACT(IOD_obs_file,'IOD',offset=[0],count=[n_years])

depresys1_IOD=OPEN_AND_EXTRACT(IOD_depresys1_file,'temp',$
                               offset=[7],$
                               count=[n_years])
depresys2_IOD=OPEN_AND_EXTRACT(IOD_depresys2_file,'temp',$
                               offset=[7],$
                               count=[n_years])
depresys3_IOD=OPEN_AND_EXTRACT(IOD_depresys3_file,'temp',$
                               offset=[7],$
                               count=[n_years])


depresys4_IOD=OPEN_AND_EXTRACT(IOD_depresys4_file,'temp',$
                               offset=[7],$
                               count=[n_years])


depresys5_IOD=OPEN_AND_EXTRACT(IOD_depresys5_file,'temp',$
                               offset=[7],$
                               count=[n_years])


depresys6_IOD=OPEN_AND_EXTRACT(IOD_depresys6_file,'temp',$
                               offset=[7],$
                               count=[n_years])


depresys7_IOD=OPEN_AND_EXTRACT(IOD_depresys7_file,'temp',$
                               offset=[7],$
                               count=[n_years])


depresys8_IOD=OPEN_AND_EXTRACT(IOD_depresys8_file,'temp',$
                               offset=[7],$
                               count=[n_years])

depresys0_IOD=OPEN_AND_EXTRACT(IOD_depresys0_file,'temp',$
                               offset=[7],$
                               count=[n_years])


obs_precip=OPEN_AND_EXTRACT(precip_obs_file,'rain',$
					offset=[obs_box_tx(1),obs_box_tx(0),0],$
					count=[obs_nlon,obs_nlat,n_years])

depresys1_rain=REFORM(OPEN_AND_EXTRACT(precip_depresys1_file,'precip',$
                                       offset=[depresys1_box_tx(1),depresys1_box_tx(0),0,58],$
                                       count=[depresys1_nlon,depresys1_nlat,1,n_years]))

depresys2_rain=REFORM(OPEN_AND_EXTRACT(precip_depresys2_file,'precip',$
                                       offset=[depresys1_box_tx(1),depresys1_box_tx(0),0,58],$
                                       count=[depresys1_nlon,depresys1_nlat,1,n_years]))

depresys3_rain=REFORM(OPEN_AND_EXTRACT(precip_depresys3_file,'precip',$
                                       offset=[depresys1_box_tx(1),depresys1_box_tx(0),0,58],$
                                       count=[depresys1_nlon,depresys1_nlat,1,n_years]))

depresys4_rain=REFORM(OPEN_AND_EXTRACT(precip_depresys4_file,'precip',$
                                       offset=[depresys1_box_tx(1),depresys1_box_tx(0),0,58],$
                                       count=[depresys1_nlon,depresys1_nlat,1,n_years]))

depresys5_rain=REFORM(OPEN_AND_EXTRACT(precip_depresys5_file,'precip',$
                                       offset=[depresys1_box_tx(1),depresys1_box_tx(0),0,58],$
                                       count=[depresys1_nlon,depresys1_nlat,1,n_years]))

depresys6_rain=REFORM(OPEN_AND_EXTRACT(precip_depresys6_file,'precip',$
                                       offset=[depresys1_box_tx(1),depresys1_box_tx(0),0,58],$
                                       count=[depresys1_nlon,depresys1_nlat,1,n_years]))

depresys7_rain=REFORM(OPEN_AND_EXTRACT(precip_depresys7_file,'precip',$
                                       offset=[depresys1_box_tx(1),depresys1_box_tx(0),0,58],$
                                       count=[depresys1_nlon,depresys1_nlat,1,n_years]))

depresys8_rain=REFORM(OPEN_AND_EXTRACT(precip_depresys8_file,'precip',$
                                       offset=[depresys1_box_tx(1),depresys1_box_tx(0),0,58],$
                                       count=[depresys1_nlon,depresys1_nlat,1,n_years]))

depresys0_rain=REFORM(OPEN_AND_EXTRACT(precip_depresys0_file,'precip',$
                                       offset=[depresys1_box_tx(1),depresys1_box_tx(0),0,58],$
                                       count=[depresys1_nlon,depresys1_nlat,1,n_years]))

depresys1_precip=depresys1_rain * 86400

depresys2_precip=depresys2_rain * 86400

depresys3_precip=depresys3_rain * 86400

depresys4_precip=depresys4_rain * 86400

depresys5_precip=depresys5_rain * 86400

depresys6_precip=depresys6_rain * 86400

depresys7_precip=depresys7_rain * 86400

depresys8_precip=depresys8_rain * 86400

depresys0_precip=depresys0_rain * 86400




obs_regress_coeff = fltarr(obs_nlon,obs_nlat)
depresys1_regress_coeff = fltarr(depresys1_nlon,depresys1_nlat)
depresys2_regress_coeff = fltarr(depresys1_nlon,depresys1_nlat)
depresys3_regress_coeff = fltarr(depresys1_nlon,depresys1_nlat)
depresys4_regress_coeff = fltarr(depresys1_nlon,depresys1_nlat)
depresys5_regress_coeff = fltarr(depresys1_nlon,depresys1_nlat)
depresys6_regress_coeff = fltarr(depresys1_nlon,depresys1_nlat)
depresys7_regress_coeff = fltarr(depresys1_nlon,depresys1_nlat)
depresys8_regress_coeff = fltarr(depresys1_nlon,depresys1_nlat)
depresys0_regress_coeff = fltarr(depresys1_nlon,depresys1_nlat)

FOR i=0,obs_nlon-1 DO BEGIN
FOR j=0,obs_nlat-1 DO BEGIN
obs_regress_coeff(i,j) = REGRESS(obs_IOD,REFORM(obs_precip(i,j,*)))
ENDFOR
ENDFOR

FOR i=0,depresys1_nlon-1 DO BEGIN
FOR j=0,depresys1_nlat-1 DO BEGIN
depresys1_regress_coeff(i,j) = REGRESS(depresys1_IOD,REFORM(depresys1_precip(i,j,*)))
ENDFOR
ENDFOR

FOR i=0,depresys1_nlon-1 DO BEGIN
FOR j=0,depresys1_nlat-1 DO BEGIN
depresys2_regress_coeff(i,j) = REGRESS(depresys2_IOD,REFORM(depresys2_precip(i,j,*)))
ENDFOR
ENDFOR

FOR i=0,depresys1_nlon-1 DO BEGIN
FOR j=0,depresys1_nlat-1 DO BEGIN
depresys3_regress_coeff(i,j) = REGRESS(depresys3_IOD,REFORM(depresys3_precip(i,j,*)))
ENDFOR
ENDFOR

FOR i=0,depresys1_nlon-1 DO BEGIN
FOR j=0,depresys1_nlat-1 DO BEGIN
depresys4_regress_coeff(i,j) = REGRESS(depresys4_IOD,REFORM(depresys4_precip(i,j,*)))
ENDFOR
ENDFOR

FOR i=0,depresys1_nlon-1 DO BEGIN
FOR j=0,depresys1_nlat-1 DO BEGIN
depresys5_regress_coeff(i,j) = REGRESS(depresys5_IOD,REFORM(depresys5_precip(i,j,*)))
ENDFOR
ENDFOR

FOR i=0,depresys1_nlon-1 DO BEGIN
FOR j=0,depresys1_nlat-1 DO BEGIN
depresys6_regress_coeff(i,j) = REGRESS(depresys6_IOD,REFORM(depresys6_precip(i,j,*)))
ENDFOR
ENDFOR

FOR i=0,depresys1_nlon-1 DO BEGIN
FOR j=0,depresys1_nlat-1 DO BEGIN
depresys7_regress_coeff(i,j) = REGRESS(depresys7_IOD,REFORM(depresys7_precip(i,j,*)))
ENDFOR
ENDFOR

FOR i=0,depresys1_nlon-1 DO BEGIN
FOR j=0,depresys1_nlat-1 DO BEGIN
depresys8_regress_coeff(i,j) = REGRESS(depresys8_IOD,REFORM(depresys8_precip(i,j,*)))
ENDFOR
ENDFOR

FOR i=0,depresys1_nlon-1 DO BEGIN
FOR j=0,depresys1_nlat-1 DO BEGIN
depresys0_regress_coeff(i,j) = REGRESS(depresys0_IOD,REFORM(depresys0_precip(i,j,*)))
ENDFOR
ENDFOR


obs_corr_coeff = fltarr(obs_nlon,obs_nlat)
depresys1_corr_coeff = fltarr(depresys1_nlon,depresys1_nlat)
depresys2_corr_coeff = fltarr(depresys1_nlon,depresys1_nlat)
depresys3_corr_coeff = fltarr(depresys1_nlon,depresys1_nlat)
depresys4_corr_coeff = fltarr(depresys1_nlon,depresys1_nlat)
depresys5_corr_coeff = fltarr(depresys1_nlon,depresys1_nlat)
depresys6_corr_coeff = fltarr(depresys1_nlon,depresys1_nlat)
depresys7_corr_coeff = fltarr(depresys1_nlon,depresys1_nlat)
depresys8_corr_coeff = fltarr(depresys1_nlon,depresys1_nlat)
depresys0_corr_coeff = fltarr(depresys1_nlon,depresys1_nlat)

FOR i=0,obs_nlon-1 DO BEGIN
FOR j=0,obs_nlat-1 DO BEGIN
obs_corr_coeff(i,j) = CORRELATE(obs_IOD,obs_precip(i,j,*))
ENDFOR
ENDFOR

FOR i=0,depresys1_nlon-1 DO BEGIN
FOR j=0,depresys1_nlat-1 DO BEGIN
depresys1_corr_coeff(i,j) = CORRELATE(depresys1_IOD,depresys1_precip(i,j,*))
ENDFOR
ENDFOR

FOR i=0,depresys1_nlon-1 DO BEGIN
FOR j=0,depresys1_nlat-1 DO BEGIN
depresys2_corr_coeff(i,j) = CORRELATE(depresys2_IOD,depresys2_precip(i,j,*))
ENDFOR
ENDFOR

FOR i=0,depresys1_nlon-1 DO BEGIN
FOR j=0,depresys1_nlat-1 DO BEGIN
depresys3_corr_coeff(i,j) = CORRELATE(depresys3_IOD,depresys3_precip(i,j,*))
ENDFOR
ENDFOR

FOR i=0,depresys1_nlon-1 DO BEGIN
FOR j=0,depresys1_nlat-1 DO BEGIN
depresys4_corr_coeff(i,j) = CORRELATE(depresys4_IOD,depresys4_precip(i,j,*))
ENDFOR
ENDFOR

FOR i=0,depresys1_nlon-1 DO BEGIN
FOR j=0,depresys1_nlat-1 DO BEGIN
depresys5_corr_coeff(i,j) = CORRELATE(depresys5_IOD,depresys5_precip(i,j,*))
ENDFOR
ENDFOR

FOR i=0,depresys1_nlon-1 DO BEGIN
FOR j=0,depresys1_nlat-1 DO BEGIN
depresys6_corr_coeff(i,j) = CORRELATE(depresys6_IOD,depresys6_precip(i,j,*))
ENDFOR
ENDFOR

FOR i=0,depresys1_nlon-1 DO BEGIN
FOR j=0,depresys1_nlat-1 DO BEGIN
depresys7_corr_coeff(i,j) = CORRELATE(depresys7_IOD,depresys7_precip(i,j,*))
ENDFOR
ENDFOR

FOR i=0,depresys1_nlon-1 DO BEGIN
FOR j=0,depresys1_nlat-1 DO BEGIN
depresys8_corr_coeff(i,j) = CORRELATE(depresys8_IOD,depresys8_precip(i,j,*))
ENDFOR
ENDFOR

FOR i=0,depresys1_nlon-1 DO BEGIN
FOR j=0,depresys1_nlat-1 DO BEGIN
depresys0_corr_coeff(i,j) = CORRELATE(depresys0_IOD,depresys0_precip(i,j,*))
ENDFOR
ENDFOR


IOD_obs_stddev=fltarr(obs_nlon,obs_nlat)
IOD_depresys1_stddev=fltarr(depresys1_nlon,depresys1_nlat)
IOD_depresys2_stddev=fltarr(depresys1_nlon,depresys1_nlat)
IOD_depresys3_stddev=fltarr(depresys1_nlon,depresys1_nlat)
IOD_depresys4_stddev=fltarr(depresys1_nlon,depresys1_nlat)
IOD_depresys5_stddev=fltarr(depresys1_nlon,depresys1_nlat)
IOD_depresys6_stddev=fltarr(depresys1_nlon,depresys1_nlat)
IOD_depresys7_stddev=fltarr(depresys1_nlon,depresys1_nlat)
IOD_depresys8_stddev=fltarr(depresys1_nlon,depresys1_nlat)
IOD_depresys0_stddev=fltarr(depresys1_nlon,depresys1_nlat)


IOD_obs_stddev=STDDEV(obs_IOD)

IOD_depresys1_stddev=STDDEV(depresys1_IOD)

IOD_depresys2_stddev=STDDEV(depresys2_IOD)

IOD_depresys3_stddev=STDDEV(depresys3_IOD)

IOD_depresys4_stddev=STDDEV(depresys4_IOD)

IOD_depresys5_stddev=STDDEV(depresys5_IOD)

IOD_depresys6_stddev=STDDEV(depresys6_IOD)

IOD_depresys7_stddev=STDDEV(depresys7_IOD)

IOD_depresys8_stddev=STDDEV(depresys8_IOD)

IOD_depresys0_stddev=STDDEV(depresys0_IOD)


obs_regress_coeff_plot = obs_regress_coeff / IOD_obs_stddev

depresys1_regress_coeff_plot = depresys1_regress_coeff / IOD_depresys1_stddev

depresys2_regress_coeff_plot = depresys2_regress_coeff / IOD_depresys2_stddev

depresys3_regress_coeff_plot = depresys3_regress_coeff / IOD_depresys3_stddev

depresys4_regress_coeff_plot = depresys4_regress_coeff / IOD_depresys4_stddev

depresys5_regress_coeff_plot = depresys5_regress_coeff / IOD_depresys5_stddev

depresys6_regress_coeff_plot = depresys6_regress_coeff / IOD_depresys6_stddev

depresys7_regress_coeff_plot = depresys7_regress_coeff / IOD_depresys7_stddev

depresys8_regress_coeff_plot = depresys8_regress_coeff / IOD_depresys8_stddev

depresys0_regress_coeff_plot = depresys0_regress_coeff / IOD_depresys0_stddev


sig_value = 0.304

obs_regress_coeff_plot[where(ABS(obs_corr_coeff)lt sig_value)]=!Values.F_NaN

depresys1_regress_coeff_plot[where(ABS(depresys1_corr_coeff)lt sig_value)]=!Values.F_NaN

depresys2_regress_coeff_plot[where(ABS(depresys2_corr_coeff)lt sig_value)]=!Values.F_NaN

depresys3_regress_coeff_plot[where(ABS(depresys3_corr_coeff)lt sig_value)]=!Values.F_NaN

depresys4_regress_coeff_plot[where(ABS(depresys4_corr_coeff)lt sig_value)]=!Values.F_NaN

depresys5_regress_coeff_plot[where(ABS(depresys5_corr_coeff)lt sig_value)]=!Values.F_NaN

depresys6_regress_coeff_plot[where(ABS(depresys6_corr_coeff)lt sig_value)]=!Values.F_NaN

depresys7_regress_coeff_plot[where(ABS(depresys7_corr_coeff)lt sig_value)]=!Values.F_NaN

depresys8_regress_coeff_plot[where(ABS(depresys8_corr_coeff)lt sig_value)]=!Values.F_NaN

depresys0_regress_coeff_plot[where(ABS(depresys0_corr_coeff)lt sig_value)]=!Values.F_NaN

obs_regress_coeff_plot[where(mask eq 0)]=!Values.F_NaN

; Plot the data. 
; Give the location of the PostScript file that this program will make.

psfile='/home/ss901165/idl/aus_decpred/Obs_IOD_precip_corr_coeff_djf.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=1300,XOFFSET=500,YOFFSET=2500,TFONT=2,$
       YSIZE=15000,TCHARSIZE=100,SPACE3=500,/PORTRAIT
; Set up the contour levels
LEVS,MANUAL=['-5','-4.5','-4','-3.5','-3','-2.5','-2','-1.5','-1','-0.5','0','0.5','1','1.5','2','2.5','3','3.5','4','4.5','5']
; Decide on a color scale
CS,SCALE=1,NCOLS=23
; Use the elements of the box variable to set the boundaries of the map.
MAP,LONMIN=112,LONMAX=155,LATMIN=-40,LATMAX=-10
; Draw the plot
CON,X=obs_longitude,Y=obs_latitude,FIELD=obs_regress_coeff_plot,/NOLINES,$
    TITLE='DJF Regression Coefficient of Rainfall and IOD from Observations(1958-2001)',$
    CB_TITLE='Regression Coefficient',/BLOCK
; Close
PSCLOSE

psfile1='/home/ss901165/idl/aus_decpred/Depresys1_IOD_precip_corr_coeff_djf.ps'
PSOPEN,file=psfile1,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=1300,XOFFSET=500,YOFFSET=2500,TFONT=2,$
       YSIZE=15000,TCHARSIZE=100,SPACE3=500,/PORTRAIT
; Set up the contour levels
LEVS,MANUAL=['-30','-28','-26','-24','-22','-20','-18','-16','-14','-12','-10','-8','-6','-4','-2','0','2','4','6','8','10','12','14','16','18','20','22','24','26','28','30']
; Decide on a color scale
CS,SCALE=1,NCOLS=33
; Use the elements of the box variable to set the boundaries of the map.
MAP,LONMIN=30,LONMAX=190,LATMIN=-50,LATMAX=20
; Draw the plot
CON,X=depresys1_longitude,Y=depresys1_latitude,FIELD=depresys1_regress_coeff_plot,/NOLINES,$
    TITLE='DJF Regression Coefficient of Rainfall and IOD from QUMP1(1958-2001)',$
    CB_TITLE='Regression Coefficient',/BLOCK,/CB_ALT
; Close
PSCLOSE

psfile2='/home/ss901165/idl/aus_decpred/Depresys2_IOD_precip_corr_coeff_djf.ps'
PSOPEN,file=psfile2,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=1300,XOFFSET=500,YOFFSET=2500,TFONT=2,$
       YSIZE=15000,TCHARSIZE=100,SPACE3=500,/PORTRAIT
; Set up the contour levels
LEVS,MANUAL=['-30','-28','-26','-24','-22','-20','-18','-16','-14','-12','-10','-8','-6','-4','-2','0','2','4','6','8','10','12','14','16','18','20','22','24','26','28','30']
; Decide on a color scale
CS,SCALE=1,NCOLS=33
; Use the elements of the box variable to set the boundaries of the map.
MAP,LONMIN=30,LONMAX=190,LATMIN=-50,LATMAX=20
; Draw the plot
CON,X=depresys1_longitude,Y=depresys1_latitude,FIELD=depresys2_regress_coeff_plot,/NOLINES,$
    TITLE='DJF Regression Coefficient of Rainfall and IOD from QUMP2(1958-2001)',$
    CB_TITLE='Regression Coefficient',/BLOCK,/CB_ALT
; Close
PSCLOSE,/NOVIEW

psfile3='/home/ss901165/idl/aus_decpred/Depresys3_IOD_precip_corr_coeff_djf.ps'
PSOPEN,file=psfile3,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=1300,XOFFSET=500,YOFFSET=2500,TFONT=2,$
       YSIZE=15000,TCHARSIZE=100,SPACE3=500,/PORTRAIT
; Set up the contour levels
LEVS,MANUAL=['-30','-28','-26','-24','-22','-20','-18','-16','-14','-12','-10','-8','-6','-4','-2','0','2','4','6','8','10','12','14','16','18','20','22','24','26','28','30']
; Decide on a color scale
CS,SCALE=1,NCOLS=33
; Use the elements of the box variable to set the boundaries of the map.
MAP,LONMIN=30,LONMAX=190,LATMIN=-50,LATMAX=20
; Draw the plot
CON,X=depresys1_longitude,Y=depresys1_latitude,FIELD=depresys3_regress_coeff_plot,/NOLINES,$
    TITLE='DJF Regression Coefficient of Rainfall and IOD from QUMP3(1958-2001)',$
    CB_TITLE='Regression Coefficient',/BLOCK,/CB_ALT
; Close
PSCLOSE,/NOVIEW

psfile4='/home/ss901165/idl/aus_decpred/Depresys4_IOD_precip_corr_coeff_djf.ps'
PSOPEN,file=psfile4,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=1300,XOFFSET=500,YOFFSET=2500,TFONT=2,$
       YSIZE=15000,TCHARSIZE=100,SPACE3=500,/PORTRAIT
; Set up the contour levels
LEVS,MANUAL=['-30','-28','-26','-24','-22','-20','-18','-16','-14','-12','-10','-8','-6','-4','-2','0','2','4','6','8','10','12','14','16','18','20','22','24','26','28','30']
; Decide on a color scale
CS,SCALE=1,NCOLS=33
; Use the elements of the box variable to set the boundaries of the map.
MAP,LONMIN=30,LONMAX=190,LATMIN=-50,LATMAX=20
; Draw the plot
CON,X=depresys1_longitude,Y=depresys1_latitude,FIELD=depresys4_regress_coeff_plot,/NOLINES,$
    TITLE='DJF Regression Coefficient of Rainfall and IOD from QUMP4(1958-2001)',$
    CB_TITLE='Regression Coefficient',/BLOCK,/CB_ALT
; Close
PSCLOSE,/NOVIEW

psfile5='/home/ss901165/idl/aus_decpred/Depresys5_IOD_precip_corr_coeff_djf.ps'
PSOPEN,file=psfile5,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=1300,XOFFSET=500,YOFFSET=2500,TFONT=2,$
       YSIZE=15000,TCHARSIZE=100,SPACE3=500,/PORTRAIT
; Set up the contour levels
LEVS,MANUAL=['-30','-28','-26','-24','-22','-20','-18','-16','-14','-12','-10','-8','-6','-4','-2','0','2','4','6','8','10','12','14','16','18','20','22','24','26','28','30']
; Decide on a color scale
CS,SCALE=1,NCOLS=33
; Use the elements of the box variable to set the boundaries of the map.
MAP,LONMIN=30,LONMAX=190,LATMIN=-50,LATMAX=20
; Draw the plot
CON,X=depresys1_longitude,Y=depresys1_latitude,FIELD=depresys5_regress_coeff_plot,/NOLINES,$
    TITLE='DJF Regression Coefficient of Rainfall and IOD from QUMP5(1958-2001)',$
    CB_TITLE='Regression Coefficient',/BLOCK,/CB_ALT
; Close
PSCLOSE,/NOVIEW

psfile6='/home/ss901165/idl/aus_decpred/Depresys6_IOD_precip_corr_coeff_djf.ps'
PSOPEN,file=psfile6,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=1300,XOFFSET=500,YOFFSET=2500,TFONT=2,$
       YSIZE=15000,TCHARSIZE=100,SPACE3=500,/PORTRAIT
; Set up the contour levels
LEVS,MANUAL=['-30','-28','-26','-24','-22','-20','-18','-16','-14','-12','-10','-8','-6','-4','-2','0','2','4','6','8','10','12','14','16','18','20','22','24','26','28','30']
; Decide on a color scale
CS,SCALE=1,NCOLS=33
; Use the elements of the box variable to set the boundaries of the map.
MAP,LONMIN=30,LONMAX=190,LATMIN=-50,LATMAX=20
; Draw the plot
CON,X=depresys1_longitude,Y=depresys1_latitude,FIELD=depresys6_regress_coeff_plot,/NOLINES,$
    TITLE='DJF Regression Coefficient of Rainfall and IOD from QUMP6(1958-2001)',$
    CB_TITLE='Regression Coefficient',/BLOCK,/CB_ALT
; Close
PSCLOSE,/NOVIEW

psfile7='/home/ss901165/idl/aus_decpred/Depresys7_IOD_precip_corr_coeff_djf.ps'
PSOPEN,file=psfile7,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=1300,XOFFSET=500,YOFFSET=2500,TFONT=2,$
       YSIZE=15000,TCHARSIZE=100,SPACE3=500,/PORTRAIT
; Set up the contour levels
LEVS,MANUAL=['-30','-28','-26','-24','-22','-20','-18','-16','-14','-12','-10','-8','-6','-4','-2','0','2','4','6','8','10','12','14','16','18','20','22','24','26','28','30']
; Decide on a color scale
CS,SCALE=1,NCOLS=33
; Use the elements of the box variable to set the boundaries of the map.
MAP,LONMIN=30,LONMAX=190,LATMIN=-50,LATMAX=20
; Draw the plot
CON,X=depresys1_longitude,Y=depresys1_latitude,FIELD=depresys7_regress_coeff_plot,/NOLINES,$
    TITLE='DJF Regression Coefficient of Rainfall and IOD from QUMP7(1958-2001)',$
    CB_TITLE='Regression Coefficient',/BLOCK,/CB_ALT
; Close
PSCLOSE,/NOVIEW

psfile8='/home/ss901165/idl/aus_decpred/Depresys8_IOD_precip_corr_coeff_djf.ps'
PSOPEN,file=psfile8,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=1300,XOFFSET=500,YOFFSET=2500,TFONT=2,$
       YSIZE=15000,TCHARSIZE=100,SPACE3=500,/PORTRAIT
; Set up the contour levels
LEVS,MANUAL=['-30','-28','-26','-24','-22','-20','-18','-16','-14','-12','-10','-8','-6','-4','-2','0','2','4','6','8','10','12','14','16','18','20','22','24','26','28','30']
; Decide on a color scale
CS,SCALE=1,NCOLS=33
; Use the elements of the box variable to set the boundaries of the map.
MAP,LONMIN=30,LONMAX=190,LATMIN=-50,LATMAX=20
; Draw the plot
CON,X=depresys1_longitude,Y=depresys1_latitude,FIELD=depresys8_regress_coeff_plot,/NOLINES,$
    TITLE='DJF Regression Coefficient of Rainfall and IOD from QUMP8(1958-2001)',$
    CB_TITLE='Regression Coefficient',/BLOCK,/CB_ALT
; Close
PSCLOSE,/NOVIEW

psfile0='/home/ss901165/idl/aus_decpred/Depresys0_IOD_precip_corr_coeff_djf.ps'
PSOPEN,file=psfile0,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=1300,XOFFSET=500,YOFFSET=2500,TFONT=2,$
       YSIZE=15000,TCHARSIZE=100,SPACE3=500,/PORTRAIT
; Set up the contour levels
LEVS,MANUAL=['-30','-28','-26','-24','-22','-20','-18','-16','-14','-12','-10','-8','-6','-4','-2','0','2','4','6','8','10','12','14','16','18','20','22','24','26','28','30']
; Decide on a color scale
CS,SCALE=1,NCOLS=33
; Use the elements of the box variable to set the boundaries of the map.
MAP,LONMIN=30,LONMAX=190,LATMIN=-50,LATMAX=20
; Draw the plot
CON,X=depresys1_longitude,Y=depresys1_latitude,FIELD=depresys0_regress_coeff_plot,/NOLINES,$
    TITLE='DJF Regression Coefficient of Rainfall and IOD from QUMP0(1958-2001)',$
    CB_TITLE='Regression Coefficient',/BLOCK,/CB_ALT
; Close
PSCLOSE,/NOVIEW

STOP
END
