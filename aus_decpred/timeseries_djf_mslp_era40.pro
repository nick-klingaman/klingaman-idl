PRO timeseries_djf_mslp_era40

; This program plots the timeseries of djf mslp over Australia

; Give the input files.
era40_file='/home/ss901165/datasets/ERA40/MSL/era40.dec-feb_smeans.1958-2001.msl.n48.nc'

depresys1_file='/home/swr06jir/links/elder/PPE/trans/atmos/qump1/qump1_MSLP_djf_DATA.nc'

depresys2_file='/home/swr06jir/links/elder/PPE/trans/atmos/qump2/qump2_MSLP_djf_DATA.nc'

depresys3_file='/home/swr06jir/links/elder/PPE/trans/atmos/qump3/qump3_MSLP_djf_DATA.nc'

depresys4_file='/home/swr06jir/links/elder/PPE/trans/atmos/qump4/qump4_MSLP_djf_DATA.nc'

depresys5_file='/home/swr06jir/links/elder/PPE/trans/atmos/qump5/qump5_MSLP_djf_DATA.nc'

depresys6_file='/home/swr06jir/links/elder/PPE/trans/atmos/qump6/qump6_MSLP_djf_DATA.nc'

depresys7_file='/home/swr06jir/links/elder/PPE/trans/atmos/qump7/qump7_MSLP_djf_DATA.nc'

depresys8_file='/home/swr06jir/links/elder/PPE/trans/atmos/qump8/qump8_MSLP_djf_DATA.nc'

depresys0_file='/home/swr06jir/links/elder/PPE/trans/atmos/qump0/qump0_MSLP_djf_DATA.nc'

; Define the region to plot as a box.  The values are:
; [starting_latitude,starting_longitude,stopping_latitude,stopping_longitude]
box=[-60,80,10,240]

; Number of years to read
n_years=44

; Read longitude and latitude co-ordinate variables from the mslp file.
era40_longitude=OPEN_AND_EXTRACT(era40_file,'longitude')
era40_latitude=OPEN_AND_EXTRACT(era40_file,'latitude')
; Restrict the dimensions to the box specified above.
DEFINE_BOUNDARIES,box,era40_latitude,era40_longitude,era40_box_tx,/LIMIT
; Get the number of longitude and latitude points in the "box" region.
era40_nlon=N_ELEMENTS(era40_longitude)
era40_nlat=N_ELEMENTS(era40_latitude)

depresys1_longitude=OPEN_AND_EXTRACT(depresys1_file,'longitude')
depresys1_latitude=OPEN_AND_EXTRACT(depresys1_file,'latitude')
; Restrict the dimensions to the box specified above.
DEFINE_BOUNDARIES,box,depresys1_latitude,depresys1_longitude,depresys1_box_tx,/LIMIT
; Get the number of longitude and latitude points in the "box" region.
depresys1_nlon=N_ELEMENTS(depresys1_longitude)
depresys1_nlat=N_ELEMENTS(depresys1_latitude)

era40_msl=OPEN_AND_EXTRACT(era40_file,'MSL',$
                               offset=[era40_box_tx(1),era40_box_tx(0),0],$
                               count=[era40_nlon,era40_nlat,n_years])

era40_mslp=era40_msl / 10

depresys1_msl=OPEN_AND_EXTRACT(depresys1_file,'p',$
                               offset=[depresys1_box_tx(1),depresys1_box_tx(0),0,58],$
                               count=[depresys1_nlon,depresys1_nlat,1,n_years])

depresys1_mslp=depresys1_msl / 10

depresys2_msl=OPEN_AND_EXTRACT(depresys2_file,'p',$
                               offset=[depresys1_box_tx(1),depresys1_box_tx(0),0,58],$
                               count=[depresys1_nlon,depresys1_nlat,1,n_years])

depresys2_mslp=depresys2_msl / 10

depresys3_msl=OPEN_AND_EXTRACT(depresys3_file,'p',$
                               offset=[depresys1_box_tx(1),depresys1_box_tx(0),0,58],$
                               count=[depresys1_nlon,depresys1_nlat,1,n_years])

depresys3_mslp=depresys3_msl / 10

depresys4_msl=OPEN_AND_EXTRACT(depresys4_file,'p',$
                               offset=[depresys1_box_tx(1),depresys1_box_tx(0),0,58],$
                               count=[depresys1_nlon,depresys1_nlat,1,n_years])

depresys4_mslp=depresys4_msl / 10

depresys5_msl=OPEN_AND_EXTRACT(depresys5_file,'p',$
                               offset=[depresys1_box_tx(1),depresys1_box_tx(0),0,58],$
                               count=[depresys1_nlon,depresys1_nlat,1,n_years])

depresys5_mslp=depresys5_msl / 10

depresys6_msl=OPEN_AND_EXTRACT(depresys6_file,'p',$
                               offset=[depresys1_box_tx(1),depresys1_box_tx(0),0,58],$
                               count=[depresys1_nlon,depresys1_nlat,1,n_years])

depresys6_mslp=depresys6_msl / 10

depresys7_msl=OPEN_AND_EXTRACT(depresys7_file,'p',$
                               offset=[depresys1_box_tx(1),depresys1_box_tx(0),0,58],$
                               count=[depresys1_nlon,depresys1_nlat,1,n_years])

depresys7_mslp=depresys7_msl / 10

depresys8_msl=OPEN_AND_EXTRACT(depresys8_file,'p',$
                               offset=[depresys1_box_tx(1),depresys1_box_tx(0),0,58],$
                               count=[depresys1_nlon,depresys1_nlat,1,n_years])

depresys8_mslp=depresys8_msl / 10

depresys0_msl=OPEN_AND_EXTRACT(depresys0_file,'p',$
                               offset=[depresys1_box_tx(1),depresys1_box_tx(0),0,58],$
                               count=[depresys1_nlon,depresys1_nlat,1,n_years])

depresys0_mslp=depresys0_msl / 10

;make new variables that will hold the standard deviation for ERA40 amd DePreSys
;these variables have two dimensions

era40_stddev=fltarr(era40_nlon,era40_nlat)
depresys1_stddev=fltarr(depresys1_nlon,depresys1_nlat)
depresys2_stddev=fltarr(depresys1_nlon,depresys1_nlat)
depresys3_stddev=fltarr(depresys1_nlon,depresys1_nlat)
depresys4_stddev=fltarr(depresys1_nlon,depresys1_nlat)
depresys5_stddev=fltarr(depresys1_nlon,depresys1_nlat)
depresys6_stddev=fltarr(depresys1_nlon,depresys1_nlat)
depresys7_stddev=fltarr(depresys1_nlon,depresys1_nlat)
depresys8_stddev=fltarr(depresys1_nlon,depresys1_nlat)
depresys0_stddev=fltarr(depresys1_nlon,depresys1_nlat)

FOR i=0,era40_nlon-1 DO BEGIN
FOR j=0,era40_nlat-1 DO BEGIN
era40_stddev(i,j)=STDDEV(era40_mslp(i,j,*))
ENDFOR
ENDFOR

FOR i=0,depresys1_nlon-1 DO BEGIN
FOR j=0,depresys1_nlat-1 DO BEGIN
depresys1_stddev(i,j)=STDDEV(depresys1_mslp(i,j,*,*))
ENDFOR
ENDFOR

FOR i=0,depresys1_nlon-1 DO BEGIN
FOR j=0,depresys1_nlat-1 DO BEGIN
depresys2_stddev(i,j)=STDDEV(depresys2_mslp(i,j,*,*))
ENDFOR
ENDFOR

FOR i=0,depresys1_nlon-1 DO BEGIN
FOR j=0,depresys1_nlat-1 DO BEGIN
depresys3_stddev(i,j)=STDDEV(depresys3_mslp(i,j,*,*))
ENDFOR
ENDFOR

FOR i=0,depresys1_nlon-1 DO BEGIN
FOR j=0,depresys1_nlat-1 DO BEGIN
depresys4_stddev(i,j)=STDDEV(depresys4_mslp(i,j,*,*))
ENDFOR
ENDFOR

FOR i=0,depresys1_nlon-1 DO BEGIN
FOR j=0,depresys1_nlat-1 DO BEGIN
depresys5_stddev(i,j)=STDDEV(depresys5_mslp(i,j,*,*))
ENDFOR
ENDFOR

FOR i=0,depresys1_nlon-1 DO BEGIN
FOR j=0,depresys1_nlat-1 DO BEGIN
depresys6_stddev(i,j)=STDDEV(depresys6_mslp(i,j,*,*))
ENDFOR
ENDFOR

FOR i=0,depresys1_nlon-1 DO BEGIN
FOR j=0,depresys1_nlat-1 DO BEGIN
depresys7_stddev(i,j)=STDDEV(depresys7_mslp(i,j,*,*))
ENDFOR
ENDFOR

FOR i=0,depresys1_nlon-1 DO BEGIN
FOR j=0,depresys1_nlat-1 DO BEGIN
depresys8_stddev(i,j)=STDDEV(depresys8_mslp(i,j,*,*))
ENDFOR
ENDFOR

FOR i=0,depresys1_nlon-1 DO BEGIN
FOR j=0,depresys1_nlat-1 DO BEGIN
depresys0_stddev(i,j)=STDDEV(depresys0_mslp(i,j,*,*))
ENDFOR
ENDFOR

; Plot the data.  See the IDL Guide for how these commands work.

; Give the location of the PostScript file that this program will make.

psfile='/home/ss901165/idl/aus_decpred/timeseries_stddev_era40_djf_mslp.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=1300,XOFFSET=500,YOFFSET=2500,TFONT=2,$
       YSIZE=15000,TCHARSIZE=100,SPACE3=500,/PORTRAIT
; Set up the contour levels
levels=['0','2','4','6','8','10','12','14','16','18','20','22','24','26','28','30','32','34','36','38','40']
LEVS,MANUAL=levels
; Decide on a color scale
CS,COLS=[251,292,423],NCOLS=N_ELEMENTS(levels)+1
; Use the elements of the box variable to set the boundaries of the map.
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
; Draw the plot
CON,X=era40_longitude,Y=era40_latitude,FIELD=era40_stddev,$
    TITLE='DJF Timeseries MSLP Standard Deviation ERA40 N48 Resolution (1958-2001)',$
    CB_TITLE='Standard Devation (mb)',/BLOCK
; Close
PSCLOSE

psfile1='/home/ss901165/idl/aus_decpred/timeseries_stddev_depresys1_djf_mslp.ps'
PSOPEN,file=psfile1,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=1300,XOFFSET=500,YOFFSET=2500,TFONT=2,$
       YSIZE=15000,TCHARSIZE=100,SPACE3=500,/PORTRAIT
; Set up the contour levels
levels=['0','2','4','6','8','10','12','14','16','18','20','22','24','26','28','30','32','34','36','38','40']
LEVS,MANUAL=levels
; Decide on a color scale
CS,COLS=[251,292,423],NCOLS=N_ELEMENTS(levels)+1
; Use the elements of the box variable to set the boundaries of the map.
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
; Draw the plot
CON,X=depresys1_longitude,Y=depresys1_latitude,FIELD=depresys1_stddev,$
    TITLE='DJF Timeseries MSLP Standard Deviation DePreSys1 N48 Resolution (1958-2001)',$
    CB_TITLE='Standard Deviation (mb)',/BLOCK
; Close
PSCLOSE


psfile2='/home/ss901165/idl/aus_decpred/timeseries_stddev_depresys2_djf_mslp.ps'
PSOPEN,file=psfile2,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=1300,XOFFSET=500,YOFFSET=2500,TFONT=2,$
       YSIZE=15000,TCHARSIZE=100,SPACE3=500,/PORTRAIT
; Set up the contour levels
levels=['0','2','4','6','8','10','12','14','16','18','20','22','24','26','28','30','32','34','36','38','40']
LEVS,MANUAL=levels
; Decide on a color scale
CS,COLS=[251,292,423],NCOLS=N_ELEMENTS(levels)+1
; Use the elements of the box variable to set the boundaries of the map.
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
; Draw the plot
CON,X=depresys1_longitude,Y=depresys1_latitude,FIELD=depresys2_stddev,$
    TITLE='DJF Timeseries MSLP Standard Deviation DePreSys2 N48 Resolution (1958-2001)',$
    CB_TITLE='Standard Deviation (mb)',/BLOCK
; Close
PSCLOSE

psfile3='/home/ss901165/idl/aus_decpred/timeseries_stddev_depresys3_djf_mslp.ps'
PSOPEN,file=psfile3,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=1300,XOFFSET=500,YOFFSET=2500,TFONT=2,$
       YSIZE=15000,TCHARSIZE=100,SPACE3=500,/PORTRAIT
; Set up the contour levels
levels=['0','2','4','6','8','10','12','14','16','18','20','22','24','26','28','30','32','34','36','38','40']
LEVS,MANUAL=levels
; Decide on a color scale
CS,COLS=[251,292,423],NCOLS=N_ELEMENTS(levels)+1
; Use the elements of the box variable to set the boundaries of the map.
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
; Draw the plot
CON,X=depresys1_longitude,Y=depresys1_latitude,FIELD=depresys3_stddev,$
    TITLE='DJF Timeseries MSLP Standard Deviation DePreSys3 N48 Resolution (1958-2001)',$
    CB_TITLE='Standard Deviation (mb)',/BLOCK
; Close
PSCLOSE

psfile4='/home/ss901165/idl/aus_decpred/timeseries_stddev_depresys4_djf_mslp.ps'
PSOPEN,file=psfile4,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=1300,XOFFSET=500,YOFFSET=2500,TFONT=2,$
       YSIZE=15000,TCHARSIZE=100,SPACE3=500,/PORTRAIT
; Set up the contour levels
levels=['0','2','4','6','8','10','12','14','16','18','20','22','24','26','28','30','32','34','36','38','40']
LEVS,MANUAL=levels
; Decide on a color scale
CS,COLS=[251,292,423],NCOLS=N_ELEMENTS(levels)+1
; Use the elements of the box variable to set the boundaries of the map.
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
; Draw the plot
CON,X=depresys1_longitude,Y=depresys1_latitude,FIELD=depresys4_stddev,$
    TITLE='DJF Timeseries MSLP Standard Deviation DePreSys4 N48 Resolution (1958-2001)',$
    CB_TITLE='Standard Deviation (mb)',/BLOCK
; Close
PSCLOSE

psfile5='/home/ss901165/idl/aus_decpred/timeseries_stddev_depresys5_djf_mslp.ps'
PSOPEN,file=psfile5,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=1300,XOFFSET=500,YOFFSET=2500,TFONT=2,$
       YSIZE=15000,TCHARSIZE=100,SPACE3=500,/PORTRAIT
; Set up the contour levels
levels=['0','2','4','6','8','10','12','14','16','18','20','22','24','26','28','30','32','34','36','38','40']
LEVS,MANUAL=levels
; Decide on a color scale
CS,COLS=[251,292,423],NCOLS=N_ELEMENTS(levels)+1
; Use the elements of the box variable to set the boundaries of the map.
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
; Draw the plot
CON,X=depresys1_longitude,Y=depresys1_latitude,FIELD=depresys5_stddev,$
    TITLE='DJF Timeseries MSLP Standard Deviation DePreSys5 N48 Resolution (1958-2001)',$
    CB_TITLE='Standard Deviation (mb)',/BLOCK
; Close
PSCLOSE

psfile6='/home/ss901165/idl/aus_decpred/timeseries_stddev_depresys6_djf_mslp.ps'
PSOPEN,file=psfile6,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=1300,XOFFSET=500,YOFFSET=2500,TFONT=2,$
       YSIZE=15000,TCHARSIZE=100,SPACE3=500,/PORTRAIT
; Set up the contour levels
levels=['0','2','4','6','8','10','12','14','16','18','20','22','24','26','28','30','32','34','36','38','40']
LEVS,MANUAL=levels
; Decide on a color scale
CS,COLS=[251,292,423],NCOLS=N_ELEMENTS(levels)+1
; Use the elements of the box variable to set the boundaries of the map.
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
; Draw the plot
CON,X=depresys1_longitude,Y=depresys1_latitude,FIELD=depresys6_stddev,$
    TITLE='DJF Timeseries MSLP Standard Deviation DePreSys6 N48 Resolution (1958-2001)',$
    CB_TITLE='Standard Deviation (mb)',/BLOCK
; Close
PSCLOSE

psfile7='/home/ss901165/idl/aus_decpred/timeseries_stddev_depresys7_djf_mslp.ps'
PSOPEN,file=psfile7,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=1300,XOFFSET=500,YOFFSET=2500,TFONT=2,$
       YSIZE=15000,TCHARSIZE=100,SPACE3=500,/PORTRAIT
; Set up the contour levels
levels=['0','2','4','6','8','10','12','14','16','18','20','22','24','26','28','30','32','34','36','38','40']
LEVS,MANUAL=levels
; Decide on a color scale
CS,COLS=[251,292,423],NCOLS=N_ELEMENTS(levels)+1
; Use the elements of the box variable to set the boundaries of the map.
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
; Draw the plot
CON,X=depresys1_longitude,Y=depresys1_latitude,FIELD=depresys7_stddev,$
    TITLE='DJF Timeseries MSLP Standard Deviation DePreSys7 N48 Resolution (1958-2001)',$
    CB_TITLE='Standard Deviation (mb)',/BLOCK
; Close
PSCLOSE

psfile8='/home/ss901165/idl/aus_decpred/timeseries_stddev_depresys8_djf_mslp.ps'
PSOPEN,file=psfile8,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=1300,XOFFSET=500,YOFFSET=2500,TFONT=2,$
       YSIZE=15000,TCHARSIZE=100,SPACE3=500,/PORTRAIT
; Set up the contour levels
levels=['0','2','4','6','8','10','12','14','16','18','20','22','24','26','28','30','32','34','36','38','40']
LEVS,MANUAL=levels
; Decide on a color scale
CS,COLS=[251,292,423],NCOLS=N_ELEMENTS(levels)+1
; Use the elements of the box variable to set the boundaries of the map.
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
; Draw the plot
CON,X=depresys1_longitude,Y=depresys1_latitude,FIELD=depresys8_stddev,$
    TITLE='DJF Timeseries MSLP Standard Deviation DePreSys8 N48 Resolution (1958-2001)',$
    CB_TITLE='Standard Deviation (mb)',/BLOCK
; Close
PSCLOSE

psfile0='/home/ss901165/idl/aus_decpred/timeseries_stddev_depresys0_djf_mslp.ps'
PSOPEN,file=psfile0,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=1300,XOFFSET=500,YOFFSET=2500,TFONT=2,$
       YSIZE=15000,TCHARSIZE=100,SPACE3=500,/PORTRAIT
; Set up the contour levels
levels=['0','2','4','6','8','10','12','14','16','18','20','22','24','26','28','30','32','34','36','38','40']
LEVS,MANUAL=levels
; Decide on a color scale
CS,COLS=[251,292,423],NCOLS=N_ELEMENTS(levels)+1
; Use the elements of the box variable to set the boundaries of the map.
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
; Draw the plot
CON,X=depresys1_longitude,Y=depresys1_latitude,FIELD=depresys0_stddev,$
    TITLE='DJF Timeseries MSLP Standard Deviation DePreSys0 N48 Resolution (1958-2001)',$
    CB_TITLE='Standard Deviation (mb)',/BLOCK
; Close
PSCLOSE

STOP
END
