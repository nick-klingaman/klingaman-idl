PRO aus_decpred_compare_silo_depresys_meanrain
  
; All IDL programs begin with a "PRO [name]" statement.  The [name] should
; be the same as the name of this file, without the .pro extension.

; This program plots the mean observed rainfall over Australia from
; the SILO dataset, at climate-model horizontal resolution (called N48).

; Give the input file of Australian rainfall from SILO.
silo_file='/home/ss901165/datasets/SILO/n48/SILO.jan-dec_amean_clim.1958-2001.precip.n48.nc'
; There are also files available for each season, called "dec-feb_smean_clim", "mar-may_smean_clim",
; "jun-aug_smean_clim" and "sep-nov_smean_clim".

; Give the identifier for the DePreSys model integration.  There are nine to choose from:
; 'std' and 'qump1' through 'qump8'
model_id='ensmean'
; This line builds the filename for the model, based on the model_id above.
qump_file='/home/ss901165/datasets/DEPRESYS/precip/'+model_id+'_precip_1958_2001_clim_ann.nc'
; The seasonal climatologies for the model have similar names, but end in 'djf' 'mam' 'jja' and 'son
; instead of 'ann'.

; Give the input file containing the land/sea mask data.  We need this because the rainfall
; observations are valid only over land.  This mask is the same as that used in the climate model.
mask_file='/home/ss901165/um_output/mask_n48.nc'

; Define the region to plot as a box.  The values are:
; [starting_latitude,starting_longitude,stopping_latitude,stopping_longitude]
box=[-40,112,-10,155]

; Read longitude and latitude co-ordinate variables from the rainfall file.
silo_longitude=OPEN_AND_EXTRACT(silo_file,'longitude')
silo_latitude=OPEN_AND_EXTRACT(silo_file,'latitude')
; Restrict the dimensions to the box specified above.
DEFINE_BOUNDARIES,box,silo_latitude,silo_longitude,silo_box_tx,/LIMIT
; Get the number of longitude and latitude points in the "box" region.
silo_nlon=N_ELEMENTS(silo_longitude)
silo_nlat=N_ELEMENTS(silo_latitude)

; Read longitude and latitude co-ordinate variables from the rainfall file.
qump_longitude=OPEN_AND_EXTRACT(qump_file,'longitude')
qump_latitude=OPEN_AND_EXTRACT(qump_file,'latitude')
; Restrict the dimensions to the box specified above.
DEFINE_BOUNDARIES,box,qump_latitude,qump_longitude,qump_box_tx,/LIMIT
; Get the number of longitude and latitude points in the "box" region.
qump_nlon=N_ELEMENTS(qump_longitude)
qump_nlat=N_ELEMENTS(qump_latitude)

; Read longitude and latitude co-ordinate variables from the land/sea mask file
mask_longitude=OPEN_AND_EXTRACT(mask_file,'longitude')
mask_latitude=OPEN_AND_EXTRACT(mask_file,'latitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)

; Read the rainfall data from the "rain" variable in the input file.
; Values are in mm/day - you could try converting to mm/year.
silo_rainfall=OPEN_AND_EXTRACT(silo_file,'rain',$
                               offset=[silo_box_tx(1),silo_box_tx(0)],$
                               count=[silo_nlon,silo_nlat])
; Read the rainfall data from the "rain" variable in the input file.
; Values are in mm/day - you could try converting to mm/year.
qump_rainfall=OPEN_AND_EXTRACT(qump_file,'precip',$
                               offset=[qump_box_tx(1),qump_box_tx(0)],$
                               count=[qump_nlon,qump_nlat])*86400.

; Read the land/sea mask in the same way.  The variable is called "lsm"
mask=OPEN_AND_EXTRACT(mask_file,'lsm',$
                      offset=[mask_box_tx(1),mask_box_tx(0)],$
                      count=[mask_nlon,mask_nlat])

; Apply the mask to the data.  All values of "0" in the mask are ocean; values of "1" 
; are land points.  Set all the ocean points to a missing value (!Values.F_NaN)
silo_rainfall[where(mask eq 0)]=!Values.F_NaN
qump_rainfall[where(mask eq 0)]=!Values.F_NaN

; Plot the data.  See the IDL Guide for how these commands work.

; Give the location of the PostScript file that this program will make.
; This needs to be somewhere in your /home directory.
psfile='/home/ss901165/idl/aus_decpred/aus_decpred_plot_mean_rainfall.silo_annual_mean.n48_block.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=1300,XOFFSET=500,YOFFSET=2500,TFONT=2,$
       YSIZE=15000,XSIZE=18000,TCHARSIZE=100,SPACE3=500,/EPS
; Set up the contour levels
levels=['0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5','1.7','1.9','2.1','2.3','2.5','2.7','2.9','3.1']
LEVS,MANUAL=levels
; Decide on a color scale
CS,SCALE=1,NCOLS=N_ELEMENTS(levels)+1,/REV
; Use the elements of the box variable to set the boundaries of the map.
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
; Draw the contour plot
CON,X=silo_longitude,Y=silo_latitude,FIELD=silo_rainfall,$
    TITLE='Annual-mean rainfall from the '+model_id+' integration at N48 resolution (1958-2001)',$
    CB_TITLE='Precipitation (mm day!U-1!N)',/BLOCK,/NOLINES
; Close
PSCLOSE,/NOVIEW

; Give the location of the PostScript file that this program will make.
; This needs to be somewhere in your /home directory.
psfile='/home/ss901165/idl/aus_decpred/aus_decpred_plot_mean_rainfall.'+model_id+'_annual_mean.n48_block.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=1300,XOFFSET=500,YOFFSET=2500,TFONT=2,$
       YSIZE=15000,XSIZE=18000,TCHARSIZE=100,SPACE3=500,/EPS
; Set up the contour levels
levels=['0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5','1.7','1.9','2.1','2.3','2.5','2.7','2.9','3.1']
LEVS,MANUAL=levels
; Decide on a color scale
CS,SCALE=1,NCOLS=N_ELEMENTS(levels)+1,/REV
; Use the elements of the box variable to set the boundaries of the map.
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
; Draw the contour plot
CON,X=qump_longitude,Y=qump_latitude,FIELD=qump_rainfall,$
    TITLE='Annual-mean rainfall from the '+model_id+' integration at N48 resolution (1958-2001)',$
    CB_TITLE='Precipitation (mm day!U-1!N)',/BLOCK,/NOLINES
; Close
PSCLOSE,/NOVIEW

; Give the location of the PostScript file that this program will make.
; This needs to be somewhere in your /home directory.
psfile='/home/ss901165/idl/aus_decpred/aus_decpred_plot_mean_rainfall.'+model_id+'-minus-silo_annual_mean.n48_block.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=1300,XOFFSET=500,YOFFSET=2500,TFONT=2,$
       YSIZE=15000,XSIZE=18000,TCHARSIZE=100,SPACE3=500,/EPS
; Set up the contour levels
levels=['-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5']
LEVS,MANUAL=levels
; Decide on a color scale
CS,SCALE=1,NCOLS=N_ELEMENTS(levels)+1,/REV
; Use the elements of the box variable to set the boundaries of the map.
MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
; Draw the contour plot
CON,X=qump_longitude,Y=qump_latitude,FIELD=qump_rainfall-silo_rainfall,$
    TITLE='Difference in annual-mean rainfall for '+model_id+' minus SILO at N48 resolution (1958-2001)',$
    CB_TITLE='Precipitation (mm day!U-1!N)',/BLOCK,/NOLINES
; Close
PSCLOSE

STOP
END
