PRO hadgem3kpp_bathymetry_check_lsm

; Check the HadGEM3-KPP bathymetry file against the land/sea mask
; to make sure we have negative ocean depths at all ocean points.
; Compare to another bathymetry file to look at difference in ocean depth.

; Bathymetry files
new_bathymetry_file='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/bathymetry/ETOPO2v2c_f4.mask_land.n96.nc'
old_bathymetry_file='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/bathymetry/lsm_ocndepth_for_kpp.n96.coastal_tiling.nearest_neighbor.um_73.nc'

; Land/sea fraction file
lsm_file='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/landfrac_n96_hadgem3-7.8.nc'

; Read bathymetry
new_bathymetry=OPEN_AND_EXTRACT(new_bathymetry_file,'max_depth')
old_bathymetry=OPEN_AND_EXTRACT(old_bathymetry_file,'max_depth')

; Read land/sea fraction
longitude=OPEN_AND_EXTRACT(lsm_file,'longitude')
latitude=OPEN_AND_EXTRACT(lsm_file,'latitude')
lsfrac=REFORM(OPEN_AND_EXTRACT(lsm_file,'lsm'))

; Mask bathymetry by lsfrac
new_bathymetry[where(lsfrac eq 1)]=9999
old_bathymetry[where(lsfrac eq 1)]=9999

temp_bathymetry=new_bathymetry
mylevs=['-6000','-5000','-4000','-3000','-2000','-1000','-800','-600','-400','-300','-200','-100','-50','0']
; Plot new bathymetry
psfile='/home/ss901165/idl/hadgem3-kpp_runs/bathymetry/hadgem3kpp_bathymetry_check_lsm.new_bathymetry.ps'
PSOPEN,file=psfile,TFONT=2,CHARSIZE=120,SPACE2=1500,YOFFSET=2000
GSET,XMIN=MIN(longitude),XMAX=MAX(longitude),YMIN=MIN(latitude),YMAX=MAX(latitude)
CS,COLS=[444,503]
LEVS,MANUAL=[9000,10000]
CON,X=longitude,Y=latitude,FIELD=temp_bathymetry,/NOLINES,/BLOCK,/NOCOLBAR
temp_bathymetry[where(temp_bathymetry eq 9999)]=!Values.F_NaN
CS,SCALE=24,NCOLS=N_ELEMENTS(mylevs),/REV
LEVS,MANUAL=mylevs
CON,X=longitude,Y=latitude,FIELD=temp_bathymetry,/NOLINES,/BLOCK,$
    TITLE='ETOP02 bathymetry dataset at N96 resolution',CB_TITLE='Depth of ocean (negative below surface)'
temp_bathymetry[where(temp_bathymetry lt 2e10)]=!Values.F_NaN
CS,COLS=[444,414]
LEVS,MANUAL=[0,2e21]
CON,X=longitude,Y=latitude,FIELD=temp_bathymetry,/NOLINES,/BLOCK,/NOCOLBAR
AXES,XSTEP=30,YSTEP=15
PSCLOSE

temp_bathymetry=old_bathymetry
mylevs=['-6000','-5000','-4000','-3000','-2000','-1000','-800','-600','-400','-300','-200','-100','-50','0']
; Plot new bathymetry
psfile='/home/ss901165/idl/hadgem3-kpp_runs/bathymetry/hadgem3kpp_bathymetry_check_lsm.old_bathymetry.ps'
PSOPEN,file=psfile,TFONT=2,CHARSIZE=120,SPACE2=1500,YOFFSET=2000
GSET,XMIN=MIN(longitude),XMAX=MAX(longitude),YMIN=MIN(latitude),YMAX=MAX(latitude)
CS,COLS=[444,503]
LEVS,MANUAL=[9000,10000]
CON,X=longitude,Y=latitude,FIELD=temp_bathymetry,/NOLINES,/BLOCK,/NOCOLBAR
temp_bathymetry[where(temp_bathymetry eq 9999)]=!Values.F_NaN
CS,SCALE=24,NCOLS=N_ELEMENTS(mylevs),/REV
LEVS,MANUAL=mylevs
CON,X=longitude,Y=latitude,FIELD=temp_bathymetry,/NOLINES,/BLOCK,$
    TITLE='FOAM-derived bathymetry dataset at N96 resolution',CB_TITLE='Depth of ocean (negative below surface)'
AXES,XSTEP=30,YSTEP=15
PSCLOSE,/NOVIEW

mylevs=['-750','-650','-550','-450','-350','-250','-150','-50','50','150','250','350','450','550','650','750']
; Plot new bathymetry
psfile='/home/ss901165/idl/hadgem3-kpp_runs/bathymetry/hadgem3kpp_bathymetry_check_lsm.new-minus-old_bathymetry.ps'
PSOPEN,file=psfile,TFONT=2,CHARSIZE=120,SPACE2=1500,YOFFSET=2000
GSET,XMIN=MIN(longitude),XMAX=MAX(longitude),YMIN=MIN(latitude),YMAX=MAX(latitude)
CS,COLS=[444,503]
LEVS,MANUAL=[9000,10000]
CON,X=longitude,Y=latitude,FIELD=old_bathymetry,/NOLINES,/BLOCK,/NOCOLBAR
old_bathymetry[where(old_bathymetry eq 9999)]=!Values.F_NaN
new_bathymetry[where(new_bathymetry eq 9999)]=!Values.F_NaN
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,white=[10]
LEVS,MANUAL=mylevs
CON,X=longitude,Y=latitude,FIELD=new_bathymetry-old_bathymetry,/NOLINES,/BLOCK,$
    TITLE='ETOP02 minus FOAM bathymetry at N96 resolution',CB_TITLE='Depth of ocean (negative below surface)'
AXES,XSTEP=30,YSTEP=15
PSCLOSE

STOP
END

