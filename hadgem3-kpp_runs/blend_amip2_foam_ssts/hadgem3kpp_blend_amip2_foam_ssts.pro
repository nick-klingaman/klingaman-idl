PRO hadgem3kpp_blend_amip2_foam_ssts

; Create a netCDF file that contains a blend of the FOAM and AMIP2
; climatological SSTs, using FOAM within the HadGEM3-KPP coupling
; region (user-specified) and the AMIP2 climatological SSTs outside
; that region.  Also, create a plot of the difference between the FOAM
; and AMIP2 climatological SSTs within the coupling region.

foam_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/FOAM_Jan-Dec_sst.clim.n96_masked.nc'
amip2_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/amip2.jan-dec_dmeans_clim.1982-2008.sst.nc'
blended_outfile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/FOAM_amip2_blend.jan-dec_dmeans_clim.1982-2008.sst.nc'

mask_infile='/home/ss901165/um_output/mask_n96_hadgem3-7.1.nc'

coupling_region=[-30,20,30,200]
; Number of points to use for a linear blend between FOAM and AMIP2 at
; the boundaries of the coupling region - the region will be centred
;                                         on the boundary - this should
;                                         be an odd number.
blending_points=5

; FOAM grid
foam_longitude=OPEN_AND_EXTRACT(foam_infile,'longitude')
foam_latitude=OPEN_AND_EXTRACT(foam_infile,'latitude')
foam_nlon=N_ELEMENTS(foam_longitude)
foam_nlat=N_ELEMENTS(foam_latitude)

; AMIP2 grid
amip2_longitude=OPEN_AND_EXTRACT(amip2_infile,'longitude')
amip2_latitude=OPEN_AND_EXTRACT(amip2_infile,'latitude')
amip2_nlon=N_ELEMENTS(amip2_longitude)
amip2_nlat=N_ELEMENTS(amip2_latitude)

; Read mask
lsm=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm'))

ndays=360

south_boundary=NEAREST(foam_latitude,coupling_region(0))
north_boundary=NEAREST(foam_latitude,coupling_region(2))
west_boundary=NEAREST(foam_longitude,coupling_region(1))
east_boundary=NEAREST(foam_longitude,coupling_region(3))

foam_ssts=OPEN_AND_EXTRACT(foam_infile,'sst',offset=[0,0,0],count=[foam_nlon,foam_nlat,ndays])
amip2_ssts=OPEN_AND_EXTRACT(amip2_infile,'temp',offset=[0,0,0],count=[amip2_nlon,amip2_nlat,ndays])

blended_ssts=fltarr(amip2_nlon,amip2_nlat,ndays)
mask=fltarr(amip2_nlon,amip2_nlat)

mask(0:west_boundary-blending_points/2-1,*)=1. ; Outside coupling region in terms of longitude
mask(east_boundary+blending_points/2+1:amip2_nlon-1,*)=1. ; Outside coupling region in terms of longitude
mask(*,0:south_boundary-blending_points/2-1)=1. ; Outside coupling region in terms of latitude
mask(*,north_boundary+blending_points/2+1:amip2_nlat-1)=1. ; Outside coupling region in terms of latitude

mask(west_boundary+blending_points/2+1:east_boundary-blending_points/2-1,$
     south_boundary+blending_points/2+1:north_boundary-blending_points/2-1)=0. ; Inside coupling region

FOR j=south_boundary-blending_points/2,south_boundary+blending_points/2 DO $
   mask(west_boundary+blending_points/2+(j-south_boundary-blending_points/2):east_boundary-blending_points/2-(j-south_boundary-blending_points/2),j)=((south_boundary+blending_points/2)-j)/FLOAT(blending_points)

FOR j=north_boundary+blending_points/2,north_boundary-blending_points/2,-1 DO BEGIN
   print,j,j-north_boundary+blending_points/2
   mask(west_boundary+blending_points/2-(j-north_boundary+blending_points/2):east_boundary-blending_points/2+(j-(north_boundary-blending_points/2)),j)=(j-north_boundary+blending_points/2)/FLOAT(blending_points)
ENDFOR

FOR j=west_boundary-blending_points/2,west_boundary+blending_points/2 DO $
   mask(j,south_boundary+blending_points/2+(j-west_boundary-blending_points/2):north_boundary-blending_points/2-(j-west_boundary-blending_points/2))=((west_boundary+blending_points/2)-j)/FLOAT(blending_points)

FOR j=east_boundary+blending_points/2,east_boundary-blending_points/2,-1 DO $
   mask(j,south_boundary+blending_points/2-(j-east_boundary+blending_points/2):north_boundary-blending_points/2+(j-(east_boundary-blending_points/2)))=(j-east_boundary+blending_points/2)/FLOAT(blending_points)

FOR i=0,ndays-1 DO BEGIN
   today_amip2=REFORM(amip2_ssts(*,*,i))
   today_foam=REFORM(foam_ssts(*,*,i))
   today_blend=(mask*today_amip2)+((1-mask)*today_foam)
   blended_ssts(*,*,i)=today_blend
ENDFOR
amip2_clim=fltarr(amip2_nlon,amip2_nlat)
foam_clim=fltarr(amip2_nlon,amip2_nlat)
blended_clim=fltarr(amip2_nlon,amip2_nlat)
FOR i=0,amip2_nlon-1 DO BEGIN
   FOR j=0,amip2_nlat-1 DO BEGIN
      IF lsm(i,j) eq 0 THEN BEGIN
         amip2_clim(i,j)=MEAN(amip2_ssts(i,j,*))
         foam_clim(i,j)=MEAN(foam_ssts(i,j,*))
         blended_clim(i,j)=MEAN(blended_ssts(i,j,*))
      ENDIF ELSE BEGIN
         amip2_clim(i,j)=!Values.F_NaN
         foam_clim(i,j)=!Values.F_NaN
         blended_clim(i,j)=!Values.F_NaN
      ENDELSE
   ENDFOR
ENDFOR

id=NCDF_CREATE(blended_outfile,/CLOBBER)
dimids=intarr(3)
dimids(0)=NCDF_DIMDEF(id,'longitude',foam_nlon)
dimids(1)=NCDF_DIMDEF(id,'latitude',foam_nlat)
dimids(2)=NCDF_DIMDEF(id,'t',ndays)
varids=intarr(4)
varids(0)=NCDF_VARDEF(id,'longitude',[dimids(0)])
varids(1)=NCDF_VARDEF(id,'latitude',[dimids(1)])
varids(2)=NCDF_VARDEF(id,'t',[dimids(2)])
varids(3)=NCDF_VARDEF(id,'sst',[dimids(0),dimids(1),dimids(2)])
NCDF_CONTROL,id,/ENDEF
NCDF_VARPUT,id,varids(0),foam_longitude
NCDF_VARPUT,id,varids(1),foam_latitude
NCDF_VARPUT,id,varids(2),indgen(ndays)+0.5
NCDF_VARPUT,id,varids(3),blended_ssts
NCDF_CLOSE,id

mylevs=['-0.01','0.01','0.2','0.4','0.6','0.8','0.99','1.01']
psfile='/home/ss901165/idl/hadgem3-kpp_runs/blend_amip2_foam_ssts/hadgem3kpp_blend_amip2_foam_ssts.couple_region_mask.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=800,XOFFSET=1000,YOFFSET=1000,TFONT=2,TCHARSIZE=100,SPACE3=400
MAP,/HIRES
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
LEVS,MANUAL=mylevs,/EXACT
CON,X=amip2_longitude,Y=amip2_latitude,FIELD=mask,/NOLINES,TITLE='Mask for blending AMIP2 and FOAM climatological SSTs (1=AMIP2, 0=FOAM)',CB_WIDTH=100,/BLOCK
PSCLOSE,/NOVIEW

mylevs_raw=['20','21','22','23','24','25','26','27','28','29','30','31']
mylevs_diff=['-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5']

psfile='/home/ss901165/idl/hadgem3-kpp_runs/blend_amip2_foam_ssts/hadgem3kpp_blend_amip2_foam_ssts.clim_ssts.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=200,XOFFSET=1000,YOFFSET=1800,TFONT=2,TCHARSIZE=90,SPACE3=400,YPLOTS=3,SPACING=2000

POS,ypos=3,YSIZE=7000
CS,SCALE=4,NCOLS=N_ELEMENTS(mylevs_raw)+1
grey=FSC_COLOR("grey",30)
LEVS,MANUAL=mylevs_raw
MAP,LATMIN=coupling_region(0),LATMAX=coupling_region(2),/HIRES,LAND=30
CON,X=amip2_longitude,Y=amip2_latitude,FIELD=amip2_clim-273.15,/NOLINELABELS,TITLE='Annual-mean, clim AMIP2 SSTs for 1982-2008 (period of MORPH3 assessment run airxv)',/NOCOLBAR

POS,ypos=2,YSIZE=7000
CS,SCALE=4,NCOLS=N_ELEMENTS(mylevs_raw)+1
grey=FSC_COLOR("grey",30)
LEVS,MANUAL=mylevs_raw
MAP,LATMIN=coupling_region(0),LATMAX=coupling_region(2),/HIRES,LAND=30
CON,X=foam_longitude,Y=foam_latitude,FIELD=foam_clim-273.15,/NOLINELABELS,TITLE='Annual-mean, clim FOAM SSTs for 2002-2008 (used as forcing for original HadGEM3-KPP and HadGEM3-A runs)',/NOCOLBAR

POS,ypos=1,YSIZE=7000
CS,SCALE=4,NCOLS=N_ELEMENTS(mylevs_raw)+1
grey=FSC_COLOR("grey",30)
LEVS,MANUAL=mylevs_raw
MAP,LATMIN=coupling_region(0),LATMAX=coupling_region(2),/HIRES,LAND=30
CON,X=foam_longitude,Y=foam_latitude,FIELD=blended_clim-273.15,/NOLINELABELS,TITLE='Annual-mean of blended FOAM and AMIP2 climatological SSTs',/NOCOLBAR

COLBAR,COORDS=[6000,1500,24000,2000],LEVS=['0',mylevs_raw,'40'],TITLE='!Uo!N Celsius',/UPPER,/LOWER

PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3-kpp_runs/blend_amip2_foam_ssts/hadgem3kpp_blend_amip2_foam_ssts.diff_clim_ssts.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=200,XOFFSET=1000,YOFFSET=1800,TFONT=2,TCHARSIZE=90,SPACE3=400,YPLOTS=3,SPACING=2000

POS,ypos=3,YSIZE=7000
CS,SCALE=21,NCOLS=N_ELEMENTS(mylevs_diff)+1,white=[10]
LEVS,MANUAL=mylevs_diff
MAP,LATMIN=coupling_region(0),LATMAX=coupling_region(2),/HIRES
CON,X=foam_longitude,Y=foam_latitude,FIELD=blended_clim-foam_clim,/NOLINELABELS,TITLE='Difference in annual-mean SST between blended FOAM/AMIP2 and FOAM (blended-FOAM)',/NOCOLBAR

POS,ypos=2,YSIZE=7000
CS,SCALE=21,NCOLS=N_ELEMENTS(mylevs_diff)+1,white=[10]
LEVS,MANUAL=mylevs_diff
MAP,LATMIN=coupling_region(0),LATMAX=coupling_region(2),/HIRES
CON,X=amip2_longitude,Y=amip2_latitude,FIELD=blended_clim-amip2_clim,/NOLINELABELS,TITLE='Difference in annual-mean SST between blended FOAM/AMIP2 and AMIP2 (blended-AMIP2)',/NOCOLBAR

POS,ypos=1,YSIZE=7000
CS,SCALE=21,NCOLS=N_ELEMENTS(mylevs_diff)+1,white=[10]
LEVS,MANUAL=mylevs_diff
MAP,LATMIN=coupling_region(0),LATMAX=coupling_region(2),/HIRES
CON,X=amip2_longitude,Y=amip2_latitude,FIELD=foam_clim-amip2_clim,/NOLINELABELS,TITLE='Difference in annual-mean SST between FOAM (2002-2008) and AMIP2 (1982-2008) (FOAM-AMIP2)',/NOCOLBAR

COLBAR,COORDS=[6000,1500,24000,2000],LEVS=['-999',mylevs_diff,'999'],TITLE='!Uo!N Celsius',/UPPER,/LOWER

PSCLOSE,/NOVIEW


STOP

END
