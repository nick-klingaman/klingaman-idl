PRO plot_tropical_cyclone_densities_eraint_compare_higam

; Create a PostScript plot of the climatological tropical-cyclone densities
; from either observations or the HiGEM model. 

; Give the netCDF input file containing the output from the TRACK program
eraint_input_file='/home/ss901165/datasets/ERA-INTERIM/tropical_storms/stat_trs_scl.oct-may_smeans.1979-2009.era_interim.nc'
varname='gden'
;contour_levels=[2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34]
;contour_levels_diff=['-30','-26','-22','-18','-14','-10','-6','-2','2','6','10','14','18','22','26','30']
contour_levels=['0.4','0.8','1.2','1.6','2.0','2.4','2.8','3.2','3.6','4.0','4.4']
contour_levels_diff=['-3.0','-2.6','-2.2','-1.8','-1.4','-1.0','-0.6','-0.2','0.2','0.6','1.0','1.4','1.8','2.2','2.6','3.0']
;contour_levels=['0.2','0.4','0.6','0.8','1.0','1.2','1.4','1.6','1.8','2.0','2.2']
;contour_levels_diff=['-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5']
higam_input_file='/home/ss901165/higam_qccce/hpcx_amip2_xcquc/tropical_cyclones/stat_trs_scl.oct-may_smeans.h9-k1.HiGAM_amip2.nc'
ibtracs_input_file='/home/ss901165/datasets/IBTRACS/stat_trs_scl.oct-may_smeans.1979-2007.IBTRACS_SP.nc'

; Give the boundaries of the region that you want to plot, using the form
; [southern_latitude,western_longitude,northern_latitude,eastern_longitude]
box=[-45,90,0,250]

; This code reads the longitude and latitude information from the netCDF file.
longitude=OPEN_AND_EXTRACT(eraint_input_file,'long')
latitude=OPEN_AND_EXTRACT(eraint_input_file,'lat')
; Restrict the longitude and latitude to the region defined by [box] above.
DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
; Get the number of longitude and latitude points in the region.
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)

; Read the climatological track density from the netCDF input file.
; If you want to read a different variable from the netCDF file, replace 'tden' 
; with the variable you want to read (e.g., 'gden' for genesis density).
eraint_density=OPEN_AND_EXTRACT(eraint_input_file,varname,$
                                offset=[box_tx(1),box_tx(0),0],$
                                count=[n_lon,n_lat,23])
ibtracs_density=OPEN_AND_EXTRACT(ibtracs_input_file,varname,$
                                 offset=[box_tx(1),box_tx(0),0],$
                                 count=[n_lon,n_lat,23])*8.
higam_density=OPEN_AND_EXTRACT(higam_input_file,varname,$
                               offset=[box_tx(1),box_tx(0),0],$
                               count=[n_lon,n_lat,23])*8.

; Create a two-dimensional array of the mean of genesis density over
; all 23 years of the HiGAM run.
mean_eraint_density=fltarr(n_lon,n_lat)
mean_higam_density=fltarr(n_lon,n_lat)
mean_ibtracs_density=fltarr(n_lon,n_lat)

FOR i=0,n_lon-1 DO BEGIN
   FOR j=0,n_lat-1 DO BEGIN
      mean_eraint_density(i,j)=MEAN(eraint_density(i,j,*))
      mean_ibtracs_density(i,j)=MEAN(ibtracs_density(i,j,*))
      mean_higam_density(i,j)=MEAN(higam_density(i,j,*))
   ENDFOR
ENDFOR

; The track density is in units of storms per 10^6 km per month, so we want to
; multiply by eight to convert into units of storm per 10^6 per season, assuming
; that the Southern Hemisphere season lasts from October through May (eight months).
;mean_genesis_density=mean_genesis_density*8

; Use the array below to define the contour levels for the plot.  Expand by adding more
; comma-separated values.
;contour_levels=[3,6,9,12,15,18,21,24]

; Define the path to the PostScript file you want to create
psfile='/home/ss901165/idl/tropical_cyclones/plot_tropical_cyclone_densities_eraint_compare_higam.eraint_'+varname+'_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1500,XOFFSET=1000,YOFFSET=1500,$
       TFONT=2,TCHARSIZE=100,CB_WIDTH=110,SPACE3=500,YSIZE=10000
CS,SCALE=26,NCOLS=N_ELEMENTS(contour_levels)+1,white=[2]
LEVS,MANUAL=contour_levels
MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3)
CON,X=longitude,Y=latitude,FIELD=mean_eraint_density,/NOLINES,$
    TITLE=varname+' from ERA-Interim (79-01)'
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/tropical_cyclones/plot_tropical_cyclone_densities_eraint_compare_higam.higam_'+varname+'_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1500,XOFFSET=1000,YOFFSET=1500,$
       TFONT=2,TCHARSIZE=100,CB_WIDTH=110,SPACE3=500,YSIZE=10000
CS,SCALE=26,NCOLS=N_ELEMENTS(contour_levels)+1,white=[2]
LEVS,MANUAL=contour_levels
MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3)
CON,X=longitude,Y=latitude,FIELD=mean_higam_density,/NOLINES,$
    TITLE=varname+' from HiGAM (79-01)'
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/tropical_cyclones/plot_tropical_cyclone_densities_eraint_compare_higam.ibtracs_'+varname+'_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1500,XOFFSET=1000,YOFFSET=1500,$
       TFONT=2,TCHARSIZE=100,CB_WIDTH=110,SPACE3=500,YSIZE=10000
CS,SCALE=26,NCOLS=N_ELEMENTS(contour_levels)+1,white=[2]
LEVS,MANUAL=contour_levels
MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3)
CON,X=longitude,Y=latitude,FIELD=mean_ibtracs_density,/NOLINES,$
    TITLE=varname+' from IBTrACS (79-01)'
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/tropical_cyclones/plot_tropical_cyclone_densities_eraint_compare_higam.higam-minus-eraint_'+varname+'_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1500,XOFFSET=1000,YOFFSET=1500,$
       TFONT=2,TCHARSIZE=100,CB_WIDTH=110,SPACE3=500,YSIZE=10000
CS,SCALE=26,NCOLS=N_ELEMENTS(contour_levels_diff)+1,white=[10]
LEVS,MANUAL=contour_levels_diff
MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3)
CON,X=longitude,Y=latitude,FIELD=mean_higam_density-mean_eraint_density,/NOLINES,$
    TITLE='Diff in '+varname+' for HiGAM minus ERA-Interim (79-01)'
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/tropical_cyclones/plot_tropical_cyclone_densities_eraint_compare_higam.higam-minus-ibtracs_'+varname+'_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1500,XOFFSET=1000,YOFFSET=1500,$
       TFONT=2,TCHARSIZE=100,CB_WIDTH=110,SPACE3=500,YSIZE=10000
CS,SCALE=26,NCOLS=N_ELEMENTS(contour_levels_diff)+1,white=[10]
LEVS,MANUAL=contour_levels_diff
MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3)
CON,X=longitude,Y=latitude,FIELD=mean_higam_density-mean_ibtracs_density,/NOLINES,$
    TITLE='Diff in '+varname+' for HiGAM minus IBTrACS (79-01)'
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/tropical_cyclones/plot_tropical_cyclone_densities_eraint_compare_higam.eraint-minus-ibtracs_'+varname+'_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1500,XOFFSET=1000,YOFFSET=1500,$
       TFONT=2,TCHARSIZE=100,CB_WIDTH=110,SPACE3=500,YSIZE=10000
CS,SCALE=26,NCOLS=N_ELEMENTS(contour_levels_diff)+1,white=[10]
LEVS,MANUAL=contour_levels_diff
MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3)
CON,X=longitude,Y=latitude,FIELD=mean_eraint_density-mean_ibtracs_density,/NOLINES,$
    TITLE='Diff in '+varname+' for ERA-Interim minus IBTrACS (79-01)'
PSCLOSE,/NOVIEW

STOP

END

