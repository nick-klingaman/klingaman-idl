PRO plot_tropical_cyclone_densities_higam_tden_enlndiff

; Create a PostScript plot of the climatological tropical-cyclone densities
; from either observations or the HiGEM model.
; This plot describes the difference between tropical cyclone track
; density in the 1950-2008 period and the HiGEM data. 

; Give the netCDF input file containing the output from the TRACK program
input_file='/home/ss901165/higam_qccce/hpcx_amip2_xcquc/tropical_cyclones/stat_trs_scl.oct-may_smeans.h9-k1.HiGAM_amip2.nc'
nino4_file='/home/ss901165/datasets/NINO/nino4_hadisst.sep-nov_smeans.1979-2007.nc'

; Give the boundaries of the region that you want to plot, using the form
; [southern_latitude,western_longitude,northern_latitude,eastern_longitude]
box=[-45,80,0,250]

; This code reads the longitude and latitude information from the netCDF file.
longitude=OPEN_AND_EXTRACT(input_file,'long')
latitude=OPEN_AND_EXTRACT(input_file,'lat')
; Restrict the longitude and latitude to the region defined by [box] above.
DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
; Get the number of longitude and latitude points in the region.
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)

; Read the climatological track density from the netCDF input file.
; If you want to read a different variable from the netCDF file, replace 'tden' 
; with the variable you want to read (e.g., 'gden' for genesis density
; and 'lden' for lysis density).
Higem_nyears=23
track_densities=OPEN_AND_EXTRACT(input_file,'tden',$
                                    offset=[box_tx(1),box_tx(0),0],$
                                    count=[n_lon,n_lat,Higem_nyears])
nino4=OPEN_AND_EXTRACT(nino4_file,'NINO4')

nino4_anomalies=nino4-MEAN(nino4)
elnino_years=where(nino4_anomalies gt 0.5)
lanina_years=where(nino4_anomalies lt -0.5)

mean_elnino=fltarr(n_lon,n_lat)

FOR i=0,n_lon-1 DO BEGIN
FOR j=0,n_lat-1 DO BEGIN
this_point_track_densities=REFORM(track_densities(i,j,*))
el_nino_track_densities=this_point_track_densities[elnino_years]
mean_elnino(i,j)=MEAN(el_nino_track_densities)
ENDFOR
ENDFOR

mean_lanina=fltarr(n_lon,n_lat)

FOR i=0,n_lon-1 DO BEGIN
FOR j=0,n_lat-1 DO BEGIN
this_point_track_densities=REFORM(track_densities(i,j,*))
la_nina_track_densities=this_point_track_densities[lanina_years]
mean_lanina(i,j)=MEAN(la_nina_track_densities)
ENDFOR
ENDFOR

enlndiff=mean_elnino-mean_lanina

; The track density is in units of storms per 10^6 km per month, so we want to
; multiply by eight to convert into units of storm per 10^6 per season, assuming
; that the Southern Hemisphere season lasts from October through May
; (eight months).

mean_elnino=mean_elnino*8
mean_lanina=mean_lanina*8

; Use the array below to define the contour levels for the plot.  Expand by adding more
; comma-separated values.
contour_levels=['-2.25','-1.95','-1.65','-1.35','-1.05','-0.75','-0.45','-0.15','0.15','0.45','0.75','1.05','1.35','1.65','1.95','2.25']

; Define the path to the PostScript file you want to create
psfile='/home/ss901165/idl/queensland/tropical_cyclones/higam_tden_enlndiff.ps'

; Open the PostScript file.  See the IDL Guide for details on all of the options used here.
; Note that the dollar sign continues long commands on the next line.
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1500,XOFFSET=1000,YOFFSET=1500,$
       TFONT=2,TCHARSIZE=100,CB_WIDTH=110,SPACE3=500,YSIZE=10000

; Define the colour scale to use.  See the IDL Guide for the possible scales.
CS,SCALE=26,NCOLS=N_ELEMENTS(contour_levels)+1,white=[10]

; Set up the contour levels, using the array defined above.
LEVS,MANUAL=contour_levels

; Draw the map, based on the [box] defined near the top
MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3)

; Draw the contour plot.  See the IDL Guide for possible options.  Note that !U makes a superscript and !N returns
; to normal text.
CON,X=longitude,Y=latitude,FIELD=enlndiff,/NOLINES,$
    TITLE='Difference in HiGAM modelled tropical-cyclone track density [storms year!U-1!N (10!U6!N km)!U-1!N] (El Nino-La Nina)'

; Close the PostScript file.  Use the /NOVIEW option to stop the plot from being displayed automatically.
PSCLOSE
; PSCLOSE,/NOVIEW

STOP

END

