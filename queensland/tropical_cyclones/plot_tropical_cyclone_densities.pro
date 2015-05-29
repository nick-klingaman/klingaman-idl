PRO plot_tropical_cyclone_densities

; Create a PostScript plot of the climatological tropical-cyclone densities
; from either observations or the HiGEM model. 

; Give the netCDF input file containing the output from the TRACK program
input_file='/home/ss901165/datasets/IBTRACS/stat_trs_scl.SIBT_79_08.nc'

; Give the boundaries of the region that you want to plot, using the form
; [southern_latitude,western_longitude,northern_latitude,eastern_longitude]
box=[-45,20,0,250]

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
; with the variable you want to read (e.g., 'gden' for genesis density).
track_density=OPEN_AND_EXTRACT(input_file,'tden',$
                               offset=[box_tx(1),box_tx(0)],$
                               count=[n_lon,n_lat])

; The track density is in units of storms per 10^6 km per month, so we want to
; multiply by eight to convert into units of storm per 10^6 per season, assuming
; that the Southern Hemisphere season lasts from October through May (eight months).
track_density=track_density*8

; Use the array below to define the contour levels for the plot.  Expand by adding more
; comma-separated values.
contour_levels=[2,4,6,8,10,12,14,16]

; Define the path to the PostScript file you want to create
psfile='/home/ss901165/idl/observed_tropical_cyclone_track_density.1979-2008.ps'

; Open the PostScript file.  See the IDL Guide for details on all of the options used here.
; Note that the dollar sign continues long commands on the next line.
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1500,XOFFSET=1000,YOFFSET=1500,$
       TFONT=2,TCHARSIZE=100,CB_WIDTH=110,SPACE3=500,YSIZE=10000

; Define the colour scale to use.  See the IDL Guide for the possible scales.
CS,SCALE=26,NCOLS=N_ELEMENTS(contour_levels)+1,white=[2]

; Set up the contour levels, using the array defined above.
LEVS,MANUAL=contour_levels

; Draw the map, based on the [box] defined near the top
MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3)

; Draw the contour plot.  See the IDL Guide for possible options.  Note that !U makes a superscript and !N returns
; to normal text.
CON,X=longitude,Y=latitude,FIELD=track_density,/NOLINELABELS,$
    TITLE='Tropical-cyclone track density [storms year!U-1!N (10!U6!N km)!U-1!N] for observations - 1979-2008'

; Close the PostScript file.  Use the /NOVIEW option to stop the plot from being displayed automatically.
PSCLOSE
; PSCLOSE,/NOVIEW

STOP

END

