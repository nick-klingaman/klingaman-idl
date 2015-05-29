PRO plot_tropical_cyclone_densities_7801higam_rh

; Create a PostScript plot of the climatological tropical-cyclone densities
; from either observations or the HiGEM model.
; This plot describes the difference between tropical cyclone track
; density in the 1950-2008 period and the HiGEM data. 

; Give the netCDF input file containing the output from the TRACK program
input_file='/home/ss901165/higam_qccce/es_amip2_eaeua/higam_eaeua.dec-feb_smeans.h8-k1.rh500.pac_domain.nc'

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
higem_nyears=23
track_density=OPEN_AND_EXTRACT(input_file,'rh',$
                                    offset=[box_tx(1),box_tx(0)],$
                                    count=[n_lon,n_lat])

; The track density is in units of storms per 10^6 km per month, so we want to
; multiply by eight to convert into units of storm per 10^6 per season, assuming
; that the Southern Hemisphere season lasts from October through May (eight months).
track_density=track_density

; Use the array below to define the contour levels for the plot.  Expand by adding more
; comma-separated values.
contour_levels=[2,4,6,8,10,12]

; Define the path to the PostScript file you want to create
psfile='/home/swu07adk/higam/7801higam_rh.ps'

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
    TITLE='500hPa Relative Humidity map from HiGAM 1978-2001'

; Close the PostScript file.  Use the /NOVIEW option to stop the plot from being displayed automatically.
PSCLOSE
; PSCLOSE,/NOVIEW

STOP

END

