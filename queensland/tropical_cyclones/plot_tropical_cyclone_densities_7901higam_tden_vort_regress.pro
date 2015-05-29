PRO plot_tropical_cyclone_densities_7901higam_tden_vort_regress

; Create a PostScript plot of the climatological tropical-cyclone densities
; from either observations or the HiGEM model.
; This plot describes the difference between tropical cyclone track
; density in the 1950-2008 period and the HiGEM data. 

; Give the netCDF input file containing the output from the TRACK program
input_file='/home/ss901165/higam_qccce/hpcx_amip2_xcquc/tropical_cyclones/stat_trs_scl.oct-may_smeans.h9-k1.HiGAM_amip2.nc'
vort_file='/home/ss901165/higam_qccce/es_amip2_eaeua/higam_eaeua.dec-feb_smeans.h8-k1.sfvp.global_domain.nc'

; Give the boundaries of the region that you want to plot, using the form
; [southern_latitude,western_longitude,northern_latitude,eastern_longitude]
box=[-45,80,0,250]
tcbox=[-25,120,0,200]
; This code reads the longitude and latitude information from the netCDF file.
; Restrict the longitude and latitude to the region defined by [box] above.
longitude=OPEN_AND_EXTRACT(input_file,'long')
latitude=OPEN_AND_EXTRACT(input_file,'lat')
DEFINE_BOUNDARIES,tcbox,latitude,longitude,tcbox_tx,/LIMIT
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)

longitude_vort=OPEN_AND_EXTRACT(vort_file,'longitude')
latitude_vort=OPEN_AND_EXTRACT(vort_file,'latitude')
DEFINE_BOUNDARIES,box,latitude_vort,longitude_vort,box_tx,/LIMIT
n_lon_vort=N_ELEMENTS(longitude_vort)
n_lat_vort=N_ELEMENTS(latitude_vort)

; Get the number of longitude and latitude points in the region.
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)
n_lon_vort=N_ELEMENTS(longitude_vort)
n_lat_vort=N_ELEMENTS(latitude_vort)

; Define array of years.
years=['1979-1980','1980-1981','1981-1982','1982-1983','1983-1984','1984-1985','1985-1986','1986-1987','1987-1988','1988-1989','1989-1990','1990-1991','1991-1992','1992-1993','1993-1994','1994-1995','1995-1996','1996-1997','1997-1998','1998-1999','1999-2000','2000-2001','2001-2002']
n_years=N_ELEMENTS(years)

; Read the climatological track density from the netCDF input file.
; If you want to read a different variable from the netCDF file, replace 'tden' 
; with the variable you want to read (e.g., 'gden' for genesis density
; and 'lden' for lysis density).
higem_nyears=23
track_density=OPEN_AND_EXTRACT(input_file,'tden',$
                                    offset=[tcbox_tx(1),tcbox_tx(0),0],$
                                    count=[n_lon,n_lat,higem_nyears])
vort=OPEN_AND_EXTRACT(vort_file,'vor',$
                                    offset=[box_tx(1),box_tx(0),0],$
                                    count=[n_lon_vort,n_lat_vort,higem_nyears])

TC_area_average=fltarr(n_years)
FOR i=0,n_years-1 DO BEGIN
   TC_area_average(i)=MEAN(track_density(*,*,i))
ENDFOR

regressions=fltarr(n_lon_vort,n_lat_vort)
correlations=fltarr(n_lon_vort,n_lat_vort)

FOR i=0,n_lon_vort-1 DO BEGIN
   FOR j=0,n_lat_vort-1 DO BEGIN
      regressions(i,j)=REGRESS(TC_area_average(*),REFORM(vort(i,j,*)),CORRELATION=temp)
      correlations(i,j)=temp(0)
   ENDFOR
ENDFOR

; The track density is in units of storms per 10^6 km per month, so we want to
; multiply by eight to convert into units of storm per 10^6 per season, assuming
; that the Southern Hemisphere season lasts from October through May (eight months).
track_density=track_density

; Use the array below to define the contour levels for the plot.  Expand by adding more
; comma-separated values.
contour_levels=[-1,-0.75,-0.5,-0.25,0,0.25,0.5,0.75,1]

; Define the path to the PostScript file you want to create
;psfile='/home/swu07adk/idl
psfile='/home/ss901165/idl/queensland/tropical_cyclones/test.ps'

; Open the PostScript file.  See the IDL Guide for details on all of the options used here.
; Note that the dollar sign continues long commands on the next line.
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1500,XOFFSET=1000,YOFFSET=1500,$
       TFONT=2,TCHARSIZE=100,CB_WIDTH=110,SPACE3=500,YSIZE=10000

; Define the colour scale to use.  See the IDL Guide for the possible scales.
CS,SCALE=26,NCOLS=N_ELEMENTS(contour_levels)+1,white=[6,7]

; Set up the contour levels, using the array defined above.
LEVS,MANUAL=contour_levels

; Draw the map, based on the [box] defined near the top
MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=80,LONMAX=250

; Draw the contour plot.  See the IDL Guide for possible options.  Note that !U makes a superscript and !N returns
; to normal text.
CON,X=longitude_vort,Y=latitude_vort,FIELD=regressions,/NOLINELABELS,$
    TITLE='Tropical-cyclone track density from HiGAM data correlated with near-surface vorticity from HiGAM'

sig_level=0.413
FOR i=0,n_lon_vort-1 DO BEGIN
   FOR j=0,n_lat_vort-1 DO BEGIN
      IF (ABS(correlations(i,j)) gt sig_level) THEN GPLOT,X=longitude_vort(i),Y=latitude_vort(j),SYM=4,SIZE=70
   ENDFOR
ENDFOR
; Close the PostScript file.  Use the /NOVIEW option to stop the plot from being displayed automatically.
PSCLOSE
; PSCLOSE,/NOVIEW

STOP

END
