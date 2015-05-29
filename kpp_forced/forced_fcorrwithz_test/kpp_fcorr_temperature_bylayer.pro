PRO kpp_fcorr_temperature_bylayer,id,months

; Plot the mean difference in temperature at each layer in the FOAM
; vertical grid for a given KPP run and a given month.

; How many months of data do we have?
nmonths=N_ELEMENTS(months)

; Location of the N144 land-sea mask
mask_infile='/home/ss901165/um_output/mask_n144.nc'

; Loop over months
FOR i=0,nmonths-1 DO BEGIN
                                ; Location of input files (as monthly
                                ; means)
    coupled_infile='/home/ss901165/kpp_ocean/'+id+'/KPPocean.'+months(i)+'mean.ensmean.nc'
    foam_infile='/home/ss901165/datasets/NCOF_OCEAN/FOAM_'+months(i)+'mean_mixedlayer_temperature.clim.n144.nc'

    IF i eq 0 THEN BEGIN
        set_plot,'ps'
        min_val=-10
        max_val=10
        mylevs = [min_val,indgen(23)*0.25-2.75,max_val]
        intervals = N_ELEMENTS(mylevs)-1
        bottom = 3
        mycolors = indgen(intervals)+bottom
        LoadCT,33,ncolors=intervals,bottom=bottom
        white = FSC_COLOR("white",intervals/2+bottom-1)
        white = FSC_COLOR("white",intervals/2+bottom)        
        ticknames=[' ',LEVS2STRING(mylevs(1:N_ELEMENTS(mylevs)-2),2,5),' ']
        !p.position=[0.02,0.02,0.98,0.80]
        
                                ; Get latitude and longitude, restrict
                                ; global grid to regionally coupled grid
        coupled_latitude = OPEN_AND_EXTRACT(coupled_infile,'latitude')
        coupled_longitude = OPEN_AND_EXTRACT(coupled_infile,'longitude')
        coupled_z = OPEN_AND_EXTRACT(coupled_infile,'z')
        foam_latitude = OPEN_AND_EXTRACT(foam_infile,'latitude')
        foam_longitude = OPEN_AND_EXTRACT(foam_infile,'longitude')
        foam_z = OPEN_AND_EXTRACT(foam_infile,'z')
        coupled_nlon = N_ELEMENTS(coupled_longitude)
        coupled_nlat = N_ELEMENTS(coupled_latitude) 
        coupled_nz = N_ELEMENTS(coupled_z)
        foam_nlon = N_ELEMENTS(coupled_longitude)
        foam_nlat = N_ELEMENTS(coupled_latitude)
        foam_bottom = NEAREST(ABS(foam_z),ABS(coupled_z(coupled_nz-1)))
        foam_z = foam_z(0:foam_bottom)
        foam_nz = N_ELEMENTS(foam_z)
        DEFINE_BOUNDARIES,[MIN([coupled_latitude(0),coupled_latitude(coupled_nlat-1)]),$
                           MIN([coupled_longitude(0),coupled_longitude(coupled_nlon-1)]),$
                           MAX([coupled_latitude(0),coupled_latitude(coupled_nlat-1)]),$
                           MAX([coupled_longitude(0),coupled_longitude(coupled_nlon-1)])],$
          foam_latitude,foam_longitude,box_tx
        foam_coupled_offset_lat = box_tx[0]
        foam_coupled_offset_lon = box_tx[1]
        ; Read in the N144 land-sea mask for the coupling region 
        mask = REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',offset=[box_tx(1),box_tx(0),0,0],$
                                   count=[coupled_nlon,coupled_nlat,1,1]))
        coupled_temps_foamgrid=fltarr(coupled_nlon,coupled_nlat,foam_nz)
    ENDIF
                                ; Get temperatures
    coupled_temps = REFORM(OPEN_AND_EXTRACT(coupled_infile,'T',offset=[0,0,0],$
                                          count=[coupled_nlon,coupled_nlat,coupled_nz]))
    foam_temps = REFORM(OPEN_AND_EXTRACT(foam_infile,'sea_water_potential_temperature',$
                                       offset=[foam_coupled_offset_lon,foam_coupled_offset_lat,$
                                               0],$
                                       count=[coupled_nlon,coupled_nlat,foam_nz]))-273.15
    
    FOR j=0,coupled_nlon-1 DO BEGIN
        FOR k=0,coupled_nlat-1 DO BEGIN
            IF mask(j,k) eq 0 THEN BEGIN
                                ; Use a cubic spline to interpolate
                                ; the KPP SSTs to the FOAM grid.
                
                                ; Initialize the cubic spline
                                ; interpolation for KPP -> FOAM
                kpp_to_foam_spline = SPL_INIT(ABS(coupled_z),REFORM(coupled_temps(j,k,*)))
                                ; Perform the spline
                coupled_temps_foamgrid(j,k,*) = SPL_INTERP(ABS(coupled_z),REFORM(coupled_temps(j,k,*)),$
                                                               kpp_to_foam_spline,ABS(foam_z))
            ENDIF
        ENDFOR
    ENDFOR

    FOR j=0,foam_nz-1 DO BEGIN
        device,file='/home/ss901165/idl/kpp_forced/'+id+'/diff_'+months(i)+'temp.'+id+'_minus_foam.layer'+$
          strtrim(string(j+1),1)+'.ps',/times,/color,bits_per_pixel=24
        map_set,0,0,limit=[-30,20,30,180],/cylindrical
        contour,coupled_temps_foamgrid(*,*,j)-foam_temps(*,*,j),coupled_longitude,coupled_latitude,c_colors=mycolors,$
          color=1,levels=mylevs,max_val=max_val,min_val=min_val,/overplot,/cell_fill
        map_continents,/hires
        map_grid,/box_axes,/no_grid,/horizon,londel=20,latdel=20,xthick=3,ythick=3,charthick=3,charsize=1.25
        colorbar,ncolors=intervals,divisions=intervals,bottom=bottom,position=[0.02,0.93,0.98,0.98],$
          ticknames=ticknames,thick=3,charthick=3,charsize=1.25,minor=0,title='Diff in ensmean '+months(i)+' T for z='+$
          strmid(strtrim(string(foam_z(j)),1),0,4)+' ('+id+' - FOAM_clim)'
        device,/close
    ENDFOR
        
ENDFOR

STOP

END

