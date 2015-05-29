PRO kpp_bias_against_foam_ensmean,ens_id,months

; Examine the differences between the coupled-experiment SSTs and the
; climatological FOAM SSTs.
;
; Examine the differences between variables in the coupled experiment
; and those in the corresponding atmosphere-only experiment.

; How many months of data do we have?
nmonths=N_ELEMENTS(months)

; Loop over months
FOR i=0,nmonths-1 DO BEGIN

                                ; Location of the SST files
    coupled_sst_infile = '/home/ss901165/kpp_ocean/'+ens_id+'/KPPocean.'+months(i)+'mean.ensmean.nc'
    foam_sst_infile = '/home/ss901165/datasets/NCOF_OCEAN/FOAM_'+months(i)+'mean_mixedlayer_temperature.clim.n144.nc'
    
    IF i eq 0 THEN BEGIN
                                ; Get latitude and longitude, restrict
                                ; global grid to regionally coupled grid
        coupled_latitude = OPEN_AND_EXTRACT(coupled_sst_infile,'latitude')
        coupled_longitude = OPEN_AND_EXTRACT(coupled_sst_infile,'longitude')
        global_latitude = OPEN_AND_EXTRACT(foam_sst_infile,'latitude')
        global_longitude = OPEN_AND_EXTRACT(foam_sst_infile,'longitude')
        coupled_nlon = N_ELEMENTS(coupled_longitude)
        coupled_nlat = N_ELEMENTS(coupled_latitude)
        global_nlon = N_ELEMENTS(coupled_longitude)
        global_nlat = N_ELEMENTS(coupled_latitude)
        DEFINE_BOUNDARIES,[MIN([coupled_latitude(0),coupled_latitude(coupled_nlat-1)]),$
                           MIN([coupled_longitude(0),coupled_longitude(coupled_nlon-1)]),$
                           MAX([coupled_latitude(0),coupled_latitude(coupled_nlat-1)]),$
                           MAX([coupled_longitude(0),coupled_longitude(coupled_nlon-1)])],$
          global_latitude,global_longitude,box_tx
        global_coupled_offset_lat = box_tx[0]
        global_coupled_offset_lon = box_tx[1]
    ENDIF
                                ; Get SSTs
    coupled_sst = REFORM(OPEN_AND_EXTRACT(coupled_sst_infile,'T',offset=[0,0,0],$
                                          count=[coupled_nlon,coupled_nlat,1]))
    foam_sst = REFORM(OPEN_AND_EXTRACT(foam_sst_infile,'sea_water_potential_temperature',$
                                       offset=[global_coupled_offset_lon,global_coupled_offset_lat,$
                                               0],$
                                       count=[coupled_nlon,coupled_nlat,1]))-273.15
    
                                ; Get mean SST difference
    sst_diff = coupled_sst - foam_sst    
                                ; Plot difference in SSTs
    set_plot,'ps'
    device,file='/home/ss901165/idl/kpp_forced/'+ens_id+'/diff_'+months(i)+'sst.kpp_'+ens_id+'_minus_foam_clim.ps',/color,/cmyk,bits_per_pixel=24
    max_val = 10
    min_val = -10
    mylevs = [min_val,indgen(23)*0.25-2.75,max_val]
    intervals = N_ELEMENTS(mylevs)-1
    bottom = 3
    mycolors = indgen(intervals)+bottom
    LoadCT,33,ncolors=intervals,bottom=bottom
    white = FSC_COLOR("white",intervals/2+bottom-1)
    white = FSC_COLOR("white",intervals/2+bottom)
    
    !p.position=[0.02,0.02,0.98,0.80]
    
    map_set,0,0,limit=[-30,20,30,180],/cylindrical
    contour,sst_diff,coupled_longitude,coupled_latitude,c_colors=mycolors,color=1,$
      levels=mylevs,max_val=max_val,min_val=min_val,/overplot,/cell_fill
    map_continents,/hires
    map_grid,/box_axes,/no_grid,/horizon,londel=20,latdel=20,xthick=3,ythick=3,charthick=3,charsize=1.25
    
    ticknames=[' ',LEVS2STRING(mylevs(1:N_ELEMENTS(mylevs)-2),2,5),' ']
    colorbar,ncolors=intervals,divisions=intervals,bottom=bottom,position=[0.02,0.93,0.98,0.98],$
      ticknames=ticknames,thick=3,charthick=3,charsize=1.25,minor=0,title='Diff in '+months(i)+' mean SST ('+ens_id+'-FOAM_clim)'
    
    device,/close    
                                ; Plot mean SSTs from the FOAM data
    device,file='/home/ss901165/idl/kpp_forced/'+ens_id+'/foam_clim.'+months(i)+'sst.ps',/color,/cmyk,bits_per_pixel=24
    max_val = 50
    min_val = 0
    mylevs = [min_val,indgen(8)+18,indgen(13)*0.5+25.5,max_val]
    intervals = N_ELEMENTS(mylevs)-1
    bottom = 3
    mycolors = indgen(intervals)+bottom
    LoadCT,33,ncolors=intervals,bottom=bottom
    white = FSC_COLOR("white",bottom)
    
    map_set,0,0,limit=[-30,20,30,180],/cylindrical
    contour,foam_sst,coupled_longitude,coupled_latitude,c_colors=mycolors,color=1,$
      levels=mylevs,max_val=max_val,min_val=min_val,/overplot,/cell_fill
    map_continents,/hires
    map_grid,/box_axes,/no_grid,/horizon,londel=20,latdel=20,xthick=3,ythick=3,charthick=3,charsize=1.25
    
    ticknames=['LAND',LEVS2STRING(mylevs(1:N_ELEMENTS(mylevs)-2),2,4),'33']
    colorbar,ncolors=intervals,divisions=intervals,bottom=bottom,position=[0.02,0.93,0.98,0.98],$
      ticknames=ticknames,thick=3,charthick=3,charsize=1.25,minor=0,title=months(i)+' mean SST (FOAM_clim)'
    
                                ; Plot mean SSTs from the coupled model
    device,file='/home/ss901165/idl/kpp_forced/'+ens_id+'/kpp_'+ens_id+'.'+months(i)+'sst.ps',/color,/cmyk,bits_per_pixel=24
    map_set,0,0,limit=[-30,20,30,180],/cylindrical
    contour,coupled_sst,coupled_longitude,coupled_latitude,c_colors=mycolors,color=1,$
      levels=mylevs,max_val=max_val,min_val=min_val,/overplot,/cell_fill
    map_continents,/hires
    map_grid,/box_axes,/no_grid,/horizon,londel=20,latdel=20,xthick=3,ythick=3,charthick=3,charsize=1.25
    colorbar,ncolors=intervals,divisions=intervals,bottom=bottom,position=[0.02,0.93,0.98,0.98],$
      ticknames=ticknames,thick=3,charthick=3,charsize=1.25,minor=0,title=months(i)+' mean SST ('+ens_id+')'
    device,/close
ENDFOR

STOP

END
