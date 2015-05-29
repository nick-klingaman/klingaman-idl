PRO hadgem3kpp_fcorr_calculate_expostfacto_perfectfluxes_entrain_junstart_ukmo_sal60_ind,id,month

; Input parameter "month" is the three-letter code for the month for
; which to compute the flux correction.

; Calculate the monthly mean flux correction by comparing to UKMO analyses

; All input files must already be monthly means.

noflx_coupled_indir='/home/ss901165/kpp_ocean3/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_junstart_ukmo_sal60_ind/'+id
noflx_coupled_infile=noflx_coupled_indir+'/KPPocean_'+month+'_mmeans.nc'

; FOAM temperatures over the mixed layer, interpolated to N144 resolution
foam_infile='/home/ss901165/datasets/METO_OCEAN_ANALYSIS/n96/meto_ocean_analysis.'+STRMID(month,0,3)+'_mmean_clim.1980-2009.pot_temp.top204m.n96.nc'

; N96 *LAND FRACTION FILE* - note that we must use the land fraction
;                            rather than the land/sea mask, because of
;                            the coastal tiling.  Any points with a
;                            land fraction less than one will be considered.
landfrac_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/landfrac_n96_hadgem3-7.3.nc'

; Ocean depth file
ocndepth_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/lsm_ocndepth_for_kpp.n96.coastal_tiling.nearest_neighbor.nc'

; Read in global and regional latitude, longitude, depth
global_latitude=OPEN_AND_EXTRACT(foam_infile,'latitude')
global_longitude=OPEN_AND_EXTRACT(foam_infile,'longitude')
global_depth=OPEN_AND_EXTRACT(foam_infile,'depth')
                                ; Reverse FOAM depths to have the same
                                ; convention as KPP
global_depth = -global_depth
coupled_latitude=OPEN_AND_EXTRACT(noflx_coupled_infile,'latitude')
coupled_longitude=OPEN_AND_EXTRACT(noflx_coupled_infile,'longitude')
coupled_depth=OPEN_AND_EXTRACT(noflx_coupled_infile,'z')
coupled_depth=coupled_depth[where(coupled_depth gt -200)]

; Restrict global latitude and longitude to the coupling region
coupled_nlon=N_ELEMENTS(coupled_longitude)
coupled_nlat=N_ELEMENTS(coupled_latitude)
coupled_nz=N_ELEMENTS(coupled_depth)
DEFINE_BOUNDARIES,[coupled_latitude(0),coupled_longitude(0),coupled_latitude(coupled_nlat-1),$
                   coupled_longitude(coupled_nlon-1)],global_latitude,global_longitude,coupled_box_tx,/LIMIT
foam_bottom=NEAREST(global_depth,coupled_depth(coupled_nz-1)*1.10)
foam_depth=global_depth(0:foam_bottom)
foam_nz=N_ELEMENTS(foam_depth)

; Read in the N96 land/sea mask for the coupling region
landfrac = REFORM(OPEN_AND_EXTRACT(landfrac_infile,'lsm',offset=[coupled_box_tx(1),coupled_box_tx(0),0,0],$
                               count=[coupled_nlon,coupled_nlat,1,1]))
; Read in the ocean depth for the coupling region
ocndepth = REFORM(OPEN_AND_EXTRACT(ocndepth_infile,'max_depth',$
                                   offset=[coupled_box_tx(1),coupled_box_tx(0)],$
                                   count=[coupled_nlon,coupled_nlat]))

e_r = 6.3781E6

fcorr_kppgrid=fltarr(coupled_nlon,coupled_nlat,coupled_nz)
fcorr_foamgrid=fltarr(foam_nz)
fcorr_kppgrid(*,*,*) = 0.
area = fltarr(coupled_nlon,coupled_nlat)
            
foam_temps_mmean = REFORM(OPEN_AND_EXTRACT(foam_infile,'temp',$
                                           offset=[coupled_box_tx(1),coupled_box_tx(0),0],$
                                           count=[coupled_nlon,coupled_nlat,foam_nz]))
noflx_temps_mmean = REFORM(OPEN_AND_EXTRACT(noflx_coupled_infile,'T',offset=[0,0,0],$
                                            count=[coupled_nlon,coupled_nlat,coupled_nz]))
noflx_rho_mmean = REFORM(OPEN_AND_EXTRACT(noflx_coupled_infile,'rho',offset=[0,0,0],$
                                          count=[coupled_nlon,coupled_nlat,coupled_nz]))
noflx_cp_mmean = REFORM(OPEN_AND_EXTRACT(noflx_coupled_infile,'cp',offset=[0,0,0],$
                                         count=[coupled_nlon,coupled_nlat,coupled_nz]))
noflx_hmix_mmean = REFORM(OPEN_AND_EXTRACT(noflx_coupled_infile,'hmix',offset=[0,0],$
                                           count=[coupled_nlon,coupled_nlat]))

; Compute the flux correction one gridpoint at a time
FOR i=0,coupled_nlon-1 DO BEGIN
    FOR j=0,coupled_nlat-1 DO BEGIN
                                ; Only compute the flux correction if this is an ocean point
        IF landfrac(i,j) lt 1 THEN BEGIN
                                ; Use a cubic spline to interpolate
                                ; the KPP SSTs to the FOAM grid.
                                
                                ; Initialize the cubic spline
                                ; interpolation for KPP -> FOAM
            kpp_to_foam_spline = SPL_INIT(ABS(coupled_depth),REFORM(noflx_temps_mmean(i,j,*)))
                                ; Perform the spline
            noflx_temps_mmean_foamgrid = SPL_INTERP(ABS(coupled_depth),REFORM(noflx_temps_mmean(i,j,*)),kpp_to_foam_spline,ABS(foam_depth))

            kpp_to_foam_spline = SPL_INIT(ABS(coupled_depth),REFORM(noflx_cp_mmean(i,j,*)))
            noflx_cp_mmean_foamgrid = SPL_INTERP(ABS(coupled_depth),REFORM(noflx_cp_mmean(i,j,*)),kpp_to_foam_spline,ABS(foam_depth))

            kpp_to_foam_spline = SPL_INIT(ABS(coupled_depth),REFORM(noflx_rho_mmean(i,j,*)))
            noflx_rho_mmean_foamgrid = SPL_INTERP(ABS(coupled_depth),REFORM(noflx_rho_mmean(i,j,*)),kpp_to_foam_spline,ABS(foam_depth))
            
                                ; Calculate the flux correction in
                                ; W m^(-3) at every FOAM grid point.
            FOR k=0,foam_nz-1 DO BEGIN
                IF (foam_depth(k) ge ocndepth(i,j)) THEN BEGIN
                    temp_diff=foam_temps_mmean(i,j,k)-noflx_temps_mmean_foamgrid(k)
                    fcorr_foamgrid(k)=noflx_rho_mmean_foamgrid(k)*noflx_cp_mmean_foamgrid(k)*$
                      temp_diff/(86400.*15.)
                ENDIF ELSE $
                  fcorr_foamgrid(k) = 0.
            ENDFOR
                                ; Initialize the cubic spline
                                ; interpolation for FOAM -> KPP
            foam_to_kpp_spline = SPL_INIT(ABS(foam_depth),fcorr_foamgrid)
            fcorr_kppgrid(i,j,*) = SPL_INTERP(ABS(foam_depth),fcorr_foamgrid,foam_to_kpp_spline,ABS(coupled_depth))
        ENDIF
    ENDFOR        
ENDFOR

FOR i=0,coupled_nlon-1 DO BEGIN
    FOR j=0,coupled_nlat-1 DO BEGIN
        FOR k=0,coupled_nz-1 DO BEGIN
            IF (ABS(fcorr_kppgrid(i,j,k)) gt 40) THEN BEGIN
                print,i,j,k
                IF (i eq 0 and j ne 0) THEN BEGIN
                    fcorr_kppgrid(i,j,*) = (fcorr_kppgrid(i+1,j,*)+fcorr_kppgrid(i+1,j+1,*)+fcorr_kppgrid(i+1,j-1,*)+$
                                            fcorr_kppgrid(i,j+1,*)+fcorr_kppgrid(i,j-1,*))/5.
                ENDIF ELSE IF (i eq 0 and j eq 0) THEN BEGIN
                    fcorr_kppgrid(i,j,*) = (fcorr_kppgrid(i+1,j,*)+fcorr_kppgrid(i+1,j+1,*)+fcorr_kppgrid(i,j+1,*))/3.
                ENDIF ELSE IF (j eq 0 and i ne 0) THEN BEGIN
                    fcorr_kppgrid(i,j,*) = (fcorr_kppgrid(i+1,j,*)+fcorr_kppgrid(i+1,j+1,*)+fcorr_kppgrid(i-1,j,*)+$
                                            fcorr_kppgrid(i-1,j+1,*)+fcorr_kppgrid(i,j+1,*))/5.
                ENDIF ELSE IF (j eq coupled_nlat-1 and i eq 0) THEN BEGIN
                    fcorr_kppgrid(i,j,*) = (fcorr_kppgrid(i+1,j,*)+fcorr_kppgrid(i+1,j-1,*)+fcorr_kppgrid(i,j-1,*))/3.
                ENDIF ELSE IF (j eq coupled_nlat-1 and i ne 0) THEN BEGIN
                    fcorr_kppgrid(i,j,*) = (fcorr_kppgrid(i+1,j,*)+fcorr_kppgrid(i+1,j-1,*)+fcorr_kppgrid(i-1,j,*)+$
                                            fcorr_kppgrid(i-1,j-1,*)+fcorr_kppgrid(i,j-1,*))/5.
                ENDIF ELSE $
                  fcorr_kppgrid(i,j,*) = (fcorr_kppgrid(i+1,j,*)+fcorr_kppgrid(i+1,j+1,*)+fcorr_kppgrid(i+1,j-1,*)+$
                                          fcorr_kppgrid(i-1,j,*)+fcorr_kppgrid(i-1,j+1,*)+fcorr_kppgrid(i-1,j-1,*)+$
                                          fcorr_kppgrid(i,j+1,*)+fcorr_kppgrid(i,j-1,*))/8.
            ENDIF
        ENDFOR
    ENDFOR
ENDFOR
            

fcorr_kpp200 = fltarr(coupled_nlon,coupled_nlat)
fcorr_kpp200(*,*) = 0.
fcorr_kppgrid_plus200 = fltarr(coupled_nlon,coupled_nlat,coupled_nz+1)
fcorr_kppgrid_plus200(*,*,0:coupled_nz-1) = fcorr_kppgrid
fcorr_kppgrid_plus200(*,*,coupled_nz) = fcorr_kpp200
; Output flux correction as a value of W m-3 to a NetCDF file to be
; read in by KPP.
id = NCDF_CREATE('/home/ss901165/datasets/HADGEM3-KPP_ANCIL/heat_corrections/hadgem3_flxcorr_1mtop_3hr_junstart_ukmo_sal60_'+id+'.'+month+'_mmean.n96.nc',/CLOBBER)
lon_dimid = NCDF_DIMDEF(id,'longitude',coupled_nlon)
lon_varid = NCDF_VARDEF(id,'longitude',lon_dimid)
lat_dimid = NCDF_DIMDEF(id,'latitude',coupled_nlat)
lat_varid = NCDF_VARDEF(id,'latitude',lat_dimid)
z_dimid = NCDF_DIMDEF(id,'z',coupled_nz+1)
z_varid = NCDF_VARDEF(id,'z',z_dimid)
t_dimid = NCDF_DIMDEF(id,'t',/UNLIMITED)
t_varid = NCDF_VARDEF(id,'t',t_dimid)
flx_varid = NCDF_VARDEF(id,'fcorr',[lon_dimid,lat_dimid,z_dimid,t_dimid])

NCDF_CONTROL,id,/ENDEF

NCDF_VARPUT,id,lon_varid,coupled_longitude
NCDF_VARPUT,id,lat_varid,coupled_latitude
NCDF_VARPUT,id,z_varid,[coupled_depth,-200]
NCDF_VARPUT,id,t_varid,15
NCDF_VARPUT,id,flx_varid,fcorr_kppgrid_plus200

NCDF_CLOSE,id

; Calculate total flux correction over the depth of the mixed layer at
; each gridpoint.
layer_thickness=fltarr(coupled_nz)
layer_thickness(0)=ABS(coupled_depth(0))
layer_thickness(1:coupled_nz-1)=ABS(coupled_depth(1:coupled_nz-1)-coupled_depth(0:coupled_nz-2))

fcorr_m2 = fltarr(coupled_nlon,coupled_nlat,coupled_nz)
fcorr_m2_ml = fltarr(coupled_nlon,coupled_nlat)
FOR i=0,coupled_nlon-1 DO BEGIN
    FOR j=0,coupled_nlat-1 DO BEGIN
        fcorr_m2(i,j,*) = fcorr_kppgrid(i,j,*)*layer_thickness
        mixed_layer_bottom = NEAREST(ABS(coupled_depth),ABS(noflx_hmix_mmean(i,j)))
        fcorr_m2_ml(i,j) = TOTAL(fcorr_m2(i,j,0:mixed_layer_bottom))
    ENDFOR
ENDFOR

; Plot total flux correction over the depth of the mixed layer
; set_plot,'ps'
; min_val_fcorr = -600
; max_val_fcorr = 600
; mylevs_fcorr = [min_val_fcorr,-150,-100,-75,-50,-40,-30,-20,-10,-5,-2,0,$
;                 2,5,10,20,30,40,50,75,100,150,max_val_fcorr]
; intervals_fcorr = N_ELEMENTS(mylevs_fcorr)-1
; bottom = 3
; LoadCT,33,ncolors=intervals_fcorr,bottom=bottom
; mycolors_fcorr = indgen(intervals_fcorr)+bottom
; ticknames_fcorr = [' ',LEVS2STRING(mylevs_fcorr(1:intervals_fcorr-1),2,5),' ']
; !p.position=[0.05,0.05,0.95,0.80]
; white = FSC_COLOR("white",intervals_fcorr/2+bottom)
; white = FSC_COLOR("white",intervals_fcorr/2+bottom-1)

; device,file='/home/ss901165/idl/hadkpp_runs/flux_corrections/hadkpp_fcorr_calculate_expostfacto_'+$
;   month+'_hmix.ps',color=1,bits_per_pixel=24,/cmyk,/times
; map_set,0,0,limit=[coupled_latitude(0),coupled_longitude(0),coupled_latitude(coupled_nlat-1),$
;                    coupled_longitude(coupled_nlon-1)],/cylindrical
; contour,fcorr_m2_ml,coupled_longitude,coupled_latitude,levels=mylevs_fcorr,min_val=min_val_fcorr,$
;   max_val=max_val_fcorr,c_colors=mycolors_fcorr,/overplot,/cell_fill,color=1
; map_continents,/hires
; map_grid,/no_grid,/box_axes,/horizon

; colorbar,ncolors=intervals_fcorr,divisions=intervals_fcorr,position=[0.05,0.90,0.95,0.95],$
;   bottom=3,title='Mean flxcorr over hmix for '+month+$
;   ' calculated ex post facto (HadKPP_'+noflx_coupled_id+')',ticknames=ticknames_fcorr
; device,/close

; ; Plot the flux correction in W/m-2 at each depth

; FOR i=0,coupled_nz-1 DO BEGIN
;     min_val_fcorr = -8.*layer_thickness(i)
;     max_val_fcorr = 8.*layer_thickness(i)
;     mylevs_fcorr = [min_val_fcorr,layer_thickness(i)*(indgen(27)*0.5-6.5),max_val_fcorr]
;     intervals_fcorr = N_ELEMENTS(mylevs_fcorr)-1
;     bottom=3
;     LoadCT,33,ncolors=intervals_fcorr,bottom=bottom
;     mycolors_fcorr = indgen(intervals_fcorr)+bottom
;     ticknames_fcorr = [' ',LEVS2STRING(mylevs_fcorr(1:intervals_fcorr-1),2,5),' ']
;     white = FSC_COLOR("white",intervals_fcorr/2+bottom)
;     white = FSC_COLOR("white",intervals_fcorr/2+bottom-1)

;     IF max_val_fcorr lt MAX(fcorr_m2(*,*,i)) THEN $
;       print, 'Out of plotting range at i=',i

;     device,file='/home/ss901165/idl/hadkpp_runs/flux_corrections/hadkpp_fcorr_calculate_expostfacto_'+$
;       month+'_layer'+strtrim(string(i+1),1)+'.ps',color=1,bits_per_pixel=24,/cmyk,/times
;     map_set,0,0,limit=[coupled_latitude(0),coupled_longitude(0),coupled_latitude(coupled_nlat-1),$
;                        coupled_longitude(coupled_nlon-1)],/cylindrical
;     contour,fcorr_m2(*,*,i),coupled_longitude,coupled_latitude,levels=mylevs_fcorr,min_val=min_val_fcorr,$
;       max_val=max_val_fcorr,c_colors=mycolors_fcorr,/overplot,/cell_fill,color=1,$
;       xthick=3,ythick=3,thick=3,charthick=3
;     map_continents,/hires
;     map_grid,/no_grid,/box_axes,/horizon,thick=3,xthick=3,ythick=3,charthick=3
    
;     colorbar,ncolor=intervals_fcorr,divisions=intervals_fcorr,position=[0.05,0.90,0.95,0.95],$
;       bottom=3,title='Mean flxcorr at z='+strmid(strtrim(string(coupled_depth(i)),1),0,6)+' for '+month+$
;       ' calc ex post facto (HadKPP_noflx_ensmean)',ticknames=ticknames_fcorr,thick=3,charthick=3
;     device,/close
; ENDFOR

STOP

END

