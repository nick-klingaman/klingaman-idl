PRO hadgem3kpp_fcorr_calculate_expostfacto_perfectfluxes_ctl_ukmo_sal60_amip_vn78_smooth_12421,runid,month1,month2

; Input parameter "month" is the three-letter code for the month for
; which to compute the flux correction.

; Calculate the monthly mean flux correction by comparing to UKMO analyses

; All input files must already be monthly means.

noflx_coupled_indir='/home/ss901165/kpp_ocean3/flxcorr_hadgem3_1.0xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421/'+runid
noflx_coupled_infile=noflx_coupled_indir+'/KPPocean_'+month1+'_mmeans.nc'

; FOAM temperatures over the mixed layer, interpolated to N144 resolution
foam_infile='/home/ss901165/datasets/METO_OCEAN_ANALYSIS/n96/meto_ocean_analysis.'+STRMID(month1,0,3)+'15-'+$
            STRMID(month2,0,3)+'15_mmean_clim.1980-2009.pot_temp.amip_type.n96.nc'

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
coupled_depth=coupled_depth[where(coupled_depth gt -205)]

; Restrict global latitude and longitude to the coupling region
coupled_nlon=N_ELEMENTS(coupled_longitude)
coupled_nlat=N_ELEMENTS(coupled_latitude)
coupled_nz=N_ELEMENTS(coupled_depth)
DEFINE_BOUNDARIES,[coupled_latitude(0),coupled_longitude(0),coupled_latitude(coupled_nlat-1),$
                   coupled_longitude(coupled_nlon-1)],global_latitude,global_longitude,coupled_box_tx,/LIMIT
foam_bottom=NEAREST(global_depth,coupled_depth(coupled_nz-1))
foam_depth=global_depth(0:foam_bottom)
foam_depth(foam_bottom)=-200
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
expcorr_kppgrid=fltarr(coupled_nlon,coupled_nlat,coupled_nz)
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

noflx_temps_mmean_foamgrid=fltarr(coupled_nlon,coupled_nlat,foam_nz)
fcorr_foamgrid=fltarr(coupled_nlon,coupled_nlat,foam_nz)
; Compute the flux correction one gridpoint at a time
FOR i=0,coupled_nlon-1 DO BEGIN
    FOR j=0,coupled_nlat-1 DO BEGIN
                                ; Only compute the flux correction if this is an ocean point
        IF landfrac(i,j) lt 1 THEN BEGIN
                                ; Use a cubic spline to interpolate
                                ; the KPP SSTs to the FOAM grid.
                                
                                ; Initialize the cubic spline
                                ; interpolation for KPP -> FOAM
            ;kpp_to_foam_spline = SPL_INIT(ABS(coupled_depth),REFORM(noflx_temps_mmean(i,j,*)))
                                ; Perform the spline
            ;noflx_temps_mmean_foamgrid = SPL_INTERP(ABS(coupled_depth),REFORM(noflx_temps_mmean(i,j,*)),kpp_to_foam_spline,ABS(foam_depth))
           noflx_temps_mmean_foamgrid(i,j,*)=SPLINE(ABS(coupled_depth),REFORM(noflx_temps_mmean(i,j,*)),ABS(foam_depth),10)
           noflx_cp_mmean_foamgrid=SPLINE(ABS(coupled_depth),REFORM(noflx_cp_mmean(i,j,*)),ABS(foam_depth),10)
           noflx_rho_mmean_foamgrid=SPLINE(ABS(coupled_depth),REFORM(noflx_rho_mmean(i,j,*)),ABS(foam_depth),10)
           
;            kpp_to_foam_spline = SPL_INIT(ABS(coupled_depth),REFORM(noflx_cp_mmean(i,j,*)))
;            noflx_cp_mmean_foamgrid = SPL_INTERP(ABS(coupled_depth),REFORM(noflx_cp_mmean(i,j,*)),kpp_to_foam_spline,ABS(foam_depth))

;            kpp_to_foam_spline = SPL_INIT(ABS(coupled_depth),REFORM(noflx_rho_mmean(i,j,*)))
;            noflx_rho_mmean_foamgrid = SPL_INTERP(ABS(coupled_depth),REFORM(noflx_rho_mmean(i,j,*)),kpp_to_foam_spline,ABS(foam_depth))
            
                                ; Calculate the flux correction in
                                ; W m^(-3) at every FOAM grid point.
            FOR k=0,foam_nz-1 DO BEGIN
                IF (foam_depth(k) ge ocndepth(i,j)) THEN BEGIN
                    temp_diff=foam_temps_mmean(i,j,k)-noflx_temps_mmean_foamgrid(i,j,k)
                    fcorr_foamgrid(i,j,k)=noflx_rho_mmean_foamgrid(k)*noflx_cp_mmean_foamgrid(k)*$
                      temp_diff/(86400.*15.)
                 ENDIF ELSE $
                  fcorr_foamgrid(k) = 0.
            ENDFOR
                                ; Initialize the cubic spline
                                ; interpolation for FOAM -> KPP
;            foam_to_kpp_spline = SPL_INIT(ABS(foam_depth),fcorr_foamgrid)
;            fcorr_kppgrid(i,j,*) = SPL_INTERP(ABS(foam_depth),fcorr_foamgrid,foam_to_kpp_spline,ABS(coupled_depth))
            test=SPLINE(ABS(foam_depth),REFORM(fcorr_foamgrid(i,j,*)),ABS(coupled_depth),10)
            fcorr_kppgrid(i,j,*)=test
            expcorr_kppgrid(i,j,*)=fcorr_kppgrid(i,j,*)*(86400.*15.)/(noflx_cp_mmean(i,j,*)*noflx_rho_mmean(i,j,*))
            ;IF i eq 63 and j eq 35 THEN STOP
         ENDIF ELSE BEGIN
            fcorr_kppgrid(i,j,*)=2e20
            foam_temps_mmean(i,j,*)=2e20
            noflx_temps_mmean_foamgrid(i,j,*)=2e20            
         ENDELSE
    ENDFOR        
ENDFOR

FOR i=0,coupled_nlon-1 DO BEGIN
    FOR j=0,coupled_nlat-1 DO BEGIN
        FOR k=0,coupled_nz-1 DO BEGIN
            IF (ABS(fcorr_kppgrid(i,j,k)) gt 40 and landfrac(i,j) lt 1) THEN BEGIN
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
                ENDIF ELSE BEGIN
                   neighbors=fcorr_kppgrid(i-1:i+1,j-1:j+1,*)
                   FOR m=0,coupled_nz-1 DO BEGIN
                      temp=REFORM(neighbors(*,*,m))
                      temp2=REFORM(landfrac(i-1:i+1,j-1:j+1,*))
                      IF TOTAL(where(temp2 eq 1)) ge 0 THEN $
                         temp[where(temp2 eq 1)]=!Values.F_NaN
                      fcorr_kppgrid(i,j,m)=TOTAL(temp,/NAN)/FLOAT(N_ELEMENTS(where(FINITE(temp) eq 1)))
                   ENDFOR                   
                ENDELSE
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
id = NCDF_CREATE('/home/ss901165/datasets/HADGEM3-KPP_ANCIL/heat_corrections/'+runid+'_flxcorr_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421.'+month1+'_mmean.n96.nc',/CLOBBER)
lon_dimid = NCDF_DIMDEF(id,'longitude',coupled_nlon)
lon_varid = NCDF_VARDEF(id,'longitude',lon_dimid)
lat_dimid = NCDF_DIMDEF(id,'latitude',coupled_nlat)
lat_varid = NCDF_VARDEF(id,'latitude',lat_dimid)
z_dimid = NCDF_DIMDEF(id,'z',coupled_nz)
foamz_dimid=NCDF_DIMDEF(id,'analysis_z',foam_nz)
z_varid = NCDF_VARDEF(id,'z',z_dimid)
foamz_varid=NCDF_VARDEF(id,'analysis_z',foamz_dimid)
t_dimid = NCDF_DIMDEF(id,'t',/UNLIMITED)
t_varid = NCDF_VARDEF(id,'t',t_dimid)
flx_varid = NCDF_VARDEF(id,'fcorr',[lon_dimid,lat_dimid,z_dimid,t_dimid])
foamt_varid=NCDF_VARDEF(id,'analysisT_analysisZ',[lon_dimid,lat_dimid,foamz_dimid,t_dimid])
kppfoam_varid=NCDF_VARDEF(id,'kppT_analysisZ',[lon_dimid,lat_dimid,foamz_dimid,t_dimid])
difft_varid=NCDF_VARDEF(id,'diffT_analysisZ',[lon_dimid,lat_dimid,foamz_dimid,t_dimid])
expcorr_varid=NCDF_VARDEF(id,'expcorr',[lon_dimid,lat_dimid,z_dimid,t_dimid])
fcorrfoam_varid=NCDF_VARDEF(id,'fcorr_analysisZ',[lon_dimid,lat_dimid,foamz_dimid,t_dimid])

NCDF_ATTPUT,id,flx_varid,'missing_value',2e20
NCDF_ATTPUT,id,foamt_varid,'missing_value',2e20
NCDF_ATTPUT,id,kppfoam_varid,'missing_value',2e20
NCDF_ATTPUT,id,difft_varid,'missing_value',2e20

NCDF_CONTROL,id,/ENDEF

NCDF_VARPUT,id,lon_varid,coupled_longitude
NCDF_VARPUT,id,lat_varid,coupled_latitude
NCDF_VARPUT,id,z_varid,[coupled_depth]
NCDF_VARPUT,id,z_varid,coupled_depth
NCDF_VARPUT,id,foamz_varid,foam_depth
NCDF_VARPUT,id,t_varid,15
NCDF_VARPUT,id,flx_varid,fcorr_kppgrid
NCDF_VARPUT,id,foamt_varid,foam_temps_mmean
NCDF_VARPUT,id,kppfoam_varid,noflx_temps_mmean_foamgrid
NCDF_VARPUT,id,difft_varid,noflx_temps_mmean_foamgrid-foam_temps_mmean
NCDF_VARPUT,id,fcorrfoam_varid,fcorr_foamgrid
NCDF_VARPUT,id,expcorr_varid,expcorr_kppgrid

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

; Plot SSTs for KPP, climatology and difference
mylevs_raw=['18','19','20','21','22','23','24','25','26','27','28','29','30','31']
mylevs_diff=['-1.7','-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5','1.7']
mylevs_fcorr=['-225','-195','-165','-135','-105','-75','-45','-15','15','45','75','105','135','165','195','225']

psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_calculate_expostfacto_perfectfluxes_entrain_ukmo_sal60_amip_vn78_smooth_12421.'+$
       month1+'_mmean_sst_clim.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=10000,SPACE3=500
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1
LEVS,MANUAL=mylevs_raw
MAP,LATMIN=MIN(coupled_latitude),LATMAX=MAX(coupled_latitude),LONMIN=MIN(coupled_longitude),LONMAX=MAX(coupled_longitude)
CON,X=coupled_longitude,Y=coupled_latitude,FIELD=REFORM(foam_temps_mmean(*,*,0)),/NOLINES,/BLOCK,$
    TITLE='15 '+month1+'--15 '+month2+' clim SSTs from UKMO AMIP2 climatology'
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_calculate_expostfacto_perfectfluxes_entrain_ukmo_sal60_amip_vn78_smooth_12421.'+$
       month1+'_mmean_sst_kpp_'+runid+'.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=10000,SPACE3=500
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1
LEVS,MANUAL=mylevs_raw
MAP,LATMIN=MIN(coupled_latitude),LATMAX=MAX(coupled_latitude),LONMIN=MIN(coupled_longitude),LONMAX=MAX(coupled_longitude)
CON,X=coupled_longitude,Y=coupled_latitude,FIELD=REFORM(noflx_temps_mmean_foamgrid(*,*,0)),/NOLINES,/BLOCK,$
    TITLE='15 '+month1+'--15 '+month2+' SSTs from KPP using fluxes from '+runid
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_calculate_expostfacto_perfectfluxes_entrain_ukmo_sal60_amip_vn78_smooth_12421.'+$
       month1+'_mmean_sst.kpp_'+runid+'-minus-clim.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=10000,SPACE3=500
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1
LEVS,MANUAL=mylevs_diff
MAP,LATMIN=MIN(coupled_latitude),LATMAX=MAX(coupled_latitude),LONMIN=MIN(coupled_longitude),LONMAX=MAX(coupled_longitude)
CON,X=coupled_longitude,Y=coupled_latitude,FIELD=REFORM(noflx_temps_mmean_foamgrid(*,*,0))-REFORM(foam_temps_mmean(*,*,0)),/NOLINES,/BLOCK,$
    TITLE='Diff in 15 '+month1+'--15 '+month2+' SSTs for KPP (fluxes from '+runid+') minus UKMO AMIP2 climatology'
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_calculate_expostfacto_perfectfluxes_entrain_ukmo_sal60_amip_vn78_smooth_12421.'+$
       month1+'_mmean_fcorr_mixedlayer.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,YSIZE=10000,SPACE3=500
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_fcorr)+1
LEVS,MANUAL=mylevs_fcorr
MAP,LATMIN=MIN(coupled_latitude),LATMAX=MAX(coupled_latitude),LONMIN=MIN(coupled_longitude),LONMAX=MAX(coupled_longitude)
CON,X=coupled_longitude,Y=coupled_latitude,FIELD=fcorr_m2_ml,/NOLINES,/BLOCK,$
    TITLE='Vertically integrated heat flux applied to mixed layer for 15 '+month1+'--15 '+month2+' for KPP (fluxes from '+runid+')'
PSCLOSE,/NOVIEW

;STOP

END

