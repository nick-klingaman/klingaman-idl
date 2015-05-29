PRO normalised_regress_temp_DJFa

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Sea-surface temperature
;Longitude name: "longitude"
;Latitude name: "latitude"
;Variable name: "temp"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; year by year tropical cyclone stats from HiGEM
input_file1='/home/ss901165/higem_qccce/es_control_eafeb/tropical_cyclones/stat_trs_scl.oct-may_smeans.i0-w8.nc'
;fields related to TC genesis
;input_file2='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.dec-feb_smeans.i0-w8.surf_temp.global_domain.nc' ;(SON)
input_file2='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.sep-nov_smeans.i0-w8.surf_temp.global_domain.nc';(DJF)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; regional boundaries [south,w.ruest,north,east]
;box A
box=[-25,30,-5,130]
;box B
;box=[-20,110,0,160]
;box C
;box=[-15,140,-5,240]
;box D
;box=[-25,140,-15,240]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;regional boundaries
box_sst=[-45,20,20,250]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

longitude=OPEN_AND_EXTRACT(input_file1,'long')
latitude=OPEN_AND_EXTRACT(input_file1,'lat')
year=OPEN_AND_EXTRACT(input_file1,'year')
DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)

longitude_sst=OPEN_AND_EXTRACT(input_file2,'longitude')
latitude_sst=OPEN_AND_EXTRACT(input_file2,'latitude')
DEFINE_BOUNDARIES,box_sst,latitude_sst,longitude_sst,box_sst_tx,/LIMIT
n_lon_sst=N_ELEMENTS(longitude_sst)
n_lat_sst=N_ELEMENTS(latitude_sst)

; Read in land/sea mask
mask_file='/home/ss901165/um_output/mask_n144_higam.nc'
longitude_mask=OPEN_AND_EXTRACT(mask_file,'longitude')
latitude_mask=OPEN_AND_EXTRACT(mask_file,'latitude')
DEFINE_BOUNDARIES,box_sst,latitude_mask,longitude_mask,box_mask_tx,/LIMIT
n_lon_mask=N_ELEMENTS(longitude_mask)
n_lat_mask=N_ELEMENTS(latitude_mask)
mask=REFORM(OPEN_AND_EXTRACT(mask_file,'lsm',$
                      offset=[box_mask_tx(1),box_mask_tx(0),0,0],$
                      count=[n_lon_mask,n_lat_mask,1,1]))
; End of reading in land/sea mask


n_years=148
track_density=OPEN_AND_EXTRACT(input_file1,'gden',$
                               offset=[box_tx(1),box_tx(0),0],$
                               count=[n_lon,n_lat,n_years])
track_density=track_density*8
sst=OPEN_AND_EXTRACT(input_file2,'temp',$
                               offset=[box_sst_tx(1),box_sst_tx(0),0],$
                               count=[n_lon_sst,n_lat_sst,n_years])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
track_density_aavg=fltarr(n_years)
FOR i=0,n_years-1 DO $
   track_density_aavg(i)=MEAN(track_density(*,*,i))

stddev_gden=STDDEV(track_density_aavg)
print,''
print, 'standard deviation of box averaged genesis density'
print,stddev_gden
print,''

regression_coefficients=fltarr(n_lon_sst,n_lat_sst)
correlation_coefficients=fltarr(n_lon_sst,n_lat_sst)
FOR i=0,n_lon_sst-1 DO BEGIN
   FOR j=0,n_lat_sst-1 DO BEGIN
      regression_coefficients(i,j)=REGRESS(REFORM(track_density_aavg(*)),REFORM(sst(i,j,*)),CORRELATION=temp)
      correlation_coefficients(i,j)=temp(0)
   ENDFOR
ENDFOR 

normalised_regressed_values=fltarr(n_lon_sst  ,n_lat_sst  )
FOR i=0,n_lon_sst  -1 DO BEGIN
   FOR j=0,n_lat_sst  -1 DO BEGIN
      normalised_regressed_values(i,j)=(correlation_coefficients(i,j,*))*stddev_gden
   ENDFOR
ENDFOR

; Apply land/sea mask - set all land points to missing values
normalised_regressed_values[where(mask eq 1)] = !Values.F_NaN
correlation_coefficients[where(mask eq 1)] = !Values.F_NaN
; End of applying land/sea mask

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;contour_levels=[-0.10,-0.09,-0.08,-0.07,-0.06,-0.05,-0.04,-0.03,-0.02,-0.01,0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.10]
contour_levels=[-0.20,-0.18,-0.16,-0.14,-0.12,-0.10,-0.08,-0.06,-0.04,-0.02,0,0.02,0.04,0.06,0.08,0.10,0.12,0.14,0.16,0.18,0.20]
;contour_levels=[-0.50,-0.45,-0.40,-0.35,-0.30,-0.25,-0.20,-0.15,-0.10,-0.05,0,0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50]
;contour_levels=[-1.0,-0.9,-0.8,-0.7,-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]

psfile='/home/dt026033/idl/regressions/Normalised/normalised_regress_temp_DJFa.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1500,XOFFSET=1000,YOFFSET=1500,$
       TFONT=2,TCHARSIZE=100,CB_WIDTH=110,SPACE3=500,YSIZE=10000
CS,SCALE=21,NCOLS=N_ELEMENTS(contour_levels)+1,WHITE=[12,13]
LEVS,MANUAL=contour_levels,NDEC=2
MAP,LATMIN=box_sst(0),LATMAX=box_sst(2),LONMIN=box_sst(1),LONMAX=box_sst(3)
CON,X=longitude_sst,Y=latitude_sst,FIELD=normalised_regressed_values,/NOLINELABELS,/NOLINES,$
    TITLE='Regressed Values; DJF surface temperature [Box A]',NEGATIVE_STYLE=1, ZERO_THICK=150,CB_TITLE='degrees per (storms year!U-1!N (10!U6!N km)!U-1!N)'

PFILL,FIELD=correlation_coefficients,X=longitude_sst,Y=latitude_sst,MIN=0.195,MAX=200
PFILL,FIELD=correlation_coefficients,X=longitude_sst,Y=latitude_sst,MIN=-200,MAX=-0.195

GPLOT,X=[box(3),box(3)],Y=[box(0),box(2)],/LINEFILL
GPLOT,X=[box(1),box(1)],Y=[box(0),box(2)],/LINEFILL
GPLOT,X=[box(1),box(3)],Y=[box(0),box(0)],/LINEFILL
GPLOT,X=[box(1),box(3)],Y=[box(2),box(2)],/LINEFILL

PSCLOSE
STOP
END
