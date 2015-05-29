PRO obs_normalised_regress_shear_DJFa
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Vertical wind shear (defined as the vector difference of the vertical
;wind gradient, i.e., ((U850-U200)^2+(V850-V200)^2)^0.5) 
;from the 20th Century Reanalysis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;TC statistics
;input_file1='/home/ss901165/datasets/IBTRACS/SP_4950_0809/stat_trs_scl.oct-may_smeans.1949-2008.ibtracs_sp.nc'
; ammended to include south indian ocean
input_file1='/home/ss901165/datasets/IBTRACS/SH_4950_0809/stat_trs_scl.oct-may_smeans.1949-2008.ibtracs_sh.nc'

;fields related to TC genesis (SON)(1949-2007, with 1964 and 1965 missing)
;input_file2='/home/ss901165/datasets_mango/20THC_REANALYSIS/vertical_wind_shear/20thc_reanalysis.sep-nov_smeans1949-2007_miss6465.shear_250_850.nc'
input_file2='/home/ss901165/datasets_mango/20THC_REANALYSIS/vertical_wind_shear/20thc_reanalysis.dec-feb_smeans.1949-2007_miss6465.shear_250_850.nc';DJF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; regional boundaries [south,west,north,east]
; box A
box=[-25,30,-5,130]
; box B
;box=[-20,110,0,160]
; box C
;box=[-15,140,-5,240]
;; box D
;box=[-25,140,-15,240]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;regional boundaries
box_shear=[-45,20,20,250]
; was originally [-45,20,0,250] but this gave a strip of white down
; the left hand side

longitude=OPEN_AND_EXTRACT(input_file1,'long')
latitude=OPEN_AND_EXTRACT(input_file1,'lat')
DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)

longitude_shear=OPEN_AND_EXTRACT(input_file2,'longitude')
latitude_shear=OPEN_AND_EXTRACT(input_file2,'latitude')
DEFINE_BOUNDARIES,box_shear,latitude_shear,longitude_shear,box_shear_tx,/LIMIT
n_lon_shear=N_ELEMENTS(longitude_shear)
n_lat_shear=N_ELEMENTS(latitude_shear)
n_years=57

track_density=OPEN_AND_EXTRACT(input_file1,'gden',$
                               offset=[box_tx(1),box_tx(0),0],$
                               count=[n_lon,n_lat,n_years])
track_density=track_density*8
shear=OPEN_AND_EXTRACT(input_file2,'shear',$
                               offset=[box_shear_tx(1),box_shear_tx(0),0],$
                               count=[n_lon_shear,n_lat_shear,n_years])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

track_density_aavg=fltarr(n_years)
FOR i=0,n_years-1 DO $
   track_density_aavg(i)=MEAN(track_density(*,*,i))

stddev_gden=STDDEV(track_density_aavg)
print,''
print, 'standard deviation of box averaged genesis density'
print,stddev_gden
print,''

regression_coefficients=fltarr(n_lon_shear,n_lat_shear)
correlation_coefficients=fltarr(n_lon_shear,n_lat_shear)
FOR i=0,n_lon_shear-1 DO BEGIN
   FOR j=0,n_lat_shear-1 DO BEGIN
      regression_coefficients(i,j)=REGRESS(REFORM(track_density_aavg(*)),REFORM(shear(i,j,*)),CORRELATION=temp)
      correlation_coefficients(i,j)=temp(0)
   ENDFOR
ENDFOR 

normalised_regressed_values=fltarr(n_lon_shear,n_lat_shear)
FOR i=0,n_lon_shear-1 DO BEGIN
   FOR j=0,n_lat_shear-1 DO BEGIN
      normalised_regressed_values(i,j)=(regression_coefficients(i,j))*stddev_gden
   ENDFOR
ENDFOR

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;contour_levels=[-0.10,-0.09,-0.08,-0.07,-0.06,-0.05,-0.04,-0.03,-0.02,-0.01,0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.10]
;contour_levels=[-0.20,-0.18,-0.16,-0.14,-0.12,-0.10,-0.08,-0.06,-0.04,-0.02,0,0.02,0.04,0.06,0.08,0.10,0.12,0.14,0.16,0.18,0.20]
;contour_levels=[-0.50,-0.45,-0.40,-0.35,-0.30,-0.25,-0.20,-0.15,-0.10,-0.05,0,0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50]
;contour_levels=[-1.0,-0.9,-0.8,-0.7,-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]
contour_levels=[-2.0,-1.8,-1.6,-1.4,-1.2,-1.0,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6,1.8,2.0]
psfile='/home/ss901165/idl/obs_normalised_regress_shear_DJFa.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1500,XOFFSET=1000,YOFFSET=1500,$
       TFONT=2,TCHARSIZE=100,CB_WIDTH=110,SPACE3=500,YSIZE=10000
CS,SCALE=21,/REV,NCOLS=N_ELEMENTS(contour_levels)+1,WHITE=[12,13]
LEVS,MANUAL=contour_levels,NDEC=1
MAP,LATMIN=box_shear(0),LATMAX=box_shear(2),LONMIN=box_shear(1),LONMAX=box_shear(3)
CON,X=longitude_shear,Y=latitude_shear,FIELD=normalised_regressed_values,/NOLINELABELS,/NOLINES,$
    TITLE='Regressed Values; DJF vertical wind shear [Box A] Observations',NEGATIVE_STYLE=1, ZERO_THICK=150,CB_TITLE='((U850-200)!E2!N + (V850-V200)!E2!N)!E-2!N per (storms year!U-1!N (10!U6!N km)!U-1!N)'

;critical value for sample of 50 years = 0.273
;critical value for sample of 60 years = 0.25
;using 0.273 for 57 years 'samples'
PFILL,FIELD=correlation_coefficients,X=longitude_shear,Y=latitude_shear,MIN=0.273,MAX=200
PFILL,FIELD=correlation_coefficients,X=longitude_shear,Y=latitude_shear,MIN=-200,MAX=-0.273

GPLOT,X=[box(3),box(3)],Y=[box(0),box(2)],/LINEFILL
GPLOT,X=[box(1),box(1)],Y=[box(0),box(2)],/LINEFILL
GPLOT,X=[box(1),box(3)],Y=[box(0),box(0)],/LINEFILL
GPLOT,X=[box(1),box(3)],Y=[box(2),box(2)],/LINEFILL

PSCLOSE
STOP
END
