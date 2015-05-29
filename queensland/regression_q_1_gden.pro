PRO regression_q_1_gden

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;500 hPa specific humidity
;Longitude name: "longitude"
;Latitude name: "latitude_1"
;Variable name: "q_1"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; year by year tropical cyclone stats from HiGEM
input_file1='/home/ss901165/higem_qccce/es_control_eafeb/tropical_cyclones/stat_trs_scl.oct-may_smeans.i0-w8.nc'
;fields related to TC genesis (SON)
input_file2='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.sep-nov_smeans.i0-w8.q500.pac_domain.nc'
; /home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.dec-feb_smeans.i0-w8.q500.pac_domain.nc (DJF)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; regional boundaries [south,west,north,east]
box=[-20,150,-10,170]
box_q_1=[-45,20,0,250]

longitude=OPEN_AND_EXTRACT(input_file1,'long')
latitude=OPEN_AND_EXTRACT(input_file1,'lat')
year=OPEN_AND_EXTRACT(input_file1,'year')
DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)

longitude_q_1=OPEN_AND_EXTRACT(input_file2,'longitude')
latitude_q_1=OPEN_AND_EXTRACT(input_file2,'latitude_1')
DEFINE_BOUNDARIES,box_q_1,latitude_q_1,longitude_q_1,box_q_1_tx,/LIMIT
n_lon_q_1=N_ELEMENTS(longitude_q_1)
n_lat_q_1=N_ELEMENTS(latitude_q_1)

;n_years=N_ELEMENTS(years)
n_years=148

track_density=OPEN_AND_EXTRACT(input_file1,'gden',$
                               offset=[box_tx(1),box_tx(0),0],$
                               count=[n_lon,n_lat,n_years])
track_density=track_density*8


q_1=OPEN_AND_EXTRACT(input_file2,'q_1',$
                               offset=[box_q_1_tx(1),box_q_1_tx(0),0],$
                               count=[n_lon_q_1,n_lat_q_1,n_years])*10000.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

track_density_aavg=fltarr(n_years)
FOR i=0,n_years-1 DO $
   track_density_aavg(i)=MEAN(track_density(*,*,i))

regression_coefficients=fltarr(n_lon_q_1,n_lat_q_1)
correlation_coefficients=fltarr(n_lon_q_1,n_lat_q_1)
FOR i=0,n_lon_q_1-1 DO BEGIN
   FOR j=0,n_lat_q_1-1 DO BEGIN
      regression_coefficients(i,j)=REGRESS(REFORM(track_density_aavg(*)),REFORM(q_1(i,j,*)),CORRELATION=temp)
      correlation_coefficients(i,j)=temp(0)
   ENDFOR
ENDFOR 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


contour_levels=[-1.0,-0.9,-0.8,-0.7,-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]
;psfile='/home/dt026033/idl/regressions/regression_q_1.ps'
psfile='/home/ss901165/idl/queensland/regression_q_1.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1500,XOFFSET=1000,YOFFSET=1500,$
       TFONT=2,TCHARSIZE=100,CB_WIDTH=110,SPACE3=500,YSIZE=10000
CS,SCALE=21,NCOLS=N_ELEMENTS(contour_levels)+1,WHITE=[12,13]
LEVS,MANUAL=contour_levels,NDEC=1
MAP,LATMIN=box_q_1(0),LATMAX=box_q_1(2),LONMIN=box_q_1(1),LONMAX=box_q_1(3)
CON,X=longitude_q_1,Y=latitude_q_1,FIELD=regression_coefficients,/NOLINELABELS,/NOLINES,$
    TITLE='Regression Coefficient; 500hPa specific humidity & Genesis Density',NEGATIVE_STYLE=1, ZERO_THICK=150
;PFILL,FIELD=regression_coefficients,X=longitude_q_1,Y=latitude_q_1,MIN=0.195,MAX=10
;PFILL,FIELD=regression_coefficients,X=longitude_q_1,Y=latitude_q_1,MIN=-10,MAX=-0.195

PSCLOSE
STOP
END
