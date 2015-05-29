PRO obs_regress_temp_gden_SONa

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Sea-surface temperature
;Longitude name: "longitude"
;Latitude name: "latitude"
;Variable name: "temp"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;TC statistics
input_file1='/home/ss901165/datasets/IBTRACS/SP_4950_0809/stat_trs_scl.oct-may_smeans.1949-2008.ibtracs_sp.nc'
;57 years, covering the 1949-1950 through 2008-2009 seasons
;Tracking is performed October-May, as for HiGEM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Sea-surface temperatures from HadISST
;September-November seasonal means (1949-2007, with 1964 and 1965 missing):
input_file2='/home/ss901165/datasets/HADISST/HadISST.sep-nov_smeans.1949-2007_miss6465.sst.nc'

;December-February seasonal means (1949-1950--2007-2008, with 1964-1965
;and 1965-1966 missing):
;input_file2='/home/ss901165/datasets/HADISST/HadISST.dec-feb_smeans.1949-2007_miss6465.sst.nc'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;box A [south,west,north,east]
;box=[-25,30,-5,130]
;box B
;box=[-20,110,0,160]
;box C
box=[-15,140,-5,240]
;box D
;box=[-25,140,-15,240]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;box map
box_sst=[-45,20,20,-120]
; was originally [-45,20,0,250] but this gave a strip of white down
; the left hand side
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

longitude=OPEN_AND_EXTRACT(input_file1,'long')
latitude=OPEN_AND_EXTRACT(input_file1,'lat')
;year=OPEN_AND_EXTRACT(input_file1,'year')
DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)

longitude_sst=OPEN_AND_EXTRACT(input_file2,'lon')
latitude_sst=OPEN_AND_EXTRACT(input_file2,'lat')
DEFINE_BOUNDARIES,box_sst,latitude_sst,longitude_sst,box_sst_tx,/LIMIT
longitude_sst[where(longitude_sst lt 0)]=longitude_sst[where(longitude_sst lt 0)]+360
n_lon_sst=N_ELEMENTS(longitude_sst)
n_lat_sst=N_ELEMENTS(latitude_sst)

n_years=57

track_density=OPEN_AND_EXTRACT(input_file1,'gden',$
                               offset=[box_tx(1),box_tx(0),0],$
                               count=[n_lon,n_lat,n_years])
track_density=track_density*8
sst=OPEN_AND_EXTRACT(input_file2,'sst',$
                               offset=[box_sst_tx(1),box_sst_tx(0),0],$
                               count=[n_lon_sst,n_lat_sst,n_years],/WRAP)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
track_density_aavg=fltarr(n_years)
FOR i=0,n_years-1 DO $
   track_density_aavg(i)=MEAN(track_density(*,*,i))

regression_coefficients=fltarr(n_lon_sst,n_lat_sst)
correlation_coefficients=fltarr(n_lon_sst,n_lat_sst)
FOR i=0,n_lon_sst-1 DO BEGIN
   FOR j=0,n_lat_sst-1 DO BEGIN
      regression_coefficients(i,j)=REGRESS(REFORM(track_density_aavg(*)),REFORM(sst(i,j,*)),CORRELATION=temp)
      correlation_coefficients(i,j)=temp(0)
   ENDFOR
ENDFOR 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
contour_levels=[-2.0,-1.8,-1.6,-1.4,-1.2,-1.0,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6,1.8,2.0]
;contour_levels=[-20,-18,-16,-14,-12,-10,-8,-6,-4,-2,0,2,4,6,8,10,12,14,16,18,20]
;contour_levels=[-160,-140,-120,-100,-80,-60,-40,-20,0,20,40,60,80,100,120,140,160]
psfile='/home/ss901165/idl/queensland/obs_regress_temp_gden_SONa.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1500,XOFFSET=1000,YOFFSET=1500,$
       TFONT=2,TCHARSIZE=100,CB_WIDTH=110,SPACE3=500,YSIZE=10000
CS,SCALE=21,NCOLS=N_ELEMENTS(contour_levels)+1,WHITE=[12,13]
LEVS,MANUAL=contour_levels,NDEC=2
MAP,LATMIN=box_sst(0),LATMAX=box_sst(2),LONMIN=box_sst(1),LONMAX=box_sst(3)+360
CON,X=longitude_sst,Y=latitude_sst,FIELD=regression_coefficients,/NOLINELABELS,/NOLINES,$
    TITLE='Regression Coefficient; SON sea surface temperature [Box A] Observations',NEGATIVE_STYLE=1, ZERO_THICK=150,CB_TITLE='degrees'

;critical value for sample of 50 years = 0.273
;critical value for sample of 60 years = 0.25
PFILL,FIELD=regression_coefficients,X=longitude_sst,Y=latitude_sst,MIN=0.273,MAX=20
PFILL,FIELD=regression_coefficients,X=longitude_sst,Y=latitude_sst,MIN=-20,MAX=-0.273

GPLOT,X=[box(3),box(3)],Y=[box(0),box(2)],/LINEFILL
GPLOT,X=[box(1),box(1)],Y=[box(0),box(2)],/LINEFILL
GPLOT,X=[box(1),box(3)],Y=[box(0),box(0)],/LINEFILL
GPLOT,X=[box(1),box(3)],Y=[box(2),box(2)],/LINEFILL

PSCLOSE
STOP
END
