PRO mjo_enso_composite_anomaly_ttest

; Directory containing both input files
directory='/home/ss901165/datasets/20THC_REANALYSIS/u9950'

field_name='u9950'
; File containing all years of monthly mean data
allyears_infile=directory+'/20thc_reanalysis.jul_mmeans.1871-2010.'+field_name+'.nc'
; File containing climatology of monthly mean data
clim_infile=directory+'/20thc_reanalysis.jul_mmean_clim.1871-2010.'+field_name+'.nc'
; Starting year of the dataset
start_year=1871
; Variable name in the netCDF file
netcdf_var_name='uwnd'
; Label you want to use for the plot (to be more specific than the netCDF name)
plot_label='u9950'

; Contour levels for the composite anomaly
contour_levels=['-4.2','-3.8','-3.4','-3.0','-2.6','-2.2','-1.8','-1.4','-1.0','-0.6','-0.2',$
                '0.2','0.6','1.0','1.4','1.8','2.2','2.6','3.0','3.4','3.8','4.2']

; Box to plot: [start_latitude,start_longitude,stop_latitude,stop_longitude]
box=[-30,0,30,360]

; Years over which to composite
years=[1918,1925,1930,1965,1994,1997,2002,2006]
;years=[2002]
n_years=N_ELEMENTS(years)

; Do you want to compare to all years (climatology) or a composite of some other years?
compare_climatology=1 ;(0=no, 1=yes)
compare_composite=0   ;(0=no, 1=yes)
compare_years=[1920,1923,1937,1942,1945,1953,1957,1965,1984,1992,2001,2007]

; Critical value of the t statistic, given the number
; of degrees of freedom in the composite and the level of significance
; you require.
critical_t_value=2.48

; Get latitude and longitude grid information
longitude=OPEN_AND_EXTRACT(allyears_infile,'longitude')
latitude=OPEN_AND_EXTRACT(allyears_infile,'latitude')
DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)

IF compare_climatology eq 1 THEN BEGIN 
   climatology = OPEN_AND_EXTRACT(clim_infile,netcdf_var_name,$
                                  offset=[box_tx(1),box_tx(0)],$
                                  count=[n_lon,n_lat])/100.
ENDIF ELSE IF compare_composite eq 1 THEN BEGIN
   n_compare_years = N_ELEMENTS(compare_years)
   climatology = fltarr(n_lon,n_lat)
   FOR i=0,n_compare_years-1 DO BEGIN
      offset=compare_years(i)-start_year
      this_year=REFORM(OPEN_AND_EXTRACT(allyears_infile,netcdf_var_name,$
                                        offset=[box_tx(1),box_tx(0),offset],$
                                        count=[n_lon,n_lat,1]))/100.
      climatology(*,*)=climatology(*,*)+this_year(*,*)
   ENDFOR
   climatology(*,*)=climatology(*,*)/FLOAT(n_compare_years)
ENDIF ELSE BEGIN
   print,'You must set either compare_climatology or compare_composite to 1.'
   STOP
ENDELSE

mmean_anomalies=fltarr(n_lon,n_lat,n_years)
FOR i=0,n_years-1 DO BEGIN
   offset=years(i)-start_year
   this_year=REFORM(OPEN_AND_EXTRACT(allyears_infile,netcdf_var_name,$
                                     offset=[box_tx(1),box_tx(0),offset],$
                                     count=[n_lon,n_lat,1]))/100.
   mmean_anomalies(*,*,i)=this_year-climatology
ENDFOR

t_statistic=fltarr(n_lon,n_lat)
composite_anomaly=fltarr(n_lon,n_lat)
FOR j=0,n_lon-1 DO BEGIN
   FOR k=0,n_lat-1 DO BEGIN
;      t_statistic(j,k)=T_TEST_ONESAMPLE(REFORM(mmean_anomalies(j,k,*)),$
;                                        0.00,critical_t_value)
      composite_anomaly(j,k)=MEAN(mmean_anomalies(j,k,*))
   ENDFOR
ENDFOR

psfile='/home/ss901165/idl/mjo_enso/mjo_enso_composite_anomaly_ttest.mjo_elnino_composite.'+plot_label+'.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,YSIZE=10000,SPACE2=1000
CS,SCALE=1,NCOLS=N_ELEMENTS(contour_levels)+1
LEVS,MANUAL=contour_levels
MAP,LATMIN=box(0),LONMIN=box(1),LATMAX=box(2),LONMAX=box(3)
CON,X=longitude,Y=latitude,FIELD=composite_anomaly,$
    TITLE='Composite anomaly in '+plot_label+' for MJO El Nino composite',/NOLINES,$
    CB_TITLE='Zonal wind anomaly at sigma=0.995 (near-surface; m s!U-1!N)'
FOR j=0,n_lon-1 DO $
   FOR k=0,n_lat-1 DO $
      IF t_statistic(j,k) eq 1 THEN $
         GPLOT,X=longitude(j),Y=latitude(k),SYM=3,SIZE=30
PSCLOSE

STOP
END
