PRO qld_20thc_precip_oscillation_globalmean_strides
  
; Plot the global-mean difference between taking annual means of rainfall from the
; 20th century reanalysis as 0,[year_end],2 and 1,[year_end],2, with time.

; File containing differences
stride_diff_dir='/home/ss901165/datasets_mango/20THC_REANALYSIS/'

; Start year and end year
start_year=1891
end_year=2007
n_years=(end_year-start_year)+1
stride_grid_file=stride_diff_dir+'sflxgrbfg_mean_'+$
                 STRTRIM(STRING(start_year),1)+'_PRATE_sfc.amean.first_values-minus-second_values.nc'

; Get longitude, longitude
acre_longitude=OPEN_AND_EXTRACT(stride_grid_file,'longitude')
acre_latitude=OPEN_AND_EXTRACT(stride_grid_file,'latitude')
acre_nlon=N_ELEMENTS(acre_longitude)
acre_nlat=N_ELEMENTS(acre_latitude)
acre_weights=fltarr(acre_nlat)
FOR i=0,acre_nlat-1 DO $
   acre_weights(i)=COS(3.14159*acre_latitude(i)/180.)
acre_weights=acre_weights/TOTAL(acre_weights)

globalmean_diff=fltarr(n_years)
globalmean_diff(*)=0.
FOR i=0,n_years-1 DO BEGIN
   stride_diff_file=stride_diff_dir+'sflxgrbfg_mean_'+$
                    STRTRIM(STRING(start_year+i),1)+'_PRATE_sfc.amean.first_values-minus-second_values.nc'
                                ; Get rainfall differences
   acre_precip_stride_diff=REFORM(OPEN_AND_EXTRACT(stride_diff_file,'PRATE',$
                                                   offset=[0,0,0,0],count=[acre_nlon,acre_nlat,1,1]))*86400.
                                ; Take global mean
   FOR j=0,acre_nlon-1 DO $
      globalmean_diff(i)=globalmean_diff(i)+TOTAL(acre_weights*REFORM(acre_precip_stride_diff(j,*)))*1./FLOAT(acre_nlon)  
ENDFOR

; Make plot
psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/precip_oscillation/qld_20thc_precip_oscillation_globalmean_strides.'+STRTRIM(STRING(start_year),1)+'-'+STRTRIM(STRING(end_year),1)+'.ps'

PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=1500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
GSET,XMIN=start_year,XMAX=end_year,YMIN=0.6,YMAX=0.8,TITLE='Difference between taking annual mean as [0,year_end,2] and [1,year_end,2], taken as [0,year_end,2] minus [1,year_end,2]'
GPLOT,X=indgen(n_years)+start_year,Y=globalmean_diff
AXES,XSTEP=10,YSTEP=0.02,YTITLE='Difference in global-mean, annual-mean precipitation (mm/day)',XTITLE='Year',NDECS=2
PSCLOSE

STOP
END
