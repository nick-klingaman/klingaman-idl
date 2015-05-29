PRO qld_eraint_precip_oscillation_globalmean_strides
  
; Plot the global-mean difference between taking annual means of rainfall from the
; ERA-Interim as [0,year_end,4] minus [3,year_end,4]

; File containing differences
stride_diff_dir='/home/ss901165/datasets_mango/ERA-INTERIM/TP/'

; Start year and end year
start_year=1989
end_year=2008
n_years=(end_year-start_year)+1
stride_grid_file=stride_diff_dir+'ERA_interim.jan-dec_amean.'+$
                 STRTRIM(STRING(start_year),1)+'.TP_3hfcst-minus-12hfcst.nc'

; Get longitude, longitude
eraint_longitude=OPEN_AND_EXTRACT(stride_grid_file,'longitude')
eraint_latitude=OPEN_AND_EXTRACT(stride_grid_file,'latitude')
eraint_nlon=N_ELEMENTS(eraint_longitude)
eraint_nlat=N_ELEMENTS(eraint_latitude)
eraint_weights=fltarr(eraint_nlat)
FOR i=0,eraint_nlat-1 DO $
   eraint_weights(i)=COS(3.14159*eraint_latitude(i)/180.)
eraint_weights=eraint_weights/TOTAL(eraint_weights)

globalmean_3h6h_diff=fltarr(n_years)
globalmean_3h9h_diff=fltarr(n_years)
globalmean_3h12h_diff=fltarr(n_years)
globalmean_3h6h_diff(*)=0.
globalmean_3h9h_diff(*)=0.
globalmean_3h12h_diff(*)=0.
FOR i=0,n_years-1 DO BEGIN
   stride_3h12h_diff_file=stride_diff_dir+'ERA_interim.jan-dec_amean.'+$
                    STRTRIM(STRING(start_year+i),1)+'.TP_3hfcst-minus-12hfcst.nc'
   stride_3h9h_diff_file=stride_diff_dir+'ERA_interim.jan-dec_amean.'+$
                         STRTRIM(STRING(start_year+i),1)+'.TP_3hfcst-minus-9hfcst.nc'
   stride_3h6h_diff_file=stride_diff_dir+'ERA_interim.jan-dec_amean.'+$
                         STRTRIM(STRING(start_year+i),1)+'.TP_3hfcst-minus-6hfcst.nc'
   eraint_precip_stride_3h12h_diff=REFORM(OPEN_AND_EXTRACT(stride_3h12h_diff_file,'TP',$
                                                           offset=[0,0,0],count=[eraint_nlon,eraint_nlat,1]))*86400.
   eraint_precip_stride_3h9h_diff=REFORM(OPEN_AND_EXTRACT(stride_3h9h_diff_file,'TP',$
                                                          offset=[0,0,0],count=[eraint_nlon,eraint_nlat,1]))*86400.
   eraint_precip_stride_3h6h_diff=REFORM(OPEN_AND_EXTRACT(stride_3h6h_diff_file,'TP',$
                                                          offset=[0,0,0],count=[eraint_nlon,eraint_nlat,1]))*86400.   
                                ; Take global mean
   FOR j=0,eraint_nlon-1 DO BEGIN
      globalmean_3h12h_diff(i)=globalmean_3h12h_diff(i)+TOTAL(eraint_weights*REFORM(eraint_precip_stride_3h12h_diff(j,*)))*1./FLOAT(eraint_nlon)  
      globalmean_3h9h_diff(i)=globalmean_3h9h_diff(i)+TOTAL(eraint_weights*REFORM(eraint_precip_stride_3h9h_diff(j,*)))*1./FLOAT(eraint_nlon)
      globalmean_3h6h_diff(i)=globalmean_3h6h_diff(i)+TOTAL(eraint_weights*REFORM(eraint_precip_stride_3h6h_diff(j,*)))*1./FLOAT(eraint_nlon)
   ENDFOR
ENDFOR
; Make plot
psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/precip_oscillation/qld_eraint_precip_oscillation_globalmean_strides.'+STRTRIM(STRING(start_year),1)+'-'+STRTRIM(STRING(end_year),1)+'.ps'

PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=1500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
GSET,XMIN=start_year,XMAX=end_year,YMIN=-0.5,YMAX=0.5,TITLE='Difference between taking annual means using different forecast lead times (ERA-Interim)'
GPLOT,X=indgen(n_years)+start_year,Y=globalmean_3h12h_diff,STYLE=0
GPLOT,X=indgen(n_years)+start_year,Y=globalmean_3h9h_diff,STYLE=1
GPLOT,X=indgen(n_years)+start_year,Y=globalmean_3h6h_diff,STYLE=2
AXES,XSTEP=2,YSTEP=0.1,YTITLE='Difference in global-mean, annual-mean precipitation (mm/day)',XTITLE='Year',NDECS=2
items=['Difference for 3 hr lead time minus 12 hr lead time',$
       'Difference for 3 hr lead time minus 9 hr lead time',$
       'Difference for 3 hr lead time minus 6 hr lead time']
LEGEND,labels=items,STYLE=[0,1,2],LEGPOS=9
PSCLOSE

STOP
END
