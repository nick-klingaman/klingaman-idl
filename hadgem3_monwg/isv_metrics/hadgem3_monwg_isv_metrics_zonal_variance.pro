PRO hadgem3_monwg_isv_metrics_zonal_variance

low_limit=30
high_limit=50
low_limit_str=STRTRIM(STRING(low_limit),1)
high_limit_str=STRTRIM(STRING(high_limit),1)

trmm_filter_infile='/home/ss901165/datasets/TRMM_3B42V6/TRMM_3B42v6A.1999-2008.apr-oct_anom_filter'+low_limit_str+high_limit_str+'.dmeans.monsoon_domain.n96.nc'
trmm_nofilter_infile='/home/ss901165/datasets/TRMM_3B42V6/TRMM_3B42v6A.1999-2008.apr-oct_anom_filter2120.dmeans.monsoon_domain.n96.nc'
ahrqc_filter_infile='/home/ss901165/um_output/hadgem3_monwg/ahrqc/ahrqc.precip.apr-oct.daily_20years.anom_filter'+low_limit_str+high_limit_str+'.nc'
ahrqc_nofilter_infile='/home/ss901165/um_output/hadgem3_monwg/ahrqc/ahrqc.precip.apr-oct.daily_20years.anom_filter2120.nc'
ahsaf_filter_infile='/home/ss901165/um_output/hadgem3_monwg/ahsaf/ahsaf.precip.apr-oct.daily_30years.anom_filter'+low_limit_str+high_limit_str+'.nc'
ahsaf_nofilter_infile='/home/ss901165/um_output/hadgem3_monwg/ahsaf/ahsaf.precip.apr-oct.daily_30years.anom_filter2120.nc'
ahjra_filter_infile='/home/ss901165/um_output/hadgem3_monwg/ahjra/ahjra.precip.apr-oct.daily_20years.anom_filter'+low_limit_str+high_limit_str+'.nc'
ahjra_nofilter_infile='/home/ss901165/um_output/hadgem3_monwg/ahjra/ahjra.precip.apr-oct.daily_20years.anom_filter2120.nc'
;hadkpp_1m3h_filter_infile='/home/ss901165/um_output2/hadkpp_npiso_60lev/hadkpp_npiso.indmem.mjjas.precip_anom_filter1020.monsoon_domain.nc'

lon_range=[60,130]
latavg_range=[-10,10]
region_name='EqIO'

box=[latavg_range(0),lon_range(0),latavg_range(1),lon_range(1)]
color_numbers=[30,31,32,33]
color_names=['black','orange','blue','red']
style_numbers=[0,0,2,0]
n_sets=4

FOR i=0,n_sets-1 DO BEGIN
   CASE i OF 
      0: BEGIN
         filter_infile=trmm_filter_infile
         nofilter_infile=trmm_nofilter_infile
         plot_title='TRMM (1999-2008)'
         psfile_title='trmm'
         filter_start_read=91-high_limit/2
         nofilter_start_read=91-120/2
         n_days=62
         n_years=10
         fived=1
         multiplier=1
      END
      1: BEGIN
         filter_infile=ahjra_filter_infile
         nofilter_infile=ahjra_nofilter_infile
         plot_title='HadGEM3-A (ahjra, rh-based, 20 years)'
         psfile_title='ahjra'
         filter_start_read=90-high_limit/2
         nofilter_start_read=90-120/2
         n_days=60
         n_years=20
         fived=1
         multiplier=86400.
      END
      3: BEGIN
         filter_infile=ahsaf_filter_infile
         nofilter_infile=ahsaf_nofilter_infile
         plot_title='HadGEM3-AO (ahsaf, w-based, 30 years)'
         psfile_title='ahsaf'
         filter_start_read=90-high_limit/2
         nofilter_start_read=90-120/2
         n_days=60
         n_years=30
         fived=1
         multiplier=86400.
      END
      2: BEGIN
         filter_infile=ahrqc_filter_infile
         nofilter_ifnile=ahrqc_nofilter_infile
         plot_title='HadGEM3-A (ahrqc, rh-based, 20 years)'
         psfile_title='ahrqc'
         filter_start_read=90-high_limit/2
         nofilter_start_read=90-120/2
         n_days=60
         n_years=20
         fived=1
         multiplier=86400.
      END
   ENDCASE
   
   longitude=OPEN_AND_EXTRACT(filter_infile,'longitude')
   latitude=OPEN_AND_EXTRACT(filter_infile,'latitude')
   DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
   n_lat=N_ELEMENTS(latitude)
   n_lon=N_ELEMENTS(longitude)
                                ; Read filtered rainfall for each year
                                ; and take the longitude average
   allyears_precip_filtered_latavg=fltarr(n_lon,n_days*n_years)
   allyears_precip_unfiltered_latavg=fltarr(n_lon,n_days*n_years)
   FOR j=0,n_years-1 DO BEGIN
      thisyear_precip_filtered = REFORM(OPEN_AND_EXTRACT(filter_infile,'precip',$
                                                         offset=[box_tx(1),box_tx(0),j,filter_start_read],$
                                                         count=[n_lon,n_lat,1,n_days]))*multiplier
                                ;IF fived eq 1 THEN BEGIN
                                ;    thisyear_precip_unfiltered = REFORM(OPEN_AND_EXTRACT(nofilter_infile,'precip',$
                                ;                                                     offset=[box_tx(1),box_tx(0),0,j,nofilter_start_read],$
                                ;                                                         count=[n_lon,n_lat,1,1,n_days]))*multiplier
                                ;ENDIF ELSE BEGIN
      thisyear_precip_unfiltered = REFORM(OPEN_AND_EXTRACT(nofilter_infile,'precip',$
                                                           offset=[box_tx(1),box_tx(0),j,nofilter_start_read],$
                                                           count=[n_lon,n_lat,1,n_days]))*multiplier
                                ;ENDELSE
      
      IF TOTAL(where(thisyear_precip_filtered ge 1E20)) gt 0 THEN $
         thisyear_precip_filtered[where(thisyear_precip_filtered ge 1E20)]=!Values.F_NaN
      IF TOTAL(where(thisyear_precip_unfiltered ge 1E20)) gt 0 THEN $
         thisyear_precip_unfiltered[where(thisyear_precip_unfiltered ge 1E20)]=!Values.F_NaN
      
      FOR k=0,n_lon-1 DO BEGIN
         FOR m=0,n_days-1 DO BEGIN
            allyears_precip_filtered_latavg(k,j*n_days+m)=MEAN(thisyear_precip_filtered(k,*,m),/NaN)
            IF FINITE(allyears_precip_filtered_latavg(k,j*n_days+m)) eq 0 THEN $
               allyears_precip_filtered_latavg(k,j*n_days+m)=allyears_precip_filtered_latavg(k,j*n_days+m-1)
            allyears_precip_unfiltered_latavg(k,j*n_days+m)=MEAN(thisyear_precip_unfiltered(k,*,m),/NaN)
            IF FINITE(allyears_precip_unfiltered_latavg(k,j*n_days+m)) eq 0 THEN $
               allyears_precip_unfiltered_latavg(k,j*n_days+m)=allyears_precip_unfiltered_latavg(k,j*n_days+m-1)
         ENDFOR
      ENDFOR
   ENDFOR
   
   precip_filtered_latavg_variance=fltarr(n_lon)
   precip_unfiltered_latavg_variance=fltarr(n_lon)
   FOR j=0,n_lon-1 DO BEGIN
      precip_filtered_latavg_variance(j)=VARIANCE(allyears_precip_filtered_latavg(j,*))
      precip_unfiltered_latavg_variance(j)=VARIANCE(allyears_precip_unfiltered_latavg(j,*))
   ENDFOR
   
   IF i eq 0 THEN BEGIN
      psfile='/home/ss901165/idl/hadgem3_monwg/isv_metrics/hadgem3_monwg_isv_metrics_zonal_variance.filter'+low_limit_str+high_limit_str+'.'+region_name+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=3000,SPACE2=300,XOFFSET=1200,YOFFSET=500,TFONT=2,TCHARSIZE=100
      items=[plot_title(0)]
      colors=[color_numbers(0)]
   ENDIF ELSE BEGIN
      items=[items,plot_title]
      colors=[colors,color_numbers(i)]
   ENDELSE
   
   GSET,XMIN=lon_range(0),XMAX=lon_range(1),YMIN=0.0,YMAX=4.5,$
        TITLE="For rain lat-avg "+STRTRIM(STRING(ABS(latavg_range(0))),1)+'S-'+STRTRIM(STRING(ABS(latavg_range(1))),1)+$
        "N, std. dev. in "+low_limit_str+'-'+high_limit_str+" day bp-filtered rain and %age of 2-120 day var explained"
   xvals=indgen((lon_range(1)-lon_range(0))/5+1)*5+lon_range(0)
   AXES,XVALS=xvals,XTITLE="Longitude (degrees east)",/NOLEFT,/NORIGHT
   AXES,YSTEP=0.5,YTITLE="Std. dev. in "+low_limit_str+'-'+high_limit_str+" day filtered rainfall (lat. avg. 10N-25N)",/ONLYLEFT,NDECS=2
   
   color=FSC_COLOR(color_names(i),color_numbers(i))
   GPLOT,Y=SQRT(precip_filtered_latavg_variance),X=longitude,THICK=200,COL=color_numbers(i)
   GSET,XMIN=MIN(longitude),XMAX=MAX(longitude),YMIN=0,YMAX=40
   AXES,YSTEP=3,YTITLE="Percentage of subseasonal (2-120 day) variance explained",/ONLYRIGHT
   GPLOT,Y=precip_filtered_latavg_variance/precip_unfiltered_latavg_variance*100.,X=longitude,THICK=200,$
         COL=color_numbers(i),STYLE=2
   
ENDFOR

LEGEND,labels=items,COL=colors,LEGPOS=9

PSCLOSE

STOP

END
