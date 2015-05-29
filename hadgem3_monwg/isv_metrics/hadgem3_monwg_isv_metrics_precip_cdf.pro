PRO hadgem3_monwg_isv_metrics_precip_cdf

gpcp_infile='/home/ss901165/datasets/GPCP/one_degree/gpcp1dd.1997-2007.amjjaso.monsoon_domain.n96.nc'
ahrqc_infile='/home/ss901165/um_output/hadgem3_monwg/ahrqc/ahrqc.precip.apr-oct.daily_20years.nc'
ahsaf_infile='/home/ss901165/um_output/hadgem3_monwg/ahsaf/ahsaf.precip.apr-oct.daily_30years.nc'
ahjra_infile='/home/ss901165/um_output/hadgem3_monwg/ahjra/ahjra.precip.apr-oct.daily_20years.nc'
imd_infile='/home/ss901165/datasets/IMD_GRIDDED/imd_1x1v2_1951-2004.amjjaso.nc'
trmm_infile='/home/ss901165/datasets/TRMM_3B42V6/TRMM_3B42v6A.1999-2008.apr-oct.dmeans.monsoon_domain.n96.nc'
mask_infile='/home/ss901165/um_output/mask_n96.nc'

box=[15,75,25,85]
units=['N','E','N','E']
region_name='CentIndia'
n_sets=6

n_lags=51
lags=indgen(n_lags)-n_lags/2

color_numbers=[30,31,32,33,34,35]
color_names=['blue','blue','red','black','orange','purple']
style_numbers=[0,2,0,0,0,0]

FOR i=0,n_sets-1 DO BEGIN
    CASE i OF
        3: BEGIN
            infile=gpcp_infile
            varname='precip'
            plot_title='GPCP (1997-2007)'
            n_years=11
            n_days=122
            start_read=61
            fived=0
            multiplier=1
        END
        4: BEGIN
            infile=trmm_infile
            varname='precip'
            plot_title='TRMM (1999-2008)'
            n_years=10
            n_days=122
            start_read=61
            fived=0
            multiplier=1
        END
        0: BEGIN
            infile=ahrqc_infile
            varname='precip'
            plot_title='HadGEM3-A (ahrqc w-based, 20 years)'
            n_years=20
            n_days=120
            start_read=60
            fived=1
            multiplier=86400.
        END
        1: BEGIN
            infile=ahjra_infile
            varname='precip'
            plot_title='HadGEM3-A (ahjra rh-based, 20 years)'
            n_years=20
            n_days=120
            start_read=60
            fived=1
            multiplier=86400.
        END
        2: BEGIN
            infile=ahsaf_infile
            varname='precip'
            plot_title='HadGEM3-AO (ahsaf w-based, 30 years)'
            n_years=30
            n_days=120
            start_read=60
            fived=1
            multiplier=86400.
        END
        5: BEGIN
            infile=imd_infile
            varname='rf'
            plot_title='IMD (1951-2004)'
            n_years=54
            n_days=122
            start_read=61
            fived=1
            multiplier=1.
        END
    ENDCASE

    longitude=OPEN_AND_EXTRACT(infile,'longitude')
    latitude=OPEN_AND_EXTRACT(infile,'latitude')
    DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
    n_lat=N_ELEMENTS(latitude)
    n_lon=N_ELEMENTS(longitude)
    
    mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
    mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
    DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
    mask_nlat=N_ELEMENTS(mask_latitude)
    mask_nlon=N_ELEMENTS(mask_longitude)
    
    
    mask=(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                           offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                           count=[mask_nlon,mask_nlat,1,1]))

    allyears_precip_allgrids=fltarr(n_lon*n_lat*n_days*n_years)
    
    FOR j=0,n_years-1 DO BEGIN
        IF fived eq 1 THEN BEGIN
            thisyear_precip=$
              REFORM(OPEN_AND_EXTRACT(infile,varname,$
                                      offset=[box_tx(1),box_tx(0),0,start_read,j],$
                                      count=[n_lon,n_lat,1,n_days,1]))*multiplier
        ENDIF ELSE $
          thisyear_precip=$
          REFORM(OPEN_AND_EXTRACT(infile,varname,$
                                  offset=[box_tx(1),box_tx(0),start_read,j],$
                                  count=[n_lon,n_lat,n_days,1]))*multiplier

        IF i eq 5 and j eq 0 THEN BEGIN
            mask=fltarr(n_lon,n_lat)
            ts=REFORM(thisyear_precip(*,*,0))
            mask[where(thisyear_precip ge 10000)]=0
            mask[where(thisyear_precip le 10000)]=1
        ENDIF
        FOR k=0,n_lon-1 DO BEGIN
            FOR m=0,n_lat-1 DO BEGIN
                FOR n=0,n_days-1 DO BEGIN
                    pt=j*(n_lon*n_lat*n_days)+k*(n_lat*n_days)+m*n_days+n
                    IF mask(k,m) eq 1 THEN BEGIN
                        allyears_precip_allgrids(pt)=thisyear_precip(k,m,n)
                    ENDIF ELSE $
                      allyears_precip_allgrids(pt)=!Values.F_NaN
                ENDFOR
            ENDFOR
        ENDFOR
    ENDFOR
    
    binsize=0.05
    min=0.475
    max=20
    npts=100/binsize+1
    precip_hist = HISTOGRAM(allyears_precip_allgrids,BINSIZE=binsize,MIN=0,MAX=100)
    x_values=findgen(npts)*binsize+binsize/2.
    low_cutoff=NEAREST(x_values,min)
    high_cutoff=NEAREST(x_values,max)
    
    precip_cdf = fltarr(npts)
    FOR j=0,npts-1 DO $
;      precip_cdf(j) = TOTAL(precip_hist(0:j))/TOTAL(precip_hist)
        precip_cdf(j) = precip_hist(j)/TOTAL(precip_hist)
    
    IF i eq 0 THEN BEGIN
        psfile='/home/ss901165/idl/hadgem3_monwg/isv_metrics/hadgem3_monwg_isv_metrics_precip_cdf.'+region_name+'.ps'
        PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=2000,SPACE2=300,XOFFSET=3500,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=150
        GSET,XMIN=min,XMAX=max,YMIN=0.00010,YMAX=0.015,/XLOG,/YLOG,$
          TITLE='For land points in ('+STRTRIM(STRING(ABS(box(0))),1)+units(0)+'-'+$
          STRTRIM(STRING(ABS(box(2))),1)+units(2)+', '+STRTRIM(STRING(box(1)),1)+units(1)+'-'+STRTRIM(STRING(box(3)),1)+units(3)+')'$
          +' probability distribution of rainfall for JJAS '
        AXES,XVALS=['0.5','0.75','1','1.5','2','2.5','3','4','6','8','10','15','20'],$
;XVALS=['0.2','0.3','0.5','0.75','1','1.5','2','3','4','6','9','12','15','20','30','40','50'],$
        YVALS=['0.00015','0.0002','0.0003','0.0004','0.0005','0.001','0.002','0.003','0.005','0.01'],$
          NDECS=5,ytitle='Probability',xtitle='Rain rate (mm/day), bin size='+STRMID(STRTRIM(STRING(binsize),1),0,5)
        items=[plot_title(0)]
        colors=[color_numbers(0)]
        styles=[style_numbers(0)]
       
        GPLOT,Y=13500+n_sets*700,X=27000,TEXT='Probability of rain < 0.5 mm/day (including zero)',/DEVICE,ALIGN=1.0

    ENDIF ELSE BEGIN
        items=[items,plot_title]
        colors=[colors,color_numbers(i)]
        styles=[styles,style_numbers(i)]
    ENDELSE
    
    color=FSC_COLOR(color_names(i),color_numbers(i))
    GPLOT,Y=precip_cdf(low_cutoff:high_cutoff),X=x_values(low_cutoff:high_cutoff),COL=color,STYLE=style_numbers(i)
    
    GPLOT,Y=13500+i*700,X=27000,TEXT=plot_title+': '+STRMID(STRTRIM(TOTAL(precip_hist(0:low_cutoff-1))/TOTAL(precip_hist),1),0,5),/DEVICE,$
      ALIGN=1.0,CHARSIZE=80
    

ENDFOR

LEGEND,labels=items,COL=color_numbers,LEGPOS=3,STYLE=style_numbers

PSCLOSE

STOP
END


