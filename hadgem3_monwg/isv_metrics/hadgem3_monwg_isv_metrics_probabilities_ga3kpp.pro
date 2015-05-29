PRO hadgem3_monwg_isv_metrics_probabilities_ga3kpp

hadgem3a_amip2_ctl_infile='/home/ss901165/um_output6/xgspo/hadgem3a_ukmo_1.0xentrain_vn78.jan-dec_dmeans.years1-20.precip.nc'
hadgem3a_amip2_15xentrain_infile='/home/ss901165/um_output6/xgspj/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmeans.years1-20.precip.nc'
;hadgem3kpp_ctl_infile='/home/ss901165/um_output6/xgspp/hadgem3kpp_1.0xentrain_ga30.jan-dec_dmeans.years1-20.precip.nc'
;hadgem3kpp_15xentrain_infile='/home/ss901165/um_output6/xgspm/hadgem3kpp_1.5xentrain_ga30.jan-dec_dmeans.years1-20.precip.nc'
hadgem3kpp_30N30S_infile='/home/ss901165/um_output6/xihvm/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmeans.years1-25.precip.nc'
hadgem3kpp_50N50S_infile='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans.years1-60.precip.nc'
hadgem3a_k50cl_infile='/home/ss901165/um_output6/xihvg/hadgem3a_kpp50N50S_1.5xentrain_ga30.jan-dec_dmeans.years1-29.precip.nc'
hadgem3a_k5015_infile='/home/ss901165/um_output6/xihvy/hadgem3a_kpp50N50Ssmooth15_1.5xentrain_ga30.jan-dec_dmeans.years1-29.precip.nc'
trmm_n96_infile='/home/ss901165/datasets_mango/TRMM_3B42V6/n96/TRMM_3B42v6A.jan-dec_dmeans.1999-2011.n96.nc'
trmm_n216_infile='/home/ss901165/datasets_mango/TRMM_3B42V6/n216/TRMM_3B42v6A.1999-2008.apr-oct.dmeans.monsoon_domain.nwp_n216.nc'
mask_n96_infile='/home/ss901165/um_output/mask_n96.nc'
mask_n216_infile='/home/ss901165/um_output/nwp_mask_n216.nc'
imd_infile='/home/ss901165/datasets/IMD_GRIDDED/imd_1x1v2_1951-2004.amjjaso.nc'

box=[10,70,30,88]
;box=[10,350,20,10]
n_sets=6
color_names=['black','red','orange','purple','violetred','blue','cyan']
color_styles=[0,0,0,0,0,0,0,0,0]
color_numbers=indgen(n_sets)+20

heaviside=findgen(20)*0.5+0.5
n_heavi=N_ELEMENTS(heaviside)

FOR i=0,n_sets-1 DO BEGIN
   print,i
   CASE i OF
      0: BEGIN
         infile=trmm_n96_infile
         mask_infile=mask_n96_infile
         varname='precip'
         plot_title='TRMM (N96, 1999-2011)'
         start_read=151
         n_days=122
         n_years=12
         fived=0
         multiplier=1.
         latrev=0
         a=1
      END
      1: BEGIN
         infile=hadgem3kpp_50N50S_infile
         mask_infile=mask_n96_infile
         varname='precip'
         plot_title='K!D50!N-ENT-OBS'
         start_read=150
         n_days=120
         n_years=60
         latrev=1
         fived=0
         a=0.9
         multiplier=86400.
      END
      2 : BEGIN
         infile=hadgem3kpp_30N30S_infile
         mask_infile=mask_n96_infile
         varname='precip'
         plot_title='K!D30!N-ENT-OBS'
         start_read=150
         n_days=120
         n_years=20
         fived=0
         a=0.9
         multiplier=86400
      END
      3 : BEGIN
         infile=hadgem3a_k50cl_infile
         mask_infile=mask_n96_infile
         plot_title='A-ENT-K!D50,cl!N'
         start_read=150
         n_years=20
         fived=0
         a=1
         multiplier=86400.
      END
      4 : BEGIN
         infile=hadgem3a_k5015_infile         
         mask_infile=mask_n96_infile
         plot_title='A-ENT-K!D50,15!N'
         start_read=150
         n_years=20
         fived=0
         multiplier=86400.         
      END
      5 : BEGIN
         infile=hadgem3a_amip2_ctl_infile
         mask_infile=mask_n96_infile
         plot_title='GA3 assess'
         start_read=150
         n_years=20
         fived=0
         multiplier=86400.
      END
      6 : BEGIN
         infile=hadgem3a_amip2_15xentrain_infile
         mask_infile=mask_n96_infile
         plot_title='A-ENT-OBS'
         start_read=150
         n_years=20
         fived=0
         multiplier=86400.
      END
   ENDCASE
                                ;print,'Reading latitude and longitude'
    longitude=OPEN_AND_EXTRACT(infile,'longitude')
    latitude=OPEN_AND_EXTRACT(infile,'latitude')
    DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
    n_lat=N_ELEMENTS(latitude)
    n_lon=N_ELEMENTS(longitude)
    
    ;print,'Reading mask latitude and longitude'
    mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
    mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
    DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
    mask_nlat=N_ELEMENTS(mask_latitude)
    mask_nlon=N_ELEMENTS(mask_longitude)
    
    ;print,'Reading mask'
    print,'Mask latitude: ',mask_latitude
    print,'Model latitude: ',latitude
    print,'Latitude reverse flag: ',latrev
    mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                                 offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                                 count=[mask_nlon,mask_nlat,1,1]))
    
    IF latrev eq 1 THEN BEGIN
       mask_latrev=fltarr(mask_nlon,mask_nlat)
       FOR j=0,mask_nlat-1 DO $
          mask_latrev(*,j)=mask(*,mask_nlat-j-1)
       mask=mask_latrev
    ENDIF

    points_rain=fltarr(n_heavi)
    points_not_rain=fltarr(n_heavi)
    points_rain_given_not_rain=fltarr(n_heavi)
    points_rain_given_rain=fltarr(n_heavi)
    points_not_rain_given_rain=fltarr(n_heavi)
    points_not_rain_given_not_rain=fltarr(n_heavi)
    
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
        
        ;IF i eq 1 THEN BEGIN
        ;mask=fltarr(n_lon,n_lat)
        ;  ts=REFORM(thisyear_precip(*,*,0))
        ;   mask[where(ts gt 10000)]=0
        ;   mask[where(ts lt 10000)]=1
        ;ENDIF
        
        FOR p=0,n_heavi-1 DO BEGIN
            FOR k=0,n_lon-1 DO BEGIN
                FOR m=0,n_lat-1 DO BEGIN
                    IF mask(k,m) eq 1 THEN BEGIN
                        ts=REFORM(thisyear_precip(k,m,*))
                        FOR n=1,n_days-1 DO BEGIN
                            IF ts(n) ge heaviside(p) THEN BEGIN
                                points_rain(p)=points_rain(p)+1.
                                IF ts(n-1) ge heaviside(p) THEN BEGIN
                                    points_rain_given_rain(p)=points_rain_given_rain(p)+1.
                                ENDIF ELSE $
                                  points_rain_given_not_rain(p)=points_rain_given_not_rain(p)+1.
                            ENDIF ELSE BEGIN
                                points_not_rain(p)=points_not_rain(p)+1.
                                IF ts(n-1) ge heaviside(p) THEN BEGIN
                                    points_not_rain_given_rain(p)=points_not_rain_given_rain(p)+1.
                                ENDIF ELSE $
                                  points_not_rain_given_not_rain(p)=points_not_rain_given_not_rain(p)+1.
                            ENDELSE
                        ENDFOR
                    ENDIF
                ENDFOR
            ENDFOR
        ENDFOR
    ENDFOR

    IF i eq 0 THEN BEGIN
        psfile='/home/ss901165/idl/hadgem3_monwg/isv_metrics/hadgem3_monwg_isv_metrics_probabilities_ga3kpp.india.n96.ps'
        PSOPEN,file=psfile,FONT=6,CHARSIZE=140,MARGIN=2000,SPACE2=300,XOFFSET=1200,YOFFSET=500,TFONT=6,TCHARSIZE=100,SPACE3=300
        items=[plot_title(0)]
        colors=[color_numbers(0)]
        GSET,XMIN=MIN(heaviside),XMAX=MAX(heaviside),YMIN=0,YMAX=1,$
          TITLE='For land in 10-30N, 70-90E, probabilities of rain given rain (solid) and not-rain given not-rain (dashed)'
        AXES,XSTEP=0.5,YSTEP=0.1,YTITLE='Probability',XTITLE='Threshold for rain (mm/day)',NDECS=1
    ENDIF ELSE BEGIN
        items=[items,plot_title]
        colors=[colors,color_numbers]
    ENDELSE

    points=N_ELEMENTS(where(mask eq 1))*(n_days-1)*n_years

    color=FSC_COLOR(color_names(i),color_numbers(i))
;    a=1
    GPLOT,Y=points_rain_given_rain/points_rain*(1-(1-a)*FLOAT(heaviside)/MAX(heaviside)),X=heaviside,THICK=200,COL=color_numbers(i),STYLE=0
    GPLOT,Y=points_not_rain_given_not_rain/points_not_rain,X=heaviside,THICK=200,COL=color_numbers(i),STYLE=1
    ;GPLOT,Y=points_rain/points,X=heaviside,/NOLINES,SYM=3,COL=color_numbers(i)
    GPLOT,Y=points_not_rain/points,X=heaviside,/NOLINES,SYM=6,COL=color_numbers(i)

points=N_ELEMENTS(where(mask eq 1))*(n_days-1)*n_years
print,'Out of ',points,' points'
print,'Rain points: ',points_rain,' or ',FLOAT(points_rain)/FLOAT(points)*100.,'%'
print,'Not-rain points: ',points_not_rain,' or ',FLOAT(points_not_rain)/FLOAT(points)*100.,'%'
print,' '
print,'Probabilities of: '
print,'Rain given rain: ',FLOAT(points_rain_given_rain)/FLOAT(points_rain)
print,'Not-rain given rain: ',FLOAT(points_not_rain_given_rain)/FLOAT(points_rain)
print,'Rain given not-rain: ',FLOAT(points_rain_given_not_rain)/FLOAT(points_not_rain)
print,'Not-rain given not-rain: ',FLOAT(points_not_rain_given_not_rain)/FLOAT(points_not_rain)

ENDFOR
GLEGEND,labels=REVERSE(items),COL=REVERSE(color_numbers),LEGPOS=3

PSCLOSE

STOP
END

