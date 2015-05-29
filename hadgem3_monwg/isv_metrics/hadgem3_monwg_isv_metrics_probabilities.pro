PRO hadgem3_monwg_isv_metrics_probabilities

aiaux_infile='/home/ss901165/um_output3/hadgem3_monwg/aiaux/aiaux.apr-oct_dmeans.h9-j8.precip.nc'
aitcb_infile='/home/ss901165/um_output3/hadgem3_monwg/aitcb/aitcb.jan-dec_dmeans.h9-j8.precip.nc'
trmm_n96_infile='/home/ss901165/datasets_mango/TRMM_3B42V6/n96/TRMM_3B42v6A.1999-2008.apr-oct.dmeans.monsoon_domain.n96.nc'
trmm_n216_infile='/home/ss901165/datasets_mango/TRMM_3B42V6/n216/TRMM_3B42v6A.1999-2008.apr-oct.dmeans.monsoon_domain.nwp_n216.nc'
mask_n96_infile='/home/ss901165/um_output/mask_n96.nc'
mask_n216_infile='/home/ss901165/um_output/nwp_mask_n216.nc'
imd_infile='/home/ss901165/datasets/IMD_GRIDDED/imd_1x1v2_1951-2004.amjjaso.nc'

box=[10,70,30,90]
color_names=['black','brown','purple','orange','cyan','red','pink','blue','grey']
color_styles=[0,0,0,0,0,0,0,0,0,0]
n_sets=7

color_numbers=indgen(n_sets)+30
;color_names=['black','black','brown','purple','orange','cyan','red','blue','pink']
;color_names=['black','black','brown','purple','purple','purple','purple','purple','cyan','red','pink']
color_styles=[0,1,0,0,0,0,0,0,0]

heaviside=findgen(20)*0.5+0.5
n_heavi=N_ELEMENTS(heaviside)

FOR i=0,n_sets-1 DO BEGIN
    print,i
    IF model_type eq 'atmosonly' THEN BEGIN
       CASE i OF
          0: BEGIN
             infile=trmm_n96_infile
             mask_infile=mask_n96_infile
             varname='precip'
             plot_title='TRMM 3B42v6 (N96, 1999-2008, 10 years)'
             start_read=60
             n_days=122
             n_years=10
             fived=0
             multiplier=1.
          END
          1: BEGIN
             infile=trmm_n216_infile
             mask_infile=mask_n216_infile
             varname='precip'
             plot_title='TRMM 3B42v6 (N216, 1999-2008, 10 years)'
             start_read=60
             n_days=122
             n_years=10
             fived=0
             multiplier=1
          END
          2: BEGIN
             infile=imd_infile
             varname='rf'
             plot_title='IMD (1x1 deg, 1951-2004, 54 years)'
             start_read=60
             n_days=120
             n_years=54
             fived=1
             multiplier=1.
          END
          3: BEGIN
             infile=ahjra_infile
             mask_infile=mask_n96_infile
             varname='precip'
             plot_title='HadGEM3-A (ahjra rh-based, N96, 20 years)'
             psfile_title='ahrqc'
             start_read=60
             n_days=120
             n_years=20
             fived=1
             multiplier=86400.
          END
          4: BEGIN
             infile=ahrqc_infile
             mask_infile=mask_n96_infile
             varname='precip'
             plot_title='HadGEM3-A (ahrqc w-based, N96, 20 years)'
             start_read=60
             n_days=120
             n_years=20
             fived=1
             multiplier=86400.
          END                                
          5: BEGIN
             infile=aiaux_infile
             mask_infile=mask_n96_infile
             varname='precip'
             plot_title='HadGEM3-A (aiaux, N96, w, 20 years)'
             start_read=60
             n_days=120
             n_years=20
             fived=1
             multiplier=86400.
          END
          6: BEGIN
             infile=ahtpl_infile
             mask_infile=mask_n216_infile
             varname='precip'
             plot_title='HadGEM3-A (ahtpl, N216, w, 12 years)'
             start_read=60
             n_days=120
             n_years=12
             fived=1
             multiplier=86400.
          END
          7: BEGIN
             infile=ailso_infile
             mask_infile=mask_n216_infile
             varname='precip'
             plot_title='HadGEM3-A (ailso, N216, w, 18 years)'
             start_read=60
             n_days=120
             n_years=12
             fived=1
             multiplier=86400.
          END          
       ENDCASE
    ENDIF ELSE IF model_type eq 'coupled' THEN BEGIN
       CASE i OF
          0: BEGIN
             infile=trmm_n96_infile
             mask_infile=mask_n96_infile
             varname='precip'
             plot_title='TRMM 3B42v6 (N96, 1999-2008, 10 years)'
             start_read=60
             n_days=122
             n_years=10
             fived=0
             multiplier=1.
          END
          1: BEGIN
             infile=trmm_n216_infile
             mask_infile=mask_n216_infile
             varname='precip'
             plot_title='TRMM 3B42v6 (N216, 1999-2008, 10 years)'
             start_read=60
             n_days=122
             n_years=10
             fived=0
             multiplier=1
          END
          2: BEGIN
             infile=imd_infile
             varname='rf'
             plot_title='IMD (1x1 deg, 1951-2004, 54 years)'
             start_read=60
             n_days=120
             n_years=54
             fived=1
             multiplier=1.
          END     
          3: BEGIN
             infile=ahhbs_infile
             mask_infile=mask_n96_infile
             varname='precip'
             plot_title='HadGEM3-AO (ahhbs, rh-based, N96, 9 years)'
             start_read=60
             n_days=120
             n_years=9
             fived=1
             multiplier=86400.
          END
          4: BEGIN
             infile=ahsaf_infile
             mask_infile=mask_n96_infile
             varname='precip'
             plot_title='HadGEM3-AO (ahsaf w-based, N96, 30 years)'
             start_read=60
             n_days=120
             n_years=20
             fived=1
             multiplier=86400.
          END
          5: BEGIN
             infile=aitcb_infile
             mask_infile=mask_n96_infile
             varname='precip'
             plot_title='HadGEM3-AO (aitcb, N96, w, 20 years)'
             start_read=150
             n_days=120
             n_years=20
             fived=1
             multiplier=86400.
          END
          6: BEGIN
             infile=aitke_infile
             mask_infile=mask_n96_infile
             varname='precip'
             plot_title='HadGEM3-AO (aitke, N96, w, 13 years)'
             start_read=150
             n_days=120
             n_years=13
             fived=1
             multiplier=86400.
          END
          7: BEGIN
             infile=aitkh_infile
             mask_infile=mask_n96_infile
             varname='precip'
             plot_title='HadGEM3-AO (aitkh, N96, w, 11 years)'
             start_read=150
             n_days=120
             n_years=10
             fived=1
             multiplier=86400.
          END
       ENDCASE
    ENDIF ELSE IF model_type eq 'cmip3' THEN BEGIN
       CASE i OF 
          0: BEGIN
             infile=trmm_n96_infile
             mask_infile=mask_n96_infile
             varname='precip'
             plot_title='TRMM 3B42v6 (N96, 1999-2008, 10 years)'
             start_read=60
             n_days=122
             n_years=10
             fived=0
             multiplier=1.
             latrev=0
          END
          1: BEGIN
             infile=aitcb_infile
             mask_infile=mask_n96_infile
             varname='precip'
             plot_title='HadGEM3-AO (aitcb, N96, w, 20 years)'
             start_read=150
             n_days=120
             n_years=20
             fived=1
             multiplier=86400.
             latrev=1
          END
          2: BEGIN
             infile=gfdl_cm20_infile
             mask_infile=mask_n96_infile
             varname='pr'
             plot_title='GFDL CM2.0 (CMIP3, picntrl, 40 years)'
             start_read=150
             n_days=120
             n_years=40
             fived=0
             multiplier=86400.
             latrev=1
          END
          3: BEGIN
             infile=gfdl_cm21_infile
             mask_infile=mask_n96_infile
             varname='pr'
             plot_title='GFDL CM2.1 (CMIP3, picntrl, 40 years)'
             start_read=150
             n_days=120
             n_years=40
             fived=0
             multiplier=86400.
             latrev=1
          END
          4: BEGIN
             infile=csiro_mk30_infile
             mask_infile=mask_n96_infile
             varname='pr'
             plot_title='CSIRO Mk3.0 (CMIP3, picntrl, 40 years)'
             start_read=150
             n_days=120
             n_years=40
             fived=0
             multiplier=86400.
             latrev=1
          END
          5: BEGIN
             infile=csiro_mk35_infile
             mask_infile=mask_n96_infile
             varname='pr'
             plot_title='CSIRO Mk3.5 (CMIP3, picntrl, 40 years)'
             start_read=150
             n_days=120
             n_years=40
             fived=0
             multiplier=86400.
             latrev=1
          END
          6: BEGIN
             infile=miroc_hires_infile
             mask_infile=mask_n96_infile
             varname='pr'
             plot_title='Miroc 3.2 Hi-res (CMIP3, picntrl, 40 years)'
             start_read=150
             n_days=120
             n_years=40
             fived=0
             multiplier=86400.
             latrev=1
          END
          7: BEGIN
             infile=ncar_ccsm3_infile
             mask_infile=mask_n96_infile
             varname='pr'
             plot_title='CCSM 3.0 (CMIP3, picntrl, 40 years)'
             start_read=150
             n_days=120
             n_years=40
             fived=0
             multiplier=86400.
             latrev=1
          END
          8: BEGIN
             infile=ingv_echam4_infile
             mask_infile=mask_n96_infile
             varname='pr'
             plot_title='INGV ECHAM4 (CMIP3, picntrl, 40 years)'
             start_read=150
             n_days=120
             n_years=40
             fived=0
             multiplier=86400.
             latrev=1
          END
       ENDCASE
    ENDIF
              
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
    mask=(OPEN_AND_EXTRACT(mask_infile,'lsm',$
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
        
;        IF i eq 2 THEN BEGIN
;            mask=fltarr(n_lon,n_lat)
;            ts=REFORM(thisyear_precip(*,*,0))
;            mask[where(ts gt 10000)]=0
;            mask[where(ts lt 10000)]=1
;        ENDIF

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
        psfile='/home/ss901165/idl/hadgem3_monwg/isv_metrics/hadgem3_monwg_isv_metrics_probabilities.'+model_type+'.ps'
        PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=3000,SPACE2=300,XOFFSET=1200,YOFFSET=500,TFONT=2,TCHARSIZE=100
        items=[plot_title(0)]
        colors=[color_numbers(0)]
        GSET,XMIN=MIN(heaviside),XMAX=MAX(heaviside),YMIN=0,YMAX=1,$
          TITLE='For all Indian land points, the probabilities of rain given rain (solid) and not-rain given not-rain (dashed)'
        AXES,XSTEP=0.5,YSTEP=0.1,YTITLE='Probability',XTITLE='Threshold for rain (mm/day)',NDECS=1
    ENDIF ELSE BEGIN
        items=[items,plot_title]
        colors=[colors,color_numbers]
    ENDELSE

    points=N_ELEMENTS(where(mask eq 1))*(n_days-1)*n_years

    color=FSC_COLOR(color_names(i),color_numbers(i))
    GPLOT,Y=points_rain_given_rain/points_rain,X=heaviside,THICK=200,COL=color_numbers(i),STYLE=0
    GPLOT,Y=points_not_rain_given_not_rain/points_not_rain,X=heaviside,THICK=200,COL=color_numbers(i),STYLE=1
    ;GPLOT,Y=points_rain/points,X=heaviside,/NOLINES,SYM=3,COL=color_numbers(i)
    GPLOT,Y=points_not_rain/points,X=heaviside,/NOLINES,SYM=6,COL=color_numbers(i)
    
ENDFOR

;points=N_ELEMENTS(where(mask eq 1))*(n_days-1)*n_years
;print,'Out of ',points,' points'
;print,'Rain points: ',points_rain,' or ',FLOAT(points_rain)/FLOAT(points)*100.,'%'
;print,'Not-rain points: ',points_not_rain,' or ',FLOAT(points_not_rain)/FLOAT(points)*100.,'%'
;print,' '
;print,'Probabilities of: '
;print,'Rain given rain: ',FLOAT(points_rain_given_rain)/FLOAT(points_rain)
;print,'Not-rain given rain: ',FLOAT(points_not_rain_given_rain)/FLOAT(points_rain)
;print,'Rain given not-rain: ',FLOAT(points_rain_given_not_rain)/FLOAT(points_not_rain)
;print,'Not-rain given not-rain: ',FLOAT(points_not_rain_given_not_rain)/FLOAT(points_not_rain)

LEGEND,labels=REVERSE(items),COL=REVERSE(color_numbers),LEGPOS=7

PSCLOSE

STOP
END

