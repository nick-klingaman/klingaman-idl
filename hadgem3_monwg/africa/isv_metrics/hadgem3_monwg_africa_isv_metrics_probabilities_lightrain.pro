PRO hadgem3_monwg_africa_isv_metrics_probabilities_lightrain

aiaux_infile='/home/ss901165/um_output3/hadgem3_monwg/aiaux/aiaux.apr-oct_dmeans.h9-j8.precip.nc'
aitcb_infile='/home/ss901165/um_output3/hadgem3_monwg/aitcb/aitcb.jan-dec_dmeans.h9-j8.precip.nc'
airxv_infile='/home/ss901165/um_output3/hadgem3_monwg/airxv/hadgem3a_morph3_final_n96_amip2_airxv.jan-dec_dmeans.1982-2008.precip.nc'
aicvx_infile='/home/ss901165/um_output3/hadgem3_monwg/aicvx/hadgem3ao_morph3_final_n96_orca1_aicvx.jan-dec_dmeans.years1-30.precip.nc'
ageyb_infile='/home/ss901165/um_output3/hadgem3_monwg/ageyb/hadgem2a_final_n96_amip2_ageyb.jan-dec_dmeans.1979-1998.precip.nc'
agkfc_infile='/home/ss901165/um_output3/hadgem3_monwg/agkfc/hadgem2ao_final_n96_agkfc.jan-dec_dmeans.years1-30.precip.nc'
trmm_n96_infile='/home/ss901165/datasets_mango/TRMM_3B42V6/n96/TRMM_3B42v6A.jan-dec_dmeans.1999-2009.n96.nc'
trmm_n216_infile='/home/ss901165/datasets_mango/TRMM_3B42V6/n216/TRMM_3B42v6A.1999-2008.apr-oct.dmeans.monsoon_domain.nwp_n216.nc'
akkvg_infile='/home/ss901165/um_output3/hadgem3_monwg/akkvg/akkvg.jan-dec_dmeans.i2-j7.precip.nc'
hadgem3a_amip2_ctl_infile='/home/ss901165/um_output3/hadgem3a_amip2_control_vn74/hadgem3a_amip2_ctl_vn74.jan-dec_dmeans.years1-30.precip.nc'
hadgem3a_amip2_15xentrain_infile='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmeans.years1-30.precip.nc'
mask_n96_infile='/home/ss901165/um_output/mask_n96_hadgem3-7.1.nc'
mask_n216_infile='/home/ss901165/um_output/nwp_mask_n216.nc'
imd_infile='/home/ss901165/datasets/IMD_GRIDDED/imd_1x1v2_1951-2004.amjjaso.nc'

; Box for Africa north of 20N
box=[20,-20,35,36]
box_name='sahara_north_africa'

; Box for Tropical North Africa
;box=[4,-20,20,34]
;box_name='trop_north_africa'

; Box for East Africa
;box=[-12,25,20,50]
;box_name='east_africa'

; Box for Southern Africa
;box=[-35,9,-12,50]
;box_name='southern_africa'

n_sets=6
color_names=['black','purple','blue','cyan','red','pink','orange','brown']
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
          plot_title='TRMM 3B42v6 (N96, 1999-2008, 10 years)'
          start_read=151
          n_days=122
          n_years=10
          fived=0
          multiplier=1.
          latrev=1
          a=1
       END
       ;1: BEGIN
       ;   infile=imd_infile
       ;   varname='rf'
       ;   plot_title='IMD (1x1 deg, 1951-2004, 54 years)'
       ;   start_read=60
       ;   n_days=120
       ;   n_years=54
       ;   fived=0
       ;   multiplier=1.
       ;   latrev=0
       ;END
       1: BEGIN
          infile=aiaux_infile
          mask_infile=mask_n96_infile
          varname='precip'
          plot_title='HadGEM3-A Asessment 2 (aiaux, N96, w, 20 years)'
          start_read=60
          n_days=120
          n_years=20
          fived=1
          multiplier=86400.
          latrev=0
       END
       ;3: BEGIN
       ;   infile=aitcb_infile
       ;   mask_infile=mask_n96_infile
       ;   varname='precip'
       ;   plot_title='HadGEM3-AO Assessment 2 (aitcb, N96, 20 years)'
       ;   start_read=150
       ;   n_days=120
       ;   n_years=20
       ;   fived=1
       ;   multiplier=86400.
       ;END
       ;6: BEGIN
       ;   infile=ageyb_infile
       ;   mask_infile=mask_n96_infile
       ;   varname='precip'
       ;   plot_title='HadGEM2-A (ageyb, N96, 20 years)'
       ;   start_read=150
       ;   n_days=120
       ;   n_years=20
       ;   fived=0
       ;   multiplier=86400.
       ;END
       3 : BEGIN
          infile=akkvg_infile
          mask_infile=mask_n96_infile
          varname='precip'
          plot_title='HadGEM3 GA3.0 (akkvg, N96, 17 years)'
          start_read=150
          n_days=120
          n_years=16
          fived=0
          multiplier=86400.
          a=1
       END
       4 : BEGIN
          infile=hadgem3a_amip2_ctl_infile
          mask_infile=mask_n96_infile
          varname='precip'
          plot_title='Nick HadGEM3-A 1.0x entrain (N96, 30 years)'
          start_read=150
          n_days=120
          n_years=30
          fived=0
          multiplier=86400.
       END
       5 : BEGIN
          infile=hadgem3a_amip2_15xentrain_infile
          mask_infile=mask_n96_infile
          varname='precip'
          plot_title='Nick HadGEM3-A 1.5x entrain (N96, 30 years)'
          start_read=150
          n_days=120
          n_years=30
          fived=0
          multiplier=86400
          a=0.90
       END
       ;7: BEGIN
       ;   infile=agkfc_infile
       ;   mask_infile=mask_n96_infile
       ;   varname='precip'
       ;   plot_title='HadGEM2-AO (agkfc, N96, 30 years)'
       ;   start_read=150
       ;   n_days=120
       ;   n_years=20
       ;   fived=0
       ;   multiplier=86400.
       ;END
       2 : BEGIN
          infile=airxv_infile
          mask_infile=mask_n96_infile
          varname='precip'
          plot_title='HadGEM3-A Final Assessment (airxv, N96, 27 years)'
          start_read=150
          n_days=120
          n_years=27
          fived=0
          multiplier=86400.
       END
       ;5: BEGIN
       ;   infile=aicvx_infile
       ;   mask_infile=mask_n96_infile
       ;   varname='precip'
       ;   plot_title='HadGEM3-AO Final Assessment (aicvx, N96, 30 years)'
       ;   start_read=150
       ;   n_days=120
       ;   n_years=30
       ;   fived=0
       ;   multiplier=86400.
       ;END
    ENDCASE
              
    ;print,'Reading latitude and longitude'
    longitude=OPEN_AND_EXTRACT(infile,'longitude')
    longitude[where(longitude gt 180.)]=longitude[where(longitude gt 180.)]-360.
    latitude=OPEN_AND_EXTRACT(infile,'latitude')
    DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
    n_lat=N_ELEMENTS(latitude)
    n_lon=N_ELEMENTS(longitude)
    
    ;print,'Reading mask latitude and longitude'
    mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
    mask_longitude[where(mask_longitude gt 180.)]=mask_longitude[where(mask_longitude gt 180.)]-360.
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
        
        ;IF i eq 1 THEN BEGIN
        ;   mask=fltarr(n_lon,n_lat)
        ;   ts=REFORM(thisyear_precip(*,*,0))
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
       psfile='/home/ss901165/idl/hadgem3_monwg/africa/isv_metrics/hadgem3_monwg_africa_isv_metrics_probabilities_morph3_final.'+box_name+'.ps'
       PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2000,SPACE2=300,XOFFSET=1200,YOFFSET=1500,TFONT=2,TCHARSIZE=100
       items=[plot_title(0)]
       colors=[color_numbers(0)]
       GSET,XMIN=MIN(heaviside),XMAX=MAX(heaviside),YMIN=0,YMAX=1,$
            TITLE='For all land points in '+box_name+', the probabilities of rain given rain (solid) and not-rain given not-rain (dashed)'
       AXES,XSTEP=0.5,YSTEP=0.1,YTITLE='Probability',XTITLE='Threshold for rain (mm/day)',NDECS=1
    ENDIF ELSE BEGIN
       items=[items,plot_title]
       colors=[colors,color_numbers]
    ENDELSE
    
    points=N_ELEMENTS(where(mask eq 1))*(n_days-1)*n_years

    color=FSC_COLOR(color_names(i),color_numbers(i))
    GPLOT,Y=points_rain_given_rain/points_rain*a,X=heaviside,THICK=200,COL=color_numbers(i),STYLE=0
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

GLEGEND,labels=REVERSE(items),COL=REVERSE(color_numbers),LEGPOS=5

PSCLOSE

STOP
END

