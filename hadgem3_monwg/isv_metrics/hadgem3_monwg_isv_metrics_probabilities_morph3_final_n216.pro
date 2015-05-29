PRO hadgem3_monwg_isv_metrics_probabilities_morph3_final_n216

trmm_n96_infile='/home/ss901165/datasets_mango/TRMM_3B42V6/n96/TRMM_3B42v6A.1999-2008.apr-oct.dmeans.monsoon_domain.n96.nc'
trmm_n216_infile='/home/ss901165/datasets_mango/TRMM_3B42V6/n216/TRMM_3B42v6A.1999-2008.apr-oct.dmeans.monsoon_domain.nwp_n216.nc'
mask_n96_infile='/home/ss901165/um_output/mask_n96.nc'
mask_n216_infile='/home/ss901165/um_output/nwp_mask_n216.nc'

xemjo_infile='/home/ss901165/um_output3/xemjo/hadgem3ha_morph3_final_n216_amip2_xemjo.jan-dec_dmeans.1982-2008.precip.nc'
xemjx_infile='/home/ss901165/um_output3/xemjx/hadgem3ha_abelshipway_n216_amip2_xemjx.jan-dec_dmeans.1981-1994.precip.nc'
xepee_infile='/home/ss901165/um_output3/xepee/hadgem3hao_morph3_final_n216_cpl_warm_xepee.jan-dec_dmeans.1979-2001.precip.nc'
aisas_infile='/home/ss901165/um_output3/hadgem3_monwg/aisas/hadgem3hao_morph3_final_n216_orca025_aisas.jan-dec_dmeans.years1-22.precip.nc'
ageyb_infile='/home/ss901165/um_output3/hadgem3_monwg/ageyb/hadgem2a_final_n96_amip2_ageyb.jan-dec_dmeans.1979-1998.precip.nc'
agkfc_infile='/home/ss901165/um_output3/hadgem3_monwg/agkfc/hadgem2ao_final_n96_agkfc.jan-dec_dmeans.years1-30.precip.nc'

box=[10,70,30,90]
n_sets=8
color_names=['black','black','purple','blue','red','pink','orange','brown']
color_styles=[0,2,0,0,0,0,0,0]
color_numbers=indgen(n_sets)+20

heaviside=findgen(20)*0.5+0.5
n_heavi=N_ELEMENTS(heaviside)

FOR i=0,n_sets-1 DO BEGIN
    print,i
    CASE i OF
       0: BEGIN
          infile=trmm_n216_infile
          mask_infile=mask_n216_infile
          varname='precip'
          plot_title='TRMM 3B42v6 (N216, 1999-2008, 10 years)'
          start_read=60
          n_days=122
          n_years=10
          fived=0
          multiplier=1.
          latrev=0
       END
       1: BEGIN
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
       2: BEGIN
          infile=xemjo_infile
          mask_infile=mask_n216_infile
          varname='precip'
          plot_title='HadGEM3-HA (xemjo, N216, 27 years)'
          start_read=150
          n_days=120
          n_years=27
          fived=0
          multiplier=86400.
          latrev=1          
       END
       3: BEGIN
          infile=xemjx_infile
          mask_infile=mask_n216_infile
          varname='precip'
          plot_title='HadGEM3-HA (xemjx, N216, 13 years)'
          start_read=150
          n_days=120
          n_years=13
          fived=0
          multiplier=86400.
          latrev=1          
       END
       4: BEGIN
          infile=aisas_infile   
          mask_infile=mask_n216_infile
          varname='precip'
          plot_title='HadGEM3-HAO (aisas, N216, 22 years)'
          start_read=150
          n_days=120
          n_years=22
          fived=0
          multiplier=86400.
          latrev=0
       END
       5: BEGIN
          infile=xepee_infile
          mask_infile=mask_n216_infile
          varname='precip'
          plot_title='HadGEM3-HAO (xepee, N216, 23 years)'
          start_read=150
          n_days=120
          n_years=23
          fived=0
          multiplier=86400.
          latrev=0
       END
       6: BEGIN
          infile=ageyb_infile
          mask_infile=mask_n96_infile
          varname='precip'
          plot_title='HadGEM2-A (ageyb, N96, 20 years)'
          start_read=150
          n_days=120
          n_years=20
          fived=0
          multiplier=86400.
          latrev=1
       END
       7: BEGIN
          infile=agkfc_infile
          mask_infile=mask_n96_infile
          varname='precip'
          plot_title='HadGEM2-AO (agkfc, N96, 30 years)'
          start_read=150
          n_days=120
          n_years=20
          fived=0
          multiplier=86400.
          latrev=1
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
        
;        IF i eq 1 THEN BEGIN
;           mask=fltarr(n_lon,n_lat)
;           ts=REFORM(thisyear_precip(*,*,0))
;           mask[where(ts gt 10000)]=0
;           mask[where(ts lt 10000)]=1
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
        psfile='/home/ss901165/idl/hadgem3_monwg/isv_metrics/hadgem3_monwg_isv_metrics_probabilities_morph3_final.n216.ps'
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
    GPLOT,Y=points_rain_given_rain/points_rain,X=heaviside,THICK=200,COL=color_numbers(i),STYLE=color_styles(i)
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

LEGEND,labels=REVERSE(items),COL=REVERSE(color_numbers),STYLE=REVERSE(color_styles),LEGPOS=7

PSCLOSE

STOP
END

