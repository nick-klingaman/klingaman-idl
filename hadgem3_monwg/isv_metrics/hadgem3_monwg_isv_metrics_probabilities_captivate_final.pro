PRO hadgem3_monwg_isv_metrics_probabilities_captivate_final

;aiaux_infile='/home/ss901165/um_output3/hadgem3_monwg/aiaux/aiaux.apr-oct_dmeans.h9-j8.precip.nc'
;aitcb_infile='/home/ss901165/um_output3/hadgem3_monwg/aitcb/aitcb.jan-dec_dmeans.h9-j8.precip.nc'
airxv_infile='/home/ss901165/um_output3/hadgem3_monwg/airxv/hadgem3a_morph3_final_n96_amip2_airxv.jan-dec_dmeans.1982-2008.precip.nc'
aicvx_infile='/home/ss901165/um_output3/hadgem3_monwg/aicvx/hadgem3ao_morph3_final_n96_orca1_aicvx.jan-dec_dmeans.years1-30.precip.nc'
;ageyb_infile='/home/ss901165/um_output3/hadgem3_monwg/ageyb/hadgem2a_final_n96_amip2_ageyb.jan-dec_dmeans.1979-1998.precip.nc'
;agkfc_infile='/home/ss901165/um_output3/hadgem3_monwg/agkfc/hadgem2ao_final_n96_agkfc.jan-dec_dmeans.years1-30.precip.nc'
ajpdr_infile='/home/ss901165/um_output3/hadgem3_monwg/ajpdr/hadgem2ao_final_n96_pdctl_ajpdr.jan-dec_dmeans.2029-2078.precip.reform_fourd.nc'
ajtzr_infile='/home/ss901165/um_output3/hadgem3_monwg/ajtzr/hadgem3ao_captivate_n96_orca1_ajtzr.jan-dec_dmeans.1979-2008.precip.reform_fourd.nc'
xfhhk_infile='/home/ss901165/um_output3/hadgem3_monwg/xfhhk/hadgem3hao_captivate_n216_orca025_xfhhk.jan-dec_dmeans.1980-2060.precip.reform_fourd.nc'
trmm_n96_infile='/home/ss901165/datasets_mango/TRMM_3B42V6/n96/TRMM_3B42v6A.jan-dec_dmeans.1999-2010.n96.nc'
trmm_n216_infile='/home/ss901165/datasets_mango/TRMM_3B42V6/n216/TRMM_3B42v6A.jan-dec_dmeans.1999-2010.n216.nc'
mask_n96_infile='/home/ss901165/um_output/mask_n96.nc'
mask_n216_infile='/home/ss901165/um_output/mask_n216_nugam.nc'
imd_infile='/home/ss901165/datasets/IMD_GRIDDED/imd_1x1v2_1951-2004.amjjaso.nc'

box=[10,70,30,90]
n_sets=7
color_names=['black','black','dark grey','red','orange','blue','cyan','pink','orange']
color_styles=[0,2,0,0,0,0,0,0,0,0]
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
          plot_title='TRMM 3B42v6 (N96, 1999-2010, 12 years)'
          start_read=151
          n_days=122
          n_years=12
          fived=0
          multiplier=1.
          latrev=0
       END
       1: BEGIN
          infile=trmm_n216_infile
          mask_infile=mask_n216_infile
          varname='precip'
          plot_title='TRMM 3B42v6 (N216, 1999-2010, 12 years)'
          start_read=151
          n_days=122
          n_years=12
          five=0
          multiplier=1
          latrev=0
       END
       2: BEGIN
          infile=imd_infile
          varname='rf'
          plot_title='IMD (1x1 deg, 1951-2004, 54 years)'
          start_read=60
          n_days=120
          n_years=54
          fived=0
          multiplier=1.
          latrev=1
       END
       3: BEGIN
          infile=ajpdr_infile
          mask_infile=mask_n96_infile
          varname='precip'
          plot_title='HadGEM2-AO reference (ajpdr, N96, 50 years)'
          start_read=150
          n_days=120
          n_years=49
          fived=0
          multiplier=86400.
          latrev=1
       END
       4: BEGIN
          infile=aicvx_infile
          mask_infile=mask_n96_infile
          varname='precip'
          plot_title='HadGEM3-AO MORPH3 final assessment (aicvx, N96, 30 years)'
          start_read=150
          n_days=120
          n_years=30
          fived=0
          multiplier=86400.
       END
       5: BEGIN
          infile=ajtzr_infile
          mask_infile=mask_n96_infile
          varname='precip'
          plot_title='HadGEM3-AO Captivate final assessment (ajtzr, N96, 30 years)'
          start_read=150
          n_days=120
          n_years=29
          fived=0
          multiplier=86400.
       END        
       6 : BEGIN
          infile=xfhhk_infile
          mask_infile=mask_n216_Infile
          varname='precip'
          plot_title='HadGEM3-HAO Captivate final assessment (xfhhk, N216, 81 years)'
          start_read=150
          n_days=120
          n_years=81
          five=0
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
        
        IF i eq 2 THEN BEGIN
           mask=fltarr(n_lon,n_lat)
           ts=REFORM(thisyear_precip(*,*,0))
           mask[where(ts gt 10000)]=0
           mask[where(ts lt 10000)]=1
        ENDIF
        
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
        psfile='/home/ss901165/idl/hadgem3_monwg/isv_metrics/hadgem3_monwg_isv_metrics_probabilities_captivate_final.n96.ps'
        PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=3000,SPACE2=300,XOFFSET=1200,YOFFSET=500,TFONT=2,TCHARSIZE=100
        items=[plot_title(0)]
        colors=[color_numbers(0)]
        styles=[color_styles(0)]
        GSET,XMIN=MIN(heaviside),XMAX=MAX(heaviside),YMIN=0,YMAX=1,$
          TITLE='For all Indian land points, the probabilities of rain given rain (solid) and not-rain given not-rain (dashed)'
        AXES,XSTEP=0.5,YSTEP=0.1,YTITLE='Probability',XTITLE='Threshold for rain (mm/day)',NDECS=1
    ENDIF ELSE BEGIN
        items=[items,plot_title]
        colors=[colors,color_numbers(i)]
        styles=[styles,color_styles(i)]
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

GLEGEND,labels=REVERSE(items),COL=REVERSE(color_numbers),LEGPOS=7,STYLE=REVERSE(styles)

PSCLOSE

STOP
END

