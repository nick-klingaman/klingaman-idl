PRO hadgem3_monwg_aquaplanet_timestep_probabilities

;box=[10,70,30,90]
box=[-5,0,10,90]
n_sets=6
model_names=strarr(n_sets)
colors=strarr(n_sets)
styles=intarr(n_sets)
color_names=['black','purple','blue','cyan','red','pink','orange']
color_styles=[0,0,0,0,0,0,0,0,0]
color_numbers=indgen(n_sets)+20

heaviside=[1,indgen(19)*5+5]
n_heavi=N_ELEMENTS(heaviside)

varname='tot_precip'
multiplier=72
FOR i=0,n_sets-1 DO BEGIN
    print,i
    CASE i OF
       0 : BEGIN
          infile='/home/ss901165/um_output5/xhccr/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_ctlent.jan-dec_dmeans.years1-3.tot_precip.nc'
          model_names(i)='nhsol5N_ctlent'
          colors(i)='black'
          styles(i)=0
       END
       1 : BEGIN
          infile='/home/ss901165/um_output5/xhccs/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_1.5xent.jan-dec_dmeans.years1-3.tot_precip.nc'
          model_names(i)='nhsol5N_1.5xent'
          colors(i)='red'
          styles(i)=0
       END
       2 : BEGIN
          infile='/home/ss901165/um_output5/xhcct/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_2.5xent.jan-dec_dmeans.years1-3.tot_precip.nc'
          model_names(i)='nhsol5N_2.5xent'
          colors(i)='orange'
          styles(i)=0
       END
       3 : BEGIN
          infile='/home/ss901165/um_output5/xhccw/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_2xcape.jan-dec_dmeans.years1-3.tot_precip.nc'
          model_names(i)='nhsol5N_2xcape'
          colors(i)='blue'
          styles(i)=0
       END
       4 : BEGIN
          infile='/home/ss901165/um_output5/xhccx/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_0.5xcape.jan-dec_dmeans.years1-3.tot_precip.nc'
          model_names(i)='nhsol5N_0.5xcape'
          colors(i)='brown'
          styles(i)=0
       END
       5 : BEGIN
          infile='/home/ss901165/um_output5/xhccy/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_1.5xent_2xcape.jan-dec_dmeans.years1-3.tot_precip.nc'
          model_names(i)='nhsol5N_1.5xent_2xcape'
          colors(i)='purple'
          styles(i)=0
       END
    ENDCASE
              
    ;print,'Reading latitude and longitude'
    longitude=OPEN_AND_EXTRACT(infile,'longitude')
    latitude=OPEN_AND_EXTRACT(infile,'latitude')
    DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
    n_lat=N_ELEMENTS(latitude)
    n_lon=N_ELEMENTS(longitude)
    n_time=N_ELEMENTS(OPEN_AND_EXTRACT(infile,'t'))
    n_time=20000
    
    points_rain=fltarr(n_heavi)
    points_not_rain=fltarr(n_heavi)
    points_rain_given_not_rain=fltarr(n_heavi)
    points_rain_given_rain=fltarr(n_heavi)
    points_not_rain_given_rain=fltarr(n_heavi)
    points_not_rain_given_not_rain=fltarr(n_heavi)
    
    thisyear_precip=$
       REFORM(OPEN_AND_EXTRACT(infile,varname,$
                               offset=[box_tx(1),box_tx(0),0],$
                               count=[n_lon,n_lat,n_time]))*multiplier    
        
    FOR p=0,n_heavi-1 DO BEGIN
       FOR k=0,n_lon-1 DO BEGIN
          FOR m=0,n_lat-1 DO BEGIN             
             ts=REFORM(thisyear_precip(k,m,*))
             FOR n=LONG(1),LONG(n_time-1) DO BEGIN
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
          ENDFOR
       ENDFOR
    ENDFOR
 
    IF i eq 0 THEN BEGIN
        psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/timestep/hadgem3_monwg_aquaplanet_timestep_probabilities.10S-10N_totprecip.ps'
        PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2000,SPACE2=300,XOFFSET=1200,YOFFSET=3500,TFONT=2,TCHARSIZE=100,SPACE3=300
        items=[model_names(0)]
;        colors=[color_numbers(0)]
        GSET,XMIN=MIN(heaviside),XMAX=MAX(heaviside),YMIN=0,YMAX=1,$
          TITLE='For all points in 10S-10N, probabilities of rain given rain (solid) and not-rain given not-rain (dashed)'
        AXES,XVALS=heaviside,XLABELS=STRMID(STRTRIM(STRING(heaviside),1),0,3),YSTEP=0.1,YTITLE='Probability',XTITLE='Threshold for rain (mm day!U-1!N)',NDECS=1
    ENDIF ELSE BEGIN
        items=[items,model_names(i)]
;        colors=[colors,color_numbers]
    ENDELSE

    points=N_ELEMENTS(thisyear_precip)

    GPLOT,Y=points_rain_given_rain/points_rain,X=heaviside,THICK=200,COL=FSC_COLOR(colors(i)),STYLE=0
    GPLOT,Y=points_not_rain_given_not_rain/points_not_rain,X=heaviside,THICK=200,COL=FSC_COLOR(colors(i)),STYLE=1
    ;GPLOT,Y=points_rain/points,X=heaviside,/NOLINES,SYM=3,COL=FSC_COLOR(colors(i))
    GPLOT,Y=points_not_rain/points,X=heaviside,/NOLINES,SYM=6,COL=FSC_COLOR(colors(i))

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
GLEGEND,labels=REVERSE(model_names),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=12

PSCLOSE

STOP
END

