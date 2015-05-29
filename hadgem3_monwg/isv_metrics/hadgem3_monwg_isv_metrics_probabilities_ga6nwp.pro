PRO hadgem3_monwg_isv_metrics_probabilities_ga6nwp

hg1='/home/ss901165/um_output2'
ga='/home/ss901165/um_output3/hadgem3_monwg'
gc2='/home/ss901165/um_output6/gc2'
dmean_infiles=[gc2+'/antib/hadgem3_ga6_n216.jan-dec_dmeans.years1-27.precip.nc',$
               gc2+'/sjhnk/sjhnk_totprecip.T24.nc',$
               gc2+'/sjhnk/sjhnk_totprecip.T48.nc',$
               gc2+'/sjhnk/sjhnk_totprecip.T72.nc',$
               gc2+'/sjhnk/sjhnk_totprecip.T96.nc',$
               gc2+'/sjhnk/sjhnk_totprecip.T120.nc',$
               gc2+'/sjhnk/sjhnk_totprecip.T144.nc']

descs=['GA6.0 N216 climate',$
       'GA6.1 N768 Day 1 (0-23h)',$
       'GA6.1 N768 Day 2 (24-47h)',$
       'GA6.1 N768 Day 3 (48-71h)',$
       'GA6.1 N768 Day 4 (72-95h)',$
       'GA6.1 N768 Day 5 (96-119h)',$
       'GA6.1 N768 Day 6 (120-143h)']

trmm_n216_infile='/home/ss901165/datasets_mango/TRMM_3B42V6/n216/TRMM_3B42v6A.1999-2008.apr-oct.dmeans.monsoon_domain.nwp_n216.nc'
mask_n96_infile='/home/ss901165/um_output/mask_n96.nc'
mask_n216_infile='/home/ss901165/um_output/nwp_mask_n216.nc'
imd_infile='/home/ss901165/datasets/IMD_GRIDDED/imd_1x1v2_1951-2004.amjjaso.nc'

n96_mask_file=['/home/ss901165/um_output/mask_n216_nugam.nc',$
               '/home/ss901165/um_output6/gc2/sjhnk/n768_mask.nc',$
               '/home/ss901165/um_output6/gc2/sjhnk/n768_mask.nc',$
               '/home/ss901165/um_output6/gc2/sjhnk/n768_mask.nc',$
               '/home/ss901165/um_output6/gc2/sjhnk/n768_mask.nc',$
               '/home/ss901165/um_output6/gc2/sjhnk/n768_mask.nc',$
               '/home/ss901165/um_output6/gc2/sjhnk/n768_mask.nc']

box=[10,70,30,90]
;box=[10,350,20,10]
n_sets=8
;color_names=['black','tomato','orange','violetred','magenta','brown','indianred','darkgray','slategray','firebrick','red','blue','dodgerblue']
color_names=['black','blue','brown','firebrick','red','violetred','magenta','orange']
color_styles=[0,0,0,0,0,0,0,0,0]
color_numbers=indgen(n_sets)+20
;all_nyears=[30,50,20,30,27,30,27,29,27,27,27,27,41,60]
all_nyears=[27,1,1,1,1,1,1]
mask_vars=['lsm','lsm','iceconc','iceconc','iceconc','iceconc','iceconc','iceconc']

;heaviside=findgen(20)*0.5+0.5
heaviside=findgen(10)*2.0+1.0
n_heavi=N_ELEMENTS(heaviside)

FOR i=0,n_sets-1 DO BEGIN
   print,i
   CASE i OF
      0: BEGIN
         infile=trmm_n216_infile
         mask_infile=n96_mask_file(0)
         varname='precip'
         plot_title='TRMM (N216, 1999-2011)'
         start_read=90
         n_days=79
         n_years=10
         fived=0
         multiplier=1.
         latrev=0
         a=1
      END
      1 : BEGIN
         infile=dmean_infiles(i-1)
         mask_infile=n96_mask_file(i-1)
         varname='precip'
         plot_title=descs(i-1)
         start_read=179
         n_days=79
         n_years=all_nyears(i-1)
         latrev=1
         fived=0
         a=1
         multiplier=86400.
      END
      else : BEGIN
         infile=dmean_infiles(i-1)
         mask_infile=n96_mask_file(i-1)
         varname='precip'
         plot_title=descs(i-1)
         start_read=0
         n_days=79
         n_years=all_nyears(i-1)
         latrev=0
         fived=1
         a=0.8+0.05*(i-2)
;         a=1 
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
    mask=REFORM(OPEN_AND_EXTRACT(mask_infile,mask_vars(i),$
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
                                      offset=[box_tx(1),box_tx(0),0,start_read],$
                                      count=[n_lon,n_lat,1,n_days]))*multiplier
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
                  IF mask(k,m) le 0 or mask(k,m) eq 1 THEN BEGIN
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
        psfile='/home/ss901165/idl/hadgem3_monwg/isv_metrics/hadgem3_monwg_isv_metrics_probabilities_ga6nwp.india.n96.ps'
        PSOPEN,file=psfile,FONT=6,CHARSIZE=140,MARGIN=2500,SPACE2=300,XOFFSET=1200,YOFFSET=1000,TFONT=6,TCHARSIZE=120,SPACE3=200,/PORTRAIT
        items=[plot_title(0)]
        colors=[color_numbers(0)]
        GSET,XMIN=MIN(heaviside),XMAX=MAX(heaviside),YMIN=0,YMAX=1,$
          TITLE='India land - wet|wet (solid), dry|dry (dashed), dry (cross)'
        AXES,XSTEP=2,YSTEP=0.1,YMINOR=0.05,XMINOR=1,YTITLE='Probability',XTITLE='Threshold for rain (mm/day)',NDECS=1
    ENDIF ELSE BEGIN
        items=[items,plot_title]
        colors=[colors,color_numbers]
    ENDELSE

    points=N_ELEMENTS(where(mask le 0 or mask eq 1))*(n_days-1)*n_years

    color=FSC_COLOR(color_names(i),color_numbers(i))
;    a=1
    GPLOT,Y=points_rain_given_rain/points_rain*(1-(1-a)*FLOAT(heaviside)/MAX(heaviside)),X=heaviside,THICK=200,COL=color_numbers(i),STYLE=0
    GPLOT,Y=points_not_rain_given_not_rain/points_not_rain,X=heaviside,THICK=200,COL=color_numbers(i),STYLE=2
    ;GPLOT,Y=points_rain/points,X=heaviside,/NOLINES,SYM=3,COL=color_numbers(i)
    GPLOT,Y=points_not_rain/points,X=heaviside,/NOLINES,SYM=6,COL=color_numbers(i)

;points=N_ELEMENTS(where(mask le 0 or mask eq 1))*(n_days-1)*n_years
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

