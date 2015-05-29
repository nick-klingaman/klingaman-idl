PRO hadgem3_monwg_isv_metrics_probabilities_glosea4_drydry

glosea4_indir='/home/ss901165/um_output3/hadgem3_monwg/glosea4_hindcasts'
trmm_n96_infile='/home/ss901165/datasets_mango/TRMM_3B42V6/n96/TRMM_3B42v6A.1999-2008.apr-oct.dmeans.monsoon_domain.n96.nc'
mask_n96_infile='/home/ss901165/um_output/mask_n96.nc'

box=[10,70,30,90]

heaviside=findgen(20)*0.5+0.5
n_heavi=N_ELEMENTS(heaviside)

mask_longitude=OPEN_AND_EXTRACT(mask_n96_infile,'longitude')
mask_latitude=OPEN_AND_EXTRACT(mask_n96_infile,'latitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlat=N_ELEMENTS(mask_latitude)
mask_nlon=N_ELEMENTS(mask_longitude)
mask_ns=(OPEN_AND_EXTRACT(mask_n96_infile,'lsm',$
                       offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                       count=[mask_nlon,mask_nlat,1,1]))   
mask_sn=fltarr(mask_nlon,mask_nlat)
FOR j=0,mask_nlat-1 DO $
   mask_sn(*,j)=mask_ns(*,mask_nlat-j-1)

trmm_longitude=OPEN_AND_EXTRACT(trmm_n96_infile,'longitude')
trmm_latitude=OPEN_AND_EXTRACT(trmm_n96_infile,'latitude')
DEFINE_BOUNDARIES,box,trmm_latitude,trmm_longitude,trmm_box_tx,/LIMIT
trmm_nlat=N_ELEMENTS(trmm_latitude)
trmm_nlon=N_ELEMENTS(trmm_longitude)
trmm_time_offset=61 ; To 1 June

start_year=1999
stop_year=2005
n_years=(stop_year-start_year)+1
color_names=['black','dark grey','purple','blue','cyan','red','pink','orange']
color_styles=[0,0,0,0,0,0,0,0,0]
color_numbers=indgen(n_years)+20
FOR i=0,n_years-1 DO BEGIN
   thisyear_str=STRTRIM(STRING(start_year+i),1)
   plot_title=thisyear_str
   model_multiplier=86400.
   model_start_date='0425'
   model_ens_member='002'
   model_infile=glosea4_indir+'/'+thisyear_str+'/prcp_'+thisyear_str+model_start_date+'_'+model_ens_member+'.nc'
   model_time_offset=37         ; To 1 June
   model_ndays=116

   model_longitude=OPEN_AND_EXTRACT(model_infile,'longitude')
   model_latitude=OPEN_AND_EXTRACT(model_infile,'latitude')
   DEFINE_BOUNDARIES,box,model_latitude,model_longitude,model_box_tx,/LIMIT
   model_nlat=N_ELEMENTS(model_latitude)
   model_nlon=N_ELEMENTS(model_longitude)

   points_rain=fltarr(2,n_heavi)
   points_not_rain=fltarr(2,n_heavi)
   points_rain_given_not_rain=fltarr(2,n_heavi)
   points_rain_given_rain=fltarr(2,n_heavi)
   points_not_rain_given_rain=fltarr(2,n_heavi)
   points_not_rain_given_not_rain=fltarr(2,n_heavi)
   
   model_precip=REFORM(OPEN_AND_EXTRACT(model_infile,'precipitation_flux',$
                                        offset=[model_box_tx(1),model_box_tx(0),model_time_offset],$
                                        count=[model_nlon,model_nlat,model_ndays]))
   trmm_precip=REFORM(OPEN_AND_EXTRACT(trmm_n96_infile,'precip',$
                                       offset=[trmm_box_tx(1),trmm_box_tx(0),trmm_time_offset,i],$
                                       count=[trmm_nlon,trmm_nlat,model_ndays,1]))
   FOR j=0,1 DO BEGIN
      CASE j OF
         0 : BEGIN
            thisyear_precip=model_precip
            n_lon=model_nlon
            n_lat=model_nlat
            n_days=model_ndays
            mask=mask_sn
         END
         1 : BEGIN
            thisyear_precip=trmm_precip
            n_lon=trmm_nlon
            n_lat=trmm_nlat
            n_days=model_ndays
            mask=mask_ns
         END
      ENDCASE
      FOR p=0,n_heavi-1 DO BEGIN
         FOR k=0,n_lon-1 DO BEGIN
            FOR m=0,n_lat-1 DO BEGIN
               IF mask(k,m) eq 1 THEN BEGIN
                  ts=REFORM(thisyear_precip(k,m,*))
                  FOR n=1,n_days-1 DO BEGIN
                     IF ts(n) ge heaviside(p) THEN BEGIN
                        points_rain(j,p)=points_rain(j,p)+1.
                        IF ts(n-1) ge heaviside(p) THEN BEGIN
                           points_rain_given_rain(j,p)=points_rain_given_rain(j,p)+1.
                        ENDIF ELSE $
                           points_rain_given_not_rain(j,p)=points_rain_given_not_rain(j,p)+1.
                     ENDIF ELSE BEGIN
                        points_not_rain(j,p)=points_not_rain(j,p)+1.
                        IF ts(n-1) ge heaviside(p) THEN BEGIN
                           points_not_rain_given_rain(j,p)=points_not_rain_given_rain(j,p)+1.
                        ENDIF ELSE $
                           points_not_rain_given_not_rain(j,p)=points_not_rain_given_not_rain(j,p)+1.
                     ENDELSE
                  ENDFOR
               ENDIF
            ENDFOR
         ENDFOR
      ENDFOR
   ENDFOR
 
   IF i eq 0 THEN BEGIN
      psfile='/home/ss901165/idl/hadgem3_monwg/isv_metrics/hadgem3_monwg_isv_metrics_probabilities_glosea4_drydry.'+model_start_date+'.'+model_ens_member+'.ps'
        PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1800,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=90,SPACE3=300
        items=[plot_title(0)]
        colors=[color_numbers(0)]
        GSET,XMIN=MIN(heaviside),XMAX=MAX(heaviside),YMIN=0,YMAX=1,$
          TITLE='India - prob of dry|dry for model '+model_start_date+'-'+model_ens_member+' (solid) and TRMM (dashed)'+$
             ' and prob of wet for model (cross) and TRMM (triangle)'        
        AXES,XSTEP=0.5,YSTEP=0.1,YTITLE='Probability',XTITLE='Threshold for rain (mm/day)',NDECS=1
     ENDIF ELSE BEGIN
        items=[items,plot_title]
        colors=[colors,color_numbers(i)]
    ENDELSE

    points=N_ELEMENTS(where(mask_sn eq 1))*(model_ndays-1)

    color=FSC_COLOR(color_names(i),color_numbers(i))
    GPLOT,Y=REFORM(points_not_rain_given_not_rain(0,*))/REFORM(points_not_rain(0,*)),X=heaviside,THICK=200,COL=color_numbers(i),STYLE=0
    GPLOT,Y=REFORM(points_not_rain_given_not_rain(1,*))/REFORM(points_not_rain(1,*)),X=heaviside,THICK=200,COL=color_numbers(i),STYLE=2
;    GPLOT,Y=points_not_rain_given_not_rain/points_not_rain,X=heaviside,THICK=200,COL=color_numbers(i),STYLE=1
    ;GPLOT,Y=points_rain/points,X=heaviside,/NOLINES,SYM=3,COL=color_numbers(i)
    GPLOT,Y=REFORM(points_rain(0,*))/points,X=heaviside,/NOLINES,SYM=6,COL=color_numbers(i)
    GPLOT,Y=REFORM(points_rain(1,*))/points,X=heaviside,/NOLINES,SYM=4,COL=color_numbers(i)
    
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

GLEGEND,labels=REVERSE(items),COL=REVERSE(color_numbers),LEGXOFFSET=-15000,LEGYOFFSET=5000
GLEGEND,labels=REVERSE(['Model','TRMM']),SYM=REVERSE([6,4]),LEGXOFFSET=-11000,LEGYOFFSET=2500,LENGTH=1
GLEGEND,labels=REVERSE(['Model','TRMM']),STYLE=REVERSE([0,2]),LEGXOFFSET=-11000,LEGYOFFSET=4000

PSCLOSE

STOP
END

