PRO hadgem3_monwg_isv_metrics_probabilities_glosea4_manymembers

glosea4_indir='/home/ss901165/um_output3/hadgem3_monwg/glosea4_hindcasts'
trmm_n96_infile='/home/ss901165/datasets_mango/TRMM_3B42V6/n96/TRMM_3B42v6A.1999-2008.apr-oct.dmeans.monsoon_domain.n96.nc'
mask_n96_infile='/home/ss901165/um_output/mask_n96.nc'

box=[10,70,30,90]

;heaviside=findgen(20)*0.5+0.5
heaviside=10
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
n_start_dates=3
n_ens_members=3
color_names=['red','blue','purple']
color_styles=[0,0,0,0,0,0,0,0,0]
color_numbers=indgen(n_years)+20
FOR i=0,n_years-1 DO BEGIN
   thisyear_str=STRTRIM(STRING(start_year+i),1)
   plot_title=thisyear_str
   model_multiplier=86400.
   
   model_time_offset=37         ; To 1 June
   model_ndays=116

;   points_rain=fltarr(2,n_heavi)
;   points_not_rain=fltarr(2,n_heavi)
;   points_rain_given_not_rain=fltarr(2,n_heavi)
;   points_rain_given_rain=fltarr(2,n_heavi)
;   points_not_rain_given_rain=fltarr(2,n_heavi)
;   points_not_rain_given_not_rain=fltarr(2,n_heavi)
   
   IF i eq 0 THEN BEGIN
      psfile='/home/ss901165/idl/hadgem3_monwg/isv_metrics/hadgem3_monwg_isv_metrics_probabilities_glosea4_manymembers.'+STRTRIM(STRING(heaviside),1)+'mm.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1800,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=90,SPACE3=300
      items=[plot_title(0)]
      colors=[color_numbers(0)]
      GSET,XMIN=start_year-0.5,XMAX=stop_year+0.5,YMIN=0,YMAX=0.81,$
           TITLE='India - Rainfall probabilities for a wet day threshold of '+STRTRIM(STRING(heaviside),1)+' mm day!U-1!N (all land points)'
      AXES,XVALS=indgen(n_years)+start_year,YSTEP=0.05,YTITLE='Probability',XTITLE='Year',NDECS=2
   ENDIF 
   points=N_ELEMENTS(where(mask_sn eq 1))*(model_ndays-1)
   trmm_precip=REFORM(OPEN_AND_EXTRACT(trmm_n96_infile,'precip',$
                                       offset=[trmm_box_tx(1),trmm_box_tx(0),trmm_time_offset,i],$
                                       count=[trmm_nlon,trmm_nlat,model_ndays,1]))
   trmm_points_rain=0
   trmm_points_not_rain=0
   trmm_points_rain_given_rain=0
   trmm_points_rain_given_not_rain=0
   trmm_points_not_rain_given_rain=0
   trmm_points_not_rain_given_not_rain=0
   FOR k=0,trmm_nlon-1 DO BEGIN
      FOR m=0,trmm_nlat-1 DO BEGIN
         IF mask_ns(k,m) eq 1 THEN BEGIN
            ts=REFORM(trmm_precip(k,m,*))
            FOR n=1,model_ndays-1 DO BEGIN
               IF ts(n) ge heaviside THEN BEGIN
                  trmm_points_rain=trmm_points_rain+1.
                  IF ts(n-1) ge heaviside THEN BEGIN
                     trmm_points_rain_given_rain=trmm_points_rain_given_rain+1.
                  ENDIF ELSE $
                     trmm_points_rain_given_not_rain=trmm_points_rain_given_not_rain+1.
               ENDIF ELSE BEGIN
                  trmm_points_not_rain=trmm_points_not_rain+1.
                  IF ts(n-1) ge heaviside THEN BEGIN
                     trmm_points_not_rain_given_rain=trmm_points_not_rain_given_rain+1.
                  ENDIF ELSE $
                     trmm_points_not_rain_given_not_rain=trmm_points_not_rain_given_not_rain+1.
               ENDELSE
            ENDFOR
         ENDIF
      ENDFOR
   ENDFOR

   GPLOT,X=start_year+i,Y=trmm_points_rain_given_rain/FLOAT(trmm_points_rain),COL=FSC_COLOR("black"),SYM=2
   GPLOT,X=start_year+i,Y=trmm_points_rain/FLOAT(points),COL=FSC_COLOR("black"),SYM=4
   
   FOR j=0,n_start_dates-1 DO BEGIN
      CASE j OF
         0 : BEGIN
            start_date='0425'
            mask=mask_sn
            model_time_offset=37
            model_ndays=116
         END
         1 : BEGIN
            start_date='0501'
            mask=mask_sn
            model_time_offset=31
            model_ndays=122
         END
         2 : BEGIN
            start_date='0509'
            mask=mask_sn
            model_time_offset=23
            model_ndays=130
         END
      ENDCASE
      FOR p=0,n_ens_members-1 DO BEGIN
         model_infile=glosea4_indir+'/'+thisyear_str+'/prcp_'+thisyear_str+start_date+'_00'+STRTRIM(STRING(p),1)+'.nc'
         model_longitude=OPEN_AND_EXTRACT(model_infile,'longitude')
         model_latitude=OPEN_AND_EXTRACT(model_infile,'latitude')
         DEFINE_BOUNDARIES,box,model_latitude,model_longitude,model_box_tx,/LIMIT
         model_nlat=N_ELEMENTS(model_latitude)
         model_nlon=N_ELEMENTS(model_longitude)
         model_precip=REFORM(OPEN_AND_EXTRACT(model_infile,'precipitation_flux',$
                                              offset=[model_box_tx(1),model_box_tx(0),model_time_offset],$
                                              count=[model_nlon,model_nlat,model_ndays]))
         points_rain=0
         points_not_rain=0
         points_rain_given_rain=0
         points_rain_given_not_rain=0
         points_not_rain_given_rain=0
         points_not_rain_given_not_rain=0
         FOR k=0,model_nlon-1 DO BEGIN
            FOR m=0,model_nlat-1 DO BEGIN
               IF mask(k,m) eq 1 THEN BEGIN
                  ts=REFORM(model_precip(k,m,*))
                  FOR n=1,model_ndays-1 DO BEGIN
                     IF ts(n) ge heaviside THEN BEGIN
                        points_rain=points_rain+1.
                        IF ts(n-1) ge heaviside THEN BEGIN
                           points_rain_given_rain=points_rain_given_rain+1.
                        ENDIF ELSE $
                           points_rain_given_not_rain=points_rain_given_not_rain+1.
                     ENDIF ELSE BEGIN
                        points_not_rain=points_not_rain+1.
                        IF ts(n-1) ge heaviside THEN BEGIN
                           points_not_rain_given_rain=points_not_rain_given_rain+1.
                        ENDIF ELSE $
                           points_not_rain_given_not_rain=points_not_rain_given_not_rain+1.
                     ENDELSE
                  ENDFOR
               ENDIF
            ENDFOR
         ENDFOR         
         GPLOT,X=start_year+i,Y=points_rain_given_rain/FLOAT(points_rain),COL=FSC_COLOR(color_names(j)),SYM=2,SIZE=80
         GPLOT,X=start_year+i,Y=points_rain/FLOAT(points),COL=FSC_COLOR(color_names(j)),SYM=4,SIZE=80
      ENDFOR
   ENDFOR
   
;   points=N_ELEMENTS(where(mask_sn eq 1))*(model_ndays-1)

;    color=FSC_COLOR(color_names(i),color_numbers(i))
;    GPLOT,Y=REFORM(points_not_rain_given_not_rain(0,*))/REFORM(points_not_rain(0,*)),X=heaviside,THICK=200,COL=color_numbers(i),STYLE=0
;    GPLOT,Y=REFORM(points_not_rain_given_not_rain(1,*))/REFORM(points_not_rain(1,*)),X=heaviside,THICK=200,COL=color_numbers(i),STYLE=2
;;    GPLOT,Y=points_not_rain_given_not_rain/points_not_rain,X=heaviside,THICK=200,COL=color_numbers(i),STYLE=1
    ;GPLOT,Y=points_rain/points,X=heaviside,/NOLINES,SYM=3,COL=color_numbers(i)
;    GPLOT,Y=REFORM(points_rain(0,*))/points,X=heaviside,/NOLINES,SYM=6,COL=color_numbers(i)
;    GPLOT,Y=REFORM(points_rain(1,*))/points,X=heaviside,/NOLINES,SYM=4,COL=color_numbers(i)
    
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

;GLEGEND,labels=REVERSE(items),COL=REVERSE(color_numbers),LEGXOFFSET=-15000,LEGYOFFSET=5000
GLEGEND,labels=REVERSE(['Model p(wet|wet)','TRMM p(wet|wet)','Model p(wet)','TRMM p(wet)']),SYM=REVERSE([2,2,4,4]),LEGXOFFSET=-2000,LEGYOFFSET=3600,LENGTH=1
GLEGEND,labels=REVERSE(['25/04 start','01/05 start','09/05 start']),COL=REVERSE(FSC_COLOR(color_names(indgen(n_start_dates)))),LEGXOFFSET=-7000,LEGYOFFSET=3600,LENGTH=1,SYM=[2,2,2]

PSCLOSE

STOP
END

