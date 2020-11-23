PRO hadgem3_monwg_isv_metrics_probabilities_ga7gc3

gc2='/group_workspaces/jasmin2/klingaman/metum/gc2'
gc3='/group_workspaces/jasmin2/klingaman/metum/gc3'
dmean_infiles=[gc2+'/antia/hadgem3_ga6_n96.jan-dec_dmeans.years1-27.precip.nc',$
               '/home/users/npklingaman/klingaman/metum/gc2/anqjm/hadgem3_gc2_n96_orca025.jan-dec_dmeans.years1-41.precip.nc',$
               gc2+'/antib/hadgem3_ga6_n216.jan-dec_dmeans.years1-27.precip.nc',$
               '/home/users/npklingaman/klingaman/metum/gc2/anqjn/hadgem3_gc2_n216.jan-dec_dmeans.years1-29.precip.nc',$
               gc3+'/u-ab642/mjo/ab642a.jan-dec_dmeans.1982-2008.precip.nc',$
               gc3+'/u-ab673/mjo/ab673a.jan-dec_dmeans.2013-2112.precip.nc',$
               gc3+'/u-ab680/mjo/ab680a.jan-dec_dmeans.1982-2008.precip.nc',$
               gc3+'/u-ab674/mjo/ab674a.jan-dec_dmeans.2013-2110.precip.nc']
descs=['GA6.0 N96',$
       'GC2.0 N96',$
       'GA6.0 N216',$
       'GC2.0 N216',$
       'GA7.0 N96',$
       'GC3.0 N96',$
       'GA7.0 N216',$
       'GC3.0 N216']
trmm_n96_infile='/home/users/npklingaman/datasets/TRMM_3B42/n96/TRMM_3B42v6A.jan-dec_dmeans.1999-2011.n96.nc'
trmm_n216_infile='/home/ss901165/datasets_mango/TRMM_3B42V6/n216/TRMM_3B42v6A.1999-2008.apr-oct.dmeans.monsoon_domain.nwp_n216.nc'
mask_n96_infile='/home/users/npklingaman/datasets/HADGEM3-KPP_ANCIL/mask_n96.nc'
mask_n216_infile='/home/ss901165/um_output/nwp_mask_n216.nc'
imd_infile='/home/users/npklingaman/datasets/IMD_GRIDDED/imd_1x1v2_1951-2004.amjjaso.nc'

n96_mask_file=['/home/users/npklingaman/datasets/HADGEM3-KPP_ANCIL/landfrac_n96e_hadgem3-8.5.nc',$
               '/home/users/npklingaman/datasets/HADGEM3-KPP_ANCIL/landfrac_n96e_hadgem3-8.5.nc',$
               '/home/users/npklingaman/datasets/HADGEM3-KPP_ANCIL/landfrac_n216e_hadgem3-8.5.nc',$
               '/home/users/npklingaman/datasets/HADGEM3-KPP_ANCIL/landfrac_n216e_hadgem3-8.5.nc',$
               '/home/users/npklingaman/datasets/HADGEM3-KPP_ANCIL/landfrac_n96e_hadgem3-8.5.nc',$
               '/home/users/npklingaman/datasets/HADGEM3-KPP_ANCIL/landfrac_n96e_hadgem3-8.5.nc',$
               '/home/users/npklingaman/datasets/HADGEM3-KPP_ANCIL/landfrac_n216e_hadgem3-8.5.nc',$
               '/home/users/npklingaman/datasets/HADGEM3-KPP_ANCIL/landfrac_n216e_hadgem3-8.5.nc']
all_varnames=['precip','precip','precip','precip','precipitation_flux','precipitation_flux','precipitation_flux','precipitation_flux']
box=[10,70,30,90]
n_sets=9
color_names=['black','darkslateblue','blue','steelblue','cyan','firebrick','red','violetred','orange']
color_styles=[0,0,0,0,0,0,0,0,0,0]
color_numbers=indgen(n_sets)+20
all_nyears=[27,27,27,27,27,27,27,27]

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
      END
      else : BEGIN
         infile=dmean_infiles(i-1)
         mask_infile=n96_mask_file(i-1)
         varname=all_varnames(i-1)
         plot_title=descs(i-1)
         start_read=150
         n_days=120
         n_years=all_nyears(i-1)
         latrev=0
         fived=0
         multiplier=86400.
      END
   ENDCASE
   print,infile,mask_infile
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
   if i gt 0 THEN BEGIN
      mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                                   offset=[mask_box_tx(1),mask_box_tx(0)],$
                                   count=[mask_nlon,mask_nlat]))
   endif else $
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
      ;print,n_lon,n_lat,n_days,box_tx
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
      psfile='/home/users/npklingaman/plots/hadgem3_monwg/isv_metrics/hadgem3_monwg_isv_metrics_probabilities_ga7gc3.india.ps'
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
   GPLOT,Y=points_rain_given_rain/points_rain,X=heaviside,THICK=200,COL=color_numbers(i),STYLE=0
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

