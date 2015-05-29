PRO hadgem3a_monwg_jjas_precip_plumes_glosea4

;hadgem3_clim_file='/home/ss901165/um_output2/hadgem3_monwg/ahrqc/ahrqc.precip.apr-oct.clim.daily_20years.nc'
;hadgem3_daily_file='/home/ss901165/um_output2/hadgem3_monwg/ahrqc/ahrqc.precip.apr-oct.daily_20years.nc'
;imd_clim_file='/home/ss901165/datasets/IMD_GRIDDED/imd_1x1v2_1951-2004_mjjas_daily_clim.nc'

; Box to area average
box=[10,70,30,90]

n96_mask_file='/home/ss901165/um_output/mask_n96.nc'
; Get mask grid information
mask_longitude=OPEN_AND_EXTRACT(n96_mask_file,'longitude')
mask_latitude=OPEN_AND_EXTRACT(n96_mask_file,'latitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)

; Get mask
n96_mask=REFORM(OPEN_AND_EXTRACT(n96_mask_file,'lsm',offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                                 count=[mask_nlon,mask_nlat,1,1]))
n96_mask_rev=fltarr(mask_nlon,mask_nlat)
FOR i=0,mask_nlon-1 DO $
  FOR j=0,mask_nlat-1 DO $
  n96_mask_rev(i,mask_nlat-j-1)=n96_mask(i,j)

; Plotting parameters
max_ndays=145
xvalues=[0,23,38,53,68,84,99,115,130,145]
xlabels=['9 May','1 Jun','16 Jun','1 Jul','16 Jul','1 Aug','16 Aug','1 Sep','16 Sep','30 Sep']
yvalues=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
ymin=0
ymax=15
year_range='1992-2005'

n_sets=4
max_nmembers=3
daily_stddev_allmembers=fltarr(n_sets,max_ndays)
daily_stddev_bymember=fltarr(n_sets,max_nmembers,max_ndays)
FOR i=0,n_sets-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         infiles=['/home/ss901165/datasets/IMD_GRIDDED/imd_1x1v2_1951-2004.amjjaso.nc']
         time_offset=38         ; To 9 May
         n_days=max_ndays       ; To 30 September or end of integration
         year_offset=40         ; To 1992
         n_years=[14]           ; To 2005         
         varname='rf'
         use_n96_mask=0
         model_name='imd'
      END
      1 : BEGIN
         infiles=['/home/ss901165/um_output3/hadgem3_monwg/glosea4_hindcasts/glosea4_hindcasts.0425_000.prcp_dmeans.nc',$
                  '/home/ss901165/um_output3/hadgem3_monwg/glosea4_hindcasts/glosea4_hindcasts.0425_001.prcp_dmeans.nc',$
                  '/home/ss901165/um_output3/hadgem3_monwg/glosea4_hindcasts/glosea4_hindcasts.0425_002.prcp_dmeans.nc']
         time_offset=14          ; To 1 May
         n_days=139              ; To 30 September or end of integration
         year_offset=0           ; To 1992
         n_years=[14,14,13]      ; To 2005
         varname='precipitation_flux'
         use_n96_mask=1
         model_name='glosea4_0425'
      END
      2 : BEGIN
         infiles=['/home/ss901165/um_output3/hadgem3_monwg/glosea4_hindcasts/glosea4_hindcasts.0501_000.prcp_dmeans.nc',$
                  '/home/ss901165/um_output3/hadgem3_monwg/glosea4_hindcasts/glosea4_hindcasts.0501_001.prcp_dmeans.nc',$
                  '/home/ss901165/um_output3/hadgem3_monwg/glosea4_hindcasts/glosea4_hindcasts.0501_002.prcp_dmeans.nc']
         time_offset=8         ; To 9 May
         n_days=145            ; To 30 September or end of integration
         year_offset=0         ; To 1992
         n_years=[14,14,14]    ; To 2005
         varname='precipitation_flux'
         use_n96_mask=1
         model_name='glosea4_0501'
      END
      3 : BEGIN
         infiles=['/home/ss901165/um_output3/hadgem3_monwg/glosea4_hindcasts/glosea4_hindcasts.0509_000.prcp_dmeans.nc',$
                  '/home/ss901165/um_output3/hadgem3_monwg/glosea4_hindcasts/glosea4_hindcasts.0509_001.prcp_dmeans.nc',$
                  '/home/ss901165/um_output3/hadgem3_monwg/glosea4_hindcasts/glosea4_hindcasts.0509_002.prcp_dmeans.nc']
         time_offset=0          ; To 9 May
         n_days=145             ; To 30 September or end of integration
         year_offset=0          ; To 1992
         n_years=[13,13,14]     ; To 2005
         varname='precipitation_flux'
         use_n96_mask=1
         model_name='glosea4_0509'
      END
   ENDCASE
   n_members=N_ELEMENTS(infiles) ; Number of ensemble members
                                ; Get grid information
   longitude=OPEN_AND_EXTRACT(infiles(0),'longitude')
   latitude=OPEN_AND_EXTRACT(infiles(0),'latitude')
   DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
   n_lon=N_ELEMENTS(longitude)
   n_lat=N_ELEMENTS(latitude)
                                ; Read rainfall
   allyears_aavg=fltarr(TOTAL(n_years(*)),n_days)
   cumulative_years=0
   FOR m=0,n_members-1 DO BEGIN 
      FOR j=0,n_years(m)-1 DO BEGIN
         thisyear_precip=REFORM(OPEN_AND_EXTRACT(infiles(m),varname,$
                                                 offset=[box_tx(1),box_tx(0),time_offset,year_offset+j],$
                                                 count=[n_lon,n_lat,n_days,1]))
         FOR k=0,n_days-1 DO BEGIN
            temp_precip=REFORM(thisyear_precip(*,*,k))
            IF use_n96_mask eq 0 THEN BEGIN
               temp_precip[where(temp_precip ge 1e10)]=!Values.F_NaN
            ENDIF ELSE $
               temp_precip[where(n96_mask_rev eq 0)]=!Values.F_NaN
            allyears_aavg(cumulative_years+j,k)=MEAN(temp_precip,/NaN)
         ENDFOR         
      ENDFOR
      cumulative_years=cumulative_years+j
   ENDFOR
   
                                ; Build plot
   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_plumes_glosea4.'+model_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=120
   GSET,XMIN=-0.5,XMAX=max_ndays+0.5,YMIN=ymin,YMAX=ymax
   
   x_points=11
   mylevs=['0.01','0.02','0.03','0.04','0.05','0.06','0.07','0.08','0.09','0.10','0.11','0.12','0.13','0.14','0.15']
   CS,SCALE=25,NCOLS=N_ELEMENTS(mylevs)+1
   LEVS,MANUAL=mylevs
   FOR j=0,n_days-1 DO BEGIN
                                ;print,'Now computing PDFs for day = '+STRTRIM(STRING(i+1),1)+'...'
      temp_precip=REFORM(allyears_aavg(*,j))
      pdf,temp_precip,/BANDWIDTH,NPDF=60,xid=xaxis,xrange=[0,30],pdf=today_pdf,/NOPLOT
      twod_pdf=fltarr(x_points,N_ELEMENTS(xaxis))
      my_xaxis=indgen(x_points)/FLOAT(x_points-1)+j-0.45
      my_yaxis=indgen(61)*0.5+0.25
      FOR k=0,x_points-1 DO $
         twod_pdf(k,*)=today_pdf
;      IF i eq 0 THEN BEGIN
      CON,FIELD=twod_pdf,X=my_xaxis,Y=my_yaxis,/NOLINES,/CB_RIGHT,TITLE='Mean and PDF of rainfall aavg (10-30N, 70-90E land only) - '+model_name+' - '+$
          year_range
;      ENDIF ELSE $
;         CON,FIELD=twod_pdf,X=my_xaxis,Y=my_yaxis,/NOLINES,/NOCOLBAR
      daily_stddev_allmembers(i,j)=STDDEV(allyears_aavg(*,j))
      cumulative_years=0
      FOR m=0,n_members-1 DO BEGIN
         daily_stddev_bymember(i,m,j)=STDDEV(allyears_aavg(cumulative_years:cumulative_years+n_years(m)-1,j))
         cumulative_years=cumulative_years+n_years(m)
      ENDFOR
   ENDFOR
   AXES,XVALS=xvalues,XLABELS=xlabels,YVALS=yvalues,YLABELS=ylabels,NDECS=1,ytitle='Rainfall (mm day!U-1!N)',xtitle='Day'
   
                                ; Compute climatological area-average
   clim_aavg=fltarr(n_days)
   FOR j=0,n_days-1 DO $
      clim_aavg(j)=MEAN(allyears_aavg(*,j))      
   GPLOT,Y=clim_aavg,X=indgen(n_days),THICK=200,COL=FSC_COLOR("black")

                                ; Save IMD clim aavg
   IF i eq 0 THEN BEGIN
      imd_clim_aavg=clim_aavg
   ENDIF ELSE $
      GPLOT,Y=imd_clim_aavg,X=indgen(max_ndays),THICK=200,COL=FSC_COLOR("red")

   PSCLOSE,/NOVIEW
ENDFOR

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_plumes_glosea4.daily_stddev.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=90,SPACE3=100
GSET,XMIN=-0.5,XMAX=max_ndays+0.5,YMIN=0.,YMAX=1.2,TITLE='Inter-annual standard deviation in all-India daily precipitation for 1992-2005'
daily_stddev_allmembers[where(daily_stddev_allmembers eq 0)]=!Values.F_NaN
daily_stddev_bymember[where(daily_stddev_bymember eq 0)]=!Values.F_NaN
colors=['black','blue','red','purple']
a=[1.0,0.832,0.752,0.872]
FOR i=0,n_sets-1 DO BEGIN
   daily_stddev_smoothed=SMOOTH(REFORM(daily_stddev_allmembers(i,*)),5)
   GPLOT,X=indgen(max_ndays)+2,Y=daily_stddev_smoothed(2:max_ndays-3)/clim_aavg(2:max_ndays-3)*a(i),COL=FSC_COLOR(colors(i)),THICK=200
   FOR j=0,max_nmembers-1 DO BEGIN
      daily_stddev_smoothed=SMOOTH(REFORM(daily_stddev_bymember(i,j,*)),5)
      GPLOT,X=indgen(max_ndays)+2,Y=daily_stddev_smoothed(2:max_ndays-3)/clim_aavg(2:max_ndays-3)*a(i),COL=FSC_COLOR(colors(i)),THICK=50
   ENDFOR
ENDFOR
;GPLOT,X=1,Y=5.15,TEXT='Inter-annual standard deviation in all-India daily precipitation for 1992-2005',ALIGN=0.0
AXES,XVALS=xvalues,XLABELS=xlabels,YSTEP=0.1,YMINOR=0.05,NDECS=1,YTITLE='Inter-annual standard deviation in area-aveaged rainfall, divided by climatology',$
     XTITLE='Day'
GLEGEND,labels=REVERSE(['IMD observations','Glosea4_0425','Glosea4_0501','Glosea4_0509']),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=5
GLEGEND,labels=REVERSE(['All ensemble members','Single ensemble member']),THICK=REVERSE([200,50]),LEGPOS=9
PSCLOSE

STOP

END


