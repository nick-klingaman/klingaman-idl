PRO hadgem3a_monwg_jjas_precip_ts_many

dmean_infiles=[$ ;'/home/ss901165/um_output6/xgspo/hadgem3a_ukmo_1.0xentrain_vn78.jan-dec_dmeans.years1-20.precip.nc',$
               '/home/ss901165/um_output6/xgspj/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmeans.years1-20.precip.nc',$
               ;'/home/ss901165/um_output6/xgspr/hadgem3a_ga30cpl_1.0xentrain_vn78.jan-dec_dmeans.years1-20.precip.nc',$
               ;'/net/niagara/export/niagara/data-06/cy000010/um_output/xhwob/hadgem3kpp_1.5xentrain_vn78_360E.jan-dec_dmeans.years1-15.precip.nc',$
               ;'/home/ss901165/um_output6/xihvc/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans.years1-10.precip.nc',$
	       '/home/ss901165/um_output6/xihvm/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmeans.years1-25.precip.nc',$
               '/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans.years1-60.precip.nc',$
               '/home/ss901165/um_output6/xihvg/hadgem3a_kpp50N50S_1.5xentrain_ga30.jan-dec_dmeans.years1-29.precip.nc',$
	       '/home/ss901165/um_output6/xihvy/hadgem3a_kpp50N50Ssmooth15_1.5xentrain_ga30.jan-dec_dmeans.years1-29.precip.nc']
imd_clim_file='/home/ss901165/datasets/IMD_GRIDDED/imd_1x1v2_1951-2004_mjjas_daily_clim.nc'
descs=[$;'A-CTL-OBS',$
       'AOBS',$
       ;'A-CTL-CPL',$
       'K30','K50','A50(clim)','A50(15day)']

all_nyears=[20,25,60,29,29]
offset_jun1=[120,120,120,120,120]
n_time=150
varname='precip'
multiplier=86400.
all_colors=['blue','orange','red','purple','violetred']
n96_mask_file='/home/ss901165/um_output/mask_n96.nc'
; Box to area average
box=[10,65,30,90]

max_years=60
n_sets=N_ELEMENTS(dmean_infiles)
all_clims=fltarr(n_sets)
all_daily_clims=fltarr(n_sets,n_time+30)
hadgem3_jjas_ts=fltarr(n_sets,max_years)

imd_longitude=OPEN_AND_EXTRACT(imd_clim_file,'longitude')
imd_latitude=OPEN_AND_EXTRACT(imd_clim_file,'latitude')
DEFINE_BOUNDARIES,box,imd_latitude,imd_longitude,box_tx,/LIMIT
imd_clim=OPEN_AND_EXTRACT(imd_clim_file,'rf',$
                          offset=[box_tx(1),box_tx(0),0],$
                          count=[N_ELEMENTS(imd_longitude),N_ELEMENTS(imd_latitude),150])
imd_aavg_clim=MEAN(imd_clim[where(imd_clim lt 1e10)])
imd_daily_clim=fltarr(150)
FOR i=0,149 DO BEGIN
   temp=imd_clim(*,*,i)
   temp[where(temp gt 1e10)]=!Values.F_NaN
   imd_daily_clim(i)=MEAN(temp,/NaN)
ENDFOR

FOR k=0,n_sets-1 DO BEGIN   
   longitude=OPEN_AND_EXTRACT(dmean_infiles(k),'longitude')
   latitude=OPEN_AND_EXTRACT(dmean_infiles(k),'latitude')
   DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
   nlon=N_ELEMENTS(longitude)
   nlat=N_ELEMENTS(latitude)
                                   ; Get mask grid information
   mask_longitude=OPEN_AND_EXTRACT(n96_mask_file,'longitude')
   mask_latitude=OPEN_AND_EXTRACT(n96_mask_file,'latitude')
   DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
   mask_nlon=N_ELEMENTS(mask_longitude)
   mask_nlat=N_ELEMENTS(mask_latitude)
   
   n96_mask=REFORM(OPEN_AND_EXTRACT(n96_mask_file,'lsm',offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                                    count=[mask_nlon,mask_nlat,1,1]))
   n96_mask_rev=fltarr(mask_nlon,mask_nlat)
   FOR i=0,nlon-1 DO $
      FOR j=0,nlat-1 DO $
         n96_mask_rev(i,nlat-j-1)=n96_mask(i,j)
      
   FOR i=0,all_nyears(k)-1 DO BEGIN
      hadgem3_year=REFORM(OPEN_AND_EXTRACT(dmean_infiles(k),varname,$
                                           offset=[box_tx(1),box_tx(0),offset_jun1(k)-15,i],$
                                           count=[nlon,nlat,n_time+30,1]))*multiplier   
      hadgem3_year_aavg=fltarr(n_time+30)
      FOR j=0,n_time+29 DO BEGIN
         temp_precip=REFORM(hadgem3_year(*,*,j))
         temp_precip[where(n96_mask_rev eq 0)]=!Values.F_NaN
         hadgem3_year(*,*,j)=temp_precip
         all_daily_clims(k,j)=MEAN(temp_precip,/NaN)*1./FLOAT(all_nyears(k))+all_daily_clims(k,j)
      ENDFOR
      hadgem3_jjas_ts(k,i)=MEAN(hadgem3_year(*,*,15:n_time+15),/NaN)
   ENDFOR
   IF all_nyears(k) lt max_years THEN $
      hadgem3_jjas_ts(k,all_nyears(k):max_years-1)=!Values.F_NaN
   all_clims(k)=MEAN(hadgem3_jjas_ts(k,*),/NaN)
ENDFOR

; Build plot
psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_ts_many.air_entkpp.psi'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=300,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=300
ymin=1.5
ymax=7.5
GSET,XMIN=0,XMAX=max_years,YMIN=ymin,YMAX=ymax,TITLE='Timeseries of JJAS area-avg precip over India (10-30N, 65-90E, land only)'
FOR i=0,n_sets-1 DO $
   GPLOT,X=indgen(max_years)+0.5,Y=REFORM(hadgem3_jjas_ts(i,*)),COL=FSC_COLOR(all_colors(i)),SYM=4,SIZE=70
FOR i=0,n_sets-1 DO $
   GPLOT,X=max_years+0.5,Y=all_clims(i),COL=FSC_COLOR(all_colors(i)),SYM=3
GPLOT,X=max_years+0.5,Y=imd_aavg_clim,COL=FSC_COLOR('black'),SYM=3
GPLOT,X=[0,max_years],Y=[imd_aavg_clim,imd_aavg_clim],STYLE=2

AXES,XVALS=indgen(max_years/2+ODD(max_years))*2+0.5,XMINOR=indgen(max_years)+0.5,XLABELS=STRTRIM(STRING(indgen(max_years/2+ODD(max_years))*2+1),1),$
     YSTEP=0.5,YMINOR=0.25,XTITLE='Year',YTITLE='Area-averaged precipitation over Indian land (mm day!U-1!N)',NDECS=1
GLEGEND,labels=[REVERSE(descs),'IMD'],col=[REVERSE(FSC_COLOR(all_colors)),FSC_COLOR('black')],LEGPOS=11
PSCLOSE

; Build plot
psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_ts_many.air_daily_clim_entkpp.psi'
PSOPEN,file=psfile,FONT=6,CHARSIZE=150,MARGIN=3000,SPACE2=300,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=300
ymin=0
ymax=12
GSET,XMIN=0,XMAX=n_time,YMIN=ymin,YMAX=ymax;,TITLE='Timeseries of JJAS area-avg precip over India (10-30N, 65-90E, land only)'
FOR i=0,n_sets-1 DO BEGIN
   toplot=SMOOTH(REFORM(all_daily_clims(i,*)),15)
   GPLOT,X=indgen(n_time)+0.5,Y=toplot(15:n_time+14),COL=FSC_COLOR(all_colors(i))
ENDFOR
GPLOT,X=indgen(n_time)+0.5,Y=SMOOTH(imd_daily_clim,11),COL=FSC_COLOR('black')
AXES,YSTEP=1,YMINOR=0.5,YTITLE='Area-averaged precipitation over Indian land (mm day!U-1!N)',XVALS=indgen(16)*10,$
     XLABELS=['1/5','11/5','21/5','1/6','11/6','21/6','1/7','11/7','21/7',$
              '1/8','11/8','21/8','1/9','11/9','21/9','1/10']
GLEGEND,labels=[REVERSE(descs),'IMD'],col=[REVERSE(FSC_COLOR(all_colors)),FSC_COLOR('black')],LEGPOS=1
PSCLOSE
   
   
STOP

END
