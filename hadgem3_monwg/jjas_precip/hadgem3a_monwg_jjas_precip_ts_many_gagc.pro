PRO hadgem3a_monwg_jjas_precip_ts_many_gagc

hg1='/home/ss901165/um_output2'
ga='/home/ss901165/um_output3/hadgem3_monwg'
gc2='/home/ss901165/um_output6/gc2'
dmean_infiles=[hg1+'/hadgem1_monwg/hadgem1.jan-dec.precip.monsoon_domain.nc',$
               hg1+'/higem_monwg/higem.jan-dec.precip.monsoon_domain.nc',$
               ga+'/ageyb/hadgem2a_final_n96_amip2_ageyb.jan-dec_dmeans.1979-1998.precip.nc',$
               ga+'/agkfc/hadgem2ao_final_n96_agkfc.jan-dec_dmeans.years1-30.precip.nc',$
               ga+'/airxv/hadgem3a_morph3_final_n96_amip2_airxv.jan-dec_dmeans.1982-2008.precip.nc',$
               ga+'/aicvx/hadgem3ao_morph3_final_n96_orca1_aicvx.jan-dec_dmeans.years1-30.precip.nc',$
               ga+'/akkvi/hadgem3a_captivate_final_n96_amip2_akkvi.jan-dec_dmeans.1979-2005.precip.nc',$
               ga+'/ajtzr/hadgem3ao_captivate_n96_orca1_ajtzr.jan-dec_dmeans.1979-2008.precip.reform_fourd.nc',$
               ga+'/aliur/hadgem3a_amip_n96_ga40_aliur.jan-dec_dmeans.1982-2008.precip.nc',$
               ga+'/aljyr/hadgem3ao_orca1_n96_ga40_aljyr.jan-dec_dmeans.1982-2008.precip.nc',$
               gc2+'/antia/hadgem3_ga6_n96.jan-dec_dmeans.years1-27.precip.nc',$
               gc2+'/antib/hadgem3_ga6_n216.jan-dec_dmeans.years1-27.precip.nc',$
               gc2+'/anqjm/hadgem3_gc2_n96_orca025.jan-dec_dmeans.years1-41.precip.nc',$
               gc2+'/anqjn/hadgem3_gc2_n216.jan-dec_dmeans.years1-60.precip.nc']

imd_file='/home/ss901165/datasets/IMD_GRIDDED/imd_1x1v2_1951-2004.mjjas.nc'
descs=['AO-N96',$
       'AO-N144',$
       'A-N96',$
       'AO-N96',$
       'A-N96',$
       'AO-N96',$
       'A-N96',$
       'AO-N96',$
       'A-N96',$
       'AO-N96',$
       'A-N96',$
       'A-N216',$
       'AO-N96',$
       'AO-N216']       

all_nyears=[30,50,20,30,27,30,27,29,27,27,27,27,41,60]
offset_may1=[120,120,120,120,120,120,120,120,120,120,120,120,120,120]
offset=[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
n_time=150
varname='precip'
multiplier=86400.
all_colors=['tomato','orange','violetred','magenta','brown','indianred','darkgray','slategray','firebrick','red','blue','purple','dodgerblue','violet']
n96_mask_file=['/home/ss901165/um_output/mask_n96_hadgem3-7.1.nc',$
               '/home/ss901165/um_output/mask_n144_higam.nc',$
               '/home/ss901165/um_output/mask_n96_hadgem3-7.1.nc',$
               '/home/ss901165/um_output/mask_n96_hadgem3-7.1.nc',$
               '/home/ss901165/um_output/mask_n96_hadgem3-7.1.nc',$
               '/home/ss901165/um_output/mask_n96_hadgem3-7.1.nc',$
               '/home/ss901165/um_output/mask_n96_hadgem3-7.1.nc',$
               '/home/ss901165/um_output/mask_n96_hadgem3-7.1.nc',$
               '/home/ss901165/um_output/mask_n96_hadgem3-7.1.nc',$
               '/home/ss901165/um_output/mask_n96_hadgem3-7.1.nc',$
               '/home/ss901165/um_output/mask_n96_hadgem3-7.1.nc',$
               '/home/ss901165/um_output/mask_n216_nugam.nc',$
               '/home/ss901165/um_output/mask_n96_hadgem3-7.1.nc',$
               '/home/ss901165/um_output/mask_n216_nugam.nc']
               
; Box to area average
box=[-10,50,10,100]
box_name='EqIO'

max_years=60
n_sets=N_ELEMENTS(dmean_infiles)
all_clims=fltarr(n_sets)
all_daily_clims=fltarr(n_sets,n_time+60)
hadgem3_jjas_ts=fltarr(n_sets,max_years)

;imd_longitude=OPEN_AND_EXTRACT(imd_file,'longitude')
;imd_latitude=OPEN_AND_EXTRACT(imd_file,'latitude')
;DEFINE_BOUNDARIES,box,imd_latitude,imd_longitude,box_tx,/LIMIT
;imd=OPEN_AND_EXTRACT(imd_file,'rf',$
;                     offset=[box_tx(1),box_tx(0),0,0],$
;                     count=[N_ELEMENTS(imd_longitude),N_ELEMENTS(imd_latitude),150,54])
;temp=imd(*,*,30:149,*)
;imd_aavg_clim=MEAN(temp[where(temp lt 1e10)])
;imd_daily_clim=fltarr(150)
;FOR i=0,149 DO BEGIN
;   temp=imd(*,*,i,*)
;   temp[where(temp gt 1e10)]=!Values.F_NaN
;   imd_daily_clim(i)=MEAN(temp,/NaN)
;ENDFOR
;imd_smean_clim=fltarr(54)
;FOR i=0,53 DO BEGIN
;   temp=imd(*,*,30:149,i)
;   temp[where(temp gt 1e10)]=!Values.F_NaN
;   imd_smean_clim(i)=MEAN(temp,/NaN)
;ENDFOR

trmm_file='/home/ss901165/datasets/TRMM_3B42V6/n96/TRMM_3B42v6A.jan-dec_dmeans.1999-2011.n96.nc'
trmm_longitude=OPEN_AND_EXTRACT(trmm_file,'longitude')
trmm_latitude=OPEN_AND_EXTRACT(trmm_file,'latitude')
DEFINE_BOUNDARIES,box,trmm_latitude,trmm_longitude,trmm_box_tx,/LIMIT
trmm=OPEN_AND_EXTRACT(trmm_file,'precip',$
                      offset=[trmm_box_tx(1),trmm_box_tx(0),120,0],$
                      count=[N_ELEMENTS(trmm_longitude),N_ELEMENTS(trmm_latitude),150,13])
trmm_smeans=fltarr(13)
FOR i=0,12 DO BEGIN
   temp=trmm(*,*,30:149,i)
   IF TOTAL(where(temp gt 1000)) ge 0 THEN $
      temp[where(temp gt 1000)]=!Values.F_NaN
   trmm_smeans(i)=MEAN(temp,/NaN)
ENDFOR

FOR k=0,n_sets-1 DO BEGIN   
   print,k
   longitude=OPEN_AND_EXTRACT(dmean_infiles(k),'longitude')
   latitude=OPEN_AND_EXTRACT(dmean_infiles(k),'latitude')
   DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
   nlon=N_ELEMENTS(longitude)
   nlat=N_ELEMENTS(latitude)
                                   ; Get mask grid information
   mask_longitude=OPEN_AND_EXTRACT(n96_mask_file(k),'longitude')
   mask_latitude=OPEN_AND_EXTRACT(n96_mask_file(k),'latitude')
   DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
   mask_nlon=N_ELEMENTS(mask_longitude)
   mask_nlat=N_ELEMENTS(mask_latitude)
   
   n96_mask=REFORM(OPEN_AND_EXTRACT(n96_mask_file(k),'lsm',offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                                    count=[mask_nlon,mask_nlat,1,1]))
;   n96_mask_rev=fltarr(mask_nlon,mask_nlat)
;   FOR i=0,nlon-1 DO $
;      FOR j=0,nlat-1 DO $
;         n96_mask_rev(i,nlat-j-1)=n96_mask(i,j)
      
   FOR i=0,all_nyears(k)-1 DO BEGIN
      hadgem3_year=REFORM(OPEN_AND_EXTRACT(dmean_infiles(k),varname,$
                                           offset=[box_tx(1),box_tx(0),offset_may1(k)-30,i],$
                                           count=[nlon,nlat,n_time+60,1]))*multiplier
      hadgem3_year_aavg=fltarr(n_time+30)
      FOR j=0,n_time+59 DO BEGIN
         temp_precip=REFORM(hadgem3_year(*,*,j))
         IF TOTAL(where(n96_mask ne 0) ge 0) THEN $
            temp_precip[where(n96_mask ne 0)]=!Values.F_NaN
         hadgem3_year(*,*,j)=temp_precip
         all_daily_clims(k,j)=MEAN(temp_precip,/NaN)*1./FLOAT(all_nyears(k))+all_daily_clims(k,j)
      ENDFOR
      hadgem3_jjas_ts(k,i)=MEAN(hadgem3_year(*,*,60:n_time+29),/NaN)
   ENDFOR
   IF all_nyears(k) lt max_years THEN $
      hadgem3_jjas_ts(k,all_nyears(k):max_years-1)=!Values.F_NaN
   all_clims(k)=MEAN(hadgem3_jjas_ts(k,*),/NaN)
ENDFOR

; Build plot
psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_ts_many_gagc.jjas_air_ts.'+box_name+'.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=140,MARGIN=2000,SPACE2=300,XOFFSET=800,YOFFSET=500,TFONT=6,TCHARSIZE=100,SPACE3=200
ymin=1.0
ymax=7.5
GSET,XMIN=2,XMAX=max_years,YMIN=ymin,YMAX=ymax,TITLE='Five-year running mean of JJAS precip over India (10-30N, 70-90E, land only)'
FOR i=0,n_sets-1 DO BEGIN
   toplot=REFORM(SMOOTH(hadgem3_jjas_ts(i,0:all_nyears(i)-1),5))
   GPLOT,X=indgen(max_years-4)+3+(i-n_sets/2)*0.05+offset(i),Y=toplot(2:all_nyears(i)-3),THICK=250,COL=FSC_COLOR(all_colors(i))
;   range=fltarr(2,all_nyears(i)-4)
;   FOR j=2+ODD(i),all_nyears(i)-3,2 DO $
;      EBAR,X=j+1+(i-n_sets/2)*0.05,Y=toplot(j),ERROR_Y=[toplot(j)-MIN(hadgem3_jjas_ts(i,j-2:j+2))*a(i),MAX(hadgem3_jjas_ts(i,j-2:j+2))*a(i)-toplot(j)],$
;           THICK=100,WIDTH=50,COL=FSC_COLOR(all_colors(i))
;      range(0,j-2)=MIN(hadgem3_jjas_ts(i,j-2:j+2))
;      range(1,j-2)=MAX(hadgem3_jjas_ts(i,j-2:j+2))
;   ENDFOR
;   GPLOT,X=indgen(max_years-5)+2.5,Y=REFORM(range(0,*)),THICK=100,COL=FSC_COLOR(all_colors(i)),STYLE=2
;   GPLOT,X=indgen(max_years-5)+2.5,Y=REFORM(range(1,*)),THICK=100,COL=FSC_COLOR(all_colors(i)),STYLE=2
ENDFOR
FOR i=0,n_sets-1 DO $
   GPLOT,X=max_years+2.5,Y=all_clims(i),COL=FSC_COLOR(all_colors(i)),SYM=3,SIZE=100
;GPLOT,X=max_years+2.5,Y=imd_aavg_clim,COL=FSC_COLOR('black'),SYM=3
;GPLOT,X=[2,max_years],Y=[imd_aavg_clim,imd_aavg_clim],STYLE=2

AXES,XSTEP=4,XMINOR=1,$
     YSTEP=0.5,YMINOR=0.25,XTITLE='Year',YTITLE='Area-averaged precipitation over Indian land (mm day!U-1!N)',NDECS=1
GLEGEND,labels=REVERSE([descs,'IMD']),col=REVERSE([FSC_COLOR(all_colors),FSC_COLOR('black')]),LEGPOS=11,SIZE=80,LENGTH=50,THICK=REPLICATE(200,n_sets+1)
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_ts_many_gagc.jjas_air_sdev.'+box_name+'.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=130,MARGIN=1800,SPACE2=0,XOFFSET=800,YOFFSET=500,TFONT=6,TCHARSIZE=100,SPACE3=0
ymin=0
ymax=10
GSET,XMIN=0,XMAX=n_sets+0.5,YMIN=ymin,YMAX=ymax,TITLE='Mean (dots) and [Min,25,Med,75,Max] of JJAS precip over Eq.Ind.Ocn. (10S-10N, 50-100E, ocean only)'
;sorted=SORT(imd_smean_clim)
;EBAR,X=0.5,BOX=[imd_smean_clim(sorted(0)),imd_smean_clim(sorted(54/4)),imd_smean_clim(sorted(54/2)),$
;                imd_smean_clim(sorted(54*3/4)),imd_smean_clim(sorted(54-1))],$
;     WIDTH=175,COL=FSC_COLOR('black'),THICK=250
sorted=SORT(trmm_smeans)
EBAR,X=0.5,BOX=[trmm_smeans(sorted(0)),trmm_smeans(sorted(13/4)),trmm_smeans(sorted(13/2)),$
                trmm_smeans(sorted(13*3/4)),trmm_smeans(sorted(13-1))],$
     WIDTH=175,COL=FSC_COLOR('black'),THICK=250
GPLOT,X=0.5,Y=MEAN(trmm_smeans),SYM=3,SIZE=100
xpos=[0.5,1.6,2.4,3.6,4.4,5.6,6.4,7.6,8.4,9.6,10.4,11.8,12.5,13.2,13.9]
FOR i=0,n_sets-1 DO BEGIN
   temp=REFORM(hadgem3_jjas_ts(i,0:all_nyears(i)-1))
   sorted=SORT(temp)
   EBAR,X=xpos(i+1),BOX=[temp(sorted(0)),temp(sorted(all_nyears(i)/4)),temp(sorted(all_nyears(i)/2+1)),temp(sorted(all_nyears(i)*3/4)),temp(sorted(all_nyears(i)-1))],$
        WIDTH=175,COL=FSC_COLOR(all_colors(i)),THICK=250
   GPLOT,X=xpos(i+1),Y=all_clims(i),COL=FSC_COLOR(all_colors(i)),SYM=3,SIZE=100
ENDFOR
GPLOT,X=[1,1],Y=[ymin,ymax],STYLE=1
GPLOT,X=[3,3],Y=[ymin,ymax],STYLE=1
GPLOT,X=[5,5],Y=[ymin,ymax],STYLE=1
GPLOT,X=[7,7],Y=[ymin,ymax],STYLE=1
GPLOT,X=[9,9],Y=[ymin,ymax],STYLE=1
GPLOT,X=[11,11],Y=[ymin,ymax],STYLE=1
GPLOT,X=0.5,Y=ymax*0.95,TEXT='Obs'
GPLOT,X=2,Y=ymax*0.95,TEXT='HadGEM1'
GPLOT,X=4,Y=ymax*0.95,TEXT='HadGEM2'
GPLOT,X=6,Y=ymax*0.95,TEXT='MORPH3'
GPLOT,X=8,Y=ymax*0.95,TEXT='GA3'
GPLOT,X=10,Y=ymax*0.95,TEXT='GA4'
GPLOT,X=12.8,Y=ymax*0.95,TEXT='GA6/GC2'
AXES,XVALS=xpos,XLABELS=['TRMM',descs],ORIENTATION=30,YTITLE='Precipitation (mm day!U-1!N)',YSTEP=1,YMINOR=0.5,/NOUPPER
PSCLOSE       

; Build plot
psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_ts_many_gagc.jjas_air_clim.'+box_name+'.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=140,MARGIN=2000,SPACE2=300,XOFFSET=800,YOFFSET=500,TFONT=6,TCHARSIZE=100,SPACE3=300
ymin=0
ymax=12
GSET,XMIN=0,XMAX=n_time,YMIN=ymin,YMAX=ymax,TITLE='Timeseries of JJAS area-avg precip over India (10-30N, 70-90E, land only)'
FOR i=0,n_sets-1 DO BEGIN
   toplot=SMOOTH(REFORM(all_daily_clims(i,*)),11)
   GPLOT,X=indgen(n_time)+0.5,Y=toplot(30:n_time+29),COL=FSC_COLOR(all_colors(i)),THICK=125
ENDFOR
GPLOT,X=indgen(n_time)+0.5,Y=SMOOTH(imd_daily_clim,5),COL=FSC_COLOR('black'),THICK=175
AXES,YSTEP=1,YMINOR=0.5,XVALS=[0,15,30,45,60,75,90,105,120,135,149]+0.5,XTITLE='Day',$
     XLABELS=['1/5','15/5','1/6','15/6','1/7','15/7','1/8','15/8','1/9','15/9','30/9'],YTITLE='Area-averaged precipitation over Indian land (mm day!U-1!N)'
GLEGEND,labels=REVERSE([descs,'IMD']),col=REVERSE([FSC_COLOR(all_colors),FSC_COLOR('black')]),LEGPOS=1,SIZE=80
PSCLOSE,/NOVIEW
   
STOP

END
