PRO hadgem3ao_monwg_jjas_precip_plumes

hadgem3_clim_file='/home/ss901165/um_output2/hadgem3_monwg/ahsaf/ahsaf.precip.clim.daily_30years.nc'
hadgem3_daily_file='/home/ss901165/um_output2/hadgem3_monwg/ahsaf/ahsaf.precip.daily_30years.nc'
imd_clim_file='/home/ss901165/datasets/IMD_GRIDDED/imd_1x1v2_1951-2004_mjjas_daily_clim.nc'

n96_mask_file='/home/ss901165/um_output/mask_n96.nc'

offset_may1=120
offset_sep30=269
n_time=offset_sep30-offset_may1+1

hadgem3_nyears=30

; Box to area average
box=[10,70,30,87]

; Get grid information
hadgem3_longitude=OPEN_AND_EXTRACT(hadgem3_clim_file,'longitude')
hadgem3_latitude=OPEN_AND_EXTRACT(hadgem3_clim_file,'latitude')
DEFINE_BOUNDARIES,box,hadgem3_latitude,hadgem3_longitude,hadgem3_box_tx,/LIMIT
hadgem3_nlon=N_ELEMENTS(hadgem3_longitude)
hadgem3_nlat=N_ELEMENTS(hadgem3_latitude)
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
FOR i=0,hadgem3_nlon-1 DO $
  FOR j=0,hadgem3_nlat-1 DO $
  n96_mask_rev(i,hadgem3_nlat-j-1)=n96_mask(i,j)

; Get climatological daily rainfall
hadgem3_clim=REFORM(OPEN_AND_EXTRACT(hadgem3_clim_file,'precip',$
                              offset=[hadgem3_box_tx(1),hadgem3_box_tx(0),0,offset_may1],$
                              count=[hadgem3_nlon,hadgem3_nlat,1,n_time]))*86400.

hadgem3_clim_aavg=fltarr(n_time)
FOR i=0,n_time-1 DO BEGIN
    temp_precip=REFORM(hadgem3_clim(*,*,i))
    temp_precip[where(n96_mask_rev eq 0)] = !Values.F_NaN
    hadgem3_clim_aavg(i)=MEAN(temp_precip,/NaN)
ENDFOR

; Build plot
psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_plumes.ahsaf_30years.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=120
xvalues=indgen(n_time/15+1)*15
xlabels=['1 May','16 May','1 Jun','16 Jun','1 Jul','16 Jul','1 Aug','16 Aug','1 Sep','16 Sep','30 Sep']
yvalues=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
ymin=0
ymax=15
GSET,XMIN=-0.5,XMAX=n_time,YMIN=ymin,YMAX=ymax
hadgem3_year_aavg=fltarr(n_time)
hadgem3_allyears_aavg=fltarr(hadgem3_nyears,n_time)
FOR i=0,hadgem3_nyears-1 DO BEGIN
    hadgem3_year=REFORM(OPEN_AND_EXTRACT(hadgem3_daily_file,'precip',$
                                         offset=[hadgem3_box_tx(1),hadgem3_box_tx(0),0,offset_may1,i],$
                                         count=[hadgem3_nlon,hadgem3_nlat,1,n_time,1]))*86400.
    FOR j=0,n_time-1 DO BEGIN
        temp_precip=REFORM(hadgem3_year(*,*,j))
        temp_precip[where(n96_mask_rev eq 0)] = !Values.F_NaN
        hadgem3_year_aavg(j)=MEAN(temp_precip,/NaN)
    ENDFOR
    hadgem3_allyears_aavg(i,*)=hadgem3_year_aavg
    IF TOTAL(where(hadgem3_year_aavg lt ymin)) ne -1 THEN $
      hadgem3_year_aavg[where(hadgem3_year_aavg lt ymin)]=!Values.F_NaN    
    ;GPLOT,Y=hadgem3_year_aavg,X=indgen(n_time),THICK=100,COL=30,STYLE=2    
ENDFOR

x_points=11
mylevs=['0.01','0.02','0.03','0.04','0.05','0.06','0.07','0.08','0.09','0.10','0.11','0.12','0.13','0.14','0.15']
CS,IDL=3,NCOLS=N_ELEMENTS(mylevs)+1,/REV
LEVS,MANUAL=mylevs
FOR i=0,n_time-1 DO BEGIN
    ;print,'Now computing PDFs for day = '+STRTRIM(STRING(i+1),1)+'...'
    temp_precip=REFORM(hadgem3_allyears_aavg(*,i))
    pdf,temp_precip,/BANDWIDTH,NPDF=60,xid=xaxis,xrange=[0,30],pdf=today_pdf,/NOPLOT
    twod_pdf=fltarr(x_points,N_ELEMENTS(xaxis))
    my_xaxis=indgen(x_points)/FLOAT(x_points-1)+i-0.45
    my_yaxis=indgen(61)*0.5+0.25
    FOR j=0,x_points-1 DO $
      twod_pdf(j,*)=today_pdf
    IF i eq 0 THEN BEGIN
        CON,FIELD=twod_pdf,X=my_xaxis,Y=my_yaxis,/NOLINES,/CB_RIGHT,TITLE='Mean and PDF of rainfall aavg (10-30N, 70-90E land only) for HadGEM3-AO_ahsaf_n96'
    ENDIF ELSE $
      CON,FIELD=twod_pdf,X=my_xaxis,Y=my_yaxis,/NOLINES,/NOCOLBAR
ENDFOR

AXES,XVALS=xvalues,XLABELS=xlabels,YVALS=yvalues,YLABELS=ylabels,NDECS=1,ytitle='Rainfall (mm day!U-1!N)',xtitle='Day'
IF TOTAL(where(hadgem3_clim_aavg lt ymin)) ne -1 THEN $
  hadgem3_clim_aavg[where(hadgem3_clim_aavg lt ymin)]=!Values.F_NaN
red=FSC_COLOR("cyan",30)
GPLOT,Y=hadgem3_clim_aavg,X=indgen(n_time),THICK=200,COL=30

; Get IMD data
imd_box=[10,70,30,90]
imd_longitude=OPEN_AND_EXTRACT(imd_clim_file,'longitude')
imd_latitude=OPEN_AND_EXTRACT(imd_clim_file,'latitude')
DEFINE_BOUNDARIES,imd_box,imd_latitude,imd_longitude,imd_box_tx,/LIMIT
imd_nlon=N_ELEMENTS(imd_longitude)
imd_nlat=N_ELEMENTS(imd_latitude)
imd_ntime=152
imd_clim=REFORM(OPEN_AND_EXTRACT(imd_clim_file,'rf',$
                                 offset=[imd_box_tx(1),imd_box_tx(0),0,0],$
                                 count=[imd_nlon,imd_nlat,1,imd_ntime]))
imd_clim_aavg=fltarr(imd_ntime)
ocean=where(imd_clim gt 1000)
imd_clim[ocean]=!Values.F_NaN
FOR i=0,imd_ntime-1 DO $
  imd_clim_aavg(i)=MEAN(imd_clim(*,*,i),/NaN)

black=FSC_COLOR("black",31)
GPLOT,Y=imd_clim_aavg,X=indgen(imd_ntime)*n_time/FLOAT(imd_ntime-1),THICK=200,COL=31
labels=['HadGEM3-AO (30 years)',$
        'IMD climatology (1951-2004)']
colors=[30,31]
LEGEND,LABELS=labels,COL=colors,LEGPOS=9

PSCLOSE

STOP

END


