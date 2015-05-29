PRO hadgem3_monwg_jjas_sst_plumes_pdf

hadgem3_clim_file='/home/ss901165/um_output2/hadgem3_monwg/ahsaf/ahsaf.surftemp.clim.daily_20years.nc'
hadgem3_daily_file='/home/ss901165/um_output2/hadgem3_monwg/ahsaf/ahsaf.surftemp.daily_20years.nc'
tmi_clim_file='/home/ss901165/datasets/TMI_AMSRE/tmi.fusion.1998-2008.amjjaso_clim.monsoon_domain.n96.nc'

n96_mask_file='/home/ss901165/um_output/mask_n96.nc'

offset_apr1=90
offset_sep30=269
n_time=offset_sep30-offset_apr1+1

hadgem3_nyears=20

; Box to area average
box=[10,80,25,100]
box_name='BoB'

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
hadgem3_clim=REFORM(OPEN_AND_EXTRACT(hadgem3_clim_file,'temp',$
                              offset=[hadgem3_box_tx(1),hadgem3_box_tx(0),0,offset_apr1],$
                              count=[hadgem3_nlon,hadgem3_nlat,1,n_time]))-273.15

hadgem3_clim_aavg=fltarr(n_time)
FOR i=0,n_time-1 DO BEGIN
    temp_sst=REFORM(hadgem3_clim(*,*,i))
    temp_sst[where(n96_mask_rev eq 1)] = !Values.F_NaN
    hadgem3_clim_aavg(i)=MEAN(temp_sst,/NaN)
ENDFOR

; Build plot
psfile='/home/ss901165/idl/hadgem3_monwg/jjas_sst/hadgem3_monwg_jjas_sst_plumes_pdf.hadgem3_ahsaf_n96.'+box_name+'.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=1200,YOFFSET=500,TFONT=2,TCHARSIZE=120
xvalues=indgen(n_time/15+1)*15
xlabels=['1 Apr','16 Apr','1 May','16 May','1 Jun','16 Jun','1 Jul','16 Jul','1 Aug','16 Aug','1 Sep','16 Sep','30 Sep']
yvalues=['25.0','25.5','26.0','26.5','27.0','27.5','28.0',$
         '28.5','29.0','29.5','30.0','30.5','31.0','31.5','32.0']
ymin=25
ymax=32
GSET,XMIN=-0.5,XMAX=n_time,YMIN=ymin,YMAX=ymax
hadgem3_year_aavg=fltarr(n_time)
hadgem3_allyears_aavg=fltarr(hadgem3_nyears,n_time)
FOR i=0,hadgem3_nyears-1 DO BEGIN
    hadgem3_year=REFORM(OPEN_AND_EXTRACT(hadgem3_daily_file,'temp',$
                                         offset=[hadgem3_box_tx(1),hadgem3_box_tx(0),0,offset_apr1,i],$
                                         count=[hadgem3_nlon,hadgem3_nlat,1,n_time,1]))-273.15
    FOR j=0,n_time-1 DO BEGIN
        temp_sst=REFORM(hadgem3_year(*,*,j))
        temp_sst[where(n96_mask_rev eq 1)] = !Values.F_NaN
        hadgem3_year_aavg(j)=MEAN(temp_sst,/NaN)
    ENDFOR
    hadgem3_allyears_aavg(i,*)=hadgem3_year_aavg
    IF TOTAL(where(hadgem3_year_aavg lt ymin)) ne -1 THEN $
      hadgem3_year_aavg[where(hadgem3_year_aavg lt ymin)]=!Values.F_NaN    
    ;GPLOT,Y=hadgem3_year_aavg,X=indgen(n_time),THICK=100,COL=30,STYLE=2    
ENDFOR

x_points=11
mylevs=['0.002','0.004','0.006','0.008','0.010','0.012','0.014','0.016','0.018','0.020','0.022','0.024','0.026','0.028','0.030']
CS,IDL=3,NCOLS=N_ELEMENTS(mylevs)+1,/REV
LEVS,MANUAL=mylevs
FOR i=0,n_time-1 DO BEGIN
    ;print,'Now computing PDFs for day = '+STRTRIM(STRING(i+1),1)+'...'
    temp_sst=REFORM(hadgem3_allyears_aavg(*,i))
    pdf,temp_sst,/BANDWIDTH,NPDF=60,xid=xaxis,xrange=[25,32],pdf=today_pdf,/NOPLOT
    twod_pdf=fltarr(x_points,N_ELEMENTS(xaxis))
    my_xaxis=indgen(x_points)/FLOAT(x_points-1)+i-0.45
    my_yaxis=indgen(61)*0.1166666+25
    FOR j=0,x_points-1 DO $
      twod_pdf(j,*)=today_pdf
    IF i eq 0 THEN BEGIN
        CON,FIELD=twod_pdf,X=my_xaxis,Y=my_yaxis,/NOLINES,/CB_RIGHT,TITLE='Mean and PDF of SST aavg (10-25N, 80-100E ocean only) for HadGEM3-AO_ahsaf_n96'
    ENDIF ELSE $
      CON,FIELD=twod_pdf,X=my_xaxis,Y=my_yaxis,/NOLINES,/NOCOLBAR
ENDFOR

AXES,XVALS=xvalues,XLABELS=xlabels,YVALS=yvalues,YLABELS=ylabels,NDECS=1,ytitle='SST (degrees C)',xtitle='Day'
IF TOTAL(where(hadgem3_clim_aavg lt ymin)) ne -1 THEN $
  hadgem3_clim_aavg[where(hadgem3_clim_aavg lt ymin)]=!Values.F_NaN
red=FSC_COLOR("cyan",30)
GPLOT,Y=hadgem3_clim_aavg,X=indgen(n_time),THICK=200,COL=30

; Get TMI data
tmi_longitude=OPEN_AND_EXTRACT(tmi_clim_file,'longitude')
tmi_latitude=OPEN_AND_EXTRACT(tmi_clim_file,'latitude')
DEFINE_BOUNDARIES,box,tmi_latitude,tmi_longitude,tmi_box_tx,/LIMIT
tmi_nlon=N_ELEMENTS(tmi_longitude)
tmi_nlat=N_ELEMENTS(tmi_latitude)
tmi_ntime=182
tmi_offset=0
tmi_clim=REFORM(OPEN_AND_EXTRACT(tmi_clim_file,'sst',$
                                 offset=[tmi_box_tx(1),tmi_box_tx(0),tmi_offset],$
                                 count=[tmi_nlon,tmi_nlat,tmi_ntime]))
tmi_clim_aavg=fltarr(tmi_ntime)
FOR i=0,tmi_ntime-1 DO BEGIN
    temp=REFORM(tmi_clim(*,*,i))
    temp[where(n96_mask eq 1)] = !Values.F_NaN
    tmi_clim_aavg(i)=MEAN(temp,/NaN)
ENDFOR

black=FSC_COLOR("black",31)
GPLOT,Y=tmi_clim_aavg,X=indgen(tmi_ntime)*n_time/FLOAT(tmi_ntime-1),THICK=200,COL=31
labels=['HadGEM3-AO (20 years)',$
        'TMI climatology (1998-2008)']
colors=[30,31]
LEGEND,LABELS=labels,COL=colors,LEGPOS=9

PSCLOSE

STOP

END




