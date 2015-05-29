PRO hadgem3_monwg_jjas_precip_atmos_minus_coupled_plumes

hadgem3a_mmean_file='/home/ss901165/um_output2/hadgem3_monwg/ahrqc/ahrqc.precip.apr-oct.mmeans_20years.nc'
hadgem3ao_clim_mmean_file='/home/ss901165/um_output2/hadgem3_monwg/ahsaf/ahsaf.precip.clim.mmeans_30years.nc'

hadgem3a_offset_jun=2
hadgem3a_offset_sep=5
hadgem3a_ntime=hadgem3a_offset_sep-hadgem3a_offset_jun+1
hadgem3a_nyears=20

hadgem3ao_offset_jun=5
hadgem3ao_offset_sep=8
hadgem3ao_ntime=hadgem3ao_offset_sep-hadgem3ao_offset_jun+1

mask_file='/home/ss901165/um_output/mask_n96.nc'

; Box for area-averaging
box=[10,70,30,90]

; Get grid information
hadgem3a_longitude=OPEN_AND_EXTRACT(hadgem3a_mmean_file,'longitude')
hadgem3a_latitude=OPEN_AND_EXTRACT(hadgem3a_mmean_file,'latitude')
DEFINE_BOUNDARIES,box,hadgem3a_latitude,hadgem3a_longitude,hadgem3a_box_tx,/LIMIT
hadgem3a_nlon=N_ELEMENTS(hadgem3a_longitude)
hadgem3a_nlat=N_ELEMENTS(hadgem3a_latitude)

hadgem3ao_longitude=OPEN_AND_EXTRACT(hadgem3ao_clim_mmean_file,'longitude')
hadgem3ao_latitude=OPEN_AND_EXTRACT(hadgem3ao_clim_mmean_file,'latitude')
DEFINE_BOUNDARIES,box,hadgem3ao_latitude,hadgem3ao_longitude,hadgem3ao_box_tx,/LIMIT
hadgem3ao_nlon=N_ELEMENTS(hadgem3ao_longitude)
hadgem3ao_nlat=N_ELEMENTS(hadgem3ao_latitude)

mask_longitude=OPEN_AND_EXTRACT(mask_file,'longitude')
mask_latitude=OPEN_AND_EXTRACT(mask_file,'latitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)
mask=REFORM(OPEN_AND_EXTRACT(mask_file,'lsm',offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                             count=[mask_nlon,mask_nlat,1,1]))
mask_rev=fltarr(mask_nlon,mask_nlat)
FOR i=0,mask_nlon-1 DO $
  FOR j=0,mask_nlat-1 DO $
  mask_rev(i,j)=mask(i,mask_nlat-j-1)
land=where(mask_rev eq 1)

; Get monthly mean rainfall for all years for HadGEM3-A
hadgem3a_precip=REFORM(OPEN_AND_EXTRACT(hadgem3a_mmean_file,'precip',$
                                        offset=[hadgem3a_box_tx(1),hadgem3a_box_tx(0),0,hadgem3a_offset_jun,0],$
                                        count=[hadgem3a_nlon,hadgem3a_nlat,1,hadgem3a_ntime,hadgem3a_nyears]))*86400.
; Get climatological monthly mean rainfall for HadGEM3-AO
hadgem3ao_precip=REFORM(OPEN_AND_EXTRACT(hadgem3ao_clim_mmean_file,'precip',$
                                         offset=[hadgem3ao_box_tx(1),hadgem3ao_box_tx(0),0,hadgem3ao_offset_jun],$
                                         count=[hadgem3ao_nlon,hadgem3ao_nlat,1,hadgem3ao_ntime]))*86400.

; Average over all months
hadgem3a_precip_timemean=fltarr(hadgem3a_nlon,hadgem3a_nlat,hadgem3a_nyears)
FOR i=0,hadgem3a_nlon-1 DO $
  FOR j=0,hadgem3a_nlat-1 DO $
  FOR k=0,hadgem3a_nyears-1 DO $
  hadgem3a_precip_timemean(i,j,k)=MEAN(hadgem3a_precip(i,j,*,k))
hadgem3ao_precip_timemean=fltarr(hadgem3ao_nlon,hadgem3ao_nlat)
FOR i=0,hadgem3ao_nlon-1 DO $
  FOR j=0,hadgem3ao_nlat-1 DO $
  hadgem3ao_precip_timemean(i,j)=MEAN(hadgem3ao_precip(i,j,*))

hadgem3a_precip_timemean[where(hadgem3a_precip_timemean ge 25)]=!Values.F_NaN

; Take difference in area average and difference at every gridpoint
atmos_minus_coupled_allpoints=fltarr(hadgem3a_nlon,hadgem3a_nlat,hadgem3a_nyears)
atmos_minus_coupled_aavg=fltarr(hadgem3a_nyears)
atmos_aavg=fltarr(hadgem3a_nyears)
coupled_land=hadgem3ao_precip_timemean[land]
coupled_aavg=MEAN(coupled_land,/NaN)
FOR i=0,hadgem3a_nyears-1 DO BEGIN
    temp_atmos=REFORM(hadgem3a_precip_timemean(*,*,i))
    temp_atmos_land=temp_atmos[land] 
    atmos_aavg(i)=MEAN(temp_atmos_land,/NaN)
    atmos_minus_coupled_aavg(i)=atmos_aavg(i)/coupled_aavg*100
    FOR j=0,hadgem3a_nlon-1 DO BEGIN
        FOR k=0,hadgem3a_nlat-1 DO BEGIN
            IF mask_rev(j,k) eq 1 THEN $
              atmos_minus_coupled_allpoints(j,k,i)=hadgem3a_precip_timemean(j,k,i)/$
              hadgem3ao_precip_timemean(j,k)*100
        ENDFOR
    ENDFOR    
ENDFOR
atmos_minus_coupled_allpoints[where(atmos_minus_coupled_allpoints eq 0)]=!Values.F_NaN

; Build plot
psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_atmos_minus_coupled_plumes.ahrqc_minus_ahsaf.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=120
xvalues=indgen(hadgem3a_nyears/4)*4
xlabels=['1979','1983','1987','1991','1995']
yvalues=[20,25,33,50,80,100,125,200,300,400,500]
ymin=20
ymax=500
GSET,XMIN=-0.5,XMAX=hadgem3a_nyears-0.5,YMIN=ymin,YMAX=ymax,/YLOG

x_points=11
mylevs=['0.05','0.1','0.15','0.2','0.25','0.3','0.35','0.4','0.45','0.5','0.55','0.6','0.65','0.7','0.75']
CS,IDL=3,NCOLS=N_ELEMENTS(mylevs)+1,/REV
LEVS,MANUAL=mylevs
FOR i=0,hadgem3a_nyears-1 DO BEGIN
    temp_grid=REFORM(atmos_minus_coupled_allpoints(*,*,i),[hadgem3a_nlon*hadgem3a_nlat])
    temp_grid=temp_grid[where(FINITE(temp_grid) eq 1)]
    pdf,temp_grid,/BANDWIDTH,NPDF=60,xid=xaxis,xrange=[0,500],pdf=today_pdf,/NOPLOT
    twod_pdf=fltarr(x_points,N_ELEMENTS(xaxis))
    my_xaxis=indgen(x_points)/FLOAT(x_points-1)+i-0.45
    my_yaxis=xaxis
    FOR j=0,x_points-1 DO $
      twod_pdf(j,*)=today_pdf
    IF i eq 0 THEN BEGIN
        CON,FIELD=twod_pdf,X=my_xaxis,Y=my_yaxis,/NOLINES,/CB_RIGHT,TITLE='JJAS-mean rain for aavg 10-30N, 70-90E (land only) and PDF for all land points - HadGEM3_ahrqc_n96'
    ENDIF ELSE $
      CON,FIELD=twod_pdf,X=my_xaxis,Y=my_yaxis,/NOLINES,/NOCOLBAR
ENDFOR

xminor=[indgen(hadgem3a_nyears)]
AXES,XVALS=xvalues,XLABELS=xlabels,YVALS=yvalues,YLABELS=ylabels,NDECS=1,ytitle='% of mean rainfall from HadGEM3-AO',xtitle='Year',$
  XMINOR=xminor
red=FSC_COLOR("cyan",30)
GPLOT,Y=atmos_minus_coupled_aavg,X=indgen(hadgem3a_nyears),COL=30,THICK=200

PSCLOSE

STOP

END
