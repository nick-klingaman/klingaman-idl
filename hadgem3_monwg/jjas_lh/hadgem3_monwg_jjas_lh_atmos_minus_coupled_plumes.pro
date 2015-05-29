PRO hadgem3_monwg_jjas_lh_atmos_minus_coupled_plumes

hadgem3ao_lh_daily_file='/home/ss901165/um_output2/hadgem3_monwg/ahsaf/ahsaf.lh.apr-oct.daily_20years.nc'
hadgem3ao_lh_clim_file='/home/ss901165/um_output2/hadgem3_monwg/ahsaf/ahsaf.lh.apr-oct.clim.daily_20years.nc'
hadgem3a_lh_daily_file='/home/ss901165/um_output2/hadgem3_monwg/ahrqc/ahrqc.lh.apr-oct.daily_20years.nc'
hadgem3a_lh_clim_file='/home/ss901165/um_output2/hadgem3_monwg/ahrqc/ahrqc.lh.apr-oct.clim.daily_20years.nc'
era40_lh_clim_file='/home/ss901165/datasets/ERA40/SLHF/SLHF.mjjas_clim.1958-2007.monsoon_domain.nc'

n96_mask_file='/home/ss901165/um_output/mask_n96.nc'
era40_mask_file='/home/ss901165/um_output2/era40_lsm.nc'

hadgem3a_offset_apr1=0
hadgem3a_offset_oct30=209
hadgem3a_ntime=hadgem3a_offset_oct30-hadgem3a_offset_apr1
hadgem3a_nyears=20

hadgem3ao_offset_apr1=0
hadgem3ao_offset_oct30=209
hadgem3ao_ntime=hadgem3ao_offset_oct30-hadgem3ao_offset_apr1
hadgem3ao_nyears=20

era40_offset_may1=0
era40_offset_sep30=151
era40_ntime=era40_offset_sep30-era40_offset_may1

; Box to area average
box=[5,50,20,70]
box_name='SomJet'

hadgem3ao_longitude=OPEN_AND_EXTRACT(hadgem3ao_lh_clim_file,'longitude')
hadgem3ao_latitude=OPEN_AND_EXTRACT(hadgem3ao_lh_clim_file,'latitude')
DEFINE_BOUNDARIES,box,hadgem3ao_latitude,hadgem3ao_longitude,hadgem3ao_box_tx,/LIMIT
hadgem3ao_nlon=N_ELEMENTS(hadgem3ao_longitude)
hadgem3ao_nlat=N_ELEMENTS(hadgem3ao_latitude)

hadgem3a_longitude=OPEN_AND_EXTRACT(hadgem3a_lh_clim_file,'longitude')
hadgem3a_latitude=OPEN_AND_EXTRACT(hadgem3a_lh_clim_file,'latitude')
DEFINE_BOUNDARIES,box,hadgem3a_latitude,hadgem3a_longitude,hadgem3a_box_tx,/LIMIT
hadgem3a_nlon=N_ELEMENTS(hadgem3a_longitude)
hadgem3a_nlat=N_ELEMENTS(hadgem3a_latitude)

era40_longitude=OPEN_AND_EXTRACT(era40_lh_clim_file,'longitude')
era40_latitude=OPEN_AND_EXTRACT(era40_lh_clim_file,'latitude')
DEFINE_BOUNDARIES,box,era40_latitude,era40_longitude,era40_box_tx,/LIMIT
era40_nlon=N_ELEMENTS(era40_longitude)
era40_nlat=N_ELEMENTS(era40_latitude)

mask_longitude=OPEN_AND_EXTRACT(n96_mask_file,'longitude')
mask_latitude=OPEN_AND_EXTRACT(n96_mask_file,'latitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)

era40_mask_longitude=OPEN_AND_EXTRACT(era40_mask_file,'longitude')
era40_mask_latitude=OPEN_AND_EXTRACT(era40_mask_file,'latitude')
DEFINE_BOUNDARIES,box,era40_mask_latitude,era40_mask_longitude,era40_mask_box_tx,/LIMIT
era40_mask_nlon=N_ELEMENTS(era40_mask_longitude)
era40_mask_nlat=N_ELEMENTS(era40_mask_latitude)

; Get mask
n96_mask=REFORM(OPEN_AND_EXTRACT(n96_mask_file,'lsm',offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                                 count=[mask_nlon,mask_nlat,1,1]))
n96_mask_rev=fltarr(mask_nlon,mask_nlat)
FOR i=0,mask_nlon-1 DO $
  FOR j=0,mask_nlat-1 DO $
  n96_mask_rev(i,j)=n96_mask(i,mask_nlat-j-1)
ocean=where(n96_mask_rev eq 0)

era40_mask=REFORM(OPEN_AND_EXTRACT(era40_mask_file,'LSM',offset=[era40_mask_box_tx(1),era40_mask_box_tx(0)],$
                                   count=[era40_mask_nlon,era40_mask_nlon]))
era40_ocean=where(era40_mask eq 0)

; Get climatological latent heat flux for coupled and atmos-only
hadgem3ao_clim=REFORM(OPEN_AND_EXTRACT(hadgem3ao_lh_clim_file,'lh',$
                                       offset=[hadgem3ao_box_tx(1),hadgem3ao_box_tx(0),0,hadgem3ao_offset_apr1],$
                                       count=[hadgem3ao_nlon,hadgem3ao_nlat,1,hadgem3ao_ntime]))
hadgem3a_clim=REFORM(OPEN_AND_EXTRACT(hadgem3a_lh_clim_file,'lh',$
                                      offset=[hadgem3a_box_tx(1),hadgem3a_box_tx(0),0,hadgem3a_offset_apr1],$
                                      count=[hadgem3a_nlon,hadgem3a_nlat,1,hadgem3a_ntime]))
era40_clim=REFORM(OPEN_AND_EXTRACT(era40_lh_clim_file,'SLHF',$
                                   offset=[era40_box_tx(1),era40_box_tx(0),era40_offset_may1],$
                                   count=[era40_nlon,era40_nlat,era40_ntime]))/86400.

diff_clim=hadgem3a_clim-hadgem3ao_clim
diff_clim_aavg=fltarr(hadgem3a_ntime)
atmos_aavg=fltarr(hadgem3a_ntime)
coupled_aavg=fltarr(hadgem3ao_ntime)
era40_aavg=fltarr(era40_ntime)
FOR i=0,hadgem3a_ntime-1 DO BEGIN
    temp=REFORM(diff_clim(*,*,i))
    temp_atmos=REFORM(hadgem3a_clim(*,*,i))
    temp_coupled=REFORM(hadgem3ao_clim(*,*,i))
    atmos_aavg(i)=MEAN(temp_atmos[ocean])
    coupled_aavg(i)=MEAN(temp_coupled[ocean])
    diff_clim_aavg(i)=MEAN(temp[ocean])
ENDFOR
FOR i=0,era40_ntime-1 DO BEGIN
    temp=REFORM(era40_clim(*,*,i))
    era40_aavg(i)=MEAN(-temp[era40_ocean])
ENDFOR

diff_indyears_aavg=fltarr(hadgem3a_nyears,hadgem3a_ntime)
FOR i=0,hadgem3a_nyears-1 DO BEGIN
    ;hadgem3ao_year=REFORM(OPEN_AND_EXTRACT(hadgem3ao_lh_daily_file,'lh',$
    ;                                       offset=[hadgem3ao_box_tx(1),hadgem3ao_box_tx(0),0,hadgem3ao_offset_apr1,i],$
    ;                                       count=[hadgem3ao_nlon,hadgem3ao_nlat,1,hadgem3ao_ntime,1]))
    hadgem3a_year=REFORM(OPEN_AND_EXTRACT(hadgem3a_lh_daily_file,'lh',$
                                          offset=[hadgem3a_box_tx(1),hadgem3a_box_tx(0),0,hadgem3a_offset_apr1,i],$
                                          count=[hadgem3a_nlon,hadgem3a_nlat,1,hadgem3a_ntime,1]))
    FOR j=0,hadgem3a_ntime-1 DO BEGIN
        temp=REFORM(hadgem3a_year(*,*,j)-hadgem3ao_clim(*,*,j))
        diff_indyears_aavg(i,j)=MEAN(temp[ocean])
    ENDFOR
ENDFOR

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_lh/hadgem3_monwg_jjas_lh_atmos_minus_coupled_plumes.ahrqc_minus_ahsaf.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=350,XOFFSET=1000,YOFFSET=100,TFONT=2,TCHARSIZE=100,$
  YSPACING=1000,SPACE3=800,YPLOTS=2
xvalues=indgen(hadgem3a_ntime/30+1)*30
xminor=indgen(hadgem3a_ntime/15+2)*15
xlabels=['1 Apr','1 May','1 Jun','1 Jul','1 Aug','1 Sep','1 Oct','30 Oct']

yvalues=['-40','-20','0','20','40','60','80','100','120','140']
ymin=-40
ymax=140
npdf=180
mult=1

GSET,XMIN=-0.5,XMAX=hadgem3a_ntime+1,YMIN=ymin,YMAX=ymax
x_points=11
mylevs=['0.002','0.006','0.008','0.012','0.016','0.020','0.024','0.028']
CS,IDL=3,NCOLS=N_ELEMENTS(mylevs)+1,/REV
LEVS,MANUAL=mylevs
FOR i=0,hadgem3a_ntime-1 DO BEGIN
    temp=REFORM(diff_indyears_aavg(*,i))
    pdf,temp,/BANDWIDTH,NPDF=npdf,xid=xaxis,xrange=[ymin,ymax],pdf=today_pdf,/NOPLOT
    twod_pdf=fltarr(x_points,N_ELEMENTS(xaxis))
    my_xaxis=indgen(x_points)/FLOAT(x_points-1)+i-0.45
    my_yaxis=indgen(npdf)*mult+ymin
    FOR j=0,x_points-1 DO $
      twod_pdf(j,*)=today_pdf
    IF i eq 0 THEN BEGIN
        CON,FIELD=twod_pdf,X=my_xaxis,Y=my_yaxis,/NOLINES
    ENDIF ELSE $
      CON,FIELD=twod_pdf,X=my_xaxis,Y=my_yaxis,/NOLINES,/NOCOLBAR
ENDFOR

AXES,XVALS=xvalues,XLABELS=xlabels,YVALS=yvalues,YLABELS=ylabels,NDECS=1,$
  ytitle='Difference in latent heat flux (W m!U-2!N)',xminor=xminor
red=FSC_COLOR("cyan",30)
GPLOT,Y=diff_clim_aavg,X=indgen(hadgem3a_ntime),THICK=200,COL=30
black=FSC_COLOR("black",31)
GPLOT,Y=REPLICATE(0,hadgem3a_ntime),X=indgen(hadgem3a_ntime),STYLE=1,THICK=200,COL=31

POS,ypos=2
ymin=80
ymax=220
yvalues=[80,100,120,140,160,180,200,220]
GSET,XMIN=-0.5,XMAX=hadgem3a_ntime,YMIN=ymin,YMAX=ymax,TITLE='Mean and PDF of diff in LH aavg (5-25N, 50-70E) for HadGEM3-A_xbrqc minus HadGEM3-AO_ahsaf'
AXES,XVALS=xvalues,XLABELS=xlabels,YVALS=yvalues,NDECS=1,$
  ytitle='Climatological latent heat flux (W m!U-2!N)',xminor=xminor
GPLOT,Y=coupled_aavg,X=indgen(hadgem3ao_ntime),THICK=200,COL=31
purple=FSC_COLOR("purple",32)
GPLOT,Y=atmos_aavg,X=indgen(hadgem3a_ntime),THICK=200,COL=32
cyan=FSC_COLOR("red",33)
GPLOT,Y=era40_aavg,X=indgen(era40_ntime)+30,THICK=200,COL=33
items=['ERA-40 and ECMWF-OP (1958-2007)','HadGEM3-AO_ahsaf','HadGEM3-A_ahrqc']
LEGEND,labels=items,COL=[33,31,32],LEGPOS=9

PSCLOSE

STOP

END
