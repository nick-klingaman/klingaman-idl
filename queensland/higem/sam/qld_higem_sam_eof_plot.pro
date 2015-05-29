PRO qld_higem_sam_eof_plot

higem_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.sam_eofs.from_mmean_mslp.nc'
era40_infile='/home/ss901165/datasets/ERA40/era40.sam_eofs.from_mmean_mslp.nc'

box=[-90,0,0,360]

higem_longitude=OPEN_AND_EXTRACT(higem_infile,'longitude')
higem_latitude=OPEN_AND_EXTRACT(higem_infile,'latitude')
DEFINE_BOUNDARIES,box,higem_latitude,higem_longitude,higem_box_tx,/LIMIT
higem_nlon=N_ELEMENTS(higem_longitude)
higem_nlat=N_ELEMENTS(higem_latitude)
higem_eof=REFORM(OPEN_AND_EXTRACT(higem_infile,'eofs',$
                                  offset=[higem_box_tx(1),higem_box_tx(0),0],$
                                  count=[higem_nlon,higem_nlat,1]))
FOR i=0,higem_nlon-1 DO BEGIN
   temp=REFORM(higem_eof(i,*))
   temp[where(higem_latitude lt -60)]=SMOOTH(temp[where(higem_latitude lt -60)],9)*1.3
   higem_eof(i,*)=temp
ENDFOR
FOR i=0,higem_nlat-1 DO BEGIN
   IF higem_latitude(i) lt -70 THEN BEGIN
      temp=REFORM(higem_eof(*,i))
      temp=SMOOTH(temp,15)
      higem_eof(*,i)=temp
   ENDIF
ENDFOR
      

mylevs=['-0.022','-0.018','-0.014','-0.010','-0.006','-0.002','0.002','0.006','0.010','0.014','0.018','0.022']
psfile='/home/ss901165/idl/queensland/higem/sam/qld_higem_sam_eof_plot.higem_eof1.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=400,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
       YSIZE=18000,SPACE3=500
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3),/SH
LEVS,MANUAL=mylevs
CON,X=higem_longitude,Y=higem_latitude,FIELD=higem_eof,/NOLINES,/NOCOLBAR;,/CB_RIGHT
COLBAR,COORDS=[23000,2000,23500,20000],TITLE='EOF loading'
GPLOT,X=[higem_longitude,0],Y=REPLICATE(-20,higem_nlon+1),THICK=70
GPLOT,X=0,Y=-16,TEXT='20S'
GPLOT,X=[higem_longitude,0],Y=REPLICATE(-40,higem_nlon+1),THICK=70
GPLOT,X=0,Y=-35,TEXT='40S'
GPLOT,X=[higem_longitude,0],Y=REPLICATE(-60,higem_nlon+1),THICK=70
GPLOT,X=0,Y=-54,TEXT='60S'
AXES
PSCLOSE

era40_longitude=OPEN_AND_EXTRACT(era40_infile,'longitude')
era40_longitude=OPEN_AND_EXTRACT(era40_infile,'longitude')
era40_latitude=OPEN_AND_EXTRACT(era40_infile,'latitude')
DEFINE_BOUNDARIES,box,era40_latitude,era40_longitude,era40_box_tx,/LIMIT
era40_nlon=N_ELEMENTS(era40_longitude)
era40_nlat=N_ELEMENTS(era40_latitude)
era40_eof=REFORM(OPEN_AND_EXTRACT(era40_infile,'eofs',$
                                  offset=[era40_box_tx(1),era40_box_tx(0),0],$
                                  count=[era40_nlon,era40_nlat,1]))*(-1)

psfile='/home/ss901165/idl/queensland/higem/sam/qld_higem_sam_eof_plot.era40_eof1.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=400,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
       YSIZE=18000,SPACE3=500
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1
MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3),/SH
LEVS,MANUAL=mylevs
CON,X=era40_longitude,Y=era40_latitude,FIELD=era40_eof,/NOLINES,/NOCOLBAR;,/CB_RIGHT
COLBAR,COORDS=[23000,2000,23500,20000],TITLE='EOF loading'
GPLOT,X=[era40_longitude,0],Y=REPLICATE(-20,era40_nlon+1),THICK=70
GPLOT,X=0,Y=-16,TEXT='20S'
GPLOT,X=[era40_longitude,0],Y=REPLICATE(-40,era40_nlon+1),THICK=70
GPLOT,X=0,Y=-35,TEXT='40S'
GPLOT,X=[era40_longitude,0],Y=REPLICATE(-60,era40_nlon+1),THICK=70
GPLOT,X=0,Y=-54,TEXT='60S'
AXES
PSCLOSE

STOP
END
