PRO hadgem3kpp_cascade_dec07_hovmollers

; Make Hovmoller diagrams for the December 2007 case study
; using climate-resolution simulations.

n_sets=5
n_variables=3
box=[-5,40,5,180]
FOR i=0,n_sets-1 DO BEGIN
   print,i
   CASE i OF
      0 : BEGIN
         precip_infile='/home/ss901165/datasets_mango/TRMM_3B42V6/n96/TRMM_3B42v6A.jan-dec_dmeans.2007-2008.n96.nc'
         precip_name='trmm'
         precip_ncname='precip'
         precip_multiplier=1.
         u200_infile='/home/ss901165/datasets_mango/ERA-INTERIM/U200/U200.jan-dec_dmeans.2007-2008.mjo_domain.n96.nc'
         u200_name='eraint'
         u200_ncname='U'
         u850_infile='/home/ss901165/datasets_mango/ERA-INTERIM/U850/U850.jan-dec_dmeans.2007-2008.mjo_domain.n96.nc'
         u850_name='eraint'
         u850_ncname='U'
         olr_infile='/home/ss901165/datasets/NOAA_CIRES_OLR/daily/NOAA_CIRES_OLR.jan-dec_dmeans.2007.nc'
         olr_name='noaa'
         olr_ncname='olr'
         time_offset=340
      END
      1 : BEGIN
         precip_infile='/home/ss901165/um_output3/xfrli/xfrlia.07dec07_precip.nc'
         precip_name='xfrli'
         precip_ncname='precip'
         precip_multiplier=86400.
         u200_infile='/home/ss901165/um_output3/xfrli/xfrlia.07dec07_u200.nc'
         u200_name='xfrli'
         u200_ncname='u'
         u850_infile='/home/ss901165/um_output3/xfrli/xfrlia.07dec07_u850.nc'
         u850_name='xfrli'
         u850_ncname='u'
         olr_infile='/home/ss901165/um_output3/xfrli/xfrlia.07dec07_olr.nc'
         olr_name='xfrli'
         olr_ncname='olr'
         time_offset=0
      END
      2 : BEGIN
         precip_infile='/home/ss901165/um_output3/xfrlj/xfrlja.07dec07_precip.nc'
         precip_name='xfrlj'
         precip_ncname='precip'
         u200_infile='/home/ss901165/um_output3/xfrlj/xfrlja.07dec07_u200.nc'
         u200_name='xfrlj'
         u200_ncname='u'
         u850_infile='/home/ss901165/um_output3/xfrlj/xfrlja.07dec07_u850.nc'
         u850_name='xfrlj'
         u850_ncname='u'
         olr_infile='/home/ss901165/um_output3/xfrlj/xfrlja.07dec07_olr.nc'
         olr_name='xfrlj'
         olr_ncname='olr'
         time_offset=0
      END
      3 : BEGIN
         precip_infile='/home/ss901165/um_output3/xfrlk/xfrlka.07dec07_precip.nc'
         precip_name='xfrlk'
         precip_ncname='precip'
         u200_infile='/home/ss901165/um_output3/xfrlk/xfrlka.07dec07_u200.nc'
         u200_name='xfrlk'
         u200_ncname='u'
         u850_infile='/home/ss901165/um_output3/xfrlk/xfrlka.07dec07_u850.nc'
         u850_name='xfrlk'
         u850_ncname='u'
         olr_infile='/home/ss901165/um_output3/xfrlk/xfrlka.07dec07_olr.nc'
         olr_name='xfrlk'
         olr_ncname='olr'
         time_offset=0
      END
      4 : BEGIN
         precip_infile='/home/ss901165/um_output3/xfrll/xfrlla.07dec07_precip.nc'
         precip_name='xfrll'
         precip_ncname='precip'
         u200_infile='/home/ss901165/um_output3/xfrll/xfrlla.07dec07_u200.nc'
         u200_name='xfrll'
         u200_ncname='u'
         u850_infile='/home/ss901165/um_output3/xfrll/xfrlla.07dec07_u850.nc'
         u850_name='xfrll'
         u850_ncname='u'
         olr_infile='/home/ss901165/um_output3/xfrll/xfrlla.07dec07_olr.nc'
         olr_name='xfrll'
         olr_ncname='olr'
         time_offset=0
      END
   ENDCASE
   FOR j=0,n_variables-1 DO BEGIN
      print,'---> ',j
      CASE j OF 
         0 : BEGIN
            infile=precip_infile
            name=precip_name
            varname='precip'
            ncname=precip_ncname
            multiplier=precip_multiplier
            units_string='Precipitation (mm day!U-1!N)'
            mylevs=['2','4','6','8','10','12','15','18','22','26','30','35','40','45','55']            
            white_level=2            
         END
         1 : BEGIN
            infile=u200_infile
            name=u200_name
            varname='u200'
            ncname=u200_ncname
            multiplier=1
            units_string='Zonal wind at 200 hPa (m s!U-1!N)'
            mylevs=['-26','-22','-18','-14','-10','-6','-2','2','6','10','14','18','22','26']
            white_level=9
         END
         2 : BEGIN
            infile=u850_infile
            name=u850_name
            varname='u850'
            ncname=u850_ncname
            multiplier=1
            units_string='Zonal wind at 850 hPa (m s!U-1!N)'
            mylevs=['-13','-11','-9','-7','-5','-3','-1','1','3','5','7','9','11','13']
         END
         3 : BEGIN
            infile=olr_infile
            name=olr_name
            varname='olr'
            ncname=olr_ncname
         END
      ENDCASE
                                ; Get grids
      longitude=OPEN_AND_EXTRACT(infile,'longitude')
      latitude=OPEN_AND_EXTRACT(infile,'latitude')
      DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
      nlon=N_ELEMENTS(longitude)
      nlat=N_ELEMENTS(latitude)
      
                                ; Get variable
      variable=OPEN_AND_EXTRACT(infile,ncname,$
                                offset=[box_tx(1),box_tx(0),time_offset],$
                                count=[nlon,nlat,30])*multiplier
      
      latavg_variable=fltarr(nlon,30)
      IF TOTAL(where(variable ge 1E10) ge 1) THEN $
         variable[where(variable ge 1E10)]=!Values.F_NaN
      
      FOR k=0,nlon-1 DO $
         FOR m=0,29 DO $
            latavg_variable(k,m)=MEAN(variable(k,*,m),/NaN)
   
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/hovmollers/hadgem3kpp_cascade_dec07_hovmollers.'+name+'_'+varname+'_n96.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=3000,SPACE2=2000,XOFFSET=1200,YOFFSET=1500,TFONT=2,TCHARSIZE=100,SPACE3=600,$
             XSIZE=12000,YSIZE=23500,/PORTRAIT
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,white=white_level,/REV
      LEVS,MANUAL=mylevs
      GSET,XMIN=MIN(longitude),XMAX=MAX(longitude),YMIN=0,YMAX=30
      CON,X=longitude,Y=indgen(30)+0.5,FIELD=latavg_variable,CB_TITLE=units_string,$
          TITLE=name+' '+varname+' latitude-averaged 5S-5N',/NOLINES
      AXES,XSTEP=15,XTITLE='Longitude (degrees east)',YTITLE='Time',YLABELS=['7 Dec 2007','12 Dec 2007','17 Dec 2007','22 Dec 2007','27 Dec 2007','1 Jan 2008','6 Jan 2008'],YVALS=[0,5,10,15,20,25,30]
      
      PSCLOSE,/NOVIEW
   ENDFOR
ENDFOR

STOP

END
