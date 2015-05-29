PRO hadgem3kpp_cascade_dec09_hovmollers

; Make Hovmoller diagrams for the October 2008 Cascade case study
; using climate-resolution simulations.

n_sets=1
n_variables=3
box=[-5,40,5,240]
n_days=90
FOR i=0,n_sets-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         precip_infile='/home/ss901165/datasets_mango/TRMM_3B42V6/n96/TRMM_3B42v6A.jan-dec_dmeans.2009-2010.n96.nc'
         precip_name='trmm'
         precip_ncname='precip'
         precip_multiplier=1.
         u200_infile='/home/ss901165/datasets_mango/ERA-INTERIM/U200/U200.jan-dec_dmeans.2009-2010.mjo_domain.n96.nc'
         u200_name='eraint'
         u200_ncname='U'
         u850_infile='/home/ss901165/datasets_mango/ERA-INTERIM/U850/U850.jan-dec_dmeans.2009-2010.mjo_domain.n96.nc'
         u850_name='eraint'
         u850_ncname='U'
         olr_infile='/home/ss901165/datasets/NOAA_CIRES_OLR/daily/NOAA_CIRES_OLR.jan-dec_dmeans.2009-2010.nc'
         olr_name='noaa'
         olr_ncname='olr'
         time_offset=335
      END
      1 : BEGIN
         precip_infile='/home/ss901165/um_output3/xfdib/xfdiba.11oct08_precip.nc'
         precip_name='xfdib'
         precip_ncname='precip'
         precip_multiplier=86400.
         u200_infile='/home/ss901165/um_output3/xfdib/xfdiba.11oct08_u200.nc'
         u200_name='xfdib'
         u200_ncname='u'
         u850_infile='/home/ss901165/um_output3/xfdib/xfdiba.11oct08_u850.nc'
         u850_name='xfdib'
         u850_ncname='u'
         olr_infile='/home/ss901165/um_output3/xfdib/xfdiba.11oct08_olr.nc'
         olr_name='xfdib'
         olr_ncname='olr'
         time_offset=0
      END
      2 : BEGIN
         precip_infile='/home/ss901165/um_output3/xfdic/xfdica.11oct08_precip.nc'
         precip_name='xfdic'
         precip_ncname='precip'
         u200_infile='/home/ss901165/um_output3/xfdic/xfdica.11oct08_u200.nc'
         u200_name='xfdic'
         u200_ncname='u'
         u850_infile='/home/ss901165/um_output3/xfdic/xfdica.11oct08_u850.nc'
         u850_name='xfdic'
         u850_ncname='u'
         olr_infile='/home/ss901165/um_output3/xfdic/xfdica.11oct08_olr.nc'
         olr_name='xfdic'
         olr_ncname='olr'
         time_offset=0
      END
      3 : BEGIN
         precip_infile='/home/ss901165/um_output3/xfdie/xfdiea.11oct08_precip.nc'
         precip_name='xfdie'
         precip_ncname='precip'
         u200_infile='/home/ss901165/um_output3/xfdie/xfdiea.11oct08_u200.nc'
         u200_name='xfdie'
         u200_ncname='u'
         u850_infile='/home/ss901165/um_output3/xfdie/xfdiea.11oct08_u850.nc'
         u850_name='xfdie'
         u850_ncname='u'
         olr_infile='/home/ss901165/um_output3/xfdie/xfdiea.11oct08_olr.nc'
         olr_name='xfdie'
         olr_ncname='olr'
         time_offset=0
      END
      4 : BEGIN
         precip_infile='/home/ss901165/um_output3/xfdif/xfdifa.11oct08_precip.nc'
         precip_name='xfdif'
         precip_ncname='precip'
         u200_infile='/home/ss901165/um_output3/xfdif/xfdifa.11oct08_u200.nc'
         u200_name='xfdif'
         u200_ncname='u'
         u850_infile='/home/ss901165/um_output3/xfdif/xfdifa.11oct08_u850.nc'
         u850_name='xfdif'
         u850_ncname='u'
         olr_infile='/home/ss901165/um_output3/xfdif/xfdifa.11oct08_olr.nc'
         olr_name='xfdif'
         olr_ncname='olr'
         time_offset=0
      END
      5 : BEGIN
         precip_infile='/home/ss901165/um_output3/xfdig/xfdiga.11oct08_precip.nc'
         precip_name='xfdig'
         precip_ncname='precip'
         u200_infile='/home/ss901165/um_output3/xfdig/xfdiga.11oct08_u200.nc'
         u200_name='xfdig'
         u200_ncname='u'
         u850_infile='/home/ss901165/um_output3/xfdig/xfdiga.11oct08_u850.nc'
         u850_name='xfdig'
         u850_ncname='u'
         olr_infile='/home/ss901165/um_output3/xfdig/xfdiga.11oct08_olr.nc'
         olr_name='xfdig'
         olr_ncname='olr'
         time_offset=0
      END
      6 : BEGIN
         precip_infile='/home/ss901165/um_output3/xfdih/xfdiha.11oct08_precip.nc'
         precip_name='xfdih'
         precip_ncname='precip'
         u200_infile='/home/ss901165/um_output3/xfdih/xfdiha.11oct08_u200.nc'
         u200_name='xfdih'
         u200_ncname='u'
         u850_infile='/home/ss901165/um_output3/xfdih/xfdiha.11oct08_u850.nc'
         u850_name='xfdih'
         u850_ncname='u'
         olr_infile='/home/ss901165/um_output3/xfdih/xfdiha.11oct08_olr.nc'
         olr_name='xfdih'
         olr_ncname='olr'
         time_offset=0
      END
      7 : BEGIN
         precip_infile='/home/ss901165/um_output3/xfdii/xfdiia.11oct08_precip.nc'
         precip_name='xfdii'
         precip_ncname='precip'
         u200_infile='/home/ss901165/um_output3/xfdii/xfdiia.11oct08_u200.nc'
         u200_name='xfdii'
         u200_ncname='u'
         u850_infile='/home/ss901165/um_output3/xfdii/xfdiia.11oct08_u850.nc'
         u850_name='xfdii'
         u850_ncname='u'
         olr_infile='/home/ss901165/um_output3/xfdii/xfdiia.11oct08_olr.nc'
         olr_name='xfdii'
         olr_ncname='olr'
         time_offset=0
      END
      8 : BEGIN
         precip_infile='/home/ss901165/um_output3/xfdij/xfdija.11oct08_precip.nc'
         precip_name='xfdij'
         precip_ncname='precip'
         u200_infile='/home/ss901165/um_output3/xfdij/xfdija.11oct08_u200.nc'
         u200_name='xfdij'
         u200_ncname='u'
         u850_infile='/home/ss901165/um_output3/xfdij/xfdija.11oct08_u850.nc'
         u850_name='xfdij'
         u850_ncname='u'
         olr_infile='/home/ss901165/um_output3/xfdij/xfdija.11oct08_olr.nc'
         olr_name='xfdij'
         olr_ncname='olr'
         time_offset=0
      END
      9 : BEGIN
         precip_infile='/home/ss901165/um_output3/xfdik/xfdika.11oct08_precip.nc'
         precip_name='xfdik'
         precip_ncname='precip'
         u200_infile='/home/ss901165/um_output3/xfdik/xfdika.11oct08_u200.nc'
         u200_name='xfdik'
         u200_ncname='u'
         u850_infile='/home/ss901165/um_output3/xfdik/xfdika.11oct08_u850.nc'
         u850_name='xfdik'
         u850_ncname='u'
         olr_infile='/home/ss901165/um_output3/xfdik/xfdika.11oct08_olr.nc'
         olr_name='xfdik'
         olr_ncname='olr'
         time_offset=0
      END
      10 : BEGIN
         precip_infile='/home/ss901165/um_output3/xfdil/xfdila.11oct08_precip.nc'
         precip_name='xfdil'
         precip_ncname='precip'
         u200_infile='/home/ss901165/um_output3/xfdil/xfdila.11oct08_u200.nc'
         u200_name='xfdil'
         u200_ncname='u'
         u850_infile='/home/ss901165/um_output3/xfdil/xfdila.11oct08_u850.nc'
         u850_name='xfdil'
         u850_ncname='u'
         olr_infile='/home/ss901165/um_output3/xfdil/xfdila.11oct08_olr.nc'
         olr_name='xfdil'
         olr_ncname='olr'
         time_offset=0
      END
      11 : BEGIN
         precip_infile='/home/ss901165/um_output3/xfdim/xfdima.11oct08_precip.nc'
         precip_name='xfdim'
         precip_ncname='precip'
         u200_infile='/home/ss901165/um_output3/xfdim/xfdima.11oct08_u200.nc'
         u200_name='xfdim'
         u200_ncname='u'
         u850_infile='/home/ss901165/um_output3/xfdim/xfdima.11oct08_u850.nc'
         u850_name='xfdim'
         u850_ncname='u'
         olr_infile='/home/ss901165/um_output3/xfdim/xfdima.11oct08_olr.nc'
         olr_name='xfdim'
         olr_ncname='olr'
         time_offset=0
      END
   ENDCASE
   FOR j=0,n_variables-1 DO BEGIN
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
            color_rev=1
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
            color_rev=0
         END
         2 : BEGIN
            infile=u850_infile
            name=u850_name
            varname='u850'
            ncname=u850_ncname
            multiplier=1
            units_string='Zonal wind at 850 hPa (m s!U-1!N)'
            mylevs=['-13','-11','-9','-7','-5','-3','-1','1','3','5','7','9','11','13']
            color_rev=0
         END
         3 : BEGIN
            infile=olr_infile
            name=olr_name
            varname='olr'
            ncname=olr_ncname
            color_rev=0
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
                                count=[nlon,nlat,n_days])*multiplier
      
      latavg_variable=fltarr(nlon,n_days)
      IF TOTAL(where(variable ge 1E10) ge 1) THEN $
         variable[where(variable ge 1E10)]=!Values.F_NaN
      
      FOR k=0,nlon-1 DO $
         FOR m=0,n_days-1 DO $
            latavg_variable(k,m)=MEAN(variable(k,*,m),/NaN)
   
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/hovmollers/hadgem3kpp_cascade_dec09_hovmollers.'+name+'_'+varname+'_n96.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=3000,SPACE2=2000,XOFFSET=1200,YOFFSET=1500,TFONT=2,TCHARSIZE=100,SPACE3=600,$
             XSIZE=12000,YSIZE=23500,/PORTRAIT
      IF color_rev eq 0 THEN BEGIN
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,white=white_level
      ENDIF ELSE $
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,white=white_level,/REV
      LEVS,MANUAL=mylevs
      GSET,XMIN=40,XMAX=240,YMIN=0,YMAX=n_days
      CON,X=longitude,Y=indgen(n_days)+0.5,FIELD=latavg_variable,CB_TITLE=units_string,$
          TITLE=name+' '+varname+' latitude-averaged 5S-5N',/NOLINES
      AXES,XSTEP=15,XTITLE='Longitude (degrees east)',YTITLE='Time',YLABELS=['1 Dec 2009','6 Dec 2009','11 Dec 2009','16 Dec 2009','21 Dec 2009','26 Dec 2009','31 Dec 2009','5 Jan 2010','10 Jan 2010','15 Jan 2010','20 Jan 2010','25 Jan 2010','30 Jan 2010','4 Feb 2010','9 Feb 2010','14 Feb 2010','19 Feb 2010','24 Feb 2010','1 Mar 2010'],YVALS=[0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90]
      
      PSCLOSE,/NOVIEW
   ENDFOR
ENDFOR

STOP

END
