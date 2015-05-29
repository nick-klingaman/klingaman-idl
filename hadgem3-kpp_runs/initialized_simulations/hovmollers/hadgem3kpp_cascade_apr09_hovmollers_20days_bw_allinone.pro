PRO hadgem3kpp_cascade_apr09_hovmollers_20days_bw_allinone

; Make Hovmoller diagrams for the April 2009 Cascade case study
; using climate-resolution simulations.

n_sets=3
n_variables=1
box=[-5,40,5,180]

FOR i=0,n_sets-1 DO BEGIN
   CASE i OF
      2 : BEGIN
         precip_infile='/home/ss901165/datasets/TRMM_3B42V6/n96/TRMM_3B42v6A.jan-dec_dmeans.2009-2010.n96.nc'
         precip_name='trmm'
         precip_title='TRMM (HadGEM3 grid)'
         precip_ncname='precip'
         precip_multiplier=1.
         u200_infile='/home/ss901165/datasets_mango/ERA-INTERIM/U200/U200.jan-dec_dmeans.2009.mjo_domain.n96.nc'
         u200_name='eraint'
         u200_title='ERA-Interim (HadGEM3 grid)'
         u200_ncname='U'
         u850_infile='/home/ss901165/datasets_mango/ERA-INTERIM/U850/U850.jan-dec_dmeans.2009.mjo_domain.n96.nc'
         u850_name='eraint'
         u850_title='ERA-Interim (HadGEM3 grid)'
         u850_ncname='U'
         olr_infile='/home/ss901165/datasets/NOAA_CIRES_OLR/daily/NOAA_CIRES_OLR.jan-dec_dmeans.2009.nc'
         olr_name='noaa'
         olr_ncname='olr'
         time_offset=95
      END
      0 : BEGIN
         precip_infile='/home/ss901165/um_output3/xfadh.old_stash/xfadha.06apr09_precip.nc'
         precip_name='xfadh'
         precip_title='HadGEM3 - 1.0x entrain and CMT (control)'
         precip_ncname='precip'
         u200_infile='/home/ss901165/um_output3/xfadh.old_stash/xfadha.06apr09_u200.nc'
         u200_name='xfadh'
         u200_title=precip_title
         u200_ncname='u'
         u850_infile='/home/ss901165/um_output3/xfadh.old_stash/xfadha.06apr09_u850.nc'
         u850_name='xfadh'
         u850_title=precip_title
         u850_ncname='u'
         olr_infile='/home/ss901165/um_output3/xfadh.old_stash/xfadha.06apr09_olr.nc'
         olr_name='xfadh'
         olr_ncname='olr'
         time_offset=0
         precip_multiplier=86400.
      END
      1 : BEGIN
         precip_infile='/home/ss901165/um_output3/xfadk.old_stash/xfadka.06apr09_precip.nc'
         precip_name='xfadk'
         precip_title='HadGEM3 - 1.5x entrain and CMT'
         precip_ncname='precip'
         u200_infile='/home/ss901165/um_output3/xfadk.old_stash/xfadka.06apr09_u200.nc'
         u200_name='xfadk'
         u200_title=precip_title
         u200_ncname='u'
         u850_infile='/home/ss901165/um_output3/xfadk.old_stash/xfadka.06apr09_u850.nc'
         u850_name='xfadk'
         u850_title=precip_title
         u850_ncname='u'
         olr_infile='/home/ss901165/um_output3/xfadk.old_stash/xfadka.06apr09_olr.nc'
         olr_name='xfadk'
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
            vartitle='precipitation'
            ncname=precip_ncname
            title=precip_title
            multiplier=precip_multiplier
            units_string='Precipitation (mm day!U-1!N)'
            mylevs=['2','4','6','8','10','12','15','18','22','26','30'];,'35','40'];,'45','55']            
            white_level=2            
            color_rev=0
         END
         1 : BEGIN
            infile=u200_infile
            name=u200_name
            varname='u200'
            vartitle='200 hPa zonal wind'
            ncname=u200_ncname
            title=u200_title
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
            vartitle='850 hPa zonal wind'
            title=u850_title
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
                                count=[nlon,nlat,20])*multiplier
      
      IF i eq 0 THEN $
         latavg_variable=fltarr(n_sets,nlon,20)
      
      domainavg_variable=fltarr(20)
      IF TOTAL(where(variable ge 1E10) ge 1) THEN $
         variable[where(variable ge 1E10)]=!Values.F_NaN
      
      FOR k=0,nlon-1 DO $
         FOR m=0,19 DO $
            latavg_variable(i,k,m)=MEAN(variable(k,*,m),/NaN)
      FOR m=0,19 DO $
      domainavg_variable(m)=MEAN(latavg_variable(i,*,m),/NaN)
   ENDFOR
ENDFOR
      
psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/hovmollers/hadgem3kpp_cascade_apr09_hovmollers_20days_bw_allinone.'+varname+'_n96.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=140,MARGIN=3000,SPACE2=2000,XOFFSET=1200,YOFFSET=1500,TFONT=2,TCHARSIZE=140,SPACE3=600,$
       CB_WIDTH=110,XPLOTS=n_sets,XSPACING=500
titles=['CONT','EXP','TRMM']
FOR i=0,n_sets-1 DO BEGIN
   POS,XPOS=i+1,XSIZE=14000
   IF color_rev eq 1 THEN BEGIN
      CS,SCALE=7,NCOLS=N_ELEMENTS(mylevs)+1,white=white_level,/REV
   ENDIF ELSE IF color_rev eq 0 THEN $
      CS,SCALE=7,NCOLS=N_ELEMENTS(mylevs)+1,white=white_level
   LEVS,MANUAL=mylevs
   GSET,XMIN=MIN(longitude),XMAX=MAX(longitude),YMIN=0,YMAX=19
   CON,X=longitude,Y=indgen(20),FIELD=REFORM(latavg_variable(i,*,*)),CB_TITLE=units_string,/NOLINES,/NOCOLBAR,TITLE=titles(i)                               
   IF i eq 0 THEN BEGIN
      AXES,XVALS=[40,55,70,85,100,115,130,145,160,175],XTITLE='Longitude (degrees east)',YTITLE='Time',$
           YLABELS=['6 Apr 2009','8 Apr 2009','10 Apr 2009','12 Apr 2009','14 Apr 2009','16 Apr 2009','18 Apr 2009','20 Apr 2009','22 Apr 2009','24 Apr 2009'],$
           YVALS=[0,2,4,6,8,10,12,14,16,18],YMINOR=indgen(20),ORIENTATION=30
   ENDIF ELSE $
      AXES,XVALS=[40,55,70,85,100,115,130,145,160,175],XTITLE='Longitude (degrees east)',YTITLE=' ',YVALS=[0,2,4,6,8,10,12,14,16,18],YLABELS=['','','','','','','','','',''],ORIENTATION=30
   COLBAR,COORDS=[5000,2000,25000,2500],TITLE='Precipitation (mm day!U-1!N)'
ENDFOR
 
PSCLOSE

STOP

END
