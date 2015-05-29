PRO hadgem3kpp_cascade_oct08_hovmollers_20days_bw

; Make Hovmoller diagrams for the October 2008 Cascade case study
; using climate-resolution simulations.

n_sets=5
n_variables=2
box=[-5,40,5,180]
FOR i=0,n_sets-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         precip_infile='/home/ss901165/datasets/TRMM_3B42V6/n96/TRMM_3B42v6A.jan-dec_dmeans.2008.n96.nc'
         precip_name='trmm'
         precip_title='TRMM (HadGEM3 grid)'
         precip_ncname='precip'
         precip_multiplier=1.
         u200_infile='/home/ss901165/datasets_mango/ERA-INTERIM/U200/U200.jan-dec_dmeans.2008.mjo_domain.n96.nc'
         u200_name='eraint'
         u200_title='ERA-Interim (HadGEM3 grid)'
         u200_ncname='U'
         u850_infile='/home/ss901165/datasets_mango/ERA-INTERIM/U850/U850.jan-dec_dmeans.2008.mjo_domain.n96.nc'
         u850_name='eraint'
         u850_title='ERA-Interim (HadGEM3 grid)'
         u850_ncname='U'
         olr_infile='/home/ss901165/datasets/NOAA_CIRES_OLR/daily/NOAA_CIRES_OLR.jan-dec_dmeans.2008.nc'
         olr_name='noaa'
         olr_ncname='olr'
         time_offset=283
      END
;      1 : BEGIN
;         precip_infile='/home/ss901165/um_output3/xfadd/xfadda.06oct08_precip.nc'
;         precip_name='xfadd'
;         precip_title='Control (UM SSTs)'
;         precip_ncname='precip'
;         precip_multiplier=86400.
;         u200_infile='/home/ss901165/um_output3/xfadd/xfadda.06oct08_u200.nc'
;         u200_name='xfadd'
;         u200_title=precip_title
;         u200_ncname='u'
;         u850_infile='/home/ss901165/um_output3/xfadd/xfadda.06oct08_u850.nc'
;         u850_name='xfadd'
;         u850_title=precip_title
;         u850_ncname='u'
;         olr_infile='/home/ss901165/um_output3/xfadd/xfadda.06oct08_olr.nc'
;         olr_name='xfadd'
;         olr_ncname='olr'
;         time_offset=0
;      END
      1 : BEGIN
         precip_infile='/home/ss901165/um_output3/xfdif/xfdifa.11oct08_precip.nc'
         precip_name='xfdif'
         precip_title='HadGEM3 - 1.0x entrain and CMT (control)'
         precip_ncname='precip'
         precip_multiplier=86400.
         u200_infile='/home/ss901165/um_output3/xfdif/xfdifa.11oct08_u200.nc'
         u200_name='xfdif'
         u200_title=precip_title
         u200_ncname='u'
         u850_infile='/home/ss901165/um_output3/xfdif/xfdifa.11oct08_u850.nc'
         u850_name='xfdif'
         u850_title=precip_title
         u850_ncname='u'
         olr_infile='/home/ss901165/um_output3/xfdif/xfdifa.11oct08_olr.nc'
         olr_name='xfdif'
         olr_ncname='olr'
         time_offset=0
      END
      2 : BEGIN
         precip_infile='/home/ss901165/um_output3/xfdii/xfdiia.11oct08_precip.nc'
         precip_name='xfdii'
         precip_title='HadGEM3 - 1.5x entrain and CMT'
         precip_ncname='precip'
         u200_infile='/home/ss901165/um_output3/xfdii/xfdiia.11oct08_u200.nc'
         u200_name='xfdii'
         u200_title=precip_title
         u200_ncname='u'
         u850_infile='/home/ss901165/um_output3/xfdii/xfdiia.11oct08_u850.nc'
         u850_name='xfdii'
         u850_title=precip_title
         u850_ncname='u'
         olr_infile='/home/ss901165/um_output3/xfdii/xfdiia.11oct08_olr.nc'
         olr_name='xfdii'
         olr_ncname='olr'
         time_offset=0
      END
      3 : BEGIN
         precip_infile='/home/ss901165/um_output3/xfdij/xfdija.11oct08_precip.nc'
         precip_name='xfdij'
         precip_title='HadGEM3 with 1.0x entrain and no CMT'
         precip_ncname='precip'
         u200_infile='/home/ss901165/um_output3/xfdij/xfdija.11oct08_u200.nc'
         u200_name='xfdij'
         u200_title=precip_title
         u200_ncname='u'
         u850_infile='/home/ss901165/um_output3/xfdij/xfdija.11oct08_u850.nc'
         u850_name='xfdij'
         u850_title=precip_title
         u850_ncname='u'
         olr_infile='/home/ss901165/um_output3/xfdij/xfdija.11oct08_olr.nc'
         olr_name='xfdij'
         olr_ncname='olr'
         time_offset=0
      END
      4 : BEGIN
         precip_infile='/home/ss901165/um_output3/xfdik/xfdika.11oct08_precip.nc'
         precip_name='xfdik'
         precip_title='HadGEM3 - 1.5x entrain and no CMT'
         precip_ncname='precip'
         u200_infile='/home/ss901165/um_output3/xfdik/xfdika.11oct08_u200.nc'
         u200_name='xfdik'
         u200_title=precip_title
         u200_ncname='u'
         u850_infile='/home/ss901165/um_output3/xfdik/xfdika.11oct08_u850.nc'
         u850_name='xfdik'
         u850_title=precip_title
         u850_ncname='u'
         olr_infile='/home/ss901165/um_output3/xfdik/xfdika.11oct08_olr.nc'
         olr_name='xfdik'
         olr_ncname='olr'
         time_offset=0
      END
;      6 : BEGIN
;         precip_infile='/home/ss901165/um_output3/xfadn/xfadna.11oct08_precip.nc'
;         precip_name='xfadn'
;         precip_ncname='precip'
;         u200_infile='/home/ss901165/um_output3/xfadn/xfadna.11oct08_u200.nc'
;         u200_name='xfadn'
;         u200_ncname='u'
;         u850_infile='/home/ss901165/um_output3/xfadn/xfadna.11oct08_u850.nc'
;         u850_name='xfadn'
;         u850_ncname='u'
;         olr_infile='/home/ss901165/um_output3/xfadn/xfadna.11oct08_olr.nc'
;         olr_name='xfadn'
;         olr_ncname='olr'
;         time_offset=0
;      END
;      7 : BEGIN
;         precip_infile='/home/ss901165/um_output3/xfado/xfadoa.11oct08_precip.nc'
;         precip_name='xfado'
;         precip_ncname='precip'
;         u200_infile='/home/ss901165/um_output3/xfado/xfadoa.11oct08_u200.nc'
;         u200_name='xfado'
;         u200_ncname='u'
;         u850_infile='/home/ss901165/um_output3/xfado/xfadoa.11oct08_u850.nc'
;         u850_name='xfado'
;         u850_ncname='u'
;         olr_infile='/home/ss901165/um_output3/xfado/xfadoa.11oct08_olr.nc'
;         olr_name='xfado'
;         olr_ncname='olr'
;         time_offset=0
;      END
;      8 : BEGIN
;         precip_infile='/home/ss901165/um_output3/xfadp/xfadpa.11oct08_precip.nc'
;         precip_name='xfadp'
;         precip_ncname='precip'
;         u200_infile='/home/ss901165/um_output3/xfadp/xfadpa.11oct08_u200.nc'
;         u200_name='xfadp'
;         u200_ncname='u'
;         u850_infile='/home/ss901165/um_output3/xfadp/xfadpa.11oct08_u850.nc'
;         u850_name='xfadp'
;         u850_ncname='u'
;         olr_infile='/home/ss901165/um_output3/xfadp/xfadpa.11oct08_olr.nc'
;         olr_name='xfadp'
;         olr_ncname='olr'
;         time_offset=0
;      END
;      9 : BEGIN
;         precip_infile='/home/ss901165/um_output3/xfadq/xfadqa.11oct08_precip.nc'
;         precip_name='xfadq'
;         precip_ncname='precip'
;         u200_infile='/home/ss901165/um_output3/xfadq/xfadqa.11oct08_u200.nc'
;         u200_name='xfadq'
;         u200_ncname='u'
;         u850_infile='/home/ss901165/um_output3/xfadq/xfadqa.11oct08_u850.nc'
;         u850_name='xfadq'
;         u850_ncname='u'
;         olr_infile='/home/ss901165/um_output3/xfadq/xfadqa.11oct08_olr.nc'
;         olr_name='xfadq'
;         olr_ncname='olr'
;         time_offset=0
;      END
;      10 : BEGIN
;         precip_infile='/home/ss901165/um_output3/xfadr/xfadra.11oct08_precip.nc'
;         precip_name='xfadr'
;         precip_ncname='precip'
;         u200_infile='/home/ss901165/um_output3/xfadr/xfadra.11oct08_u200.nc'
;         u200_name='xfadr'
;         u200_ncname='u'
;         u850_infile='/home/ss901165/um_output3/xfadr/xfadra.11oct08_u850.nc'
;         u850_name='xfadr'
;         u850_ncname='u'
;         olr_infile='/home/ss901165/um_output3/xfadr/xfadra.11oct08_olr.nc'
;         olr_name='xfadr'
;         olr_ncname='olr'
;         time_offset=0
;      END
;      11 : BEGIN
;         precip_infile='/home/ss901165/um_output3/xfadv/xfadva.11oct08_precip.nc'
;         precip_name='xfadv'
;         precip_ncname='precip'
;         u200_infile='/home/ss901165/um_output3/xfadv/xfadva.11oct08_u200.nc'
;         u200_name='xfadv'
;         u200_ncname='u'
;         u850_infile='/home/ss901165/um_output3/xfadv/xfadva.11oct08_u850.nc'
;         u850_name='xfadv'
;         u850_ncname='u'
;         olr_infile='/home/ss901165/um_output3/xfadv/xfadva.11oct08_olr.nc'
;         olr_name='xfadv'
;         olr_ncname='olr'
;         time_offset=0
;      END
   ENDCASE
   FOR j=0,n_variables-1 DO BEGIN
      CASE j OF 
         1 : BEGIN
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
         0 : BEGIN
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
            color_rev=-999            
            positive_style=0
            negative_style=2
            fill_value=14
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
            color_rev=-999
            fill_value=7
            negative_style=2
            positive_style=0
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
      domainavg_variable=fltarr(30)
      IF TOTAL(where(variable ge 1E10) ge 1) THEN $
         variable[where(variable ge 1E10)]=!Values.F_NaN
      
      FOR k=0,nlon-1 DO $
         FOR m=0,29 DO $
            latavg_variable(k,m)=MEAN(variable(k,*,m),/NaN)
      FOR m=0,29 DO $
         domainavg_variable(m)=MEAN(latavg_variable(*,m),/NaN)
   
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/hovmollers/hadgem3kpp_cascade_oct08_hovmollers_30days_bw.'+name+'_'+varname+'_n96.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=160,MARGIN=3000,SPACE2=2000,XOFFSET=1200,YOFFSET=1500,TFONT=2,TCHARSIZE=100,SPACE3=600,$
             XSIZE=12000,YSIZE=23500,/PORTRAIT,CB_WIDTH=110
      IF color_rev eq 1 THEN BEGIN
         CS,SCALE=7,NCOLS=N_ELEMENTS(mylevs)+1,white=white_level,/REV
      ENDIF ELSE IF color_rev eq 0 THEN $
         CS,SCALE=7,NCOLS=N_ELEMENTS(mylevs)+1,white=white_level
      LEVS,MANUAL=mylevs
      GSET,XMIN=MIN(longitude),XMAX=MAX(longitude),YMIN=0,YMAX=29
      IF color_rev eq 0 or color_rev eq 1 THEN BEGIN
         CON,X=longitude,Y=indgen(30),FIELD=latavg_variable,CB_TITLE=units_string,/NOLINES
      ENDIF ELSE BEGIN
         CS,COLS=[495,453,453,495]
         LEVS,MANUAL=[-1*fill_value,0,1*fill_value]
         CON,X=longitude,Y=indgen(30),FIELD=latavg_variable,/NOLINES,/NOLINELABELS,/NOCOLBAR
         LEVS,MANUAL=mylevs
         CON,X=longitude,Y=indgen(30),FIELD=latavg_variable,/NOFILL,positive_style=positive_style,negative_style=negative_style,/NOLINELABELS         
      ENDELSE
      AXES,XTITLE='Longitude (degrees east)',YTITLE='Time',$
           YLABELS=['11/10/08','14/10/08','17/10/08','20/10/08','23/10/08','26/10/08',$
                    '29/10/08','01/11/08','04/11/08','07/11/08'],$
           YVALS=[0,3,6,9,12,15,18,21,24,27]+0.5,YMINOR=indgen(29)+0.5,XVALS=[40,55,70,85,100,115,130,145,160,175]      
      PSCLOSE,/NOVIEW
   ENDFOR
ENDFOR

STOP

END
