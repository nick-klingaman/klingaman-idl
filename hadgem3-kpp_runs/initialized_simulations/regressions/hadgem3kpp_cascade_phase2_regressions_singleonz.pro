PRO hadgem3kpp_cascade_phase2_regressions_singleonz

um3='/home/ss901165/um_output3'
date_ranges=['04nov09-06nov09','26aug08-28aug08','07dec07-09dec07','07sep06-09sep06','18mar05-20mar05','03dec03-05dec03',$
             '29oct02-01nov02','05jun02-07jun02','26apr02-28apr02','16jan01-18jan01','13nov00-15nov00','28sep00-30sep00']

n_sets=4
box=[-5,60,5,100]
box_desc='5S-5N, 60-100E'
n_times=24

single_varname='lsrain'
single_ncvarname='lsrain'
single_longitude_var='longitude'
single_latitude_var='latitude_1'
single_multiplier=86400.
single_vardesc='large-scale precipitation'
z_varname='w_after_ts'
z_ncvarname='dz_dt'
z_longitude_var='longitude'
z_latitude_var='latitude'
z_height_var='hybrid_ht'
z_vardesc='vertical velocity'
z_multiplier=100.
legpos=11

xmin=-0.10
xmax=1.30
xstep=0.10
regression_units='cm s!U-1!N (mm day!U-1!N)!U-1!N'

FOR i=0,n_sets-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         runids=['xfrla','xfrle','xfrli','xfrlm','xfrlq','xfrlu','xfsea','xfsee','xfsei','xfsem','xfseq','xfseu']
         set_name='control'
      END
      1 : BEGIN
         runids=['xfrlb','xfrlf','xfrlj','xfrln','xfrlr','xfrlv','xfseb','xfsef','xfsej','xfsen','xfser','xfsev']
         set_name='15xentrain'
      END
      2 : BEGIN
         runids=['xfrlc','xfrlg','xfrlk','xfrlo','xfrls','xfrlw','xfsec','xfseg','xfsek','xfseo','xfses','xfsew']
         set_name='nocmt'
      END
      3 : BEGIN
         runids=['xfrld','xfrlh','xfrll','xfrlp','xfrlt','xfrlx','xfsed','xfseh','xfsel','xfsep','xfset','xfsex']
         set_name='15xentrain_nocmt'
      END
   ENDCASE
;   n_cases=N_ELEMENTS(runids)   
   n_cases=3
   
   FOR j=0,n_cases-1 DO BEGIN
      single_file=um3+'/'+runids(j)+'.new_stash/'+runids(j)+'.'+date_ranges(j)+'_3hrmeans.'+single_varname+'.nc'
      single_longitude=OPEN_AND_EXTRACT(single_file,single_longitude_var)
      single_latitude=OPEN_AND_EXTRACT(single_file,single_latitude_var)
      DEFINE_BOUNDARIES,box,single_latitude,single_longitude,single_box_tx,/LIMIT
      single_nlon=N_ELEMENTS(single_longitude)
      single_nlat=N_ELEMENTS(single_latitude)

      single_var=REFORM(OPEN_AND_EXTRACT(single_file,single_ncvarname,offset=[single_box_tx(1),single_box_tx(0),0,0],$
                                         count=[single_nlon,single_nlat,1,n_times]))*single_multiplier

      single_var_reformed=fltarr(single_nlon*single_nlat*n_times)
      single_var_reformed(*)=REFORM(single_var,[single_nlon*single_nlat*n_times])

      z_file=um3+'/'+runids(j)+'.new_stash/'+runids(j)+'.'+date_ranges(j)+'_3hrmeans.'+z_varname+'.nc'
      print,z_file
      z_longitude=OPEN_AND_EXTRACT(z_file,z_longitude_var)
      z_latitude=OPEN_AND_EXTRACT(z_file,z_latitude_var)
      z_height=OPEN_AND_EXTRACT(z_file,z_height_var)
      n_z=N_ELEMENTS(z_height)
      DEFINE_BOUNDARIES,box,z_latitude,z_longitude,z_box_tx,/LIMIT
      z_nlon=N_ELEMENTS(z_longitude)
      z_nlat=N_ELEMENTS(z_latitude)

      z_var=REFORM(OPEN_AND_EXTRACT(z_file,z_ncvarname,offset=[z_box_tx(1),z_box_tx(0),0,0],$
                                    count=[z_nlon,z_nlat,n_z,n_times]))*z_multiplier

      z_var_reformed=fltarr(z_nlon*z_nlat*n_times)

      IF i eq 0 and j eq 0 THEN BEGIN
         regression_coefficients=fltarr(n_sets,n_cases,n_z)
         correlation_coefficients=fltarr(n_sets,n_cases,n_z)
      ENDIF
      
      FOR k=0,n_z-1 DO BEGIN
         z_var_reformed(*)=REFORM(z_var(*,*,k,*),[z_nlon*z_nlat*n_times])
         regression_coefficients(i,j,k)=REGRESS(single_var_reformed,z_var_reformed,$
                                            CORRELATION=temp)
         correlation_coefficients(i,j,k)=temp(0)
      ENDFOR
   ENDFOR
ENDFOR

thlvl_pres_file=um3+'/'+runids(0)+'.new_stash/'+runids(0)+'.'+date_ranges(0)+'_3hrmeans.p_thlvl_after_ts.nc'
thlvl_longitude=OPEN_AND_EXTRACT(thlvl_pres_file,'longitude')
thlvl_latitude=OPEN_AND_EXTRACT(thlvl_pres_file,'latitude')
DEFINE_BOUNDARIES,box,thlvl_latitude,thlvl_longitude,thlvl_box_tx,/LIMIT
thlvl_nlon=N_ELEMENTS(thlvl_longitude)
thlvl_nlat=N_ELEMENTS(thlvl_latitude)

thlvl_pres=REFORM(OPEN_AND_EXTRACT(thlvl_pres_file,'p_1',$
                                    offset=[thlvl_box_tx(1),thlvl_box_tx(0),0,0],$
                                    count=[thlvl_nlon,thlvl_nlat,n_z,n_times]))
thlvl_pres_mean=fltarr(n_z)
FOR m=0,n_z-1 DO $
   thlvl_pres_mean(m)=MEAN(thlvl_pres(*,*,m,*))/100.


psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/regressions/hadgem3kpp_cascade_phase2_regressions_singleonz.'+single_varname+'-on-'+z_varname+'.hours0-71.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=200,XOFFSET=1400,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT,$
       SPACE3=200
GSET,XMIN=xmin,XMAX=xmax,YMIN=MAX(thlvl_pres_mean),YMAX=100.,$
     TITLE='Regression of '+z_vardesc+' on '+single_vardesc+' over '+box_desc+' for hours 0-71 of all integrations'
red=FSC_COLOR('red',30)
blue=FSC_COLOR('blue',31)
brown=FSC_COLOR('brown',32)
purple=FSC_COLOR('purple',33)
top=NEAREST(thlvl_pres_mean,100.)
FOR i=0,n_sets-1 DO BEGIN
   toplot=fltarr(n_z)
   FOR j=0,n_z-1 DO $
      toplot(j)=MEAN(regression_coefficients(i,*,j))
   GPLOT,X=REFORM(toplot(0:top)),Y=thlvl_pres_mean(0:top),COL=30+i,STYLE=0
   FOR j=0,top-1 DO BEGIN
      meancorr=MEAN(correlation_coefficients(i,*,j))
      IF ABS(meancorr) gt 0.189 THEN $
         GPLOT,X=toplot(j),Y=thlvl_pres_mean(j),COL=30+i,SYM=6,/NOLINES
   ENDFOR
ENDFOR       
GPLOT,X=[0,0],Y=[100,MAX(thlvl_pres_mean)],STYLE=1
AXES,XSTEP=xstep,YVALS=indgen(19)*50+100,YTITLE='Pressure (hPa)',XTITLE='Regression coefficient ('+regression_units+')',$
     NDECS=2,XMINOR=xstep/2.

labels=['Control','1.5x entrainment','No CMT','1.5x entrainment and no CMT']
GLEGEND,labels=REVERSE(labels),COL=REVERSE(indgen(n_sets)+30),LEGPOS=legpos

PSCLOSE

STOP
END

