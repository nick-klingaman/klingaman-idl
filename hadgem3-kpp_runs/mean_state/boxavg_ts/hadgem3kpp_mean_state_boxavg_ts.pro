PRO hadgem3kpp_mean_state_boxavg_ts
  
; Make timeseries of area-averaged quantities to look for drift 
; in the HadGEM3-KPP simulations.

n_vars=4
indir='/home/ss901165/um_output5/xgspm'
model_name='hadgem3kpp_1.5xentrain_ga30'
n_years=20
n_days_per_year=360

;box_aavg=[-30,40,30,200]
;box_name='coupling_box'
;ymin=[3.4,250,298.5,299.5]
;ymax=[6.8,276,301.5,301]
;ystep=[0.2,2,0.3,0.1]
;yminor=[0.1,1,0.15,0.05]

;box_aavg=[-10,60,10,100]
;box_name='eqIndOcn'
;ymin=[0,180,300.5,300.5]
;ymax=[20,330,303.5,303.5]
;ystep=[2,10,0.3,0.3]
;yminor=[0.5,5,0.15,0.15]

;box_aavg=[-20,130,20,170]
;box_name='WestPacOcn'
;ymin=[2,210,300,301]
;ymax=[16,305,303,303]
;ystep=[1,5,0.25,0.2]
;yminor=[0.5,2.5,0.125,0.1]

box_aavg=[-10,100,10,130]
box_name='MaritimeCon'
ymin=[0,170,300,300.5]
ymax=[18,330,303.,303.5]
ystep=[2,10,0.2,0.2]
yminor=[0.5,5,0.1,0.1]

;box_aavg=[-10,60,-20,110]
;box_name='SIndOcn'
;ymin=[0,200,297,297.]
;ymax=[18,320,303.,303.]
;ystep=[1.5,10,0.3,0.3]
;yminor=[0.5,5,0.15,0.15]

FOR i=0,n_vars-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         varname='precip'
         nc_varname='precip'
         model_name='hadgem3kpp_1.5xentrain_ga30'
         multiplier=86400.         
         ylabel='Area-averaged precipitation rate (mm day!U-1!N)'
      END
      1 : BEGIN
         varname='olr'
         nc_varname='olr'
         model_name='hadgem3kpp_1.5xentrain_ga30'
         multiplier=1.
         ylabel='Area-averaged outgoing longwave radiation (W m!U-2!N)'
      END
      2 : BEGIN
         varname='surf_temp'
         nc_varname='temp'
         model_name='hadgem3kpp_1.5xentrain_ga30'
         multiplier=1.
         ylabel='Area-averaged surface temperature over all gridpoints (K)'
      END
      3 : BEGIN
         varname='sst'
         nc_varname='temp_1'     
         model_name='hadgem3kpp_1.5xentrain_ga30'
         multiplier=1.
         ylabel='Area-averaged surface temperature over ocean gridpoints only (K)'
      END
   ENDCASE

   infile=indir+'/hadgem3kpp_1.5xentrain_ga30.jan-dec_dmeans.i2-k1.'+varname+'.nc'
   longitude=OPEN_AND_EXTRACT(infile,'longitude')
   latitude=OPEN_AND_EXTRACT(infile,'latitude')
   DEFINE_BOUNDARIES,box_aavg,latitude,longitude,box_aavg_tx,/LIMIT
   n_lon=N_ELEMENTS(longitude)
   n_lat=N_ELEMENTS(latitude)

   variable=OPEN_AND_EXTRACT(infile,nc_varname,offset=[box_aavg_tx(1),box_aavg_tx(0),0,0],$
                             count=[n_lon,n_lat,n_days_per_year,n_years])*multiplier

   IF TOTAL(where(variable ge 1e10)) ge 0 THEN $
      variable[where(variable ge 1e10)]=!Values.F_NaN

   variable_aavg=fltarr(n_days_per_year*n_years)
   FOR j=0,n_years-1 DO $
      FOR k=0,n_days_per_year-1 DO $
         variable_aavg(j*n_days_per_year+k)=MEAN(variable(*,*,k,j),/NaN)
   
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/boxavg_ts/hadgem3kpp_mean_state_boxavg_ts.'+$
          model_name+'.'+varname+'.'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2500,SPACE2=500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=200
   GSET,XMIN=0,XMAX=n_days_per_year*n_years,YMIN=ymin(i),YMAX=ymax(i),TITLE='Evolution of daily area-averaged '+varname+' over '+box_name+' from '+model_name
   GPLOT,X=indgen(n_days_per_year*n_years)+0.5,Y=variable_aavg
   smoothed=SMOOTH(variable_aavg,361)
   GPLOT,X=indgen(n_days_per_year*n_years-360)+180.5,Y=smoothed(180:n_days_per_year*n_years-181),COL=FSC_COLOR('red')
   AXES,XSTEP=360,YSTEP=ystep(i),yminor=yminor(i),xminor=120,XTITLE='Day since start of simulation',YTITLE=ylabel,NDECS=2
   GLEGEND,labels=REVERSE(['Daily timeseries','360-day smoothed']),LEGPOS=4,COL=REVERSE([FSC_COLOR('black'),FSC_COLOR('red')])
   PSCLOSE,/NOVIEW

ENDFOR

STOP
END
