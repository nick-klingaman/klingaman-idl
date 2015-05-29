PRO hadgem3kpp_cascade_phase2_moisture_budget

n_sets=1
pt=[0,70]
time_offset=0
n_times=8
FOR i=0,n_sets-1 DO BEGIN
   CASE i OF 
      0 : BEGIN
         set_name='control'
         runid='xfioj'
      END
      1 : BEGIN
         set_name='15xentrain'
         runid='xfrlb'
      END
      2 : BEGIN
         set_name='nocmt'
         runid='xfrlc'
      END
      3 : BEGIN
         set_name='15xentrain_nocmt'
         runid='xfrld'
      END
   ENDCASE

   q_file='/home/ss901165/um_output3/'+runid+'/'+runid+'a.06apr09-08apr09.q_after_ts.nc'
   longitude=OPEN_AND_EXTRACT(q_file,'longitude_1')
   latitude=OPEN_AND_EXTRACT(q_file,'latitude')
   z=OPEN_AND_EXTRACT(q_file,'hybrid_ht_1')
   longitude_offset=NEAREST(longitude,pt(1))
   latitude_offset=NEAREST(latitude,pt(0))
   n_z=N_ELEMENTS(z)
   
   q_after_ts=REFORM(OPEN_AND_EXTRACT(q_file,'q',offset=[longitude_offset,latitude_offset,0,time_offset],$
                                      count=[1,1,n_z,n_times]))
   
   advect_file='/home/ss901165/um_output3/'+runid+'/'+runid+'a.06apr09-08apr09.Qinc_advect.nc'
   qinc_advect=REFORM(OPEN_AND_EXTRACT(advect_file,'unspecified',offset=[longitude_offset,latitude_offset,0,time_offset],$
                                       count=[1,1,n_z,n_times]))
   
   bdylrlscld_file='/home/ss901165/um_output3/'+runid+'/'+runid+'a.06apr09-08apr09.Qinc_bdylr.nc'
   qinc_bdylrlscld=REFORM(OPEN_AND_EXTRACT(bdylrlscld_file,'unspecified_6',offset=[longitude_offset,latitude_offset,0,time_offset],$
                                           count=[1,1,n_z,n_times]))

   conv_file='/home/ss901165/um_output3/'+runid+'/'+runid+'a.06apr09-08apr09.Qinc_conv.nc'
   qinc_conv=REFORM(OPEN_AND_EXTRACT(conv_file,'unspecified_8',offset=[longitude_offset,latitude_offset,0,time_offset],$
                                     count=[1,1,n_z,n_times]))

   lsrain_file='/home/ss901165/um_output3/'+runid+'/'+runid+'a.06apr09-08apr09.Qinc_lsrain.nc'
   qinc_lsrain=REFORM(OPEN_AND_EXTRACT(lsrain_file,'unspecified_1',offset=[longitude_offset,latitude_offset,0,time_offset],$
                                       count=[1,1,n_z,n_times]))

   lwrad_file='/home/ss901165/um_output3/'+runid+'/'+runid+'a.06apr09-08apr09.Qinc_lwrad.nc'
   qinc_lwrad=REFORM(OPEN_AND_EXTRACT(lwrad_file,'unspecified_1',offset=[longitude_offset,latitude_offset,0,time_offset],$
                                      count=[1,1,n_z,n_times]))
   
   swrad_file='/home/ss901165/um_output3/'+runid+'/'+runid+'a.06apr09-08apr09.Qinc_swrad.nc'
   qinc_swrad=REFORM(OPEN_AND_EXTRACT(swrad_file,'unspecified_1',offset=[longitude_offset,latitude_offset,0,time_offset],$
                                      count=[1,1,n_z,n_times]))

   residual=fltarr(n_z,n_times-1)
   FOR j=0,n_z-1 DO BEGIN
      FOR k=0,n_times-2 DO $
         residual(j,k)=q_after_ts(j,k+1)-(q_after_ts(j,k)+(qinc_advect(j,k)+qinc_conv(j,k)+qinc_bdylrlscld(j,k)+$
                                                           qinc_lwrad(j,k)+qinc_swrad(j,k)+qinc_lsrain(j,k)))
   ENDFOR

   thlvl_pres_file='/home/ss901165/um_output3/'+runid+'/'+runid+'a.06apr09-08apr09.p_thlvl_after_ts.nc'
   thlvl_longitude=OPEN_AND_EXTRACT(thlvl_pres_file,'longitude_1')
   thlvl_latitude=OPEN_AND_EXTRACT(thlvl_pres_file,'latitude')
   thlvl_longitude_offset=NEAREST(thlvl_longitude,pt(1))
   thlvl_latitude_offset=NEAREST(thlvl_latitude,pt(0))
   
   thlvl_pres=REFORM(OPEN_AND_EXTRACT(thlvl_pres_file,'p_1',$
                                      offset=[thlvl_longitude_offset,thlvl_latitude_offset,0,0],$
                                      count=[1,1,n_z,n_times]))
   thlvl_pres_mean=fltarr(n_z)
   FOR m=0,n_z-1 DO $
      thlvl_pres_mean(m)=MEAN(thlvl_pres(m,*))/100.
   
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/budgets/hadgem3kpp_cascade_phase2_moisture_budget.'+set_name+'.ts_'+$
          STRTRIM(STRING(n_times-4),1)+'.pt'+STRTRIM(STRING(pt(0)),1)+STRTRIM(STRING(pt(1)),1)+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=200,XOFFSET=1400,YOFFSET=7000,TFONT=2,TCHARSIZE=80,/PORTRAIT,$
       SPACE3=200
   xmin=0.00000005
   xmax=1.0
   GSET,XMIN=xmin,XMAX=xmax,YMIN=MAX(thlvl_pres_mean),YMAX=100.,$
        TITLE='Moisture budget at timestep '+STRTRIM(STRING(n_times-4),1)+' at point '+$
        STRTRIM(STRING(pt(0)),1)+','+STRTRIM(STRING(pt(1)),1)+' using data from a control integration',/XLOG
   red=FSC_COLOR('red',30)
   blue=FSC_COLOR('blue',31)
   brown=FSC_COLOR('brown',32)
   purple=FSC_COLOR('purple',33)
   black=FSC_COLOR('black',34)
   pink=FSC_COLOR('pink',35)
   cyan=FSC_COLOR('cyan',36)
   top=NEAREST(thlvl_pres_mean,100.)
   blsmall=where(ABS(qinc_bdylrlscld*1000.) lt 10.^(-7))
   lsrain_small=where(ABS(qinc_lsrain*1000.) lt 10.^(-7))
   qinc_bdylrlscld[blsmall]=5*10.^(-11)
   qinc_lsrain[lsrain_small]=5*10.^(-11)
   GPLOT,X=ABS(qinc_advect(0:top-1,n_times-4)*1000.),Y=thlvl_pres_mean(0:top-1),COL=30,STYLE=0
   GPLOT,X=ABS(qinc_bdylrlscld(0:top-1,n_times-4)*1000.),Y=thlvl_pres_mean(0:top-1),COL=31,STYLE=0
   GPLOT,X=ABS(qinc_conv(0:top-1,n_times-4)*1000.),Y=thlvl_pres_mean(0:top-1),COL=32,STYLE=0
   GPLOT,X=ABS(qinc_lsrain(0:top-1,n_times-4)*1000.),Y=thlvl_pres_mean(0:top-1),COL=32,STYLE=2
   GPLOT,X=ABS(qinc_lwrad(0:top-1,n_times-4)*1000.),Y=thlvl_pres_mean(0:top-1),COL=33,STYLE=0
   GPLOT,X=ABS(qinc_swrad(0:top-1,n_times-4)*1000.),Y=thlvl_pres_mean(0:top-1),COL=33,STYLE=2   
   GPLOT,X=ABS(residual(0:top-1,n_times-4)*1000.),Y=thlvl_pres_mean(0:top-1),COL=36,STYLE=0
   GPLOT,X=ABS((qinc_advect(0:top-1,n_times-4)+qinc_bdylrlscld(0:top-1,n_times-4)+$
                qinc_conv(0:top-1,n_times-4)+qinc_lsrain(0:top-1,n_times-4)+$
                qinc_lwrad(0:top-1,n_times-4)+qinc_swrad(0:top-1,n_times-4))*1000.),COL=34,STYLE=0,$
         Y=thlvl_pres_mean(0:top-1)
   GPLOT,X=ABS(q_after_ts(0:top-1,n_times-3)-q_after_ts(0:top-1,n_times-4))*1000.,COL=35,STYLE=0,$
         Y=thlvl_pres_mean(0:top-1)
   AXES,XVALS=[0.0000001,0.0000003,0.000001,0.000003,0.00001,0.00003,0.0001,0.0003,0.001,0.003,0.01,0.03,0.1,0.3,1.0],$
        XLABELS=['10!U-8','3*10!U-8','10!U-7','3*10!U-7','10!U-6','3*10!U-6','10!U-5','3*10!U-5','10!U-4','3*10!U-4','10!U-3','3*10!U-3','10!U-2','3*10!U-2','10!U-1'],YVALS=indgen(19)*50+100,$
        ORIENTATION=20,XTITLE='Increment or difference (g kg!U-1!N (20 minutes)!U-1!N)',YTITLE='Pressure (hPa)'
   labels=['Advection','Boundary-layer and large-scale cloud','Convection','Large-scale rain','Longwave radiation','Shortwave radiation','Total','Difference (ts_n+1-ts_n)','Residual (difference minus total increment)']
   GLEGEND,labels=REVERSE(labels),COL=REVERSE([30,31,32,32,33,33,34,35,36]),STYLE=REVERSE([0,0,0,2,0,2,0,0,0]),LEGXOFFSET=0,LEGYOFFSET=-1000
   PSCLOSE

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/budgets/hadgem3kpp_cascade_phase2_moisture_budget.'+set_name+'.only_totals.ts_'+$
          STRTRIM(STRING(n_times-4),1)+'.pt'+STRTRIM(STRING(pt(0)),1)+STRTRIM(STRING(pt(1)),1)+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=200,XOFFSET=1400,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT,$
          SPACE3=200
   xmin=-0.6
   xmax=0.3
   GSET,XMIN=xmin,XMAX=xmax,YMIN=MAX(thlvl_pres_mean),YMAX=100.,$
        TITLE='Moisture budget at timestep '+STRTRIM(STRING(n_times-4),1)+' at point '+$
        STRTRIM(STRING(pt(0)),1)+','+STRTRIM(STRING(pt(1)),1)+' using data from a control integration'
   red=FSC_COLOR("red",30)
   blue=FSC_COLOR("blue",31)
   purple=FSC_COLOR("purple",32)
   top=NEAREST(thlvl_pres_mean,100.)
   GPLOT,X=residual(0:top-1,n_times-4)*1000.,Y=thlvl_pres_mean(0:top-1),COL=30,STYLE=0
   GPLOT,X=(qinc_advect(0:top-1,n_times-4)+qinc_bdylrlscld(0:top-1,n_times-4)+$
            qinc_conv(0:top-1,n_times-4)+qinc_lsrain(0:top-1,n_times-4)+$
            qinc_lwrad(0:top-1,n_times-4)+qinc_swrad(0:top-1,n_times-4))*1000.,COL=31,STYLE=0,$
         Y=thlvl_pres_mean(0:top-1)
   GPLOT,X=(q_after_ts(0:top-1,n_times-3)-q_after_ts(0:top-1,n_times-4))*1000.,COL=32,STYLE=0,$
         Y=thlvl_pres_mean(0:top-1)
;   GPLOT,X=[0,0],Y=[100,MAX(thlvl_pres_mean)],STYLE=2,COL=FSC_COLOR('black')
   AXES,XSTEP=0.06,XMINOR=0.03,NDECS=2,YVALS=indgen(19)*50+100,XTITLE='Total increment or difference (g kg!U-1!N (20 minutes)!U-1!N)',$
        YTITLE='Pressure (hPa)'
   labels=['Total increment','Difference (ts_n+1 minus ts_n)','Residual (total increment minus difference)']
   GLEGEND,labels=REVERSE(labels),COL=REVERSE([31,32,30]),STYLE=[0,0,0],LEGPOS=1
   PSCLOSE
ENDFOR

   

STOP

END
