PRO mjo_diabatic_test_metum_precip

  infile='/home/ss901165/um_output6/mjodiab_20day/metum/20091011/MetUM.hfss.20091011.00Z.nc'
;  infile='/home/ss901165/um_output6/mjodiab_20day/ecmwf/20091010/ECMWF_IFS.pr.20091010.00Z.nc'
  ;infile='/home/ss901165/um_output6/mjodiab_20day/metum_coupled_ann/Atmos_control/20091015/MetUM.pr.20091015.00Z.nc'

  metum_pr = OPEN_AND_EXTRACT(infile,'hfss')
  n_points = LONG(N_ELEMENTS(metum_pr))

  discrete_values=[metum_pr(0)]
  n_discrete_values=LONG(0)
  FOR i=LONG(1),LONG(n_points)-1 DO BEGIN
     IF TOTAL(where(discrete_values eq metum_pr(i))) eq -1 THEN BEGIN
        n_discrete_values=LONG(n_discrete_values+1)
        discrete_values=[discrete_values,metum_pr(i)]
     ENDIF
     IF n_discrete_values MOD 1000 eq 0 THEN $
        print,n_discrete_values,i
  ENDFOR
  
  print,N_ELEMENTS(metum_pr),n_discrete_values
  
  sorted_values=discrete_values[SORT(discrete_values)]
  distance=fltarr(n_discrete_values-1)
  FOR i=1,n_discrete_values-1 DO $
     distance(i-1)=sorted_values(i)-sorted_values(i-1)
  discrete_values_scaled=sorted_values/0.5 ; 0.164795
  discrete_values_remainder=sorted_values MOD 0.5
 
  psfile='/home/ss901165/idl/mjo_diabatic/checks/mjo_diabatic_test_metum_precip.discrete_values_hfls.ps'
  PSOPEN,file=psfile,FONT=2
  GSET,XMIN=0,XMAX=n_discrete_values,YMIN=0.5,YMAX=2000,/YLOG
  GPLOT,X=indgen(n_discrete_values),Y=sorted_values
  AXES,XVALS=indgen(n_discrete_values/30+1)*30,$
       YVALS=[0.1,0.2,0.3,0.4,0.6,0.8,1.0,2.0,3.0,4.0,6.0,8.0,10.0,$
              20.0,30.0,40.0,60.0,80.0,100.,200.,300.,400.,600.,800.,1000,2000],NDECS=2,$
       XTITLE='Number in array',YTITLE=['Latent heat flux (W s!U-1!N)'],/NORIGHT
  GSET,XMIN=0,XMAX=n_discrete_values,YMIN=1,YMAX=n_discrete_values
  GPLOT,X=indgen(n_discrete_values-1),Y=discrete_values_scaled,STYLE=2
  GPLOT,X=[0,n_discrete_values],Y=[0,n_discrete_values],STYLE=1
  AXES,YVALS=indgen(n_discrete_values/30+1)*30,YTITLE='Latent heat flux / 0.5 W m!U-2!N',/ONLYRIGHT
  GLEGEND,LEGPOS=1,labels=REVERSE(['Latent heat flux','Latent heat flux scaled by 0.5 W m!U-2!N']),$
          STYLE=REVERSE([0,2])
  PSCLOSE

STOP
END
