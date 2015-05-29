PRO hadgem3kpp_mjo_iav_foam_ensembles
  
; Plot inter-annual variations in MJO indices from the FOAM ensembles, 
; to see whether there are particular years when MJO activity is
; actually reasonable

n_sets=3
ndays_per_year=360
max_years=19
amplitude_byyear=fltarr(n_sets,max_years)
strong_amplitude_byyear=fltarr(n_sets,max_years)
frequency_byyear=fltarr(n_sets,max_years)
all_plot_descriptions=strarr(n_sets)
all_colors=strarr(n_sets)
all_nyears=intarr(n_sets)
all_syms=intarr(n_sets)
FOR i=0,n_sets-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         rmm_infile='/home/ss901165/um_output3/hadgem3a_foam_control/hadgem3a_foam_ctl.jan-dec_dmeans.1-10.rmm_indices.nc'
         psfile_description='foam_vn73'
         plot_description='HadGEM3-A vn7.3 FOAM'
         n_years=10
         color='red'
         sym=4
      END
      1 : BEGIN
         rmm_infile='/home/ss901165/um_output3/hadgem3a_foam_control_vn74/hadgem3a_foam_ctl_vn74.jan-dec_dmeans.1-15.rmm_indices.nc'
         psfile_description='foam_vn74'
         plot_description='HadGEM3-A vn7.4 FOAM'
         n_years=15
         color='blue'
         sym=5
      END
      2 : BEGIN
         rmm_infile='/home/ss901165/um_output3/hadgem3a_foam_control_vn74_blendsst/hadgem3a_foam_ctl_vn74_blendsst.jan-dec_dmeans.1-19.rmm_indices.nc'
         psfile_description='foam_vn74_blendsst'
         plot_description='HadGEM3-A vn7.4 FOAM/AMIP2'
         n_years=19
         color='purple'
         sym=6
      END
   ENDCASE
   all_plot_descriptions(i)=plot_description
   all_colors(i)=color
   all_nyears(i)=n_years
   all_syms(i)=sym
   FOR j=0,n_years-1 DO BEGIN
      thisyear_amplitude=REFORM(OPEN_AND_EXTRACT(rmm_infile,'amplitude',$
                                                 offset=[j,0],count=[1,ndays_per_year]))
      amplitude_byyear(i,j)=MEAN(thisyear_amplitude)
      strong_amplitude_byyear(i,j)=MEAN(thisyear_amplitude[where(thisyear_amplitude ge 1)])
      frequency_byyear(i,j)=N_ELEMENTS(where(thisyear_amplitude ge 1))/FLOAT(ndays_per_year)
   ENDFOR
ENDFOR
psfile='/home/ss901165/idl/hadgem3-kpp_runs/mjo_iav/hadgem3kpp_mjo_iav_foam_ensembles.amplitude_ts.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2500,SPACE2=400,XOFFSET=200,YOFFSET=100,TFONT=2,TCHARSIZE=120,SPACE3=400
GSET,XMIN=0,XMAX=max_years,YMIN=0.2,YMAX=1.7,TITLE='Mean amplitude and frequency for each year in each FOAM ensemble'
FOR i=0,n_sets-1 DO BEGIN
   thiscol=FSC_COLOR(all_colors(i),i+20)
   GPLOT,X=indgen(all_nyears(i))+0.5,Y=REFORM(amplitude_byyear(i,0:all_nyears(i)-1)),COL=i+20,STYLE=0,SYM=all_syms(i)
   GPLOT,X=indgen(all_nyears(i))+0.5,Y=REFORM(strong_amplitude_byyear(i,0:all_nyears(i)-1)),COL=i+20,STYLE=2,SYM=all_syms(i)
ENDFOR
AXES,XSTEP=2,XMINOR=1,YSTEP=0.2,YMINOR=0.1,XTITLE='Ensemble member or year',YTITLE='Mean amplitude of RMM indices',NDECS=2,/NORIGHT
GSET,XMIN=0,XMAX=max_years,YMIN=0,YMAX=1.0
FOR i=0,n_sets-1 DO BEGIN
   thiscol=FSC_COLOR(all_colors(i),i+20)
   GPLOT,X=indgen(all_nyears(i))+0.5,Y=REFORM(frequency_byyear(i,0:all_nyears(i)-1)),COL=i+20,STYLE=4,SYM=all_syms(i)
ENDFOR
AXES,XSTEP=2,XMINOR=1,YSTEP=0.1,YMINOR=0.05,YTITLE='Frequency of activity outside unit circle',NDECS=2,/ONLYRIGHT

LEGEND,labels=all_plot_descriptions,SYM=all_syms,STYLE=REPLICATE(0,n_sets),LEGPOS=11,COL=indgen(n_sets)+20
LEGEND,labels=['Frequency outside unit circle',$
               'Mean amplitude on days with amplitude >= 1',$
               'Mean amplitude on all days'],STYLE=[4,2,0],COL=[1,1,1],LEGPOS=3

PSCLOSE

STOP

END


         
