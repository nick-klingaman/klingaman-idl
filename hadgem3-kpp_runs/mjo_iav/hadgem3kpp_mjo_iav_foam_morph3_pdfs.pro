PRO hadgem3kpp_mjo_iav_foam_morph3_pdfs

; Plot PDFs of inter-annual variability in MJO activity
; for our HadGEM3-A ensembles and the MORPH3 assessment runs
; and observations

n_sets=6
max_days_per_year=365
max_years=30
n_pdf=60

daily_amplitude_pdfs=fltarr(n_sets,n_pdf+1)
annual_amplitude_pdfs=fltarr(n_sets,n_pdf+1)
annual_strong_amplitude_pdfs=fltarr(n_sets,n_pdf+1)
annual_frequency_pdfs=fltarr(n_sets,n_pdf+1)

amplitude_byyear=fltarr(max_years)
amplitude_allyears=fltarr(max_years*max_days_per_year)
strong_amplitude_byyear=fltarr(max_years)
frequency_byyear=fltarr(max_years)

all_plot_descriptions=strarr(n_sets)
all_colors=strarr(n_sets)
all_nyears=intarr(n_sets)
all_syms=intarr(n_sets)
FOR i=0,n_sets-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         rmm_infile='/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2009.index_values.nc'
         psfile_description='obs'
         plot_description='Observations (NOAA/NCEP, 1982-2008, 27 years)'
         n_years=27
         color='black'
         offset_year=7
         ndays_per_year=365
      END
      1 : BEGIN
         rmm_infile='/home/ss901165/um_output3/hadgem3a_foam_control/hadgem3a_foam_ctl.jan-dec_dmeans.1-10.rmm_indices.nc'
         psfile_description='foam_vn73'
         plot_description='HadGEM3-A vn7.3 FOAM (10 years)'
         n_years=10
         color='red'
         offset_year=0
         ndays_per_year=360
      END
      2 : BEGIN
         rmm_infile='/home/ss901165/um_output3/hadgem3a_foam_control_vn74/hadgem3a_foam_ctl_vn74.jan-dec_dmeans.1-15.rmm_indices.nc'
         psfile_description='foam_vn74'
         plot_description='HadGEM3-A vn7.4 FOAM (15 years)'
         n_years=15
         color='blue'
         offset_year=0
      END
      3 : BEGIN
         rmm_infile='/home/ss901165/um_output3/hadgem3a_foam_control_vn74_blendsst/hadgem3a_foam_ctl_vn74_blendsst.jan-dec_dmeans.1-19.rmm_indices.nc'
         psfile_description='foam_vn74_blendsst'
         plot_description='HadGEM3-A vn7.4 FOAM/AMIP2 (19 years)'
         n_years=19
         color='purple'
         offset_year=0
      END
      4 : BEGIN
         rmm_infile='/home/ss901165/um_output3/hadgem3_monwg/airxv/hadgem3a_morph3_final_n96_amip2_airxv.jan-dec_dmeans.1982-2008.rmm_indices.nc'
         psfile_description='morph3_amip2_airxv'
         plot_description='HadGEM3-A MORPH3 AMIP2 (airxv, 1982-2008, 27 years)'
         n_years=27
         color='brown'
         offset_year=0
      END
      5 : BEGIN
         rmm_infile='/home/ss901165/um_output3/hadgem3_monwg/airxv/hadgem3a_morph3_final_n96_amip2_airxv_01-08.jan-dec_dmeans.2001-2008.rmm_indices.nc'
         psfile_description='morph3_amip2_airxv_02-08'
         plot_description='HadGEM3-A MORPH3 AMIP2 (airxv, 2002-2008 only)'
         n_years=7
         color='orange'
         offset_year=1
      END
   ENDCASE
   all_plot_descriptions(i)=plot_description
   all_colors(i)=color
   all_nyears(i)=n_years
   FOR j=0,n_years-1 DO BEGIN
      thisyear_amplitude=REFORM(OPEN_AND_EXTRACT(rmm_infile,'amplitude',$
                                                 offset=[j,0],count=[1,ndays_per_year]))
      IF TOTAL(where(thisyear_amplitude eq -9999)) gt 0 THEN $
         thisyear_amplitude[where(thisyear_amplitude eq -9999)]=-1.0
      amplitude_allyears[j*ndays_per_year:(j+1)*ndays_per_year-1]=thisyear_amplitude
      amplitude_byyear(j)=MEAN(thisyear_amplitude,/NaN)
      strong_amplitude_byyear(j)=MEAN(thisyear_amplitude[where(thisyear_amplitude ge 1)],/NaN)
      frequency_byyear(j)=N_ELEMENTS(where(thisyear_amplitude ge 1))/FLOAT(ndays_per_year)
   ENDFOR
   
   pdf,amplitude_allyears(0:ndays_per_year*n_years-1),/BANDWIDTH,NPDF=n_pdf,xid=xaxis,xrange=[0,3],pdf=this_amplitude_pdf,/NOPLOT
   daily_amplitude_pdfs(i,*)=this_amplitude_pdf
   daily_amplitude_xaxis=xaxis
   pdf,amplitude_byyear(0:n_years-1),/BANDWIDTH,NPDF=n_pdf,xid=xaxis,xrange=[0.1,2],pdf=this_amplitude_pdf,/NOPLOT
   annual_amplitude_pdfs(i,*)=this_amplitude_pdf
   annual_amplitude_xaxis=xaxis
   pdf,strong_amplitude_byyear(0:n_years-1),/BANDWIDTH,NPDF=n_pdf,xid=xaxis,xrange=[1,2.5],pdf=this_amplitude_pdf,/NOPLOT
   annual_strong_amplitude_pdfs(i,*)=this_amplitude_pdf
   annual_strong_amplitude_xaxis=xaxis
   pdf,frequency_byyear(0:n_years-1),/BANDWIDTH,NPDF=n_pdf,xid=xaxis,xrange=[0,1],pdf=this_amplitude_pdf,/NOPLOT
   annual_frequency_pdfs(i,*)=this_amplitude_pdf
   annual_frequency_xaxis=xaxis

ENDFOR

psfile='/home/ss901165/idl/hadgem3-kpp_runs/mjo_iav/hadgem3kpp_mjo_iav_foam_morph3_pdfs.daily_amplitude_pdf.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1800,SPACE2=400,XOFFSET=1000,YOFFSET=400,TFONT=2,TCHARSIZE=120,SPACE3=400
GSET,XMIN=0,XMAX=3,YMIN=0.,YMAX=0.003,TITLE='PDF of daily RMM amplitude'
FOR i=0,n_sets-1 DO BEGIN
   this_color=FSC_COLOR(all_colors(i),i+20)
   GPLOT,X=daily_amplitude_xaxis,Y=REFORM(daily_amplitude_pdfs(i,*)),COL=i+20,STYLE=0,THICK=175
ENDFOR
GPLOT,X=[1.0,1.0],Y=[0,0.003],STYLE=1,COL=1,THICK=250
AXES,XSTEP=0.3,XMINOR=0.15,YSTEP=0.0003,YMINOR=0.00015,XTITLE='Daily RMM amplitude',YTITLE='Probability density',NDECS=4
LEGEND,labels=all_plot_descriptions,COL=indgen(n_sets)+20,LEGPOS=9,STYLE=REPLICATE(0,n_sets)
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3-kpp_runs/mjo_iav/hadgem3kpp_mjo_iav_foam_morph3_pdfs.annual_amplitude_pdf.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1800,SPACE2=400,XOFFSET=1000,YOFFSET=400,TFONT=2,TCHARSIZE=120,SPACE3=400
GSET,XMIN=0,XMAX=2,YMIN=0.,YMAX=0.008,TITLE='PDF of annual-mean RMM amplitude'
FOR i=0,n_sets-1 DO BEGIN
   this_color=FSC_COLOR(all_colors(i),i+20)
   GPLOT,X=annual_amplitude_xaxis,Y=REFORM(annual_amplitude_pdfs(i,*)),COL=i+20,STYLE=0,THICK=175
ENDFOR
;GPLOT,X=[1.0,1.0],Y=[0,0.003],STYLE=1,COL=1,THICK=250
AXES,XSTEP=0.2,XMINOR=0.1,YSTEP=0.0008,YMINOR=0.0004,XTITLE='Annual-mean RMM amplitude',YTITLE='Probability density',NDECS=4
LEGEND,labels=all_plot_descriptions,COL=indgen(n_sets)+20,LEGPOS=9,STYLE=REPLICATE(0,n_sets)
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3-kpp_runs/mjo_iav/hadgem3kpp_mjo_iav_foam_morph3_pdfs.annual_strong_amplitude_pdf.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1800,SPACE2=400,XOFFSET=1000,YOFFSET=400,TFONT=2,TCHARSIZE=120,SPACE3=400
GSET,XMIN=1,XMAX=2.5,YMIN=0.,YMAX=0.01,TITLE='PDF of annual-mean RMM amplitude on days with amplitude > 1'
FOR i=0,n_sets-1 DO BEGIN
   this_color=FSC_COLOR(all_colors(i),i+20)
   GPLOT,X=annual_strong_amplitude_xaxis,Y=REFORM(annual_strong_amplitude_pdfs(i,*)),COL=i+20,STYLE=0,THICK=175
ENDFOR
;GPLOT,X=[1.0,1.0],Y=[0,0.003],STYLE=1,COL=1,THICK=250
AXES,XSTEP=0.15,XMINOR=0.075,YSTEP=0.001,YMINOR=0.0005,XTITLE='Annual-mean RMM amplitude on days with amplitude > 1',YTITLE='Probability density',NDECS=4
LEGEND,labels=all_plot_descriptions,COL=indgen(n_sets)+20,LEGPOS=9,STYLE=REPLICATE(0,n_sets)
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3-kpp_runs/mjo_iav/hadgem3kpp_mjo_iav_foam_morph3_pdfs.annual_frequency_pdf.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1800,SPACE2=400,XOFFSET=1000,YOFFSET=400,TFONT=2,TCHARSIZE=120,SPACE3=400
GSET,XMIN=0,XMAX=1,YMIN=0.,YMAX=0.003,TITLE='PDF of annual strong MJO frequency'
FOR i=0,n_sets-1 DO BEGIN
   this_color=FSC_COLOR(all_colors(i),i+20)
   GPLOT,X=annual_frequency_xaxis,Y=REFORM(annual_frequency_pdfs(i,*)),COL=i+20,STYLE=0,THICK=175
ENDFOR
;GPLOT,X=[1.0,1.0],Y=[0,0.003],STYLE=1,COL=1,THICK=250
AXES,XSTEP=0.1,XMINOR=0.05,YSTEP=0.0003,YMINOR=0.00015,XTITLE='Fraction of days in year with amplitude > 1',YTITLE='Probability density',NDECS=4
LEGEND,labels=all_plot_descriptions,COL=indgen(n_sets)+20,LEGPOS=9,STYLE=REPLICATE(0,n_sets)
PSCLOSE,/NOVIEW

STOP

END



