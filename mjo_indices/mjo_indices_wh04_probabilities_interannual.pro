PRO mjo_indices_wh04_probabilities_interannual

airxv_infile='/home/ss901165/um_output3/hadgem3_monwg/airxv/hadgem3a_morph3_final_n96_amip2_airxv.jan-dec_dmeans.1982-2008.rmm_indices.nc'
airxv_0108_infile='/home/ss901165/um_output3/hadgem3_monwg/airxv/hadgem3a_morph3_final_n96_amip2_airxv_01-08.jan-dec_dmeans.2001-2008.rmm_indices.nc'
xemjo_infile='/home/ss901165/um_output3/xemjo/hadgem3ha_morph3_final_n216_amip2_xemjo.jan-dec_dmeans.1982-2008.rmm_indices.nc'
xemjo_0108_infile='/home/ss901165/um_output3/xemjo/hadgem3ha_morph3_final_n216_amip2_xemjo_01-08.jan-dec_dmeans.2001-2008.rmm_indices.nc'
xemjx_infile='/home/ss901165/um_output3/xemjx/hadgem3ha_abelshipway_n216_amip2_xemjx.jan-dec_dmeans.1981-1994.rmm_indices.nc'
obs_infile='/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2009.index_values.nc'

twentyc_infile='/home/ss901165/datasets_mango/20THC_REANALYSIS/mjo_diagnostics/rmm_indices.nc'

n_sets=4
legend_titles=strarr(n_sets)
colors=['black','red','blue','cyan','blue','cyan']
styles=[0,0,0,0,2,2]
missing_value=-9999

psfile='/home/ss901165/idl/mjo_indices/mjo_indices_wh04_probabilities_interannual.hadgem3_and_20thc-compare-obs_freq.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=400,XOFFSET=900,YOFFSET=500,TFONT=2,TCHARSIZE=120,SPACE3=400
GSET,XMIN=1979.5,XMAX=2008.5,YMIN=0.,YMAX=1.0,TITLE='Probability of being outside the RMM1/RMM2 unit circle for each year'
FOR i=0,n_sets-1 DO BEGIN
   print,i
   CASE i OF
      ;1 : BEGIN
      ;   infile=twentyc_infile
      ;   nyears=29
      ;   file_offset=1
      ;   start_year=1980
      ;   graph_offset=0
      ;   ndays_per_year=365
      ;   legend_title='20th Century Reanalysis V2 (1980-2008)'
      ;   short_title='20thc_v2'
      ;   print_correlation_stddev=1
      ;END
      3 : BEGIN
         infile=xemjx_infile
         nyears=12
         file_offset=1
         start_year=1983
         graph_offset=3
         ndays_per_year=360
         legend_title='HadGEM3-HA AMIP2 (xemjx, 1983-1994)'
         short_title='xemjx'
         print_correlation_stddev=1
      END
      1 : BEGIN
         infile=airxv_infile
         nyears=26
         file_offset=1
         start_year=1983
         graph_offset=3
         ndays_per_year=360
         legend_title='HadGEM3-A AMIP2 (airxv, 1983-2008)'
         short_title='airxv'
         print_correlation_stddev=1
      END
      ;4: BEGIN
      ;   infile=airxv_0108_infile
      ;   nyears=7
      ;   file_offset=1
      ;   start_year=2002
      ;   graph_offset=22
      ;   ndays_per_year=360
      ;   legend_title='HadGEM3-A MORPH3 AMIP2 (airxv, 2002-2008)'
      ;   short_title='airxv_02-08'
      ;   print_correlation_stddev=0
      ;END
      ;5 : BEGIN
      ;   infile=xemjo_0108_infile
      ;   nyears=7
      ;   file_offset=1
      ;   start_year=2002
      ;   graph_offset=22
      ;   ndays_per_year=360
      ;   legend_title='HadGEM3-HA MORPH3 AMIP2 (xemjo, 2002-2008)'
      ;   short_title='xemjo_02-08'
      ;   print_correlation_stddev=0
      ;END
      2 : BEGIN
         infile=xemjo_infile
         nyears=26
         file_offset=1
         start_year=1983
         graph_offset=3
         ndays_per_year=360
         legend_title='HadGEM3-HA AMIP2 (xemjo, 1983-2008)'
         short_title='xemjo'
         print_correlation_stddev=1
      END
      0 : BEGIN
         infile=obs_infile
         nyears=29
         file_offset=5
         start_year=1980
         graph_offset=0
         ndays_per_year=365
         legend_title='Observations (NOAA/NCEP, 1980-2008)'
         short_title='obs'
         print_correlation_stddev=1
      END
   ENDCASE
   IF i eq 0 THEN BEGIN
      probability_outside=fltarr(n_sets,nyears)
      mean_amplitude_outside=fltarr(n_sets,nyears)
      mean_amplitude=fltarr(n_sets,nyears)
      all_nyears=fltarr(n_sets)
      all_start_year=fltarr(n_sets)
      all_print_correlation_stddev=fltarr(n_sets)
      all_short_title=strarr(n_sets)
      all_graph_offset=intarr(n_sets)
   ENDIF

   all_nyears(i)=nyears
   all_start_year(i)=start_year
   legend_titles(i)=legend_title
   all_print_correlation_stddev(i)=print_correlation_stddev
   all_short_title(i)=short_title
   all_graph_offset(i)=graph_offset
   FOR j=0,nyears-1 DO BEGIN
      phase_ts=REFORM(OPEN_AND_EXTRACT(infile,'phase',$
                                       offset=[file_offset+j,0],count=[1,ndays_per_year]))
      amplitude_ts=REFORM(OPEN_AND_EXTRACT(infile,'amplitude',$
                                           offset=[file_offset+j,0],count=[1,ndays_per_year]))
      probability_outside(i,j)=N_ELEMENTS(where(amplitude_ts ge 1))/FLOAT(ndays_per_year)
      mean_amplitude_outside(i,j)=MEAN(amplitude_ts[where(amplitude_ts ge 1 and amplitude_ts ne missing_value)])
      mean_amplitude(i,j)=MEAN(amplitude_ts[where(amplitude_ts ne missing_value)])
   ENDFOR
   this_color=FSC_COLOR(colors(i),i+2)
   GPLOT,X=indgen(nyears)+start_year,Y=REFORM(probability_outside(i,0:nyears-1)),SYM=5,THICK=200,SIZE=125,COL=i+2,STYLE=styles(i)
   IF print_correlation_stddev eq 1 THEN $
      GPLOT,X=1980,Y=0.05+i*0.04,TEXT='Inter-annual std.dev. in MJO activity for '+short_title+': '+$
            STRMID(STRTRIM(STRING(STDDEV(probability_outside(i,0:nyears-1))),1),0,5),ALIGN=0.0,CHARSIZE=90
   IF i ne 0 and print_correlation_stddev eq 1 THEN $
      GPLOT,X=1996,Y=0.05+i*0.04,TEXT='Correlation between '+short_title+' and observations: '+$
            STRMID(STRTRIM(STRING(CORRELATE(probability_outside(i,0:nyears-1),probability_outside(0,graph_offset:N_ELEMENTS(probability_outside(0,*))-1))),1),0,5),ALIGN=0.0,CHARSIZE=90
ENDFOR

LEGEND,labels=REVERSE(legend_titles),COL=REVERSE(indgen(n_sets)+2),LEGPOS=5,STYLE=REVERSE(styles)
AXES,XVALS=[1980,1982,1984,1986,1988,1990,1992,1994,1996,1998,2000,2002,2004,2006,2008],YSTEP=0.1,YTITLE='Probability',XTITLE='Year',NDECS=2,YMINOR=0.05
PSCLOSE

hadisst_nino34=OPEN_AND_EXTRACT('/home/ss901165/datasets/NINO/nino34_hadisst.jan-dec_ameans.1871-2008.nc','NINO34',$
                                offset=[112],count=[26])

psfile='/home/ss901165/idl/mjo_indices/mjo_indices_wh04_probabilities_interannual.hadgem3_and_20thc-compare-obs_ampl_outside.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=400,XOFFSET=900,YOFFSET=500,TFONT=2,TCHARSIZE=120,SPACE3=400
GSET,XMIN=1979.5,XMAX=2008.5,YMIN=0.7,YMAX=2.1,TITLE='Mean amplitude of RMM1/RMM2 (SQRT(RMM1^2+RMM2^2)) on days when amplitude > 1'
FOR i=0,n_sets-1 DO BEGIN
   this_color=FSC_COLOR(colors(i),i+2)
;   GPLOT,X=indgen(all_nyears(i))+all_start_year(i),Y=REFORM(mean_amplitude(i,*)),SYM=5,THICK=200,SIZE=125,COL=i+2,STYLE=styles(i)
   GPLOT,X=indgen(all_nyears(i))+all_start_year(i),Y=REFORM(mean_amplitude_outside(i,0:all_nyears(i)-1)),SYM=5,THICK=200,SIZE=125,COL=i+2,STYLE=styles(i)
   IF all_print_correlation_stddev(i) eq 1 THEN $
      GPLOT,X=1996,Y=1.20-i*0.05,TEXT='Inter-ann std.dev. in mean amp > 1 for '+all_short_title(i)+': '+$
            STRMID(STRTRIM(STRING(STDDEV(mean_amplitude_outside(i,0:all_nyears(i)-1))),1),0,5),ALIGN=0.0,CHARSIZE=90
   IF i ne 0 and all_print_correlation_stddev(i) eq 1 THEN $
      GPLOT,X=1996,Y=0.90-i*0.05,TEXT='Corr of mean amp > 1 for '+all_short_title(i)+' and obs: '+$
            STRMID(STRTRIM(STRING(CORRELATE(mean_amplitude_outside(i,0:all_nyears(i)-1),mean_amplitude_outside(0,all_graph_offset(i):N_ELEMENTS(mean_amplitude_outside(0,*))-1))),1),0,5),ALIGN=0.0,CHARSIZE=90
ENDFOR

LEGEND,labels=REVERSE(legend_titles),COL=REVERSE(indgen(n_sets)+2),LEGPOS=3,STYLE=REVERSE(styles)
AXES,XVALS=[1980,1982,1984,1986,1988,1990,1992,1994,1996,1998,2000,2002,2004,2006,2008],YSTEP=0.1,YTITLE='Amplitude',XTITLE='Year',NDECS=2,YMINOR=0.05

PSCLOSE

psfile='/home/ss901165/idl/mjo_indices/mjo_indices_wh04_probabilities_interannual.hadgem3_and_20thc-compare-obs_ampl.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=400,XOFFSET=900,YOFFSET=500,TFONT=2,TCHARSIZE=120,SPACE3=400
GSET,XMIN=1979.5,XMAX=2008.5,YMIN=0.2,YMAX=1.8,TITLE='Mean amplitude of RMM1/RMM2 (SQRT(RMM1^2+RMM2^2)) for each year'
FOR i=0,n_sets-1 DO BEGIN
   this_color=FSC_COLOR(colors(i),i+2)
   GPLOT,X=indgen(all_nyears(i))+all_start_year(i),Y=REFORM(mean_amplitude(i,0:all_nyears(i)-1)),SYM=5,THICK=200,SIZE=125,COL=i+2,STYLE=styles(i)
   IF all_print_correlation_stddev(i) eq 1 THEN $
      GPLOT,X=1996,Y=0.70-i*0.07,TEXT='Inter-ann std.dev. in mean amp for '+all_short_title(i)+': '+$
            STRMID(STRTRIM(STRING(STDDEV(mean_amplitude(i,0:all_nyears(i)-1))),1),0,5),ALIGN=0.0,CHARSIZE=90
   IF i ne 0 and all_print_correlation_stddev(i) eq 1 THEN $
      GPLOT,X=1996,Y=0.45-i*0.07,TEXT='Corr of mean amp for '+all_short_title(i)+' and obs: '+$
            STRMID(STRTRIM(STRING(CORRELATE(mean_amplitude(i,0:all_nyears(i)-1),mean_amplitude(0,all_graph_offset(i):N_ELEMENTS(mean_amplitude(0,*))-1))),1),0,5),ALIGN=0.0,CHARSIZE=90
ENDFOR
LEGEND,labels=REVERSE(legend_titles),COL=REVERSE(indgen(n_sets)+2),LEGPOS=3,STYLE=REVERSE(styles)
AXES,XVALS=[1980,1982,1984,1986,1988,1990,1992,1994,1996,1998,2000,2002,2004,2006,2008],YSTEP=0.1,YTITLE='Amplitude',XTITLE='Year',NDECS=2,YMINOR=0.05

PSCLOSE

STOP

END

