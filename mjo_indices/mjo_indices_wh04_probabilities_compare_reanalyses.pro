PRO mjo_indices_wh04_probabilities_compare_reanalyses

obs_infile='/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2009.index_values.nc'
twentyc_infile='/home/ss901165/datasets_mango/20THC_REANALYSIS/mjo_diagnostics/20thc_reanalysis.jan-dec_dmeans.1978-2008.rmm_indices.nc'
twentyc_with_noaa_infile='/home/ss901165/datasets_mango/20THC_REANALYSIS/mjo_diagnostics/20thc_reanalysis_with_noaa_olr.jan-dec_dmeans.1978-2008.rmm_indices.nc'
twentyc_with_ncep_u850_infile='/home/ss901165/datasets_mango/20THC_REANALYSIS/mjo_diagnostics/20thc_reanalysis_with_ncep_u850.jan-dec_dmeans.1978-2008.rmm_indices.nc'
twentyc_with_ncep_u200_infile='/home/ss901165/datasets_mango/20THC_REANALYSIS/mjo_diagnostics/20thc_reanalysis_with_ncep_u200.jan-dec_dmeans.1978-2008.rmm_indices.nc'
eraint_infile='/home/ss901165/datasets_mango/ERA-INTERIM/mjo_diagnostics/ERA_interim.jan-dec_dmeans.1989-2008.rmm_indices.nc'
eraint_with_noaa_infile='/home/ss901165/datasets/MJO_INDICES/noaa_eraint_obs.jan-dec_dmeans.1989-2008.rmm_indices.nc'

n_sets=8
max_years=30
earliest_year=1979
colors=['black','black','red','red','red','red','blue','blue']
styles=[0,1,0,1,2,5,0,1]
symbols=[5,5,5,4,6,7,5,4]
missing_value=-9999

probability_outside=fltarr(n_sets,max_years)
probability_outside_byphase=fltarr(n_sets,8)
mean_amplitude=fltarr(n_sets,max_years)
mean_amplitude_outside=fltarr(n_sets,max_years)
phase_ts_obs=fltarr(max_years,365)
amplitude_ts_obs=fltarr(max_years,365)
probability_correct_phase=fltarr(n_sets,max_years)
rmse_amplitude=fltarr(n_sets,max_years)
all_nyears=fltarr(n_sets)
all_start_year=fltarr(n_sets)
all_print_correlation_stddev=fltarr(n_sets)
all_legend_title=strarr(n_sets)
all_short_title=strarr(n_sets)
all_graph_offset=intarr(n_sets)
all_offset_to_eraint_start=intarr(n_sets)
all_plot_interannual=intarr(n_sets)

FOR i=0,n_sets-1 DO BEGIN
   print,i
   CASE i OF
      0 : BEGIN
         infile=obs_infile
         all_nyears(i)=30
         file_offset=4
         all_start_year(i)=1979
         all_graph_offset(i)=0
         ndays_per_year=366
         all_legend_title(i)='NOAA OLR and NCEP winds (1979-2008)'
         all_short_title(i)='obs'
         all_print_correlation_stddev(i)=1
         all_offset_to_eraint_start(i)=11
         all_plot_interannual(i)=1
      END
      1 : BEGIN
         infile=obs_infile
         all_nyears(i)=19
         file_offset=15
         all_graph_offset(i)=11
         ndays_per_year=365
         all_legend_title(i)='NOAA OLR and NCEP winds (1990-2008)'
         all_short_title(i)='obs_eraint'
         all_print_correlation_stddev(i)=1
         all_offset_to_eraint_start(i)=0
         all_plot_interannual(i)=0
      END
      2 : BEGIN
         infile=twentyc_infile
         all_nyears(i)=30
         file_offset=1
         all_start_year(i)=1979
         all_graph_offset(i)=0
         ndays_per_year=365
         all_legend_title(i)='20th Century Reanalysis V2 (1979-2008)'
         all_short_title(i)='20thc_v2'
         all_print_correlation_stddev(i)=1
         all_offset_to_eraint_start(i)=11
         all_plot_interannual(i)=1         
      END
      6 : BEGIN
         infile=eraint_infile
         all_nyears(i)=19
         file_offset=1
         all_start_year(i)=1990
         all_graph_offset(i)=11
         ndays_per_year=365
         all_legend_title(i)='ERA-Interim reanalysis (1990-2008)'
         all_short_title(i)='ERA-Int'
         all_print_correlation_stddev(i)=1
         all_offset_to_eraint_start(i)=0
         all_plot_interannual(i)=1
      END
      3 : BEGIN
         infile=twentyc_with_noaa_infile
         all_nyears(i)=30
         file_offset=1
         all_start_year(i)=1979
         all_graph_offset(i)=0
         ndays_per_year=365
         all_legend_title(i)='20th Century winds with NOAA OLR (1979-2008)'
         all_short_title(i)='20thc_v2_noaa'
         all_print_correlation_stddev(i)=1
         all_offset_to_eraint_start(i)=11
         all_plot_interannual(i)=1
      END
      4 : BEGIN
         infile=twentyc_with_ncep_u850_infile
         all_nyears(i)=30
         file_offset=1
         all_start_year(i)=1979
         all_graph_offset(i)=0
         ndays_per_year=365
         all_legend_title(i)='20th Century OLR and U200 with NCEP U850 (1980-2008)'
         all_short_title(i)='20thc_v2_ncep_u850'
         all_print_correlation_stddev(i)=1
         all_offset_to_eraint_start(i)=10
         all_plot_interannual(i)=1
      END
      5 : BEGIN
         infile=twentyc_with_ncep_u200_infile
         all_nyears(i)=30
         file_offset=1
         all_start_year(i)=1979
         all_graph_offset(i)=0
         ndays_per_year=365
         all_legend_title(i)='20th Century OLR and U850 with NCEP U200 (1980-2008)'
         all_short_title(i)='20thc_v2_ncep_u200'
         all_print_correlation_stddev(i)=1
         all_offset_to_eraint_start(i)=11
         all_plot_interannual(i)=1
      END
      7 : BEGIN
         infile=eraint_with_noaa_infile
         all_nyears(i)=19
         file_offset=1
         all_start_year(i)=1990
         all_graph_offset(i)=11
         ndays_per_year=365
         all_legend_title(i)='ERA-Interim winds with NOAA OLR (1990-2008)'
         all_short_title(i)='ERA-Int_noaa'
         all_print_correlation_stddev(i)=1
         all_offset_to_eraint_start(i)=0
         all_plot_interannual(i)=1
      END
   ENDCASE
   FOR j=0,all_nyears(i)-1 DO BEGIN
      phase_ts=REFORM(OPEN_AND_EXTRACT(infile,'phase',$
                                       offset=[file_offset+j,0],count=[1,ndays_per_year]))
      amplitude_ts=REFORM(OPEN_AND_EXTRACT(infile,'amplitude',$
                                           offset=[file_offset+j,0],count=[1,ndays_per_year]))
    
      IF TOTAL(where(phase_ts eq missing_value)) ge 0 THEN $
         phase_ts=phase_ts[where(phase_ts ne missing_value)]
      IF TOTAL(where(phase_ts ge 10000)) ge 0 THEN $
         phase_ts=phase_ts[where(phase_ts lt 10000)]
      IF TOTAL(where(amplitude_ts eq missing_value)) ge 0 THEN $
         amplitude_ts=amplitude_ts[where(amplitude_ts ne missing_value)]
      IF TOTAL(where(amplitude_ts ge 10000)) ge 0 THEN $
         amplitude_ts=amplitude_ts[where(amplitude_ts lt 10000)]
      IF j eq 0 THEN $
         print,all_short_title(i)+' number of points in 1979 is '+STRTRIM(STRING(N_ELEMENTS(amplitude_ts)),1)
      FOR k=0,7 DO $
         probability_outside_byphase(i,k)=probability_outside_byphase(i,k)+(N_ELEMENTS(where(amplitude_ts ge 1 and amplitude_ts ne missing_value and phase_ts eq k+1))/FLOAT(N_ELEMENTS(amplitude_ts))/FLOAT(all_nyears(i)))
      probability_outside(i,j)=N_ELEMENTS(where(amplitude_ts ge 1 and amplitude_ts ne missing_value))/FLOAT(N_ELEMENTS(amplitude_ts))
      mean_amplitude_outside(i,j)=MEAN(amplitude_ts[where(amplitude_ts ge 1 and amplitude_ts ne missing_value)])
      mean_amplitude(i,j)=MEAN(amplitude_ts[where(amplitude_ts ne missing_value)])
      IF i ne 0 THEN BEGIN
         probability_correct_phase(i,j)=N_ELEMENTS(where(phase_ts eq phase_ts_obs(j+all_graph_offset(i),*) and amplitude_ts ge 1 and amplitude_ts_obs(j+all_graph_offset(i),*) ge 1))/$
                                        FLOAT(N_ELEMENTS(where(amplitude_ts_obs(j+all_graph_offset(i),*) ge 1)))
         rmse_amplitude(i,j)=SQRT(MEAN((amplitude_ts-amplitude_ts_obs(j+all_graph_offset(i),*))^2))
      ENDIF ELSE BEGIN
         phase_ts_obs(j,*)=phase_ts(0:364)
         amplitude_ts_obs(j,*)=amplitude_ts(0:364)
      ENDELSE
   ENDFOR
ENDFOR

psfile='/home/ss901165/idl/mjo_indices/mjo_indices_wh04_probabilities_compare_reanalyses.freq_eachphase.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=400,XOFFSET=900,YOFFSET=1000,TFONT=2,TCHARSIZE=120,SPACE3=400
GSET,XMIN=0.5,XMAX=8.5,YMIN=0.05,YMAX=0.11,TITLE='Probability of being outside the RMM1/RMM2 unit circle for each phase'
FOR i=0,n_sets-1 DO BEGIN
   this_color=FSC_COLOR(colors(i),i+2)
   GPLOT,X=indgen(8)+1,Y=REFORM(probability_outside_byphase(i,*)),SYM=symbols(i),THICK=200,SIZE=125,COL=i+2,STYLE=styles(i)
ENDFOR
LEGEND,labels=REVERSE(all_legend_title),COL=REVERSE(indgen(n_sets)+2),LEGPOS=5,STYLE=REVERSE(styles),SYM=REVERSE(symbols)
AXES,XVALS=[1,2,3,4,5,6,7,8],YSTEP=0.01,YMINOR=0.005,YTITLE='Probability',XTITLE='RMM1/RMM2 phase',NDECS=2
PSCLOSE

psfile='/home/ss901165/idl/mjo_indices/mjo_indices_wh04_probabilities_compare_reanalyses.freq_allphases.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=400,XOFFSET=900,YOFFSET=1000,TFONT=2,TCHARSIZE=120,SPACE3=400
GSET,XMIN=1978.5,XMAX=2008.5,YMIN=0.3,YMAX=1.0,TITLE='Probability of being outside the RMM1/RMM2 unit circle for each year'
this_legend_title=strarr(N_ELEMENTS(where(all_plot_interannual eq 1)))
this_styles=intarr(N_ELEMENTS(where(all_plot_interannual eq 1)))
this_symbols=intarr(N_ELEMENTS(where(all_plot_interannual eq 1)))
j=0
FOR i=0,n_sets-1 DO BEGIN
   IF all_plot_interannual(i) eq 1 THEN BEGIN
      this_legend_title(j)=all_legend_title(i)
      this_styles(j)=styles(i)
      this_symbols(j)=symbols(i)
      this_color=FSC_COLOR(colors(i),i+2)
      GPLOT,X=indgen(all_nyears(i))+earliest_year+all_graph_offset(i),Y=REFORM(probability_outside(i,*)),SYM=symbols(i),THICK=200,SIZE=125,COL=i+2,STYLE=styles(i)
      IF all_print_correlation_stddev(i) eq 1 THEN $
         print,'Inter-annual std.dev. for '+all_short_title(i)+': '+$
               STRMID(STRTRIM(STRING(STDDEV(probability_outside(i,0:all_nyears(i)-1))),1),0,5)
      IF i ne 0 and all_print_correlation_stddev(i) eq 1 THEN $
         print,'Corr '+all_short_title(i)+' and obs: '+$
               STRMID(STRTRIM(STRING(CORRELATE(probability_outside(i,0:all_nyears(i)-1),$
                                               probability_outside(0,all_graph_offset(i):all_nyears(0)-1))),1),0,5)               
      IF i eq 2 or i eq 3 or i eq 4 or i eq 5 THEN $
         print,'Corr '+all_short_title(i)+' and obs over ERA-Int period: '+$
               STRMID(STRTRIM(STRING(CORRELATE(probability_outside(i,all_offset_to_eraint_start(i):all_nyears(i)-1),$
                                               probability_outside(0,all_offset_to_eraint_start(0):all_nyears(0)-1))),1),0,5)
      GPLOT,X=2009,Y=MEAN(probability_outside(i,all_graph_offset(i):all_nyears(i)-1)),SYM=symbols(i),THICK=200,SIZE=125,COL=i+2,/NOLINES
      j=j+1
   ENDIF
ENDFOR
LEGEND,labels=REVERSE(this_legend_title),COL=REVERSE(indgen(n_sets)+2),LEGPOS=5,STYLE=REVERSE(this_styles),SYM=REVERSE(this_symbols)
AXES,XVALS=[1979,1981,1983,1985,1987,1989,1991,1993,1995,1997,1999,2001,2003,2005,2007],YSTEP=0.1,YMINOR=0.05,YTITLE='Probability',XTITLE='Year',NDECS=2,XMINOR=indgen(30)+1979
PSCLOSE

psfile='/home/ss901165/idl/mjo_indices/mjo_indices_wh04_probabilities_compare_reanalyses.ampl_outside_allphases.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=400,XOFFSET=900,YOFFSET=1000,TFONT=2,TCHARSIZE=120,SPACE3=400
GSET,XMIN=1978.5,XMAX=2008.5,YMIN=1.1,YMAX=2.05,TITLE='Mean amplitude of RMM1/RMM2 (SQRT(RMM1^2+RMM2^2)) on days when amplitude > 1'
j=0
FOR i=0,n_sets-1 DO BEGIN
   IF all_plot_interannual(i) eq 1 THEN BEGIN
      this_legend_title(j)=all_legend_title(i)
      this_styles(j)=styles(i)
      this_symbols(j)=symbols(i)
      print,i
      this_color=FSC_COLOR(colors(i),i+2)
      GPLOT,X=indgen(all_nyears(i))+earliest_year+all_graph_offset(i),Y=REFORM(mean_amplitude_outside(i,*)),SYM=symbols(i),THICK=200,SIZE=125,COL=i+2,STYLE=styles(i)
      IF all_print_correlation_stddev(i) eq 1 THEN $
         print,'Inter-ann std.dev. in mean amp > 1 for '+all_short_title(i)+': '+$
               STRMID(STRTRIM(STRING(STDDEV(mean_amplitude_outside(i,0:all_nyears(i)-1))),1),0,5)
      IF i ne 0 and all_print_correlation_stddev(i) eq 1 THEN $
         print,'Corr of mean amp > 1 for '+all_short_title(i)+' and obs: '+$
               STRMID(STRTRIM(STRING(CORRELATE(mean_amplitude_outside(i,0:all_nyears(i)-1),$
                                               mean_amplitude_outside(0,all_graph_offset(i):N_ELEMENTS(mean_amplitude_outside(0,*))-1))),1),0,5)             
      GPLOT,X=2009,Y=MEAN(mean_amplitude_outside(i,all_graph_offset(i):all_nyears(i)-1)),SYM=symbols(i),THICK=200,SIZE=125,COL=i+2,/NOLINES
      j=j+1
   ENDIF
ENDFOR
LEGEND,labels=REVERSE(this_legend_title),COL=REVERSE(indgen(n_sets)+2),LEGPOS=11,STYLE=REVERSE(this_styles),SYM=REVERSE(this_symbols)
AXES,XVALS=[1979,1981,1983,1985,1987,1989,1991,1993,1995,1997,1999,2001,2003,2005,2007],YSTEP=0.1,YMINOR=0.05,YTITLE='Amplitude',XTITLE='Year',NDECS=2,XMINOR=indgen(30)+1979
PSCLOSE

psfile='/home/ss901165/idl/mjo_indices/mjo_indices_wh04_probabilities_compare_reanalyses.ampl_allphases.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=400,XOFFSET=900,YOFFSET=1000,TFONT=2,TCHARSIZE=120,SPACE3=400
GSET,XMIN=1978.5,XMAX=2008.5,YMIN=0.4,YMAX=1.7,TITLE='Mean amplitude of RMM1/RMM2 (SQRT(RMM1^2+RMM2^2))'
FOR i=0,n_sets-1 DO BEGIN
   IF all_plot_interannual(i) eq 1 THEN BEGIN
      this_color=FSC_COLOR(colors(i),i+2)
      GPLOT,X=indgen(all_nyears(i))+earliest_year+all_graph_offset(i),Y=REFORM(mean_amplitude(i,*)),SYM=symbols(i),THICK=200,SIZE=125,COL=i+2,STYLE=styles(i)
      IF all_print_correlation_stddev(i) eq 1 THEN $
         print,'Inter-ann std.dev. in mean amp for '+all_short_title(i)+': '+$
               STRMID(STRTRIM(STRING(STDDEV(mean_amplitude(i,0:all_nyears(i)-1))),1),0,5)
      IF i ne 0 and all_print_correlation_stddev(i) eq 1 THEN $
         print,'Corr of mean amp for '+all_short_title(i)+' and obs: '+$
               STRMID(STRTRIM(STRING(CORRELATE(mean_amplitude(i,0:all_nyears(i)-1),$
                                               mean_amplitude(0,all_graph_offset(i):N_ELEMENTS(mean_amplitude(0,*))-1))),1),0,5)  
      GPLOT,X=2009,Y=MEAN(mean_amplitude(i,all_graph_offset(i):all_nyears(i)-1)),SYM=symbols(i),THICK=200,SIZE=125,COL=i+2,/NOLINES
   ENDIF
ENDFOR
LEGEND,labels=REVERSE(all_legend_title),COL=REVERSE(indgen(n_sets)+2),LEGPOS=7,STYLE=REVERSE(styles),SYM=REVERSE(symbols)
AXES,XVALS=[1979,1981,1983,1985,1987,1989,1991,1993,1995,1997,1999,2001,2003,2005,2007],YSTEP=0.1,YMINOR=0.05,YTITLE='Amplitude',XTITLE='Year',NDECS=2,XMINOR=indgen(30)+1979
PSCLOSE

psfile='/home/ss901165/idl/mjo_indices/mjo_indices_wh04_probabilities_compare_reanalyses.prob_correct_phase.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=400,XOFFSET=900,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=400
GSET,XMIN=1978.5,XMAX=2008.5,YMIN=0.2,YMAX=0.9,TITLE='Frac of obs amplitude > 1 days when reanalysis has correct phase and amplitude > 1'
FOR i=2,n_sets-1 DO BEGIN
   this_color=FSC_COLOR(colors(i),i+2)
   GPLOT,X=indgen(all_nyears(i))+earliest_year+all_graph_offset(i),Y=REFORM(probability_correct_phase(i,*)),SYM=symbols(i),THICK=200,SIZE=125,COL=i+2,STYLE=styles(i)
   GPLOT,X=2009,Y=MEAN(probability_correct_phase(i,all_graph_offset(i):all_nyears(i)-1)),SYM=symbols(i),THICK=200,SIZE=125,COL=i+2,/NOLINES
ENDFOR
LEGEND,labels=REVERSE(all_legend_title(2:n_sets-1)),COL=REVERSE(indgen(n_sets-1)+3),LEGPOS=3,STYLE=REVERSE(styles(2:n_sets-1)),SYM=REVERSE(symbols(2:n_sets-1))
AXES,XVALS=[1979,1981,1983,1985,1987,1989,1991,1993,1995,1997,1999,2001,2003,2005,2007],YSTEP=0.1,YMINOR=0.05,YTITLE='Probability',XTITLE='Year',NDECS=2,XMINOR=indgen(30)+1979
PSCLOSE

psfile='/home/ss901165/idl/mjo_indices/mjo_indices_wh04_probabilities_compare_reanalyses.amplitude_daily_rmse.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=400,XOFFSET=900,YOFFSET=500,TFONT=2,TCHARSIZE=120,SPACE3=400
GSET,XMIN=1978.5,XMAX=2008.5,YMIN=0.0,YMAX=0.6,TITLE='RMSE in daily RMM1/RMM2 amplitude'
FOR i=2,n_sets-1 DO BEGIN
   this_color=FSC_COLOR(colors(i),i+2)
   GPLOT,X=indgen(all_nyears(i))+earliest_year+all_graph_offset(i),Y=REFORM(rmse_amplitude(i,*)),SYM=symbols(i),THICK=200,SIZE=125,COL=i+2,STYLE=styles(i)
   GPLOT,X=2009,Y=MEAN(rmse_amplitude(i,all_graph_offset(i):all_nyears(i)-1)),SYM=symbols(i),THICK=200,SIZE=125,COL=i+2,/NOLINES
ENDFOR
LEGEND,labels=REVERSE(all_legend_title(2:n_sets-1)),COL=REVERSE(indgen(n_sets-1)+3),LEGPOS=3,STYLE=REVERSE(styles(2:n_sets-1)),SYM=REVERSE(symbols(2:n_sets-1))
AXES,XVALS=[1979,1981,1983,1985,1987,1989,1991,1993,1995,1997,1999,2001,2003,2005,2007],YSTEP=0.1,YMINOR=0.05,YTITLE='Root mean squared error',XTITLE='Year',NDECS=2,XMINOR=indgen(30)+1979
PSCLOSE

STOP

END
