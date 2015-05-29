PRO qld_wet_days_distributions_terciles

; File containing timeseries of Nov-Apr mean rainfall
silo_seasmean_infile='/home/ss901165/datasets_mango/SILO/one_quarter/SILO_precip.nov-apr_smeans.1900-2008.0.25x0.25.nc'

; File containing daily timeseries of Nov-Apr rainfall
silo_dailymean_infile='/home/ss901165/datasets_mango/SILO/one_quarter/SILO_precip.nov-apr_dmeans.1900-2008.0.25x0.25.nc'

silo_nyears=107
silo_ndays_per_year=181

; Point to consider
point=[-15,145]

; Read latitude and longitude, find point of interest
silo_longitude=OPEN_AND_EXTRACT(silo_seasmean_infile,'longitude')
silo_latitude=OPEN_AND_EXTRACT(silo_seasmean_infile,'latitude')
latpt=NEAREST(silo_latitude,point(0))
lonpt=NEAREST(silo_longitude,point(1))

; Read seasonal-mean rainfall, separate into terciles
silo_seasmeans=REFORM(OPEN_AND_EXTRACT(silo_seasmean_infile,'rain',$
                                       offset=[lonpt,latpt,0],count=[1,1,silo_nyears]))
sorted_indices=SORT(silo_seasmeans)
silo_seasmeans_sorted=silo_seasmeans[sorted_indices]
upper_tercile_indices=sorted_indices(silo_nyears*2/3+1:silo_nyears-1)
n_upper=N_ELEMENTS(upper_tercile_indices)
lower_tercile_indices=sorted_indices(0:silo_nyears/3-1)
n_lower=N_ELEMENTS(lower_tercile_indices)
middle_tercile_indices=sorted_indices(silo_nyears/3:silo_nyears*2/3)
n_middle=N_ELEMENTS(middle_tercile_indices)

silo_mean_upper=MEAN(silo_seasmeans[upper_tercile_indices])
silo_mean_middle=MEAN(silo_seasmeans[middle_tercile_indices])
silo_mean_lower=MEAN(silo_seasmeans[lower_tercile_indices])

; Create arrays for daily rainfall at that point in upper, middle,
; lower terciles
silo_daily_upper=fltarr(n_upper*silo_ndays_per_year)
silo_daily_middle=fltarr(n_middle*silo_ndays_per_year)
silo_daily_lower=fltarr(n_lower*silo_ndays_per_year)

; Read daily mean rainfall for each year in each tercile, organize
; into a single array for each tercile

FOR i=0,n_lower-1 DO BEGIN
   silo_daily_lower(i*silo_ndays_per_year:(i+1)*silo_ndays_per_year-1)=$
      REFORM(OPEN_AND_EXTRACT(silo_dailymean_infile,'rain',$
                              offset=[lonpt,latpt,0,lower_tercile_indices(i)],count=[1,1,silo_ndays_per_year,1]))
ENDFOR
FOR i=0,n_middle-1 DO BEGIN
   silo_daily_middle(i*silo_ndays_per_year:(i+1)*silo_ndays_per_year-1)=$
      REFORM(OPEN_AND_EXTRACT(silo_dailymean_infile,'rain',$
                              offset=[lonpt,latpt,0,middle_tercile_indices(i)],count=[1,1,silo_ndays_per_year,1]))
ENDFOR
FOR i=0,n_upper-1 DO BEGIN
   silo_daily_upper(i*silo_ndays_per_year:(i+1)*silo_ndays_per_year-1)=$
      REFORM(OPEN_AND_EXTRACT(silo_dailymean_infile,'rain',$
                              offset=[lonpt,latpt,0,upper_tercile_indices(i)],count=[1,1,silo_ndays_per_year,1]))
ENDFOR

; Limit to only days > 1 mm/day and sort
silo_daily_lower_sorted=silo_daily_lower[where(silo_daily_lower gt 1)]
silo_daily_middle_sorted=silo_daily_middle[where(silo_daily_middle gt 1)]
silo_daily_upper_sorted=silo_daily_upper[where(silo_daily_upper gt 1)]

silo_daily_lower_sorted=silo_daily_lower_sorted[SORT(silo_daily_lower_sorted)]
silo_daily_middle_sorted=silo_daily_middle_sorted[SORT(silo_daily_middle_sorted)]
silo_daily_upper_sorted=silo_daily_upper_sorted[SORT(silo_daily_upper_sorted)]

psfile='/home/ss901165/idl/queensland/wet_days/distributions/qld_wet_days_distributions_terciles.pt'+STRTRIM(STRING(ABS(point(0))),1)+'S'+STRTRIM(STRING(ABS(point(1))),1)+'E_histogram.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=125,MARGIN=2000,SPACE2=300,XOFFSET=700,YOFFSET=500,TFONT=2,TCHARSIZE=100
GSET,XMIN=1,XMAX=105,YMIN=1,YMAX=1500,/YLOG,TITLE='Frequency of daily rainfall amounts at point '+$
     STRTRIM(STRING(ABS(point(0))),1)+'S, '+STRTRIM(STRING(ABS(point(1))),1)+'E - SILO 0.25 - Nov-Apr (1900-2008)'
AXES,XVALS=indgen(21)*5+2.5,XLABELS=STRMID(STRTRIM(STRING(indgen(21)*5+2.5),1),0,4),$
     YVALS=[1,2,3,5,7,10,15,20,30,50,70,100,150,200,300,500,700,1000,1500],YTITLE='Count',XTITLE='Daily rainfall (mm), using 5 mm bins beginning at 1 mm',NDECS=2
blue=FSC_COLOR("blue",12)
red=FSC_COLOR("red",10)
black=FSC_COLOR("black",11)
lower_hist=HISTOGRAM(silo_daily_lower_sorted,binsize=5,MIN=1,MAX=200)
middle_hist=HISTOGRAM(silo_daily_middle_sorted,binsize=5,MIN=1,MAX=200)
upper_hist=HISTOGRAM(silo_daily_upper_sorted,binsize=5,MIN=1,MAX=200)
HIST,X=indgen(40)*5+1.5,Y=lower_hist,FILLCOL=10,width=70
HIST,X=indgen(40)*5+2.5,Y=middle_hist,FILLCOL=11,width=70
HIST,X=indgen(40)*5+3.5,Y=upper_hist,FILLCOL=12,width=70

colors=[10,11,12]
sym=[1,1,1]
size=[100,100,100]
items=['Lower tercile (by seasonal total)','Middle tercile (by seasonal total)','Upper tercile (by seasonal total)']
LEGEND,labels=items,sym=sym,col=colors,size=size,length=0,LEGPOS=5

PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/wet_days/distributions/qld_wet_days_distributions_terciles.pt'+STRTRIM(STRING(ABS(point(0))),1)+'S'+STRTRIM(STRING(ABS(point(1))),1)+'E_histogram_scaled.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=125,MARGIN=2000,SPACE2=300,XOFFSET=700,YOFFSET=500,TFONT=2,TCHARSIZE=100
GSET,XMIN=1,XMAX=105,YMIN=0.005,YMAX=0.6,/YLOG,TITLE='Fractional frequency of occurrence of daily rainfall amounts for point '+$
     STRTRIM(STRING(ABS(point(0))),1)+'S, '+STRTRIM(STRING(ABS(point(1))),1)+'E - SILO 0.25 - Nov-Apr (1900-2008)'
AXES,XVALS=indgen(21)*5+2.5,YVALS=[0.005,0.007,0.01,0.02,0.03,0.05,0.07,0.10,0.15,0.20,0.30,0.50,0.60],$
     XLABELS=STRMID(STRTRIM(STRING(indgen(21)*5+2.5),1),0,4),$
     YLABELS=['0.005','0.007','0.01','0.02','0.03','0.05','0.07','0.10','0.15','0.20','0.30','0.50','0.60'],NDECS=3
blue=FSC_COLOR("blue",12)
red=FSC_COLOR("red",10)
black=FSC_COLOR("black",11)
HIST,X=indgen(40)*5+1.5,Y=lower_hist/FLOAT(N_ELEMENTS(silo_daily_lower_sorted)),FILLCOL=10,width=70
HIST,X=indgen(40)*5+2.5,Y=middle_hist/FLOAT(N_ELEMENTS(silo_daily_middle_sorted)),FILLCOL=11,width=70
HIST,X=indgen(40)*5+3.5,Y=upper_hist/FLOAT(N_ELEMENTS(silo_daily_upper_sorted)),FILLCOL=12,width=70

colors=[10,11,12]
sym=[1,1,1]
size=[100,100,100]
items=['Lower tercile (by seasonal total)','Middle tercile (by seasonal total)','Upper tercile (by seasonal total)']
LEGEND,labels=items,sym=sym,col=colors,size=size,length=0,LEGPOS=5

PSCLOSE

n_cdf_pts=50
lower_cdf_values=fltarr(n_cdf_pts)
middle_cdf_values=fltarr(n_cdf_pts)
upper_cdf_values=fltarr(n_cdf_pts)
FOR i=1,n_cdf_pts-1 DO BEGIN
   this_cdf_pt=i/FLOAT(n_cdf_pts)
   last_cdf_pt=(i-1)/FLOAT(n_cdf_pts)

   last_lower_cdf_cutoff=FLOOR(N_ELEMENTS(silo_daily_lower_sorted)*last_cdf_pt)
   last_middle_cdf_cutoff=FLOOR(N_ELEMENTS(silo_daily_middle_sorted)*last_cdf_pt)
   last_upper_cdf_cutoff=FLOOR(N_ELEMENTS(silo_daily_upper_sorted)*last_cdf_pt)

   this_lower_cdf_cutoff=FLOOR(N_ELEMENTS(silo_daily_lower_sorted)*this_cdf_pt)-1
   this_middle_cdf_cutoff=FLOOR(N_ELEMENTS(silo_daily_middle_sorted)*this_cdf_pt)-1
   this_upper_cdf_cutoff=FLOOR(N_ELEMENTS(silo_daily_upper_sorted)*this_cdf_pt)-1

;   lower_cdf_values(i)=TOTAL(silo_daily_lower_sorted(0:this_lower_cdf_cutoff))/TOTAL(silo_daily_lower_sorted)
;   middle_cdf_values(i)=TOTAL(silo_daily_middle_sorted(0:this_middle_cdf_cutoff))/TOTAL(silo_daily_middle_sorted)
;   upper_cdf_values(i)=TOTAL(silo_daily_upper_sorted(0:this_upper_cdf_cutoff))/TOTAL(silo_daily_upper_sorted)

;   lower_cdf_values(i)=TOTAL(silo_daily_lower_sorted(last_lower_cdf_cutoff:this_lower_cdf_cutoff))/TOTAL(silo_daily_lower_sorted)
;   middle_cdf_values(i)=TOTAL(silo_daily_middle_sorted(last_middle_cdf_cutoff:this_middle_cdf_cutoff))/TOTAL(silo_daily_middle_sorted)
;   upper_cdf_values(i)=TOTAL(silo_daily_upper_sorted(last_upper_cdf_cutoff:this_upper_cdf_cutoff))/TOTAL(silo_daily_upper_sorted)

   lower_cdf_values(i)=MEAN(silo_daily_lower_sorted(last_lower_cdf_cutoff:this_lower_cdf_cutoff))
   middle_cdf_values(i)=MEAN(silo_daily_middle_sorted(last_middle_cdf_cutoff:this_middle_cdf_cutoff))
   upper_cdf_values(i)=MEAN(silo_daily_upper_sorted(last_upper_cdf_cutoff:this_upper_cdf_cutoff))

ENDFOR
   
psfile='/home/ss901165/idl/queensland/wet_days/distributions/qld_wet_days_distributions_terciles.pt'+STRTRIM(STRING(ABS(point(0))),1)+'S'+STRTRIM(STRING(ABS(point(1))),1)+'E_cdf_scaled.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=2500,SPACE2=300,XOFFSET=700,YOFFSET=500,TFONT=2,TCHARSIZE=100
GSET,XMIN=0,XMAX=100,YMIN=1,YMAX=100,/YLOG
AXES,XSTEP=10,/NORIGHT,YTITLE='Daily total rainfall (mm)',XTITLE='Percentile (based on frequency of occurrence)',$
     YVALS=[1,2,3,5,7,10,15,20,30,50,70,100]
blue=FSC_COLOR("blue",12)
red=FSC_COLOR("red",10)
black=FSC_COLOR("black",11)
GPLOT,X=(indgen(n_cdf_pts))*100/n_cdf_pts+100/(2*n_cdf_pts),Y=lower_cdf_values,THICK=150,COL=10,SYM=5,/NOLINE
GPLOT,X=(indgen(n_cdf_pts))*100/n_cdf_pts+100/(2*n_cdf_pts),Y=middle_cdf_values,THICK=150,COL=11,SYM=5,/NOLINE
GPLOT,X=(indgen(n_cdf_pts))*100/n_cdf_pts+100/(2*n_cdf_pts),Y=upper_cdf_values,THICK=150,COL=12,SYM=5,/NOLINE
GSET,XMIN=0,XMAX=100,YMIN=1,YMAX=2,TITLE='Mean intensity within percentiles, defined separately for each tercile, for point '+$
     STRTRIM(STRING(ABS(point(0))),1)+'S, '+STRTRIM(STRING(ABS(point(1))),1)+'E - SILO 0.25 - Nov-Apr (1900-2008)'
AXES,XSTEP=10,YSTEP=0.1,/ONLYRIGHT,NDECS=2,YTITLE='Ratio of wet seasons to dry seasons'
purple=FSC_COLOR("purple",13)
GPLOT,X=(indgen(n_cdf_pts))*100/n_cdf_pts+100/(2*n_cdf_pts),Y=upper_cdf_values/lower_cdf_values,THICK=150,COL=13,SYM=5,/NOLINE
GPLOT,X=(indgen(n_cdf_pts))*100/n_cdf_pts+100/(2*n_cdf_pts),Y=REPLICATE(MEAN(silo_daily_upper_sorted)/MEAN(silo_daily_lower_sorted),n_cdf_pts),$
      THICK=150,COL=13,SYM=3,SIZE=0,/NOLINE

colors=[13,13,10,11,12]
sym=[3,5,5,5,5]
size=[0,100,100,100,100]
items=['Mean increase in upper tercile, relative to lower tercile','Ratio of upper tercile to lower tercile',$
       'Lower tercile (by seasonal total)','Middle tercile (by seasonal total)','Upper tercile (by seasonal total)']
LEGEND,labels=items,sym=sym,col=colors,size=size,length=0,LEGPOS=5

PSCLOSE

STOP

END

   
