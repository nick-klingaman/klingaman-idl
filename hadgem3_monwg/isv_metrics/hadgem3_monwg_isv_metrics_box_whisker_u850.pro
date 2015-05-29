PRO hadgem3_monwg_isv_metrics_box_whisker_u850

eraint_n96_1020_file='/home/ss901165/datasets/ERA-INT/U850/U850.amjjaso.1989-2008.monsoon_domain.n96.anom_filter1020.nc'
eraint_n96_3050_file='/home/ss901165/datasets/ERA-INT/U850/U850.amjjaso.1989-2008.monsoon_domain.n96.anom_filter3050.nc'
ahjra_1020_file='/home/ss901165/um_output/hadgem3_monwg/ahjra/ahjra.u850.apr-oct.daily_11years.anom_filter1020.nc'
ahjra_3050_file='/home/ss901165/um_output/hadgem3_monwg/ahjra/ahjra.u850.apr-oct.daily_11years.anom_filter3050.nc'
ahsaf_1020_file='/home/ss901165/um_output/hadgem3_monwg/ahsaf/ahsaf.u850.daily_20years.anom_filter1020.nc'
ahsaf_3050_file='/home/ss901165/um_output/hadgem3_monwg/ahsaf/ahsaf.u850.daily_20years.anom_filter3050.nc'
ahhbs_1020_file='/home/ss901165/um_output/hadgem3_monwg/ahhbs/ahhbs.u850.apr-oct.daily_9years.anom_filter1020.nc'
ahhbs_3050_file='/home/ss901165/um_output/hadgem3_monwg/ahhbs/ahhbs.u850.apr-oct.daily_9years.anom_filter3050.nc'

n_metrics=6
boxes_metrics=fltarr(n_metrics,4)
boxes_metrics(0,*)=[-10,70,5,100]
boxes_metrics(1,*)=[10,80,20,100]
boxes_metrics(2,*)=[10,115,20,145]
boxes_metrics(3,*)=[10,80,20,100]
boxes_metrics(4,*)=[-10,50,30,150]
boxes_metrics(5,*)=[-10,50,30,150]
metric_names=['N EEqIO','N BoB','W NWPac','W BoB','MonDom']
titles=['30-50 day in E eq Ind Ocn (10S-5N, 70-100E)',$
        '30-50 day in Bay of Bengal (10-20N, 80-100E)',$
        '10-20 day in NW trop Pac (10-20N, 115-145E)',$
        '10-20 day in Bay of Bengal (10-20N, 80-100E)',$
        '30-50 day across domain (10S-30N, 50-150E)',$
        '10-20 day across domain (10S-30N, 50-150E)']

n_sets=4
max_years=20

width=10
xlocation=0.5
missing=-9999

metrics=fltarr(n_metrics*n_sets,max_years)
xlabels=strarr(n_metrics*n_sets)
colors=strarr(n_metrics*n_sets)

FOR i=0,n_sets-1 DO BEGIN
   CASE i OF
      0: BEGIN
         metrics_infiles=[eraint_n96_3050_file,eraint_n96_3050_file,eraint_n96_1020_file,eraint_n96_1020_file,eraint_n96_3050_file,eraint_n96_1020_file]
         start_read=[90-50/2,90-50/2,90-20/2,90-20/2,90-50/2,90-20/2]
         plot_title='ERA-INT N96'  
         varname='U'
         n_days=62
         n_years=10
         multiplier=1         
      END        
      1: BEGIN
         metrics_infiles=[ahjra_3050_file,ahjra_3050_file,ahjra_1020_file,ahjra_1020_file,ahjra_3050_file,ahjra_1020_file]
         start_read=[90-50/2,90-50/2,90-20/2,90-20/2,90-50/2,90-20/2]
         plot_title='ahjra'
         varname='u'
         n_days=60
         n_years=11
         multiplier=1.
      END
      2: BEGIN
         metrics_infiles=[ahhbs_3050_file,ahhbs_3050_file,ahhbs_1020_file,ahhbs_1020_file,ahhbs_3050_file,ahhbs_1020_file]
         start_read=[90-50/2,90-50/2,90-20/2,90-20/2,90-50/2,90-20/2]
         plot_title='ahhbs'
         varname='u'
         n_days=60
         n_years=9
         multiplier=1.
      END
      3: BEGIN
         metrics_infiles=[ahsaf_3050_file,ahsaf_3050_file,ahsaf_1020_file,ahsaf_1020_file,ahsaf_3050_file,ahsaf_1020_file]
         start_read=[180-50/2,180-50/2,180-20/2,180-20/2,180-50/2,180-20/2]
         plot_title='ahsaf'
         varname='u'
         n_days=60
         n_years=20
         multiplier=1.
      END
   ENDCASE
   
   FOR k=0,n_metrics-1 DO BEGIN
        longitude=OPEN_AND_EXTRACT(metrics_infiles(k),'longitude')
        latitude=OPEN_AND_EXTRACT(metrics_infiles(k),'latitude')
        DEFINE_BOUNDARIES,REFORM(boxes_metrics(k,*)),latitude,longitude,box_tx,/LIMIT
        n_lon=N_ELEMENTS(longitude)
        n_lat=N_ELEMENTS(latitude)
        allyears_u850_filtered=fltarr(n_lon,n_lat,n_days*n_years)
        FOR j=0,n_years-1 DO BEGIN
            thisyear_u850_filtered=REFORM(OPEN_AND_EXTRACT(metrics_infiles(k),varname,$
                                                             offset=[box_tx(1),box_tx(0),j,start_read(k)],$
                                                             count=[n_lon,n_lat,1,n_days]))*multiplier
            IF TOTAL(where(thisyear_u850_filtered ge 1E20)) gt 0 THEN $
              thisyear_u850_filtered[where(thisyear_u850_filtered ge 1E20)]=!Values.F_NaN
            allyears_u850_filtered(*,*,j*n_days:(j+1)*n_days-1)=thisyear_u850_filtered
        ENDFOR
        u850_variance=fltarr(n_years,n_lon,n_lat)
        u850_variance(*,*,*)=0.
        FOR m=0,n_lon-1 DO BEGIN
            FOR n=0,n_lat-1 DO BEGIN
                thispt_mean=MEAN(allyears_u850_filtered(m,n,*),/NaN)
                FOR p=0,n_years-1 DO BEGIN
                    FOR r=0,n_days-1 DO BEGIN
                        u850_variance(p,m,n)=u850_variance(p,m,n)+(allyears_u850_filtered(m,n,p*n_days+r)-thispt_mean)^2*1./FLOAT(n_days)
                    ENDFOR
                ENDFOR
            ENDFOR
        ENDFOR
        
        FOR j=0,n_years-1 DO $
          metrics(i+n_sets*k,j)=MEAN(SQRT(u850_variance(j,*,*)),/NaN)

;        colors(i+n_sets*k)=color_name

    ENDFOR

    IF n_years LT max_years THEN BEGIN
        FOR k=0,n_metrics-1 DO $
          metrics(i+n_sets*k,n_years:max_years-1)=missing
    ENDIF

    FOR k=0,n_metrics-1 DO $
      xlabels(i+n_sets*k)=plot_title;+' '+metric_names(k)        
    
ENDFOR

psfile='/home/ss901165/idl/hadgem3_monwg/isv_metrics/hadgem3_monwg_isv_metrics_box_whisker_u850.allinone.ps'
set_plot,'ps'
device,file=psfile,bits_per_pixel=24,set_font='Hershey',/color
BoxPlot,metrics,labels=xlabels,missing_data_value=missing,yrange=[0,9],ystyle=1,$
        ytitle='Stddev in area-avg, filtered U850 for July and Aug',thick=3,charthick=1.50,$
        xthick=3,ythick=3,rotate=45,charsize=1.20,color="black"
device,/close

range=fltarr(n_metrics,2)
range(0,*)=[0,4]
range(1,*)=[0,4]
range(2,*)=[0,4]
range(3,*)=[0,4]
range(4,*)=[0,4]
range(5,*)=[0,4]
FOR i=0,n_metrics-1 DO BEGIN
   psfile='/home/ss901165/idl/hadgem3_monwg/isv_metrics/hadgem3_monwg_isv_metrics_box_whisker_u850.part'+STRTRIM(STRING(i),1)+'.ps'
   device,file=psfile,bits_per_pixel=24,set_font='Hershey',/color
   BoxPlot,metrics(i*n_sets:(i+1)*n_sets-1,*),labels=xlabels(i*n_sets:(i+1)*n_sets-1),missing_data_value=missing,$
           yrange=REFORM(range(i,*)),ystyle=1,ytitle='Stddev in area-avg, filtered U850 for July and Aug',$
           thick=3,charthick=1.50,xthick=3,ythick=3,rotate=45,charsize=1.20,color="black",title=titles(i)
   device,/close
ENDFOR

    ; FOR k=0,n_metrics-1 DO BEGIN
;         this_metric=REFORM(metrics(k,*))
;         sorted_metric=this_metric[Sort(this_metric)]
;         IF N_ELEMENTS(sorted_metric) MOD 2 EQ 0 THEN BEGIN
;             index=N_ELEMENTS(sorted_metric)/2
;             medianData=(sorted_metric[index-1]+sorted_metric[index])/2.0
;             lowerGroup=sorted_metric[0:index-1]
;             higherGroup=sorted_metric[index:N_ELEMENTS(sorted_metric)-1]
;         ENDIF ELSE BEGIN
;             index=N_ELEMENTS(sorted_metric)/2
;             medianData=sorted_metric[index]
;             lowerGroup=sorted_metric[0:index-1]
;             higherGroup=sorted_metric[index+1:N_ELEMENTS(sorted_metric)-1]
;         ENDELSE
;         quartile_25=Median(lowerGroup,/EVEN)
;         quartile_75=Median(higherGroup,/EVEN)
;         irq=quartile_75-quartile_25
        
;         min_metric=MIN(this_metric,MAX=max_metric)
;         halfwidth=width/2.0
;         x1=xlocation-halfwidth
;         x2=xlocation+halfwidth
;         y1=quartile_25
;         y2=quartile_75
;         PLOTS,[x1,x1,x2,x2,x1],[y1,y2,y2,y1,y1]
;         PLOTS,[x1,x2],[medianData,medianData]
        
;         imax=WHERE(this_metric GT quartile_75 + (1.5*irq),maxcount)
;         IF maxcount EQ 0 THEN BEGIN
;             top=max_metric
;         ENDIF ELSE BEGIN
;             index=Value_Locate(sorted_metric,quartile_75+(1.5*irq))
;             top=sorted_metric[0 > (index) < (N_ELEMENTS(this_metric)-1)]
;         ENDELSE
        
;         imin=WHERE(this_metric LT quartile_25 - (1.5*irq),mincount)
;         IF mincount EQ 0 THEN BEGIN
;             bottom=min_metric
;         ENDIF ELSE BEGIN
;             index=Value_Locate(sorted_metric,quartile_25-(1.5*irq))
;             bottom=sorted_metric[0 > (index+1) < (N_ELEMENTS(this_metric)-1)]
;         ENDELSE
        
;         PLOTS,[xlocation,xlocation],[quartile_75,top]
;         PLOTS,[xlocation,xlocation],[quartile_25,bottom]
;         PLOTS, [xlocation - (halfwidth*0.5), xlocation + (halfwidth*0.5)], $
;           [top, top], COLOR=FSC_Color(color)
;         PLOTS, [xlocation - (halfwidth*0.5), xlocation + (halfwidth*0.5)], $
;           [bottom, bottom], COLOR=FSC_Color(color)
        
;                                 ; Draw outliners if there are any.
;         IF maxcount GT 0 THEN BEGIN
;             FOR m=0,maxcount-1 DO PLOTS, xlocation, this_metric[imax[m]];, $
;                                 ;PSYM=SymCat(9), COLOR=FSC_Color(color)
;         ENDIF
;         IF mincount GT 0 THEN BEGIN
;             FOR m=0,mincount-1 DO PLOTS, xlocation, this_metric[imin[m]];, $
;                                 ;PSYM=SymCat(9), COLOR=FSC_Color(color)
;         ENDIF
        
;     ENDFOR

STOP

END
