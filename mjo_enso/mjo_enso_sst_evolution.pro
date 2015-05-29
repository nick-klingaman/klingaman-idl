PRO mjo_enso_sst_evolution

; Plot timeseries of NINO SST anomaly evolution
; for set(s) of years.

; Number of sets of years to plot
n_sets=2

nino_infile='/home/ss901165/datasets/NINO/nino3_hadisst.jan-dec_mmeans.1871-2008.nc'
var_name='NINO3'
start_year=1871

; Number of centre month (0 = January)
centre_month=6
; Number of months to plot before and after centre month
n_before=8
n_after=8
month_names=['Nov-1','Dec-1','Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov',$
             'Dec','Jan+1','Feb+1','Mar+1']

max_nyears=50
enso_evolution=fltarr(n_sets,max_nyears,n_before+n_after+1)
composite_enso_evolution=fltarr(n_sets,n_before+n_after+1)

all_colors=strarr(n_sets)
all_years=intarr(n_sets,max_nyears)
all_nyears=intarr(n_sets)
all_set_names=strarr(n_sets)
FOR i=0,n_sets-1 DO BEGIN
   plot_subsample=0
   CASE i OF 
      0 : BEGIN
         years=[1918,1925,1930,1965,1994,1997,2002,2006]
         all_colors(i)='red'
         all_set_names(i)='MJO El Nino'  ; This will appear in the legend on the plot
         subsample=0
      END
      1 : BEGIN
         years=[1905,1911,1913,1923,1940,1941,1951,1957,1963,1968,1969,1972,1976,1977,1982,1986,1987,1991,2004]
         all_colors(i)='blue'
         all_set_names(i)='non-MJO El Nino'; This will appear in the legend on the plot
         subsample=1
         n_subsamples=1000
         subsample_size=8
      END
   ENDCASE
   n_years=N_ELEMENTS(years)
   all_years(i,0:n_years-1)=years
   all_nyears(i)=n_years

   allyears_enso=OPEN_AND_EXTRACT(nino_infile,var_name)
   trend=REGRESS(indgen(N_ELEMENTS(allyears_enso)),allyears_enso)   
   clim_mmean=fltarr(12)
   FOR j=0,11 DO $ 
      clim_mmean(j)=MEAN(allyears_enso(j:N_ELEMENTS(allyears_enso)-1:12))

   FOR j=0,n_years-1 DO BEGIN
      offset=(years(j)-start_year)*12+centre_month-n_before
      enso_evolution(i,j,*)=OPEN_AND_EXTRACT(nino_infile,var_name,$
                                           offset=[offset],count=[n_before+n_after+1])
      FOR k=0,n_before+n_after DO BEGIN
         this_month=(centre_month-n_before+k)
         IF this_month ge 12 THEN this_month=this_month-12
         IF this_month lt 0 THEN this_month=this_month+12
         enso_evolution(i,j,k)=enso_evolution(i,j,k)-clim_mmean(this_month)-$
                             trend(0)*(offset+k)
      ENDFOR
   ENDFOR
   FOR j=0,n_before+n_after DO $
      composite_enso_evolution(i,j)=MEAN(enso_evolution(i,0:n_years-1,j))
   IF subsample eq 1 THEN BEGIN
      subsample_means=fltarr(n_before+n_after+1,n_subsamples)
      random=RANDOMU(seed,subsample_size*(n_before+n_after+1)*LONG(n_subsamples))
      index=FIX(n_years*random)
      position=LONG(0)
      FOR m=0,n_before+n_after DO BEGIN
         temp_enso=REFORM(enso_evolution(i,0:n_years-1,m))
         FOR j=0,n_subsamples-1 DO BEGIN
            subsample_means(m,j)=MEAN(temp_enso[index(LONG(position):LONG(position)+subsample_size-1)])
            position=LONG(position)+subsample_size
         ENDFOR
      ENDFOR
      plot_subsample=1
   ENDIF
ENDFOR

psfile='/home/ss901165/idl/mjo_enso/mjo_enso_sst_evolution.ps'
PSOPEN,file=psfile,TFONT=2,CHARSIZE=120

GSET,XMIN=0,XMAX=n_before+n_after+1,YMAX=3.3,YMIN=-1.5,$
     TITLE='Evolution of SST anomalies in the '+var_name+' region'
labels=strarr(n_sets)
thick=intarr(n_sets)
style=intarr(n_sets)
style(*)=0
FOR i=0,n_sets-1 DO BEGIN
;   FOR j=0,all_nyears(i)-1 DO $
;      GPLOT,X=indgen(n_before+n_after)+0.5,Y=REFORM(enso_evolution(i,j,*)),$
;            THICK=50,COL=FSC_COLOR(all_colors(i*2))
   FOR j=0,n_before+n_after DO BEGIN
      temp=REFORM(enso_evolution(i,0:all_nyears(i)-1,j))
      sorted=SORT(temp)
      EBAR,X=j+0.375+0.25*i,BOX=[temp(sorted(0)),temp(sorted(N_ELEMENTS(temp)/4)),$
                              temp(sorted(N_ELEMENTS(temp)/2)),$
                              temp(sorted(N_ELEMENTS(temp)*3/4)),temp(sorted(N_ELEMENTS(temp)-1))],$
           COL=FSC_COLOR(all_colors(i))
   ENDFOR

   GPLOT,X=indgen(n_before+n_after+1)+0.5,$
         Y=REFORM(composite_enso_evolution(i,*)),$
         THICK=150,COL=FSC_COLOR(all_colors(i))
   GPLOT,X=[0,n_before+n_after+1],Y=[0,0],COL=FSC_COLOR('black'),STYLE=1
   labels(i)='Mean of '+all_set_names(i)+' years'
   thick(i)=150
ENDFOR
IF plot_subsample eq 1 THEN BEGIN
   confidence=fltarr(n_before+n_after+1,2)
   FOR j=0,n_before+n_after DO BEGIN
      temp=REFORM(subsample_means(j,*))
      sorted=SORT(temp)
      confidence(j,0)=temp(sorted(FLOOR(n_subsamples*0.05)))
      confidence(j,1)=temp(sorted(FLOOR(n_subsamples*0.95)))
   ENDFOR
   GPLOT,X=indgen(n_before+n_after+1)+0.5,$
         Y=REFORM(confidence(*,0)),STYLE=2,COL=FSC_COLOR('black')
   GPLOT,X=indgen(n_before+n_after+1)+0.5,$
         Y=REFORM(confidence(*,1)),STYLE=2,COL=FSC_COLOR('black')
   labels=[labels,'90% confidence interval']
   all_colors=[all_colors,'black']
   thick=[thick,100]
   style=[style,2]
ENDIF

AXES,XVALS=indgen(n_before+n_after+1)+0.5,XLABELS=month_names,YSTEP=0.3,YMINOR=0.1,$
     XTITLE='Month (-1 indicates one year before; +1 indicates one year later)',$
     YTITLE=var_name+' SST anomaly (detrended and with long-term mean removed)',$
     NDECS=1,ORIENTATION=30
GLEGEND,labels=REVERSE(labels),COL=REVERSE(FSC_COLOR(all_colors)),$
        THICK=REVERSE(thick),LEGPOS=1,STYLE=REVERSE(style)
PSCLOSE      

STOP

END
