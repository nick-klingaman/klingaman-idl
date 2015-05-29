PRO qld_iacorr_regions_with_nino

; Using sliding windows of many lengths to compute the correlations
; between area-averaged rainfall in pre-defined regions of Queensland
; with Nino indices

; Timeseries of observed NINO values
obs_nino_infile='/home/ss901165/datasets/NINO/nino4_hadisst.may-apr_ameans.1871-2008.nc'
obs_nino_start_year=1871
obs_nino_end_year=2008
obs_nino_nyears=obs_nino_end_year-obs_nino_start_year+1

; Timeseries of Australian rainfall from SILO, area-averaged by region
silo_precip_infile='/home/ss901165/datasets_mango/SILO/one_quarter/SILO_precip.may-apr_ameans.1900-2008.0.25x0.25.qld_region_aavg.nc'
silo_start_year=1900
silo_end_year=2007
silo_nyears=silo_end_year-silo_start_year+1

; Read number of regions from SILO input file
n_regions=N_ELEMENTS(OPEN_AND_EXTRACT(silo_precip_infile,'region'))

; Read NINO indices
obs_nino_offset=(silo_start_year-obs_nino_start_year)
obs_nino_count=silo_nyears
obs_nino4_amean=OPEN_AND_EXTRACT(obs_nino_infile,'NINO4',offset=[obs_nino_offset],count=[obs_nino_count])

; Read timeseries of area-averaged rainfall
silo_precip_region_aavg=OPEN_AND_EXTRACT(silo_precip_infile,'rain_aavg_region',$
                                         offset=[0,0],count=[n_regions,silo_nyears])
silo_precip_qld_aavg=OPEN_AND_EXTRACT(silo_precip_infile,'rain_aavg_allqld',$
                                      offset=[0],count=[silo_nyears])

windows=[10,20,30,40,60,80,100]
n_windows=N_ELEMENTS(windows)
windows_strings=strarr(n_windows)
FOR i=0,n_windows-1 DO $
   windows_strings(i)='Using '+STRTRIM(STRING(windows(i)+1),1)+'-year centred window'
windows_strings_short=strarr(n_windows)
FOR i=0,n_windows-1 DO $
   windows_strings_short(i)=STRTRIM(STRING(windows(i)+1),1)+' years'

regions_corr_windows=fltarr(n_regions,n_windows,silo_nyears)
regions_precipvar_windows=fltarr(n_regions,n_windows,silo_nyears)

FOR k=0,n_regions-1 DO BEGIN
   FOR i=0,n_windows-1 DO BEGIN
      FOR j=windows(i)/2,silo_nyears-1-windows(i)/2 DO BEGIN
         regions_corr_windows(k,i,j)=CORRELATE(obs_nino4_amean(j-windows(i)/2:windows(i)/2+j),$
                                               silo_precip_region_aavg(k,j-windows(i)/2:windows(i)/2+j))          
         regions_precipvar_windows(k,i,j)=STDDEV(silo_precip_region_aavg(k,j-windows(i)/2:windows(i)/2+j))
      ENDFOR
   ENDFOR
ENDFOR

obs_ninovar_windows=fltarr(n_windows,silo_nyears)
FOR i=0,n_windows-1 DO BEGIN
   FOR j=windows(i)/2,silo_nyears-1-windows(i)/2 DO $
      obs_ninovar_windows(i,j)=STDDEV(obs_nino4_amean(j-windows(i)/2:windows(i)/2+j))
ENDFOR

FOR k=0,n_regions-1 DO BEGIN
   psfile='/home/ss901165/idl/queensland/interannual_correlations/qld_iacorr_regions_with_nino.sliding_windows.may-apr_ameans.region'+STRTRIM(STRING(k+1),1)+'_nino4.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2500,SPACE2=200,XOFFSET=200,YOFFSET=0,TFONT=2,TCHARSIZE=100,SPACE3=300,SPACING=500,YPLOTS=3

   POS,YPOS=3
   CS,SCALE=26,NCOLS=n_windows,/REV
   black=FSC_COLOR("black",1+n_windows)
   GSET,XMIN=1900,XMAX=1900+silo_nyears,YMIN=-1.0,YMAX=0.8
   AXES,XSTEP=10,YSTEP=0.2,XTITLE='Year in SILO dataset (1900-2007)',$
        YTITLE='Corr reg '+STRTRIM(STRING(k+1),1)+' & Nino 4 (May-Apr)',NDECS=2,/NORIGHT,/NOUPPER
   FOR i=0,n_windows-1 DO $
      GPLOT,X=1900+indgen(silo_nyears-windows(i))+windows(i)/2,$
      Y=REFORM(regions_corr_windows(k,i,windows(i)/2:silo_nyears-1-windows(i)/2)),COL=i+2,THICK=150
   GPLOT,X=1900+indgen(silo_nyears),Y=REPLICATE(0,silo_nyears),STYLE=1

   POS,YPOS=2

   GSET,XMIN=1900,XMAX=1900+silo_nyears,YMIN=0,YMAX=0.7
   FOR i=0,n_windows-1 DO $
      GPLOT,X=1900+indgen(silo_nyears-windows(i))+windows(i)/2,$
      Y=REFORM(obs_ninovar_windows(i,windows(i)/2:silo_nyears-1-windows(i)/2)),COL=i+2,THICK=150,STYLE=0

   AXES,XSTEP=10,/ONLYLEFT,YTITLE='Stdev. Nino 4',YVALS=['0.10','0.20','0.30','0.40','0.50','0.60']

   GSET,XMIN=1900,XMAX=1900+silo_nyears,YMIN=0,YMAX=1.5
   FOR i=0,n_windows-1 DO $
      GPLOT,X=1900+indgen(silo_nyears-windows(i))+windows(i)/2,$
      Y=REFORM(regions_precipvar_windows(k,i,windows(i)/2:silo_nyears-1-windows(i)/2)),COL=i+2,THICK=150,STYLE=2

   AXES,XSTEP=10,/ONLYRIGHT,YTITLE='Stdev. May-Apr rain',NDECS=2,YVALS=['0.20','0.40','0.60','0.80','1.00','1.20','1.40'],/NOUPPER
   AXES,XSTEP=10,YSTEP=0.2,/NOLEFT,/NORIGHT,/NOUPPER

   LEGEND,labels=REVERSE(windows_strings),COL=REVERSE(indgen(n_windows)+2),LEGXOFFSET=2000,LEGYOFFSET=13500

   POS,YPOS=1
   
   GSET,XMIN=0,XMAX=n_windows,YMIN=-1,YMAX=1.5
   FOR i=0,n_windows-1 DO BEGIN
      GPLOT,X=i+0.2,Y=CORRELATE(regions_precipvar_windows(k,i,windows(i)/2:silo_nyears-1-windows(i)/2),$
                                regions_corr_windows(k,i,windows(i)/2:silo_nyears-1-windows(i)/2)),$
            COL=i+2,THICK=150,SYM=6
      GPLOT,X=i+0.4,Y=CORRELATE(regions_precipvar_windows(k,i,windows(i)/2:silo_nyears-1-windows(i)/2),$
                                obs_ninovar_windows(i,windows(i)/2:silo_nyears-1-windows(i)/2)),$
            COL=i+2,THICK=150,SYM=3
   ENDFOR
   GPLOT,X=indgen(n_windows+1),Y=REPLICATE(0,n_windows+1),STYLE=1
   AXES,XVALS=indgen(n_windows)+0.5,XLABELS=windows_strings_short,YVALS=['-1.00','-0.75','-0.50','-0.25','0.00','0.25','0.50','0.75','1.00'],$
        NDECS=2,/NORIGHT,/NOUPPER,$
        XTITLE='Length of window',YTITLE='Correlation coefficient'
 
   GSET,XMIN=0,XMAX=n_windows,YMIN=0,YMAX=0.4
   FOR i=0,n_windows-1 DO BEGIN
      GPLOT,X=i+0.6,Y=STDDEV(regions_corr_windows(k,i,windows(i)/2:silo_nyears-1-windows(i)/2)),COL=i+2,THICK=150,SYM=4
      GPLOT,X=i+0.8,Y=STDDEV(regions_precipvar_windows(k,i,windows(i)/2:silo_nyears-1-windows(i)/2)),COL=i+2,THICK=150,SYM=5
   ENDFOR

   AXES,XVALS=indgen(n_windows)+0.5,YVALS=['0.00','0.10','0.20','0.30','0.40'],YTITLE='Standard deviation',/ONLYRIGHT,NDECS=2
   LEGEND,labels=REVERSE(['Correlation of windowed corr with NINO4 and std.dev. of rain (left)','Correlation of windowed std.dev. of rain and std.dev. of NINO4','Std.dev. of windowed correlation (right)','Std.dev. of windowed std.dev. of rain (right)']),$
          SYM=REVERSE([6,3,4,5]),LENGTH=0,LEGXOFFSET=-7000,LEGYOFFSET=18500

   PSCLOSE
ENDFOR
STOP

END
