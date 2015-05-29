PRO qld_higem_eots_smeans_eafeb_replot
  
; Replot timeseries of Queensland rainfall EOTs without having to recompute 
; the blasted things themselves.

n_seasons=4
n_eots=4
n_years=149
higem_offset=0
higem_year_range='h9-w8'
sig_level=0.198
trend_sig_level=0.371
nino_start=0

;windows=[10,20,30,40,50]
;signif_levels=[0.602,0.433,0.355,0.304,0.273]
windows=[10,30,50]
signif_levels=[0.602,0.355,0.273]
n_windows=N_ELEMENTS(windows)
items=strarr(n_windows)
FOR i=0,n_windows-1 DO $
   items(i)='Centred window of '+STRTRIM(STRING(windows(i)+1),1)+' years'

mylevs=['-0.54','-0.42','-0.30','-0.18','-0.06','0.06','0.18','0.30','0.42','0.54','0.66','0.78','0.90']

FOR i=0,n_seasons-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         season_name='dec-feb'
         ymin=[-400,-350,-250,-500]
         ymax=[600,550,450,700]
         ystep=[100,100,50,100]
         xtitle='Year at beginning of December-February period'
      END
      1 : BEGIN
         season_name='mar-may'
         xtitle='Year'
         ymin=[-350,-200,-150,-500]
         ymax=[850,400,450,1400]
         ystep=[100,50,50,100]
      END
      2 : BEGIN
         season_name='jun-aug'         
         ymin=[-50,-200,-75,-75]
         ymax=[200,400,200,225]
         ystep=[25,50,25,25]
      END
      3 : BEGIN
         season_name='sep-nov'
         ymin=[-100,-200,-75,-175]
         ymax=[300,200,125,175]
         ystep=[25,25,25,25]
      END
   ENDCASE

   infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.'+season_name+'_smeans.'+higem_year_range+'.eots.nc'
  
   nino_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.'+season_name+'_smeans.h9-w8.nino_indices.nc' 
   nino4_ts=OPEN_AND_EXTRACT(nino_infile,'nino4',offset=[nino_start],count=[n_years])

;   ipo_infile='/home/ss901165/datasets/HADISST/hadisst_ipo.'+season_name+'_smeans.1871-2008.nc'
;   ipo_ts=OPEN_AND_EXTRACT(ipo_infile,'ipo_index',offset=[nino_start],count=[n_years])

   eots_ts=OPEN_AND_EXTRACT(infile,'loading',$
                            offset=[0,0],count=[n_years,n_eots])
   longitude=OPEN_AND_EXTRACT(infile,'longitude')
   latitude=OPEN_AND_EXTRACT(infile,'latitude')
   n_lon=N_ELEMENTS(longitude)
   n_lat=N_ELEMENTS(latitude)
   eots_pattern=OPEN_AND_EXTRACT(infile,'spatial_pattern',$
                                offset=[0,0,0],count=[n_lon,n_lat,n_eots])
   eots_expvar=OPEN_AND_EXTRACT(infile,'variance_explained',$
                                offset=[0],count=[n_eots])

   FOR j=0,n_eots-1 DO BEGIN
      this_eot_str=STRTRIM(STRING(j+1),1)
      
      running_trends=fltarr(n_years-29)
      running_correlations=fltarr(n_years-29)
      temp=REGRESS(indgen(n_years),REFORM(eots_ts(*,j)),$
                   CORRELATION=correlation)      
      FOR k=29,n_years-1 DO BEGIN
         temp=REGRESS(indgen(30),REFORM(eots_ts(k-29:k,j)),$
                      CORRELATION=running)
         running_correlations(k-29)=running
         running_trends(k-29)=temp(0)
      ENDFOR

      psfile='/home/ss901165/idl/queensland/higem/eots/qld_higem_eots_smeans_eafeb.'+season_name+'_smeans.eot'+this_eot_str+'_ts.'+higem_year_range+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1800,SPACE2=300,XOFFSET=400,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=112,/PORTRAIT
      GSET,XMIN=0,XMAX=150,YMIN=ymin(j),YMAX=ymax(j)
           ;TITLE='Timeseries of EOT'+this_eot_str+' for HiGEM control, using seasonal means for '+season_name+' '+higem_year_range+''
      black=FSC_COLOR("black",9)
      HIST,X=indgen(n_years)+higem_offset,Y=REFORM(eots_ts(*,j)),COL=9,WIDTH=30,FILLCOL=FSC_COLOR("black")
      AXES,XSTEP=10,YSTEP=ystep(j),XMINOR=2,YMINOR=ystep(j)/2.,YTITLE='EOT timeseries',XTITLE=xtitle
      GPLOT,X=indgen(n_years)+higem_offset,Y=REPLICATE(0,n_years),STYLE=1
      red=FSC_COLOR("red",10)
      smoothed=SMOOTH(REFORM(eots_ts(*,j)),11)
      GPLOT,X=indgen(n_years-10)+higem_offset+5,Y=smoothed(5:n_years-6),STYLE=0,THICK=150,COL=10
      FOR k=0,n_years-31 DO $
         IF ABS(running_correlations(k)) gt trend_sig_level THEN $
            GPLOT,X=higem_offset+k+30,Y=ymin(j)*0.95,SYM=3,SIZE=100,COL=10
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/higem/eots/qld_higem_eots_smeans_eafeb.'+season_name+'_smeans.eot'+this_eot_str+'_nino4.'+higem_year_range+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=2500,SPACE2=300,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=112,SPACE3=300
      GSET,XMIN=0,XMAX=150,YMIN=-1,YMAX=1,$
           TITLE='Instantaneous Nino 4 correlations with '+season_name+' EOT '+this_eot_str+' for '+higem_year_range+''
      GPLOT,X=indgen(n_years)+higem_offset,Y=REPLICATE(0,n_years),STYLE=1
;      GPLOT,X=1905,Y=0.9,ALIGN=0,TEXT='Explains '+STRMID(STRTRIM(STRING(higem_smeans_eots_expvar(this_eot)*100.),1),0,5)+$
;            '% of domain space-time variance'
;      GPLOT,X=1905,Y=0.85,ALIGN=0,TEXT='Correlation with seasonal-mean ('+season_name+') NINO4: '+$
;            STRMID(STRTRIM(STRING(CORRELATE(REFORM(eots_ts(*,j)),nino4_ts)),1),0,5)
;      GPLOT,X=1905,Y=0.8,ALIGN=0,TEXT='Correlation with seasonal-mean ('+season_name+') NINO3.4: '+$
;            STRMID(STRTRIM(STRING(CORRELATE(REFORM(eots_ts(*,j)),nino34_ts)),1),0,5)
;      GPLOT,X=1905,Y=0.75,ALIGN=0,TEXT='Correlation with seasonal-mean ('+season_name+') NINO3: '+$
;            STRMID(STRTRIM(STRING(CORRELATE(REFORM(eots_ts(*,j)),nino3_ts)),1),0,5)
      
      this_eot_corr_nino4_windows=fltarr(n_windows,n_years)
      this_eot_corr_nino4_windows_signif=fltarr(n_windows,n_years)
      this_eot_corr_nino4_windows_signif(*,*)=!Values.F_NaN
      this_eot_corr_nino4_windows(*,*)=!Values.F_NaN
      FOR k=0,n_windows-1 DO BEGIN
         FOR m=windows(k)/2,n_years-windows(k)/2-1 DO BEGIN
            this_eot_corr_nino4_windows(k,m)=CORRELATE(REFORM(eots_ts(m-windows(k)/2:m+windows(k)/2,j)),nino4_ts(m-windows(k)/2:m+windows(k)/2))
            IF ABS(this_eot_corr_nino4_windows(k,m)) gt signif_levels(k) THEN $
               this_eot_corr_nino4_windows_signif(k,m)=this_eot_corr_nino4_windows(k,m)
         ENDFOR
      ENDFOR
      print,season_name+' EOT'+STRTRIM(STRING(j+1),1)+' '+STRTRIM(STRING(CORRELATE(REFORM(eots_ts(*,j)),nino4_ts(*))),1)
      colors=['red','purple','blue']
      FOR k=0,n_windows-1 DO BEGIN
         GPLOT,X=indgen(n_years)+higem_offset,Y=REFORM(this_eot_corr_nino4_windows(k,*)),COL=FSC_COLOR(colors(k)),THICK=150
         GPLOT,X=indgen(n_years)+higem_offset,Y=REFORM(this_eot_corr_nino4_windows_signif(k,*)),COL=FSC_COLOR(colors(k)),/NOLINES,SYM=2,SIZE=70
      ENDFOR
      AXES,YSTEP=0.2,NDECS=2,XSTEP=10,YTITLE='Correlation with NINO 4 index (HadISST)',XTITLE='Year at centre of window',XMINOR=5

;      GSET,XMIN=0,XMAX=150,YMIN=-3.0,YMAX=3.0
;      ipo_smoothed=SMOOTH(ipo_ts,11)
;      ipo_smoothed=ipo_smoothed(5:n_years-6)
;      GPLOT,X=indgen(n_years-11)+higem_offset+5,Y=ipo_smoothed,COL=FSC_COLOR("black")
;      AXES,YSTEP=0.5,/ONLYRIGHT,NDECS=1,YTITLE='11-year running mean of IPO index'
;      print,CORRELATE(ipo_smoothed,REFORM(this_eot_corr_nino4_windows(0,5:n_years-6)))
;      STOP

      GLEGEND,labels=REVERSE(items),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=1
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/higem/eots/qld_higem_eots_smeans_eafeb.'+season_name+'_smeans.eot'+this_eot_str+'.'+higem_year_range+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1800,SPACE2=300,XOFFSET=400,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=112,/PORTRAIT
      CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs)+3,MIN=4,white=[7]
;   CS, COLS=[93, 123, 91, 267, 272, 367, 409, 419, 407]
      MAP,LONMIN=138,LONMAX=154,LATMIN=-29,LATMAX=-10,/hires
      LEVS,MANUAL=mylevs
      CON,FIELD=REFORM(eots_pattern(*,*,j)),X=longitude,Y=latitude,$
          TITLE='EOT'+this_eot_str+' for HiGEM control, using seasonal means for '+season_name+' '+higem_year_range+'',/NOLINELABELS
      thiseot_max=MAX(REFORM(eots_pattern(*,*,j)),/NaN)
      ;print,thiseot_max
      FOR m=0,n_lon-1 DO BEGIN
         FOR n=0,n_lat-1 DO BEGIN
            IF ABS(eots_pattern(m,n,j)) gt sig_level THEN $
               GPLOT,X=longitude(m),Y=latitude(n),SYM=3,SIZE=30
            IF (eots_pattern(m,n,j)) eq thiseot_max THEN BEGIN
               central_lon=longitude(m)
               central_lat=latitude(n)
            ENDIF
         ENDFOR
      ENDFOR
      black=FSC_COLOR("black",2)
      GPLOT,X=central_lon,Y=central_lat,SYM=8,COL=2
      GPLOT,X=149,Y=-12,TEXT='Explains '+STRMID(STRTRIM(STRING(eots_expvar(j)*100.),1),0,5)+$
            '% of domain space-time variance'
      PSCLOSE,/NOVIEW

   ENDFOR
ENDFOR

STOP
END

