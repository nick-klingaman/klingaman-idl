PRO qld_eots_smeans_silo025_replot_ts
  
; Replot timeseries of Queensland rainfall EOTs without having to recompute 
; the blasted things themselves.

n_seasons=4
n_eots=3
n_years=111
silo_start_year=1900
silo_start_year_str=STRTRIM(STRING(silo_start_year),1)
sig_level=0.198
trend_sig_level=0.361
;nino_start=silo_start_year-1871

;windows=[10,20,30,40,50]
;signif_levels=[0.602,0.433,0.355,0.304,0.273]
windows=[10,30,50]
signif_levels=[0.602,0.355,0.273]
n_windows=N_ELEMENTS(windows)
items=strarr(n_windows)
FOR i=0,n_windows-1 DO $
   items(i)='Centred window of '+STRTRIM(STRING(windows(i)+1),1)+' years'

mylevs=['-0.42','-0.30','-0.18','-0.06','0.06','0.18','0.30','0.42','0.54','0.66','0.78','0.90']

FOR i=0,n_seasons-1 DO BEGIN
   CASE i OF
      1 : BEGIN
         season_name='dec-feb'
         ymin=[-400,-500,-300]
         ymax=[1100,600,500]
         ystep=[100,100,50]
         xtitle='Year at beginning of December-February period'
      END
      0 : BEGIN
         season_name='mar-may'
         xtitle='Year'
         ymin=[-450,-200,-200]
         ymax=[600,300,450]
         ystep=[100,50,50]
      END
      2 : BEGIN
         season_name='jun-aug'         
         ymin=[-100,-200,-75]
         ymax=[250,450,150]
         ystep=[25,50,25]
      END
      3 : BEGIN
         season_name='sep-nov'
         ymin=[-100,-125,-125]
         ymax=[300,200,275]
         ystep=[25,25,25]
      END
   ENDCASE

   infile='/home/ss901165/datasets_mango/SILO/one_quarter/SILO.'+season_name+'_smeans.1900-2010.eots.nc'
   
   ;nino4_infile='/home/ss901165/datasets/NINO/nino4_hadisst.'+season_name+'_smeans.1871-2010.nc'
   ;nino4_ts=OPEN_AND_EXTRACT(nino4_infile,'NINO4',offset=[nino_start],count=[n_years])

   ;ipo_infile='/home/ss901165/datasets/HADISST/hadisst_ipo.'+season_name+'_smeans.1871-2008.nc'
   ;ipo_ts=OPEN_AND_EXTRACT(ipo_infile,'ipo_index',offset=[nino_start],count=[n_years])

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
      
      running_trends=fltarr(n_years-30)
      running_correlations=fltarr(n_years-30)
      temp=REGRESS(indgen(n_years),REFORM(eots_ts(*,j)),$
                   CORRELATION=correlation)      
      FOR k=15,n_years-16 DO BEGIN
         temp=REGRESS(indgen(31),REFORM(eots_ts(k-15:k+15,j)),$
                      CORRELATION=temp2)
         running_correlations(k-15)=temp2(0)
         running_trends(k-15)=temp(0)
      ENDFOR

      ;psfile='/home/ss901165/idl/queensland/eots/qld_eots_smeans_silo025.'+season_name+'_smeans.eot'+this_eot_str+'_ts.'+silo_start_year_str+'-2010.bw.ps'
      psfile='/home/ss901165/idl/queensland/eots/qld_eots_smeans_silo025.'+season_name+'_smeans.eot'+this_eot_str+'_ts.'+silo_start_year_str+'-2010.bw.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=160,MARGIN=2500,SPACE2=300,XOFFSET=400,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=112
      GSET,XMIN=1900,XMAX=2010,YMIN=ymin(j),YMAX=ymax(j)
           ;TITLE='Timeseries of EOT'+this_eot_str+' for SILO 0.25, using seasonal means for '+season_name+' '+silo_start_year_str+'-2010'
      black=FSC_COLOR("black",9)
      HIST,X=indgen(n_years)+silo_start_year,Y=REFORM(eots_ts(*,j)),COL=9,WIDTH=30,THICK=80;,FILLCOL=FSC_COLOR("black")
      AXES,XSTEP=10,YSTEP=ystep(j),XMINOR=2,YMINOR=ystep(j)/2.,YTITLE='EOT timeseries (mm season!U-1!N)',XTITLE=xtitle
      GPLOT,X=indgen(n_years)+silo_start_year,Y=REPLICATE(0,n_years),STYLE=1
      ;red=FSC_COLOR("red",10)
      smoothed=SMOOTH(REFORM(eots_ts(*,j)),11)
      GPLOT,X=indgen(n_years-10)+silo_start_year+5,Y=smoothed(5:n_years-6),STYLE=0,THICK=300,COL=9
      ;FOR k=0,n_years-31 DO $
      ;   IF ABS(running_correlations(k)) gt trend_sig_level THEN $
      ;      GPLOT,X=silo_start_year+k+15,Y=ymin(j)*0.95,SYM=3,SIZE=100,COL=10
      PSCLOSE,/NOVIEW

                                ; psfile='/home/ss901165/idl/queensland/eots/qld_eots_smeans_silo025.'+season_name+'_smeans.eot'+this_eot_str+'_nino4.'+silo_start_year_str+'-2010.ps'
      ;psfile='/home/ss901165/idl/queensland/eots/qld_eots_smeans_silo025.'+season_name+'_smeans.eot'+this_eot_str+'_nino4.'+silo_start_year_str+'-2010.bw.ps'
      ;PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=2500,SPACE2=300,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=112,SPACE3=300
      ;GSET,XMIN=1900,XMAX=2010,YMIN=-1,YMAX=1
      ;     ;TITLE='Instantaneous Nino 4 correlations with '+season_name+' EOT '+this_eot_str+' for '+silo_start_year_str+'-2010'
      ;GPLOT,X=indgen(n_years)+silo_start_year,Y=REPLICATE(0,n_years),STYLE=2,THICK=70
      ;
      ;this_eot_corr_nino4_windows=fltarr(n_windows,n_years)
      ;this_eot_corr_nino4_windows_signif=fltarr(n_windows,n_years)
      ;this_eot_corr_nino4_windows_signif(*,*)=!Values.F_NaN
      ;this_eot_corr_nino4_windows(*,*)=!Values.F_NaN
      ;FOR k=0,n_windows-1 DO BEGIN
      ;   FOR m=windows(k)/2,n_years-windows(k)/2-1 DO BEGIN
      ;      this_eot_corr_nino4_windows(k,m)=CORRELATE(REFORM(eots_ts(m-windows(k)/2:m+windows(k)/2,j)),nino4_ts(m-windows(k)/2:m+windows(k)/2))
      ;      IF ABS(this_eot_corr_nino4_windows(k,m)) gt signif_levels(k) THEN $
      ;         this_eot_corr_nino4_windows_signif(k,m)=this_eot_corr_nino4_windows(k,m)
      ;   ENDFOR
      ;ENDFOR
;     ; colors=['red','purple','blue']
      ;styles=[2,5,0]
      ;syms=[2,5,3]
      ;FOR k=0,n_windows-1 DO BEGIN
      ;   GPLOT,X=indgen(n_years)+silo_start_year,Y=REFORM(this_eot_corr_nino4_windows(k,*)),STYLE=styles(k),THICK=200;COL=FSC_COLOR(colors(k)),THICK=150
      ;   GPLOT,X=indgen(n_years)+silo_start_year,Y=REFORM(this_eot_corr_nino4_windows_signif(k,*)),/NOLINES,SYM=syms(k),SIZE=60 ;COL=FSC_COLOR(colors(k)),/NOLINES,SYM=2,SIZE=70
      ;ENDFOR
      ;AXES,YSTEP=0.2,NDECS=2,XSTEP=10,YTITLE='Correlation with NINO 4 index (HadISST)',XTITLE='Year at centre of window',XMINOR=5,/NORIGHT

      ;GSET,XMIN=1900,XMAX=2010,YMIN=-5.0,YMAX=2.0
      ;ipo_smoothed=SMOOTH(ipo_ts,11)
      ;ipo_smoothed=ipo_smoothed(5:n_years-6)
      ;GPLOT,X=indgen(n_years-11)+silo_start_year+5,Y=ipo_smoothed,COL=FSC_COLOR("black"),STYLE=1
      ;AXES,YSTEP=0.5,/ONLYRIGHT,NDECS=1,YTITLE='11-year running mean of IPO index'
;
;      print,CORRELATE(ipo_smoothed,REFORM(this_eot_corr_nino4_windows(0,5:n_years-6)))
;      STOP

      ;GLEGEND,labels=REVERSE(items),STYLE=REVERSE(styles),LEGPOS=11,SYM=REVERSE(syms);COL=REVERSE(FSC_COLOR(colors)),LEGPOS=1
      ;PSCLOSE,/NOVIEW
      
                                ;psfile='/home/ss901165/idl/queensland/eots/qld_eots_smeans_silo025.'+season_name+'_smeans.eot'+this_eot_str+'.'+silo_start_year_str+'-2010.ps'

      psfile='/home/ss901165/idl/queensland/eots/qld_eots_smeans_silo025.'+season_name+'_smeans.eot'+this_eot_str+'.'+silo_start_year_str+'-2010.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=112,/PORTRAIT
      CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs)+3,MIN=4
      ;                          ;   CS, COLS=[93, 123, 91, 267, 272, 367, 409, 419, 407]
      ;CS,SCALE=7,NCOLS=N_ELEMENTS(mylevs)+1,MIN=5
      MAP,LONMIN=138,LONMAX=154,LATMIN=-29,LATMAX=-10,/hires
      LEVS,MANUAL=mylevs
      temp=REFORM(eots_pattern(*,*,j))
      IF TOTAL(where(ABS(temp) le sig_level)) gt 0 THEN $
         temp[where(ABS(temp) le sig_level)]=!Values.F_NaN
      CON,FIELD=temp,X=longitude,Y=latitude,$
                                ;TITLE='EOT'+this_eot_str+' for SILO 0.25, using seasonal means for '+season_name+' '+silo_start_year_str+'-2010',$
          /NOLINELABELS,POSITIVE_STYLE=2,NEGATIVE_STYLE=1,ZERO_THICK=300,ZERO_STYLE=2
      AXES
      thiseot_max=MAX(REFORM(eots_pattern(*,*,j)),/NaN)
      print,thiseot_max
      FOR m=0,n_lon-1 DO BEGIN
         FOR n=0,n_lat-1 DO BEGIN
      ;      IF ABS(eots_pattern(m,n,j)) gt sig_level THEN $
      ;         GPLOT,X=longitude(m),Y=latitude(n),SYM=3,SIZE=30
            IF (eots_pattern(m,n,j)) eq thiseot_max THEN BEGIN
               central_lon=longitude(m)
               central_lat=latitude(n)
            ENDIF
         ENDFOR
      ENDFOR
      black=FSC_COLOR("grey",2)
      GPLOT,X=central_lon,Y=central_lat,SYM=8,COL=2,SIZE=250
                                ;GPLOT,X=149,Y=-12,TEXT='Explains '+STRMID(STRTRIM(STRING(eots_expvar(j)*100.),1),0,5)+$
                                ;      '% of domain space-time variance'
      
      PSCLOSE,/NOVIEW
   ENDFOR
ENDFOR

STOP
END

