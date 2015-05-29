PRO qld_eots_correlations_silo025

; Give files containing patterns and loadings of EOTs for annual and
; seasonal means.

silo_dir='/home/ss901165/datasets_mango/SILO/one_quarter'
silo_ameans_infile=silo_dir+'/SILO.may-apr_ameans.1900-2008.eots.nc'
silo_smeans_infiles=[silo_dir+'/SILO.may-oct_smeans.1900-2008.eots.nc',$
                     silo_dir+'/SILO.nov-apr_smeans.1900-2008.eots.nc']
n_seasons=N_ELEMENTS(silo_smeans_infiles)

; Number of EOTs to consider
n_eots=6
; Number of years for each EOT
n_years=108
; Number of lags to consider
n_lags=21
lags=indgen(n_lags)-n_lags/2

silo_ameans_eots=REFORM(OPEN_AND_EXTRACT(silo_ameans_infile,'loading'))

silo_smeans_eots=fltarr(n_seasons,n_years,n_eots)
FOR i=0,n_seasons-1 DO $
   silo_smeans_eots(i,*,*)=REFORM(OPEN_AND_EXTRACT(silo_smeans_infiles(i),'loading'))

amean_smean_correlations=fltarr(n_eots,n_seasons,n_eots,n_lags)
smean_smean_correlations=fltarr(n_eots,n_eots,n_lags)
amean_autocorrelations=fltarr(n_eots,n_lags)
FOR i=0,n_eots-1 DO BEGIN
   FOR j=0,n_seasons-1 DO BEGIN
      FOR k=0,n_eots-1 DO BEGIN
         amean_smean_correlations(i,j,k,*)=$
            C_CORRELATE(REFORM(silo_ameans_eots(*,i)),$
                        REFORM(silo_smeans_eots(j,*,k)),lags)
         ;print,amean_smean_correlations(i,j,k,*),silo_ameans_eots(*,i),silo_smeans_eots(j,*,k)
      ENDFOR
      IF j eq 1 THEN BEGIN
         FOR k=0,n_eots-1 DO BEGIN
            smean_smean_correlations(i,k,*)=$
               C_CORRELATE(REFORM(silo_smeans_eots(j-1,*,i)),REFORM(silo_smeans_eots(j,*,k)),lags)
         ENDFOR
      ENDIF
   ENDFOR
   amean_autocorrelations(i,*)=A_CORRELATE(REFORM(silo_ameans_eots(*,i)),lags)
ENDFOR

sig_level=0.198
colors=indgen(n_eots)+21
color_names=['blue','red','cyan','brown','pink','purple']
FOR i=0,n_eots-1 DO BEGIN
   psfile='/home/ss901165/idl/queensland/eots/qld_eots_correlations_silo025.corr_amean'+STRTRIM(STRING(i+1),1)+'_with_allsmeans.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1500,SPACE2=300,XOFFSET=1500,YOFFSET=1500,TFONT=2,TCHARSIZE=100
   GSET,XMIN=MIN(lags)-0.5,XMAX=MAX(lags)+0.5,YMIN=-1,YMAX=1
   GPLOT,X=lags,Y=REFORM(amean_autocorrelations(i,*)),STYLE=0,SYM=4
   FOR j=0,n_seasons-1 DO BEGIN
      FOR k=0,n_eots-1 DO BEGIN
         this_color=FSC_COLOR(color_names(k),colors(k))
         FOR m=0,n_lags-1 DO BEGIN
            IF ABS(amean_smean_correlations(i,j,k,m)) ge sig_level THEN $
               GPLOT,X=lags(m),Y=REFORM(amean_smean_correlations(i,j,k,m)),/NOLINES,SYM=5+j,COL=colors(k)
         ENDFOR
      ENDFOR
   ENDFOR
   AXES,XSTEP=1,YSTEP=0.1,NDECS=2
   PSCLOSE,/NOVIEW
ENDFOR

; Correlate the first mode in each season using sliding windows

windows=[10,20,30,40,50]
signif_levels=[0.602,0.433,0.355,0.304,0.273]
n_windows=N_ELEMENTS(windows)
items=strarr(n_windows)
FOR i=0,n_windows-1 DO $
   items(i)='Centred window of '+STRTRIM(STRING(windows(i)+1),1)+' years'
smeans_corr_windows=fltarr(n_windows,n_years)
smeans_corr_windows_signif=fltarr(n_windows,n_years)
smeans_corr_windows_signif(*,*)=!Values.F_NaN
FOR i=0,n_windows-1 DO BEGIN
   FOR j=windows(i)/2,n_years-1-windows(i)/2 DO BEGIN
      smeans_corr_windows(i,j)=CORRELATE(silo_smeans_eots(0,j-windows(i)/2:windows(i)/2+j,0),$
                                         silo_smeans_eots(1,j-windows(i)/2:windows(i)/2+j,0))
      IF ABS(smeans_corr_windows(i,j)) gt signif_levels(i) THEN BEGIN
         smeans_corr_windows_signif(i,j)=smeans_corr_windows(i,j)
         print,'Signif',smeans_corr_windows(i,j),smeans_corr_windows_signif(i,j)
      ENDIF ELSE $
         smeans_corr_windows_signif(i,j)=!Values.F_NaN
   ENDFOR
ENDFOR

psfile='/home/ss901165/idl/queensland/eots/qld_eots_correlations_silo025.corr_first_smeans_sliding_windows.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2500,SPACE2=200,XOFFSET=200,YOFFSET=1000,TFONT=2,TCHARSIZE=100,SPACE3=300
CS,SCALE=26,NCOLS=n_windows,/REV
black=FSC_COLOR("black",1+n_windows)
GSET,XMIN=1900,XMAX=1900+n_years,YMIN=-1.0,YMAX=0.8
AXES,XSTEP=10,YSTEP=0.2,XTITLE='Year in SILO dataset (1900-2007)',$
     YTITLE='Corr of May-Oct EOT1 with Nov-Apr EOT1',NDECS=2
FOR i=0,n_windows-1 DO BEGIN
   GPLOT,X=1900+indgen(n_years-windows(i))+windows(i)/2,$
         Y=REFORM(smeans_corr_windows(i,windows(i)/2:n_years-1-windows(i)/2)),COL=i+2,THICK=150
   GPLOT,X=indgen(n_years)+1900,Y=REFORM(smeans_corr_windows_signif(i,*)),COL=2+i,/NOLINES,SYM=2,SIZE=70
ENDFOR
GPLOT,X=1900+indgen(n_years),Y=REPLICATE(0,n_years),STYLE=1

PSCLOSE

STOP

END
