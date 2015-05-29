PRO qld_eots_correlate_ipo_silo025
  
; Correlate EOT timeseries against timeseries of the 
; Inter-Decadal Pacific Oscillation (IPO) from HadISST

silo_dir='/home/ss901165/datasets_mango/SILO/one_quarter'
silo_eots_infiles=[silo_dir+'/SILO.dec-feb_smeans.1900-2007.eots.nc',$
                   silo_dir+'/SILO.mar-may_smeans.1900-2007.eots.nc',$
                   silo_dir+'/SILO.jun-aug_smeans.1900-2007.eots.nc',$
                   silo_dir+'/SILO.sep-nov_smeans.1900-2007.eots.nc']
season_names=['dec-feb','mar-may','jun-aug','sep-nov']
n_seasons=N_ELEMENTS(silo_eots_infiles)
; Number of EOTs to consider
n_eots=3
; Number of years to consider
silo_nyears=108

ipo_dir='/home/ss901165/datasets/HADISST'
ipo_infiles=[ipo_dir+'/hadisst_ipo.dec-feb_smeans.1871-2008.nc',$
             ipo_dir+'/hadisst_ipo.mar-may_smeans.1871-2008.nc',$
             ipo_dir+'/hadisst_ipo.jun-aug_smeans.1871-2008.nc',$
             ipo_dir+'/hadisst_ipo.sep-nov_smeans.1871-2008.nc']
ipo_offset=29 ; As offset to start of SILO EOT timeseries


windows=[10,20,30,40,50]
signif_levels=[0.602,0.433,0.355,0.304,0.273]
n_windows=N_ELEMENTS(windows)
items=strarr(n_windows)
FOR i=0,n_windows-1 DO $
   items(i)='Centred window of '+STRTRIM(STRING(windows(i)+1),1)+' years'

silo_smeans_eots=fltarr(n_seasons,silo_nyears,n_eots)
ipo_smeans=fltarr(n_seasons,silo_nyears)
print,''
print,'Correlations with Inter-Decadal Pacific Oscillation: '
FOR i=0,n_seasons-1 DO BEGIN
   silo_smeans_eots(i,*,*)=REFORM(OPEN_AND_EXTRACT(silo_eots_infiles(i),'loading',$
                                                   offset=[0,0],$
                                                   count=[silo_nyears,n_eots]))
   ipo_smeans(i,*)=REFORM(OPEN_AND_EXTRACT(ipo_infiles(i),'ipo_index',$
                                           offset=[ipo_offset],count=[silo_nyears]))
   ;ipo_smeans_trend=REGRESS(indgen(ipo_nyears),REFORM(ipo_smeans(i,*)))
   ;ipo_smeans(i,*)=ipo_smeans(i,*)-ipo_smeans_trend(0)*indgen(ipo_nyears)

   season_correlations=fltarr(n_eots)
   FOR j=0,n_eots-1 DO $
      season_correlations(j)=CORRELATE(REFORM(silo_smeans_eots(i,*,j)),$
                                       ipo_smeans(i,*))
   print,season_names(i)
   FOR j=0,n_eots-1 DO BEGIN
      print,'EOT '+STRTRIM(STRING(j+1),1)+': '+STRMID(STRTRIM(STRING(season_correlations(j)),1),0,6)

      this_eot_corr_ipo_windows=fltarr(n_windows,silo_nyears)
      this_eot_corr_ipo_windows_signif=fltarr(n_windows,silo_nyears)
      this_eot_corr_ipo_windows_signif(*,*)=!Values.F_NaN
      this_eot_corr_ipo_windows(*,*)=!Values.F_NaN
      FOR k=0,n_windows-1 DO BEGIN
         FOR m=windows(k)/2,silo_nyears-windows(k)/2-1 DO BEGIN
            this_eot_corr_ipo_windows(k,m)=CORRELATE(REFORM(silo_smeans_eots(i,m-windows(k)/2:m+windows(k)/2,j)),$
                                                     REFORM(ipo_smeans(i,m-windows(k)/2:m+windows(k)/2)))
            IF ABS(this_eot_corr_ipo_windows(k,m)) gt signif_levels(k) THEN $
               this_eot_corr_ipo_windows_signif(k,m)=this_eot_corr_ipo_windows(k,m)
         ENDFOR
      ENDFOR
      
      psfile='/home/ss901165/idl/queensland/eots/qld_eots_correlate_ipo_silo025.sliding_windows.'+season_names(i)+'_smeans_eot'+$
             STRTRIM(STRING(j+1),1)+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1800,SPACE2=300,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=112
      GSET,XMIN=1900,XMAX=2007,YMIN=-1,YMAX=1,$
           TITLE='IPO correlations with EOT'+STRTRIM(STRING(j+1),1)+' for SILO 0.25, using seasonal-means for '+season_names(i)+' 1900-2007'
      CS,SCALE=26,NCOLS=n_windows+1,/REV   
      FOR k=0,n_windows-1 DO BEGIN
         GPLOT,X=indgen(silo_nyears)+1900,Y=REFORM(this_eot_corr_ipo_windows(k,*)),COL=2+k,THICK=150
         GPLOT,X=indgen(silo_nyears)+1900,Y=REFORM(this_eot_corr_ipo_windows_signif(k,*)),COL=2+k,/NOLINES,SYM=2,SIZE=70
      ENDFOR
      GPLOT,X=[1900,2007],Y=[0,0],STYLE=1
      GPLOT,X=1905,Y=0.85,TEXT='Correlation with seasonal-mean IPO: '+STRMID(STRTRIM(STRING(season_correlations(j)),1),0,6),ALIGN=0.0
      AXES,YSTEP=0.1,NDECS=2,XSTEP=10,YTITLE='Correlation with IPO index (HadISST)',XTITLE='Year at centre of window',XMINOR=5      
      LEGEND,labels=REVERSE(items),COL=REVERSE(indgen(n_windows)+2),LEGPOS=9      
      PSCLOSE,/NOVIEW

   ENDFOR

   

ENDFOR 

STOP

END

