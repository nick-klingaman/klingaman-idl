PRO qld_reanalysis_twentieth_century_eots_smeans

; Perform the EOT procedure described in Smith et al. (2004) from van
; den Dool et al. (2000).

n_seasons=2
twentyc_nyears=108
twentyc_offset=9
twentyc_mask_infile='/home/ss901165/datasets_mango/20THC_REANALYSIS/mask_t62.nc'
nino_start=1900-1871
box=[-29,138,-10,154]
n_eots=6

FOR p=0,n_seasons-1 DO BEGIN

   CASE p OF
      0 : BEGIN
         smean_period='nov-apr'
         twentyc_smeans_infile='/home/ss901165/datasets_mango/20THC_REANALYSIS/precip/20thc_reanalysis.nov-apr_smeans.1891-2007.precip.nc'
         nino4_infile='/home/ss901165/datasets/NINO/nino4_hadisst.nov-apr_smeans.1871-2008.nc'
         nino3_infile='/home/ss901165/datasets/NINO/nino3_hadisst.nov-apr_smeans.1871-2008.nc'
         nino34_infile='/home/ss901165/datasets/NINO/nino34_hadisst.nov-apr_smeans.1871-2008.nc'
      END
      1 : BEGIN
         smean_period='may-oct'
         twentyc_smeans_infile='/home/ss901165/datasets_mango/20THC_REANALYSIS/precip/20thc_reanalysis.may-oct_smeans.1891-2007.precip.nc'
         nino4_infile='/home/ss901165/datasets/NINO/nino4_hadisst.may-oct_smeans.1871-2008.nc'
         nino3_infile='/home/ss901165/datasets/NINO/nino3_hadisst.may-oct_smeans.1871-2008.nc'
         nino34_infile='/home/ss901165/datasets/NINO/nino34_hadisst.may-oct_smeans.1871-2008.nc'
      END
   ENDCASE   
                                ; Read latitude and longitude from file
   twentyc_latitude=OPEN_AND_EXTRACT(twentyc_smeans_infile,'latitude')
   twentyc_longitude=OPEN_AND_EXTRACT(twentyc_smeans_infile,'longitude')
   DEFINE_BOUNDARIES,box,twentyc_latitude,twentyc_longitude,twentyc_box_tx,/LIMIT
   twentyc_nlat=N_ELEMENTS(twentyc_latitude)
   twentyc_nlon=N_ELEMENTS(twentyc_longitude)

   mask_latitude=OPEN_AND_EXTRACT(twentyc_mask_infile,'latitude')
   mask_longitude=OPEN_AND_EXTRACT(twentyc_mask_infile,'longitude')
   DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
   mask_nlat=N_ELEMENTS(mask_latitude)
   mask_nlon=N_ELEMENTS(mask_longitude)
                                ; Read NINO annual-mean timeseries
   nino4_ts=OPEN_AND_EXTRACT(nino4_infile,'NINO4',offset=[nino_start],count=[twentyc_nyears])
   nino3_ts=OPEN_AND_EXTRACT(nino3_infile,'NINO3',offset=[nino_start],count=[twentyc_nyears])
   nino34_ts=OPEN_AND_EXTRACT(nino34_infile,'NINO34',offset=[nino_start],count=[twentyc_nyears])
   
                                ; Read precipitation data, mask, and create an area-averaged
                                ; timeseries.
   twentyc_smeans=REFORM(OPEN_AND_EXTRACT(twentyc_smeans_infile,'PRATE',$
                                          offset=[twentyc_box_tx(1),twentyc_box_tx(0),0,twentyc_offset],$
                                          count=[twentyc_nlon,twentyc_nlat,1,twentyc_nyears]))*86400.
   twentyc_mask=REFORM(OPEN_AND_EXTRACT(twentyc_mask_infile,'lsm',$
                                     offset=[mask_box_tx(1),mask_box_tx(0)],$
                                     count=[mask_nlon,mask_nlat]))
   
   twentyc_smeans_eots_ts=fltarr(n_eots,twentyc_nyears)
   twentyc_smeans_eots_pts=fltarr(n_eots,2)
   twentyc_smeans_eots_expvar=fltarr(n_eots)
   twentyc_smeans_aavg=fltarr(twentyc_nyears)
   twentyc_smeans_avg=fltarr(twentyc_nyears)
   twentyc_smeans_corr_with_eots=fltarr(n_eots,twentyc_nlon,twentyc_nlat)
   
   FOR i=0,twentyc_nyears-1 DO BEGIN
      temp=REFORM(twentyc_smeans(*,*,i))
      temp[where(twentyc_mask ne 1.0)]=!Values.F_NaN
;      temp[where(temp ge 1E10)]=!Values.F_NaN      
      twentyc_smeans(*,*,i)=temp
      IF i eq 0 THEN $
         twentyc_n_valid_pts=N_ELEMENTS(where(FINITE(temp) eq 1))
   ENDFOR
   FOR i=0,twentyc_nlon-1 DO BEGIN
      FOR j=0,twentyc_nlat-1 DO BEGIN
         IF FINITE(twentyc_smeans(i,j,0)) eq 1 THEN $
            twentyc_smeans(i,j,*)=twentyc_smeans(i,j,*)-MEAN(twentyc_smeans(i,j,*),/NaN)
      ENDFOR
   ENDFOR
   
   twentyc_aavg_weight=fltarr(twentyc_nlat)
   FOR i=0,twentyc_nlat-1 DO $
      twentyc_aavg_weight(i)=COS(3.14159*twentyc_latitude(i)/180.)
   twentyc_aavg_weight=twentyc_aavg_weight/TOTAL(twentyc_aavg_weight)
   
   FOR i=0,twentyc_nyears-1 DO BEGIN
      FOR j=0,twentyc_nlon-1 DO BEGIN
         temp=REFORM(twentyc_smeans(j,*,i))
         temp_aavg=TOTAL(twentyc_aavg_weight*temp,/NaN)
         twentyc_smeans_aavg(i)=temp_aavg*1./FLOAT(twentyc_nlon)+twentyc_smeans_aavg(i)
      ENDFOR
      twentyc_smeans_avg(i)=MEAN(twentyc_smeans(*,*,i),/NaN)
   ENDFOR
   
   twentyc_smeans_stvar=VARIANCE(twentyc_smeans,/NaN)
;twentyc_smeans_stvar=1./(FLOAT(twentyc_n_valid_pts)*FLOAT(twentyc_nyears))*$
;                  TOTAL(twentyc_smeans(*,*,*)^2,/NaN)
;twentyc_smeans_aavg_stddev=SQRT(1./(FLOAT(twentyc_nyears))*TOTAL(twentyc_smeans_aavg^2))
   FOR k=0,n_eots-1 DO BEGIN
      k_str=STRTRIM(STRING(k+1),1)
      twentyc_smeans_corr_with_aavg=fltarr(twentyc_nlon,twentyc_nlat)
      
      twentyc_smeans_aavg=fltarr(twentyc_nyears)
      FOR i=0,twentyc_nyears-1 DO BEGIN
         FOR j=0,twentyc_nlon-1 DO BEGIN
            temp=REFORM(twentyc_smeans(j,*,i))
            temp_aavg=TOTAL(twentyc_aavg_weight*temp,/NaN)
            twentyc_smeans_aavg(i)=temp_aavg*1./FLOAT(twentyc_nlon)+twentyc_smeans_aavg(i)
         ENDFOR
      ENDFOR
;   twentyc_smeans_stvar=1./(FLOAT(twentyc_n_valid_pts)*FLOAT(twentyc_nyears))*$
;                     TOTAL(twentyc_smeans(*,*,*)^2,/NaN)
      
;   twentyc_smeans_aavg_stddev=SQRT(1./FLOAT(twentyc_nyears)*TOTAL(twentyc_smeans_aavg^2))
      twentyc_smeans_aavg_stddev=STDDEV(twentyc_smeans_aavg)
      twentyc_smeans_aavg_var=VARIANCE(twentyc_smeans_aavg)
      
      twentyc_eot_max_corr=0
      FOR i=0,twentyc_nlon-1 DO BEGIN
         FOR j=0,twentyc_nlat-1 DO BEGIN
            IF twentyc_mask(i,j) eq 1.0 and twentyc_smeans(i,j,0) lt 1E10 THEN BEGIN
               twentyc_smeans_thispt_ts=REFORM(twentyc_smeans(i,j,*))
;            thispt_stddev=SQRT((1./FLOAT(twentyc_nyears))*TOTAL(twentyc_smeans_thispt_ts^2))
               thispt_stddev=STDDEV(twentyc_smeans_thispt_ts)
;            twentyc_smeans_corr_with_aavg(i,j)=(1./FLOAT(twentyc_nyears)*TOTAL(twentyc_smeans_thispt_ts*twentyc_smeans_aavg))/(thispt_stddev*twentyc_smeans_aavg_stddev)
               twentyc_smeans_corr_with_aavg(i,j)=CORRELATE(twentyc_smeans_thispt_ts,twentyc_smeans_aavg)
               IF twentyc_smeans_corr_with_aavg(i,j) gt twentyc_eot_max_corr THEN BEGIN
                  twentyc_eot_max_corr=twentyc_smeans_corr_with_aavg(i,j)
                  twentyc_eot_pt=[i,j]
               ENDIF
            ENDIF ELSE $
               twentyc_smeans_corr_with_aavg(i,j)=!Values.F_NaN
         ENDFOR
      ENDFOR
   
      print,'EOT'+k_str+': Maximum correlation is '+STRTRIM(STRING(twentyc_eot_max_corr),1)
      print,'EOT'+k_str+': at point '+STRTRIM(STRING(twentyc_latitude(twentyc_eot_pt(1))),1)+' '+STRTRIM(STRING(twentyc_longitude(twentyc_eot_pt(0))),1)
      
      twentyc_smeans_eots_pts(k,*)=[twentyc_longitude(twentyc_eot_pt(0)),twentyc_latitude(twentyc_eot_pt(1))]
      twentyc_smeans_eots_ts(k,*)=REFORM(twentyc_smeans(twentyc_eot_pt(0),twentyc_eot_pt(1),*))
      this_eot_ts=fltarr(twentyc_nyears)
      this_eot_ts(*)=REFORM(twentyc_smeans_eots_ts(k,*))
      
      FOR i=0,twentyc_nlon-1 DO BEGIN
         FOR j=0,twentyc_nlat-1 DO BEGIN
            IF twentyc_mask(i,j) eq 1.0 and twentyc_smeans(i,j,0) lt 1E10 THEN BEGIN
               twentyc_smeans_thispt_ts=REFORM(twentyc_smeans(i,j,*))
               temp=REGRESS(this_eot_ts,twentyc_smeans_thispt_ts,CORRELATION=temp2)
               twentyc_smeans_corr_with_eots(k,i,j)=REFORM(temp2(0))
            ENDIF ELSE $
               twentyc_smeans_corr_with_eots(k,i,j)=!Values.F_NaN
         ENDFOR
      ENDFOR
      
      twentyc_smeans_eots_explained=fltarr(twentyc_nlon,twentyc_nlat,twentyc_nyears)
;   twentyc_smeans_eot_stddev=SQRT((1./FLOAT(twentyc_nyears))*TOTAL(this_eot_ts^2))
      twentyc_smeans_eot_stddev=STDDEV(this_eot_ts)
      FOR i=0,twentyc_nlon-1 DO BEGIN
         FOR j=0,twentyc_nlat-1 DO BEGIN
            IF twentyc_mask(i,j) eq 1.0 THEN BEGIN
               twentyc_smeans_thispt_ts=REFORM(twentyc_smeans(i,j,*))
;            thispt_stddev=SQRT((1./FLOAT(twentyc_nyears))*TOTAL(twentyc_smeans_thispt_ts^2))
               thispt_stddev=STDDEV(twentyc_smeans_thispt_ts)
                                ;corr_thispt_eot=(1./FLOAT(twentyc_nyears)*TOTAL(twentyc_smeans_thispt_ts*this_eot_ts))/(thispt_stddev*twentyc_smeans_eot_stddev)
            ;IF ABS(corr_thispt_eot) gt 1.01 THEN $
            ;   print,'Correlation > 1 - STOP!  ',corr_thispt_eot
            ;thispt_eot=corr_thispt_eot*thispt_stddev/twentyc_smeans_eot_stddev
               thispt_eot=CORRELATE(twentyc_smeans_thispt_ts,this_eot_ts)*thispt_stddev/twentyc_smeans_eot_stddev
               FOR m=0,twentyc_nyears-1 DO BEGIN
                  twentyc_smeans_eots_explained(i,j,m)=this_eot_ts(m)*thispt_eot
                  twentyc_smeans(i,j,m)=twentyc_smeans(i,j,m)-twentyc_smeans_eots_explained(i,j,m)
               ENDFOR
            ENDIF ELSE $
               twentyc_smeans_eots_explained(i,j,*)=!Values.F_NaN
         ENDFOR
      ENDFOR
;   IF k eq 1 THEN STOP

      twentyc_smeans_eots_expvar(k)=1./FLOAT(twentyc_n_valid_pts*twentyc_nyears)*$
                                 TOTAL(twentyc_smeans_eots_explained(*,*,*)^2,/NaN)/twentyc_smeans_stvar
      print,twentyc_smeans_eots_expvar(k)
      print,VARIANCE(twentyc_smeans_eots_explained,/NaN)/twentyc_smeans_stvar
   ENDFOR

   windows=[10,20,30,40,50]
   signif_levels=[0.602,0.433,0.355,0.304,0.273]
   n_windows=N_ELEMENTS(windows)
   items=strarr(n_windows)
   FOR i=0,n_windows-1 DO $
      items(i)='Centred window of '+STRTRIM(STRING(windows(i)+1),1)+' years'
   
   FOR k=0,n_eots-1 DO BEGIN
      
      this_eot=REFORM(where(twentyc_smeans_eots_expvar eq MAX(twentyc_smeans_eots_expvar)))
      this_eot_str=STRTRIM(STRING(k+1),1)
      
      mylevs=['-0.30','-0.18','-0.06','0.06','0.18','0.30','0.42','0.54','0.66','0.78','0.90']
      psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/eots/qld_reanalysis_twentieth_century_eots_smeans.'+smean_period+'_smeans.eot'+this_eot_str+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=112,/PORTRAIT
      CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs)+3,MIN=4,white=[5]
;   CS, COLS=[93, 123, 91, 267, 272, 367, 409, 419, 407]
      MAP,LONMIN=139,LONMAX=153.5,LATMIN=box(0),LATMAX=box(2),/hires
      LEVS,MANUAL=mylevs
      CON,FIELD=REFORM(twentyc_smeans_corr_with_eots(this_eot,*,*)),X=twentyc_longitude,Y=twentyc_latitude,$
          TITLE='EOT'+this_eot_str+' for TWENTYC T62, using seasonal means for '+smean_period+' 1900-2008',/NOLINES,/BLOCK
      black=FSC_COLOR("black",2)
      GPLOT,X=twentyc_smeans_eots_pts(this_eot,0),Y=twentyc_smeans_eots_pts(this_eot,1),SYM=8,COL=2
      GPLOT,X=149,Y=-12,TEXT='Explains '+STRMID(STRTRIM(STRING(twentyc_smeans_eots_expvar(this_eot)*100.),1),0,5)+$
            '% of domain space-time variance'
      PSCLOSE,/NOVIEW

      this_eot_ts=REFORM(twentyc_smeans_eots_ts(this_eot,*)*365.)
      ymax=ROUND(MAX(this_eot_ts)/100.+1)*100.
      ymin=ROUND(MIN(this_eot_ts)/100.-1)*100.
      
      psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/eots/qld_reanalysis_twentieth_century_eots_smeans.'+smean_period+'_smeans.eot'+this_eot_str+'_ts.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1800,SPACE2=300,XOFFSET=400,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=112
      GSET,XMIN=1900,XMAX=2008,YMIN=ymin,YMAX=ymax,$
           TITLE='Timeseries of EOT'+this_eot_str+' for TWENTYC T62, using seasonal means for '+smean_period+' 1900-2008'
      black=FSC_COLOR("black",9)
      HIST,X=indgen(twentyc_nyears)+1900,Y=this_eot_ts,COL=9,WIDTH=50
      AXES,XSTEP=10,YSTEP=100,XMINOR=2,YMINOR=50,YTITLE='EOT timeseries',XTITLE='Year at beginning of '+smean_period+' period'
      GPLOT,X=indgen(twentyc_nyears)+1900,Y=REPLICATE(0,twentyc_nyears),STYLE=1
      red=FSC_COLOR("red",10)
      smoothed=SMOOTH(this_eot_ts,11)
      GPLOT,X=indgen(twentyc_nyears-10)+1905,Y=smoothed(5:twentyc_nyears-6),STYLE=0,THICK=150,COL=10
      GPLOT,X=1905,Y=ymax-(ymax-ymin)*0.03,ALIGN=0,TEXT='Explains '+STRMID(STRTRIM(STRING(twentyc_smeans_eots_expvar(this_eot)*100.),1),0,5)+$
            '% of domain space-time variance'
      GPLOT,X=1905,Y=ymax-(ymax-ymin)*0.06,ALIGN=0,TEXT='Correlation with seasonal-mean ('+smean_period+') NINO4: '+$
            STRMID(STRTRIM(STRING(CORRELATE(this_eot_ts,nino4_ts)),1),0,5)
      GPLOT,X=1905,Y=ymax-(ymax-ymin)*0.09,ALIGN=0,TEXT='Correlation with seasonal-mean ('+smean_period+') NINO3.4: '+$
            STRMID(STRTRIM(STRING(CORRELATE(this_eot_ts,nino34_ts)),1),0,5)
      GPLOT,X=1905,Y=ymax-(ymax-ymin)*0.12,ALIGN=0,TEXT='Correlation with seasonal-mean ('+smean_period+') NINO3: '+$
            STRMID(STRTRIM(STRING(CORRELATE(this_eot_ts,nino3_ts)),1),0,5)
      
      PSCLOSE,/NOVIEW
      
      psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/eots/qld_reanalysis_twentieth_century_eots_smeans.'+smean_period+'_smeans.eot'+this_eot_str+'_nino4.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1800,SPACE2=300,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=112
      GSET,XMIN=1900,XMAX=2008,YMIN=-1,YMAX=1,$
        TITLE='NINO4 correlations with EOT'+this_eot_str+' for TWENTYC T62, using ann-means for '+smean_period+' 1900-2008'
      GPLOT,X=indgen(twentyc_nyears)+1900,Y=REPLICATE(0,twentyc_nyears),STYLE=1
      GPLOT,X=1905,Y=0.9,ALIGN=0,TEXT='Explains '+STRMID(STRTRIM(STRING(twentyc_smeans_eots_expvar(this_eot)*100.),1),0,5)+$
            '% of domain space-time variance'
      GPLOT,X=1905,Y=0.85,ALIGN=0,TEXT='Correlation with seasonal-mean ('+smean_period+') NINO4: '+$
            STRMID(STRTRIM(STRING(CORRELATE(this_eot_ts,nino4_ts)),1),0,5)
      GPLOT,X=1905,Y=0.8,ALIGN=0,TEXT='Correlation with seasonal-mean ('+smean_period+') NINO3.4: '+$
            STRMID(STRTRIM(STRING(CORRELATE(this_eot_ts,nino34_ts)),1),0,5)
      GPLOT,X=1905,Y=0.75,ALIGN=0,TEXT='Correlation with seasonal-mean ('+smean_period+') NINO3: '+$
            STRMID(STRTRIM(STRING(CORRELATE(this_eot_ts,nino3_ts)),1),0,5)
      
      this_eot_corr_nino4_windows=fltarr(n_windows,twentyc_nyears)
      this_eot_corr_nino4_windows_signif=fltarr(n_windows,twentyc_nyears)
      this_eot_corr_nino4_windows_signif(*,*)=!Values.F_NaN
      this_eot_corr_nino4_windows(*,*)=!Values.F_NaN
      FOR i=0,n_windows-1 DO BEGIN
         FOR j=windows(i)/2,twentyc_nyears-windows(i)/2-1 DO BEGIN
            this_eot_corr_nino4_windows(i,j)=CORRELATE(this_eot_ts(j-windows(i)/2:j+windows(i)/2),nino4_ts(j-windows(i)/2:j+windows(i)/2))
            IF ABS(this_eot_corr_nino4_windows(i,j)) gt signif_levels(i) THEN $
               this_eot_corr_nino4_windows_signif(i,j)=this_eot_corr_nino4_windows(i,j)
         ENDFOR
      ENDFOR
      CS,SCALE=26,NCOLS=n_windows+1,/REV   
      FOR i=0,n_windows-1 DO BEGIN
         GPLOT,X=indgen(twentyc_nyears)+1900,Y=REFORM(this_eot_corr_nino4_windows(i,*)),COL=2+i,THICK=150
         GPLOT,X=indgen(twentyc_nyears)+1900,Y=REFORM(this_eot_corr_nino4_windows_signif(i,*)),COL=2+i,/NOLINES,SYM=2,SIZE=70
      ENDFOR
      AXES,YSTEP=0.1,NDECS=2,XSTEP=10,YTITLE='Correlation with NINO 4 index (HadISST)',XTITLE='Year at centre of window',XMINOR=5      
      LEGEND,labels=REVERSE(items),COL=REVERSE(indgen(n_windows)+2),LEGPOS=9      
      PSCLOSE,/NOVIEW
      twentyc_smeans_eots_expvar(this_eot)=0
      
   ENDFOR
ENDFOR   

STOP

END


                         
