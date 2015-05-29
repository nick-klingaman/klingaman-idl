PRO qld_eots_smeans_silot62

; Perform the EOT procedure described in Smith et al. (2004) from van
; den Dool et al. (2000).

n_seasons=2
silo_nyears=108
silo_mask_infile='/home/ss901165/datasets_mango/20THC_REANALYSIS/mask_t62.nc'
nino_start=1900-1871
box=[-29,138,-10,154]
n_eots=6

FOR p=0,n_seasons-1 DO BEGIN

   CASE p OF
      0 : BEGIN
         smean_period='nov-apr'
         silo_smeans_infile='/home/ss901165/datasets_mango/SILO/t62/SILO.nov-apr_smeans.1900-2008.precip.t62.nc'
         nino4_infile='/home/ss901165/datasets/NINO/nino4_hadisst.nov-apr_smeans.1871-2008.nc'
         nino3_infile='/home/ss901165/datasets/NINO/nino3_hadisst.nov-apr_smeans.1871-2008.nc'
         nino34_infile='/home/ss901165/datasets/NINO/nino34_hadisst.nov-apr_smeans.1871-2008.nc'
      END
      1 : BEGIN
         smean_period='may-oct'
         silo_smeans_infile='/home/ss901165/datasets_mango/SILO/t62/SILO.may-oct_smeans.1900-2008.precip.t62.nc'
         nino4_infile='/home/ss901165/datasets/NINO/nino4_hadisst.may-oct_smeans.1871-2008.nc'
         nino3_infile='/home/ss901165/datasets/NINO/nino3_hadisst.may-oct_smeans.1871-2008.nc'
         nino34_infile='/home/ss901165/datasets/NINO/nino34_hadisst.may-oct_smeans.1871-2008.nc'
      END
   ENDCASE
   
                                ; Read latitude and longitude from file
   silo_latitude=OPEN_AND_EXTRACT(silo_smeans_infile,'latitude')
   silo_longitude=OPEN_AND_EXTRACT(silo_smeans_infile,'longitude')
   DEFINE_BOUNDARIES,box,silo_latitude,silo_longitude,silo_box_tx,/LIMIT
   silo_nlat=N_ELEMENTS(silo_latitude)
   silo_nlon=N_ELEMENTS(silo_longitude)

   mask_latitude=OPEN_AND_EXTRACT(silo_mask_infile,'latitude')
   mask_longitude=OPEN_AND_EXTRACT(silo_mask_infile,'longitude')
   DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
   mask_nlat=N_ELEMENTS(mask_latitude)
   mask_nlon=N_ELEMENTS(mask_longitude)

                                ; Read NINO annual-mean timeseries
   nino4_ts=OPEN_AND_EXTRACT(nino4_infile,'NINO4',offset=[nino_start],count=[silo_nyears])
   nino3_ts=OPEN_AND_EXTRACT(nino3_infile,'NINO3',offset=[nino_start],count=[silo_nyears])
   nino34_ts=OPEN_AND_EXTRACT(nino34_infile,'NINO34',offset=[nino_start],count=[silo_nyears])
   
                                ; Read precipitation data, mask, and create an area-averaged
                                ; timeseries.
   silo_smeans=OPEN_AND_EXTRACT(silo_smeans_infile,'rain',$
                                offset=[silo_box_tx(1),silo_box_tx(0),0],$
                                count=[silo_nlon,silo_nlat,silo_nyears])
   silo_mask=REFORM(OPEN_AND_EXTRACT(silo_mask_infile,'lsm',$
                                     offset=[mask_box_tx(1),mask_box_tx(0)],$
                                     count=[mask_nlon,mask_nlat]))
   
   silo_smeans_eots_ts=fltarr(n_eots,silo_nyears)
   silo_smeans_eots_pts=fltarr(n_eots,2)
   silo_smeans_eots_expvar=fltarr(n_eots)
   silo_smeans_aavg=fltarr(silo_nyears)
   silo_smeans_avg=fltarr(silo_nyears)
   silo_smeans_corr_with_eots=fltarr(n_eots,silo_nlon,silo_nlat)
   
   FOR i=0,silo_nyears-1 DO BEGIN
      temp=REFORM(silo_smeans(*,*,i))
      temp[where(silo_mask ne 1.0)]=!Values.F_NaN
;      temp[where(temp ge 1E10)]=!Values.F_NaN      
      silo_smeans(*,*,i)=temp
      IF i eq 0 THEN $
         silo_n_valid_pts=N_ELEMENTS(where(FINITE(temp) eq 1))
   ENDFOR
   FOR i=0,silo_nlon-1 DO BEGIN
      FOR j=0,silo_nlat-1 DO BEGIN
         IF FINITE(silo_smeans(i,j,0)) eq 1 THEN $
            silo_smeans(i,j,*)=silo_smeans(i,j,*)-MEAN(silo_smeans(i,j,*),/NaN)
      ENDFOR
   ENDFOR
   
   silo_aavg_weight=fltarr(silo_nlat)
   FOR i=0,silo_nlat-1 DO $
      silo_aavg_weight(i)=COS(3.14159*silo_latitude(i)/180.)
   silo_aavg_weight=silo_aavg_weight/TOTAL(silo_aavg_weight)
   
   FOR i=0,silo_nyears-1 DO BEGIN
      FOR j=0,silo_nlon-1 DO BEGIN
         temp=REFORM(silo_smeans(j,*,i))
         temp_aavg=TOTAL(silo_aavg_weight*temp,/NaN)
         silo_smeans_aavg(i)=temp_aavg*1./FLOAT(silo_nlon)+silo_smeans_aavg(i)
      ENDFOR
      silo_smeans_avg(i)=MEAN(silo_smeans(*,*,i),/NaN)
   ENDFOR
   
   silo_smeans_stvar=VARIANCE(silo_smeans,/NaN)
;silo_smeans_stvar=1./(FLOAT(silo_n_valid_pts)*FLOAT(silo_nyears))*$
;                  TOTAL(silo_smeans(*,*,*)^2,/NaN)
;silo_smeans_aavg_stddev=SQRT(1./(FLOAT(silo_nyears))*TOTAL(silo_smeans_aavg^2))
   FOR k=0,n_eots-1 DO BEGIN
      k_str=STRTRIM(STRING(k+1),1)
      silo_smeans_corr_with_aavg=fltarr(silo_nlon,silo_nlat)
      
      silo_smeans_aavg=fltarr(silo_nyears)
      FOR i=0,silo_nyears-1 DO BEGIN
         FOR j=0,silo_nlon-1 DO BEGIN
            temp=REFORM(silo_smeans(j,*,i))
            temp_aavg=TOTAL(silo_aavg_weight*temp,/NaN)
            silo_smeans_aavg(i)=temp_aavg*1./FLOAT(silo_nlon)+silo_smeans_aavg(i)
         ENDFOR
      ENDFOR
;   silo_smeans_stvar=1./(FLOAT(silo_n_valid_pts)*FLOAT(silo_nyears))*$
;                     TOTAL(silo_smeans(*,*,*)^2,/NaN)
      
;   silo_smeans_aavg_stddev=SQRT(1./FLOAT(silo_nyears)*TOTAL(silo_smeans_aavg^2))
      silo_smeans_aavg_stddev=STDDEV(silo_smeans_aavg)
      silo_smeans_aavg_var=VARIANCE(silo_smeans_aavg)
      
      silo_eot_max_corr=0
      FOR i=0,silo_nlon-1 DO BEGIN
         FOR j=0,silo_nlat-1 DO BEGIN
            IF silo_mask(i,j) eq 1.0 and silo_smeans(i,j,0) lt 1E10 THEN BEGIN
               silo_smeans_thispt_ts=REFORM(silo_smeans(i,j,*))
;            thispt_stddev=SQRT((1./FLOAT(silo_nyears))*TOTAL(silo_smeans_thispt_ts^2))
               thispt_stddev=STDDEV(silo_smeans_thispt_ts)
;            silo_smeans_corr_with_aavg(i,j)=(1./FLOAT(silo_nyears)*TOTAL(silo_smeans_thispt_ts*silo_smeans_aavg))/(thispt_stddev*silo_smeans_aavg_stddev)
               silo_smeans_corr_with_aavg(i,j)=CORRELATE(silo_smeans_thispt_ts,silo_smeans_aavg)
               IF silo_smeans_corr_with_aavg(i,j) gt silo_eot_max_corr THEN BEGIN
                  silo_eot_max_corr=silo_smeans_corr_with_aavg(i,j)
                  silo_eot_pt=[i,j]
               ENDIF
            ENDIF ELSE $
               silo_smeans_corr_with_aavg(i,j)=!Values.F_NaN
         ENDFOR
      ENDFOR
   
      print,'EOT'+k_str+': Maximum correlation is '+STRTRIM(STRING(silo_eot_max_corr),1)
      print,'EOT'+k_str+': at point '+STRTRIM(STRING(silo_latitude(silo_eot_pt(1))),1)+' '+STRTRIM(STRING(silo_longitude(silo_eot_pt(0))),1)
      
      silo_smeans_eots_pts(k,*)=[silo_longitude(silo_eot_pt(0)),silo_latitude(silo_eot_pt(1))]
      silo_smeans_eots_ts(k,*)=REFORM(silo_smeans(silo_eot_pt(0),silo_eot_pt(1),*))
      this_eot_ts=fltarr(silo_nyears)
      this_eot_ts(*)=REFORM(silo_smeans_eots_ts(k,*))
      
      FOR i=0,silo_nlon-1 DO BEGIN
         FOR j=0,silo_nlat-1 DO BEGIN
            IF silo_mask(i,j) eq 1.0 and silo_smeans(i,j,0) lt 1E10 THEN BEGIN
               silo_smeans_thispt_ts=REFORM(silo_smeans(i,j,*))
               temp=REGRESS(this_eot_ts,silo_smeans_thispt_ts,CORRELATION=temp2)
               silo_smeans_corr_with_eots(k,i,j)=REFORM(temp2(0))
            ENDIF ELSE $
               silo_smeans_corr_with_eots(k,i,j)=!Values.F_NaN
         ENDFOR
      ENDFOR
      
      silo_smeans_eots_explained=fltarr(silo_nlon,silo_nlat,silo_nyears)
;   silo_smeans_eot_stddev=SQRT((1./FLOAT(silo_nyears))*TOTAL(this_eot_ts^2))
      silo_smeans_eot_stddev=STDDEV(this_eot_ts)
      FOR i=0,silo_nlon-1 DO BEGIN
         FOR j=0,silo_nlat-1 DO BEGIN
            IF silo_mask(i,j) eq 1.0 THEN BEGIN
               silo_smeans_thispt_ts=REFORM(silo_smeans(i,j,*))
;            thispt_stddev=SQRT((1./FLOAT(silo_nyears))*TOTAL(silo_smeans_thispt_ts^2))
               thispt_stddev=STDDEV(silo_smeans_thispt_ts)
                                ;corr_thispt_eot=(1./FLOAT(silo_nyears)*TOTAL(silo_smeans_thispt_ts*this_eot_ts))/(thispt_stddev*silo_smeans_eot_stddev)
            ;IF ABS(corr_thispt_eot) gt 1.01 THEN $
            ;   print,'Correlation > 1 - STOP!  ',corr_thispt_eot
            ;thispt_eot=corr_thispt_eot*thispt_stddev/silo_smeans_eot_stddev
               thispt_eot=CORRELATE(silo_smeans_thispt_ts,this_eot_ts)*thispt_stddev/silo_smeans_eot_stddev
               FOR m=0,silo_nyears-1 DO BEGIN
                  silo_smeans_eots_explained(i,j,m)=this_eot_ts(m)*thispt_eot
                  silo_smeans(i,j,m)=silo_smeans(i,j,m)-silo_smeans_eots_explained(i,j,m)
               ENDFOR
            ENDIF ELSE $
               silo_smeans_eots_explained(i,j,*)=!Values.F_NaN
         ENDFOR
      ENDFOR
;   IF k eq 1 THEN STOP

      silo_smeans_eots_expvar(k)=1./FLOAT(silo_n_valid_pts*silo_nyears)*$
                                 TOTAL(silo_smeans_eots_explained(*,*,*)^2,/NaN)/silo_smeans_stvar
      print,silo_smeans_eots_expvar(k)
      print,VARIANCE(silo_smeans_eots_explained,/NaN)/silo_smeans_stvar
   ENDFOR

   windows=[10,20,30,40,50]
   signif_levels=[0.602,0.433,0.355,0.304,0.273]
   n_windows=N_ELEMENTS(windows)
   items=strarr(n_windows)
   FOR i=0,n_windows-1 DO $
      items(i)='Centred window of '+STRTRIM(STRING(windows(i)+1),1)+' years'
   
   FOR k=0,n_eots-1 DO BEGIN
      
      this_eot=REFORM(where(silo_smeans_eots_expvar eq MAX(silo_smeans_eots_expvar)))
      this_eot_str=STRTRIM(STRING(k+1),1)
      
      mylevs=['-0.30','-0.18','-0.06','0.06','0.18','0.30','0.42','0.54','0.66','0.78','0.90']
      psfile='/home/ss901165/idl/queensland/eots/qld_eots_smeans_silot62.'+smean_period+'_smeans.eot'+this_eot_str+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=112,/PORTRAIT
      CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs)+3,MIN=4,white=[5]
;   CS, COLS=[93, 123, 91, 267, 272, 367, 409, 419, 407]
      MAP,LONMIN=139,LONMAX=153.5,LATMIN=box(0),LATMAX=box(2),/hires
      LEVS,MANUAL=mylevs
      CON,FIELD=REFORM(silo_smeans_corr_with_eots(this_eot,*,*)),X=silo_longitude,Y=silo_latitude,$
          TITLE='EOT'+this_eot_str+' for SILO T62, using seasonal means for '+smean_period+' 1900-2008',/NOLINES,/BLOCK
      black=FSC_COLOR("black",2)
      GPLOT,X=silo_smeans_eots_pts(this_eot,0),Y=silo_smeans_eots_pts(this_eot,1),SYM=8,COL=2
      GPLOT,X=149,Y=-12,TEXT='Explains '+STRMID(STRTRIM(STRING(silo_smeans_eots_expvar(this_eot)*100.),1),0,5)+$
            '% of domain space-time variance'
      PSCLOSE,/NOVIEW

      this_eot_ts=REFORM(silo_smeans_eots_ts(this_eot,*)*365.)
      ymax=ROUND(MAX(this_eot_ts)/100.+1)*100.
      ymin=ROUND(MIN(this_eot_ts)/100.-1)*100.
      
      psfile='/home/ss901165/idl/queensland/eots/qld_eots_smeans_silot62.'+smean_period+'_smeans.eot'+this_eot_str+'_ts.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1800,SPACE2=300,XOFFSET=400,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=112
      GSET,XMIN=1900,XMAX=2008,YMIN=ymin,YMAX=ymax,$
           TITLE='Timeseries of EOT'+this_eot_str+' for SILO T62, using seasonal means for '+smean_period+' 1900-2008'
      black=FSC_COLOR("black",9)
      HIST,X=indgen(silo_nyears)+1900,Y=this_eot_ts,COL=9,WIDTH=50
      AXES,XSTEP=10,YSTEP=100,XMINOR=2,YMINOR=50,YTITLE='EOT timeseries',XTITLE='Year at beginning of '+smean_period+' period'
      GPLOT,X=indgen(silo_nyears)+1900,Y=REPLICATE(0,silo_nyears),STYLE=1
      red=FSC_COLOR("red",10)
      smoothed=SMOOTH(this_eot_ts,11)
      GPLOT,X=indgen(silo_nyears-10)+1905,Y=smoothed(5:silo_nyears-6),STYLE=0,THICK=150,COL=10
      GPLOT,X=1905,Y=ymax-(ymax-ymin)*0.03,ALIGN=0,TEXT='Explains '+STRMID(STRTRIM(STRING(silo_smeans_eots_expvar(this_eot)*100.),1),0,5)+$
            '% of domain space-time variance'
      GPLOT,X=1905,Y=ymax-(ymax-ymin)*0.06,ALIGN=0,TEXT='Correlation with seasonal-mean ('+smean_period+') NINO4: '+$
            STRMID(STRTRIM(STRING(CORRELATE(this_eot_ts,nino4_ts)),1),0,5)
      GPLOT,X=1905,Y=ymax-(ymax-ymin)*0.09,ALIGN=0,TEXT='Correlation with seasonal-mean ('+smean_period+') NINO3.4: '+$
            STRMID(STRTRIM(STRING(CORRELATE(this_eot_ts,nino34_ts)),1),0,5)
      GPLOT,X=1905,Y=ymax-(ymax-ymin)*0.12,ALIGN=0,TEXT='Correlation with seasonal-mean ('+smean_period+') NINO3: '+$
            STRMID(STRTRIM(STRING(CORRELATE(this_eot_ts,nino3_ts)),1),0,5)
      
      PSCLOSE,/NOVIEW
      
      psfile='/home/ss901165/idl/queensland/eots/qld_eots_smeans_silot62.'+smean_period+'_smeans.eot'+this_eot_str+'_nino4.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1800,SPACE2=300,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=112
      GSET,XMIN=1900,XMAX=2008,YMIN=-1,YMAX=1,$
        TITLE='NINO4 correlations with EOT'+this_eot_str+' for SILO T62, using ann-means for '+smean_period+' 1900-2008'
      GPLOT,X=indgen(silo_nyears)+1900,Y=REPLICATE(0,silo_nyears),STYLE=1
      GPLOT,X=1905,Y=0.9,ALIGN=0,TEXT='Explains '+STRMID(STRTRIM(STRING(silo_smeans_eots_expvar(this_eot)*100.),1),0,5)+$
            '% of domain space-time variance'
      GPLOT,X=1905,Y=0.85,ALIGN=0,TEXT='Correlation with seasonal-mean ('+smean_period+') NINO4: '+$
            STRMID(STRTRIM(STRING(CORRELATE(this_eot_ts,nino4_ts)),1),0,5)
      GPLOT,X=1905,Y=0.8,ALIGN=0,TEXT='Correlation with seasonal-mean ('+smean_period+') NINO3.4: '+$
            STRMID(STRTRIM(STRING(CORRELATE(this_eot_ts,nino34_ts)),1),0,5)
      GPLOT,X=1905,Y=0.75,ALIGN=0,TEXT='Correlation with seasonal-mean ('+smean_period+') NINO3: '+$
            STRMID(STRTRIM(STRING(CORRELATE(this_eot_ts,nino3_ts)),1),0,5)
      
      this_eot_corr_nino4_windows=fltarr(n_windows,silo_nyears)
      this_eot_corr_nino4_windows_signif=fltarr(n_windows,silo_nyears)
      this_eot_corr_nino4_windows_signif(*,*)=!Values.F_NaN
      this_eot_corr_nino4_windows(*,*)=!Values.F_NaN
      FOR i=0,n_windows-1 DO BEGIN
         FOR j=windows(i)/2,silo_nyears-windows(i)/2-1 DO BEGIN
            this_eot_corr_nino4_windows(i,j)=CORRELATE(this_eot_ts(j-windows(i)/2:j+windows(i)/2),nino4_ts(j-windows(i)/2:j+windows(i)/2))
            IF ABS(this_eot_corr_nino4_windows(i,j)) gt signif_levels(i) THEN $
               this_eot_corr_nino4_windows_signif(i,j)=this_eot_corr_nino4_windows(i,j)
         ENDFOR
      ENDFOR
      CS,SCALE=26,NCOLS=n_windows+1,/REV   
      FOR i=0,n_windows-1 DO BEGIN
         GPLOT,X=indgen(silo_nyears)+1900,Y=REFORM(this_eot_corr_nino4_windows(i,*)),COL=2+i,THICK=150
         GPLOT,X=indgen(silo_nyears)+1900,Y=REFORM(this_eot_corr_nino4_windows_signif(i,*)),COL=2+i,/NOLINES,SYM=2,SIZE=70
      ENDFOR
      AXES,YSTEP=0.1,NDECS=2,XSTEP=10,YTITLE='Correlation with NINO 4 index (HadISST)',XTITLE='Year at centre of window',XMINOR=5      
      LEGEND,labels=REVERSE(items),COL=REVERSE(indgen(n_windows)+2),LEGPOS=9      
      PSCLOSE,/NOVIEW
      silo_smeans_eots_expvar(this_eot)=0
      
   ENDFOR
ENDFOR   

STOP

END


                         
