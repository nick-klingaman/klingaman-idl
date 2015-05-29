PRO qld_eots_smeans_silo025

; Perform the EOT procedure described in Smith et al. (2004) from van
; den Dool et al. (2000).

n_seasons=4
silo_mask_infile='/home/ss901165/datasets_mango/SILO/one_quarter_lsm.nc'
box=[-29,138,-10,154]
;box=[-15,138,-10,154]
n_eots=3
;sig_level=0.35 ; for 1970-2008
sig_level=0.19 ; for 1900-2008
silo_start_year=1900
silo_offset=silo_start_year-1900
silo_nyears=2011-silo_start_year
nino_start=silo_start_year-1871
silo_start_year_str=STRTRIM(STRING(silo_start_year),1)

FOR p=0,n_seasons-1 DO BEGIN
   CASE p OF
      0 : BEGIN
         smean_period='mar-may'
         ndays_in_season=92
      END
      1 : BEGIN
         smean_period='jun-aug'
         ndays_in_season=92
      END
      2 : BEGIN
         smean_period='sep-nov'
         ndays_in_season=91
      END
      3 : BEGIN
         smean_period='dec-feb'
         ndays_in_season=90
      END
   ENDCASE
   print,'Season: ',smean_period
   silo_smeans_infile='/home/ss901165/datasets_mango/SILO/one_quarter/SILO_precip.'+smean_period+'_smeans.1900-2010.0.25x0.25.nc'
   ;nino4_infile='/home/ss901165/datasets/NINO/nino4_hadisst.'+smean_period+'_smeans.1871-2010.nc'
   ;nino3_infile='/home/ss901165/datasets/NINO/nino3_hadisst.'+smean_period+'_smeans.1871-2010.nc'
   ;nino34_infile='/home/ss901165/datasets/NINO/nino34_hadisst.'+smean_period+'_smeans.1871-2010.nc'   
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
   ;nino4_ts=OPEN_AND_EXTRACT(nino4_infile,'NINO4',offset=[nino_start],count=[silo_nyears])
   ;nino3_ts=OPEN_AND_EXTRACT(nino3_infile,'NINO3',offset=[nino_start],count=[silo_nyears])
   ;nino34_ts=OPEN_AND_EXTRACT(nino34_infile,'NINO34',offset=[nino_start],count=[silo_nyears])
   
                                ; Read precipitation data, mask, and create an area-averaged
                                ; timeseries.
   silo_smeans=OPEN_AND_EXTRACT(silo_smeans_infile,'rain',$
                                offset=[silo_box_tx(1),silo_box_tx(0),silo_offset],$
                                count=[silo_nlon,silo_nlat,silo_nyears])
   silo_mask=REFORM(OPEN_AND_EXTRACT(silo_mask_infile,'lsm',$
                                     offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                                     count=[mask_nlon,mask_nlat,1,1]))
   
   silo_smeans_eots_ts=fltarr(n_eots,silo_nyears)
   silo_smeans_eots_pts=fltarr(n_eots,2)
   silo_smeans_eots_expvar=fltarr(n_eots)
   silo_smeans_eots_total_expvar=fltarr(n_eots)
   silo_smeans_eots_expvar_out=fltarr(n_eots)
   silo_smeans_aavg=fltarr(silo_nyears)
   silo_smeans_avg=fltarr(silo_nyears)
   silo_smeans_corr_with_eots=fltarr(n_eots,silo_nlon,silo_nlat)
   
   FOR i=0,silo_nyears-1 DO BEGIN
      temp=REFORM(silo_smeans(*,*,i))   
      temp[where(silo_mask ne 1.0)]=!Values.F_NaN
      temp[where(temp ge 1E10)]=!Values.F_NaN      
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
   
   silo_aavg_weight=fltarr(silo_nlon,silo_nlat)
   FOR i=0,silo_nlon-1 DO BEGIN
      FOR j=0,silo_nlat-1 DO BEGIN 
         IF FINITE(silo_smeans(i,j,0)) eq 1 THEN BEGIN
            silo_aavg_weight(i,j)=COS(3.14159*silo_latitude(j)/180.)
         ENDIF ELSE $
            silo_aavg_weight(i,j)=0.
      ENDFOR
   ENDFOR
   silo_aavg_weight=silo_aavg_weight/TOTAL(silo_aavg_weight)
   
   silo_smeans_stvar=0.
   FOR i=0,silo_nyears-1 DO BEGIN
      silo_smeans_aavg(i)=TOTAL(REFORM(silo_smeans(*,*,i))*silo_aavg_weight,/NaN)                               
      silo_smeans_stvar=TOTAL(REFORM(silo_smeans(*,*,i)^2)*silo_aavg_weight,/NaN)+silo_smeans_stvar
   ENDFOR
   silo_smeans_stvar=silo_smeans_stvar/FLOAT(silo_nyears)
   print,'Total domain space-time variance: ',silo_smeans_stvar
   
   silo_smeans_eots_total_explained=fltarr(silo_nlon,silo_nlat,silo_nyears)
   FOR k=0,n_eots-1 DO BEGIN
      k_str=STRTRIM(STRING(k+1),1)
      silo_smeans_corr_with_aavg=fltarr(silo_nlon,silo_nlat)
      
      silo_smeans_aavg=fltarr(silo_nyears)
      FOR i=0,silo_nyears-1 DO BEGIN
         silo_smeans_aavg(i)=TOTAL(REFORM(silo_smeans(*,*,i))*silo_aavg_weight,/NaN)
      ENDFOR
      silo_smeans_aavg_stddev=STDDEV(silo_smeans_aavg)
      silo_smeans_aavg_var=VARIANCE(silo_smeans_aavg)
      
      silo_eot_max_explained=0
      FOR i=0,silo_nlon-1 DO BEGIN
         ;print,'Seaching longitude '+STRTRIM(STRING(i+1),1)+' of '+STRTRIM(STRING(silo_nlon),1)
         FOR j=0,silo_nlat-1 DO BEGIN
            IF FINITE(silo_smeans(i,j,0)) eq 1 and TOTAL(silo_smeans(i,j,*)) ne 0 THEN BEGIN
               silo_smeans_thispt_ts=REFORM(silo_smeans(i,j,*))
               variance_explained=0.
               FOR m=0,silo_nlon-1 DO BEGIN
                  FOR n=0,silo_nlat-1 DO BEGIN
                     IF FINITE(silo_smeans(m,n,0)) eq 1 and TOTAL(silo_smeans(m,n,*)) ne 0 THEN BEGIN
                        temp_ts=REFORM(silo_smeans(m,n,*))
                        variance_explained=CORRELATE(temp_ts,silo_smeans_thispt_ts)^2*VARIANCE(temp_ts)+variance_explained
                        IF FINITE(variance_explained) eq 0 THEN STOP
                     ENDIF
                  ENDFOR                     
               ENDFOR
;               IF variance_explained eq 0 THEN STOP
               IF variance_explained gt silo_eot_max_explained THEN BEGIN
                  silo_eot_max_explained=variance_explained
                  silo_eot_pt=[i,j]
               ENDIF
            ENDIF ELSE $
               silo_smeans_corr_with_aavg(i,j)=!Values.F_NaN
         ENDFOR
      ENDFOR
   
      print,'EOT'+k_str+': Maximum correlation is '+STRTRIM(STRING(silo_eot_max_explained),1)
      print,'EOT'+k_str+': at point '+STRTRIM(STRING(silo_latitude(silo_eot_pt(1))),1)+' '+STRTRIM(STRING(silo_longitude(silo_eot_pt(0))),1)
      
      silo_smeans_eots_pts(k,*)=[silo_longitude(silo_eot_pt(0)),silo_latitude(silo_eot_pt(1))]
      silo_smeans_eots_ts(k,*)=REFORM(silo_smeans(silo_eot_pt(0),silo_eot_pt(1),*))
      this_eot_ts=fltarr(silo_nyears)
      this_eot_ts(*)=REFORM(silo_smeans_eots_ts(k,*))
      
      silo_smeans_eots_explained=fltarr(silo_nlon,silo_nlat,silo_nyears)
      silo_smeans_eot_stddev=SQRT(1./FLOAT(silo_nyears)*TOTAL(this_eot_ts^2))
      FOR i=0,silo_nlon-1 DO BEGIN
         FOR j=0,silo_nlat-1 DO BEGIN
            IF FINITE(silo_smeans(i,j,0)) eq 1 THEN BEGIN
               silo_smeans_thispt_ts=REFORM(silo_smeans(i,j,*))
               thispt_stddev=SQRT(1./FLOAT(silo_nyears)*TOTAL(silo_smeans_thispt_ts^2))
               thispt_corr=(1./FLOAT(silo_nyears)*TOTAL(this_eot_ts*silo_smeans_thispt_ts))/(silo_smeans_eot_stddev*thispt_stddev)
               thispt_eot=thispt_corr*thispt_stddev/silo_smeans_eot_stddev
               FOR m=0,silo_nyears-1 DO BEGIN
                  silo_smeans_eots_explained(i,j,m)=this_eot_ts(m)*thispt_eot
                  silo_smeans_eots_total_explained(i,j,m)=silo_smeans_eots_total_explained(i,j,m)+silo_smeans_eots_explained(i,j,m)
                  silo_smeans(i,j,m)=silo_smeans(i,j,m)-silo_smeans_eots_explained(i,j,m)
               ENDFOR
               silo_smeans_corr_with_eots(k,i,j)=thispt_corr
            ENDIF ELSE BEGIN
               silo_smeans_eots_explained(i,j,*)=!Values.F_NaN
               silo_smeans_corr_with_eots(k,i,j)=!Values.F_NaN
            ENDELSE
         ENDFOR
      ENDFOR
      silo_smeans_eots_expvar(k)=0.
      silo_smeans_eots_total_expvar(k)=0.
      FOR m=0,silo_nyears-1 DO BEGIN
         silo_smeans_eots_expvar(k)=TOTAL(REFORM(silo_smeans_eots_explained(*,*,m)^2)*silo_aavg_weight,/NaN)+silo_smeans_eots_expvar(k)      
         silo_smeans_eots_total_expvar(k)=TOTAL(REFORM(silo_smeans_eots_total_explained(*,*,m)^2)*silo_aavg_weight,/NaN)+silo_smeans_eots_total_expvar(k)
      ENDFOR
      silo_smeans_eots_expvar(k)=silo_smeans_eots_expvar(k)/(silo_smeans_stvar*FLOAT(silo_nyears))
      silo_smeans_eots_total_expvar(k)=silo_smeans_eots_total_expvar(k)/(silo_smeans_stvar*FLOAT(silo_nyears))
      print,silo_smeans_eots_expvar(k),silo_smeans_eots_total_expvar(k)
   ENDFOR

   ; Sliding windows for Nino correlations
   ;windows=[10,20,30,40,50]
   ;signif_levels=[0.602,0.433,0.355,0.304,0.273]
   ;n_windows=N_ELEMENTS(windows)
   ;items=strarr(n_windows)
   ;FOR i=0,n_windows-1 DO $
   ;   items(i)='Centred window of '+STRTRIM(STRING(windows(i)+1),1)+' years'
   
   silo_smeans_eots_expvar_out=silo_smeans_eots_expvar
   FOR k=0,n_eots-1 DO BEGIN
      
      this_eot=REFORM(where(silo_smeans_eots_expvar eq MAX(silo_smeans_eots_expvar)))
      this_eot_str=STRTRIM(STRING(k+1),1)
      
      mylevs=['-0.54','-0.42','-0.30','-0.18','-0.06','0.06','0.18','0.30','0.42','0.54','0.66','0.78','0.90']
      psfile='/home/ss901165/idl/queensland/eots/qld_eots_smeans_silo025.'+smean_period+'_smeans.eot'+this_eot_str+'.'+silo_start_year_str+'-2010.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=112,/PORTRAIT
      CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs)+3,MIN=4,white=[7]
;   CS, COLS=[93, 123, 91, 267, 272, 367, 409, 419, 407]
      MAP,LONMIN=138,LONMAX=154,LATMIN=box(0),LATMAX=box(2),/hires
      LEVS,MANUAL=mylevs
      CON,FIELD=REFORM(silo_smeans_corr_with_eots(this_eot,*,*)),X=silo_longitude,Y=silo_latitude,$
          TITLE='EOT'+this_eot_str+' for SILO 0.25, using seasonal means for '+smean_period+' '+silo_start_year_str+'-2010',/NOLINELABELS
      FOR m=0,silo_nlon-1 DO BEGIN
         FOR n=0,silo_nlat-1 DO BEGIN
            IF ABS(silo_smeans_corr_with_eots(this_eot,m,n)) gt sig_level THEN $
               GPLOT,X=silo_longitude(m),Y=silo_latitude(n),SYM=3,SIZE=30
         ENDFOR
      ENDFOR
      black=FSC_COLOR("black",2)
      GPLOT,X=silo_smeans_eots_pts(this_eot,0),Y=silo_smeans_eots_pts(this_eot,1),SYM=8,COL=2
      GPLOT,X=149,Y=-12,TEXT='Explains '+STRMID(STRTRIM(STRING(silo_smeans_eots_expvar(this_eot)*100.),1),0,5)+$
            '% of domain space-time variance'
      PSCLOSE,/NOVIEW

      this_eot_ts=REFORM(silo_smeans_eots_ts(this_eot,*)*FLOAT(ndays_in_season))
      ymax=ROUND(MAX(this_eot_ts)/100.+1)*100.
      ymin=ROUND(MIN(this_eot_ts)/100.-1)*100.
      
      psfile='/home/ss901165/idl/queensland/eots/qld_eots_smeans_silo025.'+smean_period+'_smeans.eot'+this_eot_str+'_ts.'+silo_start_year_str+'-2010.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1800,SPACE2=300,XOFFSET=400,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=112
      GSET,XMIN=1900,XMAX=2010,YMIN=ymin,YMAX=ymax,$
           TITLE='Timeseries of EOT'+this_eot_str+' for SILO 0.25, using seasonal means for '+smean_period+' '+silo_start_year_str+'-2010'
      black=FSC_COLOR("black",9)
      HIST,X=indgen(silo_nyears)+silo_start_year,Y=this_eot_ts,COL=9,WIDTH=50
      AXES,XSTEP=10,YSTEP=100,XMINOR=2,YMINOR=50,YTITLE='EOT timeseries',XTITLE='Year at beginning of '+smean_period+' period'
      GPLOT,X=indgen(silo_nyears)+silo_start_year,Y=REPLICATE(0,silo_nyears),STYLE=1
      red=FSC_COLOR("red",10)
      smoothed=SMOOTH(this_eot_ts,11)
      GPLOT,X=indgen(silo_nyears-10)+silo_start_year+5,Y=smoothed(5:silo_nyears-6),STYLE=0,THICK=150,COL=10

      ; Prints percentage variance explained on plot
      ;GPLOT,X=1905,Y=ymax-(ymax-ymin)*0.03,ALIGN=0,TEXT='Explains '+STRMID(STRTRIM(STRING(silo_smeans_eots_expvar(this_eot)*100.),1),0,5)+$
      ;      '% of domain space-time variance'

      ; Prints Nino correlations on plot
      ;GPLOT,X=1905,Y=ymax-(ymax-ymin)*0.06,ALIGN=0,TEXT='Correlation with seasonal-mean ('+smean_period+') NINO4: '+$
      ;      STRMID(STRTRIM(STRING(CORRELATE(this_eot_ts,nino4_ts)),1),0,5)
      ;GPLOT,X=1905,Y=ymax-(ymax-ymin)*0.09,ALIGN=0,TEXT='Correlation with seasonal-mean ('+smean_period+') NINO3.4: '+$
      ;      STRMID(STRTRIM(STRING(CORRELATE(this_eot_ts,nino34_ts)),1),0,5)
      ;GPLOT,X=1905,Y=ymax-(ymax-ymin)*0.12,ALIGN=0,TEXT='Correlation with seasonal-mean ('+smean_period+') NINO3: '+$
      ;      STRMID(STRTRIM(STRING(CORRELATE(this_eot_ts,nino3_ts)),1),0,5)
      
      PSCLOSE,/NOVIEW

                                ; Plots timeseries of Nino correlations
;      psfile='/home/ss901165/idl/queensland/eots/qld_eots_smeans_silo025.'+smean_period+'_smeans.eot'+this_eot_str+'_nino4.'+silo_start_year_str+'-2010.ps'
;      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1800,SPACE2=300,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=112
;      GSET,XMIN=1900,XMAX=2010,YMIN=-1,YMAX=1,$
;        TITLE='NINO4 correlations with EOT'+this_eot_str+' for SILO 0.25, using ann-means for '+smean_period+' '+silo_start_year_str+'-2010'
;      GPLOT,X=indgen(silo_nyears)+silo_start_year,Y=REPLICATE(0,silo_nyears),STYLE=1
;      GPLOT,X=1905,Y=0.9,ALIGN=0,TEXT='Explains '+STRMID(STRTRIM(STRING(silo_smeans_eots_expvar(this_eot)*100.),1),0,5)+$
;            '% of domain space-time variance'
;      GPLOT,X=1905,Y=0.85,ALIGN=0,TEXT='Correlation with seasonal-mean ('+smean_period+') NINO4: '+$
;            STRMID(STRTRIM(STRING(CORRELATE(this_eot_ts,nino4_ts)),1),0,5)
;      GPLOT,X=1905,Y=0.8,ALIGN=0,TEXT='Correlation with seasonal-mean ('+smean_period+') NINO3.4: '+$
;            STRMID(STRTRIM(STRING(CORRELATE(this_eot_ts,nino34_ts)),1),0,5)
;      GPLOT,X=1905,Y=0.75,ALIGN=0,TEXT='Correlation with seasonal-mean ('+smean_period+') NINO3: '+$
;            STRMID(STRTRIM(STRING(CORRELATE(this_eot_ts,nino3_ts)),1),0,5)
;      
;      this_eot_corr_nino4_windows=fltarr(n_windows,silo_nyears)
;      this_eot_corr_nino4_windows_signif=fltarr(n_windows,silo_nyears)
;      this_eot_corr_nino4_windows_signif(*,*)=!Values.F_NaN
;      this_eot_corr_nino4_windows(*,*)=!Values.F_NaN
;      FOR i=0,n_windows-1 DO BEGIN
;         FOR j=windows(i)/2,silo_nyears-windows(i)/2-1 DO BEGIN
;            this_eot_corr_nino4_windows(i,j)=CORRELATE(this_eot_ts(j-windows(i)/2:j+windows(i)/2),nino4_ts(j-windows(i)/2:j+windows(i)/2))
;            IF ABS(this_eot_corr_nino4_windows(i,j)) gt signif_levels(i) THEN $
;               this_eot_corr_nino4_windows_signif(i,j)=this_eot_corr_nino4_windows(i,j)
;         ENDFOR
;      ENDFOR
;      CS,SCALE=26,NCOLS=n_windows+1,/REV   
;      FOR i=0,n_windows-1 DO BEGIN
;         GPLOT,X=indgen(silo_nyears)+silo_start_year,Y=REFORM(this_eot_corr_nino4_windows(i,*)),COL=2+i,THICK=150
;         GPLOT,X=indgen(silo_nyears)+silo_start_year,Y=REFORM(this_eot_corr_nino4_windows_signif(i,*)),COL=2+i,/NOLINES,SYM=2,SIZE=70
;      ENDFOR
;      AXES,YSTEP=0.1,NDECS=2,XSTEP=10,YTITLE='Correlation with NINO 4 index (HadISST)',XTITLE='Year at centre of window',XMINOR=5      
;      LEGEND,labels=REVERSE(items),COL=REVERSE(indgen(n_windows)+2),LEGPOS=9      
;      PSCLOSE,/NOVIEW      
      silo_smeans_eots_expvar(this_eot)=0      
   ENDFOR

   output_eot_ts=fltarr(silo_nyears,n_eots)
   output_spatial_pattern=fltarr(silo_nlon,silo_nlat,n_eots)
   
   FOR j=0,n_eots-1 DO BEGIN
      FOR i=0,silo_nyears-1 DO $
         output_eot_ts(i,j)=silo_smeans_eots_ts(j,i)
      FOR i=0,silo_nlon-1 DO $
         FOR k=0,silo_nlat-1 DO $
            output_spatial_pattern(i,k,j)=silo_smeans_corr_with_eots(j,i,k)
   ENDFOR
      
   output_file='/home/ss901165/datasets_mango/SILO/one_quarter/SILO.'+smean_period+'_smeans.'+silo_start_year_str+'-2010.eots.nc'
   id=NCDF_CREATE(output_file,/CLOBBER)
   dimids=intarr(4)
   dimids(0)=NCDF_DIMDEF(id,'year',silo_nyears)
   dimids(1)=NCDF_DIMDEF(id,'eot',n_eots)
   dimids(2)=NCDF_DIMDEF(id,'longitude',silo_nlon)
   dimids(3)=NCDF_DIMDEF(id,'latitude',silo_nlat)
   varids=intarr(7)
   varids(0)=NCDF_VARDEF(id,'year',[dimids(0)])
   varids(1)=NCDF_VARDEF(id,'eot',[dimids(1)])
   varids(2)=NCDF_VARDEF(id,'longitude',[dimids(2)])
   varids(3)=NCDF_VARDEF(id,'latitude',[dimids(3)])
   varids(4)=NCDF_VARDEF(id,'loading',[dimids(0),dimids(1)])
   varids(5)=NCDF_VARDEF(id,'spatial_pattern',[dimids(2),dimids(3),dimids(1)])
   varids(6)=NCDF_VARDEF(id,'variance_explained',[dimids(1)])
   NCDF_CONTROL,id,/ENDEF
   NCDF_VARPUT,id,varids(0),indgen(silo_nyears)+silo_start_year
   NCDF_VARPUT,id,varids(1),indgen(n_eots)+1
   NCDF_VARPUT,id,varids(2),silo_longitude
   NCDF_VARPUT,id,varids(3),silo_latitude
   NCDF_VARPUT,id,varids(4),output_eot_ts*FLOAT(ndays_in_season)
   NCDF_VARPUT,id,varids(5),output_spatial_pattern
   NCDF_VARPUT,id,varids(6),silo_smeans_eots_expvar_out
   NCDF_CLOSE,id
ENDFOR   

STOP

END


                         
