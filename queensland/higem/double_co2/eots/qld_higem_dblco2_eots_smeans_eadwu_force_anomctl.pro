PRO qld_higem_dblco2_eots_smeans_eadwu_force_anomctl

; Perform the EOT procedure described in Smith et al. (2004) from van
; den Dool et al. (2000).

; Force the basis points from the CTL simulation

n_seasons=4
higem_mask_infile='/home/ss901165/um_output/mask_n144_higam.nc'
box=[-30,138.75,-10,154]
;box=[-15,138,-10,154]
n_eots=6
;sig_level=0.29 ; for 31 years
sig_level=0.22 ; for 51 years
higem_stbl_offset=0
higem_stbl_nyears=31

; Use transient integration?
higem_trans_flag=1
                                ; Appended to psfile name
IF higem_trans_flag eq 1 THEN BEGIN
   filename_runids='eadwu_eafee'
ENDIF ELSE $
   filename_runids='eadwu'
; Offset for the 2% CO2 transient integration (years)
higem_trans_offset=20
; Number of years to read from the 2% CO2 transient integration
higem_trans_nyears=20
; Offset for where to split the 2% CO2 transient integration, to splice in the 2xCO2 integration 
higem_trans_split=11

IF higem_trans_flag eq 1 THEN BEGIN
   higem_nyears=higem_stbl_nyears+higem_trans_nyears
ENDIF ELSE $
   higem_nyears=higem_stbl_nyears

nino_start=0
stbl_year_range_str='o2-r3'
trans_year_range_str='k9-u7'

FOR p=0,n_seasons-1 DO BEGIN
   CASE p OF
      0 : BEGIN
         smean_period='mar-may'
         ndays_in_season=90
      END
      1 : BEGIN
         smean_period='jun-aug'
         ndays_in_season=90
      END
      2 : BEGIN
         smean_period='sep-nov'
         ndays_in_season=90
      END
      3 : BEGIN
         smean_period='dec-feb'
         ndays_in_season=90
      END
   ENDCASE
   print,'Season: ',smean_period
   higem_stbl_smeans_infile='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.'+smean_period+'_smeans.'+$
                            stbl_year_range_str+'.precip.global_domain.nc'
   higem_trans_smeans_infile='/home/ss901165/higem_qccce/es_2pctco2_eafee/higem_eafee.'+smean_period+'_smeans.'+$
                             trans_year_range_str+'.precip.global_domain.nc'

   higem_stbl_nino_infile='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.'+smean_period+'_smeans.'+$
                          stbl_year_range_str+'.nino_indices.nc'
   higem_trans_nino_infile='/home/ss901165/higem_qccce/es_2pctco2_eafee/higem_eafee.'+smean_period+'_smeans.'+$
                           trans_year_range_str+'.nino_indices.nc'

   higem_ctl_eots_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.'+smean_period+'_smeans.h9-w8.eots.nc'
   higem_ctl_smean_clim_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.'+smean_period+'_smean_clim.h9-w8.precip.global_domain.nc'

                                ; Read latitude and longitude from file
   higem_latitude=OPEN_AND_EXTRACT(higem_stbl_smeans_infile,'latitude')
   higem_longitude=OPEN_AND_EXTRACT(higem_stbl_smeans_infile,'longitude')
   DEFINE_BOUNDARIES,box,higem_latitude,higem_longitude,higem_box_tx,/LIMIT
   higem_nlat=N_ELEMENTS(higem_latitude)
   higem_nlon=N_ELEMENTS(higem_longitude)

   higem_latitude_twod=fltarr(higem_nlon,higem_nlat)
   higem_longitude_twod=fltarr(higem_nlon,higem_nlat)
   FOR i=0,higem_nlon-1 DO BEGIN
      FOR j=0,higem_nlat-1 DO BEGIN
         higem_latitude_twod(i,j)=higem_latitude(j)
         higem_longitude_twod(i,j)=higem_longitude(i)
      ENDFOR
   ENDFOR      

   mask_latitude=OPEN_AND_EXTRACT(higem_mask_infile,'latitude')
   mask_longitude=OPEN_AND_EXTRACT(higem_mask_infile,'longitude')
   DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
   mask_nlat=N_ELEMENTS(mask_latitude)
   mask_nlon=N_ELEMENTS(mask_longitude)

                                ; Read NINO annual-mean timeseries
   stbl_nino4_ts=OPEN_AND_EXTRACT(higem_stbl_nino_infile,'nino4',offset=[higem_stbl_offset],count=[higem_stbl_nyears])
   stbl_nino3_ts=OPEN_AND_EXTRACT(higem_stbl_nino_infile,'nino3',offset=[higem_stbl_offset],count=[higem_stbl_nyears])
   stbl_nino34_ts=OPEN_AND_EXTRACT(higem_stbl_nino_infile,'nino34',offset=[higem_stbl_offset],count=[higem_stbl_nyears])
   
   IF higem_trans_flag eq 1 THEN BEGIN
      trans_nino4_ts=OPEN_AND_EXTRACT(higem_trans_nino_infile,'nino4',offset=[higem_trans_offset],count=[higem_trans_nyears])
      trans_nino3_ts=OPEN_AND_EXTRACT(higem_trans_nino_infile,'nino3',offset=[higem_trans_offset],count=[higem_trans_nyears])
      trans_nino34_ts=OPEN_AND_EXTRACT(higem_trans_nino_infile,'nino34',offset=[higem_trans_offset],count=[higem_trans_nyears])

      nino4_ts=[trans_nino4_ts(0:higem_trans_split),stbl_nino4_ts,trans_nino4_ts(higem_trans_split+1:higem_trans_nyears-1)] 
      nino34_ts=[trans_nino34_ts(0:higem_trans_split),stbl_nino34_ts,trans_nino34_ts(higem_trans_split+1:higem_trans_nyears-1)] 
      nino3_ts=[trans_nino3_ts(0:higem_trans_split),stbl_nino3_ts,trans_nino3_ts(higem_trans_split+1:higem_trans_nyears-1)]
   ENDIF ELSE BEGIN
      nino4_ts=stbl_nino4_ts
      nino34_ts=stbl_nino34_ts
      nino3_ts=stbl_nino3_ts
   ENDELSE
   nino4_trend=REGRESS(indgen(higem_nyears),nino4_ts)
   nino4_ts=nino4_ts-nino4_trend(0)*indgen(higem_nyears)
   nino34_trend=REGRESS(indgen(higem_nyears),nino34_ts)
   nino34_ts=nino34_ts-nino34_trend(0)*indgen(higem_nyears)
   nino3_trend=REGRESS(indgen(higem_nyears),nino3_ts)
   nino3_ts=nino3_ts-nino3_trend(0)*indgen(higem_nyears)
   
                                ; Read precipitation data, mask, and create an area-averaged
                                ; timeseries.
   higem_stbl_smeans=OPEN_AND_EXTRACT(higem_stbl_smeans_infile,'precip',$
                                      offset=[higem_box_tx(1),higem_box_tx(0),higem_stbl_offset],$
                                      count=[higem_nlon,higem_nlat,higem_stbl_nyears])*86400.
   higem_ctl_smean_clim=OPEN_AND_EXTRACT(higem_ctl_smean_clim_infile,'precip',$
                                         offset=[higem_box_tx(1),higem_box_tx(0)],$
                                         count=[higem_nlon,higem_nlat])*86400.

   higem_ctl_eots=OPEN_AND_EXTRACT(higem_ctl_eots_infile,'spatial_pattern')

   IF higem_trans_flag eq 1 THEN BEGIN
      higem_trans_smeans=OPEN_AND_EXTRACT(higem_trans_smeans_infile,'precip',$
                                          offset=[higem_box_tx(1),higem_box_tx(0),higem_trans_offset],$
                                          count=[higem_nlon,higem_nlat,higem_trans_nyears])*86400.
      higem_smeans=fltarr(higem_nlon,higem_nlat,higem_nyears)
      FOR i=0,higem_nlon-1 DO BEGIN
         FOR j=0,higem_nlat-1 DO BEGIN
            stbl_temp=REFORM(higem_stbl_smeans(i,j,*))
            trans_temp1=REFORM(higem_trans_smeans(i,j,0:higem_trans_split))
            trans_temp2=REFORM(higem_trans_smeans(i,j,higem_trans_split+1:higem_trans_nyears-1))         
            higem_smeans(i,j,*)=[trans_temp1,stbl_temp,trans_temp2]
         ENDFOR
      ENDFOR
   ENDIF ELSE $
      higem_smeans=higem_stbl_smeans

   higem_mask=REFORM(OPEN_AND_EXTRACT(higem_mask_infile,'lsm',$
                                     offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                                     count=[mask_nlon,mask_nlat,1,1]))
   
                                ; Mark the southern tip of New Guinea as ocean, not land, 
                                ; to prevent being used for the EOT calculation.
   FOR i=0,mask_nlon-1 DO $
      FOR j=0,mask_nlat-1 DO $
         IF mask_longitude(i) gt 146 and mask_latitude(j) gt -13 and higem_mask(i,j) gt 0 THEN $
            higem_mask(i,j)=0.
   
   higem_smeans_eots_ts=fltarr(n_eots,higem_nyears)
   higem_smeans_eots_pts=fltarr(n_eots,2)
   higem_smeans_eots_expvar=fltarr(n_eots)
   higem_smeans_eots_total_expvar=fltarr(n_eots)
   higem_smeans_eots_expvar_out=fltarr(n_eots)
   higem_smeans_aavg=fltarr(higem_nyears)
   higem_smeans_avg=fltarr(higem_nyears)
   higem_smeans_clim=fltarr(higem_nlon,higem_nlat)
   higem_smeans_corr_with_eots=fltarr(n_eots,higem_nlon,higem_nlat)
   
   FOR i=0,higem_nyears-1 DO BEGIN
      temp=REFORM(higem_smeans(*,*,i))
      temp[where(higem_mask ne 1.0)]=!Values.F_NaN
;      temp[where(temp ge 1E10)]=!Values.F_NaN      
      higem_smeans(*,*,i)=temp
      IF i eq 0 THEN $
         higem_n_valid_pts=N_ELEMENTS(where(FINITE(temp) eq 1))
   ENDFOR
   FOR i=0,higem_nlon-1 DO BEGIN
      FOR j=0,higem_nlat-1 DO BEGIN
         IF FINITE(higem_smeans(i,j,0)) eq 1 THEN BEGIN
;            higem_smeans_clim(i,j)=MEAN(higem_smeans(i,j,*),/NaN)
;            higem_smeans(i,j,*)=higem_smeans(i,j,*)-higem_smeans_clim(i,j)
            higem_smeans(i,j,*)=higem_smeans(i,j,*)-higem_ctl_smean_clim(i,j)
         ENDIF
      ENDFOR
   ENDFOR
   
   higem_aavg_weight=fltarr(higem_nlon,higem_nlat)
   FOR i=0,higem_nlon-1 DO BEGIN
      FOR j=0,higem_nlat-1 DO BEGIN 
         IF FINITE(higem_smeans(i,j,0)) eq 1 THEN BEGIN
            higem_aavg_weight(i,j)=COS(3.14159*higem_latitude(j)/180.)
         ENDIF ELSE $
            higem_aavg_weight(i,j)=0.
      ENDFOR
   ENDFOR
   higem_aavg_weight=higem_aavg_weight/TOTAL(higem_aavg_weight)
   
   higem_smeans_stvar=0.
   FOR i=0,higem_nyears-1 DO BEGIN
      higem_smeans_aavg(i)=TOTAL(REFORM(higem_smeans(*,*,i))*higem_aavg_weight,/NaN)                               
      higem_smeans_stvar=TOTAL(REFORM(higem_smeans(*,*,i)^2)*higem_aavg_weight,/NaN)+higem_smeans_stvar
   ENDFOR
   higem_smeans_stvar=higem_smeans_stvar/FLOAT(higem_nyears)
   print,'Total domain space-time variance: ',higem_smeans_stvar
   
   higem_smeans_eots_total_explained=fltarr(higem_nlon,higem_nlat,higem_nyears)
   FOR k=0,n_eots-1 DO BEGIN
      k_str=STRTRIM(STRING(k+1),1)
      higem_smeans_corr_with_aavg=fltarr(higem_nlon,higem_nlat)
      
      higem_smeans_aavg=fltarr(higem_nyears)
      FOR i=0,higem_nyears-1 DO BEGIN
         higem_smeans_aavg(i)=TOTAL(REFORM(higem_smeans(*,*,i))*higem_aavg_weight,/NaN)
      ENDFOR
      higem_smeans_aavg_stddev=STDDEV(higem_smeans_aavg)
      higem_smeans_aavg_var=VARIANCE(higem_smeans_aavg)

      this_ctl_eot=REFORM(higem_ctl_eots(*,*,k))
      higem_eot_pt=intarr(2)
      temp=higem_longitude_twod(where(this_ctl_eot eq MAX(this_ctl_eot)))
      higem_eot_pt(0)=NEAREST(higem_longitude,temp(0))
      temp=higem_latitude_twod(where(this_ctl_eot eq MAX(this_ctl_eot)))
      higem_eot_pt(1)=NEAREST(higem_latitude,temp(0))

      higem_eot_max_explained=0.
      higem_smeans_thispt_ts=REFORM(higem_smeans(higem_eot_pt(0),higem_eot_pt(1),*))
      FOR i=0,higem_nlon-1 DO BEGIN
         FOR j=0,higem_nlat-1 DO BEGIN
            IF FINITE(higem_smeans(i,j,0)) eq 1 and TOTAL(higem_smeans(i,j,*)) ne 0 THEN BEGIN
               temp_ts=REFORM(higem_smeans(i,j,*))
               temp_stddev=STDDEV(temp_ts)
               higem_eot_max_explained=CORRELATE(temp_ts,higem_smeans_thispt_ts)^2*VARIANCE(temp_ts)+higem_eot_max_explained
               IF FINITE(higem_eot_max_explained) eq 0 THEN STOP
            ENDIF
         ENDFOR
      ENDFOR
      
      print,'EOT'+k_str+': Maximum correlation is '+STRTRIM(STRING(higem_eot_max_explained),1)
      print,'EOT'+k_str+': at point '+STRTRIM(STRING(higem_latitude(higem_eot_pt(1))),1)+' '+STRTRIM(STRING(higem_longitude(higem_eot_pt(0))),1)

      higem_smeans_eots_pts(k,*)=[higem_longitude(higem_eot_pt(0)),higem_latitude(higem_eot_pt(1))]
      higem_smeans_eots_ts(k,*)=REFORM(higem_smeans(higem_eot_pt(0),higem_eot_pt(1),*))
      this_eot_ts=fltarr(higem_nyears)
      this_eot_ts(*)=REFORM(higem_smeans_eots_ts(k,*))
      
      ;FOR i=0,higem_nlon-1 DO BEGIN
      ;   FOR j=0,higem_nlat-1 DO BEGIN
      ;      IF higem_mask(i,j) eq 1.0 and higem_smeans(i,j,0) lt 1E10 THEN BEGIN
      ;         higem_smeans_thispt_ts=REFORM(higem_smeans(i,j,*))
      ;         temp=REGRESS(this_eot_ts,higem_smeans_thispt_ts,CORRELATION=temp2)
      ;         higem_smeans_corr_with_eots(k,i,j)=REFORM(temp2(0))
      ;      ENDIF ELSE $
      ;         higem_smeans_corr_with_eots(k,i,j)=!Values.F_NaN
      ;   ENDFOR
      ;ENDFOR
      
      higem_smeans_eots_explained=fltarr(higem_nlon,higem_nlat,higem_nyears)
;      higem_smeans_eot_stddev=STDDEV(this_eot_ts)
      higem_smeans_eot_stddev=SQRT(1./FLOAT(higem_nyears)*TOTAL(this_eot_ts^2))
      FOR i=0,higem_nlon-1 DO BEGIN
         FOR j=0,higem_nlat-1 DO BEGIN
            IF FINITE(higem_smeans(i,j,0)) eq 1 THEN BEGIN
               higem_smeans_thispt_ts=REFORM(higem_smeans(i,j,*))
               ;thispt_stddev=STDDEV(higem_smeans_thispt_ts)
               thispt_stddev=SQRT(1./FLOAT(higem_nyears)*TOTAL(higem_smeans_thispt_ts^2))
               thispt_corr=(1./FLOAT(higem_nyears)*TOTAL(this_eot_ts*higem_smeans_thispt_ts))/(higem_smeans_eot_stddev*thispt_stddev)
               thispt_eot=thispt_corr*thispt_stddev/higem_smeans_eot_stddev
               FOR m=0,higem_nyears-1 DO BEGIN
                  higem_smeans_eots_explained(i,j,m)=this_eot_ts(m)*thispt_eot
                  higem_smeans_eots_total_explained(i,j,m)=higem_smeans_eots_total_explained(i,j,m)+higem_smeans_eots_explained(i,j,m)
                  higem_smeans(i,j,m)=higem_smeans(i,j,m)-higem_smeans_eots_explained(i,j,m)
               ENDFOR
               higem_smeans_corr_with_eots(k,i,j)=thispt_corr
            ENDIF ELSE BEGIN
               higem_smeans_eots_explained(i,j,*)=!Values.F_NaN
               higem_smeans_corr_with_eots(k,i,j)=!Values.F_NaN
            ENDELSE
         ENDFOR
      ENDFOR
      higem_smeans_eots_expvar(k)=0.
      higem_smeans_eots_total_expvar(k)=0.
      FOR m=0,higem_nyears-1 DO BEGIN
         higem_smeans_eots_expvar(k)=TOTAL(REFORM(higem_smeans_eots_explained(*,*,m)^2)*higem_aavg_weight,/NaN)+higem_smeans_eots_expvar(k)      
         higem_smeans_eots_total_expvar(k)=TOTAL(REFORM(higem_smeans_eots_total_explained(*,*,m)^2)*higem_aavg_weight,/NaN)+higem_smeans_eots_total_expvar(k)
      ENDFOR
      higem_smeans_eots_expvar(k)=higem_smeans_eots_expvar(k)/(higem_smeans_stvar*FLOAT(higem_nyears))
      higem_smeans_eots_total_expvar(k)=higem_smeans_eots_total_expvar(k)/(higem_smeans_stvar*FLOAT(higem_nyears))
      print,higem_smeans_eots_expvar(k),higem_smeans_eots_total_expvar(k)
;      print,VARIANCE(higem_smeans_eots_explained,/NaN)/higem_smeans_stvar
   ENDFOR

   windows=[4,6,8,10]
   signif_levels=[0.602,0.433,0.355,0.304,0.273]
   n_windows=N_ELEMENTS(windows)
   items=strarr(n_windows)
   FOR i=0,n_windows-1 DO $
      items(i)='Centred window of '+STRTRIM(STRING(windows(i)+1),1)+' years'
   
   higem_smeans_eots_expvar_out=higem_smeans_eots_expvar
   FOR k=0,n_eots-1 DO BEGIN
      
;      this_eot=REFORM(where(higem_smeans_eots_expvar eq MAX(higem_smeans_eots_expvar)))
;      this_eot_str=STRTRIM(STRING(k+1),1)
      this_eot=k
      this_eot_str=STRTRIM(STRING(k+1),1)
      
      mylevs=['-0.54','-0.42','-0.30','-0.18','-0.06','0.06','0.18','0.30','0.42','0.54','0.66','0.78','0.90']
      psfile='/home/ss901165/idl/queensland/higem/double_co2/eots/qld_higem_eots_'+filename_runids+'_force.'+smean_period+'_smeans_anomctl.eot'+this_eot_str+'.'+$
             stbl_year_range_str+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=112,/PORTRAIT
      CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs)+3,MIN=4,white=[7]
;   CS, COLS=[93, 123, 91, 267, 272, 367, 409, 419, 407]
      MAP,LONMIN=138,LONMAX=154,LATMIN=box(0),LATMAX=box(2),/hires
      LEVS,MANUAL=mylevs
      CON,FIELD=REFORM(higem_smeans_corr_with_eots(this_eot,*,*)),X=higem_longitude,Y=higem_latitude,$
          TITLE='EOT'+this_eot_str+' for HiGEM 2xCO2+2%CO2, using seasonal means for '+smean_period+' '+stbl_year_range_str+'',/NOLINELABELS
      FOR m=0,higem_nlon-1 DO BEGIN
         FOR n=0,higem_nlat-1 DO BEGIN
            IF ABS(higem_smeans_corr_with_eots(this_eot,m,n)) gt sig_level THEN $
               GPLOT,X=higem_longitude(m),Y=higem_latitude(n),SYM=3,SIZE=30
         ENDFOR
      ENDFOR
      black=FSC_COLOR("black",2)
      GPLOT,X=higem_smeans_eots_pts(this_eot,0),Y=higem_smeans_eots_pts(this_eot,1),SYM=8,COL=2
      GPLOT,X=149,Y=-11,TEXT='Explains '+STRMID(STRTRIM(STRING(higem_smeans_eots_expvar(this_eot)*100.),1),0,5)+$
            '% of domain space-time variance'
      PSCLOSE,/NOVIEW

      this_eot_ts=REFORM(higem_smeans_eots_ts(this_eot,*)*FLOAT(ndays_in_season))
      ymax=ROUND(MAX(this_eot_ts)/100.+1)*100.
      ymin=ROUND(MIN(this_eot_ts)/100.-1)*100.
      
      psfile='/home/ss901165/idl/queensland/higem/double_co2/eots/qld_higem_eots_'+filename_runids+'_force.'+smean_period+'_smeans_anomctl.eot'+this_eot_str+$
             '_ts.'+stbl_year_range_str+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1800,SPACE2=200,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=112,SPACE3=200
      GSET,XMIN=0,XMAX=higem_nyears,YMIN=ymin,YMAX=ymax,$
           TITLE='Timeseries of EOT'+this_eot_str+' for HiGEM 2xCO2+2%CO2, using seasonal means for '+smean_period+' '+stbl_year_range_str+''
      black=FSC_COLOR("black",9)
      HIST,X=indgen(higem_nyears)+higem_stbl_offset,Y=this_eot_ts,COL=9,WIDTH=50
      AXES,XSTEP=10,YSTEP=100,XMINOR=2,YMINOR=50,YTITLE='EOT timeseries',XTITLE='Year at beginning of '+smean_period+' period'
      GPLOT,X=indgen(higem_nyears)+higem_stbl_offset,Y=REPLICATE(0,higem_nyears),STYLE=1
      red=FSC_COLOR("red",10)
      smoothed=SMOOTH(this_eot_ts,11)
      GPLOT,X=indgen(higem_nyears-10)+higem_stbl_offset+5,Y=smoothed(5:higem_nyears-6),STYLE=0,THICK=150,COL=10
      GPLOT,X=higem_stbl_offset+5,Y=ymax-(ymax-ymin)*0.03,ALIGN=0,TEXT='Explains '+STRMID(STRTRIM(STRING(higem_smeans_eots_expvar(this_eot)*100.),1),0,5)+$
            '% of domain space-time variance'
      GPLOT,X=higem_stbl_offset+5,Y=ymax-(ymax-ymin)*0.06,ALIGN=0,TEXT='Correlation with seasonal-mean ('+smean_period+') NINO4: '+$
            STRMID(STRTRIM(STRING(CORRELATE(this_eot_ts,nino4_ts)),1),0,5)
      GPLOT,X=higem_stbl_offset+5,Y=ymax-(ymax-ymin)*0.09,ALIGN=0,TEXT='Correlation with seasonal-mean ('+smean_period+') NINO3.4: '+$
            STRMID(STRTRIM(STRING(CORRELATE(this_eot_ts,nino34_ts)),1),0,5)
      GPLOT,X=higem_stbl_offset+5,Y=ymax-(ymax-ymin)*0.12,ALIGN=0,TEXT='Correlation with seasonal-mean ('+smean_period+') NINO3: '+$
            STRMID(STRTRIM(STRING(CORRELATE(this_eot_ts,nino3_ts)),1),0,5)
      
      PSCLOSE,/NOVIEW
      
      psfile='/home/ss901165/idl/queensland/higem/double_co2/eots/qld_higem_eots_'+filename_runids+'_force.'+smean_period+'_smeans_anomctl.eot'+this_eot_str+$
             '_nino4.'+stbl_year_range_str+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1800,SPACE2=200,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=112
      GSET,XMIN=0,XMAX=higem_nyears,YMIN=-1,YMAX=1,$
        TITLE='NINO4 correlations with EOT'+this_eot_str+' for HiGEM 2xCO2+2%CO2, using ann-means for '+smean_period+' '+stbl_year_range_str+''
      GPLOT,X=indgen(higem_nyears)+higem_stbl_offset,Y=REPLICATE(0,higem_nyears),STYLE=1
      GPLOT,X=higem_stbl_offset+5,Y=0.9,ALIGN=0,TEXT='Explains '+STRMID(STRTRIM(STRING(higem_smeans_eots_expvar(this_eot)*100.),1),0,5)+$
            '% of domain space-time variance'
      GPLOT,X=higem_stbl_offset+5,Y=0.85,ALIGN=0,TEXT='Correlation with seasonal-mean ('+smean_period+') NINO4: '+$
            STRMID(STRTRIM(STRING(CORRELATE(this_eot_ts,nino4_ts)),1),0,5)
      GPLOT,X=higem_stbl_offset+5,Y=0.8,ALIGN=0,TEXT='Correlation with seasonal-mean ('+smean_period+') NINO3.4: '+$
            STRMID(STRTRIM(STRING(CORRELATE(this_eot_ts,nino34_ts)),1),0,5)
      GPLOT,X=higem_stbl_offset+5,Y=0.75,ALIGN=0,TEXT='Correlation with seasonal-mean ('+smean_period+') NINO3: '+$
            STRMID(STRTRIM(STRING(CORRELATE(this_eot_ts,nino3_ts)),1),0,5)
      
      this_eot_corr_nino4_windows=fltarr(n_windows,higem_nyears)
      this_eot_corr_nino4_windows_signif=fltarr(n_windows,higem_nyears)
      this_eot_corr_nino4_windows_signif(*,*)=!Values.F_NaN
      this_eot_corr_nino4_windows(*,*)=!Values.F_NaN
      FOR i=0,n_windows-1 DO BEGIN
         FOR j=windows(i)/2,higem_nyears-windows(i)/2-1 DO BEGIN
            this_eot_corr_nino4_windows(i,j)=CORRELATE(this_eot_ts(j-windows(i)/2:j+windows(i)/2),nino4_ts(j-windows(i)/2:j+windows(i)/2))
            IF ABS(this_eot_corr_nino4_windows(i,j)) gt signif_levels(i) THEN $
               this_eot_corr_nino4_windows_signif(i,j)=this_eot_corr_nino4_windows(i,j)
         ENDFOR
      ENDFOR
      CS,SCALE=26,NCOLS=n_windows+1,/REV   
      FOR i=0,n_windows-1 DO BEGIN
         GPLOT,X=indgen(higem_nyears)+higem_stbl_offset,Y=REFORM(this_eot_corr_nino4_windows(i,*)),COL=2+i,THICK=150
         GPLOT,X=indgen(higem_nyears)+higem_stbl_offset,Y=REFORM(this_eot_corr_nino4_windows_signif(i,*)),COL=2+i,/NOLINES,SYM=2,SIZE=70
      ENDFOR
      AXES,YSTEP=0.1,NDECS=2,XSTEP=10,YTITLE='Correlation with NINO 4 index (HadISST)',XTITLE='Year at centre of window',XMINOR=5      
      GLEGEND,labels=REVERSE(items),COL=REVERSE(indgen(n_windows)+2),LEGPOS=9      
      PSCLOSE,/NOVIEW      
      higem_smeans_eots_expvar(this_eot)=0      
   ENDFOR

   output_eot_ts=fltarr(higem_nyears,n_eots)
   output_eot_raw_ts=fltarr(higem_nyears,n_eots)
   output_spatial_pattern=fltarr(higem_nlon,higem_nlat,n_eots)
   
   FOR j=0,n_eots-1 DO BEGIN
      FOR i=0,higem_nyears-1 DO BEGIN
         output_eot_ts(i,j)=higem_smeans_eots_ts(j,i)
;         output_eot_raw_ts(i,j)=higem_smeans_eots_raw_ts(j,i)
      ENDFOR
      FOR i=0,higem_nlon-1 DO $
         FOR k=0,higem_nlat-1 DO $
            output_spatial_pattern(i,k,j)=higem_smeans_corr_with_eots(j,i,k)
   ENDFOR
      
   output_file='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_'+filename_runids+'_force.'+smean_period+'_smeans_anomctl.'+stbl_year_range_str+'.eots.nc'
   id=NCDF_CREATE(output_file,/CLOBBER)
   dimids=intarr(4)
   dimids(0)=NCDF_DIMDEF(id,'year',higem_nyears)
   dimids(1)=NCDF_DIMDEF(id,'eot',n_eots)
   dimids(2)=NCDF_DIMDEF(id,'longitude',higem_nlon)
   dimids(3)=NCDF_DIMDEF(id,'latitude',higem_nlat)
   varids=intarr(8)
   varids(0)=NCDF_VARDEF(id,'year',[dimids(0)])
   varids(1)=NCDF_VARDEF(id,'eot',[dimids(1)])
   varids(2)=NCDF_VARDEF(id,'longitude',[dimids(2)])
   varids(3)=NCDF_VARDEF(id,'latitude',[dimids(3)])
   varids(4)=NCDF_VARDEF(id,'loading',[dimids(0),dimids(1)])
   varids(5)=NCDF_VARDEF(id,'spatial_pattern',[dimids(2),dimids(3),dimids(1)])
   varids(6)=NCDF_VARDEF(id,'variance_explained',[dimids(1)])
;   varids(7)=NCDF_VARDEF(id,'loading_raw',[dimids(0),dimids(1)])
   NCDF_CONTROL,id,/ENDEF
   NCDF_VARPUT,id,varids(0),indgen(higem_nyears)+higem_stbl_offset
   NCDF_VARPUT,id,varids(1),indgen(n_eots)+1
   NCDF_VARPUT,id,varids(2),higem_longitude
   NCDF_VARPUT,id,varids(3),higem_latitude
   NCDF_VARPUT,id,varids(4),output_eot_ts*FLOAT(ndays_in_season)
   NCDF_VARPUT,id,varids(5),output_spatial_pattern
   NCDF_VARPUT,id,varids(6),higem_smeans_eots_expvar_out
;   NCDF_VARPUT,id,varids(7),output_eot_raw_ts*FLOAT(ndays_in_season)
   NCDF_CLOSE,id
ENDFOR   

STOP

END


                         
