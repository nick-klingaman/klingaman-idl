PRO hadgem3kpp_mean_state_drift_diff_lastfirst

n_vars=4
indir='/home/ss901165/um_output6/xgspe'
model_name='hadgem3kpp_1.5xentrain_ga30'
n_years=16

; Take difference of MEAN(last_years) minus MEAN(first_years)
first_years=5
last_years=5

FOR i=0,n_vars-1 DO BEGIN
   CASE i OF
      2 : BEGIN
         varname='precip'
         nc_varname='precip'
         multiplier=86400.         
         vardesc='precipitation rate (mm day!U-1!N)'
         levels_mean=['0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5','8.5','9.5','10.5','11.5','12.5']
         levels_diff=['-3.9','-3.3','-2.7','-2.1','-1.5','-0.9','-0.3','0.3','0.9','1.5','2.1','2.7','3.3','3.9']
         reverse=1
      END
      1 : BEGIN
         varname='olr'
         nc_varname='olr'
         multiplier=1.
         levels_mean=['160','170','180','190','200','210','220','230','240','250','260','270','280',$
                      '290','300','310']
         levels_diff=['-22','-18','-14','-10','-6','-2','2','6','10','14','18','22']
         vardesc='outgoing longwave radiation (W m!U-2!N)'
         reverse=0
      END
      3 : BEGIN
         varname='surf_temp'
         nc_varname='temp'
         multiplier=1.
         vardesc='surface temperature (K)'
         levels_mean=['225','230','235','240','245','250','255','260','265','270','275','280','285','290','295',$
                      '300','305']
         levels_diff=['-3.9','-3.3','-2.7','-2.1','-1.5','-0.9','-0.3','0.3','0.9','1.5','2.1','2.7','3.3','3.9']
         reverse=0
      END
      0 : BEGIN
         varname='sst'
         nc_varname='temp_1'          
         multiplier=1.
         vardesc='sea-surface temperature (K)'
         levels_mean=['275','277','279','281','283','285','287','289','291','293','295','297','299','301','303','305']
         levels_diff=['-0.75','-0.65','-0.55','-0.45','-0.35','-0.25','-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75']
         levels_scaled=['0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95','1.05','1.15','1.25','1.35','1.45','1.55','1.65','1.75','1.85']
         reverse=0
      END
   ENDCASE

   infile=indir+'/hadgem3kpp_1.5xentrain_ga30.jan-dec_ameans.i3-j8.'+varname+'.nc'
   longitude=OPEN_AND_EXTRACT(infile,'longitude')
   latitude=OPEN_AND_EXTRACT(infile,'latitude')
;   DEFINE_BOUNDARIES,box_aavg,latitude,longitude,box_aavg_tx,/LIMIT
   n_lon=N_ELEMENTS(longitude)
   n_lat=N_ELEMENTS(latitude)

   variable=OPEN_AND_EXTRACT(infile,nc_varname,offset=[0,0,0],$
                             count=[n_lon,n_lat,n_years])*multiplier

   variable_first=fltarr(n_lon,n_lat)
   variable_last=fltarr(n_lon,n_lat)
   variable_mean=fltarr(n_lon,n_lat)
   variable_iav=fltarr(n_lon,n_lat)

   FOR j=0,n_lon-1 DO BEGIN
      FOR k=0,n_lat-1 DO BEGIN
         variable_first(j,k)=MEAN(variable(j,k,0:first_years-1))
         variable_last(j,k)=MEAN(variable(j,k,(n_years-last_years):n_years-1))
         variable_mean(j,k)=MEAN(variable(j,k,*))
         variable_iav(j,k)=STDDEV(variable(j,k,*))
      ENDFOR
   ENDFOR

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/drift/hadgem3kpp_mean_state_drift_diff_lastfirst.'+model_name+'.'+varname+'.last'+STRTRIM(STRING(last_years),1)+'.global.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=400
   MAP,/hires
   LEVS,MANUAL=levels_mean
   IF reverse eq 1 THEN BEGIN
      CS,SCALE=1,NCOLS=N_ELEMENTS(levels_mean)+1,/REV
   ENDIF ELSE $
      CS,SCALE=1,NCOLS=N_ELEMENTS(levels_mean)+1
   CON,X=longitude,Y=latitude,FIELD=variable_last,$
       TITLE='Mean of last '+STRTRIM(STRING(last_years),1)+' years of '+model_name+' in '+varname,$
       CB_TITLE=vardesc,/NOLINES
   AXES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/drift/hadgem3kpp_mean_state_drift_diff_lastfirst.'+model_name+'.'+varname+'.first'+STRTRIM(STRING(first_years),1)+'.global.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=400
   MAP,/hires
   LEVS,MANUAL=levels_mean
   IF reverse eq 1 THEN BEGIN
      CS,SCALE=1,NCOLS=N_ELEMENTS(levels_mean)+1,/REV
   ENDIF ELSE $
      CS,SCALE=1,NCOLS=N_ELEMENTS(levels_mean)+1
   CON,X=longitude,Y=latitude,FIELD=variable_first,$
       TITLE='Mean of first '+STRTRIM(STRING(first_years),1)+' years of '+model_name+' in '+varname,$
       CB_TITLE=vardesc,/NOLINES
   AXES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/drift/hadgem3kpp_mean_state_drift_diff_lastfirst.'+model_name+'.'+varname+'.last'+STRTRIM(STRING(last_years),1)+'-minus-first'+STRTRIM(STRING(first_years),1)+'.global.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=400
   MAP,/hires
   LEVS,MANUAL=levels_diff
   IF reverse eq 1 THEN BEGIN
      CS,SCALE=1,NCOLS=N_ELEMENTS(levels_diff)+1,/REV
   ENDIF ELSE $
      CS,SCALE=1,NCOLS=N_ELEMENTS(levels_diff)+1
   CON,X=longitude,Y=latitude,FIELD=variable_last-variable_first,$
       TITLE='Diff beteween mean of last '+STRTRIM(STRING(last_years),1)+' years and first '+$
       STRTRIM(STRING(first_years),1)+' of '+model_name+' in '+varname,$
       CB_TITLE='Difference in '+vardesc,/NOLINES
   AXES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/drift/hadgem3kpp_mean_state_drift_diff_lastfirst.'+model_name+'.'+varname+'.last'+STRTRIM(STRING(last_years),1)+'-minus-first'+STRTRIM(STRING(first_years),1)+'_scaled_iav.global.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=400
   MAP,/hires
   LEVS,MANUAL=levels_scaled   
   CS,SCALE=1,NCOLS=N_ELEMENTS(levels_scaled)+1
   scaled=(variable_last-variable_first)/variable_iav
   IF TOTAL(where(variable_iav lt 0.01)) gt 0 THEN $
      scaled[where(variable_iav lt 0.01)]=!Values.F_NaN
   CON,X=longitude,Y=latitude,FIELD=scaled,$
       TITLE='Diff beteween mean of last '+STRTRIM(STRING(last_years),1)+' years and first '+$
       STRTRIM(STRING(first_years),1)+' of '+model_name+' in '+varname+' - scaled by inter-ann stddev',$
       CB_TITLE='Difference in '+vardesc,/NOLINES
   AXES
   PSCLOSE

ENDFOR

STOP
END
