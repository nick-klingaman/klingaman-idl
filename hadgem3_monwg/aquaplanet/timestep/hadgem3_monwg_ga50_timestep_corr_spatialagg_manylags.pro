PRO hadgem3_monwg_ga50_timestep_corr_spatialagg_manylags

box=[-10,50,10,90]
;box=[-5,65,5,75]

n_runs=2
model_names=strarr(n_runs)
styles=intarr(n_runs)

spatial_x= [1,  2, 3, 4, 5, 6, 7, 8];, 10,15,20];,25,30,35,40]
spatial_y= [1,  2, 3, 4, 5, 6, 7, 8];, 10,15,20];,25,30,35,40]
hires_only=[-1,-1,-1,-1,-1,-1,-1,-1];, 0, 0, 0];, 0, 0, 0, 0]
; As offset to spatial_x and spatial_y
equal_area=[4,8,9,10,11,12,13,14]

lags=[indgen(12)+1,24,36,72,144]
n_lags=N_ELEMENTS(lags)

n_fields=6
n_spatial=N_ELEMENTS(spatial_x)
model_autocorr=fltarr(n_runs,n_spatial,n_lags)

FOR p=0,n_fields-1 DO BEGIN
   CASE p OF
      0 : BEGIN
         field_name='total precipitation'
         netcdf_name='precip'
         file_name='precip'
      END
      1 : BEGIN
         field_name='convective rainfall'
         netcdf_name='cvrain'
         file_name='cvrain'
      END
      2 : BEGIN
         field_name='large-scale rainfall'
         netcdf_name='lsrain'
         file_name='lsrain'
      END
      3 : BEGIN
         field_name='deep conv rainfall'
         netcdf_name='precip_1'
         file_name='dcvrain'
      END
      4 : BEGIN
         field_name='mid conv rainfall'
         netcdf_name='precip_3'
         file_name='mcvrain'
      END
      5 : BEGIN
         field_name='shallow conv rainfall'
         netcdf_name='precip_2'
         file_name='scvrain'
      END
   ENDCASE
   FOR i=0,n_runs-1 DO BEGIN
      print,'---> '+STRTRIM(STRING(i),1)
      CASE i OF
         0 : BEGIN
            infile='/home/ss901165/um_output3/hadgem3_monwg/amzgg/amzgga.jun-sep_tsmeans.1982.'+file_name+'.nc'
            model_names(i)='ga50_n96'
         END
         1 : BEGIN
            infile='/home/ss901165/um_output3/hadgem3_monwg/anbba/anbbaa.jun-sep_tsmeans.1982.'+file_name+'.nc'
            model_names(i)='ga50_n512'
         END
      ENDCASE
      
      longitude=OPEN_AND_EXTRACT(infile,'longitude')
      latitude=OPEN_AND_EXTRACT(infile,'latitude')
      DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
      n_lon=N_ELEMENTS(longitude)
      n_lat=N_ELEMENTS(latitude)
      n_time=N_ELEMENTS(OPEN_AND_EXTRACT(infile,'t'))
      ;n_time=100
      
                                ; Model precipitation
      model_precip=OPEN_AND_EXTRACT(infile,netcdf_name,$
                                    offset=[box_tx(1),box_tx(0),0],$
                                    count=[n_lon,n_lat,n_time])   
      
      FOR j=0,n_spatial-1 DO BEGIN  
         IF i gt hires_only(j) THEN BEGIN
            print,'-------> '+STRTRIM(STRING(j),1)
            this_nlon=n_lon/spatial_x(j)
            this_nlat=n_lat/spatial_y(j)
            this_model_autocorr=fltarr(this_nlon,this_nlat,n_lags)
            n_valid_pts=0
            FOR k=0,this_nlon-1 DO BEGIN
               FOR m=0,this_nlat-1 DO BEGIN
                  x_low=k*spatial_x(j)
                  x_high=(k+1)*spatial_x(j)-1
                  y_low=m*spatial_y(j)
                  y_high=(m+1)*spatial_y(j)-1
                  IF j gt 0 and y_high le n_lat-1 and y_low ge 0 $
                     and x_low ge 0 and x_high le n_lon-1 THEN BEGIN
                     model_precip_boxavg=fltarr(n_time)
                     FOR n=LONG(0),LONG(n_time-1) DO BEGIN                  
                        model_precip_box=model_precip(x_low:x_high,y_low:y_high,n)
                        model_precip_boxavg(n)=MEAN(model_precip_box)
                        IF TOTAL(where(model_precip_box gt 0)) ge 0 THEN $;BEGIN
                           temp=N_ELEMENTS(where(model_precip_box gt 0))
;                           n_precip_pts(i,j,temp)=n_precip_pts(i,j,temp)+1
;                        ENDIF ELSE $
;                           n_precip_pts(i,j,0)=n_precip_pts(i,j,0)+1
                     ENDFOR
                     this_model_autocorr(k,m,*)=A_CORRELATE(model_precip_boxavg,lags)                
                     n_valid_pts=n_valid_pts+1
                  ENDIF ELSE IF j gt 0 THEN BEGIN
                     this_model_autocorr(k,m,*)=!Values.F_NaN
                  ENDIF ELSE IF j eq 0 THEN $
                     this_model_autocorr(k,m,*)=A_CORRELATE(model_precip(k,m,*),lags)
               ENDFOR
            ENDFOR
            FOR k=0,n_lags-1 DO BEGIN
               temp=REFORM(this_model_autocorr(*,*,k),[this_nlon*this_nlat])
               model_autocorr(i,j,k)=MEDIAN(temp);[where(FINITE(temp) eq 1)])
            ENDFOR
         ENDIF
      ENDFOR
      
      mylevs=['-0.25','-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95']
      psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/timestep/hadgem3_monwg_ga50_timestep_corr_spatialagg_manylags.'+$
             netcdf_name+'.'+model_names(i)+'.ps'
      PSOPEN,file=psfile,MARGIN=2500,XOFFSET=500,CHARSIZE=120,SPACE2=700,SPACE1=150
      GSET,XMIN=-0.5,XMAX=n_spatial-0.5,YMIN=-0.5,YMAX=n_lags-0.5
      CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs)+1
      LEVS,MANUAL=mylevs
      CON,X=indgen(n_spatial),Y=indgen(n_lags),FIELD=REFORM(model_autocorr(i,*,*)),/NOLINES,/BLOCK,$
          CB_TITLE='Correlation coefficient',TITLE='Lag correlations of '+field_name+' with spatial meaning from '+model_names(i)
      AXES,XVALS=indgen(n_spatial),XLABELS=STRMID(STRTRIM(STRING(spatial_x),1),0,2),$
           YVALS=indgen(n_lags),YLABELS=lags,$
           XTITLE='Length of box for spatial meaning (gridpoints)',$
           YTITLE='Lag for auto-correlation (timesteps)'
      PSCLOSE,/NOVIEW
   ENDFOR
ENDFOR

STOP
END
