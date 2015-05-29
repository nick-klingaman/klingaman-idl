PRO hadgem3_monwg_ga50_timestep_corr_temporalagg

box=[-10,50,10,90]

n_runs=2
n_fields=6
model_names=strarr(n_runs)
model_colors=strarr(n_runs)
random_colors=strarr(n_runs)

temporal=[1,2,3,6,9,18,36,72,144,288,576]
n_temporal=N_ELEMENTS(temporal)
model_lagone_corr=fltarr(n_runs,n_temporal,5)
random_lagone_corr=fltarr(n_runs,n_temporal,5)

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
            model_colors(i)='red'
            random_colors(i)='violetred'         
         END
         1 : BEGIN
            infile='/home/ss901165/um_output3/hadgem3_monwg/anbba/anbbaa.jun-sep_tsmeans.1982.'+file_name+'.nc'
            model_names(i)='ga50_n512'
            model_colors(i)='blue'
            random_colors(i)='cyan'
         END
      ENDCASE
      
      longitude=OPEN_AND_EXTRACT(infile,'longitude')
      latitude=OPEN_AND_EXTRACT(infile,'latitude')
      DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
      n_lon=N_ELEMENTS(longitude)
      n_lat=N_ELEMENTS(latitude)
      n_time=N_ELEMENTS(OPEN_AND_EXTRACT(infile,'t'))
      ;n_time=1000
      
                                ; Model precipitation
      model_precip=OPEN_AND_EXTRACT(infile,netcdf_name,$
                                    offset=[box_tx(1),box_tx(0),0],$
                                    count=[n_lon,n_lat,n_time])  
      FOR j=0,n_temporal-1 DO BEGIN
         this_model_lagone_corr=fltarr(n_lon,n_lat)      
         FOR k=0,n_lon-1 DO BEGIN
            FOR m=0,n_lat-1 DO BEGIN
               temp_precip=REFORM(model_precip(k,m,*))
               IF temporal(j) ne 1 THEN BEGIN
                  this_mean_precip=fltarr(n_time/temporal(j))
                  FOR n=0,n_time/temporal(j)-1 DO $
                     this_mean_precip(n)=MEAN(temp_precip(n*temporal(j):(n+1)*temporal(j)-1))
               ENDIF ELSE $
                  this_mean_precip=temp_precip
               this_model_lagone_corr(k,m)=A_CORRELATE(this_mean_precip,1)
            ENDFOR
         ENDFOR
         sorted=SORT(this_model_lagone_corr)
         n_finite=N_ELEMENTS(where(FINITE(this_model_lagone_corr) eq 1))
         model_lagone_corr(i,j,2)=MEDIAN(this_model_lagone_corr)
         model_lagone_corr(i,j,0)=this_model_lagone_corr(sorted(n_finite*1/100.)) ;MIN(this_model_lagone_corr,/NaN)
         model_lagone_corr(i,j,4)=this_model_lagone_corr(sorted(n_finite*99/100.)) ;MAX(this_model_lagone_corr,/NaN)
         model_lagone_corr(i,j,1)=this_model_lagone_corr(sorted(n_finite/4.))
         model_lagone_corr(i,j,3)=this_model_lagone_corr(sorted(n_finite*3/4.))
      ENDFOR
   ENDFOR

   psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/timestep/hadgem3_monwg_ga50_timestep_corr_temporalagg.IndOcn.'+netcdf_name+'.lagone_corr.equal_tstep.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2500,XOFFSET=500
   GSET,XMIN=0,XMAX=n_temporal,YMIN=-0.7,YMAX=1
   FOR i=0,n_runs-1 DO BEGIN
      FOR j=0,n_temporal-1 DO BEGIN
         EBAR,X=j+0.35+0.3*i,BOX=model_lagone_corr(i,j,*),COL=FSC_COLOR(model_colors(i)),WIDTH=60
;      EBAR,X=j+0.3+0.4*i,BOX=random_lagone_corr(i,j,*),COL=FSC_COLOR(random_colors(i)),WIDTH=60
;      EBAR,X=j+0.4+0.4*i,BOX=scrambled_lagone_corr(i,j,*),COL=FSC_COLOR(scrambled_colors(i)),WIDTH=60
      ENDFOR
   ENDFOR
   xlabels=strarr(n_temporal)
   FOR j=0,n_temporal-1 DO $
      xlabels(j)=STRTRIM(STRING(temporal(j)),1);+'x'+STRTRIM(STRING(temporal_y(j)),1)
   GPLOT,X=[0,n_temporal],Y=[0,0],COL=FSC_COLOR('black'),STYLE=1
   AXES,XVALS=indgen(n_temporal)+0.5,XLABELS=xlabels,YSTEP=0.1,YMINOR=0.05,NDECS=1,$
        YTITLE='Lag-1 corr of gridpoint '+field_name,XTITLE='Length of meaning in native timesteps'
   labels=strarr(n_runs);*3)
   colors=strarr(n_runs);*3)
   FOR i=0,n_runs-1 DO BEGIN
      labels(i)=model_names(i)
      ;labels(i*3+1)=model_names(i)+' random'
      ;labels(i*3+2)=model_names(i)+' scrambled'
      colors(i)=model_colors(i)
      ;colors(i*3+1)=random_colors(i)
      ;colors(i*3+2)=scrambled_colors(i)
   ENDFOR
   GLEGEND,labels=labels,COL=FSC_COLOR(colors),LEGPOS=11
   PSCLOSE,/NOVIEW
   
ENDFOR



STOP
END
