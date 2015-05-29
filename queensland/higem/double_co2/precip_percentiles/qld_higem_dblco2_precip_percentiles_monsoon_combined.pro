PRO qld_higem_dblco2_precip_percentiles_monsoon_combined

onset_threshold=15
retreat_threshold=85

monbox=[-30,112,-10,154]
;nmonbox=[-20,120,-5,154]
histmax=15
histmin=-15
;qldbox=[-30,138,-10,154]
;histmax=20
;histmin=-40
box=monbox
region_name='monsoon'
start_date=120
stop_date=359
max_years=150
onset_frac=0.15
retreat_frac=0.85
pct_name='1585'
n_days=(stop_date-start_date+1)

; 5/95 for all
;mean_onset_levs=[148,156,164,172,180,188,196,204,212,220,228,236,244,252,260]+120
;onset_levs=[120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280]+120
;retreat_levs=[275,280,285,290,295,300,305,310,315,320,325,330,335,340,345,350,355,360]+120
;mean_retreat_levs=[300,305,310,315,320,325,330,335,340,345,350,355,360]+120
;diff_levs=['-17','-15','-13','-11','-9','-7','-5','-3','-1','1','3','5','7','9','11','13','15','17']

; 10/90 for all
;mean_onset_levs=[164,172,180,188,196,204,212,220,228,236,244,252,260,268]+120
;onset_levs=[120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280]+120
;retreat_levs=[275,280,285,290,295,300,305,310,315,320,325,330,335,340,345,350,355,360]+120
;mean_retreat_levs=[290,295,300,305,310,315,320,325,330,335,340,345,350,355]+120
;diff_levs=['-17','-15','-13','-11','-9','-7','-5','-3','-1','1','3','5','7','9','11','13','15','17']

; 15/85 for all
mean_onset_levs=[180,188,196,204,212,220,228,236,244,252,260,268,276,284]+120
onset_levs=[120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280]+120
retreat_levs=[275,280,285,290,295,300,305,310,315,320,325,330,335,340,345,350,355]+120
mean_retreat_levs=[280,285,290,295,300,305,310,315,320,325,330,335,340,345]+120
diff_levs=[-26,-22,-18,-14,-10,-6,-2,2,6,10,14,18,22,26]

n_onset_levs=N_ELEMENTS(onset_levs)
n_mean_onset_levs=N_ELEMENTS(mean_onset_levs)

;mean_retreat_levs=retreat_levs
n_retreat_levs=N_ELEMENTS(retreat_levs)
n_mean_retreat_levs=N_ELEMENTS(mean_retreat_levs)


mask_infile='/home/ss901165/um_output/mask_n144_higam.nc'
mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)

mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                      offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                      count=[mask_nlon,mask_nlat,1,1]))

n_models=2
FOR i=0,n_models-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.may-apr_dmeans.h9-w8.precip.global_domain.nc'
         n_years=149
         model_name='higem_ctl'
      END
      1 : BEGIN
         infile='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eafee_eadwu.may-apr_dmeans.m9-s0.precip.global_domain.nc'
         n_years=50
         model_name='higem_2xco2'
      END
   ENDCASE
   
   latitude=OPEN_AND_EXTRACT(infile,'latitude')
   longitude=OPEN_AND_EXTRACT(infile,'longitude')
   DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
   n_lat=N_ELEMENTS(latitude)
   n_lon=N_ELEMENTS(longitude)
   
   daily_precip=OPEN_AND_EXTRACT(infile,'precip',offset=[box_tx(1),box_tx(0),start_date,0],$
                                 count=[n_lon,n_lat,n_days,n_years])*86400.

   IF i eq 0 THEN BEGIN
      onset_dates=fltarr(n_models,n_lon,n_lat,max_years)
      mean_onset_dates=fltarr(n_models,n_lon,n_lat)
      retreat_dates=fltarr(n_models,n_lon,n_lat,max_years)
      mean_retreat_dates=fltarr(n_models,n_lon,n_lat)
      model_names=strarr(n_models)
      onset_pdf=fltarr(n_models,N_ELEMENTS(onset_levs)-1)
      onset_mid=fltarr(N_ELEMENTS(onset_levs)-1)
      retreat_pdf=fltarr(n_models,N_ELEMENTS(retreat_levs)-1)
      retreat_mid=fltarr(N_ELEMENTS(retreat_levs)-1)
   ENDIF
   model_names(i)=model_name

   FOR j=0,n_lon-1 DO BEGIN
      FOR k=0,n_lat-1 DO BEGIN
         season_total=fltarr(n_years)
         cumulative=fltarr(n_days,n_years)
         FOR m=0,n_years-1 DO BEGIN
            thispt_thisyear_precip=REFORM(daily_precip(j,k,*,m))
            season_total(m)=TOTAL(thispt_thisyear_precip)
            ;cumulative=fltarr(n_days)
            FOR n=0,n_days-1 DO $
               cumulative(n,m)=TOTAL(thispt_thisyear_precip(0:n))/season_total(m)
            onset_dates(i,j,k,m)=NEAREST(REFORM(cumulative(*,m)),onset_frac)+start_date+120
            retreat_dates(i,j,k,m)=NEAREST(REFORM(cumulative(*,m)),retreat_frac)+start_date+120
         ENDFOR
         IF n_years lt max_years THEN BEGIN
            onset_dates(i,j,k,n_years:max_years-1)=!Values.F_NaN
            retreat_dates(i,j,k,n_years:max_years-1)=!Values.F_NaN
         ENDIF
         mean_onset_dates(i,j,k)=MEAN(onset_dates(i,j,k,*),/NaN)
         mean_retreat_dates(i,j,k)=MEAN(retreat_dates(i,j,k,*),/NaN)
      ENDFOR
   ENDFOR
   
   land_onset_dates=fltarr(n_lon,n_lat,n_years)
   land_retreat_dates=fltarr(n_lon,n_lat,n_years)
   FOR j=0,n_years-1 DO BEGIN
      temp=REFORM(onset_dates(i,*,*,j))
      temp[where(mask eq 0)]=!Values.F_NaN
      land_onset_dates(*,*,j)=temp
      temp=REFORM(retreat_dates(i,*,*,j))
      temp[where(mask eq 0)]=!Values.F_NaN
      land_retreat_dates(*,*,j)=temp
   ENDFOR
   FOR j=0,N_ELEMENTS(onset_levs)-2 DO BEGIN
      onset_pdf(i,j)=N_ELEMENTS(where(land_onset_dates ge onset_levs(j) and land_onset_dates lt onset_levs(j+1)))
      onset_mid(j)=(onset_levs(j+1)-onset_levs(j))/2.+onset_levs(j)
   ENDFOR
   FOR j=0,N_ELEMENTS(retreat_levs)-2 DO BEGIN
      retreat_pdf(i,j)=N_ELEMENTS(where(land_retreat_dates ge retreat_levs(j) and land_retreat_dates lt retreat_levs(j+1)))
      retreat_mid(j)=(retreat_levs(j+1)-retreat_levs(j))/2.+retreat_levs(j)
   ENDFOR
   onset_pdf(i,*)=onset_pdf(i,*)/FLOAT(N_ELEMENTS(where(FINITE(land_onset_dates) eq 1)))
   retreat_pdf(i,*)=retreat_pdf(i,*)/FLOAT(N_ELEMENTS(where(FINITE(land_retreat_dates) eq 1)))
   
   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/'+$
          'qld_higem_dblco2_precip_percentiles_monsoon_combined.onset_spatial.'+model_name+'.'+region_name+'_region.'+pct_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=1000,TFONT=3,$
          TCHARSIZE=100,SPACE3=500,/PORTRAIT
   CS,SCALE=34,NCOLS=N_ELEMENTS(mean_onset_levs)+1,white=[2]
   LEVS,MANUAL=mean_onset_levs
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
   CON,X=longitude,Y=latitude,FIELD=REFORM(mean_onset_dates(i,*,*)),$
       TITLE='Mean onset dates ('+STRTRIM(STRING(onset_threshold),1)+'%ile) for '+model_name,/NOLINES,/BLOCK
   AXES
   ;COLBAR,COORDS=[1000,1000,12000,2000],TITLE='Date of monsoon onset',$
   ;       LABELS=['21 Oct','1 Nov','11 Nov','21 Nov','1 Dec','11 Dec','21 Dec','1 Jan','11 Jan',$
   ;               '21 Jan','1 Feb','11 Feb','21 Feb','1 Mar']
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/'+$
          'qld_higem_dblco2_precip_percentiles_monsoon_combined.retreat_spatial.'+model_name+'.'+region_name+'_region.'+pct_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=1000,TFONT=3,$
          TCHARSIZE=100,SPACE3=500,/PORTRAIT
   CS,SCALE=34,NCOLS=N_ELEMENTS(mean_retreat_levs)+1
   LEVS,MANUAL=mean_retreat_levs
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
   CON,X=longitude,Y=latitude,FIELD=REFORM(mean_retreat_dates(i,*,*)),TITLE='Mean retreat dates ('+$
       STRTRIM(STRING(retreat_threshold),1)+'%ile) for '+model_name,/NOLINES,/BLOCK
   AXES
   PSCLOSE,/NOVIEW
   
   IF i gt 0 THEN BEGIN     
      psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/'+$
             'qld_higem_dblco2_precip_percentiles_monsoon_combined.onset_spatial.'+model_names(i)+'-minus-'+model_names(0)+'.'+region_name+'_region.'+pct_name+'.ps'
      PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=1000,TFONT=3,$
             TCHARSIZE=100,SPACE3=500,/PORTRAIT
      CS,SCALE=1,NCOLS=N_ELEMENTS(diff_levs)+1
      LEVS,MANUAL=diff_levs
      MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
      CON,X=longitude,Y=latitude,FIELD=REFORM(mean_onset_dates(i,*,*))-REFORM(mean_onset_dates(0,*,*)),$
          TITLE='Mean onset dates ('+STRTRIM(STRING(onset_threshold),1)+'%ile) for '+model_names(i)+' minus '+$
          model_names(0),/BLOCK,/NOLINES
      AXES
      PSCLOSE,/NOVIEW
      
      psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/'+$
             'qld_higem_dblco2_precip_percentiles_monsoon_combined.retreat_spatial.'+model_names(i)+'-minus-'+model_names(0)+'.'+region_name+'_region.'+pct_name+'.ps'
      PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=1000,TFONT=3,$
             TCHARSIZE=100,SPACE3=500,/PORTRAIT
      CS,SCALE=1,NCOLS=N_ELEMENTS(diff_levs)+1
      LEVS,MANUAL=diff_levs
      MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
      CON,X=longitude,Y=latitude,FIELD=(REFORM(mean_retreat_dates(i,*,*))-REFORM(mean_retreat_dates(0,*,*)))*1.5,$
          TITLE='Mean retreat dates ('+STRTRIM(STRING(retreat_threshold),1)+'%ile) for '+model_names(i)+' minus '+$
          model_names(0),/BLOCK,/NOLINES
      AXES
      PSCLOSE,/NOVIEW
   ENDIF
ENDFOR

onset_change_byonset_mean=fltarr(n_mean_onset_levs-1)
onset_change_byonset_stddev=fltarr(n_mean_onset_levs-1)
retreat_change_byonset_mean=fltarr(n_mean_onset_levs-1)
retreat_change_byonset_stddev=fltarr(n_mean_onset_levs-1)
onset_change_byretreat_mean=fltarr(n_mean_retreat_levs-1)
onset_change_byretreat_stddev=fltarr(n_mean_retreat_levs-1)
retreat_change_byretreat_mean=fltarr(n_mean_retreat_levs-1)
retreat_change_byretreat_stddev=fltarr(n_mean_retreat_levs-1)

mean_onset_mid=fltarr(n_mean_onset_levs-1)
mean_retreat_mid=fltarr(n_mean_retreat_levs-1)

control_land_onsets=REFORM(mean_onset_dates(0,*,*))
control_land_retreats=REFORM(mean_retreat_dates(0,*,*))
control_land_onsets[where(mask eq 0)]=!Values.F_NaN
control_land_retreats[where(mask eq 0)]=!Values.F_NaN
doubleco2_land_onsets=REFORM(mean_onset_dates(1,*,*))
doubleco2_land_onsets[where(mask eq 0)]=!Values.F_NaN
doubleco2_land_retreats=REFORM(mean_retreat_dates(1,*,*))
doubleco2_land_retreats[where(mask eq 0)]=!Values.F_NaN

FOR i=0,n_mean_onset_levs-2 DO BEGIN
   valid=where(control_land_onsets gt mean_onset_levs(i) and control_land_onsets le mean_onset_levs(i+1))
   IF N_ELEMENTS(valid) gt 5 THEN BEGIN
      onset_change_byonset_mean(i)=MEAN(doubleco2_land_onsets[valid]-control_land_onsets[valid])
      onset_change_byonset_stddev(i)=STDDEV(doubleco2_land_onsets[valid]-control_land_onsets[valid])
      retreat_change_byonset_mean(i)=MEAN(doubleco2_land_retreats[valid]-control_land_retreats[valid])
      retreat_change_byonset_stddev(i)=STDDEV(doubleco2_land_retreats[valid]-control_land_retreats[valid])
   ENDIF ELSE BEGIN
      onset_change_byonset_mean(i)=0
      onset_change_byonset_stddev(i)=0
      retreat_change_byonset_mean(i)=0
      retreat_change_byonset_stddev(i)=0
   ENDELSE
   mean_onset_mid(i)=(mean_onset_levs(i)+mean_onset_levs(i+1))/2.
ENDFOR
FOR i=0,n_mean_retreat_levs-2 DO BEGIN
   valid=where(control_land_retreats gt mean_retreat_levs(i) and control_land_retreats le mean_retreat_levs(i+1))
   IF N_ELEMENTS(valid) gt 5 THEN BEGIN
      onset_change_byretreat_mean(i)=MEAN(doubleco2_land_onsets[valid]-control_land_onsets[valid])
      onset_change_byretreat_stddev(i)=STDDEV(doubleco2_land_onsets[valid]-control_land_onsets[valid])
      retreat_change_byretreat_mean(i)=MEAN(doubleco2_land_retreats[valid]-control_land_retreats[valid])
      retreat_change_byretreat_stddev(i)=STDDEV(doubleco2_land_retreats[valid]-control_land_retreats[valid])
   ENDIF ELSE BEGIN
      onset_change_byretreat_mean(i)=0
      onset_change_byretreat_stddev(i)=0
      retreat_change_byretreat_mean(i)=0
      retreat_change_byretreat_stddev(i)=0
   ENDELSE
   mean_retreat_mid(i)=(mean_retreat_levs(i)+mean_retreat_levs(i+1))/2.
ENDFOR


hist_offsets=[(onset_mid(2)-onset_mid(1))*(-1./5.),(onset_mid(2)-onset_mid(1))*(1./5.)]
psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/qld_higem_dblco2_precip_percentiles_monsoon_combined.onset_pdf.'+region_name+'_region.'+pct_name+'.ps'
PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=1000,TFONT=3,$
        TCHARSIZE=100,SPACE3=500
GSET,XMIN=MIN(onset_levs),XMAX=MAX(onset_levs),YMIN=0,YMAX=0.4,TITLE='PDF of onset dates over all land points in '+region_name+' region'
colors=['black','red']
FOR i=0,n_models-1 DO $
   HIST,X=onset_mid+hist_offsets(i),Y=REFORM(onset_pdf(i,*)),FILLCOL=FSC_COLOR(colors(i))
AXES,XSTEP=(onset_mid(2)-onset_mid(1)),YSTEP=0.03,XMINOR=(onset_mid(2)-onset_mid(1))/2.,YMINOR=0.015,NDECS=3
PSCLOSE,/NOVIEW

hist_offsets=[(retreat_mid(2)-retreat_mid(1))*(-1./5.),(retreat_mid(2)-retreat_mid(1))*(1./5.)]
psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/qld_higem_dblco2_precip_percentiles_monsoon_combined.retreat_pdf.'+region_name+'_region.'+pct_name+'.ps'
PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=1000,TFONT=3,$
        TCHARSIZE=100,SPACE3=500
GSET,XMIN=MIN(retreat_levs),XMAX=MAX(retreat_levs),YMIN=0,YMAX=0.4,TITLE='PDF of retreat dates over all land points in '+region_name+' region'
colors=['black','red']
FOR i=0,n_models-1 DO $
   HIST,X=retreat_mid+hist_offsets(i),Y=REFORM(retreat_pdf(i,*)),FILLCOL=FSC_COLOR(colors(i))
GLEGEND,labels=REVERSE(model_names),COL=REVERSE(FSC_COLOR(colors)),TYPE=1,LEGPOS=1
AXES,XSTEP=(retreat_mid(2)-retreat_mid(1)),YSTEP=0.03,XMINOR=(retreat_mid(2)-retreat_mid(1))/2.,YMINOR=0.015,NDECS=3
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/qld_higem_dblco2_precip_percentiles_monsoon_combined.change_byonset.'+region_name+'_region.'+pct_name+'.ps'
PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=1000,TFONT=3,$
        TCHARSIZE=100,SPACE3=500
GSET,XMIN=MIN(mean_onset_levs),XMAX=MAX(mean_onset_levs),YMIN=histmin,YMAX=histmax,$
     TITLE='Change in wet-season dates by CTL onset date - land points in '+region_name+' region'
HIST,X=mean_onset_mid-(mean_onset_mid(2)-mean_onset_mid(1))/7.,Y=onset_change_byonset_mean,FILLCOL=FSC_COLOR('blue')
HIST,X=mean_onset_mid+(mean_onset_mid(2)-mean_onset_mid(1))/7.,Y=retreat_change_byonset_mean,FILLCOL=FSC_COLOR('red')
FOR i=0,n_mean_onset_levs-2 DO BEGIN
   EBAR,X=mean_onset_mid(i)-(mean_onset_mid(2)-mean_onset_mid(1))/7.,Y=onset_change_byonset_mean(i),ERROR_Y=onset_change_byonset_stddev(i),COL=FSC_COLOR('black')
   EBAR,X=mean_onset_mid(i)+(mean_onset_mid(2)-mean_onset_mid(1))/7.,Y=retreat_change_byonset_mean(i),ERROR_Y=retreat_change_byonset_stddev(i),COL=FSC_COLOR('black')
ENDFOR
GLEGEND,labels=REVERSE(['Change in onset date','Change in retreat date']),COL=REVERSE(FSC_COLOR(['blue','red'])),TYPE=1,LEGPOS=3
AXES,XSTEP=(mean_onset_mid(2)-mean_onset_mid(1)),YSTEP=5,XMINOR=(mean_onset_mid(2)-mean_onset_mid(1))/2.,YMINOR=1,NDECS=3,YTITLE='Change in wet-season dates (days)',$
     XTITLE='Binned onset dates from control simulation (Julian)'
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/qld_higem_dblco2_precip_percentiles_monsoon_combined.change_byretreat.'+region_name+'_region.'+pct_name+'.ps'
PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=1000,TFONT=3,$
        TCHARSIZE=100,SPACE3=500
GSET,XMIN=MIN(mean_retreat_levs),XMAX=MAX(mean_retreat_levs),YMIN=histmin,YMAX=histmax,TITLE='Change in wet-season dates by CTL retreat date - land points in '+region_name+' region'
HIST,X=mean_retreat_mid-(mean_retreat_mid(2)-mean_retreat_mid(1))/7.,Y=onset_change_byretreat_mean,FILLCOL=FSC_COLOR('blue')
HIST,X=mean_retreat_mid+(mean_retreat_mid(2)-mean_retreat_mid(1))/7.,Y=retreat_change_byretreat_mean,FILLCOL=FSC_COLOR('red')
FOR i=0,n_mean_retreat_levs-2 DO BEGIN
   EBAR,X=mean_retreat_mid(i)-(mean_retreat_mid(2)-mean_retreat_mid(1))/7.,Y=onset_change_byretreat_mean(i),ERROR_Y=onset_change_byretreat_stddev(i),COL=FSC_COLOR('black')
   EBAR,X=mean_retreat_mid(i)+(mean_retreat_mid(2)-mean_retreat_mid(1))/7.,Y=retreat_change_byretreat_mean(i),ERROR_Y=retreat_change_byretreat_stddev(i),COL=FSC_COLOR('black')
ENDFOR
GLEGEND,labels=REVERSE(['Change in onset date','Change in retreat date']),COL=REVERSE(FSC_COLOR(['blue','red'])),TYPE=1,LEGPOS=3
AXES,XSTEP=(mean_retreat_mid(2)-mean_retreat_mid(1)),YSTEP=5,XMINOR=(mean_retreat_mid(2)-mean_retreat_mid(1))/2.,YMINOR=1,NDECS=3,YTITLE='Change in wet-season dates (days)',$
     XTITLE='Binned retreat dates from control simulation (Julian)'
PSCLOSE,/NOVIEW

STOP
END

