PRO hadgem3kpp_cascade_phase2_verticalxsec_rhthreedays
  
; Plot x-z cross sections of change in moisture over first three days of forecast,
; between 1.5x and 1.0x entrainment runs.
; Plot for each forecast date separately.

n_sets=2
n_variables=1
um3='/home/ss901165/um_output3'
date_ranges=['04nov09-06nov09','26aug08-28aug08','07dec07-09dec07','07sep06-09sep06','18mar05-20mar05','03dec03-05dec03',$
             '29oct02-01nov02','05jun02-07jun02','26apr02-28apr02','16jan01-18jan01','13nov00-15nov00','28sep00-30sep00']
box=[-5,40,5,240]
n_z=85

first_period=[8,9]
second_period=[16,17]
period_name='day3-minus-day2_first6hr'

;levels_diff=['-5.1','-4.5','-3.9','-3.3','-2.7','-2.1','-1.5','-0.9','-0.3','0.3','0.9','1.5','2.1','2.7','3.3','3.9','4.5','5.1']
;levels_entrain_diff=['-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5']
;levels_mean_diff=['-0.75','-0.65','-0.55','-0.45','-0.35','-0.25','-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75']

levels_diff=['-60','-52','-44','-36','-28','-20','-12','-4','4','12','20','28','36','44','52','60']
levels_entrain_diff=['-30','-26','-22','-18','-14','-10','-6','-2','2','6','10','14','18','22','26','30']
levels_mean_diff=['-15','-13','-11','-9','-7','-5','-3','-1','1','3','5','7','9','11','13','15']

;levels_mean_diff=['-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5']

set_names=strarr(n_sets)
FOR i=0,n_sets-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         runids=['xfrla','xfrle','xfrli','xfrlm','xfrlq','xfrlu','xfsea','xfsee','xfsei','xfsem','xfseq','xfseu']
         set_name='control'
      END
      1 : BEGIN
         runids=['xfrlb','xfrlf','xfrlj','xfrln','xfrlr','xfrlv','xfseb','xfsef','xfsej','xfsen','xfser','xfsev']
         set_name='15xentrain'
      END
   ENDCASE
   
   n_cases=N_ELEMENTS(runids)
   thlvl_pres_file=um3+'/'+runids(0)+'.new_stash/'+runids(0)+'.'+date_ranges(0)+'_3hrmeans.p_thlvl_after_ts.nc'
   thlvl_longitude=OPEN_AND_EXTRACT(thlvl_pres_file,'longitude')
   thlvl_latitude=OPEN_AND_EXTRACT(thlvl_pres_file,'latitude')
   DEFINE_BOUNDARIES,box,thlvl_latitude,thlvl_longitude,thlvl_box_tx,/LIMIT
   thlvl_nlon=N_ELEMENTS(thlvl_longitude)
   thlvl_nlat=N_ELEMENTS(thlvl_latitude)
   thlvl_pres=REFORM(OPEN_AND_EXTRACT(thlvl_pres_file,'p_1',$
                                      offset=[thlvl_box_tx(1),thlvl_box_tx(0),0,0],$
                                      count=[thlvl_nlon,thlvl_nlat,n_z,second_period(1)]))
   thlvl_pres_mean=fltarr(n_z)
   FOR k=0,n_z-1 DO $
      thlvl_pres_mean(k)=MEAN(thlvl_pres(*,*,k,*))/100.

   FOR k=0,n_cases-1 DO BEGIN
      infile=um3+'/'+runids(k)+'.new_stash/'+runids(k)+'.'+date_ranges(k)+'_3hrmeans.rh_after_maincld.nc'
      precip_infile=um3+'/'+runids(k)+'.new_stash/'+runids(k)+'.'+date_ranges(k)+'_3hrmeans.rain.nc'
      print,infile
      IF k eq 0 THEN BEGIN
         longitude=OPEN_AND_EXTRACT(infile,'longitude_1')
         latitude=OPEN_AND_EXTRACT(infile,'latitude_2')
         DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
         n_lon=N_ELEMENTS(longitude)
         n_lat=N_ELEMENTS(latitude)
         IF i eq 0 THEN BEGIN
            all_var_diff_lonavg=fltarr(n_sets,n_cases,n_lon,n_z)
            all_precip_lonavg=fltarr(n_sets,n_cases,n_lon)
         ENDIF
      ENDIF

      precip_longitude=OPEN_AND_EXTRACT(precip_infile,'longitude_1')
      precip_latitude=OPEN_AND_EXTRACT(precip_infile,'latitude')
      DEFINE_BOUNDARIES,box,precip_latitude,precip_longitude,precip_box_tx,/LIMIT
      precip_nlon=N_ELEMENTS(precip_longitude)
      precip_nlat=N_ELEMENTS(precip_latitude)

      first_var=REFORM(OPEN_AND_EXTRACT(infile,'rh',offset=[box_tx(1),box_tx(0),0,first_period(0)],$
                                        count=[n_lon,n_lat,n_z,first_period(1)-first_period(0)+1]))
      second_var=REFORM(OPEN_AND_EXTRACT(infile,'rh',offset=[box_tx(1),box_tx(0),0,second_period(0)],$
                                         count=[n_lon,n_lat,n_z,second_period(1)-second_period(0)+1]))

;      first_precip=REFORM(OPEN_AND_EXTRACT(precip_infile,'rain',offset=[precip_box_tx(1),precip_box_tx(0),0,first_period(0)],$
;                                           count=[precip_nlon,precip_nlat,1,first_period(1)-first_period(0)+1]))*86400.
      second_precip=REFORM(OPEN_AND_EXTRACT(precip_infile,'rain',offset=[precip_box_tx(1),precip_box_tx(0),0,first_period(1)],$
                                            count=[precip_nlon,precip_nlat,1,second_period(0)-first_period(1)+1]))*86400.

      var_diff_lonavg=fltarr(n_lon,n_z)
      precip_lonavg=fltarr(n_lon)
      FOR n=0,n_lon-1 DO BEGIN
         FOR p=0,n_z-1 DO $
            var_diff_lonavg(n,p)=MEAN(second_var(n,*,p,*))-MEAN(first_var(n,*,p,*))
         precip_lonavg(n)=MEAN(second_precip(n,*,*))
      ENDFOR

      all_var_diff_lonavg(i,k,*,*)=var_diff_lonavg
      all_precip_lonavg(i,k,*)=precip_lonavg

      psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/vertical_xsec/hadgem3kpp_cascade_phase2_verticalxsec_rhthreedays.'+date_ranges(k)+'_'+set_name+'.diff_'+period_name+'.ps'
      PSOPEN,file=psfile,TFONT=2,CHARSIZE=100,MARGIN=2200,SPACE3=500,XOFFSET=1000,YOFFSET=200,FONT=2,TCHARSIZE=100,SPACE2=700
      CS,SCALE=1,NCOLS=N_ELEMENTS(levels_diff)+1,/REV
      LEVS,MANUAL=levels_diff
      GSET,XMIN=box(1),XMAX=box(3),YMIN=1015.,YMAX=100.
      CON,X=longitude,Y=thlvl_pres_mean,FIELD=var_diff_lonavg,/NOLINELABELS,POSITIVE_STYLE=0,NEGATIVE_STYLE=2,$
          CB_TITLE='Difference in relative humidity (%)',$
          TITLE='Difference in relative humidity for hours '+STRTRIM(STRING(second_period(0)*3),1)+'-'+STRTRIM(STRING((second_period(1)+1)*3),1)+$
          ' minus hours '+STRTRIM(STRING(first_period(0)*3),1)+'-'+STRTRIM(STRING((first_period(1)+1)*3),1)+' - '+date_ranges(k)+' - '+set_name
      AXES,XSTEP=10,YVALS=indgen(10)*(-100)+1000.,YMINOR=indgen(17)*(-50)+950.,$
           XMINOR=5,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)',NDECS=1,/NORIGHT
      GSET,XMIN=box(1),XMAX=box(3),YMIN=0,YMAX=50
      GPLOT,X=longitude,Y=precip_lonavg,STYLE=0,COL=FSC_COLOR('purple'),THICK=200
      AXES,XSTEP=10,YSTEP=5,YMINOR=2.5,NDECS=1,YTITLE='Precipitation (mm day!U-1!N)',/ONLYRIGHT
      PSCLOSE,/NOVIEW

   ENDFOR
   set_names(i)=set_name
ENDFOR

FOR k=0,n_cases-1 DO BEGIN
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/vertical_xsec/hadgem3kpp_cascade_phase2_verticalxsec_rhthreedays.'+date_ranges(k)+'_diff_1.5x-1.0x.diff_'+period_name+'.ps'
   PSOPEN,file=psfile,TFONT=2,CHARSIZE=100,MARGIN=2200,SPACE3=500,XOFFSET=1000,YOFFSET=200,FONT=2,TCHARSIZE=100,SPACE2=700
   CS,SCALE=1,NCOLS=N_ELEMENTS(levels_entrain_diff)+1,/REV
   LEVS,MANUAL=levels_entrain_diff
   GSET,XMIN=box(1),XMAX=box(3),YMIN=1015.,YMAX=100.
   CON,X=longitude,Y=thlvl_pres_mean,FIELD=REFORM(all_var_diff_lonavg(1,k,*,*))-REFORM(all_var_diff_lonavg(0,k,*,*)),$
       /NOLINELABELS,POSITIVE_STYLE=0,NEGATIVE_STYLE=2,$
       CB_TITLE='Difference in relative humidity (%)',$
       TITLE='Difference of differences in relative humidity for hours '+STRTRIM(STRING(second_period(0)*3),1)+'-'+STRTRIM(STRING((second_period(1)+1)*3),1)+$
       ' minus hours '+STRTRIM(STRING(first_period(0)*3),1)+'-'+STRTRIM(STRING((first_period(1)+1)*3),1)+' - '+date_ranges(k)+' - 1.5x entrain minus control'
   AXES,XSTEP=10,YVALS=indgen(10)*(-100)+1000.,YMINOR=indgen(17)*(-50)+950.,$
        XMINOR=5,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)',NDECS=1,/NORIGHT
   GSET,XMIN=box(1),XMAX=box(3),YMIN=-20,YMAX=20
   GPLOT,X=longitude,Y=REFORM(all_precip_lonavg(1,k,*))-REFORM(all_precip_lonavg(0,k,*)),$
         COL=FSC_COLOR('purple'),THICK=200
   GPLOT,X=[box(1),box(3)],Y=[0,0],STYLE=1,THICK=200,COL=FSC_COLOR('purple')
   AXES,XSTEP=10,YSTEP=4,YMINOR=2,/ONLYRIGHT,YTITLE='Difference in precipitation (mm day!U-1!N)'
   PSCLOSE,/NOVIEW
ENDFOR

set_mean=fltarr(n_sets,n_lon,n_z)
precip_set_mean=fltarr(n_sets,n_lon)
FOR k=0,n_sets-1 DO BEGIN
   FOR m=0,n_lon-1 DO BEGIN
      FOR n=0,n_z-1 DO $
         set_mean(k,m,n)=MEAN(all_var_diff_lonavg(k,*,m,n))
      precip_set_mean(k,m)=MEAN(all_precip_lonavg(k,*,m))
   ENDFOR

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/vertical_xsec/hadgem3kpp_cascade_phase2_verticalxsec_rhthreedays.'+set_names(k)+'_mean.diff_'+period_name+'.ps'
   PSOPEN,file=psfile,TFONT=2,CHARSIZE=100,MARGIN=2200,SPACE3=500,XOFFSET=1000,YOFFSET=200,FONT=2,TCHARSIZE=100,SPACE2=700
   CS,SCALE=1,NCOLS=N_ELEMENTS(levels_entrain_diff)+1,/REV
   LEVS,MANUAL=levels_entrain_diff
   GSET,XMIN=box(1),XMAX=box(3),YMIN=1015.,YMAX=100.
   CON,X=longitude,Y=thlvl_pres_mean,FIELD=REFORM(set_mean(k,*,*)),$
       /NOLINELABELS,POSITIVE_STYLE=0,NEGATIVE_STYLE=2,$
       CB_TITLE='Difference in relative humidity (%)',$
       TITLE='Mean of differences in relative humidity for hours '+STRTRIM(STRING(second_period(0)*3),1)+'-'+STRTRIM(STRING((second_period(1)+1)*3),1)+$
          ' minus hours '+STRTRIM(STRING(first_period(0)*3),1)+'-'+STRTRIM(STRING((first_period(1)+1)*3),1)+' - '+set_names(k)
   AXES,XSTEP=10,YVALS=indgen(10)*(-100)+1000.,YMINOR=indgen(17)*(-50)+950.,$
        XMINOR=5,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)',NDECS=1,/NORIGHT
   GSET,XMIN=box(1),XMAX=box(3),YMIN=0,YMAX=20
   GPLOT,X=longitude,Y=REFORM(precip_set_mean(k,*)),STYLE=0,COL=FSC_COLOR('purple'),THICK=150
   GPLOT,X=[box(1),box(3)],Y=[0,0],STYLE=1,THICK=200,COL=FSC_COLOR('purple')
   AXES,XSTEP=10,YSTEP=2,YMINOR=1,YTITLE='Mean precipitation (mm day!U-1!N)',/ONLYRIGHT
   PSCLOSE,/NOVIEW
ENDFOR

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/vertical_xsec/hadgem3kpp_cascade_phase2_verticalxsec_rhthreedays.diff_1.5xmean-minus-1.0xmean.diff_'+period_name+'.ps'
PSOPEN,file=psfile,TFONT=2,CHARSIZE=100,MARGIN=2200,SPACE3=500,XOFFSET=1000,YOFFSET=200,FONT=2,TCHARSIZE=100,SPACE2=700
CS,SCALE=1,NCOLS=N_ELEMENTS(levels_mean_diff)+1,/REV
LEVS,MANUAL=levels_mean_diff
GSET,XMIN=box(1),XMAX=box(3),YMIN=1015.,YMAX=100.
CON,X=longitude,Y=thlvl_pres_mean,FIELD=REFORM(set_mean(1,*,*))-REFORM(set_mean(0,*,*)),$
    /NOLINELABELS,POSITIVE_STYLE=0,NEGATIVE_STYLE=2,$
    CB_TITLE='Difference in relative humidity (%)',$
    TITLE='Difference of mean differences in relative humidity for hours '+STRTRIM(STRING(second_period(0)*3),1)+'-'+STRTRIM(STRING((second_period(1)+1)*3),1)+$
          ' minus hours '+STRTRIM(STRING(first_period(0)*3),1)+'-'+STRTRIM(STRING((first_period(1)+1)*3),1)+' - 1.5x entrain minus control'
AXES,XSTEP=10,YVALS=indgen(10)*(-100)+1000.,YMINOR=indgen(17)*(-50)+950.,$
     XMINOR=5,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)',NDECS=1,/NORIGHT
GSET,XMIN=box(1),XMAX=box(3),YMIN=-3,YMAX=3
GPLOT,X=longitude,Y=REFORM(precip_set_mean(1,*)-precip_set_mean(0,*)),STYLE=0,COL=FSC_COLOR('purple'),THICK=150
GPLOT,X=[box(1),box(3)],Y=[0,0],STYLE=1,THICK=200,COL=FSC_COLOR('purple')
AXES,XSTEP=10,YSTEP=0.6,YMINOR=0.3,YTITLE='Mean difference in precipitation (mm day!U-1!N)',/ONLYRIGHT,NDECS=1
PSCLOSE

STOP
END
