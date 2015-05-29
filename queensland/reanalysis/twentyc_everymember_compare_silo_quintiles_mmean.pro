PRO twentyc_everymember_compare_silo_quintiles_mmean

twentyc_infile='/home/ss901165/datasets_mango/20THC_REANALYSIS/every_member/precip/'+$
               '20thc_reanalysis_allmembers.jan-dec_mmeans.1900-2008.precip.revlat.nc'
twentyc_ensmean_infile='/home/ss901165/datasets_mango/20THC_REANALYSIS/precip/'+$
                       '20thc_reanalysis.jan-dec_mmeans.1891-2008.precip.nc'
silo_infile='/home/ss901165/datasets_mango/SILO/t62/SILO.jan-dec_mmeans.1900-2008.precip.t62.nc'

box=[-39,112,-10,154]
box_name='aus'
righty_max=80000
righty_min=300
righty_vals=[300,400,500,700,1000,1500,2000,3000,4000,5000,7000,10000,15000,20000,30000,50000,80000]
;box=[-39,113,-17,130]
;box_name='west'
;box=[-25,138,-10,154]
;box_name='neaus'
;righty_max=30000
;righty_min=50
;righty_vals=[50,100,150,200,250,300,400,500,700,1000,1500,2000,3000,4000,5000,7000,10000,15000,20000,30000]
;box=[-39,138,-25,154]
;box_name='seaus'
;box=[-23,113,-10,138]
;box_name='naus'
;box=[-39,124,-23,138]
;box_name='saus'
title_name='All Australia'
time_period='mmean'
title='Reliability diagram for 20th Century Reanalysis ensemble members - Mthly precip anom for '+title_name+' land points - 1900-2008'

n_years=109
n_months=n_years*12

twentyc_longitude=OPEN_AND_EXTRACT(twentyc_infile,'lon')
twentyc_latitude=OPEN_AND_EXTRACT(twentyc_infile,'lat')
DEFINE_BOUNDARIES,box,twentyc_latitude,twentyc_longitude,twentyc_box_tx,/LIMIT
twentyc_nlon=N_ELEMENTS(twentyc_longitude)
twentyc_nlat=N_ELEMENTS(twentyc_latitude)
twentyc_nmembers=56

twentyc_ensmean_longitude=OPEN_AND_EXTRACT(twentyc_ensmean_infile,'longitude')
twentyc_ensmean_latitude=OPEN_AND_EXTRACT(twentyc_ensmean_infile,'latitude')
DEFINE_BOUNDARIES,box,twentyc_ensmean_latitude,twentyc_ensmean_longitude,twentyc_ensmean_box_tx,/LIMIT
twentyc_ensmean_nlon=N_ELEMENTS(twentyc_ensmean_longitude)
twentyc_ensmean_nlat=N_ELEMENTS(twentyc_ensmean_latitude)

silo_longitude=OPEN_AND_EXTRACT(silo_infile,'longitude')
silo_latitude=OPEN_AND_EXTRACT(silo_infile,'latitude')
DEFINE_BOUNDARIES,box,silo_latitude,silo_longitude,silo_box_tx,/LIMIT
silo_nlon=N_ELEMENTS(silo_longitude)
silo_nlat=N_ELEMENTS(silo_latitude)

; Extract rainfall in box

twentyc_mmean_rainfall=OPEN_AND_EXTRACT(twentyc_infile,'prate',$
                                        offset=[twentyc_box_tx(1),twentyc_box_tx(0),0,0],$
                                        count=[twentyc_nlon,twentyc_nlat,twentyc_nmembers,n_months])*86400.
twentyc_ensmean_mmean_rainfall=OPEN_AND_EXTRACT(twentyc_ensmean_infile,'PRATE',$
                                                offset=[twentyc_ensmean_box_tx(1),twentyc_ensmean_box_tx(0),108],$
                                                count=[twentyc_ensmean_nlon,twentyc_ensmean_nlat,n_months])*86400.
silo_mmean_rainfall=OPEN_AND_EXTRACT(silo_infile,'rain',$
                                     offset=[silo_box_tx(1),silo_box_tx(0),0],$
                                     count=[silo_nlon,silo_nlat,n_months])

; Form distributions
division_weights=[0.20,0.60,0.20]
;division_weights=[0.20,0.20,0.20,0.20,0.20]
n_divisions=N_ELEMENTS(division_weights)
colors=['brown','orange','purple']
;colors=['brown','red','orange','blue','purple']

twentyc_mmean_rainfall_quintiles=fltarr(twentyc_nlon,twentyc_nlat,twentyc_nmembers,n_months)
silo_mmean_rainfall_quintiles=fltarr(twentyc_nlon,twentyc_nlat,n_months)
twentyc_ensmean_mmean_rainfall_quintiles=fltarr(twentyc_nlon,twentyc_nlat,n_months)
prob_bins=[0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90]
ensmem_bins=[0,4,8,12,16,20,24,28,32,36,40,44,48,52]
n_prob_bins=N_ELEMENTS(prob_bins)
n_ensmem_bins=N_ELEMENTS(ensmem_bins)
twentyc_ens_prob=fltarr(n_months)
validation_prob=fltarr(n_divisions+1,n_prob_bins)
temp=fltarr(twentyc_nlon,twentyc_nlat,n_months)
FOR i=0,twentyc_nlon-1 DO BEGIN
   FOR j=0,twentyc_nlat-1 DO BEGIN
      IF silo_mmean_rainfall(i,j,0) lt 2e19 and MEAN(silo_mmean_rainfall(i,j,*)) ge 1. THEN BEGIN
         FOR k=0,twentyc_nmembers-1 DO BEGIN
            thispt_thismem_rainfall=REFORM(twentyc_mmean_rainfall(i,j,k,*))
            FOR m=0,11 DO BEGIN
               temp=REFORM(thispt_thismem_rainfall(m:n_months-1:12))
               thispt_thismem_thismonth_rainfall=temp-MEAN(temp)
               IF TOTAL(where(temp eq 0)) ge 0 THEN BEGIN
                  thispt_thismem_thismonth_rainfall[where(temp lt 0.1)]=-999
                  valid_points=where(thispt_thismem_thismonth_rainfall ne -999)
               ENDIF ELSE $
                  valid_points=indgen(n_years)
               IF TOTAL(valid_points) ge 0 THEN BEGIN
                  n_valid_pts=N_ELEMENTS(valid_points)
                  temp_rain=thispt_thismem_thismonth_rainfall[valid_points]
                  temp_sort=SORT(temp_rain)
                  temp_bins=fltarr(n_divisions)
                  FOR n=0,n_divisions-1 DO BEGIN
                     IF n eq 0 THEN BEGIN
                        temp_bins(n)=temp_rain(temp_sort(0))
                     ENDIF ELSE BEGIN
                        boundary=FIX(n_valid_pts*TOTAL(division_weights(0:n-1)))
                        IF boundary lt N_ELEMENTS(temp_sort)-1 THEN $
                           boundary=boundary+1
                        temp_bins(n)=temp_rain(temp_sort(boundary))
                     ENDELSE
                  ENDFOR
                  indices=VALUE_LOCATE(temp_bins,thispt_thismem_thismonth_rainfall[valid_points])
                  replace=fltarr(n_years)
                  replace[valid_points]=indices
                  IF TOTAL(where(thispt_thismem_thismonth_rainfall eq -999)) gt 0 THEN $
                     replace[where(thispt_thismem_thismonth_rainfall eq -999)]=-1
                  twentyc_mmean_rainfall_quintiles(i,j,k,m:n_months-1:12)=replace
               ENDIF ELSE $
                  twentyc_mmean_rainfall_quintiles(i,j,k,m:n_months-1:12)=!Values.F_NaN
            ENDFOR
         ENDFOR
         thispt_rainfall=REFORM(silo_mmean_rainfall(i,j,*))
         FOR m=0,11 DO BEGIN
            temp=thispt_rainfall(m:n_months-1:12)
            thispt_thismonth_rainfall=temp-MEAN(temp)
            IF TOTAL(where(temp eq 0)) ge 0 THEN BEGIN
               thispt_thismonth_rainfall[where(temp lt 0.1)]=-999
               valid_points=where(thispt_thismonth_rainfall ne -999)
            ENDIF ELSE $
               valid_points=indgen(n_years)           
            n_valid_pts=N_ELEMENTS(valid_points)
            IF TOTAL(valid_points) gt 0 THEN BEGIN
               temp_rain=thispt_thismonth_rainfall[valid_points]
               temp_sort=SORT(temp_rain)
               FOR n=0,n_divisions-1 DO BEGIN
                  IF n eq 0 THEN BEGIN
                     temp_bins(n)=temp_rain(temp_sort(0))
                  ENDIF ELSE $
                     temp_bins(n)=temp_rain(temp_sort(FIX(n_valid_pts*TOTAL(division_weights(0:n-1)))))
               ENDFOR
               indices=VALUE_LOCATE(temp_bins,thispt_thismonth_rainfall[valid_points])
               replace=fltarr(n_years)
               replace[valid_points]=indices
               IF TOTAL(where(thispt_thismonth_rainfall eq -999)) gt 0 THEN $
                  replace[where(thispt_thismonth_rainfall eq -999)]=-1
               silo_mmean_rainfall_quintiles(i,j,m:n_months-1:12)=replace            
            ENDIF ELSE $
               silo_mmean_rainfall_quintiles(i,j,m:n_months-1:12)=!Values.F_NaN
         ENDFOR
         thispt_rainfall=REFORM(twentyc_ensmean_mmean_rainfall(i,j,*))
         FOR m=0,11 DO BEGIN
            temp=thispt_rainfall(m:n_months-1:12)
            thispt_thismonth_rainfall=temp-MEAN(temp)
            IF TOTAL(where(temp eq 0)) ge 0 THEN BEGIN
               thispt_thismonth_rainfall[where(temp lt 0.1)]=-999
               valid_points=where(thispt_thismonth_rainfall ne -999)
            ENDIF ELSE $
               valid_points=indgen(n_years)
            IF TOTAL(valid_points) gt 0 THEN BEGIN
               n_valid_pts=N_ELEMENTS(valid_points)
               temp_rain=thispt_thismonth_rainfall[valid_points]
               temp_sort=SORT(temp_rain)
               FOR n=0,n_divisions-1 DO BEGIN
                  IF n eq 0 THEN BEGIN
                     temp_bins(n)=temp_rain(temp_sort(0))
                  ENDIF ELSE $
                     temp_bins(n)=temp_rain(temp_sort(FIX(n_valid_pts*TOTAL(division_weights(0:n-1)))))
               ENDFOR
               indices=VALUE_LOCATE(temp_bins,thispt_thismonth_rainfall[valid_points])
               replace=fltarr(n_years)
               replace[valid_points]=indices
               IF TOTAL(where(thispt_thismonth_rainfall eq -999)) gt 0 THEN $
                  replace[where(thispt_thismonth_rainfall eq -999)]=-1
               twentyc_ensmean_mmean_rainfall_quintiles(i,j,m:n_months-1:12)=replace            
            ENDIF ELSE $
               twentyc_ensmean_mmean_rainfall_quintiles(i,j,m:n_months-1:12)=!Values.F_NaN
         ENDFOR
      ENDIF ELSE BEGIN
         silo_mmean_rainfall_quintiles(i,j,*)=!Values.F_NaN
         twentyc_mmean_rainfall_quintiles(i,j,*,*)=!Values.F_NaN
         twentyc_ensmean_mmean_rainfall_quintiles(i,j,*,*)=!Values.F_NaN
      ENDELSE   
   ENDFOR
ENDFOR

twentyc_mmean_rainfall_quintiles_allpts=fltarr(twentyc_nlon*twentyc_nlat*n_months,twentyc_nmembers)
twentyc_ensmean_mmean_rainfall_quintiles_allpts=fltarr(twentyc_nlon*twentyc_nlat*n_months)
silo_mmean_rainfall_quintiles_allpts=fltarr(silo_nlon*silo_nlat*n_months)
FOR j=0,twentyc_nmembers-1 DO $
   twentyc_mmean_rainfall_quintiles_allpts(*,j)=REFORM(twentyc_mmean_rainfall_quintiles(*,*,j,*),[twentyc_nlon*twentyc_nlat*n_months])
silo_mmean_rainfall_quintiles_allpts(*)=REFORM(silo_mmean_rainfall_quintiles,[silo_nlon*silo_nlat*n_months])
twentyc_ensmean_mmean_rainfall_quintiles_allpts(*)=REFORM(twentyc_ensmean_mmean_rainfall_quintiles,[twentyc_nlon*twentyc_nlat*n_months])

random_rainfall_quintiles_allpts=fltarr(twentyc_nlon*twentyc_nlat*n_months,twentyc_nmembers)
constrain_rainfall_quintiles_allpts=fltarr(twentyc_nlon*twentyc_nlat*n_months,twentyc_nmembers)
FOR m=0,twentyc_nmembers-1 DO BEGIN
   temp=FIX(n_divisions*RANDOMU(random_rainfall_seed,LONG(twentyc_nlon*twentyc_nlat*n_months)))
   temp[where(FINITE(REFORM(twentyc_mmean_rainfall_quintiles_allpts(*,m))) eq 0)]=!Values.F_NaN
   random_rainfall_quintiles_allpts(*,m)=temp
ENDFOR
FOR i=LONG(0),LONG(silo_nlon*silo_nlat*n_months)-1 DO BEGIN
   IF FINITE(silo_mmean_rainfall_quintiles_allpts(i)) eq 1 THEN BEGIN
      mapping=[silo_mmean_rainfall_quintiles_allpts(i)-2,silo_mmean_rainfall_quintiles_allpts(i)-1,silo_mmean_rainfall_quintiles(i)-1,$
               silo_mmean_rainfall_quintiles(i),silo_mmean_rainfall_quintiles(i),silo_mmean_rainfall_quintiles(i),silo_mmean_rainfall_quintiles(i),$
               silo_mmean_rainfall_quintiles_allpts(i)+1,silo_mmean_rainfall_quintiles(i)+1,silo_mmean_rainfall_quintiles_allpts(i)+2]
      IF TOTAL(where(mapping lt 0)) ge 0 THEN $
         mapping[where(mapping lt 0)]=0
      IF TOTAL(where(mapping gt n_divisions-1)) ge 0 THEN $
         mapping[where(mapping gt n_divisions-1)]=n_divisions-1
      prediction=FIX(10*RANDOMU(constrain_rainfall_seed,twentyc_nmembers))
      FOR m=0,twentyc_nmembers-1 DO $
         constrain_rainfall_quintiles_allpts(i,m)=mapping(REFORM(prediction(m)))     
   ENDIF ELSE $
      constrain_rainfall_quintiles_allpts(i,*)=!Values.F_NaN
ENDFOR

twentyc_ens_prob=fltarr(twentyc_nlon*twentyc_nlat*n_months)
random_ens_prob=fltarr(twentyc_nlon*twentyc_nlat*n_months)
constrain_ens_prob=fltarr(twentyc_nlon*twentyc_nlat*n_months)
twentyc_ens_spread=fltarr(n_divisions,n_ensmem_bins,n_divisions)
twentyc_ensmean_spread=fltarr(n_divisions,n_divisions)
twentyc_freq_lowprob=fltarr(n_divisions,n_divisions) ; Frequency with with <= 4 20CR ensemble members predict division, 
                                                     ; stratified by obs division
twentyc_freq_highprob=fltarr(n_divisions,n_divisions)
random_freq_lowprob=fltarr(n_divisions,n_divisions)
constrain_freq_lowprob=fltarr(n_divisions,n_divisions)
random_freq_highprob=fltarr(n_divisions,n_divisions)
constrain_freq_highprob=fltarr(n_divisions,n_divisions)

twentyc_ensmean_match_spread=fltarr(n_divisions,n_ensmem_bins,n_divisions)
twentyc_ensmean_nomatch_spread=fltarr(n_divisions,n_ensmem_bins,n_divisions)
match_counter=fltarr(n_divisions)
nomatch_counter=fltarr(n_divisions)
FOR k=0,n_divisions-1 DO BEGIN
   twentyc_ensmean_match_count=fltarr(twentyc_nlon*twentyc_nlat*n_months,n_divisions)
   twentyc_ensmean_nomatch_count=fltarr(twentyc_nlon*twentyc_nlat*n_months,n_divisions)
   match_counter(k)=0
   nomatch_counter(k)=0
   FOR m=LONG(0),LONG(twentyc_nlon*twentyc_nlat*n_months)-LONG(1) DO BEGIN
      IF silo_mmean_rainfall_quintiles_allpts(m) eq k THEN BEGIN
         IF twentyc_ensmean_mmean_rainfall_quintiles_allpts(m) eq k THEN BEGIN
            FOR n=0,n_divisions-1 DO BEGIN
               IF TOTAL(where(twentyc_mmean_rainfall_quintiles_allpts(m,*) eq n)) gt 0 THEN BEGIN
                  twentyc_ensmean_match_count(m,n)=N_ELEMENTS(where(twentyc_mmean_rainfall_quintiles_allpts(m,*) eq n))            
               ENDIF ELSE $
                  twentyc_ensmean_match_count(m,n)=0
            ENDFOR
            match_counter(k)=match_counter(k)+1
            twentyc_ensmean_nomatch_count(m,*)=-1.
         ENDIF ELSE BEGIN
            FOR n=0,n_divisions-1 DO BEGIN
               IF TOTAL(where(twentyc_mmean_rainfall_quintiles_allpts(m,*) eq n)) gt 0 THEN BEGIN
                  twentyc_ensmean_nomatch_count(m,n)=N_ELEMENTS(where(twentyc_mmean_rainfall_quintiles_allpts(m,*) eq n))
               ENDIF ELSE $
                  twentyc_ensmean_nomatch_count(m,n)=0
            ENDFOR
            nomatch_counter(k)=nomatch_counter(k)+1
            twentyc_ensmean_match_count(m,*)=-1.
         ENDELSE
      ENDIF ELSE BEGIN
         twentyc_ensmean_match_count(m,*)=-1.
         twentyc_ensmean_nomatch_count(m,*)=-1.
      ENDELSE
   ENDFOR
   FOR n=0,n_divisions-1 DO BEGIN
      twentyc_ensmean_match_count_indices=VALUE_LOCATE(ensmem_bins,REFORM(twentyc_ensmean_match_count(*,n)))
      twentyc_ensmean_nomatch_count_indices=VALUE_LOCATE(ensmem_bins,REFORM(twentyc_ensmean_nomatch_count(*,n)))
      FOR p=0,n_ensmem_bins-1 DO BEGIN
         IF TOTAL(where(twentyc_ensmean_match_count_indices eq p)) ge 0 THEN $
            twentyc_ensmean_match_spread(k,p,n)=N_ELEMENTS(where(twentyc_ensmean_match_count_indices eq p))
         IF TOTAL(where(twentyc_ensmean_nomatch_count_indices eq p)) ge 0 THEN $
            twentyc_ensmean_nomatch_spread(k,p,n)=N_ELEMENTS(where(twentyc_ensmean_nomatch_count_indices eq p))
      ENDFOR
   ENDFOR
   twentyc_ensmean_match_spread(k,*,*)=twentyc_ensmean_match_spread(k,*,*)/FLOAT(match_counter(k))
   twentyc_ensmean_nomatch_spread(k,*,*)=twentyc_ensmean_nomatch_spread(k,*,*)/FLOAT(nomatch_counter(k))
ENDFOR

n_samples=fltarr(n_divisions,n_prob_bins)
FOR k=0,n_divisions-1 DO BEGIN ; SILO quintiles
   FOR m=LONG(0),LONG(twentyc_nlon*twentyc_nlat*n_months)-LONG(1) DO BEGIN
      IF TOTAL(where(twentyc_mmean_rainfall_quintiles_allpts(LONG(m),*) eq k)) ge 0 and $
         TOTAL(where(FINITE(twentyc_mmean_rainfall_quintiles_allpts(LONG(m),*)) eq 1)) gt 0 THEN BEGIN
         twentyc_ens_prob(LONG(m))=N_ELEMENTS(where(twentyc_mmean_rainfall_quintiles_allpts(LONG(m),*) eq k))/FLOAT(twentyc_nmembers)
      ENDIF ELSE $
         twentyc_ens_prob(LONG(m))=-1
      
      IF TOTAL(where(random_rainfall_quintiles_allpts(LONG(m),*) eq k)) ge 0 and $
         TOTAL(where(FINITE(random_rainfall_quintiles_allpts(LONG(m),*)) eq 1)) gt 0 THEN BEGIN
         random_ens_prob(LONG(m))=N_ELEMENTS(where(random_rainfall_quintiles_allpts(LONG(m),*) eq k))/FLOAT(twentyc_nmembers)
      ENDIF ELSE $
         random_ens_prob(LONG(m))=-1

      IF TOTAL(where(constrain_rainfall_quintiles_allpts(LONG(m),*) eq k)) ge 0 and $
         TOTAL(where(FINITE(constrain_rainfall_quintiles_allpts(LONG(m),*)) eq 1)) gt 0 THEN BEGIN
         constrain_ens_prob(LONG(m))=N_ELEMENTS(where(random_rainfall_quintiles_allpts(LONG(m),*) eq k))/FLOAT(twentyc_nmembers)
      ENDIF ELSE $
         constrain_ens_prob(LONG(m))=-1
   ENDFOR
  
   twentyc_prob_indices=VALUE_LOCATE(prob_bins,twentyc_ens_prob)
   random_prob_indices=VALUE_LOCATE(prob_bins,random_ens_prob)
   constrain_prob_indices=VALUE_LOCATE(prob_bins,constrain_ens_prob)

   FOR m=0,n_prob_bins-1 DO BEGIN
      IF TOTAL(where(twentyc_prob_indices eq m)) ge 0 THEN BEGIN
         IF TOTAL(where(silo_mmean_rainfall_quintiles_allpts[where(twentyc_prob_indices eq m)] eq k)) ge 0 THEN BEGIN
            validation_prob(k,m)=N_ELEMENTS(where(silo_mmean_rainfall_quintiles_allpts[where(twentyc_prob_indices eq m)] eq k))$
                                 /FLOAT(N_ELEMENTS(where(twentyc_prob_indices eq m)))
         ENDIF ELSE $
            validation_prob(k,m)=!Values.F_NaN
      ENDIF ELSE $
         validation_prob(k,m)=!Values.F_NaN
      n_samples(k,m)=N_ELEMENTS(where(twentyc_prob_indices eq m))
   ENDFOR

   silo_this_quintile=where(silo_mmean_rainfall_quintiles_allpts eq k)
   twentyc_ens_count=fltarr(N_ELEMENTS(silo_this_quintile))
   random_ens_count=fltarr(N_ELEMENTS(silo_this_quintile))
   constrain_ens_count=fltarr(N_ELEMENTS(silo_this_quintile))
   FOR m=0,n_divisions-1 DO BEGIN ; 20CR quintiles
      FOR n=LONG(0),LONG(N_ELEMENTS(silo_this_quintile)-1) DO BEGIN
         IF TOTAL(where(twentyc_mmean_rainfall_quintiles_allpts(silo_this_quintile(n),*) eq m)) ge 0 THEN BEGIN
            twentyc_ens_count(LONG(n))=N_ELEMENTS(where(twentyc_mmean_rainfall_quintiles_allpts(silo_this_quintile(LONG(n)),*) eq m))
         ENDIF ELSE $
            twentyc_ens_count(LONG(n))=0
         IF TOTAL(where(random_rainfall_quintiles_allpts(silo_this_quintile(LONG(n)),*) eq m)) ge 0 THEN BEGIN
            random_ens_count(LONG(n))=N_ELEMENTS(where(random_rainfall_quintiles_allpts(silo_this_quintile(LONG(n)),*) eq m))            
         ENDIF ELSE $
            random_ens_count(LONG(n))=0
         IF TOTAL(where(constrain_rainfall_quintiles_allpts(silo_this_quintile(LONG(n)),*) eq m)) ge 0 THEN BEGIN
            constrain_ens_count(LONG(n))=N_ELEMENTS(where(constrain_rainfall_quintiles_allpts(silo_this_quintile(LONG(n)),*) eq m))
         ENDIF ELSE $
            constrain_ens_count(LONG(n))=0
         IF twentyc_ensmean_mmean_rainfall_quintiles_allpts(silo_this_quintile(n)) eq m THEN $
            twentyc_ensmean_spread(k,m)=twentyc_ensmean_spread(k,m)+1
      ENDFOR
      twentyc_ensmean_spread(k,m)=twentyc_ensmean_spread(k,m)/FLOAT(N_ELEMENTS(silo_this_quintile))      

      twentyc_ensmem_indices=VALUE_LOCATE(ensmem_bins,twentyc_ens_count)
      twentyc_ensmean_match_indices=VALUE_LOCATE(ensmem_bins,twentyc_ensmean_match_count)
      random_ensmem_indices=VALUE_LOCATE(ensmem_bins*2,random_ens_count)
      constrain_ensmem_indices=VALUE_LOCATE(ensmem_bins,constrain_ens_count)
      FOR n=0,n_ensmem_bins-1 DO $
         twentyc_ens_spread(k,n,m)=N_ELEMENTS(where(twentyc_ensmem_indices eq n))/FLOAT(N_ELEMENTS(silo_this_quintile))

      twentyc_freq_lowprob(k,m)=N_ELEMENTS(where(twentyc_ensmem_indices eq 0))/FLOAT(N_ELEMENTS(silo_this_quintile))
      random_freq_lowprob(k,m)=N_ELEMENTS(where(random_ensmem_indices eq 0))/FLOAT(N_ELEMENTS(silo_this_quintile))
      constrain_freq_lowprob(k,m)=N_ELEMENTS(where(constrain_ensmem_indices eq 0))/FLOAT(N_ELEMENTS(silo_this_quintile))
      twentyc_freq_highprob(k,m)=N_ELEMENTS(where(twentyc_ensmem_indices ge NEAREST(ensmem_bins,28)))/FLOAT(N_ELEMENTS(silo_this_quintile))
      random_freq_highprob(k,m)=N_ELEMENTS(where(random_ensmem_indices ge NEAREST(ensmem_bins,28)))/FLOAT(N_ELEMENTS(silo_this_quintile))
      constrain_freq_highprob(k,m)=N_ELEMENTS(where(constrain_ensmem_indices ge NEAREST(ensmem_bins,28)))/FLOAT(N_ELEMENTS(silo_this_quintile))
   ENDFOR
ENDFOR

psfile='/home/ss901165/idl/queensland/reanalysis/twentyc_everymember_compare_silo_quintiles_mmean.20_60_20.'+box_name+'box_'+time_period+'.ps'
;psfile='/home/ss901165/idl/queensland/reanalysis/twentyc_everymember_compare_silo_quintiles_mmean.'+box_name+'box_'+time_period+'.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2500,SPACE3=500,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100,$
       SPACE2=700
GSET,XMIN=0,XMAX=1,YMIN=0,YMAX=1,TITLE=title
FOR k=0,n_divisions-1 DO $
   GPLOT,X=prob_bins+0.05,Y=REFORM(validation_prob(k,*)),COL=FSC_COLOR(colors(k)),SYM=3
GPLOT,X=[0,1],Y=[0,1],COL=FSC_COLOR('black'),STYLE=1
AXES,XSTEP=0.1,YSTEP=0.1,XTITLE='20th Century Reanalysis ensemble probability of event',$
     YTITLE='SILO frequency of event given 20CR probability',NDECS=2,/NORIGHT
GSET,XMIN=0,XMAX=1,YMIN=righty_min,YMAX=righty_max,/YLOG
FOR k=0,n_divisions-1 DO $
   GPLOT,X=prob_bins+0.05,Y=REFORM(n_samples(k,*)),COL=FSC_COLOR(colors(k)),STYLE=2,SYM=5,THICK=80,SIZE=50
AXES,yvals=righty_vals,/ONLYRIGHT,YTITLE='Number of samples in 20CR'

labels=strarr(n_divisions)
FOR i=0,n_divisions-1 DO $
   labels(i)='Division '+STRTRIM(STRING(i+1))+' (freq = '+STRTRIM(STRING(division_weights(i)),1)+')'
GLEGEND,labels=REVERSE(labels),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=5
PSCLOSE,/NOVIEW

FOR i=0,n_divisions-1 DO BEGIN
   psfile='/home/ss901165/idl/queensland/reanalysis/twentyc_everymember_compare_silo_quintiles_mmean.20_60_20.'+box_name+'box_distribution_division'+STRTRIM(STRING(i+1),1)+'_'+time_period+'.ps'
;   psfile='/home/ss901165/idl/queensland/reanalysis/twentyc_everymember_compare_silo_quintiles_mmean.'+box_name+'box_distribution_division'+$
;          STRTRIM(STRING(i+1),1)+'_'+time_period+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2500,SPACE3=500,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100,SPACE2=700
   dist_title='Distribution of 20CR ensemble members during SILO division '+STRTRIM(STRING(i+1),1)+' - Mthly precip anom for '+title_name+$
              ' land points - 1900-2008'
   GSET,XMIN=0,XMAX=twentyc_nmembers+10,YMIN=0,YMAX=0.88,TITLE=dist_title
   FOR k=0,n_divisions-1 DO BEGIN
      HIST,X=ensmem_bins+k*2./5.+1.2,Y=REFORM(twentyc_ens_spread(i,*,k)),FILLCOL=FSC_COLOR(colors(k)),WIDTH=40
      HIST,X=ensmem_bins(n_ensmem_bins-1)+8+k*2./5.+1.2,Y=twentyc_ensmean_spread(i,k),FILLCOL=FSC_COLOR(colors(k)),WIDTH=40
   ENDFOR
   AXES,XVALS=[indgen(n_ensmem_bins+1)*4,62],XLABELS=[STRTRIM(STRING(ensmem_bins),1),'56',$
                                                      'Ensemble mean'],$
        YSTEP=0.08,XTITLE='Number of 20CR members predicting division (based on 20CR divisions)',$
        YTITLE='Fractional frequency of occurrence of 20CR members predicting division',$
        NDECS=2,YMINOR=0.04,XMINOR=indgen(28)*2
   GPLOT,X=[59,59],Y=[0,0.88],STYLE=0
   GLEGEND,labels=REVERSE(labels),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=5
   PSCLOSE,/NOVIEW

;   psfile='/home/ss901165/idl/queensland/reanalysis/twentyc_everymember_compare_silo_quintiles_mmean.'+box_name+'box_distribution_division'+$
;          STRTRIM(STRING(i+1),1)+'.ensmean_match.'+time_period+'.ps'
   psfile='/home/ss901165/idl/queensland/reanalysis/twentyc_everymember_compare_silo_quintiles_mmean.20_60_20.'+box_name+'box_distribution_division'+$
          STRTRIM(STRING(i+1),1)+'.ensmean_match.'+time_period+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2500,SPACE3=500,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=90,SPACE2=700
   dist_title='Dist of 20CR ens members when 20CR ensmean matches SILO (division '+STRTRIM(STRING(i+1),1)+') - Mthly precip anom for '+title_name+$
              ' land points - 1900-2008'
   GSET,XMIN=0,XMAX=twentyc_nmembers,YMIN=0,YMAX=1.0,TITLE=dist_title
   FOR k=0,n_divisions-1 DO $
      HIST,X=ensmem_bins+k*2./5.+1.2,Y=REFORM(twentyc_ensmean_match_spread(i,*,k)),FILLCOL=FSC_COLOR(colors(k)),WIDTH=40
   AXES,XSTEP=4,YSTEP=0.10,XTITLE='Number of 20CR members predicting division (based on 20CR divisions)',$
        YTITLE='Fractional frequency of occurrence of 20CR members predicting division',$
        NDECS=2,YMINOR=0.05,XMINOR=2
   GPLOT,X=40,Y=0.6,TEXT='Frequency of matching = '+STRTRIM(STRING(match_counter(i)/(FLOAT(match_counter(i)+nomatch_counter(i)))),1)
   GLEGEND,labels=REVERSE(labels),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=5
   PSCLOSE,/NOVIEW

;   psfile='/home/ss901165/idl/queensland/reanalysis/twentyc_everymember_compare_silo_quintiles_mmean.'+box_name+'box_distribution_division'+$
;          STRTRIM(STRING(i+1),1)+'.ensmean_nomatch.'+time_period+'.ps'
   psfile='/home/ss901165/idl/queensland/reanalysis/twentyc_everymember_compare_silo_quintiles_mmean.20_60_20.'+box_name+'box_distribution_division'+$
          STRTRIM(STRING(i+1),1)+'.ensmean_nomatch.'+time_period+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2500,SPACE3=500,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=90,SPACE2=700
   dist_title='Dist of 20CR ens members when 20CR ensmean does not match SILO (division '+STRTRIM(STRING(i+1),1)+') - Mthly precip anom for '+title_name+$
              ' land points - 1900-2008'
   GSET,XMIN=0,XMAX=twentyc_nmembers,YMIN=0,YMAX=1.0,TITLE=dist_title
   FOR k=0,n_divisions-1 DO $
      HIST,X=ensmem_bins+k*2./5.+1.2,Y=REFORM(twentyc_ensmean_nomatch_spread(i,*,k)),FILLCOL=FSC_COLOR(colors(k)),WIDTH=40
   AXES,XSTEP=4,YSTEP=0.10,XTITLE='Number of 20CR members predicting division (based on 20CR divisions)',$
        YTITLE='Fractional frequency of occurrence of 20CR members predicting division',$
        NDECS=2,YMINOR=0.05,XMINOR=2
   GPLOT,X=40,Y=0.6,TEXT='Frequency of not matching = '+STRTRIM(STRING(nomatch_counter(i)/(FLOAT(match_counter(i)+nomatch_counter(i)))),1)
   GLEGEND,labels=REVERSE(labels),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=5
   PSCLOSE,/NOVIEW

ENDFOR

model_labels=['20th Century Reanalysis','Random (p=obs(p))','1-2-4-2-1 weighted and collapsed at edges']

psfile='/home/ss901165/idl/queensland/reanalysis/twentyc_everymember_compare_silo_quintiles_mmean.'+box_name+'box_lowprob_bydivision_'+time_period+'.ps'
;psfile='/home/ss901165/idl/queensland/reanalysis/twentyc_everymember_compare_silo_quintiles_mmean.20_60_20.'+box_name+'box_lowprob_bydivision_'+time_period+'.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2500,SPACE3=500,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,SPACE2=700
lowprob_title='Frequency of <= 4 ensemble members predicting division - Monthly precip anom for '+title_name+' land points - 1900-2008'
GSET,XMIN=0,XMAX=n_divisions,YMIN=0,YMAX=1,TITLE=lowprob_title
FOR i=0,n_divisions-1 DO BEGIN
   GPLOT,X=indgen(n_divisions)+0.5,Y=REFORM(twentyc_freq_lowprob(*,i)),COL=FSC_COLOR(colors(i))
   GPLOT,X=indgen(n_divisions)+0.5,Y=REFORM(random_freq_lowprob(*,i)),COL=FSC_COLOR(colors(i)),STYLE=1
   GPLOT,X=indgen(n_divisions)+0.5,Y=REFORM(constrain_freq_lowprob(*,i)),COL=FSC_COLOR(colors(i)),STYLE=2
ENDFOR
AXES,XVALS=indgen(n_divisions)+0.5,XLABELS=STRTRIM(STRING(indgen(n_divisions)+1),1),YSTEP=0.1,YMINOR=0.05,XTITLE='Observed division',NDECS=2,$
     YTITLE='Frequency of <= 4 ensemble members predicting division (conditioned on observed division)'
GLEGEND,labels=REVERSE(labels),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=4
GLEGEND,labels=REVERSE(model_labels),STYLE=REVERSE([0,1,2]),LEGPOS=12
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/reanalysis/twentyc_everymember_compare_silo_quintiles_mmean.'+box_name+'box_highprob_bydivision_'+time_period+'.ps'
;psfile='/home/ss901165/idl/queensland/reanalysis/twentyc_everymember_compare_silo_quintiles_mmean.20_60_20.'+box_name+'box_lowprob_bydivision_'+time_period+'.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2500,SPACE3=500,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,SPACE2=700
highprob_title='Frequency of > 28 ensemble members predicting division - Monthly precip anom for '+title_name+' land points - 1900-2008'
GSET,XMIN=0,XMAX=n_divisions,YMIN=0,YMAX=1,TITLE=highprob_title
IF TOTAL(where(twentyc_freq_highprob le 0.001)) ge 0 THEN $
   twentyc_freq_highprob[where(twentyc_freq_highprob le 0.001)]=!Values.F_NaN
IF TOTAL(where(random_freq_highprob le 0.001)) ge 0 THEN $
   random_freq_highprob[where(random_freq_highprob le 0.001)]=!Values.F_NaN
IF TOTAL(where(constrain_freq_highprob le 0.001)) ge 0 THEN $
   constrain_freq_highprob[where(constrain_freq_highprob le 0.001)]=!Values.F_NaN
FOR i=0,n_divisions-1 DO BEGIN
   GPLOT,X=indgen(n_divisions)+0.5,Y=REFORM(twentyc_freq_highprob(*,i)),COL=FSC_COLOR(colors(i))
   GPLOT,X=indgen(n_divisions)+0.5,Y=REFORM(random_freq_highprob(*,i)),COL=FSC_COLOR(colors(i)),STYLE=1
   GPLOT,X=indgen(n_divisions)+0.5,Y=REFORM(constrain_freq_highprob(*,i)),COL=FSC_COLOR(colors(i)),STYLE=2,SYM=3,/NOLINES
ENDFOR
AXES,XVALS=indgen(n_divisions)+0.5,XLABELS=STRTRIM(STRING(indgen(n_divisions)+1),1),YSTEP=0.1,YMINOR=0.05,XTITLE='Observed division',NDECS=3,$
     YTITLE='Frequency of >= 28 ensemble members predicting division (conditioned on observed division)'
GLEGEND,labels=REVERSE(labels),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=4
GLEGEND,labels=REVERSE(model_labels),STYLE=REVERSE([0,1,2]),SYM=REVERSE([0,0,3]),LEGPOS=12
PSCLOSE

STOP
END

