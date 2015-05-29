PRO qld_iacorr_bypoint_silo25_regions

; Correlate the annual-mean rainfall in each gridpoint with all others

ameans_infile='/home/ss901165/datasets_mango/SILO/two_and_a_half/SILO.may-apr_ameans.1900-2008.precip.2.5x2.5.nc'

box=[-30,138,-10,154]
silo_latitude=OPEN_AND_EXTRACT(ameans_infile,'latitude')
silo_longitude=OPEN_AND_EXTRACT(ameans_infile,'longitude')
DEFINE_BOUNDARIES,box,silo_latitude,silo_longitude,silo_box_tx,/LIMIT
silo_nlon=N_ELEMENTS(silo_longitude)
silo_nlat=N_ELEMENTS(silo_latitude)
silo_nyears=108

regions=[[0,0,0,0,0,0,0],$
         [0,0,0,0,0,0,0],$
         [1,1,1,0,0,0,0],$
         [1,1,1,0,0,0,0],$
         [2,2,2,3,0,0,0],$
         [2,2,2,3,3,0,0],$
         [2,4,4,3,3,3,0],$
         [4,4,4,4,5,5,0],$
         [4,4,4,4,4,5,5]]
n_regions=5
region_aavg_ameans=fltarr(n_regions,silo_nyears)
region_npoints=fltarr(n_regions)
FOR i=0,n_regions-1 DO $
   region_npoints(i)=N_ELEMENTS(where(regions eq i+1))

; Read annual means on grid
precip_ameans=REFORM(OPEN_AND_EXTRACT(ameans_infile,'rain',$
                                      offset=[silo_box_tx(1),silo_box_tx(0),0],$
                                      count=[silo_nlon,silo_nlat,silo_nyears]))

FOR i=0,silo_nlon-1 DO BEGIN
   FOR j=0,silo_nlat-1 DO BEGIN
      IF precip_ameans(i,j,0) lt 10000 THEN BEGIN
         this_pt=REFORM(precip_ameans(i,j,*))
         region_aavg_ameans(regions(i,j)-1,*)=region_aavg_ameans(regions(i,j)-1,*)+precip_ameans(i,j,*)*1./FLOAT(region_npoints(regions(i,j)-1))               
      ENDIF
   ENDFOR
ENDFOR

openw,lun,'/home/ss901165/idl/queensland/interannual_correlations/regions_correlation_may-apr.txt',/get_lun
correlation_matrix=fltarr(silo_nlon,silo_nlat,n_regions)

max_iterations=100

FOR m=0,max_iterations-1 DO BEGIN
   FOR i=0,silo_nlon-1 DO BEGIN
      FOR j=0,silo_nlat-1 DO BEGIN
         IF precip_ameans(i,j,0) lt 10000 THEN BEGIN
            thispt_correlations=fltarr(n_regions)
            FOR k=0,n_regions-1 DO BEGIN
               temp_region_aavg=fltarr(silo_nyears)
               IF k+1 eq regions(i,j) THEN BEGIN
                  temp_region_aavg(*)=(region_aavg_ameans(k,*)-precip_ameans(i,j,*)/FLOAT(region_npoints(k)))*(FLOAT(region_npoints(k))/FLOAT(region_npoints(k)-1))
               ENDIF ELSE $
                  temp_region_aavg(*)=region_aavg_ameans(k,*)
               thispt_correlations(k)=CORRELATE(precip_ameans(i,j,*),temp_region_aavg)
               correlation_matrix(i,j,k)=CORRELATE(precip_ameans(i,j,*),temp_region_aavg)
            ENDFOR         
            IF where(thispt_correlations eq MAX([thispt_correlations])) ne regions(i,j)-1 THEN BEGIN
               new_region=where(thispt_correlations eq MAX([thispt_correlations]))+1               
               print,'iteration '+STRTRIM(STRING(m),1)+' switching pt '+STRTRIM(STRING(i),1)+', '+STRTRIM(STRING(j),1)+' to region '+STRTRIM(STRING(new_region),1)+' (corr='+STRTRIM(STRING(thispt_correlations(new_region-1)),1)+') from region '+STRTRIM(STRING(regions(i,j)),1)+' (corr='+STRTRIM(STRING(thispt_correlations(regions(i,j)-1)),1)+')'
               regions(i,j)=where(thispt_correlations eq MAX([thispt_correlations]))+1
            ENDIF
         ENDIF
      ENDFOR                             
   ENDFOR
ENDFOR

FOR i=0,silo_nlon-1 DO BEGIN
   FOR j=0,silo_nlat-1 DO BEGIN
      IF precip_ameans(i,j,0) lt 10000 THEN $
         printf,lun,'Point '+STRTRIM(STRING(-silo_latitude(j)),1)+'S, '+STRTRIM(STRING(silo_longitude(i)),1)+'E in region '+STRTRIM(STRING(regions(i,j)),1),REFORM(correlation_matrix(i,j,*)),format='(A40,5x,5(F7.5,2x))'
   ENDFOR
ENDFOR

free_lun,lun

STOP
END

