pro calc_PPE_atmos_anom_CORR
 
;------------------------------------------------------------
;this program will read in the PPE predictions, and calculate the
;annual means for the first 9 years.
;------------------------------------------------------------
  
                                ; Root directory for the DePreSys assimilated (deproot) and un-assimilated (noaroot) predictions.
  deproot = '/home/swr06jir/links/elder/PPE/depresys/atmos/precip/'
  noaroot = '/home/swr06jir/links/elder/PPE/noassim/atmos/precip/'
  
                                ; Define the array to consider for correlation analysis: [start_lat,start_lon,stop_lat,stop_lon]
  box=[-40,112,-10,155]                                
  
                                ; Number of initial dates for DePreSys (number of predictions - 1960 through 2005).
  nstart = 46
                                ; Number of DePreSys ensemble members
  nmem = 9
                                ; Not used.
  ndep = 1
                                ; Number of forecast lead times (year+1, year+2, year+3, etc.) to consider
  ntim_yr = 9 
;ntim_3yr = ntim_yr - nyear
                                ; Prefixes for filenames of DePreSys assimilated predictions
  depprefix=STRARR(9)
  depprefix(0)='z02g0'
  depprefix(1)='z02g1'
  depprefix(2)='z02g2'
  depprefix(3)='z02g3'
  depprefix(4)='z02g4'
  depprefix(5)='z02g5'
  depprefix(6)='z02g6'
  depprefix(7)='z02g7'
  depprefix(8)='z02g8'
                                ; Prefixes for filenames of DePreSys un-assimilated predictions
  noaprefix=STRARR(9)
  noaprefix(0)='z01u0'
  noaprefix(1)='z01u1'
  noaprefix(2)='z01u2'
  noaprefix(3)='z01u3'
  noaprefix(4)='z01u4'
  noaprefix(5)='z01u5'
  noaprefix(6)='z01u6'
  noaprefix(7)='z01u7'
  noaprefix(8)='z01u8'
                                ; Names of seasons
  seasname = STRARR(4)
  seasname(0) = 'DJF'
  seasname(1) = 'MAM'
  seasname(2) = 'JJA'
  seasname(3) = 'SON'
  
                                ; Use one file to read the latitude and longitude grid for all files.
  grid_file=deproot+depprefix(0)+'/'+depprefix(0)+'_2000_precip.nc'
  longitude=OPEN_AND_EXTRACT(grid_file,'longitude')
  latitude=OPEN_AND_EXTRACT(grid_file,'latitude')
                                ; Restrict the dimensions to the box specified above.
  DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
                                ; Get the number of longitude and latitude points in the "box" region.
  nlon=N_ELEMENTS(longitude)
  nlat=N_ELEMENTS(latitude)
  
                                ; Set up the arrays needed to store the data
  depresys_data_array = fltarr(nmem,nstart, nlon, nlat, ntim_yr)
  noassim_data_array = fltarr(nmem,nstart, nlon, nlat, ntim_yr)
                                ; Fill with missing values initially
  depresys_data_array(*,*,*,*,*) = !VALUES.F_NAN
  noassim_data_array(*,*,*,*,*) = !VALUES.F_NAN
  
  noassim_time_array = fltarr(nstart, ntim_yr)
  noassim_time_array_same = fltarr(nstart, ntim_yr)
  
                                ; Read in observed rainfall from SILO, annual means
  silo_infile='/home/ss901165/datasets/SILO/n48/SILO.jan-dec_ameans.1958-2001.precip.n48.nc'
  silo_longitude=OPEN_AND_EXTRACT(silo_infile,'longitude')
  silo_latitude=OPEN_AND_EXTRACT(silo_infile,'latitude')
  DEFINE_BOUNDARIES,box,silo_latitude,silo_longitude,silo_box_tx,/LIMIT
  silo_nlon=N_ELEMENTS(silo_longitude)
  silo_nlat=N_ELEMENTS(silo_latitude)
  
  silo_year_start = 3           ;1961 in SILO dataset
  silo_year_stop  = 43          ;2001 in SILO dataset
  silo_nyear=(silo_year_stop-silo_year_start)+1 
  silo_rainfall=OPEN_AND_EXTRACT(silo_infile,'rain',$
                                 offset=[silo_box_tx(1),silo_box_tx(0),silo_year_start],$
                                 count=[silo_nlon,silo_nlat,silo_nyear])
  
                                ; Read in DePreSys data for seasonal means, all initial dates and all ensemble members
  FOR p = 0, 8 DO BEGIN
     
     deppperoot = deproot+depprefix(p)+'/'     
     noapperoot = noaroot+noaprefix(p)+'/'
     
     count = 0
     
     yr = 1960
     li = 1
     decnumi = 0
     
     while yr LT 2006 do begin
        
        year = string(yr)
        year = strtrim(year,1)
        m = 0
        l = li
        
        dep_filenm = deppperoot+depprefix(p)+'_'+year+'_precip.nc'
        noa_filenm = noapperoot+noaprefix(p)+'_'+year+'_precip.nc'
        
;    a = NCREAD(dep_filenm)
;    b = NCREAD(noa_filenm)
        
;    atemp = REFORM( a.precip )
;    btemp = REFORM( b.precip )     
        
        atemp=REFORM(OPEN_AND_EXTRACT(dep_filenm,'precip',$
                                      offset=[box_tx(1),box_tx(0),0,0],$
                                      count=[nlon,nlat,1,39]))
        btemp=REFORM(OPEN_AND_EXTRACT(noa_filenm,'precip',$
                                      offset=[box_tx(1),box_tx(0),0,0],$
                                      count=[nlon,nlat,1,39]))
        btime=OPEN_AND_EXTRACT(noa_filenm,'time')
        atime=OPEN_AND_EXTRACT(dep_filenm,'time')
        
;    btime = REFORM( b.time )
        
;    ntim = N_ELEMENTS(a.time) 
        
        
        FOR tim = 0,ntim_yr - 1 DO BEGIN           
                                ; REBIN the data into the annual means; (tim*4):(tim*4)+3 gives the four seasonal-mean values for each year, which
                                ; are then averaged into the annual means.
           depresys_data_array(p,count,*,*,tim) = REBIN( atemp(*,*,(tim*4):(tim*4)+3), nlon, nlat, 1)
           noassim_data_array(p,count,*,*,tim) = REBIN( btemp(*,*,(tim*4):(tim*4)+3), nlon, nlat, 1)
                      
           IF p EQ 0 THEN BEGIN
              
              noassim_time_array(count,tim) = (MEAN(btime((tim*4):(tim*4)+3))/360)+year+(11./12)
              noassim_time_array_same(count,*) = noassim_time_array(count,0)
             
           ENDIF 
          
        ENDFOR 
        
;    longitude = a.longitude
;    latitude = a.latitude
        
        yr = yr+1
        count = count+1
     ENDWHILE 
     
  ENDFOR    
                                ; Calculate the ENSEMBLE means of the hindcasts by averaging over the nine ensemble members.
  depresys_ensmean_array = REFORM( REBIN( depresys_data_array, 1, nstart, nlon, nlat, ntim_yr) ) 
  noassim_ensmean_array = REFORM( REBIN( noassim_data_array, 1, nstart, nlon, nlat, ntim_yr) ) 
  
                                ; Arrays to hold the correlations, over all longitude and latitude points
                                ; and over all forecast lead times (year+1, year+2, year+3, etc.)
  depresys_corr = FLTARR(nlon,nlat,ntim_yr)
  noassim_corr = FLTARR(nlon,nlat,ntim_yr)
  
                                ; Compute the correlations against SILO rainfall at each gridpoint.
                                ; The tim=0,ntim_yr-1 loop slides the window of the verifying observations along, so that each 
                                ; hindcast is compared against the correct years from SILO.
  FOR lon = 0, nlon-1 DO BEGIN
     FOR lat = 0, nlat-1 DO BEGIN
        FOR tim = 0, ntim_yr-1 DO BEGIN
           
           depresys_corr(lon,lat,tim) = CORRELATE( REFORM(silo_rainfall(lon,lat,tim:silo_nyear-1) ), REFORM( depresys_ensmean_array(0:nstart-6-tim,lon,lat,tim) )  )               
           noassim_corr(lon,lat,tim) = CORRELATE( REFORM(silo_rainfall(lon,lat,tim:silo_nyear-1) ), REFORM( noassim_ensmean_array(0:nstart-6-tim,lon,lat,tim) )  )
           
        ENDFOR
     ENDFOR
  ENDFOR
  
                                ; Do plotting here.  One plot per forecast lead time, nine plots in total, for assimilated and non-assimilated forecasts.

  STOP

END  
