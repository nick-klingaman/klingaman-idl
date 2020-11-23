; Code developed by Claudia Stephan, National Centre for Atmospheric Science, 
; University of Reading, United Kingdom, 2015-2016.

; This work and its author were supported by the UK-China Research & Innovation 
; Partnership Fund through the Met Office Climate Science for Service Parternship 
; (CSSP) China as part of the Newton Fund.

; This code is licensed under a Creative Commons Attribution 2.0 UK: 
; England & Wales License, available at https://creativecommons.org/licenses/by/2.0/uk/legalcode.  
; You are free to copy and modify the code for any purpose, as long as you give appropriate 
; credit to the author, provide the above link to the license and indicate
; if changes were made.  You may give credit in any reasonable manner, 
; but not in any way that suggests the licensor endorses you or your use.  
; No warranty or guarantee of fitness for any particular purpose is supplied 
; with this code or this license.



;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; ABOUT THIS CODE:
; It performs the EOT procedure described in Smith et al. (2004) 
; 
; INPUT: 
; - Seasonal precipitation data on a lat-lon grid
; - A mask defining China: 0 outside of China, 1 inside
; 
; OUTPUT:
; the following files are saved separately 
; for each season:
; 
;    <NAME>             <CONTENT>                      <DIMENSIONS>   
; - eot_ts           EOT time series                  (neots,nyears)
; - eot_pts          EOT base points (lon,lat)        (neots,2) 
; - eot_expvar       Variance expl. by individual EOT (neots)
; - eot_spatial      EOT spatial patterns             (neots,nlons,nlats)
; - eot_signifi      Significance of EOT pattern      (neots,nlons,nlats)
; - eot_lon          longitudes                       (nlons)
; - eot_lat          latitudes                        (nlats)
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

; get precipitation data, longitudes, latitudes and mask file:
restore, '/group_workspaces/jasmin/cssp_china/users/cstephan/APHRODITE_IDL/APHRO_seas.sav'
restore, '/group_workspaces/jasmin/cssp_china/users/cstephan/APHRODITE_IDL/lon.sav'
restore, '/group_workspaces/jasmin/cssp_china/users/cstephan/APHRODITE_IDL/lat.sav'
restore, '/group_workspaces/jasmin/cssp_china/users/cstephan/APHRODITE_IDL/aphmask.sav'

; reduce arrays to relevant longitudes and latitiudes
dummy=max(aphmask,dimension=2)
milo=min(where(dummy eq 1)) & malo=max(where(dummy eq 1))
dummy=max(aphmask,dimension=1)
mila=min(where(dummy eq 1)) & mala=max(where(dummy eq 1))
mask=aphmask[milo:malo,mila:mala]
data=APHRO_seas[milo:malo,mila:mala,*,*]
lon=lon[milo:malo] & lat=lat[mila:mala]

; this will be saved:
eot_lon=lon & eot_lat=lat        
                  
nlons=n_elements(lon) & nlats=n_elements(lat)
nyears=n_elements(APHRO_seas[0,0,0,*])

; set data outside of China to NAN
for s=0, 3 do begin ;season
    for a=0, nyears-1 do begin ;year
        subset=data[*,*,s,a]
        subset[where(mask eq 0)]=!Values.f_nan
        data[*,*,s,a]=subset
    endfor
endfor

; number of seasons and number of EOTs to be computed for each season
nseasons=4
neots=3
  
for s=0, nseasons-1 do begin 
    case s of
        0: sea='_djf'
        1: sea='_mam'
        2: sea='_jja'
        3: sea='_son'
    endcase

    ;pick data for the season
    data_sea=reform(data[*,*,s,*])               
  
    ;define fields that will be returned 
    eot_ts     =fltarr(neots,nyears)             ;EOT time series
    eot_pts    =fltarr(neots,2)                  ;EOT base points (lon,lat)
    eot_expvar =fltarr(neots)                    ;Variance expl. by individual EOT
    eot_spatial=fltarr(neots,nlons,nlats)        ;EOT spatial patterns
    eot_signifi=fltarr(neots,nlons,nlats)        ;Significance of EOT pattern

    ;remove time-mean from each grid point
    for i=0, nlons-1 do begin 
        for j=0, nlats-1 do begin
            if finite(data_sea[i,j,0]) eq 1 then $
                data_sea[i,j,*]=data_sea[i,j,*]-mean(data_sea[i,j,*],/NAN)
        endfor
    endfor

    ;we do not have an equidistant grid, compute weights:
    aavg_weight=fltarr(nlons,nlats) 
    for i=0, nlons-1 do begin 
        for j=0, nlats-1 do begin 
            if finite(data_sea[i,j,0]) eq 1 then begin
                aavg_weight[i,j]=cos(3.14159*lat[j]/180.)
            endif else begin 
                aavg_weight[i,j]=!Values.f_nan
            endelse
        endfor
    endfor
    aavg_weight=aavg_weight/total(aavg_weight,/NAN)
    
    ;compute total space-time variance: data_stvar
    dummy=data_sea
    for a=0, nyears-1 do begin
        dummy[*,*,a]=reform(dummy[*,*,a])*sqrt(aavg_weight)
    endfor
    data_stvar=variance(dummy,/NAN,/DOUBLE)


    for k=0, neots-1 do begin
        ;spatial pattern for this EOT
        pattern=fltarr(nlons,nlats)                                    

        ;compute area-averaged timeseries
        data_sea_aavg_ts=fltarr(nyears)
        for a=0, nyears-1 do begin
            data_sea_aavg_ts[a]=mean(reform(data_sea[*,*,i])*aavg_weight,/NAN,/DOUBLE)
        endfor
      
        eot_max_explained=0
        for i=0, nlons-1 do begin 
            for j=0, nlats-1 do begin 
                if finite(data_sea[i,j,0]) eq 1 and max(abs(data_sea[i,j,*]),/NAN) ne 0 then begin
                   
                    ;pick time series at a point inside China 
                    data_sea_point_ts=reform(data_sea[i,j,*])          
           
                    ;area-averaged variance that is explained by variance at point (i,j)
                    variance_explained=correlate(data_sea_aavg_ts,data_sea_point_ts,/DOUBLE)^2*variance(data_sea_aavg_ts,/DOUBLE,/NAN)
           
                    ;store information if (i,j) is the new best match
                    if variance_explained gt eot_max_explained then begin
                        eot_max_explained=variance_explained
                        eot_pt=[i,j]
                    endif
                endif  
            endfor    
        endfor            
   
        ;store lon and lat of base point 
        eot_pts[k,*]=[lon[eot_pt[0]],lat[eot_pt[1]]]
      
        ;store base point time series
        eot_ts[k,*]=reform(data_sea[eot_pt[0],eot_pt[1],*])
      
        ;for now we will need the time series again...
        this_eot_ts=reform(eot_ts[k,*])
        ;...to compute the spatial spattern
      
        ;portion of data explained by this EOT:
        data_eot_explained=fltarr(nlons,nlats,nyears)                    
      
        ;standard deviation of this EOT
        this_eot_stddev=stddev(this_eot_ts,/NAN,/DOUBLE)
      
        for i=0, nlons-1 do begin 
            for j=0, nlats-1 do begin
                if finite(data_sea[i,j,0]) eq 1 then begin
                    data_sea_point_ts=reform(data_sea[i,j,*])         
                    point_stddev=stddev(data_sea_point_ts,/NAN,/DOUBLE)
                    
                    ;this is shown in spatial maps
                    point_corr=correlate(data_sea_point_ts,this_eot_ts,/DOUBLE)
                   
                    ; use Spearman's rank correlation to determine significance 
                    ; of the spatial correlation of each point with base point
                    rho=r_correlate(data_sea_point_ts,this_eot_ts)
            
                    ;compute part of time series that is explained by base point EOT time series
                    point_eot=point_corr*point_stddev/this_eot_stddev
                    for a=0, nyears-1 do begin 
                        data_eot_explained[i,j,a]      =this_eot_ts[a]*point_eot   
                        ;remove it by linear regression for the next higher-order EOT computation
                        data_sea[i,j,a]                =data_sea[i,j,a]            -data_eot_explained[i,j,a]         
                    endfor
           
                    ;store spatial pattern
                    eot_spatial[k,i,j]=point_corr   
                    ;and significance
                    eot_signifi[k,i,j]=1-rho[1]     
                endif else begin 
                    eot_spatial[k,i,j]=!Values.f_nan
                    eot_signifi[k,i,j]=!Values.f_nan
                    data_eot_explained[i,j,*]=!Values.f_nan
                endelse
            endfor
        endfor     

        ;compute percentage of total space-time variance explained by this EOT    
        dummy=data_eot_explained
        for a=0, nyears-1 do begin
            dummy[*,*,a]=reform(dummy[*,*,a])*sqrt(aavg_weight)
        endfor
        eot_expvar[k] =variance(dummy,/NAN,/DOUBLE)/data_stvar*100.
      
    endfor ;k ends
   

    ;reorder the eots by highest explained variance
    order=reverse(sort(eot_expvar))
    Deot_ts     =fltarr(neots,nyears)             ;EOT time series 
    Deot_pts    =fltarr(neots,2)                  ;EOT base points (lon,lat)
    Deot_expvar =fltarr(neots)                    ;Variance expl. by individual EOT
    Deot_spatial=fltarr(neots,nlons,nlats)        ;EOT spatial patterns
    Deot_signifi=fltarr(neots,nlons,nlats)        ;Significance of EOT patterns
    for e=0, neots-1 do begin
        Deot_ts[e,*]       =eot_ts[order[e],*]
        Deot_pts[e,*]      =eot_pts[order[e],*] 
        Deot_expvar[e]     =eot_expvar[order[e]]
        Deot_spatial[e,*,*]=eot_spatial[order[e],*,*]
        Deot_signifi[e,*,*]=eot_signifi[order[e],*,*]
    endfor
    eot_ts      =Deot_ts
    eot_pts     =Deot_pts
    eot_expvar  =Deot_expvar
    eot_spatial =Deot_spatial
    eot_signifi =Deot_signifi

    ;saving the output
    save, eot_ts,      FILENAME='/group_workspaces/jasmin/cssp_china/users/cstephan/APHRODITE_IDL/eot_ts_'+opt+sea+'.sav'
    save, eot_pts,     FILENAME='/group_workspaces/jasmin/cssp_china/users/cstephan/APHRODITE_IDL/eot_pts_'+opt+sea+'.sav'
    save, eot_expvar,  FILENAME='/group_workspaces/jasmin/cssp_china/users/cstephan/APHRODITE_IDL/eot_expvar_'+opt+sea+'.sav'
    save, eot_spatial, FILENAME='/group_workspaces/jasmin/cssp_china/users/cstephan/APHRODITE_IDL/eot_spatial_'+opt+sea+'.sav'
    save, eot_signifi, FILENAME='/group_workspaces/jasmin/cssp_china/users/cstephan/APHRODITE_IDL/eot_signifi_'+opt+sea+'.sav'
    save, eot_lon,     FILENAME='/group_workspaces/jasmin/cssp_china/users/cstephan/APHRODITE_IDL/eot_lon_'+opt+sea+'.sav'
    save, eot_lat,     FILENAME='/group_workspaces/jasmin/cssp_china/users/cstephan/APHRODITE_IDL/eot_lat_'+opt+sea+'.sav'

endfor ;season

end



