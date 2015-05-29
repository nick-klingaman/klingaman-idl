PRO hadgem3_monwg_ga50_timestep_corr_spatialagg

box=[-20,45,15,175]
;box=[-5,65,5,75]

use_random=0
use_scrambled=0

n_runs=16
model_names=strarr(n_runs)
model_colors=strarr(n_runs)
random_colors=strarr(n_runs)
scrambled_colors=strarr(n_runs)
styles=intarr(n_runs)
syms=intarr(n_runs)
;spatial_x= [1,2,3,5,10,15,20];,25,30,35,50,75,100,125,150]
;spatial_y= [1,1,1,1,2,  3, 4];, 5, 6, 7,10,15, 20, 25, 30]
;hires_only=[-1,-1,-1,-1,-1,  -1, -1, -1, -1, -1, 0, 0, 0,  0,  0]
;equal_area=[3,4,5,7,9,10,11,12,13,14]

spatial_x= [1,  2, 3, 4, 5, 6, 7, 8];, 10,15,20];,25,30,35,40]
spatial_y= [1,  2, 3, 4, 5, 6, 7, 8];, 10,15,20];,25,30,35,40]
hires_only=[-1,-1,-1,-1,-1,-1,-1,-1];, -1, -1, -1];, 0, 0, 0, 0]
; As offset to spatial_x and spatial_y
equal_area=[4,8,9,10,11,12,13,14]

;spatial_x=[1,2,3,4,5,6,7,8]
;spatial_y=[1,2,3,4,5,6,7,8]

field_name='total precipitation'
netcdf_name='precip'

n_fields=6
n_spatial=N_ELEMENTS(spatial_x)
model_lagone_corr=fltarr(n_runs,n_spatial,5)
random_lagone_corr=fltarr(n_runs,n_spatial,5)
scrambled_lagone_corr=fltarr(n_runs,n_spatial,5)
n_precip_pts=fltarr(n_runs,n_spatial,MAX(spatial_x)*MAX(spatial_y)+1)
random_n_precip_pts=fltarr(n_runs,n_spatial,MAX(spatial_x)*MAX(spatial_y)+1)
FOR p=0,0 DO BEGIN;n_fields-1 DO BEGIN
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
   ymax_npts=intarr(n_runs,n_fields)
   ystep_npts=intarr(n_runs,n_fields)
   FOR i=10,n_runs-1 DO BEGIN
      print,'---> '+STRTRIM(STRING(i),1)
      CASE i OF
         0 : BEGIN
            infile='/home/ss901165/um_output3/hadgem3_monwg/amzgg/amzgga.jun-sep_tsmeans.1982.'+file_name+'.nc'
            model_names(i)='GA5.0 N96 timestep'
            model_colors(i)='red'
            random_colors(i)='orange'
            scrambled_colors(i)='brown'
            styles(i)=0
            ymax_npts(i,*)= [ 64,64,64,24,24,64]
            ystep_npts(i,*)=[  4, 4, 4, 2, 2, 4]
            syms(i)=3
            time_name='t'
         END
         1 : BEGIN
            infile='/home/ss901165/um_output3/hadgem3_monwg/amzgg/amzgga.jun-sep_3hrmeans.1982.'+file_name+'.nc'
            model_names(i)='GA5.0 N96 3hr'
            model_colors(i)='red'
            random_colors(i)='orange'
            scrambled_colors(i)='brown'
            styles(i)=1
            ymax_npts(i,*)= [ 64,64,64,24,24,64]
            ystep_npts(i,*)=[  4, 4, 4, 2, 2, 4]
            syms(i)=3
            time_name='t'           
         END
         2 : BEGIN
            infile='/home/ss901165/um_output3/hadgem3_monwg/amzgg/amzgga.jun-sep_dmeans.1982.'+file_name+'.nc'
            model_names(i)='GA5.0 N96 daily'
            model_colors(i)='red'
            random_colors(i)='orange'
            scrambled_colors(i)='brown'
            styles(i)=2
            ymax_npts(i,*)= [ 64,64,64,24,24,64]
            ystep_npts(i,*)=[  4, 4, 4, 2, 2, 4]
            syms(i)=3
            time_name='t'                        
         END
         ;3 : BEGIN
         ;   infile='/home/ss901165/um_output3/hadgem3_monwg/amzgg/amzgga.jun-sep_5dmeans.1982.'+file_name+'.nc'
         ;   model_names(i)='N96 5-day mean'
         ;   model_colors(i)='red'
         ;   random_colors(i)='orange'
         ;   scrambled_colors(i)='brown'
         ;   styles(i)=3
         ;   ymax_npts(i,*)= [ 64,64,64,24,24,64]
         ;   ystep_npts(i,*)=[  4, 4, 4, 2, 2, 4]
         ;   syms(i)=3
         ;END
         3 : BEGIN
            infile='/home/ss901165/um_output3/hadgem3_monwg/anbba/anbbaa.jun-sep_tsmeans.1982.'+file_name+'.nc'
            model_names(i)='GA5.0 N512 timestep'
            model_colors(i)='blue'
            random_colors(i)='cyan'
            scrambled_colors(i)='steelblue'
            styles(i)=0
            ymax_npts(i,*) =[400,200,400,100,100,400]
            ystep_npts(i,*)=[ 20, 10, 20,  5,  5, 20]
            syms(i)=4
            time_name='t'           
         END
         4 : BEGIN
            infile='/home/ss901165/um_output3/hadgem3_monwg/anbba/anbbaa.jun-sep_3hrmeans.1982.'+file_name+'.nc'
            model_names(i)='GA5.0 N512 3hr'
            model_colors(i)='blue'
            random_colors(i)='cyan'
            scrambled_colors(i)='steelblue'
            styles(i)=1
            ymax_npts(i,*) =[400,200,400,100,100,400]
            ystep_npts(i,*)=[ 20, 10, 20,  5,  5, 20]
            syms(i)=4
            time_name='t'           
         END
         5 : BEGIN
            infile='/home/ss901165/um_output3/hadgem3_monwg/anbba/anbbaa.jun-sep_dmeans.1982.'+file_name+'.nc'
            model_names(i)='GA5.0 N512 daily'
            model_colors(i)='blue'
            random_colors(i)='cyan'
            scrambled_colors(i)='steelblue'
            styles(i)=2
            ymax_npts(i,*) =[400,200,400,100,100,400]
            ystep_npts(i,*)=[ 20, 10, 20,  5,  5, 20]
            syms(i)=4
            time_name='t'
         END
         ;7 : BEGIN
         ;   infile='/home/ss901165/um_output3/hadgem3_monwg/anbba/anbbaa.jun-sep_5dmeans.1982.'+file_name+'.nc'
         ;   model_names(i)='N512 (25km)'
         ;   model_colors(i)='purple'
         ;   random_colors(i)='cyan'
         ;   scrambled_colors(i)='steelblue'
         ;   styles(i)=3
         ;   ymax_npts(i,*) =[400,200,400,100,100,400]
         ;   ystep_npts(i,*)=[ 20, 10, 20,  5,  5, 20]
         ;   syms(i)=4
         ;END
         6 : BEGIN
            infile='/home/ss901165/datasets/TRMM_3B42V6/n96/TRMM_3B42v6A.jan-dec_3hrmeans_ts.1999-2010.n96.nc'
            model_names(i)='TRMM N96 3hr'
            model_colors(i)='orange'
            random_colors(i)='cyan'
            scrambled_colors(i)='steelblue'
            styles(i)=1
            ymax_npts(i,*) =[400,200,400,100,100,400]
            ystep_npts(i,*)=[ 20, 10, 20,  5,  5, 20]
            syms(i)=5  
            time_name='time'
         END
         7 : BEGIN
            infile='/home/ss901165/datasets/TRMM_3B42V6/n96/TRMM_3B42v6A.jan-dec_dmeans_ts.1999-2011.n96.nc'
            model_names(i)='TRMM N96 daily'
            model_colors(i)='orange'
            random_colors(i)='cyan'
            scrambled_colors(i)='steelblue'
            styles(i)=2
            ymax_npts(i,*) =[400,200,400,100,100,400]
            ystep_npts(i,*)=[ 20, 10, 20,  5,  5, 20]
            syms(i)=5
            time_name='time'
         END
         8 : BEGIN
            infile='/home/ss901165/datasets/TRMM_3B42V6/native_resolution/TRMM_3B42v6A.jan-dec_3hrmeans.2003.precip.nc'
            model_names(i)='TRMM 0.25 deg 3hr'
            model_colors(i)='cyan'
            random_colors(i)='cyan'
            scrambled_colors(i)='steelblue'
            styles(i)=1
            ymax_npts(i,*) =[400,200,400,100,100,400]
            ystep_npts(i,*)=[ 20, 10, 20,  5,  5, 20]
            syms(i)=6  
            time_name='time'
         END
         9 : BEGIN
            infile='/home/ss901165/datasets/TRMM_3B42V6/native_resolution/TRMM_3B42v6A.jan-dec_dmeans_ts.1999-2010.nc'
            model_names(i)='TRMM 0.25 deg daily'
            model_colors(i)='cyan'
            random_colors(i)='cyan'
            scrambled_colors(i)='steelblue'
            styles(i)=2
            ymax_npts(i,*) =[400,200,400,100,100,400]
            ystep_npts(i,*)=[ 20, 10, 20,  5,  5, 20]
            syms(i)=6
            time_name='time'
         END
         10 : BEGIN
            infile='/home/ss901165/um_output6/answa_N1024param/wp.nc'
            model_names(i)='GA6.0 N1024 param'
            model_colors(i)='red'
            styles(i)=0
            ymax_npts(i,*) =[1600,800,1600,400,400,1600]
            ystep_npts(i,*)=[ 80, 40, 80,  20,  20, 80]
            netcdf_name='tot_precip'
            syms(i)=3
            time_name='t'
         END
         11 : BEGIN
            infile='/home/ss901165/um_output6/answa_N1024param/wp_3hrmeans.nc'
            model_names(i)='GA6.0 N1024 param'
            model_colors(i)='red'
            styles(i)=1
            ymax_npts(i,*) =[1600,800,1600,400,400,1600]
            ystep_npts(i,*)=[ 80, 40, 80,  20,  20, 80]
            netcdf_name='tot_precip'
            syms(i)=3
         END
         12 : BEGIN
            infile='/home/ss901165/um_output6/answa_N1024param/wp_dmeans.nc'
            model_names(i)='GA6.0 N1024 param'
            model_colors(i)='red'
            styles(i)=2
            ymax_npts(i,*) =[1600,800,1600,400,400,1600]
            ystep_npts(i,*)=[ 80, 40, 80,  20,  20, 80]
            netcdf_name='tot_precip'
            syms(i)=3
         END
         13 : BEGIN
            infile='/home/ss901165/um_output6/answc_N1024explt/wp.nc'
            model_names(i)='GA6.0 N1024 explt'
            model_colors(i)='blue'
            styles(i)=0
            ymax_npts(i,*) =[1600,800,1600,400,400,1600]
            ystep_npts(i,*)=[ 80, 40, 80,  20,  20, 80]
            netcdf_name='tot_precip'
            syms(i)=4
         END
         14 : BEGIN
            infile='/home/ss901165/um_output6/answc_N1024explt/wp_3hrmeans.nc'
            model_names(i)='GA6.0 N1024 explt'
            model_colors(i)='blue'
            styles(i)=1
            ymax_npts(i,*) =[1600,800,1600,400,400,1600]
            ystep_npts(i,*)=[ 80, 40, 80,  20,  20, 80]
            netcdf_name='tot_precip'
            syms(i)=4
         END
         15 : BEGIN
            infile='/home/ss901165/um_output6/answc_N1024explt/wp_dmeans.nc'
            model_names(i)='GA6.0 N1024 explt'
            model_colors(i)='blue'
            styles(i)=2
            ymax_npts(i,*) =[1600,800,1600,400,400,1600]
            ystep_npts(i,*)=[ 80, 40, 80,  20,  20, 80]
            netcdf_name='tot_precip'
            syms(i)=4
         END

         999 : BEGIN
            infile='/home/ss901165/um_output3/hadgem3_monwg/anbba/anbbaa.jun-sep_tsmeans.1982.'+file_name+'.n96.nc'
            model_names(i)='ga50_n512-interp-n96'
            model_colors(i)='purple'
            random_colors(i)='violet'
            scrambled_colors(i)='deeppink'
            styles(i)=0
            ymax_npts(i,*)= [ 64,64,64,24,24,64]
            ystep_npts(i,*)=[  4, 4, 4, 2, 2, 4]
         END
      ENDCASE
      
      longitude=OPEN_AND_EXTRACT(infile,'longitude')
      latitude=OPEN_AND_EXTRACT(infile,'latitude')
      DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
      n_lon=N_ELEMENTS(longitude)
      n_lat=N_ELEMENTS(latitude)
      n_time=N_ELEMENTS(OPEN_AND_EXTRACT(infile,time_name))
      ;n_time=10
      
                                ; Model precipitation
      model_precip=OPEN_AND_EXTRACT(infile,netcdf_name,$
                                    offset=[box_tx(1),box_tx(0),0],$
                                    count=[n_lon,n_lat,n_time])      
      IF TOTAL(where(model_precip le 0)) ge 0 THEN $
         model_precip[where(model_precip le 0)]=0.
      IF TOTAL(where(model_precip ge 10000)) ge 0 THEN $
         model_precip[where(model_precip ge 10000)]=0.

                                ; Scrambled data
      IF use_scrambled eq 1 THEN BEGIN
         n_pts=n_lon*n_lat
         scrambled_precip=fltarr(n_lon,n_lat,n_time)
         order=SORT(RANDOMU(seed,n_pts))
         FOR j=0,n_time-1 DO BEGIN
            temp=REFORM(model_precip(*,*,j))
            scrambled_precip(*,*,j)=temp[order]
         ENDFOR
      ENDIF
      
                                ; Synthetic data
      IF use_random eq 1 THEN BEGIN
         random_precip=fltarr(n_lon,n_lat,n_time)   
         FOR j=0,n_lon-1 DO BEGIN
            FOR k=0,n_lat-1 DO BEGIN
               precip=REFORM(model_precip(j,k,*))
               precip_bins=findgen(200)*(MAX(precip)/200.)
               n_bins=N_ELEMENTS(precip_bins)
               precip_prob=fltarr(n_bins+1)
               mean_precip=fltarr(n_bins+1)
               precip_cdf=fltarr(n_bins+1)
               FOR m=1,n_bins-1 DO BEGIN
                  valid=where(precip gt precip_bins(m-1) and precip le precip_bins(m))
                  IF TOTAL(valid) ge 0 THEN BEGIN
                     precip_prob(m)=N_ELEMENTS(valid)
                     mean_precip(m)=MEAN(precip[valid])
                  ENDIF ELSE $
                     mean_precip(m)=mean_precip(m-1)
               ENDFOR
               precip_prob(0)=N_ELEMENTS(where(precip eq 0))
               mean_precip(0)=0.
               FOR m=2,n_bins DO $
                  precip_cdf(m)=TOTAL(precip_prob(1:m-1))/TOTAL(precip_prob(1:n_bins))
               precip_cdf(1)=0.
               precip_cdf(0)=-999.
               n_autocorrs=2
               p_wet=fltarr(n_autocorrs+1)
               p_dry=fltarr(n_autocorrs+1)
               p_wet(0)=n_time-precip_prob(0)
               p_dry(0)=precip_prob(0)
               FOR m=LONG(n_autocorrs+1),LONG(n_time)-1 DO BEGIN
                  IF precip(m) gt 0 THEN BEGIN
                     n=1
                     WHILE n le n_autocorrs and precip(m-n) gt 0 DO BEGIN
                        p_wet(n)=p_wet(n)+1
                        n=n+1
                     ENDWHILE
                  ENDIF ELSE BEGIN
                     n=1
                     WHILE n le n_autocorrs and precip(m-n) eq 0 DO BEGIN
                        p_dry(n)=p_dry(n)+1
                        n=n+1
                     ENDWHILE
                  ENDELSE
               ENDFOR
               FOR m=n_autocorrs,1,-1 DO BEGIN
                  p_wet(m)=p_wet(m)/p_wet(m-1)
                  p_dry(m)=p_dry(m)/p_dry(m-1)
               ENDFOR         
               p_wet(0)=p_wet(0)/n_time
               p_dry(0)=p_dry(0)/n_time
;         p_wet=p_wet/n_time
;         p_dry=p_dry/n_time
               prob=p_wet(0)
               last=intarr(n_autocorrs)
               last(*)=0
               last(n_autocorrs-1)=999
               FOR m=LONG(0),LONG(n_time)-1 DO BEGIN
                  wet=RANDOMU(seed,1,binomial=[1,prob(0)])
                  undefine,seed
                  IF wet eq 1 THEN BEGIN
                     temp=RANDOMU(seed,1)
                     undefine,seed
                     random_precip(j,k,m)=mean_precip(NEAREST(precip_cdf,temp(0)))
                     n=0
                     WHILE n lt n_autocorrs-1 and last(n) eq 1 DO BEGIN
                        prob=p_wet(n+1)
                        n=n+1
                     ENDWHILE
                  ENDIF ELSE BEGIN
                     random_precip(j,k,m)=0.
                     n=0
                     WHILE n lt n_autocorrs-1 and last(n) eq 0 DO BEGIN
                     prob=1-p_dry(n+1)
                     n=n+1
                  ENDWHILE
                  ENDELSE
                  FOR n=n_autocorrs-2,1,-1 DO $
                     last(n)=last(n-1)
                  last(0)=wet
               ENDFOR
            ENDFOR
         ENDFOR
      ENDIF
      
      this_nprecip_pts=fltarr(n_time)
      FOR j=0,n_spatial-1 DO BEGIN  
         IF i gt hires_only(j) THEN BEGIN
            print,'-------> '+STRTRIM(STRING(j),1)
            this_nlon=n_lon/spatial_x(j)
            this_nlat=n_lat/spatial_y(j)
            this_model_lagone_corr=fltarr(this_nlon,this_nlat)
            IF use_random eq 1 THEN $
               this_random_lagone_corr=fltarr(this_nlon,this_nlat)
            IF use_scrambled eq 1 THEN $
               this_scrambled_lagone_corr=fltarr(this_nlon,this_nlat)
            n_valid_pts=0
            FOR k=0,this_nlon-1 DO BEGIN
               FOR m=0,this_nlat-1 DO BEGIN
                  x_low=k*spatial_x(j)
                  x_high=(k+1)*spatial_x(j)-1
                  y_low=m*spatial_y(j)
                  y_high=(m+1)*spatial_y(j)-1
                  IF i eq 2 and j eq 4 THEN $
                     print,x_low,x_high,y_low,y_high,i,j
                  IF j gt 0 and y_high le n_lat-1 and y_low ge 0 $
                     and x_low ge 0 and x_high le n_lon-1 THEN BEGIN
                     model_precip_boxavg=fltarr(n_time)
                     IF use_random eq 1 THEN $
                        random_precip_boxavg=fltarr(n_time)
                     IF use_scrambled eq 1 THEN $
                        scrambled_precip_boxavg=fltarr(n_time)
                     FOR n=LONG(0),LONG(n_time-1) DO BEGIN                  
                        model_precip_box=model_precip(x_low:x_high,y_low:y_high,n)
                        IF use_random eq 1 THEN BEGIN
                           random_precip_box=random_precip(x_low:x_high,y_low:y_high,n)                        
                           random_precip_boxavg(n)=MEAN(random_precip_box)
                           IF TOTAL(where(random_precip_box gt 0)) ge 0 THEN BEGIN
                              temp=N_ELEMENTS(where(random_precip_box gt 0))
                              random_n_precip_pts(i,j,temp)=random_n_precip_pts(i,j,temp)+1
                           ENDIF ELSE $
                              random_n_precip_pts(i,j,0)=random_n_precip_pts(i,j,0)+1
                        ENDIF
                        IF use_scrambled eq 1 THEN BEGIN
                           scrambled_precip_box=scrambled_precip(x_low:x_high,y_low:y_high,n)
                           scrambled_precip_boxavg(n)=MEAN(scrambled_precip_box)
                        ENDIF
                        model_precip_boxavg(n)=MEAN(model_precip_box)
                        IF TOTAL(where(model_precip_box gt 0)) ge 0 THEN BEGIN
                           temp=N_ELEMENTS(where(model_precip_box gt 0))
                           n_precip_pts(i,j,temp)=n_precip_pts(i,j,temp)+1
                        ENDIF ELSE $
                           n_precip_pts(i,j,0)=n_precip_pts(i,j,0)+1
                        
                     ENDFOR
                     this_model_lagone_corr(k,m)=A_CORRELATE(model_precip_boxavg,1)
                     IF use_random eq 1 THEN $
                        this_random_lagone_corr(k,m)=A_CORRELATE(random_precip_boxavg,1)
                     IF use_scrambled eq 1 THEN $
                        this_scrambled_lagone_corr(k,m)=A_CORRELATE(scrambled_precip_boxavg,1)
                     n_valid_pts=n_valid_pts+1
                  ENDIF ELSE IF j gt 0 THEN BEGIN
                     this_model_lagone_corr(k,m)=!Values.F_NaN
                     IF use_random eq 1 THEN $
                        this_random_lagone_corr(k,m)=!Values.F_NaN
                     IF use_scrambled eq 1 THEN $
                        this_scrambled_lagone_corr(k,m)=!Values.F_NaN
                  ENDIF ELSE IF j eq 0 THEN BEGIN
                     this_model_lagone_corr(k,m)=A_CORRELATE(model_precip(k,m,*),1)       
                     IF use_scrambled eq 1 THEN $
                        this_scrambled_lagone_corr(k,m)=A_CORRELATE(scrambled_precip(k,m,*),1)                  
                     n_precip_pts(i,j,0)=n_precip_pts(i,j,0)+N_ELEMENTS(where(model_precip(k,m,*) eq 0))
                     n_precip_pts(i,j,1)=n_precip_pts(i,j,1)+N_ELEMENTS(where(model_precip(k,m,*) gt 0))
                     IF use_random eq 1 THEN BEGIN
                        this_random_lagone_corr(k,m)=A_CORRELATE(random_precip(k,m,*),1)
                        random_n_precip_pts(i,j,0)=random_n_precip_pts(i,j,0)+N_ELEMENTS(where(random_precip(k,m,*) eq 0))
                        random_n_precip_pts(i,j,1)=random_n_precip_pts(i,j,1)+N_ELEMENTS(where(random_precip(k,m,*) gt 0))
                     ENDIF
                  ENDIF
               ENDFOR
            ENDFOR
;            IF i eq 2 and j eq 4 THEN STOP
            n_precip_pts(i,j,*)=n_precip_pts(i,j,*)/TOTAL(n_precip_pts(i,j,*))
            IF use_random eq 1 THEN BEGIN
               random_n_precip_pts(i,j,*)=random_n_precip_pts(i,j,*)/TOTAL(random_n_precip_pts(i,j,*))
               random_n_precip_pts(i,0,0)=n_precip_pts(i,0,0)
               random_n_precip_pts(i,0,1)=n_precip_pts(i,0,1)
            ENDIF 
            IF j eq 0 THEN BEGIN
               mylevs=['-0.30','-0.26','-0.22','-0.18','-0.14','-0.10','-0.06','-0.02','0.02','0.06','0.10','0.14','0.18','0.22','0.26','0.30']
               psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/timestep/hadgem3_monwg_ga50_timestep_corr_spatialagg_withrandom.model_lagone.ps'
               PSOPEN,file=psfile,FONT=2,CHARSIZE=120
               MAP,LONMIN=MIN(longitude),LONMAX=MAX(longitude),LATMIN=MIN(latitude),LATMAX=MAX(latitude)
               CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
               LEVS,MANUAL=mylevs
               CON,X=longitude,Y=latitude,FIELD=this_model_lagone_corr,/NOLINES,/BLOCK,CB_TITLE='Lag-1 autocorrelation of model cvrain'
               AXES
               PSCLOSE,/NOVIEW
               
               IF use_random eq 1 THEN BEGIN
                  psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/timestep/hadgem3_monwg_ga50_timestep_corr_spatialagg_withrandom.random_lagone.ps'
                  PSOPEN,file=psfile,FONT=2,CHARSIZE=120
                  MAP,LONMIN=MIN(longitude),LONMAX=MAX(longitude),LATMIN=MIN(latitude),LATMAX=MAX(latitude)
                  CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
                  LEVS,MANUAL=mylevs
                  CON,X=longitude,Y=latitude,FIELD=this_random_lagone_corr,/NOLINES,/BLOCK,CB_TITLE='Lag-1 autocorrelation of randomly generated cvrain'
                  AXES
                  PSCLOSE,/NOVIEW     
               ENDIF
               
               IF use_scrambled eq 1 THEN BEGIN
                  psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/timestep/hadgem3_monwg_ga50_timestep_corr_spatialagg_withrandom.scrambled_lagone.ps'
                  PSOPEN,file=psfile,FONT=2,CHARSIZE=120
                  MAP,LONMIN=MIN(longitude),LONMAX=MAX(longitude),LATMIN=MIN(latitude),LATMAX=MAX(latitude)
                  CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
                  LEVS,MANUAL=mylevs
                  CON,X=longitude,Y=latitude,FIELD=this_scrambled_lagone_corr,/NOLINES,/BLOCK,CB_TITLE='Lag-1 autocorrelation of spatially scrambled cvrain'
                  AXES
                  PSCLOSE,/NOVIEW     
               ENDIF
            ENDIF
            
            sorted=SORT(this_model_lagone_corr)
            n_finite=N_ELEMENTS(where(FINITE(this_model_lagone_corr) eq 1))
            model_lagone_corr(i,j,2)=MEDIAN(this_model_lagone_corr)
            model_lagone_corr(i,j,0)=this_model_lagone_corr(sorted(n_finite*1/100.)) ;MIN(this_model_lagone_corr,/NaN)
            model_lagone_corr(i,j,4)=this_model_lagone_corr(sorted(n_finite*99/100.)) ;MAX(this_model_lagone_corr,/NaN)
            model_lagone_corr(i,j,1)=this_model_lagone_corr(sorted(n_finite/4.))
            model_lagone_corr(i,j,3)=this_model_lagone_corr(sorted(n_finite*3/4.))
            
            IF use_random eq 1 THEN BEGIN
               sorted=SORT(this_random_lagone_corr)
               n_finite=N_ELEMENTS(where(FINITE(this_random_lagone_corr) eq 1))
               random_lagone_corr(i,j,2)=MEDIAN(this_random_lagone_corr)
               random_lagone_corr(i,j,0)=this_random_lagone_corr(sorted(n_finite*1/100.)) ;MIN(this_random_lagone_corr,/NaN)
               random_lagone_corr(i,j,4)=this_random_lagone_corr(sorted(n_finite*99/100.)) ;MAX(this_random_lagone_corr,/NaN)
               random_lagone_corr(i,j,1)=this_random_lagone_corr(sorted(n_finite/4.))
               random_lagone_corr(i,j,3)=this_random_lagone_corr(sorted(n_finite*3/4.))
            ENDIF
            
            IF use_scrambled eq 1 THEN BEGIN
               sorted=SORT(this_scrambled_lagone_corr)
               n_finite=N_ELEMENTS(where(FINITE(this_scrambled_lagone_corr) eq 1))
               scrambled_lagone_corr(i,j,2)=MEDIAN(this_scrambled_lagone_corr)
               scrambled_lagone_corr(i,j,0)=this_scrambled_lagone_corr(sorted(n_finite*1/100.)) ;MIN(this_scrambled_lagone_corr,/NaN)
               scrambled_lagone_corr(i,j,4)=this_scrambled_lagone_corr(sorted(n_finite*99/100.)) ;MAX(this_scrambled_lagone_corr,/NaN)
               scrambled_lagone_corr(i,j,1)=this_scrambled_lagone_corr(sorted(n_finite/4.))
               scrambled_lagone_corr(i,j,3)=this_scrambled_lagone_corr(sorted(n_finite*3./4.))
            ENDIF
;         sorted=SORT(this_nprecip_pts)
;         n_precip_pts(i,j,2)=MEDIAN(this_nprecip_pts)
;         n_precip_pts(i,j,0)=MIN(this_nprecip_pts)
;         n_precip_pts(i,j,4)=MAX(this_nprecip_pts)
;         n_precip_pts(i,j,1)=this_nprecip_pts(sorted(N_ELEMENTS(this_nprecip_pts)/4))
;         n_precip_pts(i,j,3)=this_nprecip_pts(sorted(N_ELEMENTS(this_nprecip_pts)*3/4))
         ENDIF
      ENDFOR
 
      ; Analysis of timestep correlations
      mylevs_corr=['0.02','0.06','0.10','0.14','0.18','0.22','0.26','0.30','0.34','0.38','0.42']
      domain_size=7
      n_lags=1
      thresholds=[1,5,20,40,100]
      n_thresholds=N_ELEMENTS(thresholds)
      
      n_precip_types=1
      FOR s=0,n_precip_types-1 DO BEGIN
         CASE s OF
            0 : BEGIN
               precip=model_precip*86400.
               type='model'
            END
            1 : BEGIN
               precip=random_precip*86400.
               type='random'
            END
            2 : BEGIN
               precip=scrambled_precip*86400.
               type='scrambled'
            END
         ENDCASE
         bad_pts=0
         corr_coeffs=fltarr(domain_size,domain_size,n_lags)
         frac_wet_values=fltarr(domain_size,domain_size,n_thresholds)
         frac_dry_values=fltarr(domain_size,domain_size,n_thresholds)
         central_count_wet=fltarr(n_thresholds)
         central_count_dry=fltarr(n_thresholds)
         avg_frac_wet=fltarr(n_thresholds)      

         FOR p=0,n_thresholds-1 DO $
            avg_frac_wet(p)=N_ELEMENTS(where(precip gt thresholds(p)))/FLOAT(n_lon*n_lat*n_time)
         FOR j=0,n_lon-domain_size,domain_size DO BEGIN
            xpt=j+domain_size/2      
            FOR k=0,n_lat-domain_size,domain_size DO BEGIN
               ypt=k+domain_size/2
               central_ts=REFORM(precip(xpt,ypt,*))
               FOR p=0,n_thresholds-1 DO BEGIN
                  central_ts_heaviside=fltarr(n_time)
                  IF TOTAL(where(central_ts gt thresholds(p))) ge 0 THEN $
                     central_ts_heaviside[where(central_ts gt thresholds(p))]=1
                  IF TOTAL(where(central_ts le thresholds(p))) ge 0 THEN $
                     central_ts_heaviside[where(central_ts le thresholds(p))]=0
                  central_count_wet(p)=central_count_wet(p)+TOTAL(central_ts_heaviside)
                  central_count_dry(p)=central_count_dry(p)+n_time-TOTAL(central_ts_heaviside)
                  FOR m=xpt-domain_size/2,xpt+domain_size/2 DO BEGIN
                     FOR n=ypt-domain_size/2,ypt+domain_size/2 DO BEGIN               
                        thispt_ts=REFORM(precip(m,n,*))
                        thispt_ts_heaviside=fltarr(n_time)
                        IF TOTAL(thispt_ts) ne 0 THEN BEGIN
                           IF TOTAL(where(thispt_ts gt thresholds(p))) ge 0 THEN $
                              thispt_ts_heaviside[where(thispt_ts gt thresholds(p))]=1
                           IF TOTAL(where(thispt_ts le thresholds(p))) ge 0 THEN $
                              thispt_ts_heaviside[where(thispt_ts le thresholds(p))]=0
                           IF p eq 0 THEN BEGIN
                              IF TOTAL(thispt_ts_heaviside) ne 0 and TOTAL(central_ts_heaviside) ne 0 THEN BEGIN
                                 FOR r=0,n_lags-1 DO $
                                    corr_coeffs(m-xpt+domain_size/2,n-ypt+domain_size/2,r)=$
                                    corr_coeffs(m-xpt+domain_size/2,n-ypt+domain_size/2,r)+$
                                    CORRELATE(central_ts(0:n_time-1-r),thispt_ts(r:n_time-1))
                              ENDIF ELSE $
                                 bad_pts=bad_pts+1
                           ENDIF
                           frac_wet_values(m-xpt+domain_size/2,n-ypt+domain_size/2,p)=$
                              frac_wet_values(m-xpt+domain_size/2,n-ypt+domain_size/2,p)+$
                              (N_ELEMENTS(where(thispt_ts_heaviside eq 1 and central_ts_heaviside eq 1)))
                           frac_dry_values(m-xpt+domain_size/2,n-ypt+domain_size/2,p)=$
                              frac_dry_values(m-xpt+domain_size/2,n-ypt+domain_size/2,p)+$
                              (N_ELEMENTS(where(thispt_ts_heaviside eq 1 and central_ts_heaviside eq 0)))
                        ENDIF
                     ENDFOR
                  ENDFOR
               ENDFOR
            ENDFOR
         ENDFOR         
         FOR p=0,n_thresholds-1 DO BEGIN
            frac_wet_values(*,*,p)=frac_wet_values(*,*,p)/FLOAT(central_count_wet(p))
            frac_dry_values(*,*,p)=frac_dry_values(*,*,p)/FLOAT(central_count_dry(p))
         ENDFOR
         corr_coeffs=corr_coeffs/FLOAT(corr_coeffs(domain_size/2,domain_size/2))         
         
;         FOR p=0,n_lags-1 DO BEGIN
;            psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_spatialscale.'+model_names(i)+'_'+type+'.'+$
;                   STRTRIM(STRING(domain_size),1)+'x'+STRTRIM(STRING(domain_size),1)+'.corr_coeffs_lag'+STRTRIM(STRING(p),1)+'.ps'
;            PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=160,MARGIN=2000,XSIZE=17000,YSIZE=17000,/PORTRAIT,XOFFSET=1000,YOFFSET=5000,SPACE2=3000,$
;                   TCHARSIZE=90,SPACE3=200
;            GSET,XMIN=domain_size/2*(-1.)-0.5,XMAX=domain_size/2+0.5,YMIN=domain_size/2*(-1.)-0.5,YMAX=domain_size/2+0.5,$
;                 TITLE='Correlation of tstep precip at (x,y) and lag '+STRTRIM(STRING(p),1)+' with (0,0) - '+model_names(i)+' '+type
;            CS,SCALE=25,NCOLS=N_ELEMENTS(mylevs_corr)+1
;            LEVS,MANUAL=mylevs_corr
;            CON,X=indgen(domain_size)-domain_size/2,Y=indgen(domain_size)-domain_size/2,FIELD=REFORM(corr_coeffs(*,*,p)),/NOLINES,/BLOCK,$
;                CB_WIDTH=110,CB_TITLE='Correlation coefficient'
;            FOR j=0,domain_size-1 DO $
;               FOR k=0,domain_size-1 DO $
;                  GPLOT,X=j-domain_size/2,Y=k-domain_size/2,TEXT=STRMID(STRTRIM(STRING(corr_coeffs(j,k,p)),1),0,4),ALIGN=0.5
;            AXES,XSTEP=1,YSTEP=1,XTITLE='West <-- gridpoints --> East',YTITLE='South <-- gridpoints --> North'  
;            PSCLOSE
;         ENDFOR         
      ENDFOR
   ENDFOR

   psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/timestep/hadgem3_monwg_ga50_timestep_corr_spatialagg_withrandom.IndOcn.'+netcdf_name+'.lagone_corr.equal_grid.n1024.ps'
   PSOPEN,file=psfile,FONT=6,CHARSIZE=150,MARGIN=2000,XOFFSET=1500,YOFFSET=1000
   GSET,XMIN=0,XMAX=n_spatial,YMIN=-0.4,YMAX=1.01

; Plot only median values
   FOR i=10,n_runs-1 DO $
      GPLOT,X=indgen(n_spatial)+0.5,Y=REFORM(model_lagone_corr(i,*,2)),COL=FSC_COLOR(model_colors(i)),SYM=syms(i),STYLE=styles(i)
;   FOR i=0,n_runs-1 DO BEGIN
;      FOR j=0,n_spatial-1 DO BEGIN
;         EBAR,X=j+0.3+0.2*i,BOX=model_lagone_corr(i,j,*),COL=FSC_COLOR(model_colors(i)),WIDTH=60
;      EBAR,X=j+0.2+0.3*i,BOX=random_lagone_corr(i,j,*),COL=FSC_COLOR(random_colors(i)),WIDTH=40
;      EBAR,X=j+0.3+0.3*i,BOX=scrambled_lagone_corr(i,j,*),COL=FSC_COLOR(scrambled_colors(i)),WIDTH=40
;      ENDFOR
;   ENDFOR
   xlabels=strarr(n_spatial)
   FOR j=0,n_spatial-1 DO $
      xlabels(j)=STRTRIM(STRING(spatial_x(j)),1)+'x'+STRTRIM(STRING(spatial_x(j)),1)
   AXES,XVALS=indgen(n_spatial)+0.5,XLABELS=xlabels,YSTEP=0.1,YMINOR=0.05,NDECS=1,$
        YTITLE='Lag-1 corr of box-averaged tstep '+field_name,XTITLE='Size of averaging box (gridpoints)'
;   labels=strarr(4)
;   colors=strarr(4)
;   symbols=strarr(4)
;   labels(0)=model_names(0)
;   colors(0)=model_colors(0)
;   symbols(0)=syms(0)
;   labels(1)=model_names(3)
;   colors(1)=model_colors(3)
;   symbols(1)=syms(3)
;   labels(2)=model_names(6)
;   colors(2)=model_colors(6)
;   symbols(2)=syms(6)
;   labels(3)=model_names(8)
;   colors(3)=model_colors(8)
;   symbols(3)=syms(8)

   labels=strarr(2)
   colors=strarr(2)
   symbols=strarr(2)
   labels(0)=model_names(10)
   colors(0)=model_colors(10)
   symbols(0)=syms(10)
   labels(1)=model_names(13)
   colors(1)=model_colors(13)
   symbols(1)=syms(13)
   
;labels=strarr(n_runs*3)
;colors=strarr(n_runs*3)
;   FOR i=0,n_runs-1 DO BEGIN
;      labels(i)=model_names(i)
;   labels(i*3+1)=model_names(i)+' random'
;   labels(i*3+2)=model_names(i)+' scrambled'
;      colors(i)=model_colors(i)
;   colors(i*3+1)=random_colors(i)
;   colors(i*3+2)=scrambled_colors(i)
;   ENDFOR
   GLEGEND,labels=REVERSE(labels),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=11,SYM=REVERSE(symbols)
   GLEGEND,labels=REVERSE(['Timestep','3-hr means','Daily means']),$
           STYLE=REVERSE([0,1,2]),LEGPOS=3
   PSCLOSE
   STOP

   FOR i=0,n_runs-1 DO BEGIN
      print,i
      mylevs=['0.1','0.2','0.5','1','2','3','4','5','6','7','8','9','10','12','14','16','18','21','24','27']
      this_spatialx=spatial_x[where(hires_only lt i)]
      this_spatialy=spatial_y[where(hires_only lt i)]
      this_nspatial=N_ELEMENTS(this_spatialx)
      FOR m=0,1 DO BEGIN
         print,m
         CASE m OF 
            0 : BEGIN
               this_n_precip_pts=n_precip_pts
               type='model'
            END
            1 : BEGIN
               this_n_precip_pts=random_n_precip_pts
               type='random'
            END
         ENDCASE
         
         psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/timestep/hadgem3_monwg_ga50_timestep_corr_spatialagg_withrandom.IndOcn.'+$
                netcdf_name+'.'+type+'.frac_box.'+model_names(i)+'.ps'
         PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=3000,XOFFSET=500,SPACE2=700,SPACE1=150
         GSET,XMIN=-0.5,XMAX=this_nspatial-0.5,YMIN=0,YMAX=1
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,white=[2]
         LEVS,MANUAL=mylevs
         max_npts=MAX(this_spatialx)*MAX(this_spatialy)*2
         plot=fltarr(this_nspatial,max_npts)
         FOR j=0,this_nspatial-1 DO BEGIN
            npts=this_spatialx(j)*this_spatialy(j)
            plot(j,0)=this_n_precip_pts(i,j,0)
            start=0
            FOR k=1,npts DO BEGIN
               stop=FLOOR(k/FLOAT(npts)*max_npts)-1
               plot(j,start:stop)=this_n_precip_pts(i,j,k)           
               start=stop+1
            ENDFOR
         ENDFOR      
         CON,X=indgen(this_nspatial),Y=findgen(max_npts)/FLOAT(max_npts)+1/FLOAT(max_npts),FIELD=plot*100.,/NOLINES,/BLOCK,$
             CB_TITLE='Probability (from '+type+' '+model_names(i)+' data)'
         AXES,XVALS=indgen(this_nspatial),YVALS=['0.01','0.05','0.10','0.15','0.20','0.25','0.30','0.35','0.40','0.45','0.50','0.55','0.60','0.65','0.70','0.75','0.80',$
                                                 '0.85','0.90','0.95','1.00'],XLABELS=xlabels,/NORIGHT,XTITLE='length of box in native gridpoints (lon x lat)',$
              YTITLE='Fraction of box with '+field_name+' > 0'
         GSET,XMIN=0,XMAX=this_nspatial,YMIN=0,YMAX=1
         GPLOT,X=indgen(this_nspatial)+0.5,Y=REFORM(this_n_precip_pts(i,0:this_nspatial-1,0)),COL=FSC_COLOR('black'),THICK=200
         AXES,YSTEP=0.05,YTITLE='Probability of no precipitation in box in timestep',/ONLYRIGHT,NDECS=2
         PSCLOSE,/NOVIEW
         
         psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/timestep/hadgem3_monwg_ga50_timestep_corr_spatialagg_withrandom.IndOcn.'+$
                netcdf_name+'.'+type+'.npts_box.'+model_names(i)+'.ps'
         PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2000,XOFFSET=1000,SPACE2=700,SPACE1=150,/PORTRAIT
         GSET,XMIN=-0.5,XMAX=this_nspatial-0.5,YMIN=0,YMAX=ymax_npts(i,p) ;YMAX=MAX(this_spatialx)*MAX(this_spatialy)
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,white=[2]
         LEVS,MANUAL=mylevs
         CON,X=indgen(this_nspatial),Y=indgen(MAX(this_spatialx)*MAX(this_spatialy)+1),$
             FIELD=REFORM(this_n_precip_pts(i,0:this_nspatial-1,0:MAX(this_spatialx)*MAX(this_spatialy)))*100.,/BLOCK,/NOLINES,$
             CB_TITLE='Probability (from '+type+' '+model_names(i)+' data)',CB_WIDTH=120
         AXES,XVALS=indgen(this_nspatial),YSTEP=ystep_npts(i,p),YMINOR=1,NDECS=0,XLABELS=xlabels,XTITLE='length of box in native gridpoints (lon x lat)',$
              YTITLE='Number of points in box with '+field_name+' > 0'
         PSCLOSE,/NOVIEW
      ENDFOR
;   mylevs=['-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5']
      mylevs=['-13','-11','-9','-7','-5','-3','-2','-1','-0.5','0.5','1','2','3','5','7','9','11','13']   
      psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/timestep/hadgem3_monwg_ga50_timestep_corr_spatialagg_withrandom.IndOcn.'+$
             netcdf_name+'.model-minus-random.frac_box.'+model_names(i)+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=3000,XOFFSET=500,SPACE2=700,SPACE1=150
      GSET,XMIN=-0.5,XMAX=this_nspatial-0.5,YMIN=0,YMAX=1
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
      LEVS,MANUAL=mylevs
      max_npts=MAX(this_spatialx)*MAX(this_spatialy)*2
      plot=fltarr(this_nspatial,max_npts)
      FOR j=0,this_nspatial-1 DO BEGIN
         npts=this_spatialx(j)*this_spatialy(j)
         plot(j,0)=this_n_precip_pts(i,j,0)
         start=0
         FOR k=1,npts DO BEGIN
            stop=FLOOR(k/FLOAT(npts)*max_npts)-1
            plot(j,start:stop)=n_precip_pts(i,j,k)-random_n_precip_pts(i,j,k)
            start=stop+1
         ENDFOR
      ENDFOR
      CON,X=indgen(this_nspatial),Y=findgen(max_npts)/FLOAT(max_npts)+1/FLOAT(max_npts),FIELD=plot*100.,/NOLINES,/BLOCK,CB_TITLE='Difference in probability '+$
          '( '+model_names(i)+' minus random)',CB_WIDTH=120
      AXES,XVALS=indgen(this_nspatial),YVALS=['0.01','0.05','0.10','0.15','0.20','0.25','0.30','0.35','0.40','0.45','0.50','0.55','0.60','0.65','0.70','0.75','0.80',$
                                              '0.85','0.90','0.95','1.00'],XLABELS=xlabels,/NORIGHT,XTITLE='length of box in native gridpoints (lon x lat)',$
           YTITLE='Fraction of box with '+field_name+' > 0'
      GSET,XMIN=0,XMAX=this_nspatial,YMIN=-0.2,YMAX=0.2
      GPLOT,X=indgen(this_nspatial)+0.5,Y=REFORM(n_precip_pts(i,0:this_nspatial-1,0)-random_n_precip_pts(i,0:this_nspatial-1,0)),COL=FSC_COLOR('black'),THICK=200
      AXES,YSTEP=0.02,YMINOR=0.01,YTITLE='Difference in probability of no precipitation in box',/ONLYRIGHT,NDECS=2
      PSCLOSE,/NOVIEW
      
      psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/timestep/hadgem3_monwg_ga50_timestep_corr_spatialagg_withrandom.IndOcn.'+$
             netcdf_name+'.model-minus-random.npts_box.'+model_names(i)+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2000,XOFFSET=1000,SPACE2=700,SPACE1=150,/PORTRAIT
      GSET,XMIN=-0.5,XMAX=this_nspatial-0.5,YMIN=0,YMAX=ymax_npts(i,p) ;YMAX=MAX(this_spatialx)*MAX(this_spatialy)
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
      LEVS,MANUAL=mylevs
      CON,X=indgen(this_nspatial),Y=indgen(MAX(this_spatialx)*MAX(this_spatialy)+1),$
          FIELD=REFORM(n_precip_pts(i,0:this_nspatial-1,0:MAX(this_spatialx)*MAX(this_spatialy))-$
                       random_n_precip_pts(i,0:this_nspatial-1,0:MAX(this_spatialx)*MAX(this_spatialy)))*100.,/BLOCK,/NOLINES,$
          CB_TITLE='Difference in probability ( '+model_names(i)+' minus random)',CB_WIDTH=120
      AXES,XVALS=indgen(this_nspatial),YSTEP=ystep_npts(i,p),YMINOR=ystep_npts(i,p)/2.,NDECS=0,XLABELS=xlabels,XTITLE='length of box in native gridpoints (lon x lat)',$
           YTITLE='Number of points in box with '+field_name+' > 0'
      PSCLOSE,/NOVIEW
   ENDFOR
   
   FOR m=0,1 DO BEGIN
      this_spatialx=spatial_x[where(hires_only lt 0)]
      this_spatialy=spatial_y[where(hires_only lt 0)]
      this_nspatial=N_ELEMENTS(this_spatialx)
      CASE m OF
         0 : BEGIN
            a=REFORM(n_precip_pts(1,*,*))
            b=REFORM(n_precip_pts(0,*,*))
            type=model_names(1)+'_model-minus-'+model_names(0)+'_model'
         END
         1 : BEGIN
            a=REFORM(random_n_precip_pts(1,*,*))
            b=REFORM(random_n_precip_pts(0,*,*))
            type=model_names(1)+'_random-minus-'+model_names(0)+'_random'
         END
      ENDCASE
      mylevs=['-13','-11','-9','-7','-5','-3','-2','-1','-0.5','0.5','1','2','3','5','7','9','11','13']
      psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/timestep/hadgem3_monwg_ga50_timestep_corr_spatialagg_withrandom.IndOcn.'+$
             netcdf_name+'.'+type+'.frac_box.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=3000,XOFFSET=500,SPACE2=700,SPACE1=150
      GSET,XMIN=-0.5,XMAX=this_nspatial-0.5,YMIN=0,YMAX=1
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
      LEVS,MANUAL=mylevs
      max_npts=MAX(this_spatialx)*MAX(this_spatialy)*2
      plot=fltarr(this_nspatial,max_npts)
      FOR j=0,this_nspatial-1 DO BEGIN
         npts=this_spatialx(j)*this_spatialy(j)
         plot(j,0)=a(j,0)-b(j,0)
         start=0
         FOR k=1,npts DO BEGIN
            stop=FLOOR(k/FLOAT(npts)*max_npts)-1
            plot(j,start:stop)=a(j,k)-b(j,k)
            start=stop+1
         ENDFOR
      ENDFOR
      CON,X=indgen(this_nspatial),Y=findgen(max_npts)/FLOAT(max_npts)+1/FLOAT(max_npts),FIELD=plot*100.,/NOLINES,/BLOCK,CB_TITLE='Difference in probability '+$
          '('+type+')'
      AXES,XVALS=indgen(this_nspatial),YVALS=['0.01','0.05','0.10','0.15','0.20','0.25','0.30','0.35','0.40','0.45','0.50','0.55','0.60','0.65','0.70','0.75','0.80',$
                                              '0.85','0.90','0.95','1.00'],XLABELS=xlabels,/NORIGHT,XTITLE='length of box in native gridpoints (lon x lat)',$
           YTITLE='Fraction of box with '+field_name+' > 0'
      GSET,XMIN=0,XMAX=this_nspatial,YMIN=-0.2,YMAX=0.2
      GPLOT,X=indgen(this_nspatial)+0.5,Y=REFORM(a(*,0)-b(*,0)),COL=FSC_COLOR('black'),THICK=200
      AXES,YSTEP=0.02,YMINOR=0.01,YTITLE='Difference in probability of no precipitation in box',/ONLYRIGHT,NDECS=2
      PSCLOSE,/NOVIEW
      
      psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/timestep/hadgem3_monwg_ga50_timestep_corr_spatialagg_withrandom.IndOcn.'+$
             netcdf_name+'.'+type+'.npts_box.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2000,XOFFSET=1000,SPACE2=700,SPACE1=150,/PORTRAIT
      GSET,XMIN=-0.5,XMAX=this_nspatial-0.5,YMIN=0,YMAX=ymax_npts(0,p) ;YMAX=MAX(this_spatialx)*MAX(this_spatialy)
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
      LEVS,MANUAL=mylevs
      CON,X=indgen(this_nspatial),Y=indgen(MAX(this_spatialx)*MAX(this_spatialy)+1),$
          FIELD=REFORM(a(0:this_nspatial-1,0:MAX(this_spatialx)*MAX(this_spatialy))-$
                       b(0:this_nspatial-1,0:MAX(this_spatialx)*MAX(this_spatialy)))*100.,/BLOCK,/NOLINES,$
          CB_TITLE='Difference in probability ('+type+')',CB_WIDTH=120
      AXES,XVALS=indgen(this_nspatial),YSTEP=ystep_npts(0,p),YMINOR=ystep_npts(0,p)/2.,NDECS=0,XLABELS=xlabels,XTITLE='length of box in native gridpoints (lon x lat)',$
           YTITLE='Number of points in box with '+field_name+' > 0'
      PSCLOSE,/NOVIEW
   ENDFOR
   
   type=model_names(1)+'_model-minus-random-minus-'+model_names(0)+'_model-minus-random'
   a=REFORM(n_precip_pts(1,*,*))
   b=REFORM(random_n_precip_pts(1,*,*))
   c=REFORM(n_precip_pts(0,*,*))
   d=REFORM(random_n_precip_pts(0,*,*))
   mylevs=['-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-1','-0.5','-0.2','0.2','0.5','1','1.5','2.5','3.5','4.5','5.5','6.5']
   psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/timestep/hadgem3_monwg_ga50_timestep_corr_spatialagg_withrandom.IndOcn.'+$
          netcdf_name+'.'+type+'.frac_box.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=3000,XOFFSET=500,SPACE2=700,SPACE1=150
   GSET,XMIN=-0.5,XMAX=this_nspatial-0.5,YMIN=0,YMAX=1
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
   LEVS,MANUAL=mylevs
   max_npts=MAX(this_spatialx)*MAX(this_spatialy)*2
   plot=fltarr(this_nspatial,max_npts)
   FOR j=0,this_nspatial-1 DO BEGIN
      npts=this_spatialx(j)*this_spatialy(j)
      plot(j,0)=(a(j,0)-b(j,0))-(c(j,0)-d(j,0))
      start=0
      FOR k=1,npts DO BEGIN
         stop=FLOOR(k/FLOAT(npts)*max_npts)-1
         plot(j,start:stop)=(a(j,k)-b(j,k))-(c(j,k)-d(j,k))
         start=stop+1
      ENDFOR
   ENDFOR
   CON,X=indgen(this_nspatial),Y=findgen(max_npts)/FLOAT(max_npts)+1/FLOAT(max_npts),FIELD=plot*100.,/NOLINES,/BLOCK,CB_TITLE='Difference in probability '+$
       '('+type+')',CB_WIDTH=120
   AXES,XVALS=indgen(this_nspatial),YVALS=['0.01','0.05','0.10','0.15','0.20','0.25','0.30','0.35','0.40','0.45','0.50','0.55','0.60','0.65','0.70','0.75','0.80',$
                                           '0.85','0.90','0.95','1.00'],XLABELS=xlabels,/NORIGHT,XTITLE='length of box in native gridpoints (lon x lat)',$
        YTITLE='Fraction of box with '+field_name+' > 0'
   GSET,XMIN=0,XMAX=this_nspatial,YMIN=-0.2,YMAX=0.2
   GPLOT,X=indgen(this_nspatial)+0.5,Y=REFORM(a(*,0)-b(*,0))-REFORM(c(*,0)-d(*,0)),COL=FSC_COLOR('black'),THICK=200
   AXES,YSTEP=0.02,YMINOR=0.01,YTITLE='Difference in probability of no precipitation in box',/ONLYRIGHT,NDECS=2
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/timestep/hadgem3_monwg_ga50_timestep_corr_spatialagg_withrandom.IndOcn.'+$
          netcdf_name+'.'+type+'.npts_box.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2000,XOFFSET=1000,SPACE2=700,SPACE1=150,/PORTRAIT
   GSET,XMIN=-0.5,XMAX=this_nspatial-0.5,YMIN=0,YMAX=ymax_npts(0,p) ;YMAX=MAX(this_spatialx)*MAX(this_spatialy)
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
   LEVS,MANUAL=mylevs
   CON,X=indgen(this_nspatial),Y=indgen(MAX(this_spatialx)*MAX(this_spatialy)+1),$
       FIELD=REFORM((a(0:this_nspatial-1,0:MAX(this_spatialx)*MAX(this_spatialy))-$
                     b(0:this_nspatial-1,0:MAX(this_spatialx)*MAX(this_spatialy)))-$
                    (c(0:this_nspatial-1,0:MAX(this_spatialx)*MAX(this_spatialy))-$
                     d(0:this_nspatial-1,0:MAX(this_spatialx)*MAX(this_spatialy))))*100.,/BLOCK,/NOLINES,$
       CB_TITLE='Difference in probability ('+type+')',CB_WIDTH=120
   AXES,XVALS=indgen(this_nspatial),YSTEP=ystep_npts(0,p),YMINOR=ystep_npts(0,p)/2.,NDECS=0,XLABELS=xlabels,XTITLE='length of box in native gridpoints (lon x lat)',$
        YTITLE='Number of points in box with '+field_name+' > 0'
   PSCLOSE,/NOVIEW
   
   
;n_area=N_ELEMENTS(equal_area)
;psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/timestep/hadgem3_monwg_ga50_timestep_corr_spatialagg.IndOcn.'+netcdf_name+'.lagone_corr.equal_area.ps'
;PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2500,XOFFSET=500
;GSET,XMIN=0,XMAX=n_area,YMIN=-0.5,YMAX=1
;FOR i=0,n_area-1 DO BEGIN
;   EBAR,X=i+0.4,BOX=lagone_corr(0,i,*),COL=FSC_COLOR(colors(0)),WIDTH=60
;   EBAR,X=i+0.6,BOX=lagone_corr(1,equal_area(i),*),COL=FSC_COLOR(colors(1)),WIDTH=60
;ENDFOR
;xlabels=strarr(n_area)
;FOR j=0,n_area-1 DO $
;   xlabels(j)=STRTRIM(STRING(this_spatialx(j)),1)+'x'+STRTRIM(STRING(this_spatialy(j)),1)
;AXES,XVALS=indgen(n_area)+0.5,XLABELS=xlabels,YSTEP=0.1,YMINOR=0.05,NDECS=1,$
;     YTITLE='Lag-1 corr of box-averaged tstep '+field_name,XTITLE='Area of box in N96 gridpoints (lon x lat)'
;GLEGEND,labels=model_names,COL=FSC_COLOR(colors),LEGPOS=11
;PSCLOSE,/NOVIEW

ENDFOR


STOP
END
