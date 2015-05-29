PRO hadgem3_monwg_aquaplanet_timestep_corr_spatialagg

box=[-10,0,10,360]

n_runs=6
model_names=strarr(n_runs)
colors=strarr(n_runs)
styles=intarr(n_runs)
spatial_x=[1,2,4,8,11,22,33,44,88]
spatial_y=[1,1,1,1,1,2,3,4,8]
n_spatial=N_ELEMENTS(spatial_x)
lagone_corr=fltarr(n_runs,n_spatial,5)
n_precip_pts=fltarr(n_runs,n_spatial,5)
FOR i=0,n_runs-1 DO BEGIN
   print,'---> '+STRTRIM(STRING(i),1)
   CASE i OF
      0 : BEGIN
         infile='/home/ss901165/um_output5/xhccr/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_ctlent.jan-dec_dmeans.years1-3.tot_precip.nc'
         model_names(i)='fixnhsol5N_sin2sin4_ctlent'
         colors(i)='black'
         styles(i)=0
      END
      1 : BEGIN
         infile='/home/ss901165/um_output5/xhccs/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_1.5xent.jan-dec_dmeans.years1-3.tot_precip.nc'
         model_names(i)='fixnhsol5N_sin2sin4_1.5xent'
         colors(i)='red'
         styles(i)=0
      END
      2 : BEGIN
         infile='/home/ss901165/um_output5/xhcct/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_2.5xent.jan-dec_dmeans.years1-3.tot_precip.nc'
         model_names(i)='fixnhsol5N_sin2sin4_2.5xent'
         colors(i)='orange'
         styles(i)=0
      END
      3 : BEGIN
         infile='/home/ss901165/um_output5/xhccw/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_2xcape.jan-dec_dmeans.years1-3.tot_precip.nc'
         model_names(i)='fixnhsol5N_sin2sin4_2xcape'
         colors(i)='blue'
         styles(i)=0
      END
      4 : BEGIN
         infile='/home/ss901165/um_output5/xhccx/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_0.5xcape.jan-dec_dmeans.years1-3.tot_precip.nc'
         model_names(i)='fixnhsol5N_sin2sin4_0.5xcape'
         colors(i)='brown'
         styles(i)=0
      END
      5 : BEGIN         
         infile='/home/ss901165/um_output5/xhccy/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_1.5xent_2xcape.jan-dec_dmeans.years1-3.tot_precip.nc'
         model_names(i)='fixnhsol5N_sin2sin4_1.5xent_2xcape'
         colors(i)='purple'
         styles(i)=0
      END
   ENDCASE
   
   longitude=OPEN_AND_EXTRACT(infile,'longitude')
   latitude=OPEN_AND_EXTRACT(infile,'latitude')
   DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
   n_lon=N_ELEMENTS(longitude)
   n_lat=N_ELEMENTS(latitude)
   n_time=N_ELEMENTS(OPEN_AND_EXTRACT(infile,'t'))
   ;n_time=10000

   precip=fltarr(n_lon*2,n_lat,n_time)
   precip(n_lon/2:n_lon/2+n_lon-1,*,*)=OPEN_AND_EXTRACT(infile,'tot_precip',$
                                          offset=[box_tx(1),box_tx(0),0],$
                                          count=[n_lon,n_lat,n_time])   
   precip(0:n_lon/2-1,*,*)=precip(n_lon:n_lon/2+n_lon-1,*,*)
   precip(n_lon/2+n_lon:n_lon*2-1,*,*)=precip(n_lon/2:n_lon-1,*,*)
   
   this_nprecip_pts=fltarr(n_time)
   FOR j=0,n_spatial-1 DO BEGIN  
      print,'-------> '+STRTRIM(STRING(j),1)
      this_nlon=n_lon/spatial_x(j)
      this_nlat=n_lat/spatial_y(j)
      this_lagone_corr=fltarr(this_nlat*this_nlon)
      n_valid_pts=0
      FOR k=0,this_nlon-1 DO BEGIN
         FOR m=0,this_nlat-1 DO BEGIN
            x_low=k*spatial_x(j)-spatial_x(j)/2+n_lon/2
            x_high=k*spatial_x(j)+spatial_x(j)/2-1+n_lon/2
            y_low=m*spatial_y(j)
            y_high=(m+1)*spatial_y(j)-1
            IF j gt 0 and y_high le n_lat-1 and y_low ge 0 THEN BEGIN
               precip_boxavg=fltarr(n_time)
               FOR n=LONG(0),LONG(n_time-1) DO BEGIN                  
                  precip_box=precip(x_low:x_high,y_low:y_high,n)
                  precip_boxavg(n)=MEAN(precip_box)
                  IF TOTAL(where(precip_box gt 0.1)) ge 0 THEN $
                     this_nprecip_pts(n)=FLOAT(N_ELEMENTS(where(precip_box gt 0.01)))/FLOAT(N_ELEMENTS(precip_box))+this_nprecip_pts(n)
               ENDFOR
               this_lagone_corr(k*this_nlat+m)=A_CORRELATE(precip_boxavg,1)                
               n_valid_pts=n_valid_pts+1
            ENDIF ELSE IF j gt 0 THEN BEGIN
               this_lagone_corr(k*this_nlat+m)=!Values.F_NaN
            ENDIF ELSE IF j eq 0 THEN $
               this_lagone_corr(k*this_nlat+m)=A_CORRELATE(precip(k,m,*),1)       
         ENDFOR
      ENDFOR
      IF j eq 0 THEN BEGIN
         FOR n=LONG(0),LONG(n_time-1) DO BEGIN
            this_nprecip_pts(n)=N_ELEMENTS(where(precip(n_lon/2:n_lon*3/2-1,*,n) gt 0.01))/FLOAT(n_lon*n_lat)
            IF this_nprecip_pts(n) gt 1 THEN STOP
         ENDFOR
      ENDIF ELSE $
         this_nprecip_pts(*)=this_nprecip_pts(*)/FLOAT(n_valid_pts)

      sorted=SORT(this_lagone_corr)
      n_finite=N_ELEMENTS(where(FINITE(this_lagone_corr) eq 1))
      lagone_corr(i,j,2)=MEDIAN(this_lagone_corr)
      lagone_corr(i,j,0)=this_lagone_corr(sorted(n_finite*10/100));MIN(this_lagone_corr,/NaN)
      lagone_corr(i,j,4)=this_lagone_corr(sorted(n_finite*90/100));MAX(this_lagone_corr,/NaN)
      lagone_corr(i,j,1)=this_lagone_corr(sorted(n_finite/3))
      lagone_corr(i,j,3)=this_lagone_corr(sorted(n_finite*2/3))

      sorted=SORT(this_nprecip_pts)
      n_precip_pts(i,j,2)=MEDIAN(this_nprecip_pts)
      n_precip_pts(i,j,0)=MIN(this_nprecip_pts)
      n_precip_pts(i,j,4)=MAX(this_nprecip_pts)
      n_precip_pts(i,j,1)=this_nprecip_pts(sorted(N_ELEMENTS(this_nprecip_pts)/4))
      n_precip_pts(i,j,3)=this_nprecip_pts(sorted(N_ELEMENTS(this_nprecip_pts)*3/4))
   ENDFOR
ENDFOR

psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/timestep/hadgem3_monwg_aquaplanet_timestep_corr_spatialagg.lagone_corr.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2500,XOFFSET=500
GSET,XMIN=0,XMAX=n_spatial,YMIN=-0.5,YMAX=1
FOR i=0,n_runs-1 DO $
   FOR j=0,n_spatial-1 DO $
      EBAR,X=j+0.25+0.1*i,BOX=lagone_corr(i,j,*),COL=FSC_COLOR(colors(i)),WIDTH=50
xlabels=strarr(n_spatial)
FOR j=0,n_spatial-1 DO $
   xlabels(j)=STRTRIM(STRING(spatial_x(j)),1)+'x'+STRTRIM(STRING(spatial_y(j)),1)
AXES,XVALS=indgen(n_spatial)+0.5,XLABELS=xlabels,YSTEP=0.1,YMINOR=0.05,NDECS=1,$
     YTITLE='Lag-1 corr of box-averaged tstep tot_precip',XTITLE='Size of box (lon x lat gridpoints)'
GLEGEND,labels=model_names,COL=FSC_COLOR(colors),LEGPOS=11
PSCLOSE

psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/timestep/hadgem3_monwg_aquaplanet_timestep_corr_spatialagg.nprecip_pts.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2500,XOFFSET=500
GSET,XMIN=0,XMAX=n_spatial,YMIN=0,YMAX=1
FOR i=0,n_runs-1 DO $
   FOR j=0,n_spatial-1 DO $
      EBAR,X=j+0.25+0.1*i,BOX=n_precip_pts(i,j,*),COL=FSC_COLOR(colors(i)),WIDTH=50
xlabels=strarr(n_spatial)
FOR j=0,n_spatial-1 DO $
   xlabels(j)=STRTRIM(STRING(spatial_x(j)),1)+'x'+STRTRIM(STRING(spatial_y(j)),1)
AXES,XVALS=indgen(n_spatial)+0.5,XLABELS=xlabels,YSTEP=0.1,YMINOR=0.05,NDECS=1,$
     YTITLE='Fraction of points with precipitation (> 0.01 mm/day)',XTITLE='Size of box (lon x lat gridpoints)'
;GLEGEND,labels=model_names,COL=FSC_COLOR(colors),LEGPOS=11
PSCLOSE

STOP
END
