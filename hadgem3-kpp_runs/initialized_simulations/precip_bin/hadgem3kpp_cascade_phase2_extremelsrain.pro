PRO hadgem3kpp_cascade_phase2_extremelsrain
  
; Pick extreme lsrain values out of phase 2 case studies, using hourly data.  
; Plot fields for surrounding gridpoints.

n_runs=3
um3='/home/ss901165/um_output3'
n_times=24
box=[-5,40,5,200]

FOR i=0,n_runs-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         lsrain_infile=um3+'/xfadh/xfadha.06apr09-05may09.lsrain.nc'
      END
      1 : BEGIN
         lsrain_infile=um3+'/xfadk/xfadka.06apr09-05may09.lsrain.nc'
      END
      2 : BEGIN
         lsrain_infile=um3+'/xfadm/xfadma.06apr09-05may09.lsrain.nc'
      END
   ENDCASE

   lsrain_longitude=OPEN_AND_EXTRACT(lsrain_infile,'longitude')
   lsrain_latitude=OPEN_AND_EXTRACT(lsrain_infile,'latitude_1')
   DEFINE_BOUNDARIES,box,lsrain_latitude,lsrain_longitude,lsrain_box_tx,/LIMIT
   lsrain_nlon=N_ELEMENTS(lsrain_longitude)
   lsrain_nlat=N_ELEMENTS(lsrain_latitude)

   lsrain=REFORM(OPEN_AND_EXTRACT(lsrain_infile,'lsrain',$
                                  offset=[lsrain_box_tx(1),lsrain_box_tx(0),0,0],$
                                  count=[lsrain_nlon,lsrain_nlat,1,n_times]))*3.
   lsrain_sorted=SORT(lsrain)
   extreme_lsrain_pts=lsrain_sorted(N_ELEMENTS(lsrain_sorted)-10:N_ELEMENTS(lsrain_sorted)-1)
   extreme_lsrain_longitude_pts=fltarr(10)
   extreme_lsrain_latitude_pts=fltarr(10)

   threed_lsrain_coord=fltarr(lsrain_nlon,lsrain_nlat,n_times)
   FOR j=0,lsrain_nlon-1 DO $
      FOR k=0,lsrain_nlat-1 DO $
         FOR m=0,n_times-1 DO $
            threed_lsrain_coord(j,k,m)=lsrain_longitude(j)
   FOR j=0,9 DO $
      extreme_lsrain_longitude_pts(j)=where(lsrain_longitude eq threed_lsrain_coord(extreme_lsrain_pts(j)))
   FOR j=0,lsrain_nlon-1 DO $
      FOR k=0,lsrain_nlat-1 DO $
         FOR m=0,n_times-1 DO $
            threed_lsrain_coord(j,k,m)=lsrain_latitude(k)
   FOR j=0,9 DO $
      extreme_lsrain_latitude_pts(j)=where(lsrain_latitude eq threed_lsrain_coord(extreme_lsrain_pts(j)))
   
   
ENDFOR


STOP
END
