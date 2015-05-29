PRO hadgem3kpp_hovmollers_olr_unfiltered_n512

n_sets=3
latavg_box=[-5,40,5,180]
lonavg_box=[-10,70,40,88]

mylevs_olr=[120,150,180,210,240,270,300]
mylevs_precip=[3,4,5,7,9,11,13,15,18,21,24,28,32,36,40]

all_names=strarr(n_sets)
FOR i=0,n_sets-1 DO BEGIN
   CASE i OF
      0: BEGIN
         indir='/home/ss901165/um_output6/xjgxc'
         all_names(i)='ga3kpp-n512'
         years=['i4','i5','i6','i7','i8','i9','j0','j1','j2','j3']     
         precip_filename='precip.nc'
         olr_filename='olr.nc'
         n_days=[180,180,150,180,155,180,180,180,180,175]
      END
      1 : BEGIN
         indir='/home/ss901165/um_output6/xjgxa'
         all_names(i)='ga3kpp-n216'
         years=['i4_clim','i5_clim','i6_clim','i7_clim','i8_clim','i9_clim','j0_clim','j1_clim','j2_clim','j3_clim']
         precip_filename='precip.nc'
         olr_filename='olr.nc'
         n_days=[REPLICATE(180,N_ELEMENTS(years)-1),165]
      END
      2: BEGIN
         indir='/home/ss901165/um_output6/xjgxb'
         all_names(i)='ga3kpp-n96'
         years=['i4','i5','i6','i7','i8','i9','j0','j1','j2','j3']
         precip_filename='precip.nc'
         olr_filename='olr.nc'
         n_days=REPLICATE(180,N_ELEMENTS(years))
      END
   ENDCASE
   print,'--> '+all_names(i)
   n_years=N_ELEMENTS(years)
   FOR j=0,n_years-1 DO BEGIN   
      print,'-----> '+years(j)
      olr_infile=indir+'/'+years(j)+'/'+olr_filename
      precip_infile=indir+'/'+years(j)+'/'+precip_filename

      longitude=OPEN_AND_EXTRACT(precip_infile,'longitude')         
      latitude=OPEN_AND_EXTRACT(precip_infile,'latitude')
      DEFINE_BOUNDARIES,latavg_box,latitude,longitude,latavg_box_tx,/LIMIT
      n_lon=N_ELEMENTS(longitude)
      n_lat=N_ELEMENTS(latitude)
   
      precip=REFORM(OPEN_AND_EXTRACT(precip_infile,'precip',offset=[latavg_box_tx(1),latavg_box_tx(0),0,0],$
                                     count=[n_lon,n_lat,1,n_days(j)]))*86400.
;      olr=REFORM(OPEN_AND_EXTRACT(olr_infile,'olr',offset=[latavg_box_tx(1),latavg_box_tx(0),0,0],$
;                                  count=[n_lon,n_lat,1,n_days(j)]))
      
      latavg_olr=fltarr(n_lon,180)
      latavg_precip=fltarr(n_lon,180)
      FOR k=0,n_lon-1 DO BEGIN
         FOR m=0,n_days(j)-1 DO BEGIN
;            latavg_olr(k,m)=MEAN(olr(k,*,m))
            latavg_precip(k,m)=MEAN(precip(k,*,m))
         ENDFOR
      ENDFOR
      IF n_days(j) lt 180 THEN BEGIN
         latavg_precip(*,n_days(j):179)=!Values.F_NaN
;         latavg_olr(*,n_days(j):179)=!Values.F_NaN
      ENDIF
;      FOR m=0,n_days(j)-1 DO $
;         latavg_olr(*,m)=SMOOTH(latavg_olr(*,m),5)
      
      ylabels=['1/5','16/5','1/6','16/6','1/7','16/7','1/8','16/8','1/9','16/9','1/10','16/10','1/11']
      
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/hovmollers/olr/hadgem3kpp_hovmollers_olr_unfiltered.'+all_names(i)+'_'+years(j)+'_latavg.ps'
      PSOPEN,file=psfile,FONT=6,CHARSIZE=150,MARGIN=1800,SPACE2=1000,XOFFSET=1500,YOFFSET=2000,TFONT=6,TCHARSIZE=120,SPACE3=50,/PORTRAIT
      CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_precip)+3,white=[2]
      GSET,XMIN=latavg_box(1),XMAX=latavg_box(3),YMIN=0,YMAX=180,TITLE=all_names(i)+' '+years(j)+', 5S-5N prcp'
      LEVS,MANUAL=[1,2,mylevs_precip]
      CON,X=longitude,Y=indgen(180)+0.5,FIELD=latavg_precip,/NOLINES,CB_WIDTH=115,CB_TITLE='Precipitation (mm day!U-1!N)'
      ;LEVS,MANUAL=mylevs_olr
      ;CON,X=longitude,Y=indgen(n_days(j))+0.5,FIELD=latavg_olr,/NOFILL,/NOLINELABELS,/NOCOLBAR,THICK=100
      AXES,XSTEP=15,$
           XTITLE='Longitude (degrees east)',YTITLE='Date',YVALS=indgen(180/15+1)*15,YMINOR=5,$
           YLABELS=ylabels
      PSCLOSE,/NOVIEW
 
      longitude=OPEN_AND_EXTRACT(precip_infile,'longitude')         
      latitude=OPEN_AND_EXTRACT(precip_infile,'latitude')
      DEFINE_BOUNDARIES,lonavg_box,latitude,longitude,lonavg_box_tx,/LIMIT
      n_lon=N_ELEMENTS(longitude)
      n_lat=N_ELEMENTS(latitude)
      
      precip=REFORM(OPEN_AND_EXTRACT(precip_infile,'precip',offset=[lonavg_box_tx(1),lonavg_box_tx(0),0,0],$
                                     count=[n_lon,n_lat,1,n_days(j)]))*86400.
;      olr=REFORM(OPEN_AND_EXTRACT(olr_infile,'olr',offset=[lonavg_box_tx(1),lonavg_box_tx(0),0,0],$
;                                  count=[n_lon,n_lat,1,n_days(j)]))
      
      lonavg_olr=fltarr(180,n_lat)
      lonavg_precip=fltarr(180,n_lat)
      FOR k=0,n_lat-1 DO BEGIN
         FOR m=0,n_days(j)-1 DO BEGIN
;            lonavg_olr(m,k)=MEAN(olr(*,k,m))
            lonavg_precip(m,k)=MEAN(precip(*,k,m))
         ENDFOR
      ENDFOR
      IF n_days(j) lt 180 THEN BEGIN
         lonavg_precip(n_days(j):179,*)=!Values.F_NaN
;         lonavg_olr(n_days(j):179,*)=!Values.F_NaN
      ENDIF
;      FOR m=0,n_days(j)-1 DO $
;         lonavg_olr(m,*)=SMOOTH(lonavg_olr(m,*),5)
      
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/hovmollers/olr/hadgem3kpp_hovmollers_olr_unfiltered.'+all_names(i)+'_'+years(j)+'_lonavg.ps'
      PSOPEN,file=psfile,FONT=6,CHARSIZE=150,MARGIN=1800,SPACE2=1000,XOFFSET=1000,YOFFSET=2000,TFONT=6,TCHARSIZE=120,SPACE3=50
      CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_precip)+3,white=[2]
      GSET,YMIN=lonavg_box(0),YMAX=lonavg_box(2),XMIN=0,XMAX=180,TITLE=all_names(i)+' '+years(j)+' 70-90E prcp'
      LEVS,MANUAL=[1,2,mylevs_precip]
      CON,Y=latitude,X=indgen(180)+0.5,FIELD=lonavg_precip,/NOLINES,CB_WIDTH=110,CB_TITLE='Precipitation (mm day!U-1!N)'
      ;LEVS,MANUAL=mylevs_olr
      ;CON,Y=latitude,X=indgen(n_days(j))+0.5,FIELD=lonavg_olr,/NOFILL,/NOLINELABELS,/NOCOLBAR,THICK=100
      AXES,YSTEP=5,XMINOR=5,$
           YTITLE='Latitude (degrees north)',XTITLE='Date',XVALS=indgen(180/15+1)*15,$
           XLABELS=ylabels
      PSCLOSE,/NOVIEW
   ENDFOR
ENDFOR

STOP

END
