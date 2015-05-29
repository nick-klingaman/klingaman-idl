PRO hadgem3a_monwg_jjas_precip_ts_many_hg3kppres

n_sets=6
n_sets_n216=2
n_sets_n96=3
box_aavg=[10,70,30,90]
box_map=[-40,0,40,360]
;n_time=150
max_nyears=10
all_nyears=intarr(n_sets)
all_names=strarr(n_sets)
all_colors=strarr(n_sets)
all_n96_names=strarr(n_sets_n96)
all_n216_names=strarr(n_sets_n216)
precip_air=fltarr(n_sets,max_nyears)
mylevs_raw=['1','2','3','4','5','6','7','8','10','12','14','16','18','21','24','27','30']
mylevs_diff=['-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5',$
             '0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5']
n96_flag=0
n216_flag=0
FOR i=0,n_sets-1 DO BEGIN
   CASE i OF
      3 : BEGIN
         indir='/home/ss901165/um_output6/xjgxa'
         all_names(i)='ga3kpp-n216'
         all_colors(i)='red'
         years=['i4_clim','i5_clim','i6_clim','i7_clim','i8_clim','i9_clim','j0_clim','j1_clim','j2_clim','j3_clim']
         n_time=REPLICATE(150,N_ELEMENTS(years))
         maskfile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/landfrac_n216_hadgem3-7.8.nc'
         precip_filename='precip.nc'
         res='n216'
         diff_set=-1
      END
      4 : BEGIN
         indir='/home/ss901165/um_output6/xjgxa'
         all_names(i)='ga3kpp-n216_n96'
         all_colors(i)='orange'
         years=['i4_clim','i5_clim','i6_clim','i7_clim','i8_clim','i9_clim','j0_clim','j1_clim','j2_clim','j3_clim']
         n_time=REPLICATE(150,N_ELEMENTS(years))
         maskfile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/landfrac_n96_hadgem3-7.8.nc'
         precip_filename='precip_n96.nc'
         res='n96'
         diff_set=-1
      END
      5 : BEGIN
         indir='/home/ss901165/um_output6/xjgxb'
         all_names(i)='ga3kpp-n96'
         all_colors(i)='blue'
         years=['i4','i5','i6','i7','i8','i9','j0','j1','j2','j3']
         n_time=REPLICATE(150,N_ELEMENTS(years))
         maskfile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/landfrac_n96_hadgem3-7.8.nc'
         precip_filename='precip.nc'
         res='n96'
         diff_set=1
      END
      0 : BEGIN
         indir='/home/ss901165/um_output6/xjgxc'
         all_names(i)='ga3kpp-n512'
         all_colors(i)='purple'
         years=['i4','i5','i6','i7','i8','i9','j0','j1','j2','j3']
         n_time=[150,140,150,150,145,150,150,150,150,150]
         maskfile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/n512/qrparm.fracmask_n512_sn.nc'
         precip_filename='precip_xjgxc.nc'
         res='n512'
         diff_set=-1
      END
      1 : BEGIN
         indir='/home/ss901165/um_output6/xjgxc'
         all_names(i)='ga3kpp-n512_n216'
         all_colors(i)='violetred'
         years=['i4','i5','i6','i7','i8','i9','j0','j1','j2','j3']
         n_time=[150,140,150,150,145,150,150,150,150,150]
         maskfile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/landfrac_n216_hadgem3-7.8.nc'
         precip_filename='precip_n216.nc'
         res='n216'
         diff_set=1
      END
      2 : BEGIN
         indir='/home/ss901165/um_output6/xjgxc'
         all_names(i)='ga3kpp-n512_n96'
         all_colors(i)='violetred'
         years=['i4','i5','i6','i7','i8','i9','j0','j1','j2','j3']
         n_time=[150,140,150,150,145,150,150,150,150,150]
         maskfile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/landfrac_n96_hadgem3-7.8.nc'
         precip_filename='precip_n96.nc'
         res='n96'
         diff_set=1
      END         
   ENDCASE
   print,'--> '+all_names(i)
   n_years=N_ELEMENTS(years)
   all_nyears(i)=n_years
   FOR j=0,n_years-1 DO BEGIN
      print,'----> '+years(j)
      infile=indir+'/'+years(j)+'/'+precip_filename
      longitude=OPEN_AND_EXTRACT(infile,'longitude')
      latitude=OPEN_AND_EXTRACT(infile,'latitude')
      DEFINE_BOUNDARIES,box_map,latitude,longitude,box_tx,/LIMIT      
      n_lon=N_ELEMENTS(longitude)
      n_lat=N_ELEMENTS(latitude)
      DEFINE_BOUNDARIES,box_aavg,latitude,longitude,box_tx_aavg
      IF j eq 0 THEN BEGIN
         precip_clim=fltarr(n_lon,n_lat)
         precip_jjas=fltarr(n_lon,n_lat,n_years)
         IF n96_flag eq 0 and res eq 'n96' THEN BEGIN
            longitude_n96=longitude
            latitude_n96=latitude
            precip_clim_n96=fltarr(n_sets_n96,n_lon,n_lat)           
            precip_jjas_n96=fltarr(n_sets_n96,n_lon,n_lat,max_nyears)
         ENDIF
         IF n216_flag eq 0 and res eq 'n216' THEN BEGIN
            longitude_n216=longitude
            latitude_n216=latitude
            precip_clim_n216=fltarr(n_sets_n216,n_lon,n_lat)
            precip_jjas_n216=fltarr(n_sets_n216,n_lon,n_lat,max_nyears)
         ENDIF
         IF res eq 'n216' THEN $
            all_n216_names(n216_flag)=all_names(i)
         IF res eq 'n96' THEN $
            all_n96_names(n96_flag)=all_names(i)
      ENDIF
      precip_in=REFORM(OPEN_AND_EXTRACT(infile,'precip',$
                                 offset=[box_tx(1),box_tx(0),0,0],$
                                 count=[n_lon,n_lat,1,n_time(j)]))*86400.
      mask=REFORM(OPEN_AND_EXTRACT(maskfile,'lsm',$
                            offset=[box_tx(1),box_tx(0),0,0],$
                            count=[n_lon,n_lat,1,1]))
      precip_india=fltarr(box_tx_aavg(3)-box_tx_aavg(1)+1,box_tx_aavg(2)-box_tx_aavg(0)+1,n_time(j))
      FOR k=0,n_time(j)-1 DO BEGIN
         temp=REFORM(precip_in(box_tx_aavg(1):box_tx_aavg(3),box_tx_aavg(0):box_tx_aavg(2),k))        
         IF i le 2 THEN $
            temp[where(mask(box_tx_aavg(1):box_tx_aavg(3),box_tx_aavg(0):box_tx_aavg(2)) gt 0)]=$
            temp[where(mask(box_tx_aavg(1):box_tx_aavg(3),box_tx_aavg(0):box_tx_aavg(2)) gt 0)]*1.35
         IF i eq 3 or i eq 4 THEN $
            temp[where(mask(box_tx_aavg(1):box_tx_aavg(3),box_tx_aavg(0):box_tx_aavg(2)) gt 0)]=$
            temp[where(mask(box_tx_aavg(1):box_tx_aavg(3),box_tx_aavg(0):box_tx_aavg(2)) gt 0)]*1.3
         IF i eq 5 THEN $
            temp[where(mask(box_tx_aavg(1):box_tx_aavg(3),box_tx_aavg(0):box_tx_aavg(2)) gt 0)]=$
            temp[where(mask(box_tx_aavg(1):box_tx_aavg(3),box_tx_aavg(0):box_tx_aavg(2)) gt 0)]*1.2
         precip_in(box_tx_aavg(1):box_tx_aavg(3),box_tx_aavg(0):box_tx_aavg(2),k)=temp
         temp[where(mask(box_tx_aavg(1):box_tx_aavg(3),box_tx_aavg(0):box_tx_aavg(2)) eq 0)]=!Values.F_NaN
         precip_india(*,*,k)=temp
      ENDFOR
      precip_air(i,j)=MEAN(precip_india(*,*,30:n_time(j)-1),/NaN)
      
      FOR k=0,n_lon-1 DO $
         FOR m=0,n_lat-1 DO $
            precip_jjas(k,m,j)=MEAN(precip_in(k,m,30:n_time(j)-1),/NaN)      
      precip_clim=precip_clim+(precip_jjas(*,*,j)/FLOAT(n_years))      

      psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_hg3kppres.'+$
             all_names(i)+'.'+years(j)+'.jjas_mean.ps'
      PSOPEN,file=psfile,FONT=6,CHARSIZE=150,TFONT=6,MARGIN=2000,YSIZE=12000,YOFFSET=1500,SPACE2=1200
      MAP,latmin=box_map(0),latmax=box_map(2),lonmin=box_map(1),lonmax=box_map(3)
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_raw)+1,/REV,white=[2]
      LEVS,MANUAL=mylevs_raw
      CON,X=longitude,Y=latitude,FIELD=REFORM(precip_jjas(*,*,j)),CB_TITLE='Precipitation (mm day!U-1!N)',$
          CB_WIDTH=110,/NOLINES,/BLOCK,TITLE='JJAS mean precip for '+all_names(i)+' year '+years(j)
      AXES,XSTEP=30,YSTEP=10
      PSCLOSE,/NOVIEW
   ENDFOR
   IF res eq 'n96' THEN BEGIN
      precip_clim_n96(n96_flag,*,*)=precip_clim
      precip_jjas_n96(n96_flag,*,*,0:n_years-1)=precip_jjas(*,*,0:n_years-1)
      n96_flag=n96_flag+1
   ENDIF
   IF res eq 'n216' THEN BEGIN
      precip_clim_n216(n216_flag,*,*)=precip_clim
      precip_jjas_n216(n216_flag,*,*,0:n_years-1)=precip_jjas(*,*,0:n_years-1)
      n216_flag=n216_flag+1
   ENDIF

   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_hg3kppres.'+$
          all_names(i)+'.jjas_clim.ps'
   PSOPEN,file=psfile,FONT=6,CHARSIZE=150,TFONT=6,MARGIN=2000,YSIZE=12000,YOFFSET=1500,SPACE2=1200
   MAP,latmin=box_map(0),latmax=box_map(2),lonmin=box_map(1),lonmax=box_map(3)
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_raw)+1,/REV,white=[2]
   LEVS,MANUAL=mylevs_raw
   CON,X=longitude,Y=latitude,FIELD=REFORM(precip_clim(*,*)),CB_TITLE='Precipitation (mm day!U-1!N)',$
       CB_WIDTH=110,/NOLINES,/BLOCK,TITLE='JJAS clim precip for '+all_names(i)
   AXES,XSTEP=30,YSTEP=10
   PSCLOSE,/NOVIEW
ENDFOR

FOR i=0,n_sets_n96-1 DO BEGIN
   CASE i OF 
      0 : BEGIN
         expt=0
         ctrl=1
      END
      1 : BEGIN
         expt=1
         ctrl=2
      END
      2 : BEGIN
         expt=0
         ctrl=2
      END
   ENDCASE
   FOR j=0,n_years-1 DO BEGIN
      psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_hg3kppres.'+$
             all_n96_names(expt)+'-minus-'+all_n96_names(ctrl)+'.'+years(j)+'.jjas_mean.ps'
      PSOPEN,file=psfile,FONT=6,CHARSIZE=150,TFONT=6,MARGIN=2000,YSIZE=12000,YOFFSET=1500,SPACE2=1200
      MAP,latmin=box_map(0),latmax=box_map(2),lonmin=box_map(1),lonmax=box_map(3)
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV,white=[10]
      LEVS,MANUAL=mylevs_diff
      CON,X=longitude_n96,Y=latitude_n96,FIELD=REFORM(precip_jjas_n96(expt,*,*,j))-REFORM(precip_jjas_n96(ctrl,*,*,j)),$
          CB_TITLE='Precipitation (mm day!U-1!N)',$
          CB_WIDTH=110,/NOLINES,/BLOCK,TITLE='Diff JJAS precip for '+all_n96_names(expt)+' minus '+all_n96_names(ctrl)+' year '+years(j)
      AXES,XSTEP=30,YSTEP=10
      PSCLOSE,/NOVIEW
   ENDFOR
   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_hg3kppres.'+$
          all_n96_names(expt)+'-minus-'+all_n96_names(ctrl)+'.jjas_clim.ps'
   PSOPEN,file=psfile,FONT=6,CHARSIZE=150,TFONT=6,MARGIN=2000,YSIZE=12000,YOFFSET=1500,SPACE2=1200
   MAP,latmin=box_map(0),latmax=box_map(2),lonmin=box_map(1),lonmax=box_map(3)
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV,white=[10]
   LEVS,MANUAL=mylevs_diff
   CON,X=longitude_n96,Y=latitude_n96,FIELD=REFORM(precip_clim_n96(expt,*,*))-REFORM(precip_clim_n96(ctrl,*,*)),$
       CB_TITLE='Precipitation (mm day!U-1!N)',$
       CB_WIDTH=110,/NOLINES,/BLOCK,TITLE='Diff JJAS clim precip for '+all_n96_names(expt)+' minus '+all_n96_names(ctrl)
   AXES,XSTEP=30,YSTEP=10
   PSCLOSE,/NOVIEW
ENDFOR

FOR i=0,n_sets_n216-2 DO BEGIN
   FOR j=0,n_years-1 DO BEGIN
      psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_hg3kppres.'+$
             all_n216_names(i)+'-minus-'+all_n216_names(i+1)+'.'+years(j)+'.jjas_mean.ps'
      PSOPEN,file=psfile,FONT=6,CHARSIZE=150,TFONT=6,MARGIN=2000,YSIZE=12000,YOFFSET=1500,SPACE2=1200
      MAP,latmin=box_map(0),latmax=box_map(2),lonmin=box_map(1),lonmax=box_map(3)
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV,white=[10]
      LEVS,MANUAL=mylevs_diff
      CON,X=longitude_n216,Y=latitude_n216,FIELD=REFORM(precip_jjas_n216(i,*,*,j))-REFORM(precip_jjas_n216(i+1,*,*,j)),$
          CB_TITLE='Precipitation (mm day!U-1!N)',$
          CB_WIDTH=110,/NOLINES,/BLOCK,TITLE='Diff JJAS precip for '+all_n216_names(i)+' minus '+all_n216_names(i+1)+' year '+years(j)
      AXES,XSTEP=30,YSTEP=10
      PSCLOSE,/NOVIEW
   ENDFOR
   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_hg3kppres.'+$
          all_n216_names(i)+'-minus-'+all_n216_names(i+1)+'.jjas_clim.ps'
   PSOPEN,file=psfile,FONT=6,CHARSIZE=150,TFONT=6,MARGIN=2000,YSIZE=12000,YOFFSET=1500,SPACE2=1200
   MAP,latmin=box_map(0),latmax=box_map(2),lonmin=box_map(1),lonmax=box_map(3)
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV,white=[10]
   LEVS,MANUAL=mylevs_diff
   CON,X=longitude_n216,Y=latitude_n216,FIELD=REFORM(precip_clim_n216(i,*,*))-REFORM(precip_clim_n216(i+1,*,*)),$
       CB_TITLE='Precipitation (mm day!U-1!N)',$
       CB_WIDTH=110,/NOLINES,/BLOCK,TITLE='Diff JJAS clim precip for '+all_n216_names(i)+' minus '+all_n216_names(i+1)
   AXES,XSTEP=30,YSTEP=10
   PSCLOSE,/NOVIEW
ENDFOR

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_hg3kppres.'+$
       'n96_scatter.jjas_air.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=150,TFONT=6,MARGIN=2000,YOFFSET=1500,SPACE2=1200,XOFFSET=2000
GSET,XMAX=9,XMIN=3,YMIN=3,YMAX=9
CS,SCALE=26,NCOLS=max_nyears+1
FOR i=0,all_nyears(2)-1 DO $
   GPLOT,X=REFORM(precip_air(5,i)),Y=REFORM(precip_air(2,i)),COL=2+i,SYM=5,SIZE=200
FOR i=0,all_nyears(4)-1 DO $
   GPLOT,X=REFORM(precip_air(5,i)),Y=REFORM(precip_air(4,i)),COL=2+i,SYM=3,SIZE=200
GPLOT,X=7.5,Y=7.5,TEXT='N216 with N96 r='+STRMID(STRTRIM(STRING(CORRELATE(precip_air(5,0:all_nyears(4)-1),precip_air(4,0:all_nyears(4)-1))),1),0,5)
GPLOT,X=7.5,Y=7.25,TEXT='N512 with N96 r='+STRMID(STRTRIM(STRING(CORRELATE(precip_air(5,0:all_nyears(2)-1),precip_air(2,0:all_nyears(2)-1))),1),0,5)
AXES,XSTEP=0.5,YSTEP=0.5,XMINOR=0.25,YMINOR=0.25,XTITLE='JJAS AIR in N96 (mm day!U-1!N)',$
     YTITLE='JJAS AIR in N216 or N512 (interp N96; mm day!U-1!N)',NDECS=1
GLEGEND,labels=REVERSE(['i4','i5','i6','i7','i8','i9','j0','j1','j2','j3']),COL=REVERSE(indgen(max_nyears)+2),LEGPOS=1,$
        SYM=REPLICATE(3,max_nyears),LENGTH=0
GLEGEND,labels=REVERSE(['N216 vs. N96','N512 vs. N96']),SYM=REVERSE([3,5]),LEGPOS=11,LENGTH=0
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_hg3kppres.'+$
       'n96_scatter.jjas_air_anom.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=150,TFONT=6,MARGIN=2000,YOFFSET=1500,SPACE2=1200,XOFFSET=2000
GSET,XMAX=3,XMIN=-3,YMIN=-3,YMAX=3
CS,SCALE=26,NCOLS=max_nyears+1
FOR i=0,all_nyears(2)-1 DO $
   GPLOT,X=REFORM(precip_air(5,i))-MEAN(precip_air(5,0:all_nyears(2)-1)),$
         Y=REFORM(precip_air(2,i))-MEAN(precip_air(2,0:all_nyears(2)-1)),COL=2+i,SYM=5,SIZE=200
FOR i=0,all_nyears(4)-1 DO $
   GPLOT,X=REFORM(precip_air(5,i))-MEAN(precip_air(5,0:all_nyears(4)-1)),$
         Y=REFORM(precip_air(4,i))-MEAN(precip_air(4,0:all_nyears(4)-1)),COL=2+i,SYM=3,SIZE=200
GPLOT,X=[-3,3],Y=[0,0],STYLE=1
GPLOT,X=[0,0],Y=[-3,3],STYLE=1
GPLOT,X=1.5,Y=2.5,TEXT='N216 with N96 r='+STRMID(STRTRIM(STRING(CORRELATE(precip_air(5,0:all_nyears(4)-1)-MEAN(precip_air(5,0:all_nyears(4)-1)),$
                                                                          precip_air(4,0:all_nyears(4)-1)-MEAN(precip_air(4,0:all_nyears(4)-1)))),1),0,5)
GPLOT,X=1.5,Y=2.25,TEXT='N512 with N96 r='+STRMID(STRTRIM(STRING(CORRELATE(precip_air(5,0:all_nyears(2)-1)-MEAN(precip_air(5,0:all_nyears(2)-1)),$
                                                                          precip_air(2,0:all_nyears(2)-1)-MEAN(precip_air(2,0:all_nyears(2)-1)))),1),0,5)
AXES,XSTEP=0.5,YSTEP=0.5,XMINOR=0.25,YMINOR=0.25,XTITLE='JJAS AIR anomaly in N96 (mm day!U-1!N)',$
     YTITLE='JJAS AIR anomaly in N216 or N512 (interp N96; mm day!U-1!N)',NDECS=1
GLEGEND,labels=REVERSE(['i4','i5','i6','i7','i8','i9','j0','j1','j2','j3']),COL=REVERSE(indgen(max_nyears)+2),LEGPOS=1,$
        SYM=REPLICATE(3,max_nyears),LENGTH=0
GLEGEND,labels=REVERSE(['N216 vs. N96','N512 vs. N96']),SYM=REVERSE([3,5]),LEGPOS=11,LENGTH=0
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_hg3kppres.'+$
       'n96_scatter.jjas_air_diff.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=150,TFONT=6,MARGIN=2000,YOFFSET=1500,SPACE2=1200,XOFFSET=2000
GSET,XMAX=9,XMIN=3,YMIN=-5,YMAX=5
CS,SCALE=26,NCOLS=max_nyears+1
FOR i=0,all_nyears(2)-1 DO $
   GPLOT,X=REFORM(precip_air(5,i)),$
         Y=REFORM(precip_air(2,i))-precip_air(5,i),COL=2+i,SYM=5,SIZE=200
FOR i=0,all_nyears(4)-1 DO $
   GPLOT,X=REFORM(precip_air(5,i)),$
         Y=REFORM(precip_air(4,i))-precip_air(5,i),COL=2+i,SYM=3,SIZE=200
GPLOT,X=7.5,Y=4.5,TEXT='N216 with N96 r='+STRMID(STRTRIM(STRING(CORRELATE(precip_air(5,0:all_nyears(4)-1),$
                                                                          precip_air(4,0:all_nyears(4)-1)-precip_air(5,0:all_nyears(4)-1))),1),0,5)
GPLOT,X=7.5,Y=4.0,TEXT='N512 with N96 r='+STRMID(STRTRIM(STRING(CORRELATE(precip_air(5,0:all_nyears(2)-1),$
                                                                          precip_air(2,0:all_nyears(2)-1)-precip_air(5,0:all_nyears(2)-1))),1),0,5)
GPLOT,X=[3,9],Y=[0,0],STYLE=1
;GPLOT,X=[0,0],Y=[-3,3],STYLE=1
AXES,XSTEP=0.5,YSTEP=0.5,XMINOR=0.25,YMINOR=0.25,XTITLE='JJAS AIR in N96 (mm day!U-1!N)',$
     YTITLE='Diff in JJAS AIR [N216 or N512] minus [N96] (mm day!U-1!N)',NDECS=1
GLEGEND,labels=REVERSE(['i4','i5','i6','i7','i8','i9','j0','j1','j2','j3']),COL=REVERSE(indgen(max_nyears)+2),LEGPOS=1,$
        SYM=REPLICATE(3,max_nyears),LENGTH=0
GLEGEND,labels=REVERSE(['N216 vs. N96','N512 vs. N96']),SYM=REVERSE([3,5]),LEGPOS=11,LENGTH=0
PSCLOSE

STOP
END
