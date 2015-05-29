PRO hadgem3kpp_seascycle_precip_xsec

um6='/home/ss901165/um_output6'
trmm_dmean_file='/home/ss901165/datasets/TRMM_3B42V6/n96/TRMM_3B42v6A.jan-dec_dmeans.1999-2009.n96.nc'
trmm_clim_file='/home/ss901165/datasets/TRMM_3B42V6/n96/TRMM_3B42v6A.jan-dec_dmean_clim.1999-2010.n96.nc'
gpcp_dmean_file='/home/ss901165/datasets/GPCP/one_degree/n96/gpcp_1dd_v1.1.jan-dec_dmeans.1997-2008.n96.nc'
gpcp_clim_file='/home/ss901165/datasets/GPCP/one_degree/n96/gpcp_1dd_v1.1.jan-dec_dmean_clim.1997-2008.n96.nc'
kpp50_dmean_file=um6+'/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans.years1-60.precip.nc'
kpp50_clim_file=um6+'/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-60.precip.nc'
kpp50_nyears=60
a50cl_dmean_file=um6+'/xihvg/hadgem3a_kpp50N50S_1.5xentrain_ga30.jan-dec_dmeans.years1-29.precip.nc'
a50cl_clim_file=um6+'/xihvg/hadgem3a_kpp50N50S_1.5xentrain_ga30.jan-dec_dmean_clim.years1-29.precip.nc'
a50_nyears=29
a5031_dmean_file=um6+'/xihvx/hadgem3a_kpp50N50Ssmooth31_1.5xentrain_ga30.jan-dec_dmeans.years1-29.precip.nc'
a5031_clim_file=um6+'/xihvx/hadgem3a_kpp50N50Ssmooth31_1.5xentrain_ga30.jan-dec_dmean_clim.years1-29.precip.nc'
a5015_dmean_file=um6+'/xihvy/hadgem3a_kpp50N50Ssmooth15_1.5xentrain_ga30.jan-dec_dmeans.years1-29.precip.nc'
a5015_clim_file=um6+'/xihvy/hadgem3a_kpp50N50Ssmooth15_1.5xentrain_ga30.jan-dec_dmean_clim.years1-29.precip.nc'
kpp30_dmean_file=um6+'/xihvm/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmeans.years1-25.precip.nc'
kpp30_clim_file=um6+'/xihvm/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-25.precip.nc'
kpp30_nyears=25

n_box=4
FOR i=0,n_box-1 DO BEGIN
   CASE i OF
      1 : BEGIN
         box=[-30,140,30,200]
         plot_box=[-18,22]
         box_name='WPac'
         mylevs=['1','2','3','4','5','6','8','10','12','14','16','18','20']
         mylevs_diff=['-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5',$         
                      '0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5']
         white_diff=[10]
         wrap=0
      END
      0 : BEGIN
         box=[-20,40,30,100]
         plot_box=[-15,25]
         box_name='IndOcn'
         mylevs=['1','2','3','4','5','6','7','8','9','10','11','12']
         mylevs_diff=['-5.2','-4.4','-3.6','-2.8','-2.0','-1.2','-0.4',$
                      '0.4','1.2','2.0','2.8','3.6','4.4','5.2']
         white_diff=[9]
         wrap=0
      END
      2 : BEGIN
         box=[-20,330,20,30]
         plot_box=[-16,12]
         box_name='Africa'
         mylevs=['1','2','3','4','5','6','7','8','9','10','11','12']
         mylevs_diff=['-3.4','-3.0','-2.6','-2.2','-1.8','-1.4','-1.0','-0.6','-0.2',$
                      '0.2','0.6','1.0','1.4','1.8','2.2','2.6','3.0','3.4']
         white_diff=[11]
         wrap=1
      END
      3 : BEGIN
         box=[-25,100,0,160]
         plot_box=[-25,10]
         box_name='Australia'
         mylevs=['1','2','3','4','5','6','7','8','9','10','11','12']
         mylevs_diff=['-3.4','-3.0','-2.6','-2.2','-1.8','-1.4','-1.0','-0.6','-0.2',$
                      '0.2','0.6','1.0','1.4','1.8','2.2','2.6','3.0','3.4']
         white_diff=[11]
         wrap=0
      END
   ENDCASE
   
   longitude=OPEN_AND_EXTRACT(kpp50_dmean_file,'longitude')
   latitude=OPEN_AND_EXTRACT(kpp50_dmean_file,'latitude')
   DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
   n_lon=N_ELEMENTS(longitude)
   n_lat=N_ELEMENTS(latitude)

   IF wrap eq 1 THEN BEGIN
      kpp50_clim=OPEN_AND_EXTRACT(kpp50_clim_file,'precip',offset=[box_tx(1),box_tx(0),0],$
                                  count=[n_lon,n_lat,360],/WRAP)*86400.
      kpp30_clim=OPEN_AND_EXTRACT(kpp30_clim_file,'precip',offset=[box_tx(1),box_tx(0),0],$
                                  count=[n_lon,n_lat,360],/WRAP)*86400.
      a50cl_clim=OPEN_AND_EXTRACT(a50cl_clim_file,'precip',offset=[box_tx(1),box_tx(0),0],$
                                  count=[n_lon,n_lat,360],/WRAP)*86400.
      a5015_clim=OPEN_AND_EXTRACT(a5015_clim_file,'precip',offset=[box_tx(1),box_tx(0),0],$
                                  count=[n_lon,n_lat,360],/WRAP)*86400.
      kpp50_dmean=OPEN_AND_EXTRACT(kpp50_dmean_file,'precip',offset=[box_tx(1),box_tx(0),0,0],$
                                  count=[n_lon,n_lat,360,kpp50_nyears],/WRAP)*86400.
      kpp30_dmean=OPEN_AND_EXTRACT(kpp30_dmean_file,'precip',offset=[box_tx(1),box_tx(0),0,0],$
                                   count=[n_lon,n_lat,360,kpp30_nyears],/WRAP)*86400.
      a50cl_dmean=OPEN_AND_EXTRACT(a50cl_dmean_file,'precip',offset=[box_tx(1),box_tx(0),0,0],$
                                  count=[n_lon,n_lat,360,a50_nyears],/WRAP)*86400.
      a5015_dmean=OPEN_AND_EXTRACT(a5015_dmean_file,'precip',offset=[box_tx(1),box_tx(0),0,0],$
                                  count=[n_lon,n_lat,360,a50_nyears],/WRAP)*86400.
   ENDIF ELSE BEGIN      
      kpp50_clim=OPEN_AND_EXTRACT(kpp50_clim_file,'precip',offset=[box_tx(1),box_tx(0),0],$
                                  count=[n_lon,n_lat,360])*86400.
      kpp30_clim=OPEN_AND_EXTRACT(kpp30_clim_file,'precip',offset=[box_tx(1),box_tx(0),0],$
                                  count=[n_lon,n_lat,360])*86400.
      a50cl_clim=OPEN_AND_EXTRACT(a50cl_clim_file,'precip',offset=[box_tx(1),box_tx(0),0],$
                                  count=[n_lon,n_lat,360])*86400.
      a5015_clim=OPEN_AND_EXTRACT(a5015_clim_file,'precip',offset=[box_tx(1),box_tx(0),0],$
                                  count=[n_lon,n_lat,360])*86400.
      kpp50_dmean=OPEN_AND_EXTRACT(kpp50_dmean_file,'precip',offset=[box_tx(1),box_tx(0),0,0],$
                                  count=[n_lon,n_lat,360,kpp50_nyears])*86400.
      kpp30_dmean=OPEN_AND_EXTRACT(kpp30_dmean_file,'precip',offset=[box_tx(1),box_tx(0),0,0],$
                                   count=[n_lon,n_lat,360,kpp30_nyears])*86400.
      a50cl_dmean=OPEN_AND_EXTRACT(a50cl_dmean_file,'precip',offset=[box_tx(1),box_tx(0),0,0],$
                                  count=[n_lon,n_lat,360,a50_nyears])*86400.
      a5015_dmean=OPEN_AND_EXTRACT(a5015_dmean_file,'precip',offset=[box_tx(1),box_tx(0),0,0],$
                                   count=[n_lon,n_lat,360,a50_nyears])*86400.
   ENDELSE

   trmm_longitude=OPEN_AND_EXTRACT(trmm_clim_file,'longitude')
   trmm_latitude=OPEN_AND_EXTRACT(trmm_clim_file,'latitude')
   DEFINE_BOUNDARIES,box,trmm_latitude,trmm_longitude,trmm_box_tx,/LIMIT
   trmm_nlon=N_ELEMENTS(trmm_longitude)
   trmm_nlat=N_ELEMENTS(trmm_latitude)
   IF wrap eq 1 THEN BEGIN
      temp_clim=OPEN_AND_EXTRACT(trmm_clim_file,'precip',offset=[trmm_box_tx(1),trmm_box_tx(0),0],$
                                 count=[trmm_nlon,n_lat,360],/WRAP)
      temp_dmean=OPEN_AND_EXTRACT(trmm_dmean_file,'precip',offset=[trmm_box_tx(1),trmm_box_tx(0),0,0],$
                                  count=[trmm_nlon,trmm_nlat,360,11],/WRAP)
      gpcp_clim=OPEN_AND_EXTRACT(gpcp_clim_file,'precip',offset=[box_tx(1),box_tx(0),0],$
                                 count=[n_lon,n_lat,360],/WRAP)
      gpcp_dmean=OPEN_AND_EXTRACT(gpcp_dmean_file,'precip',offset=[box_tx(1),box_tx(0),0,0],$
                                  count=[n_lon,n_lat,360,12],/WRAP)
   ENDIF ELSE BEGIN
      temp_clim=OPEN_AND_EXTRACT(trmm_clim_file,'precip',offset=[trmm_box_tx(1),trmm_box_tx(0),0],$
                                 count=[trmm_nlon,trmm_nlat,360])
      temp_dmean=OPEN_AND_EXTRACT(trmm_dmean_file,'precip',offset=[trmm_box_tx(1),trmm_box_tx(0),0,0],$
                                  count=[trmm_nlon,trmm_nlat,360,11])
      gpcp_clim=OPEN_AND_EXTRACT(gpcp_clim_file,'precip',offset=[box_tx(1),box_tx(0),0],$
                                 count=[n_lon,n_lat,360])
      gpcp_dmean=OPEN_AND_EXTRACT(gpcp_dmean_file,'precip',offset=[box_tx(1),box_tx(0),0,0],$
                                  count=[n_lon,n_lat,360,12])
   ENDELSE
   temp_dmean[where(temp_dmean ge 1000)]=!Values.F_NaN
   temp_clim[where(temp_clim ge 1000)]=!Values.F_NaN
   trmm_clim=fltarr(trmm_nlon,trmm_nlat,360)
   trmm_dmean=fltarr(trmm_nlon,trmm_nlat,360,11)
   FOR k=0,trmm_nlat-1 DO BEGIN
      trmm_clim(*,k,*)=temp_clim(*,trmm_nlat-k-1,*)
      trmm_dmean(*,k,*,*)=temp_dmean(*,trmm_nlat-k-1,*,*)
   ENDFOR

   kpp50_lonavg=fltarr(360,n_lat)
   kpp30_lonavg=fltarr(360,n_lat)
   a50cl_lonavg=fltarr(360,n_lat)
   a5015_lonavg=fltarr(360,n_lat)
   trmm_lonavg=fltarr(360,n_lat)
   FOR k=0,n_lat-1 DO BEGIN
      FOR m=0,359 DO BEGIN
         kpp50_lonavg(m,k)=MEAN(kpp50_clim(*,k,m))
         kpp30_lonavg(m,k)=MEAN(kpp30_clim(*,k,m))
         a50cl_lonavg(m,k)=MEAN(a50cl_clim(*,k,m))
         a5015_lonavg(m,k)=MEAN(a5015_clim(*,k,m))
         trmm_lonavg(m,k)=MEAN(trmm_clim(*,k,m))
      ENDFOR
      kpp50_lonavg(*,k)=SMOOTH(kpp50_lonavg(*,k),11,/EDGE_WRAP)
      kpp30_lonavg(*,k)=SMOOTH(kpp50_lonavg(*,k),11,/EDGE_WRAP)
      a50cl_lonavg(*,k)=SMOOTH(a50cl_lonavg(*,k),11,/EDGE_WRAP)
      a5015_lonavg(*,k)=SMOOTH(a5015_lonavg(*,k),11,/EDGE_WRAP)
      trmm_lonavg(*,k)=SMOOTH(trmm_lonavg(*,k),11,/EDGE_WRAP)
   ENDFOR

   FOR j=0,5 DO BEGIN
      CASE j OF 
         0 : BEGIN
            in=kpp50_dmean
            n_years=kpp50_nyears
            lat=latitude
         END
         1 : BEGIN
            in=a50cl_dmean
            n_years=a50_nyears
            lat=latitude
         END
         2 : BEGIN
            in=a5015_dmean
            n_years=a50_nyears
            lat=latitude
         END
         3 : BEGIN
            in=kpp30_dmean
            n_years=kpp30_nyears
            lat=latitude
         END
         4 : BEGIN
            in=trmm_dmean
            n_years=11
            lat=latitude
         END
         5 : BEGIN
            in=gpcp_dmean
            n_years=12
            lat=latitude
         END
      ENDCASE
      out=fltarr(n_years,360)
      out_box=fltarr(360,6)
      FOR k=0,n_years-1 DO BEGIN
         FOR m=0,359 DO BEGIN
            temp_lonavg=fltarr(N_ELEMENTS(lat))
            FOR n=0,N_ELEMENTS(lat)-1 DO $
               temp_lonavg(n)=MEAN(in(*,n,m,k))
            temp=lat[where(temp_lonavg eq MAX(temp_lonavg,/NaN))]
            out(k,m)=temp(0)
         ENDFOR
         out(k,*)=SMOOTH(out(k,*),21,/EDGE_WRAP)
      ENDFOR
      FOR m=0,359 DO BEGIN
         temp=REFORM(out(*,m))
         out_box(m,2)=MEDIAN(temp)
         out_box(m,3)=MEAN(temp)
         sorted=SORT(temp)
         out_box(m,1)=temp[sorted(n_years/4)]
         out_box(m,4)=temp[sorted(n_years*3/4)]
         out_box(m,0)=MIN(temp)
         out_box(m,5)=MAX(temp)
      ENDFOR
      CASE j OF 
         0 : BEGIN
            kpp50_latmax=out
            kpp50_latbox=out_box
         END
         1 : BEGIN
            a50cl_latmax=out
            a50cl_latbox=out_box
         END
         2 : BEGIN
            a5015_latmax=out
            a5015_latbox=out_box
         END
         3 : BEGIN
            kpp30_latmax=out
            kpp30_latbox=out_box
         END
         4 : BEGIN
            trmm_latmax=out
            trmm_latbox=out_box
         END
         5 : BEGIN
            gpcp_latmax=out
            gpcp_latbox=out_box
         END
      ENDCASE
   ENDFOR
   ;IF i eq 0 THEN $
      kpp30_latbox=kpp30_latbox-(kpp30_latbox-kpp50_latbox)*0.25

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/seasonal_cycle/hadgem3kpp_seascycle_precip_xsec.latmax_spaghetti_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=140,MARGIN=2000,SPACE2=800,XOFFSET=1200
   GSET,XMIN=0,XMAX=360,YMIN=plot_box(0),YMAX=plot_box(1)
   ;FOR j=0,kpp50_nyears-1 DO $
   ;   GPLOT,X=indgen(360)+0.5,Y=REFORM(kpp50_latmax(j,*)),COL=FSC_COLOR('red'),THICK=30   
   ;FOR j=0,a50_nyears-1 DO BEGIN
   ;   GPLOT,X=indgen(360)+0.5,Y=REFORM(a50cl_latmax(j,*)),COL=FSC_COLOR('blue'),THICK=30
   ;   GPLOT,X=indgen(360)+0.5,Y=REFORM(a5015_latmax(j,*)),COL=FSC_COLOR('purple'),THICK=30
   ;ENDFOR
   GPLOT,X=indgen(360)+0.5,Y=REFORM(kpp50_latbox(*,2)),COL=FSC_COLOR('red'),THICK=225
   GPLOT,X=indgen(360)+0.5,Y=REFORM(kpp30_latbox(*,2)),COL=FSC_COLOR('orange'),THICK=225
   GPLOT,X=indgen(360)+0.5,Y=REFORM(a50cl_latbox(*,2)),COL=FSC_COLOR('blue'),THICK=225
   GPLOT,X=indgen(360)+0.5,Y=REFORM(a5015_latbox(*,2)),COL=FSC_COLOR('purple'),THICK=225
   ;GPLOT,X=indgen(360)+0.5,Y=REFORM(trmm_latbox(*,3)),COL=FSC_COLOR('black'),THICK=225   
   GPLOT,X=indgen(360)+0.5,Y=REFORM(gpcp_latbox(*,2)),COL=FSC_COLOR('black'),THICK=225
   FOR m=14,359,30 DO BEGIN
      EBAR,X=m+0.5,BOX=[kpp50_latbox(m,0),kpp50_latbox(m,1),!Values.F_NaN,kpp50_latbox(m,4),kpp50_latbox(m,5)],THICK=150,WIDTH=100,COL=FSC_COLOR('red')
      EBAR,X=m+0.5,BOX=[kpp30_latbox(m,0),kpp30_latbox(m,1),!Values.F_NaN,kpp30_latbox(m,4),kpp30_latbox(m,5)],THICK=150,WIDTH=100,COL=FSC_COLOR('orange')      
      EBAR,X=m+0.5,BOX=[a50cl_latbox(m,0),a50cl_latbox(m,1),!Values.F_NaN,a50cl_latbox(m,4),a50cl_latbox(m,5)],THICK=150,WIDTH=100,COL=FSC_COLOR('blue')
      EBAR,X=m+0.5,BOX=[a5015_latbox(m,0),a5015_latbox(m,1),!Values.F_NaN,a5015_latbox(m,4),a5015_latbox(m,5)],THICK=150,WIDTH=100,COL=FSC_COLOR('purple')
                                ;EBAR,X=m+0.5,BOX=[trmm_latbox(m,0),trmm_latbox(m,1),!Values.F_NaN,trmm_latbox(m,4),trmm_latbox(m,5)],THICK=150,WIDTH=100,COL=FSC_COLOR('black')
   ENDFOR
   GLEGEND,labels=REVERSE(['GPCP','K!D50!N-ENT-OBS','K!D30!N-ENT-OBS','A-ENT-K!D50,cl!N','A-ENT-K!D50,15!N']),$
           COL=REVERSE(FSC_COLOR(['black','red','orange','blue','purple'])),LEGPOS=1
;      GPLOT,X=indgen(360)+0.5,Y=REFORM(a5015_latbox(*,1)),COL=FSC_COLOR('blue'),THICK=150,STYLE=2
;      GPLOT,X=indgen(360)+0.5,Y=REFORM(a5015_latbox(*,4)),COL=FSC_COLOR('blue'),THICK=150,STYLE=2
;      GPLOT,X=indgen(360)+0.5,Y=REFORM(a5015_latbox(*,1)),COL=FSC_COLOR('purple'),THICK=150,STYLE=2
;      GPLOT,X=indgen(360)+0.5,Y=REFORM(a5015_latbox(*,4)),COL=FSC_COLOR('purple'),THICK=150,STYLE=2
   
   AXES,XVALS=indgen(12)*30+15,XLABELS=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'],$
        YSTEP=(box(2)-box(0))/10.,YTITLE='Latitude',XTITLE='Month',NDECS=1
   PSCLOSE

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/seasonal_cycle/hadgem3kpp_seascycle_precip_xsec.k50-ent-obs_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=140,MARGIN=2000,SPACE2=800,XOFFSET=1200
   GSET,XMIN=0,XMAX=360,YMIN=box(0),YMAX=box(2)
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[2]
   LEVS,MANUAL=mylevs
   CON,X=indgen(360)+0.5,Y=latitude,FIELD=kpp50_lonavg,CB_TITLE='Precipitation (mm day!U-1!N)',CB_WIDTH=115,$
       TITLE='K!D50!N-ENT-OBS daily clim precip for lonavg '+STRTRIM(STRING(box(1)),1)+'-'+STRTRIM(STRING(box(3)),1)+'E'
   AXES,XVALS=indgen(12)*30+15,XLABELS=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'],$
        YSTEP=(box(2)-box(0))/10.,YTITLE='Latitude',XTITLE='Month',NDECS=1
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/seasonal_cycle/hadgem3kpp_seascycle_precip_xsec.a-ent-k50cl_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=140,MARGIN=2000,SPACE2=800,XOFFSET=1200
   GSET,XMIN=0,XMAX=360,YMIN=box(0),YMAX=box(2)
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[2]
   LEVS,MANUAL=mylevs
   CON,X=indgen(360)+0.5,Y=latitude,FIELD=a50cl_lonavg,CB_TITLE='Precipitation (mm day!U-1!N)',CB_WIDTH=115,$
       TITLE='A-ENT-K!D50,cl!N daily clim precip for lonavg '+STRTRIM(STRING(box(1)),1)+'-'+STRTRIM(STRING(box(3)),1)+'E'
   AXES,XVALS=indgen(12)*30+15,XLABELS=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'],$
        YSTEP=(box(2)-box(0))/10.,YTITLE='Latitude',XTITLE='Month',NDECS=1
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/seasonal_cycle/hadgem3kpp_seascycle_precip_xsec.a-ent-k5015_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=140,MARGIN=2000,SPACE2=800,XOFFSET=1200
   GSET,XMIN=0,XMAX=360,YMIN=box(0),YMAX=box(2)
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[2]
   LEVS,MANUAL=mylevs
   CON,X=indgen(360)+0.5,Y=latitude,FIELD=a5015_lonavg,CB_TITLE='Precipitation (mm day!U-1!N)',CB_WIDTH=115,$
       TITLE='A-ENT-K!D50,15!N daily clim precip for lonavg '+STRTRIM(STRING(box(1)),1)+'-'+STRTRIM(STRING(box(3)),1)+'E'
   AXES,XVALS=indgen(12)*30+15,XLABELS=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'],$
        YSTEP=(box(2)-box(0))/10.,YTITLE='Latitude',XTITLE='Month',NDECS=1
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/seasonal_cycle/hadgem3kpp_seascycle_precip_xsec.k50-ent-obs_minus_a-ent-k50cl_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=140,MARGIN=2000,SPACE2=800,XOFFSET=1200
   GSET,XMIN=0,XMAX=360,YMIN=box(0),YMAX=box(2)
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV,white=white_diff
   LEVS,MANUAL=mylevs_diff
   CON,X=indgen(360)+0.5,Y=latitude,FIELD=kpp50_lonavg-a50cl_lonavg,CB_TITLE='Precipitation (mm day!U-1!N)',CB_WIDTH=115,$
       TITLE='K!D50!N-ENT-OBS minus A-ENT-K!D50,cl!N daily clim precip for lonavg '+STRTRIM(STRING(box(1)),1)+'-'+STRTRIM(STRING(box(3)),1)+'E'
   AXES,XVALS=indgen(12)*30+15,XLABELS=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'],$
        YSTEP=(box(2)-box(0))/10.,YTITLE='Latitude',XTITLE='Month',NDECS=1
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/seasonal_cycle/hadgem3kpp_seascycle_precip_xsec.a-ent-k5015_minus_a-ent-k50cl_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=140,MARGIN=2000,SPACE2=800,XOFFSET=1200
   GSET,XMIN=0,XMAX=360,YMIN=box(0),YMAX=box(2)
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV,white=white_diff
   LEVS,MANUAL=mylevs_diff
   CON,X=indgen(360)+0.5,Y=latitude,FIELD=a5015_lonavg-a50cl_lonavg,CB_TITLE='Precipitation (mm day!U-1!N)',CB_WIDTH=115,$
       TITLE='A-ENT-K!D50,15!N minus A-ENT-K!D50,cl!N daily clim precip for lonavg '+STRTRIM(STRING(box(1)),1)+'-'+STRTRIM(STRING(box(3)),1)+'E'
   AXES,XVALS=indgen(12)*30+15,XLABELS=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'],$
        YSTEP=(box(2)-box(0))/10.,YTITLE='Latitude',XTITLE='Month',NDECS=1
   PSCLOSE,/NOVIEW

ENDFOR

STOP
END
