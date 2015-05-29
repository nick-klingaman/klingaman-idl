PRO hadgem3kpp_ssts_compute_iav

mmean_infile='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_mmeans.years1-60.sst.nc'
mmean_T_infile='/home/ss901165/um_output6/xihvd/kpp_ocean/KPPocean.jan-dec_mmeans.i2-o2.nc'
mmean_S_infile=mmean_T_infile

box=[-55,0,55,360]

longitude=OPEN_AND_EXTRACT(mmean_infile,'longitude')
latitude=OPEN_AND_EXTRACT(mmean_infile,'latitude')
DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)

mmean_ssts=OPEN_AND_EXTRACT(mmean_infile,'temp_2',offset=[box_tx(1),box_tx(0),0,0],$
                            count=[n_lon,n_lat,12,60])

kpp_longitude=OPEN_AND_EXTRACT(mmean_T_infile,'longitude')
kpp_latitude=OPEN_AND_EXTRACT(mmean_T_infile,'latitude')
kpp_z=OPEN_AND_EXTRACT(mmean_T_infile,'z_1')
DEFINE_BOUNDARIES,box,kpp_latitude,kpp_longitude,kpp_box_tx,/LIMIT
kpp_nlon=N_ELEMENTS(kpp_longitude)
kpp_nlat=N_ELEMENTS(kpp_latitude)
kpp_nz=N_ELEMENTS(kpp_z)

mmean_T=OPEN_AND_EXTRACT(mmean_T_infile,'T',offset=[kpp_box_tx(1),kpp_box_tx(0),0,0],$
                         count=[kpp_nlon,kpp_nlat,kpp_nz,12*60])
mmean_T[where(mmean_T ge 1e10)]=!Values.F_NaN
mmean_S=OPEN_AND_EXTRACT(mmean_T_infile,'S',offset=[kpp_box_tx(1),kpp_box_tx(0),0,0],$
                         count=[kpp_nlon,kpp_nlat,kpp_nz,12*60])
mmean_S[where(mmean_S ge 1e10)]=!Values.F_NaN
                         
n_periods=5
FOR i=0,n_periods-1 DO BEGIN
   CASE i OF 
      0 : BEGIN
         months=indgen(12)+11
         period_name='dec-nov'
         ts_yrange=[-0.4,0.4]
         ts_ymajor=0.2
      END
      1 : BEGIN
         months=[2,3,4]
         period_name='mar-may'
         ts_yrange=[-0.4,0.4]
         ts_ymajor=0.2
      END
      2 : BEGIN
         months=[5,6,7]
         period_name='jun-aug'         
         ts_yrange=[-0.8,0.8]
         ts_ymajor=0.4
      END
      3 : BEGIN
         months=[8,9,10]
         period_name='sep-nov'
      END
      4 : BEGIN
         months=[11,12,13]
         period_name='dec-feb'         
         ts_yrange=[-0.4,0.4]
         ts_ymajor=0.2
      END
   ENDCASE
   print,'------'
   print,'Season '+STRTRIM(STRING(i))

   n_months=N_ELEMENTS(months)
   smean_ssts=fltarr(n_lon,n_lat,60)
   smean_T=fltarr(kpp_nlon,kpp_nlat,kpp_nz,60)
   smean_S=fltarr(kpp_nlon,kpp_nlat,kpp_nz,60)  
   FOR j=0,n_months-1 DO BEGIN
      IF months(j) ge 12 THEN BEGIN
         this_month=months(j)-12
         offset=1
      ENDIF ELSE BEGIN
         this_month=months(j)
         offset=0
      ENDELSE         
      print,'month = '+STRTRIM(STRING(this_month),1)
      FOR m=0,n_lon-1 DO $
         FOR n=0,n_lat-1 DO $
            smean_ssts(m,n,offset:59)=smean_ssts(m,n,offset:59)+(mmean_ssts(m,n,this_month,offset:59))/FLOAT(n_months)
      FOR m=0,kpp_nlon-1 DO BEGIN
         FOR n=0,kpp_nlat-1 DO BEGIN
            FOR p=0,kpp_nz-1 DO BEGIN
               smean_T(m,n,p,offset:59)=smean_T(m,n,p,offset:59)+(mmean_T(m,n,p,this_month+offset*12:60*12-1:12))/FLOAT(n_months)
               smean_S(m,n,p,offset:59)=smean_S(m,n,p,offset:59)+(mmean_S(m,n,p,this_month+offset*12:60*12-1:12))/FLOAT(n_months)
            ENDFOR
         ENDFOR
      ENDFOR
   ENDFOR
   
   iav_sst=fltarr(n_lon,n_lat)
   acorr_sst=fltarr(n_lon,n_lat)
   detrend_ssts=fltarr(n_lon,n_lat,60)
   FOR m=0,n_lon-1 DO BEGIN
      FOR n=0,n_lat-1 DO BEGIN
         temp=REGRESS(indgen(60-offset),REFORM(smean_ssts(m,n,offset:59)))
         detrend_ssts(m,n,offset:59)=smean_ssts(m,n,offset:59)-indgen(60-offset)*temp(0)
         iav_sst(m,n)=STDDEV(detrend_ssts(m,n,offset:59))
         acorr_sst(m,n)=A_CORRELATE(detrend_ssts(m,n,offset:59),1)
      ENDFOR
   ENDFOR

   iav_sst[where(iav_sst le 0.01)]=!Values.F_NaN
   acorr_sst[where(FINITE(iav_sst) eq 0)]=!Values.F_NaN

   mylevs_iav=['0.1','0.2','0.3','0.4','0.5','0.6','0.7','0.8','0.9','1.0','1.1','1.2']
   mylevs_acorr=['-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9']

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/ssts/hadgem3kpp_ssts_compute_iav.xihvd_'+period_name+'_iav.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,MARGIN=2000,YSIZE=14000,YOFFSET=2000,SPACE2=1500,TCHARSIZE=140
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
   CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_iav)+1
   LEVS,MANUAL=mylevs_iav
   CON,X=longitude,Y=latitude,FIELD=iav_sst,POSITIVE_STYLE=2,CB_TITLE='Standard deviation in SST (K)',$
       TITLE='Inter-annual variability in '+period_name+' mean SST (75 years of GA3-KPP)',/NOLINELABELS
   AXES,XSTEP=30,YSTEP=10
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/ssts/hadgem3kpp_ssts_compute_iav.xihvd_'+period_name+'_acorr.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,MARGIN=2000,YSIZE=14000,YOFFSET=2000,SPACE2=1500,TCHARSIZE=140
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_acorr)+1;,white=[7]
   LEVS,MANUAL=mylevs_acorr
   CON,X=longitude,Y=latitude,FIELD=acorr_sst,POSITIVE_STYLE=2,CB_TITLE='Correlation coefficient',$
       TITLE='Lag-1 auto-correlation in '+period_name+' mean SST (75 years of GA3-KPP)',/NOLINELABELS,NEGATIVE_STYLE=1
   AXES,XSTEP=30,YSTEP=10
   PSCLOSE,/NOVIEW

   n_basins=4
   basin_ts=fltarr(n_basins,60)
   basin_title=strarr(n_basins)
   basin_color=strarr(n_basins)   
   basin_names=strarr(n_basins)

   nz=100
   basin_T=fltarr(n_basins,60,nz)
   basin_S=fltarr(n_basins,60,nz)


   psfile='/home/ss901165/idl/hadgem3-kpp_runs/ssts/hadgem3kpp_ssts_compute_iav.xihvd_'+period_name+'_basints.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,MARGIN=1200,YOFFSET=200,SPACE2=100,TCHARSIZE=120,XOFFSET=1500,SPACE3=25,YPLOTS=4,SPACE1=25,$
          YSPACING=2500
   FOR j=0,n_basins-1 DO BEGIN
      CASE j OF 
         0 : BEGIN
            basin_box=[40,280,55,360]
            basin_names(j)='n_atlantic'
            basin_title(j)='North Atlantic (40-55N,280-360E)'
            basin_color(j)='blue'
         END
         1 : BEGIN
            basin_box=[40,150,55,240]
            basin_names(j)='n_pacific'
            basin_title(j)='North Pacific (40-55N,150-240E)'
            basin_color(j)='purple'
         END
         2 : BEGIN
            basin_box=[-15,180,15,300]
            basin_names(j)='trop_pacific'
            basin_title(j)='Tropical Pacific (15S-15N,180-300E)'
            basin_color(j)='orange'
         END
         3 : BEGIN
            basin_box=[-15,270,15,360]
            basin_names(j)='trop_atlantic'
            basin_title(j)='Tropical Atlantic (15S-15N,270-360E)'
            basin_color(j)='red'
         END
      ENDCASE
      
      DEFINE_BOUNDARIES,basin_box,latitude,longitude,basin_box_tx
      FOR k=offset,59 DO $
         basin_ts(j,k)=MEAN(detrend_ssts(basin_box_tx(1):basin_box_tx(3),basin_box_tx(0):basin_box_tx(2),k),/NaN)
      basin_ts(j,*)=basin_ts(j,*)-MEAN(basin_ts(j,offset:59))
   
      DEFINE_BOUNDARIES,basin_box,kpp_latitude,kpp_longitude,kpp_basin_box_tx
      FOR k=offset,59 DO BEGIN
         FOR m=0,kpp_nz-1 DO BEGIN
            basin_T(j,k,m)=MEAN(smean_T(kpp_basin_box_tx(1):kpp_basin_box_tx(3),$
                                        kpp_basin_box_tx(0):kpp_basin_box_tx(2),m,k),/NaN)
            basin_S(j,k,m)=MEAN(smean_S(kpp_basin_box_tx(1):kpp_basin_box_tx(3),$
                                        kpp_basin_box_tx(0):kpp_basin_box_tx(2),m,k),/NaN)
         ENDFOR
      ENDFOR
      FOR m=0,kpp_nz-1 DO BEGIN
         temp=REGRESS(indgen(60-offset),REFORM(basin_T(j,offset:59,m)))
         basin_T(j,offset:59,m)=basin_T(j,offset:59,m)-indgen(60-offset)*temp(0)
         temp=REGRESS(indgen(60-offset),REFORM(basin_S(j,offset:59,m)))
         basin_S(j,offset:59,m)=basin_S(j,offset:59,m)-indgen(60-offset)*temp(0)         
      ENDFOR

      POS,YPOS=j+1
      GSET,XMIN=0,XMAX=60,YMIN=ts_yrange(0),YMAX=ts_yrange(1),TITLE=basin_title(j)

      GPLOT,X=indgen(60-offset+1)+0.5,Y=REFORM(basin_ts(j,offset:59)),COL=FSC_COLOR(basin_color(j)),SYM=3+j,SIZE=70,THICK=80
      GPLOT,X=indgen(60-offset+1)+0.5,Y=REFORM(SMOOTH(basin_ts(j,offset:59),5)),COL=FSC_COLOR(basin_color(j)),THICK=300,STYLE=2

      GPLOT,X=[offset,60],Y=[0,0],STYLE=2
      AXES,XSTEP=5,XMINOR=1,XTITLE='Year of simulation',YTITLE='SST anomaly (K)',NDECS=2,YMINOR=ts_ymajor/2.,YSTEP=ts_ymajor,/NOUPPER
   ENDFOR
   PSCLOSE,/NOVIEW
   
   mylevs_T=['-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1']
   mylevs_S=['-0.22','-0.18','-0.14','-0.10','-0.06','-0.02','0.02','0.06','0.10','0.14','0.18','0.22']
   FOR j=0,n_basins-1 DO BEGIN
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/ssts/hadgem3kpp_ssts_compute_iav.xihvd_'+period_name+'_'+basin_names(j)+'_Tz.ps'
      FOR m=0,kpp_nz-1 DO BEGIN
         basin_T(j,offset:59,m)=basin_T(j,offset:59,m)-MEAN(basin_T(j,offset:59,m),/NaN)
         basin_S(j,offset:59,m)=basin_S(j,offset:59,m)-MEAN(basin_S(j,offset:59,m),/NaN)
      ENDFOR
      PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,MARGIN=2500,YOFFSET=1000,XOFFSET=500,SPACE2=1000
      GSET,XMIN=offset,XMAX=59,YMIN=1000,YMAX=MIN(ABS(kpp_z))
      LEVS,MANUAL=mylevs_T
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_T)+1,white=[8]
      CON,X=indgen(60-offset)+0.5,Y=ABS(kpp_z),FIELD=REFORM(basin_T(j,offset:59,*)),/NOLINES,/BLOCK,CB_TITLE='Anomaly in temperature from model climatology (K)',TITLE='T anomaly for '+basin_title(j)+' - '+period_name+' - GA3-KPP 50N-50S'
      AXES,XSTEP=3,XMINOR=1,YSTEP=-100,YMINOR=-25,YTITLE='Depth below surface (m)',XTITLE='Year of simulation'
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/hadgem3-kpp_runs/ssts/hadgem3kpp_ssts_compute_iav.xihvd_'+period_name+'_'+basin_names(j)+'_Sz.ps'
      PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,MARGIN=2500,YOFFSET=1000,XOFFSET=500,SPACE2=1000
      GSET,XMIN=offset,XMAX=59,YMIN=1000,YMAX=MIN(ABS(kpp_z))
      LEVS,MANUAL=mylevs_S
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_S)+1,white=[8] 
      CON,X=indgen(60-offset)+0.5,Y=ABS(kpp_z),FIELD=REFORM(basin_S(j,offset:59,*)),/NOLINES,/BLOCK,CB_TITLE='Anomaly in salinity from model climatology (psu)',TITLE='S anomaly for '+basin_title(j)+' - '+period_name+' - GA3-KPP 50N-50S'
      AXES,XSTEP=3,XMINOR=1,YSTEP=-100,YMINOR=-25,YTITLE='Depth below surface (m)',XTITLE='Year of simulation'
      PSCLOSE,/NOVIEW
   ENDFOR

ENDFOR

STOP
END
