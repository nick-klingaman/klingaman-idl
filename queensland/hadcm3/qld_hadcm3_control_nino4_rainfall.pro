PRO qld_hadcm3_control_nino4_rainfall

n_periods=7
indir='/home/ss901165/datasets/HadCM3_CONTROL'

start_month=10 ; Offset from DECEMBER
stop_month=15  ; Offset from DECEMBER
months_range='oct-mar'
n_months=stop_month-start_month+1

precip_box=[-45,135,-10,155]
temp_box=[-5,160,5,210]

FOR i=0,n_periods-1 DO BEGIN
   CASE i OF 
      0 : BEGIN
         start_year=101
         stop_year=1000
      END
      1 : BEGIN
         start_year=101 ; For filename
         stop_year=250  ; For filename
      END      
      2 : BEGIN
         start_year=251 ; For filename
         stop_year=400  ; For filename
      END
      3 : BEGIN
         start_year=401 ; For filename
         stop_year=550  ; For filename
      END
      4 : BEGIN
         start_year=551 ; For filename
         stop_year=700  ; For filename
      END
      5 : BEGIN
         start_year=701 ; For filename
         stop_year=850  ; For filename
      END
      6 : BEGIN
         start_year=851 ; For filename
         stop_year=1000  ; For filename
      END
   ENDCASE
   
   mask_infile='/home/ss901165/um_output/mask_n48.nc'

   precip_infile=indir+'/hadcm3_control.dec-nov_mmeans.years'+$
                 STRTRIM(STRING(start_year),1)+'-'+STRTRIM(STRING(stop_year),1)+'.precip.nc'
   temp_infile=indir+'/hadcm3_control.dec-nov_mmeans.years'+$
               STRTRIM(STRING(start_year),1)+'-'+STRTRIM(STRING(stop_year),1)+'.surf_temp.nc'
   
   precip_longitude=OPEN_AND_EXTRACT(precip_infile,'longitude')
   precip_latitude=OPEN_AND_EXTRACT(precip_infile,'latitude')
   DEFINE_BOUNDARIES,precip_box,precip_latitude,precip_longitude,precip_box_tx,/LIMIT
   precip_nlon=N_ELEMENTS(precip_longitude)
   precip_nlat=N_ELEMENTS(precip_latitude)
   
   temp_longitude=OPEN_AND_EXTRACT(temp_infile,'longitude')
   temp_latitude=OPEN_AND_EXTRACT(temp_infile,'latitude')
   DEFINE_BOUNDARIES,temp_box,temp_latitude,temp_longitude,temp_box_tx,/LIMIT
   temp_nlon=N_ELEMENTS(temp_longitude)
   temp_nlat=N_ELEMENTS(temp_latitude)
   
   mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
   mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
   DEFINE_BOUNDARIES,precip_box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
   mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                                count=[precip_nlon,precip_nlat,1,1]))

   n_years=stop_year-start_year+1
   IF stop_month gt 11 THEN $
      n_years=n_years-1

   clim_sst=fltarr(temp_nlon,temp_nlat,n_months)
   FOR j=0,n_months-1 DO BEGIN
      this_month=start_month+j
      IF this_month gt 11 THEN $
         this_month=this_month-12
      clim_sst(*,*,j)=REFORM(OPEN_AND_EXTRACT('/home/ss901165/datasets/HadCM3_CONTROL/hadcm3_control.dec-nov_mmeans_clim.years1-1000.nc',$
                                              'temp',offset=[temp_box_tx(1),temp_box_tx(0),0,this_month],$
                                              count=[temp_nlon,temp_nlat,1,1]))
   ENDFOR

   mean_precip=fltarr(n_years)
   mean_sst=fltarr(n_years)
   FOR j=0,n_years-1 DO BEGIN
      temp_precip=REFORM(OPEN_AND_EXTRACT(precip_infile,'precip',$
                                           offset=[precip_box_tx(1),precip_box_tx(0),start_month+j*12],$
                                           count=[precip_nlon,precip_nlat,n_months]))*86400.*n_months*30.
      FOR k=0,n_months-1 DO BEGIN
         temp=REFORM(temp_precip(*,*,k))
         temp[where(mask eq 0)]=!Values.F_NaN
         temp_precip(*,*,k)=temp
      ENDFOR
      mean_precip(j)=MEAN(temp_precip,/NaN)

      temp_sst=REFORM(OPEN_AND_EXTRACT(temp_infile,'temp',$
                                       offset=[temp_box_tx(1),temp_box_tx(0),start_month+j*12],$
                                       count=[temp_nlon,temp_nlat,n_months]))
      mean_sst(j)=MEAN(temp_sst-clim_sst)
   ENDFOR

   nino=where(mean_sst gt 0.5)
   nina=where(mean_sst lt -0.5)
   regress_nino=REGRESS(mean_sst[nino],mean_precip[nino],CONST=const_nino)
   correlate_nino=CORRELATE(mean_sst[nino],mean_precip[nino])   
   regress_nina=REGRESS(mean_sst[nina],mean_precip[nina],CONST=const_nina)
   correlate_nina=CORRELATE(mean_sst[nina],mean_precip[nina])
   regress_all=REGRESS(mean_sst,mean_precip,CONST=const_all)
   correlate_all=CORRELATE(mean_sst,mean_precip)
  
   psfile='/home/ss901165/idl/queensland/hadcm3/qld_hadcm3_control_nino4_rainfall.oct-mar_years_'+STRTRIM(STRING(start_year),1)+'-'+STRTRIM(STRING(stop_year),1)+'_scatter.ps'
   PSOPEN,file=psfile,FONT=2,TFONT=2,CHARSIZE=120
   GSET,XMIN=-3,XMAX=3,YMIN=200,YMAX=1000
   GPLOT,X=mean_sst,Y=mean_precip,SYM=5,/NOLINES
   GPLOT,X=-2.5,Y=300,TEXT='For La Nina years ('+STRTRIM(STRING(N_ELEMENTS(nina)),1)+') : ',ALIGN=0
   GPLOT,X=-2.5,Y=260,TEXT='rain = '+STRMID(STRTRIM(STRING(regress_nina),1),0,5)+' * nino4_sst + '+STRMID(STRTRIM(STRING(const_nina),1),0,5),ALIGN=0
   GPLOT,X=-2.5,Y=220,TEXT='Correlation = '+STRMID(STRTRIM(STRING(correlate_nina),1),0,5),ALIGN=0
   GPLOT,X=[0,0],Y=[200,1000],STYLE=1
   GPLOT,X=[-0.5,MIN(mean_sst[nina])],Y=[-0.5*regress_nina(0)+const_nina,MIN(mean_sst[nina])*regress_nina(0)+const_nina],COL=FSC_COLOR('blue'),THICK=200
   
   GPLOT,X=1.0,Y=960,TEXT='For El Nino years ('+STRTRIM(STRING(N_ELEMENTS(nino)),1)+') : ',ALIGN=0
   GPLOT,X=1.0,Y=920,TEXT='rain = '+STRMID(STRTRIM(STRING(regress_nino),1),0,5)+' * nino4_sst + '+STRMID(STRTRIM(STRING(const_nino),1),0,5),ALIGN=0
   GPLOT,X=1.0,Y=880,TEXT='Correlation = '+STRMID(STRTRIM(STRING(correlate_nino),1),0,5),ALIGN=0
   GPLOT,X=[0,0],Y=[200,1000],STYLE=1
   GPLOT,X=[0.5,MAX(mean_sst[nino])],Y=[0.5*regress_nino(0)+const_nino,MAX(mean_sst[nino])*regress_nino(0)+const_nino],COL=FSC_COLOR('red'),THICK=200
   
   GPLOT,X=-2.5,Y=960,TEXT='For all years ('+STRTRIM(STRING(n_years),1)+') : ',ALIGN=0
   GPLOT,X=-2.5,Y=920,TEXT='rain = '+STRMID(STRTRIM(STRING(regress_all),1),0,5)+' * nino4_sst + '+STRMID(STRTRIM(STRING(const_all),1),0,5),ALIGN=0
   GPLOT,X=-2.5,Y=880,TEXT='Correlation = '+STRMID(STRTRIM(STRING(correlate_all),1),0,5),ALIGN=0
   GPLOT,X=[0,0],Y=[200,1000],STYLE=1
   GPLOT,X=[MIN(mean_sst),MAX(mean_sst)],Y=[MIN(mean_sst)*regress_all(0)+const_all,MAX(mean_sst)*regress_all(0)+const_all],COL=FSC_COLOR('black'),THICK=200

   AXES,XTITLE=months_range+' Nino4 SST anomaly (from 1000-year climatology)',YTITLE=months_range+' total rainfall (mm)',$
        YSTEP=100,XSTEP=0.5,XMINOR=0.25,YMINOR=50,NDECS=2
   PSCLOSE

ENDFOR

STOP
END
