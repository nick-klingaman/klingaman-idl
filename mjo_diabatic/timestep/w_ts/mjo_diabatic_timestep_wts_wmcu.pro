PRO mjo_diabatic_timestep_wts_wmcu

; Plot timeseries of timestep W and M with precipitation

n_pts=7
n_lev=2
n_models=3

;wap_levs=['-0.55','-0.45','-0.35','-0.25','-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55']

FOR m=0,n_models-1 DO BEGIN
   CASE m OF
      0 : BEGIN
         dir='/home/ss901165/um_output6/mjodiab_2day/metum'
         model='MetUM'
         upper_range='27-48'
         full_range='0-48'
         pr_file=dir+'/MetUM.pr.20091020-20100110.lead_12-48hrs.nc'
         pr_mult=86400.
         time_offset=180*4
         nt=180
         lon_name='longitude'
         lat_name='latitude'
         w_mult=1
         p_mult=0.01
         nz=48
         wap_levs=['-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11']
      END
      1 : BEGIN
         dir='/home/ss901165/um_output6/mjodiab_2day/mri'
         model='MRI-AGCM'
         upper_range='12-26'
         full_range='0-26'
         pr_file=dir+'/MRI-AGCM.pr.20091020-20100110.lead_12-48hrs.nc'
         pr_mult=86400.
         time_offset=72*4
         nt=72
         lon_name='lon'
         lat_name='lat'
         w_mult=-1.
         nz=26
         p_mult=1.
         wap_levs=['-0.33','-0.27','-0.21','-0.15','-0.09','-0.03','0.03','0.09','0.15','0.21','0.27','0.33']

;wap_levs=['-0.011','-0.009','-0.007','-0.005','-0.003','-0.001','0.001','0.003','0.005','0.007','0.009','0.011']
      END
      2 : BEGIN
         dir='/home/ss901165/um_output6/mjodiab_2day/giss'
         model='ModelE'
         upper_range='11-25'
         full_range='0-25'
         pr_file=dir+'/ModelE.pr.20091020-20100110.lead_12-48hrs.nc'
         pr_mult=1.
         time_offset=72*4
         nt=72
         lon_name='longitude'
         lat_name='latitude'
         w_mult=10.
         nz=25
         wap_levs=['-0.22','-0.18','-0.14','-0.10','-0.06','-0.02','0.02','0.06','0.10','0.14','0.18','0.22']
      END
   ENDCASE

FOR i=0,n_pts-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         pt_lon=75
         pt_lat=0
      END     
      1 : BEGIN
         pt_lon=75
         pt_lat=-5
      END
      2 : BEGIN
         pt_lon=75
         pt_lat=5
      END
      3 : BEGIN
         pt_lon=70
         pt_lat=0
      END
      4 : BEGIN
         pt_lon=80
         pt_lat=0
      END
      5 : BEGIN
         pt_lon=70
         pt_lat=5
      END
      6 : BEGIN
         pt_lon=80
         pt_lat=5
      END
   ENDCASE

   lon=OPEN_AND_EXTRACT(pr_file,lon_name)
   lat=OPEN_AND_EXTRACT(pr_file,lat_name)
   lonpt=NEAREST(lon,pt_lon)
   latpt=NEAREST(lat,pt_lat)
   print,lonpt,latpt
   pr_ts=REFORM(OPEN_AND_EXTRACT(pr_file,'pr',$
                                 offset=[lonpt,latpt,time_offset],count=[1,1,nt]))*pr_mult

   FOR j=0,n_lev-1 DO BEGIN
      CASE j OF
         0 : BEGIN
            lev_range=upper_range
            w_file=dir+'/'+model+'.wap_intrho_lev'+lev_range+'.20091020-20100110.lead_12-48hrs.nc'
            wap_file=dir+'/'+model+'.wap.20091020-20100110.lead_12-48hrs.nc'
            p_file=dir+'/'+model+'.p.20100110.00Z.nc'
            m_file=dir+'/'+model+'.mcu_intrho_lev'+lev_range+'.20091020-20100110.lead_12-48hrs.nc'
         END
         ;1 : BEGIN
         ;   lev_range='20-26'
         ;   w_file=metum+'/MetUM.wap_intrho_lev'+lev_range+'.20091020-20100110.lead_12-48hrs.nc'
         ;   m_file=metum+'/MetUM.mcu_intrho_lev'+lev_range+'.20091020-20100110.lead_12-48hrs.nc'
         ;END
         1 : BEGIN            
            lev_range=full_range
            w_file=dir+'/'+model+'.wap_intrho_lev'+lev_range+'.20091020-20100110.lead_12-48hrs.nc'
            m_file=dir+'/'+model+'.mcu_intrho_lev'+lev_range+'.20091020-20100110.lead_12-48hrs.nc'
         END
      ENDCASE
      
      lon=OPEN_AND_EXTRACT(w_file,'longitude')
      lat=OPEN_AND_EXTRACT(w_file,'latitude')
      lonpt=NEAREST(lon,pt_lon)
      latpt=NEAREST(lat,pt_lat)
      print,lonpt,latpt
      w_ts=REFORM(OPEN_AND_EXTRACT(w_file,'wap_intrho',$
                                   offset=[lonpt,latpt,time_offset],count=[1,1,nt]))*w_mult      

      psfile='/home/ss901165/idl/mjo_diabatic/timestep/w_ts/mjo_diabatic_timestep_wts_wmcu.'+model+'_pt_'+$
             STRTRIM(STRING(pt_lon),1)+'E_'+STRTRIM(STRING(pt_lat),1)+'N_lev'+lev_range+'_time'+STRTRIM(STRING(time_offset),1)+'.ps'
      PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,MARGIN=3000,YOFFSET=1500,XOFFSET=1000
      GSET,XMIN=0,XMAX=nt,YMIN=1,YMAX=200,/YLOG
      HIST,X=indgen(nt)+0.5,Y=pr_ts,WIDTH=20,FILLCOL=FSC_COLOR('black')
      IF TOTAL(where(pr_ts le 1)) ge 0 THEN $
         pr_ts[where(pr_ts le 1)]=1.
      GPLOT,X=indgen(nt)+0.5,Y=pr_ts,/NOLINES,SYM=3,COL=FSC_COLOR('black'),SIZE=50
      ;GPLOT,X=indgen(nt)+0.5,Y=m_ts*10.,COL=FSC_COLOR('blue'),SYM=3,SIZE=40
      AXES,XSTEP=10,YVALS=['1.5','2','3','4','5.5','7','10','15','20','30','40','55','70','100','150','200'],/ONLYRIGHT,YTITLE='Precipitation (mm day!U-1!N)',$
           NDECS=1
      GSET,XMIN=0,XMAX=nt,YMIN=-0.09,YMAX=0.24
      GPLOT,X=indgen(nt)+0.5,Y=w_ts,COL=FSC_COLOR('red'),SYM=3,SIZE=50
      GPLOT,X=[0,nt],Y=[0,0],COL=FSC_COLOR('red'),STYLE=1
      AXES,XSTEP=nt/10,XMINOR=1,YSTEP=0.03,YMINOR=0.01,YTITLE='rho-weighted wap (levels '+lev_range+'; Pa s!U-1!N; pos up)',XTITLE='Timestep',/NORIGHT,NDECS=2
      PSCLOSE,/NOVIEW

      IF j eq 0 THEN BEGIN
         lon=OPEN_AND_EXTRACT(wap_file,lon_name)
         lat=OPEN_AND_EXTRACT(wap_file,lat_name)
         lonpt=NEAREST(lon,pt_lon)
         latpt=NEAREST(lat,pt_lat)
         print,lonpt,latpt
         wap=REFORM(OPEN_AND_EXTRACT(wap_file,'wap',$
                                     offset=[lonpt,latpt,0,time_offset],count=[1,1,nz,nt]))*w_mult

         wap_toplot=fltarr(nt,nz)
         FOR p=0,nz-1 DO $
            wap_toplot(*,p)=wap(p,*)

         p=REFORM(OPEN_AND_EXTRACT(p_file,'p',$
                                   offset=[lonpt,latpt,0,0],count=[1,1,nz,1]))*p_mult

         psfile='/home/ss901165/idl/mjo_diabatic/timestep/w_ts/mjo_diabatic_timestep_wts_wmcu.'+model+'_pt_'+$
                STRTRIM(STRING(pt_lon),1)+'E_'+STRTRIM(STRING(pt_lat),1)+'N_wzsec_time'+STRTRIM(STRING(time_offset),1)+'.ps'
         PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,MARGIN=3000,YOFFSET=1000,XOFFSET=1000,SPACE2=1000
         GSET,XMIN=0,XMAX=nt,YMIN=1000.,YMAX=100.
         CS,SCALE=1,NCOL=N_ELEMENTS(wap_levs)+1,white=[8]
         LEVS,MANUAL=wap_levs
         CON,X=indgen(nt)+0.5,Y=p,FIELD=wap_toplot,/NOLINES,/BLOCK,CB_TITLE='Vertical velocity (Pa s!U-1!N, positive up)',$
             CB_WIDTH=110
         AXES,XSTEP=nt/10,XMINOR=1,YSTEP=-100,YMINOR=-25,YTITLE='Pressure (hPa)',XTITLE='Timestep',/NORIGHT
         GSET,XMIN=0,XMAX=nt,YMIN=1,YMAX=200,/YLOG
         HIST,X=indgen(nt)+0.5,Y=pr_ts,WIDTH=8,FILLCOL=FSC_COLOR('black')
         IF TOTAL(where(pr_ts le 1)) ge 0 THEN $
            pr_ts[where(pr_ts le 1)]=1.
         GPLOT,X=indgen(nt)+0.5,Y=pr_ts,/NOLINES,SYM=3,COL=FSC_COLOR('black'),SIZE=40
         AXES,XSTEP=10,YVALS=['1.5','2','3','4','5.5','7','10','15','20','30','40','55','70','100','150','200'],/ONLYRIGHT,YTITLE='Precipitation (mm day!U-1!N)',$
              NDECS=1
         PSCLOSE
      ENDIF

   ENDFOR
ENDFOR
ENDFOR

STOP
END

      
