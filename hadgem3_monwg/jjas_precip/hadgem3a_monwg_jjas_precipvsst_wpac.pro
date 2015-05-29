PRO hadgem3a_monwg_jjas_precipvsst_wpac

n_sets=6
box=[10,130,25,170]
n_days=150

max_nyears=27
smooth_precip=fltarr(n_sets,max_nyears*(n_days-10))
smooth_sst=fltarr(n_sets,max_nyears*(n_days-10))

precip_bins=findgen(25)
sst_bins=findgen(21)*0.2+299.4
;precip_bins=findgen(21)*1-10
;sst_bins=findgen(21)*0.1-1
n_precip_bins=N_ELEMENTS(precip_bins)
n_sst_bins=N_ELEMENTS(sst_bins)

twod_pdf=fltarr(n_sets,n_sst_bins+1,n_precip_bins+1)
pdf_levels=['0.001','0.002','0.003','0.004','0.005','0.006','0.007','0.008','0.009','0.010','0.011','0.012',$
            '0.013','0.014','0.015','0.016','0.017','0.018','0.019','0.020']
;pdf_levels=['0.002','0.004','0.006','0.008','0.010','0.012','0.014','0.016','0.018','0.020','0.022','0.024',$
;            '0.026','0.028','0.030','0.032','0.034','0.036','0.038','0.040']

FOR i=0,n_sets-1 DO BEGIN
   CASE i OF 
      0 : BEGIN
         dir='/home/ss901165/um_output6/xihvd'
         precip_infile=dir+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans.years1-27.precip.nc'
         sst_infile=dir+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans.years1-27.sst.nc'
         n_years=27         
         sst_varname='temp_2'
         runid='hadgem3kpp_1.5xentrain_ga30_50N50S'
         time_offset=120
         precip_multiplier=86400.
         sst_year_offset=0
         sst_offset=0
      END
      1 : BEGIN
         dir='/home/ss901165/um_output6/xihvc'
         precip_infile=dir+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans.years1-10.precip.nc'
         sst_infile=dir+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans.years1-10.surf_temp.nc'
         n_years=10 
         sst_varname='temp_1'
         runid='hadgem3kpp_1.5xentrain_ga30_50N50S_relax'
         time_offset=120
      END
      2 : BEGIN
         dir='/export/niagara/data-06/cy000010/um_output/xhwob'
         precip_infile=dir+'/hadgem3kpp_1.5xentrain_vn78_360E.jan-dec_dmeans.years1-15.precip.nc'
         sst_infile=dir+'/hadgem3kpp_1.5xentrain_vn78_360E.jan-dec_dmeans.years1-15.sst.nc'
         sst_varname='temp_1'
         n_years=15
         runid='hadgem3kpp_1.5xentrain_ga30_30N30S'
         time_offset=105
      END
      3 : BEGIN
         dir='/home/ss901165/um_output6/xgspm'
         precip_infile=dir+'/hadgem3kpp_1.5xentrain_ga30.jan-dec_dmeans.years1-20.precip.nc'
         sst_infile=dir+'/hadgem3kpp_1.5xentrain_ga30.jan-dec_dmeans.i2-k1.sst.nc'
         sst_varname='temp_1'
         n_years=15
         runid='hadgem3kpp_1.5xentrain_ga30'
         time_offset=105
      END
      4 : BEGIN
         dir='/home/ss901165/um_output6/xgspp'
         precip_infile=dir+'/hadgem3kpp_1.0xentrain_ga30.jan-dec_dmeans.years1-20.precip.nc'
         sst_infile=dir+'/hadgem3kpp_1.0xentrain_ga30.jan-dec_dmeans.years1-20.sst.nc'
         sst_varname='temp_1'
         n_years=15
         runid='hadgem3kpp_1.0xentrain_ga30'
         time_offset=105
      END
      5 : BEGIN
         precip_infile='/home/ss901165/datasets/TRMM_3B42V6/n96/TRMM_3B42v6A.jan-dec_dmeans.1999-2011.n96.nc'
         sst_infile='/home/ss901165/datasets/TMI_AMSRE/n96/tmi_fusion.jan-dec_dmeans.1998-2012.n96.nc'
         sst_varname='sst'
         n_years=13
         runid='TRMM_TMI'
         time_offset=120
         precip_multiplier=1
         sst_year_offset=1
         sst_offset=273.15
      END
   ENDCASE

   precip_lon=OPEN_AND_EXTRACT(precip_infile,'longitude')
   precip_lat=OPEN_AND_EXTRACT(precip_infile,'latitude')
   DEFINE_BOUNDARIES,box,precip_lat,precip_lon,precip_box_tx,/LIMIT
   precip_nlon=N_ELEMENTS(precip_lon)
   precip_nlat=N_ELEMENTS(precip_lat)
   
   sst_lon=OPEN_AND_EXTRACT(sst_infile,'longitude')
   sst_lat=OPEN_AND_EXTRACT(sst_infile,'latitude')
   DEFINE_BOUNDARIES,box,sst_lat,sst_lon,sst_box_tx,/LIMIT
   sst_nlon=N_ELEMENTS(sst_lon)
   sst_nlat=N_ELEMENTS(sst_lat)  

   FOR j=0,n_years-1 DO BEGIN
      year_precip=REFORM(OPEN_AND_EXTRACT(precip_infile,'precip',$
                                          offset=[precip_box_tx(1),precip_box_tx(0),time_offset,j],$
                                          count=[precip_nlon,precip_nlat,n_days,1]))*precip_multiplier
      IF TOTAL(where(year_precip ge 1e10)) ge 0 THEN BEGIN
         print,'Precip: ',TOTAL(where(year_precip ge 1e10))
         year_precip[where(year_precip ge 1e10)]=!Values.F_NaN
      ENDIF
      year_sst=REFORM(OPEN_AND_EXTRACT(sst_infile,sst_varname,$
                                       offset=[sst_box_tx(1),sst_box_tx(0),time_offset,j+sst_year_offset],$
                                       count=[sst_nlon,sst_nlat,n_days,1]))+sst_offset
      IF TOTAL(where(year_sst ge 1e10)) ge 0 THEN BEGIN
         print,'SST: ',TOTAL(where(year_sst ge 1e10))
         year_sst[where(year_sst ge 1e10)]=!Values.F_NaN
      ENDIF
      boxavg_precip=fltarr(n_days)
      boxavg_sst=fltarr(n_days)
      FOR k=0,n_days-1 DO BEGIN
         boxavg_precip(k)=MEAN(year_precip(*,*,k),/NaN)
         boxavg_sst(k)=MEAN(year_sst(*,*,k))
      ENDFOR
      FOR k=5,n_days-6 DO BEGIN
         smooth_precip(i,j*(n_days-10)+(k-5))=MEAN(boxavg_precip(k-4:k+4),/NaN)
         smooth_sst(i,j*(n_days-10)+(k-5))=MEAN(boxavg_sst(k-4:k+4))
      ENDFOR
   ENDFOR

 ;  FOR j=0,n_days-11 DO BEGIN
 ;     smooth_precip(i,j:(n_days-10)*n_years-1:(n_days-10))=(smooth_precip(i,j:(n_days-10)*n_years-1:(n_days-10))-MEAN(smooth_precip(i,j:(n_days-10)*n_years-1:(n_days-10)),/NaN))
 ;     smooth_sst(i,j:(n_days-10)*n_years-1:(n_days-10))=smooth_sst(i,j:(n_days-10)*n_years-1:(n_days-10))-MEAN(smooth_sst(i,j:(n_days-10)*n_years-1:(n_days-10)))
 ;  ENDFOR

   IF n_years lt max_nyears THEN BEGIN
      smooth_precip(i,(n_years-1)*(n_days-10):max_nyears*(n_days-10)-1)=!Values.F_NaN
      smooth_sst(i,(n_years-1)*(n_days-10):max_nyears*(n_days-10)-1)=!Values.F_NaN
   ENDIF
   
   FOR j=0,n_sst_bins DO BEGIN
      CASE j OF
         0 : BEGIN
            sst_pts=where(smooth_sst(i,*) lt sst_bins(0))
         END
         n_sst_bins : BEGIN
            sst_pts=where(smooth_sst(i,*) ge sst_bins(n_sst_bins-1))
         END
         ELSE : BEGIN
            sst_pts=where(smooth_sst(i,*) ge sst_bins(j-1) and smooth_sst(i,*) lt sst_bins(j))
         END
      ENDCASE

      IF TOTAL(sst_pts) ge 0 THEN BEGIN
         temp=REFORM(smooth_precip(i,*))
         valid_precip=temp[sst_pts]
         FOR k=0,n_precip_bins DO BEGIN
            CASE k OF
               0 : BEGIN
                  precip_pts=where(valid_precip lt precip_bins(0))
               END
               n_precip_bins : BEGIN
                  precip_pts=where(valid_precip ge precip_bins(n_precip_bins-1))
               END
               ELSE : BEGIN
                  precip_pts=where(valid_precip ge precip_bins(k-1) and valid_precip lt precip_bins(k))
               END
            ENDCASE
            IF TOTAL(precip_pts) ge 0 THEN BEGIN
               twod_pdf(i,j,k)=N_ELEMENTS(precip_pts)
            ENDIF ELSE $
               twod_pdf(i,j,k)=0.
         ENDFOR
      ENDIF
   ENDFOR
   
   twod_pdf(i,*,*)=twod_pdf(i,*,*)/TOTAL(twod_pdf(i,*,*))

   ;IF i eq 0 or i eq 4 or i eq 3 THEN BEGIN
   ;   FOR j=0,n_sst_bins DO $
   ;      twod_pdf(i,j,*)=twod_pdf(i,j,*)*(1.0+0.025*(n_sst_bins/2-j)*(indgen(n_precip_bins+1)-n_precip_bins/2))
   ;   FOR k=0,n_precip_bins DO $
   ;      twod_pdf(i,*,k)=twod_pdf(i,*,k)*(1.0+0.025*(n_precip_bins/2-k)*(indgen(n_sst_bins+1)-n_sst_bins/2))
   ;ENDIF
   ; IF i eq 5 THEN BEGIN
   ;   FOR j=0,n_sst_bins DO $
   ;      twod_pdf(i,j,*)=twod_pdf(i,j,*)*(1.0+0.035*(n_sst_bins/2-j)*(indgen(n_precip_bins+1)-n_precip_bins/2))
   ;   FOR k=0,n_precip_bins DO $
   ;      twod_pdf(i,*,k)=twod_pdf(i,*,k)*(1.0+0.035*(n_precip_bins/2-k)*(indgen(n_sst_bins+1)-n_sst_bins/2))
   ;ENDIF
   ;IF i eq 2 THEN BEGIN
   ;   FOR j=0,n_sst_bins DO $
   ;      twod_pdf(i,j,*)=twod_pdf(i,j,*)*(1.0-0.01*(n_sst_bins/2-j)*(indgen(n_precip_bins+1)-n_precip_bins/2))
   ;   FOR k=0,n_precip_bins DO $
   ;      twod_pdf(i,*,k)=twod_pdf(i,*,k)*(1.0-0.01*(n_precip_bins/2-k)*(indgen(n_sst_bins+1)-n_sst_bins/2))
   ;ENDIF

   psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3a_monwg_jjas_precipvsst_wpac.'+runid+'.twod_pdf.ps'
   PSOPEN,file=psfile,TFONT=2,FONT=2,CHARSIZE=120,MARGIN=2000,SPACE2=800,SPACE1=200,XOFFSET=1000
   GSET,XMIN=0,XMAX=n_sst_bins+1,YMIN=0,YMAX=n_precip_bins+1
   CS,SCALE=2,NCOLS=N_ELEMENTS(pdf_levels)+1,white=[2]
   LEVS,MANUAL=pdf_levels
   CON,X=indgen(n_sst_bins+1)+0.5,Y=indgen(n_precip_bins+1)+0.5,FIELD=REFORM(twod_pdf(i,*,*)),/BLOCK,/NOLINES,CB_WIDTH=115,$
       CB_TITLE='Probability'
;   GPLOT,X=[n_sst_bins/2+1,n_sst_bins/2+1],Y=[0,n_precip_bins+1],STYLE=1,COL=FSC_COLOR('black')
;   GPLOT,X=[0,n_sst_bins+1],Y=[n_precip_bins/2+1,n_precip_bins/2+1],STYLE=1,COL=FSC_COLOR('black')
   AXES,XVALS=indgen(n_sst_bins+2),XLABELS=['<'+STRMID(STRTRIM(STRING(sst_bins(0)),1),0,5),STRMID(STRTRIM(STRING(sst_bins),1),0,5),$
                                          '>'+STRMID(STRTRIM(STRING(sst_bins(n_sst_bins-1)),1),0,5)],$
        YVALS=indgen(n_precip_bins+2),YLABELS=['<'+STRMID(STRTRIM(STRING(precip_bins(0)),1),0,4),STRMID(STRTRIM(STRING(precip_bins),1),0,4),$
                                               '>'+STRMID(STRTRIM(STRING(precip_bins(n_precip_bins-1)),1),0,4)],ORIENTATION=25,$
;        XTITLE='SST anomaly (K)',YTITLE='Precipitation anomaly (mm day!U-1!N)'
        XTITLE='SST (K)',YTITLE='Precipitation (mm day!U-1!N)'
   PSCLOSE

ENDFOR

STOP
END

