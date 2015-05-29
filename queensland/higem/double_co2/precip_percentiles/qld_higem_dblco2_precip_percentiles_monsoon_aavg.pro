PRO qld_higem_dblco2_precip_percentiles_monsoon_aavg

higem_stbl_infile='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.may-apr_dmeans.o2-r3.precip.qld_region_aavg.nc'
higem_control_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.may-apr_dmeans.h9-w8.precip.qld_region_aavg.nc'

higem_stbl_nyears=31
higem_control_nyears=149

season_start=120
season_stop=359
season_ndays=(season_stop-season_start)+1

n_regions=5

FOR i=0,n_regions-1 DO BEGIN
   higem_stbl_precip=REFORM(OPEN_AND_EXTRACT(higem_stbl_infile,'rain_aavg_region',$
                                             offset=[season_start,0,i],$
                                             count=[season_ndays,higem_stbl_nyears,1]))*86400.

   stbl_cumulative=fltarr(season_ndays,higem_stbl_nyears)
   FOR j=0,higem_stbl_nyears-1 DO BEGIN
      cumulative=TOTAL(higem_stbl_precip(*,j))
      ;print,cumulative
      FOR k=0,season_ndays-1 DO $
         stbl_cumulative(k,j)=TOTAL(higem_stbl_precip(0:k,j))/cumulative
   ENDFOR

   stbl_mean=fltarr(season_ndays)
   stbl_median=fltarr(season_ndays)
   stbl_iqr=fltarr(2,season_ndays)
   stbl_range=fltarr(2,season_ndays)
   FOR j=0,season_ndays-1 DO BEGIN
      temp=REFORM(stbl_cumulative(j,*))
      sorted=SORT(temp)
      stbl_median(j)=temp(sorted(higem_stbl_nyears/2))
      stbl_mean(j)=MEAN(temp)
      stbl_iqr(0,j)=temp(sorted(higem_stbl_nyears/4))
      stbl_iqr(1,j)=temp(sorted(higem_stbl_nyears*3/4))
      stbl_range(0,j)=temp(sorted(0))
      stbl_range(1,j)=temp(sorted(higem_stbl_nyears-1))
   ENDFOR

   higem_control_precip=REFORM(OPEN_AND_EXTRACT(higem_control_infile,'rain_aavg_region',$
                                             offset=[season_start,0,i],$
                                             count=[season_ndays,higem_control_nyears,1]))*86400.

   control_cumulative=fltarr(season_ndays,higem_control_nyears)
   FOR j=0,higem_control_nyears-1 DO BEGIN
      cumulative=TOTAL(higem_control_precip(*,j))
      ;print,cumulative
      FOR k=0,season_ndays-1 DO $
         control_cumulative(k,j)=TOTAL(higem_control_precip(0:k,j))/cumulative
   ENDFOR

   control_mean=fltarr(season_ndays)
   control_median=fltarr(season_ndays)
   control_iqr=fltarr(2,season_ndays)
   control_range=fltarr(2,season_ndays)
   FOR j=0,season_ndays-1 DO BEGIN
      temp=REFORM(control_cumulative(j,*))
      sorted=SORT(temp)
      control_mean(j)=MEAN(temp)
      control_median(j)=temp(sorted(higem_control_nyears/2))
      control_iqr(0,j)=temp(sorted(higem_control_nyears/4))
      control_iqr(1,j)=temp(sorted(higem_control_nyears*3/4))
      control_range(0,j)=temp(sorted(0))
      control_range(1,j)=temp(sorted(higem_control_nyears-1))
   ENDFOR

   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/qld_higem_dblco2_precip_percentiles_monsoon_aavg.qld_region'+STRTRIM(STRING(i+1),1)+'_cumulative.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=1000,TFONT=3,$
          TCHARSIZE=100,SPACE3=500,/PORTRAIT
   
   GSET,XMIN=season_start+120,XMAX=season_stop+120-1,YMIN=0,YMAX=1.1,TITLE='Cumulative Sep-Apr precip for QLD region '+STRTRIM(STRING(i+1),1)

   GPLOT,X=indgen(season_ndays)+season_start+120.5,Y=stbl_median,COL=FSC_COLOR('red'),THICK=150
   GPLOT,X=indgen(season_ndays)+season_start+120.5,Y=stbl_mean,COL=FSC_COLOR('red'),THICK=150,STYLE=3
   GPLOT,X=indgen(season_ndays)+season_start+120.5,Y=REFORM(stbl_iqr(0,*)),COL=FSC_COLOR('red'),STYLE=2
   GPLOT,X=indgen(season_ndays)+season_start+120.5,Y=REFORM(stbl_iqr(1,*)),COL=FSC_COLOR('red'),STYLE=2
;   GPLOT,X=indgen(season_ndays)+season_start+120.5,Y=REFORM(stbl_range(0,*)),COL=FSC_COLOR('red'),STYLE=1
;   GPLOT,X=indgen(season_ndays)+season_start+120.5,Y=REFORM(stbl_range(1,*)),COL=FSC_COLOR('red'),STYLE=1

   GPLOT,X=indgen(season_ndays)+season_start+120.5,Y=control_median,COL=FSC_COLOR('black'),THICK=150
   GPLOT,X=indgen(season_ndays)+season_start+120.5,Y=control_mean,COL=FSC_COLOR('black'),THICK=150,STYLE=3
   GPLOT,X=indgen(season_ndays)+season_start+120.5,Y=REFORM(control_iqr(0,*)),COL=FSC_COLOR('black'),STYLE=2
   GPLOT,X=indgen(season_ndays)+season_start+120.5,Y=REFORM(control_iqr(1,*)),COL=FSC_COLOR('black'),STYLE=2
;   GPLOT,X=indgen(season_ndays)+season_start+120.5,Y=REFORM(control_range(0,*)),COL=FSC_COLOR('black'),STYLE=1
;   GPLOT,X=indgen(season_ndays)+season_start+120.5,Y=REFORM(control_range(1,*)),COL=FSC_COLOR('black'),STYLE=1


   GLEGEND,labels=REVERSE(['Median','Mean','Inter-quartile range']),STYLE=REVERSE([0,3,2]),LEGPOS=1
   GLEGEND,labels=REVERSE(['HiGEM control','HiGEM 2xCO2']),COL=REVERSE(FSC_COLOR(['black','red'])),LEGPOS=2

   AXES,XVALS=[0,15,30,45,60,75,90,105,120,135,150,165,180,195,210,225,239]+120+season_start,$
        XLABELS=['1 Sep','16 Sep','1 Oct','16 Oct','1 Nov','16 Nov','1 Dec','16 Dec','1 Jan','16 Jan','1 Feb','16 Feb','1 Mar','16 Mar','1 Apr','16 Apr','30 Apr'],$
        YSTEP=0.1,YMINOR=0.05,NDECS=2,ORIENTATION=45
   PSCLOSE,/NOVIEW

ENDFOR

higem_stbl_precip=REFORM(OPEN_AND_EXTRACT(higem_stbl_infile,'rain_aavg_allqld',$
                                          offset=[season_start,0],$
                                          count=[season_ndays,higem_stbl_nyears]))*86400.

stbl_cumulative=fltarr(season_ndays,higem_stbl_nyears)
FOR j=0,higem_stbl_nyears-1 DO BEGIN
   cumulative=TOTAL(higem_stbl_precip(*,j))
                                ;print,cumulative
   FOR k=0,season_ndays-1 DO $
      stbl_cumulative(k,j)=TOTAL(higem_stbl_precip(0:k,j))/cumulative
ENDFOR

stbl_mean=fltarr(season_ndays)
stbl_median=fltarr(season_ndays)
stbl_iqr=fltarr(2,season_ndays)
stbl_range=fltarr(2,season_ndays)
FOR j=0,season_ndays-1 DO BEGIN
   temp=REFORM(stbl_cumulative(j,*))
   sorted=SORT(temp)
   stbl_median(j)=temp(sorted(higem_stbl_nyears/2))
   stbl_mean(j)=MEAN(temp)
   stbl_iqr(0,j)=temp(sorted(higem_stbl_nyears/4))
   stbl_iqr(1,j)=temp(sorted(higem_stbl_nyears*3/4))
   stbl_range(0,j)=temp(sorted(0))
   stbl_range(1,j)=temp(sorted(higem_stbl_nyears-1))
ENDFOR

higem_control_precip=REFORM(OPEN_AND_EXTRACT(higem_control_infile,'rain_aavg_allqld',$
                                             offset=[season_start,0],$
                                             count=[season_ndays,higem_control_nyears]))*86400.

control_cumulative=fltarr(season_ndays,higem_control_nyears)
FOR j=0,higem_control_nyears-1 DO BEGIN
   cumulative=TOTAL(higem_control_precip(*,j))
                                ;print,cumulative
   FOR k=0,season_ndays-1 DO $
      control_cumulative(k,j)=TOTAL(higem_control_precip(0:k,j))/cumulative
ENDFOR

control_mean=fltarr(season_ndays)
control_median=fltarr(season_ndays)
control_iqr=fltarr(2,season_ndays)
control_range=fltarr(2,season_ndays)
FOR j=0,season_ndays-1 DO BEGIN
   temp=REFORM(control_cumulative(j,*))
   sorted=SORT(temp)
   control_mean(j)=MEAN(temp)
   control_median(j)=temp(sorted(higem_control_nyears/2))
   control_iqr(0,j)=temp(sorted(higem_control_nyears/4))
   control_iqr(1,j)=temp(sorted(higem_control_nyears*3/4))
   control_range(0,j)=temp(sorted(0))
   control_range(1,j)=temp(sorted(higem_control_nyears-1))
ENDFOR

psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/qld_higem_dblco2_precip_percentiles_monsoon_aavg.qld_aavg_cumulative.ps'
PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=1000,TFONT=3,$
       TCHARSIZE=100,SPACE3=500,/PORTRAIT

GSET,XMIN=season_start+120,XMAX=season_stop+120-1,YMIN=0,YMAX=1.1,TITLE='Cumulative Sep-Apr precip for all QLD (area-average)'

GPLOT,X=indgen(season_ndays)+season_start+120.5,Y=stbl_median,COL=FSC_COLOR('red'),THICK=150
GPLOT,X=indgen(season_ndays)+season_start+120.5,Y=stbl_mean,COL=FSC_COLOR('red'),THICK=150,STYLE=3
GPLOT,X=indgen(season_ndays)+season_start+120.5,Y=REFORM(stbl_iqr(0,*)),COL=FSC_COLOR('red'),STYLE=2
GPLOT,X=indgen(season_ndays)+season_start+120.5,Y=REFORM(stbl_iqr(1,*)),COL=FSC_COLOR('red'),STYLE=2
;   GPLOT,X=indgen(season_ndays)+season_start+120.5,Y=REFORM(stbl_range(0,*)),COL=FSC_COLOR('red'),STYLE=1
;   GPLOT,X=indgen(season_ndays)+season_start+120.5,Y=REFORM(stbl_range(1,*)),COL=FSC_COLOR('red'),STYLE=1

GPLOT,X=indgen(season_ndays)+season_start+120.5,Y=control_median,COL=FSC_COLOR('black'),THICK=150
GPLOT,X=indgen(season_ndays)+season_start+120.5,Y=control_mean,COL=FSC_COLOR('black'),THICK=150,STYLE=3
GPLOT,X=indgen(season_ndays)+season_start+120.5,Y=REFORM(control_iqr(0,*)),COL=FSC_COLOR('black'),STYLE=2
GPLOT,X=indgen(season_ndays)+season_start+120.5,Y=REFORM(control_iqr(1,*)),COL=FSC_COLOR('black'),STYLE=2
;   GPLOT,X=indgen(season_ndays)+season_start+120.5,Y=REFORM(control_range(0,*)),COL=FSC_COLOR('black'),STYLE=1
;   GPLOT,X=indgen(season_ndays)+season_start+120.5,Y=REFORM(control_range(1,*)),COL=FSC_COLOR('black'),STYLE=1


GLEGEND,labels=REVERSE(['Median','Mean','Inter-quartile range']),STYLE=REVERSE([0,3,2]),LEGPOS=1
GLEGEND,labels=REVERSE(['HiGEM control','HiGEM 2xCO2']),COL=REVERSE(FSC_COLOR(['black','red'])),LEGPOS=2

AXES,XVALS=[0,15,30,45,60,75,90,105,120,135,150,165,180,195,210,225,239]+120+season_start,$
     XLABELS=['1 Sep','16 Sep','1 Oct','16 Oct','1 Nov','16 Nov','1 Dec','16 Dec','1 Jan','16 Jan','1 Feb','16 Feb','1 Mar','16 Mar','1 Apr','16 Apr','30 Apr'],$
     YSTEP=0.1,YMINOR=0.05,NDECS=2,ORIENTATION=45
PSCLOSE

STOP
END

