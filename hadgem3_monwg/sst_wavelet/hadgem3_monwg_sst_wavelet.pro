PRO hadgem3_monwg_sst_wavelet

; Plot wavelet transforms of SST for the HadGEM3-AO run ahsaf.
; Consider all months in the year and all 30 years of high-frequency
; data available.
; Additionally, generate a probability distribution function for the
; temporally integrated, normalized power in the 30-50 day band that
; is at or above the 95% confidence interval (vs. red noise) during
; the monsoon season.  Output this PDF to a netCDF file.


; Input file containing all months, all years
hadgem3_infile='/home/ss901165/um_output2/hadgem3_monwg/ahsaf/ahsaf.surftemp.daily_20years.nc'

; Box to use for area-averaging data
box=[15,85,20,90]

; Read in grid information
hadgem3_longitude=OPEN_AND_EXTRACT(hadgem3_infile,'longitude')
hadgem3_latitude=OPEN_AND_EXTRACT(hadgem3_infile,'latitude')
DEFINE_BOUNDARIES,box,hadgem3_latitude,hadgem3_longitude,hadgem3_box_tx,/LIMIT
hadgem3_nlon=N_ELEMENTS(hadgem3_longitude)
hadgem3_nlat=N_ELEMENTS(hadgem3_latitude)

; Number of years and days per year
n_years=20
n_days_per_year=360
n_days=n_years*n_days_per_year

offset_may1=120
offset_jun1=150
offset_sep30=269
offset_oct30=299

sst_aavg=fltarr(n_days)
thisyear_sst_aavg=fltarr(n_days_per_year)
isv_sig_power=fltarr(n_years)
FOR i=0,n_years-1 DO BEGIN
    print,'Now calculating intraseasonal power metric for year '+STRTRIM(STRING(i+1),1)+'...'
    thisyear_sst=REFORM(OPEN_AND_EXTRACT(hadgem3_infile,'temp',$
                                     offset=[hadgem3_box_tx(1),hadgem3_box_tx(0),0,0,i],$
                                     count=[hadgem3_nlon,hadgem3_nlat,1,n_days_per_year,1]))
    FOR j=0,n_days_per_year-1 DO $
      thisyear_sst_aavg(j)=MEAN(thisyear_sst(*,*,j))
    sst_aavg(i*n_days_per_year:(i+1)*n_days_per_year-1)=thisyear_sst_aavg
ENDFOR

print,'Now computing wavelet for all years and all months ...'
wave = WAVELET(sst_aavg,1,/pad,lag1=A_CORRELATE(sst_aavg,1),siglvl=0.90,signif=sig90,$
               period=period,scale=scale)
wave = WAVELET(sst_aavg,1,/pad,lag1=A_CORRELATE(sst_aavg,1),siglvl=0.95,signif=sig95,$
               period=period,scale=scale,coi=coi)

power=ABS(wave^2)

n_scale=N_ELEMENTS(scale)
isv_start=NEAREST(period,30)
isv_stop=NEAREST(period,50)
sig90_allyears = REBIN(TRANSPOSE(sig90),n_days,n_scale)
sig95_allyears = REBIN(TRANSPOSE(sig95),n_days,n_scale)

mylevs=['0.01','0.03','0.06','0.12','0.25','0.50','1.00','2.00','4.00','8.00','16.00','32.00']
xvals=indgen((offset_oct30-offset_may1)/15+1)*15+offset_may1
xlabels=['1 May','16 May','1 Jun','16 Jun','1 Jul','16 Jul','1 Aug','16 Aug','1 Sep','16 Sep','1 Oct','16 Oct']
yvals=['3','5','7','10','15','20','30','40','50','70','100','150']
FOR i=0,n_years-1 DO BEGIN
    year_start_char=STRTRIM(STRING(i+1),1)
    ;psfile='/home/ss901165/idl/hadgem3_monwg/sst_wavelet/hadgem3_monwg_sst_wavelet.ahsaf.year'+$
    ;  year_start_char+'.NBoB.ps'
    ;PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
    ;CS,IDL=3,NCOLS=N_ELEMENTS(mylevs)+1,/REV
    ;GSET,XMIN=offset_may1,XMAX=offset_oct30,YMIN=150,YMAX=MIN(period),/YLOG
    ;LEVS,MANUAL=mylevs
    ;CON,FIELD=power(i*n_days_per_year:(i+1)*n_days_per_year-1,*),X=indgen(n_days_per_year),Y=period,$
    ;  TITLE="Wavelet transform of rainfall in the northern BoB (15-20N, 85-90E) for year "+$
    ;  year_start_char,/NOLINES
    ;LEVS,MANUAL=['0','95']
    ;white=FSC_COLOR("white",50)
    ;CON,FIELD=power(i*n_days_per_year:(i+1)*n_days_per_year-1,*)/$
    ;  sig95_allyears(i*n_days_per_year:(i+1)*n_days_per_year-1,*)*95.,X=indgen(n_days_per_year),Y=period,$
    ;  /NOFILL,/NOCOLBAR,COL=[50,50,50,50]
    ;LEVS,MANUAL=['0','90']
    ;CON,FIELD=power(i*n_days_per_year:(i+1)*n_days_per_year-1,*)/$
    ;  sig90_allyears(i*n_days_per_year:(i+1)*n_days_per_year-1,*)*90.,X=indgen(n_days_per_year),Y=period,$
    ;  /NOFILL,/NOCOLBAR,COL=[50,50,50,50]
    ;AXES,XVALS=xvals,xlabels=xlabels,YVALS=yvals
    ;PSCLOSE,/NOVIEW

    IF i eq 0 THEN BEGIN
        n_scale=N_ELEMENTS(scale)
        power_ensmean=fltarr(n_days_per_year,n_scale)
        sig90_ensmean=fltarr(n_days_per_year,n_scale)
        sig95_ensmean=fltarr(n_days_per_year,n_scale)
        power_ensmean(*,*)=0.
        isv_start=NEAREST(period,30)
        isv_stop=NEAREST(period,50)
    ENDIF

    this_power=power(i*n_days_per_year:(i+1)*n_days_per_year-1,*)
    this_sig95=sig95_allyears(i*n_days_per_year:(i+1)*n_days_per_year-1,*)
    sig_power=this_power/this_sig95
    sig_power=sig_power(offset_jun1:offset_sep30,isv_start:isv_stop)
    IF (TOTAL(where(sig_power gt 1)) gt -1) THEN $
      isv_sig_power(i)=TOTAL(sig_power(where(sig_power gt 1)))
    print,'For year '+year_start_char+' 30-50 day integrated normalized power = '+STRTRIM(STRING(isv_sig_power(i)),1)

    power_ensmean(*,*)=power_ensmean(*,*)+power(i*n_days_per_year:(i+1)*n_days_per_year-1,*)*1./FLOAT(n_years)
    sig90_ensmean(*,*)=sig90_ensmean(*,*)+sig90_allyears(i*n_days_per_year:(i+1)*n_days_per_year-1,*)*1./FLOAT(n_years)
    sig95_ensmean(*,*)=sig95_ensmean(*,*)+sig95_allyears(i*n_days_per_year:(i+1)*n_days_per_year-1,*)*1./FLOAT(n_years)

ENDFOR

psfile='/home/ss901165/idl/hadgem3_monwg/sst_wavelet/hadgem3_monwg_sst_wavelet.ahsaf_mean.NBoB.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=800,XOFFSET=1000,YOFFSET=100,TFONT=2,TCHARSIZE=110
CS,IDL=3,NCOLS=N_ELEMENTS(mylevs)+1,/REV
GSET,XMIN=offset_may1,XMAX=offset_oct30,YMIN=150,YMAX=MIN(period),/YLOG
LEVS,MANUAL=mylevs
CON,FIELD=power_ensmean,X=indgen(n_days_per_year),Y=period,$
  TITLE="Mean wavelet transform of SST in the northern BoB (15-20N, 85-90E) for HadGEM3-AO_ahsaf_n96 (20 years)",/NOLINES
LEVS,MANUAL=['0','95']
white=FSC_COLOR("white",50)
black=FSC_COLOR("cyan",51)
n_time_plot=offset_oct30-offset_may1+1
GPLOT,X=indgen(n_time_plot)+offset_may1,Y=REPLICATE(30,n_time_plot),COL=51,THICK=200
GPLOT,X=indgen(n_time_plot)+offset_may1,Y=REPLICATE(50,n_time_plot),COL=51,THICK=200
period_plot=period[where(period le 150)]
GPLOT,X=REPLICATE(offset_jun1,N_ELEMENTS(period_plot)),Y=period_plot,COL=51,THICK=200
GPLOT,X=REPLICATE(offset_sep30,N_ELEMENTS(period_plot)),Y=period_plot,COL=51,THICK=200
;sig95_ensmean=REBIN(TRANSPOSE(sig95_ensmean),n_days_per_year,n_scale)
;sig90_ensmean=REBIN(TRANSPOSE(sig90_ensmean),n_days_per_year,n_scale)
AXES,XVALS=xvals,xlabels=xlabels,YVALS=yvals,xtitle='Day',ytitle='Period (days)'
CON,FIELD=power_ensmean/sig95_ensmean*95,X=indgen(n_days_per_year),Y=period,/NOFILL,/NOCOLBAR,COL=[50,50,50,50]
LEVS,MANUAL=['0','90']
CON,FIELD=power_ensmean/sig90_ensmean*90,X=indgen(n_days_per_year),Y=period,/NOFILL,/NOCOLBAR,COL=[50,50,50,50]
PSCLOSE

pdf,isv_sig_power,/BANDWIDTH,NPDF=1000,pdf=isv_pdf,xid=xaxis,xrange=[0,3000]

out_id=NCDF_CREATE('/home/ss901165/idl/hadgem3_monwg/sst_wavelet/hadgem3_monwg_isv_power_pdf.ahsaf.NBoB.nc',/CLOBBER)
out_dimid=NCDF_DIMDEF(out_id,'pdf_axis',N_ELEMENTS(xaxis))
out_varids=intarr(2)
out_varids(0)=NCDF_VARDEF(out_id,'pdf_axis',[out_dimid])
out_varids(1)=NCDF_VARDEF(out_id,'pdf_value',[out_dimid])
NCDF_ATTPUT,out_id,out_varids(0),'long_name','Horizontal axis for the PDF graph (values of ISV power)'
NCDF_ATTPUT,out_id,out_varids(0),'units','power'
NCDF_ATTPUT,out_id,out_varids(1),'long_name','Probability distribution function for power of 30-50 day variability of SST in the northern Bay of Bengal'
NCDF_ATTPUT,out_id,out_varids(1),'units','dimensionless'
NCDF_ATTPUT,out_id,out_varids(1),'area','15-20N, 85-90E'
NCDF_CONTROL,out_id,/ENDEF
NCDF_VARPUT,out_id,out_varids(0),xaxis
NCDF_VARPUT,out_id,out_varids(1),isv_pdf
NCDF_CLOSE,out_id

STOP

END
