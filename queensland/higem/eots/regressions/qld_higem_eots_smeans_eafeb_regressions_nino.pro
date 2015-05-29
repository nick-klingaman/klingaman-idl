PRO qld_higem_eots_smeans_eafeb_regressions_nino

eots_basedir='/home/ss901165/higem_qccce/es_control_eafeb'
nino3_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.jan-dec_mmeans.h9-w8.nino_indices.nc'
nino4_infile=nino3_infile
nino34_infile=nino3_infile

n_eots=4
n_seasons=4
nino_offset=0
eot_offset=1
n_years=147
sig_level=0.178

nino3_ts=OPEN_AND_EXTRACT(nino3_infile,'nino3',offset=[nino_offset],count=[(n_years+2)*12])
nino4_ts=OPEN_AND_EXTRACT(nino4_infile,'nino4',offset=[nino_offset],count=[(n_years+2)*12])
nino34_ts=OPEN_AND_EXTRACT(nino34_infile,'nino34',offset=[nino_offset],count=[(n_years+2)*12])
nino3_ts_bymonth=fltarr(12,n_years+2)
nino4_ts_bymonth=fltarr(12,n_years+2)
nino34_ts_bymonth=fltarr(12,n_years+2)
FOR i=0,11 DO BEGIN
   FOR j=i,(n_years+2)*12-1,12 DO BEGIN
      nino3_ts_bymonth(i,j/12)=nino3_ts(j)
      nino4_ts_bymonth(i,j/12)=nino4_ts(j)
      nino34_ts_bymonth(i,j/12)=nino34_ts(j)
   ENDFOR
ENDFOR

xaxis_labels=STRARR(36)
xaxis_labels(0:11)=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
xaxis_labels(12:23)=xaxis_labels(0:11)+' 0'
xaxis_labels(24:35)=xaxis_labels(0:11)+' +1'
xaxis_labels(0:11)=xaxis_labels(0:11)+' -1'

FOR i=0,n_seasons-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         season_name='dec-feb'
         center_month=24
         eots_to_plot=[1]
      END
      1 : BEGIN
         season_name='mar-may'
         center_month=15
         eots_to_plot=[3]
      END
      2 : BEGIN
         season_name='jun-aug'
         center_month=18
         eots_to_plot=[1]
      END
      3 : BEGIN
         season_name='sep-nov'
         center_month=21
         eots_to_plot=[1,3]
      END
   ENDCASE
   
   eots_infile=eots_basedir+'/higem_eafeb.'+season_name+'_smeans.h9-w8.eots.nc'
   eots_loadings=REFORM(OPEN_AND_EXTRACT(eots_infile,'loading',$
                                         offset=[eot_offset,0],count=[n_years,n_eots]))
   
   eot_nino3_correlations=fltarr(n_eots,36)
   eot_nino3_regressions=fltarr(n_eots,36)
   eot_nino4_correlations=fltarr(n_eots,36)
   eot_nino4_regressions=fltarr(n_eots,36)
   eot_nino34_correlations=fltarr(n_eots,36)
   eot_nino34_regressions=fltarr(n_eots,36)
   FOR j=0,n_eots-1 DO BEGIN
      thiseot_ts=REFORM(eots_loadings(*,j))
      FOR k=0,11 DO BEGIN
         
         thisnino3_ts=REFORM(nino3_ts_bymonth(k,0:n_years-1))
         temp=REGRESS(thiseot_ts,thisnino3_ts,CORRELATION=temp2)*STDDEV(thiseot_ts)
         eot_nino3_correlations(j,k)=temp2(0)
         eot_nino3_regressions(j,k)=temp(0)
         thisnino3_ts=REFORM(nino3_ts_bymonth(k,1:n_years))
         temp=REGRESS(thiseot_ts,thisnino3_ts,CORRELATION=temp2)*STDDEV(thiseot_ts)
         eot_nino3_correlations(j,k+12)=temp2(0)
         eot_nino3_regressions(j,k+12)=temp(0)
         thisnino3_ts=REFORM(nino3_ts_bymonth(k,2:n_years+1))
         temp=REGRESS(thiseot_ts,thisnino3_ts,CORRELATION=temp2)*STDDEV(thiseot_ts)
         eot_nino3_correlations(j,k+24)=temp2(0)
         eot_nino3_regressions(j,k+24)=temp(0)

         thisnino4_ts=REFORM(nino4_ts_bymonth(k,0:n_years-1))
         temp=REGRESS(thiseot_ts,thisnino4_ts,CORRELATION=temp2)*STDDEV(thiseot_ts)
         eot_nino4_correlations(j,k)=temp2(0)
         eot_nino4_regressions(j,k)=temp(0)
         thisnino4_ts=REFORM(nino4_ts_bymonth(k,1:n_years))
         temp=REGRESS(thiseot_ts,thisnino4_ts,CORRELATION=temp2)*STDDEV(thiseot_ts)
         eot_nino4_correlations(j,k+12)=temp2(0)
         eot_nino4_regressions(j,k+12)=temp(0)
         thisnino4_ts=REFORM(nino4_ts_bymonth(k,2:n_years+1))
         temp=REGRESS(thiseot_ts,thisnino4_ts,CORRELATION=temp2)*STDDEV(thiseot_ts)
         eot_nino4_correlations(j,k+24)=temp2(0)
         eot_nino4_regressions(j,k+24)=temp(0)

         thisnino34_ts=REFORM(nino34_ts_bymonth(k,0:n_years-1))
         temp=REGRESS(thiseot_ts,thisnino34_ts,CORRELATION=temp2)*STDDEV(thiseot_ts)
         eot_nino34_correlations(j,k)=temp2(0)
         eot_nino34_regressions(j,k)=temp(0)
         thisnino34_ts=REFORM(nino34_ts_bymonth(k,1:n_years))
         temp=REGRESS(thiseot_ts,thisnino34_ts,CORRELATION=temp2)*STDDEV(thiseot_ts)
         eot_nino34_correlations(j,k+12)=temp2(0)
         eot_nino34_regressions(j,k+12)=temp(0)
         thisnino34_ts=REFORM(nino34_ts_bymonth(k,2:n_years+1))
         temp=REGRESS(thiseot_ts,thisnino34_ts,CORRELATION=temp2)*STDDEV(thiseot_ts)
         eot_nino34_correlations(j,k+24)=temp2(0)
         eot_nino34_regressions(j,k+24)=temp(0)

      ENDFOR
   ENDFOR
   
   psfile='/home/ss901165/idl/queensland/higem/eots/regressions/qld_higem_eots_smeans_eafeb_regressions_nino.'+season_name+'_allnino.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1800,SPACE3=300,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=112
   GSET,XMIN=-11,XMAX=11,YMIN=-0.60,YMAX=0.3,TITLE='Regression of monthly-mean Nino SSTs onto EOTs of '+season_name+' total rainfall from HiGEM eafeb - h9-w8'
   colors=['red','purple','blue']
   FOR j=0,n_eots-1 DO BEGIN
      IF where(eots_to_plot eq j+1) ne -1 THEN BEGIN
         GPLOT,X=indgen(23)-11,Y=REFORM(eot_nino3_regressions(j,center_month-11:center_month+11)),STYLE=where(eots_to_plot eq j+1),COL=FSC_COLOR(colors(0))
         GPLOT,X=indgen(23)-11,Y=REFORM(eot_nino34_regressions(j,center_month-11:center_month+11)),STYLE=where(eots_to_plot eq j+1),COL=FSC_COLOR(colors(1))
         GPLOT,X=indgen(23)-11,Y=REFORM(eot_nino4_regressions(j,center_month-11:center_month+11)),STYLE=where(eots_to_plot eq j+1),COL=FSC_COLOR(colors(2))
         FOR k=center_month-11,center_month+11 DO BEGIN
            IF ABS(eot_nino3_correlations(j,k)) ge sig_level THEN $
               GPLOT,X=k-center_month,Y=eot_nino3_regressions(j,k),SYM=3+where(eots_to_plot eq j+1),SIZE=100,COL=FSC_COLOR(colors(0))
            IF ABS(eot_nino34_correlations(j,k)) ge sig_level THEN $
               GPLOT,X=k-center_month,Y=eot_nino34_regressions(j,k),SYM=3+where(eots_to_plot eq j+1),SIZE=100,COL=FSC_COLOR(colors(1))
            IF ABS(eot_nino4_correlations(j,k)) ge sig_level THEN $
               GPLOT,X=k-center_month,Y=eot_nino4_regressions(j,k),SYM=3+where(eots_to_plot eq j+1),SIZE=100,COL=FSC_COLOR(colors(2))
         ENDFOR
      ENDIF
   ENDFOR
   GPLOT,X=[0,0],Y=[0.30,-0.60],THICK=70
   GPLOT,X=[-11,11],Y=[0,0],THICK=70
   AXES,XSTEP=1,YSTEP=0.05,YMINOR=0.025,NDECS=2,XTITLE='Month (year relative to beginning of season for EOT of rainfall)',YTITLE='Coefficient of regression of Nino 4 onto rainfall EOT (!Uo!NC per 1*stddev change in EOT)',XVALS=indgen(23)-11,XLABELS=xaxis_labels(center_month-11:center_month+11),ORIENTATION=20
   IF N_ELEMENTS(eots_to_plot) gt 1 THEN BEGIN
      labels=strarr(N_ELEMENTS(eots_to_plot))
      FOR j=0,N_ELEMENTS(eots_to_plot)-1 DO $
         labels(j)='Rainfall EOT '+STRTRIM(STRING(j+1),1)      
      GLEGEND,labels=REVERSE(labels),style=REVERSE(indgen(N_ELEMENTS(eots_to_plot))),LEGPOS=1,SYM=REVERSE(indgen(N_ELEMENTS(eots_to_plot))+3)
   ENDIF
   GLEGEND,labels=REVERSE(['Nino 3','Nino 3.4','Nino 4']),style=REPLICATE(0,3),LEGPOS=11,COL=REVERSE(FSC_COLOR(colors))
   PSCLOSE

;   STOP
ENDFOR

STOP
END
