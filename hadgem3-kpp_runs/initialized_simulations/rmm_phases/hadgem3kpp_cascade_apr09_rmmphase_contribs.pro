PRO hadgem3kpp_cascade_apr09_rmmphase_contribs

analysis_input_file='/home/ss901165/datasets_mango/UM_ANALYSES/cascade_mjo_wh04/metum_analysis.dec-may_dmeans.2008-2009.rmm_indices.nc'
analysis_time_offset=126
analysis_ndays=30

analysis_rmm1=REFORM(OPEN_AND_EXTRACT(analysis_input_file,'rmm1',$
                                      offset=[0,analysis_time_offset],count=[1,analysis_ndays]))
analysis_rmm2=REFORM(OPEN_AND_EXTRACT(analysis_input_file,'rmm2',$
                                      offset=[0,analysis_time_offset],count=[1,analysis_ndays]))
analysis_contrib_rmm1_olr=REFORM(OPEN_AND_EXTRACT(analysis_input_file,'contrib_rmm1_olr',$
                                                  offset=[0,analysis_time_offset],count=[1,analysis_ndays]))
analysis_contrib_rmm1_u850=REFORM(OPEN_AND_EXTRACT(analysis_input_file,'contrib_rmm1_u850',$
                                                   offset=[0,analysis_time_offset],count=[1,analysis_ndays]))
analysis_contrib_rmm1_u200=REFORM(OPEN_AND_EXTRACT(analysis_input_file,'contrib_rmm1_u200',$
                                                   offset=[0,analysis_time_offset],count=[1,analysis_ndays]))
analysis_contrib_rmm2_olr=REFORM(OPEN_AND_EXTRACT(analysis_input_file,'contrib_rmm2_olr',$
                                                  offset=[0,analysis_time_offset],count=[1,analysis_ndays]))
analysis_contrib_rmm2_u850=REFORM(OPEN_AND_EXTRACT(analysis_input_file,'contrib_rmm2_u850',$
                                                   offset=[0,analysis_time_offset],count=[1,analysis_ndays]))
analysis_contrib_rmm2_u200=REFORM(OPEN_AND_EXTRACT(analysis_input_file,'contrib_rmm2_u200',$
                                                   offset=[0,analysis_time_offset],count=[1,analysis_ndays]))

n_models=4
model_ndays=30
model_rmm1=fltarr(n_models,model_ndays)
model_rmm2=fltarr(n_models,model_ndays)
model_contrib_rmm1_olr=fltarr(n_models,model_ndays)
model_contrib_rmm1_u850=fltarr(n_models,model_ndays)
model_contrib_rmm1_u200=fltarr(n_models,model_ndays)
model_contrib_rmm2_olr=fltarr(n_models,model_ndays)
model_contrib_rmm2_u850=fltarr(n_models,model_ndays)
model_contrib_rmm2_u200=fltarr(n_models,model_ndays)
model_colors=strarr(n_models)
model_shortnames=strarr(n_models)
model_styles=intarr(n_models)
model_symbols=intarr(n_models)
model_runids=strarr(n_models)

um3='/home/ss901165/um_output3'
FOR i=0,n_models-1 DO BEGIN
   CASE i OF 
;      0 : BEGIN
;         runid='xfadd'
;         dirname='xfadd'
;         model_time_offset=0
;         color='red'
;         style=0
;         symbol=6
;      END
;      1 : BEGIN
;         runid='xfade'
;         dirname='xfade'
;         model_time_offset=0
;         color='red'
;         style=1
;         symbol=3
;      END
      0 : BEGIN
         runid='xfadh'
         shortname='Control'
         dirname='xfadh.old_stash'
         model_time_offset=0
         color='red'
         style=0
         symbol=6
      END
      1 : BEGIN
         runid='xfadl'
         shortname='No CMT'
         dirname='xfadl'
         model_time_offset=0
         color='red'
         style=1
         symbol=3
      END
      2 : BEGIN
         runid='xfadk'
         shortname='1.5x ent'
         dirname='xfadk.old_stash'
         model_time_offset=0
         color='blue'
         style=0
         symbol=6
      END
      3 : BEGIN
         runid='xfadm'
         shortname='No CMT'
         dirname='xfadm.old_stash'
         model_time_offset=0
         color='blue'
         style=1
         symbol=3
      END
   ENDCASE
   model_input_file=um3+'/'+dirname+'/'+runid+'.06apr09.rmm_indices.nc'
   model_rmm1(i,*)=REFORM(OPEN_AND_EXTRACT(model_input_file,'rmm1',$
                                           offset=[0,model_time_offset],count=[1,model_ndays]))
   model_rmm2(i,*)=REFORM(OPEN_AND_EXTRACT(model_input_file,'rmm2',$
                                           offset=[0,model_time_offset],count=[1,model_ndays]))
   model_contrib_rmm1_olr(i,*)=REFORM(OPEN_AND_EXTRACT(model_input_file,'contrib_rmm1_olr',$
                                                       offset=[0,model_time_offset],count=[1,model_ndays]))
   model_contrib_rmm1_u850(i,*)=REFORM(OPEN_AND_EXTRACT(model_input_file,'contrib_rmm1_u850',$
                                                        offset=[0,model_time_offset],count=[1,model_ndays]))
   model_contrib_rmm1_u200(i,*)=REFORM(OPEN_AND_EXTRACT(model_input_file,'contrib_rmm1_u200',$
                                                        offset=[0,model_time_offset],count=[1,model_ndays]))
   model_contrib_rmm2_olr(i,*)=REFORM(OPEN_AND_EXTRACT(model_input_file,'contrib_rmm2_olr',$
                                                       offset=[0,model_time_offset],count=[1,model_ndays]))
   model_contrib_rmm2_u850(i,*)=REFORM(OPEN_AND_EXTRACT(model_input_file,'contrib_rmm2_u850',$
                                                        offset=[0,model_time_offset],count=[1,model_ndays]))
   model_contrib_rmm2_u200(i,*)=REFORM(OPEN_AND_EXTRACT(model_input_file,'contrib_rmm2_u200',$
                                                        offset=[0,model_time_offset],count=[1,model_ndays]))
   model_colors(i)=color
   model_styles(i)=style
   model_symbols(i)=symbol
   model_runids(i)=runid
   model_shortnames(i)=shortname
ENDFOR

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/rmm_phases/hadgem3kpp_cascade_apr09_rmmphase_contribs.rmm1_16days.ps'
PSOPEN,file=psfile,FONT=4,BOLD=1,CHARSIZE=80,MARGIN=1500,XOFFSET=1000,YOFFSET=3500,TFONT=4,TCHARSIZE=90,YPLOTS=4,YSPACING=900

FOR j=0,3 DO BEGIN
   CASE j OF 
      0 : BEGIN
         pos=4
         ymin=-2.4
         ymax=2.4
         rmse_xpos=0.5
         rmse_ypos=1.8
         rmse_xspace=3.5
         rmse_yspace=-0.6
         ystep=0.8
         yminor=0.4
         ytitle='RMM1'
         model_ts=model_rmm1
         analysis_ts=analysis_rmm1         
      END
      1 : BEGIN
         pos=3
         ymin=-0.45
         ymax=0.5
         rmse_xpos=0.5
         rmse_ypos=-0.2
         rmse_xspace=3.5
         rmse_yspace=-0.1
         ystep=0.15
         yminor=0.075
         ytitle='RMM1 from OLR'
         model_ts=model_contrib_rmm1_olr
         analysis_ts=analysis_contrib_rmm1_olr
      END
      2 : BEGIN
         pos=2
         ymin=-1.2
         ymax=1.2
         rmse_xpos=0.5
         rmse_ypos=0.9
         rmse_xspace=3.5
         rmse_yspace=-0.25
         ystep=0.4
         yminor=0.2
         ytitle='RMM1 from U850'
         model_ts=model_contrib_rmm1_u850
         analysis_ts=analysis_contrib_rmm1_u850
      END
      3 : BEGIN
         pos=1
         ymin=-0.9
         ymax=1.5
         rmse_xpos=0.5
         rmse_ypos=1.2
         rmse_xspace=3.5
         rmse_yspace=-0.3
         ystep=0.5
         yminor=0.25
         ytitle='RMM1 from U200'
         model_ts=model_contrib_rmm1_u200
         analysis_ts=analysis_contrib_rmm1_u200
      END
   ENDCASE
   
   POS,YPOS=pos
   GSET,XMIN=0,XMAX=16,YMIN=ymin,YMAX=ymax
   black=FSC_COLOR("black",3)
   GPLOT,X=indgen(16)+0.5,Y=analysis_ts(0:15),COL=3,STYLE=0,SYM=4,SIZE=60
   GPLOT,X=rmse_xpos+1.5,Y=rmse_ypos,$
         TEXT='RMSE',ALIGN=0.0,COL=FSC_COLOR("black"),/BOLD
   GPLOT,X=rmse_xpos+2.2,Y=rmse_ypos,$
         TEXT='Corr',ALIGN=0.0,COL=FSC_COLOR("black"),/BOLD
   GPLOT,X=rmse_xpos+rmse_xspace+1.5,Y=rmse_ypos,$
         TEXT='RMSE',ALIGN=0.0,COL=FSC_COLOR("black"),/BOLD
   GPLOT,X=rmse_xpos+rmse_xspace+2.2,Y=rmse_ypos,$
         TEXT='Corr',ALIGN=0.0,COL=FSC_COLOR("black"),/BOLD
   
   FOR i=0,n_models-1 DO BEGIN
      this_color=FSC_COLOR(model_colors(i),i+4)
      GPLOT,X=indgen(16)+0.5,Y=REFORM(model_ts(i,0:15)),COL=i+4,STYLE=REFORM(model_styles(i)),SYM=model_symbols(i),SIZE=60,THICK=90
      
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i),Y=rmse_ypos+rmse_yspace*(i/2+1),$
            TEXT=model_shortnames(i)+': ',ALIGN=0.0,COL=i+4,/BOLD
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.5,Y=rmse_ypos+rmse_yspace*(i/2+1),$
            TEXT=STRMID(STRTRIM(STRING(SQRT(MEAN((model_ts(i,0:15)-$
                                                  analysis_ts(0:15))^2))),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
      this_correlation=CORRELATE(model_ts(i,0:15),analysis_ts(0:15))
      IF this_correlation ge 0 THEN BEGIN
         GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+2.2,Y=rmse_ypos+rmse_yspace*(i/2+1),$
               TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
      ENDIF ELSE $
         GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+2.2,Y=rmse_ypos+rmse_yspace*(i/2+1),$
               TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,5),ALIGN=0.0,COL=i+4,/BOLD
   ENDFOR
   GPLOT,X=[0,16],Y=[0,0],STYLE=2,COL=3,THICK=50
   AXES,XMINOR=1,YSTEP=ystep,YMINOR=yminor,YTITLE=ytitle,$
        XVALS=indgen(9)*2,XLABELS=[STRTRIM(STRING(indgen(9)*2+6),1)+' Apr 2009'],NDECS=2
ENDFOR
LEGEND,labels=['Without CMT','With CMT'],COL=[3,3],SYM=[3,6],LEGXOFFSET=-5000,LEGYOFFSET=-300,SIZE=60,STYLE=[1,0]
LEGEND,labels=['1.5x entrainment','1.0x entrainment','Analysis'],COL=[n_models+2,n_models,3],SYM=[6,6,4],$
       LEGXOFFSET=-10000,LEGYOFFSET=-300,SIZE=60

PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/rmm_phases/hadgem3kpp_cascade_apr09_rmmphase_contribs.rmm2_16days.ps'
PSOPEN,file=psfile,FONT=4,BOLD=1,CHARSIZE=80,MARGIN=1500,XOFFSET=1000,YOFFSET=3500,TFONT=4,TCHARSIZE=90,YPLOTS=4,YSPACING=900

FOR j=0,3 DO BEGIN
   CASE j OF 
      0 : BEGIN
         pos=4
         ymin=-3.2
         ymax=1.2
         rmse_xpos=0.5
         rmse_ypos=0.2
         rmse_xspace=3.5
         rmse_yspace=-0.6
         ystep=0.8
         yminor=0.4
         ytitle='RMM2'
         model_ts=model_rmm2
         analysis_ts=analysis_rmm2         
      END
      1 : BEGIN
         pos=3
         ymin=-1.2
         ymax=0.6
         rmse_xpos=0.5
         rmse_ypos=0.4
         rmse_xspace=3.5
         rmse_yspace=-0.2
         ystep=0.3
         yminor=0.15
         ytitle='RMM2 from OLR'
         model_ts=model_contrib_rmm2_olr
         analysis_ts=analysis_contrib_rmm2_olr
      END
      2 : BEGIN
         pos=2
         ymin=-1.5
         ymax=0.3
         rmse_xpos=0.5
         rmse_ypos=-0.1
         rmse_xspace=3.5
         rmse_yspace=-0.2
         ystep=0.3
         yminor=0.15
         ytitle='RMM2 from U850'
         model_ts=model_contrib_rmm2_u850
         analysis_ts=analysis_contrib_rmm2_u850
      END
      3 : BEGIN
         pos=1
         ymin=-1.4
         ymax=1.1
         rmse_xpos=0.5
         rmse_ypos=0.8
         rmse_xspace=3.5
         rmse_yspace=-0.3
         ystep=0.35
         yminor=0.175
         ytitle='RMM2 from U200'
         model_ts=model_contrib_rmm2_u200
         analysis_ts=analysis_contrib_rmm2_u200
      END
   ENDCASE
   
   POS,YPOS=pos
   GSET,XMIN=0,XMAX=16,YMIN=ymin,YMAX=ymax
   black=FSC_COLOR("black",3)
   GPLOT,X=indgen(16)+0.5,Y=analysis_ts(0:15),COL=3,STYLE=0,SYM=4,SIZE=60
   GPLOT,X=rmse_xpos+1.5,Y=rmse_ypos,$
         TEXT='RMSE',ALIGN=0.0,COL=FSC_COLOR("black"),/BOLD
   GPLOT,X=rmse_xpos+2.2,Y=rmse_ypos,$
         TEXT='Corr',ALIGN=0.0,COL=FSC_COLOR("black"),/BOLD
   GPLOT,X=rmse_xpos+rmse_xspace+1.5,Y=rmse_ypos,$
         TEXT='RMSE',ALIGN=0.0,COL=FSC_COLOR("black"),/BOLD
   GPLOT,X=rmse_xpos+rmse_xspace+2.2,Y=rmse_ypos,$
         TEXT='Corr',ALIGN=0.0,COL=FSC_COLOR("black"),/BOLD
   
   FOR i=0,n_models-1 DO BEGIN
      this_color=FSC_COLOR(model_colors(i),i+4)
      GPLOT,X=indgen(16)+0.5,Y=REFORM(model_ts(i,0:15)),COL=i+4,STYLE=REFORM(model_styles(i)),SYM=model_symbols(i),SIZE=60,THICK=90
      
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i),Y=rmse_ypos+rmse_yspace*(i/2+1),$
            TEXT=model_shortnames(i)+': ',ALIGN=0.0,COL=i+4,/BOLD
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.5,Y=rmse_ypos+rmse_yspace*(i/2+1),$
            TEXT=STRMID(STRTRIM(STRING(SQRT(MEAN((model_ts(i,0:15)-$
                                                  analysis_ts(0:15))^2))),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
      this_correlation=CORRELATE(model_ts(i,0:15),analysis_ts(0:15))
      IF this_correlation ge 0 THEN BEGIN
         GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+2.2,Y=rmse_ypos+rmse_yspace*(i/2+1),$
               TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
      ENDIF ELSE $
         GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+2.2,Y=rmse_ypos+rmse_yspace*(i/2+1),$
               TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,5),ALIGN=0.0,COL=i+4,/BOLD
   ENDFOR
   GPLOT,X=[0,16],Y=[0,0],STYLE=2,COL=3,THICK=50
   AXES,XMINOR=1,YSTEP=ystep,YMINOR=yminor,YTITLE=ytitle,$
        XVALS=indgen(9)*2,XLABELS=[STRTRIM(STRING(indgen(9)*2+6),1)+' Apr 2009'],NDECS=2
ENDFOR
LEGEND,labels=['Without CMT','With CMT'],COL=[3,3],SYM=[3,6],LEGXOFFSET=-5000,LEGYOFFSET=-300,SIZE=60,STYLE=[1,0]
LEGEND,labels=['1.5x entrainment','1.0x entrainment','Analysis'],COL=[n_models+2,n_models,3],SYM=[6,6,4],$
       LEGXOFFSET=-10000,LEGYOFFSET=-300,SIZE=60

PSCLOSE

STOP
END

