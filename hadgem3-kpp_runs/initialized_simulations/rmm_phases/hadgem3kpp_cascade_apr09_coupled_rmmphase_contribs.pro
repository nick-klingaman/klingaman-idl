PRO hadgem3kpp_cascade_apr09_coupled_rmmphase_contribs

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
model_styles=intarr(n_models)
model_symbols=intarr(n_models)
model_runids=strarr(n_models)
um3='/home/ss901165/um_output3'
FOR i=0,n_models-1 DO BEGIN
   CASE i OF 
      0 : BEGIN
         runid='xfadh'
         dirname='xfadh.old_stash'
         model_time_offset=0
         color='red'
         style=0
         symbol=6
      END
      1 : BEGIN
         runid='xfadl'
         dirname='xfadl'
         model_time_offset=0
         color='red'
         style=1
         symbol=3
      END
      2 : BEGIN
         runid='xfadk'
         dirname='xfadk.old_stash'
         model_time_offset=0
         color='blue'
         style=0
         symbol=6
      END
      3: BEGIN
         runid='xfadm'
         dirname='xfadm.old_stash'
         model_time_offset=0
         color='blue'
         style=1
         symbol=3
      END
      4 : BEGIN
         runid='xfadm'
         dirname='xfadm'
         model_time_offset=0
         color='blue'
         style=0
         symbol=6
      END
      5 : BEGIN
         runid='xfadr'
         dirname='xfadr'
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
ENDFOR

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/rmm_phases/hadgem3kpp_cascade_apr09_coupled_rmmphase_contribs.rmm1_16days.ps'
PSOPEN,file=psfile,FONT=4,BOLD=1,CHARSIZE=110,MARGIN=1500,XOFFSET=1000,YOFFSET=3500,TFONT=4,TCHARSIZE=90,YPLOTS=4,YSPACING=900

POS,YPOS=4
GSET,XMIN=0,XMAX=16,YMIN=-2.4,YMAX=2.4
black=FSC_COLOR("black",3)
GPLOT,X=indgen(16)+0.5,Y=analysis_rmm1(0:15),COL=3,STYLE=0,SYM=4,SIZE=40
rmse_xpos=0.5
rmse_ypos=1.8
rmse_xspace=2.5
rmse_yspace=-0.6
FOR i=0,n_models-1 DO BEGIN
   this_color=FSC_COLOR(model_colors(i),i+4)
   GPLOT,X=indgen(16)+0.5,Y=REFORM(model_rmm1(i,0:15)),COL=i+4,STYLE=REFORM(model_styles(i)),SYM=model_symbols(i),SIZE=40,THICK=70
   GPLOT,X=rmse_xpos+rmse_xspace*ODD(i),Y=rmse_ypos+rmse_yspace*(i/2),$
         TEXT=model_runids(i)+': '+STRMID(STRTRIM(STRING(SQRT(MEAN((model_rmm1(i,0:15)-$
                                                                    analysis_rmm1(0:15))^2))),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   this_correlation=CORRELATE(model_rmm1(i,0:15),analysis_rmm1(0:15))
   IF this_correlation ge 0 THEN BEGIN
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.4,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   ENDIF ELSE $
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.4,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,5),ALIGN=0.0,COL=i+4,/BOLD
ENDFOR
GPLOT,X=[0,16],Y=[0,0],STYLE=2,COL=3,THICK=50
AXES,XSTEP=2,XMINOR=1,YSTEP=0.8,YMINOR=0.4,YTITLE='RMM1',NDECS=2

POS,YPOS=3
GSET,XMIN=0,XMAX=16,YMIN=-0.20,YMAX=0.40;,TITLE='Contribution from OLR'
black=FSC_COLOR("black",3)
GPLOT,X=indgen(16)+0.5,Y=analysis_contrib_rmm1_olr(0:15),COL=3,STYLE=0,SYM=4,SIZE=40
rmse_xpos=0.5
rmse_ypos=0.3
rmse_xspace=2.5
rmse_yspace=-0.09
FOR i=0,n_models-1 DO BEGIN
   this_color=FSC_COLOR(model_colors(i),i+4)
   GPLOT,X=indgen(16)+0.5,Y=REFORM(model_contrib_rmm1_olr(i,0:15)),COL=i+4,STYLE=REFORM(model_styles(i)),SYM=model_symbols(i),SIZE=40,THICK=70
   GPLOT,X=rmse_xpos+rmse_xspace*ODD(i),Y=rmse_ypos+rmse_yspace*(i/2),$
         TEXT=model_runids(i)+': '+STRMID(STRTRIM(STRING(SQRT(MEAN((model_contrib_rmm1_olr(i,0:15)-$
                                                                    analysis_contrib_rmm1_olr(0:15))^2))),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   this_correlation=CORRELATE(model_contrib_rmm1_olr(i,0:15),analysis_contrib_rmm1_olr(0:15))
   IF this_correlation ge 0 THEN BEGIN
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.4,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   ENDIF ELSE $
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.4,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,5),ALIGN=0.0,COL=i+4,/BOLD
ENDFOR
GPLOT,X=[0,16],Y=[0,0],STYLE=2,COL=3,THICK=50
AXES,XSTEP=2,XMINOR=1,YSTEP=0.1,YMINOR=0.05,YTITLE='RMM1 from OLR',NDECS=2

POS,YPOS=2
GSET,XMIN=0,XMAX=16,YMIN=-1.0,YMAX=1.0;,TITLE='Contribution from U850'
GPLOT,X=indgen(16)+0.5,Y=analysis_contrib_rmm1_u850(0:15),COL=3,STYLE=0,SYM=4,SIZE=40
rmse_xpos=0.5
rmse_ypos=0.75
rmse_xspace=2.5
rmse_yspace=-0.25
FOR i=0,n_models-1 DO BEGIN
   this_color=FSC_COLOR(model_colors(i),i+4)
   GPLOT,X=indgen(16)+0.5,Y=REFORM(model_contrib_rmm1_u850(i,0:15)),COL=i+4,STYLE=REFORM(model_styles(i)),SYM=model_symbols(i),SIZE=40,THICK=70
   GPLOT,X=rmse_xpos+rmse_xspace*ODD(i),Y=rmse_ypos+rmse_yspace*(i/2),$
         TEXT=model_runids(i)+': '+STRMID(STRTRIM(STRING(SQRT(MEAN((model_contrib_rmm1_u850(i,0:15)-$
                                                                    analysis_contrib_rmm1_u850(0:15))^2))),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   this_correlation=CORRELATE(model_contrib_rmm1_u850(i,0:15),analysis_contrib_rmm1_u850(0:15))
   IF this_correlation ge 0 THEN BEGIN
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.4,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   ENDIF ELSE $
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.4,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,5),ALIGN=0.0,COL=i+4,/BOLD
ENDFOR
GPLOT,X=[0,16],Y=[0,0],STYLE=2,COL=3,THICK=50
AXES,XSTEP=2,XMINOR=1,YSTEP=0.5,YMINOR=0.25,YTITLE='RMM1 from U850',NDECS=2

POS,YPOS=1
GSET,XMIN=0,XMAX=16,YMIN=-1.0,YMAX=1.5;,TITLE='Contribution from U200'
GPLOT,X=indgen(16)+0.5,Y=analysis_contrib_rmm1_u200(0:15),COL=3,STYLE=0,SYM=4,SIZE=40
rmse_xpos=0.5
rmse_ypos=1.2
rmse_xspace=2.5
rmse_yspace=-0.4
FOR i=0,n_models-1 DO BEGIN
   this_color=FSC_COLOR(model_colors(i),i+4)
   GPLOT,X=indgen(16)+0.5,Y=REFORM(model_contrib_rmm1_u200(i,0:15)),COL=i+4,STYLE=REFORM(model_styles(i)),SYM=model_symbols(i),SIZE=40,THICK=70
   GPLOT,X=rmse_xpos+rmse_xspace*ODD(i),Y=rmse_ypos+rmse_yspace*(i/2),$
         TEXT=model_runids(i)+': '+STRMID(STRTRIM(STRING(SQRT(MEAN((model_contrib_rmm1_u200(i,0:15)-$
                                                                    analysis_contrib_rmm1_u200(0:15))^2))),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   this_correlation=CORRELATE(model_contrib_rmm1_u200(i,0:15),analysis_contrib_rmm1_u200(0:15))
   IF this_correlation ge 0 THEN BEGIN
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.4,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   ENDIF ELSE $
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.4,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,5),ALIGN=0.0,COL=i+4,/BOLD
ENDFOR
GPLOT,X=[0,16],Y=[0,0],STYLE=2,COL=3,THICK=50
AXES,XSTEP=2,XMINOR=1,YSTEP=0.5,YMINOR=0.25,YTITLE='RMM1 from U200',XTITLE='Day of integration (since 6 April 2009)',NDECS=2
;LEGEND,labels=['Coupled to KPP','Atmosphere-only'],COL=[3,3],SYM=model_symbols(0:1),STYLE=[1,0],LEGXOFFSET=-8000,LEGYOFFSET=-900,SIZE=40
LEGEND,labels=['1.5x entrain and no CMT','1.5x entrain with CMT','1.0x entrain with no CMT','Control (1.0x entrain with CMT)','Analysis'],$
       COL=[REVERSE(indgen(n_models)+4),3],SYM=[REVERSE(model_symbols),4],$
       LEGXOFFSET=1000,LEGYOFFSET=0,SIZE=40,STYLE=[1,0,1,0,0]
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/rmm_phases/hadgem3kpp_cascade_apr09_coupled_rmmphase_contribs.rmm2_16days.ps'
PSOPEN,file=psfile,FONT=4,BOLD=1,CHARSIZE=110,MARGIN=1500,XOFFSET=1000,YOFFSET=3500,TFONT=4,TCHARSIZE=90,YPLOTS=4,YSPACING=900
POS,YPOS=4
GSET,XMIN=0,XMAX=16,YMIN=-3.2,YMAX=1.6
black=FSC_COLOR("black",3)
GPLOT,X=indgen(16)+0.5,Y=analysis_rmm2(0:15),COL=3,STYLE=0,SYM=4,SIZE=40
rmse_xpos=0.5
rmse_ypos=1.2
rmse_xspace=2.5
rmse_yspace=-0.6
FOR i=0,n_models-1 DO BEGIN
   this_color=FSC_COLOR(model_colors(i),i+4)
   GPLOT,X=indgen(16)+0.5,Y=REFORM(model_rmm2(i,0:15)),COL=i+4,STYLE=REFORM(model_styles(i)),SYM=model_symbols(i),SIZE=40,THICK=70
   GPLOT,X=rmse_xpos+rmse_xspace*ODD(i),Y=rmse_ypos+rmse_yspace*(i/2),$
         TEXT=model_runids(i)+': '+STRMID(STRTRIM(STRING(SQRT(MEAN((model_rmm2(i,0:15)-$
                                                                    analysis_rmm2(0:15))^2))),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   this_correlation=CORRELATE(model_rmm2(i,0:15),analysis_rmm2(0:15))
   IF this_correlation ge 0 THEN BEGIN
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.4,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   ENDIF ELSE $
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.4,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,5),ALIGN=0.0,COL=i+4,/BOLD
ENDFOR
GPLOT,X=[0,16],Y=[0,0],STYLE=2,COL=3,THICK=50
AXES,XSTEP=2,XMINOR=1,YSTEP=0.8,YMINOR=0.4,YTITLE='RMM2',NDECS=2

POS,YPOS=3
GSET,XMIN=0,XMAX=16,YMIN=-0.8,YMAX=0.6;,TITLE='Contribution from OLR'
black=FSC_COLOR("black",3)
GPLOT,X=indgen(16)+0.5,Y=analysis_contrib_rmm2_olr(0:15),COL=3,STYLE=0,SYM=4,SIZE=40
rmse_xpos=0.5
rmse_ypos=0.4
rmse_xspace=2.5
rmse_yspace=-0.2
FOR i=0,n_models-1 DO BEGIN
   this_color=FSC_COLOR(model_colors(i),i+4)
   GPLOT,X=indgen(16)+0.5,Y=REFORM(model_contrib_rmm2_olr(i,0:15)),COL=i+4,STYLE=REFORM(model_styles(i)),SYM=model_symbols(i),SIZE=40,THICK=70
   GPLOT,X=rmse_xpos+rmse_xspace*ODD(i),Y=rmse_ypos+rmse_yspace*(i/2),$
         TEXT=model_runids(i)+': '+STRMID(STRTRIM(STRING(SQRT(MEAN((model_contrib_rmm2_olr(i,0:15)-$
                                                                    analysis_contrib_rmm2_olr(0:15))^2))),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   this_correlation=CORRELATE(model_contrib_rmm2_olr(i,0:15),analysis_contrib_rmm2_olr(0:15))
   IF this_correlation ge 0 THEN BEGIN
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.4,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   ENDIF ELSE $
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.4,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,5),ALIGN=0.0,COL=i+4,/BOLD
ENDFOR
GPLOT,X=[0,16],Y=[0,0],STYLE=2,COL=3,THICK=50
AXES,XSTEP=2,XMINOR=1,YSTEP=0.2,YMINOR=0.1,YTITLE='RMM2 from OLR',NDECS=2

POS,YPOS=2
GSET,XMIN=0,XMAX=16,YMIN=-1.5,YMAX=0.5;,TITLE='Contribution from U850'
GPLOT,X=indgen(16)+0.5,Y=analysis_contrib_rmm2_u850(0:15),COL=3,STYLE=0,SYM=4,SIZE=40
rmse_xpos=0.5
rmse_ypos=-0.25
rmse_xspace=2.5
rmse_yspace=-0.3
FOR i=0,n_models-1 DO BEGIN
   this_color=FSC_COLOR(model_colors(i),i+4)
   GPLOT,X=indgen(16)+0.5,Y=REFORM(model_contrib_rmm2_u850(i,0:15)),COL=i+4,STYLE=REFORM(model_styles(i)),SYM=model_symbols(i),SIZE=40,THICK=70
   GPLOT,X=rmse_xpos+rmse_xspace*ODD(i),Y=rmse_ypos+rmse_yspace*(i/2),$
         TEXT=model_runids(i)+': '+STRMID(STRTRIM(STRING(SQRT(MEAN((model_contrib_rmm2_u850(i,0:15)-$
                                                                    analysis_contrib_rmm2_u850(0:15))^2))),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   this_correlation=CORRELATE(model_contrib_rmm2_u850(i,0:15),analysis_contrib_rmm2_u850(0:15))
   IF this_correlation ge 0 THEN BEGIN
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.4,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   ENDIF ELSE $
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.4,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,5),ALIGN=0.0,COL=i+4,/BOLD
ENDFOR
GPLOT,X=[0,16],Y=[0,0],STYLE=2,COL=3,THICK=50
AXES,XSTEP=2,XMINOR=1,YSTEP=0.25,YMINOR=0.125,YTITLE='RMM2 from U850',NDECS=2

POS,YPOS=1
GSET,XMIN=0,XMAX=16,YMIN=-1.5,YMAX=1.0;,TITLE='Contribution from U200'
GPLOT,X=indgen(16)+0.5,Y=analysis_contrib_rmm2_u200(0:15),COL=3,STYLE=0,SYM=4,SIZE=40
rmse_xpos=0.5
rmse_ypos=0.5
rmse_xspace=2.5
rmse_yspace=-0.3
FOR i=0,n_models-1 DO BEGIN
   this_color=FSC_COLOR(model_colors(i),i+4)
   GPLOT,X=indgen(16)+0.5,Y=REFORM(model_contrib_rmm2_u200(i,0:15)),COL=i+4,STYLE=REFORM(model_styles(i)),SYM=model_symbols(i),SIZE=40,THICK=70
   GPLOT,X=rmse_xpos+rmse_xspace*ODD(i),Y=rmse_ypos+rmse_yspace*(i/2),$
         TEXT=model_runids(i)+': '+STRMID(STRTRIM(STRING(SQRT(MEAN((model_contrib_rmm2_u200(i,0:15)-$
                                                                    analysis_contrib_rmm2_u200(0:15))^2))),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   this_correlation=CORRELATE(model_contrib_rmm2_u200(i,0:15),analysis_contrib_rmm2_u200(0:15))
   IF this_correlation ge 0 THEN BEGIN
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.4,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   ENDIF ELSE $
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.4,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,5),ALIGN=0.0,COL=i+4,/BOLD
ENDFOR
GPLOT,X=[0,16],Y=[0,0],STYLE=2,COL=3,THICK=50
AXES,XSTEP=2,XMINOR=1,YSTEP=0.5,YMINOR=0.25,YTITLE='RMM2 from U200',XTITLE='Day of integration (since 6 April 2009)',NDECS=2
;LEGEND,labels=['Coupled to KPP','Atmosphere-only'],COL=[3,3],SYM=model_symbols(0:1),STYLE=[1,0],LEGXOFFSET=-8000,LEGYOFFSET=-900,SIZE=40
LEGEND,labels=['1.5x entrain and no CMT','1.5x entrain with CMT','1.0x entrain with no CMT','Control (1.0x entrain with CMT)','Analysis'],$
       COL=[REVERSE(indgen(n_models)+4),3],SYM=[REVERSE(model_symbols),4],$
       LEGXOFFSET=1000,LEGYOFFSET=0,SIZE=40,STYLE=[1,0,1,0,0]
PSCLOSE

STOP
END

