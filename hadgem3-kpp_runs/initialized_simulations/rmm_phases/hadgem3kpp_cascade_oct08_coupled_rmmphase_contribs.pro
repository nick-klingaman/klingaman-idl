PRO hadgem3kpp_cascade_oct08_rmmphase_contribs

analysis_input_file='/home/ss901165/datasets_mango/UM_ANALYSES/cascade_mjo_wh04/metum_analysis.jun-nov_dmeans.2008.rmm_indices.nc'
analysis_time_offset=132
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

n_models=6
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
         runid='xfdib'
         dirname='xfdib'
         model_time_offset=0
         color='red'
         style=0
         symbol=6
      END
      1 : BEGIN
         runid='xfdic'
         dirname='xfdic'
         model_time_offset=0
         color='red'
         style=1
         symbol=3
      END
      2 : BEGIN
         runid='xfdif'
         dirname='xfdif'
         model_time_offset=0
         color='brown'
         style=0
         symbol=6
      END
      3: BEGIN
         runid='xfdij'
         dirname='xfdij'
         model_time_offset=0
         color='brown'
         style=1
         symbol=3
      END
      4 : BEGIN
         runid='xfdii'
         dirname='xfdii'
         model_time_offset=0
         color='purple'
         style=0
         symbol=6
      END
      5 : BEGIN
         runid='xfdik'
         dirname='xfdik'
         model_time_offset=0
         color='purple'
         style=1
         symbol=3
      END
   ENDCASE
   print,runid
   model_input_file=um3+'/'+dirname+'/'+runid+'.11oct08.rmm_indices.nc'
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

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/rmm_phases/hadgem3kpp_cascade_oct08_rmmphase_contribs.rmm1_16days.ps'
PSOPEN,file=psfile,FONT=4,CHARSIZE=80,MARGIN=1500,XOFFSET=800,YOFFSET=3000,TFONT=2,TCHARSIZE=90,YPLOTS=4,YSPACING=900,BOLD=1

POS,YPOS=4
GSET,XMIN=0,XMAX=16,YMIN=-1.4,YMAX=2.8
black=FSC_COLOR("black",3)
GPLOT,X=indgen(16)+0.5,Y=analysis_rmm1(0:15),COL=3,STYLE=0,SYM=4,SIZE=60
rmse_xpos=0.5
rmse_ypos=2.4
rmse_xspace=2.5
rmse_yspace=-0.6
FOR i=0,n_models-1 DO BEGIN
   this_color=FSC_COLOR(model_colors(i),i+4)
   GPLOT,X=indgen(16)+0.5,Y=REFORM(model_rmm1(i,0:15)),COL=i+4,STYLE=REFORM(model_styles(i)),SYM=model_symbols(i),SIZE=60,THICK=90
   GPLOT,X=rmse_xpos+rmse_xspace*ODD(i),Y=rmse_ypos+rmse_yspace*(i/2),$
         TEXT=model_runids(i)+': '+STRMID(STRTRIM(STRING(SQRT(MEAN((model_rmm1(i,0:15)-$
                                                                    analysis_rmm1(0:15))^2))),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   this_correlation=CORRELATE(model_rmm1(i,0:15),analysis_rmm1(0:15))
   IF this_correlation ge 0 THEN BEGIN
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.6,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   ENDIF ELSE $
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.6,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,5),ALIGN=0.0,COL=i+4,/BOLD
ENDFOR
GPLOT,X=[0,16],Y=[0,0],STYLE=2,COL=3,THICK=50
AXES,XSTEP=2,XMINOR=1,YSTEP=0.7,YMINOR=0.35,YTITLE='RMM1',NDECS=2

POS,YPOS=3
GSET,XMIN=0,XMAX=16,YMIN=-0.5,YMAX=0.5;,TITLE='Contribution from OLR'
black=FSC_COLOR("black",3)
GPLOT,X=indgen(16)+0.5,Y=analysis_contrib_rmm1_olr(0:15),COL=3,STYLE=0,SYM=4,SIZE=60
rmse_xpos=0.5
rmse_ypos=0.4
rmse_xspace=2.5
rmse_yspace=-0.12
FOR i=0,n_models-1 DO BEGIN
   this_color=FSC_COLOR(model_colors(i),i+4)
   GPLOT,X=indgen(16)+0.5,Y=REFORM(model_contrib_rmm1_olr(i,0:15)),COL=i+4,STYLE=REFORM(model_styles(i)),SYM=model_symbols(i),SIZE=60,THICK=90
   GPLOT,X=rmse_xpos+rmse_xspace*ODD(i),Y=rmse_ypos+rmse_yspace*(i/2),$
         TEXT=model_runids(i)+': '+STRMID(STRTRIM(STRING(SQRT(MEAN((model_contrib_rmm1_olr(i,0:15)-$
                                                                    analysis_contrib_rmm1_olr(0:15))^2))),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   this_correlation=CORRELATE(model_contrib_rmm1_olr(i,0:15),analysis_contrib_rmm1_olr(0:15))
   IF this_correlation ge 0 THEN BEGIN
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.6,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   ENDIF ELSE $
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.6,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,5),ALIGN=0.0,COL=i+4,/BOLD
ENDFOR
GPLOT,X=[0,16],Y=[0,0],STYLE=2,COL=3,THICK=50
AXES,XSTEP=2,XMINOR=1,YSTEP=0.25,YMINOR=0.125,YTITLE='RMM1 from OLR',NDECS=2

POS,YPOS=2
GSET,XMIN=0,XMAX=16,YMIN=-0.75,YMAX=1.25;,TITLE='Contribution from U850'
GPLOT,X=indgen(16)+0.5,Y=analysis_contrib_rmm1_u850(0:15),COL=3,STYLE=0,SYM=4,SIZE=60
rmse_xpos=0.5
rmse_ypos=1.0
rmse_xspace=2.5
rmse_yspace=-0.25
FOR i=0,n_models-1 DO BEGIN
   this_color=FSC_COLOR(model_colors(i),i+4)
   GPLOT,X=indgen(16)+0.5,Y=REFORM(model_contrib_rmm1_u850(i,0:15)),COL=i+4,STYLE=REFORM(model_styles(i)),SYM=model_symbols(i),SIZE=60,THICK=90
   GPLOT,X=rmse_xpos+rmse_xspace*ODD(i),Y=rmse_ypos+rmse_yspace*(i/2),$
         TEXT=model_runids(i)+': '+STRMID(STRTRIM(STRING(SQRT(MEAN((model_contrib_rmm1_u850(i,0:15)-$
                                                                    analysis_contrib_rmm1_u850(0:15))^2))),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   this_correlation=CORRELATE(model_contrib_rmm1_u850(i,0:15),analysis_contrib_rmm1_u850(0:15))
   IF this_correlation ge 0 THEN BEGIN
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.6,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   ENDIF ELSE $
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.6,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,5),ALIGN=0.0,COL=i+4,/BOLD
ENDFOR
GPLOT,X=[0,16],Y=[0,0],STYLE=2,COL=3,THICK=50
AXES,XSTEP=2,XMINOR=1,YSTEP=0.5,YMINOR=0.25,YTITLE='RMM1 from U850',NDECS=2

POS,YPOS=1
GSET,XMIN=0,XMAX=16,YMIN=-1.5,YMAX=1.5;,TITLE='Contribution from U200'
GPLOT,X=indgen(16)+0.5,Y=analysis_contrib_rmm1_u200(0:15),COL=3,STYLE=0,SYM=4,SIZE=60
rmse_xpos=0.5
rmse_ypos=1.15
rmse_xspace=2.5
rmse_yspace=-0.3
FOR i=0,n_models-1 DO BEGIN
   this_color=FSC_COLOR(model_colors(i),i+4)
   GPLOT,X=indgen(16)+0.5,Y=REFORM(model_contrib_rmm1_u200(i,0:15)),COL=i+4,STYLE=REFORM(model_styles(i)),SYM=model_symbols(i),SIZE=60,THICK=90
   GPLOT,X=rmse_xpos+rmse_xspace*ODD(i),Y=rmse_ypos+rmse_yspace*(i/2),$
         TEXT=model_runids(i)+': '+STRMID(STRTRIM(STRING(SQRT(MEAN((model_contrib_rmm1_u200(i,0:15)-$
                                                                    analysis_contrib_rmm1_u200(0:15))^2))),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   this_correlation=CORRELATE(model_contrib_rmm1_u200(i,0:15),analysis_contrib_rmm1_u200(0:15))
   IF this_correlation ge 0 THEN BEGIN
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.6,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   ENDIF ELSE $
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.6,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,5),ALIGN=0.0,COL=i+4,/BOLD
ENDFOR
GPLOT,X=[0,16],Y=[0,0],STYLE=2,COL=3,THICK=50
AXES,XSTEP=2,XMINOR=1,YSTEP=0.5,YMINOR=0.25,YTITLE='RMM1 from U200',XTITLE='Day of integration (since 11 October 2008)',NDECS=2

LEGEND,labels=['Without CMT','With CMT'],COL=[3,3],SYM=model_symbols(0:1),LEGXOFFSET=-8000,LEGYOFFSET=-900,SIZE=60
LEGEND,labels=['T-vary OSTIA and 1.5x mix entrain','T-vary OSTIA','Control','Analysis'],COL=[n_models+2,n_models,n_models-2,3],SYM=[6,6,6,4],$
       LEGXOFFSET=1000,LEGYOFFSET=0,SIZE=60

PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/rmm_phases/hadgem3kpp_cascade_oct08_rmmphase_contribs.rmm2_16days.ps'
PSOPEN,file=psfile,FONT=4,CHARSIZE=80,MARGIN=1500,XOFFSET=800,YOFFSET=2500,TFONT=2,TCHARSIZE=90,YPLOTS=4,YSPACING=900,BOLD=1

POS,YPOS=4
GSET,XMIN=0,XMAX=16,YMIN=-2.0,YMAX=2.0
black=FSC_COLOR("black",3)
GPLOT,X=indgen(16)+0.5,Y=analysis_rmm2(0:15),COL=3,STYLE=0,SYM=4,SIZE=60
rmse_xpos=0.5
rmse_ypos=1.6
rmse_xspace=2.5
rmse_yspace=-0.4
FOR i=0,n_models-1 DO BEGIN
   this_color=FSC_COLOR(model_colors(i),i+4)
   GPLOT,X=indgen(16)+0.5,Y=REFORM(model_rmm2(i,0:15)),COL=i+4,STYLE=REFORM(model_styles(i)),SYM=model_symbols(i),SIZE=60,THICK=90
   GPLOT,X=rmse_xpos+rmse_xspace*ODD(i),Y=rmse_ypos+rmse_yspace*(i/2),$
         TEXT=model_runids(i)+': '+STRMID(STRTRIM(STRING(SQRT(MEAN((model_rmm2(i,0:15)-$
                                                                    analysis_rmm2(0:15))^2))),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   this_correlation=CORRELATE(model_rmm2(i,0:15),analysis_rmm2(0:15))
   IF this_correlation ge 0 THEN BEGIN
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.6,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   ENDIF ELSE $
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.6,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,5),ALIGN=0.0,COL=i+4,/BOLD
ENDFOR
GPLOT,X=[0,16],Y=[0,0],STYLE=2,COL=3,THICK=50
AXES,XSTEP=2,XMINOR=1,YSTEP=0.5,YMINOR=0.25,YTITLE='RMM2',NDECS=2

POS,YPOS=3
GSET,XMIN=0,XMAX=16,YMIN=-0.5,YMAX=0.75;,TITLE='Contribution from OLR'
black=FSC_COLOR("black",3)
GPLOT,X=indgen(16)+0.5,Y=analysis_contrib_rmm2_olr(0:15),COL=3,STYLE=0,SYM=4,SIZE=60
rmse_xpos=0.5
rmse_ypos=0.6
rmse_xspace=2.5
rmse_yspace=-0.15
FOR i=0,n_models-1 DO BEGIN
   this_color=FSC_COLOR(model_colors(i),i+4)
   GPLOT,X=indgen(16)+0.5,Y=REFORM(model_contrib_rmm2_olr(i,0:15)),COL=i+4,STYLE=REFORM(model_styles(i)),SYM=model_symbols(i),SIZE=60,THICK=90
   GPLOT,X=rmse_xpos+rmse_xspace*ODD(i),Y=rmse_ypos+rmse_yspace*(i/2),$
         TEXT=model_runids(i)+': '+STRMID(STRTRIM(STRING(SQRT(MEAN((model_contrib_rmm2_olr(i,0:15)-$
                                                                    analysis_contrib_rmm2_olr(0:15))^2))),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   this_correlation=CORRELATE(model_contrib_rmm2_olr(i,0:15),analysis_contrib_rmm2_olr(0:15))
   IF this_correlation ge 0 THEN BEGIN
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.6,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   ENDIF ELSE $
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.6,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,5),ALIGN=0.0,COL=i+4,/BOLD
ENDFOR
GPLOT,X=[0,16],Y=[0,0],STYLE=2,COL=3,THICK=50
AXES,XSTEP=2,XMINOR=1,YSTEP=0.25,YMINOR=0.125,YTITLE='RMM2 from OLR',NDECS=2

POS,YPOS=2
GSET,XMIN=0,XMAX=16,YMIN=-1.2,YMAX=0.8;,TITLE='Contribution from U850'
GPLOT,X=indgen(16)+0.5,Y=analysis_contrib_rmm2_u850(0:15),COL=3,STYLE=0,SYM=4,SIZE=60
rmse_xpos=0.5
rmse_ypos=0.6
rmse_xspace=2.5
rmse_yspace=-0.2
FOR i=0,n_models-1 DO BEGIN
   this_color=FSC_COLOR(model_colors(i),i+4)
   GPLOT,X=indgen(16)+0.5,Y=REFORM(model_contrib_rmm2_u850(i,0:15)),COL=i+4,STYLE=REFORM(model_styles(i)),SYM=model_symbols(i),SIZE=60,THICK=90
   GPLOT,X=rmse_xpos+rmse_xspace*ODD(i),Y=rmse_ypos+rmse_yspace*(i/2),$
         TEXT=model_runids(i)+': '+STRMID(STRTRIM(STRING(SQRT(MEAN((model_contrib_rmm2_u850(i,0:15)-$
                                                                    analysis_contrib_rmm2_u850(0:15))^2))),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   this_correlation=CORRELATE(model_contrib_rmm2_u850(i,0:15),analysis_contrib_rmm2_u850(0:15))
   IF this_correlation ge 0 THEN BEGIN
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.6,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   ENDIF ELSE $
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.6,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,5),ALIGN=0.0,COL=i+4,/BOLD
ENDFOR
GPLOT,X=[0,16],Y=[0,0],STYLE=2,COL=3,THICK=50
AXES,XSTEP=2,XMINOR=1,YSTEP=0.4,YMINOR=0.2,YTITLE='RMM2 from U850',NDECS=2

POS,YPOS=1
GSET,XMIN=0,XMAX=16,YMIN=-1.5,YMAX=1.5;,TITLE='Contribution from U200'
GPLOT,X=indgen(16)+0.5,Y=analysis_contrib_rmm2_u200(0:15),COL=3,STYLE=0,SYM=4,SIZE=60
rmse_xpos=0.5
rmse_ypos=1.2
rmse_xspace=2.5
rmse_yspace=-0.3
FOR i=0,n_models-1 DO BEGIN
   this_color=FSC_COLOR(model_colors(i),i+4)
   GPLOT,X=indgen(16)+0.5,Y=REFORM(model_contrib_rmm2_u200(i,0:15)),COL=i+4,STYLE=REFORM(model_styles(i)),SYM=model_symbols(i),SIZE=60,THICK=90
   GPLOT,X=rmse_xpos+rmse_xspace*ODD(i),Y=rmse_ypos+rmse_yspace*(i/2),$
         TEXT=model_runids(i)+': '+STRMID(STRTRIM(STRING(SQRT(MEAN((model_contrib_rmm2_u200(i,0:15)-$
                                                                    analysis_contrib_rmm2_u200(0:15))^2))),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   this_correlation=CORRELATE(model_contrib_rmm2_u200(i,0:15),analysis_contrib_rmm2_u200(0:15))
   IF this_correlation ge 0 THEN BEGIN
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.6,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,4),ALIGN=0.0,COL=i+4,/BOLD
   ENDIF ELSE $
      GPLOT,X=rmse_xpos+rmse_xspace*ODD(i)+1.6,Y=rmse_ypos+rmse_yspace*(i/2),$
            TEXT=STRMID(STRTRIM(STRING(this_correlation),1),0,5),ALIGN=0.0,COL=i+4,/BOLD
ENDFOR
GPLOT,X=[0,16],Y=[0,0],STYLE=2,COL=3,THICK=50
AXES,XSTEP=2,XMINOR=1,YSTEP=0.5,YMINOR=0.25,YTITLE='RMM2 from U200',XTITLE='Day of integration (since 11 October 2008)',NDECS=2
LEGEND,labels=['Without CMT','With CMT'],COL=[3,3],SYM=model_symbols(0:1),LEGXOFFSET=-7000,LEGYOFFSET=-900,SIZE=60
LEGEND,labels=['T-vary OSTIA and 1.5x mix entrain','T-vary OSTIA','Control','Analysis'],COL=[n_models+2,n_models,n_models-2,3],SYM=[6,6,6,4],$
       LEGXOFFSET=1000,LEGYOFFSET=0,SIZE=60

PSCLOSE

STOP
END

