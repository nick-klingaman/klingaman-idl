PRO SOI_depresys

; Give the input files.

n_seasons=4
depresys1_mean=fltarr(n_seasons)
depresys1_stddev=fltarr(n_seasons)
depresys1_coeff_var=fltarr(n_seasons)

depresys2_mean=fltarr(n_seasons)
depresys2_stddev=fltarr(n_seasons)
depresys2_coeff_var=fltarr(n_seasons)

depresys3_mean=fltarr(n_seasons)
depresys3_stddev=fltarr(n_seasons)
depresys3_coeff_var=fltarr(n_seasons)

depresys4_mean=fltarr(n_seasons)
depresys4_stddev=fltarr(n_seasons)
depresys4_coeff_var=fltarr(n_seasons)

depresys5_mean=fltarr(n_seasons)
depresys5_stddev=fltarr(n_seasons)
depresys5_coeff_var=fltarr(n_seasons)

depresys6_mean=fltarr(n_seasons)
depresys6_stddev=fltarr(n_seasons)
depresys6_coeff_var=fltarr(n_seasons)

depresys7_mean=fltarr(n_seasons)
depresys7_stddev=fltarr(n_seasons)
depresys7_coeff_var=fltarr(n_seasons)

depresys8_mean=fltarr(n_seasons)
depresys8_stddev=fltarr(n_seasons)
depresys8_coeff_var=fltarr(n_seasons)

depresys0_mean=fltarr(n_seasons)
depresys0_stddev=fltarr(n_seasons)
depresys0_coeff_var=fltarr(n_seasons)

obs_mean=fltarr(n_seasons)
obs_stddev=fltarr(n_seasons)
obs_coeff_var=fltarr(n_seasons)


FOR i=0,n_seasons-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         season='djf'
         era40_season='dec-feb'
      END
      1 : BEGIN
         season='mam'
         era40_season='mar-may'
      END
      2 : BEGIN
         season='jja'
         era40_season='jun-aug'
      END
      3 : BEGIN
         season='son'
         era40_season='sep-nov'
      END
   ENDCASE

   depresys1_file='/home/swr06jir/links/elder/PPE/trans/atmos/qump1/qump1_MSLP_'+season+'_DATA.nc'
   depresys2_file='/home/swr06jir/links/elder/PPE/trans/atmos/qump2/qump2_MSLP_'+season+'_DATA.nc'
   depresys3_file='/home/swr06jir/links/elder/PPE/trans/atmos/qump3/qump3_MSLP_'+season+'_DATA.nc'
   depresys4_file='/home/swr06jir/links/elder/PPE/trans/atmos/qump4/qump4_MSLP_'+season+'_DATA.nc'
   depresys5_file='/home/swr06jir/links/elder/PPE/trans/atmos/qump5/qump5_MSLP_'+season+'_DATA.nc'
   depresys6_file='/home/swr06jir/links/elder/PPE/trans/atmos/qump6/qump6_MSLP_'+season+'_DATA.nc'
   depresys7_file='/home/swr06jir/links/elder/PPE/trans/atmos/qump7/qump7_MSLP_'+season+'_DATA.nc'
   depresys8_file='/home/swr06jir/links/elder/PPE/trans/atmos/qump8/qump8_MSLP_'+season+'_DATA.nc'
   depresys0_file='/home/swr06jir/links/elder/PPE/trans/atmos/qump0/qump0_MSLP_'+season+'_DATA.nc'

   era40_file='/home/ss901165/datasets/ERA40/MSL/era40.'+era40_season+'_smeans.1958-2001.msl.nc'

; Define the region to plot as a box.  The values are:
; [starting_latitude,starting_longitude,stopping_latitude,stopping_longitude]
   box=[-60,80,10,240]

; Number of years to read
   n_years=44

   depresys1_longitude=OPEN_AND_EXTRACT(depresys1_file,'longitude')
   depresys1_latitude=OPEN_AND_EXTRACT(depresys1_file,'latitude')
; Restrict the dimensions to the box specified above.
   DEFINE_BOUNDARIES,box,depresys1_latitude,depresys1_longitude,depresys1_box_tx,/LIMIT
; Get the number of longitude and latitude points in the "box" region.
   depresys1_nlon=N_ELEMENTS(depresys1_longitude)
   depresys1_nlat=N_ELEMENTS(depresys1_latitude)


depresys1_MSL=OPEN_AND_EXTRACT(depresys1_file,'p',$
                               offset=[depresys1_box_tx(1),depresys1_box_tx(0),0,58],$
                               count=[depresys1_nlon,depresys1_nlat,1,n_years])

depresys1_MSLP = depresys1_MSL / 100

depresys2_MSL=OPEN_AND_EXTRACT(depresys2_file,'p',$
                               offset=[depresys1_box_tx(1),depresys1_box_tx(0),0,58],$
                               count=[depresys1_nlon,depresys1_nlat,1,n_years])

depresys2_MSLP = depresys2_MSL / 100

depresys3_MSL=OPEN_AND_EXTRACT(depresys3_file,'p',$
                               offset=[depresys1_box_tx(1),depresys1_box_tx(0),0,58],$
                               count=[depresys1_nlon,depresys1_nlat,1,n_years])

depresys3_MSLP = depresys3_MSL / 100

depresys4_MSL=OPEN_AND_EXTRACT(depresys4_file,'p',$
                               offset=[depresys1_box_tx(1),depresys1_box_tx(0),0,58],$
                               count=[depresys1_nlon,depresys1_nlat,1,n_years])

depresys4_MSLP = depresys4_MSL / 100

depresys5_MSL=OPEN_AND_EXTRACT(depresys5_file,'p',$
                               offset=[depresys1_box_tx(1),depresys1_box_tx(0),0,58],$
                               count=[depresys1_nlon,depresys1_nlat,1,n_years])

depresys5_MSLP = depresys5_MSL / 100

depresys6_MSL=OPEN_AND_EXTRACT(depresys6_file,'p',$
                               offset=[depresys1_box_tx(1),depresys1_box_tx(0),0,58],$
                               count=[depresys1_nlon,depresys1_nlat,1,n_years])

depresys6_MSLP = depresys6_MSL / 100

depresys7_MSL=OPEN_AND_EXTRACT(depresys7_file,'p',$
                               offset=[depresys1_box_tx(1),depresys1_box_tx(0),0,58],$
                               count=[depresys1_nlon,depresys1_nlat,1,n_years])

depresys7_MSLP = depresys7_MSL / 100

depresys8_MSL=OPEN_AND_EXTRACT(depresys8_file,'p',$
                               offset=[depresys1_box_tx(1),depresys1_box_tx(0),0,58],$
                               count=[depresys1_nlon,depresys1_nlat,1,n_years])

depresys8_MSLP = depresys8_MSL / 100

depresys0_MSL=OPEN_AND_EXTRACT(depresys0_file,'p',$
                               offset=[depresys1_box_tx(1),depresys1_box_tx(0),0,58],$
                               count=[depresys1_nlon,depresys1_nlat,1,n_years])

depresys0_MSLP = depresys0_MSL / 100

Darwin_longitude=NEAREST(depresys1_longitude,130.83)
Darwin_latitude=NEAREST(depresys1_latitude,-12.47)

Tahiti_longitude=NEAREST(depresys1_longitude,209.55)
Tahiti_latitude=NEAREST(depresys1_latitude,-17.67)

Darwin1 = REFORM(depresys1_MSLP(Darwin_longitude,Darwin_latitude,*,*))
Tahiti1 = REFORM(depresys1_MSLP(Tahiti_longitude,Tahiti_latitude,*,*))

Darwin2 = REFORM(depresys2_MSLP(Darwin_longitude,Darwin_latitude,*,*))
Tahiti2 = REFORM(depresys2_MSLP(Tahiti_longitude,Tahiti_latitude,*,*))

Darwin3 = REFORM(depresys3_MSLP(Darwin_longitude,Darwin_latitude,*,*))
Tahiti3 = REFORM(depresys3_MSLP(Tahiti_longitude,Tahiti_latitude,*,*))

Darwin4 = REFORM(depresys4_MSLP(Darwin_longitude,Darwin_latitude,*,*))
Tahiti4 = REFORM(depresys4_MSLP(Tahiti_longitude,Tahiti_latitude,*,*))

Darwin5 = REFORM(depresys5_MSLP(Darwin_longitude,Darwin_latitude,*,*))
Tahiti5 = REFORM(depresys5_MSLP(Tahiti_longitude,Tahiti_latitude,*,*))

Darwin6 = REFORM(depresys6_MSLP(Darwin_longitude,Darwin_latitude,*,*))
Tahiti6 = REFORM(depresys6_MSLP(Tahiti_longitude,Tahiti_latitude,*,*))

Darwin7 = REFORM(depresys7_MSLP(Darwin_longitude,Darwin_latitude,*,*))
Tahiti7 = REFORM(depresys7_MSLP(Tahiti_longitude,Tahiti_latitude,*,*))

Darwin8 = REFORM(depresys8_MSLP(Darwin_longitude,Darwin_latitude,*,*))
Tahiti8 = REFORM(depresys8_MSLP(Tahiti_longitude,Tahiti_latitude,*,*))

Darwin0 = REFORM(depresys0_MSLP(Darwin_longitude,Darwin_latitude,*,*))
Tahiti0 = REFORM(depresys0_MSLP(Tahiti_longitude,Tahiti_latitude,*,*))


depresys1_SOI = (Darwin1-Tahiti1) ;- (MEAN(Darwin1-Tahiti1)); / STDDEV(Darwin1-Tahiti1)
depresys2_SOI = (Darwin2-Tahiti2) ;- (MEAN(Darwin2-Tahiti2)); / STDDEV(Darwin2-Tahiti2)
depresys3_SOI = (Darwin3-Tahiti3) ;- (MEAN(Darwin3-Tahiti3)); / STDDEV(Darwin3-Tahiti3)
depresys4_SOI = (Darwin4-Tahiti4) ;- (MEAN(Darwin4-Tahiti4)); / STDDEV(Darwin4-Tahiti4)
depresys5_SOI = (Darwin5-Tahiti5) ;- (MEAN(Darwin5-Tahiti5)); / STDDEV(Darwin5-Tahiti5)
depresys6_SOI = (Darwin6-Tahiti6) ;- (MEAN(Darwin6-Tahiti6)); / STDDEV(Darwin6-Tahiti6)
depresys7_SOI = (Darwin7-Tahiti7) ;- (MEAN(Darwin7-Tahiti7)); / STDDEV(Darwin7-Tahiti7)
depresys8_SOI = (Darwin8-Tahiti8) ;- (MEAN(Darwin8-Tahiti8)); / STDDEV(Darwin8-Tahiti8)
depresys0_SOI = (Darwin0-Tahiti0) ;- (MEAN(Darwin0-Tahiti0)); / STDDEV(Darwin0-Tahiti0)

era40_longitude=OPEN_AND_EXTRACT(era40_file,'longitude')
era40_latitude=OPEN_AND_EXTRACT(era40_file,'latitude')
DEFINE_BOUNDARIES,box,era40_latitude,era40_longitude,era40_box_tx,/LIMIT
; Get the number of longitude and latitude points in the "box" region.
era40_nlon=N_ELEMENTS(era40_longitude)
era40_nlat=N_ELEMENTS(era40_latitude)
era40_MSL=OPEN_AND_EXTRACT(era40_file,'MSL',$
                               offset=[era40_box_tx(1),era40_box_tx(0),0],$
                               count=[era40_nlon,era40_nlat,n_years])

era40_MSLP = era40_MSL / 100

Darwin_longitude=NEAREST(era40_longitude,130.83)
Darwin_latitude=NEAREST(era40_latitude,-12.47)
Tahiti_longitude=NEAREST(era40_longitude,209.55)
Tahiti_latitude=NEAREST(era40_latitude,-17.67)
Darwin_era40 = REFORM(era40_MSLP(Darwin_longitude,Darwin_latitude,*))
Tahiti_era40 = REFORM(era40_MSLP(Tahiti_longitude,Tahiti_latitude,*))

era40_SOI=(Darwin_era40-Tahiti_era40)

obs_stddev(i)=STDDEV(era40_SOI)

depresys1_stddev(i) = STDDEV(depresys1_SOI)
depresys2_stddev(i) = STDDEV(depresys2_SOI)
depresys3_stddev(i) = STDDEV(depresys3_SOI)
depresys4_stddev(i) = STDDEV(depresys4_SOI)
depresys5_stddev(i) = STDDEV(depresys5_SOI)
depresys6_stddev(i) = STDDEV(depresys6_SOI)
depresys7_stddev(i) = STDDEV(depresys7_SOI)
depresys8_stddev(i) = STDDEV(depresys8_SOI)
depresys0_stddev(i) = STDDEV(depresys0_SOI)

obs_mean(i) = MEAN(era40_SOI)

depresys1_mean(i) = MEAN(depresys1_SOI)
depresys2_mean(i) = MEAN(depresys2_SOI)
depresys3_mean(i) = MEAN(depresys3_SOI)
depresys4_mean(i) = MEAN(depresys4_SOI)
depresys5_mean(i) = MEAN(depresys5_SOI)
depresys6_mean(i) = MEAN(depresys6_SOI)
depresys7_mean(i) = MEAN(depresys7_SOI)
depresys8_mean(i) = MEAN(depresys8_SOI)
depresys0_mean(i) = MEAN(depresys0_SOI)

obs_coeff_var(i) = obs_stddev(i) / obs_mean(i)
depresys1_coeff_var(i) = depresys1_stddev(i) / depresys1_mean(i)
depresys2_coeff_var(i) = depresys2_stddev(i) / depresys2_mean(i)
depresys3_coeff_var(i) = depresys3_stddev(i) / depresys3_mean(i)
depresys4_coeff_var(i) = depresys4_stddev(i) / depresys4_mean(i)
depresys5_coeff_var(i) = depresys5_stddev(i) / depresys5_mean(i)
depresys6_coeff_var(i) = depresys6_stddev(i) / depresys6_mean(i)
depresys7_coeff_var(i) = depresys7_stddev(i) / depresys7_mean(i)
depresys8_coeff_var(i) = depresys8_stddev(i) / depresys8_mean(i)
depresys0_coeff_var(i) = depresys0_stddev(i) / depresys0_mean(i)

ENDFOR

; Plot the data. 

; Give the location of the PostScript file that this program will make.

psfile1='/home/ss901165/idl/aus_decpred/mean_SOI.ps'
PSOPEN,file=psfile1,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=1300,XOFFSET=500,YOFFSET=2500,TFONT=2,$
       YSIZE=23000,TCHARSIZE=100,SPACE3=500,/PORTRAIT
GSET,XMIN=0,XMAX=9,YMIN=-6.2,YMAX=0.2,TITLE='SOI Seasonal Mean Value'
xvals=[0,1,2,3,4,5,6,7,8,9]
model=['OBS','QUMP1','QUMP2','QUMP3','QUMP4','QUMP5','QUMP6','QUMP7','QUMP8','QUMP0']
colors=['red','orange','blue','green']
FOR i=0,n_seasons-1 DO BEGIN
   ypts=[obs_mean(i),depresys1_mean(i),depresys2_mean(i),depresys3_mean(i),depresys4_mean(i),depresys5_mean(i),depresys6_mean(i),depresys7_mean(i),depresys8_mean(i),depresys0_mean(i)]
   GPLOT,X=xvals,Y=ypts,SYM=3,/NOLINES,COL=FSC_COLOR(colors(i))
ENDFOR
AXES,XLABELS=model,YSTEP=0.2,NDECS=1,YTITLE='Mean SOI',/GRID,GCOL=1,ORIENTATION=90
GLEGEND,STYLE=[-1,-1,-1,-1],LEGPOS=3,SYM=[3,3,3,3],LABELS=['SON','JJA','MAM','DJF'],$
        COL=[FSC_COLOR('green'),FSC_COLOR('blue'),FSC_COLOR('orange'),FSC_COLOR('red')],LENGTH=0
PSCLOSE

psfile2='/home/ss901165/idl/aus_decpred/stddev_SOI.ps'
PSOPEN,file=psfile2,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=1300,XOFFSET=500,YOFFSET=2500,TFONT=2,$
       YSIZE=23000,TCHARSIZE=100,SPACE3=500,/PORTRAIT
GSET,XMIN=0,XMAX=9,YMIN=0.6,YMAX=2.4,TITLE='Seasonal Standard Deviation on SOI'
xvals=[0,1,2,3,4,5,6,7,8,9]
model=['OBS','QUMP1','QUMP2','QUMP3','QUMP4','QUMP5','QUMP6','QUMP7','QUMP8','QUMP0']
colors=['red','orange','blue','green']
FOR i=0,n_seasons-1 DO BEGIN
   ypts=[obs_stddev(i),depresys1_stddev(i),depresys2_stddev(i),depresys3_stddev(i),depresys4_stddev(i),depresys5_stddev(i),depresys6_stddev(i),depresys7_stddev(i),depresys8_stddev(i),depresys0_stddev(i)]
   GPLOT,X=xvals,Y=ypts,SYM=3,/NOLINES,COL=FSC_COLOR(colors(i))
ENDFOR
AXES,XLABELS=model,YSTEP=0.05,NDECS=1,YTITLE='Standard Deviation SOI',/GRID,GCOL=1,ORIENTATION=90
GLEGEND,LEGPOS=3,STYLE=[-1,-1,-1,-1],SYM=[3,3,3,3],LABELS=['SON','JJA','MAM','DJF'],COL=[FSC_COLOR('green'),FSC_COLOR('blue'),FSC_COLOR('orange'),FSC_COLOR('red')]
PSCLOSE

psfile3='/home/ss901165/idl/aus_decpred/coeff_var_SOI.ps'
PSOPEN,file=psfile3,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=1300,XOFFSET=500,YOFFSET=2500,TFONT=2,$
       YSIZE=23000,TCHARSIZE=100,SPACE3=500,/PORTRAIT
GSET,XMIN=0,XMAX=9,YMIN=0.1,YMAX=20,/YLOG,TITLE='Seasonal Coefficient of Variation on SOI'
xvals=[0,1,2,3,4,5,6,7,8,9]
yvals=['0.1','0.2','0.3','0.4','0.5','0.6','0.7','0.8','0.9','1','2','3','4','5','6','7','8','9','10','20']
model=['OBS','QUMP1','QUMP2','QUMP3','QUMP4','QUMP5','QUMP6','QUMP7','QUMP8','QUMP0']
colors=['red','orange','blue','green']
FOR i=0,n_seasons-1 DO BEGIN
   ypts=[ABS(obs_coeff_var(i)),ABS(depresys1_coeff_var(i)),ABS(depresys2_coeff_var(i)),ABS(depresys3_coeff_var(i)),ABS(depresys4_coeff_var(i)),ABS(depresys5_coeff_var(i)),ABS(depresys6_coeff_var(i)),ABS(depresys7_coeff_var(i)),ABS(depresys8_coeff_var(i)),ABS(depresys0_coeff_var(i))]
   GPLOT,X=xvals,Y=ypts,SYM=3,/NOLINES,COL=FSC_COLOR(colors(i))
ENDFOR
AXES,XLABELS=model,YVALS=yvals,NDECS=1,YTITLE='Coefficient of Variation SOI',/GRID,GCOL=1,ORIENTATION=90
GLEGEND,LEGPOS=9,STYLE=[-1,-1,-1,-1],SYM=[3,3,3,3],LABELS=['SON','JJA','MAM','DJF'],COL=[FSC_COLOR('green'),FSC_COLOR('blue'),FSC_COLOR('orange'),FSC_COLOR('red')]
PSCLOSE

STOP
END
