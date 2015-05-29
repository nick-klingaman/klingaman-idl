PRO qld_eots_nino_frequencies_silo025

; Timeseries of EOTs for SILO seasonal means
silo_dir='/home/ss901165/datasets_mango/SILO/one_quarter'
silo_smeans_infiles=[silo_dir+'/SILO.dec-feb_smeans.1900-2007.eots.nc',$
                     silo_dir+'/SILO.mar-may_smeans.1900-2007.eots.nc',$
                     silo_dir+'/SILO.jun-aug_smeans.1900-2007.eots.nc',$
                     silo_dir+'/SILO.sep-nov_smeans.1900-2007.eots.nc']

nino_dir='/home/ss901165/datasets/NINO'
nino4_smeans_infiles=[nino_dir+'/nino4_hadisst.dec-feb_smeans.1871-2007.nc',$
                      nino_dir+'/nino4_hadisst.mar-may_smeans.1871-2007.nc',$
                      nino_dir+'/nino4_hadisst.jun-aug_smeans.1871-2007.nc',$
                      nino_dir+'/nino4_hadisst.sep-nov_smeans.1871-2007.nc']
nino_offset=29

n_seasons=N_ELEMENTS(silo_smeans_infiles)
season_periods=['dec-feb','mar-may','jun-aug','sep-nov']

; Number of EOTs to consider
n_eots=3
; NUmber of years for each EOT
n_years=108

silo_smeans_eots=fltarr(n_seasons,n_years,n_eots)
FOR i=0,n_seasons-1 DO $
   silo_smeans_eots(i,*,*)=REFORM(OPEN_AND_EXTRACT(silo_smeans_infiles(i),'loading',offset=[0,0],count=[n_years,n_eots]))

nino_smeans=fltarr(n_seasons,n_years)
FOR i=0,n_seasons-1 DO $
   nino_smeans(i,*)=REFORM(OPEN_AND_EXTRACT(nino4_smeans_infiles(i),'NINO4',offset=[nino_offset],count=[n_years]))

nino_smeans_anomalies=fltarr(n_seasons,n_years)
FOR i=0,n_seasons-1 DO $
   FOR j=0,n_years-1 DO $
      nino_smeans_anomalies(i,j)=nino_smeans(i,j)-MEAN(nino_smeans(i,*))

FOR i=0,n_seasons-1 DO BEGIN
   FOR j=0,n_eots-1 DO BEGIN
      
      this_season_nino=REFORM(nino_smeans_anomalies(i,*))
      this_eot_silo=REFORM(silo_smeans_eots(i,*,j))

      lanina_pos=where(this_season_nino lt $
                       0-STDDEV(this_season_nino) and $
                       this_eot_silo gt 0)
      lanina_neg=where(this_season_nino lt $
                       0-STDDEV(this_season_nino) and $
                       this_eot_silo lt 0)
      elnino_pos=where(this_season_nino gt $
                       STDDEV(this_season_nino) and $
                       this_eot_silo gt 0)
      elnino_neg=where(this_season_nino gt $
                       STDDEV(this_season_nino) and $
                       this_eot_silo lt 0)
      neutral_pos=where(this_season_nino lt STDDEV(this_season_nino) and $
                        this_season_nino gt 0-STDDEV(this_season_nino) and $
                        this_eot_silo gt 0)
      neutral_neg=where(this_season_nino lt STDDEV(this_season_nino) and $
                        this_season_nino gt 0-STDDEV(this_season_nino) and $
                        this_eot_silo lt 0)      

      wet_warm=where(this_eot_silo gt MEAN(this_eot_silo)+STDDEV(this_eot_silo,/NaN) and $
                     this_season_nino gt 0)
      wet_cold=where(this_eot_silo gt MEAN(this_eot_silo)+STDDEV(this_eot_silo,/NaN) and $
                     this_season_nino lt 0)
      dry_warm=where(this_eot_silo lt MEAN(this_eot_silo)-STDDEV(this_eot_silo,/NaN) and $
                     this_season_nino gt 0)
      dry_cold=where(this_eot_silo lt MEAN(this_eot_silo)-STDDEV(this_eot_silo,/NaN) and $
                     this_season_nino lt 0)
      neutral_warm=where(this_eot_silo gt MEAN(this_eot_silo)-STDDEV(this_eot_silo,/NaN) and $
                         this_eot_silo lt MEAN(this_eot_silo)+STDDEV(this_eot_silo,/NaN) and $
                         this_season_nino gt 0)
      neutral_cold=where(this_eot_silo gt MEAN(this_eot_silo)-STDDEV(this_eot_silo,/NaN) and $
                         this_eot_silo lt MEAN(this_eot_silo)+STDDEV(this_eot_silo,/NaN) and $
                         this_season_nino lt 0)

      lanina_wet=where(this_eot_silo gt MEAN(this_eot_silo)+STDDEV(this_eot_silo) and $
                       this_season_nino lt 0-STDDEV(this_season_nino))
      elnino_wet=where(this_eot_silo gt MEAN(this_eot_silo)+STDDEV(this_eot_silo) and $
                       this_season_nino gt STDDEV(this_season_nino))
      lanina_dry=where(this_eot_silo lt MEAN(this_eot_silo)-STDDEV(this_eot_silo) and $
                       this_season_nino lt 0-STDDEV(this_season_nino))
      elnino_dry=where(this_eot_silo lt MEAN(this_eot_silo)-STDDEV(this_eot_silo) and $
                       this_season_nino gt STDDEV(this_season_nino))
      neutral_wet=where(this_eot_silo gt MEAN(this_eot_silo)+STDDEV(this_eot_silo) and $
                        this_season_nino lt STDDEV(this_season_nino) and this_season_nino gt 0-STDDEV(this_season_nino))
      neutral_dry=where(this_eot_silo lt MEAN(this_eot_silo)-STDDEV(this_eot_silo) and $
                        this_season_nino lt STDDEV(this_season_nino) and this_season_nino gt 0-STDDEV(this_season_nino))
      
      neutral_elnino=where(this_eot_silo gt MEAN(this_eot_silo)-STDDEV(this_eot_silo,/NaN) and $
                         this_eot_silo lt MEAN(this_eot_silo)+STDDEV(this_eot_silo,/NaN) and $
                         this_season_nino gt STDDEV(this_season_nino))
      neutral_lanina=where(this_eot_silo gt MEAN(this_eot_silo)-STDDEV(this_eot_silo,/NaN) and $
                         this_eot_silo lt MEAN(this_eot_silo)+STDDEV(this_eot_silo,/NaN) and $
                         this_season_nino lt 0-STDDEV(this_season_nino))

      lanina_regression=REGRESS(this_season_nino[[lanina_pos,lanina_neg]],this_eot_silo[[lanina_pos,lanina_neg]],CONST=lanina_constant,$
                                CORRELATION=lanina_correlation)
      elnino_regression=REGRESS(this_season_nino[[elnino_pos,elnino_neg]],this_eot_silo[[elnino_pos,elnino_neg]],CONST=elnino_constant,$
                                CORRELATION=elnino_correlation)
      neutral_regression=REGRESS(this_season_nino[[neutral_pos,neutral_neg]],this_eot_silo[[neutral_pos,neutral_neg]],CONST=neutral_constant,$
                                 CORRELATION=neutral_correlation)
      all_regression=REGRESS(this_season_nino,this_eot_silo,CONST=all_constant,CORRELATION=all_correlation)

      psfile='/home/ss901165/idl/queensland/eots/qld_eots_nino_frequencies_silo025.'+season_periods(i)+'_eot'+STRTRIM(STRING(j+1),1)+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1500,SPACE2=200,XOFFSET=1000,YOFFSET=1200,TFONT=2,TCHARSIZE=100
      ymin=MIN(REFORM(FLOOR(this_eot_silo/100)),/NaN)*100-100
      ymax=MAX(REFORM(FLOOR(this_eot_silo/100)),/NaN)*100+250
      print,ymin,ymax
      GSET,XMIN=-2,XMAX=2,YMIN=ymin,YMAX=ymax,TITLE=season_periods(i)+' mean Nino 4 against EOT '+STRTRIM(STRING(j+1),1)+' of '+season_periods(i)+' rainfall'
      GPLOT,Y=this_eot_silo,X=this_season_nino,SYM=6,/NOLINES
      
      AXES,XSTEP=0.4,YSTEP=100,XTITLE='Seasonal-mean Nino 4',YTITLE='Value of EOT '+STRTRIM(STRING(j+1),1),NDECS=2
      GPLOT,X=[-2,2],Y=[0,0],STYLE=2
      GPLOT,X=[0,0],Y=[ymin,ymax],STYLE=2
      CS,SCALE=1,NCOLS=3
      red=FSC_COLOR("red",8)
      blue=FSC_COLOR("blue",9)
      purple=FSC_COLOR("purple",10)      
      black=FSC_COLOR("black",11)
      pink=FSC_COLOR("hot pink",12)
      cyan=FSC_COLOR("cyan",13)
      salmon=FSC_COLOR("plum",14)
      darkblue=FSC_COLOR("navy",15)
      darkpurple=FSC_COLOR("dark orchid",16)
      orange=FSC_COLOR("orange",17)

      GPLOT,X=this_season_nino[[lanina_pos,lanina_neg]],Y=this_season_nino[[lanina_pos,lanina_neg]]*lanina_regression(0)+lanina_constant,$
            THICK=150,COL=9
      GPLOT,X=this_season_nino[[elnino_pos,elnino_neg]],Y=this_season_nino[[elnino_pos,elnino_neg]]*elnino_regression(0)+elnino_constant,$
            THICK=150,COL=8
      GPLOT,X=this_season_nino,Y=this_season_nino*all_regression(0)+all_constant,THICK=150,COL=11
      
      GPLOT,X=this_season_nino[elnino_pos],Y=this_eot_silo[elnino_pos],COL=10,SYM=6,/NOLINES
      GPLOT,X=this_season_nino[elnino_neg],Y=this_eot_silo[elnino_neg],COL=8,SYM=6,/NOLINES
      GPLOT,X=this_season_nino[lanina_pos],Y=this_eot_silo[lanina_pos],COL=9,SYM=6,/NOLINES
      GPLOT,X=this_season_nino[lanina_neg],Y=this_eot_silo[lanina_neg],COL=10,SYM=6,/NOLINES
      GPLOT,X=[0.4,0.4],Y=[ymax*0.56,ymax*0.92],STYLE=0,THICK=200
      GPLOT,X=[1.9,1.9],Y=[ymax*0.56,ymax*0.92],STYLE=0,THICK=200
      GPLOT,X=[0.4,1.9],Y=[ymax*0.56,ymax*0.56],STYLE=0,THICK=200
      GPLOT,X=[0.4,1.9],Y=[ymax*0.92,ymax*0.92],STYLE=0,THICK=200
      GPLOT,X=[1.0,1.0],Y=[ymax*0.56,ymax*0.92],STYLE=0,THICK=200
      GPLOT,X=[1.3,1.3],Y=[ymax*0.56,ymax*0.92],STYLE=0,THICK=200
      GPLOT,X=[0.4,1.9],Y=[ymax*0.70,ymax*0.70],STYLE=0,THICK=200
      GPLOT,X=[0.4,1.9],Y=[ymax*0.78,ymax*0.78],STYLE=0,THICK=200
      GPLOT,X=[0.4],Y=[ymax*0.96],TEXT='Nina',COL=9,ALIGN=0.0
      GPLOT,X=[0.98],Y=[ymax*0.93],TEXT='-ve N4',COL=13,ALIGN=1.0
      GPLOT,X=[1.3],Y=[ymax*0.96],TEXT='Nino',COL=8,ALIGN=0.0
      GPLOT,X=[1.9],Y=[ymax*0.93],TEXT='+ve N4',COL=12,ALIGN=1.0
      GPLOT,X=[1.0],Y=[ymax*0.96],TEXT='Neut',COL=11,ALIGN=0.0
      
      ;GPLOT,X=[0.01],Y=[ymax*0.87],TEXT='+ve EOT',COL=9,ALIGN=0.0
      ;GPLOT,X=[0.01],Y=[ymax*0.65],TEXT='-ve EOT',COL=8,ALIGN=0.0
      GPLOT,X=[0.05],Y=[ymax*0.87],TEXT='Wet',COL=9,ALIGN=0.0
      GPLOT,X=[0.05],Y=[ymax*0.73],TEXT='Neut',COL=11,ALIGN=0.0
      GPLOT,X=[0.05],Y=[ymax*0.65],TEXT='Dry',COL=8,ALIGN=0.0
      GPLOT,X=[0.37],Y=[ymax*0.79],TEXT='+EOT',COL=13,ALIGN=1.0
      GPLOT,X=[0.37],Y=[ymax*0.58],TEXT='-EOT',COL=12,ALIGN=1.0
      
      ; El Nino and above-zero loading - plot in purple
      GPLOT,X=[1.55],Y=[ymax*0.79],TEXT=N_ELEMENTS(elnino_pos),COL=10,ALIGN=1.0 
      ; Wet and above-zero Nino 4 - plot in salmon
      GPLOT,X=[1.75],Y=[ymax*0.87],TEXT=N_ELEMENTS(wet_warm),COL=14,ALIGN=1.0
      ; La Nina and above-zero loading - plot in blue
      GPLOT,X=[0.65],Y=[ymax*0.79],TEXT=N_ELEMENTS(lanina_pos),COL=9,ALIGN=1.0
      ; Wet and below-zero Nino 4 - plot in cyan
      GPLOT,X=[0.85],Y=[ymax*0.87],TEXT=N_ELEMENTS(wet_cold),COL=13,ALIGN=1.0
      ; La Nina and below-zero loading - plot in purple
      GPLOT,X=[0.65],Y=[ymax*0.58],TEXT=N_ELEMENTS(lanina_neg),COL=10,ALIGN=1.0
      ; El Nino and below-zero loading - plot in red
      GPLOT,X=[1.55],Y=[ymax*0.58],TEXT=N_ELEMENTS(elnino_neg),COL=8,ALIGN=1.0
      ; Dry and below-zero Nino 4 - plot in salmon
      GPLOT,X=[0.85],Y=[ymax*0.65],TEXT=N_ELEMENTS(dry_cold),COL=14,ALIGN=1.0
      ; Dry and above-zero Nino 4 - plot in pink
      GPLOT,X=[1.75],Y=[ymax*0.65],TEXT=N_ELEMENTS(dry_warm),COL=12,ALIGN=1.0

      ; Neutral Nino 4 and below-zero loading - plot in black
      GPLOT,X=[1.12],Y=[ymax*0.58],TEXT=N_ELEMENTS(neutral_neg),COL=11,ALIGN=1.0
      ; Neutral Nino 4 and above-zero loading - plot in black
      GPLOT,X=[1.12],Y=[ymax*0.79],TEXT=N_ELEMENTS(neutral_pos),COL=11,ALIGN=1.0
      ; Neutral loading and below-zero Nino 4 - plot in black
      GPLOT,X=[0.85],Y=[ymax*0.73],TEXT=N_ELEMENTS(neutral_cold),COL=11,ALIGN=1.0
      ; Neutral loading and above-zero Nino 4 - plot in black      
      GPLOT,X=[1.75],Y=[ymax*0.73],TEXT=N_ELEMENTS(neutral_warm),COL=11,ALIGN=1.0
      ; Neutral Nino 4 and wet - plot in black
      GPLOT,X=[1.12],Y=[ymax*0.87],TEXT=N_ELEMENTS(neutral_wet),COL=11,ALIGN=1.0
      ; Neutral Nino 4 and dry - plot in black
      GPLOT,X=[1.12],Y=[ymax*0.65],TEXT=N_ELEMENTS(neutral_dry),COL=11,ALIGN=1.0
      ; Neutral loading and La Nina - plot in black
      GPLOT,X=[0.55],Y=[ymax*0.73],TEXT=N_ELEMENTS(neutral_lanina),COL=11,ALIGN=1.0
      ; Neutral loading and El Nino - plot in black
      GPLOT,X=[1.45],Y=[ymax*0.73],TEXT=N_ELEMENTS(neutral_elnino),COL=11,ALIGN=1.0

      ; La Nina and wet - plot in dark blue
      GPLOT,X=[0.55],Y=[ymax*0.87],TEXT=N_ELEMENTS(lanina_wet),COL=15,ALIGN=1.0
      ; La Nina and dry - plot in dark purple
      GPLOT,X=[0.55],Y=[ymax*0.65],TEXT=N_ELEMENTS(lanina_dry),COL=16,ALIGN=1.0
      ; El Nino and dry - plot in orange
      GPLOT,X=[1.45],Y=[ymax*0.65],TEXT=N_ELEMENTS(elnino_dry),COL=17,ALIGN=1.0
      ; El Nino and wet - plot in dark purple
      GPLOT,X=[1.45],Y=[ymax*0.87],TEXT=N_ELEMENTS(elnino_wet),COL=16,ALIGN=1.0

      GPLOT,X=-1.9,Y=ymin*0.85,TEXT='La Nina is < -1*stddev of Nino 4 timeseries',COL=9,ALIGN=0.0
      GPLOT,X=-1.9,Y=ymin*0.95,TEXT='El Nino is > 1*stddev of Nino 4 timeseries',COL=8,ALIGN=0.0
      GPLOT,X=-1.9,Y=ymin*0.65,TEXT='Wet is > mean+stddev of EOT timeseries',COL=13,ALIGN=0.0
      GPLOT,X=-1.9,Y=ymin*0.75,TEXT='Dry is < mean-stddev of EOT timeseries',COL=12,ALIGN=0.0

      ; Mark standard deviations
      GPLOT,X=[-2,2],Y=REPLICATE(MEAN(this_eot_silo)+STDDEV(this_eot_silo),2),STYLE=1,COL=11,THICK=100
      GPLOT,X=[-2,2],Y=REPLICATE(MEAN(this_eot_silo)-STDDEV(this_eot_silo),2),STYLE=1,COL=11,THICK=100
      GPLOT,X=REPLICATE(STDDEV(this_season_nino),2),Y=[ymin,ymax*0.56],STYLE=1,COL=11,THICK=100
      GPLOT,X=REPLICATE(0-STDDEV(this_season_nino),2),Y=[ymin*0.65,ymax],STYLE=1,COL=11,THICK=100

      PSCLOSE,/NOVIEW
   ENDFOR
ENDFOR

STOP
END

