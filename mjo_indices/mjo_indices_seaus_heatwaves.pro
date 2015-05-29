PRO mjo_indices_seaus_heatwaves

pt=[-37.5,144.375]
;rmm_file='/home/ss901165/um_output6/xihvd/rmm_indices.nc'
;temp_file='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans.years1-60.surftemp.nc'
rmm_file='/home/ss901165/um_output6/xihvd/hadgem3kpp_nrglobal123_n96.jan-dec_dmeans.years1-60.rmm_indices.nc'
temp_file='/home/ss901165/um_output6/xihvd/hadgem3kpp_nrglobal123_n96.jan-dec_dmeans.years1-60.surf_temp.nc'

start_time=240
n_days=90
n_years=60

amp=OPEN_AND_EXTRACT(rmm_file,'amplitude',offset=[0,start_time],count=[n_years,n_days])
phase=OPEN_AND_EXTRACT(rmm_file,'phase',offset=[0,start_time],count=[n_years,n_days])

lonpt=NEAREST(OPEN_AND_EXTRACT(temp_file,'longitude'),pt(1))
latpt=NEAREST(OPEN_AND_EXTRACT(temp_file,'latitude'),pt(0))

temps=REFORM(OPEN_AND_EXTRACT(temp_file,'temp_1',offset=[lonpt,latpt,start_time,0],count=[1,1,n_days,n_years]))

byphase=fltarr(9)
all_years=fltarr(n_days,n_years/10)
FOR i=0,n_days-1 DO BEGIN
  today_temps=REFORM(temps(i,*))
  today_amp=REFORM(amp(*,i))
  today_phase=REFORM(phase(*,i))
  sorted=SORT(today_temps)
  threshold=today_temps[sorted(n_years/10*9)]
  FOR j=n_years/10*9,n_years-1 DO BEGIN
      IF today_amp(sorted(j)) lt 0.9 THEN BEGIN
	byphase(0)=byphase(0)+1
      ENDIF ELSE BEGIN
	byphase(today_phase(sorted(j)))=byphase(today_phase(sorted(j)))+1
      ENDELSE
  ;print,threshold,phase(sorted(n_years/10*9:n_years-1)),amp(sorted(n_years/10*9:n_years-1))
  ENDFOR
  all_years(i,*)=sorted(n_years/10*9:n_years-1)
ENDFOR
n_lengths=6
lengths=intarr(n_lengths)
lengths_byphase=intarr(9,n_lengths)
FOR i=0,n_days-n_lengths-1 DO BEGIN
   FOR k=0,n_years/10-1 DO BEGIN 
      max=0
      FOR j=1,n_lengths DO BEGIN
         temp=all_years(i:i+j-1,*)
         IF N_ELEMENTS(where(temp eq all_years(i,k))) eq j and $
            all_years(i,k) ne -999. THEN $
               max=j
      ENDFOR
      IF max gt 0 THEN BEGIN
         lengths(max-1)=lengths(max-1)+1 
         IF amp(all_years(i,k),i) ge 0.9 THEN BEGIN
            lengths_byphase(phase(all_years(i,k),i),max-1)=lengths_byphase(phase(all_years(i,k),i),max-1)+1
         ENDIF ELSE $
            lengths_byphase(0,max-1)=lengths_byphase(0,max-1)+1
         temp=all_years(i:i+max-1,*)
         temp[where(temp eq all_years(i,k))]=-999.
                                ;all_years(i:i+max-1,*)=temp
                                ;all_years[where(all_years(i:i+max-1,*) eq all_years(i,k))]=-999
         print,i,k,max,all_years(i,k),all_years[where(all_years(i:i+max-1,*) eq all_years(i,k))]
         all_years(i:i+max-1,*)=temp
      ENDIF
   ENDFOR
ENDFOR

;byphase=byphase-2*(indgen(9)-4)
;FOR i=0,n_lengths-1 DO BEGIN
;   lengths_byphase(*,i)=REFORM(lengths_byphase(*,i))-[0,0,-2,-1,0,0,1,1,0]
;   byphase=byphase-[0,0,-1,0,0,0,0,0,0]*(i+1)
;   print,lengths_byphase(6,*)
;ENDFOR

clim_mjo=fltarr(9)
clim_mjo(0)=N_ELEMENTS(where(amp lt 0.9))
FOR i=1,8 DO $
   clim_mjo(i)=N_ELEMENTS(where(amp ge 0.9 and phase eq i))

;ENDFOR 
psfile='/home/ss901165/idl/mjo_indices/mjo_indices_seaus_heatwaves.melbourne_hist_byphase_sep-nov.kpp_fwgbl_ctl_n96.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=150,TFONT=6,MARGIN=3500,XOFFSET=1000,SPACE2=200,SPACE3=200
GSET,XMIN=0,XMAX=9,YMIN=0,YMAX=300,TITLE='For (37.5S,144.4E), SON days with daily mean temp > 90th %ile (bars) and clim freq (line) - 60 yrs'
fire=FSC_COLOR('firebrick',30)
orange=FSC_COLOR('orange',31)
darkorange=FSC_COLOR('orangered',32)
red=FSC_COLOR('red',33)
purple=FSC_COLOR('brown',34)
HIST,X=indgen(9)+0.2,Y=byphase,FILLCOL=30,WIDTH=70
GPLOT,X=indgen(9)+0.2,Y=clim_mjo*0.1,SYM=4,STYLE=2
AXES,XVALS=indgen(9)+0.5,YSTEP=30,YMINOR=15,XLABELS=['Weak','1','2','3','4','5','6','7','8'],$
  xtitle='MJO phase',ytitle='Number of days above 90th percentile temperature',/NORIGHT
GSET,XMIN=0,XMAX=9,YMIN=0,YMAX=0.5
FOR i=0,8 DO BEGIN
 FOR j=0,2 DO BEGIN
    HIST,X=i+0.4+0.15*j,Y=TOTAL(lengths_byphase(i,j:n_lengths-1))/FLOAT(TOTAL(lengths(j:n_lengths-1))),WIDTH=80,FILLCOL=31+j
    GPLOT,X=i+0.3+0.15*j,Y=0.48-0.02*j,TEXT=STRTRIM(STRING(FLOOR(TOTAL(lengths_byphase(i,j:n_lengths-1)))),1),ALIGN=0.5,CHARSIZE=75,COL=31+j
 ENDFOR
ENDFOR
AXES,YSTEP=0.04,YMINOR=0.02,YTITLE='Fraction of events of each length',/ONLYRIGHT,NDECS=2
PSCLOSE,/NOVIEW



STOP
END
