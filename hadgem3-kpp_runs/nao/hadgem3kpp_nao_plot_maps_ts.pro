PRO hadgem3kpp_nao_plot_maps_ts

nmodels=4
box=[20,0,90,360]

pattern_levs=['-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5']

FOR m=0,nmodels-1 DO BEGIN
   CASE m OF
      0 : BEGIN
         nao_file='/home/ss901165/public_html/cvdp_output/MetUM-GOML1_N216.cvdp_data.1992-2057.nc'
         name='MetUM-GOML1 N216'
         psname='goml1'
      END
      1 : BEGIN
         nao_file='/home/ss901165/public_html/cvdp_output/MetUM-GA3_GOML1_31-day_SST.cvdp_data.1992-2057.nc'
         name='MetUM-GA3 31-day GOML1 SST'
         psname='ga3-31d'
      END
      2 : BEGIN
         nao_file='/home/ss901165/public_html/cvdp_output/MetUM-GA3_GOML1_clim_SST.cvdp_data.1992-2057.nc'
         name='MetUM-GA3 clim GOML1 SST'
         psname='ga3-clim'
      END
      3 : BEGIN         
         nao_file='/home/ss901165/public_html/cvdp_output/ERA-Interim.cvdp_data.1979-2013.nc'
         name='ERA-Interim'
         psname='eraint'
      END
   ENDCASE
   
   print,m,nao_file,psname
   lon=OPEN_AND_EXTRACT(nao_file,'lon')
   lat=OPEN_AND_EXTRACT(nao_file,'lat')
   DEFINE_BOUNDARIES,box,lat,lon,box_tx,/LIMIT
   nlon=N_ELEMENTS(lon)
   nlat=N_ELEMENTS(lat)

   nao_pattern=OPEN_AND_EXTRACT(nao_file,'nao_mon',offset=[box_tx(1),box_tx(0)],count=[nlon,nlat])
   nao_ts=OPEN_AND_EXTRACT(nao_file,'nao_pc_mon')
   nao_ts=nao_ts/STDDEV(nao_ts)

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/nao/hadgem3kpp_nao_plot_maps_ts.nao_eof_'+psname+'.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,MARGIN=2500,CB_WIDTH=110,SPACE3=1500
   MAP,LONMIN=-180,LONMAX=180,LATMIN=box(0),LATMAX=box(2),/NH
   LEVS,MANUAL=pattern_levs
   CS,SCALE=1,NCOL=N_ELEMENTS(pattern_levs)+1,white=[9]
   CON,X=lon,Y=lat,FIELD=nao_pattern,TITLE='Regress MSLP anom on norm 1st PC from'+name,$
       CB_TITLE='hPa',/NOLINES,/NOLINELABELS,/BLOCK
   ;GPLOT,X=[20,20],Y=[0,360]
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/nao/hadgem3kpp_nao_plot_maps_ts.nao_ts_'+psname+'.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,MARGIN=2500,CB_WIDTH=110,SPACE3=1500
   GSET,XMIN=0,XMAX=N_ELEMENTS(nao_ts)/12.,YMIN=-4,YMAX=4
   red=FSC_COLOR('red',30)
   blue=FSC_COLOR('blue',31)
   black=FSC_COLOR('black',32)
   FOR i=0,N_ELEMENTS(nao_ts)-1 DO BEGIN
      IF nao_ts(i) gt 0 THEN $
         HIST,X=i/12.+0.5,Y=nao_ts(i),FILLCOL=32,WIDTH=5
      IF nao_ts(i) lt 0 THEN $
         HIST,X=i/12.+0.5,Y=nao_ts(i),FILLCOL=32,WIDTH=5
   ENDFOR
   nao_ts_smooth=SMOOTH(nao_ts,13)
   GPLOT,X=(findgen(N_ELEMENTS(nao_ts)-13)+6.5)/12.,Y=nao_ts_smooth(6:N_ELEMENTS(nao_ts)-7),THICK=200,COL=FSC_COLOR('red')
   
   AXES,XSTEP=3,XMINOR=1,YSTEP=1,YMINOR=0.5,YTITLE='Monthly NAO index (normalised)',XTITLE='Year'
   PSCLOSE,/NOVIEW


ENDFOR

STOP
END
