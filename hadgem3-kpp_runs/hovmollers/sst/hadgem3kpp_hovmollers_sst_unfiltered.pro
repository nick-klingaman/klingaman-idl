PRO hadgem3kpp_hovmollers_sst_unfiltered

hadgem3kpp_1m3h_indmem_file='/home/ss901165/um_output3/hadgem3kpp_1m3h_control/hadgem3kpp_1m3h_ctl.indmem.jjasondjfmam.sst.nc'
hadgem3kpp_1m3h_ensmean_file='/home/ss901165/um_output3/hadgem3kpp_1m3h_control/hadgem3kpp_1m3h_ctl.ensmean.jjasondjfmam.sst.nc'
hadgem3kpp_10m24h_indmem_file='/home/ss901165/um_output3/hadgem3kpp_10m24h_control/hadgem3kpp_10m24h_ctl.indmem.jjasondjfmam.sst.nc'
hadgem3kpp_10m24h_ensmean_file='/home/ss901165/um_output3/hadgem3kpp_10m24h_control/hadgem3kpp_10m24h_ctl.ensmean.jjasondjfmam.sst.nc'
hadgem3a_foam_indmem_file='/home/ss901165/um_output3/hadgem3a_foam_control/hadgem3a_foam_ctl.indmem.jjasondjfmam.sst.nc'

hadgem3kpp_nruns=5
hadgem3kpp_ndays=360

hadgem3a_nruns=5

n_sets=2
box=[-10,40,10,240]

mylevs=['-1.35','-1.05','-0.75','-0.45','-0.15','0.15','0.45','0.75','1.05','1.35']

FOR i=0,n_sets-1 DO BEGIN
   CASE i OF 
      0: BEGIN
         infile=hadgem3kpp_1m3h_indmem_file
         clim_infile=hadgem3kpp_1m3h_ensmean_file
         n_members=hadgem3kpp_nruns
         n_days=hadgem3kpp_ndays
         run_name='hadgem3kpp_1m3h_ctl'
         title_name='HadGEM3-KPP 1M-3H Control (5 members)'
      END
      1: BEGIN
         infile=hadgem3kpp_10m24h_indmem_file
         clim_infile=hadgem3kpp_10m24h_ensmean_file
         n_members=hadgem3kpp_nruns
         n_days=hadgem3kpp_ndays
         run_name='hadgem3kpp_10m24h_ctl'
         title_name='HadGEM3-KPP 10M-24H Control (5 members)'
      END
      2: BEGIN
         infile=hadgem3a_foam_indmem_file
         n_members=hadgem3a_nruns
         n_days=hadgem3kpp_ndays
         run_name='hadgem3a_foam_ctl'
         title_name='HadGEM3-A FOAM SST Control (5 members)'
      END
   ENDCASE
   
   FOR j=0,n_members-1 DO BEGIN
      IF j eq 0 THEN BEGIN
         longitude=OPEN_AND_EXTRACT(infile,'longitude')         
         latitude=OPEN_AND_EXTRACT(infile,'latitude')
         DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
         n_lon=N_ELEMENTS(longitude)
         n_lat=N_ELEMENTS(latitude)
         indmem_latavg_sst=fltarr(n_members,n_lon,n_days)
         clim_sst=REFORM(OPEN_AND_EXTRACT(clim_infile,'temp_1',offset=[box_tx(1),box_tx(0),0,0],$
                                          count=[n_lon,n_lat,1,n_days]))-273.15
      ENDIF
      thismem_sst=REFORM(OPEN_AND_EXTRACT(infile,'temp_1',offset=[box_tx(1),box_tx(0),0,0,j],$
                                          count=[n_lon,n_lat,1,n_days,1]))-273.15
      thismem_sst=thismem_sst-clim_sst
      FOR k=0,n_lon-1 DO $
         FOR m=0,n_days-1 DO $
            indmem_latavg_sst(j,k,m)=MEAN(thismem_sst(k,*,m))
   ENDFOR
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/hovmollers/sst/hadgem3kpp_hovmollers_sst_unfiltered.'+run_name+'.allmembers_allmonths.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1800,SPACE2=200,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=80,SPACE3=300,XPLOTS=n_members,SPACING=400
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,white=[7]
   LEVS,MANUAL=mylevs
   FOR j=0,n_members-1 DO BEGIN
      POS,XPOS=j+1
      GSET,XMIN=box(1),XMAX=box(3),YMIN=0,YMAX=n_days,TITLE='Member '+STRTRIM(STRING(j+1),1)
      CON,X=longitude,Y=indgen(n_days),FIELD=REFORM(indmem_latavg_sst(j,*,*)),/NOLINES,/NOCOLBAR
      IF j eq 0 THEN BEGIN
         AXES,XVALS=[60,90,120,150,180,210],$
              XTITLE='Longitude (degrees east)',YTITLE='Day in integration',YVALS=[0,30,60,90,120,150,180,210,240,270,300,330],$
              YLABELS=['1 Jun','1 Jul','1 Aug','1 Sep','1 Oct','1 Nov','1 Dec','1 Jan','1 Feb','1 Mar','1 Apr','1 May'],/NORIGHT
      ENDIF ELSE IF j eq n_members-1 THEN BEGIN
         AXES,YVALS=[0,30,60,90,120,150,180,210,240,270,300,330],$
              YLABELS=['1 Jun','1 Jul','1 Aug','1 Sep','1 Oct','1 Nov','1 Dec','1 Jan','1 Feb','1 Mar','1 Apr','1 May'],/ONLYRIGHT
         AXES,XVALS=[60,90,120,150,180,210],XTITLE='Longitude (degrees east)',/NOLEFT,/NORIGHT
         COLBAR,COORDS=[2000,1200,26000,1600],/UPPER,/LOWER,LEVS=['-9',mylevs,'9'],TITLE='Unfiltered anomalies in daily-mean SST from ensemble mean (!Uo!NC) for '+title_name
      ENDIF ELSE $
         AXES,XVALS=[60,90,120,150,180,210],XTITLE='Longitude (degrees east)',/NOLEFT,/NORIGHT
   ENDFOR

   PSCLOSE
ENDFOR

STOP

END
