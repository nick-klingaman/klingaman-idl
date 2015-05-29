PRO hadgem3kpp_fcorr_mmean_sst_bias
  
; Plot the monthly-mean SST biases against some climatology
; from a HadGEM3-KPP simulation.

months=['jun','jul','aug','sep','oct','nov','dec','jan','feb','mar','apr','may']
years=['h9','i0']

runid='xgcii'
hadgem3kpp_indir='/home/ss901165/um_output5/'+runid
clim_indir='/home/ss901165/datasets/NCOF_OCEAN/FOAM_bymonth'

landfrac_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/landfrac_n96_hadgem3-7.3.nc'

box=[-40,0,40,360]

n_months=N_ELEMENTS(months)
n_years=N_ELEMENTS(years)-1

mylevs=['17','18','19','20','21','22','23','24','25','26','27','28','29','30','31','32']
mylevs_diff=['-3.0','-2.6','-2.2','-1.8','-1.4','-1.0','-0.6','-0.2','0.2','0.6','1.0','1.4','1.8','2.2','2.6','3.0']

landfrac_longitude=OPEN_AND_EXTRACT(landfrac_infile,'longitude')
landfrac_latitude=OPEN_AND_EXTRACT(landfrac_infile,'latitude')
DEFINE_BOUNDARIES,box,landfrac_latitude,landfrac_longitude,landfrac_box_tx,/LIMIT
landfrac_nlon=N_ELEMENTS(landfrac_longitude)
landfrac_nlat=N_ELEMENTS(landfrac_latitude)
landfrac=REFORM(OPEN_AND_EXTRACT(landfrac_infile,'lsm',$
                                 offset=[landfrac_box_tx(1),landfrac_box_tx(0),0,0],$
                                 count=[landfrac_nlon,landfrac_nlat,1,1]))

FOR i=0,n_years-1 DO BEGIN
   this_year=years(i)
   FOR j=0,n_months-1 DO BEGIN
      hadgem3kpp_infile=hadgem3kpp_indir+'/'+this_year+'/'+runid+'a.pm'+this_year+months(j)+'.nc'
      hadgem3kpp_longitude=OPEN_AND_EXTRACT(hadgem3kpp_infile,'longitude')
      hadgem3kpp_latitude=OPEN_AND_EXTRACT(hadgem3kpp_infile,'latitude')
      DEFINE_BOUNDARIES,box,hadgem3kpp_latitude,hadgem3kpp_longitude,hadgem3kpp_box_tx,/LIMIT
      hadgem3kpp_nlon=N_ELEMENTS(hadgem3kpp_longitude)
      hadgem3kpp_nlat=N_ELEMENTS(hadgem3kpp_latitude)
 
      clim_infile=clim_indir+'/FOAM_'+months(j)+'mean_mixedlayer_temperature.clim.n96.nc'
      clim_longitude=OPEN_AND_EXTRACT(clim_infile,'longitude')
      clim_latitude=OPEN_AND_EXTRACT(clim_infile,'latitude')
      DEFINE_BOUNDARIES,box,clim_latitude,clim_longitude,clim_box_tx,/LIMIT
      clim_nlon=N_ELEMENTS(clim_longitude)
      clim_nlat=N_ELEMENTS(clim_latitude)

      hadgem3kpp_sst=REFORM(OPEN_AND_EXTRACT(hadgem3kpp_infile,'temp_1',$
                                             offset=[hadgem3kpp_box_tx(1),hadgem3kpp_box_tx(0),0,0],$
                                             count=[hadgem3kpp_nlon,hadgem3kpp_nlat,1,1]))
      clim_sst=REFORM(OPEN_AND_EXTRACT(clim_infile,'sea_water_potential_temperature',$
                                       offset=[clim_box_tx(1),clim_box_tx(0),0],$
                                       count=[clim_nlon,clim_nlat,1]))
      hadgem3kpp_sst[where(landfrac eq 1)]=!Values.F_NaN
      clim_sst[where(landfrac eq 1)]=!Values.F_NaN

      diff_sst=hadgem3kpp_sst-clim_sst
     
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_mmean_sst_bias.'+$
             runid+'_'+this_year+months(j)+'_sst.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1200,XOFFSET=1000,YOFFSET=1800,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=10000
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
      LEVS,MANUAL=mylevs
      MAP,LATMIN=box(0),LONMIN=box(1),LATMAX=box(2),LONMAX=box(3)
      CON,X=hadgem3kpp_longitude,Y=hadgem3kpp_latitude,FIELD=hadgem3kpp_sst-273.15,TITLE='SST from HadGEM3-KPP run '+runid+$
          ' for year '+this_year+' month '+months(j),CB_TITLE='!Uo!NC',/BLOCK,/NOLINES
      GPLOT,X=[40,40],Y=[-30,30]
      GPLOT,X=[200,200],Y=[-30,30]
      GPLOT,X=[40,200],Y=[-30,-30]
      GPLOT,X=[40,200],Y=[30,30]
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_mmean_sst_bias.'+$
             'foam_'+months(j)+'_sst.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1200,XOFFSET=1000,YOFFSET=1800,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=10000 
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
      LEVS,MANUAL=mylevs
      MAP,LATMIN=box(0),LONMIN=box(1),LATMAX=box(2),LONMAX=box(3)
      CON,X=clim_longitude,Y=clim_latitude,FIELD=clim_sst-273.15,TITLE='Clim SST from FOAM '+runid+$
          ' for '+months(j),CB_TITLE='!Uo!NC',/BLOCK,/NOLINES
      GPLOT,X=[40,40],Y=[-30,30]
      GPLOT,X=[200,200],Y=[-30,30]
      GPLOT,X=[40,200],Y=[-30,-30]
      GPLOT,X=[40,200],Y=[30,30]
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_mmean_sst_bias.'+$
             runid+'_'+this_year+months(j)+'_sst.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1200,XOFFSET=1000,YOFFSET=1800,TFONT=2,TCHARSIZE=90,SPACE3=400,YSIZE=10000
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1
      LEVS,MANUAL=mylevs_diff
      MAP,LATMIN=box(0),LONMIN=box(1),LATMAX=box(2),LONMAX=box(3)
      CON,X=hadgem3kpp_longitude,Y=hadgem3kpp_latitude,FIELD=diff_sst,TITLE='SST bias in HadGEM3-KPP '+runid+$
          ' for year '+this_year+' month '+months(j)+' (against FOAM clim)',CB_TITLE='!Uo!NC',/BLOCK,/NOLINES
      GPLOT,X=[40,40],Y=[-30,30]
      GPLOT,X=[200,200],Y=[-30,30]
      GPLOT,X=[40,200],Y=[-30,-30]
      GPLOT,X=[40,200],Y=[30,30]
      AXES
      PSCLOSE

      IF months(j) eq 'dec' THEN $
         this_year=years(i+1)
   ENDFOR
ENDFOR

STOP
END
