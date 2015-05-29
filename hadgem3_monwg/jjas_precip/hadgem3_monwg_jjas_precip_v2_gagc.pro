PRO hadgem3_monwg_jjas_precip_v2_gagc

monwg='/home/ss901165/um_output3/hadgem3_monwg'
gc2='/home/ss901165/um_output6/gc2'

hg2a=monwg+'/ageyb/hadgem2a_final_n96_amip2_ageyb.jan-dec_dmeans.1979-1998.precip.nc'
ga2=monwg+'/airxv/hadgem3a_morph3_final_n96_amip2_airxv.jan-dec_dmeans.1982-2008.precip.nc'
ga3=monwg+'/akkvi/hadgem3a_captivate_final_n96_amip2_akkvi.jan-dec_dmeans.1979-2005.precip.nc'
ga4=monwg+'/aliur/hadgem3a_amip_n96_ga40_aliur.jan-dec_dmeans.1982-2008.precip.nc'
ga6=gc2+'/antia/hadgem3_ga6_n96.jan-dec_dmeans.years1-27.precip.nc'
gc2_n96=gc2+'/anqjm/hadgem3_gc2_n96_orca025.jan-dec_dmeans.years1-41.precip.nc'

trmm_infile='/home/ss901165/datasets/TRMM_3B42V6/n96/TRMM_3B42v6A.jan-dec_dmeans.1999-2011.n96.nc'

mylevs=['1','3','5','7','9','11','13','15','17','19','21','23','25']
mylevs_bias=['-13','-11','-9','-7','-5','-3','-1','1','3','5','7','9','11','13']
mylevs_diff=['-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5']

box=[-40,0,40,360]

n_sets=16
FOR m=14,n_sets-1 DO BEGIN
   print,m
   CASE m OF
      0 : BEGIN
         expt_infile=hg2a
         ctrl_infile=trmm_infile
         expt_name='hadgem2a'
         ctrl_name='trmm'
         expt_desc='HadGEM2-A'
         ctrl_desc='TRMM'
         expt_offset=150
         ctrl_offset=150
         expt_nyears=20
         expt_count=120
         ctrl_nyears=13
         ctrl_count=120         
         ctrl_revlat=1
         expt_mult=86400.
         ctrl_mult=1.
         this_mylevs_diff=mylevs_diff
      END 
      1 : BEGIN
         expt_infile=ga2
         ctrl_infile=trmm_infile
         expt_name='morph3'
         ctrl_name='trmm'
         expt_desc='MetUM MORPH3'
         ctrl_desc='TRMM'
         expt_offset=150
         ctrl_offset=150
         expt_nyears=27
         expt_count=120
         ctrl_nyears=13
         ctrl_count=120         
         ctrl_revlat=1
         expt_mult=86400.
         ctrl_mult=1.
         this_mylevs_diff=mylevs_diff
      END 
      2 : BEGIN
         expt_infile=ga3
         ctrl_infile=trmm_infile
         expt_name='ga3'
         ctrl_name='trmm'
         expt_desc='MetUM GA3'
         ctrl_desc='TRMM'
         expt_offset=150
         ctrl_offset=150
         expt_nyears=27
         expt_count=120
         ctrl_nyears=13
         ctrl_count=120         
         ctrl_revlat=1
         expt_mult=86400.
         ctrl_mult=1.
         this_mylevs_diff=mylevs_diff
      END 
      3 : BEGIN
         expt_infile=ga4
         ctrl_infile=trmm_infile
         expt_name='ga4'
         ctrl_name='trmm'
         expt_desc='MetUM GA4'
         ctrl_desc='TRMM'
         expt_offset=150
         ctrl_offset=150
         expt_nyears=27
         expt_count=120
         ctrl_nyears=13
         ctrl_count=120         
         ctrl_revlat=1
         expt_mult=86400.
         ctrl_mult=1.
         this_mylevs_diff=mylevs_diff
      END 
      4 : BEGIN
         expt_infile=ga6
         ctrl_infile=trmm_infile
         expt_name='ga6'
         ctrl_name='trmm'
         expt_desc='MetUM GA6'
         ctrl_desc='TRMM'
         expt_offset=150
         ctrl_offset=150
         expt_nyears=27
         expt_count=120
         ctrl_nyears=13
         ctrl_count=120         
         ctrl_revlat=1
         expt_mult=86400.
         ctrl_mult=1.
         this_mylevs_diff=mylevs_diff
      END 
      5 : BEGIN
         expt_infile=ga2
         ctrl_infile=hg2a
         expt_name='morph3'
         ctrl_name='hadgem2a'
         expt_desc='MetUM MORPH3'
         ctrl_desc='HadGEM2-A'
         expt_offset=150
         ctrl_offset=150
         expt_nyears=27
         expt_count=120
         ctrl_nyears=20
         ctrl_count=120         
         ctrl_revlat=0
         expt_mult=86400.
         ctrl_mult=86400.
         this_mylevs_diff=mylevs_diff
      END 
      6 : BEGIN
         expt_infile=ga3
         ctrl_infile=hg2a
         expt_name='ga3'
         ctrl_name='hadgem2a'
         expt_desc='MetUM GA3'
         ctrl_desc='HadGEM2-A'
         expt_offset=150
         ctrl_offset=150
         expt_nyears=27
         expt_count=120
         ctrl_nyears=20
         ctrl_count=120         
         ctrl_revlat=0
         expt_mult=86400.
         ctrl_mult=86400.
         this_mylevs_diff=mylevs_diff
      END 
      7 : BEGIN
         expt_infile=ga4
         ctrl_infile=hg2a
         expt_name='ga4'
         ctrl_name='hadgem2a'
         expt_desc='MetUM GA4'
         ctrl_desc='HadGEM2-A'
         expt_offset=150
         ctrl_offset=150
         expt_nyears=27
         expt_count=120
         ctrl_nyears=20
         ctrl_count=120         
         ctrl_revlat=0
         expt_mult=86400.
         ctrl_mult=86400.
         this_mylevs_diff=mylevs_diff
      END 
      8 : BEGIN
         expt_infile=ga6
         ctrl_infile=hg2a
         expt_name='ga6'
         ctrl_name='hadgem2a'
         expt_desc='MetUM GA6'
         ctrl_desc='HadGEM2-A'
         expt_offset=150
         ctrl_offset=150
         expt_nyears=27
         expt_count=120
         ctrl_nyears=20
         ctrl_count=120         
         ctrl_revlat=0
         expt_mult=86400.
         ctrl_mult=86400.
         this_mylevs_diff=mylevs_diff
      END 
      9 : BEGIN
         expt_infile=ga6
         ctrl_infile=ga2
         expt_name='ga6'
         ctrl_name='morph3'
         expt_desc='MetUM GA6'
         ctrl_desc='MetUM MORPH3'
         expt_offset=150
         ctrl_offset=150
         expt_nyears=27
         expt_count=120
         ctrl_nyears=20
         ctrl_count=120         
         ctrl_revlat=0
         expt_mult=86400.
         ctrl_mult=86400.
         this_mylevs_diff=mylevs_diff
      END 
      10 : BEGIN
         expt_infile=ga6
         ctrl_infile=ga3
         expt_name='ga6'
         ctrl_name='ga3'
         expt_desc='MetUM GA6'
         ctrl_desc='MetUM GA3'
         expt_offset=150
         ctrl_offset=150
         expt_nyears=27
         expt_count=120
         ctrl_nyears=20
         ctrl_count=120         
         ctrl_revlat=0
         expt_mult=86400.
         ctrl_mult=86400.
         this_mylevs_diff=mylevs_diff
      END 
      11 : BEGIN
         expt_infile=ga6
         ctrl_infile=ga4
         expt_name='ga6'
         ctrl_name='ga4'
         expt_desc='MetUM GA6'
         ctrl_desc='MetUM GA4'
         expt_offset=150
         ctrl_offset=150
         expt_nyears=27
         expt_count=120
         ctrl_nyears=20
         ctrl_count=120         
         ctrl_revlat=0
         expt_mult=86400.
         ctrl_mult=86400.
         this_mylevs_diff=mylevs_diff
      END 
      12 : BEGIN
         expt_infile=ga3
         ctrl_infile=ga2
         expt_name='ga3'
         ctrl_name='morph3'
         expt_desc='MetUM GA3'
         ctrl_desc='MetUM MORPH3'
         expt_offset=150
         ctrl_offset=150
         expt_nyears=27
         expt_count=120
         ctrl_nyears=20
         ctrl_count=120         
         ctrl_revlat=0
         expt_mult=86400.
         ctrl_mult=86400.
         this_mylevs_diff=mylevs_diff
      END 
      13 : BEGIN
         expt_infile=ga4
         ctrl_infile=ga3
         expt_name='ga4'
         ctrl_name='ga3'
         expt_desc='MetUM GA4'
         ctrl_desc='MetUM GA3'
         expt_offset=150
         ctrl_offset=150
         expt_nyears=27
         expt_count=120
         ctrl_nyears=20
         ctrl_count=120         
         ctrl_revlat=0
         expt_mult=86400.
         ctrl_mult=86400.
         this_mylevs_diff=mylevs_diff
      END 
      14 : BEGIN
         expt_infile=gc2_n96
         ctrl_infile=trmm_infile
         expt_name='gc2'
         ctrl_name='trmm'
         expt_desc='MetUM GC2'
         ctrl_desc='TRMM'
         expt_offset=150
         ctrl_offset=150
         expt_nyears=41
         expt_count=120
         ctrl_nyears=13
         ctrl_count=120         
         ctrl_revlat=1
         expt_mult=86400.
         ctrl_mult=1.
         this_mylevs_diff=mylevs_bias
      END 
      15 : BEGIN
         expt_infile=gc2_n96
         ctrl_infile=ga6
         expt_name='gc2'
         ctrl_name='ga6'
         expt_desc='MetUM GC2'
         ctrl_desc='MetUM GA6'
         expt_offset=150
         ctrl_offset=150
         expt_nyears=41
         expt_count=120
         ctrl_nyears=20
         ctrl_count=120         
         ctrl_revlat=0
         expt_mult=86400.
         ctrl_mult=86400.
         this_mylevs_diff=mylevs_diff
      END 
   ENDCASE

                                ; Precip grid information
   expt_longitude=OPEN_AND_EXTRACT(expt_infile,'longitude')
   expt_latitude=OPEN_AND_EXTRACT(expt_infile,'latitude')
   DEFINE_BOUNDARIES,box,expt_latitude,expt_longitude,expt_box_tx,/LIMIT
   expt_nlon=N_ELEMENTS(expt_longitude)
   expt_nlat=N_ELEMENTS(expt_latitude)
   
   ctrl_longitude=OPEN_AND_EXTRACT(ctrl_infile,'longitude')
   ctrl_latitude=OPEN_AND_EXTRACT(ctrl_infile,'latitude')
   DEFINE_BOUNDARIES,box,ctrl_latitude,ctrl_longitude,ctrl_box_tx,/LIMIT
   ctrl_nlon=N_ELEMENTS(ctrl_longitude)
   ctrl_nlat=N_ELEMENTS(ctrl_latitude)   

   expt_precip_mean=fltarr(expt_nlon,expt_nlat)
   ctrl_precip_mean=fltarr(ctrl_nlon,ctrl_nlat)
   
   this_precip=REFORM(OPEN_AND_EXTRACT(expt_infile,'precip',$
                                       offset=[expt_box_tx(1),expt_box_tx(0),expt_offset,0],$
                                       count=[expt_nlon,expt_nlat,expt_count,expt_nyears]))*expt_mult
   FOR j=0,expt_nlon-1 DO $
      FOR k=0,expt_nlat-1 DO $
         expt_precip_mean(j,k)=MEAN(this_precip(j,k,*,*))
    
    this_precip=REFORM(OPEN_AND_EXTRACT(ctrl_infile,'precip',$
                                        offset=[ctrl_box_tx(1),ctrl_box_tx(0),ctrl_offset,0],$
                                        count=[ctrl_nlon,ctrl_nlat,ctrl_count,ctrl_nyears]))*ctrl_mult
    IF TOTAL(where(this_precip ge 1000)) gt 0 THEN $
       this_precip[where(this_precip ge 1000)]=!Values.F_NaN
    FOR j=0,ctrl_nlon-1 DO $
       FOR k=0,ctrl_nlat-1 DO $
          ctrl_precip_mean(j,k)=MEAN(this_precip(j,k,*,*),/NaN)    
    IF ctrl_revlat eq 1 THEN BEGIN
       temp=fltarr(ctrl_nlon,ctrl_nlat)
       FOR j=0,ctrl_nlat-1 DO $
          temp(*,j)=ctrl_precip_mean(*,ctrl_nlat-j-1,*,*)
       ctrl_precip_mean=temp
    ENDIF
    diff_expt_ctrl=expt_precip_mean-ctrl_precip_mean

    psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_gagc.'+expt_name+'.clim_precip.ps'
    PSOPEN,file=psfile,FONT=6,CHARSIZE=160,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=2500,TFONT=6,TCHARSIZE=100,YSIZE=9000,SPACE3=500
    CS,SCALE=25,NCOLS=N_ELEMENTS(mylevs)+1,white=[2]
    MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
    LEVS,MANUAL=mylevs
    CON,FIELD=expt_precip_mean,X=expt_longitude,Y=expt_latitude,$
        TITLE="JJAS clim rainfall from "+expt_desc,CB_TITLE='Precipitation (mm day!U-1!N)',$
        /NOLINES
    PSCLOSE,/NOVIEW
    
    psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_gagc.'+ctrl_name+'.clim_precip.ps'
    PSOPEN,file=psfile,FONT=6,CHARSIZE=160,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=2500,TFONT=6,TCHARSIZE=100,YSIZE=9000,SPACE3=500
    CS,SCALE=25,NCOLS=N_ELEMENTS(mylevs)+1,white=[2]
    MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
    LEVS,MANUAL=mylevs
    CON,FIELD=ctrl_precip_mean,X=expt_longitude,Y=expt_latitude,$
        TITLE="JJAS clim rainfall from "+ctrl_desc,CB_TITLE='Precipitation (mm day!U-1!N)',$
        /NOLINES
    PSCLOSE,/NOVIEW
    
    psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_gagc.'+expt_name+'-minus-'+ctrl_name+'.clim_precip.ps'
    PSOPEN,file=psfile,FONT=6,CHARSIZE=160,MARGIN=1500,SPACE2=1500,XOFFSET=500,YOFFSET=2500,TFONT=6,TCHARSIZE=100,YSIZE=9000,SPACE3=500
    CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV,white=[9]
    MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
    LEVS,MANUAL=this_mylevs_diff
    CON,FIELD=diff_expt_ctrl,X=expt_longitude,Y=expt_latitude,$
        TITLE="Diff in JJAS clim rainfall for "+expt_desc+" minus "+ctrl_desc,$
        CB_TITLE='Difference in precipitation (mm day!U-1!N)',/NOLINES,CB_WIDTH=110
    PSCLOSE,/NOVIEW
 ENDFOR

STOP
END
