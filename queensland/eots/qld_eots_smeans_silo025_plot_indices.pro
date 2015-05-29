PRO qld_eots_smeans_silo025_plot_indices
  
; Plot seasonal- and annual-mean indices used in EOT regression analysis.

n_indices=8
FOR i=0,n_indices-1 DO BEGIN
   CASE i OF
      4 : BEGIN
         infile_basename='/home/ss901165/datasets/NINO/nino3_hadisst.'
         year_ranges=['1871-2007','1871-2007','1871-2007','1871-2007','1871-2008']
         seasons=['dec-feb_smean','mar-may_smean','jun-aug_smean','sep-nov_smean','jan-dec_amean']
         season_names=['DJF','MAM','JJA','SON','Annual']
         time_start=29
         year_start=1900
         n_years=108
         varname='NINO3'
         psfilename=varname        
         vardesc='Nino 3 SST'
         varunits='!Uo!NC'
         xstep=10
         xminor=5
         ystep=0.5
         yminor=0.25
         legpos=1
      END
      1 : BEGIN
         infile_basename='/home/ss901165/datasets/NINO/nino34_hadisst.'
         year_ranges=['1871-2007','1871-2007','1871-2007','1871-2007','1871-2008']
         seasons=['dec-feb_smean','mar-may_smean','jun-aug_smean','sep-nov_smean','jan-dec_amean']
         season_names=['DJF','MAM','JJA','SON','Annual']
         time_start=29
         year_start=1900
         n_years=108
         varname='NINO34'
         psfilename=varname
         vardesc='Nino 3.4 SST'
         varunits='!Uo!NC'
         xstep=10
         xminor=5
         ystep=0.5
         yminor=0.25
         legpos=1
      END
      2 : BEGIN
         infile_basename='/home/ss901165/datasets/NINO/nino4_hadisst.'
         year_ranges=['1871-2007','1871-2007','1871-2007','1871-2007','1871-2008']
         seasons=['dec-feb_smean','mar-may_smean','jun-aug_smean','sep-nov_smean','jan-dec_amean']
         season_names=['DJF','MAM','JJA','SON','Annual']
         time_start=29
         year_start=1900
         n_years=108
         varname='NINO4'
         psfilename=varname        
         vardesc='Nino 4 SST'
         varunits='!Uo!NC'
         xstep=10
         xminor=5
         ystep=0.5
         yminor=0.25
         legpos=1
      END
      3 : BEGIN
         infile_basename='/home/ss901165/datasets/IOD/iod_index.'
         year_ranges=['1958-2007.hadisst','1958-2007.hadisst','1958-2007.hadisst','1958-2007.hadisst','1958-2007.hadisst']
         seasons=['dec-feb_smean','mar-may_smean','jun-aug_smean','sep-nov_smean','mar-feb_smean']
         season_names=['DJF','MAM','JJA','SON','Annual']
         time_start=0
         year_start=1958
         n_years=50
         varname='IOD'
         psfilename=varname        
         vardesc='Indian Ocean Dipole index'
         varunits='!Uo!NC'
         xstep=4
         xminor=2
         ystep=0.5
         yminor=0.25
         legpos=1
      END    
      5 : BEGIN
         infile_basename='/home/ss901165/datasets/HADISST/hadisst_ipo.'
         year_ranges=['1871-2008','1871-2008','1871-2008','1871-2008','1871-2008']
         seasons=['dec-feb_smean','mar-may_smean','jun-aug_smean','sep-nov_smean','jan-dec_amean']
         season_names=['DJF','MAM','JJA','SON','Annual']
         time_start=29
         year_start=1900
         n_years=108
         varname='ipo_index'
         psfilename=varname 
         vardesc='Interdecadal Pacific Osillation index '
         varunits='!Uo!NC'
         xstep=10
         xminor=5
         ystep=1
         yminor=0.5
         legpos=3
      END   
      6 : BEGIN
         infile_basename='/home/ss901165/datasets/SAM/SAM_index_BAS.'
         year_ranges=['1957-2009','1957-2009','1957-2009','1957-2009','1957-2009']
         seasons=['dec-feb_smean','mar-may_smean','jun-aug_smean','sep-nov_smean','jan-dec_smean']
         season_names=['DJF','MAM','JJA','SON','Annual']
         time_start=0
         year_start=1957
         n_years=51
         varname='SAM'
         psfilename=varname        
         vardesc='Southern Annular Mode index'
         varunits='hPa'
         xstep=4
         xminor=2
         ystep=1
         yminor=0.5
         legpos=11
      END
      7 : BEGIN
         infile_basename='/home/ss901165/datasets_mango/20THC_REANALYSIS/zonal_wind/20thc_reanalysis.'
         year_ranges=['1891-2007.bom_blocking_index.lonavg120-150E','1891-2007.bom_blocking_index.lonavg120-150E','1891-2007.bom_blocking_index.lonavg120-150E','1891-2007.bom_blocking_index.lonavg120-150E','1891-2008.bom_blocking_index.lonavg120-150E']
         seasons=['dec-feb_smean','mar-may_smean','jun-aug_smean','sep-nov_smean','jan-dec_amean']
         season_names=['DJF','MAM','JJA','SON','Annual']
         time_start=9
         year_start=1900
         n_years=107
         varname='blocking_index'
         psfilename='blocking_index_120-150'
         vardesc='BoM blocking index 120-150E'
         varunits='m s!U-1!N'
         xstep=10
         xminor=5
         ystep=3
         yminor=1.5
         legpos=1
      END
      0 : BEGIN
         infile_basename='/home/ss901165/datasets_mango/20THC_REANALYSIS/zonal_wind/20thc_reanalysis.'
         year_ranges=['1891-2007.bom_blocking_index.lonavg150-180E','1891-2007.bom_blocking_index.lonavg150-180E','1891-2007.bom_blocking_index.lonavg150-180E','1891-2007.bom_blocking_index.lonavg150-180E','1891-2008.bom_blocking_index.lonavg150-180E']
         seasons=['dec-feb_smean','mar-may_smean','jun-aug_smean','sep-nov_smean','jan-dec_amean']
         season_names=['DJF','MAM','JJA','SON','Annual']
         time_start=9
         year_start=1900
         n_years=107
         varname='blocking_index'
         psfilename='blocking_index_150-180'
         vardesc='BoM blocking index 150-180E'
         varunits='m s!U-1!N'
         xstep=10
         xminor=5
         ystep=3
         yminor=1.5
         legpos=1
      END
   ENDCASE

   n_seasons=N_ELEMENTS(seasons)
   seasonal_timeseries=fltarr(n_seasons,n_years)
   FOR j=0,n_seasons-1 DO BEGIN
      print,seasons(j)
      infile=infile_basename+seasons(j)+'s.'+year_ranges(j)+'.nc'
      print,infile
      timeseries=REFORM(OPEN_AND_EXTRACT(infile,varname,offset=[time_start],count=[n_years]))
      timeseries=timeseries-MEAN(timeseries)
      seasonal_timeseries(j,*)=timeseries

   ENDFOR
   psfile='/home/ss901165/idl/queensland/eots/indices/qld_eots_smeans_silo025_plot_indices.'+psfilename+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=700,XOFFSET=1000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,SPACE3=400
   colors=['blue','green','orange','brown','black']
   GSET,XMIN=year_start,XMAX=year_start+n_years,YMIN=FLOOR(MIN(seasonal_timeseries)*1.1),$
        YMAX=FLOOR(MAX(seasonal_timeseries)*1.1)+1
   FOR j=0,n_seasons-1 DO $
      GPLOT,X=indgen(n_years)+year_start,Y=REFORM(seasonal_timeseries(j,*)),COL=FSC_COLOR(colors(j))
   GLEGEND,labels=REVERSE(season_names),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=legpos
   AXES,XSTEP=xstep,XMINOR=xminor,YSTEP=ystep,YMINOR=yminor,XTITLE='Year',YTITLE=vardesc+' anomaly ('+varunits+')',NDECS=2
   GPLOT,X=[0,n_years]+year_start,Y=[0,0],STYLE=2,COL=FSC_COLOR('black')
   PSCLOSE,/NOVIEW
ENDFOR

STOP
END


         
         
