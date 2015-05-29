PRO qld_eots_smeans_silo025_correlate_lawdome

nseasons=4
lawdome_offset=40
nyears=108
neots=3

eot_indir='/home/ss901165/datasets/SILO/one_quarter'
lawdome_infile='/home/ss901165/datasets/LAW_DOME_ICE_CORE/lawdome_sss.ameans.1860-2009.nc'

lawdome_sss=OPEN_AND_EXTRACT(lawdome_infile,'ldsss',offset=[lawdome_offset],count=[nyears])
lawdome_sss_trend=REGRESS(indgen(nyears),lawdome_sss)
lawdome_sss_detrend=lawdome_sss-(indgen(nyears)*lawdome_sss_trend(0))

FOR i=0,nseasons-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         season='dec-feb'
      END
      1 : BEGIN
         season='mar-may'
      END
      2 : BEGIN
         season='jun-aug'
      END
      3 : BEGIN
         season='sep-nov'
      END
   ENDCASE

   infile=eot_indir+'/SILO.'+season+'_smeans.1900-2007.eots.nc'      
   eots_ts=OPEN_AND_EXTRACT(infile,'loading',offset=[0,0],count=[nyears,neots])
   
   print,season
   FOR j=0,neots-1 DO BEGIN
      instant_correlation=CORRELATE(REFORM(eots_ts(*,j)),lawdome_sss)
      lagminus_correlation=C_CORRELATE(REFORM(eots_ts(*,j)),lawdome_sss,-1)
      lagplus_correlation=C_CORRELATE(REFORM(eots_ts(*,j)),lawdome_sss,1)
      print,'EOT '+STRTRIM(STRING(j+1),1)+':  '+$
            STRMID(STRTRIM(STRING(instant_correlation),1),0,5)+'  '+$
            STRMID(STRTRIM(STRING(lagminus_correlation),1),0,5)+'  '+$
            STRMID(STRTRIM(STRING(lagplus_correlation),1),0,5)

      eots_trend=REGRESS(indgen(nyears),REFORM(eots_ts(*,j)))
      eots_ts_detrend=REFORM(eots_ts(*,j))-(indgen(nyears)*eots_trend(0))

      instant_correlation=CORRELATE(eots_ts_detrend,lawdome_sss_detrend)
      lagminus_correlation=C_CORRELATE(eots_ts_detrend,lawdome_sss_detrend,-1)
      lagplus_correlation=C_CORRELATE(eots_ts_detrend,lawdome_sss_detrend,1)
      print,'EOT '+STRTRIM(STRING(j+1),1)+':  '+$
            STRMID(STRTRIM(STRING(instant_correlation),1),0,5)+'  '+$
            STRMID(STRTRIM(STRING(lagminus_correlation),1),0,5)+'  '+$
            STRMID(STRTRIM(STRING(lagplus_correlation),1),0,5)

   ENDFOR
   
ENDFOR

STOP
END
