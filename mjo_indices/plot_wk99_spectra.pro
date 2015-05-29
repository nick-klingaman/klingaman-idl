PRO plot_wk99_spectra,sym_file,asym_file,runid,varname

sym_wave=OPEN_AND_EXTRACT(sym_file,'wavenumber')
sym_freq=OPEN_AND_EXTRACT(sym_file,'frequency')
sym_power=OPEN_AND_EXTRACT(sym_file,'power')

asym_power=OPEN_AND_EXTRACT(asym_file,'power')

CASE varname OF 
   'olr' : BEGIN
      raw_levs=['-0.18','-0.15','-0.12','-0.09','-0.06','-0.03',$
                '0.00','0.03','0.06','0.09','0.12','0.15','0.18']
   END
ENDCASE

IF TOTAL(where(sym_power le -1000)) ge 0 THEN $
   sym_power[where(sym_power le -1000)]=!Values.F_NaN

psfile='/home/ss901165/idl/mjo_indices/wk99_spectra.'+runid+'_'+varname+'.'+'symmetric.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=160,TFONT=6,TCHARSIZE=100,MARGIN=3000,SPACE2=1000
GSET,XMIN=-15,XMAX=15,YMIN=0,YMAX=0.5
CS,SCALE=1,NCOLS=N_ELEMENTS(raw_levs)+1
LEVS,MANUAL=raw_levs
CON,X=sym_wave,Y=sym_freq,FIELD=sym_power,/NOLINES,CB_TITLE='LOG10(power)',$
    TITLE='Symmetric/background power in '+varname+' for '+runid
GPLOT,X=[-15,15],Y=[1/70.,1/70.],STYLE=1
GPLOT,X=12,Y=1/55.,TEXT='70 days'
GPLOT,X=[-15,15],Y=[1/20.,1/20.],STYLE=1
GPLOT,X=12,Y=1/18.,TEXT='20 days'
GPLOT,X=[-15,15],Y=[1/10.,1/10.],STYLE=1
GPLOT,X=12,Y=1/9.,TEXT='10 days'
GPLOT,X=[-15,15],Y=[1/5.,1/5.],STYLE=1
GPLOT,X=12,Y=1/4.8,TEXT='5 days'
GPLOT,X=[-15,15],Y=[1/3.,1/3.],STYLE=1
GPLOT,X=12,Y=0.34,TEXT='3 days'
GPLOT,X=[0,0],Y=[0,0.5],STYLE=1
AXES,XSTEP=5,XMINOR=1,YSTEP=0.05,YMINOR=0.025,XTITLE='Zonal wavenumber',$
     YTITLE='Frequency (day!U-1!N)',NDECS=2
PSCLOSE

psfile='/home/ss901165/idl/mjo_indices/wk99_spectra.'+runid+'_'+varname+'.'+'antisymmetric.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=160,TFONT=6,TCHARSIZE=100,MARGIN=3000,SPACE2=1000
GSET,XMIN=-15,XMAX=15,YMIN=0,YMAX=0.5
CS,SCALE=1,NCOLS=N_ELEMENTS(raw_levs)+1
LEVS,MANUAL=raw_levs
CON,X=sym_wave,Y=sym_freq,FIELD=asym_power,/NOLINES,CB_TITLE='LOG10(power)',$
    TITLE='Anti-symmetric/background power in '+varname+' for '+runid
GPLOT,X=[-15,15],Y=[1/70.,1/70.],STYLE=1
GPLOT,X=12,Y=1/55.,TEXT='70 days'
GPLOT,X=[-15,15],Y=[1/20.,1/20.],STYLE=1
GPLOT,X=12,Y=1/18.,TEXT='20 days'
GPLOT,X=[-15,15],Y=[1/10.,1/10.],STYLE=1
GPLOT,X=12,Y=1/9.,TEXT='10 days'
GPLOT,X=[-15,15],Y=[1/5.,1/5.],STYLE=1
GPLOT,X=12,Y=1/4.8,TEXT='5 days'
GPLOT,X=[-15,15],Y=[1/3.,1/3.],STYLE=1
GPLOT,X=12,Y=0.34,TEXT='3 days'
GPLOT,X=[0,0],Y=[0,0.5],STYLE=1
AXES,XSTEP=5,XMINOR=1,YSTEP=0.05,YMINOR=0.025,XTITLE='Zonal wavenumber',$
     YTITLE='Frequency (day!U-1!N)',NDECS=2
PSCLOSE

STOP
END
