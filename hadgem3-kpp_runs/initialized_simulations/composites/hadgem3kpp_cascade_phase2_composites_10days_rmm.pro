PRO hadgem3kpp_cascade_phase2_composites_10days_rmm

; Make day-by-day composites of precip, U850, U200, OLR from
; the phase 2 integrations for the control and 1.5x entrainment.

um3='/home/ss901165/um_output3'

n_sets=4
control_runs=['xfrla',  'xfadh',  'xfdif',  'xfrle',  'xfrli',  'xfrlm',  'xfrlq',  'xfrlu',  'xfsea',  'xfsee',  'xfsei',  'xfsem',  'xfseq',  'xfseu']
control_dirs=['xfrla','xfadh.old_stash','xfdif','xfrle','xfrli','xfrlm',  'xfrlq',  'xfrlu',  'xfsea',  'xfsee',  'xfsei',  'xfsem',  'xfseq',  'xfseu']
entrain_runs=['xfrlb',  'xfadk',  'xfdii',  'xfrlf',  'xfrlj',  'xfrln',  'xfrlr',  'xfrlv',  'xfseb',  'xfsef',  'xfsej',  'xfsen',  'xfser',  'xfsev']
entrain_dirs=['xfrlb','xfadk.old_stash','xfdii','xfrlf','xfrlj','xfrln',  'xfrlr',  'xfrlv',  'xfseb',  'xfsef',  'xfsej',  'xfsen',  'xfser',  'xfsev']
nocmt_runs=  ['xfrlc',  'xfadl',  'xfdij',  'xfrlg',  'xfrlk',  'xfrlo',  'xfrls',  'xfrlw',  'xfsec',  'xfseg',  'xfsek',  'xfseo',  'xfses',  'xfsew']
nocmt_dirs=  ['xfrlc','xfadl','xfdij','xfrlg','xfrlk','xfrlo',  'xfrls',  'xfrlw',  'xfsec',  'xfseg',  'xfsek',  'xfseo',  'xfses',  'xfsew']
combine_runs=['xfrld',  'xfadm',  'xfdik',  'xfrlh',  'xfrll',  'xfrlp',  'xfrlt',  'xfrlx',  'xfsed',  'xfseh',  'xfsel',  'xfsep',  'xfset',  'xfsex']
combine_dirs=['xfrld','xfadm.old_stash','xfdik','xfrlh','xfrll','xfrlp',  'xfrlt',  'xfrlx',  'xfsed',  'xfseh',  'xfsel',  'xfsep',  'xfset',  'xfsex']
start_dates= ['04nov09','06apr09','11oct08','26aug08','07dec07','07sep06','18mar05','03dec03','29oct02','05jun02','26apr02','16jan01','13nov00','28sep00']
offset_years=[20,       20,       19,       19,       18,       17,       16,       14,       13,       13,       13,       12,       11,       11]+14
offset_dates=[308,      96,       284,      238,      341,      250,      77,       337,      302,      156,      116,      16,       317,      271]-1

n_days=30
n_cases=N_ELEMENTS(control_runs)

model_composite_rmm1=fltarr(n_sets,n_days)
model_composite_rmm2=fltarr(n_sets,n_days)
obs_composite_rmm1=fltarr(n_days)
obs_composite_rmm2=fltarr(n_days)
FOR j=0,n_sets-1 DO BEGIN
   CASE j OF
      0 : BEGIN
         set_name='CTL'
         runids=control_runs
         dirs=control_dirs
      END
      3 : BEGIN
         set_name='1.5*epsilon'
         runids=entrain_runs
         dirs=entrain_dirs
      END
      2 : BEGIN
         set_name='NoCMT'
         runids=nocmt_runs
         dirs=nocmt_dirs
      END
      1 : BEGIN
         set_name='1.5*epsilon+NoCMT'
         runids=combine_runs
         dirs=combine_dirs
      END
   ENDCASE
   FOR k=0,n_cases-1 DO BEGIN
      model_infile=um3+'/'+dirs(k)+'/'+runids(k)+'.'+start_dates(k)+'.rmm_indices.nc'
      
      model_rmm1=REFORM(OPEN_AND_EXTRACT(model_infile,'rmm1',offset=[0,0],count=[1,n_days]))
      model_rmm2=REFORM(OPEN_AND_EXTRACT(model_infile,'rmm2',offset=[0,0],count=[1,n_days]))

      FOR m=0,n_days-1 DO BEGIN
         model_composite_rmm1(j,m)=model_composite_rmm1(j,m)+model_rmm1(m)*1./FLOAT(n_cases)
         model_composite_rmm2(j,m)=model_composite_rmm2(j,m)+model_rmm2(m)*1./FLOAT(n_cases)
      ENDFOR
   ENDFOR
ENDFOR     
      
obs_rmm_infile='/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2009.index_values.nc'
FOR i=0,n_cases-1 DO BEGIN
   obs_rmm1=REFORM(OPEN_AND_EXTRACT(obs_rmm_infile,'rmm1_ts',$
                                    offset=[(offset_years(i))*365+offset_dates(i)+offset_years(i)/4+1],count=[n_days]))
   obs_rmm2=REFORM(OPEN_AND_EXTRACT(obs_rmm_infile,'rmm2_ts',$
                                    offset=[(offset_years(i))*365+offset_dates(i)+offset_years(i)/4+1],count=[n_days]))
   FOR j=0,n_days-1 DO BEGIN
      obs_composite_rmm1(j)=obs_composite_rmm1(j)+obs_rmm1(j)*1./FLOAT(n_cases)
      obs_composite_rmm2(j)=obs_composite_rmm2(j)+obs_rmm2(j)*1./FLOAT(n_cases)
   ENDFOR
ENDFOR

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/composites/hadgem3kpp_cascade_phase2_composites_10days_rmm.allexpts.color.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=170,MARGIN=1500,SPACE3=1500,XOFFSET=1500,YOFFSET=1000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,XSIZE=14000,YSIZE=14000
GSET,XMIN=-2.0,XMAX=2.5,YMIN=-2.5,YMAX=2.00
GPLOT,X=REPLICATE(0,2),Y=[-2.5,-1],STYLE=0,THICK=80
GPLOT,X=REPLICATE(0,2),Y=[1,2.0],STYLE=0,THICK=80
GPLOT,X=[1,2.5],Y=REPLICATE(0,2),STYLE=0,THICK=80
GPLOT,X=[-1,-2.0],Y=REPLICATE(0,2),STYLE=0,THICK=80
GPLOT,X=[SQRT(2)/2.,1,2.0],Y=[SQRT(2)/2.,1,2.0],STYLE=0,THICK=80
GPLOT,X=[-SQRT(2)/2.,-1,-2.0],Y=[SQRT(2)/2.,1,2.0],STYLE=0,THICK=80
GPLOT,X=[-SQRT(2)/2.,-1,-2.0],Y=[-SQRT(2)/2.,-1,-2.0],STYLE=0,THICK=80
GPLOT,X=[SQRT(2)/2.,1,2.5],Y=[-SQRT(2)/2.,-1,-2.5],STYLE=0,THICK=80

GPLOT,X=0,Y=-3.25,TEXT='Indian Ocean',ALIGN=0.5,CHARSIZE=100
GPLOT,X=2.85,Y=0,TEXT='Maritime Continent',ALIGN=0.5,CHARSIZE=100,ORIENTATION=90
GPLOT,X=0,Y=2.25,TEXT='Western Pacific',ALIGN=0.5,CHARSIZE=100
GPLOT,X=-2.75,Y=0,TEXT='Western Hemisphere and Africa',ALIGN=0.5,CHARSIZE=100,ORIENTATION=90
GPLOT,X=0,Y=-3.00,TEXT='RMM1'
GPLOT,X=-2.50,Y=0,TEXT='RMM2',ORIENTATION=90
points=(2*!PI/99.0)*findgen(100)
x=COS(points)
y=SIN(points)
white=FSC_COLOR("white",28)
GPLOT,X=x,Y=y,FILLCOL=28

GPLOT,X=-1.50,Y=-2.25,TEXT='Phase 2'
GPLOT,X=1.50,Y=-2.25,TEXT='Phase 3'
GPLOT,X=1.75,Y=-1.00,TEXT='Phase 4'
GPLOT,X=1.75,Y=1.00,TEXT='Phase 5'
GPLOT,X=0.75,Y=1.50,TEXT='Phase 6'
GPLOT,X=-0.75,Y=1.50,TEXT='Phase 7'
GPLOT,X=-1.50,Y=0.50,TEXT='Phase 8'
GPLOT,X=-1.50,Y=-0.50,TEXT='Phase 1'

AXES,XSTEP=1.0,YSTEP=1.0,XMINOR=0.25,YMINOR=0.25,NDECS=2
GPLOT,X=0,Y=0,TEXT='Weak MJO',ALIGN=0.5

colors=['red','blue','brown','purple']
styles=[0,0,2,2]
syms=[6,7,6,7]
FOR i=0,n_sets-1 DO BEGIN
   GPLOT,X=REFORM(model_composite_rmm1(i,*)),Y=REFORM(model_composite_rmm2(i,*)),STYLE=0,THICK=150,COL=FSC_COLOR(colors(i));STYLE=styles(i)
   FOR j=0,n_days-1,5 DO $
      GPLOT,X=model_composite_rmm1(i,j),Y=REFORM(model_composite_rmm2(i,j)),SYM=syms(i),/NOLINES,SIZE=100,THICK=150,COL=FSC_COLOR(colors(i))
   GPLOT,X=model_composite_rmm1(i,n_days-1),Y=model_composite_rmm2(i,n_days-1),SYM=syms(i),SIZE=100,COL=FSC_COLOR(colors(i))
ENDFOR
black=FSC_COLOR('black',30+n_sets)
GPLOT,X=obs_composite_rmm1(*),Y=obs_composite_rmm2(*),STYLE=0,THICK=150
FOR j=0,n_days-1,5 DO $
   GPLOT,X=obs_composite_rmm1(j),Y=REFORM(obs_composite_rmm2(j)),STYLE=0,SYM=3,/NOLINES,SIZE=100
GPLOT,X=obs_composite_rmm1(n_days-1),Y=obs_composite_rmm2(n_days-1),SYM=3,SIZE=100

labels=['Observations','CTL','1.5*F','NoCMT','1.5*F+NoCMT']
GLEGEND,labels=REVERSE(labels),SYM=REVERSE([3,syms]),LEGXOFFSET=2500,LEGYOFFSET=19000,COL=REVERSE([FSC_COLOR('black'),FSC_COLOR(colors)]);,STYLE=REVERSE([0,styles])
;GLEGEND,labels=REVERSE(['Observations','Control','1.5x entrainment']),SYM=REVERSE([2,3,4]),COL=REVERSE([FSC_COLOR('black'),FSC_COLOR('red'),FSC_COLOR('blue')]),$
;        STYLE=REVERSE([0,0,0]),LEGXOFFSET=1000,LEGYOFFSET=18500

PSCLOSE

STOP
END
