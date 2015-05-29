PRO new_enso_boxa_obs
; Directory containing TRACK output files (text format)
track_indir='/home/ss901165/datasets/IBTRACS/SP_4950_0809'
; Seasons of the HiGEM integration to analyse.  Tracking is performed from
; October of the first year given to May of the second year.
years=['4950','5051','5152','5253','5354','5455','5556','5758','5859',$
       '5960','6061','6162','6263','6364','6465','6566','6768','6869',$
       '6970','7071','7172','7273','7374','7475','7576','7778','7879',$
       '7980','8081','8182','8283','8384','8485','8586','8788','8889',$
       '8990','9091','9192','9293','9394','9495','9596','9798','9899',$
       '9900','0001','0102','0203','0304','0405','0506','0708','0809']

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
n_years=N_ELEMENTS(years)
months=['Oct','Nov','Dec','Jan','Feb','Mar','Apr','May']
n_months=N_ELEMENTS(months)
; Specify number of month (in year, *not* in season) corresponding to "months" above
month_numbers=['10','11','12','01','02','03','04','05']
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;map box
;box=[-30,20,0,250]
;box a
;box=[-25,30,-5,130]
;box b
box=[-20,110,0,160]
;box c
;box=[-15,140,-5,240]
;box d
;box=[-25,140,-15,240]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;box a
print, ''
print, 'Genesis Months'

genesis_months=fltarr(n_years,n_months)
FOR i=0,n_years-1 DO BEGIN
   track_infile=track_indir+'/tr_trs.IBTRAC_SP_'+years(i)+'.time'
   openr,lun,track_infile,/GET_LUN
   mystr=''
   FOR j=0,2 DO $
      readf,lun,mystr
   readf,lun,mystr
   temp=STRSPLIT(mystr,' ',/EXTRACT)
   n_storms=temp(1)
   FOR j=0,n_storms-1 DO BEGIN
      IF j eq 0 THEN BEGIN
         readf,lun,mystr
         temp=STRSPLIT(mystr,' ',/EXTRACT)
         genesis_month=STRMID(temp(3),4,2)
      ENDIF   
   
      readf,lun,mystr
      temp=STRSPLIT(mystr,' ',/EXTRACT)
      n_points=temp(1)
      readf,lun,date,templon,templat
      print,templon,templat
      IF templon gt box(1) and templon lt box(3) and $
         templat gt box(0) and templat lt box(2) THEN BEGIN
         readf,lun,mystr
         FOR k=0,n_months-1 DO BEGIN
            IF genesis_month eq month_numbers(k) THEN BEGIN
               print,genesis_month
               genesis_months(i,k)=genesis_months(i,k)+1              
            ENDIF
         ENDFOR
      ENDIF
                          
      FOR k=1,n_points DO BEGIN
         IF NOT EOF(lun) THEN BEGIN
            readf,lun,mystr
            temp=STRSPLIT(mystr,' ',/EXTRACT)
            IF temp(0) eq 'TRACK_ID' THEN BEGIN
               genesis_month=STRMID(temp(3),4,2)
;               print,genesis_month
               BREAK
            ENDIF
         ENDIF
      ENDFOR
   ENDFOR
   FREE_LUN,lun 
ENDFOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
average_genesis_months=fltarr(n_months)
FOR i=0,n_months-1 DO BEGIN $
   average_genesis_months(i)=MEAN(genesis_months(*,i))
ENDFOR

print, ''
print, 'Average Genesis Months'
print,average_genesis_months
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;September-November seasonal mean Nino 4 SST anomolies
input_file2='/home/ss901165/datasets/NINO/nino4_hadisst.sep-nov_smeans.1949-2008.nc'

nino4_ssts=OPEN_AND_EXTRACT(input_file2,'NINO4')

av_nino4_ssts=fltarr(n_years)
FOR i=0,n_years-1 DO BEGIN $
av_nino4_ssts=MEAN(nino4_ssts)
ENDFOR

print, ''
print, 'NINO4 SSTS'
print, nino4_ssts
print, ''
print, 'Average NINO4 SST'
print, av_nino4_ssts
print, ''

ENSO_value=(av_nino4_ssts-nino4_ssts)

lanina_years=WHERE(ENSO_value lt -0.5)
elnino_years=WHERE(ENSO_value gt 0.5)
remainder_years=WHERE((ENSO_value gt -0.5) AND (ENSO_value lt 0.5))
lanina_gden=fltarr(n_months)
elnino_gden=fltarr(n_months)
remainder=fltarr(n_months)
FOR i=0,n_months-1 DO BEGIN
   temp=REFORM(genesis_months(*,i))
   lanina_gden(i)=MEAN(temp[lanina_years])
   elnino_gden(i)=MEAN(temp[elnino_years])
   remainder(i)=MEAN(temp[remainder_years])
ENDFOR

print, 'Storms formed in La Nina Years'
print, lanina_gden
print, ''
print, 'Storms formed in El Nino Years'
print, elnino_gden
print, ''
print, 'Storms formed in normal years'
print, remainder
print, ''
;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

psfile='/home/ss901165/idl/queensland/enso_boxa.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1500,XOFFSET=1000,YOFFSET=1500,$
       TFONT=2,TCHARSIZE=100,SPACE3=500,YSIZE=10000
CS,SCALE=2
GSET,XMIN=0,XMAX=8,YMIN=0,YMAX=7
AXES,YSTEP=1,NDECS=1,XTITLE='Monthly mean tropical cyclone genesis [model output] Box A',YTITLE='Storms',/HGRID,XVALS=INDGEN(8)+0.5,XLABELS=['Oct','Nov','Dec','Jan','Feb','Mar','Apr','May']
HIST,X=INDGEN(8)+0.3,Y=lanina_gden,WIDTH=100,FILLCOL=8
HIST,X=INDGEN(8)+0.5,Y=remainder,WIDTH=100,FILLCOL=10
HIST,X=INDGEN(8)+0.7,Y=elnino_gden,WIDTH=100,FILLCOL=9
;HIST1  
;HIST2 
;HIST3                     HIST3       HIST2         HIST1     HIST[3,2,1]
GLEGEND,LEGPOS=9,LABELS=['El Nino','Non-ENSO Years','La Nina'],COL=[9,10,8],TYPE=1
PSCLOSE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

STOP
END
