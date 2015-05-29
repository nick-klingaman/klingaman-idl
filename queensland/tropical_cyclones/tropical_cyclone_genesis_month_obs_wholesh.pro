PRO tropical_cyclone_genesis_month_obs_wholesh
  
; Determine the seasonal cycle of tropical-cyclone genesis in a particular box
; using the output from TRACK.

; Directory containing TRACK output files (text format)
track_indir='/home/ss901165/datasets/IBTRACS/SH_4950_0809'

; Seasons of the HiGEM integration to analyse.  Tracking is performed from
; October of the first year given to May of the second year.
years=['4950','5051','5152','5253','5354','5455','5556','5758','5859',$
       '5960','6061','6162','6263','6364','6465','6566','6768','6869',$
       '6970','7071','7172','7273','7374','7475','7576','7778','7879',$
       '7980','8081','8182','8283','8384','8485','8586','8788','8889',$
       '8990','9091','9192','9293','9394','9495','9596','9798','9899',$
       '9900','0001','0102','0203','0304','0405','0506','0708','0809']

n_years=N_ELEMENTS(years)

; Specify possible months for genesis
months=['Oct','Nov','Dec','Jan','Feb','Mar','Apr','May']
n_months=N_ELEMENTS(months)
; Specify number of month (in year, *not* in season) corresponding to "months" above
month_numbers=['10','11','12','01','02','03','04','05']

; Box is a vector of boundaries and must be in the order [south,west,north,east]
box=[-20,60,0,100]

; Vector to hold the number of cyclones generated in the box in each month and year.
genesis_months=fltarr(n_years,n_months)
FOR i=0,n_years-1 DO BEGIN
                                ; This year's TRACK input file
   track_infile=track_indir+'/tr_trs.IBTRAC_SH_'+years(i)+'.time'
                                ; Open file for reading
   openr,lun,track_infile,/GET_LUN
                                ; Initialize temporary string
   mystr=''
                                ; Read past header information
   FOR j=0,2 DO $
      readf,lun,mystr
                                ; Read number of storms for this year
   readf,lun,mystr
   temp=STRSPLIT(mystr,' ',/EXTRACT)
   n_storms=1000
                                ; Read tropical-cyclone information
   FOR j=0,n_storms-1 DO BEGIN
      IF NOT EOF(lun) THEN BEGIN
         IF j eq 0 THEN BEGIN
                                ; Read genesis date
            readf,lun,mystr
            temp=STRSPLIT(mystr,' ',/EXTRACT)
            genesis_month=STRMID(temp(3),4,2)
         ENDIF      
                                ; Read genesis point
         readf,lun,mystr
         temp=STRSPLIT(mystr,' ',/EXTRACT)
         n_points=temp(1)
         readf,lun,date,templon,templat
                                ; Find if genesis point is within specified box=[] above
         IF templon gt box(1) and templon lt box(3) and $
            templat gt box(0) and templat lt box(2) THEN BEGIN
            readf,lun,mystr
            FOR k=0,n_months-1 DO $
               IF genesis_month eq month_numbers(k) THEN $
                  genesis_months(i,k)=genesis_months(i,k)+1
         ENDIF
                                ; Jump to next storm
         FOR k=1,n_points+10 DO BEGIN
            IF NOT EOF(lun) THEN BEGIN
               readf,lun,mystr
               temp=STRSPLIT(mystr,' ',/EXTRACT)
               IF temp(0) eq 'TRACK_ID' THEN BEGIN
                  genesis_month=STRMID(temp(3),4,2)
                  print,genesis_month
                  BREAK
               ENDIF
            ENDIF
         ENDFOR
      ENDIF ELSE $
         n_storms=1000      
   ENDFOR
                                ; Close input file
   FREE_LUN,lun
ENDFOR

STOP
END
