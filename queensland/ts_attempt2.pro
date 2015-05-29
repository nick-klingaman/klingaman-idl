PRO  ts_attempt2
; Directory containing TRACK output files (text format)
track_indir='/home/ss901165/higem_qccce/es_control_eafeb/tropical_cyclones'
; Seasons of the HiGEM integration to analyse.  Tracking is performed from
; October of the first year given to May of the second year (e.g., the first
; season is October of year i0 to May of year i1).
years=['i0i1','i1i2','i2i3','i3i4','i4i5','i5i6','i6i7','i7i8','i8i9','i9j0',$
       'j0j1','j1j2','j2j3','j3j4','j4j5','j5j6','j6j7','j7j8','j8j9','j9k0',$
       'k0k1','k1k2','k2k3','k3k4','k4k5','k5k6','k6k7','k7k8','k8k9','k9l0',$
       'l0l1','l1l2','l2l3','l3l4','l4l5','l5l6','l6l7','l7l8','l8l9','l9m0',$
       'm0m1','m1m2','m2m3','m3m4','m4m5','m5m6','m6m7','m7m8','m8m9','m9n0',$
       'n0n1','n1n2','n2n3','n3n4','n4n5','n5n6','n6n7','n7n8','n8n9','n9o0',$
       'o0o1','o1o2','o2o3','o3o4','o4o5','o5o6','o6o7','o7o8','o8o9','o9p0',$
       'p0p1','p1p2','p2p3','p3p4','p4p5','p5p6','p6p7','p7p8','p8p9','p9q0',$
       'q0q1','q1q2','q2q3','q3q4','q4q5','q5q6','q6q7','q7q8','q8q9','q9r0',$
       'r0r1','r1r2','r2r3','r3r4','r4r5','r5r6','r6r7','r7r8','r8r9','r9s0',$
       's0s1','s1s2','s2s3','s3s4','s4s5','s5s6','s6s7','s7s8','s8s9','s9t0',$
       't0t1','t1t2','t2t3','t3t4','t4t5','t5t6','t6t7','t7t8','t8t9','t9u0',$
       'u0u1','u1u2','u2u3','u3u4','u4u5','u5u6','u6u7','u7u8','u8u9','u9v0',$
       'v0v1','v1v2','v2v3','v3v4','v4v5','v5v6','v6v7','v7v8','v8v9','v9w0',$
       'w0w1','w1w2','w2w3','w3w4','w4w5','w5w6','w6w7','w7w8','w8w9']

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
n_years=N_ELEMENTS(years)
months=['Oct','Nov','Dec','Jan','Feb','Mar','Apr','May']
n_months=N_ELEMENTS(months)
month_numbers=['10','11','12','01','02','03','04','05']
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; year by year tropical cyclone stats from HiGEM
;input_file1='/home/ss901165/higem_qccce/es_control_eafeb/tropical_cyclones/stat_trs_scl.oct-may_smeans.i0-w8.nc'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; map box
;box=[-45,20,20,250]
; box A
;box=[-25,30,-5,130]
; box B
;box=[-20,110,0,160]
; box C
box=[-15,140,-5,240]
; box D
;box=[-25,140,-15,240]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;longitude=OPEN_AND_EXTRACT(input_file1,'long')
;latitude=OPEN_AND_EXTRACT(input_file1,'lat')
;year=OPEN_AND_EXTRACT(input_file1,'year')
;DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
;n_lon=N_ELEMENTS(longitude)
;n_lat=N_ELEMENTS(latitude)
n_year=N_ELEMENTS(years)

; read climatological data
; the 3rd zero in the offset command represents the year which is 1
; dimensional - hence we start counting at 0
;track_density=OPEN_AND_EXTRACT(input_file1,'gden',$
;                               offset=[box_tx(1),box_tx(0),(0)],$
;                               count=[n_lon,n_lat,n_year])
;track_density=track_density*8

; Vector to hold the number of cyclones generated in the box in each month and year.
genesis_months=fltarr(n_years,n_months)
FOR i=0,n_years-1 DO BEGIN
                                ; This year's TRACK input file
   track_infile=track_indir+'/tr_trs.HiGEM_eadwh_'+years(i)+'.new'
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
   n_storms=temp(1)
                                ; Read tropical-cyclone information
  
 FOR j=0,n_storms-1 DO BEGIN
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
      FOR k=1,n_points DO BEGIN
         IF NOT EOF(lun) THEN BEGIN
            readf,lun,mystr
            temp=STRSPLIT(mystr,' ',/EXTRACT)
            IF temp(0) eq 'TRACK_ID' THEN BEGIN
               genesis_month=STRMID(temp(3),4,2)
               BREAK
            ENDIF
         ENDIF
      ENDFOR
   ENDFOR
                                ; Close input file
   FREE_LUN,lun

ENDFOR

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; will eventually use this to plot nino4 time series
;;;September-November seasonal mean Nino 4 SST anomolies
;input_file2='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.sep-nov_smeans.i0-w8.nino_indices.nc'
;nino4_ssts=OPEN_AND_EXTRACT(input_file2,'nino4')
;lanina_gden=fltarr(n_year)
;elnino_gden=fltarr(n_year)
;remainder=fltarr(n_year)
;all_gden=fltarr(n_year)
;lanina_years=WHERE(nino4_ssts lt -0.5)
;elnino_years=WHERE(nino4_ssts gt 0.5)
;remainder_years=WHERE((nino4_ssts gt -0.5) AND (nino4_ssts lt 0.5))
;all_years=(elnino_years+lanina_years+remainder_years)
;FOR i=0,n_months-1 DO BEGIN
;   temp=REFORM(genesis_months(*,i))
;   lanina_gden(i)=MEAN(temp[lanina_years])
;   elnino_gden(i)=MEAN(temp[elnino_years])
;   remainder(i)=MEAN(temp[remainder_years])
;   all(i)=MEAN(temp[all_years])
;ENDFOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

yearly_densities=fltarr(n_year)
FOR i=0,n_year-1 DO BEGIN $
   yearly_densities(i)=TOTAL(genesis_months(i,*))
ENDFOR

year_count=fltarr(n_year)
FOR i=0,n_year-1 DO BEGIN $
    year_count(i)=i
ENDFOR

print, year_count
print, yearly_densities

running_stddev=fltarr(n_year)
FOR i=5,n_year-6 DO $
   running_stddev(i)=STDDEV(yearly_densities(i-5:i+5))

yearly_densities_smoothed=SMOOTH(yearly_densities,11)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
psfile='/home/ss901165/idl/queensland/ts_attempt2.ps'
PSOPEN,file=psfile
CS,SCALE=5
GSET,XMIN=0,XMAX=150,YMIN=0,YMAX=35
AXES,XTITLE='Years',YTITLE='Storms Year!U-1!N (10!U6!N km)!U-1!N',XSTEP=10,YSTEP=3,YMINOR=1.5,NDECS=1
GPLOT,X=year_count,Y=yearly_densities,/LEGEND,LEGPOS=9,LABELS='Year by Year Genesis Density [model output]',COL=4
GPLOT,X=year_count(5:n_year-6),Y=yearly_densities_smoothed(5:n_year-6),COL=1,THICK=150
GPLOT,X=year_count(5:n_year-6),Y=running_stddev(5:n_year-6),COL=1,THICK=150,STYLE=2
PSCLOSE
STOP
END




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; key
;HIST1  
;HIST2 
;HIST3                     HIST3         HIST2         HIST1     HIST[3,2,1]
;GPLOT            LABELS=['hist3 name','hist2 name','hist1 name'],COL=[9,10,8]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
