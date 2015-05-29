PRO mjo_indices_mean_state,infile,model_var_name,contour_var_name,obs_infile,obs_var_name,model_name,obs_name,model_desc,obs_desc,var_desc,box=box,$
                           obs_mult=obs_mult,model_mult=model_mult,obs_offset=obs_offset,model_offset=model_offset,latrev_obs=latrev_obs,$
                           mask_plot=mask_plot,mask_file=mask_file

; Plot the annual-mean of a quantity for a model integration and
; observations.  Note that no attempt is made to ensure that the model
; and observations are on the same grid.

;box=[-30,0,30,360]

IF KEYWORD_SET(box) THEN BEGIN
   box(0)=box(0)-1
   box(2)=box(2)+1
ENDIF

print,'Now processing ',model_name,' for variable ',model_var_name,' using contour levels for ',contour_var_name

CASE contour_var_name OF
   'precip' : BEGIN
      mylevs_raw=['1','2','3','4','5','7','9','11','13','15','18','21']
      mylevs_diff=['-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5']
      raw_lower_white=1
      reverse_colors=1
   END
   'u200' : BEGIN
      mylevs_diff=['-10.5','-8.5','-6.5','-4.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','4.5','6.5','8.5','10.5']
      mylevs_raw=['-33','-27','-21','-15','-9','-3','0','3','9','15','21','27','33']
      raw_lower_white=0
      reverse_colors=0
   END
   'u850' : BEGIN
      mylevs_diff=['-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5']
      mylevs_raw=['-8','-6','-4','-2','0','2','4','6','8']
      raw_lower_white=0
      reverse_colors=0
   END
   'sst' : BEGIN
      mylevs_diff=['-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5']
      mylevs_raw=['290','291','292','293','294','295','296','297','298','299','300','301','302','303']
      raw_lower_white=0
      reverse_colors=0
   END
   'olr' : BEGIN
      mylevs_diff=['-60','-52','-44','-36','-28','-20','-12','-4','4','12','20','28','36','44','52','60']
      mylevs_raw=['160','180','200','220','240','260','280','300','320']
      raw_lower_white=0
      reverse_colors=0
   END
ENDCASE

IF NOT KEYWORD_SET(obs_mult) THEN $
   obs_mult=1.
IF NOT KEYWORD_SET(model_mult) THEN $
   model_mult=1.
IF NOT KEYWORD_SET(obs_offset) THEN $
   obs_offset=0.
IF NOT KEYWORD_SET(model_offset) THEN $
   model_offset=0.

IF KEYWORD_SET(mask_plot) THEN BEGIN
   mask_latitude=OPEN_AND_EXTRACT(mask_file,'latitude')
   mask_longitude=OPEN_AND_EXTRACT(mask_file,'longitude')
   IF KEYWORD_SET(box) THEN $
      DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
   mask_nlon=N_ELEMENTS(mask_longitude)
   mask_nlat=N_ELEMENTS(mask_latitude)
   mask=REFORM(OPEN_AND_EXTRACT(mask_file,'lsm',$
                                offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                                count=[mask_nlon,mask_nlat,1,1]))
ENDIF

obs_longitude=OPEN_AND_EXTRACT(obs_infile,'longitude')
obs_latitude=OPEN_AND_EXTRACT(obs_infile,'latitude')
IF KEYWORD_SET(box) THEN $
   DEFINE_BOUNDARIES,box,obs_latitude,obs_longitude,obs_box_tx,/LIMIT
obs_nlon=N_ELEMENTS(obs_longitude)
obs_nlat=N_ELEMENTS(obs_latitude)
IF NOT KEYWORD_SET(box) THEN $
   obs_box_tx=[0,0,obs_nlat,obs_nlon]

obs_id=NCDF_OPEN(obs_infile)
obs_varstruct=NCDF_VARINQ(obs_id,obs_var_name)
print,obs_varstruct.ndims
IF (obs_varstruct.ndims eq 2) THEN BEGIN
   obs_annmean=REFORM(OPEN_AND_EXTRACT(obs_infile,obs_var_name,$
                                       offset=[obs_box_tx(1),obs_box_tx(0)],$
                                       count=[obs_nlon,obs_nlat]))*obs_mult+obs_offset
ENDIF ELSE IF (obs_varstruct.ndims eq 4) THEN BEGIN
   obs_annmean=REFORM(OPEN_AND_EXTRACT(obs_infile,obs_var_name,$
                                       offset=[obs_box_tx(1),obs_box_tx(0),0,0],$
                                       count=[obs_nlon,obs_nlat,1,1]))*obs_mult+obs_offset
ENDIF ELSE IF (obs_varstruct.ndims eq 3) THEN BEGIN
   obs_annmean=REFORM(OPEN_AND_EXTRACT(obs_infile,obs_var_name,$
                                       offset=[obs_box_tx(1),obs_box_tx(0),0],$
                                       count=[obs_nlon,obs_nlat,1]))*obs_mult+obs_offset
ENDIF

model_longitude=OPEN_AND_EXTRACT(infile,'longitude')
model_latitude=OPEN_AND_EXTRACT(infile,'latitude')
IF KEYWORD_SET(box) THEN $
   DEFINE_BOUNDARIES,box,model_latitude,model_longitude,model_box_tx,/LIMIT
model_nlon=N_ELEMENTS(model_longitude)
model_nlat=N_ELEMENTS(model_latitude)
IF NOT KEYWORD_SET(box) THEN $
   model_box_tx=[0,0,model_nlat,model_nlon]

id=NCDF_OPEN(infile)
varstruct=NCDF_VARINQ(id,model_var_name)
print,varstruct.ndims

IF (varstruct.ndims eq 4) THEN BEGIN
   model_annmean=REFORM(OPEN_AND_EXTRACT(infile,model_var_name,$
                                         offset=[model_box_tx(1),model_box_tx(0),0,0],$
                                         count=[model_nlon,model_nlat,1,1]))*model_mult+model_offset
ENDIF ELSE IF (varstruct.ndims eq 2) THEN BEGIN
   model_annmean=REFORM(OPEN_AND_EXTRACT(infile,model_var_name,$
                                         offset=[model_box_tx(1),model_box_tx(0)],$
                                         count=[model_nlon,model_nlat]))*model_mult+model_offset
ENDIF ELSE IF (varstruct.ndims eq 3) THEN BEGIN
   model_annmean=REFORM(OPEN_AND_EXTRACT(infile,model_var_name,$
                                         offset=[model_box_tx(1),model_box_tx(0),0],$
                                         count=[model_nlon,model_nlat,1]))*model_mult+model_offset
ENDIF

diff_model_obs=fltarr(model_nlon,model_nlat)
FOR j=0,model_nlon-1 DO BEGIN
   FOR k=0,model_nlat-1 DO BEGIN
      IF KEYWORD_SET(latrev_obs) THEN BEGIN
         diff_model_obs(j,k)=model_annmean(j,k)-obs_annmean(j,model_nlat-k-1)
      ENDIF ELSE $
         diff_model_obs(j,k)=model_annmean(j,k)-obs_annmean(j,k)
   ENDFOR
ENDFOR

IF KEYWORD_SET(mask_plot) THEN BEGIN
   model_annmean[where(mask eq 1)]=!Values.F_NaN
   obs_annmean[where(mask eq 1)]=!Values.F_NaN
   diff_model_obs[where(mask eq 1)]=!Values.F_NaN
ENDIF

psfile='/home/ss901165/idl/mjo_indices/mjo_indices_mean_state.'+contour_var_name+'.annual_mean.'+model_name+'-compare-'+obs_name+'.ps'

PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=800,SPACE2=200,XOFFSET=500,YOFFSET=300,TFONT=2,TCHARSIZE=100,SPACE3=400,YPLOTS=3,SPACING=1500

POS,ypos=1,YSIZE=(ABS(box(2))+ABS(box(0)))/180.*21000
white=[N_ELEMENTS(mylevs_diff)/2+2]
print,white
IF reverse_colors eq 1 THEN BEGIN
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1,white=white,/REV
ENDIF ELSE $
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1,white=white
white=FSC_COLOR("white",20)
black=FSC_COLOR("black",22)
MAP,LONMIN=1,LONMAX=359,LATMIN=box(0),LATMAX=box(2),/HIRES
LEVS,MANUAL=mylevs_diff
CON,Y=model_latitude,X=model_longitude,FIELD=diff_model_obs,NEGATIVE_STYLE=2,POSITIVE_STYLE=0,/NOLINELABELS,$
    TITLE='Diff in ann-mean '+contour_var_name+' for '+model_desc+' minus '+obs_desc,NEGATIVE_COL=22,POSITIVE_COL=20,CB_WIDTH=80,$
    ZERO_COL=22,ZERO_STYLE=0,CB_HEIGHT=65

POS,ypos=2,YSIZE=(ABS(box(2))+ABS(box(0)))/180.*21000
IF raw_lower_white eq 1 THEN BEGIN
   IF reverse_colors eq 1 THEN BEGIN
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1,white=[2],/REV
   ENDIF ELSE $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1,white=[2]
ENDIF ELSE BEGIN
   IF reverse_colors eq 1 THEN BEGIN
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1,/REV
   ENDIF ELSE $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1
ENDELSE
white=FSC_COLOR("white",20)
black=FSC_COLOR("black",22)
MAP,LONMIN=1,LONMAX=359,LATMIN=box(0),LATMAX=box(2),/HIRES
LEVS,MANUAL=mylevs_raw
CON,Y=model_latitude,X=model_longitude,FIELD=model_annmean,NEGATIVE_STYLE=2,POSITIVE_STYLE=0,/NOLINELABELS,$
    TITLE='Ann-mean '+contour_var_name+' for '+model_desc,NEGATIVE_COL=22,POSITIVE_COL=20,CB_WIDTH=80,ZERO_COL=22,ZERO_STYLE=2,$
    ZERO_THICK=200,CB_HEIGHT=65,/NOLINES

POS,ypos=3,YSIZE=(ABS(box(2))+ABS(box(0)))/180.*21000
IF raw_lower_white eq 1 THEN BEGIN
   IF reverse_colors eq 1 THEN BEGIN
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1,white=[2],/REV
   ENDIF ELSE $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1,white=[2]
ENDIF ELSE BEGIN
   IF reverse_colors eq 1 THEN BEGIN
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1,/REV
   ENDIF ELSE $
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1
ENDELSE
white=FSC_COLOR("white",20)
black=FSC_COLOR("black",22)
MAP,LONMIN=1,LONMAX=359,LATMIN=box(0),LATMAX=box(2),/HIRES
LEVS,MANUAL=mylevs_raw
CON,Y=obs_latitude,X=obs_longitude,FIELD=obs_annmean,NEGATIVE_STYLE=2,POSITIVE_STYLE=0,/NOLINELABELS,$
    TITLE='Ann-mean '+contour_var_name+' for '+obs_desc,NEGATIVE_COL=22,POSITIVE_COL=20,CB_WIDTH=80,ZERO_COL=22,ZERO_STYLE=2,$
    ZERO_THICK=200,CB_HEIGHT=65,/NOLINES

PSCLOSE,/NOVIEW

;STOP

END

