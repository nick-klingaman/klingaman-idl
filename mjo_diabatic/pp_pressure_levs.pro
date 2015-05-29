function pp_pressure_levs, data, p_theta, data_on_plevs
;
; Very simple IDL function to convert data from theta levels to pressure levels.
;
; Note that if the pressure is oustide the range of p_theta we do not extrapolate.
; 
; INPUT:
; data:      3-D field on n theta levels
; p_theta:   Pressure on n model theta levels
; p_levs:    Array of some other data on the pressure levels required

p_theta=p_theta(sort(p_theta.lblev))
data=data(sort(data.lblev))

for i_press=0, n_elements(data_on_plevs)-1 do begin
;   convert pressure to Pa
    pressure=100*data_on_plevs(i_press).blev
;
;   Loop over model levels, setting the bottom of the 2 levels from 
;   which we wish to interpolate to the highest levels at which the 
;   pressure is more than the required pressure.
;
    pressure_below=p_theta(0)
    pressure_above=p_theta(1)
    data_below=data(0)
    data_above=data(1)
    for i=1,n_elements(p_theta)-2 do begin
        indices=where(p_theta(i).data gt pressure)
        if indices(0) ne -1 then begin
            pressure_below.data(indices)=p_theta(i).data(indices)
            pressure_above.data(indices)=p_theta(i+1).data(indices)
            data_below.data(indices)=data(i).data(indices)
            data_above.data(indices)=data(i+1).data(indices)
        endif
    endfor

    weight_upper=pp_ff('alog(a/b)/alog(c/b)',pressure,pressure_below,pressure_above)
    weight_lower=pp_ff('1.0-a',weight_upper)

    thisdata=pp_ff('a*b+c*d',weight_upper,data_above,weight_lower,data_below)
;
;   Set points where lowest level pressure is less than the required pressure
;   to the value at that level
;
    indices=where(p_theta(0).data(*) lt pressure)
    if indices(0) ne -1 then begin
        thisdata.data(indices)=data(0).data(indices)
    endif
;
;   Do the same with the highest level
;
;
    indices=where(p_theta(n_elements(p_theta)-1).data(*) gt pressure)
    if indices(0) ne -1 then begin
        thisdata.data(indices)=data(n_elements(p_theta)-1).data(indices)
    endif

    data_on_plevs(i_press).data=thisdata.data

    headers=['lbyr','lbmon','lbdat','lbhr','lbmin','lbday',$
             'lbyrd','lbmond','lbdatd','lbhrd','lbmind','lbdayd',$
             'lbtim','lbft','lbfc','lbcfc','lbproc','lbexp','lbuser(*)']

    for iheader=0,n_elements(headers)-1 do begin
        ierr=execute('data_on_plevs(i_press).'+headers(iheader)+'=data_below.'+headers(iheader))
    endfor

    data_on_plevs.BRSVD(0)=0
    data_on_plevs.BRSVD(1)=0
        
endfor
return, data_on_plevs
end
