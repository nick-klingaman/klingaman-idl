function nicks_power_spectrum,x1,m,double=double,tukey=tukey,parzen=parzen,$
			in_frequency=in_frequency,in_period=in_period,$
			out_frequency=out_frequency,out_period=out_period,$
			bartlett=bartlett,taper_wt=taper_wt,$
			hanning=hanning,split_cosine_bell=split_cosine_bell,$
			linear=linear,percentage=percentage,lag=lag,$
			daniell=daniell,significance=significance,$
			no_detrend_taper=no_detrend_taper,xgrad=xgrad,$
			periodogram=periodogram,confidence_ratio=confidence_ratio,$
			dof=dof
;+
; NAME:	Power-spectrum
; PURPOSE:	Do a spectral analysis of a timeseries.
; CATEGORY: Statistics
; CLASSIFICATION KEYWORDS: Statisitics, Spectral analysis.
; CALLING SEQUENCE:
;		a=power_spectrum(x,m[,frequency])
; INPUTS:
;	X:		Vector of timeseries.
; OPTIONAL INPUT PARAMETERS:
;       M:        Integer. For Parzen, Turkey or Bartlett methods this is the 
;			cut-off lag for the autocovariance function.
;			By default, the number of frequencies is set to M if this is
;			set. However, this should not be set for the Daniell method.
;                 frequencies 2pi*j/N for j=0,..,M where N=length of timeseries.
;                 A value of fix(N/2) is used if M is omitted.
;                 IN_FREQUENCY and IN_PERIOD will override this.
;			***WARNING*** The formulae used to derive the power spectrum
;			should really be used only when N is even. Chatfield 
;			recommends chopping off one time point if N is odd.
; KEYWORD PARAMETERS:
;	See TAPER for PERCENTAGE,HANNING,SPLIT_COSINE_BELL,LINEAR.
;	See LAG_WINDOW for BARTLETT (rectangular), TUKEY and PARZEN.
;	IN_FREQUENCY:	Vector of frequencies used to calculate periodogram.
;				This overrides M and IN_PERIOD.
;	IN_PERIOD:		Vector of periods used to calculate periodogram.
;				This overrides M.
;	OUT_FREQUENCY:	Vector of frequencies used.
;	OUT_PERIOD: 	Vector of periods used. ***NB If a frequency is 0.0 the
;				period is set to the length of the timeseries. This may 
;				cause a funny vertical line at end of spectrum plot***
;	DOUBLE:	Use double precision.
;	PERCENTAGE:	A number between 0 and 100, normally 10-20, that is the
;			percentage of data to be tapered at each end.
;	TAPER_WT:	Vector of taper weights, same length as timeseries X.
;	LAG:		Vector of lag-window weights.
;	DANIELL:	Specifies the number of Daniell weights (must be odd) or an 
;			array of weights. If the latter, then significance can not be 
;			calculated. Setting this overrides any lag-window weighting.
;	DOF:		Degrees of freedom of power spectrum estimate.
;	CONFIDENCE_RATIO: 95% confidence ration for any theoretical spectrum.
;				2-D array. Multiply the theoretical power spectrum by
;				the confidence ration to give a confidence spectrum.
;				This is just a function of chi-squared and DOF.
;	SIGNIFICANCE: 95% confidence intervals for white noise spectrum. This is
;			  CONFIDENCE_RATIO multiplied by variance(x)/!pi, the 
;			  theoretical white noise spectrum.
;	PERIODOGRAM: Periodogram of timeseries. Only with DANIELL weighting.
;	PERIODS:	Vector of periods that correspond to the frequencies used.
;	XGRAD:	Trend of X that is removed prior to calculation.
;	NO_DETREND_TAPER: Do not detrend and taper data. Used by CROSS_SPECTRUM 
;				for which inputs have already been detrended and tapered
; RETURN VALUE:   Vector, length M+1, of the smoothed power spectrum.
; EXAMPLES:
;  a) Daniell method
;		a=power_spectrum(x,daniell=5,out_frequency=outf,periodogram=per)
;		Calculates a Daniell estimate of the power spectrum using a 5-point
;		running mean of the periodogram.
;  b) Parzen method (preferred)
;		a=power_spectrum(x,14,/parzen,out_frequency=outf,/linear,percent=15)
;		will calculate a Parzen estimate of the power spectrum based on the
;		autocovariance function up to lag 14. A linear
; 		taper is done on the front and last 15% of the data.
; EXTRA INFO: See http://hc0500/~hadds/spectral/spectral.html
; PROCEDURE:
;		First data is detrended and tapered. Seasonalities should have been
;		removed prior to this.
;		The periodogram is calculated.
;		The estimate of the power spectrum is a smoothed version of the
;		periodogram. This can be achieved two ways.
;		1) DANIELL weighting simply smooths the periodogram with a moving
;		average.
;		2) LAG-WINDOW weighting does the smoothing during the calculation
;		of the periodogram, and so the keyword PERIODOGRAM cannot be used
; 		when this type of smoothing is used.
;		Read Chatfield,'Analysis of Timeseries',chp7 for details.
;
;		Parzen or Daniell weighting with Hanning or Split Cosone Bell 
;		tapering is preferable.
;		if N is length of X and Y then
;		Length of lag_window which is M should be about 2*sqrt(N)
;		Number of Daniell weights about N/40. N=5 is good for short 
;		timeseries.
; MODIFICATION HISTORY:
;-
  author_name = '$Author: hadds $'
  date_name = '$Date: 2002/01/29 10:11:54 $'
  version_name = '$Revision: 1.5 $'
@comm_error
; make daniell default
; 
;______________________________________________________________________________
if n_params() lt 1 or n_params() gt 2 then message,'Bad number of parameters'
n=n_elements(x1)
if n mod 2 eq 1 then $
message,'WARNING: N is odd. Formulae apply for N even only. See man page.',/info
if keyword_set(double) then begin
	x=double(x1-avg(x1))
	pi=!dpi
	ndf=double(n)
	v=dindgen(n)+1
endif else begin
	x=x1-avg(x1)
	pi=!pi
	ndf=float(n)
	v=findgen(n)+1
endelse
npi=ndf*pi

; decide which method to use
ldaniell=0
ltukey=0
lparzen=0
lbartlett=0
llag=0
if n_elements(daniell) gt 0 then begin
	ldaniell=1
endif else if n_elements(lag) gt 0 then begin
	llag=1
endif else if keyword_set(parzen) then begin
	lparzen=1
endif else if keyword_set(tukey) then begin
	ltukey=1
endif else if keyword_set(bartlett) then begin
	lbartlett=1
endif else message,'No method selected'
nlag=n_elements(lag)

; set up the frequency and period and lag window information
; set up FREQUENCY and M1=lag cut-off
if n_elements(in_frequency) gt 0 then begin
	if min(in_frequency) lt 0.0 or max(in_frequency) gt pi then $
		message,'Inputted frequencies out of range (0-pi)'
	frequency=in_frequency
	if keyword_set(double) then frequency=double(frequency)
	if (lparzen or ltukey or lbartlett) and n_params() eq 1 then m1=n_elements(frequency)-1
endif else if n_elements(in_period) gt 0 then begin
	if min(in_period) lt 0.0 or max(in_period) gt ndf then $
		message,'Inputted periods out of range (0-length of timeseries)'
	if keyword_set(double) then pi1=double(in_period) else pi1=in_period
	frequency=2.0*pi/pi1
	if (lparzen or ltukey or lbartlett) and n_params() eq 1 then m1=n_elements(frequency)-1
endif else if n_params() eq 2 and ldaniell eq 0 then begin
	m1=fix(m)
	if m1 mod 2 ne 0 then begin
		message,'Setting truncation to an even number',/info
		m1=m1-1
	endif
	if keyword_set(double) then frequency=dindgen(m1+1)/double(m1)*pi $
	else frequency=findgen(m1+1)/float(m1)*pi
endif else if ldaniell then begin
	m1=n/2
	m=m1
	if keyword_set(double) then frequency=dindgen(m1+1)/double(m1)*pi $
	else frequency=findgen(m1+1)/float(m1)*pi
endif else begin
	if nlag eq 0 then message,'2nd parameter must be selected if not using Daniell method.'
	if keyword_set(double) then frequency=dindgen(nlag)/double(nlag-1)*pi $
	else frequency=findgen(nlag)/float(nlag-1)*pi
endelse

nfreq=n_elements(frequency)
findex=where(frequency ne 0.0)
periods=replicate(ndf,nfreq)
periods(findex)=2.0*pi/frequency(findex)
index=where(periods le n,count)
if count ne nfreq then begin
	periods=periods(index)
	frequency=frequency(index)
	nfreq=count
endif

out_frequency=frequency
out_period=periods

if llag then begin
	lag1=lag
	if nlag ne nfreq then message,'LAG not correct length'
endif else if ldaniell eq 0 then begin
	lag1=lag_window(m1,double=double,tukey=ltukey,parzen=lparzen,bartlett=lbartlett)
endif

;______________________________________________________________________________

; if NO_DETREND_TAPER set (ie when CROSS_SPECTRUM calls this routine) then 
; don't detrend,taper, and centre the data.

if keyword_set(no_detrend_taper) eq 0 then begin
; detrend data
;	message,'Detrending data',/info
	yf=1
	g=svdfit(v,x,2,yfit=yf)
	x=x-yf
	xgrad=g(1)

; taper data
;message,'Tapering data',/info
	if keyword_set(taper_wt) then begin
		if n_elements(taper_wt) ne n then $
		message,'Arrays of timeseries and taper are different lengths'
		x=x*taper_wt
	endif else begin
		if not keyword_set(percentage) then  percentage=10
		taper_wt=taper(n,percentage,hanning=hanning,double=double,$
					split_cosine_bell=split_cosine_bell,linear=linear)
		x=x*taper_wt
	endelse
endif else taper_wt=[1.0]

;______________________________________________________________________________

; calculate cross_covariance function weighted by any lag-windows.

if ldaniell then wcov=cross_covariance(x,double=double) $
else wcov=cross_covariance(x,num=n_elements(lag1)+1,double=double)*lag1

;______________________________________________________________________________
; calculate periodogram

if keyword_set(double) then begin
	ans=dblarr(nfreq)
	v=dindgen(n_elements(wcov))+1.0d
endif else begin
	ans=fltarr(nfreq)
endelse
for i=0,nfreq-1 do ans(i)=wcov(0)+2.0*total(wcov(1:*)*cos(frequency(i)*v))
ans=ans/(avg(taper_wt^2)*pi)
if ldaniell then begin
	index=where(frequency eq 0.0,count)
	if count eq 1 then ans(index)=0.0
endif
periodogram=ans


;______________________________________________________________________________

; do any Daniell weighting

if ldaniell then begin

	if n_elements(daniell) eq 1 then begin
		if daniell mod 2 ne 1 then message,'Daniell must be an odd number'
		d2=(daniell-1)/2
		ans1=[reverse(ans(1:d2)),ans,reverse(ans(nfreq-d2-1:nfreq-2))]
		ans=([convol(ans1,replicate(1.0,daniell),daniell)])(d2:nfreq+d2-1)
	endif else if n_elements(daniell) gt 1 then begin
		d2=n_elements(daniell)/2
		ans1=[reverse(ans(1:d2)),ans,reverse(ans(nfreq-d2-1:nfreq-2))]
		ans=([convol(ans1,daniell/total(daniell))])(d2:nfreq+d2-1)
	endif else message,'Error in Daniell weighting.'
endif
;______________________________________________________________________________

; do significance testing

ks=[ldaniell,ltukey,lparzen,lbartlett,llag]
case extrareform(where(ks)) of
	0:	dof=2*daniell
	1:	dof=8.0*n/(3*m+2)
	2:	dof=3.71*n/m
	3:	dof=3.0*n/m
	4:	dof=2.0*n/total(lag1^2)
endcase

;started=options(/math,/start)
confidence_ratio=[chisqr_cvf(.025,dof*1.5),chisqr_cvf(.975,dof*1.5)]
significance=(stdev(x)^2)*confidence_ratio/!pi

;______________________________________________________________________________

return,ans
end
