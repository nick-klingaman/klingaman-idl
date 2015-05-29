PRO mjo_diabatic_ngms_20day_byinit,models,case_letter

;models=['cam5zm']
leads=indgen(20)
n_models=N_ELEMENTS(models)

IF case_letter eq 'E' THEN BEGIN
   dates=['20091010','20091011','20091012','20091013','20091014','20091015','20091016','20091017','20091018','20091019',$
          '20091020','20091021','20091022','20091023','20091024','20091025','20091026','20091027','20091028','20091029',$
          '20091030','20091031','20091101','20091102','20091103','20091104','20091105','20091106','20091107','20091108',$
          '20091109','20091110','20091111','20091112','20091113','20091114','20091115','20091116','20091117','20091118',$
          '20091119','20091120','20091121','20091122','20091123','20091124','20091125']
ENDIF ELSE $
   dates=['20091210','20091211','20091212','20091213','20091214','20091215','20091216','20091217','20091218','20091219',$
          '20091220','20091221','20091222','20091223','20091224','20091225','20091226','20091227','20091228','20091229',$
          '20091230','20091231','20100101','20100102','20100103','20100104','20100105','20100106','20100107','20100108',$
          '20100109','20100110','20100111','20100112','20100113','20100114','20100115','20100116','20100117','20100118',$
          '20100119','20100120','20100121','20100122','20100123','20100124','20100125']

offset_time=15
nt=160-offset_time
ndates=N_ELEMENTS(dates)
ngms_horiz=fltarr(n_models,ndates)
ngms_vert=fltarr(n_models,ndates)

; Various constants
cpd=1005.
cpv=1850.
rv0=461.5
rd0=287.05
eps=rd0/rv0
alv0=2.5e6
tref=273.1
pref=1.e3
esf=6.1078

dgx=2.5
dgy=2.5
ds=111.1*1.e+03

idx=1

iy0=25
pi=4.*atan(1.)

; SET CASE LETTER
;case_letter='E'

   
FOR i=0,n_models-1 DO BEGIN  
   print,'----- '+models(i)+' -----'
   CASE models(i) OF
      'cam5zm' : BEGIN
         dir='/home/ss901165/um_output6/mjodiab_20day/cam5zm'
         filename='CAM5ZMMicroCAPT'
         z_name='level'
         z_mult=1. ; to convert to hPa
         lat_name='latitude'
         lon_name='longitude'
         ;nt=44.
         ;offset_time=44
         missval=1e10
      END
      'cam5' : BEGIN
         dir='/home/ss901165/um_output6/mjodiab_20day/cam5'
         z_name='levels'
         z_mult=1.
         lon_name='longitude'
         lat_name='latitude'
         ;nt=44
         ;offset_time=44
         missval=1e10
      END
      'cancm4' : BEGIN
         dir='/home/ss901165/um_output6/mjodiab_20day/cancm4'
         filename='CanCM4'
         z_name='level'
         z_mult=0.01
         lon_name='longitude'
         lat_name='latitude'
         ;nt=44
         ;offset_time=44
         missval=1e10
      END
      'cnrm_atmos' : BEGIN
         dir='/home/ss901165/um_output6/mjodiab_20day/cnrm_atmos'
         filename='CNRM'
         z_name='lev'
         z_mult=1
         lon_name='longitude'
         lat_name='latitude'
         ;nt=44
         ;offset_time=44
         missval=1e10
      END
      'ecearth' : BEGIN
         dir='/home/ss901165/um_output6/mjodiab_20day/ecearth'
         filename='ecearth3'
         z_name='lev'
         z_mult=0.01
         lon_name='lon'
         lat_name='lat'
         ;nt=44
         ;offset_time=0
         missval=1e10
      END
      'ecmwf' : BEGIN
         dir='/home/ss901165/um_output6/mjodiab_20day/ecmwf'
         z_name='level'
         z_mult=1
         lon_name='longitude'
         lat_name='latitude'
         ;nt=44
         ;offset_time=0
         missval=1e10
      END
      'giss' : BEGIN
         dir='/home/ss901165/um_output6/mjodiab_20day/giss'
         filename='ModelE'
         z_name='level'
         z_mult=1
         lon_name='longitude'
         lat_name='latitude'
         ;nt=44
         ;offset_time=0
         missval=-1e10
      END
      'metum' : BEGIN
         dir='/home/ss901165/um_output6/mjodiab_20day/metum'
         filename='MetUM'
         z_name='level'
         z_mult=1
         lon_name='longitude'
         lat_name='latitude'
         ;nt=44
         ;offset_time=0
         missval=1e5
         leads=indgen(19)
         n_leads=19
      END
      'miroc' : BEGIN
         dir='/home/ss901165/um_output6/mjodiab_20day/miroc'
         filename='miroc5'
         z_name='level'
         z_mult=1
         lon_name='longitude'
         lat_name='latitude'
         ;nt=44
         ;offset_time=0
         missval=-100.
      END           
      'mri' : BEGIN
         dir='/home/ss901165/um_output6/mjodiab_20day/mri'
         filename='MRI-AGCM'
         z_name='plev'
         z_mult=0.01
         lon_name='lon'
         lat_name='lat'
         ;nt=44
         ;offset_time=0
         missval=1e10
      END
      'nasa' : BEGIN
         dir='/home/ss901165/um_output6/mjodiab_20day/nasa'
         filename='GEOS5_AGCM'
         z_name='lev'
         z_mult=1
         lon_name='lon'
         lat_name='lat'
         ;nt=44
         ;offset_time=0
         missval=1e10
      END
      'nicam' : BEGIN
         dir='/home/ss901165/um_output6/mjodiab_20day/nicam'
         z_name='level'
         z_mult=1
         lon_name='longitude'
         lat_name='latitude'
         ;nt=44
         ;offset_time=0
         missval=-1e10
      END      
      'nrl' : BEGIN
         dir='/home/ss901165/um_output6/mjodiab_20day/nrl'
         z_name='level'
         z_mult=1
         lon_name='longitude'
         lat_name='latitude'
         ;nt=44
         ;offset_time=0
         missval=-1e8
      END
      'spcam' : BEGIN
         dir='/home/ss901165/um_output6/mjodiab_20day/spcam'
         z_name='lev_p'
         z_mult=1
         lon_name='longitude'
         lat_name='latitude'
         ;nt=44
         ;offset_time=0
         missval=1e10
      END
   ENDCASE

   openw,lun,dir+'/ngms_'+case_letter+'_byinit.txt',/get_lun
   printf,lun,'Normalised Gross Moist Stability for Case '+case_letter

   FOR l=0,ndates-1 DO BEGIN
      print,'   -->  For date '+STRTRIM(STRING(dates(l)),1)
      printf,lun,'Date '+STRTRIM(STRING(dates(l)),1)
   
;   ta_file=dir+'/'+models(i)+'.20091010-20100125_dmeans.ta_lead'+STRTRIM(STRING(leads(l)),1)+'.nc'
;   ua_file=dir+'/'+models(i)+'.20091010-20100125_dmeans.ua_lead'+STRTRIM(STRING(leads(l)),1)+'.nc'
;   va_file=dir+'/'+models(i)+'.20091010-20100125_dmeans.va_lead'+STRTRIM(STRING(leads(l)),1)+'.nc'
;   wap_file=dir+'/'+models(i)+'.20091010-20100125_dmeans.wap_lead'+STRTRIM(STRING(leads(l)),1)+'.nc'
;   hus_file=dir+'/'+models(i)+'.20091010-20100125_dmeans.hus_lead'+STRTRIM(STRING(leads(l)),1)+'.nc'

      ta_file=dir+'/'+dates(l)+'/'+filename+'.ta.'+dates(l)+'.00Z.nc'
      ua_file=dir+'/'+dates(l)+'/'+filename+'.ua.'+dates(l)+'.00Z.nc'
      va_file=dir+'/'+dates(l)+'/'+filename+'.va.'+dates(l)+'.00Z.nc'
      wap_file=dir+'/'+dates(l)+'/'+filename+'.wap.'+dates(l)+'.00Z.nc'
      hus_file=dir+'/'+dates(l)+'/'+filename+'.hus.'+dates(l)+'.00Z.nc'


   landsea_file='/home/ss901165/um_output6/mjodiab_20day/ecmwf/20091010/ECMWF_IFS.landsea.20091010.00Z.nc'
   
   ; Read vertical coordinate
   p=OPEN_AND_EXTRACT(ta_file,z_name)*z_mult*100. ; Convert to Pa
   np=N_ELEMENTS(p)
   dp=fltarr(np)
   p1=fltarr(np)
   p1(0)=1000.*100.
   FOR j=1,np-1 DO $
      p1(j)=(p(j-1)+p(j))/2.
   FOR j=1,np-2 DO $
      dp(j)=p1(j)-p1(j+1)
   dp(0)=p(0)-p1(1)
   dp(np-1)=p1(np-1)-p(np-1)
   
   ; Read latitudes and longitudes
   xlon=OPEN_AND_EXTRACT(ta_file,lon_name)
   ylat=OPEN_AND_EXTRACT(ta_file,lat_name)
   nlon=N_ELEMENTS(xlon)
   nlat=N_ELEMENTS(ylat)
   dx=fltarr(nlat)
   alat=fltarr(nlat)
   FOR j=0,nlat-1 DO BEGIN
      dx(j)=dgx*ds*cos(ylat(j)/180.*pi*idx)
      alat(j)=ylat(j)/180.*pi
   ENDFOR
   dy=dgy*ds

   ; Read all times and gridpoints in input files
   ta = OPEN_AND_EXTRACT(ta_file,'ta',offset=[0,0,0,offset_time],$
                         count=[nlon,nlat,np,nt])
   hus = OPEN_AND_EXTRACT(hus_file,'hus',offset=[0,0,0,offset_time],$
                         count=[nlon,nlat,np,nt])
   ua = OPEN_AND_EXTRACT(ua_file,'ua',offset=[0,0,0,offset_time],$
                         count=[nlon,nlat,np,nt])
   va = OPEN_AND_EXTRACT(va_file,'va',offset=[0,0,0,offset_time],$
                         count=[nlon,nlat,np,nt])
   wap = OPEN_AND_EXTRACT(wap_file,'wap',offset=[0,0,0,offset_time],$
                         count=[nlon,nlat,np,nt])
   
   IF models(i) eq 'miroc' THEN BEGIN
       IF TOTAL(where(ta lt missval)) ge 0 THEN $
          ta[where(ta lt missval)]=-9999.
       IF TOTAL(where(hus lt missval)) ge 0 THEN $
          hus[where(hus lt missval)]=-9999.
       IF TOTAL(where(ua lt missval)) ge 0 THEN $
          ua[where(ua lt missval)]=-9999.
       IF TOTAL(where(va lt missval)) ge 0 THEN $
          va[where(va lt missval)]=-9999.
       IF TOTAL(where(wap lt missval)) ge 0 THEN $
          wap[where(wap lt missval)]=-9999.
    ENDIF ELSE BEGIN
       IF TOTAL(where(ABS(ta) ge ABS(missval))) ge 0 THEN $
          ta[where(ABS(ta) ge ABS(missval))]=-9999.
       IF TOTAL(where(ABS(hus) ge ABS(missval))) ge 0 THEN $
          hus[where(ABS(hus) ge ABS(missval))]=-9999.
       IF TOTAL(where(ABS(ua) ge ABS(missval))) ge 0 THEN $
          ua[where(ABS(ua) ge ABS(missval))]=-9999.
       IF TOTAL(where(ABS(va) ge ABS(missval))) ge 0 THEN $
          va[where(ABS(va) ge ABS(missval))]=-9999.
       IF TOTAL(where(ABS(wap) ge ABS(missval))) ge 0 THEN $
          wap[where(ABS(wap) ge ABS(missval))]=-9999.
    ENDELSE

   landsea = OPEN_AND_EXTRACT(landsea_file,'landsea')

                                ; Reversible moist entropy
   ru=fltarr(nlon,nlat,np,nt)
   rv=fltarr(nlon,nlat,np,nt)
   dsdp=fltarr(nlon,nlat,np,nt)
   dsdx=fltarr(nlon,nlat,np,nt)
   dsdy=fltarr(nlon,nlat,np,nt)
   drudx=fltarr(nlon,nlat,np,nt)
   drvdy=fltarr(nlon,nlat,np,nt)
   s=fltarr(nlon,nlat,np,nt)
   t1=fltarr(nlon,nlat,nt)
   t1m=fltarr(nlon,nlat)
   t2=fltarr(nlon,nlat,nt)
   t2m=fltarr(nlon,nlat)
   rr=fltarr(nlon,nlat,nt)
   rrm=fltarr(nlon,nlat)
   FOR t=0,nt-1 DO BEGIN
      FOR z=0,np-1 DO BEGIN
         FOR x=0,nlon-1 DO BEGIN
            FOR y=0,nlat-1 DO BEGIN
               
               IF hus(x,y,z,t)-hus(x,y,z,t) ne 0 THEN $
                  hus(x,y,z,t)=-9999.

               IF hus(x,y,z,t) ne -9999 THEN $
                  IF hus(x,y,z,t) le 0 THEN $
                     hus(x,y,z,t)=1.e-5

               IF hus(x,y,z,t) ne -9999 THEN BEGIN
                  rp=hus(x,y,z,t)/(1-hus(x,y,z,t))
               ENDIF ELSE $
                  rp=-9999.

               IF ta(x,y,z,t) ne -9999 THEN $
                  IF ta(x,y,z,t) le 0 THEN $
                     ta(x,y,z,t)=200.

               tp=REFORM(ta(x,y,z,t))
               pp=p(z)/100.

               IF rp ne -9999 and ua(x,y,z,t) ne -9999 THEN BEGIN
                  ru(x,y,z,t)=rp*ua(x,y,z,t)
               ENDIF ELSE $
                  ru(x,y,z,t)=-9999.

               IF rp ne -9999 and va(x,y,z,t) ne -9999 THEN BEGIN
                  rv(x,y,z,t)=rp*va(x,y,z,t)
               ENDIF ELSE $
                  rv(x,y,z,t)=-9999.
               
               IF rp ne -9999 THEN BEGIN
                  evd=eps*pp/(eps+rp)
                  evp=rp*pp/(eps+rp)
               ENDIF ELSE BEGIN
                  evd=-9999.
                  evp=-9999.
               ENDELSE

               IF rp ne -9999 and tp ne -9999 and evd ne -9999 and evp ne -9999 THEN BEGIN
                  s(x,y,z,t)=(cpd+rp*cpv)*alog(tp/tref)-rd0*alog(evd/pref)-$
                             rp*rv0*alog(evp/esf)+alv0*rp/tref      
               ENDIF ELSE $
                  s(x,y,z,t)=-9999.

            ENDFOR
         ENDFOR
      ENDFOR

      FOR x=0,nlon-1 DO BEGIN
         FOR y=0,nlat-1 DO BEGIN
            FOR z=1,np-2 DO BEGIN
               dsdp(x,y,z,t)=(s(x,y,z-1,t)-s(x,y,z+1,t))/(p(z-1)-p(z+1))
               IF s(x,y,z-1,t) eq -9999 or s(x,y,z+1,t) eq -9999 THEN $
                  dsdp(x,y,z,t)=-9999.
            ENDFOR
            dsdp(x,y,0,t)=(s(x,y,0,t)-s(x,y,1,t))/(p(0)-p(1))
            dsdp(x,y,np-1,t)=(s(x,y,np-2)-s(x,y,np-1))/(p(np-2)-p(np-1))
            IF s(x,y,0,t) eq -9999 or s(x,y,1,t) eq -9999 THEN $
               dsdp(x,y,0,t)=-9999.
            IF s(x,y,np-1,t) eq -9999 or s(x,y,np-2,t) eq -9999 THEN $
               dsdp(x,y,np-1,t)=-9999.
            
         ENDFOR
      ENDFOR
               
      FOR y=0,nlat-1 DO BEGIN
         FOR x=0,nlon-1 DO BEGIN            
            ia=max([x-1,0])
            ib=min([x+1,nlon-1])
            FOR z=0,np-1 DO BEGIN
               dsdx(x,y,z,t)=(s(ib,y,z,t)-s(ia,y,z,t))/dx(y)/2.
               IF s(ia,y,z,t) eq -9999 or s(ib,y,z,t) eq -9999 THEN $
                  dsdx(x,y,z)=-9999.
               drudx(x,y,z,t)=(ru(ib,y,z,t)-ru(ia,y,z,t))/dx(y)/2.
               IF ru(ia,y,z,t) eq -9999 or ru(ib,y,z,t) eq -9999 THEN $
                  drudx(x,y,z,t)=-9999.
            ENDFOR
         ENDFOR
      ENDFOR
   
      FOR x=0,nlon-1 DO BEGIN
         FOR y=2,nlat-2 DO BEGIN
            ja=y-1
            jb=y+1
            FOR z=0,np-1 DO BEGIN
               dsdy(x,y,z,t)=(s(x,jb,z,t)-s(x,ja,z,t))/dy/2.
               IF s(x,ja,z,t) eq -9999 or s(x,jb,z,t) eq -9999 THEN $
                  dsdy(x,y,z,t)=-9999.
               drvdy(x,y,z,t)=(rv(x,jb,z,t)-rv(x,ja,z,t))/dy/2.
               IF rv(x,ja,z,t) eq -9999 or rv(x,jb,z,t) eq -9999 THEN $
                  drvdy(x,y,z,t)=-9999.
            ENDFOR
         ENDFOR
      ENDFOR
            
      FOR y=0,nlat-1 DO BEGIN
         FOR x=0,nlon-1 DO BEGIN
            t1mm=0.
            t2mm=0.
            rrmm=0.
            nt1=0               
            nt2=0               
            nrr=0
            FOR z=0,np-2 DO BEGIN
               IF ua(x,y,z,t) ne -9999 and va(x,y,z,t) ne -9999 $
                  and dsdx(x,y,z,t) ne -9999 and dsdy(x,y,z,t) ne -9999 THEN BEGIN
                  nt1=nt1+1
                  t1mm=t1mm+ua(x,y,z,t)*dsdx(x,y,z,t)*dp(z)+va(x,y,z,t)*dsdy(x,y,z,t)*dp(z)
               ENDIF
               IF wap(x,y,z,t) ne -9999 and dsdp(x,y,z,t) ne -9999 THEN BEGIN
                  nt2=nt2+1
                  t2mm=t2mm+wap(x,y,z,t)*dsdp(x,y,z,t)*dp(z)
               ENDIF
               IF drudx(x,y,z,t) ne -9999 and drvdy(x,y,z,t) ne -9999 THEN BEGIN
                  nrr=nrr+1
                  rrmm=rrmm+drudx(x,y,z,t)*dp(z)+drvdy(x,y,z,t)*dp(z)
               ENDIF
            ENDFOR
            IF nt1 ne 0 THEN BEGIN
               t1(x,y,t)=t1mm/9.8*tref/alv0*86400.*28.4
            ENDIF ELSE $
               t1(x,y,t)=-9999.         
            IF nt2 ne 0 THEN BEGIN
               t2(x,y,t)=t2mm/9.8*tref/alv0*86400.*28.4
            ENDIF ELSE $
               t2(x,y,t)=-9999.
            IF nrr ne 0 THEN BEGIN
               rr(x,y,t)=rrmm/9.8*86400.*28.4    ; wm-2
            ENDIF ELSE $
               rr(x,y,t)=-9999.
         ENDFOR
      ENDFOR

      FOR y=0,nlat-1 DO BEGIN
         FOR x=0,nlon-1 DO BEGIN
            IF landsea(x,y) eq 1.0 THEN BEGIN
               s(x,y,*,t)=-9999.
               t1(x,y,t)=-9999.
               t2(x,y,t)=-9999.
               rr(x,y,t)=-9999.
            ENDIF
         ENDFOR
      ENDFOR
   ENDFOR

   t1a=t1
   t2a=t2
   rra=rr
   
   FOR y=0,nlat-1 DO BEGIN
      FOR x=0,nlon-1 DO BEGIN
         IF landsea(x,y) ne 1 THEN BEGIN
            FOR t=8*8,nt-9*8 DO BEGIN
               t1s=0.
               t2s=0.
               rrs=0.
               nt1=0            
               nt2=0            
               nrr=0
               FOR id=8,17*8 DO BEGIN                  
                  IF t1(x,y,t+id-9*8) ne -9999. THEN BEGIN
                     nt1=nt1+1
                     t1s=t1s+t1(x,y,t+id-9*8)
                  ENDIF
                  IF t2(x,y,t+id-9*8) ne -9999. THEN BEGIN
                     nt2=nt2+1
                     t2s=t2s+t2(x,y,t+id-9*8)
                  ENDIF                
                  IF rr(x,y,t+id-9*8) ne -9999. THEN BEGIN
                     nrr=nrr+1
                     rrs=rrs+rr(x,y,t+id-9*8)
                  ENDIF
               ENDFOR               
               IF nt1 eq 0 THEN BEGIN
                  t1a(x,y,t)=-9999.
               ENDIF ELSE $
                  t1a(x,y,t)=t1s/float(nt1)               
               IF nt2 eq 0 THEN BEGIN
                  t2a(x,y,t)=-9999.
               ENDIF ELSE $
                  t2a(x,y,t)=t2s/float(nt2)
               IF nrr eq 0 THEN BEGIN
                  t2a(x,y,t)=-9999.
               ENDIF ELSE $
                  rra(x,y,t)=rrs/float(nrr)               
            ENDFOR
         ENDIF
      ENDFOR
   ENDFOR
   
   t1b=t1a
   t2b=t2a
   rrb=rra

   FOR t=0,nt-1 DO BEGIN
      FOR y=1,nlat-2 DO BEGIN
         FOR x=1,nlon-2 DO BEGIN
            IF landsea(x,y) ne 1 THEN BEGIN
               t1s=0.
               t2s=0.
               rrs=0.
               nt1=0
               nt2=0
               nrr=0

               FOR xx=x-1,x+1 DO BEGIN
                  FOR yy=y-1,y+1 DO BEGIN
                     IF t1a(xx,yy,t) ne -9999. THEN BEGIN
                        nt1=nt1+1
                        t1s=t1s+t1a(xx,yy,t)
                     ENDIF
                     IF t2a(xx,yy,t) ne -9999. THEN BEGIN
                        nt2=nt2+1
                        t2s=t2s+t2a(xx,yy,t)
                     ENDIF
                     IF rra(xx,yy,t) ne -9999. THEN BEGIN
                        nrr=nrr+1
                        rrs=rrs+rra(xx,yy,t)
                     ENDIF
                  ENDFOR
               ENDFOR
               
               IF nt1 eq 0 THEN BEGIN
                  t1b(x,y,t)=-9999.
               ENDIF ELSE $
                  t1b(x,y,t)=t1s/float(nt1)
               IF nt2 eq 0 THEN BEGIN
                  t2b(x,y,t)=-9999.
               ENDIF ELSE $
                  t2b(x,y,t)=t2s/float(nt2)
               IF nrr eq 0 THEN BEGIN
                  rrb(x,y,t)=-9999.
               ENDIF ELSE $
                  rrb(x,y,t)=rrs/float(nrr)
            ENDIF
         ENDFOR
      ENDFOR
   ENDFOR

   FOR x=0,nlon-1 DO BEGIN
      FOR y=0,nlat-1 DO BEGIN
         t1s=0.
         t2s=0.
         rrs=0.         
         nn=0
         nnt=0

         FOR t=0,nt-1 DO BEGIN
            IF t1b(x,y,t) ne -9999 and t2b(x,y,t) ne -9999 and rrb(x,y,t) ne -9999. THEN BEGIN
               nn=nn+1
               t1s=t1s+t1b(x,y,t)
               t2s=t2s+t2b(x,y,t)
               rrs=rrs+rrb(x,y,t)
            ENDIF
         ENDFOR

         IF nn eq 0 THEN BEGIN
            t1m(x,y)=-9999.
            t2m(x,y)=-9999.
            rrm(x,y)=-9999.
         ENDIF ELSE BEGIN
            t1m(x,y)=t1s/float(nn)
            t2m(x,y)=t2s/float(nn)
            rrm(x,y)=rrs/float(nn)
         ENDELSE

      ENDFOR
   ENDFOR

   FOR x=0,nlon-1 DO BEGIN
      FOR y=0,nlat-1 DO BEGIN
         t1s=0.
         t2s=0.
         nn=0
         nnt=0
         FOR t=0,nt-1 DO BEGIN
            IF t1b(x,y,t) ne -9999 and t2b(x,y,t) ne -9999 and rrb(x,y,t) ne -9999. $
               and abs(rrb(x,y,t)) ge 5 THEN BEGIN
               nn=nn+1
               t1s=t1s-t1b(x,y,t)/rrb(x,y,t)
               t2s=t2s-t2b(x,y,t)/rrb(x,y,t)
            ENDIF
         ENDFOR
         IF nn eq 0 THEN BEGIN
            t1m(x,y)=-9999.
            t2m(x,y)=-9999.
         ENDIF ELSE BEGIN
            t1m(x,y)=t1s/float(nn)
            t2m(x,y)=t2s/float(nn)
         ENDELSE         
      ENDFOR
   ENDFOR

   t1s=0.
   t2s=0.
   nn=0
   nnt=0
   
   FOR x=25,73 DO BEGIN
      FOR y=19,31 DO BEGIN         
         IF t1m(x,y) ne -9999 and t2m(x,y) ne -9999 and rrm(x,y) ne -9999 and rrm(x,y) lt 0 THEN BEGIN
            nn=nn+1
            t1s=t1s+t1m(x,y)
            t2s=t2s+t2m(x,y)
         ENDIF
      ENDFOR
   ENDFOR

   IF nn eq 0 THEN BEGIN
      t1m(x,y)=-9999.
      t2m(x,y)=-9999.
   ENDIF ELSE BEGIN
      ngms_horiz(i,l)=t1s/float(nn)
      ngms_vert(i,l)=t2s/float(nn)
   ENDELSE

   print,'-------> Horizontal NGMS = ',ngms_horiz(i,l)
   print,'-------> Vertical NGMS = ',ngms_vert(i,l)

   printf,lun,'Horizontal NGMS = '+STRTRIM(STRING(ngms_horiz(i,l)),1)
   printf,lun,'Vertical NGMS = '+STRTRIM(STRING(ngms_vert(i,l)),1)

ENDFOR
   
   printf,lun,'Mean Horizontal NGMS = '+STRTRIM(STRING(MEAN(ngms_horiz(i,*))),1)
   printf,lun,'Mean Vertical NGMS = '+STRTRIM(STRING(MEAN(ngms_vert(i,*))),1)

   close,lun
   free_lun,lun
ENDFOR



STOP
END
