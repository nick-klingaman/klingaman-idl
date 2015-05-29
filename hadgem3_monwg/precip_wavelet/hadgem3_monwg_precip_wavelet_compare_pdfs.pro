PRO hadgem3_monwg_precip_wavelet_compare_pdfs

hadgem1_file='/home/ss901165/idl/hadgem1_monwg/precip_wavelet/hadgem1_monwg_isv_power_pdf.NBoB.nc'
hadgem3_file='/home/ss901165/idl/hadgem3_monwg/precip_wavelet/hadgem3_monwg_isv_power_pdf.ahsaf.NBoB.nc'
hadam3_daily_file='/home/ss901165/um_output/dge/dge_isv_power_pdf.nc'
hadam3_monthly_file='/home/ss901165/um_output/mge/mge_isv_power_pdf.nc'
hadam3_fdy_file='/home/ss901165/um_output/fdy/fdy_isv_power_pdf.nc'
gpcp_file='/home/ss901165/um_output/obs/gpcp_isv_power_pdf.nc'

; Open the daily file and get the x-axis and the PDF
did=NCDF_OPEN(hadam3_daily_file)
dvarid=NCDF_VARID(did,'pdf_value')
daxisid=NCDF_VARID(did,'pdf_axis')
NCDF_VARGET,did,daxisid,daxis
NCDF_VARGET,did,dvarid,dpdf

; Open the monthly file and get the x-axis and the PDF
mid=NCDF_OPEN(hadam3_monthly_file)
mvarid=NCDF_VARID(mid,'pdf_value')
maxisid=NCDF_VARID(mid,'pdf_axis')
NCDF_VARGET,mid,maxisid,maxis
NCDF_VARGET,mid,mvarid,mpdf

; Open the five-daily file and get the x-axis and the PDF
fid=NCDF_OPEN(hadam3_fdy_file)
fvarid=NCDF_VARID(fid,'pdf_value')
faxisid=NCDF_VARID(fid,'pdf_axis')
NCDF_VARGET,fid,faxisid,faxis
NCDF_VARGET,fid,fvarid,fpdf

; Open the GPCP file and get the x-axis and the PDF
gid=NCDF_OPEN(gpcp_file)
gvarid=NCDF_VARID(gid,'pdf_value')
gaxisid=NCDF_VARID(gid,'pdf_axis')
NCDF_VARGET,gid,gaxisid,gaxis
NCDF_VARGET,gid,gvarid,gpdf

; Open the HadGEM1 file and get the x-axis and the PDF
eid=NCDF_OPEN(hadgem1_file)
evarid=NCDF_VARID(eid,'pdf_value')
eaxisid=NCDF_VARID(eid,'pdf_axis')
NCDF_VARGET,eid,eaxisid,eaxis
NCDF_VARGET,eid,evarid,epdf

; Open the HadGEM3 file and get the x-axis and the PDF
e3id=NCDF_OPEN(hadgem3_file)
e3varid=NCDF_VARID(eid,'pdf_value')
e3axisid=NCDF_VARID(eid,'pdf_axis')
NCDF_VARGET,e3id,e3axisid,e3axis
NCDF_VARGET,e3id,e3varid,e3pdf

psfile='/home/ss901165/idl/hadgem3_monwg/precip_wavelet/hadgem3_monwg_precip_wavelet_compare_pdfs.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=120
GSET,XMIN=0,XMAX=1200,YMIN=0,YMAX=MAX([dpdf,mpdf,fpdf,gpdf,epdf])*1.5
AXES,xstep=100,ystep=0.002,NDECS=3,$
  xtitle='Integrated, normalized power at periods between 30 and 50 days (bin width=3)',$
  ytitle='Probability'
black=FSC_COLOR("black",1)
red=FSC_COLOR("red",2)
cyan=FSC_COLOR("cyan",3)
orange=FSC_COLOR("orange",4)
brown=FSC_COLOR("brown",5)

; HadAM3 Daily (N144, 30 members)
good=where(daxis lt 1200)
GPLOT,X=daxis[good],Y=dpdf[good],COL=2,STYLE=0,THICK=200
; HadAM3 Monthly (N144, 30 members)
good=where(maxis lt 1200)
GPLOT,X=maxis[good],Y=mpdf[good],COL=2,STYLE=2,THICK=200
; HadAM3 Pentad (N144, 30 members)
good=where(faxis lt 1200)
GPLOT,X=faxis[good],Y=fpdf[good],COL=2,STYLE=1,THICK=200
; GPCP (1x1 degree, 11 years)
good=where(gaxis lt 1200)
GPLOT,X=gaxis[good],Y=gpdf[good],COL=1,STYLE=0,THICK=200
; HadGEM1a - companion to HiGEM (N96, 30 years)
good=where(eaxis lt 1200)
GPLOT,X=eaxis[good],Y=epdf[good],COL=3,STYLE=0,THICK=200
; HadGEM3-AO (N96, 20 years)
good=where(e3axis lt 1200)
GPLOT,X=e3axis[good],Y=e3pdf[good],COL=5,STYLE=0,THICK=200

labels=['GPCP (1x1 degree, 1997-2007)',$
        'HadGEM3-AO, ahsaf (N96, 20 years)',$
        'HadGEM1a, companion to HiGEM, xciel (N96, 30 years)',$
        'HadAM3 with daily OSTIA SSTs (N144, 30 ensemble members)',$
        'HadAM3 with pentad-mean OSTIA SSTs (N144, 30 ensemble members)',$
        'HadAM3 with monthly-mean OSTIA SSTs (N144, 30 ensemble members)']

colors=[1,3,5,2,2,2]
styles=[0,0,0,0,1,2]
LEGEND,LABELS=REVERSE(labels),COL=REVERSE(colors),STYLE=REVERSE(styles),legpos=9

PSCLOSE


STOP

END
