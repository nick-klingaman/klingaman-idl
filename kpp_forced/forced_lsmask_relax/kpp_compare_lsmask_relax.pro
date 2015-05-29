PRO kpp_compare_lsmask_relax_sst

; Compare surface temperatures in the forced KPP runs with and without
; relaxation to OSTIA monthly mean SSTs for 2005.

norelax_output='/home/ss901165/kpp_ocean/forced_lsmask_test/KPPocean.T.nc'
relax_output='/home/ss901165/kpp_ocean/forced_lsmask_relax/KPPocean.T.nc'
reference_sst='/home/ss901165/datasets/GHRSST/FOR_KPP/May-Sep_2005_n144_for_kpp_mmean.nc'

points=[[21.666,5.],[143.75,100.]]

latitudes = OPEN_AND_EXTRACT(norelax_output,'latitude')
longitudes = OPEN_AND_EXTRACT(norelax_output,'longitude')

STOP

norelax_sst = OPEN_AND_EXTRACT(norelax_output,'T')
relax_sst = OPEN_AND_EXTRACT(relax_output,'T')
reference_sst = OPEN_AND_EXTRACT(reference_sst,'SST')

set_plot,'ps'
device,file='/home/ss901165/kpp_ocean/forced_lsmask_relax/kpp_compare_lsmask_relax.ps',bits_per_pixel=24,color=1,set_font='Hershey'

STOP

END
