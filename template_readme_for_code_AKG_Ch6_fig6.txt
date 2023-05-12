##########################################################################
# ---------------------------------------------------------------------------------------------------------------------
# These are two IDL (Interactive Data Language) codes to produce IPCC AR6 WGI Figure 6.6a (*map_v5.pro) and 6.6b (*timeseries_v5.pro) 
# Creator: Dr. Aristeidis K. Georgoulias, Dept. of Meteorology and Climatology, AUTH, Thessaloniki, Greece 
# Contact: ageor@auth.gr
# Last updated on: April 4th, 2021
# --------------------------------------------------------------------------------------------------------------------
#
# - Code functionality: *map_v5.pro reads the GOME_SCIA_GOME2ab_TroposNO2_v2.3_041996-092017_correctedx3_timmean.nc file and plots trop. NO2 patterns (1996-2017). *timeseries_v5.pro reads the IPCC_TEMIS_NO2_trends.dat file and plots the annual trop. NO2 values relative to 2016 for various subregions
# - Input data: GOME_SCIA_GOME2ab_TroposNO2_v2.3_041996-092017_correctedx3_timmean.nc and IPCC_TEMIS_NO2_trends.dat. The original files vailable on http://www.temis.nl (https://d1qb6yzwaaq4he.cloudfront.net/airpollution/no2col/GOME_SCIAMACHY_GOME2ab_TroposNO2_v2.3_041996-092017_temis.nc)
# - Output variables: map_v5.pro plots Fig. 6.6a and *timeseries_v5.pro plots Fig. 6.6b. The two figures can then be merged with ImageMagick. 
#
# ----------------------------------------------------------------------------------------------------
# Information on  the software used
# - Software Version: IDL8.4
# - Landing page to access the software: - 
# - Operating System: Windows10
# - Environment required to compile and run: Windows but will also run on unix/linux machines
#  ----------------------------------------------------------------------------------------------------
#
# License: Apache 2.0
# ----------------------------------------------------------------------------------------------------
# How to cite: Georgoulias A.K., 2021, IDL code for Fig. 6.6a (Fig. 6.6b) in IPCC AR6 WGI 
# When citing this code, please include both the code citation and the following citation for the related report component: Georgoulias, A. K., van der A, R. J., Stammes, P., Boersma, K. F., and Eskes, H. J.: Trends and trend reversal detection in 2 decades of tropospheric NO2 satellite observations, Atmos. Chem. Phys. 19, 6269-6294, doi:10.5194/acp-19-6269-2019, 2019.
##########################################################################