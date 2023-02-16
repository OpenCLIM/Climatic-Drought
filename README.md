# Climatic-Drought README
### Ben Smith
### 26/10/2022

## SUMMARY

SPEI analysis of the 12 UKCP18 Regional Climate Projections. All analysis is based on the SPEI R package.
SPEI statistics are calculated for 3, 6 and 12 month periods and then summarised over decades and warming periods (i.e. where temperatures exceed warming thresholds).

## NOTES

Calculations/outputs are not bias controlled.

Outputs and metrics can be used to investigate future droughts according to the UKCP18 climate data - 'climatic drought' naming convention referes to the input (climate) data, not the dought application (i.e. hydrological/meteorological/agricultural/ecconomic etc.).

All calculations and scripts for creating metrics are available here: 
https://github.com/OpenCLIM/Climatic-Drought


Calculations are available for SPEI 3, 6, & 12. These are similar to, but not the same, as the 3/6/12 month moving average of total monthly precipitation minus total monthly potential evaporation.

Within each folder (SPEI_3/6/12) the datasets are seperated into three gridded metrics:
- Maximum length of drought: the maximum drought length within the period (months).
- Average length of drought: the average length of the droughts within the period (months).
- Probabilty of drought: the probability that a month is a drought for the period. This is static across the whole period. Calculated as the count of drought months / total number of months in period.

Folder naming convention: 
- Metric + format + RCP number
    - Format ended up always being gridded, i.e. ascii.

File naming convention: 
- metric + _ + format + _ + RCP + _ + calculation period
- e.g. drought_max_length + raster + 15 + 2006-2036
- All dates are inclusive (i.e. 01/01/2006 - 30/12/2036)

Metrics are calculated per:
- Decade: e.g. drought_mean_length_raster_05_19801201-19901130.asc
- 30 year period: e.g. drought_mean_length_raster_05_19901201-20201130.asc
- Warming period: e.g. drought_mean_length_raster_05_2007-2037.asc
    - These change between each RCP
    - Warming periods are 1.5, 2.0, 2.5, 3.0, 3.5, and 4.0 degrees.
    - Warming periods are 30 year periods centered across the year that the warming level is reached.

The start dates of each warming period are listed below. *s Indicate periods that are shorter due to no data after 2080. Probabilities account for this.
    
|RCM | 1.5  | 2.0  | 2.5  | 3.0  | 3.5  | 4.0 |
| 01 | 2006 | 2016 | 2026 | 2034 | 2042 |2049 |
| 04 | 2003 | 2013 | 2023 | 2031 | 2039 |2046 |
| 05 | 2007 | 2018 | 2028 | 2037 | 2044 |2051* |
| 06 | 2005 | 2016 | 2025 | 2034 | 2042 |2049 |
| 07 | 2005 | 2017 | 2027 | 2036 | 2043 |2050 |
| 08 | 2006 | 2018 | 2029 | 2038 | 2047 |2055* |
| 09 | 2004 | 2014 | 2023 | 2030 | 2037 |2044 |
| 10 | 2008 | 2018 | 2027 | 2036 | 2045 |2052* |
| 11 | 2004 | 2015 | 2025 | 2034 | 2042 |2050 |
| 12 | 2010 | 2020 | 2030 | 2038 | 2045 |2052* |
| 13 | 2005 | 2016 | 2026 | 2035 | 2043 |2050 |
| 15 | 2006 | 2019 | 2030 | 2038 | 2046 |2054* |

SS folder is superseeded work and is for reference only - unlikely to be useful / correct.

