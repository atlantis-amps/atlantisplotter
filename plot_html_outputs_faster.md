---
title: "Atlantis Output Plots"
output:
  html_document:
    toc: yes
    toc_float: yes
    code_folding: hide
  pdf_document:
    toc: yes
---

```r
set_options()
```

```
## Error in set_options(): could not find function "set_options"
```

```r
import::from(dplyr, "mutate", "select","filter")

thisfolder <- "outputFolder"
thisbgmfile <- "PugetSound_89b_NAD83.bgm"
thesecountries <- c('Canada','United States') # Natural Earth regions to use as background
outdietfile <- "AMPS_OUTDietCheck.txt"
#functional groups csv files should have colnames Code IsTurnedOn Name `Long Name` GroupType
groups_csv <- "PugetSoundAtlantisFunctionalGroups_salmon_rectype4_bf.csv"
thisoutncfile <- "AMPS_OUT.nc"
startyear <- 2011 #model start year, used for diets
timeperiod <- 73 #output frequency, used for diets
#code assumes that non-dynamic polygons are continuous
ini.pol <- 61 #initial non-dynamic polygon 
end.pol <- 89 #final non-dynamic polygon
```


















