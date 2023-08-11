# Ontario_fish_body_size

> Thermal tolerance and habitat preferences mediate how freshwater fish body sizes respond to warming

Species included:

|Species         | SpeciesCode|
|:---------------|-----------:|
|Lake Whitefish  |          91|
|Lake Trout      |          81|
|Walleye         |         334|
|Yellow Perch    |         331|
|Smallmouth Bass |         316|


## Installation

[R (>= 4.2)](https://www.r-project.org/) is required to install the package and re-run the analyses. Clone this repository, then set the cloned repository as your working directory, then run:

```R
install.packages("devtools")
remotes::install_deps()
```

This should install all required dependencies. Then, in order to use the functions in `/R`, you need to load the package:

```R
devtools::load_all()
```

Alternatively, the package can be directly installed from GitHub and then be loaded as any other package: 

```R
# install
install.packages("remotes")
remotes::install_github("McCannLab/Ontario_fish_body_size")
# load
library(ffbs2w)
```

## Reproducing the analysis 

⚠️ We are not allowed to share the data for this project, we nonetheless provide a mock data set to demonstrate how the code can be used. 


```R 
# 1. prepare data 
## !!NB!!: the data to reproduce this step (2 csv files) are not available
formatData()
# 2. fit Von Bertalanffy curves with `nlsr`
## Results are exported in output (created if needed)
fitVBGF(bsm_master = BSM_MASTER_MOCK)
# 3. INLA models (requires step 2)
## Analysis 
INLA::inla.setOption(num.threads = "1:1") # adjust as desired
res <- fitInla(nMinIndiv = 15, bsm_lake_clim = BSM_LAKE_CLIM_MOCK)
## Main figure (file written in `.fig/`)
allExfPlot(res)
# 4. context map
## !!NB!!: data to reproduce this map are not available, but pointers to file 
## available are provided in the code 
mapBSM()
# --- Supp Age distribution for Small Mouth Bass
## (file written in `.fig/`)
figSup(bsm_master = BSM_MASTER_MOCK)
```

The analysis was run in the following environment: 

```R
─ Session info ─────────────────────────────────────────────────────────────────
 setting  value
 version  R version 4.3.1 (2023-06-16)
 os       Debian GNU/Linux trixie/sid
 system   x86_64, linux-gnu
 ui       X11
 language en_CA:en
 collate  en_CA.UTF-8
 ctype    en_CA.UTF-8
 tz       America/Toronto
 date     2023-08-14
 pandoc   2.17.1.1 @ /usr/bin/pandoc
```
