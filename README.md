# Ontario_fish_body_size

> Thermal tolerance and habitat preferences mediate how freshwater fish body sizes respond to warming


## Installation

[R (>= 4.2)](https://www.r-project.org/) is required to install the package and re-run the analyses. Clone this repository, then set the cloned repository as your working directory, then run:

```R
install.packages("devtools")
remotes::install_deps()
```

This should install all required dependencies. Then, use the functions in `/R`:


## Reproducing the analysis 

⚠️ We are not allowed to share the data for this project, we nonetheless provide a mock data set to demonstrate how the code can be used. 


```R 
devtools::load_all()
# 1. prepare data 
## NB: the data to reproduce this step (2 csv files) are not available
formatData()
# 2. fit Von Bertalanffy curves with `nlsr`
## Results are exported in output
fitVBGF(bsm_master = readRDS("data/BSM_MASTER_mock.rds"))
# 3. INLA models (requires step 2)
res <- fitInla(nMinIndiv = 15, useMock = TRUE)
allExfPlot(res)
# 4. context map
## data to reproduce this map are not available, but pointers to file 
## available are provided in the code
mapBSM()
# --- Supp Age distribution for Small Mouth Bass
figSup(bsm_master = readRDS("data/BSM_MASTER_mock.rds"))
```
