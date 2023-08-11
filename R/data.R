#' Mock main dataset
#'
#' Mock dataset designed to reproduce the analysis without sharing original
#' data (we are not allowed to distribute the data).
#'
#' @docType data
#' @name BSM_MASTER_MOCK
#' @usage BSM_MASTER_MOCK
#' @format
#' A data frame with 130,776 rows and 27 columns containing fake data.
#' @references
#' <https://www.ontario.ca/page/broad-scale-fish-community-monitoring>
#'
#' @details
#' Each row is a data entry for one fish. The table includes data for lakes:
#' * Cycle: BSM cycle during which the lake was sample
#' * FMZ: Fisheries management zones
#' * Wby_LID_Year: Lake identifiers (dummy ones)
#' * Wby_LID_Year: Lake identifiers (serial number to ease some data operations)
#' * Latitude, Longitude
#' * Mean.Depth, Max.Depth: mean and maximum depth of the lake
#' * Depth_Stratum
#' * Secchi.Depth
#' * pH
#' * Conductivity
#' * Total.Phosphorus
#' * Area_ha: lake area
#' * pLittoral, SDF: proportion of littoral, Shoreline development factor
#' * WC_TDS: Total dissolved solids
#'
#' as well as fish data:
#' * Species, SpeciesCode: Species name and code
#' * FLEN: Fork length
#' * RWT: Round Weight
#' * Sex: 1 = male, 2 = female, and 3 (or 4) = unknown
#' * Age, AgeConfidence: Species Age and its confidence (Age +/- AgeConfidence)
#' * AgingStructure: structure used to determine the age
"BSM_MASTER_MOCK"


#' Mock lake-climate dataset
#'
#' Fake climate values for lakes included in the studies.
#'
#' @docType data
#' @name BSM_LAKE_CLIM_MOCK
#' @usage BSM_LAKE_CLIM_MOCK
#' @format
#' A data frame with 1000 rows (lakes) and 27 columns containing fake data.
#' @details
#' Each row is a data entry for one lake. The following abbreviations are used 
#' in column names:
#' * temp: temperature
#' * precip: precipitation
#' * GDD: growing degree days
#' * GS: growing season
#' The values at the end of the variable names (10, 15, 20 and 25) correspond
#' to the length of the temporal window (in years prior to sampling) used to
#' compute mean values. Finally `dist_highway` is the shortest distance to a
#' highway and `angling_intensity_SUM` is the angling pressure.
"BSM_LAKE_CLIM_MOCK"