#' Prepare data
#'
#' @export
# Step 1 -- read and format data
formatData <- function() {
  cli::cli_progress_step("Reading and formating data")
  on.exit(cli::cli_progress_done())
  GDD_lakes <- read.csv(
    "data/bsm_clim_all_yrs_hwy_ap.csv",
    stringsAsFactors = FALSE
  ) |> dplyr::select(-X)
  saveRDS(GDD_lakes, file = "data/bsm_lake_clim.rds")
  # BSM DATA
  BSM1 <- read.csv("data/bsm_cy1_aged.csv", header = TRUE, sep = ",", comment.char = "", fill = TRUE)
  colnames(BSM1) <- c("Cycle", "FMZ", "WbyName", "WbyLID", "WbyLID_YR", "GearType", "Depth_Stratum", "Netting_Start_Month", "Netting_Start_Day", "Netting_Start_Year", "Netting_End_Month", "Netting_End_Day", "Netting_End_Year", "Species Code", "Species", "FLEN", "RWT", "Sex", "Aging_Structure", "Age", "Age confidence")
  ## read cycle 2
  BSM2 <- read.csv("data/bsm_cy2_aged.csv", header = TRUE, sep = ",", comment.char = "", fill = TRUE)
  colnames(BSM2) <- c("Cycle", "FMZ", "WbyName", "WbyLID", "WbyLID_YR", "Target Species", "Lake Selection", "Netting_Start_Month", "Netting_Start_Day", "Netting_Start_Year", "Netting_End_Month", "Netting_End_Day", "Netting_End_Year", "GearType", "Depth_Stratum", "Species Code", "Species", "FLEN", "RWT", "Sex", "Aging_Structure", "Age", "Age confidence")
  BSM2_vars <- BSM2[c(1:5, 14, 15, 8:13, 16:23)]
  BSM_aged <- rbind(BSM1, BSM2_vars)
  BSM_aged$Wby_LID_Year <- paste(BSM_aged$WbyLID, BSM_aged$Netting_End_Year, sep = "_")
  
  # Lakes data
  ## -- Cycle 1
  cnm <- c("Cycle", "FMZ", "Wby_LID_Year", "Latitude", "Longitude", "Mean.Depth", "Max.Depth", "Secchi.Depth", "pH", "Conductivity", "Total.Phosphorus", "Area_ha", "SDF", "pLittoral", "WC_TDS", "WC_TP")
  BSM1_Lakes <- read.csv(
    "data/bsm_cy1_lake_data.csv",
    header = TRUE, sep = ","
  ) |>
    dplyr::select(c(1:2, 8:10, 13, 12, 21, 29, 30, 45, 11, 14, 27, 23, 24))
  colnames(BSM1_Lakes) <- cnm
  ## -- Cycle 2
  BSM2_Lakes <- read.csv("data/bsm_cy2_lake_data.csv",
                         header = TRUE,
                         sep = ","
  ) |>
    dplyr::select(c(1:2, 5, 8, 9, 13, 14, 16:19, 23, 25:28))
  colnames(BSM2_Lakes) <- cnm
  ## -- Combining lakes data
  BSM_Lakes <- rbind(BSM1_Lakes, BSM2_Lakes)
  BSM_MASTER <- BSM_Lakes |>
    dplyr::left_join(BSM_aged, by = "Wby_LID_Year") |>
    dplyr::select(c(1:16, 19, 20, 22:37))
  colnames(BSM_MASTER) <- c(cnm, "WbyName", "WbyLID", "Gear", "Depth_Stratum", "NetStart_Month", "NetStart_Day", "NetStart_Year", "NetEnd_Month", "NetEnd_Day", "NetEnd_Year", "SpeciesCode", "Species", "FLEN", "RWT", "Sex", "AgingStructure", "Age", "AgeConfidence")
  
  ## Master data set
  BSM_MASTER <- BSM_MASTER |>
    dplyr::group_by(Wby_LID_Year) |>
    dplyr::filter(Area_ha <= 10000) |>
    # there was one 'Lake  Whitefish' ()double blank space
    dplyr::mutate(Species = gsub(" +", " ", Species) |> trimws()) |>
    dplyr::left_join(
      BSM_MASTER |>
        dplyr::group_by(Wby_LID_Year) |>
        dplyr::summarise() |>
        dplyr::ungroup() |>
        # create unique value per lake from 0 to the number of unique
        # combination - 1
        dplyr::mutate(lake_serial = dplyr::row_number() - 1)
    )
  # write the main data set
  saveRDS(BSM_MASTER, file = "data/BSM_MASTER.rds")
  invisible(TRUE)
}
