#' Fit Von Bertalanffy model for all lakes
#'
#' @param bsm_master main dataset.
#' @param nMinIndiv a vector of integers providing the minimal number
#' individuals required for a lake to be included in the analysis.
#'
#' Three parameters are estimated:
#' * Linf: asymptotic size,
#' * k: growth coefficient,
#' * t0: age when size is 0.
#'
#' @references
#' <https://en.wikipedia.org/wiki/Von_Bertalanffy_function>
#'
#' @export
fitVBGF <- function(
    bsm_master = readRDS("data/BSM_MASTER.rds"),
    nMinIndiv = 15) {
  # start = starting priors for VBGF - varied Linf starting value for each species
  # unreal = unrealistic Linf values in mm (to be removed from the dataframe)
  ls_spc_val <- list(
    SMB = list(start = c(300, 0.15, 0), unreal = 600, code = 316),
    YP = list(start = c(200, 0.15, 0), unreal = 400, code = 331),
    WALL = list(start = c(400, 0.15, 0), unreal = 900, code = 334),
    LWF = list(start = c(500, 0.15, 0), unreal = 1000, code = 91),
    LT = list(start = c(500, 0.15, 0), unreal = 1000, code = 81)
  )
  spCodes <- ls_spc_val |>
    lapply(\(x) x$code) |>
    unlist()
  dat0 <- bsm_master |>
    dplyr::filter(SpeciesCode %in% spCodes)
  # loop over # individual thresholds
  for (j in nMinIndiv) {
    ls_out <- list()
    for (i in seq_along(ls_spc_val)) {
      dat <- dat0 |>
        dplyr::filter(SpeciesCode == ls_spc_val[[i]]$code) |>
        na.omit() |>
        dplyr::filter(Age < 100)
      # hist(dat$Age, breaks = 100)
      nIndivPerLake <- dat |>
        dplyr::group_by(lake_serial) |>
        dplyr::summarise(count = dplyr::n()) |>
        dplyr::filter(count > j)
      spc <- names(ls_spc_val)[i]
      cli::cli_progress_step(
        paste("Running Von Bertalanffy models for", spc, "with min indiv =", j)
      )
      ls_out[[i]] <- getLinfKappa(
        spc,
        ls_spc_val[[i]],
        dat |> dplyr::filter(lake_serial %in% nIndivPerLake$lake_serial)
      )
      cli::cli_progress_done()
    }
    # once the nlxb function (VBGF) has been run for all 5 species,
    # combine those data frames and output as an RDS - this was done for
    # multiple specimens/lakes for a supplemental analysis (15 specimens
    # per lake is the threshold we present in our study)
    if (!dir.exists("output")) dir.create("output")
    fl <- file.path("output", paste0("sp_all_nlxb_", j, ".rds"))
    cli::cli_progress_step("Generating {fl}")
    out <- do.call(rbind, ls_out) |>
      # add Wby_LID_Year and environmental variables
      dplyr::left_join(
        bsm_master |>
          dplyr::select(
            "lake_serial", "Wby_LID_Year", "Longitude", "Latitude", "Cycle",
            "FMZ", "Mean.Depth", "Max.Depth", "Secchi.Depth", "pH", "SDF", "Conductivity", "Total.Phosphorus", "Area_ha", "pLittoral", "WC_TDS", "WC_TP"
          ) |>
          dplyr::distinct(),
        by = dplyr::join_by("lake_serial")
      ) |>
      dplyr::left_join(
        bsm_master |>
          dplyr::ungroup() |>
          dplyr::select("SpeciesCode", "Species") |>
          dplyr::distinct(),
        by = dplyr::join_by("SpeciesCode")
      )
    saveRDS(out, file = fl)
    cli::cli_progress_done()
  }
  invisible(out)
}
# all steps to fit VBGF for one species
getLinfKappa <- function(spc, ls_val, dat) {
  start_val <- ls_val$start
  start <- c(Linf = start_val[1], k = start_val[2], t0 = start_val[3])
  vecLake <- unique(dat$lake_serial)
  x <- mod <- list()
  msg <- paste("NLS models for", spc, paste0("(", ls_val$code, ")"))
  cli::cli_progress_bar(msg, total = length(vecLake))
  for (i in seq(vecLake)) {
    # create subsets of the data for only a specific proj (i)
    x[[i]] <- dat |> dplyr::filter(lake_serial == vecLake[i])
    #
    mod[[i]] <- nlsr::nlxb(FLEN ~ Linf * (1 - exp(-k * (Age - t0))), data = x[[i]], start = start)
    cli::cli_progress_update()
  }
  cli::cli_progress_done()
  # [row,column] extract values from nls equations:
  nlxbCoef <- lapply(mod, coefficients) |>
    do.call(what = rbind) |>
    as.data.frame() |>
    dplyr::rename("Linf_nlxb" = "Linf", "Kappa_nlxb" = "k", "t0_nlxb" = "t0")
  out <- cbind(nlxbCoef, lake_serial = vecLake, SpeciesCode = ls_val$code) |>
    dplyr::filter(Linf_nlxb < ls_val$unreal)
  # this includes the results that should be merged in a second step.

  return(out)
}
