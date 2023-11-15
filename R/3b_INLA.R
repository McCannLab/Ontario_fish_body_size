#' fit INLA models
#'
#' @param nMinIndiv minimal number of individuals.
#' @param bsm_lake_clim BSM lake-climate dataset (see `?BSM_LAKE_CLIM_MOCK` for
#' further details).
#' @param ... Further argument passed to `[INLA::inla()]`.
#'
#' @export
fitInla <- function(nMinIndiv = 15,
                    bsm_lake_clim = readRDS("data/bsm_lake_clim.rds"), ...) {
  spc <- c(316, 331, 334, 91, 81)
  allData <- prepareData(nMinIndiv = nMinIndiv, bsm_lake_clim = bsm_lake_clim)
  # species
  spcTbl <- allData |>
    dplyr::select(SpeciesCode, Species) |>
    dplyr::distinct() |>
    dplyr::filter(SpeciesCode %in% spc)
  lsMod <- list()
  for (i in seq(spc)) {
    cli::cli_progress_step(
      paste("Running INLA models for", spc[i], "with min indiv =", nMinIndiv)
    )
    TestHosts <- allData |>
      dplyr::filter(SpeciesCode == spc[i])
    lsMod[[i]] <- fitData(TestHosts, species_code = spc[i], ...)
  }
  names(lsMod) <- spc
  cli::cli_progress_done()
  return(lsMod)
}

prepareData <- function(nMinIndiv = 15, bsm_lake_clim) {
  bsm_lake_clim <- bsm_lake_clim |>
    dplyr::distinct()
  # step 2 required
  fl <- paste0("output/sp_all_nlxb_", nMinIndiv, ".rds")
  if (!file.exists(fl)) {
    stop("Data from step 2 are required, see ?fitVBGF")
  }
  df <- readRDS(fl) |>
    dplyr::inner_join(bsm_lake_clim, by = "Wby_LID_Year") |>
    # filtering out NAs
    na.omit()
  names(df) <- transfoNames(names(df))
  # filtering unrealistic values for GDD and lake area:
  df <- df |>
    dplyr::filter(GDD5.mean.GS15 > 0) |>
    dplyr::filter(Area.ha <= 10000) |>
    # Turning variables into factors
    dplyr::mutate(dplyr::across(c(FMZ, Cycle, Species), as.factor)) |>
    # Standardizing covars:
    dplyr::mutate(dplyr::across(dplyr::starts_with("temp"), scale)) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("prec"), scale)) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("GDD"), scale)) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("Days_"), scale)) |>
    dplyr::mutate(dplyr::across(
      c(Mean.Depth, Max.Depth, Secchi.Depth, Conductivity, WC.TDS, SDF),
      scale
    ))
  df$pH <- scale(df$PH)
  df$Area <- scale(df$Area.ha)
  df$Prop.Littoral <- scale(df$PLittoral)
  df$Total.Phosphorus <- scale(df$Total.Phosphorus)
  df$Dist.Nrst.Hwy <- scale(df$Dist.highway)
  df$Angling.Pressure <- scale(df$Angling.intensity.SUM)
  return(df)
}

transfoNames <- function(x) {
  gsub("_", ".", x) |>
    strsplit("") |>
    lapply(\(y) paste0(
      toupper(y[1]),
      paste0(y[-1], collapse = ""),
      collapse = ""
    )) |>
    unlist()
}

fitData <- function(TestHosts, species_code = 81, ...) {
  phen <- c("FMZ", "Longitude", "Latitude", "Cycle", "Year", "Waterbody_LID_Year") # Base columns with spatial information we'll need
  resp <- "Linf.nlxb" # Response variable
  Finalcovar <- c(
    "Mean.Depth",
    "Max.Depth",
    "Secchi.Depth",
    "pH",
    "Conductivity",
    "Area",
    "Prop.Littoral",
    "Total.Phosphorus",
    "WC.TDS",
    "SDF",
    "Temp.mean20",
    "Temp.mean.GS20",
    "GDD5.mean.GS20",
    "Days.GS.mean20",
    "Precip.mean20",
    "Precip.mean.GS20",
    "Dist.Nrst.Hwy",
    "Angling.Pressure"
  )
  lm_vifcheck <- lm(paste0(resp, " ~ ", paste(Finalcovar, collapse = " + ")),
    data = TestHosts
  )
  car::vif(lm_vifcheck)
  Finalcovar <- c(
    "Total.Phosphorus",
    "Max.Depth",
    "Secchi.Depth",
    "Prop.Littoral",
    "Area",
    "GDD5.mean.GS15",
    "Precip.mean.GS15",
    "Angling.Pressure"
  )
  if (species_code == 81) {
      Finalcovar <- c("Angling.Pressure", "Area", "GDD5.mean.GS15", "Precip.mean.GS15")
  } else if (species_code == 91) {
    Finalcovar <- c("Angling.Pressure", "Area", "GDD5.mean.GS15", "Max.Depth")
  }
  cli::cli_alert_info("Final variable set: {Finalcovar}")
  lm_vifcheck2 <- lm(paste0(resp, " ~ ", paste(Finalcovar, collapse = " + ")), data = TestHosts)
  car::vif(lm_vifcheck2)

  Locations <- cbind(TestHosts$Longitude, TestHosts$Latitude) # using the sampling locations
  ConvHull <- INLA::inla.nonconvex.hull(Locations)
  Mesh <- INLA::inla.mesh.2d(boundary = ConvHull, Locations, max.edge = c(2.5, 5))
  # control plot
  # plot(Mesh)
  # points(Locations, col = "red", pch = 2)

  # setting priors:
  sigma0 <- 1
  size <- min(c(diff(range(Mesh$loc[, 1])), diff(range(Mesh$loc[, 2]))))
  range0 <- size / 15
  kappa0 <- sqrt(8) / range0
  tau0 <- 1 / (sqrt(4 * pi) * kappa0 * sigma0)

  HostsA <- INLA::inla.spde.make.A(Mesh, loc = Locations) # Making A matrix

  # Hosts.spde = inla.spde2.matern(mesh = Mesh, alpha=2) # Making SPDE
  Hosts.spde <- INLA::inla.spde2.matern(
    mesh = Mesh,
    B.tau = cbind(log(tau0), -1, +1),
    B.kappa = cbind(log(kappa0), 0, -1),
    theta.prior.mean = c(0, 0),
    theta.prior.prec = c(0.1, 1)
  )
  # making the w
  w.Host <- INLA::inla.spde.make.index("w", n.spde = Hosts.spde$n.spde)

  # Making the model matrix ####
  X <- paste0(" ~ -1 + ", paste(Finalcovar, collapse = " + ")) |>
    as.formula() |>
    # make the model matrix using the final model selection formula without a response variable.
    model.matrix(data = TestHosts) |>
    as.data.frame() # convert to a data frame. Eliminate the base level of the first categorical variable if applicable (you will manually specify an intercept below)
  # head(X)
  # change name of interaction term column - cannot have colons in it:
  colnames(X) <- gsub(":", "_", colnames(X))

  # Making the stack ####
  N <- nrow(TestHosts)
  StackHost <- INLA::inla.stack(
    data = list(y = TestHosts[, resp]), # specify the response variable
    A = list(1, 1, 1, HostsA), # Vector of Multiplication factors for random and fixed effects
    effects = list(
      Intercept = rep(1, N), # specify the manual intercept!
      X = X, # attach the model matrix
      FMZ = TestHosts$FMZ, # insert vectors of any random effects
      w = w.Host
    )
  ) # attach the w

  # need to record the current environment for formula
  curenv <- environment()
  f1 <- paste0("y ~ -1 + Intercept + ", paste0(colnames(X), collapse = " + "))
  ls_formula <- list(
    f1 = f1,
    f2 = paste0(f1, " + f(FMZ, model = 'iid')"), # f2 + SPDE random effect
    f3 = paste0(f1, " + f(FMZ, model = 'iid') + f(w, model = Hosts.spde)"),
    f4 = paste0(f1, " + f(w, model = Hosts.spde)") # SPDE random effect
  ) |>
    lapply(as.formula, env = curenv)

  ls_mod <- lapply(
    ls_formula,
    \(x) INLA::inla(x, # Base model (no random effects)
      family = "gaussian",
      data = INLA::inla.stack.data(StackHost),
      control.compute = list(dic = TRUE),
      control.predictor = list(A = INLA::inla.stack.A(StackHost)),
      control.inla = list(cmin = 0, strategy = "Laplace"),
      num.threads = "1:1",
      ...
    )
  )

  return(list(formula = ls_formula, models = ls_mod))
}

#' @describeIn fitInla Generates ExfPlot plots.
#' @param lsMod List of INLA model.
#' @export
allExfPlot <- function(lsMod, nMinIndiv = 15) {
  # creating species-specific exfplots - these will show the various
  # (1-4) model output for the one species that you have just run the INLA for:
  resFigDic <- resDic <- resFigExf <- list()
  for (i in seq(lsMod)) {
    tmp <- lsMod[[i]]$models
    resFigExf[[i]] <- Efxplot(tmp, Intercept = FALSE, Size = 3, tips = 0.3)
    resDic[[i]] <- lapply(tmp, function(f) f$dic$dic) |> unlist()
    resFigDic[[i]] <- INLADICFig(
      tmp,
      ModelNames = c("Base", "FMZ, Cycle", "FMZ, Cycle, SPDE", "SPDE")
    )
  }
  # resDic
  pltExf <- resFigExf[[1]] + resFigExf[[2]] + resFigExf[[3]] + resFigExf[[4]]
  png("fig/pltExf.png", width = 18, height = 20, units = "in", res = 300)
  pltExf
  dev.off()
  ##
  pltDic <- resFigDic[[1]] + resFigDic[[2]] + resFigDic[[3]] + resFigDic[[4]]
  png("fig/pltDic.png", width = 18, height = 20, units = "in", res = 300)
  pltDic
  dev.off()
  # pltExf
  #
  # DIC for the various models:
  # pltDic
  lowestDIC <- resDic |>
    lapply(which.min) |>
    unlist()
  lsFinalMod <- list()
  for (i in seq(lsMod)) {
    lsFinalMod[[i]] <- lsMod[[i]]$models[[lowestDIC[i]]]
  }
  # making the efxplot using the best model (lowest DIC) for each species:
  y <- Efxplot(lsFinalMod,
    StarLoc = NULL,
    Intercept = FALSE, Size = 1.6, tips = 0.5, lwd = 0.5,
    ModelNames = c("Smallmouth Bass", "Yellow Perch", "Walleye", "Lake Whitefish", "Lake Trout")
  )

  y + theme_classic(base_size = 12)
  flo <- paste0(
    "Linf_inla_output_",
    nMinIndiv,
    "spl_15yrs_mesh2.5_5_all5species_CJFAS.png"
  )
  if (!dir.exists("fig")) dir.create("fig")
  ggsave(file.path("fig", flo),
    width = 8, height = 5, units = "in",
    dpi = 500
  )

  list(models = lsFinalMod, dic = resDic)
}
