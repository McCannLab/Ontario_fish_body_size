#' Context map
#' 
#' @param bsm_master main dataset.
#' @param useElevation a logical. Should a raster of elevation be used as base 
#' map?
#'
#' @export
mapBSM <- function(bsm_master = readRDS("data/BSM_MASTER.rds"),
                   useElevation = FALSE) {
  # admin boundaries (will be downloaded the first time)
  can1 <- geodata::gadm("CAN", 1, path = "data/maps") |>
    sf::st_as_sf()
  usa1 <- geodata::gadm("USA", 1, path = "data/maps") |>
    sf::st_as_sf()
  # see https://www.sciencebase.gov/catalog/item/530f8a0ee4b0e7e46bd300dd
  greatLakes <- c(
    "hydro_p_LakeOntario/hydro_p_LakeOntario.shp",
    "hydro_p_LakeMichigan/hydro_p_LakeMichigan.shp",
    "hydro_p_LakeHuron/hydro_p_LakeHuron.shp",
    "hydro_p_LakeStClair/hydro_p_LakeStClair.shp",
    "hydro_p_LakeErie/hydro_p_LakeErie.shp",
    "hydro_p_LakeSuperior/hydro_p_LakeSuperior.shp"
  ) |>
    lapply(\(x) sf::st_read(file.path("data/maps", x))) |>
    do.call(what = rbind)

  watSheds3 <- readRDS("data/maps/watSheds3.rds") |>
    sf::st_as_sf()

  lakes <- bsm_master |>
    dplyr::ungroup() |>
    dplyr::select(Longitude, Latitude, WbyLID, Cycle) |>
    dplyr::distinct() |>
    na.omit() |>
    dplyr::group_by(WbyLID) |>
    dplyr::summarize(Longitude = mean(Longitude), Latitude = mean(Latitude), Cycle = sum(Cycle)) |>
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = sf::st_crs(4326))

  # ----- plot
  # colors
  blu1 <- "#87caec"
  gre1 <- "grey60"
  gre2 <- "grey70"
  gre3 <- "grey80"
  red1 <- "firebrick"
  yel1 <- "gold"
  blu2 <- "navy"
  png("fig/mapBSM.png", width = 10, height = 10, res = 300, units = "in")
  par(mar = c(3, 3, 3, 3))
  plot(
    can1[9, ] |> sf::st_geometry(),
    bgc = blu1,
    col = gre1
  )

  if (useElevation) {
    # see https://rdrr.io/cran/geodata/man/elevation.html
    alt <- readRDS("data/maps/altONT.rds") |> stars::st_as_stars()
    mypal <- colorRampPalette(c("#f0dec3", "#361f09"))(512)
    plot(alt, add = TRUE, pal = mypal)
  }
  plot(can1 |>
    dplyr::filter(grepl("^Qu|^Man", NAME_1)) |>
    sf::st_geometry(), add = TRUE, col = gre3)
  plot(usa1 |> sf::st_geometry(),
    add = TRUE, col = gre3, lwd = 0.8,
    border = gre1
  )
  plot(greatLakes |> sf::st_geometry(),
    col = blu1, add = TRUE, lwd = 0.8,
    border = gre1
  )
  plot(watSheds3 |> sf::st_geometry(), col = NA, border = gre2, add = TRUE)
  plot(
    can1[9, ] |> sf::st_geometry(),
    col = NA,
    add = TRUE,
    lwd = 1.5
  )
  plot(lakes |> dplyr::select(Cycle), add = TRUE, pch = 20, pal = c(red1, yel1, blu2))
  customAxis()
  box(bty = "o")
  legend("bottomleft", c("Cycle 1", "Cycle 2", "Cycle 1 and 2"),
    pch = 20, col = c(red1, yel1, blu2), cex = 1.4, bty = "n"
  )
  dev.off()
  return(invisible(TRUE))
}


customAxis <- function() {
  for (i in c(1, 3)) {
    axis(i,
      lwd = 0, lwd.ticks = 1,
      at = seq(-95, -75, 5), labels = paste0(rev(seq(75, 95, 5)), "\u00b0W")
    )
  }
  for (i in c(2, 4)) {
    axis(i,
      lwd = 0, lwd.ticks = 1,
      at = seq(45, 55, 5), labels = paste0(seq(45, 55, 5), "\u00b0N")
    )
  }
}
