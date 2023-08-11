#' Supplementary figure
#'
#' @param bsm_master main dataset.
#'
#' @export
figSup <- function(bsm_master = readRDS("data/BSM_MASTER.rds")) {

  # for supplementary
  spe_perc  <- bsm_master |>
    dplyr::group_by(Species) |>
    dplyr::summarise(Percent = mean(Age <= 15, na.rm = TRUE))

  dat <- bsm_master |>
    na.omit(dat) |>
    dplyr::filter(SpeciesCode == 316) |>
    dplyr::filter(Age < 100)
  # hist(dat$Age, breaks = 100)
  # create age histogram plots:
  p <- dat |> ggplot(aes(x = Age)) +
    geom_histogram(aes(y = after_stat(density)),
      colour = "black",
      fill = "white"
    ) +
    xlim(0, 25) +
    geom_density(alpha = .2, fill = "#FF6666")
  #
  p + theme_classic(base_size = 50)
  ggsave("fig/SMB_AgePlot_CJFAS.png",
    width = 40, height = 20, units = "in",
    dpi = 400
  )

  invisible(spe_perc)
}
