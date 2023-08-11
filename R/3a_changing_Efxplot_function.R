#' Plot function
#'
#' Adapted from ggregplot (Model Effects Plot.R)
#'
#' @import ggplot2 patchwork
#'
#' @references
#' * <https://rdrr.io/github/gfalbery/ggregplot/src/R/Model%20Effects%20Plot.R>
#' * https://rdrr.io/github/gfalbery/ggregplot/src/R/INLA%20DIC%20Plot.R

Efxplot <- function(ModelList,
                    Sig = TRUE, StarLoc = NULL,
                    Alpha1 = 1, Alpha2 = 1,
                    PointOutline = FALSE,
                    ModelNames = NULL,
                    VarNames = NULL, VarOrder = NULL,
                    Intercept = TRUE, Size = 1,
                    tips = 0.2, lwd = 0.1, pdw = 0.6) {
  Graphlist <- list()

  if (!inherits(ModelList, "list")) {
    ModelList <- list(ModelList)
  }

  for (i in seq(ModelList)) {
    model <- ModelList[[i]]

    if (inherits(model, "inla")) {
      Graph <- as.data.frame(summary(model)$fixed)
      colnames(Graph)[which(colnames(Graph) %in% c("0.025quant", "0.975quant"))] <- c("Lower", "Upper")
      colnames(Graph)[which(colnames(Graph) %in% c("0.05quant", "0.95quant"))] <- c("Lower", "Upper")
      colnames(Graph)[which(colnames(Graph) %in% c("mean"))] <- c("Estimate")
    }

    Graph$Model <- i
    Graph$Factor <- rownames(Graph)
    Graphlist[[i]] <- Graph
  }

  Graph <- dplyr::bind_rows(Graphlist)
  Graph$Sig <- with(Graph, ifelse(Lower * Upper > 0, "*", ""))
  Graph$Model <- as.factor(Graph$Model)

  if (!is.null(ModelNames)) {
    levels(Graph$Model) <- ModelNames
  }

  position <- ifelse(length(unique(Graph$Model)) == 1, "none", "right")

  if (is.null(VarOrder)) VarOrder <- rev(unique(Graph$Factor))
  if (is.null(VarNames)) VarNames <- VarOrder

  Graph$Factor <- factor(Graph$Factor, levels = VarOrder)
  levels(Graph$Factor) <- VarNames

  Graph |>
    as.data.frame() |>
    dplyr::filter(!is.na(Factor)) -> Graph

  if (!Intercept) {
    VarNames <- VarNames[!stringr::str_detect(VarNames, "ntercept")]
    Graph <- Graph |> dplyr::filter(Factor %in% VarNames)
  }

  Graph$starloc <- NA

  min <- min(Graph$Lower, na.rm = TRUE)
  max <- max(Graph$Upper, na.rm = TRUE)

  if (Sig) {
    Graph$starloc <- max + (max - min) / 10
  }

  if (!is.null(StarLoc)) {
    Graph$starloc <- StarLoc
  }

  Graph$Alpha <- with(Graph, ifelse(Lower * Upper > 0, Alpha1, Alpha2))

  Graph |>
    dplyr::mutate(SigAlpha = factor(as.numeric(Lower * Upper > 0),
      levels = c(1, 0)
    )) -> Graph

  if (PointOutline) {
    PointOutlineAlpha <- Alpha1
  } else {
    PointOutlineAlpha <- 0
  }

  ggplot(
    Graph,
    aes(
      x = as.factor(Factor),
      y = Estimate,
      group = Model,
      colour = Model,
      alpha = SigAlpha
    )
  ) +
    geom_hline(aes(yintercept = 0), lty = 2, linewidth = lwd * 0.6) +
    scale_color_manual(values = c("#a70000", "#ff5252", "lightskyblue", "royalblue", "navy")) +
    geom_point(position = position_dodge(width = pdw), size = Size) +
    geom_errorbar(
      position = position_dodge(width = pdw),
      aes(ymin = Lower, ymax = Upper), size = 0.75,
      width = tips,
      linewidth = lwd
    ) +
    labs(x = NULL) +
    coord_flip() +
    theme(legend.position = position) +
    geom_text(aes(label = Sig, y = starloc),
      position = position_dodge(width = pdw),
      size = Size * 2.5,
      fontface = "bold",
      show.legend = FALSE
    ) +
    scale_alpha_manual(values = c(Alpha1, Alpha2)) +
    guides(alpha = "none") +
    geom_point(
      colour = "black", aes(group = Model),
      position = position_dodge(width = pdw), size = 4,
      alpha = PointOutlineAlpha
    ) +
    geom_errorbar(aes(ymin = Lower, ymax = Upper, group = Model),
      width = tips,
      linewidth = lwd,
      position = position_dodge(width = pdw),
      colour = "black",
      alpha = PointOutlineAlpha
    ) +
    geom_point(
      position = position_dodge(width = pdw), size = 3,
      alpha = PointOutlineAlpha
    )
}


INLADICFig <- function(ModelList,
                       ModelNames = NULL,
                       Just = FALSE, Order = FALSE,
                       Delta = FALSE,
                       Legend = FALSE,
                       Responses = NULL,
                       CutOff = 2, OverPlot = FALSE) {
  if (!OverPlot) {
    if (!class(ModelList) == "list") {
      ModelList <- list(ModelList)
    }

    df <- data.frame(
      Model = length(ModelList) |> seq_len(),
      DIC = sapply(ModelList, function(m) m$dic$dic)
    )

    if (is.null(ModelNames)) {
      ModelNames <- length(ModelList) |> seq_len()
    }

    df$ModelName <- ModelNames
    df$Competitive <- with(df, ifelse(DIC < min(DIC + CutOff), "Y", "N"))

    if (Just) {
      Angle <- 45
      Hjust <- 1
    } else {
      Angle <- 0
      Hjust <- 0.5
    }

    if (Order) {
      df |>
        dplyr::arrange(desc(DIC)) |>
        dplyr::mutate_at("Model", ~ factor(.x, levels = .x)) |>
        dplyr::mutate_at("ModelName", ~ factor(.x, levels = .x)) -> df
    }

    if (Delta) {
      df |> dplyr::mutate_at("DIC", ~ .x - min(.x)) -> df
    }

    DICPlot <- ggplot(df, aes(as.numeric(as.factor(ModelName)), DIC))

    if (Delta) {
      DICPlot <- DICPlot + geom_hline(yintercept = 0, lty = 2, alpha = 0.6)
    }

    DICPlot <- DICPlot +
      geom_point(aes(shape = Competitive)) +
      scale_shape_manual(values = c(1, 2)) +
      geom_line() +
      labs(x = "Model") +
      scale_x_continuous(breaks = as.numeric(as.factor(df$ModelName)), labels = df$ModelName) +
      # geom_point(data = df[df$Competitive == "Y",]) +
      theme(axis.text.x = element_text(angle = Angle, hjust = Hjust))

    if (!Legend) {
      DICPlot <- DICPlot + theme(legend.position = "none")
    }

    return(DICPlot)
  } else {
    if (is.null(ModelNames)) {
      ModelNames <- length(ModelList[[1]]) |> seq_len()
    }

    length(ModelList) |>
      seq_len() |>
      purrr::map_dfc(~ sapply(ModelList[[.x]], MDIC)) |>
      dplyr::mutate(Model = 1:n()) -> df

    if (is.null(Responses)) {
      Responses <- paste0("Resp.", 1:length(ModelList))
    }

    colnames(df)[!(colnames(df) == "Model")] <- Responses
    df$ModelName <- ModelNames

    df <- df |>
      tidyr::gather("Response", "DIC", -c(Model, ModelName)) |>
      dplyr::group_by(Response) |>
      dplyr::mutate(Competitive = ifelse(DIC < min(DIC + CutOff), "Y", "N"))

    if (Just) {
      Angle <- 45
      Hjust <- 1
    } else {
      Angle <- 0
      Hjust <- 0.5
    }

    if (Order) {
      df <- df |>
        dplyr::arrange(Response, desc(DIC)) |>
        dplyr::mutate_at("Model", ~ factor(.x, levels = .x)) |>
        dplyr::mutate_at("ModelName", ~ factor(.x, levels = .x))
    }

    if (Delta) {
      df |>
        dplyr::group_by(Response) |>
        dplyr::mutate_at("DIC", ~ .x - min(.x)) -> df
    }

    DICPlot <- ggplot(df, aes(as.numeric(as.factor(ModelName)), DIC))

    if (Delta) {
      DICPlot <- DICPlot + geom_hline(yintercept = 0, lty = 2, alpha = 0.6) +
        labs(y = "DeltaDIC")
    }

    DICPlot <- DICPlot +
      geom_point(aes(shape = Competitive)) +
      scale_shape_manual(values = c(1, 2)) +
      geom_line(aes(group = Response, colour = Response)) +
      labs(x = "Model") +
      scale_x_continuous(breaks = as.numeric(as.factor(df$ModelName)), labels = df$ModelName) +
      theme(axis.text.x = element_text(angle = Angle, hjust = Hjust))

    if (!Legend) {
      DICPlot <- DICPlot + theme(legend.position = "none")
    }

    return(DICPlot)
  }
}
