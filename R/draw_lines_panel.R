#' Create a summary line graph from a single analyte; suitable for multipanelled plots
#'
#' A function to create a summary line graph from a
#' dataset based on a single analyte.
#'
#' @param DF A data.frame containing data for a single analyte
#' @param palette_graph A palette with 5 colours
#'
#' @return a ggplot2 object and the function writes to
#' disk in the directory defined by the function
#' saveInImageDirectory
#'
#' @export


draw_lines_panel <- function(DF,
                             palette_graph,
                             ymax = max(DF$mean_conc * 1.5),
                             ymin = min(DF$mean_conc * 1.5)) {
  ##############################
  # load library & functions
  ##############################


  ################################
  # get analyte name
  ################################
  nameLine <- DF$analyte[1] %>%
    tolower()


  nameFile <- c(nameLine)


  ################################
  # get analyte metadata
  ################################
  analyte_annotations <- readxl::read_excel(
    path =
      file.path(
        root,
        "data-raw",
        "Copy of analytes_complete_ref_unit_SKa.xlsx"
      ),
    sheet = 1
  )

  y_axis_label <- analyte_annotations %>%
    dplyr::filter(tolower(analyte_short) == nameLine) %>%
    dplyr::select(units) %>%
    as.character()

  ###########################
  # long name for analyte
  ###########################
  long_name <- analyte_annotations %>%
    dplyr::filter(tolower(analyte_short) == nameLine) %>%
    dplyr::select(analyte_long_name) %>%
    as.character()

  firstup <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }

  plot_title <- firstup(long_name)

  y_lim_max <- ymax
  y_lim_min <- ymin


  plot <- ggplot2::ggplot(data = DF, aes(x = time,
                                         y = mean_conc,
                                         colour = protocol)) +
    ggplot2::geom_point(
      aes(shape = protocol),
      size = 1.6,
      position = position_dodge(width = 0.2),
      show.legend = FALSE
    ) +
    ggplot2::geom_line(
      aes(group = protocol),
      size = 2,
      alpha = 0.8,
      position = position_dodge(width = 0.2)
    ) +
    ggplot2::scale_colour_manual(
      labels = c(" Rest ",
                 " 70% Wmax ",
                 " 70% Wmax-DH ",
                 " 50% Wmax ",
                 " 55% / 85% Wmax "),
      values = palette_graph
    )   +
    ggplot2::scale_shape_manual(values = c(5, 6, 15:17)) +
    ggplot2::scale_y_continuous(limits = c(ymin, ymax)) +
    ggplot2::xlab("Time (hours)") +
    ggplot2::ylab(paste("Concentration", paste0("(",
                                                y_axis_label,
                                              ")"))) +
    ggplot2::geom_vline(
      xintercept = 1,
      colour = "gray",
      linetype = "dashed",
      size = 1
    ) +
    ggplot2::geom_vline(
      xintercept = 2,
      colour = "gray",
      linetype = "dashed",
      size = 1
    ) +
    geom_label(aes(x = 1, y = ymin + (0.1*ymin),
                   label = "T0"), colour = "black") +

    geom_label(aes(x = 2, y = ymin + (0.1*ymin),
                   label = "T1"), colour = "black") +

    ggplot2::ggtitle(plot_title) +

    citrulliner::theme_panel()

  ##assigning a name to the file
  png <- paste(nameFile, "panel_line.png", sep = "")
  #  eps <- paste(nameFile,"line.eps", sep = "")
  svg <- paste(nameFile, "panel_line.svg", sep = "")
  pdf <- paste(nameFile, "panel_line.pdf", sep = "")
  ## saving the file to the imageDir
  citrulliner::save_in_image_directory(svg)
  # saveInImageDirectory(eps)
  citrulliner::save_in_image_directory(png)
  citrulliner::save_in_image_directory(pdf)


  return(plot)


}

