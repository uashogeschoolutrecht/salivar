#' Create a summary line graph from a single analyte
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


draw_lines <- function(DF,
                       palette_graph,
                       analyte_x,
                       ...){
  ################################
  # get analyte name
  ################################
  nameLine <- analyte_x
  #%>%
   # tolower()

  nameFile <- c(nameLine)
  ################################
  # get analyte metadata
  ################################
  data(package = "salivar", "analyte_annotations")

  y_axis_label <- analyte_x

    #analyte_annotations %>%
    #dplyr::filter(tolower(analyte_short) == nameLine) %>%
    #dplyr::select(units) %>%
    #as.character()

  ###########################
  # long name for analyte
  ###########################
  long_name <- analyte_x


    #analyte_annotations %>%
    #dplyr::filter(tolower(analyte_short) == nameLine) %>%
    #dplyr::select(analyte_long_name) %>%
    #as.character()

  #firstup <- function(x) {
   # substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  #  x
  #}

  plot_title <- analyte_x

    #firstup(long_name)

  #############################
  # axis limits
  #############################

  plot <- ggplot2::ggplot(data = DF, aes(x = time,
                                         y = mean,
                                         colour = protocol)) +
      ggplot2::geom_point(
      aes(shape = protocol),
      size = 3,
      position = position_dodge(width = 0.2),
      show.legend = FALSE
    ) +
    ggplot2::geom_line(aes(group = protocol),
                       size = 1.6,
                       position = position_dodge(width = 0.2)) +
    ggplot2::scale_colour_manual(labels = c(" Rest ",
                                            " 70% ",
                                            " 70%-DH ",
                                            " 50% ",
                                            " 55%/85% "),
                                  values = palette_graph)   +
    ggplot2::scale_shape_manual(values = c(5, 6, 15:17)) +
    ggplot2::xlab("Time (hours)") +
    ggplot2::ylab(paste("Concentration", paste0("(",
                                                y_axis_label,
                                                ")"))) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = 2, linetype = "dashed") +

    geom_label(aes(x = 1, y = 0,
                   label = "T0"), colour = "black") +

    geom_label(aes(x = 2, y = 0,
                   label = "T1"), colour = "black") +

  salivar::theme_individual()

  plot
  ##assigning a name to the file
  png <- paste(nameFile, "line.png", sep = "")
  #  eps <- paste(nameFile,"line.eps", sep = "")
  svg <- paste(nameFile, "line.svg", sep = "")

  ## saving the file to the imageDir
  salivar::save_in_image_directory(svg, ...)
  # saveInImageDirectory(eps)
  salivar::save_in_image_directory(png, ...)

  return(plot)


}
