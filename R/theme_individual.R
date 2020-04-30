#' Theme for individual line graphs
#'
#' @param base_size General setting for size of font
#' @param base_family General setting for font type
#'
#' @return a ggplot2 theme
#'
#' @export


################################################
# define a theme
################################################


# generating a theme for publication quality graph
theme_individual <- function (base_size = 30,
                       base_family = "") {

  theme_grey(base_size = base_size,
             base_family = base_family) %+replace%

    theme(# Set text size
      plot.title = element_text(size = 30, margin = margin(0,0,20,0)),

      axis.title.x = element_text(colour = 'black',
                                  size = 25,
                                  hjust = 0.5,
                                  vjust = -1,
                                  margin = unit(c(5, 0, 0, 0), "mm")),

      axis.title.y = element_text(size = 25,
                                  hjust = 0.5,
                                  vjust = 3,
                                  angle = 90,
                                  margin = unit(c(0, 5, 0, 0), "mm")),



     # axis.text.x = element_text(size = 15, color = "black"),
    # axis.text.y = element_text(size = 15, color = "black"),

      strip.text.x = element_text(size = 25),
      strip.text.y = element_text(size = 25,
                                  angle = -90),

      # Legend
    legend.title=element_blank(),

  #  legend.title = element_text(size = 20, margin = margin(0,10,10,0, unit = "pt"),
  #                                                           colour = "black"),
      legend.text = element_text(size = 20),

      legend.position = "right",
      legend.key = element_rect(size = 5, colour = "white"),
      legend.key.size = unit(1.5, 'lines'),



    #legend.margin = margin(50,0,0,0),
      # Configure lines and axes
     # axis.ticks.x = element_line(colour = "black"),
    #  axis.ticks.y = element_line(colour = "black"),

      # Distance between ticks and tick labels
      axis.text.x = element_text(margin=margin(5,0,0,0, unit = "pt"),
                                 color = "black", size = 25),
      axis.text.y = element_text(margin=margin(0,5,0,0, unit = "pt"),
                                 color = "black", size = 25),



      # Plot background
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour = "grey83",
                                      size = 0.1),
      panel.grid.minor = element_line(colour = "grey88",
                                      size = 0.1),

      #plot.title = element_text(margin=margin(b = 2,
      #                                       unit = "pt"), hjust=0.5),

      #plot.title = element_text(vjust = 2),

      plot.margin=unit(c(1.5,1.5,1.5,1.5),"cm"),

      # Facet labels

      strip.background = element_rect(fill = "grey80",
                                      colour = "grey50",
                                      size = 0.2)

    )
}
