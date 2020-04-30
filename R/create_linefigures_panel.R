#' Function to combine multiple ggplot graphs in a panel,
#'
#' This functions plots multiple plots in a panel, keeping only the fist
#' legend of the fist plot
#' keeping only the legend of the first plot
#'
#' @param ncol Number of columns in the panel, defaults to
#' the number of plots in the plotlist
#' @param ... additional parameters to ggplot
#' @param nrow Number of row, defaults to 1
#' @param position Legend position, defaults to c("bottom" , "right")
#'
#' @examples
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' p1 <- qplot(carat, price, data = dsamp, colour = clarity)
# 'p2 <- qplot(cut, price, data = dsamp, colour = clarity)
# 'p3 <- qplot(color, price, data = dsamp, colour = clarity)
#' p4 <- qplot(depth, price, data = dsamp, colour = clarity)
#' grid_arrange_shared_legend(p1, p2, p3, p4, ncol = 4, nrow = 1)
# 'grid_arrange_shared_legend(p1, p2, p3, p4, ncol = 2, nrow = 2)
#' scale_linetype_manual(values=c("solid",
#'           "dotted", "longdash", "dotdash", "dashed"))
#'
#' @export

grid_arrange_shared_legend <- function(..., ncol = length(list(...)),
                                       nrow = 1, position = c("bottom", "right")) {

  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)

  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))

  grid.newpage()
  grid.draw(combined)

  # return gtable invisibly
  invisible(combined)

}


