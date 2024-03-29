% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_linefigures_panel.R,
%   R/grid_arrange_shared_legend.R
\name{grid_arrange_shared_legend}
\alias{grid_arrange_shared_legend}
\title{Function to combine multiple ggplot graphs in a panel,}
\usage{
grid_arrange_shared_legend(
  ...,
  ncol = length(list(...)),
  nrow = 1,
  position = c("bottom", "right")
)

grid_arrange_shared_legend(
  ...,
  ncol = length(list(...)),
  nrow = 1,
  position = c("bottom", "right")
)
}
\arguments{
\item{...}{additional parameters to ggplot}

\item{ncol}{Number of columns in the panel, defaults to
the number of plots in the plotlist}

\item{nrow}{Number of row, defaults to 1}

\item{position}{Legend position, defaults to c("bottom" , "right")}
}
\description{
This functions plots multiple plots in a panel, keeping only the fist
legend of the fist plot
keeping only the legend of the first plot

This functions plots multiple plots in a panel, keeping only the fist
legend of the fist plot
keeping only the legend of the first plot
}
\examples{
dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
p1 <- qplot(carat, price, data = dsamp, colour = clarity)
p4 <- qplot(depth, price, data = dsamp, colour = clarity)
grid_arrange_shared_legend(p1, p2, p3, p4, ncol = 4, nrow = 1)
scale_linetype_manual(values=c("solid",
          "dotted", "longdash", "dotdash", "dashed"))

dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
p1 <- qplot(carat, price, data = dsamp, colour = clarity)
p4 <- qplot(depth, price, data = dsamp, colour = clarity)
grid_arrange_shared_legend(p1, p2, p3, p4, ncol = 4, nrow = 1)
scale_linetype_manual(values=c("solid",
          "dotted", "longdash", "dotdash", "dashed"))

}
