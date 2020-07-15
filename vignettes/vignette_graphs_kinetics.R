## ---- setup, include=FALSE----------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, 
                      warning = FALSE,
                      error = FALSE,
                      message = FALSE,
                      include = TRUE,
                      fig.width = 11,
                      fig.height = 5)

## ---- root--------------------------------------------------------------------
if(!require("rprojroot")) install.packages("rprojroot", dependencies = TRUE)
library(rprojroot)
root <- rprojroot::find_root_file(criterion = is_rstudio_project)
root

