## test new model mixed effects model on saliva data


## R Packages
if(!require("pacman")) install.packages("pacman", dependencies = TRUE)

library(gramlyr)
library(pacman)
library(arm)
library(stargazer)
library(gdata)
library(RCurl)
library(purrr)
library(readxl)
library(RColorBrewer)
library(wesanderson)
library(tidyverse)
library(nlme)
# install.packages("docxtools")
library(docxtools)



## Data
# data(package = "gramlyr")
data(package = "salivar")
data(package = "salivar", dataset = "data_all_tidy")
data(package = "salivar", dataset = "analyte_annotations")


#diagrams_clean$analyte %>% unique()

## check factor levels
map(data_all_tidy[, c(1:4)], unique)


## nest for analyte
data_nested <- data_all_tidy %>%
  group_by(analyte) %>%
  nest()


## run linear model
## dummy:
df <- data_nested$data[[1]]

model_lm <- function(df, ...){

  model_lm <- lm(data = df,
                concentration ~ protocol*time, ...)
  return(model_lm)
}


model_lme <- function(df, ...){

 # df <- na.omit(df)

  model_lme <- lme(data = df,
                  concentration ~ protocol*time,
                  random = ~1|subject, ...)
  return(model_lme)
}


## apply models to data
data_nested <- data_nested %>%
  mutate(model_lm = map(data, model_lm, na.action = "na.omit"),
         model_lme = map(data, model_lme, na.action = "na.omit"))

## inspect

i = 3

plot(ranef(data_nested$model_lme[[i]]))
plot(data_nested$model_lme[[i]])


## add summaries for both models
data_nested <- data_nested %>%
  mutate(summary_lm = map(model_lm, summary),
         summary_lme = map(model_lme, summary))



i = 27
data_nested$plots[[i]]
data_nested$analyte
data_nested$analyte[[i]]
data_nested$summary_lm[[i]]
data_nested$summary_lme[[i]]


## plots
analyte_plot <- function(df, ...){

  plot <- df %>%
    group_by(time, protocol) %>%
    summarise(mean_conc = mean(concentration, na.rm = TRUE)) %>%
    ggplot(aes(x = time, y = mean_conc)) +
    geom_line(aes(group = protocol, colour = protocol))

  return(plot)

}


data_nested <- data_nested %>%
  mutate(plots = map(data, analyte_plot))

plot(ranef(data_nested$model_lme[[i]]))
plot(data_nested$model_lme[[i]])
