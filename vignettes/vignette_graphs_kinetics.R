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


## ---- packages----------------------------------------------------------------
# install.packages("gdata")
# install.packages("stargazer")
# install.packages("arm")
# library(arm)
# library(stargazer)
# library(gdata)
# library(RCurl)
# install.packages("svglite")
library(tidyverse)
library(purrr)
library(readxl)
library(readr)
library(svglite)
library(ggplot2)
library(gridExtra)
library(grid)
library(citrulliner)
library(salivar)


## -----------------------------------------------------------------------------
## data(package = "salivar")
data(package = "salivar", "data_all_tidy")
data(package = "salivar", "data_order_tidy")
data(package = "salivar", "analyte_annotations")



## -----------------------------------------------------------------------------
map(
  .x = data_all_tidy[, -5],
  .f = levels
)

## analytes 
unique(data_all_tidy$analyte)


## -----------------------------------------------------------------------------
sum(is.na(data_all_tidy$concentration))
naniar::vis_miss(data_all_tidy)



## -----------------------------------------------------------------------------
names(data_all_tidy)
data_all_tidy %>%
  na.omit() %>%
#   dplyr::filter(duplicated == "FALSE") %>%
   dplyr::group_by(protocol,
           time,
           analyte) %>%

  dplyr::summarise(mean_conc = mean(concentration),
                   sd = round(sd(concentration), digits = 3),
                   n_obs = n(),
                   sem = round(sd/sqrt(n_obs), digits = 3)) %>%
  
  dplyr::arrange(protocol, analyte) -> data_summary



## -----------------------------------------------------------------------------
# levels(data_summary$analyte %>% as_factor)

relevant_analytes <- c(
  "slpi.se",
  "MMP-9",
  "mmp9.se",
  "SLPI"
)

data_summary_filtered <- data_summary %>%
  dplyr::filter(analyte %in% relevant_analytes)



## -----------------------------------------------------------------------------
data_summary_filtered <- data_summary_filtered %>%
  mutate(mean_conc = mean_conc/1000000)



## -----------------------------------------------------------------------------

## split for analytes
data_by_analyte <- data_summary_filtered %>%
#  as_tibble() %>%
#  gather(exam:numeracy, key = "measure", value = "value") %>%
  group_by(analyte) %>%
  nest() %>% print()

data_by_analyte$data[[1]]



## -----------------------------------------------------------------------------

data_summary_filtered$analyte <- as.factor(data_summary_filtered$analyte)
levels(data_summary_filtered$analyte)

data_split <- split(data_summary_filtered, 
                    data_summary_filtered$analyte)




## -----------------------------------------------------------------------------
RColorBrewer::display.brewer.all()
palette <- RColorBrewer::brewer.pal(7, "Set1")
palette <- c("#000000", palette[c(1:3,4)])


## ---- eval=FALSE--------------------------------------------------------------
## # define image directory
## image_directory <- file.path(here::here("inst", "images", "paper"))
## 
## # argument for function test
## analyte = "SLPI"
## 
## print_lines <- function(analyte, ...){
## 
##   #ymax_data <- max(data_split[[analyte]]$mean_conc)
## #  sem_max <- max(data_split[[analyte]]$sem)
## #  ymax <- 1.6*(ymax_data)
## 
## #  ymin_data <- min(data_split[[analyte]]$mean_conc)
## #  ymin <- ymin_data - 1.6*(ymin_data)
## 
## data_plot <- data_split[[analyte]]
## 
## 
##  plot <- citrulliner::draw_lines(DF = data_plot,
##                                  palette_graph = palette,
##                                 # ymax = ymax,
##                                 # ymin = ymin
##                                 f.width = 14, f.height = 10)
## 
## 
## 
## 
##   return(plot)
## }
## 
## #ala_lines
## plot_list <- lapply(levels(data_summary_filtered$analyte), print_lines)
## plot_list[[1]]
## 
## names(plot_list) <- levels(data_summary_filtered$analyte)
## names(plot_list)
## 


## -----------------------------------------------------------------------------
library(profvis)
library(citrulliner)
library(gtable)
library(cowplot)

print_lines_panel <- function(analyte){

  ymax_data <- max(data_split[[analyte]]$mean_conc)
  sem_max <- max(data_split[[analyte]]$sem)
  ymax <- 1.05*(ymax_data)
  
  ymin_data <- min(data_split[[analyte]]$mean_conc)
  ymin <- ymin_data - (0.05*ymin_data)
  
data_plot <- data_split[[analyte]]
    
  
 plot <- citrulliner::draw_lines_panel(DF = data_plot, 
                                       palette_graph = palette, 
                                       ymax = ymax,
                                       ymin = ymin)

  

  return(plot)
}


panel_plot_list <- lapply(levels(data_summary_filtered$analyte), print_lines_panel)
names(panel_plot_list) <- levels(data_summary_filtered$analyte)
names(panel_plot_list)




## -----------------------------------------------------------------------------

# figure 1; panel
p1_a <- panel_plot_list[["SLPI"]]
p1_b <- panel_plot_list[["slpi.se"]]
p1_c <- panel_plot_list[["MMP-9"]]
p1_d <- panel_plot_list[["mmp9.se"]]

# # figure 2; panel
# p2_a <- panel_plot_list[["BICARB"]]
# p2_b <- panel_plot_list[["HB"]]
# p2_c <- panel_plot_list[["GLU_NS"]]
# p2_d <- panel_plot_list[["citrul"]]
# 
# figure 3
# p3_a <- panel_plot_list[["SODIUM"]]
# p3_b <- panel_plot_list[["INSULINE"]]
# p3_c <- panel_plot_list[["HT"]]
# 
# # figure 4
# p4_a <- panel_plot_list[["CHLORIDE"]]
# p4_b <- panel_plot_list[["ALB"]]
# p4_c <- panel_plot_list[["ERY"]]



## -----------------------------------------------------------------------------
p1_a + ylim(c(0, max(max(data_split[["SLPI"]]$mean_conc))))
p1_b + ylim(c(0, max(max(data_split[["SLPI"]]$mean_conc))))
 

slpi_sa_lines_ <- print_lines(analyte = "SLPI")
slpi_sa_lines + ylim(c(0, ))


slpi_se_lines <- print_lines(analyte = "slpi.se")
slpi_se_lines + ylim(c(0, max(data_split[["SLPI"]]$mean_conc)))



## ---- fig.width=16, fig.height=24---------------------------------------------
# https://cran.r-project.org/web/packages/cowplot/vignettes/shared_legends.html

figure_1 <- cowplot::plot_grid(
           p1_a + theme(legend.position="none"),
           p1_b + theme(legend.position="none"),
           p1_c + theme(legend.position="none"),
           p1_d + theme(legend.position="none"),
           align = 'vh',
           labels = c("A", "B", "C", "D"),
           hjust = -8,
           ncol = 2,
           nrow = 3,
           label_size = 18
           )

# figure_1

legend_b <- get_legend(p1_a + theme(legend.position="bottom"))

p_1 <- plot_grid(legend_b, figure_1, ncol = 1, rel_heights = c(0.05, 1))
p_1 %>% class

ggsave(filename = file.path(root,
                            "inst",
                            "images",
                            "paper",
                            "figure_1.svg"), plot = p_1,
       width = 40, height = 60, dpi = 300, units = "cm")
 
ggsave(filename = file.path(root,
                            "inst",
                            "images",
                            "paper",
                            "figure_1.png"), plot = p_1,
       width = 40, height = 60, dpi = 300, units = "cm")




## ---- fig.width=16, fig.height=20---------------------------------------------

figure_2 <- cowplot::plot_grid(
           p2_a + theme(legend.position="none"),
           p2_b + theme(legend.position="none"),
           p2_c + theme(legend.position="none"),
           p2_d + theme(legend.position="none"),
           align = 'vh',
           labels = c("A", "B", "C", "D"),
           hjust = -8,
           ncol = 2,
           nrow = 3,
           label_size = 18
           )

# figure_2

legend_b <- get_legend(p2_a + theme(legend.position="bottom"))

p_2 <- plot_grid(legend_b, figure_2, ncol = 1, rel_heights = c(0.05, 1))
p_2 %>% class

ggsave(filename = file.path(root,
                            "inst",
                            "images",
                            "kinetics_2.svg"), plot = p_2,
       width = 40, height = 60, dpi = 300, units = "cm")
 




## ---- fig.width=16, fig.height=20---------------------------------------------

figure_3 <- cowplot::plot_grid(
           p3_a + theme(legend.position="none"),
           p3_b + theme(legend.position="none"),
           p3_c + theme(legend.position="none"),
           align = 'vh',
           labels = c("A", "B", "C", "D"),
           hjust = -8,
           ncol = 1,
           nrow = 3,
           label_size = 18
           )

figure_3

legend_b <- get_legend(p3_a + theme(legend.position="bottom"))

p_3 <- plot_grid(legend_b, figure_3, ncol = 1, rel_heights = c(0.05, 1))
p_3 %>% class

ggsave(filename = file.path(root,
                            "inst",
                            "images",
                            "kinetics_3.svg"), plot = p_3,
       width = 25, height = 50, dpi = 300, units = "cm")
 




## ---- fig.width=16, fig.height=20---------------------------------------------

figure_4 <- cowplot::plot_grid(
           p4_a + theme(legend.position="none"),
           p4_b + theme(legend.position="none"),
           p4_c + theme(legend.position="none"),
           align = 'vh',
           labels = c("A", "B", "C", "D"),
           hjust = -8,
           ncol = 1,
           nrow = 3,
           label_size = 18
           )

# figure_2

legend_b <- get_legend(p4_a + theme(legend.position="bottom"))

p_4 <- plot_grid(legend_b, figure_4, ncol = 1, rel_heights = c(0.05, 1))
p_4 %>% class

ggsave(filename = file.path(root,
                            "inst",
                            "images",
                            "kinetics_4.svg"), plot = p_4,
       width = 25, height = 50, dpi = 300, units = "cm")
 




## -----------------------------------------------------------------------------
RColorBrewer::display.brewer.all()
palette <- RColorBrewer::brewer.pal(7, "Set1")

# GetColorHexAndDecimal <- function(color){
#   c <- col2rgb(color)
#   
#   sprintf("#%02X%02X%02X%3d%3d%3d", c[1],c[2],c[3],c[1],c[2],c[3])
# } 
# 
# black <- GetColorHexAndDecimal("black")
palette_graph <- c("#000000", palette[c(1:3)])

print(palette_graph)
cat(palette_graph)


## -----------------------------------------------------------------------------
data_zonulin <- gramlyr::data_clean_grinta_zonulin()

time_points <- levels(data_zonulin$time %>% as_factor())
subject_ids_zonulin <- gramlyr::get_levels(data_zonulin$subject)
subect_ids_grinta <- gramlyr::get_levels(data_clean_diagrams$subject)



## filter grinta data for ifab, citrulline, for above subjects and time_points
data_ifab_citrulline <- data_clean_diagrams %>%
  dplyr::filter(analyte == "ifabp" | analyte == "citrul") %>%
  dplyr::filter(subject %in% subject_ids_zonulin) %>%
#  dplyr::filter(time %in% time_points) %>%
  dplyr::filter(time == "0" |
                time == "1" |
                time == "2" |
                time == "24",
                protocol != "P5") %>%
  mutate(time = droplevels(time),
         protocol = droplevels(protocol)) %>%
  select(subject, time, protocol, analyte, concentration) 

levels(data_ifab_citrulline$time)
levels(data_ifab_citrulline$protocol)



## -----------------------------------------------------------------------------

data_zonuline_select <- data_zonulin %>%
  dplyr::rename(analyte = analyse) %>%
  dplyr::filter(time == "0" |
                time == "1" |
                time == "2" |
                time == "24") %>%
  mutate(subject = as_factor(subject %>% as.character()),
         concentration = result,
         time = droplevels(time),
         analyte = tolower(analyte)) %>%
  select(subject, time, protocol, analyte, concentration)


data_zonuline_select$time %>% levels()
data_zonuline_select$analyte %>% unique()

data_ifabp_citrul_zonul <- dplyr::bind_rows(
  data_ifab_citrulline,
  data_zonuline_select) 

data_ifabp_citrul_zonul



## -----------------------------------------------------------------------------

## dummy 
df = data_zonuline_select

normalize_for_time_0 <- function(df){
  
baseline <- df %>% 
  dplyr::filter(time == "0")

df_with_baseline <- df %>%
  left_join(
    baseline,
    by = c('subject',
           'protocol',
           'analyte'),
    suffix = c('', '.baseline')
  )
# The datasets diagram and baseline are joined by matching the columns 
# subject_fct, protocol_fct, analyte and study. Note that time_fct is NOT used 
# to match rows because baseline only contains t = 0 rows by definition and 
# these should be added to all other rows (t = 0.5, 1 etc.).
# 
# A left join is used to prevent loss of rows from diagrams that have no 
# matching row in baseline (i.e. no baseline value available for the given 
# subject, protocol, analyte and study). The number of rows in the resulting
# dataset is equal to the the diagrams set.
# 
# Suffix is added to the column names to discriminate columns from the left
# (diagrams) and right (baseline) dataset with equal names. In this case,
# nothing is added to the columns from the diagrams and '.baseline' is added to
# the additional columns from baseline.
#
# The resulting dataset has 8 columns: 6 diagrams + 6 baseline - 4 duplicate.
# The duplicates are the four columns that were used to match rows from both
# sets.

# Rows without baseline measurement.
no_baseline <- df_with_baseline %>%
  dplyr::filter(is.na(concentration.baseline))

# Now, add a column with the normalized concentration that is calculated by
# dividing the concentration (of a given study, participant etc.) by the 
# corresponding baseline concentration.
df_normalized <-
  dplyr::mutate(
    df_with_baseline,
    concentration_normalized = concentration / 
      concentration.baseline
  ) 
  

  return(df_normalized)

}



## -----------------------------------------------------------------------------

df_normalized_zonuline <- data_zonuline_select %>%
#  dplyr::filter(concentration > 20) %>%

gramlyr::normalize_for_time_0()



df_normalized_ifabp_citrulline <- gramlyr::normalize_for_time_0(data_ifab_citrulline)  

df_bar <- dplyr::bind_rows(
  df_normalized_zonuline,
  df_normalized_ifabp_citrulline
) 

df_bar$subject %>% as_factor() %>% levels()
df_bar$time %>% as_factor() %>% levels()
df_bar$protocol %>% as_factor() %>% levels()
df_bar$analyte %>% as_factor() %>% levels()



## -----------------------------------------------------------------------------
library(forcats)

df_bar <- df_bar %>%
  mutate(time = fct_relevel(time %>% as_factor,
                           "0", "1", "2", "24"),
         protocol = fct_relevel(protocol %>% as_factor,
                           "P1", "P2", "P3", "P4"))


df_bar$subject %>% as_factor() %>% levels()
df_bar$time %>% as_factor() %>% levels()
df_bar$protocol %>% as_factor() %>% levels()
df_bar$analyte %>% as_factor() %>% levels()




## -----------------------------------------------------------------------------

## checks

df_bar_check <- df_bar %>%
  dplyr::filter(analyte == "zonulin",
                time == "0" | time == "2",
                protocol == "P1") %>% 
  group_by(protocol, time) %>%
  summarise(mean_conc = mean(concentration_normalized, na.rm = TRUE))


names(df_bar)
bars <- data_ifabp_citrul_zonul %>%
  group_by(time, protocol, analyte) %>%
    summarise(mean_conc = mean(concentration, na.rm = TRUE)) %>%
  ggplot(aes(x = time, 
             y = mean_conc)) +
    geom_bar(aes(fill = protocol),
             colour = 'black',
             stat = 'identity', 
             position = 'dodge') +
#  geom_hline(aes(yintercept = 1.0), 
#             linetype = 'dashed',
#             size = 1.5) +
  xlab("Time(hours)") +
  ylab("Mean conc.") +
  
  ggplot2::scale_fill_manual(
      labels = c(" Rest ",
                 " 70% Wmax ",
                 " 70% Wmax-DH ",
                 " 50% Wmax "),
                 values = palette_graph
    ) + 
   
  facet_wrap(~ analyte, scales = 'free') +
#  theme_bw() 
  citrulliner::theme_individual() +
  theme(strip.background =element_rect(fill="black"))+
  theme(strip.text = element_text(colour = 'white'))

bars

ggsave(filename = file.path(root,
                            "inst",
                            "images",
                            "kinetics_bars.svg"), 
       dpi = 300, 
       height = 18, width = 38,
       units = 'cm')

ggsave(filename = file.path(root,
                            "inst",
                            "images",
                            "kinetics_bars.png"), 
       dpi = 300, 
       height = 18, width = 38,
       units = 'cm')



## -----------------------------------------------------------------------------
df_bar$protocol %>% 
  as_factor() %>%
  levels()


dots_to_check_normalized <- df_bar %>%
#  dplyr::filter(concentration_normalized < 7.5) %>%
#  group_by(time, protocol, analyte) %>%
#    summarise(mean_conc = mean(concentration_normalized, na.rm = TRUE)) %>%
  ggplot(aes(x = time, 
             y = concentration_normalized)) +
   
  geom_boxplot(aes(colour = protocol, 
                  group = protocol)) +
  
#   geom_point(aes(colour = protocol, 
#                  group = protocol),
#              alpha = 0.8, position = 'jitter') +
  geom_hline(aes(yintercept = 1.0), 
             linetype = 'dashed',
             size = .5) +
  xlab("Time(hours)") +
  ylab("Fold change") +
  
  ggplot2::scale_colour_manual(
      labels = c(" Rest ",
                 " 70% Wmax ",
                 " 70% Wmax-DH ",
                 " 50% Wmax "),
                 
      values = palette_graph
    ) + 
   
  facet_wrap(~ analyte) +
#  theme_bw() 
#  citrulliner::theme_individual() +
  theme(strip.background =element_rect(fill="black"))+
  theme(strip.text = element_text(colour = 'white'))


dots_to_check_normalized






## -----------------------------------------------------------------------------

dots_to_check_raw <- data_zonuline_select %>%
#  dplyr::filter(concentration_normalized < 7.5) %>%
#  group_by(time, protocol, analyte) %>%
#    summarise(mean_conc = mean(concentration_normalized, na.rm = TRUE)) %>%
  ggplot(aes(x = time, 
             y = concentration)) +
   
#  geom_boxplot(aes(colour = protocol, 
#                  group = protocol)) +
  
   geom_point(aes(colour = protocol, 
                  group = protocol),
              alpha = 0.8, position = 'jitter', size = 2) +
 # geom_hline(aes(yintercept = 1.0), 
#             linetype = 'dashed',
#             size = .5) +
  xlab("Time(hours)") +
  ylab("Fold change") +
  
  ggplot2::scale_colour_manual(
      labels = c(" Rest ",
                 " 70% Wmax ",
                 " 70% Wmax-DH ",
                 " 50% Wmax "),
                 
      values = palette_graph
    ) + 
   
  facet_wrap(~ analyte) +
#  theme_bw() 
#  citrulliner::theme_individual() +
  theme(strip.background =element_rect(fill="black"))+
  theme(strip.text = element_text(colour = 'white'))


dots_to_check_raw



# 
 ggsave(filename = file.path(root,
                             "inst",
                             "images",
                             "dots_to_chek_raw.svg"), 
        dpi = 300, 
        height = 12, width = 20,
        units = 'cm')



## -----------------------------------------------------------------------------
data_zonuline_select %>%
  group_by(time, protocol, analyte) %>%
    summarise(mean_conc = mean(concentration, na.rm = TRUE)) %>%
  ggplot(aes(x = time, 
             y = mean_conc)) +
    geom_line(aes(colour = protocol, 
                  group = protocol),
              size = 1.5,
              alpha = 0.8) +
#  geom_hline(aes(yintercept = 1.0), 
#             linetype = 'dashed',
#             size = 1.5) +
  xlab("Time(hours)") +
  ylab("Fold change") +
  
  ggplot2::scale_colour_manual(
      labels = c(" Rest ",
                 " 70% Wmax ",
                 " 70% Wmax-DH ",
                 " 50% Wmax "),
                 
      values = palette_graph
    ) + 
   
  facet_wrap(~ analyte) +
#  theme_bw() 
#  citrulliner::theme_individual() +
  theme(strip.background =element_rect(fill="black"))+
  theme(strip.text = element_text(colour = 'white'))

zonuline_chek_fold <- data_zonuline_select %>%
  dplyr::filter(protocol == "P1",
                time == "0" | time == "2")

mean_0 <- zonuline_chek_fold %>%
  dplyr::filter(time == "0") %>%
  summarise(mean(concentration))

  


## -----------------------------------------------------------------------------
df_bar$protocol %>% 
  as_factor() %>%
  levels()


line_as_alternative_for_bar <- df_bar %>%
  group_by(time, protocol, analyte) %>%
    summarise(mean_conc = mean(concentration_normalized, na.rm = TRUE)) %>%
  ggplot(aes(x = time, 
             y = mean_conc)) +
    geom_line(aes(colour = protocol, 
                  group = protocol),
              size = 1.5,
              alpha = 0.8) +
  geom_hline(aes(yintercept = 1.0), 
             linetype = 'dashed',
             size = 1.5) +
  xlab("Time(hours)") +
  ylab("Fold change") +
  
  ggplot2::scale_colour_manual(
      labels = c(" Rest ",
                 " 70% Wmax ",
                 " 70% Wmax-DH ",
                 " 50% Wmax "),
                 
      values = palette_graph
    ) + 
   
  facet_wrap(~ analyte) +
#  theme_bw() 
  citrulliner::theme_individual() +
  theme(strip.background =element_rect(fill="black"))+
  theme(strip.text = element_text(colour = 'white'))


line_as_alternative_for_bar


ggsave(filename = file.path(root,
                            "inst",
                            "images",
                            "kinetics_line_alternative_for_bar_normalized.svg"), 
       dpi = 300, 
       height = 18, width = 38,
       units = 'cm')

ggsave(filename = file.path(root,
                            "inst",
                            "images",
                            "kinetics_line_alternative_for_bar_normalized.png"), 
       dpi = 300, 
       height = 18, width = 38,
       units = 'cm')



## -----------------------------------------------------------------------------
data_zonuline_select %>%
  group_by(time, protocol, analyte) %>%
    summarise(mean_conc = mean(concentration, na.rm = TRUE)) %>%
  ggplot(aes(x = time, 
             y = mean_conc)) +
    geom_line(aes(colour = protocol, 
                  group = protocol),
              size = 1.5,
              alpha = 0.8) +
#  geom_hline(aes(yintercept = 1.0), 
#             linetype = 'dashed',
#             size = 1.5) +
  xlab("Time(hours)") +
  ylab("Fold change") +
  
  ggplot2::scale_colour_manual(
      labels = c(" Rest ",
                 " 70% Wmax ",
                 " 70% Wmax-DH ",
                 " 50% Wmax "),
                 
      values = palette_graph
    ) + 
   
  facet_wrap(~ analyte) +
#  theme_bw() 
#  citrulliner::theme_individual() +
  theme(strip.background =element_rect(fill="black"))+
  theme(strip.text = element_text(colour = 'white'))

ggsave(filename = file.path(root,
                            "inst",
                            "images",
                            "kinetics_line_zonulin_non_normalized.svg"), 
       dpi = 300, 
       height = 18, width = 38,
       units = 'cm')

ggsave(filename = file.path(root,
                            "inst",
                            "images",
                            "kinetics_line_zonulin_non_normalized.png"), 
       dpi = 300, 
       height = 18, width = 38,
       units = 'cm')

  


## -----------------------------------------------------------------------------
data_ifabp_citrul_zonul %>%
  group_by(time, protocol, analyte) %>%
    summarise(mean_conc = mean(concentration, na.rm = TRUE)) %>%
  ggplot(aes(x = time, 
             y = mean_conc)) +
    geom_line(aes(colour = protocol, 
                  group = protocol),
              size = 1.5,
              alpha = 0.8) +
#  geom_hline(aes(yintercept = 1.0), 
#             linetype = 'dashed',
#             size = 1.5) +
  xlab("Time(hours)") +
  ylab("Mean conc.") +
  
  ggplot2::scale_colour_manual(
      labels = c(" Rest ",
                 " 70% Wmax ",
                 " 70% Wmax-DH ",
                 " 50% Wmax "),
                 
      values = palette_graph
    ) + 
   
  facet_wrap(~ analyte, scales = 'free') +
#  theme_bw() 
  citrulliner::theme_individual() +
  theme(strip.background =element_rect(fill="black"))+
  theme(strip.text = element_text(colour = 'white'))

ggsave(filename = file.path(root,
                            "inst",
                            "images",
                            "kinetics_line_citrul_ifabp_zonulin_non-normalized.svg"), 
       dpi = 300, 
       height = 18, width = 45,
       units = 'cm')

ggsave(filename = file.path(root,
                            "inst",
                            "images",
                            "kinetics_line_citrul_ifabp_zonulin_non-normalized.png"), 
       dpi = 300, 
       height = 18, width = 45,
       units = 'cm')

  
