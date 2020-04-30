## Functions to package salivar
## Author: Marc A.T. Teunis
## September 2019

##################################################################################

#' @title Function to plot histograms from a (nested) dataframe
#' @param df An object of class "data.frame", "tbl", "tbl-df"
#' @export

plot_histogram_from_nested_df <- function(df, var, title){

  histogram <- df %>%
    ggplot(aes(
      x = get(var)
    )) +
    geom_histogram() +
    ggtitle(title)



  return(histogram)
}
####################################################################################


#' @title Function to plot boxplots from a (nested) dataframe
#' @param df An object of class "data.frame", "tbl", "tbl-df"
#' @export

plot_boxplot_from_nested_df <- function(df,
                                        title,
                                        var_x,
                                        var_y,
                                        xlab = var_x,
                                        ylab = var_y){

  boxplot <- df %>%
    ggplot(aes(
      x = get(var_x),
      y = get(var_y)
    )) +
    geom_boxplot() +
    ggtitle(title) +
    xlab(xlab) +
    ylab(ylab)


  return(boxplot)
}
####################################################################################

#' @title Function to get summary statistics for a nested analyte dataframe
#' @param df An object of class "data.frame", "tbl", "tbl-df"
#' @export

summarize_df <- function(df){

  summary_stats <- df %>%
    group_by(protocol, time) %>%
    summarise(mean = mean(concentration, na.rm = TRUE),
              median = median(concentration, na.rm = TRUE),
              n = n(),
              sdev = sd(concentration, na.rm = TRUE))
  return(summary_stats)

}

#' Toy example of a switch function for educational  puposes
#' ## toy example
#' @export

## toy example

distro <- function(n, type, ...){

  switch(type,
         norm = {
           distribution <- rnorm(n, ...)
         },

         binom = {
           distribution <- rbinom(n, ...)
         },
         stop("Enter something that switches me!")
  )
  return(distribution)
}


#' Compare a flat list of models (no nesting) with ANOVA
#' @details Take care that the underlying model data is equal in the
#' models that are being compared, otherwise this function will fail to compare
#' them
#'
#'  @export

compare_models <- function(model_list){

  anova_on_models <- eval(
    parse(
      text=paste(
        "anova(" ,
        paste("model_list[[" ,1:length(model_list) ,"]]" ,
              sep="" ,
              collapse=",") ,")"
      )
    )
  )
  return(anova_on_models)

}


#' @export

log10_transform <- function(df){

  df <- df %>%
    dplyr::mutate(log_10_conc = log10(concentration+1))

  return(df)
}

## extractor p-value
#' @export

extract_results_from_model <- function(model) {

  model_summary <- summary(model)
  model_df <-  model_summary$tTable %>%
    as.data.frame() %>%
    mutate(param = rownames(.)) %>%
    as_tibble()

  return(model_df)

}

#' @export

plot_residuals_from_model <- function(model ){

  plot <- plot(model, which=1, col=c("blue"))
  return(plot)
}

