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

# TODO Create an S3 object (salivar_model_list) as input for this function
#' Compare a flat list of models (no nesting) with ANOVA
#' @details Take care that the underlying model data is equal in the
#' models that are being compared, otherwise this function will fail to compare
#' them
#'
#' @export

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


#' @export

extract_results_from_model <- function(model, digits, ...) {

  model_summary <- summary(model)
  model_df <-  model_summary$tTable %>%
    as.data.frame() %>%
    mutate(param = rownames(.),
          `p-value` = format(`p-value`, ...),
          `p-value` = as.numeric(`p-value`),
          `p-value` = round(`p-value`, digits = digits)) %>%
    as_tibble()

  return(model_df)

}

#' @export

plot_residuals_from_model <- function(model ){

  plot <- plot(model, which=1, col=c("blue"))
  return(plot)
}

#' @export
add_grouping_value_back_to_list_column_df <- function(
  df,
  group_var,
  name_var){

    df_new <- df %>%
      dplyr::mutate(name_var = group_var)

    return(df_new)
  }


#' @export
set_names_on_list_column <- function(sv_list_column, name_values){
  names(sv_list_column) <- name_values
  return(sv_list_column)
}

#' @export
plot_model_results <- function(salivar_model_result, significance, ...){

  plot_title <- salivar_model_result$name_var %>% unique()
  salivar_model_result$order_param <- c(1:nrow(salivar_model_result))
  signi_data <- salivar_model_result %>%
    dplyr::filter(
      `p-value` < significance
    )

  plot <- salivar_model_result %>%
    ggplot(aes(x = reorder(as_factor(param), order = order_param),
               y = as.numeric(`p-value`))) +
    geom_point() +
    ggtitle(plot_title) +
    coord_flip() +
    geom_hline(yintercept = significance, ...) +
    geom_point(data = signi_data, ...) +
    xlab("Model parameter") +
    ylab("P-value")

  return(plot)
}


