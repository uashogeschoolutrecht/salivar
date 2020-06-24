#######################################
## Functions to package salivar
## Author: Marc A.T. Teunis
## September 2019
######################################

#' @title Function to plot histograms from a (nested) dataframe
#' @param df An object of class "data.frame", "tbl", "tbl-df"
#' @export

plot_histogram_from_nested_df <- function(df, var, title){

  histogram <- df %>%
    ggplot2::ggplot(aes(
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
  group_var){

  df_new <- df %>%
    dplyr::mutate("analyte" = group_var)
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

#' @export

print_lines <- function(analyte_x, width, height, palette){

  data_plot <- data_nested$summary_statistics[[analyte_x]]
  plot <- salivar::draw_lines(DF = data_plot,
                              palette_graph = palette,
                              analyte_x = analyte_x,
                              f.width = width, f.height = height)




  return(plot)
}

#' @export
normalize_for_protocol_P1 <- function(df){

  baseline <- df %>%
    dplyr::filter(time == "0")

  mean_baseline <- mean(baseline$concentration, na.rm = TRUE)

  #  df_with_baseline <- df %>%
  #    left_join(
  #      baseline,
  #      by = c('subject', 'time', 'analyte'),
  #      suffix = c('', '.baseline')
  #    )
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
  # Rows without baseline measurement.
  # no_baseline <- df_with_baseline %>%
  #    dplyr::filter(is.na(concentration.baseline))

  # Now, add a column with the normalized concentration that is calculated by
  # dividing the concentration (of a given study, participant etc.) by the
  # corresponding baseline concentration.
  df_normalized <- df %>%
    dplyr::mutate(
      concentration_normalized = concentration/mean_baseline
    )


  return(df_normalized)

}


#' @export
create_normalized_summary <- function(df){

  df_new <- df %>%
    normalize_for_protocol_P1() %>%
    dplyr::select(
      subject, time, protocol, analyte, concentration_normalized
    ) %>%
    dplyr::mutate(concentration = concentration_normalized) %>%
    salivar::summarize_df()

  return(df_new)
}

#' @export

run_pca <- function(df, time_filter, na_filter) {
  df_wide <- df %>%
    salivar::normalize_for_protocol_P1() %>%
    dplyr::select(subject,
                  protocol,
                  time,
                  analyte,
                  concentration_normalized) %>%
    dplyr::filter(time == time_filter) %>%
    tidyr::pivot_wider(names_from = analyte,
                       values_from = concentration_normalized)

  df_wide <- df_wide %>%
    tidyr::unite(col = "keval",
          c(subject, protocol, time),
          remove = FALSE)

  df_wide <- df_wide %>%
    as.data.frame()

  rownames(df_wide) <- df_wide$keval

  ## many missing values, the step  below removes them
  naniar::vis_miss(df_wide)
  names(df_wide)

  ind_isnummeric <- purrr::map_lgl(df_wide, is.numeric)

  df_wide_nummeric <- df_wide[ , ind_isnummeric]

  ## columns with many NAs
  ## test
  #  x = df_wide$`IL-1ß`

  get_percentage_na <- function(x){
    n_na <- sum(is.na(x))
    perc_na <- ((n_na / length(x))*100) %>% round(digits = 2)

    return(perc_na)
  }

  percentage_na <- map_dbl(
    .x = df_wide_nummeric,
    .f = get_percentage_na
  )

  many_na <- percentage_na > na_filter

  df_wide_new <- df_wide_nummeric[ , !many_na]

  naniar::vis_miss(df_wide_new)

  df_wide_new <- df_wide_new %>%
    na.omit()

  pca <-
    prcomp(df_wide_new, center = TRUE, scale. = TRUE)

  summary(pca)

  data_pca <- as.data.frame(pca$x)

  data_pca <- data_pca %>%
    dplyr::mutate(keyval = rownames(.)) %>%
    tidyr::separate(
      col = "keyval",
      into = c("subject", "protocol", "time"),
      remove = FALSE
    )

  plot_pca <- data_pca %>%
    ggplot2::ggplot(aes(x = PC1,
               y = PC2)) +
    ggplot2::geom_point(aes(colour = protocol, shape = time)) +
    ggplot2::theme_bw()

  plot_pca

}


#' @export
get_percentage_na <- function(x){

    n_na <- sum(is.na(x))
    perc_na <- ((n_na / length(x))*100) %>%
    round(digits = 2)

    return(perc_na)
}
