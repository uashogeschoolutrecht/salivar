#' Render report of full EDA and statistical analysis salivar_data
#' @export

render_report <- function(){

rmarkdown::render(
  input = here::here(
    "vignettes",
    "statistical_analysis_complete.Rmd"
  ),
  output_file = here::here(
    "vignettes",
    "statistical_analysis_complete.html"
  )
)

}
