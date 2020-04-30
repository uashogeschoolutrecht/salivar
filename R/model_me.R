#' @title Switch between statistical models by entering a type and formula
#' @description This function is a switch function allowing you to explore
#' all studied models in this package. Currently 'lm', 'gls' and 'lme'
#' models are implemented
#' @param df A dataframe
#' @param formula An R formula or model specification, using the expression
#' form dependent_variable ~ predictors
#' @param type One of type 'lm', 'gls' or 'lme'
#' @details The structure for lme is the most intricate a flexible, the model
#' formula for the fixed structure of the model can be entered as an R fomula
#' as stated under 'formula'. The random effects can be added to the model. See
#' ?nlme::lme  for more details (@seealso package {nlme})
#' @export

model_me <- function(df, formula, type, ...){

  switch(type,
         lm = {
           mod <- lm(data = df, formula = formula)
         },

         gls = {
           mod <- gls(data = df, model = formula, ...)
         },

         lme = {
           mod <- do.call(
             "lme", args = list(data = df, fixed = formula, ...)
             )
         },

         stop("Unvalid model selection, did you enter one of the following types:
              'lm', 'gls' or 'lme'")
  )
  return(mod)
}
