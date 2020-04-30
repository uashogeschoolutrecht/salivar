#' Name all list columns in a nested dataframe on the basis of a column index
#'
#' @export
#'
#' TODO complete function, doe not work yet

#df_nested = data_nested
#group_col = "analyte"

name_list_columns <- function(df_nested, group_col){

   index <- map_lgl(df_nested, is.list)
   names <- df_nested[, group_col]


    to_be_named <- df_nested[,index]

    map(to_be_named, set_names, names)


    set_names(to_be_named, names)

  }

  }



}
