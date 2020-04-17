#' Create metadata for a data frame
#'
#' Prompts the user for short descriptions of a dataset as well as descriptions of the data contained in each of its columns.
#'
#' @param df a data frame
#'
#' @return a list
#' @import glue
#' @importFrom purrr map2
#' @export
#'
#' @examples
describe <- function(df){
  foo <- function(x, colname) {
    x_class <- class(x)[1]
    class_abrev <- switch(x_class,
                          "character" = "<chr>",
                          "numeric" ="<dbl>",
                          "integer" = "<int>",
                          "logical" = "<lgl>",
                          "factor" = "<fct>",
                          "Date" = "<date>",
                          "POSIXct" = "<POSIXct>")
    #factors
    if (is.factor(x)) {
      lvls <- levels(x)
      lvl_str <- glue_collapse(lvls, sep = ", ", width = 25, last = ", and ")
      desc <-
        readline(prompt = glue(
          'Enter description for `{colname}` <fctr w/ {length(lvls)} levels>: '
        ))
      details <-
        readline(prompt = glue("Describe how to interpret the levels ({lvl_str}): "))

      #posixct date times
    } else if (inherits(x, "POSIXct")) {
      desc <- readline(prompt = glue("Enter description for `{colname}` {class_abrev}: "))
      tz <- tz(x)
      if (tz != "")
        ans <-
        menu(
          title = glue("Is '{tz(x)}' the correct time zone for `{colname}`?"),
          choices = c("Yes", "No")
        )
      if(ans == "No" | tz == "") {
        tz <- readline(prompt = "Enter a timezone for `{colname}`: ")
      }
      details <- glue("time zone: {tz}")

      #double
    } else if (inherits(x, "numeric")) {
      desc <- readline(prompt = glue("Enter description for `{colname}` {class_abrev}: "))
      u <- readline(prompt = glue("What are the units for {colname}? "))
      details <- glue("Units: {u}")

      #everythign else
    } else {
      desc <- readline(prompt = glue("Enter description for `{colname}` {class_abrev}: "))
      details <- NULL
    }
    return(list(desc = desc, details = details))
  }
  cat("Enter a description for this dataset.\n Include details such as the when, where, how, why, and by whom it was collected: ")
  gen_desc <- readline(prompt = "Description: ")
  col_desc <- map2(df, colnames(df), ~foo(.x, .y))
  return(list(gen_desc = gen_desc, col_desc = col_desc))
}
