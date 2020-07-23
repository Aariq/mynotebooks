#' Insert data dictionary
#'
#' @param x a data frame
#'
#' @return Inserts a data dictionary template as a markdown list at the cursor
#' @import rstudioapi
#' @import glue
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df_dict(warpbreaks)
#' }
df_dict <- function(x) {
  adc <- rstudioapi::getSourceEditorContext()
  if(inherits(x, "data.frame")) {
    col_names <- colnames(x)
    col_types <- sapply(x, class)
    header <- glue::glue("`{substitute(x)}`: description\n\n")
    dict <- glue::glue("- `{col_names}` ({col_types}):")
    out <- glue::glue_collapse(c(header, dict), sep = "\n")
  } else {
    stop("df_dict() currently only works on data frames")
  }
  rstudioapi::insertText(
    text = out,
    location = adc$selection[[1]]$range,
    id = adc$id
  )
}
