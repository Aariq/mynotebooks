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
df_dict <- function(x = NULL) {
  adc <- rstudioapi::getSourceEditorContext()

  src_txt <- adc$selection[[1]]$text

  if (nzchar(src_txt)) {
    eval(parse(text = sprintf("x <- %s", src_txt)))
    df_name <- src_txt
  } else {
    df_name <- substitute(x)
  }


  if(inherits(x, "data.frame")) {
    col_names <- colnames(x)
    col_types <- sapply(x, class)
    header <- glue::glue("`{df_name}`: <description>\n\n")
    dict <- glue::glue("- `{col_names}` ({col_types}):")
    out <- glue::glue_collapse(c(header, dict), sep = "\n")
  } else if (is.null(x) & !nzchar(src_txt)){
    col_names <- c("col1", "col2", "col3")
    col_types <- c("type", "type", "type")
    header <- glue::glue("`data_frame`: <description>\n\n")
    dict <- glue::glue("- `{col_names}` ({col_types}):")
    out <- glue::glue_collapse(c(header, dict), sep = "\n")
  } else {
    stop("This feature only works for data.frame objects.")
  }
  rstudioapi::insertText(
    text = out,
    location = adc$selection[[1]]$range,
    id = adc$id
  )
}


