#' Sanitize text
#'
#' Sanitize text for compatability with latex. Used for example when
#'  escape = FALSE needed in kableExtra table (with output latex),
#'  for example when needing footnotes.
#'
#' @param str Character vector to be sanitized.
#'
#' @return The sanitized character vector.
#'
#' @examples
#' var_names <- c(">50", "#Test", "Test1 & Test2")
#' var_names <- sanitize_text(var_names)
#' @export

sanitize_text <- function(str) {
  result <- str
  result <- gsub("\\\\", "SANITIZE.BACKSLASH", result)
  result <- gsub("$", "\\$", result, fixed = TRUE)
  result <- gsub(">", "$>$", result, fixed = TRUE)
  result <- gsub("<", "$<$", result, fixed = TRUE)
  result <- gsub("|", "$|$", result, fixed = TRUE)
  result <- gsub("{", "\\{", result, fixed = TRUE)
  result <- gsub("}", "\\}", result, fixed = TRUE)
  result <- gsub("%", "\\%", result, fixed = TRUE)
  result <- gsub("&", "\\&", result, fixed = TRUE)
  result <- gsub("_", "\\_", result, fixed = TRUE)
  result <- gsub("#", "\\#", result, fixed = TRUE)
  result <- gsub("^", "\\verb|^|", result, fixed = TRUE)
  result <- gsub("~", "\\~{}", result, fixed = TRUE)
  result <- gsub("SANITIZE.BACKSLASH", "$\\backslash$", result, fixed = TRUE)
  result <- gsub("$\\backslash$newline", "\\newline", result, fixed = TRUE)
  return(invisible(result))
}
