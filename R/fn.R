#' Format numbers (including p-values)
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' @param x Number that should be formatted.
#' @param dig Number of digits.
#' @param p is x a p-value? If <0.x1 will then format to <0.x1.
#' @param pequalsign if x is a p-value should = be placed in front if not <0.x1
#' @param ... Additional arguments to formatC.
#'
#' @return The formatted number.
#'
#' @examples
#' x <- 1.234
#' fn(x)
#' @export

fn <- function(x, dig = 0, p = FALSE, pequalsign = FALSE, ...) {
  out <- formatC(x, format = "f", digits = dig, big.mark = "", ...)
  if (p) {
    out <- replace(
      out,
      out == paste0("0.", paste0(rep(0, dig), collapse = "")), paste0("<0.", paste0(rep(0, max(dig - 1), 0), collapse = ""), "1")
    )
    if (pequalsign) {
      out <- dplyr::if_else(stringr::str_detect(out, "<", negate = TRUE), paste0("=", out), out)
    }
  }
  return(out)
}
