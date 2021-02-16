#' Format numbers (including p-values)
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' @param x Number that should be formatted.
#' @param dig Number of digits.
#' @param p is x a p-value? If 0.000 will then format to <0.001.
#' @param ... Additional arguments to formatC.
#'
#' @return The formatted number.
#'
#' @examples
#' x <- 1.234
#' fn(x)
#' @export

fn <- function(x, dig = 0, p = FALSE, ...) {
  out <- formatC(x, format = "f", digits = dig, big.mark = "", ...)
  if (p) {
    out <- replace(
      out,
      out == paste0("0.", paste0(rep(0, dig), collapse = "")), "<0.001"
    )
  }
  return(out)
}
