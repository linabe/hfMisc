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
<<<<<<< HEAD
      out == paste0("0.", paste0(rep(0, dig), collapse = "")), paste0("<0.", paste0(rep(0, max(dig - 1), 0), collapse = ""), "1")
=======
      out == paste0("0.", paste0(rep(0, dig), collapse = "")), paste0("<0.", paste0(rep(0, min(dig - 1), 0), collapse = ""), "1")
>>>>>>> df1c11aa4502c10201460ae1f8ea85bf76b7a0ed
    )
    if (pequalsign) {
      out <- dplyr::if_else(stringr::str_detect(out, "<", negate = TRUE), paste0("=", out), out)
    }
  }
  return(out)
}
