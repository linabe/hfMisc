#' Preferred default values for creating table with kableExtra
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' @param tab Number that should be formatted.
#' @param font_size A numeric input for table font size.
#' @param scale_down Is useful for super wide table. It will automatically adjust the table to page width.
#' @param row.names Logical: whether to include row names. Default is FALSE.
#' @param ... Additional arguments to knitr::kable.
#'
#' @return The kabletable.
#'
#' @examples
#' tab <- tibble::tibble(a = c(2, 3, 4), b = c(5, 6, 7))
#' default_kable(tab)
#' @import dplyr
#' @export

default_kable <- function(tab, font_size = NULL, scale_down = TRUE, row.names = FALSE, ...) {
  parms <- list(...)

  if (any(names(parms) == "longtable")) {
    latexoptions <- c("striped", "hold_position", "repeat_header")
  } else {
    if (scale_down) {
      latexoptions <- c("striped", "hold_position", "scale_down")
    } else {
      latexoptions <- c("striped", "hold_position")
    }
  }

  knitr::kable(tab,
    booktabs = TRUE,
    linesep = "",
    row.names = row.names,
    align = c(rep("l", ncol(tab))),
    ...
  ) %>%
    kableExtra::kable_styling(
      latex_options = latexoptions,
      font_size = font_size, full_width = F
    )
}
