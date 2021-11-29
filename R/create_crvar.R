#' Create numeric dummy xvars for competing risk analysis (cmprsk::crr)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param data Data.
#' @param var String containing variable that should be numeric and dummified.
#' @param forcenum Should variables that are not character/factor be dummified? Default is FALSE.
#'
#' @return Data with a numeric xvars x levels - 1.
#'
#' @examples
#' rs_data <- create_crvar(rs_data, "xvar_4_num", forcenum = TRUE)
#' @import dplyr
#' @export


create_crvar <- function(data, var, forcenum = FALSE) {
  if (!class(data %>% pull(!!sym(var))) %in% c("character", "factor") & !forcenum) {
    warning("Variable is not a character or factor and will not be returned. Use forcenum = TRUE to proceed")
  } else {
    levs <- levels(data %>% mutate(!!sym(var) := factor(!!sym(var))) %>% pull(!!sym(var)))

    namelevs <- gsub(" ", "", levs)
    namelevs <- gsub(">=|>", "over", namelevs)
    namelevs <- gsub("<=|<", "under", namelevs)
    namelevs <- gsub("%", "", namelevs)
    namelevs <- gsub("&", "", namelevs)
    namelevs <- gsub("/", "", namelevs)
    namelevs <- gsub("\\+", "p", namelevs)
    namelevs <- gsub("\\-", "m", namelevs)

    for (i in 2:length(levs)) {
      var2 <- paste0(var, "_cr_", gsub(" ", "", namelevs[i]))

      data <- data %>%
        mutate(!!sym(var2) := case_when(
          is.na(!!sym(var)) ~ NA_real_,
          !!sym(var) == levs[i] ~ 1,
          TRUE ~ 0
        ))
    }
  }
  return(data)
}
