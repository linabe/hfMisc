#' Create event for competing risk analysis (cmprsk::crr)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param event Event of interest.
#' @param compevent Competing event (assumed to occur after event of interest).
#' @param eventvalues Vector with values indicating event in event and compevent (in that order).
#'
#' @return Competing event variable where 1 = event, 2 = competing event, 0 = censoring.
#'
#' @examples
#' suppressMessages(library(dplyr, quietly = TRUE), classes = "warnings")
#' rs_data <- rs_data %>% mutate(event_cr = create_crevent(out_hosphf, out_death_fac, eventvalues = c(1, "Yes")))
#' @import dplyr
#' @export

create_crevent <- function(event, compevent, eventvalues = c("Yes", "Yes")) {
  outvarcr <- case_when(
    is.na(event) | is.na(compevent) ~ NA_real_,
    event == eventvalues[1] ~ 1,
    compevent == eventvalues[2] ~ 2,
    TRUE ~ 0
  )
}
