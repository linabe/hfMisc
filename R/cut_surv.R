#' Limit event and survival times to a time point
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param data Data.
#' @param event Event that should be cut.
#' @param time Time that should be cut. See also cuttime.
#' @param at At what time point should event/time be cut? In units of time.
#' @param cuttime Should time be cut? (can be needed for later evaluation). Default is TRUE.
#' @param rename Suffix if event and time should be renamed. Default is not to rename.
#' @param censval Value of censoring in variable event.
#'   If not given the function will use 0 if event is numeric or "No" if event is character or factor if these exist in event.
#'
#' @return Data with cut event and time variable.
#'
#' @examples
#' rs_data <- cut_surv(rs_data, out_death_fac, outtime_death, at = 365, rename = "1yr")
#' @import dplyr
#' @export

cut_surv <- function(data, event, time, at, cuttime = TRUE, rename = NULL, censval = NULL) {
  event <- enquo(event)
  time <- enquo(time)

  if (is.null(censval)) {
    if (class(data %>% pull(!!event)) %in% c("factor", "character")) {
      if (!all(levels(factor(data %>% pull(!!event))) %in% c("No", "Yes"))) {
        stop("event is not No, Yes and no censval is given.")
      }
    } else if (class(data %>% pull(!!event)) %in% c("numeric", "integer")) {
      if (!all(levels(factor(data %>% pull(!!event))) %in% c("0", "1"))) {
        stop("event is not 0, 1 and no censval is given.")
      }
    } else {
      stop(paste0("event is ", class(data %>% pull(!!event)), " and no censval is given."))
    }
  }

  if (!is.null(rename)) {
    event2 <- paste0(rlang::as_name(event), rename)
    if (cuttime) time2 <- paste0(rlang::as_name(time), rename)
  } else {
    event2 <- paste0(rlang::as_name(event))
    time2 <- paste(rlang::as_name(time))
  }

  if (class(data %>% pull(!!event)) %in% c("factor", "character")) {
    censval <- if_else(is.null(censval), "No", censval)
    data <- data %>%
      mutate(
        !!event2 := if_else(!!time >= at, censval, as.character(!!event))
      )
  }
  else if (class(data %>% pull(!!event)) %in% c("numeric", "integer")) {
    censval <- if_else(is.null(censval), 0, censval)
    data <- data %>%
      mutate(
        !!event2 := if_else(!!time >= at, censval, !!event)
      )
  } else {
    data <- data %>%
      mutate(
        !!event2 := if_else(!!time >= at, censval, !!event)
      )
  }

  if (cuttime) {
    data <- data %>%
      mutate(!!time2 := if_else(!!time >= at, at, !!time))
  }
  return(data)
}
