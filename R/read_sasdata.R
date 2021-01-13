#' Read sas data
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("maturing")}
#'
#' Read sas data, clean and remove labels and formats
#'
#' @param path The path to the dataset.
#' @param filename The filename (without extension).
#' @param years Optional. If dataset from different yeas (for example from LISA)
#'   bind together.
#' @param checkdups Should check for duplicates? Default is FALSE.
#' @param id If duplicates should be checked, identifier that should be unique.
#'   Should be character ("LopNr")
#' @param clean Should the dataset be cleaned from spaces and special characters
#'   in character columns? Default is TRUE.
#' @param removeformats Should sas formats be removed? Default is TRUE.
#' @param removelabels Should sas labels be removed? Default is TRUE.
#'
#' @return A dataset.
#'
#' @examples
#' \dontrun{
#' antalbarn <- read_sasdata(path = scbpath, filename = "lb_lev_antal_barn", years = 1999:2018, clean = FALSE, checkdups = TRUE, id = "LopNr")
#' }
#' @import dplyr
#' @export

read_sasdata <- function(path, filename, years,
                         checkdups = FALSE, id,
                         clean = TRUE,
                         removeformats = TRUE, removelabels = TRUE) {
  if (checkdups & missing(id)) {
    stop("Need id if should check for duplicates.")
  }

  if (missing(years)) {
    outdata <- haven::read_sas(paste0(path, filename, ".sas7bdat"))
    if (checkdups) dups <- anyDuplicated(outdata %>% pull(!!sym(id)))
  } else {
    for (y in years) {
      print(y)
      tmp <- haven::read_sas(paste0(path, filename, "_", y, ".sas7bdat"))
      tmp$year <- y
      if (y == min(years)) {
        outdata <- tmp
      } else {
        outdata <- bind_rows(outdata, tmp)
      }
    }
    if (checkdups) dups <- anyDuplicated((outdata %>% pull(!!sym(id), "year")))
  }
  if (checkdups) {
    if (dups > 0) cat("Duplicates, please check") else cat("No duplicates")
    if (any(is.na(outdata %>% pull(!!sym(id))))) cat("Missing id, please check") else cat("No missing id")
  }
  if (removeformats) outdata <- haven::zap_formats(outdata)
  if (removelabels) outdata <- haven::zap_label(outdata)
  if (clean) {
    outdata <- clean_data(outdata)
  }
  return(outdata)
}
