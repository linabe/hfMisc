#' Data from National Patient Registry
#'
#' sos_data aims to mimic data recived from Socialstyrelsen,
#'   used in the examples given in functions prep_sosdata and create_sosvar.
#'
#' @format A data frame with 5500 rows and 8 variables:
#' \describe{
#'   \item{id}{Patient id. Not unique. Integer.}
#'   \item{INDATUM}{Date of visit. Date.}
#'   \item{UTDATUM}{Date of discharge. Date.}
#'   \item{HDIA}{Main diagnosis. Character.}
#'   \item{DIA01}{Character.}
#'   \item{OP01}{Procedure code, position 1. Character.}
#'   \item{OP02}{Procedure code, position 2. Character.}
#'   \item{ekod1}{Character.}
#'   \item{ekod2}{Character.}
#'   \item{AR}{Year of visit date. Numeric.}
#' }
"sos_data"

#' Data from Cause of Death Registry
#'
#' dors_data aims to mimic data recived from Socialstyrelsen,
#'   used in the examples given in functions prep_sosdata and create_deathvar.
#'
#' @format A data frame with 100 rows and 3 variables:
#' \describe{
#'   \item{id}{Patient id. Not unique. Integer.}
#'   \item{ULORSAK}{Underlaying cause of death}
#'   \item{DODSDAT}{Date of death}
#' }
"dors_data"

#' Data from RiksSvikt (SwedeHF)
#'
#' rs_data aims to mimic data from SwedeHF (RiksSvikt),
#'   used in the examples given in functions prep_sosdata, create_sosvar and create_deathvar.
#'
#' @format A data frame with 500 rows and 3 variables:
#' \describe{
#'   \item{id}{Patient id. Not unique. Integer.}
#'   \item{indexdtm}{Index date. Combination id and date is unique. Date.}
#'   \item{deathdtm}{Date of death or 2015-12-31 (censored). Date.}
#' }
"rs_data"

