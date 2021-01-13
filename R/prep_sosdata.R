#' Prepares SoS data for input into function create_sosvar
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' Prepares raw data received from Socialstyrelsen (national patient registry or
#'   cause of death registry) to be processed by function make_sosvar. Removes
#'   post with missing information on relevant dates and joins diagnosis columns
#'   together. Reason for preparation is to save space in
#'   internal memory (by joining columns together) and removing surplus columns.
#' usethis::use_lifecycle_badge("stable")
#'
#' @param sosdata Data from Socialstyrelsen that should be prepared.
#'   Should contain at least HDIA, OP, ekod, INDATUM
#' @param registry What registry is being prepared?
#'   Takes values patreg (National patient registry) or dors
#'   (Cause of death registry). Default is National patient registry.
#' @param utdatum Should the sosdtm (usually to be used in comparison with indexdate)
#'   be date of discharge for hospitalisation? Default is FALSE.
#'   Else date of admission is used.
#' @param remove Should the individual DIA (except HDIA), OP, ekod variables
#'   be removed after constructing combined variable (to save space)?
#'   Default is TRUE.
#' @param impute If patreg = "dors" imputes missing month and date in DODSDAT.
#'   Possibly to be implemented in patreg. kom ihåg att kolla om finns värden i INDATUMA
#'   (text) men inte i INDATUM (numerisk). i så fall imputera.
#' @param diavar The starting characters of variables containing diagnosis (used if registry = patreg).
#' @param opvar The starting characters of variables containing operations (used if registry = patreg).
#' @param evar The starting characters of variables containing ekod (used if registry = patreg).
#' @param orsakvar The starting characters of variables containing causes of death (used if registry = dors).
#' @param mainvar The main variable that should be saved in the data even if remove = TRUE.
#'
#' @seealso \code{\link{create_sosvar}}
#'
#' @return sosdataset with combined DIA, OP, ekod variables and space
#'   in front of HDIA and sosdtm.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' sos_data <- bind_rows(
#'   sv %>% mutate(source = "SV"),
#'   ov %>% mutate(source = "OV"),
#'   dagkir %>% mutate(source = "DK")
#' )
#' }
#' sos_data <- prep_sosdata(sos_data, evar = "ekod")
#' dors_data <- prep_sosdata(dors_data, registry = "dors", impute = FALSE)
#' @export

prep_sosdata <- function(sosdata,
                         registry = "patreg",
                         utdatum = FALSE,
                         remove = TRUE,
                         impute = TRUE,
                         diavar = c("HDIA", "DIA"),
                         opvar = "OP",
                         evar = "EKOD",
                         orsakvar = c("ULORSAK", "MORSAK"),
                         mainvar = ifelse(registry == "patreg", "HDIA", "ULORSAK")) {
  if (!registry %in% c("patreg", "dors")) stop("registry must be either patreg or dors")

  if (registry == "patreg") {
    sosdata <- sosdata %>%
      mutate_at(
        vars(dplyr::starts_with(c(diavar, opvar, evar))),
        list(~ tidyr::replace_na(., ""))
      )
    if (remove) {
      sosdata <- sosdata %>%
        # create temp HDIA variable (not containing DIA) so is not removed in unite step
        mutate(tmp_mainvar = paste0(" ", !!sym(mainvar)))
    }
    sosdata <- sosdata %>%
      tidyr::unite(DIA_all, starts_with(diavar, ignore.case = FALSE),
        sep = " ",
        remove = remove
      ) %>%
      tidyr::unite(OP_all, starts_with(opvar, ignore.case = FALSE),
        sep = " ",
        remove = remove
      ) %>%
      tidyr::unite(ekod_all, starts_with(evar, ignore.case = FALSE),
        sep = " ",
        remove = remove
      ) %>%
      mutate(
        DIA_all = paste0(" ", stringr::str_squish(DIA_all)),
        OP_all = paste0(" ", stringr::str_squish(OP_all)),
        ekod_all = paste0(" ", stringr::str_squish(ekod_all))
      )
    if (remove) {
      sosdata <- sosdata %>%
        # rename HDIA back to org name
        rename(!!mainvar := tmp_mainvar)
    }

    if (utdatum) {
      sosdata <- sosdata %>%
        mutate(
          # saknas datum Sahlgrenska dagkirurgi 1999 och Arvika, Torsby 2009
          # oppenvard så imp med 30 juni år
          sosdtm = coalesce(UTDATUM, INDATUM, lubridate::ymd(paste0(AR, "-06-30")))
        )
    } else {
      sosdata <- sosdata %>%
        mutate(
          # saknas datum Sahlgrenska dagkirurgi 1999 och Arvika, Torsby 2009
          # oppenvard så imp med 1 jan år
          sosdtm = coalesce(INDATUM, lubridate::ymd(paste0(AR, "-06-30")))
        )
    }
    sosdata <- sosdata %>%
      filter(!is.na(sosdtm))
  }

  if (registry == "dors") {
    sosdata <- sosdata %>%
      filter(!is.na(DODSDAT)) %>%
      mutate_at(
        vars(dplyr::starts_with(orsakvar)),
        list(~ tidyr::replace_na(., ""))
      )
    if (remove) {
      sosdata <- sosdata %>%
        # create temp ULORSAK variable (not containing ORSAK) so is not removed in unite step
        mutate(tmp_mainvar = paste0(" ", !!sym(mainvar)))
    }
    sosdata <- sosdata %>%
      tidyr::unite(ORSAK_all, starts_with(orsakvar, ignore.case = FALSE),
        sep = " ",
        remove = remove
      ) %>%
      mutate(
        ORSAK_all = paste0(" ", stringr::str_squish(ORSAK_all))
      )
    if (remove) {
      sosdata <- sosdata %>%
        # rename ULORSAK back to org name
        rename(!!mainvar := tmp_mainvar)
    }
    if (impute) {
      sosdata <- sosdata %>%
        mutate(sos_deathdtm = lubridate::ymd(case_when(
          substr(DODSDAT, 5, 8) == "0000" ~ paste0(substr(DODSDAT, 1, 4), "0701"),
          substr(DODSDAT, 7, 8) == "00" ~ paste0(substr(DODSDAT, 1, 6), "15"),
          TRUE ~ DODSDAT
        )))
    }
  }
  return(sosdata)
}
