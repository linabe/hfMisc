#' Prepares SoS data for input into function create_sosvar
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' Prepares raw data recieved from Socialstyrelsen (national patient registry or
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
#' @param impute To be implemented. kom ihåg att kolla om finns värden i INDATUMA
#'   (text) men inte i INDATUM (numerisk). i så fall imputera.
#'
#' @seealso \code{\link{create_sosvar}}
#'
#' @return sosdataset with combined DIA, OP, ekod variables and space
#'   in front of HDIA and sosdtm.
#'
#' @examples
#' \donttest{
#' sos_data <- bind_rows(
#'   sv %>% mutate(source = "SV"),
#'   ov %>% mutate(source = "OV"),
#'   dagkir %>% mutate(source = "DK")
#' )
#' }
#' sos_data <- prep_sosdata(sos_data)
#' @export

prep_sosdata <- function(sosdata,
                         registry = "patreg",
                         utdatum = FALSE,
                         remove = TRUE,
                         impute = TRUE) {
  if (!registry %in% c("patreg", "dors")) stop("registry is not patreg or dors")

  if (registry == "patreg") {
    sosdata <- sosdata %>%
      mutate_at(
        vars(dplyr::matches("DIA|OP|ekod")),
        list(~ tidyr::replace_na(., ""))
      ) %>%
      # create temp HDIA variable (not containing DIA) so is not removed in unite step
      mutate(tmp_H = paste0(" ", HDIA)) %>%
      tidyr::unite(DIA_all, contains("DIA", ignore.case = FALSE),
        sep = " ",
        remove = remove
      ) %>%
      tidyr::unite(OP_all, contains("OP", ignore.case = FALSE),
        sep = " ",
        remove = remove
      ) %>%
      tidyr::unite(ekod_all, contains("ekod", ignore.case = FALSE),
        sep = " ",
        remove = remove
      ) %>%
      # rename HDIA back to org name
      rename(HDIA = tmp_H) %>%
      mutate(
        DIA_all = trimws(paste0(" ", DIA_all), which = "right"),
        OP_all = trimws(paste0(" ", OP_all), which = "right"),
        ekod_all = trimws(paste0(" ", ekod_all), which = "right")
      )
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
      mutate_at(
        vars(dplyr::matches("ORSAK")),
        list(~ tidyr::replace_na(., ""))
      ) %>%
      # create temp ULORSAK variable (not containing ORSAK) so is not removed in unite step
      mutate(tmp_U = paste0(" ", ULORSAK)) %>%
      tidyr::unite(ORSAK_all, contains("ORSAK", ignore.case = FALSE),
        sep = " ",
        remove = remove
      ) %>%
      # rename ULORSAK back to org name
      rename(ULORSAK = tmp_U) %>%
      mutate(
        ORSAK_all = trimws(paste0(" ", ORSAK_all), which = "right")
      )
    sosdata <- sosdata %>%
      filter(!is.na(DODSDAT))
  }
  return(sosdata)
}
