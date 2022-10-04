#' Calculates medicines from the National Prescribed Drug Register
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Calculates medicines from ATC from the National Prescribed Drug Register.
#'
#'
#' @param atc String of ATC as a regular expression defining the medicine. Default is ATC.
#' @param medvar Name of variable where ATC are found.
#' @param medname Name of resulting variable
#'   (with prefix sos_lm_).
#' @param cohortdata Data to add medvar to.
#' @param meddata Data where ATC codes are found.
#' @param id Vector of post identifier (meds for each post). Default is lopnr.
#' @param valsclass optional argument specifying the class of the
#'   meds. Allowed values are "num" (numeric: 1, 0),
#'   "char" (character: "Yes", "No"), "fac" (factor: "Yes", "No").
#'   Default is "num".
#' @param fromdate Optional date where post with a indexdate prior to this are set to NA (e.g. prior to start of DDR + x months).
#' @param indexdate If fromdate is given, variable were indexdate is found.
#' @param metatime For what time span are the meds selected in data? Printed in metalm. Default text is "-5mo-14days".
#'
#' @return cohortdata with column containing medicine.
#'   Also dataset metalm that writes directly to global enviroment
#'   with information on constructed variables.
#'
#'
#' @examples
#'
#' \dontrun{
#' lmtmp <- left_join(
#'   rsdata %>%
#'     select(LopNr, shf_indexdtm),
#'   lmsel,
#'   by = "LopNr"
#' ) %>%
#'   mutate(diff = as.numeric(EDATUM - shf_indexdtm)) %>%
#'   filter(diff >= -30.5 * 5, diff <= 14) %>%
#'   select(LopNr, shf_indexdtm, EDATUM, ATC)
#'
#' rsdata <- create_medvar(atc = "^C07A", medname = "bbl", cohortdata = rsdata, meddata = lmtmp, id = c("LopNr", "shf_indexdtm"))
#' }
#'
#' @import dplyr
#' @export

create_medvar <- function(atc,
                          medvar = ATC,
                          medname,
                          cohortdata,
                          meddata,
                          id = "lopnr",
                          valsclass = "num",
                          fromdate,
                          indexdate,
                          metatime = "-5mo-14days") {
  if (anyDuplicated(cohortdata %>% select(!!!syms(id))) > 0) {
    stop(paste0("id is not unique. Supply additional id variable."))
  }

  medvar <- enquo(medvar)
  if (!missing(indexdate)) indexdate <- enquo(indexdate)

  medname <- paste0("sos_lm_", medname)

  if (any(rlang::has_name(cohortdata, medname))) {
    warning(paste0(
      medname, " already exists in cohortdata and is overwritten."
    ))
    cohortdata <- cohortdata %>%
      select(-!!medname)
  }

  tmp_data <- meddata %>%
    mutate(
      atcneed = stringr::str_detect(!!medvar, atc)
    ) %>%
    filter(atcneed) %>%
    select(-atcneed, -!!medvar)

  tmp_data <- tmp_data %>%
    group_by(!!!syms(id)) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(!!medname := 1) %>%
    select(!!!syms(id), !!sym(medname))

  out_data <- left_join(
    cohortdata,
    tmp_data,
    by = id
  ) %>%
    mutate(
      !!medname := tidyr::replace_na(!!sym(medname), 0)
    )

  if (!missing(fromdate)) {
    out_data <- out_data %>%
      mutate(!!medname := if_else(!!indexdate < ymd(fromdate), NA_real_, !!sym(medname)))
  }

  if (valsclass == "char") {
    out_data <- out_data %>%
      mutate(!!medname := if_else(!!sym(medname) == 1, "Yes", "No"))
  }
  if (valsclass == "fac") {
    out_data <- out_data %>%
      mutate(!!medname := factor(!!sym(medname), levels = 0:1, labels = c("No", "Yes")))
  }

  # metadata
  atc <- stringr::str_replace_all(atc, " ", "")
  atc <- stringr::str_replace_all(atc, "\\[", "")
  atc <- stringr::str_replace_all(atc, "\\]", "")
  atc <- stringr::str_replace_all(atc, "\\|", ",")
  atc <- stringr::str_replace_all(atc, "\\^", "")
  atc <- stringr::str_replace_all(atc, "\\(\\?!", " (excl. ")
  atc <- stringr::str_replace_all(atc, "\\(", "")
  atc <- stringr::str_replace_all(atc, "\\)", "")

  metatmp <- data.frame(medname, atc, "Dispensed Drug Registry", metatime)
  colnames(metatmp) <- c("Variable", "ATC", "Register", "Period")

  if (exists("metalm")) {
    metalm <<- rbind(metalm, metatmp) # global variable, writes to global env
  } else {
    metalm <<- metatmp # global variable, writes to global env
  }

  return(out_data)
}
