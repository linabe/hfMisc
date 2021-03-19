#' Calculates comorbidities and outcomes from SoS data
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Calculates comorbidities and outcomes (including time to for the latter)
#'   from DIA, OP and ekod variables from National Patient Registry.
#'
#'
#' @param type Possible values are "out" and "com".
#'   Is the resulting variable an outcome (and time to is calculated)
#'   or comorbidity?
#' @param name Name of resulting variable
#'   (prefix sos_out_ is added to outcome and sos_com_ to comorbidity).
#' @param sosdata Data where DIA, OP, Ekod variables are found.
#' @param cohortdata Data containing the cohort with at least columns
#'   patid and indexdate.
#' @param patid Patient identifier. Default is lopnr.
#' @param indexdate Index date for the patient (usually date of admission or
#'   discharge entered into SwedeHF). Default is indexdtm.
#' @param add_unique If patid and indexdate are not unique in cohortdata, this
#'   identifies unique posts.
#' @param sosdate Date of incident (comorbidity or outcome)
#'   in sosdata. Default is sosdtm.
#' @param censdate Vector of dates. Only applicable for type = "out". Outcomes are
#'   allowed up until this day (usually date of death or end follow-up).
#' @param allowedcenslimit Optional single value with allowed time unit between
#'   sosdate and censdate that still makes the sosdate be a valid event.
#'   Used for example when patients who are discharged day after death
#'   should still be counted as an event.
#'   Can cause unexpected results if the sos_data contains information > than
#'   last follow-up.
#' @param noof Should comorbs/outcomes be number of or first? Default it first (FALSE).
#'   No time to is calculated for noof = TRUE and type = "out".
#' @param comduration Should duration of the comorbidity be calculated (TRUE). Default is FALSE.
#' @param starttime If type = "out" amount of time (in days) AFTER indexdate
#'   to start counting outcomes. If type = "com" amount of time (in days)
#'   PRIOR to indexdate. Indexdate = 0, all values prior to
#'   indexdate are negative. Default is 1 ("out") and 0 ("com").
#' @param stoptime If type = "out" amount of time (in days) AFTER indexdate to
#'   be counted. Note that censdate will be adjusted to stoptime.
#'   If type = "com" amount of time (in days) PRIOR to indexdate.
#'   Indexdate = 0, all values prior to indexdate are negative.
#'   Default is any time prior to starttime is considered a comorbidity
#'   and any time after starttime is considered an outcome.
#' @param diakod String of icd codes as a regular expression
#'   defining the outcome/comorbidity. Should start
#'   with blank space " ". Also see diakodneg.
#' @param opkod String of procedure codes as a regular expression
#'   defining the outcome/comorbidity. Should start
#'   with blank space " ". Also see opkodneg.
#' @param ekod String of e codes as a regular expression defining
#'   the outcome/comorbidity. Should start
#'   with blank space " ". Also see ekodneg.
#' @param diakodneg Should return non-matching codes in diakod? Default is FALSE.
#' @param opkodneg Should return non-matching codes in opkod? Default is FALSE.
#' @param ekodneg Should return non-matching codes in ekod? Default is FALSE.
#' @param diavar Column where diakod is found. All codes should start
#'   with blank space " ". Default is DIA_all.
#' @param opvar Column where opkod is found. All codes should start
#'   with blank space " ". Default is OP_all.
#' @param evar Column where ekod is found. All codes should start
#'   with blank space " ". Default is ekod_all.
#' @param valsclass optional argument specifying the class of the
#'   comorbidities/outcomes. Allowed values are "num" (numeric: 1, 0),
#'   "char" (character: "yes", "no"), "fac" (factor: "yes", "no").
#'   Default is "num".
#' @param meta_reg Optional argument specifying registries used. Printed in
#'   metadatatable. Default is
#'   "NPR (in+out)".
#' @param meta_pos Optional argument specifying positions used to search for
#'   outcomes/comorbidities. Printed in metadatatable.
#' @param warnings Should warnings be printed. Default is FALSE.
#'
#' @seealso \code{\link{prep_sosdata}}
#' @seealso \code{\link{create_deathvar}}
#'
#' @return dataset with column containing comorbidity/outcome.
#'   Also dataset metaout that writes directly to global enviroment
#'   with information on constructed variables.
#'
#'
#' @examples
#'
#' sos_data <- prep_sosdata(sos_data, evar = "ekod")
#'
#' rs_data <- create_sosvar(
#'   sosdata = sos_data,
#'   cohortdata = rs_data,
#'   patid = id,
#'   indexdate = indexdtm,
#'   sosdate = sosdtm,
#'   type = "com",
#'   name = "cv1y",
#'   diakod = " I",
#'   stoptime = -365.25,
#'   warnings = FALSE
#' )
#' @import dplyr
#' @export

create_sosvar <- function(type,
                          name,
                          sosdata,
                          cohortdata,
                          patid = lopnr,
                          indexdate = indexdtm,
                          add_unique,
                          sosdate = sosdtm,
                          censdate,
                          allowedcenslimit = 0,
                          noof = FALSE,
                          comduration = FALSE,
                          starttime = ifelse(type == "com", 0, 1),
                          stoptime,
                          diakod,
                          opkod,
                          ekod,
                          diakodneg = FALSE,
                          opkodneg = FALSE,
                          ekodneg = FALSE,
                          diavar = DIA_all,
                          opvar = OP_all,
                          evar = ekod_all,
                          valsclass = "num",
                          meta_reg =
                            "NPR (in+out)",
                          meta_pos,
                          warnings = TRUE) {
  patid <- enquo(patid)
  indexdate <- enquo(indexdate)
  sosdate <- enquo(sosdate)
  if (!missing(censdate)) censdate <- enquo(censdate)
  if (!missing(add_unique)) add_unique <- enquo(add_unique)

  diavar <- enquo(diavar)
  opvar <- enquo(opvar)
  evar <- enquo(evar)

  # check input arguments ok
  if (!rlang::has_name(sosdata, rlang::as_name(patid))) {
    stop(paste0(rlang::as_label(patid), " does not exist in sosdata"))
  }

  if (!rlang::has_name(cohortdata, rlang::as_name(patid))) {
    stop(paste0(rlang::as_label(patid), " does not exist in cohortdata"))
  }

  if (!rlang::has_name(cohortdata, rlang::as_name(indexdate))) {
    stop(paste0(rlang::as_label(indexdate), " does not exist in cohortdata"))
  }

  if (!rlang::has_name(sosdata, rlang::as_name(sosdate))) {
    stop(paste0(rlang::as_label(sosdate), " does not exist in sosdata"))
  }

  if (any(rlang::has_name(sosdata, names(cohortdata)[!names(cohortdata) == rlang::as_name(patid)]))) {
    if (warnings) {
      warning(paste0(
        "cohortdata and sosdata have overlapping columns. Only ",
        rlang::as_label(patid), " should be the same. This might cause unexpected results."
      ))
    }
  }

  if (!type %in% c("out", "com")) {
    stop("type should be either 'out' (outcome) or 'com' (comorbidity).")
  }

  if (type == "out" & missing(censdate)) {
    stop("censdate is needed for variables of type out.")
  }

  if (missing(diakod) & missing(opkod) & missing(ekod)) {
    stop("Either dia, op or ekod must be specified.")
  }

  name2 <- paste0("sos_", type, "_", name)
  if (type == "out") timename2 <- paste0("sos_outtime_", name)
  if (type == "com" & comduration) timename2 <- paste0("sos_comdur_", name)

  if (any(rlang::has_name(cohortdata, name2))) {
    if (warnings) {
      warning(paste0(
        name2, " already exists in cohortdata. This might cause unexpected results."
      ))
    }
    cohortdata <- cohortdata %>%
      select(-!!enquo(name2))
  }

  if (type == "com" & !missing(stoptime)) {
    if (stoptime > 0) {
      if (warnings) {
        warning("stoptime for comorbidity is not negative.")
      }
    }
  }

  if ((type == "out" | noof) & comduration) {
    if (warnings) {
      warning("comduration does not have an effect if type = out or noof = TRUE")
    }
  }

  if (!any(duplicated(cohortdata %>% select(!!patid)))) {
    groupbyvars <- rlang::as_name(patid)
  } else {
    if (!any(duplicated(cohortdata %>% select(!!patid, !!indexdate)))) {
      groupbyvars <- c(rlang::as_name(patid), rlang::as_name(indexdate))
      if (warnings) {
        warning(paste0(
          rlang::as_name(patid),
          " is not unique in cohortdata. Output data will be for unique ",
          rlang::as_name(patid), " and ", rlang::as_name(indexdate), "."
        ))
      }
    } else {
      if (!missing(add_unique)) {
        groupbyvars <- c(rlang::as_name(patid), rlang::as_name(indexdate), rlang::as_name(add_unique))
      } else {
        stop(paste0(
          rlang::as_name(patid), " and ", rlang::as_name(indexdate),
          " are not unique in cohortdata. Supply additional
          unique column in argument add_unique (for example postnr)."
        ))
      }
    }
  }

  tmp_data <- inner_join(cohortdata %>%
    select(!!patid, !!indexdate, !!!syms(groupbyvars)),
  sosdata,
  by = rlang::as_name(patid)
  ) %>%
    mutate(difft = difftime(!!sosdate, !!indexdate,
      units = "days"
    ))


  if (type == "out") tmp_data <- tmp_data %>% dplyr::filter(difft >= starttime)
  if (type == "com") tmp_data <- tmp_data %>% dplyr::filter(difft <= starttime)

  if (!missing(stoptime)) {
    if (type == "out") tmp_data <- tmp_data %>% dplyr::filter(difft <= stoptime)
    if (type == "com") tmp_data <- tmp_data %>% dplyr::filter(difft >= stoptime)
  }

  if (!missing(diakod)) {
    tmp_data <- tmp_data %>%
      mutate(name_dia = stringr::str_detect(!!diavar, diakod, negate = diakodneg))
  }
  if (!missing(opkod)) {
    tmp_data <- tmp_data %>%
      mutate(name_op = stringr::str_detect(!!opvar, opkod, negate = opkodneg))
  }
  if (!missing(ekod)) {
    tmp_data <- tmp_data %>%
      mutate(name_ekod = stringr::str_detect(!!evar, ekod, negate = ekodneg))
  }

  tmp_data <- tmp_data %>%
    mutate(!!name2 := ifelse(rowSums(dplyr::select(., contains("name_"))) > 0,
      1, 0
    ))

  if (!noof) {
    tmp_data <- tmp_data %>%
      filter(!!(sym(name2)) == 1) %>%
      group_by(!!!syms(groupbyvars)) %>%
      arrange(!!sosdate) %>%
      slice(1) %>%
      ungroup()

    if (type == "com") {
      out_data <- left_join(
        cohortdata,
        tmp_data %>% dplyr::select(!!!syms(groupbyvars), !!name2, !!sosdate),
        by = groupbyvars
      ) %>%
        mutate(!!name2 := tidyr::replace_na(!!sym(name2), 0))

      if (comduration) {
        out_data <- out_data %>%
          mutate(!!timename2 := as.numeric(!!indexdate + starttime - !!sosdate))
      }

      out_data <- out_data %>%
        select(-!!sosdate)
    }

    if (type == "out") {
      out_data <- left_join(
        cohortdata,
        tmp_data %>% dplyr::select(!!!syms(groupbyvars), !!name2, !!sosdate),
        by = groupbyvars
      )

      if (!missing(stoptime)) {
        out_data <- out_data %>%
          mutate(tmp_censdate = pmin(!!indexdate + stoptime, !!censdate, na.rm = TRUE))
      } else {
        out_data <- out_data %>%
          mutate(tmp_censdate = !!censdate)
      }

      out_data <- out_data %>%
        mutate(
          !!name2 := tidyr::replace_na(!!sym(name2), 0),
          !!name2 := ifelse(!is.na(!!sosdate) & !!sosdate - allowedcenslimit > tmp_censdate, 0,
            # allow sosdate to be x days after censdate
            # (to fix that pats that have date of discharge AFTER
            # date of death are still counted as an event)
            !!sym(name2)
          ),
          !!timename2 := as.numeric(pmin(!!sosdate, tmp_censdate, na.rm = TRUE) - !!indexdate) + 1 - starttime) %>%
        select(-!!sosdate, -tmp_censdate)
    }
  }

  if (noof) {
    if (type == "com") {
      tmp_data <- tmp_data %>%
        filter(!!(sym(name2)) == 1) %>%
        group_by(!!!syms(groupbyvars)) %>%
        count(name = name2) %>%
        ungroup()
    }
    if (type == "out") {
      tmp_data <- left_join(cohortdata,
        tmp_data,
        by = groupbyvars
      )

      if (!missing(stoptime)) {
        tmp_data <- tmp_data %>%
          mutate(tmp_censdate = pmin(!!indexdate + stoptime, !!censdate, na.rm = TRUE))
      } else {
        tmp_data <- tmp_data %>%
          mutate(tmp_censdate = !!censdate)
      }

      tmp_data <- tmp_data %>%
        filter(!!(sym(name2)) == 1 &
          (!is.na(!!sosdate) & !!sosdate + allowedcenslimit <= tmp_censdate)) %>%
        group_by(!!!syms(groupbyvars)) %>%
        count(name = name2) %>%
        ungroup()
    }
    out_data <- left_join(
      cohortdata,
      tmp_data %>% dplyr::select(!!!syms(groupbyvars), !!name2),
      by = groupbyvars
    ) %>%
      mutate(!!name2 := tidyr::replace_na(!!sym(name2), 0))
  }
  if (valsclass %in% c("char", "fac")) {
    out_data <- out_data %>%
      mutate(!!name2 := case_when(
        !!sym(name2) == 1 ~ "Yes",
        TRUE ~ "No"
      ))
    if (valsclass == "fac") {
      out_data <- out_data %>%
        mutate(!!name2 := factor(!!sym(name2)))
    }
  }

  # create meta data to print in table in statistical report
  fixkod <- function(kod) {
    kod <- stringr::str_replace_all(kod, " ", "")
    kod <- stringr::str_replace_all(kod, "\\[", "")
    kod <- stringr::str_replace_all(kod, "\\]", "")
    kod <- stringr::str_replace_all(kod, "\\|", ",")
    kod <- stringr::str_replace_all(kod, "\\(\\?!", " (excl. ")
    kod <- paste0(kod, collapse = ": ")
  }

  meta_kod <- paste(
    if (!missing(diakod)) {
      paste0("ICD:", ifelse(diakodneg, "Not ", ""), fixkod(diakod))
    },
    if (!missing(opkod)) {
      paste0("OP:", ifelse(opkodneg, "Not ", ""), fixkod(opkod))
    },
    if (!missing(ekod)) {
      paste0("Ekod:", ifelse(ekodneg, "Not ", ""), fixkod(ekod))
    }
  )


  meta_time <- paste0(starttime, "-", ifelse(!missing(stoptime), stoptime, ""))

  if (missing(meta_pos)) {
    meta_pos <- paste(
      if (!missing(diakod)) rlang::as_name(diavar),
      if (!missing(opkod)) rlang::as_name(opvar),
      if (!missing(ekod)) rlang::as_name(evar),
      sep = " "
    )
  }

  # paste0(
  # "All positions",
  # if (!is.null(diakod)) " (HDIA+BDIA1-BDIAXX)",
  # if (!is.null(opkod)) " (OP1-OPXX)",
  # if (!is.null(ekod)) " (Ekod1-EkodXX)"
  # )

  metatmp <- data.frame(name2, meta_kod, meta_reg, meta_pos, meta_time)
  colnames(metatmp) <- c("Variable", "Code", "Register", "Position", "Period")

  if (exists("metaout")) {
    metaout <<- rbind(metaout, metatmp) # global variable, writes to global env
  } else {
    metaout <<- metatmp # global variable, writes to global env
  }
  return(out_data)
}
