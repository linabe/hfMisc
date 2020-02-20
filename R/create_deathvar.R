#' Calculates death from SoS data
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' Calculates cause-specific death (and optionally time to death)
#'   from codes in the Cause of Death Registry.
#'
#'
#' @param cohortdata Data containing the cohort.
#' @param sosdata Optional data where ORSAK is found. If not supplied will use cohortdata.
#' @param patid Patient identifier. Must be given if both cohortdata and sosdata are supplied.
#' @param indexdate Index date for the patient (usually date of admission or
#'   discharge entered into SwedeHF). Must be given if calctimetodeath = TRUE.
#' @param censdate Vector of dates. Deaths are
#'   allowed up until this day (usually date of death or end follow-up).
#' @param calctimetodeath Should time to death be calculated? Default is FALSE.
#' @param name Name of resulting variable
#'   (prefix is sos_out_death).
#' @param orsakkod String of icd codes as a regular expression
#'   defining the cause specific death. Should start
#'   with blank space " ".
#' @param orsakvar Column where diakod is found. All codes should start
#'   with blank space " ". Default is ULORSAK.
#' @param valsclass optional argument specifying the class of the
#'   comorbidities/outcomes. Allowed values are "num" (numeric: 1, 0),
#'   "char" (character: "yes", "no"), "fac" (factor: "yes", "no").
#'   Default is "num".
#' @param meta_reg Optional argument specifying registries used. Printed in
#'   metadatatable. Default is
#'   "Dödsorsaksregistret".
#' @param meta_pos Optional argument specifying positions used to search for
#'   outcomes/comorbidities. Printed in metadatatable.
#' @param warnings Should warnings be printed. Default is FALSE.
#'
#' @seealso \code{\link{prep_sosdata}}
#' @seealso \code{\link{create_sosvar}}
#'
#' @return dataset with column containing cause-specifc death.
#'   Also dataset metaout that writes directly to global enviroment
#'   with information on constructed variables.
#'
#'
#' @examples
#'
#' dors_data <- prep_sosdata(dors_data, registry = "dors")
#'
#' rs_data <- create_deathvar(
#'   sosdata = dors_data,
#'   cohortdata = rs_data,
#'   patid = id,
#'   name = "cv",
#'   orsakkod = " I"
#' )
#' @import dplyr
#' @import rlang
#'
#' @export

create_deathvar <- function(cohortdata,
                            sosdata,
                            patid,
                            indexdate,
                            censdate,
                            calctimetodeath = FALSE,
                            name,
                            orsakkod,
                            orsakvar = ULORSAK,
                            valsclass = "num",
                            meta_reg =
                              "Dodsorsaksregistret",
                            meta_pos,
                            warnings = TRUE) {
  if (!missing(patid)) patid <- enquo(patid)
  if (!missing(indexdate)) indexdate <- enquo(indexdate)
  if (!missing(censdate)) censdate <- enquo(censdate)
  orsakvar <- enquo(orsakvar)

  if (missing(orsakkod)) {
    stop("Orsakkod is needed.")
  }

  name2 <- paste0("sos_out_death", name)
  if (calctimetodeath) timename2 <- paste0("sos_outtime_death", name)

  if (any(has_name(cohortdata, name2))) {
    if (warnings) {
      warning(paste0(
        name2, " already exists in cohortdata. This might cause unexpected results."
      ))
    }
    cohortdata <- cohortdata %>%
      dplyr::select(-!!enquo(name2))
  }

  if (!missing(sosdata)) {
    if (any(duplicated(sosdata %>% dplyr::select(!!patid)))) {
      if (warnings) {
        warning(paste0(
          "sosdata has duplicated ",
          as_label(patid), " this might cause unexpected results."
        ))
      }
    }
  }

  if (missing(sosdata) & (!missing(censdate) | !missing(indexdate) | !missing(patid) | calctimetodeath)) {
    if (warnings) {
      warning(paste0(
        "sosdata has not been supplied and therefore arguments censdate, indexdate, patid and calctimetodeath will have no affect."
      ))
    }
  }

  if (missing(sosdata)) {
    out_data <- cohortdata %>%
      mutate(
        !!name2 := stringr::str_detect(!!orsakvar, orsakkod),
        !!name2 := case_when(
          !!sym(name2) ~ 1,
          TRUE ~ 0
        )
      )
  }

  else {
    tmp_data <- left_join(cohortdata,
      sosdata %>% dplyr::select(!!patid, !!orsakvar),
      by = as_name(patid)
    ) %>%
      mutate(
        !!name2 := stringr::str_detect(!!orsakvar, orsakkod),
        !!name2 := case_when(
          !!sym(name2) ~ 1,
          TRUE ~ 0
        )
      ) %>%
      select(-!!orsakvar)

    if (calctimetodeath) {
      tmp_data <- tmp_data %>%
        mutate(!!timename2 := as.numeric(!!censdate - !!indexdate))
    }

    out_data <- tmp_data
  }

  if (valsclass %in% c("char", "fac")) {
    out_data <- out_data %>%
      mutate(!!name2 := case_when(
        !!sym(name2) == 1 ~ "yes",
        TRUE ~ "no"
      ))
    if (valsclass == "fac") {
      out_data <- out_data %>%
        mutate(!!name2 := factor(!!sym(name2)))
    }
  }

  # create meta data to print in table in statistical report
  fixkod <- function(kod) {
    kod <- stringr::str_replace_all(kod, " ", "")
    kod <- stringr::str_replace_all(kod, "\\|", ",")
    kod <- stringr::str_replace_all(kod, "\\(\\?!", " (excl. ")
    kod <- paste0(kod, collapse = ": ")
  }

  meta_kod <- paste0("ICD:", fixkod(orsakkod))

  meta_time <- ""

  if (missing(meta_pos)) {
    meta_pos <- paste(as_name(orsakvar))
  }

  metatmp <- data.frame(name2, meta_kod, meta_reg, meta_pos, meta_time)
  colnames(metatmp) <- c("Variable", "Code", "Register", "Position", "Period")

  if (exists("metaout")) {
    metaout <<- rbind(metaout, metatmp) # global variable, writes to global env
  } else {
    metaout <<- metatmp # global variable, writes to global env
  }
  return(out_data)
}
