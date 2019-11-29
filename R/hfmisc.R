#' hfmisc: Miscellaneous functions created mainly for use in RiksSvikt (SwedeHF)
#'
#' ...
#'
#' @author Lina Benson \email{lina.benson@ki.se}
#' @docType package
#' @name hfmisc
#' @importFrom dplyr %>%
"_PACKAGE"

# Update this function call this should be done for tidyverse
# to avoid undefined global variables when running check
# utils::globalVariables(c("weather", "Temp"))

utils::globalVariables(c("AR", "DIA_all", "DODSDAT", "HDIA", "INDATUM",
                         "OP_all", "ORSAK_all", "ULORSAK", "UTDATUM",
                         "ekod_all", "sosdtm", "tmp_H", "tmp_U",
                         "difft", "indexdtm", "lopnr", "metaout", "."))
