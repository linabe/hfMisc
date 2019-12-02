#' Clean data
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' Removes spaces and special characters from character column(s).
#'
#' @param x A dataset to clean.
#'
#' @return A cleaned dataset.
#'
#' @examples
#' sos_data <- clean_data(sos_data)
#' @import dplyr
#' @export

clean_data <- function(x) {
  x <- x %>%
    mutate_if(is.factor, as.character) %>%
    # Hantera teckenkodning
    rename_all(iconv, from = "UTF-8") %>%
    mutate_if(is.character, iconv, from = "UTF-8") %>%
    mutate_if(
      is.character,
      list(~ gsub("[[:space:]]", " ", .))
    ) %>%
    mutate_if(
      is.character,
      list(~ gsub("\u00E4|\u00E5", "a", .))
    ) %>%
    mutate_if(
      is.character,
      list(~ gsub("\u00C4|\u00C5", "A", .))
    ) %>%
    mutate_if(
      is.character,
      list(~ gsub("\u00F6", "o", .))
    ) %>%
    mutate_if(
      is.character,
      list(~ gsub("\u00D6", "O", .))
    ) %>%
    mutate_if(is.character, list(~ na_if(., ""))) %>%
    mutate_if(is.character, list(~ na_if(., "Unknown"))) %>%
    mutate_if(is.character, list(~ na_if(., "Okant")))
}
