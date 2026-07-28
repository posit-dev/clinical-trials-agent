# Listing (L) measures. Shared setup and the fidelity note live in helpers.R.
#
# A listing is row-level detail rather than an aggregate, so these measures
# return the records themselves as a tidy data frame (no tidy_tlg()).

#' Adverse event term listing
#'
#' @description
#' A listing of the distinct adverse-event terms recorded in the study, showing
#' the MedDRA hierarchy for each: System Organ Class (AESOC), Preferred Term
#' (AEDECOD), Lowest Level Term (AELLT), and the investigator-specified reported
#' term (AETERM). One row per unique term combination, ordered by the hierarchy.
#' Reproduces tlg-catalog AEL01.
#'
#' @return A data frame with columns AESOC, AEDECOD, AELLT, and AETERM, one row
#'   per unique combination.
#' @provenance https://github.com/insightsengineering/tlg-catalog/blob/b3019fec92280384bac322680face57b2f685bfc/book/listings/adverse-events/ael01.qmd
#' @measure
ae_term_listing <- function(adae = adam_data()$adae) {
  adae %>%
    dplyr::select(AESOC, AEDECOD, AELLT, AETERM) %>%
    dplyr::distinct() %>%
    dplyr::arrange(AESOC, AEDECOD, AELLT) %>%
    as.data.frame()
}
