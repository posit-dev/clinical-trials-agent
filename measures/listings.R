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

#' Adverse event listing
#'
#' @description
#' A record-level listing of adverse events, one row per reported event:
#' center/patient identifier, the patient's age/sex/race, treatment, MedDRA
#' Preferred Term, study day of onset, duration in days, and the event's
#' seriousness, greatest intensity, relationship to study drug, outcome,
#' whether it was treated, and the action taken with study drug. Reproduces
#' tlg-catalog AEL02. The outcome column reflects AEOUT as recorded in the
#' simulated data, which marks more events fatal than there are patient deaths.
#'
#' @return A data frame with one row per adverse-event record.
#' @provenance https://github.com/insightsengineering/tlg-catalog/blob/b3019fec92280384bac322680face57b2f685bfc/book/listings/adverse-events/ael02.qmd
#' @measure
ae_detail_listing <- function(adae = adam_data()$adae) {
  adae %>%
    dplyr::transmute(
      patient_id = USUBJID,
      age_sex_race = paste(AGE, SEX, RACE, sep = "/"),
      treatment = TRT01A,
      preferred_term = AEDECOD,
      onset_study_day = ASTDY,
      duration_days = AENDY - ASTDY + 1,
      serious = ifelse(AESER == "Y", "Yes", "No"),
      intensity = AESEV,
      related = ifelse(AEREL == "Y", "Yes", "No"),
      outcome = AEOUT,
      treated_for_ae = ifelse(AECONTRT == "Y", "Yes", "No"),
      action_taken = AEACN
    ) %>%
    dplyr::arrange(treatment, patient_id, onset_study_day) %>%
    as.data.frame()
}

#' Serious adverse event listing
#'
#' @description
#' A record-level listing of serious adverse events (AESER = 'Y'), one row per
#' event: center/patient identifier, age/sex/race, treatment, MedDRA Preferred
#' Term, study day of onset, duration, greatest intensity, relationship to
#' study drug, outcome, treatment for the event and action taken, followed by
#' the reasons the event was classified as serious (life threatening, required
#' hospitalization, disabling, congenital anomaly, other medically important).
#' Adapts tlg-catalog AEL03. The catalog's "results in death" seriousness
#' reason (AESDTH) is omitted because that flag is not reliably populated in
#' this simulated data (it marks more fatal events than there are patient
#' deaths), and the catalog's demonstration-only edit of AESCONG is not applied.
#'
#' @return A data frame with one row per serious adverse-event record.
#' @provenance https://github.com/insightsengineering/tlg-catalog/blob/b3019fec92280384bac322680face57b2f685bfc/book/listings/adverse-events/ael03.qmd
#' @measure
serious_ae_listing <- function(adae = adam_data()$adae) {
  adae %>%
    dplyr::filter(AESER == "Y") %>%
    dplyr::transmute(
      patient_id = USUBJID,
      age_sex_race = paste(AGE, SEX, RACE, sep = "/"),
      treatment = TRT01A,
      preferred_term = AEDECOD,
      onset_study_day = ASTDY,
      duration_days = AENDY - ASTDY + 1,
      intensity = AESEV,
      related = ifelse(AEREL == "Y", "Yes", "No"),
      outcome = AEOUT,
      treated_for_ae = ifelse(AECONTRT == "Y", "Yes", "No"),
      action_taken = AEACN,
      life_threatening = ifelse(AESLIFE == "Y", "Yes", "No"),
      hospitalization = ifelse(AESHOSP == "Y", "Yes", "No"),
      disability = ifelse(AESDISAB == "Y", "Yes", "No"),
      congenital_anomaly = ifelse(AESCONG == "Y", "Yes", "No"),
      other_medically_important = ifelse(AESMIE == "Y", "Yes", "No")
    ) %>%
    dplyr::arrange(treatment, patient_id, onset_study_day) %>%
    as.data.frame()
}

#' Patient death listing
#'
#' @description
#' A listing of patients who died on study (those with a recorded day of
#' death), one row per patient: center/patient identifier, age/sex/race,
#' treatment, date of first study-drug administration, day of last study-drug
#' administration, day of death, cause of death, and whether an autopsy was
#' performed. Reproduces tlg-catalog AEL04.
#'
#' @return A data frame with one row per deceased patient.
#' @provenance https://github.com/insightsengineering/tlg-catalog/blob/b3019fec92280384bac322680face57b2f685bfc/book/listings/adverse-events/ael04.qmd
#' @measure
patient_death_listing <- function(adsl = adam_data()$adsl) {
  adsl %>%
    dplyr::filter(!is.na(DTHADY)) %>%
    dplyr::transmute(
      patient_id = USUBJID,
      age_sex_race = paste(AGE, SEX, RACE, sep = "/"),
      treatment = TRT01A,
      first_dose_date = toupper(format(as.Date(TRTSDTM), "%d%b%Y")),
      last_study_day = EOSDY,
      death_study_day = DTHADY,
      cause_of_death = DTHCAUS,
      autopsy_performed = ADTHAUT
    ) %>%
    dplyr::arrange(treatment, patient_id) %>%
    as.data.frame()
}
