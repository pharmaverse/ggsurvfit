#' Formatted Copy of `survival::lung`
#'
#' This is a copy of the lung data set exported by the survival
#' package. This data set, however, has column labels and
#' numeric categorical variables are now factors with assigned levels.
"df_lung"

#' Formatted Copy of `survival::colon`
#'
#' This is a copy of the colon data set exported by the survival
#' package. This data set, however, has column labels,
#' numeric categorical variables are now factors with assigned levels,
#' and we only include the recurrence outcome.
"df_colon"


#' Example phase III clinical trial data set
#'
#' Background
#' The example simulated data set is based on large phase III clinical trials in
#' breast cancer such as the ALTTO trial
#' `https://ascopubs.org/doi/abs/10.1200/JCO.2015.62.1797`.
#' The example “trial” aims to determine if a combination of two therapies
#' tablemab (T) plus vismab (V) improves outcomes for metastatic human epidermal
#' growth factor 2–positive breast cancer and increases the pathologic complete
#' response in the neoadjuvant setting (i.e. treatment given as a first step to
#' shrink a tumor before the main treatment or surgery).
#'
#' The trial has four treatment arms, patients with centrally confirmed human
#' epidermal growth factor 2-positive early breast cancer were randomly assigned
#' to 1 year of adjuvant therapy with V, T, their sequence (T to V), or their
#' combination (T+V) for 52 weeks.
#'
#' The primary end point was progression-free survival (PFS) as defined by
#' Cancer.gov: '“the length of time during and after the treatment of a disease,
#' such as cancer, that a patient lives with the disease but it does not get
#' worse. In a clinical trial, measuring the progression-free survival is one
#' way to see how well a new treatment works”'.
#'
#' A number of baseline measurements (taken at randomization) are also included
#' such as age, hormone receptor status and prior radiotherapy treatment.
#'
#' Additional details on reasons for study discontinuation and censoring
#' event description are also included.
#'
#' The data set adopts an abridged version of the CDISC ADaM ADTTE
#' time to event data model. See here for more info on CDISC ADaM data standards
#' \url{https://www.cdisc.org/standards/foundational/adam} and specifically the
#' ADTTE time to event data model here
#' \url{https://www.cdisc.org/standards/foundational/adam/adam-basic-data-structure-bds-time-event-tte-analyses-v1-0}.
#'
#' @format The data set contains the following variables:
#'
#' \describe{
#'     \item{STUDYID}{The study identifier. A code unique to the clinical trial}
#'     \item{SUBJID}{subject identifier. Numeric ID unique to each patient}
#'     \item{USUBJID}{unique subject identifier. Text ID combining study and patient IDs}
#'     \item{AGE}{age at randomisation (years)}
#'     \item{STR01}{Hormone receptor status at randomisation}
#'     \item{STR01N}{Hormone receptor positive (Numeric)}
#'     \item{STR01L}{Hormone receptor positive (Long format)}
#'     \item{STR02}{Prior Radiotherapy at randomisation}
#'     \item{STR02N}{Prior Radiotherapy at randomisation (Numeric)}
#'     \item{STR02L}{Prior Radiotherapy at randomisation (Long format)}
#'     \item{TRT01P}{Planned treatment assigned at randomisation}
#'     \item{TRT01PN}{Planned treatment assigned at randomisation (Numeric)}
#'     \item{PARAM}{Analysis parameter: Progression free survival}
#'     \item{PARAMCD}{Analysis parameter code}
#'     \item{AVAL}{Analysis value (time to event (years)}
#'     \item{CNSR}{Censoring (0 = Event, 1 = Censored)}
#'     \item{EVNTDESC}{Event description}
#'     \item{CNSDTDSC}{Censoring description}
#'     \item{DCTREAS}{Discontinuation from study reason}
#' }
#'
#' @source \url{https://github.com/VIS-SIG/Wonderful-Wednesdays/tree/master/data/2020/2020-04-08}
"adtte"

