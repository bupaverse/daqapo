#' An activity log of 20 patients in a hospital (data frame)
#'
#' A dataset containing the logged activities in an illustrative hospital process.
#' 20 patients are described in the log.
#' Process activities include Registration, Triage, Clinical exam, Treatment and Treatment evaluation.
#'
#' @format A data frame with 53 rows and 7 variables:
#' \describe{
#'   \item{patient_visit_nr}{the patient's identifier}
#'   \item{activity}{the executed activity}
#'   \item{originator}{the resource performing the activity execution}
#'   \item{start_ts}{the timestamp at which the activity was started}
#'   \item{complete_ts}{the timestamp at which the activity was completed}
#'   \item{triagecode}{a case attribute describing the triage code}
#'   \item{specialization}{a case attribute describing the specialization}
#' }
#' @source An illustrative example developed in-house for demonstrational purposes.
"hospital"


#' An event log of 20 patients in a hospital
#'
#' A dataset containing the logged activities in an illustrative hospital process.
#' 20 patients are described in this log
#' Process activities include Registration, Triage, Clinical exam, Treatment and Treatment evaluation.
#'
#' @format A data frame with 53 rows and 7 variables:
#' \describe{
#'   \item{patient_visit_nr}{the patient's identifier}
#'   \item{activity}{the executed activity}
#'   \item{originator}{the resource performing the activity execution}
#'   \item{event_lifecycle_state}{the state the activity is in at the given timestamp}
#'   \item{timestamp}{the moment in time the lifecycle state was reached}
#'   \item{triagecode}{a case attribute describing the triage code}
#'   \item{specialization}{a case attribute describing the specialization}
#'   \item{event_matching}{a specification of which events form a pair in the log}
#' }
#' @source An illustrative example developed in-house for demonstrational purposes.
"hospital_events"


#' An activity log of 20 patients in a hospital (activity log object)
#'
#' A dataset containing the logged activities in an illustrative hospital process.
#' 20 patients are described in the log.
#' Process activities include Registration, Triage, Clinical exam, Treatment and Treatment evaluation.
#'
#' @format An activity log with 53 rows and 7 variables:
#' \describe{
#'   \item{patient_visit_nr}{the patient's identifier}
#'   \item{activity}{the executed activity}
#'   \item{originator}{the resource performing the activity execution}
#'   \item{start}{the timestamp at which the activity was started}
#'   \item{complete}{the timestamp at which the activity was completed}
#'   \item{triagecode}{a case attribute describing the triage code}
#'   \item{specialization}{a case attribute describing the specialization}
#' }
#' @source An illustrative example developed in-house for demonstrational purposes.
"hospital_actlog"
