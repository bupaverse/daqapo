## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  error = TRUE,
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
library(daqapo)
library(dplyr)
data("hospital")
data("hospital_events")

## ----LogTypes_Activity--------------------------------------------------------
str(hospital)

## ----LogTypes_Event-----------------------------------------------------------
str(hospital_events)

## ----rename-------------------------------------------------------------------
hospital %>%
  rename(start = start_ts,
         complete = complete_ts) -> hospital

## ----convert_timestamps-------------------------------------------------------
hospital %>%
  convert_timestamps(c("start","complete"), format = dmy_hms) -> hospital

## ----create_activitylog-------------------------------------------------------
hospital %>%
  activitylog(case_id = "patient_visit_nr",
              activity_id = "activity",
              resource_id = "originator",
              lifecycle_ids = c("start", "complete")) -> hospital

## ----ReadEventLog-------------------------------------------------------------
hospital_events

## ----ReadEventLog_Cols--------------------------------------------------------
hospital_events %>%
  convert_timestamps(c("timestamp"), format = dmy_hms) %>%
  mutate(event_matching = paste(patient_visit_nr, activity, event_matching)) %>%
  events_to_activitylog(case_id = "patient_visit_nr", 
                        activity_id = "activity", 
                        activity_instance_id = "event_matching", 
                        timestamp = "timestamp", 
                        resource_id = "originator",
                        lifecycle_id = "event_lifecycle_state") -> hospital_events


## -----------------------------------------------------------------------------
hospital %>%
  detect_activity_frequency_violations("Registration" = 1,
                                       "Clinical exam" = 1)

## -----------------------------------------------------------------------------
hospital %>%
  detect_activity_order_violations(activity_order = c("Registration", "Triage", "Clinical exam",
                                                      "Treatment", "Treatment evaluation"))

## -----------------------------------------------------------------------------
hospital %>% 
  detect_attribute_dependencies(antecedent = activity == "Registration",
                                consequent = startsWith(originator,"Clerk"))

## -----------------------------------------------------------------------------
hospital %>%
  detect_case_id_sequence_gaps()

## -----------------------------------------------------------------------------
hospital %>%
  detect_conditional_activity_presence(condition = specialization == "TRAU",
                                       activities = "Clinical exam")

## -----------------------------------------------------------------------------
hospital %>%
  detect_duration_outliers(Treatment = duration_within(bound_sd = 1))

## -----------------------------------------------------------------------------
hospital %>%
  detect_duration_outliers(Treatment = duration_within(lower_bound = 0, upper_bound = 15))

## -----------------------------------------------------------------------------
hospital %>%
  detect_inactive_periods(threshold = 30)

## -----------------------------------------------------------------------------
hospital %>%
  detect_incomplete_cases(activities = c("Registration","Triage","Clinical exam","Treatment","Treatment evaluation"))

## -----------------------------------------------------------------------------
hospital %>%
  detect_incorrect_activity_names(allowed_activities = c("Registration","Triage","Clinical exam","Treatment","Treatment evaluation"))

## -----------------------------------------------------------------------------
hospital %>%
  detect_missing_values(column = "activity")
## column heeft hier geen zin?!

## -----------------------------------------------------------------------------
hospital %>% 
  detect_missing_values(level_of_aggregation = "activity")

## -----------------------------------------------------------------------------
hospital %>% 
  detect_missing_values(
  level_of_aggregation = "column",
  column = "triagecode")

## -----------------------------------------------------------------------------
hospital %>%
  detect_multiregistration(threshold_in_seconds = 10)

## -----------------------------------------------------------------------------
hospital %>%
  detect_overlaps()

## -----------------------------------------------------------------------------
hospital %>%
  detect_related_activities(antecedent = "Treatment evaluation", 
                            consequent = "Treatment")

## -----------------------------------------------------------------------------
hospital %>%
  detect_similar_labels(column_labels = "activity", max_edit_distance = 3)

## -----------------------------------------------------------------------------
hospital %>%
  detect_time_anomalies()

## -----------------------------------------------------------------------------
hospital %>%
  detect_unique_values(column_labels = "activity")

## -----------------------------------------------------------------------------
hospital %>%
  detect_unique_values(column_labels = c("activity", "originator"))

## -----------------------------------------------------------------------------
hospital %>%
  detect_value_range_violations(triagecode = domain_numeric(from = 0, to = 5))

