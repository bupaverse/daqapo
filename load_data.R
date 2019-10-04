library(daqapo)

data("hospital")
hospital <- read_activity_log(hospital, "patient_visit_nr", "activity", "originator", "start_ts", "complete_ts",
                              c("triagecode", "specialization"), "dd/mm/yyyy hh:mm:ss")
hospital_loaded <- read_activity_log(hospital, "patient_visit_nr", "activity", "originator", "start_ts", "complete_ts",
                                     c("triagecode", "specialization"), "dd/mm/yyyy hh:mm:ss")

data("hospital_events")
hospital_events <- resource_inconsistencies(hospital_events, "patient_visit_nr", "activity", "originator",
                                            "event_lifecycle_state", "timestamp", "event_matching", detect_only = F)
hospital_events <- read_event_log(hospital_events, "patient_visit_nr", "activity", "originator",
                                  "event_lifecycle_state", "timestamp", c("triagecode", "specialization"),
                                  "dd/mm/yyyy hh:mm:ss", "event_matching")

hospital_events <- read_event_log(hospital_events, "patient_visit_nr", "activity", "originator",
                                  "event_lifecycle_state", "timestamp", c("triagecode", "specialization"),
                                  "dd/mm/yyyy hh:mm:ss")

library(swirl)
library(swirlify)
