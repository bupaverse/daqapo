#' Detect incorrect activity names
#'
#' Function returning the incorrect activity labels in the log as indicated by the user. If details are requested, the entire activity log's rows containing incorrect activities are returned.
#' @inheritParams detect_activity_frequency_violations
#' @param allowed_activities Vector with correct activity labels. If NULL, user input will be asked.
#' @return activitylog containing the rows of the original activity log having incorrect activity labels
#' @examples
#' \donttest{
#' data("hospital_actlog")
#' detect_incorrect_activity_names(activitylog = hospital_actlog,
#'      allowed_activities = c(
#'          "Registration",
#'          "Triage",
#'          "Clinical exam",
#'          "Treatment",
#'          "Treatment evaluation"))
#' }
#' @import shiny
#' @import miniUI
#' @export
#'
detect_incorrect_activity_names <- function(activitylog, allowed_activities, details, filter_condition) {
  UseMethod("detect_incorrect_activity_names")

}
#' @export
detect_incorrect_activity_names.activitylog <- function(activitylog, allowed_activities = NULL, details = TRUE, filter_condition = NULL){

  # Predefine variables
  activity <- NULL
  absolute_frequency <- NULL

  # Apply filter condition when specified
  filter_specified <- FALSE
  tryCatch({
    is.null(filter_condition)
  }, error = function(e) {
    filter_specified <<- TRUE
  }
  )

  if(!filter_specified) {
    # geen filter gespecifieerd.

  } else {
    filter_condition_q <- enquo(filter_condition)
    activitylog <- APPLY_FILTER(activitylog, filter_condition_q = filter_condition_q)
  }

  # Show all unique activity names to the user
  unique_names <- activitylog %>% activities() %>%
    mutate(label = str_c(!!activity_id_(activitylog), " (", absolute_frequency,")", sep = ""))
  # message("The following activities were detected in the activity log:", "\n")
  # print.data.frame(unique_names)

  if(is.null(allowed_activities)) {


    ui <- miniPage(
      gadgetTitleBar("Detect Incorrect activity names"),
      miniContentPanel(
        selectInput("activities", "Incorrect activities:", choices = unique_names$label, multiple = T)
      )
    )
    server <- function(input, output, session){
      observeEvent(input$done, {
        to_remove <- input$activities
        to_remove <- unique_names[unique_names$label %in% to_remove,] %>% pull(1) %>% as.character()
        n_to_remove <- length(to_remove)

        message("*** OUTPUT ***")
        message(n_to_remove, " out of ", nrow(unique_names), " (", round(n_to_remove / nrow(unique_names) * 100, 2), "% ) activity labels are identified to be incorrect.")

        if (n_to_remove > 0) {
          if (!details) {
            message("These activity labels are:\n")
            anomalies <- to_remove
          } else {
            message("These activity labels are:", "\n", paste(to_remove, collapse = " - "))
            anomalies <- activitylog %>% filter_activity(to_remove)
            message("Given this information, ", nrow(anomalies), " of ", nrow(activitylog), " (", round(nrow(anomalies) / nrow(activitylog) * 100, 2), "%) rows in the activity log are incorrect. These are the following:")
          }

        }
        stopApp(anomalies)
      })
    }
      runGadget(ui, server, viewer = dialogViewer("Detect Incorrect Activity Names", height = 400))

    # # Ask the user for input on the wrong names
    # wrong_indices <- readline("Please indicate by index and separated by a comma which names are incorrect (Enter N if everything is correct): ")
    # if(str_to_upper(wrong_indices) == "N") {
    #   to_remove <- NULL
    #   n_to_remove <- 0
    # } else {
    #   wrong_indices <- wrong_indices %>% str_split(",") %>% unlist() %>% str_squish() %>% as.integer()
    #     # Get the activity labels from the entered indices
    #   to_remove <- unique_names[wrong_indices,] %>% pull(1) %>% as.character()
    #
    # }

    } else {
      to_remove <- activity_labels(activitylog)[!(activity_labels(activitylog) %in% allowed_activities)] %>% as.character()
      n_to_remove <- length(to_remove)

      message("*** OUTPUT ***")
      message(n_to_remove, " out of ", nrow(unique_names), " (", round(n_to_remove / nrow(unique_names) * 100, 2), "% ) activity labels are identified to be incorrect.")
      if (n_to_remove > 0) {
        if (!details) {
          message("These activity labels are:\n")
          return(to_remove)
        } else {
          message("These activity labels are:", "\n", paste(to_remove, collapse = " - "))
          anomalies <- activitylog %>% filter_activity(to_remove)
          message("Given this information, ", nrow(anomalies), " of ", nrow(activitylog), " (", round(nrow(anomalies) / nrow(activitylog) * 100, 2), "%) rows in the activity log are incorrect. These are the following:")
          return(anomalies)
        }
      } else {
        return(NULL)
      }
    }

  }
