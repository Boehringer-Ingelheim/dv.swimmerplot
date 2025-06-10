#' Mock app for the swimmerplot module
#'
#' This function creates a standalone Shiny application that demonstrates the
#' swimmerplot module with example data from the pharmaversesdtm package.
#'
#' @examples
#' \dontrun{
#' mock_swimmerplot()
#' }
#'
#' @importFrom rlang .data
#' 
#' @export
mock_swimmerplot <- function() {
  mock_swimmerplot_UI <- function(id) { # nolint
    ns <- ifelse(is.character(id), shiny::NS(id), shiny::NS(NULL))
    
    shiny::fluidPage(
      shiny::titlePanel("Swimmer Plot Module Example"),
      swimmerplot_UI(
        id = ns(MODULE_IDS$SWIMMERPLOT),
        group_by_vars = c("SEX", "RACE"),
        sort_by_vars = c("AGE", "USUBJID"),
        jumping_enabled = FALSE
      )
    )
  }
  
  mock_swimmerplot_server <- function(input, output, session) {
    dm <- pharmaversesdtm::dm |>
      dplyr::select(
        USUBJID = .data$USUBJID, 
        AGE = .data$AGE, 
        SEX = .data$SEX, 
        RACE = .data$RACE, 
        ARM = .data$ARM, 
        RFSTDTC = .data$RFSTDTC, 
        RFENDTC = .data$RFENDTC
      ) |> 
      dplyr::filter(.data$ARM != "Screen Failure")
    
    ex <- dplyr::left_join(x = pharmaversesdtm::ex, y = dm, by = "USUBJID") |>
      dplyr::mutate(
        study_day = as.numeric(as.Date(.data$RFENDTC) - as.Date(.data$RFSTDTC)),
        ex_trt = paste(.data$EXTRT, .data$EXDOSE, .data$EXDOSU),      
        ex_ongoing = is.na(.data$EXENDY),
        ex_end = ifelse(.data$ex_ongoing, .data$EXSTDY + 10, .data$EXENDY)
      )
    
    rs <- dplyr::left_join(x = pharmaversesdtm::rs_onco, y = dm, by = "USUBJID") |>
      dplyr::filter(.data$RSTEST == "Overall Response") |>
      dplyr::filter(.data$RSEVAL == "INVESTIGATOR") |>
      dplyr::filter(!is.na(.data$RSDY))
    
    datasets <- list(dm = dm, ex = ex, rs = rs)
    
    subject_level_dataset <- shiny::reactive(datasets$dm)
    exposure_dataset <- shiny::reactive(datasets$ex)
    response_dataset <- shiny::reactive(datasets$rs)
    
    swimmerplot_server(
      id = MODULE_IDS$SWIMMERPLOT,
      subject_level_dataset = subject_level_dataset,
      exposure_dataset = exposure_dataset,
      response_dataset = response_dataset,
      subjid_var = "USUBJID",
      trt_start_day_var = "EXSTDY",
      trt_end_day_var = "ex_end",
      trt_ongoing_var = "ex_ongoing",
      trt_tooltip_vars = c(
        "Subject ID: " = "USUBJID",
        "Exposure: " = "ex_trt",
        "Start Day: " = "EXSTDY",
        "End Day: " = "EXENDY"
      ),
      trt_group_var = "ex_trt",
      trt_legend_label = "Exposure",
      color_palette = c(
        "PLACEBO 0 mg" = "#FFCCBC",
        "XANOMELINE 54 mg" = "#B3E5FC",
        "XANOMELINE 81 mg" = "#C8E6C9"
      ),
      result_study_day_var = "RSDY",
      result_tooltip_vars = c(
        "Subject ID: " = "USUBJID",
        "Study Day: " = "RSDY",
        "Response: " = "RSORRES"
      ),
      result_cat_var = "RSORRES",
      result_legend_label = "Response",
      shape_mapping = c(
        "CR" = 16, 
        "PR" = 17,
        "SD" = 15, 
        "PD" = 18  
      ),
      plot_title = "Interactive Swimmer Plot of Subject-Level Exposure and Response Data",
      plot_subtitle = "Arrows indicate ongoing exposure with missing end times",
      plot_x_label = "Study Day",
      plot_y_label = "Subject ID",
      plot_width = 10,
      plot_height = NULL,
      sort_by_vars = c("AGE", "USUBJID"),
      sort_direction = "asc"
    )
  }
  
  shiny::shinyApp(
    mock_swimmerplot_UI,
    mock_swimmerplot_server
  )
}
