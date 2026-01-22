#' Run an example swimmer plot module in the module manager
#'
#' Launches an example app that shows a swimmer plot module integrated in the
#' module manager surface. Displays data prepared from the pharmaversesdtm package.
#'
#' @examples
#' \dontrun{
#' mock_swimmerplot_mm()
#' }
#'
#' @importFrom rlang .data
#' @export
mock_swimmerplot_mm <- function() {
  dm <- pharmaversesdtm::dm |>
    dplyr::select(
      USUBJID = "USUBJID", 
      AGE = "AGE", 
      SEX = "SEX", 
      RACE = "RACE", 
      ARM = "ARM", 
      RFSTDTC = "RFSTDTC", 
      RFENDTC = "RFENDTC"
    ) |> 
    dplyr::filter(.data$ARM != "Screen Failure")
  
  ex <- dplyr::left_join(x = pharmaversesdtm::ex, y = dm, by = "USUBJID") |>
    dplyr::mutate(
      study_day = as.numeric(as.Date(.data$RFENDTC) - as.Date(.data$RFSTDTC)),
      ex_trt = paste(.data$EXTRT, .data$EXDOSE, .data$EXDOSU),      
      ex_ongoing = is.na(.data$EXENDY),
      ex_end = ifelse(.data$ex_ongoing, .data$EXSTDY + 10, .data$EXENDY) # For ongoing treatments
    )
  
  rs <- dplyr::left_join(x = pharmaversesdtm::rs_onco, y = dm, by = "USUBJID") |>
    dplyr::filter(.data$RSTEST == "Overall Response") |>
    dplyr::filter(.data$RSEVAL == "INVESTIGATOR") |>
    dplyr::filter(!is.na(.data$RSDY))
  
  sdtm_datasets <- list(dm = dm, ex = ex, rs = rs)
  
  swimmer_plot_module <- mod_swimmerplot(
    module_id = "swimmer1",
    subject_level_dataset_name = "dm",
    exposure_dataset_name = "ex",
    response_dataset_name = "rs",
    subjid_var = "USUBJID",
    group_by_vars = c("SEX", "RACE"),
    sort_by_vars = c("AGE", "USUBJID"),
    sort_direction = "asc",
    trt_start_day_var = "EXSTDY",
    trt_end_day_var = "ex_end",
    trt_group_var = "ex_trt",
    trt_ongoing_var = "ex_ongoing",
    trt_tooltip_vars = c(
      "Subject ID: " = "USUBJID",
      "Exposure: " = "ex_trt",
      "Start Day: " = "EXSTDY",
      "End Day: " = "EXENDY"
    ),
    result_study_day_var = "RSDY",
    result_tooltip_vars = c(
      "Subject ID: " = "USUBJID",
      "Study Day: " = "RSDY",
      "Response: " = "RSORRES"
    ),
    result_cat_var = "RSORRES",
    trt_legend_label = "Exposure",
    result_legend_label = "Response",
    color_palette = c(
      "PLACEBO 0 mg" = "#FFCCBC",
      "XANOMELINE 54 mg" = "#B3E5FC",
      "XANOMELINE 81 mg" = "#C8E6C9"
    ),
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
    filter_data = "rs",
    filter_var = "RSORRES",
    filter_values= c("CR","SD")
  )
  
  module_list <- list(
    "Swimmer Plot" = swimmer_plot_module
  )
  
  dv.manager::run_app(
    data = list("SDTM Datasets" = sdtm_datasets),
    module_list = module_list,
    title = "Swimmer Plot Example",
    filter_data = "dm",
    filter_key = "USUBJID"
  )
}
