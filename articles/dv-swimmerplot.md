# Swimmer Plot Module

## Introduction

Swimmer plots are specialized visualizations that display subject-level
data over time. Each subject is represented as a horizontal “swim lane”
showing their treatment exposures and optionally clinical outcomes or
responses. This vignette demonstrates how to prepare data and use the
swimmer plot module within a DaVinci application.

**Note:** Response data is completely optional. You can create swimmer
plots that only show exposure data, or include both exposure and
response data depending on your analysis needs.

## Data Preparation

The swimmer plot module requires at minimum two types of datasets, with
a third being optional:

1.  **Subject Level Dataset** (Required): Contains subject level
    demographic information (e.g., `dm`, `adsl`)
2.  **Exposure Dataset** (Required): Contains treatment exposure
    information (e.g., `ex`, `adex`)
3.  **Response Dataset** (Optional): Contains outcome or response
    assessments (e.g., `rs`, `adrs`, or other clinical outcomes)

In this example, we’ll use SDTM data from the `pharmaversesdtm` package,
which contains oncology data, but the module can be adapted for various
therapeutic areas. We’ll prepare all three datasets, but remember that
the response dataset is optional:

``` r

dm <- pharmaversesdtm::dm |>
  dplyr::select(USUBJID, AGE, SEX, RACE, ARM, RFSTDTC, RFENDTC) |> 
  dplyr::filter(ARM != "Screen Failure")

ex <- dplyr::left_join(x = pharmaversesdtm::ex, y = dm, by = "USUBJID") |>
  dplyr::mutate(
    study_day = as.numeric(as.Date(RFENDTC) - as.Date(RFSTDTC)),
    ex_trt = paste(EXTRT, EXDOSE, EXDOSU),      
    ex_ongoing = is.na(EXENDY),
    ex_end = ifelse(ex_ongoing, EXSTDY + 10, EXENDY) # For ongoing treatments
  )

# Optional: Response dataset for clinical outcomes
# This example uses oncology response data, but any type of response or outcome data can be used
# You can omit this entirely if you only want to show exposure data
rs <- dplyr::left_join(x = pharmaversesdtm::rs_onco, y = dm, by = "USUBJID") |>
  dplyr::filter(RSTEST == "Overall Response") |>
  dplyr::filter(RSEVAL == "INVESTIGATOR") |>
  dplyr::filter(!is.na(RSDY))

sdtm_datasets <- list(dm = dm, ex = ex, rs = rs)
```

## Creating a Swimmer Plot Without Response Data (Exposure Only)

You can create a swimmer plot that only displays subject-level exposure
data by setting `response_dataset_name = NULL`. This is useful when you
want to focus solely on treatment patterns or when response data is not
available:

``` r

swimmer_plot_exposure_only <- dv.swimmerplot::mod_swimmerplot(
    module_id = "mod1",
    subject_level_dataset_name = "dm",
    exposure_dataset_name = "ex",
    response_dataset_name = NULL,  # Omit response dataset
    subjid_var = "USUBJID",
    group_by_vars = c("SEX", "ARM"),
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
    trt_legend_label = "Exposure",
    color_palette = c(
      "PLACEBO 0 mg" = "#FFCCBC",
      "XANOMELINE 54 mg" = "#B3E5FC",
      "XANOMELINE 81 mg" = "#C8E6C9"
    ),
    plot_title = "Subject Exposure Only",
    plot_subtitle = "Arrows indicate ongoing exposure with missing end times",
    plot_x_label = "Study Day",
    plot_y_label = "Subject ID",
    plot_width = 10,
    plot_height = NULL
)
```

**Important:** When `response_dataset_name` is set to `NULL`, all
response-related parameters (`result_study_day_var`,
`result_tooltip_vars`, `result_cat_var`, `result_legend_label`, and
`shape_mapping`) can be omitted as they will not be used. The plot will
only show treatment exposure timelines.

## Creating a Swimmer Plot Module with Response Data (Full Featured)

For a more comprehensive visualization, you can include both exposure
and response data. This example shows how to use the `mod_swimmerplot`
function with all available features, including response assessments
displayed as points on the timeline:

``` r

swimmer_plot_module <- dv.swimmerplot::mod_swimmerplot(
    module_id = "mod2",
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
      "CR" = 16,  # filled circle - Complete Response
      "PR" = 17,  # filled triangle - Partial Response
      "SD" = 15,  # filled diamond - Stable Disease
      "PD" = 18   # filled square - Progressive Disease
    ),
    plot_title = "Interactive Swimmer Plot of Subject-Level Exposure and Response Data",
    plot_subtitle = "Arrows indicate ongoing exposure with missing end times",
    plot_x_label = "Study Day",
    plot_y_label = "Subject ID",
    plot_width = 10,
    plot_height = NULL
)
```

## Running the Application

Finally, we’ll use the `run_app` function from the
[`dv.manager`](https://boehringer-ingelheim.github.io/dv.manager/)
package to create a Shiny application that includes both swimmer plot
modules - one showing exposure only and one showing both exposure and
response data:

``` r

dv.manager::run_app(
  data = list("SDTM Datasets" = sdtm_datasets),
  module_list = list(
    "Swimmer Plot - Exposure Only" = swimmer_plot_exposure_only,
    "Swimmer Plot with Response" = swimmer_plot_module
  ),
  title = "Swimmer Plot Examples",
  filter_data = "dm",
  filter_key = "USUBJID"
)
```
