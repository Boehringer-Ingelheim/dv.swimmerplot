# Module Customization

This vignette covers the customization options available in
`dv.swimmerplot`. Examples build on the data prepared in the [Swimmer
Plot
Module](https://boehringer-ingelheim.github.io/dv.swimmerplot/articles/dv-swimmerplot.md)
vignette.

## Data Setup

``` r

library(dv.swimmerplot)

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
  dplyr::filter(ARM != "Screen Failure")

ex <- dplyr::left_join(x = pharmaversesdtm::ex, y = dm, by = "USUBJID") |>
  dplyr::mutate(
    study_day = as.numeric(as.Date(RFENDTC) - as.Date(RFSTDTC)),
    ex_trt = paste(EXTRT, EXDOSE, EXDOSU),
    ex_ongoing = is.na(EXENDY),
    ex_end = ifelse(ex_ongoing, EXSTDY + 10, EXENDY)
  )

rs <- dplyr::left_join(x = pharmaversesdtm::rs_onco, y = dm, by = "USUBJID") |>
  dplyr::filter(RSTEST == "Overall Response") |>
  dplyr::filter(RSEVAL == "INVESTIGATOR") |>
  dplyr::filter(!is.na(RSDY))

sdtm_datasets <- list(dm = dm, ex = ex, rs = rs)
```

------------------------------------------------------------------------

## Color Palette

Use `color_palette` to assign specific colors to exposure groups. Supply
a **named character vector** where names match values of
`trt_group_var`.

``` r

mod_swimmerplot(
  module_id                  = "swimmer_colors",
  subject_level_dataset_name = "dm",
  exposure_dataset_name      = "ex",
  response_dataset_name      = "rs",
  subjid_var                 = "USUBJID",
  trt_start_day_var          = "EXSTDY",
  trt_end_day_var            = "ex_end",
  trt_group_var              = "ex_trt",
  trt_ongoing_var            = "ex_ongoing",
  trt_legend_label           = "Treatment",
  result_study_day_var       = "RSDY",
  result_cat_var             = "RSORRES",
  result_legend_label        = "Response",
  plot_x_label               = "Study Day",
  plot_y_label               = "Subject",
  color_palette = c(
    "PLACEBO 0 mg"     = "#FFCCBC",
    "XANOMELINE 54 mg" = "#B3E5FC",
    "XANOMELINE 81 mg" = "#C8E6C9"
  )
)
```

When `color_palette` is `NULL` (the default), ggplot2 automatically
assigns colors.

------------------------------------------------------------------------

## Shape Mapping for Response Points

Use `shape_mapping` to control the point shape for each response
category. Supply a **named numeric vector** where names match values of
`result_cat_var` and values are R `pch` codes.

``` r

mod_swimmerplot(
  module_id                  = "swimmer_shapes",
  subject_level_dataset_name = "dm",
  exposure_dataset_name      = "ex",
  response_dataset_name      = "rs",
  subjid_var                 = "USUBJID",
  trt_start_day_var          = "EXSTDY",
  trt_end_day_var            = "ex_end",
  trt_group_var              = "ex_trt",
  trt_ongoing_var            = "ex_ongoing",
  trt_legend_label           = "Treatment",
  result_study_day_var       = "RSDY",
  result_cat_var             = "RSORRES",
  result_legend_label        = "Response",
  plot_x_label               = "Study Day",
  plot_y_label               = "Subject",
  shape_mapping = c(
    "CR" = 16,   # filled circle  — Complete Response
    "PR" = 17,   # filled triangle — Partial Response
    "SD" = 15,   # filled square  — Stable Disease
    "PD" = 18    # filled diamond — Progressive Disease
  )
)
```

------------------------------------------------------------------------

## Tooltips

Both exposure bars and response points support interactive tooltips.
Pass a **named character vector** where names are the label text and
values are column names. Labels appear before the value in the tooltip
(e.g., `"Dose: 54"`).

``` r

mod_swimmerplot(
  module_id                  = "swimmer_tooltips",
  subject_level_dataset_name = "dm",
  exposure_dataset_name      = "ex",
  response_dataset_name      = "rs",
  subjid_var                 = "USUBJID",
  trt_start_day_var          = "EXSTDY",
  trt_end_day_var            = "ex_end",
  trt_group_var              = "ex_trt",
  trt_ongoing_var            = "ex_ongoing",
  trt_legend_label           = "Treatment",
  result_study_day_var       = "RSDY",
  result_cat_var             = "RSORRES",
  result_legend_label        = "Response",
  plot_x_label               = "Study Day",
  plot_y_label               = "Subject",
  trt_tooltip_vars = c(
    "Subject ID: " = "USUBJID",
    "Treatment: "  = "ex_trt",
    "Start Day: "  = "EXSTDY",
    "End Day: "    = "EXENDY"
  ),
  result_tooltip_vars = c(
    "Subject ID: " = "USUBJID",
    "Study Day: "  = "RSDY",
    "Response: "   = "RSORRES"
  )
)
```

Omit `trt_tooltip_vars` or `result_tooltip_vars` (or set them to `NULL`)
to disable tooltips for that layer.

------------------------------------------------------------------------

## Bar Annotations

`trt_annotation_vars` adds text labels directly on the plot, next to the
end of each subject’s last exposure bar. This is useful for annotating
treatment summaries or end-of-study status.

``` r

mod_swimmerplot(
  module_id                  = "swimmer_annot_trail",
  subject_level_dataset_name = "dm",
  exposure_dataset_name      = "ex",
  response_dataset_name      = "rs",
  subjid_var                 = "USUBJID",
  trt_start_day_var          = "EXSTDY",
  trt_end_day_var            = "ex_end",
  trt_group_var              = "ex_trt",
  trt_ongoing_var            = "ex_ongoing",
  trt_legend_label           = "Treatment",
  result_study_day_var       = "RSDY",
  result_cat_var             = "RSORRES",
  result_legend_label        = "Response",
  plot_x_label               = "Study Day",
  plot_y_label               = "Subject",
  plot_width                 = 10,
  trt_annotation_vars        = c("ARM"),  # column(s) to concatenate as annotation text
  trt_annotation_x           = NULL       # NULL = place annotation after bar end
)
```

Use `trt_annotation_x` to pin all annotations to a fixed x position,
creating an aligned column:

``` r

mod_swimmerplot(
  module_id                  = "swimmer_annot_fixed",
  subject_level_dataset_name = "dm",
  exposure_dataset_name      = "ex",
  response_dataset_name      = "rs",
  subjid_var                 = "USUBJID",
  trt_start_day_var          = "EXSTDY",
  trt_end_day_var            = "ex_end",
  trt_group_var              = "ex_trt",
  trt_ongoing_var            = "ex_ongoing",
  trt_legend_label           = "Treatment",
  result_study_day_var       = "RSDY",
  result_cat_var             = "RSORRES",
  result_legend_label        = "Response",
  plot_x_label               = "Study Day",
  plot_y_label               = "Subject",
  plot_width                 = 10,
  trt_annotation_vars        = c("ARM"),
  trt_annotation_x           = 200  # fixed x position for all annotation labels
)
```

------------------------------------------------------------------------

## Sorting

Control subject ordering with `sort_by_vars` and `sort_direction`.
Multiple variables are supported and applied in order.

``` r

mod_swimmerplot(
  module_id                  = "swimmer_sort",
  subject_level_dataset_name = "dm",
  exposure_dataset_name      = "ex",
  response_dataset_name      = "rs",
  subjid_var                 = "USUBJID",
  trt_start_day_var          = "EXSTDY",
  trt_end_day_var            = "ex_end",
  trt_group_var              = "ex_trt",
  trt_ongoing_var            = "ex_ongoing",
  trt_legend_label           = "Treatment",
  result_study_day_var       = "RSDY",
  result_cat_var             = "RSORRES",
  result_legend_label        = "Response",
  plot_x_label               = "Study Day",
  plot_y_label               = "Subject",
  sort_by_vars               = c("AGE", "USUBJID"),  # primary sort: AGE, secondary: USUBJID
  sort_direction             = "asc"                  # "asc" or "desc"
)
```

Users can also change sorting interactively at runtime via the **Plot
Options** dropdown. The `sort_by_vars` and `sort_direction` arguments
set the initial defaults.

------------------------------------------------------------------------

## Grouping (Faceting)

`group_by_vars` splits the plot into facets by one or more categorical
variables. Each combination gets its own panel with a free y-axis scale.

``` r

mod_swimmerplot(
  module_id                  = "swimmer_group",
  subject_level_dataset_name = "dm",
  exposure_dataset_name      = "ex",
  response_dataset_name      = "rs",
  subjid_var                 = "USUBJID",
  trt_start_day_var          = "EXSTDY",
  trt_end_day_var            = "ex_end",
  trt_group_var              = "ex_trt",
  trt_ongoing_var            = "ex_ongoing",
  trt_legend_label           = "Treatment",
  result_study_day_var       = "RSDY",
  result_cat_var             = "RSORRES",
  result_legend_label        = "Response",
  plot_x_label               = "Study Day",
  plot_y_label               = "Subject",
  group_by_vars              = c("SEX", "RACE")  # facet by sex and race
)
```

Set `group_by_vars = NULL` to display all subjects in a single unfaceted
panel.

------------------------------------------------------------------------

## Filter Control

The module includes a built-in filter dropdown. Use `filter_var` to
choose which variable drives the filter, `filter_values` to restrict the
selectable choices, and `filter_default_vals` to pre-select values on
startup.

``` r

mod_swimmerplot(
  module_id                  = "swimmer_filter",
  subject_level_dataset_name = "dm",
  exposure_dataset_name      = "ex",
  response_dataset_name      = "rs",
  subjid_var                 = "USUBJID",
  trt_start_day_var          = "EXSTDY",
  trt_end_day_var            = "ex_end",
  trt_group_var              = "ex_trt",
  trt_ongoing_var            = "ex_ongoing",
  trt_legend_label           = "Treatment",
  result_study_day_var       = "RSDY",
  result_cat_var             = "RSORRES",
  result_legend_label        = "Response",
  plot_x_label               = "Study Day",
  plot_y_label               = "Subject",
  filter_var                 = "ARM",  # variable to filter on
  filter_values              = c("Xanomeline High Dose", "Xanomeline Low Dose")
)
```

### Filtering from a Different Dataset

By default the filter is drawn from the subject-level dataset. Set
`filter_data` to the name of any dataset in your app (e.g. the response
dataset) to populate filter choices from there instead:

``` r

mod_swimmerplot(
  module_id                  = "swimmer_filter_rs",
  subject_level_dataset_name = "dm",
  exposure_dataset_name      = "ex",
  response_dataset_name      = "rs",
  subjid_var                 = "USUBJID",
  trt_start_day_var          = "EXSTDY",
  trt_end_day_var            = "ex_end",
  trt_group_var              = "ex_trt",
  trt_ongoing_var            = "ex_ongoing",
  trt_legend_label           = "Treatment",
  result_study_day_var       = "RSDY",
  result_cat_var             = "RSORRES",
  result_legend_label        = "Response",
  plot_x_label               = "Study Day",
  plot_y_label               = "Subject",
  filter_data                = "rs",           # draw filter choices from rs
  filter_var                 = "RSORRES",      # filter variable in rs
  filter_values              = c("CR", "SD"),  # restrict available choices
  filter_default_vals        = c("CR", "SD")   # pre-select these on startup
)
```

------------------------------------------------------------------------

## Axis Labels and Plot Titles

All text labels on the plot are configurable:

``` r

mod_swimmerplot(
  module_id                  = "swimmer_labels",
  subject_level_dataset_name = "dm",
  exposure_dataset_name      = "ex",
  response_dataset_name      = "rs",
  subjid_var                 = "USUBJID",
  trt_start_day_var          = "EXSTDY",
  trt_end_day_var            = "ex_end",
  trt_group_var              = "ex_trt",
  trt_ongoing_var            = "ex_ongoing",
  trt_legend_label           = "Treatment Arm",
  result_study_day_var       = "RSDY",
  result_cat_var             = "RSORRES",
  result_legend_label        = "Response",
  plot_title                 = "Subject-Level Exposure Timeline",
  plot_subtitle              = "Arrows indicate ongoing treatment",
  plot_x_label               = "Days from First Dose",
  plot_y_label               = "Patient ID"
)
```

------------------------------------------------------------------------

## Plot Dimensions

Control the initial plot size (in inches). `plot_height = NULL`
auto-scales height based on the number of subjects (approximately 0.3
inches per subject, minimum 6 inches).

``` r

mod_swimmerplot(
  module_id                  = "swimmer_size",
  subject_level_dataset_name = "dm",
  exposure_dataset_name      = "ex",
  response_dataset_name      = "rs",
  subjid_var                 = "USUBJID",
  trt_start_day_var          = "EXSTDY",
  trt_end_day_var            = "ex_end",
  trt_group_var              = "ex_trt",
  trt_ongoing_var            = "ex_ongoing",
  trt_legend_label           = "Treatment",
  result_study_day_var       = "RSDY",
  result_cat_var             = "RSORRES",
  result_legend_label        = "Response",
  plot_x_label               = "Study Day",
  plot_y_label               = "Subject",
  plot_width                 = 10,    # width in inches
  plot_height                = NULL   # auto-calculated from subject count
)
```

------------------------------------------------------------------------

## Combining Multiple Customizations

The following example, mirroring the configuration in
[`mock_swimmerplot_mm()`](https://boehringer-ingelheim.github.io/dv.swimmerplot/reference/mock_swimmerplot_mm.md),
demonstrates how to combine all customization options into a single
module call:

``` r

swimmer_plot_module <- mod_swimmerplot(
  module_id                  = "swimmer_full",
  subject_level_dataset_name = "dm",
  exposure_dataset_name      = "ex",
  response_dataset_name      = "rs",
  subjid_var                 = "USUBJID",
  group_by_vars              = c("SEX", "RACE"),
  sort_by_vars               = c("AGE", "USUBJID"),
  sort_direction             = "asc",
  trt_start_day_var          = "EXSTDY",
  trt_end_day_var            = "ex_end",
  trt_group_var              = "ex_trt",
  trt_ongoing_var            = "ex_ongoing",
  trt_tooltip_vars = c(
    "Subject ID: " = "USUBJID",
    "Exposure: "   = "ex_trt",
    "Start Day: "  = "EXSTDY",
    "End Day: "    = "EXENDY"
  ),
  result_study_day_var       = "RSDY",
  result_tooltip_vars = c(
    "Subject ID: " = "USUBJID",
    "Study Day: "  = "RSDY",
    "Response: "   = "RSORRES"
  ),
  result_cat_var             = "RSORRES",
  trt_legend_label           = "Exposure",
  result_legend_label        = "Response",
  color_palette = c(
    "PLACEBO 0 mg"     = "#FFCCBC",
    "XANOMELINE 54 mg" = "#B3E5FC",
    "XANOMELINE 81 mg" = "#C8E6C9"
  ),
  shape_mapping = c(
    "CR" = 16,
    "PR" = 17,
    "SD" = 15,
    "PD" = 18
  ),
  plot_title                 = "Interactive Swimmer Plot of Subject-Level Exposure and Response Data",
  plot_subtitle              = "Arrows indicate ongoing exposure with missing end times",
  plot_x_label               = "Study Day",
  plot_y_label               = "Subject ID",
  plot_width                 = 10,
  plot_height                = NULL,
  filter_data                = "rs",
  filter_var                 = "RSORRES",
  filter_values              = c("CR", "SD")
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
```

------------------------------------------------------------------------
