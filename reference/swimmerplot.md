# Create an interactive swimmer plot

Generates an interactive swimmer plot showing subject-level treatment
exposure periods and response events. Response data is optional - when
not provided, the plot will only show subject-level and exposure data.

## Usage

``` r
swimmerplot(
  subject_level_dataset,
  subjid_var,
  group_by_vars = NULL,
  sort_by_vars = NULL,
  sort_direction = "asc",
  exposure_dataset,
  trt_start_day_var,
  trt_end_day_var,
  trt_ongoing_var,
  trt_tooltip_vars = NULL,
  trt_group_var = NULL,
  trt_legend_label = "Treatment",
  color_palette = NULL,
  response_dataset = NULL,
  result_study_day_var = NULL,
  result_tooltip_vars = NULL,
  result_cat_var = NULL,
  result_legend_label = "Response",
  shape_mapping = NULL,
  plot_title = NULL,
  plot_subtitle = NULL,
  plot_x_label = "Study Day",
  plot_y_label = "Subject ID",
  plot_width = NULL,
  plot_height = NULL,
  x_rng_lower = NULL,
  x_rng_upper = NULL,
  trt_annotation_vars = NULL,
  trt_annotation_x = NULL,
  enable_jumping = FALSE,
  interactive_plot = TRUE
)
```

## Arguments

- subject_level_dataset:

  \`\[data.frame\]\` Dataset containing subject-level data.

- subjid_var:

  \`\[character(1)\]\` Name of the variable representing subject ID.

- group_by_vars:

  \`\[character(n)\]\` Name of the variables to group subjects by in the
  plot.

- sort_by_vars:

  \`\[character(n)\]\` Name of the variables to sort by in the plot.

- sort_direction:

  \`\[character(1)\]\` Direction to sort ("asc" for ascending, "desc"
  for descending).

- exposure_dataset:

  \`\[data.frame\]\` Dataset containing treatment exposure data.

- trt_start_day_var:

  \`\[character(1)\]\` Name of the variable representing the start of
  exposure.

- trt_end_day_var:

  \`\[character(1)\]\` Name of the variable representing the end of
  exposure.

- trt_ongoing_var:

  \`\[character(1)\]\` Name of the variable indicating ongoing exposure.

- trt_tooltip_vars:

  \`\[character(n)\]\` Character vector of variables for tooltip
  information on exposure. Optionally named to add labels in tooltips
  (e.g., c("Dose: " = "EXDOSE")).

- trt_group_var:

  \`\[character(1)\]\` Name of the variable representing the type of
  exposure.

- trt_legend_label:

  \`\[character(1)\]\` Legend title for the exposure type.

- color_palette:

  \`\[character(n)\]\` Named character vector mapping exposure types to
  colors (e.g., c("Drug A" = "#FF0000", "Drug B" = "#00FF00")).

- response_dataset:

  \`\[data.frame\]\` Dataset containing response/event data. Optional -
  set to NULL to create a plot without response data.

- result_study_day_var:

  \`\[character(1)\]\` Name of the variable representing the response
  time. Only used when response_dataset is provided.

- result_tooltip_vars:

  \`\[character(n)\]\` Character vector of variables for tooltip
  information on response. Optionally named to add labels in tooltips
  (e.g., c("Response: " = "RSORRES")). Only used when response_dataset
  is provided.

- result_cat_var:

  \`\[character(1)\]\` Name of the variable representing the type of
  response. Only used when response_dataset is provided.

- result_legend_label:

  \`\[character(1)\]\` Legend title for the response type. Only used
  when response_dataset is provided.

- shape_mapping:

  \`\[numeric(n)\]\` Named numeric vector mapping response types to
  point shapes (e.g., c("CR" = 16, "PR" = 17)). Only used when
  response_dataset is provided.

- plot_title:

  \`\[character(1)\]\` Title of the plot.

- plot_subtitle:

  \`\[character(1)\]\` Subtitle of the plot.

- plot_x_label:

  \`\[character(1)\]\` Label for the x-axis.

- plot_y_label:

  \`\[character(1)\]\` Label for the y-axis.

- plot_width:

  \`\[numeric(1)\]\` Width of the graphics region in inches.

- plot_height:

  \`\[numeric(1)\]\` Height of the graphics region in inches.

- x_rng_lower:

  \`\[numeric(1) \| NULL\]\` Lower limit of the x-axis visible range.

- x_rng_upper:

  \`\[numeric(1) \| NULL\]\` Upper limit of the x-axis visible range.

- trt_annotation_vars:

  \`\[character(n)\]\` Character vector of variables for fixed text
  annotation on exposure.

- trt_annotation_x:

  \`\[numeric(1)\]\` X-axis position for a left-aligned fixed exposure
  annotation column. If NULL, the exposure annotation is placed after
  each exposure bar end.

- enable_jumping:

  \`\[logical(1)\]\` Whether to enable jumping behavior on hover.

- interactive_plot:

  \`\[logical(1)\]\` Whether to return an interactive girafe object
  (TRUE) or a ggplot2 object (FALSE).

## Value

A ggiraph interactive plot object if interactive_plot=TRUE, otherwise a
ggplot2 object
