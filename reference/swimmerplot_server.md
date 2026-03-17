# Swimmer Plot Module server

(For use outside of the DaVinci framework)\
Runs the server that populates the UI returned by \[swimmerplot_UI()\].\
Requires a matching call to that function.

## Usage

``` r
swimmerplot_server(
  id,
  subject_level_dataset,
  exposure_dataset,
  response_dataset,
  subjid_var,
  trt_start_day_var,
  trt_end_day_var,
  trt_ongoing_var,
  trt_tooltip_vars = NULL,
  trt_group_var = NULL,
  trt_legend_label = "Treatment",
  color_palette = NULL,
  result_study_day_var = NULL,
  result_tooltip_vars = NULL,
  trt_annotation_vars = NULL,
  trt_annotation_x = NULL,
  result_cat_var = NULL,
  result_legend_label = "Response",
  shape_mapping = NULL,
  plot_title = NULL,
  plot_subtitle = NULL,
  plot_x_label = "Study Day",
  plot_y_label = "Subject ID",
  plot_width = NULL,
  plot_height = NULL,
  sort_by_vars = NULL,
  sort_direction = "asc",
  receiver_id = NULL,
  afmm = NULL,
  filter_dataset = NULL,
  filter_on_exposure = FALSE,
  filter_on_response = FALSE,
  filter_var = "ARM",
  filter_values = NULL,
  filter_default_vals = NULL
)
```

## Arguments

- id:

  \`\[character(1)\]\` Unique shiny ID. Must match the ID provided to
  \[swimmerplot_server()\].

- subject_level_dataset:

  \`\[reactive(data.frame)\]\` Subject-level dataset containing baseline
  information.

- exposure_dataset:

  \`\[reactive(data.frame)\]\` Dataset containing treatment exposure
  records.

- response_dataset:

  \`\[reactive(data.frame)\]\` Dataset containing response/event
  records. Optional - can be a reactive that returns NULL to create a
  plot without response data.

- subjid_var:

  \`\[character(1)\]\` Name of the variable representing subject ID.

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

- result_study_day_var:

  \`\[character(1)\]\` Name of the variable representing the response
  time. Only used when response_dataset_name is provided.

- result_tooltip_vars:

  \`\[character(n)\]\` Character vector of variables for tooltip
  information on response. Optionally named to add labels in tooltips
  (e.g., c("Response: " = "RSORRES")). Only used when
  response_dataset_name is provided.

- trt_annotation_vars:

  \`\[character(n)\]\` Character vector of variables for fixed text
  annotation on exposure.

- trt_annotation_x:

  \`\[numeric(1)\]\` X-axis position for a left-aligned fixed exposure
  annotation column. If NULL, the exposure annotation is placed after
  each exposure bar end.

- result_cat_var:

  \`\[character(1)\]\` Name of the variable representing the type of
  response. Only used when response_dataset_name is provided.

- result_legend_label:

  \`\[character(1)\]\` Legend title for the response type. Only used
  when response_dataset_name is provided.

- shape_mapping:

  \`\[numeric(n)\]\` Named numeric vector mapping response types to
  point shapes (e.g., c("CR" = 16, "PR" = 17)). Only used when
  response_dataset_name is provided.

- plot_title:

  \`\[character(1)\]\` Title of the plot.

- plot_subtitle:

  \`\[character(1)\]\` Subtitle of the plot.

- plot_x_label:

  \`\[character(1)\]\` Label for the x-axis.

- plot_y_label:

  \`\[character(1)\]\` Label for the y-axis.

- plot_width:

  \`\[numeric(1)\]\` Width of the plot in inches.

- plot_height:

  \`\[numeric(1)\]\` Height of the plot in inches. If NULL, height will
  be calculated automatically based on the number of subjects.

- sort_by_vars:

  \`\[character(n)\]\` Variables available for sorting subjects.

- sort_direction:

  \`\[character(1)\]\` Direction to sort ("asc" for ascending, "desc"
  for descending).

- receiver_id:

  \`\[character(1)\]\` ID of the module to communicate with (e.g.,
  Patient Profile module ID). Set to NULL to disable the communication
  functionality.

- afmm:

  \`\[list\]\` Arguments from Module Manager containing datasets,
  utilities and module communication channels.

- filter_dataset:

  \`\[reactive(data.frame)\]\` Reactive data frame used to populate the
  filter control.

- filter_on_exposure:

  \`\[logical(1)\]\` Whether to apply the selected filter to the
  exposure dataset.

- filter_on_response:

  \`\[logical(1)\]\` Whether to apply the selected filter to the
  response dataset.

- filter_var:

  \`\[character(1)\]\` Name of the variable to use for filtering
  subjects.

- filter_values:

  \`\[character(n)\]\` Character vector restricting the available filter
  choices.

- filter_default_vals:

  \`\[character(n)\]\` Default selected values for the filter variable
  upon initialization.

## See also

\[mod_swimmerplot()\] and \[swimmerplot_UI()\]
