# Swimmer Plot Module

\`mod_swimmerplot\` is a DaVinci Shiny module that displays
subject-level treatment exposures and outcomes over time. It consists of
an interactive visualization with the following features:

- Each subject is represented as a horizontal "swim lane"

- Treatment exposures are shown as horizontal bars along the timeline

- Clinical outcomes or responses are shown as points along the timeline
  (optional)

- Subjects can be grouped by categorical variables

- Interactive tooltips provide additional information on hover

- Optional linking to other modules (e.g., Patient Profile) when
  clicking on subjects, treatment duration bars, or response points

## Usage

``` r
mod_swimmerplot(
  module_id,
  subject_level_dataset_name = "dm",
  exposure_dataset_name = "ex",
  response_dataset_name = NULL,
  subjid_var = "USUBJID",
  group_by_vars = c("SEX", "RACE"),
  sort_by_vars = NULL,
  sort_direction = "asc",
  trt_start_day_var = "EXSTDY",
  trt_end_day_var = "EXENDY",
  trt_ongoing_var = NULL,
  trt_tooltip_vars = NULL,
  trt_group_var = "EXTRT",
  trt_legend_label = NULL,
  color_palette = NULL,
  result_study_day_var = NULL,
  result_tooltip_vars = NULL,
  trt_annotation_vars = NULL,
  trt_annotation_x = NULL,
  result_cat_var = NULL,
  result_legend_label = NULL,
  shape_mapping = NULL,
  plot_title = NULL,
  plot_subtitle = NULL,
  plot_x_label = NULL,
  plot_y_label = NULL,
  plot_width = NULL,
  plot_height = NULL,
  receiver_id = NULL,
  filter_data = subject_level_dataset_name,
  filter_var = "ARM",
  filter_values = NULL,
  filter_default_vals = NULL
)
```

## Arguments

- module_id:

  \`\[character(1)\]\` Unique ID for the module.

- subject_level_dataset_name:

  \`\[character(1)\]\` Name of the dataset containing subject-level
  dataset.

- exposure_dataset_name:

  \`\[character(1)\]\` Name of the dataset containing exposure data.

- response_dataset_name:

  \`\[character(1)\]\` Name of the dataset containing response data. Set
  to NULL to create a plot without response data.

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

- receiver_id:

  \`\[character(1)\]\` ID of the module to communicate with (e.g.,
  Patient Profile module ID). Set to NULL to disable the communication
  functionality.

- filter_data:

  \`\[character(1)\]\` Name of the dataset to use for populating the
  filter control.

- filter_var:

  \`\[character(1)\]\` Name of the variable to use for filtering
  subjects.

- filter_values:

  \`\[character(n)\]\` Character vector restricting the available filter
  choices.

- filter_default_vals:

  \`\[character(n)\]\` Default selected values for the filter variable
  upon initialization.

## Value

A list composed of the following elements:

- \`ui\`: Shiny UI function.

- \`server\`: Shiny server function.

- \`module_id\`: Shiny unique identifier.

- \`meta\`: A list with element \`dataset_info\`.

## See also

\[swimmerplot_UI()\], \[swimmerplot_server()\]
