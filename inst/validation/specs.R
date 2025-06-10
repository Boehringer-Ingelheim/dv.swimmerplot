specs <- list(
  common = list(
    input_validation = "Module validates all input parameters"
  ),
  plot = list(
    basic_functionality = "Swimmerplot function displays subject-level data over time",
    customization = "Plot appearance can be customized with titles, labels, and dimensions",
    sorting = "Subjects can be sorted by specified variables in ascending or descending order",
    grouping = "Subjects can be grouped by categorical variables using facets",
    legend = "Plot displays appropriate legends for exposure and response data"
  ),
  interactivity = list(
    tooltip_format = "Tooltips support named list format with labels and values",
    jumping_feature = "Plot supports optional subject jumping feature when enabled",
    tooltips = "Hovering over plot elements provides additional information via tooltips"
  ),
  visualization = list(
    color_palette = "Exposure data can be colored according to custom color palettes",
    shape_mapping = "Response data points can use custom shapes based on categories",
    ongoing_treatment = "Ongoing treatments are indicated with arrows"
  ),
  integration = list(
    davinci_compatibility = "Module is compatible with the DaVinci framework",
    standalone_app = "Module can be launched as a standalone Shiny application",
    module_manager = "Module integrates correctly with the DaVinci Module Manager framework"
  ),
  apps = list(
    mock_swimmerplot = "The mock swimmerplot app correctly demonstrates basic functionality",
    mock_swimmerplot_mm = "The mock swimmerplot module manager app correctly demonstrates integration"
  )
)
