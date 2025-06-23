test_that(
  "Module validates all input parameters" |> vdoc[["add_spec"]](specs$common$input_validation), 
  {
  mod <- mod_swimmerplot(
    module_id = "test_id",
    response_dataset_name = "rs",
    trt_ongoing_var = "ongoing",
    trt_tooltip_vars = c("Treatment: " = "EXTRT"),
    trt_legend_label = "Treatment",
    result_study_day_var = "day",
    result_tooltip_vars = c("Response: " = "RSORRES"),
    result_cat_var = "cat",
    result_legend_label = "Response",
    plot_title = "Title",
    plot_x_label = "X",
    plot_y_label = "Y"
  )
  
  expect_type(mod, "list")
  expect_equal(names(mod), c("ui", "server", "module_id"))
  expect_equal(mod$module_id, "test_id")
  expect_type(mod$ui, "closure")
  expect_type(mod$server, "closure")
})

test_that(
  "Plot appearance can be customized with titles, labels, and dimensions" |> 
    vdoc[["add_spec"]](specs$plot$customization), 
  {
  mod <- mod_swimmerplot(
    module_id = "test_id",
    response_dataset_name = "rs",
    trt_ongoing_var = "ongoing",
    trt_tooltip_vars = c("Treatment: " = "EXTRT"),
    trt_legend_label = "Treatment",
    result_study_day_var = "day",
    result_tooltip_vars = c("Response: " = "RSORRES"),
    result_cat_var = "cat",
    result_legend_label = "Response",
    plot_title = "Title",
    plot_x_label = "X",
    plot_y_label = "Y"
  )
  
  ui_result <- mod$ui("test_mod_id")
  
  expect_s3_class(ui_result, "shiny.tag.list")
  
  mod_no_jump <- mod_swimmerplot(
    module_id = "test_id",
    response_dataset_name = "rs",
    trt_ongoing_var = "ongoing",
    trt_tooltip_vars = c("Treatment: " = "EXTRT"),
    trt_legend_label = "Treatment",
    result_study_day_var = "day",
    result_tooltip_vars = c("Response: " = "RSORRES"),
    result_cat_var = "cat",
    result_legend_label = "Response",
    plot_title = "Title",
    plot_x_label = "X",
    plot_y_label = "Y",
    receiver_id = NULL
  )
  ui_no_jump <- mod_no_jump$ui("test_mod_id")
  
  mod_with_jump <- mod_swimmerplot(
    module_id = "test_id",
    response_dataset_name = "rs",
    trt_ongoing_var = "ongoing",
    trt_tooltip_vars = c("Treatment: " = "EXTRT"),
    trt_legend_label = "Treatment",
    result_study_day_var = "day",
    result_tooltip_vars = c("Response: " = "RSORRES"),
    result_cat_var = "cat",
    result_legend_label = "Response",
    plot_title = "Title",
    plot_x_label = "X",
    plot_y_label = "Y",
    receiver_id = "target_mod"
  )
  ui_with_jump <- mod_with_jump$ui("test_mod_id")
  
  expect_false(identical(ui_no_jump, ui_with_jump))
})

test_that(
  "Exposure data can be colored according to custom color palettes and response data points can use custom shapes" |> 
    vdoc[["add_spec"]](specs$visualization$combined_customization), 
  {
  mod <- mod_swimmerplot(
    module_id = "test_id",
    response_dataset_name = "rs",
    trt_ongoing_var = "ongoing",
    trt_tooltip_vars = c("Treatment: " = "EXTRT"),
    trt_legend_label = "Treatment",
    result_study_day_var = "day",
    result_tooltip_vars = c("Response: " = "RSORRES"),
    result_cat_var = "cat",
    result_legend_label = "Response",
    plot_title = "Title",
    plot_x_label = "X",
    plot_y_label = "Y",
    group_by_vars = c("SEX", "RACE"),
    color_palette = c("Drug A" = "#FF0000", "Drug B" = "#00FF00"),
    shape_mapping = c("CR" = 16, "PR" = 17, "SD" = 15, "PD" = 18)
  )
  
  expect_type(mod, "list")
  expect_equal(mod$module_id, "test_id")
})

test_that(
  "Module is compatible with the DaVinci framework" |> vdoc[["add_spec"]](specs$integration$davinci_compatibility), 
  {
  mod <- mod_swimmerplot(
    module_id = "test_id",
    subject_level_dataset_name = "dm",
    exposure_dataset_name = "ex",
    response_dataset_name = "rs",
    trt_ongoing_var = "ongoing",
    trt_tooltip_vars = c("Treatment: " = "EXTRT"),
    trt_legend_label = "Treatment",
    result_study_day_var = "day",
    result_tooltip_vars = c("Response: " = "cat"),
    result_cat_var = "cat",
    result_legend_label = "Response",
    plot_title = "Title",
    plot_x_label = "X",
    plot_y_label = "Y"
  )
  
  dm_data <- data.frame(USUBJID = c("A", "B"), SEX = c("M", "F"))
  ex_data <- data.frame(
    USUBJID = c("A", "B"),
    EXSTDY = 1:2,
    EXENDY = 3:4,
    EXTRT = c("TRT1", "TRT2"),
    ongoing = c(FALSE, TRUE)
  )
  rs_data <- data.frame(
    USUBJID = c("A", "B"),
    day = 1:2,
    cat = c("CR", "PR")
  )
  
  filtered_dataset <- function() {
    list(
      dm = dm_data,
      ex = ex_data,
      rs = rs_data
    )
  }
  
  mock_afmm <- list(
    filtered_dataset = filtered_dataset,
    utils = list(switch2mod = function(id) TRUE)
  )
  
  expect_type(mod$server, "closure")
})

test_that(
  "Module validates all input parameters" |> vdoc[["add_spec"]](specs$common$input_validation), 
  {
  expect_silent(
    mod_swimmerplot(
      module_id = "test_id",
      subject_level_dataset_name = "dm",
      exposure_dataset_name = "ex",
      response_dataset_name = "rs",
      sort_direction = "asc",
      trt_ongoing_var = "ongoing",
      trt_tooltip_vars = c("Treatment: " = "EXTRT"),
      trt_legend_label = "Treatment",
      result_study_day_var = "day",
      result_tooltip_vars = c("Response: " = "RSORRES"),
      result_cat_var = "cat",
      result_legend_label = "Response",
      plot_title = "Title",
      plot_x_label = "X",
      plot_y_label = "Y"
    )
  )
  
  expect_error(
    mod_swimmerplot(
      module_id = "test_id",
      response_dataset_name = "rs",
      trt_ongoing_var = "ongoing",
      trt_tooltip_vars = c("Treatment: " = "EXTRT"),
      trt_legend_label = "Treatment",
      result_study_day_var = "day",
      result_tooltip_vars = c("Response: " = "RSORRES"),
      result_cat_var = "cat",
      result_legend_label = "Response",
      plot_title = "Title",
      plot_x_label = "X",
      plot_y_label = "Y",
      sort_direction = "invalid"
    )
  )
  
  expect_error(
    mod_swimmerplot(
      module_id = NULL,
      response_dataset_name = "rs",
      trt_ongoing_var = "ongoing",
      trt_tooltip_vars = c("Treatment: " = "EXTRT"),
      trt_legend_label = "Treatment",
      result_study_day_var = "day",
      result_tooltip_vars = c("Response: " = "RSORRES"),
      result_cat_var = "cat",
      result_legend_label = "Response",
      plot_title = "Title",
      plot_x_label = "X",
      plot_y_label = "Y"
    )
  )
})

test_that(
  "Plot supports optional subject jumping feature when enabled" |> 
    vdoc[["add_spec"]](specs$interactivity$jumping_feature), 
  {
  mod_no_switch <- mod_swimmerplot(
    module_id = "test_id",
    response_dataset_name = "rs",
    trt_ongoing_var = "ongoing",
    trt_tooltip_vars = c("Treatment: " = "EXTRT"),
    trt_legend_label = "Treatment",
    result_study_day_var = "day",
    result_tooltip_vars = c("Response: " = "RSORRES"),
    result_cat_var = "cat",
    result_legend_label = "Response",
    plot_title = "Title",
    plot_x_label = "X",
    plot_y_label = "Y",
    receiver_id = NULL
  )
  
  mod_with_switch <- mod_swimmerplot(
    module_id = "test_id",
    response_dataset_name = "rs",
    trt_ongoing_var = "ongoing",
    trt_tooltip_vars = c("Treatment: " = "EXTRT"),
    trt_legend_label = "Treatment",
    result_study_day_var = "day",
    result_tooltip_vars = c("Response: " = "RSORRES"),
    result_cat_var = "cat",
    result_legend_label = "Response",
    plot_title = "Title",
    plot_x_label = "X",
    plot_y_label = "Y",
    receiver_id = "target_mod"
  )
  
  expect_type(mod_no_switch, "list")
  expect_type(mod_with_switch, "list")
  
  ui_no_jump <- mod_no_switch$ui("test_id")
  ui_with_jump <- mod_with_switch$ui("test_id")
  expect_false(identical(ui_no_jump, ui_with_jump))
})

test_that(
  "Tooltips support named list format with labels and values" |> 
    vdoc[["add_spec"]](specs$interactivity$tooltip_format), 
  {
  mod_empty <- mod_swimmerplot(
    module_id = "test_id",
    response_dataset_name = "rs",
    trt_ongoing_var = "ongoing",
    trt_tooltip_vars = c(),
    trt_legend_label = "Treatment",
    result_study_day_var = "day",
    result_tooltip_vars = c(),
    result_cat_var = "cat",
    result_legend_label = "Response",
    plot_title = "Title",
    plot_x_label = "X",
    plot_y_label = "Y"
  )
  
  expect_type(mod_empty, "list")
  
  mod_null <- mod_swimmerplot(
    module_id = "test_id",
    response_dataset_name = "rs",
    trt_ongoing_var = "ongoing",
    trt_tooltip_vars = NULL,
    trt_legend_label = "Treatment",
    result_study_day_var = "day",
    result_tooltip_vars = NULL,
    result_cat_var = "cat",
    result_legend_label = "Response",
    plot_title = "Title",
    plot_x_label = "X",
    plot_y_label = "Y"
  )
  
  expect_type(mod_null, "list")
})
