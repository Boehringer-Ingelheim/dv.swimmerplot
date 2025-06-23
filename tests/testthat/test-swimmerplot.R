test_that(
  "Swimmerplot function displays subject-level data over time" |> vdoc[["add_spec"]](c(
    specs$plot$basic_functionality,
    specs$plot$sorting,
    specs$plot$legend,
    specs$visualization$ongoing_treatment
  )), 
  {
  df1 <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002"),
    SEX = c("M", "F")
  )
  
  df2 <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-001", "SUBJ-002"),
    EXSTDY = c(1, 30, 1),
    ex_end = c(30, 40, 25),
    ex_trt = c("Drug A", "Drug B", "Drug B"),
    ex_ongoing = c(FALSE, TRUE, FALSE)
  )
  
  df3 <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002"),
    RSDY = c(15, 20),
    RSORRES = c("CR", "PR")
  )
  
  plot_obj <- dv.swimmerplot:::swimmerplot(
    subject_level_dataset = df1,
    subjid_var = "USUBJID",
    group_by_vars = NULL,
    sort_by_vars = "SEX",
    sort_direction = "asc",
    exposure_dataset = df2,
    trt_start_day_var = "EXSTDY",
    trt_end_day_var = "ex_end",
    trt_ongoing_var = "ex_ongoing",
    trt_tooltip_vars = c(
      "Subject: " = "USUBJID",
      "Treatment: " = "ex_trt"
    ),
    trt_group_var = "ex_trt",
    trt_legend_label = "Exposure",
    color_palette = NULL,
    response_dataset = df3,
    result_study_day_var = "RSDY",
    result_tooltip_vars = c(
      "Subject: " = "USUBJID",
      "Response: " = "RSORRES"
    ),
    result_cat_var = "RSORRES",
    result_legend_label = "Response",
    shape_mapping = NULL,
    plot_title = "Test Plot",
    plot_subtitle = "Test Subtitle",
    plot_x_label = "Day",
    plot_y_label = "Subject",
    plot_width = 8,
    plot_height = 6
  )
  
  expect_s3_class(plot_obj, "girafe")
})

test_that(
  "Swimmerplot function returns a ggplot object when interactive_plot=FALSE" |> vdoc[["add_spec"]](
    specs$plot$non_interactive
  ),
  {
  df1 <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002"),
    SEX = c("M", "F")
  )
  
  df2 <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-001", "SUBJ-002"),
    EXSTDY = c(1, 30, 1),
    ex_end = c(30, 40, 25),
    ex_trt = c("Drug A", "Drug B", "Drug B"),
    ex_ongoing = c(FALSE, TRUE, FALSE)
  )
  
  df3 <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002"),
    RSDY = c(15, 20),
    RSORRES = c("CR", "PR")
  )
  
  plot_obj <- dv.swimmerplot:::swimmerplot(
    subject_level_dataset = df1,
    subjid_var = "USUBJID",
    group_by_vars = NULL,
    sort_by_vars = "SEX",
    sort_direction = "asc",
    exposure_dataset = df2,
    trt_start_day_var = "EXSTDY",
    trt_end_day_var = "ex_end",
    trt_ongoing_var = "ex_ongoing",
    trt_tooltip_vars = c(
      "Subject: " = "USUBJID",
      "Treatment: " = "ex_trt"
    ),
    trt_group_var = "ex_trt",
    trt_legend_label = "Exposure",
    color_palette = NULL,
    response_dataset = df3,
    result_study_day_var = "RSDY",
    result_tooltip_vars = c(
      "Subject: " = "USUBJID",
      "Response: " = "RSORRES"
    ),
    result_cat_var = "RSORRES",
    result_legend_label = "Response",
    shape_mapping = NULL,
    plot_title = "Test Plot",
    plot_subtitle = "Test Subtitle",
    plot_x_label = "Day",
    plot_y_label = "Subject",
    plot_width = 8,
    plot_height = 6,
    interactive_plot = FALSE
  )
  
  expect_s3_class(plot_obj, "ggplot")
  
  vdiffr::expect_doppelganger("swimmerplot non-interactive", plot_obj)
})

test_that(
  "Subjects can be grouped by categorical variables using facets" |> vdoc[["add_spec"]](specs$plot$grouping),
  {
  df1 <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002", "SUBJ-003"),
    SEX = c("M", "F", "M"),
    GROUP = c("A", "B", "A")
  )
  
  df2 <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002", "SUBJ-003"),
    EXSTDY = c(1, 1, 2),
    ex_end = c(30, 25, 35),
    ex_trt = c("Drug A", "Drug B", "Drug A"),
    ex_ongoing = c(FALSE, FALSE, TRUE),
    GROUP = c("A", "B", "A")
  )
  
  df3 <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002", "SUBJ-003"),
    RSDY = c(15, 20, 25),
    RSORRES = c("CR", "PR", "SD"),
    GROUP = c("A", "B", "A")
  )
  
  plot_obj_grouped <- dv.swimmerplot:::swimmerplot(
    subject_level_dataset = df1,
    subjid_var = "USUBJID",
    group_by_vars = "GROUP",
    sort_by_vars = "SEX",
    sort_direction = "asc",
    exposure_dataset = df2,
    trt_start_day_var = "EXSTDY",
    trt_end_day_var = "ex_end",
    trt_ongoing_var = "ex_ongoing",
    trt_tooltip_vars = c(
      "Subject: " = "USUBJID",
      "Treatment: " = "ex_trt"
    ),
    trt_group_var = "ex_trt",
    trt_legend_label = "Exposure",
    color_palette = NULL,
    response_dataset = df3,
    result_study_day_var = "RSDY",
    result_tooltip_vars = c(
      "Subject: " = "USUBJID",
      "Response: " = "RSORRES"
    ),
    result_cat_var = "RSORRES",
    result_legend_label = "Response",
    shape_mapping = NULL,
    plot_title = "Test Plot",
    plot_subtitle = "Test Subtitle",
    plot_x_label = "Day",
    plot_y_label = "Subject",
    plot_width = 8,
    plot_height = 6
  )
  
  expect_s3_class(plot_obj_grouped, "girafe")
})

test_that(
  "Swimmerplot with grouping returns a ggplot object when interactive_plot=FALSE" |> vdoc[["add_spec"]](
    specs$plot$grouped_non_interactive
  ),
  {
  df1 <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002", "SUBJ-003"),
    SEX = c("M", "F", "M"),
    GROUP = c("A", "B", "A")
  )
  
  df2 <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002", "SUBJ-003"),
    EXSTDY = c(1, 1, 2),
    ex_end = c(30, 25, 35),
    ex_trt = c("Drug A", "Drug B", "Drug A"),
    ex_ongoing = c(FALSE, FALSE, TRUE),
    GROUP = c("A", "B", "A")
  )
  
  df3 <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002", "SUBJ-003"),
    RSDY = c(15, 20, 25),
    RSORRES = c("CR", "PR", "SD"),
    GROUP = c("A", "B", "A")
  )
  
  plot_obj_grouped <- dv.swimmerplot:::swimmerplot(
    subject_level_dataset = df1,
    subjid_var = "USUBJID",
    group_by_vars = "GROUP",
    sort_by_vars = "SEX",
    sort_direction = "asc",
    exposure_dataset = df2,
    trt_start_day_var = "EXSTDY",
    trt_end_day_var = "ex_end",
    trt_ongoing_var = "ex_ongoing",
    trt_tooltip_vars = c(
      "Subject: " = "USUBJID",
      "Treatment: " = "ex_trt"
    ),
    trt_group_var = "ex_trt",
    trt_legend_label = "Exposure",
    color_palette = NULL,
    response_dataset = df3,
    result_study_day_var = "RSDY",
    result_tooltip_vars = c(
      "Subject: " = "USUBJID",
      "Response: " = "RSORRES"
    ),
    result_cat_var = "RSORRES",
    result_legend_label = "Response",
    shape_mapping = NULL,
    plot_title = "Test Plot",
    plot_subtitle = "Test Subtitle",
    plot_x_label = "Day",
    plot_y_label = "Subject",
    plot_width = 8,
    plot_height = 6,
    interactive_plot = FALSE
  )
  
  expect_s3_class(plot_obj_grouped, "ggplot")
  
  vdiffr::expect_doppelganger("swimmerplot grouped non-interactive", plot_obj_grouped)
})

test_that(
  "Tooltips support named list format with labels and values" |> vdoc[["add_spec"]](c(
    specs$interactivity$tooltip_format,
    specs$interactivity$tooltips
  )), 
  {
  test_data <- data.frame(
    SUBJID = c("S001", "S002"),
    PARAM = c("Weight", "Height"),
    VALUE = c(80, 175),
    UNIT = c("kg", "cm"),
    stringsAsFactors = FALSE
  )
  
  tooltip_vector <- c(
    "Parameter: " = "PARAM",
    "Value: " = "VALUE",
    "Unit: " = "UNIT"
  )
  
  result <- dv.swimmerplot:::generate_tooltip(test_data, tooltip_vector)
  
  expected <- c(
    "Parameter: Weight<br>Value: 80<br>Unit: kg",
    "Parameter: Height<br>Value: 175<br>Unit: cm"
  )
  
  expect_equal(result, expected)
  
  empty_result <- dv.swimmerplot:::generate_tooltip(test_data, c())
  expect_equal(empty_result, c("", ""))
  
  null_result <- dv.swimmerplot:::generate_tooltip(test_data, NULL)
  expect_equal(null_result, c("", ""))
  
  unnamed_result <- dv.swimmerplot:::generate_tooltip(test_data, c("PARAM", "VALUE"))
  expect_equal(unnamed_result, c("", ""))
  
  tooltip_with_missing <- c(
    "Parameter: " = "PARAM",
    "Value: " = "VALUE",
    "Missing: " = "NONEXISTENT" 
  )
  
  result_with_missing <- dv.swimmerplot:::generate_tooltip(test_data, tooltip_with_missing)
  
  expected_with_missing <- c(
    "Parameter: Weight<br>Value: 80",
    "Parameter: Height<br>Value: 175"
  )
  
  expect_equal(result_with_missing, expected_with_missing)
})
