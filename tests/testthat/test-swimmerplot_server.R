test_that(
  "swimmerplot_server() implements subject jumping with AFMM integration" |> 
    vdoc[["add_spec"]](specs$jumping_configuration), 
  {
  subject_level_dataset <- shiny::reactive(data.frame(USUBJID = c("A", "B", "C"), SEX = c("M", "F", "M")))
  exposure_dataset <- shiny::reactive(data.frame(
    USUBJID = c("A", "B", "C"),
    EXSTDY = 1:3,
    EXENDY = 3:5,
    EXTRT = c("TRT1", "TRT2", "TRT1"),
    ex_ongoing = c(FALSE, TRUE, FALSE),
    ex_tooltip = c("t1", "t2", "t3")
  ))
  response_dataset <- shiny::reactive(data.frame(
    USUBJID = c("A", "B", "C"),
    RSDY = 1:3,
    RSORRES = c("CR", "PR", "SD"),
    rs_tooltip = c("r1", "r2", "r3")
  ))

  jump_called <- shiny::reactiveVal(FALSE)
  
  # Create a mock afmm object with a utils element containing a switch2mod function
  mock_afmm <- list(
    utils = list(
      switch2mod = function(id) jump_called(TRUE)
    )
  )

  shiny::testServer(
    app = swimmerplot_server,
    args = list(
      id = "test",
      subject_level_dataset = subject_level_dataset,
      exposure_dataset = exposure_dataset,
      response_dataset = response_dataset,
      subjid_var = "USUBJID",
      trt_start_day_var = "EXSTDY",
      trt_end_day_var = "EXENDY",
      trt_ongoing_var = "ex_ongoing",
      trt_tooltip_vars = "ex_tooltip",
      trt_group_var = "EXTRT",
      trt_legend_label = "Treatment",
      color_palette = NULL,
      result_study_day_var = "RSDY",
      result_tooltip_vars = "rs_tooltip",
      result_cat_var = "RSORRES",
      result_legend_label = "Response",
      shape_mapping = NULL,
      plot_title = "Test Plot",
      plot_subtitle = NULL,
      plot_x_label = "Day",
      plot_y_label = "Subject",
      plot_width = 8,
      plot_height = 6,
      sort_by_vars = NULL,
      sort_direction = "asc",
      receiver_id = "papo1",
      afmm = mock_afmm
    ),
    {
      expect_type(session$returned, "list")
      expect_true("subj_id" %in% names(session$returned))
      expect_true(shiny::is.reactive(session$returned$subj_id))
      expect_false(jump_called())
      expect_null(session$returned$subj_id())

      session$setInputs(swimmer_plot_selected = "A")
      expect_true(jump_called())
      expect_equal(session$returned$subj_id(), "A")

      jump_called(FALSE)
      session$setInputs(swimmer_plot_selected = "C")
      expect_true(jump_called())
      expect_equal(session$returned$subj_id(), "C")

      expect_true(shiny::is.reactive(session$returned$subj_id))
    }
  )
})
