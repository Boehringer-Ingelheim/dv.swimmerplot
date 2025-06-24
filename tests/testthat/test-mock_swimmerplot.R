test_that(
  "mock swimmerplot app works with dynamic controls" |> vdoc[["add_spec"]](
    specs$app_functionality
  ), 
  {  
  app_dir <- testthat::test_path("apps/mock_swimmerplot")
  app_file <- file.path(app_dir, "app.R")

  expect_true(file.exists(app_file))
  
  app <- shinytest2::AppDriver$new(
    app_dir = app_dir, 
    name = "test_mock_app"
  )
  
  app$wait_for_idle()
  
  expect_null(app$get_value(input = "swimmerplot-group_vars"))
  expect_null(app$get_value(input = "swimmerplot-sort_vars"))
  expect_equal(app$get_value(input = "swimmerplot-sort_direction"), "asc")
  
  app$set_inputs(`swimmerplot-group_vars` = "SEX")
  app$wait_for_idle()
  expect_equal(app$get_value(input = "swimmerplot-group_vars"), "SEX")
  
  app$set_inputs(`swimmerplot-sort_vars` = "AGE")
  app$set_inputs(`swimmerplot-sort_direction` = "desc")
  app$wait_for_idle()
  expect_equal(app$get_value(input = "swimmerplot-sort_vars"), "AGE")
  expect_equal(app$get_value(input = "swimmerplot-sort_direction"), "desc")
  
  expect_true(app$get_value(output = "swimmerplot-swimmer_plot") != "")
  
  app$stop()
})
